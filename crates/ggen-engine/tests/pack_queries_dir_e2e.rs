//! RFC-GPACK-001 D1 falsifier pair — §96 "Exact Falsifier Requirement":
//!
//! - Gate != Query: a returning SELECT under `queries/` must NOT refuse
//!   (its rows are named render bindings, RFC §13, §3.5 Query Is Not Gate).
//! - Gate semantics: the SAME returning SELECT under `gates/` MUST refuse
//!   via the gate-violation error path (RFC §14: `Violation = RowCount > 0`
//!   for SELECT gates).
//!
//! Scaffold conventions copied from `reflexive_law_e2e.rs` /
//! `framework_packs_e2e.rs` (TempDir + real `ggen` binary via
//! `CliHarness`): the consumer project wires one synthetic pack and runs
//! `ggen sync run` at the real process boundary.
//!
//! Binding-exposure is part of the positive falsifier: the consumer
//! template references `{{ entities }}` with NO frontmatter `sparql:` of
//! its own, so the only possible source of that binding is the pack's
//! `queries/entities.rq`. If `queries/` discovery were missing or silently
//! ignored, Tera would fail with an unknown-variable render error and the
//! test would catch it.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// The query text used by BOTH falsifiers — §96 requires the SAME query in
/// both directories. A positive SELECT (≥1 row over the union graph):
/// every `ex:Entity` individual.
const POSITIVE_SELECT: &str = "PREFIX ex: <http://example.org/gap#>\n\
                               SELECT ?e WHERE { ?e a ex:Entity }\n";

/// Consumer ontology asserting exactly one entity, so the SELECT above
/// returns ≥1 row against the post-materialization union graph.
const CONSUMER_ONTOLOGY: &str = r"
@prefix ex: <http://example.org/gap#> .
ex:e1 a ex:Entity .
";

/// Write a minimal synthetic pack (pack.toml + ontology.ttl + templates)
/// into `dir/<name>/`, optionally with `queries/entities.rq` and/or
/// `gates/gate.rq` holding `query_text`. Mirrors
/// `reflexive_law_e2e.rs::write_synthetic_pack`.
fn write_synthetic_pack(dir: &Path, name: &str, query_text: Option<&str>, gate_text: Option<&str>) {
    let pack = dir.join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.0.1\"\ndescription = \"synthetic test pack\"\n"
        ),
    )
    .expect("write pack.toml");
    std::fs::write(
        pack.join("ontology.ttl"),
        "@prefix ex: <http://example.org/gap#> .\n",
    )
    .expect("write pack ontology");
    if let Some(query) = query_text {
        std::fs::create_dir_all(pack.join("queries")).expect("mkdir pack queries");
        std::fs::write(pack.join("queries/entities.rq"), query).expect("write pack query");
    }
    if let Some(gate) = gate_text {
        std::fs::create_dir_all(pack.join("gates")).expect("mkdir pack gates");
        std::fs::write(pack.join("gates/gate.rq"), gate).expect("write pack gate");
    }
    // Static marker template: proves the pack itself was admitted, distinct
    // from the binding-consumer template below.
    std::fs::write(
        pack.join("templates/marker.txt.tmpl"),
        format!("---\nto: {name}_marker.txt\nforce: true\n---\npack {name} ran\n"),
    )
    .expect("write pack marker template");
    // Binding-consumer template: renders the pack query's named binding
    // (`entities`, the `queries/entities.rq` file stem) with NO frontmatter
    // `sparql:` — the binding can only come from `queries/` discovery.
    std::fs::write(
        pack.join("templates/entities.txt.tmpl"),
        "---\n\
         to: entities.txt\n\
         force: true\n\
         ---\n\
         {% for row in entities %}{{ row.e }}\n{% endfor %}",
    )
    .expect("write pack entities template");
}

/// Minimal consumer project wired to the named synthetic packs.
/// Conventions copied from `reflexive_law_e2e.rs::scaffold_synthetic_consumer`.
fn scaffold_synthetic_consumer(dir: &Path, pack_names: &[&str]) -> PathBuf {
    use std::fmt::Write as _;

    let project = dir.join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), CONSUMER_ONTOLOGY).expect("write ontology.ttl");
    let packs_lines: String = pack_names.iter().fold(String::new(), |mut lines, n| {
        let _ = writeln!(lines, "{n} = {{ path = \"../{n}\" }}");
        lines
    });
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_lines}\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// §96 falsifier row 1 — Gate != Query: the SAME positive SELECT that
/// refuses under `gates/` must NOT refuse under `queries/`; its rows must
/// surface as the named render binding `entities` (the file stem).
#[test]
fn query_positive_select_does_not_refuse() {
    let dir = TempDir::new().expect("tempdir");
    write_synthetic_pack(dir.path(), "query-pack", Some(POSITIVE_SELECT), None);

    let project = scaffold_synthetic_consumer(dir.path(), &["query-pack"]);

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // The binding-consumer output proves the query executed and its rows
    // were exposed as named render bindings — `entities` has no other
    // possible source (the template declares no `sparql:` of its own).
    let entities_path = project.join("entities.txt");
    let entities = std::fs::read_to_string(&entities_path)
        .expect("entities.txt must exist: a returning SELECT in queries/ must render, not refuse");
    assert!(
        entities.contains("e1"),
        "the SELECT row for ex:e1 must be rendered from the pack query binding: {entities}"
    );
    assert!(
        project.join("query-pack_marker.txt").exists(),
        "pack marker must still render: queries/ never refuses"
    );
}

/// §96 falsifier row 2 — Gate semantics: the SAME positive SELECT in
/// `gates/` refuses the sync through the gate-violation error path
/// (`[FM-PACK-013]`, "refused the sync") and nothing is written.
#[test]
fn gate_positive_select_refuses() {
    let dir = TempDir::new().expect("tempdir");
    write_synthetic_pack(dir.path(), "gate-pack", None, Some(POSITIVE_SELECT));

    let project = scaffold_synthetic_consumer(dir.path(), &["gate-pack"]);

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_failure();
    output.assert_stderr_contains("FM-PACK-013");
    output.assert_stderr_contains("refused the sync");
    // Refusal happens in the validate stage, before any write.
    assert!(
        !project.join("entities.txt").exists(),
        "a refused sync must not have written any template output"
    );
    assert!(
        !project.join("gate-pack_marker.txt").exists(),
        "a refused sync must not have written any template output"
    );
}
