//! Explicit frontmatter projection cardinality — real graph, Tera, filesystem,
//! determinism recheck, and refusal boundaries.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "cardinality"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:a ex:name "alpha" .
ex:z ex:name "zeta" .
ex:aux ex:aux "not-the-driver" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("manifest");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("ontology");
    std::fs::create_dir_all(root.join("templates")).expect("templates");
}

fn template(root: &Path, source: &str) {
    std::fs::write(root.join("templates/cardinality.tmpl"), source).expect("template");
}

fn run(root: &Path) -> ggen_engine::sync::SyncReport {
    sync(root, SyncOptions::default()).expect("sync")
}

const QUERIES: &str = r"
sparql:
  00_aux: |
    PREFIX ex: <http://example.org/>
    SELECT ?aux WHERE { ?s ex:aux ?aux } ORDER BY ?aux
  entities: |
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
for_each: entities
";

#[test]
fn explicit_driver_aggregates_static_target_in_row_order() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: registry.txt\n{QUERIES}force: true\ndeterminism: true\n---\n{{{{ row.name }}}}\n"
        ),
    );

    let report = run(dir.path());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("registry.txt")).expect("output"),
        "alpha\nzeta\n"
    );
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("registry.txt")]
    );
}

#[test]
fn explicit_driver_fans_out_dynamic_targets() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: \"out/{{{{ row.name }}}}.txt\"\n{QUERIES}determinism: true\n---\n{{{{ row.name }}}}\n"
        ),
    );

    run(dir.path());
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/alpha.txt")).expect("alpha"),
        "alpha\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/zeta.txt")).expect("zeta"),
        "zeta\n"
    );
}

#[test]
fn missing_driver_refuses_before_hook_or_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: out.txt\nsparql:\n  entities: SELECT ?name WHERE { ?s <http://example.org/name> ?name }\nfor_each: missing\nsh_before: \"echo ran > hook.log\"\n---\nbody\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-019"), "{err}");
    assert!(!dir.path().join("hook.log").exists());
    assert!(!dir.path().join("out.txt").exists());
}

#[test]
fn scalar_driver_refuses() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: out.txt\nsparql:\n  enabled: ASK { ?s <http://example.org/name> ?name }\nfor_each: enabled\n---\nbody\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-019"), "{err}");
}

/// LEAD 3 (2026-08-03 unverified-leads audit): "the implicit `for_each`
/// driver degrades to zero rows silently." Confirmed real for the EXPLICIT
/// named `for_each:` driver: `render_aggregate_projection` legitimately
/// returns `Ok(None)` for a zero-row driver query, and the call site used to
/// `continue` with no trace anywhere in the report -- a real empty answer
/// was indistinguishable from a template silently dropped for any other
/// reason. Contrast with the sibling `when:`-guard-false path in the same
/// function, which already records `decisions`/`skipped` -- this asymmetry
/// is the gap. Zero rows itself is not the bug (see `ggen-mcp`'s
/// `honest_zero_rows_is_lawful` referee invariant); being unobservable was.
#[test]
fn explicit_driver_zero_rows_is_recorded_not_silently_skipped() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: registry.txt\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:name ?name . FILTER(?name = \"nobody\") }\nfor_each: entities\n---\n{{ row.name }}\n",
    );

    let report = run(dir.path());
    assert!(
        !dir.path().join("registry.txt").exists(),
        "zero rows must still write nothing"
    );
    assert!(
        report.written.is_empty(),
        "zero rows must write nothing: {:?}",
        report.written
    );
    let decision = report.decisions.get("registry.txt").unwrap_or_else(|| {
        panic!(
            "zero-row for_each must be recorded in decisions: {:?}",
            report.decisions
        )
    });
    assert!(
        decision.contains("0 rows") || decision.contains("skipped"),
        "decision must name the zero-row cause honestly: {decision}"
    );
    assert!(
        report
            .skipped
            .iter()
            .any(|(p, _)| p == std::path::Path::new("registry.txt")),
        "zero-row for_each must appear in report.skipped: {:?}",
        report.skipped
    );
}

/// Same LEAD 3 gap, but for the IMPLICIT per-row fan-out driver (a
/// templated `to:` interpolating over the primary `sparql:` result, no
/// named `for_each:` field at all) -- the other half of "the implicit
/// `for_each` driver degrades to zero rows silently."
#[test]
fn implicit_fan_out_zero_rows_is_recorded_not_silently_skipped() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        "---\nto: \"out/{{ row.name }}.txt\"\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:name ?name . FILTER(?name = \"nobody\") }\n---\n{{ row.name }}\n",
    );

    let report = run(dir.path());
    assert!(
        !dir.path().join("out").exists(),
        "zero rows must write nothing"
    );
    assert!(
        report.written.is_empty(),
        "zero rows must write nothing: {:?}",
        report.written
    );
    let key = "@template/templates/cardinality.tmpl";
    let decision = report.decisions.get(key).unwrap_or_else(|| {
        panic!(
            "zero-row implicit fan-out must be recorded in decisions: {:?}",
            report.decisions
        )
    });
    assert!(
        decision.contains("0 rows") || decision.contains("skipped"),
        "decision must name the zero-row cause honestly: {decision}"
    );
    assert!(
        report
            .skipped
            .iter()
            .any(|(p, _)| p == std::path::Path::new(key)),
        "zero-row implicit fan-out must appear in report.skipped: {:?}",
        report.skipped
    );
}

#[test]
fn static_aggregate_refuses_row_varying_lifecycle_law() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    template(
        dir.path(),
        &format!(
            "---\nto: registry.txt\n{QUERIES}sh_after: \"echo {{{{ row.name }}}} >> hook.log\"\n---\n{{{{ row.name }}}}\n"
        ),
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    assert!(err.to_string().contains("FM-TPL-020"), "{err}");
    assert!(!dir.path().join("registry.txt").exists());
    assert!(!dir.path().join("hook.log").exists());
}
