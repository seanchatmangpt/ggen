//! Chicago-TDD e2e proofs for the two P1/P2 changes on `feat/reflexive-law`:
//!
//! - P1: pack-shipped `hook.ttl` Knowledge Hooks go live in `ggen sync` (via
//!   `GraphEngine::load_hook_pack`, delegating to the real, test-proven
//!   `praxis_graphlaw::TripleStore::load_hook_pack` + `.materialize()`
//!   mechanism — see `crates/praxis-graphlaw/tests/self_monitoring_hook_actuation.rs`
//!   and `packs/self-monitoring-pack/hook.ttl`).
//! - P2: opt-in reflexive receipts (`[law] reflexive = true`) fold the
//!   project's own `.ggen-v2/receipt-log.jsonl` history back into the graph
//!   as `ggenr:Sync` facts before templates render.
//!
//! Scaffold conventions copied verbatim from `framework_packs_e2e.rs`'s
//! `write_synthetic_pack`/`scaffold_synthetic_consumer` helpers (same
//! `TempDir` + real `ggen` binary via `CliHarness` shape), extended with an
//! optional `hook.ttl` for a synthetic pack.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Write a minimal synthetic pack (pack.toml + ontology.ttl + one trivial
/// template) into `dir/<name>/`, optionally with `shapes.ttl` and/or
/// `hook.ttl`. Mirrors `framework_packs_e2e.rs::write_synthetic_pack`,
/// extended with a `hook_ttl` parameter.
fn write_synthetic_pack(
    dir: &Path, name: &str, ontology_ttl: &str, shapes_ttl: Option<&str>, hook_ttl: Option<&str>,
) {
    let pack = dir.join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.0.1\"\ndescription = \"synthetic test pack\"\n"
        ),
    )
    .expect("write pack.toml");
    std::fs::write(pack.join("ontology.ttl"), ontology_ttl).expect("write pack ontology");
    if let Some(shapes) = shapes_ttl {
        std::fs::write(pack.join("shapes.ttl"), shapes).expect("write pack shapes");
    }
    if let Some(hook) = hook_ttl {
        std::fs::write(pack.join("hook.ttl"), hook).expect("write pack hook");
    }
    // One trivial static template so the pack passes FM-PACK-005 (zero
    // templates refused) without depending on any graph content.
    std::fs::write(
        pack.join("templates/marker.txt.tmpl"),
        format!("---\nto: {name}_marker.txt\nforce: true\n---\npack {name} ran\n"),
    )
    .expect("write pack template");
}

/// Minimal consumer wired to the named synthetic packs. `extra_ggen_toml`
/// (e.g. `"[law]\nreflexive = true\n"`) is appended verbatim after the
/// `[templates]` table.
fn scaffold_synthetic_consumer(
    dir: &Path, pack_names: &[&str], consumer_ontology_ttl: &str, extra_ggen_toml: &str,
) -> PathBuf {
    let project = dir.join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), consumer_ontology_ttl)
        .expect("write ontology.ttl");
    let packs_lines: String = pack_names
        .iter()
        .map(|n| format!("{n} = {{ path = \"../{n}\" }}\n"))
        .collect();
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_lines}\n\
             [templates]\ndir = \"templates\"\n\n\
             {extra_ggen_toml}"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// The smallest `kh:` hook shape that derives one new fact
/// (`ex:derived ex:flag 'fired'`) from `ex:t1 a ex:TriggerClass` — mirrors
/// `packs/self-monitoring-pack/hook.ttl`'s real, tested `kh:Hook`
/// (`kh:name`/`kh:kind "sparql"`/`kh:query` SELECT trigger/`kh:effect`
/// `"emit-delta"`/`kh:action`/`kh:priority`) + separate `kh:Action`
/// individual (`kh:handler <.../handler#sparql-construct>`, `kh:query`
/// CONSTRUCT) shape, run via the same
/// `praxis_graphlaw::TripleStore::load_hook_pack` + `.materialize()`
/// mechanism that test exercises. `ex:TriggerClass` is asserted by the
/// CONSUMER's own ontology.ttl (not this pack's own ontology), so a fire
/// proves the hook evaluates over the real union graph. The CONSTRUCT's
/// object literal uses single quotes (not double) — the same
/// `clean_term`-survival convention `packs/self-monitoring-pack/hook.ttl`'s
/// header documents (double-quoted literals embedded in a `kh:query`
/// string get one quote stripped by the engine's term-cleaning pass).
const HOOK_TTL: &str = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .

<http://example.org/gap#derive_fired>
    a kh:Hook ;
    kh:name "derive_fired" ;
    kh:kind "sparql" ;
    kh:query """SELECT ?t WHERE { ?t a <http://example.org/gap#TriggerClass> }""" ;
    kh:effect "emit-delta" ;
    kh:action <http://example.org/gap#derive_fired_action> ;
    kh:priority 1 .

<http://example.org/gap#derive_fired_action>
    a kh:Action ;
    kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
    kh:query """CONSTRUCT { <http://example.org/gap#derived> <http://example.org/gap#flag> 'fired' } WHERE { ?t a <http://example.org/gap#TriggerClass> }""" .
"#;

/// A pack template that SELECTs `ex:derived ex:flag ?flag` into a rendered
/// output file.
fn write_flag_template(pack_dir: &Path) {
    std::fs::write(
        pack_dir.join("templates/flag.txt.tmpl"),
        "---\n\
         to: flag.txt\n\
         force: true\n\
         sparql:\n\
         \x20 rows: |\n\
         \x20   PREFIX ex: <http://example.org/gap#>\n\
         \x20   SELECT ?flag WHERE { ex:derived ex:flag ?flag }\n\
         ---\n\
         {% for row in rows %}{{ row.flag }}\n{% endfor %}",
    )
    .expect("write flag template");
}

const TRIGGER_ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/gap#> .
ex:t1 a ex:TriggerClass .
"#;

#[test]
fn hooks_live_and_are_queryable_by_templates() {
    let dir = TempDir::new().expect("tempdir");
    write_synthetic_pack(
        dir.path(),
        "hook-pack",
        "@prefix ex: <http://example.org/gap#> .\n",
        None,
        Some(HOOK_TTL),
    );
    write_flag_template(&dir.path().join("hook-pack"));

    let project = scaffold_synthetic_consumer(dir.path(), &["hook-pack"], TRIGGER_ONTOLOGY, "");

    // (a) First sync succeeds and the output contains "fired".
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();
    let flag_path = project.join("flag.txt");
    let flag = std::fs::read_to_string(&flag_path).expect("flag.txt");
    assert!(flag.contains("fired"), "hook must have fired: {flag}");

    // (b) Two consecutive syncs produce byte-identical output.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let flag2 = std::fs::read_to_string(&flag_path).expect("flag.txt 2");
    assert_eq!(flag, flag2, "second sync must be byte-identical");

    // (c) Removing the triggering fact makes the derived fact (and hence
    // "fired") disappear — no stale derivation left over from a previous
    // run.
    std::fs::write(
        project.join("ontology.ttl"),
        "@prefix ex: <http://example.org/gap#> .\n",
    )
    .expect("clear trigger fact");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("third sync")
        .assert_success();
    let flag3 = std::fs::read_to_string(&flag_path).expect("flag.txt 3");
    assert!(
        !flag3.contains("fired"),
        "derived fact must not survive removal of its trigger: {flag3}"
    );
}

#[test]
fn hook_derived_facts_are_shape_gated() {
    let dir = TempDir::new().expect("tempdir");
    // A shapes.ttl violated ONLY by the fact the hook derives: ex:derived
    // must have at most 0 ex:flag values (i.e. any ex:flag on it violates).
    let shapes = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/gap#> .
ex:DerivedShape
    a sh:NodeShape ;
    sh:targetNode ex:derived ;
    sh:property [
        sh:path ex:flag ;
        sh:maxCount 0 ;
        sh:message "ex:derived must never carry ex:flag (hook-derived-only guard)" ;
    ] .
"#;
    write_synthetic_pack(
        dir.path(),
        "hook-pack",
        "@prefix ex: <http://example.org/gap#> .\n",
        Some(shapes),
        Some(HOOK_TTL),
    );
    write_flag_template(&dir.path().join("hook-pack"));

    let project = scaffold_synthetic_consumer(dir.path(), &["hook-pack"], TRIGGER_ONTOLOGY, "");

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_failure();
    output.assert_stderr_contains("FM-PACK-013");
    assert!(
        !project.join("flag.txt").exists(),
        "a refused sync must not have written any template output"
    );
    assert!(
        !project.join("hook-pack_marker.txt").exists(),
        "a refused sync must not have written any template output"
    );
}

#[test]
fn reflexive_receipts_second_sync_sees_first() {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        project.join("templates/count.txt.tmpl"),
        "---\n\
         to: count.txt\n\
         force: true\n\
         sparql:\n\
         \x20 syncs: |\n\
         \x20   PREFIX ggenr: <http://seanchatmangpt.github.io/ggen/receipt#>\n\
         \x20   SELECT ?s WHERE { ?s a ggenr:Sync }\n\
         ---\n\
         {{ syncs | length }}",
    )
    .expect("write count template");

    // First sync: log is empty at load time (before this sync's own
    // receipt is written) -> "0".
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();
    let count1 = std::fs::read_to_string(project.join("count.txt")).expect("count.txt");
    assert_eq!(
        count1.trim(),
        "0",
        "first sync must see zero prior syncs: {count1}"
    );

    // Second sync: the first sync's own receipt-log line is now visible.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let count2 = std::fs::read_to_string(project.join("count.txt")).expect("count.txt 2");
    assert_eq!(
        count2.trim(),
        "1",
        "second sync must see exactly one prior sync: {count2}"
    );

    // Receipt verification still succeeds after both syncs.
    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(&project)
        .run()
        .expect("receipt verify")
        .assert_success();
}

#[test]
fn reflexive_off_by_default_is_unaffected() {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        project.join("templates/hello.txt.tmpl"),
        "---\nto: hello.txt\nforce: true\n---\nhello\n",
    )
    .expect("write template");

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("first sync")
        .assert_success();
    let hello1 = std::fs::read(project.join("hello.txt")).expect("hello.txt");
    let receipt1 = std::fs::read(project.join(".ggen-v2/receipt.json")).expect("receipt.json");
    let lock1 = std::fs::read(project.join("ggen.lock")).ok();

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let hello2 = std::fs::read(project.join("hello.txt")).expect("hello.txt 2");
    assert_eq!(hello1, hello2, "files must be byte-identical");

    // The two receipts differ only by the intentional chain fields
    // (prev_chain_hash_hex / chain_hash_hex / signature); the payload's
    // graph_hash must be identical across the two syncs since reflexive is
    // off and nothing else in the graph changed.
    let r1: serde_json::Value = serde_json::from_slice(&receipt1).expect("receipt1 json");
    let receipt2_bytes =
        std::fs::read(project.join(".ggen-v2/receipt.json")).expect("receipt.json 2");
    let r2: serde_json::Value = serde_json::from_slice(&receipt2_bytes).expect("receipt2 json");
    assert_eq!(
        r1["payload"]["graph_hash"], r2["payload"]["graph_hash"],
        "graph_hash must be identical across identical syncs when reflexive is off"
    );
    assert_eq!(
        r1["payload"]["outputs"], r2["payload"]["outputs"],
        "output hashes must be identical across identical syncs when reflexive is off"
    );

    let lock2 = std::fs::read(project.join("ggen.lock")).ok();
    assert_eq!(lock1, lock2, "lock (if any) must be byte-identical");
}

#[test]
fn malformed_receipt_log_line_is_skipped_not_fatal() {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        "[project]\nname = \"consumer\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n\n\
         [law]\nreflexive = true\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        project.join("templates/count.txt.tmpl"),
        "---\n\
         to: count.txt\n\
         force: true\n\
         sparql:\n\
         \x20 syncs: |\n\
         \x20   PREFIX ggenr: <http://seanchatmangpt.github.io/ggen/receipt#>\n\
         \x20   SELECT ?s WHERE { ?s a ggenr:Sync }\n\
         ---\n\
         {{ syncs | length }}",
    )
    .expect("write count template");

    // First sync creates a real, well-formed log line + establishes .ggen-v2/.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("seed sync")
        .assert_success();

    // Hand-insert one garbage (non-JSON) line into the receipt log. Prepended
    // rather than appended: `write_receipt`'s own chain-head lookup
    // (`read_prev_head`) reads the log's TAIL to determine what to chain
    // onto for the *next* write, and that mechanism is intentionally
    // fail-closed on a malformed tail (a real, separate invariant from this
    // test's concern — a tampered/garbage chain head must never be silently
    // extended). This test's own concern is Stage 1's reflexive ingestion,
    // which reads and tolerates malformed lines ANYWHERE in the log — so the
    // garbage line is placed first, leaving the real, well-formed line as
    // the tail `read_prev_head` still chains onto correctly.
    let log_path = project.join(".ggen-v2/receipt-log.jsonl");
    let existing = std::fs::read_to_string(&log_path).expect("read seeded log");
    std::fs::write(&log_path, format!("not-json-at-all-{{garbage\n{existing}"))
        .expect("prepend garbage line");

    // Sync still succeeds; the good (first) line is still reflected...
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync with malformed line")
        .assert_success();
    let count = std::fs::read_to_string(project.join("count.txt")).expect("count.txt");
    assert_eq!(
        count.trim(),
        "1",
        "the one well-formed prior line must still be reflected: {count}"
    );

    // ...and the receipt's closure names the skip.
    let receipt: serde_json::Value = serde_json::from_slice(
        &std::fs::read(project.join(".ggen-v2/receipt.json")).expect("receipt.json"),
    )
    .expect("receipt json");
    let closure = receipt["payload"]["closure"]
        .as_object()
        .expect("closure object");
    assert!(
        closure
            .keys()
            .any(|k| k.starts_with("receipt-log:MALFORMED-")),
        "closure must record the malformed-line skip: {closure:?}"
    );
}
