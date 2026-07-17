//! Chicago-TDD end-to-end tests for the sync pipeline: real filesystem,
//! real oxigraph store, real Tera rendering — no mocks.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

#[test]
fn first_sync_writes_second_sync_skips_unchanged_and_hash_is_stable() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    // First run: writes the file.
    let first = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    assert!(
        first.skipped.is_empty(),
        "unexpected skips: {:?}",
        first.skipped
    );
    let content1 = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(content1, "alice\nbob\n");

    // Second run: all Skipped(unchanged), byte-identical file, same hash.
    let second = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second run wrote: {:?}",
        second.written
    );
    assert_eq!(second.skipped.len(), 1);
    assert!(
        second.skipped[0].1.contains("unchanged"),
        "reason: {}",
        second.skipped[0].1
    );
    let content2 = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(content2, content1, "output must be byte-identical");
    assert_eq!(
        second.graph_hash_hex, first.graph_hash_hex,
        "graph hash must be stable"
    );
}

#[test]
fn dry_run_writes_nothing() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry run");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "dry run must not write"
    );
    assert!(
        !dir.path().join(RECEIPT_REL_PATH).exists(),
        "dry run must not emit a receipt"
    );
}

#[test]
fn non_dry_sync_emits_verifiable_receipt() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    let raw = std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt exists");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("receipt parses");

    assert_eq!(receipt.payload.graph_hash, report.graph_hash_hex);
    assert_eq!(receipt.payload.outputs.len(), 1);
    let file_bytes = std::fs::read(dir.path().join("out/names.txt")).expect("output");
    assert_eq!(
        receipt
            .payload
            .outputs
            .get("out/names.txt")
            .map(String::as_str),
        Some(blake3::hash(&file_bytes).to_hex().as_str()),
        "receipt must bind the real output bytes"
    );

    // Chain hash recomputes to the stored value via praxis-core.
    let recomputed = receipt.record.recompute_chain_hash().expect("recompute");
    let recomputed_hex: String = recomputed.iter().map(|b| format!("{b:02x}")).collect();
    assert_eq!(recomputed_hex, receipt.record.chain_hash_hex);
    assert_eq!(receipt.record.ts_ns, 0, "no wall clock: ts_ns pinned to 0");
}

#[test]
fn missing_ggen_toml_fails_closed() {
    let dir = TempDir::new().expect("tempdir");
    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must fail");
    assert!(err.to_string().contains("FM-CONFIG-001"), "{err}");
}

#[test]
fn render_failure_names_available_context_keys() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    // `row.nuon` is a typo for `row.noun` — Tera has no such field and
    // errors; the error message should name what WAS available (`results`)
    // rather than just repeating Tera's own "not found" text.
    std::fs::write(
        dir.path().join("templates/typo.tmpl"),
        "---\nto: out.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name }\n---\n{% for row in results %}{{ row.nuon }}\n{% endfor %}",
    )
    .expect("write template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("typo must fail render");
    let msg = err.to_string();
    assert!(msg.contains("render failed"), "{msg}");
    assert!(
        msg.contains("Available top-level context keys"),
        "error should name available context keys: {msg}"
    );
    assert!(
        msg.contains("results"),
        "should list the `results` key: {msg}"
    );
}

#[test]
fn oversized_rendered_output_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    // No SPARQL rows needed: a fixed-count Tera loop is enough to blow past
    // the 10MB cap with a single long repeated line.
    std::fs::write(
        dir.path().join("templates/huge.tmpl"),
        "---\nto: out.txt\n---\n{% for i in range(end=200000) %}0123456789012345678901234567890123456789012345678901234567890123\n{% endfor %}",
    )
    .expect("write template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("oversized output must be refused");
    let msg = err.to_string();
    assert!(msg.contains("over the"), "{msg}");
    assert!(msg.contains("byte cap"), "{msg}");
    assert!(
        !dir.path().join("out.txt").exists(),
        "no output must be written on refusal"
    );
}

#[test]
fn a_render_failure_leaves_no_writes_from_other_templates_in_the_same_run() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    // "a_good" sorts before "b_bad", so if writes were applied as each
    // template rendered (the old behavior) rather than only after every
    // template renders successfully, a_good's output would land on disk
    // even though the run as a whole fails.
    std::fs::write(
        dir.path().join("templates/a_good.tmpl"),
        "---\nto: good.txt\n---\nfine\n",
    )
    .expect("write good template");
    std::fs::write(
        dir.path().join("templates/b_bad.tmpl"),
        "---\nto: bad.txt\n---\n{{ row.nuon }}\n",
    )
    .expect("write bad template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("the bad template must fail the whole run");
    assert!(err.to_string().contains("render failed"), "{err}");
    assert!(
        !dir.path().join("good.txt").exists(),
        "a_good's write must not land when a later template in the same run fails to render"
    );
    assert!(!dir.path().join("bad.txt").exists());
}

/// Two rows rendering to the same `to:` target must refuse (FM-WRITE-008)
/// instead of silently last-row-winning on disk.
#[test]
fn duplicate_render_targets_are_refused() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    // Templated `to:` that collapses every row onto one constant path.
    let tpl = "---\nto: \"out/{{ row.name | length }}.txt\"\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ row.name }}";
    std::fs::write(dir.path().join("templates/one.tmpl"), tpl).expect("write template");

    // alice/bob → different lengths; use equal-length names to collide.
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"anna\" .\nex:b ex:name \"bert\" .\n",
    )
    .expect("write ontology");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-WRITE-008"), "{msg}");
    assert!(msg.contains("same output"), "{msg}");
    assert!(
        !dir.path().join("out").exists(),
        "refusal must happen before any write"
    );
}

/// A `determinism: true` template whose `to:` path is non-deterministic is
/// refused even when the body is deterministic — the path is output too.
#[test]
fn nondeterministic_to_path_violates_determinism_check() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let tpl = "---\nto: \"out/{{ now() | date(format='%f') }}-{{ row.name }}.txt\"\ndeterminism: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ row.name }}";
    std::fs::write(dir.path().join("templates/one.tmpl"), tpl).expect("write template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-009"), "{msg}");
    assert!(msg.contains("to:"), "{msg}");
}

/// Dry-run classifies an existing non-UTF-8 target as a typed refusal, not
/// as a cheerful "planned: write".
#[test]
fn dry_run_refuses_non_utf8_existing_target() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::create_dir_all(dir.path().join("out")).expect("mkdir out");
    std::fs::write(dir.path().join("out/names.txt"), [0xFFu8, 0xFE, 0x00, 0x9F])
        .expect("write binary target");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-WRITE-009"), "{msg}");
    assert!(msg.contains("UTF-8"), "{msg}");
}
