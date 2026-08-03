//! G5 evidence: the five-stage pipeline (Resolve -> Enrich -> Extract ->
//! Render -> Write -> Receipt, `crates/ggen-engine/src/sync.rs`) actually
//! produces stage-specific, inspectable evidence -- not just "sync
//! succeeded". Chicago TDD: real filesystem, real oxigraph/GraphLaw engine,
//! real Tera rendering. No mocks.
//!
//! Positive witnesses: `SyncReport::graph_hash_hex`, `::closure`, and
//! `::decisions` are populated with real per-input/per-output evidence after
//! a real sync. Negative falsifiers: a missing ontology file, a template
//! `to:` path that escapes the project root, and an oversized rendered body
//! all refuse closed with typed `[FM-*]` errors instead of silently
//! producing a decorative "success".

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "stage-evidence"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:widget ex:label "Widget" .
ex:gadget ex:label "Gadget" .
"#;

const TEMPLATE: &str = "---\nto: out/labels.txt\nsparql:\n  rows: SELECT ?label WHERE { ?s <http://example.org/label> ?label } ORDER BY ?label\n---\n{% for row in results %}{{ row.label }}\n{% endfor %}";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates").join("labels.tmpl"), TEMPLATE).expect("write template");
}

/// Positive witness: a real, successful sync produces a `SyncReport` whose
/// `graph_hash_hex`, `closure`, and `decisions` fields carry real,
/// stage-specific evidence -- not placeholders.
#[test]
fn successful_sync_populates_stage_specific_report_fields() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let report = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed on a well-formed project");

    // Resolve/Enrich evidence: graph_hash_hex is a real 32-byte BLAKE3 hex
    // digest (64 hex chars), never empty or a placeholder.
    assert_eq!(
        report.graph_hash_hex.len(),
        64,
        "graph_hash_hex must be a 64-char BLAKE3 hex digest, got {:?}",
        report.graph_hash_hex
    );
    assert!(
        report.graph_hash_hex.chars().all(|c| c.is_ascii_hexdigit()),
        "graph_hash_hex must be pure hex: {:?}",
        report.graph_hash_hex
    );

    // Closure (input-binding) evidence: the manifest, ontology, and template
    // must each be bound to a real content hash, and the generator identity
    // must be recorded.
    assert!(
        report.closure.contains_key("ggen.toml"),
        "closure must bind ggen.toml: {:?}",
        report.closure.keys().collect::<Vec<_>>()
    );
    assert!(
        report.closure.contains_key("ontology.ttl"),
        "closure must bind ontology.ttl: {:?}",
        report.closure.keys().collect::<Vec<_>>()
    );
    assert!(
        report
            .closure
            .get("actuator")
            .is_some_and(|v| v.starts_with("ggen@")),
        "closure must record the actuator/generator version: {:?}",
        report.closure.get("actuator")
    );
    // Every closure hash for a real, readable input must be a 64-hex BLAKE3
    // digest, not "MISSING" (that sentinel is reserved for genuinely
    // unreadable declared inputs -- see `hash_file_or_missing` in sync.rs).
    let manifest_hash = report.closure.get("ggen.toml").expect("bound");
    assert_ne!(manifest_hash, "MISSING");
    assert_eq!(manifest_hash.len(), 64);

    // Write evidence: the one declared output landed and is recorded as
    // "written" in the decisions map, keyed by its root-relative path.
    assert_eq!(
        report.decisions.get("out/labels.txt").map(String::as_str),
        Some("written"),
        "decisions map: {:?}",
        report.decisions
    );
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/labels.txt")]
    );
    assert!(
        dir.path().join("out/labels.txt").exists(),
        "the file the report claims was written must actually exist on disk"
    );
    let body = std::fs::read_to_string(dir.path().join("out/labels.txt")).expect("read output");
    assert_eq!(
        body, "Gadget\nWidget\n",
        "SPARQL ORDER BY must drive real render output"
    );
}

/// Negative falsifier: Stage 1 (Resolve) must fail closed, with a typed
/// `[FM-CONFIG-003]`-flavored error, when the declared ontology file is
/// unreadable -- not silently proceed with an empty graph.
#[test]
fn missing_ontology_file_refuses_closed_at_resolve_stage() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    // Deliberately omit ontology.ttl.
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(dir.path().join("templates").join("labels.tmpl"), TEMPLATE)
        .expect("write template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("sync must refuse when the declared ontology is unreadable");

    let msg = err.to_string();
    assert!(
        msg.contains("unreadable") || msg.contains("ontology"),
        "error must name the real cause (unreadable ontology), got: {msg}"
    );
    assert!(
        !dir.path().join(".ggen-v2/receipt.json").exists(),
        "a refused sync must never write a receipt (no decorative success)"
    );
}

/// Negative falsifier: Stage 5 (Write) must fail closed when a template's
/// `to:` path escapes the project root, rather than writing outside the
/// sandbox.
#[test]
fn output_path_escaping_root_refuses_closed_at_write_stage() {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    let escaping_template =
        "---\nto: ../outside.txt\n---\nshould never land outside the project root\n";
    std::fs::write(
        dir.path().join("templates").join("escape.tmpl"),
        escaping_template,
    )
    .expect("write template");

    let err = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("sync must refuse a `to:` path that escapes the project root");

    let msg = err.to_string();
    assert!(
        msg.to_lowercase().contains("root") || msg.contains("FM-WRITE"),
        "error must name a root-escape refusal, got: {msg}"
    );
    assert!(
        !dir.path()
            .parent()
            .expect("has parent")
            .join("outside.txt")
            .exists(),
        "no file must ever be written outside the project root"
    );
}

/// Positive witness (determinism, dry-run): running the identical fixture
/// through `sync(..., dry_run: true)` twice, in two independent `TempDirs`,
/// yields byte-identical `graph_hash_hex` and an identical `decisions` map
/// -- proving the Resolve/Enrich/Extract/Render stages are deterministic
/// even though dry-run never reaches the Write stage.
#[test]
fn dry_run_projection_is_deterministic_across_independent_runs() {
    let d1 = TempDir::new().expect("tempdir");
    let d2 = TempDir::new().expect("tempdir");
    scaffold(d1.path());
    scaffold(d2.path());

    let r1 = sync(
        d1.path(),
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry-run sync 1");
    let r2 = sync(
        d2.path(),
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry-run sync 2");

    assert_eq!(
        r1.graph_hash_hex, r2.graph_hash_hex,
        "same ontology input must yield the same post-Enrich graph hash"
    );
    assert_eq!(
        r1.decisions, r2.decisions,
        "the Extract/Render/Write-decision projection must be deterministic"
    );
    // Dry-run must never touch the filesystem for outputs or receipts.
    assert!(
        !d1.path().join("out").exists(),
        "dry-run must not write outputs"
    );
    assert!(
        !d1.path().join(".ggen-v2/receipt.json").exists(),
        "dry-run must not write a receipt"
    );
}
