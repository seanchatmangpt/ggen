//! G2 — sync-hardening plan item 1: `[ontology.aggregate]` flag-gated
//! aggregate corpus regeneration.
//!
//! Golden tests over a real on-disk project through the public `sync()`
//! entry point (Chicago: real collaborators, real files, no mocks):
//!
//! 1. stale corpus + `auto_regen = true` → output rewritten as the
//!    deterministic sorted concat of constituents; running sync twice
//!    yields byte-identical output (determinism ×2);
//! 2. stale corpus + default (`auto_regen` absent) → typed refusal
//!    `[FM-ONTOLOGY-AGG-001]`, output bytes untouched;
//! 3. section absent → no-op: stale corpus untouched, sync succeeds;
//! 4. fresh corpus (marker matches) → no rewrite, sync succeeds.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};

const CONSTITUENT_A: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

const CONSTITUENT_B: &str = r#"
@prefix ex: <http://example.org/> .
ex:bob ex:name "bob" .
"#;

/// Stale corpus: the on-disk `corpus.ttl` does NOT match the concat of the
/// constituents (as if a constituent was edited after the last regen).
const STALE_CORPUS: &str = r#"
@prefix ex: <http://example.org/> .
ex:stale ex:name "stale" .
"#;

fn aggregate_section(auto_regen: Option<bool>) -> String {
    let flag = auto_regen
        .map(|b| format!("\nauto_regen = {b}"))
        .unwrap_or_default();
    format!(
        "[ontology.aggregate]\noutput = \"corpus.ttl\"\nconstituents = [\"parts/b.ttl\", \"parts/a.ttl\"]{flag}\n"
    )
}

fn scaffold(root: &Path, corpus: &str, aggregate_section: Option<&str>) {
    let manifest = format!(
        "[project]\nname = \"agg\"\n\n[ontology]\nsource = \"corpus.ttl\"\n\n[templates]\ndir = \"templates\"\n\n{}",
        aggregate_section.unwrap_or("")
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
    std::fs::create_dir_all(root.join("parts")).expect("mkdir parts");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("parts/a.ttl"), CONSTITUENT_A).expect("write a.ttl");
    std::fs::write(root.join("parts/b.ttl"), CONSTITUENT_B).expect("write b.ttl");
    std::fs::write(root.join("corpus.ttl"), corpus).expect("write corpus.ttl");
}

fn read(root: &Path, rel: &str) -> String {
    std::fs::read_to_string(root.join(rel)).expect(rel)
}

#[test]
fn stale_corpus_auto_regen_rewrites_deterministically() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    scaffold(root, STALE_CORPUS, Some(&aggregate_section(Some(true))));

    // Constituents deliberately listed out of order ([b, a]); the aggregate
    // must be the SORTED concat, byte-identical across two runs.
    let expected = format!("{CONSTITUENT_A}\n{CONSTITUENT_B}");

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("regen sync succeeds");
    assert_eq!(read(root, "corpus.ttl"), expected);
    let after_first = read(root, "corpus.ttl");

    // Marker written and reflects the freshness hash.
    let marker = read(root, ".ggen-v2/aggregate-freshness");
    assert!(!marker.trim().is_empty());

    // Second run: corpus is now fresh → no-op, byte-identical.
    let report2 = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("second sync succeeds");
    assert_eq!(read(root, "corpus.ttl"), after_first);
    assert_eq!(read(root, ".ggen-v2/aggregate-freshness"), marker);

    // Both syncs produce identical receipts (determinism ×2 at the receipt
    // level too — same inputs, same graph, same outputs).
    assert_eq!(
        serde_json::to_string(&report).expect("serialize report 1"),
        serde_json::to_string(&report2).expect("serialize report 2")
    );
}

#[test]
fn stale_corpus_default_refuses_typed_and_leaves_output_untouched() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    scaffold(root, STALE_CORPUS, Some(&aggregate_section(None)));

    let err = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect_err("must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("[FM-ONTOLOGY-AGG-001]"),
        "expected typed refusal, got: {msg}"
    );
    assert!(
        msg.contains("corpus.ttl"),
        "refusal must name the corpus: {msg}"
    );

    // Write-expansion fence: nothing was rewritten.
    assert_eq!(read(root, "corpus.ttl"), STALE_CORPUS);
    assert!(
        !root.join(".ggen-v2/aggregate-freshness").exists(),
        "marker must not be written on refusal"
    );
}

#[test]
fn absent_section_is_a_noop_on_stale_corpus() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    scaffold(root, STALE_CORPUS, None);

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("sync succeeds");
    assert_eq!(read(root, "corpus.ttl"), STALE_CORPUS);
    assert!(
        !root.join(".ggen-v2/aggregate-freshness").exists(),
        "no section → no marker"
    );
    assert!(report.written.is_empty(), "no templates → no writes");
}

#[test]
fn fresh_corpus_marker_match_skips_regen() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    // First sync regenerates and stamps the marker.
    scaffold(root, STALE_CORPUS, Some(&aggregate_section(Some(true))));
    sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("first sync");
    let corpus_after_regen = read(root, "corpus.ttl");

    // Second sync with the SAME constituents: fresh → nothing rewritten
    // (mtime/content unchanged), and no refusal.
    sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("second sync on fresh corpus");
    assert_eq!(read(root, "corpus.ttl"), corpus_after_regen);
}

#[test]
fn empty_constituents_is_a_typed_refusal() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    let manifest = "[project]\nname = \"agg\"\n\n[ontology]\nsource = \"corpus.ttl\"\n\n\
                   [templates]\ndir = \"templates\"\n\n\
                   [ontology.aggregate]\noutput = \"corpus.ttl\"\nconstituents = []\n";
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("corpus.ttl"), STALE_CORPUS).expect("write corpus.ttl");

    let err = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect_err("must refuse");
    assert!(
        err.to_string().contains("[FM-ONTOLOGY-AGG-002]"),
        "expected typed empty-constituents refusal, got: {err}"
    );
}
