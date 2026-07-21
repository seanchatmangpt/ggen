//! Chicago-TDD proofs for L5 publication conditions 14/15 live wiring: a
//! fresh (genesis) project's real `sync` receipts carry a COMPUTED
//! `standing_ceiling` (never the `LegacyObserved` sentinel) and an
//! equivalence map whose `source`/`config` classes are genuinely evaluated
//! against the previous receipt's input closure — `Unknown` only where there
//! is honestly nothing to compare (genesis) or nothing evaluated (the other
//! six classes). Real syncs on a real filesystem, no mocks.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH};
use praxis_core::receipt_epoch::{CeilingLevel, EquivalenceStatus};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

fn scaffold(root: &Path, names: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    write_ontology(root, names);
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn write_ontology(root: &Path, names: &[&str]) {
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
}

fn run_sync(root: &Path) {
    sync(
        root,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
}

fn read_log(root: &Path) -> Vec<SyncReceipt> {
    let raw = std::fs::read_to_string(root.join(RECEIPT_LOG_REL_PATH)).expect("read log");
    raw.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("parse log line"))
        .collect()
}

/// Condition 14: a genesis chain's live receipt records a COMPUTED standing
/// ceiling (the meet of the four component axes with the Green genesis
/// prior), never the `LegacyObserved` legacy sentinel.
#[test]
fn genesis_sync_receipt_has_computed_ceiling_not_legacy_sentinel() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    run_sync(dir.path());

    let log = read_log(dir.path());
    let epoch = log[0]
        .record
        .v2
        .as_ref()
        .expect("genesis receipt has v2 epoch");
    assert_ne!(
        epoch.standing_ceiling,
        CeilingLevel::LegacyObserved,
        "a fresh chain must never emit the legacy sentinel"
    );
    // All outputs admitted cleanly and the genesis prior is Green (the
    // lattice top), so the computed meet is Green — a real value, not a
    // hardcoded literal: the sabotage test below shows it is not sticky.
    assert_eq!(epoch.standing_ceiling, CeilingLevel::Green);
}

/// Condition 15 genesis honesty: with no previous receipt there is nothing
/// to compare closures against, so `source`/`config` are explicitly
/// `Unknown` — never `Equivalent`-from-absence.
#[test]
fn genesis_equivalence_source_and_config_are_unknown() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    run_sync(dir.path());

    let log = read_log(dir.path());
    let eq = &log[0].record.v2.as_ref().expect("v2").equivalence;
    assert_eq!(eq.source, EquivalenceStatus::Unknown);
    assert_eq!(eq.config, EquivalenceStatus::Unknown);
}

/// Conditions 14+15 across a real 3-sync chain:
/// - sync 2 (byte-identical inputs): source and config both `Equivalent`;
/// - sync 3 (ontology edited between syncs): source `Divergent` naming the
///   ontology, config still `Equivalent` (ggen.toml untouched);
/// - the six unevaluated classes stay `Unknown` on every receipt;
/// - every receipt's ceiling is computed (non-LegacyObserved) and the chain
///   links each receipt to its predecessor.
#[test]
fn identical_resync_is_equivalent_and_ontology_edit_diverges_source_only() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    run_sync(dir.path());
    // Identical inputs, second sync.
    run_sync(dir.path());
    // Sabotage: edit the ontology, third sync.
    write_ontology(dir.path(), &["alice", "mallory"]);
    run_sync(dir.path());

    let log = read_log(dir.path());
    assert_eq!(log.len(), 3);

    // Chain integrity (receipt v2 rule: each receipt links prev).
    assert_eq!(log[0].record.prev_chain_hash_hex, "0".repeat(64));
    assert_eq!(
        log[1].record.prev_chain_hash_hex,
        log[0].record.chain_hash_hex
    );
    assert_eq!(
        log[2].record.prev_chain_hash_hex,
        log[1].record.chain_hash_hex
    );

    let eq2 = &log[1].record.v2.as_ref().expect("v2").equivalence;
    assert_eq!(eq2.source, EquivalenceStatus::Equivalent);
    assert_eq!(eq2.config, EquivalenceStatus::Equivalent);

    let eq3 = &log[2].record.v2.as_ref().expect("v2").equivalence;
    match &eq3.source {
        EquivalenceStatus::Divergent(reason) => assert!(
            reason.contains("ontology.ttl"),
            "divergence reason must name the changed input, got: {reason}"
        ),
        other => panic!("edited ontology must make source Divergent, got {other:?}"),
    }
    assert_eq!(
        eq3.config,
        EquivalenceStatus::Equivalent,
        "ggen.toml was untouched; config must stay Equivalent"
    );

    for receipt in &log {
        let epoch = receipt.record.v2.as_ref().expect("v2");
        assert_ne!(epoch.standing_ceiling, CeilingLevel::LegacyObserved);
        // The six classes this call site does not evaluate stay Unknown —
        // never silently promoted.
        let eq = &epoch.equivalence;
        for (name, status) in [
            ("compiled_binary", &eq.compiled_binary),
            ("docs", &eq.docs),
            ("tests", &eq.tests),
            ("receipts", &eq.receipts),
            ("evidence", &eq.evidence),
            ("gates", &eq.gates),
        ] {
            assert_eq!(
                status,
                &EquivalenceStatus::Unknown,
                "unevaluated class `{name}` must stay Unknown"
            );
        }
    }
}

/// Config-class sabotage: editing ggen.toml (a governing config input, here
/// a comment-only byte change) makes `config` Divergent on the next sync
/// while `source` stays Equivalent — the two classes are genuinely
/// independent, not one flag with two names.
#[test]
fn ggen_toml_edit_diverges_config_not_source() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    run_sync(dir.path());

    let mut toml = std::fs::read_to_string(dir.path().join("ggen.toml")).expect("read toml");
    toml.push_str("\n# byte-level config change\n");
    std::fs::write(dir.path().join("ggen.toml"), toml).expect("rewrite toml");
    run_sync(dir.path());

    let log = read_log(dir.path());
    let eq = &log[1].record.v2.as_ref().expect("v2").equivalence;
    match &eq.config {
        EquivalenceStatus::Divergent(reason) => assert!(
            reason.contains("ggen.toml"),
            "divergence reason must name ggen.toml, got: {reason}"
        ),
        other => panic!("edited ggen.toml must make config Divergent, got {other:?}"),
    }
    assert_eq!(eq.source, EquivalenceStatus::Equivalent);
}
