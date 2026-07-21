//! Chicago-TDD proofs for L5 publication conditions 14/15 live wiring: a
//! fresh (genesis) project's real `sync` receipts carry a COMPUTED
//! `standing_ceiling` (never the `LegacyObserved` sentinel) and an
//! equivalence map whose `source`/`config` classes are genuinely evaluated
//! against the previous receipt's input closure, and whose `tests`/`gates`
//! classes (plus non-uniform per-axis `ComponentLevels`) are evaluated from
//! admitted external `ver:Check` evidence (the ggen-verify-pack lane) when
//! present — `Unknown` only where there is honestly nothing to compare
//! (genesis, or no evidence for the class). Real syncs on a real
//! filesystem, no mocks.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_LOG_REL_PATH};
use praxis_core::receipt_epoch::{
    AdmissionLedger, AndonLevel, CeilingLevel, EquivalenceStatus, ObservedOutcome,
};
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

/// Rewrite the project ontology with domain individuals PLUS admitted
/// `ver:` evidence facts (the ggen-verify-pack lane): one `ver:Check` per
/// `(name, exit_code)` pair and one `ver:KnownDivergence` per allowlisted
/// name. Same single-file lane the real emitter uses -- the facts enter via
/// the union graph, never via any side channel.
fn write_ontology_with_evidence(
    root: &Path, names: &[&str], checks: &[(&str, i64)], known_divergences: &[&str],
) {
    let mut ttl = String::from(
        "@prefix ex: <http://example.org/> .\n@prefix ver: <http://seanchatmangpt.github.io/packs/ggen-verify#> .\n",
    );
    for name in names {
        ttl.push_str(&format!("ex:{name} ex:name \"{name}\" .\n"));
    }
    for (i, (name, exit)) in checks.iter().enumerate() {
        ttl.push_str(&format!(
            "ver:c{i} a ver:Check ; ver:name \"{name}\" ; ver:exitCode {exit} .\n"
        ));
    }
    for (i, name) in known_divergences.iter().enumerate() {
        ttl.push_str(&format!(
            "ver:kd{i} a ver:KnownDivergence ; ver:name \"{name}\" .\n"
        ));
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

fn ver_admission_outcome(receipt: &SyncReceipt, evidence_id: &str) -> ObservedOutcome {
    let epoch = receipt.record.v2.as_ref().expect("v2 epoch");
    let AdmissionLedger::Recorded(items) = &epoch.admission else {
        panic!("live receipt must carry a Recorded ledger");
    };
    items
        .iter()
        .find(|i| i.evidence_id == evidence_id)
        .unwrap_or_else(|| panic!("admission item `{evidence_id}` missing"))
        .observed_outcome
        .clone()
}

/// Evidence consumption, green path: admitted `ver:Check` facts become
/// recorded admission items on the first receipt (tests/gates still
/// honestly `Unknown` -- the genesis receipt has no predecessor to compare
/// outcomes against), and on the second sync the `tests` and `gates`
/// equivalence classes are genuinely EVALUATED (`Equivalent`) instead of
/// staying at the Unknown floor. Ceiling stays a computed Green (all
/// evidence green, meet is the identity there).
#[test]
fn verify_evidence_makes_tests_and_gates_classes_evaluable() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    write_ontology_with_evidence(
        dir.path(),
        &["alice"],
        &[("test-workspace", 0), ("build", 0)],
        &[],
    );
    run_sync(dir.path());
    run_sync(dir.path());

    let log = read_log(dir.path());
    assert_eq!(log.len(), 2);

    // Receipt 1: evidence recorded, but no predecessor to compare against.
    assert_eq!(
        ver_admission_outcome(&log[0], "ver:test-workspace"),
        ObservedOutcome::Pass
    );
    assert_eq!(
        ver_admission_outcome(&log[0], "ver:build"),
        ObservedOutcome::Pass
    );
    let eq1 = &log[0].record.v2.as_ref().expect("v2").equivalence;
    assert_eq!(eq1.tests, EquivalenceStatus::Unknown);
    assert_eq!(eq1.gates, EquivalenceStatus::Unknown);

    // Receipt 2: both classes evaluated against receipt 1's recorded items.
    let eq2 = &log[1].record.v2.as_ref().expect("v2").equivalence;
    assert_eq!(eq2.tests, EquivalenceStatus::Equivalent);
    assert_eq!(eq2.gates, EquivalenceStatus::Equivalent);

    for receipt in &log {
        let epoch = receipt.record.v2.as_ref().expect("v2");
        assert_eq!(epoch.standing_ceiling, CeilingLevel::Green);
        assert_eq!(epoch.andon, AndonLevel::Green);
    }
}

/// Sabotage (meet law): a red `ver:Check` on the test axis lowers the
/// standing ceiling to Red -- the four axes are now genuinely non-uniform
/// (test Red from evidence, the others Green from clean admission) and the
/// recorded ceiling is the weakest-of-four meet, not the admission-derived
/// Green. The `tests` equivalence class simultaneously reports `Divergent`
/// naming the flipped check, while `gates` (whose `build` check is
/// unchanged) stays `Equivalent`. A third sync proves ceiling monotonicity:
/// once Red, the meet with the Red prior keeps it Red.
#[test]
fn red_test_check_lowers_ceiling_and_diverges_tests_class() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    write_ontology_with_evidence(
        dir.path(),
        &["alice"],
        &[("test-workspace", 0), ("build", 0)],
        &[],
    );
    run_sync(dir.path());
    // Sabotage: the externally-run test check goes red.
    write_ontology_with_evidence(
        dir.path(),
        &["alice"],
        &[("test-workspace", 1), ("build", 0)],
        &[],
    );
    run_sync(dir.path());
    run_sync(dir.path());

    let log = read_log(dir.path());
    assert_eq!(log.len(), 3);

    let epoch2 = log[1].record.v2.as_ref().expect("v2");
    assert_eq!(
        epoch2.standing_ceiling,
        CeilingLevel::Red,
        "a red test check must lower the ceiling via the meet law"
    );
    // The red check was ADMITTED (recorded evidence, not a refusal this
    // sync detected), so the derived andon stays Green while the ceiling
    // drops -- the two signals are independent by design.
    assert_eq!(epoch2.andon, AndonLevel::Green);
    assert_eq!(
        ver_admission_outcome(&log[1], "ver:test-workspace"),
        ObservedOutcome::Fail
    );
    match &epoch2.equivalence.tests {
        EquivalenceStatus::Divergent(reason) => assert!(
            reason.contains("test-workspace"),
            "divergence reason must name the flipped check, got: {reason}"
        ),
        other => panic!("flipped test check must make tests Divergent, got {other:?}"),
    }
    assert_eq!(epoch2.equivalence.gates, EquivalenceStatus::Equivalent);

    // Monotonicity: identical re-sync cannot climb back above the Red prior.
    let epoch3 = log[2].record.v2.as_ref().expect("v2");
    assert_eq!(epoch3.standing_ceiling, CeilingLevel::Red);
}

/// Allowlist semantics (mirrors the pack's own `020_evidence_green.rq`
/// gate): a red check whose name is an admitted `ver:KnownDivergence` is
/// recorded as `Fail` honestly but does NOT lower its component axis, so
/// the ceiling stays a computed Green.
#[test]
fn allowlisted_red_check_is_recorded_fail_but_does_not_lower_ceiling() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    write_ontology_with_evidence(dir.path(), &["alice"], &[("fmt-check", 1)], &["fmt-check"]);
    run_sync(dir.path());

    let log = read_log(dir.path());
    let epoch = log[0].record.v2.as_ref().expect("v2");
    assert_eq!(
        ver_admission_outcome(&log[0], "ver:fmt-check"),
        ObservedOutcome::Fail,
        "the outcome is never laundered to Pass by the allowlist"
    );
    assert_eq!(epoch.standing_ceiling, CeilingLevel::Green);
}
