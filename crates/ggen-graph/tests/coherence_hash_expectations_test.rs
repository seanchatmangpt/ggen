//! Phase 2 Task A6: Hash expectation validation in CoherenceChecker.
//!
//! Tests validate that CoherenceChecker can compare freshly computed pole hashes
//! against previously recorded expectations (e.g., from prior receipts) and emit
//! HashMismatch drifts when poles change.
//!
//! Chicago TDD: Real PoleStates with computed hashes, real drift emission,
//! state-based assertions on CoherenceReport output.
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::literal_string_with_formatting_args
)]

use ggen_graph::coherence::{CoherenceChecker, CoherenceDrift, DriftKind, Pole};
use std::collections::HashMap;

// ─── Hash expectation validation: single pole mismatch ──────────────────────────

#[test]
fn test_ontology_hash_mismatch_against_expectation() {
    // Arrange: real ontology data — compute two different fingerprints
    let old_triples = ["<s> <p> <o> ."];
    let new_triples = ["<s> <p> <o> .", "<s2> <p2> <o2> ."];

    // Compute the old hash (what we expect to see)
    let expected_o_hash = CoherenceChecker::fingerprint_ontology(&old_triples).hash;

    // Compute fresh hashes with NEW ontology data (has changed)
    let o_fresh = CoherenceChecker::fingerprint_ontology(&new_triples);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 512)]);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Verify precondition: ontology hash has actually changed
    assert_ne!(
        o_fresh.hash, expected_o_hash,
        "test setup: expected and fresh ontology hashes must differ"
    );

    // Act: check with expectations (expected_o_hash from a prior run)
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, expected_o_hash.clone());

    let report = CoherenceChecker::check_with_expectations(
        &[o_fresh.clone(), a_fresh, l_fresh],
        &expectations,
    );

    // Assert: a HashMismatch drift is emitted for Ontology pole
    let hash_mismatch_drifts: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch && d.source_pole == Pole::Ontology)
        .collect();

    assert!(
        !hash_mismatch_drifts.is_empty(),
        "expected a HashMismatch drift for Ontology pole; got drifts: {:?}",
        report.drifts
    );

    // Assert: the detail mentions both declared and computed hashes
    let drift = hash_mismatch_drifts[0];
    assert!(
        drift.detail.contains(&expected_o_hash) && drift.detail.contains(&o_fresh.hash),
        "drift detail must cite both declared and computed hashes: {}",
        drift.detail
    );

    // Assert: report is not admitted (HashMismatch blocks admission)
    assert!(
        !report.admitted,
        "a HashMismatch drift must force admitted = false"
    );
}

#[test]
fn test_artifact_hash_mismatch_against_expectation() {
    // Arrange: real artifact data — compute two different fingerprints
    let old_artifacts = [("src/lib.rs", 256u64)];
    let new_artifacts = [("src/lib.rs", 256u64), ("src/main.rs", 512u64)];

    // Compute the old hash (what we expect to see)
    let expected_a_hash = CoherenceChecker::fingerprint_artifacts(&old_artifacts).hash;

    // Compute fresh hashes with NEW artifact data (has changed)
    let o_fresh = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&new_artifacts);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Verify precondition
    assert_ne!(
        a_fresh.hash, expected_a_hash,
        "test setup: expected and fresh artifact hashes must differ"
    );

    // Act
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Artifact, expected_a_hash.clone());

    let report = CoherenceChecker::check_with_expectations(
        &[o_fresh, a_fresh.clone(), l_fresh],
        &expectations,
    );

    // Assert: a HashMismatch drift is emitted for Artifact pole
    let hash_mismatch_drifts: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch && d.source_pole == Pole::Artifact)
        .collect();

    assert!(
        !hash_mismatch_drifts.is_empty(),
        "expected a HashMismatch drift for Artifact pole; got drifts: {:?}",
        report.drifts
    );

    assert!(!report.admitted);
}

#[test]
fn test_event_log_hash_mismatch_against_expectation() {
    // Arrange: real event log data
    let old_events = [r#"{"activity":"sync"}"#];
    let new_events = [r#"{"activity":"sync"}"#, r#"{"activity":"validate"}"#];

    // Compute the old hash
    let expected_l_hash = CoherenceChecker::fingerprint_event_log(&old_events).hash;

    // Compute fresh hashes with NEW event log data
    let o_fresh = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 128)]);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&new_events);

    // Verify precondition
    assert_ne!(
        l_fresh.hash, expected_l_hash,
        "test setup: expected and fresh event log hashes must differ"
    );

    // Act
    let mut expectations = HashMap::new();
    expectations.insert(Pole::EventLog, expected_l_hash.clone());

    let report = CoherenceChecker::check_with_expectations(
        &[o_fresh, a_fresh, l_fresh.clone()],
        &expectations,
    );

    // Assert: a HashMismatch drift is emitted for EventLog pole
    let hash_mismatch_drifts: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch && d.source_pole == Pole::EventLog)
        .collect();

    assert!(
        !hash_mismatch_drifts.is_empty(),
        "expected a HashMismatch drift for EventLog pole; got drifts: {:?}",
        report.drifts
    );

    assert!(!report.admitted);
}

// ─── Cross-pole coherence fracture: Rule 6 (one pole stable, one drifted) ────────

#[test]
fn test_rule_6_cross_pole_coherence_fracture() {
    // Arrange: O matches expectation but L drifted (or vice versa)
    let triples = ["<s> <p> <o> ."];
    let old_events = [r#"{"activity":"sync"}"#];
    let new_events = [r#"{"activity":"sync"}"#, r#"{"activity":"validate"}"#];

    // Compute the expected hashes for both O and L
    let expected_o = CoherenceChecker::fingerprint_ontology(&triples).hash;
    let expected_l = CoherenceChecker::fingerprint_event_log(&old_events).hash;

    // Compute fresh: O unchanged (matches expected), L changed (differs from expected)
    let o_fresh = CoherenceChecker::fingerprint_ontology(&triples);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 256)]);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&new_events);

    // Verify preconditions
    assert_eq!(
        o_fresh.hash, expected_o,
        "test setup: O should match expectation"
    );
    assert_ne!(
        l_fresh.hash, expected_l,
        "test setup: L should differ from expectation"
    );

    // Act: check with expectations for both O and L
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, expected_o);
    expectations.insert(Pole::EventLog, expected_l);

    let report =
        CoherenceChecker::check_with_expectations(&[o_fresh, a_fresh, l_fresh], &expectations);

    // Assert: Rule 6 emits a cross-pole fracture drift (O stable, L drifted)
    let cross_pole_drifts: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| {
            d.kind == DriftKind::HashMismatch
                && d.source_pole == Pole::Ontology
                && d.target_pole == Pole::EventLog
        })
        .collect();

    assert!(
        !cross_pole_drifts.is_empty(),
        "expected a cross-pole coherence fracture drift; got drifts: {:?}",
        report.drifts
    );

    // Assert: the detail describes the fracture
    assert!(
        cross_pole_drifts[0].detail.contains("stable")
            && cross_pole_drifts[0].detail.contains("drifted"),
        "drift detail should explain the fracture"
    );

    assert!(!report.admitted, "coherence fracture must block admission");
}

// ─── Multiple poles with mismatches (Rule 6 in effect) ────────────────────────

#[test]
fn test_multiple_poles_with_hash_mismatches() {
    // Arrange: both O and A have changed since last run
    let old_triples = ["<s> <p> <o> ."];
    let new_triples = ["<new> <new> <new> ."];

    let old_artifacts = [("old.rs", 1u64)];
    let new_artifacts = [("new.rs", 2u64)];

    let expected_o = CoherenceChecker::fingerprint_ontology(&old_triples).hash;
    let expected_a = CoherenceChecker::fingerprint_artifacts(&old_artifacts).hash;

    // Compute fresh with new data
    let o_fresh = CoherenceChecker::fingerprint_ontology(&new_triples);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&new_artifacts);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Act
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, expected_o.clone());
    expectations.insert(Pole::Artifact, expected_a.clone());

    let report = CoherenceChecker::check_with_expectations(
        &[o_fresh.clone(), a_fresh.clone(), l_fresh],
        &expectations,
    );

    // Assert: multiple self-comparison HashMismatch drifts (Rule 5) for O and A
    let self_comparison_mismatches: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch && d.source_pole == d.target_pole)
        .collect();

    assert_eq!(
        self_comparison_mismatches.len(),
        2,
        "expected exactly 2 self-comparison HashMismatch drifts (O and A); got: {:?}",
        report.drifts
    );

    // Assert: drifts cover both poles
    let o_drifts = self_comparison_mismatches
        .iter()
        .filter(|d| d.source_pole == Pole::Ontology)
        .count();
    let a_drifts = self_comparison_mismatches
        .iter()
        .filter(|d| d.source_pole == Pole::Artifact)
        .count();

    assert_eq!(o_drifts, 1, "exactly one drift should be for Ontology pole");
    assert_eq!(a_drifts, 1, "exactly one drift should be for Artifact pole");

    // Assert: admitted is false (multiple mismatches block admission)
    assert!(
        !report.admitted,
        "multiple mismatches must force admitted = false"
    );
}

// ─── Backward compatibility: empty expectations → no hash drifts ──────────────

#[test]
fn test_empty_expectations_produces_no_hash_drifts() {
    // Arrange: real poles with content
    let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
    let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 256)]);
    let l = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Act: check with empty expectations (no prior hashes to compare against)
    let expectations = HashMap::new();
    let report = CoherenceChecker::check_with_expectations(&[o, a, l], &expectations);

    // Assert: no HashMismatch drifts (backward compatible with old behavior)
    let hash_mismatches = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch)
        .count();

    assert_eq!(
        hash_mismatches, 0,
        "empty expectations should produce no HashMismatch drifts"
    );

    // Assert: report is admitted (three poles, no count discrepancies)
    assert!(
        report.admitted,
        "empty expectations should allow admission if no other drift"
    );
}

// ─── Matching expectations produce no drift ────────────────────────────────────

#[test]
fn test_matching_expectations_no_hash_drift() {
    // Arrange: compute hashes, then check against themselves
    let triples = ["<s> <p> <o> .", "<s2> <p2> <o2> ."];
    let artifacts = [("src/lib.rs", 512), ("src/main.rs", 256)];
    let events = [r#"{"activity":"sync"}"#, r#"{"activity":"validate"}"#];

    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l = CoherenceChecker::fingerprint_event_log(&events);

    // Act: expectations match the fresh hashes exactly
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, o.hash.clone());
    expectations.insert(Pole::Artifact, a.hash.clone());
    expectations.insert(Pole::EventLog, l.hash.clone());

    let report = CoherenceChecker::check_with_expectations(
        &[o.clone(), a.clone(), l.clone()],
        &expectations,
    );

    // Assert: no HashMismatch drifts
    let hash_mismatches = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch)
        .count();

    assert_eq!(
        hash_mismatches, 0,
        "matching hashes must not emit HashMismatch drifts"
    );

    // Assert: fully admitted (three poles, matching hashes, no count issues)
    assert!(
        report.admitted,
        "matching expectations with all poles should be admitted"
    );
}

// ─── Sabotage test: partial expectations with mismatch on one pole ────────────

#[test]
fn test_partial_expectations_only_checks_declared_poles() {
    // Arrange: declare expectations for only O and A, not L
    let old_triples = ["<old> <old> <old> ."];
    let new_triples = ["<new> <new> <new> ."];

    let expected_o = CoherenceChecker::fingerprint_ontology(&old_triples).hash;

    let o_fresh = CoherenceChecker::fingerprint_ontology(&new_triples);
    let a_fresh = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 128)]);
    let l_fresh = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Act: expectations only for O (not A, not L)
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, expected_o.clone());

    let report =
        CoherenceChecker::check_with_expectations(&[o_fresh, a_fresh, l_fresh], &expectations);

    // Assert: only one HashMismatch (for O), not for A or L
    let hash_mismatches: Vec<&CoherenceDrift> = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch)
        .collect();

    assert_eq!(
        hash_mismatches.len(),
        1,
        "only declared poles should be checked for hash mismatch"
    );
    assert_eq!(
        hash_mismatches[0].source_pole,
        Pole::Ontology,
        "the single mismatch should be for Ontology"
    );
}

// ─── Regression: count discrepancies still work with expectations ──────────────

#[test]
fn test_expectations_do_not_suppress_count_discrepancies() {
    // Arrange: O has content but A is empty (count discrepancy)
    let triples = ["<s> <p> <o> ."];
    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&[]); // empty
    let l = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

    // Act: provide hash expectations (matching) but still have count mismatch
    let mut expectations = HashMap::new();
    expectations.insert(Pole::Ontology, o.hash.clone());
    expectations.insert(Pole::Artifact, a.hash.clone());
    expectations.insert(Pole::EventLog, l.hash.clone());

    let report = CoherenceChecker::check_with_expectations(&[o, a, l], &expectations);

    // Assert: CountDiscrepancy drifts are still detected (O→A)
    let count_drifts = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::CountDiscrepancy)
        .count();

    assert!(
        count_drifts > 0,
        "count discrepancies must be detected even with matching hash expectations"
    );

    // Assert: no HashMismatch (expectations matched)
    let hash_drifts = report
        .drifts
        .iter()
        .filter(|d| d.kind == DriftKind::HashMismatch)
        .count();

    assert_eq!(
        hash_drifts, 0,
        "matching expectations must not emit HashMismatch"
    );

    // Assert: not admitted (count discrepancy present)
    assert!(!report.admitted);
}
