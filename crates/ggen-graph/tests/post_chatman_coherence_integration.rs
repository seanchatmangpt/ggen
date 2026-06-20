//! Integration tests for the post-Chatman three-pole coherence checker.
//!
//! Tests use REAL data: real RDF triple strings, real (path, size) pairs, real OCEL JSON strings.
//! No mocks. State-based assertions on [`CoherenceReport`] output.
//!
//! Chicago TDD: assertions target observable state changes in the returned report,
//! not internal implementation details.
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::literal_string_with_formatting_args
)]

use ggen_graph::coherence::{CoherenceChecker, DriftKind, Pole};

// ─── full coherence: all poles present ────────────────────────────────────────

#[test]
fn test_three_pole_full_coherence_all_poles_present() {
    // Arrange: real ontology triples (N-Triples format)
    let triples = [
        "<https://example.org/ggen> <https://example.org/type> <https://example.org/Tool> .",
        "<https://example.org/ggen> <https://example.org/version> \"26.6.11\" .",
    ];

    // Arrange: real artifact inventory (path, byte_size)
    let artifacts = [
        ("crates/ggen-core/src/lib.rs", 4096u64),
        ("crates/ggen-graph/src/lib.rs", 2048u64),
    ];

    // Arrange: real OCEL event log entries (JSON strings)
    let events = [
        r#"{"ocel:activity":"sync","ocel:timestamp":"2026-06-20T00:00:00Z","ocel:type":"artifact"}"#,
        r#"{"ocel:activity":"validate","ocel:timestamp":"2026-06-20T00:01:00Z","ocel:type":"receipt"}"#,
    ];

    // Act
    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l = CoherenceChecker::fingerprint_event_log(&events);
    let report = CoherenceChecker::check(&[o, a, l]);

    // Assert: all poles present, no Missing drift
    assert_eq!(report.poles.len(), 3, "all three poles should be recorded");
    assert!(
        !report.drifts.iter().any(|d| d.kind == DriftKind::Missing),
        "no Missing drift expected when all poles supplied; got: {:?}",
        report.drifts
    );

    // Assert: ontology pole has non-empty hash and correct item count
    let o_pole = report
        .poles
        .iter()
        .find(|p| p.pole == Pole::Ontology)
        .expect("Ontology pole must be in report");
    assert!(
        !o_pole.hash.is_empty(),
        "ontology fingerprint must be non-empty"
    );
    assert_eq!(o_pole.item_count, 2, "two triples → item_count = 2");

    // Assert: artifact pole has correct item count
    let a_pole = report
        .poles
        .iter()
        .find(|p| p.pole == Pole::Artifact)
        .expect("Artifact pole must be in report");
    assert_eq!(a_pole.item_count, 2, "two artifacts → item_count = 2");

    // Assert: event-log pole has correct item count
    let l_pole = report
        .poles
        .iter()
        .find(|p| p.pole == Pole::EventLog)
        .expect("EventLog pole must be in report");
    assert_eq!(l_pole.item_count, 2, "two events → item_count = 2");

    // Assert: admitted because no drifts and all poles present
    assert!(
        report.admitted,
        "full coherence with non-empty poles should be admitted; drifts: {:?}",
        report.drifts
    );

    // Assert: operation_id is non-empty (real UUID, not sentinel)
    assert!(!report.operation_id.is_empty());
}

// ─── empty event log → CountDiscrepancy A→L ───────────────────────────────────

#[test]
fn test_coherence_empty_event_log_produces_count_discrepancy() {
    // Arrange: ontology and artifacts non-empty, event log empty
    let triples = ["<https://example.org/s> <https://example.org/p> <https://example.org/o> ."];
    let artifacts = [("crates/ggen-core/src/lib.rs", 1024u64)];

    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l = CoherenceChecker::fingerprint_event_log(&[]); // empty log

    // Act
    let report = CoherenceChecker::check(&[o, a, l]);

    // Assert: A→L CountDiscrepancy because artifacts exist but log is empty
    assert!(
        report
            .drifts
            .iter()
            .any(|d| d.kind == DriftKind::CountDiscrepancy
                && d.source_pole == Pole::Artifact
                && d.target_pole == Pole::EventLog),
        "expected A→L CountDiscrepancy; got drifts: {:?}",
        report.drifts
    );
    assert!(
        !report.admitted,
        "report must not be admitted when a CountDiscrepancy exists"
    );
}

// ─── fingerprint determinism ───────────────────────────────────────────────────

#[test]
fn test_coherence_fingerprints_are_deterministic() {
    let triples = [
        "<https://example.org/s1> <https://example.org/p1> <https://example.org/o1> .",
        "<https://example.org/s2> <https://example.org/p2> <https://example.org/o2> .",
    ];
    let artifacts = [("foo/bar.rs", 512u64)];

    // Same input called twice → same hash
    let o1 = CoherenceChecker::fingerprint_ontology(&triples);
    let o2 = CoherenceChecker::fingerprint_ontology(&triples);
    assert_eq!(o1.hash, o2.hash, "ontology fingerprint must be deterministic");
    assert_eq!(o1.item_count, 2);

    let a1 = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let a2 = CoherenceChecker::fingerprint_artifacts(&artifacts);
    assert_eq!(
        a1.hash, a2.hash,
        "artifact fingerprint must be deterministic"
    );

    let events = [r#"{"ocel:activity":"sync"}"#];
    let l1 = CoherenceChecker::fingerprint_event_log(&events);
    let l2 = CoherenceChecker::fingerprint_event_log(&events);
    assert_eq!(
        l1.hash, l2.hash,
        "event-log fingerprint must be deterministic"
    );
}

// ─── different inputs → different hashes ──────────────────────────────────────

#[test]
fn test_coherence_different_inputs_produce_different_hashes() {
    let o1 = CoherenceChecker::fingerprint_ontology(&[
        "<https://example.org/s1> <https://example.org/p1> <https://example.org/o1> .",
    ]);
    let o2 = CoherenceChecker::fingerprint_ontology(&[
        "<https://example.org/s2> <https://example.org/p2> <https://example.org/o2> .",
    ]);
    assert_ne!(
        o1.hash, o2.hash,
        "different triple sets must produce different hashes"
    );

    let a1 = CoherenceChecker::fingerprint_artifacts(&[("path/a.rs", 100u64)]);
    let a2 = CoherenceChecker::fingerprint_artifacts(&[("path/b.rs", 200u64)]);
    assert_ne!(
        a1.hash, a2.hash,
        "different artifact sets must produce different hashes"
    );
}

// ─── missing ontology pole → Missing drift, not admitted ──────────────────────

#[test]
fn test_coherence_missing_ontology_pole_not_admitted() {
    // Provide only A and L — O is absent
    let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 512u64)]);
    let l = CoherenceChecker::fingerprint_event_log(&[r#"{"ocel:activity":"sync"}"#]);
    let report = CoherenceChecker::check(&[a, l]);

    assert!(
        report
            .drifts
            .iter()
            .any(|d| d.kind == DriftKind::Missing && d.source_pole == Pole::Ontology),
        "expected Missing drift for Ontology; got: {:?}",
        report.drifts
    );
    assert!(!report.admitted);
}

// ─── operation_id uniqueness across calls ─────────────────────────────────────

#[test]
fn test_coherence_report_operation_ids_are_unique_across_calls() {
    let triples = ["<s> <p> <o> ."];
    let artifacts = [("src/lib.rs", 1u64)];
    let events = [r#"{"ocel:activity":"sync"}"#];

    let o1 = CoherenceChecker::fingerprint_ontology(&triples);
    let a1 = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l1 = CoherenceChecker::fingerprint_event_log(&events);
    let r1 = CoherenceChecker::check(&[o1, a1, l1]);

    let o2 = CoherenceChecker::fingerprint_ontology(&triples);
    let a2 = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l2 = CoherenceChecker::fingerprint_event_log(&events);
    let r2 = CoherenceChecker::check(&[o2, a2, l2]);

    assert_ne!(
        r1.operation_id, r2.operation_id,
        "each coherence check run must produce a unique operation_id"
    );
    // Both must be admitted (same valid data, all poles present, no count discrepancy)
    assert!(r1.admitted, "first report should be admitted");
    assert!(r2.admitted, "second report should be admitted");
}

// ─── ontology with triples but zero artifacts → O→A CountDiscrepancy ──────────

#[test]
fn test_coherence_zero_artifacts_with_triples_emits_o_to_a_discrepancy() {
    let triples = ["<s> <p> <o> ."];
    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&[]); // no artifacts
    let l = CoherenceChecker::fingerprint_event_log(&[]);

    let report = CoherenceChecker::check(&[o, a, l]);

    assert!(
        report
            .drifts
            .iter()
            .any(|d| d.kind == DriftKind::CountDiscrepancy
                && d.source_pole == Pole::Ontology
                && d.target_pole == Pole::Artifact),
        "expected O→A CountDiscrepancy; got: {:?}",
        report.drifts
    );
    assert!(!report.admitted);
}

// ─── large realistic ontology snapshot ────────────────────────────────────────

#[test]
fn test_coherence_realistic_multi_triple_ontology() {
    // Simulate a realistic ggen ontology snapshot with multiple subjects/predicates.
    let triples = [
        "<https://ggen.dev/ont/GgenPipeline> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://ggen.dev/ont/Pipeline> .",
        "<https://ggen.dev/ont/GgenPipeline> <https://ggen.dev/ont/stage> <https://ggen.dev/ont/mu1_load> .",
        "<https://ggen.dev/ont/GgenPipeline> <https://ggen.dev/ont/stage> <https://ggen.dev/ont/mu2_extract> .",
        "<https://ggen.dev/ont/GgenPipeline> <https://ggen.dev/ont/stage> <https://ggen.dev/ont/mu3_generate> .",
        "<https://ggen.dev/ont/GgenPipeline> <https://ggen.dev/ont/stage> <https://ggen.dev/ont/mu4_validate> .",
        "<https://ggen.dev/ont/GgenPipeline> <https://ggen.dev/ont/stage> <https://ggen.dev/ont/mu5_emit> .",
    ];
    let artifacts = [
        ("crates/ggen-core/src/lib.rs", 8192u64),
        ("crates/ggen-core/src/pipeline.rs", 16384u64),
        ("crates/ggen-cli/src/main.rs", 4096u64),
        ("crates/ggen-graph/src/lib.rs", 12288u64),
        ("crates/ggen-config/src/lib.rs", 2048u64),
        ("crates/genesis-types-v2/src/lib.rs", 20480u64),
    ];
    let events = [
        r#"{"ocel:activity":"mu1_load","ocel:timestamp":"2026-06-20T10:00:00Z","ocel:object-type":"artifact"}"#,
        r#"{"ocel:activity":"mu2_extract","ocel:timestamp":"2026-06-20T10:00:01Z","ocel:object-type":"artifact"}"#,
        r#"{"ocel:activity":"mu3_generate","ocel:timestamp":"2026-06-20T10:00:02Z","ocel:object-type":"artifact"}"#,
        r#"{"ocel:activity":"mu4_validate","ocel:timestamp":"2026-06-20T10:00:03Z","ocel:object-type":"receipt"}"#,
        r#"{"ocel:activity":"mu5_emit","ocel:timestamp":"2026-06-20T10:00:04Z","ocel:object-type":"receipt"}"#,
        r#"{"ocel:activity":"sync_complete","ocel:timestamp":"2026-06-20T10:00:05Z","ocel:object-type":"receipt"}"#,
    ];

    let o = CoherenceChecker::fingerprint_ontology(&triples);
    let a = CoherenceChecker::fingerprint_artifacts(&artifacts);
    let l = CoherenceChecker::fingerprint_event_log(&events);
    let report = CoherenceChecker::check(&[o, a, l]);

    // All poles present and non-empty with matching counts
    assert_eq!(
        report.poles.iter().find(|p| p.pole == Pole::Ontology).map(|p| p.item_count),
        Some(6),
        "six triples"
    );
    assert_eq!(
        report.poles.iter().find(|p| p.pole == Pole::Artifact).map(|p| p.item_count),
        Some(6),
        "six artifacts"
    );
    assert_eq!(
        report.poles.iter().find(|p| p.pole == Pole::EventLog).map(|p| p.item_count),
        Some(6),
        "six events"
    );
    assert!(report.admitted, "realistic multi-item coherence must be admitted");
    assert!(report.drifts.is_empty(), "no drifts expected; got: {:?}", report.drifts);
}
