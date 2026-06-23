//! Three-pole coherence — O ≅ A ≅ L isomorphism checker.
//!
//! Computes BLAKE3 fingerprints of the ontology pole (O = RDF triples),
//! artifact pole (A = file paths + sizes), and event-log pole (L = OCEL events),
//! then reports drift between poles.
//!
//! # Thesis
//!
//! The post-Chatman equation states A ≅ O ≅ L — artifact (A), ontology (O), and event log (L)
//! are isomorphic representations of one canonical formal object. The [`CoherenceChecker`] makes
//! this auditable: it computes a BLAKE3 fingerprint of each pole and reports drift.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Identifies which of the three canonical poles a [`PoleState`] represents.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Pole {
    /// O — RDF triplestore (ontology source of truth).
    Ontology,
    /// A — generated code/assets (artifact pole).
    Artifact,
    /// L — OCEL process evidence (event-log pole).
    EventLog,
}

/// Snapshot of a single pole's fingerprint and item count.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PoleState {
    /// Which pole this state represents.
    pub pole: Pole,
    /// Hex-encoded BLAKE3 fingerprint of the pole's contents.
    pub hash: String,
    /// Number of items ingested (triples / file count / event count).
    pub item_count: usize,
    /// Timestamp when the fingerprint was computed.
    pub timestamp: DateTime<Utc>,
}

/// Classifies the kind of drift detected between poles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DriftKind {
    /// The same pole produced a different hash at two points in time.
    HashMismatch,
    /// Item counts diverge unexpectedly between poles (e.g., O has triples but A has 0 files).
    CountDiscrepancy,
    /// A required pole has no state recorded in the report.
    Missing,
}

/// A single drift observation between two poles.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoherenceDrift {
    /// What kind of divergence was detected.
    pub kind: DriftKind,
    /// The pole that is the subject (or the missing pole).
    pub source_pole: Pole,
    /// The pole that is the comparison target.
    pub target_pole: Pole,
    /// Human-readable detail string describing the drift.
    pub detail: String,
}

/// Summary produced by [`CoherenceChecker::check`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoherenceReport {
    /// UUID v4 identifying this coherence check run.
    pub operation_id: String,
    /// Pole states provided to the check (0–3 entries).
    pub poles: Vec<PoleState>,
    /// All drift observations detected. Empty iff the system is coherent.
    pub drifts: Vec<CoherenceDrift>,
    /// `true` iff all three poles are present and no drift was detected.
    pub admitted: bool,
}

/// Three-pole coherence checker implementing the A ≅ O ≅ L isomorphism audit.
pub struct CoherenceChecker;

impl CoherenceChecker {
    /// Fingerprint the ontology pole from a slice of serialized RDF triples (N-Triples lines).
    ///
    /// Each element of `triples` is treated as one canonical item. The BLAKE3 hash
    /// is computed over the byte representation of each triple string in order.
    pub fn fingerprint_ontology(triples: &[&str]) -> PoleState {
        let (hash, item_count) = Self::blake3_of_strings(triples);
        PoleState {
            pole: Pole::Ontology,
            hash,
            item_count,
            timestamp: Utc::now(),
        }
    }

    /// Fingerprint the artifact pole from a list of `(path, byte_length)` pairs.
    ///
    /// Both the path string and the little-endian byte encoding of the length are
    /// fed into the hasher for each artifact.
    pub fn fingerprint_artifacts(artifacts: &[(&str, u64)]) -> PoleState {
        let mut hasher = blake3::Hasher::new();
        for (path, size) in artifacts {
            hasher.update(path.as_bytes());
            hasher.update(&size.to_le_bytes());
        }
        let hash = hex::encode(hasher.finalize().as_bytes());
        PoleState {
            pole: Pole::Artifact,
            hash,
            item_count: artifacts.len(),
            timestamp: Utc::now(),
        }
    }

    /// Fingerprint the event-log pole from a slice of serialized OCEL event JSON strings.
    ///
    /// Each element is treated as one canonical event. The BLAKE3 hash is computed
    /// over the byte representation of each JSON string in order.
    pub fn fingerprint_event_log(events: &[&str]) -> PoleState {
        let (hash, item_count) = Self::blake3_of_strings(events);
        PoleState {
            pole: Pole::EventLog,
            hash,
            item_count,
            timestamp: Utc::now(),
        }
    }

    /// Compare up to three pole states and produce a [`CoherenceReport`].
    ///
    /// # Drift rules
    ///
    /// 1. If any of O, A, or L is absent → [`DriftKind::Missing`].
    /// 2. If artifact `item_count == 0` and ontology `item_count > 0` →
    ///    [`DriftKind::CountDiscrepancy`] between O and A.
    /// 3. If event-log `item_count == 0` and artifact `item_count > 0` →
    ///    [`DriftKind::CountDiscrepancy`] between A and L.
    ///
    /// `admitted` is `true` iff `drifts` is empty **and** all three poles are present.
    pub fn check(poles: &[PoleState]) -> CoherenceReport {
        let operation_id = Uuid::new_v4().to_string();

        // Index poles by kind for O(1) lookup.
        let mut by_pole: HashMap<Pole, &PoleState> = HashMap::new();
        for p in poles {
            by_pole.insert(p.pole, p);
        }

        let mut drifts: Vec<CoherenceDrift> = Vec::new();

        // Rule 1 — report any missing pole.
        for required in [Pole::Ontology, Pole::Artifact, Pole::EventLog] {
            if !by_pole.contains_key(&required) {
                drifts.push(CoherenceDrift {
                    kind: DriftKind::Missing,
                    source_pole: required,
                    target_pole: required,
                    detail: format!("{required:?} pole not provided"),
                });
            }
        }

        // Rule 2 — O→A count discrepancy.
        if let (Some(o), Some(a)) = (by_pole.get(&Pole::Ontology), by_pole.get(&Pole::Artifact)) {
            if a.item_count == 0 && o.item_count > 0 {
                drifts.push(CoherenceDrift {
                    kind: DriftKind::CountDiscrepancy,
                    source_pole: Pole::Ontology,
                    target_pole: Pole::Artifact,
                    detail: format!(
                        "Ontology has {} triple(s) but artifact pole reports 0 files",
                        o.item_count
                    ),
                });
            }
        }

        // Rule 3 — A→L count discrepancy.
        if let (Some(a), Some(l)) = (by_pole.get(&Pole::Artifact), by_pole.get(&Pole::EventLog)) {
            if l.item_count == 0 && a.item_count > 0 {
                drifts.push(CoherenceDrift {
                    kind: DriftKind::CountDiscrepancy,
                    source_pole: Pole::Artifact,
                    target_pole: Pole::EventLog,
                    detail: format!(
                        "Artifact pole has {} file(s) but event-log pole reports 0 events",
                        a.item_count
                    ),
                });
            }
        }

        let admitted = drifts.is_empty() && poles.len() == 3;

        CoherenceReport {
            operation_id,
            poles: poles.to_vec(),
            drifts,
            admitted,
        }
    }

    // ── private helpers ────────────────────────────────────────────────────────

    /// Feed each string through a BLAKE3 hasher and return `(hex_hash, count)`.
    fn blake3_of_strings(items: &[&str]) -> (String, usize) {
        let mut hasher = blake3::Hasher::new();
        for item in items {
            hasher.update(item.as_bytes());
        }
        (hex::encode(hasher.finalize().as_bytes()), items.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_poles_present_no_drift() {
        // Arrange — real fingerprint calls on real data structures (Chicago TDD).
        let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> .", "<s2> <p2> <o2> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[("crates/foo/src/lib.rs", 1024)]);
        let l = CoherenceChecker::fingerprint_event_log(&[
            r#"{"activity":"sync","timestamp":"2026-06-20"}"#,
        ]);

        // Act
        let report = CoherenceChecker::check(&[o, a, l]);

        // Assert — observable state: no drift, admitted
        assert!(
            report.drifts.is_empty(),
            "expected no drift but got: {:?}",
            report.drifts
        );
        assert!(
            report.admitted,
            "report should be admitted when all poles present"
        );
        assert_eq!(report.poles.len(), 3);
        assert!(!report.operation_id.is_empty());
    }

    #[test]
    fn test_missing_event_log_emits_drift() {
        // Arrange
        let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[("crates/foo/src/lib.rs", 512)]);

        // Act — L missing
        let report = CoherenceChecker::check(&[o, a]);

        // Assert — Missing drift for EventLog
        assert!(
            report.drifts.iter().any(|d| d.kind == DriftKind::Missing),
            "expected a Missing drift but got: {:?}",
            report.drifts
        );
        assert!(
            !report.admitted,
            "report should not be admitted with a missing pole"
        );
    }

    #[test]
    fn test_zero_artifacts_with_triples_emits_count_discrepancy() {
        // Arrange — ontology has content but artifact pole is empty
        let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[]); // empty
        let l = CoherenceChecker::fingerprint_event_log(&[]);

        // Act
        let report = CoherenceChecker::check(&[o, a, l]);

        // Assert — CountDiscrepancy between O and A
        assert!(
            report
                .drifts
                .iter()
                .any(|d| d.kind == DriftKind::CountDiscrepancy),
            "expected a CountDiscrepancy drift but got: {:?}",
            report.drifts
        );
        assert!(!report.admitted);
    }

    #[test]
    fn test_fingerprint_is_deterministic() {
        // Same input must produce the same hash (determinism invariant).
        let triples = &["<s> <p> <o> .", "<s2> <p2> <o2> ."];
        let h1 = CoherenceChecker::fingerprint_ontology(triples);
        let h2 = CoherenceChecker::fingerprint_ontology(triples);
        assert_eq!(h1.hash, h2.hash);
    }

    #[test]
    fn test_empty_all_poles_admitted() {
        // Three poles all empty — no count discrepancies (counts match at zero).
        let o = CoherenceChecker::fingerprint_ontology(&[]);
        let a = CoherenceChecker::fingerprint_artifacts(&[]);
        let l = CoherenceChecker::fingerprint_event_log(&[]);
        let report = CoherenceChecker::check(&[o, a, l]);
        assert!(
            report.drifts.is_empty(),
            "all-empty poles should be admitted: {:?}",
            report.drifts
        );
        assert!(report.admitted);
    }

    #[test]
    fn test_artifact_without_event_log_events_emits_count_discrepancy() {
        // A has files, L has zero events → CountDiscrepancy A→L.
        let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 256)]);
        let l = CoherenceChecker::fingerprint_event_log(&[]);
        let report = CoherenceChecker::check(&[o, a, l]);
        assert!(
            report
                .drifts
                .iter()
                .any(|d| d.kind == DriftKind::CountDiscrepancy
                    && d.source_pole == Pole::Artifact
                    && d.target_pole == Pole::EventLog),
            "expected A→L CountDiscrepancy: {:?}",
            report.drifts
        );
        assert!(!report.admitted);
    }

    #[test]
    fn test_operation_id_is_unique_per_check() {
        // Each call to check() must produce a fresh UUID (not a hardcoded sentinel).
        let o1 = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a1 = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 1)]);
        let l1 = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"x"}"#]);
        let r1 = CoherenceChecker::check(&[o1, a1, l1]);

        let o2 = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a2 = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 1)]);
        let l2 = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"x"}"#]);
        let r2 = CoherenceChecker::check(&[o2, a2, l2]);

        assert_ne!(
            r1.operation_id, r2.operation_id,
            "operation_ids must be unique across invocations"
        );
    }
}
