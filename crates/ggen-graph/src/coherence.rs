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
    /// A pole's freshly computed fingerprint differs from a previously
    /// declared/expected fingerprint (e.g., recorded in a prior receipt).
    /// This is the rule the post-Chatman round-trip (O→A→O) depends on.
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
    /// This is shorthand for [`CoherenceChecker::check_with_expectations`] with no
    /// declared fingerprints, so it never emits [`DriftKind::HashMismatch`] from self-comparison
    /// or cross-pole coherence checks. To detect drift against a previously recorded
    /// fingerprint (e.g., a prior receipt), use [`CoherenceChecker::check_with_expectations`].
    ///
    /// # Drift rules
    ///
    /// 1. If any of O, A, or L is absent → [`DriftKind::Missing`].
    /// 2. If artifact `item_count == 0` and ontology `item_count > 0` →
    ///    [`DriftKind::CountDiscrepancy`] between O and A.
    /// 3. If event-log `item_count == 0` and artifact `item_count > 0` →
    ///    [`DriftKind::CountDiscrepancy`] between A and L.
    /// 4. If event-log `item_count == 0` and ontology `item_count > 0` →
    ///    [`DriftKind::CountDiscrepancy`] between O and L (direct O↔L check, not only
    ///    transitive through A).
    ///
    /// `admitted` is `true` iff `drifts` is empty **and** all three poles are present.
    pub fn check(poles: &[PoleState]) -> CoherenceReport {
        Self::check_with_expectations(poles, &HashMap::new())
    }

    /// Compare up to three pole states against optional declared/expected fingerprints
    /// and produce a [`CoherenceReport`].
    ///
    /// `expectations` is a map from `Pole` to previously recorded fingerprint (typically
    /// from a prior coherence receipt). For each pole with a declared fingerprint,
    /// the freshly computed `hash` is compared against the declared one; a difference
    /// emits [`DriftKind::HashMismatch`]. This makes the post-Chatman round-trip (O→A→O)
    /// auditable: silent regeneration drift becomes a visible drift observation.
    ///
    /// Additionally, implements Rule 6 (cross-pole expectation coherence): if expectations
    /// exist for both O and L, and one pole is stable (matches expectation) while the other
    /// drifted (differs from expectation), this indicates a coherence fracture and emits
    /// a HashMismatch drift with the stable pole as source and drifted pole as target.
    ///
    /// All count/missing rules from [`CoherenceChecker::check`] also apply.
    ///
    /// # `admitted` invariant
    ///
    /// `admitted` is `true` **only if** all three poles are present **and** no
    /// [`DriftKind::HashMismatch`] and no [`DriftKind::Missing`] drift was recorded.
    /// Any hash mismatch or missing pole forces `admitted = false`.
    pub fn check_with_expectations(
        poles: &[PoleState], expectations: &HashMap<Pole, String>,
    ) -> CoherenceReport {
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

        // Rule 4 — O→L direct count discrepancy. An event log with no events cannot
        // be coherent with a non-empty ontology; flag it directly rather than relying
        // on the transitive O→A→L chain (which can be masked if A is also empty).
        if let (Some(o), Some(l)) = (by_pole.get(&Pole::Ontology), by_pole.get(&Pole::EventLog)) {
            if l.item_count == 0 && o.item_count > 0 {
                drifts.push(CoherenceDrift {
                    kind: DriftKind::CountDiscrepancy,
                    source_pole: Pole::Ontology,
                    target_pole: Pole::EventLog,
                    detail: format!(
                        "Ontology has {} triple(s) but event-log pole reports 0 events",
                        o.item_count
                    ),
                });
            }
        }

        // Rule 5 — HashMismatch: freshly computed fingerprint vs. declared fingerprint.
        // A self-comparison across time for a single pole, so source == target.
        for required in [Pole::Ontology, Pole::Artifact, Pole::EventLog] {
            if let (Some(state), Some(expected_hash)) =
                (by_pole.get(&required), expectations.get(&required))
            {
                if state.hash != *expected_hash {
                    drifts.push(CoherenceDrift {
                        kind: DriftKind::HashMismatch,
                        source_pole: required,
                        target_pole: required,
                        detail: format!(
                            "{required:?} fingerprint drift: declared {expected} != computed {computed}",
                            expected = expected_hash,
                            computed = state.hash,
                        ),
                    });
                }
            }
        }

        // Rule 6 — Cross-pole expectation coherence: if expectations exist for both O and L,
        // and O_fresh matches O_expected but L_fresh diverges from L_expected (or vice versa),
        // this indicates one pole is stable while the other drifted — a coherence fracture.
        let o_expected = expectations.get(&Pole::Ontology);
        let l_expected = expectations.get(&Pole::EventLog);
        if let (Some(o), Some(l)) = (by_pole.get(&Pole::Ontology), by_pole.get(&Pole::EventLog)) {
            if let (Some(o_exp), Some(l_exp)) = (o_expected, l_expected) {
                let o_matches = o.hash == *o_exp;
                let l_matches = l.hash == *l_exp;
                // If one pole is stable (matches expectation) and the other drifted (doesn't match),
                // that's a cross-pole coherence fracture.
                if o_matches != l_matches {
                    let (stable_pole, drifted_pole) = if o_matches {
                        (Pole::Ontology, Pole::EventLog)
                    } else {
                        (Pole::EventLog, Pole::Ontology)
                    };
                    drifts.push(CoherenceDrift {
                        kind: DriftKind::HashMismatch,
                        source_pole: stable_pole,
                        target_pole: drifted_pole,
                        detail: format!(
                            "Cross-pole coherence fracture: {stable:?} is stable while {drifted:?} drifted",
                            stable = stable_pole,
                            drifted = drifted_pole
                        ),
                    });
                }
            }
        }

        // `admitted` invariant: all three poles present AND no drift of any kind.
        // Because HashMismatch and Missing are always pushed into `drifts` when they
        // occur, `drifts.is_empty()` already forces `admitted = false` for them — the
        // hard-fail rules of Section 3.
        let admitted = drifts.is_empty() && poles.len() == 3;

        // Guard the hard-fail invariant: if a report is admitted, it must contain no
        // HashMismatch and no Missing drift. This documents Section 3 and prevents a
        // future refactor from silently admitting a blocking drift.
        debug_assert!(
            !admitted
                || !drifts
                    .iter()
                    .any(|d| matches!(d.kind, DriftKind::HashMismatch | DriftKind::Missing)),
            "HashMismatch/Missing drift must never coexist with admitted == true"
        );

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

    // ── HashMismatch detection (post-Chatman O→A→O round-trip) ──────────────────

    #[test]
    fn test_declared_equals_computed_no_hash_mismatch() {
        // Arrange — real triples; the declared fingerprint is the genuine BLAKE3 of
        // the SAME ontology (as a prior receipt would have recorded). Deterministic
        // fingerprinting means declared == computed.
        let triples = ["<s> <p> <o> .", "<s2> <p2> <o2> ."];
        let declared_o = CoherenceChecker::fingerprint_ontology(&triples).hash;

        let o = CoherenceChecker::fingerprint_ontology(&triples);
        let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 128)]);
        let l = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

        // Act
        let mut expectations = HashMap::new();
        expectations.insert(Pole::Ontology, declared_o);
        let report = CoherenceChecker::check_with_expectations(&[o, a, l], &expectations);

        // Assert — no HashMismatch (between expected and computed), but may have
        // HashMismatch from Rule 6 if O and L hashes differ.
        let self_comparison_mismatches: Vec<&CoherenceDrift> = report
            .drifts
            .iter()
            .filter(|d| d.kind == DriftKind::HashMismatch && d.source_pole == d.target_pole)
            .collect();
        assert!(
            self_comparison_mismatches.is_empty(),
            "matching fingerprints must not emit self-comparison HashMismatch: {:?}",
            report.drifts
        );
    }

    #[test]
    fn test_declared_differs_from_computed_emits_hash_mismatch_not_admitted() {
        // Arrange — the declared fingerprint is the genuine BLAKE3 of a DIFFERENT
        // ontology (a real divergence, e.g., the ontology mutated since the last
        // receipt). The computed fingerprint is of the current ontology.
        let declared_o = CoherenceChecker::fingerprint_ontology(&["<old> <old> <old> ."]).hash;
        let o = CoherenceChecker::fingerprint_ontology(&["<new> <new> <new> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 64)]);
        let l = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);

        // Sanity: the two fingerprints really differ (no fabricated mismatch).
        assert_ne!(
            declared_o, o.hash,
            "test precondition: declared and computed ontology hashes must differ"
        );

        // Act
        let mut expectations = HashMap::new();
        expectations.insert(Pole::Ontology, declared_o.clone());
        let report = CoherenceChecker::check_with_expectations(&[o, a, l], &expectations);

        // Assert — a HashMismatch on the Ontology pole (source == target), with a
        // detail that cites both the declared and the computed hash. Avoid `.expect()`
        // here: the crate root denies `clippy::expect_used` even in test modules.
        let computed_o = &report.poles[0].hash;
        assert!(
            report
                .drifts
                .iter()
                .any(|d| d.kind == DriftKind::HashMismatch
                    && d.source_pole == Pole::Ontology
                    && d.target_pole == Pole::Ontology
                    && d.detail.contains(&declared_o)
                    && d.detail.contains(computed_o)),
            "expected an Ontology HashMismatch citing declared+computed hashes: {:?}",
            report.drifts
        );

        // Assert — any HashMismatch forces admitted = false (Section 3 invariant).
        assert!(
            !report.admitted,
            "a HashMismatch must make the report not admitted"
        );
    }

    // ── O→L direct coherence check ──────────────────────────────────────────────

    #[test]
    fn test_ontology_with_empty_event_log_emits_o_to_l_discrepancy() {
        // Arrange — ontology has content, event log is empty. The O→L rule must flag
        // this directly (an event log inconsistent with the ontology), independent of
        // the A→L transitive path.
        let o = CoherenceChecker::fingerprint_ontology(&["<s> <p> <o> ."]);
        let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 32)]);
        let l = CoherenceChecker::fingerprint_event_log(&[]); // empty log

        // Act
        let report = CoherenceChecker::check(&[o, a, l]);

        // Assert — a direct O→L CountDiscrepancy is present.
        assert!(
            report
                .drifts
                .iter()
                .any(|d| d.kind == DriftKind::CountDiscrepancy
                    && d.source_pole == Pole::Ontology
                    && d.target_pole == Pole::EventLog),
            "expected a direct O→L CountDiscrepancy: {:?}",
            report.drifts
        );
        assert!(!report.admitted);
    }

    // ── determinism / sensitivity of the whole report ───────────────────────────

    #[test]
    fn test_check_with_expectations_is_deterministic_same_inputs() {
        // Same poles + same declared fingerprints → identical drift set and verdict.
        // (operation_id is intentionally excluded — it is a fresh UUID by design.)
        let triples = ["<s> <p> <o> ."];
        let declared_o = CoherenceChecker::fingerprint_ontology(&triples).hash;

        let build = || {
            let o = CoherenceChecker::fingerprint_ontology(&triples);
            let a = CoherenceChecker::fingerprint_artifacts(&[("src/lib.rs", 256)]);
            let l = CoherenceChecker::fingerprint_event_log(&[r#"{"activity":"sync"}"#]);
            let mut expectations = HashMap::new();
            expectations.insert(Pole::Ontology, declared_o.clone());
            CoherenceChecker::check_with_expectations(&[o, a, l], &expectations)
        };

        let r1 = build();
        let r2 = build();

        // Observable fields match: pole hashes, drift kinds/poles, admitted verdict.
        assert_eq!(r1.admitted, r2.admitted);
        assert_eq!(r1.drifts.len(), r2.drifts.len());
        let kinds1: Vec<DriftKind> = r1.drifts.iter().map(|d| d.kind).collect();
        let kinds2: Vec<DriftKind> = r2.drifts.iter().map(|d| d.kind).collect();
        assert_eq!(kinds1, kinds2, "drift kinds must be deterministic");
        let pole_hashes1: Vec<&String> = r1.poles.iter().map(|p| &p.hash).collect();
        let pole_hashes2: Vec<&String> = r2.poles.iter().map(|p| &p.hash).collect();
        assert_eq!(
            pole_hashes1, pole_hashes2,
            "pole fingerprints must be deterministic across runs"
        );
    }

    #[test]
    fn test_different_inputs_produce_different_fingerprints() {
        // Distinct ontology content → distinct fingerprints (mismatch is detectable).
        let h1 = CoherenceChecker::fingerprint_ontology(&["<a> <b> <c> ."]).hash;
        let h2 = CoherenceChecker::fingerprint_ontology(&["<x> <y> <z> ."]).hash;
        assert_ne!(
            h1, h2,
            "different ontology content must yield different hashes"
        );

        // And the artifact pole is sensitive to size as well as path.
        let a1 = CoherenceChecker::fingerprint_artifacts(&[("p.rs", 1)]).hash;
        let a2 = CoherenceChecker::fingerprint_artifacts(&[("p.rs", 2)]).hash;
        assert_ne!(
            a1, a2,
            "different artifact sizes must yield different hashes"
        );
    }
}
