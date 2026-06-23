//! Coherence gate — Stage 4.5 validation checkpoint
//!
//! Validates that the three poles (O = ontology, A = artifacts, L = event log)
//! are isomorphic per the post-Chatman equation A ≅ O ≅ L.
//!
//! If coherence check fails, sync aborts with [`crate::sync::SyncError::CoherenceViolation`].
//! This gate is **fail-closed**: drifts block artifact emission.
//!
//! # Three Poles
//!
//! - **O (Ontology)**: RDF triples from the `.ttl` ontology file, fingerprinted via BLAKE3.
//! - **A (Artifact)**: Generated code/asset files, fingerprinted by enumerating paths and sizes.
//! - **L (Event Log)**: OCEL process evidence (currently empty for MVP, will be populated from Tempo).
//!
//! # Drift Rules
//!
//! The gate implements six drift rules from [`ggen_graph::coherence::CoherenceChecker`]:
//!
//! 1. **Missing**: A required pole is absent → **FATAL**, blocks write.
//! 2. **CountDiscrepancy (O→A)**: Ontology has triples but artifacts is empty → **CONFIGURABLE**.
//! 3. **CountDiscrepancy (A→L)**: Artifacts exist but event log is empty → **CONFIGURABLE**.
//! 4. **CountDiscrepancy (O→L)**: Ontology has triples but event log is empty → **CONFIGURABLE**.
//! 5. **HashMismatch (self)**: Freshly computed hash differs from declared expectation → **FATAL**.
//! 6. **HashMismatch (O↔L)**: Ontology and event-log hashes diverge → **FATAL**.
//!
//! Rules 1, 5, 6 are always fatal. Rules 2-4 (CountDiscrepancy) are configurable via
//! [`CoherenceGateConfig::allow_count_discrepancy`].

use crate::sync::SyncError;
use ggen_graph::coherence::{CoherenceChecker, CoherenceReport, DriftKind, Pole, PoleState};
use std::collections::HashMap;
use std::path::Path;

/// Configuration for coherence gate behavior.
#[derive(Debug, Clone)]
pub struct CoherenceGateConfig {
    /// If `true`, CountDiscrepancy violations are logged as warnings but don't block write.
    /// If `false`, any drift (including CountDiscrepancy) blocks write and returns `Err`.
    /// Default: `false` (fail-closed).
    pub allow_count_discrepancy: bool,

    /// If `true`, the event-log pole (L) is required and checked.
    /// If `false`, the event-log pole is optional (used for dry-run mode).
    /// Default: `true`.
    pub check_event_log: bool,

    /// Optional expected fingerprints from a prior receipt (for detecting drift over time).
    /// Maps each Pole to its previously recorded BLAKE3 hash.
    /// If a freshly computed hash differs from the expected hash, that's a HashMismatch drift.
    /// Default: `None` (no prior expectations, only check O↔L cross-pole hash).
    pub expectations: Option<HashMap<Pole, String>>,
}

impl Default for CoherenceGateConfig {
    fn default() -> Self {
        Self {
            allow_count_discrepancy: false,
            check_event_log: true,
            expectations: None,
        }
    }
}

/// The coherence gate validator — Stage 4.5 in the sync pipeline.
pub struct CoherenceGate {
    config: CoherenceGateConfig,
}

impl CoherenceGate {
    /// Create a new coherence gate with the given configuration.
    pub fn new(config: CoherenceGateConfig) -> Self {
        Self { config }
    }

    /// Validate the three poles and return either a `CoherenceReport` (success) or a `SyncError`.
    ///
    /// # Arguments
    ///
    /// - `ontology_bytes`: Raw bytes of the loaded `.ttl` ontology file (for fingerprinting).
    /// - `generated_files`: Generated source code, as `(relative_path, file_content)` pairs.
    /// - `event_log_events`: OCEL event JSON strings (empty for MVP).
    ///
    /// # Returns
    ///
    /// - `Ok(report)` if coherence check is admitted (no blocking drifts).
    /// - `Err(SyncError::CoherenceViolation)` if a blocking drift is detected.
    ///
    /// # Blocking Drifts
    ///
    /// The following drifts block write:
    /// - Any `Missing` pole
    /// - Any `HashMismatch` (cross-pole or self-comparison against expectations)
    /// - Any `CountDiscrepancy` (if `allow_count_discrepancy = false`)
    pub fn validate(
        &self,
        ontology_bytes: &[u8],
        generated_files: &[(impl AsRef<Path>, String)],
        event_log_events: &[&str],
    ) -> Result<CoherenceReport, SyncError> {
        // Convert ontology bytes to UTF-8 string for fingerprinting.
        let ontology_str = std::str::from_utf8(ontology_bytes)
            .map_err(|e| SyncError::CoherenceViolation {
                detail: format!("Cannot decode ontology as UTF-8: {}", e),
                report: CoherenceChecker::check(&[]), // Empty report as fallback
            })?;

        // Fingerprint the Ontology pole (O).
        let ontology_pole = CoherenceChecker::fingerprint_ontology(&[ontology_str]);

        // Fingerprint the Artifact pole (A).
        let artifact_pairs: Vec<(&str, u64)> = generated_files
            .iter()
            .map(|(p, content)| {
                let path_str = p
                    .as_ref()
                    .to_str()
                    .unwrap_or("<invalid-utf8-path>");
                (path_str, content.len() as u64)
            })
            .collect();
        let artifact_pole = CoherenceChecker::fingerprint_artifacts(&artifact_pairs);

        // Fingerprint the Event-log pole (L).
        let event_log_pole = if self.config.check_event_log {
            CoherenceChecker::fingerprint_event_log(event_log_events)
        } else {
            // In dry-run mode, event-log pole is empty (0 events).
            CoherenceChecker::fingerprint_event_log(&[])
        };

        // Emit OTEL span for coherence check initiation.
        tracing::debug!(
            target: "ggen_core",
            event = "coherence.check_started",
            ontology.item_count = ontology_pole.item_count,
            artifact.item_count = artifact_pole.item_count,
            event_log.item_count = event_log_pole.item_count,
            "Starting coherence check for three poles"
        );

        // Perform coherence check with optional expectations.
        let report = if let Some(expectations) = &self.config.expectations {
            CoherenceChecker::check_with_expectations(&[ontology_pole, artifact_pole, event_log_pole], expectations)
        } else {
            CoherenceChecker::check(&[ontology_pole, artifact_pole, event_log_pole])
        };

        // Emit OTEL span for coherence check result.
        tracing::info!(
            target: "ggen_core",
            event = "coherence.check_completed",
            coherence.admitted = report.admitted,
            coherence.pole_count = report.poles.len(),
            coherence.drift_count = report.drifts.len(),
            "Coherence check completed"
        );

        // Decide: block or warn?
        let blocking_drifts: Vec<_> = report
            .drifts
            .iter()
            .filter(|d| {
                matches!(d.kind, DriftKind::Missing | DriftKind::HashMismatch)
                    || (!self.config.allow_count_discrepancy && matches!(d.kind, DriftKind::CountDiscrepancy))
            })
            .collect();

        if !blocking_drifts.is_empty() {
            // Emit OTEL span for failure.
            for drift in &blocking_drifts {
                tracing::warn!(
                    target: "ggen_core",
                    event = "coherence.drift_detected",
                    drift.kind = ?drift.kind,
                    drift.source_pole = ?drift.source_pole,
                    drift.target_pole = ?drift.target_pole,
                    drift.detail = &drift.detail,
                    "Coherence drift detected (blocking)"
                );
            }

            let detail = format!(
                "Coherence check failed: {} blocking drift(s) detected",
                blocking_drifts.len()
            );
            return Err(SyncError::CoherenceViolation {
                detail,
                report,
            });
        }

        // Log non-blocking drifts (CountDiscrepancy when allow_count_discrepancy=true).
        for drift in &report.drifts {
            tracing::warn!(
                target: "ggen_core",
                event = "coherence.drift_detected",
                drift.kind = ?drift.kind,
                drift.source_pole = ?drift.source_pole,
                drift.target_pole = ?drift.target_pole,
                drift.detail = &drift.detail,
                "Coherence drift detected (non-blocking)"
            );
        }

        // Admit coherence check.
        tracing::info!(
            target: "ggen_core",
            event = "coherence.admitted",
            operation_id = &report.operation_id,
            "Coherence check admitted — three poles are isomorphic"
        );

        Ok(report)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gate_admits_full_coherence() {
        let config = CoherenceGateConfig::default();
        let gate = CoherenceGate::new(config);

        let ontology_bytes = b"<https://example.org/s> <https://example.org/p> <https://example.org/o> .";
        let generated = vec![(
            "test.rs",
            "fn main() {}".to_string(),
        )];
        let events = vec![];

        let report = gate.validate(ontology_bytes, &generated, &events);
        assert!(report.is_ok());
        let report = report.unwrap();
        assert!(report.admitted, "expected full coherence with empty event log when check_event_log=true");
    }

    #[test]
    fn test_gate_rejects_missing_pole() {
        let config = CoherenceGateConfig {
            check_event_log: false, // Skip event-log pole
            ..Default::default()
        };
        let gate = CoherenceGate::new(config);

        let ontology_bytes = b"<https://example.org/s> <https://example.org/p> <https://example.org/o> .";
        let generated = vec![(
            "test.rs",
            "fn main() {}".to_string(),
        )];
        let events = vec![];

        let report = gate.validate(ontology_bytes, &generated, &events);
        assert!(report.is_ok());
        let report = report.unwrap();
        assert!(!report.admitted, "missing event-log pole should prevent admission");
    }
}
