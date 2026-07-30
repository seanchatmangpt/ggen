//! Capacity envelopes and deterministic breaking-point analysis.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::model::Severity;

/// Multidimensional architecture workload.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default)]
pub struct WorkloadVector {
    /// Independent ontology or architecture documents.
    #[serde(default)]
    pub documents: u64,
    /// Parsed RDF quads.
    #[serde(default)]
    pub quads: u64,
    /// Blank nodes requiring document-local identity and canonicalization.
    #[serde(default)]
    pub blank_nodes: u64,
    /// Derivation or denial rules.
    #[serde(default)]
    pub rules: u64,
    /// SHACL or equivalent validation shapes.
    #[serde(default)]
    pub shapes: u64,
    /// Tera or other manufacturing templates.
    #[serde(default)]
    pub templates: u64,
    /// Requested output projections.
    #[serde(default)]
    pub projections: u64,
}

impl WorkloadVector {
    /// Deterministic workload units used for slope and knee comparisons.
    #[must_use]
    pub fn units(&self) -> u64 {
        self.documents
            .saturating_mul(10)
            .saturating_add(self.quads)
            .saturating_add(self.blank_nodes.saturating_mul(2))
            .saturating_add(self.rules.saturating_mul(100))
            .saturating_add(self.shapes.saturating_mul(50))
            .saturating_add(self.templates.saturating_mul(20))
            .saturating_add(self.projections.saturating_mul(20))
    }
}

/// One observed architecture execution profile.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapacitySample {
    /// Stable sample label.
    pub label: String,
    /// Workload exercised by the sample.
    pub workload: WorkloadVector,
    /// End-to-end elapsed time in milliseconds.
    pub elapsed_ms: u64,
    /// Peak resident or allocated memory in bytes.
    pub peak_memory_bytes: u64,
    /// Optional phase timings in milliseconds.
    #[serde(default)]
    pub phase_ms: BTreeMap<String, u64>,
}

/// Capacity standing for one sample or envelope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CapacityLevel {
    /// Within the admitted operating envelope.
    Healthy,
    /// Approaching or exceeding a warning budget.
    Warning,
    /// Exceeds a refusal threshold or hard cap.
    Refuse,
}

/// Capacity governance policy.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapacityPolicy {
    /// Latency warning budget.
    pub warn_elapsed_ms: u64,
    /// Latency refusal budget.
    pub refuse_elapsed_ms: u64,
    /// Memory warning budget.
    pub warn_memory_bytes: u64,
    /// Memory refusal budget.
    pub refuse_memory_bytes: u64,
    /// Optional absolute document cap.
    #[serde(default)]
    pub max_documents: Option<u64>,
    /// Optional absolute quad cap.
    #[serde(default)]
    pub max_quads: Option<u64>,
    /// Minimum adjacent-slope multiplication classified as a nonlinear knee.
    pub knee_slope_ratio: f64,
}

impl Default for CapacityPolicy {
    fn default() -> Self {
        Self {
            warn_elapsed_ms: 1_000,
            refuse_elapsed_ms: 5_000,
            warn_memory_bytes: 512 * 1024 * 1024,
            refuse_memory_bytes: 2 * 1024 * 1024 * 1024,
            max_documents: None,
            max_quads: None,
            knee_slope_ratio: 2.0,
        }
    }
}

/// One policy finding produced from an observed sample.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CapacityFinding {
    /// Stable diagnostic code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Human-readable explanation.
    pub message: String,
    /// Suggested bounded response.
    pub remediation: String,
}

impl CapacityPolicy {
    /// Evaluate one observed sample against all budgets and hard caps.
    #[must_use]
    pub fn evaluate(&self, sample: &CapacitySample) -> Vec<CapacityFinding> {
        let mut findings = Vec::new();

        if sample.elapsed_ms >= self.refuse_elapsed_ms {
            findings.push(CapacityFinding {
                code: "EA-CAP-001".to_string(),
                severity: Severity::Critical,
                message: format!(
                    "sample `{}` required {} ms, exceeding the {} ms refusal budget",
                    sample.label, sample.elapsed_ms, self.refuse_elapsed_ms
                ),
                remediation: "refuse promotion; profile phases and select a smaller architecture profile"
                    .to_string(),
            });
        } else if sample.elapsed_ms >= self.warn_elapsed_ms {
            findings.push(CapacityFinding {
                code: "EA-CAP-002".to_string(),
                severity: Severity::Warning,
                message: format!(
                    "sample `{}` required {} ms, exceeding the {} ms warning budget",
                    sample.label, sample.elapsed_ms, self.warn_elapsed_ms
                ),
                remediation: "measure phase dominance and consider caching, pruning, or lazy materialization"
                    .to_string(),
            });
        }

        if sample.peak_memory_bytes >= self.refuse_memory_bytes {
            findings.push(CapacityFinding {
                code: "EA-CAP-003".to_string(),
                severity: Severity::Critical,
                message: format!(
                    "sample `{}` used {} bytes, exceeding the {} byte refusal budget",
                    sample.label, sample.peak_memory_bytes, self.refuse_memory_bytes
                ),
                remediation: "refuse promotion; partition the graph or select a bounded profile"
                    .to_string(),
            });
        } else if sample.peak_memory_bytes >= self.warn_memory_bytes {
            findings.push(CapacityFinding {
                code: "EA-CAP-004".to_string(),
                severity: Severity::Warning,
                message: format!(
                    "sample `{}` used {} bytes, exceeding the {} byte warning budget",
                    sample.label, sample.peak_memory_bytes, self.warn_memory_bytes
                ),
                remediation: "inspect graph density, blank-node canonicalization, and materialization growth"
                    .to_string(),
            });
        }

        if let Some(max_documents) = self.max_documents {
            if sample.workload.documents > max_documents {
                findings.push(CapacityFinding {
                    code: "EA-CAP-005".to_string(),
                    severity: Severity::Critical,
                    message: format!(
                        "sample `{}` contains {} documents, exceeding the hard cap of {}",
                        sample.label, sample.workload.documents, max_documents
                    ),
                    remediation: "refuse admission or select an explicitly larger approved profile"
                        .to_string(),
                });
            }
        }

        if let Some(max_quads) = self.max_quads {
            if sample.workload.quads > max_quads {
                findings.push(CapacityFinding {
                    code: "EA-CAP-006".to_string(),
                    severity: Severity::Critical,
                    message: format!(
                        "sample `{}` contains {} quads, exceeding the hard cap of {}",
                        sample.label, sample.workload.quads, max_quads
                    ),
                    remediation: "refuse admission or split the ontology composition"
                        .to_string(),
                });
            }
        }

        findings
    }

    /// Classify a sample by its most severe capacity finding.
    #[must_use]
    pub fn classify(&self, sample: &CapacitySample) -> CapacityLevel {
        let findings = self.evaluate(sample);
        if findings
            .iter()
            .any(|finding| finding.severity >= Severity::Error)
        {
            CapacityLevel::Refuse
        } else if findings.is_empty() {
            CapacityLevel::Healthy
        } else {
            CapacityLevel::Warning
        }
    }
}

/// Observed capacity envelope with the first policy crossings and nonlinear knee.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapacityEnvelope {
    /// Samples sorted by deterministic workload units.
    pub samples: Vec<CapacitySample>,
    /// First warning-level sample.
    pub first_warning: Option<String>,
    /// First refusal-level sample.
    pub first_refusal: Option<String>,
    /// First sample where adjacent cost slope multiplies beyond policy.
    pub first_knee: Option<String>,
    /// Maximum observed workload units.
    pub max_observed_units: u64,
    /// Standing of the largest observed sample.
    pub latest_level: CapacityLevel,
}

impl CapacityEnvelope {
    /// Analyze samples without inventing a breaking point beyond observation.
    #[must_use]
    pub fn analyze(samples: &[CapacitySample], policy: &CapacityPolicy) -> Self {
        let mut ordered = samples.to_vec();
        ordered.sort_by(|left, right| {
            left.workload
                .units()
                .cmp(&right.workload.units())
                .then(left.elapsed_ms.cmp(&right.elapsed_ms))
                .then(left.label.cmp(&right.label))
        });

        let first_warning = ordered
            .iter()
            .find(|sample| policy.classify(sample) == CapacityLevel::Warning)
            .map(|sample| sample.label.clone());
        let first_refusal = ordered
            .iter()
            .find(|sample| policy.classify(sample) == CapacityLevel::Refuse)
            .map(|sample| sample.label.clone());

        let mut first_knee = None;
        let mut previous_slope = None;
        for pair in ordered.windows(2) {
            let left = &pair[0];
            let right = &pair[1];
            let units_delta = right
                .workload
                .units()
                .saturating_sub(left.workload.units());
            if units_delta == 0 {
                continue;
            }
            let elapsed_delta = right.elapsed_ms.saturating_sub(left.elapsed_ms);
            let slope = elapsed_delta as f64 / units_delta as f64;
            if let Some(previous) = previous_slope {
                if previous > 0.0
                    && slope / previous >= policy.knee_slope_ratio
                    && first_knee.is_none()
                {
                    first_knee = Some(right.label.clone());
                }
            }
            previous_slope = Some(slope);
        }

        let latest_level = ordered
            .last()
            .map_or(CapacityLevel::Healthy, |sample| policy.classify(sample));
        let max_observed_units = ordered
            .last()
            .map_or(0, |sample| sample.workload.units());

        Self {
            samples: ordered,
            first_warning,
            first_refusal,
            first_knee,
            max_observed_units,
            latest_level,
        }
    }

    /// Predict elapsed milliseconds using only the final observed segment.
    ///
    /// Returns `None` when fewer than two distinct workload observations exist.
    #[must_use]
    pub fn predict_elapsed_ms(&self, workload: &WorkloadVector) -> Option<u64> {
        let pair = self.samples.windows(2).last()?;
        let left = &pair[0];
        let right = &pair[1];
        let left_units = left.workload.units();
        let right_units = right.workload.units();
        let units_delta = right_units.checked_sub(left_units)?;
        if units_delta == 0 {
            return None;
        }
        let elapsed_delta = right.elapsed_ms.saturating_sub(left.elapsed_ms);
        let target_units = workload.units();
        if target_units <= right_units {
            return Some(right.elapsed_ms);
        }
        let additional_units = target_units.saturating_sub(right_units);
        let additional_ms = (elapsed_delta as u128)
            .saturating_mul(additional_units as u128)
            / units_delta as u128;
        let predicted = (right.elapsed_ms as u128).saturating_add(additional_ms);
        u64::try_from(predicted).ok()
    }
}
