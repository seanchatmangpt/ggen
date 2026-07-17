//! Parsing helpers for Weaver live-check validation results.
//!
//! Weaver emits newline separated JSON objects containing advice and summary
//! statistics.  This module converts that output into type-safe Rust structures
//! that can be consumed by Chicago TDD assertions.

#![cfg(all(feature = "weaver", feature = "otel"))]

use std::fmt::{self, Display};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

use serde::Deserialize;
use serde_json::{self, Value};

use crate::observability::{ObservabilityError, ObservabilityResult};

/// Complete set of validation artefacts emitted by Weaver live-check.
#[derive(Debug, Clone)]
pub struct ValidationResults {
    advices: Vec<LiveCheckAdvice>,
    statistics: Option<LiveCheckStatistics>,
    report_path: PathBuf,
}

impl ValidationResults {
    /// Load validation results from the Weaver report directory.
    ///
    /// # Errors
    ///
    /// Returns an error if the report file cannot be found, opened, read, or parsed.
    pub fn from_report_dir(dir: &Path) -> ObservabilityResult<Self> {
        let report_path = dir.join("live_check.json");

        if !report_path.exists() {
            return Err(ObservabilityError::ValidationFailed(format!(
                "Weaver report not found at {}",
                report_path.display()
            )));
        }

        let file = File::open(&report_path).map_err(|err| {
            ObservabilityError::ValidationFailed(format!(
                "Failed to open Weaver report {}: {err}",
                report_path.display()
            ))
        })?;

        let reader = BufReader::new(file);

        let mut advices = Vec::new();
        let mut statistics = None;

        for line_result in reader.lines() {
            let line = line_result.map_err(|err| {
                ObservabilityError::ValidationFailed(format!(
                    "Failed to read Weaver report {}: {err}",
                    report_path.display()
                ))
            })?;

            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            let value: Value = serde_json::from_str(trimmed).map_err(|err| {
                ObservabilityError::ValidationFailed(format!(
                    "Failed to parse Weaver JSON output: {err}"
                ))
            })?;

            if let Some(result) = value.get("live_check_result") {
                if let Ok(advice_set) = serde_json::from_value::<LiveCheckResult>(result.clone()) {
                    advices.extend(advice_set.all_advice.into_iter().map(Into::into));
                }
            } else if value.get("advice_level_counts").is_some() {
                if let Ok(stats) = serde_json::from_value::<LiveCheckStatistics>(value.clone()) {
                    statistics = Some(stats);
                }
            }
        }

        Ok(Self { advices, statistics, report_path })
    }

    /// Whether any `violation` level advice is present.
    #[must_use]
    pub fn has_violations(&self) -> bool {
        self.advices.iter().any(|advice| matches!(advice.level, AdviceLevel::Violation))
    }

    /// Iterator over all advice records.
    pub fn advices(&self) -> impl Iterator<Item = &LiveCheckAdvice> {
        self.advices.iter()
    }

    /// Human readable summary of all violations.
    #[must_use]
    pub fn violations_summary(&self) -> String {
        if !self.has_violations() {
            return "No Weaver live-check violations detected.".to_string();
        }

        let mut lines = vec!["Weaver live-check detected violations:".to_string()];

        for advice in self.advices.iter().filter(|a| matches!(a.level, AdviceLevel::Violation)) {
            lines.push(format!(
                "- [{}] {} :: {}",
                advice.signal_descriptor(),
                advice.advice_type,
                advice.message
            ));
        }

        lines.join("\n")
    }

    /// Access statistics when available.
    #[must_use]
    pub const fn statistics(&self) -> Option<&LiveCheckStatistics> {
        self.statistics.as_ref()
    }

    /// Location of the report file that produced these results.
    #[must_use]
    pub fn report_path(&self) -> &Path {
        &self.report_path
    }
}

#[derive(Debug, Clone, Deserialize)]
struct LiveCheckResult {
    #[serde(default)]
    all_advice: Vec<RawAdvice>,
}

#[derive(Debug, Clone, Deserialize)]
struct RawAdvice {
    #[serde(default)]
    advice_level: AdviceLevel,
    #[serde(default)]
    advice_type: String,
    #[serde(default)]
    message: String,
    #[serde(default)]
    signal_type: Option<String>,
    #[serde(default)]
    signal_name: Option<String>,
    #[serde(default)]
    advice_context: serde_json::Value,
}

impl From<RawAdvice> for LiveCheckAdvice {
    fn from(value: RawAdvice) -> Self {
        Self {
            level: value.advice_level,
            advice_type: value.advice_type,
            message: value.message,
            signal_type: value.signal_type,
            signal_name: value.signal_name,
            context: value.advice_context,
        }
    }
}

/// Simplified view of an advice record emitted by Weaver.
#[derive(Debug, Clone)]
pub struct LiveCheckAdvice {
    /// Severity of the advice (violation / improvement / information).
    pub level: AdviceLevel,
    /// Machine readable identifier describing the advice category.
    pub advice_type: String,
    /// Human readable message with remediation guidance.
    pub message: String,
    /// Signal type associated with the advice (e.g. `span`, `metric`).
    pub signal_type: Option<String>,
    /// Name of the signal the advice relates to.
    pub signal_name: Option<String>,
    /// Additional structured context emitted by Weaver.
    pub context: serde_json::Value,
}

impl LiveCheckAdvice {
    fn signal_descriptor(&self) -> String {
        match (self.signal_type.as_deref(), self.signal_name.as_deref()) {
            (Some(signal_type), Some(signal_name)) => format!("{signal_type}:{signal_name}"),
            (Some(signal_type), None) => signal_type.to_string(),
            (None, Some(signal_name)) => signal_name.to_string(),
            (None, None) => "<unknown>".to_string(),
        }
    }
}

/// Aggregated statistics emitted once live-check completes.
#[derive(Debug, Clone, Deserialize)]
pub struct LiveCheckStatistics {
    #[serde(default)]
    /// Number of advice entries per severity level.
    pub advice_level_counts: serde_json::Value,
    #[serde(default)]
    /// Number of advice entries per advice type.
    pub advice_type_counts: serde_json::Value,
    #[serde(default)]
    /// Highest advice level observed per sample.
    pub highest_advice_level_counts: serde_json::Value,
    #[serde(default)]
    /// Number of samples containing no advice entries.
    pub no_advice_count: Option<u64>,
    #[serde(default)]
    /// Total number of advice entries across all samples.
    pub total_advisories: Option<u64>,
    #[serde(default)]
    /// Total number of telemetry entities inspected.
    pub total_entities: Option<u64>,
    #[serde(default)]
    /// Count of entities grouped by signal type.
    pub total_entities_by_type: serde_json::Value,
    #[serde(default)]
    /// Fraction of registry entities observed during validation.
    pub registry_coverage: Option<f64>,
}

/// Severity levels reported by Weaver.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum AdviceLevel {
    /// A semantic convention violation.
    Violation,
    /// A recommended improvement.
    Improvement,
    /// Informational advice.
    Information,
    /// Advice of an unknown or custom level.
    #[serde(other)]
    #[default]
    Unknown,
}

impl Display for AdviceLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Violation => write!(f, "violation"),
            Self::Improvement => write!(f, "improvement"),
            Self::Information => write!(f, "information"),
            Self::Unknown => write!(f, "unknown"),
        }
    }
}
