//! SLO validation engine

use crate::error::{Result, ValidationError};
use crate::slo::metrics::{MetricType, SloMetrics};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// SLO validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SloValidationResult {
    /// All metrics checked
    pub metrics: Vec<SloMetrics>,
    /// Number of violations
    pub violation_count: usize,
    /// Are all SLOs met
    pub all_compliant: bool,
    /// Violation details
    pub violations: Vec<SloViolation>,
}

impl SloValidationResult {
    /// Check if all SLOs are met
    pub fn is_compliant(&self) -> bool {
        self.all_compliant
    }

    /// Get critical violations (most over threshold)
    pub fn critical_violations(&self) -> Vec<&SloViolation> {
        let mut violations = self.violations.iter().collect::<Vec<_>>();
        violations.sort_by(|a, b| b.excess_percent.partial_cmp(&a.excess_percent).unwrap());
        violations.into_iter().take(3).collect()
    }
}

/// Individual SLO violation details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SloViolation {
    /// Metric name
    pub metric: String,
    /// Actual value
    pub actual: f64,
    /// Threshold
    pub threshold: f64,
    /// How much over threshold (percentage)
    pub excess_percent: f64,
    /// Remediation guidance
    pub remediation: String,
}

impl SloViolation {
    /// Create new violation
    pub fn new(metric: String, actual: f64, threshold: f64) -> Self {
        let excess_percent = if threshold == 0.0 {
            100.0
        } else {
            ((actual - threshold) / threshold * 100.0).max(0.0)
        };

        let remediation = Self::suggest_remediation(&metric, &excess_percent);

        Self {
            metric,
            actual,
            threshold,
            excess_percent,
            remediation,
        }
    }

    /// Suggest remediation based on metric and excess
    fn suggest_remediation(metric: &str, excess_percent: &f64) -> String {
        match metric {
            "build_time" => format!(
                "Build time exceeded by {:.1}%. Consider: parallel compilation, incremental builds, or dependency optimization",
                excess_percent
            ),
            "incremental_build_time" => format!(
                "Incremental build exceeded by {:.1}%. Check for: unnecessary recompilation, macro expansion, or large intermediate files",
                excess_percent
            ),
            "rdf_processing_time" => format!(
                "RDF processing exceeded by {:.1}%. Optimize: SPARQL queries, reduce triple count, or use indexing",
                excess_percent
            ),
            "memory_usage" => format!(
                "Memory usage exceeded by {:.1}%. Review: data structures, caching strategy, or reduce working set size",
                excess_percent
            ),
            "cli_end_to_end" => format!(
                "CLI execution exceeded by {:.1}%. Profile: startup time, I/O operations, or parallelization opportunities",
                excess_percent
            ),
            _ => format!(
                "Performance metric exceeded by {:.1}%. Investigate performance profile and bottlenecks",
                excess_percent
            ),
        }
    }
}

/// SLO validator
pub struct SloValidator {
    /// SLO thresholds by metric type
    thresholds: HashMap<MetricType, f64>,
}

impl SloValidator {
    /// Create new SLO validator with default thresholds
    pub fn new() -> Self {
        let mut thresholds = HashMap::new();
        thresholds.insert(MetricType::BuildTime, super::SLO_BUILD_FIRST);
        thresholds.insert(
            MetricType::IncrementalBuildTime,
            super::SLO_BUILD_INCREMENTAL,
        );
        thresholds.insert(MetricType::RdfProcessingTime, super::SLO_RDF_PROCESSING);
        thresholds.insert(MetricType::MemoryUsage, super::SLO_MEMORY_MAX);
        thresholds.insert(MetricType::CliEndToEnd, super::SLO_CLI_END_TO_END);

        Self { thresholds }
    }

    /// Set custom threshold for a metric
    pub fn with_threshold(mut self, metric_type: MetricType, threshold: f64) -> Self {
        if threshold > 0.0 {
            self.thresholds.insert(metric_type, threshold);
        }
        self
    }

    /// Validate single metric
    pub fn validate_metric(&self, metric_type: MetricType, value: f64) -> Result<SloMetrics> {
        let threshold = self.thresholds.get(&metric_type).ok_or_else(|| {
            ValidationError::ConfigError(format!(
                "No SLO threshold configured for {:?}",
                metric_type
            ))
        })?;

        let mut metrics = SloMetrics::new(metric_type, value, *threshold);
        if metrics.is_violating() {
            // Record violation
            metrics.violation_count_7d += 1;
        }
        Ok(metrics)
    }

    /// Validate multiple metrics
    pub fn validate_metrics(&self, measurements: Vec<SloMetrics>) -> SloValidationResult {
        let mut violations = Vec::new();
        let mut violation_count = 0;

        for metrics in &measurements {
            if metrics.is_violating() {
                violation_count += 1;
                violations.push(SloViolation::new(
                    metrics.metric_type.to_string(),
                    metrics.current,
                    metrics.threshold,
                ));
            }
        }

        let all_compliant = violations.is_empty();

        SloValidationResult {
            metrics: measurements,
            violation_count,
            all_compliant,
            violations,
        }
    }

    /// Get all configured thresholds
    pub fn thresholds(&self) -> &HashMap<MetricType, f64> {
        &self.thresholds
    }
}

impl Default for SloValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validator_creation() {
        let validator = SloValidator::new();
        assert!(!validator.thresholds().is_empty());
    }

    #[test]
    fn test_validate_compliant_metric() {
        let validator = SloValidator::new();
        let result = validator.validate_metric(MetricType::BuildTime, 12.0);
        assert!(result.is_ok());
        let metrics = result.unwrap();
        assert!(!metrics.is_violating());
    }

    #[test]
    fn test_validate_violating_metric() {
        let validator = SloValidator::new();
        let result = validator.validate_metric(MetricType::BuildTime, 20.0);
        assert!(result.is_ok());
        let metrics = result.unwrap();
        assert!(metrics.is_violating());
    }

    #[test]
    fn test_violation_remediation() {
        let violation = SloViolation::new("build_time".to_string(), 20.0, 15.0);
        assert!(!violation.remediation.is_empty());
        assert!(violation.excess_percent > 0.0);
    }

    #[test]
    fn test_validation_result() {
        let validator = SloValidator::new();
        let metrics1 = SloMetrics::new(MetricType::BuildTime, 12.0, 15.0);
        let metrics2 = SloMetrics::new(MetricType::MemoryUsage, 150.0, 100.0);

        let result = validator.validate_metrics(vec![metrics1, metrics2]);
        assert_eq!(result.violation_count, 1);
        assert!(!result.all_compliant);
    }
}
