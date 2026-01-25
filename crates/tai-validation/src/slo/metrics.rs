//! SLO metrics collection and tracking

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// SLO metric type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MetricType {
    /// Build time in seconds
    BuildTime,
    /// Incremental build time in seconds
    IncrementalBuildTime,
    /// RDF processing time in seconds
    RdfProcessingTime,
    /// Memory usage in megabytes
    MemoryUsage,
    /// CLI end-to-end execution time in seconds
    CliEndToEnd,
    /// Test execution time in seconds
    TestExecutionTime,
    /// Test count
    TestCount,
    /// Coverage percentage
    CoveragePercent,
}

impl std::fmt::Display for MetricType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetricType::BuildTime => write!(f, "build_time"),
            MetricType::IncrementalBuildTime => write!(f, "incremental_build_time"),
            MetricType::RdfProcessingTime => write!(f, "rdf_processing_time"),
            MetricType::MemoryUsage => write!(f, "memory_usage"),
            MetricType::CliEndToEnd => write!(f, "cli_end_to_end"),
            MetricType::TestExecutionTime => write!(f, "test_execution_time"),
            MetricType::TestCount => write!(f, "test_count"),
            MetricType::CoveragePercent => write!(f, "coverage_percent"),
        }
    }
}

/// Single metric measurement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Measurement {
    /// Metric type
    pub metric_type: MetricType,
    /// Value
    pub value: f64,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// Context/labels (e.g., crate name, test suite)
    pub context: HashMap<String, String>,
}

impl Measurement {
    /// Create a new measurement
    pub fn new(metric_type: MetricType, value: f64) -> Self {
        Self {
            metric_type,
            value,
            timestamp: Utc::now(),
            context: HashMap::new(),
        }
    }

    /// Add context label
    pub fn with_context(mut self, key: String, value: String) -> Self {
        self.context.insert(key, value);
        self
    }
}

/// Aggregated SLO metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SloMetrics {
    /// Metric type
    pub metric_type: MetricType,
    /// Current value
    pub current: f64,
    /// Minimum value (from history)
    pub min: f64,
    /// Maximum value (from history)
    pub max: f64,
    /// Average value (from history)
    pub average: f64,
    /// P95 percentile (from history)
    pub p95: f64,
    /// P99 percentile (from history)
    pub p99: f64,
    /// SLO threshold
    pub threshold: f64,
    /// Is currently violating SLO
    pub violating: bool,
    /// Number of violations in last 7 days
    pub violation_count_7d: u32,
    /// Timestamp of last update
    pub updated_at: DateTime<Utc>,
}

impl SloMetrics {
    /// Create new SLO metrics
    pub fn new(metric_type: MetricType, current: f64, threshold: f64) -> Self {
        Self {
            metric_type,
            current,
            min: current,
            max: current,
            average: current,
            p95: current,
            p99: current,
            threshold,
            violating: current > threshold,
            violation_count_7d: 0,
            updated_at: Utc::now(),
        }
    }

    /// Check if metric violates SLO
    pub fn is_violating(&self) -> bool {
        self.current > self.threshold
    }

    /// Get compliance percentage (how much under threshold)
    pub fn compliance_percent(&self) -> f64 {
        if self.threshold == 0.0 {
            0.0
        } else {
            ((self.threshold - self.current) / self.threshold * 100.0).max(0.0)
        }
    }

    /// Update with new measurement
    pub fn update(&mut self, value: f64) {
        self.current = value;
        self.min = self.min.min(value);
        self.max = self.max.max(value);
        self.violating = value > self.threshold;
        self.updated_at = Utc::now();
    }

    /// Get trend (increasing/decreasing/stable)
    pub fn trend(&self) -> Trend {
        if self.current > self.average * 1.1 {
            Trend::Increasing
        } else if self.current < self.average * 0.9 {
            Trend::Decreasing
        } else {
            Trend::Stable
        }
    }
}

/// Metric trend direction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Trend {
    /// Value is increasing
    Increasing,
    /// Value is decreasing
    Decreasing,
    /// Value is stable
    Stable,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_measurement_creation() {
        let m = Measurement::new(MetricType::BuildTime, 12.5);
        assert_eq!(m.value, 12.5);
        assert_eq!(m.metric_type, MetricType::BuildTime);
    }

    #[test]
    fn test_measurement_with_context() {
        let m = Measurement::new(MetricType::BuildTime, 12.5)
            .with_context("crate".to_string(), "ggen-core".to_string());
        assert_eq!(m.context.get("crate"), Some(&"ggen-core".to_string()));
    }

    #[test]
    fn test_slo_metrics_violation() {
        let metrics = SloMetrics::new(MetricType::BuildTime, 16.0, 15.0);
        assert!(metrics.is_violating());
        assert_eq!(metrics.metric_type, MetricType::BuildTime);
    }

    #[test]
    fn test_slo_metrics_compliance() {
        let metrics = SloMetrics::new(MetricType::BuildTime, 12.0, 15.0);
        assert!(!metrics.is_violating());
        let compliance = metrics.compliance_percent();
        assert!(compliance > 0.0);
    }

    #[test]
    fn test_metric_trend() {
        let mut metrics = SloMetrics::new(MetricType::BuildTime, 10.0, 15.0);
        metrics.average = 10.0;
        metrics.update(12.0); // 20% increase -> Increasing
        assert_eq!(metrics.trend(), Trend::Increasing);

        metrics.update(8.5); // Less than 0.9 * average (9.0) -> Decreasing
        assert_eq!(metrics.trend(), Trend::Decreasing);

        metrics.update(10.1); // Between 9.0 and 11.0 -> Stable
        assert_eq!(metrics.trend(), Trend::Stable);
    }
}
