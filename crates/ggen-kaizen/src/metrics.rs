//! Improvement metrics tracking and calculation.
//!
//! This module provides structures for tracking quantitative metrics
//! related to continuous improvement activities.

use crate::{KaizenError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Metrics snapshot at a point in time.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    /// When the snapshot was taken.
    pub timestamp: DateTime<Utc>,
    /// Cycle time in seconds.
    pub cycle_time_secs: Option<f64>,
    /// Defect rate (defects per unit).
    pub defect_rate: Option<f64>,
    /// Throughput (units per hour).
    pub throughput: Option<f64>,
    /// Quality score (0.0 - 1.0).
    pub quality_score: Option<f64>,
    /// Cost savings in currency units.
    pub cost_savings: Option<f64>,
    /// Custom metrics.
    pub custom_metrics: std::collections::HashMap<String, f64>,
}

impl MetricsSnapshot {
    /// Creates a new empty metrics snapshot.
    pub fn new() -> Self {
        Self {
            timestamp: Utc::now(),
            cycle_time_secs: None,
            defect_rate: None,
            throughput: None,
            quality_score: None,
            cost_savings: None,
            custom_metrics: std::collections::HashMap::new(),
        }
    }

    /// Sets the cycle time in seconds.
    pub fn with_cycle_time(mut self, secs: f64) -> Result<Self> {
        if secs < 0.0 {
            return Err(KaizenError::InvalidMetric(
                "Cycle time cannot be negative".to_string(),
            ));
        }
        self.cycle_time_secs = Some(secs);
        Ok(self)
    }

    /// Sets the defect rate.
    pub fn with_defect_rate(mut self, rate: f64) -> Result<Self> {
        if rate < 0.0 {
            return Err(KaizenError::InvalidMetric(
                "Defect rate cannot be negative".to_string(),
            ));
        }
        self.defect_rate = Some(rate);
        Ok(self)
    }

    /// Sets the throughput.
    pub fn with_throughput(mut self, throughput: f64) -> Result<Self> {
        if throughput < 0.0 {
            return Err(KaizenError::InvalidMetric(
                "Throughput cannot be negative".to_string(),
            ));
        }
        self.throughput = Some(throughput);
        Ok(self)
    }

    /// Sets the quality score (0.0 - 1.0).
    pub fn with_quality_score(mut self, score: f64) -> Result<Self> {
        if !(0.0..=1.0).contains(&score) {
            return Err(KaizenError::InvalidMetric(
                "Quality score must be between 0.0 and 1.0".to_string(),
            ));
        }
        self.quality_score = Some(score);
        Ok(self)
    }

    /// Sets the cost savings.
    pub fn with_cost_savings(mut self, savings: f64) -> Self {
        self.cost_savings = Some(savings);
        self
    }

    /// Adds a custom metric.
    pub fn add_custom_metric(&mut self, name: String, value: f64) {
        self.custom_metrics.insert(name, value);
    }

    /// Gets a custom metric value.
    pub fn get_custom_metric(&self, name: &str) -> Option<f64> {
        self.custom_metrics.get(name).copied()
    }
}

impl Default for MetricsSnapshot {
    fn default() -> Self {
        Self::new()
    }
}

/// Aggregated metrics over a time period.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedMetrics {
    /// Start of the aggregation period.
    pub period_start: DateTime<Utc>,
    /// End of the aggregation period.
    pub period_end: DateTime<Utc>,
    /// Average cycle time in seconds.
    pub avg_cycle_time_secs: Option<f64>,
    /// Average defect rate.
    pub avg_defect_rate: Option<f64>,
    /// Average throughput.
    pub avg_throughput: Option<f64>,
    /// Average quality score.
    pub avg_quality_score: Option<f64>,
    /// Total cost savings.
    pub total_cost_savings: Option<f64>,
    /// Number of data points aggregated.
    pub sample_count: usize,
}

impl AggregatedMetrics {
    /// Creates aggregated metrics from a series of snapshots.
    pub fn from_snapshots(
        snapshots: &[MetricsSnapshot],
        period_start: DateTime<Utc>,
        period_end: DateTime<Utc>,
    ) -> Result<Self> {
        if snapshots.is_empty() {
            return Err(KaizenError::MissingData(
                "No snapshots provided for aggregation".to_string(),
            ));
        }

        let avg_cycle_time_secs = {
            let values: Vec<f64> = snapshots.iter().filter_map(|s| s.cycle_time_secs).collect();
            if values.is_empty() {
                None
            } else {
                Some(values.iter().sum::<f64>() / values.len() as f64)
            }
        };

        let avg_defect_rate = {
            let values: Vec<f64> = snapshots.iter().filter_map(|s| s.defect_rate).collect();
            if values.is_empty() {
                None
            } else {
                Some(values.iter().sum::<f64>() / values.len() as f64)
            }
        };

        let avg_throughput = {
            let values: Vec<f64> = snapshots.iter().filter_map(|s| s.throughput).collect();
            if values.is_empty() {
                None
            } else {
                Some(values.iter().sum::<f64>() / values.len() as f64)
            }
        };

        let avg_quality_score = {
            let values: Vec<f64> = snapshots.iter().filter_map(|s| s.quality_score).collect();
            if values.is_empty() {
                None
            } else {
                Some(values.iter().sum::<f64>() / values.len() as f64)
            }
        };

        let total_cost_savings = {
            let values: Vec<f64> = snapshots.iter().filter_map(|s| s.cost_savings).collect();
            if values.is_empty() {
                None
            } else {
                Some(values.iter().sum::<f64>())
            }
        };

        Ok(Self {
            period_start,
            period_end,
            avg_cycle_time_secs,
            avg_defect_rate,
            avg_throughput,
            avg_quality_score,
            total_cost_savings,
            sample_count: snapshots.len(),
        })
    }

    /// Calculates the improvement percentage for a metric.
    pub fn calculate_improvement(baseline: f64, current: f64, lower_is_better: bool) -> f64 {
        if baseline == 0.0 {
            return 0.0;
        }

        let change_pct = ((current - baseline) / baseline) * 100.0;

        if lower_is_better {
            -change_pct
        } else {
            change_pct
        }
    }
}

/// Metrics calculator for improvement tracking.
#[derive(Debug, Clone)]
pub struct MetricsCalculator {
    snapshots: Vec<MetricsSnapshot>,
}

impl MetricsCalculator {
    /// Creates a new metrics calculator.
    pub fn new() -> Self {
        Self {
            snapshots: Vec::new(),
        }
    }

    /// Adds a snapshot to the calculator.
    pub fn add_snapshot(&mut self, snapshot: MetricsSnapshot) {
        self.snapshots.push(snapshot);
    }

    /// Gets all snapshots.
    pub fn snapshots(&self) -> &[MetricsSnapshot] {
        &self.snapshots
    }

    /// Calculates cycle time trend (lower is better).
    pub fn cycle_time_trend(&self) -> Option<f64> {
        self.calculate_trend(|s| s.cycle_time_secs)
    }

    /// Calculates defect rate trend (lower is better).
    pub fn defect_rate_trend(&self) -> Option<f64> {
        self.calculate_trend(|s| s.defect_rate)
    }

    /// Calculates throughput trend (higher is better).
    pub fn throughput_trend(&self) -> Option<f64> {
        self.calculate_trend(|s| s.throughput)
    }

    /// Calculates quality score trend (higher is better).
    pub fn quality_score_trend(&self) -> Option<f64> {
        self.calculate_trend(|s| s.quality_score)
    }

    /// Generic trend calculation using linear regression slope.
    fn calculate_trend<F>(&self, extractor: F) -> Option<f64>
    where
        F: Fn(&MetricsSnapshot) -> Option<f64>,
    {
        let data: Vec<(f64, f64)> = self
            .snapshots
            .iter()
            .enumerate()
            .filter_map(|(i, s)| extractor(s).map(|v| (i as f64, v)))
            .collect();

        if data.len() < 2 {
            return None;
        }

        let n = data.len() as f64;
        let sum_x: f64 = data.iter().map(|(x, _)| x).sum();
        let sum_y: f64 = data.iter().map(|(_, y)| y).sum();
        let sum_xy: f64 = data.iter().map(|(x, y)| x * y).sum();
        let sum_x2: f64 = data.iter().map(|(x, _)| x * x).sum();

        let denominator = n * sum_x2 - sum_x * sum_x;
        if denominator.abs() < f64::EPSILON {
            return None;
        }

        let slope = (n * sum_xy - sum_x * sum_y) / denominator;
        Some(slope)
    }

    /// Gets the latest snapshot.
    pub fn latest_snapshot(&self) -> Option<&MetricsSnapshot> {
        self.snapshots.last()
    }

    /// Gets snapshots within a time range.
    pub fn snapshots_in_range(
        &self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Vec<&MetricsSnapshot> {
        self.snapshots
            .iter()
            .filter(|s| s.timestamp >= start && s.timestamp <= end)
            .collect()
    }

    /// Aggregates metrics over a time period.
    pub fn aggregate_period(
        &self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<AggregatedMetrics> {
        let snapshots: Vec<MetricsSnapshot> = self
            .snapshots_in_range(start, end)
            .into_iter()
            .cloned()
            .collect();

        AggregatedMetrics::from_snapshots(&snapshots, start, end)
    }
}

impl Default for MetricsCalculator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Duration;

    #[test]
    fn test_metrics_snapshot_creation() {
        // Arrange & Act
        let snapshot = MetricsSnapshot::new()
            .with_cycle_time(10.5)
            .unwrap()
            .with_defect_rate(0.05)
            .unwrap()
            .with_throughput(100.0)
            .unwrap()
            .with_quality_score(0.95)
            .unwrap()
            .with_cost_savings(1000.0);

        // Assert
        assert_eq!(snapshot.cycle_time_secs, Some(10.5));
        assert_eq!(snapshot.defect_rate, Some(0.05));
        assert_eq!(snapshot.throughput, Some(100.0));
        assert_eq!(snapshot.quality_score, Some(0.95));
        assert_eq!(snapshot.cost_savings, Some(1000.0));
    }

    #[test]
    fn test_metrics_snapshot_invalid_cycle_time() {
        // Arrange & Act
        let result = MetricsSnapshot::new().with_cycle_time(-1.0);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_metrics_snapshot_invalid_quality_score() {
        // Arrange & Act
        let result1 = MetricsSnapshot::new().with_quality_score(-0.1);
        let result2 = MetricsSnapshot::new().with_quality_score(1.1);

        // Assert
        assert!(result1.is_err());
        assert!(result2.is_err());
    }

    #[test]
    fn test_custom_metrics() {
        // Arrange
        let mut snapshot = MetricsSnapshot::new();

        // Act
        snapshot.add_custom_metric("memory_usage".to_string(), 256.0);
        snapshot.add_custom_metric("cpu_usage".to_string(), 45.5);

        // Assert
        assert_eq!(snapshot.get_custom_metric("memory_usage"), Some(256.0));
        assert_eq!(snapshot.get_custom_metric("cpu_usage"), Some(45.5));
        assert_eq!(snapshot.get_custom_metric("disk_usage"), None);
    }

    #[test]
    fn test_aggregated_metrics_from_snapshots() {
        // Arrange
        let start = Utc::now();
        let snapshots = vec![
            MetricsSnapshot::new().with_cycle_time(10.0).unwrap(),
            MetricsSnapshot::new().with_cycle_time(12.0).unwrap(),
            MetricsSnapshot::new().with_cycle_time(8.0).unwrap(),
        ];

        // Act
        let aggregated = AggregatedMetrics::from_snapshots(&snapshots, start, Utc::now()).unwrap();

        // Assert
        assert_eq!(aggregated.sample_count, 3);
        assert_eq!(aggregated.avg_cycle_time_secs, Some(10.0));
    }

    #[test]
    fn test_aggregated_metrics_empty_snapshots() {
        // Arrange
        let snapshots: Vec<MetricsSnapshot> = vec![];
        let start = Utc::now();

        // Act
        let result = AggregatedMetrics::from_snapshots(&snapshots, start, Utc::now());

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_calculate_improvement_lower_is_better() {
        // Arrange & Act
        let improvement = AggregatedMetrics::calculate_improvement(10.0, 8.0, true);

        // Assert
        assert!((improvement - 20.0).abs() < f64::EPSILON); // 20% improvement
    }

    #[test]
    fn test_calculate_improvement_higher_is_better() {
        // Arrange & Act
        let improvement = AggregatedMetrics::calculate_improvement(100.0, 120.0, false);

        // Assert
        assert!((improvement - 20.0).abs() < f64::EPSILON); // 20% improvement
    }

    #[test]
    fn test_metrics_calculator_add_snapshot() {
        // Arrange
        let mut calculator = MetricsCalculator::new();
        let snapshot = MetricsSnapshot::new().with_cycle_time(10.0).unwrap();

        // Act
        calculator.add_snapshot(snapshot);

        // Assert
        assert_eq!(calculator.snapshots().len(), 1);
    }

    #[test]
    fn test_metrics_calculator_trend_insufficient_data() {
        // Arrange
        let mut calculator = MetricsCalculator::new();
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());

        // Act
        let trend = calculator.cycle_time_trend();

        // Assert
        assert_eq!(trend, None);
    }

    #[test]
    fn test_metrics_calculator_trend_calculation() {
        // Arrange
        let mut calculator = MetricsCalculator::new();
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(9.0).unwrap());
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

        // Act
        let trend = calculator.cycle_time_trend();

        // Assert
        assert!(trend.is_some());
        let slope = trend.unwrap();
        assert!(slope < 0.0); // Decreasing trend
    }

    #[test]
    fn test_metrics_calculator_latest_snapshot() {
        // Arrange
        let mut calculator = MetricsCalculator::new();
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

        // Act
        let latest = calculator.latest_snapshot();

        // Assert
        assert!(latest.is_some());
        assert_eq!(latest.unwrap().cycle_time_secs, Some(8.0));
    }

    #[test]
    fn test_metrics_calculator_snapshots_in_range() {
        // Arrange
        let mut calculator = MetricsCalculator::new();
        let now = Utc::now();
        let past = now - Duration::hours(2);
        let future = now + Duration::hours(2);

        calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());

        // Act
        let snapshots = calculator.snapshots_in_range(past, future);

        // Assert
        assert_eq!(snapshots.len(), 1);
    }

    #[test]
    fn test_aggregated_metrics_multiple_metrics() {
        // Arrange
        let start = Utc::now();
        let snapshots = vec![
            MetricsSnapshot::new()
                .with_cycle_time(10.0)
                .unwrap()
                .with_defect_rate(0.05)
                .unwrap()
                .with_throughput(100.0)
                .unwrap(),
            MetricsSnapshot::new()
                .with_cycle_time(12.0)
                .unwrap()
                .with_defect_rate(0.03)
                .unwrap()
                .with_throughput(120.0)
                .unwrap(),
        ];

        // Act
        let aggregated = AggregatedMetrics::from_snapshots(&snapshots, start, Utc::now()).unwrap();

        // Assert
        assert_eq!(aggregated.avg_cycle_time_secs, Some(11.0));
        assert_eq!(aggregated.avg_defect_rate, Some(0.04));
        assert_eq!(aggregated.avg_throughput, Some(110.0));
    }
}
