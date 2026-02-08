//! SLO metric tracking and history

use crate::slo::metrics::{Measurement, MetricType, SloMetrics};
use chrono::{Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// SLO tracker with historical data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SloTracker {
    /// Measurements by metric type
    measurements: HashMap<String, Vec<Measurement>>,
    /// Max history size per metric (keep last N measurements)
    max_history: usize,
}

impl SloTracker {
    /// Create new SLO tracker
    pub fn new() -> Self {
        Self {
            measurements: HashMap::new(),
            max_history: 1000,
        }
    }

    /// Create with custom history size
    pub fn with_history_size(max_history: usize) -> Self {
        Self {
            measurements: HashMap::new(),
            max_history,
        }
    }

    /// Record a measurement
    pub fn record(&mut self, measurement: Measurement) {
        let key = measurement.metric_type.to_string();
        let entry = self.measurements.entry(key).or_insert_with(Vec::new);

        entry.push(measurement);

        // Trim history if exceeds max
        if entry.len() > self.max_history {
            entry.remove(0);
        }
    }

    /// Get current metrics for a type
    pub fn get_metrics(&self, metric_type: MetricType) -> Option<SloMetrics> {
        let key = metric_type.to_string();
        self.measurements.get(&key).and_then(|measurements| {
            if measurements.is_empty() {
                return None;
            }

            let latest = &measurements[measurements.len() - 1];
            let values: Vec<f64> = measurements.iter().map(|m| m.value).collect();

            let min = values.iter().cloned().fold(f64::MAX, f64::min);
            let max = values.iter().cloned().fold(f64::MIN, f64::max);
            let average = values.iter().sum::<f64>() / values.len() as f64;
            let p95 = calculate_percentile(&values, 0.95);
            let p99 = calculate_percentile(&values, 0.99);

            Some(SloMetrics {
                metric_type,
                current: latest.value,
                min,
                max,
                average,
                p95,
                p99,
                threshold: 0.0, // Will be set by validator
                violating: false,
                violation_count_7d: 0,
                updated_at: latest.timestamp,
            })
        })
    }

    /// Get measurements within time window
    pub fn get_recent(&self, metric_type: MetricType, duration: Duration) -> Vec<Measurement> {
        let key = metric_type.to_string();
        let cutoff = Utc::now() - duration;

        self.measurements
            .get(&key)
            .map(|measurements| {
                measurements
                    .iter()
                    .filter(|m| m.timestamp > cutoff)
                    .cloned()
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get last N measurements
    pub fn get_last_n(&self, metric_type: MetricType, n: usize) -> Vec<Measurement> {
        let key = metric_type.to_string();

        self.measurements
            .get(&key)
            .map(|measurements| {
                measurements
                    .iter()
                    .rev()
                    .take(n)
                    .cloned()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Count violations in last 7 days
    pub fn count_violations_7d(&self, metric_type: MetricType, threshold: f64) -> usize {
        let duration = Duration::days(7);
        let recent = self.get_recent(metric_type, duration);
        recent.iter().filter(|m| m.value > threshold).count()
    }

    /// Get all metric types with data
    pub fn metric_types(&self) -> Vec<String> {
        self.measurements.keys().cloned().collect()
    }

    /// Clear all data
    pub fn clear(&mut self) {
        self.measurements.clear();
    }

    /// Get total measurement count
    pub fn total_measurements(&self) -> usize {
        self.measurements.values().map(|m| m.len()).sum()
    }
}

impl Default for SloTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate percentile from sorted values
fn calculate_percentile(values: &[f64], percentile: f64) -> f64 {
    if values.is_empty() {
        return 0.0;
    }

    let mut sorted = values.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

    let index = ((percentile * sorted.len() as f64) as usize).min(sorted.len() - 1);
    sorted[index]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracker_creation() {
        let tracker = SloTracker::new();
        assert_eq!(tracker.total_measurements(), 0);
    }

    #[test]
    fn test_record_measurement() {
        let mut tracker = SloTracker::new();
        let measurement = Measurement::new(MetricType::BuildTime, 12.5);
        tracker.record(measurement);
        assert_eq!(tracker.total_measurements(), 1);
    }

    #[test]
    fn test_get_metrics() {
        let mut tracker = SloTracker::new();
        tracker.record(Measurement::new(MetricType::BuildTime, 10.0));
        tracker.record(Measurement::new(MetricType::BuildTime, 12.0));
        tracker.record(Measurement::new(MetricType::BuildTime, 14.0));

        let metrics = tracker.get_metrics(MetricType::BuildTime);
        assert!(metrics.is_some());
        let m = metrics.unwrap();
        assert_eq!(m.current, 14.0);
        assert_eq!(m.min, 10.0);
        assert_eq!(m.max, 14.0);
    }

    #[test]
    fn test_violations_count() {
        let mut tracker = SloTracker::new();
        tracker.record(Measurement::new(MetricType::BuildTime, 12.0));
        tracker.record(Measurement::new(MetricType::BuildTime, 16.0));
        tracker.record(Measurement::new(MetricType::BuildTime, 18.0));

        let violation_count = tracker.count_violations_7d(MetricType::BuildTime, 15.0);
        assert_eq!(violation_count, 2);
    }

    #[test]
    fn test_percentile_calculation() {
        let values = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        let p95 = calculate_percentile(&values, 0.95);
        assert!(p95 > 4.0 && p95 <= 5.0);
    }
}
