//! Improvement history and trend tracking.
//!
//! This module provides structures for tracking improvement history over time
//! and analyzing trends.

use crate::{
    metrics::{AggregatedMetrics, MetricsSnapshot},
    pdca::PdcaCycle,
    Improvement, KaizenError, Result,
};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A historical record of an improvement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementRecord {
    /// The improvement details.
    pub improvement: Improvement,
    /// All PDCA cycles for this improvement.
    pub cycles: Vec<PdcaCycle>,
    /// Metrics snapshots over time.
    pub metrics_snapshots: Vec<MetricsSnapshot>,
    /// When the record was created.
    pub created_at: DateTime<Utc>,
    /// When the record was last updated.
    pub updated_at: DateTime<Utc>,
}

impl ImprovementRecord {
    /// Creates a new improvement record.
    pub fn new(improvement: Improvement) -> Self {
        let now = Utc::now();
        Self {
            improvement,
            cycles: Vec::new(),
            metrics_snapshots: Vec::new(),
            created_at: now,
            updated_at: now,
        }
    }

    /// Adds a PDCA cycle to the record.
    pub fn add_cycle(&mut self, cycle: PdcaCycle) {
        self.cycles.push(cycle);
        self.updated_at = Utc::now();
    }

    /// Adds a metrics snapshot to the record.
    pub fn add_metrics_snapshot(&mut self, snapshot: MetricsSnapshot) {
        self.metrics_snapshots.push(snapshot);
        self.updated_at = Utc::now();
    }

    /// Gets the latest cycle.
    pub fn latest_cycle(&self) -> Option<&PdcaCycle> {
        self.cycles.last()
    }

    /// Gets a mutable reference to the latest cycle.
    pub fn latest_cycle_mut(&mut self) -> Option<&mut PdcaCycle> {
        self.cycles.last_mut()
    }

    /// Gets the latest metrics snapshot.
    pub fn latest_metrics(&self) -> Option<&MetricsSnapshot> {
        self.metrics_snapshots.last()
    }

    /// Counts completed cycles.
    pub fn completed_cycles_count(&self) -> usize {
        self.cycles.iter().filter(|c| c.is_completed()).count()
    }

    /// Calculates the average cycle duration.
    pub fn average_cycle_duration(&self) -> Option<Duration> {
        let durations: Vec<Duration> = self
            .cycles
            .iter()
            .filter_map(|c| c.total_duration())
            .collect();

        if durations.is_empty() {
            return None;
        }

        let total_secs: i64 = durations.iter().map(|d| d.num_seconds()).sum();
        let avg_secs = total_secs / durations.len() as i64;

        Some(Duration::seconds(avg_secs))
    }

    /// Gets metrics snapshots within a time range.
    pub fn metrics_in_range(
        &self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Vec<&MetricsSnapshot> {
        self.metrics_snapshots
            .iter()
            .filter(|s| s.timestamp >= start && s.timestamp <= end)
            .collect()
    }
}

/// Trend direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TrendDirection {
    /// Improving trend.
    Improving,
    /// Stable trend.
    Stable,
    /// Declining trend.
    Declining,
    /// Insufficient data.
    Unknown,
}

/// Trend analysis result.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrendAnalysis {
    /// Direction of the trend.
    pub direction: TrendDirection,
    /// Slope of the trend line.
    pub slope: f64,
    /// Confidence level (0.0 - 1.0).
    pub confidence: f64,
    /// Sample size used for analysis.
    pub sample_size: usize,
}

impl TrendAnalysis {
    /// Creates a new trend analysis.
    pub fn new(slope: f64, sample_size: usize) -> Self {
        let direction = if sample_size < 3 {
            TrendDirection::Unknown
        } else if slope.abs() < 0.01 {
            TrendDirection::Stable
        } else if slope > 0.0 {
            TrendDirection::Improving
        } else {
            TrendDirection::Declining
        };

        let confidence = if sample_size < 3 {
            0.0
        } else {
            (sample_size as f64 / (sample_size as f64 + 10.0)).min(1.0)
        };

        Self {
            direction,
            slope,
            confidence,
            sample_size,
        }
    }
}

/// History manager for tracking all improvements.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementHistory {
    /// Map of improvement ID to record.
    records: HashMap<String, ImprovementRecord>,
}

impl ImprovementHistory {
    /// Creates a new improvement history.
    pub fn new() -> Self {
        Self {
            records: HashMap::new(),
        }
    }

    /// Adds an improvement to the history.
    pub fn add_improvement(&mut self, improvement: Improvement) -> Result<()> {
        let id = improvement.id.clone();
        if self.records.contains_key(&id) {
            return Err(KaizenError::InvalidMetric(format!(
                "Improvement {} already exists",
                id
            )));
        }

        self.records
            .insert(id, ImprovementRecord::new(improvement));
        Ok(())
    }

    /// Gets an improvement record by ID.
    pub fn get_record(&self, id: &str) -> Option<&ImprovementRecord> {
        self.records.get(id)
    }

    /// Gets a mutable reference to an improvement record by ID.
    pub fn get_record_mut(&mut self, id: &str) -> Option<&mut ImprovementRecord> {
        self.records.get_mut(id)
    }

    /// Lists all improvement IDs.
    pub fn list_ids(&self) -> Vec<&String> {
        self.records.keys().collect()
    }

    /// Counts total improvements.
    pub fn total_improvements(&self) -> usize {
        self.records.len()
    }

    /// Counts active improvements (with incomplete cycles).
    pub fn active_improvements(&self) -> usize {
        self.records
            .values()
            .filter(|r| {
                r.latest_cycle()
                    .map(|c| !c.is_completed())
                    .unwrap_or(false)
            })
            .count()
    }

    /// Gets all records sorted by creation date.
    pub fn records_by_date(&self) -> Vec<&ImprovementRecord> {
        let mut records: Vec<&ImprovementRecord> = self.records.values().collect();
        records.sort_by_key(|r| r.created_at);
        records
    }

    /// Analyzes trend for a specific improvement.
    pub fn analyze_improvement_trend(&self, id: &str, metric_name: &str) -> Option<TrendAnalysis> {
        let record = self.get_record(id)?;

        let data: Vec<(f64, f64)> = record
            .metrics_snapshots
            .iter()
            .enumerate()
            .filter_map(|(i, s)| {
                let value = match metric_name {
                    "cycle_time" => s.cycle_time_secs,
                    "defect_rate" => s.defect_rate,
                    "throughput" => s.throughput,
                    "quality_score" => s.quality_score,
                    _ => s.get_custom_metric(metric_name),
                };
                value.map(|v| (i as f64, v))
            })
            .collect();

        if data.len() < 2 {
            return Some(TrendAnalysis::new(0.0, data.len()));
        }

        let slope = self.calculate_slope(&data)?;
        Some(TrendAnalysis::new(slope, data.len()))
    }

    /// Calculates linear regression slope.
    fn calculate_slope(&self, data: &[(f64, f64)]) -> Option<f64> {
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

    /// Gets aggregated metrics across all improvements for a time period.
    pub fn aggregate_all(
        &self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<AggregatedMetrics> {
        let all_snapshots: Vec<MetricsSnapshot> = self
            .records
            .values()
            .flat_map(|r| r.metrics_in_range(start, end))
            .cloned()
            .collect();

        AggregatedMetrics::from_snapshots(&all_snapshots, start, end)
    }

    /// Gets summary statistics.
    pub fn summary(&self) -> HistorySummary {
        let total_cycles: usize = self.records.values().map(|r| r.cycles.len()).sum();
        let completed_cycles: usize = self
            .records
            .values()
            .map(|r| r.completed_cycles_count())
            .sum();

        HistorySummary {
            total_improvements: self.total_improvements(),
            active_improvements: self.active_improvements(),
            total_cycles,
            completed_cycles,
        }
    }
}

impl Default for ImprovementHistory {
    fn default() -> Self {
        Self::new()
    }
}

/// Summary statistics for improvement history.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistorySummary {
    /// Total number of improvements.
    pub total_improvements: usize,
    /// Number of active improvements.
    pub active_improvements: usize,
    /// Total number of cycles.
    pub total_cycles: usize,
    /// Number of completed cycles.
    pub completed_cycles: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Category, Priority};

    fn create_test_improvement(id: &str) -> Improvement {
        Improvement::new(
            id.to_string(),
            "Test Improvement".to_string(),
            "Test Description".to_string(),
            Category::Performance,
            Priority::High,
            "owner@example.com".to_string(),
        )
    }

    #[test]
    fn test_improvement_record_creation() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");

        // Act
        let record = ImprovementRecord::new(improvement);

        // Assert
        assert_eq!(record.improvement.id, "IMP-001");
        assert!(record.cycles.is_empty());
        assert!(record.metrics_snapshots.is_empty());
    }

    #[test]
    fn test_improvement_record_add_cycle() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");
        let mut record = ImprovementRecord::new(improvement.clone());
        let cycle = PdcaCycle::new(improvement, 1);

        // Act
        record.add_cycle(cycle);

        // Assert
        assert_eq!(record.cycles.len(), 1);
    }

    #[test]
    fn test_improvement_record_add_metrics() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");
        let mut record = ImprovementRecord::new(improvement);
        let snapshot = MetricsSnapshot::new().with_cycle_time(10.0).unwrap();

        // Act
        record.add_metrics_snapshot(snapshot);

        // Assert
        assert_eq!(record.metrics_snapshots.len(), 1);
    }

    #[test]
    fn test_improvement_record_latest_cycle() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");
        let mut record = ImprovementRecord::new(improvement.clone());
        let cycle1 = PdcaCycle::new(improvement.clone(), 1);
        let cycle2 = PdcaCycle::new(improvement, 2);

        // Act
        record.add_cycle(cycle1);
        record.add_cycle(cycle2);

        // Assert
        let latest = record.latest_cycle();
        assert!(latest.is_some());
        assert_eq!(latest.unwrap().cycle_number, 2);
    }

    #[test]
    fn test_improvement_record_completed_cycles_count() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");
        let mut record = ImprovementRecord::new(improvement.clone());
        let mut cycle1 = PdcaCycle::new(improvement.clone(), 1);
        cycle1.advance("Do".to_string()).ok();
        cycle1.advance("Check".to_string()).ok();
        cycle1.advance("Act".to_string()).ok();
        cycle1.complete().ok();

        let cycle2 = PdcaCycle::new(improvement, 2);

        // Act
        record.add_cycle(cycle1);
        record.add_cycle(cycle2);

        // Assert
        assert_eq!(record.completed_cycles_count(), 1);
    }

    #[test]
    fn test_trend_analysis_improving() {
        // Arrange & Act
        let analysis = TrendAnalysis::new(0.5, 10);

        // Assert
        assert_eq!(analysis.direction, TrendDirection::Improving);
        assert!(analysis.confidence > 0.0);
    }

    #[test]
    fn test_trend_analysis_declining() {
        // Arrange & Act
        let analysis = TrendAnalysis::new(-0.5, 10);

        // Assert
        assert_eq!(analysis.direction, TrendDirection::Declining);
    }

    #[test]
    fn test_trend_analysis_stable() {
        // Arrange & Act
        let analysis = TrendAnalysis::new(0.005, 10);

        // Assert
        assert_eq!(analysis.direction, TrendDirection::Stable);
    }

    #[test]
    fn test_trend_analysis_unknown() {
        // Arrange & Act
        let analysis = TrendAnalysis::new(0.5, 2);

        // Assert
        assert_eq!(analysis.direction, TrendDirection::Unknown);
        assert_eq!(analysis.confidence, 0.0);
    }

    #[test]
    fn test_improvement_history_add() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement = create_test_improvement("IMP-001");

        // Act
        let result = history.add_improvement(improvement);

        // Assert
        assert!(result.is_ok());
        assert_eq!(history.total_improvements(), 1);
    }

    #[test]
    fn test_improvement_history_duplicate_add() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement1 = create_test_improvement("IMP-001");
        let improvement2 = create_test_improvement("IMP-001");

        // Act
        history.add_improvement(improvement1).ok();
        let result = history.add_improvement(improvement2);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_improvement_history_get_record() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement = create_test_improvement("IMP-001");
        history.add_improvement(improvement).ok();

        // Act
        let record = history.get_record("IMP-001");

        // Assert
        assert!(record.is_some());
        assert_eq!(record.unwrap().improvement.id, "IMP-001");
    }

    #[test]
    fn test_improvement_history_list_ids() {
        // Arrange
        let mut history = ImprovementHistory::new();
        history.add_improvement(create_test_improvement("IMP-001")).ok();
        history.add_improvement(create_test_improvement("IMP-002")).ok();

        // Act
        let ids = history.list_ids();

        // Assert
        assert_eq!(ids.len(), 2);
    }

    #[test]
    fn test_improvement_history_active_improvements() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement = create_test_improvement("IMP-001");
        history.add_improvement(improvement.clone()).ok();

        let cycle = PdcaCycle::new(improvement, 1);
        if let Some(record) = history.get_record_mut("IMP-001") {
            record.add_cycle(cycle);
        }

        // Act
        let active = history.active_improvements();

        // Assert
        assert_eq!(active, 1);
    }

    #[test]
    fn test_improvement_history_summary() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement = create_test_improvement("IMP-001");
        history.add_improvement(improvement.clone()).ok();

        let mut cycle = PdcaCycle::new(improvement, 1);
        cycle.advance("Do".to_string()).ok();
        cycle.advance("Check".to_string()).ok();
        cycle.advance("Act".to_string()).ok();
        cycle.complete().ok();

        if let Some(record) = history.get_record_mut("IMP-001") {
            record.add_cycle(cycle);
        }

        // Act
        let summary = history.summary();

        // Assert
        assert_eq!(summary.total_improvements, 1);
        assert_eq!(summary.total_cycles, 1);
        assert_eq!(summary.completed_cycles, 1);
    }

    #[test]
    fn test_improvement_history_trend_analysis() {
        // Arrange
        let mut history = ImprovementHistory::new();
        let improvement = create_test_improvement("IMP-001");
        history.add_improvement(improvement).ok();

        if let Some(record) = history.get_record_mut("IMP-001") {
            record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
            record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(9.0).unwrap());
            record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());
        }

        // Act
        let trend = history.analyze_improvement_trend("IMP-001", "cycle_time");

        // Assert
        assert!(trend.is_some());
        let analysis = trend.unwrap();
        assert!(analysis.slope < 0.0); // Improving (lower is better)
    }

    #[test]
    fn test_metrics_in_range() {
        // Arrange
        let improvement = create_test_improvement("IMP-001");
        let mut record = ImprovementRecord::new(improvement);
        let now = Utc::now();

        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());

        // Act
        let past = now - Duration::hours(1);
        let future = now + Duration::hours(1);
        let metrics = record.metrics_in_range(past, future);

        // Assert
        assert_eq!(metrics.len(), 1);
    }
}
