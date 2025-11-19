//! Value Stream Mapping Metrics
//!
//! Comprehensive metrics for tracking performance, efficiency, and quality
//! throughout the semantic generation pipeline.

use serde::{Deserialize, Serialize};

/// Timing metrics for a stage (all values in milliseconds)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TimingMetrics {
    /// Total elapsed time from stage entry to exit
    pub lead_time_ms: f64,
    /// Active processing time (value-added work)
    pub process_time_ms: f64,
    /// Idle time waiting for resources or approvals
    pub wait_time_ms: f64,
    /// Average time between stage completions
    pub cycle_time_ms: f64,
    /// Actual hands-on work time
    pub touch_time_ms: f64,
}

impl TimingMetrics {
    /// Create new timing metrics with all values set to zero
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate total non-value-added time
    pub fn non_value_added_time(&self) -> f64 {
        self.wait_time_ms
    }

    /// Calculate value-added ratio (process time / lead time)
    pub fn value_added_ratio(&self) -> f64 {
        if self.lead_time_ms > 0.0 {
            self.process_time_ms / self.lead_time_ms
        } else {
            0.0
        }
    }

    /// Get timing summary as human-readable string
    pub fn summary(&self) -> String {
        format!(
            "Lead: {:.2}ms, Process: {:.2}ms, Wait: {:.2}ms (VA ratio: {:.1}%)",
            self.lead_time_ms,
            self.process_time_ms,
            self.wait_time_ms,
            self.value_added_ratio() * 100.0
        )
    }
}

/// Throughput and capacity metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ThroughputMetrics {
    /// Actual throughput (items per hour)
    pub throughput: f64,
    /// Maximum theoretical capacity (items per hour)
    pub capacity: f64,
    /// Utilization ratio (throughput / capacity)
    pub utilization: f64,
}

impl ThroughputMetrics {
    /// Create new throughput metrics
    pub fn new(throughput: f64, capacity: f64) -> Self {
        let utilization = if capacity > 0.0 {
            throughput / capacity
        } else {
            0.0
        };

        Self {
            throughput,
            capacity,
            utilization,
        }
    }

    /// Check if stage is under-utilized
    pub fn is_underutilized(&self) -> bool {
        self.utilization < 0.5
    }

    /// Check if stage is at capacity
    pub fn is_at_capacity(&self) -> bool {
        self.utilization > 0.9
    }

    /// Get available capacity
    pub fn available_capacity(&self) -> f64 {
        (self.capacity - self.throughput).max(0.0)
    }
}

/// Quality metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct QualityMetrics {
    /// Proportion passing without rework (0.0 to 1.0)
    pub first_pass_yield: f64,
    /// Proportion requiring rework (0.0 to 1.0)
    pub rework_rate: f64,
    /// Defects per thousand opportunities
    pub defect_rate: f64,
    /// Proportion meeting compliance rules (0.0 to 1.0)
    pub compliance_rate: f64,
}

impl QualityMetrics {
    /// Create new quality metrics
    pub fn new(first_pass_yield: f64, defect_rate: f64, compliance_rate: f64) -> Self {
        Self {
            first_pass_yield,
            rework_rate: 1.0 - first_pass_yield,
            defect_rate,
            compliance_rate,
        }
    }

    /// Check if quality is acceptable (>80% first pass yield)
    pub fn is_acceptable(&self) -> bool {
        self.first_pass_yield > 0.8
    }

    /// Get quality score (0.0 to 1.0)
    pub fn quality_score(&self) -> f64 {
        let fpy_weight = 0.5;
        let defect_weight = 0.3;
        let compliance_weight = 0.2;

        // Normalize defect rate (lower is better, cap at 100 DPMO)
        let defect_score = 1.0 - (self.defect_rate / 100.0).min(1.0);

        fpy_weight * self.first_pass_yield
            + defect_weight * defect_score
            + compliance_weight * self.compliance_rate
    }
}

/// Comprehensive metrics for a value stream stage
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Metrics {
    /// Timing metrics
    pub timing: TimingMetrics,
    /// Throughput metrics
    pub throughput: ThroughputMetrics,
    /// Quality metrics
    pub quality: QualityMetrics,
    /// Process efficiency ratio (process time / lead time)
    pub process_efficiency: f64,
}

impl Metrics {
    /// Create new metrics with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Calculate overall efficiency score (0.0 to 1.0)
    pub fn overall_efficiency(&self) -> f64 {
        let timing_weight = 0.4;
        let throughput_weight = 0.3;
        let quality_weight = 0.3;

        timing_weight * self.process_efficiency
            + throughput_weight * self.throughput.utilization
            + quality_weight * self.quality.quality_score()
    }

    /// Get efficiency grade (A, B, C, D, F)
    pub fn efficiency_grade(&self) -> char {
        let score = self.overall_efficiency();
        match score {
            s if s >= 0.9 => 'A',
            s if s >= 0.8 => 'B',
            s if s >= 0.7 => 'C',
            s if s >= 0.6 => 'D',
            _ => 'F',
        }
    }

    /// Get metrics summary
    pub fn summary(&self) -> String {
        format!(
            "Efficiency: {:.1}% ({}), {}, Throughput: {:.1}/{:.1} items/hr, Quality: {:.1}%",
            self.overall_efficiency() * 100.0,
            self.efficiency_grade(),
            self.timing.summary(),
            self.throughput.throughput,
            self.throughput.capacity,
            self.quality.quality_score() * 100.0
        )
    }
}

/// Aggregated metrics across multiple stages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedMetrics {
    /// Total lead time across all stages
    pub total_lead_time_ms: f64,
    /// Total process time across all stages
    pub total_process_time_ms: f64,
    /// Total wait time across all stages
    pub total_wait_time_ms: f64,
    /// Overall process efficiency
    pub overall_efficiency: f64,
    /// Average first-pass yield
    pub average_fpy: f64,
    /// Total throughput
    pub total_throughput: f64,
    /// Number of stages
    pub stage_count: usize,
}

impl AggregatedMetrics {
    /// Create aggregated metrics from individual stage metrics
    pub fn from_stages(metrics: &[Metrics]) -> Self {
        let total_lead_time_ms = metrics.iter().map(|m| m.timing.lead_time_ms).sum();
        let total_process_time_ms = metrics.iter().map(|m| m.timing.process_time_ms).sum();
        let total_wait_time_ms = metrics.iter().map(|m| m.timing.wait_time_ms).sum();

        let overall_efficiency = if total_lead_time_ms > 0.0 {
            total_process_time_ms / total_lead_time_ms
        } else {
            0.0
        };

        let average_fpy = if !metrics.is_empty() {
            metrics.iter().map(|m| m.quality.first_pass_yield).sum::<f64>()
                / metrics.len() as f64
        } else {
            0.0
        };

        let total_throughput = metrics.iter().map(|m| m.throughput.throughput).sum();

        Self {
            total_lead_time_ms,
            total_process_time_ms,
            total_wait_time_ms,
            overall_efficiency,
            average_fpy,
            total_throughput,
            stage_count: metrics.len(),
        }
    }

    /// Get total cycle time (slowest stage's cycle time)
    pub fn total_cycle_time(&self, metrics: &[Metrics]) -> f64 {
        metrics
            .iter()
            .map(|m| m.timing.cycle_time_ms)
            .max_by(|a, b| a.partial_cmp(b).unwrap())
            .unwrap_or(0.0)
    }

    /// Calculate takt time (available time / customer demand)
    pub fn takt_time(&self, available_time_ms: f64, demand: f64) -> f64 {
        if demand > 0.0 {
            available_time_ms / demand
        } else {
            0.0
        }
    }

    /// Summary string
    pub fn summary(&self) -> String {
        format!(
            "{} stages, Total lead time: {:.2}ms, Process efficiency: {:.1}%, Avg FPY: {:.1}%",
            self.stage_count,
            self.total_lead_time_ms,
            self.overall_efficiency * 100.0,
            self.average_fpy * 100.0
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_timing_metrics() {
        let mut timing = TimingMetrics::new();
        timing.lead_time_ms = 1000.0;
        timing.process_time_ms = 600.0;
        timing.wait_time_ms = 400.0;

        assert_eq!(timing.value_added_ratio(), 0.6);
        assert_eq!(timing.non_value_added_time(), 400.0);
    }

    #[test]
    fn test_throughput_metrics() {
        let tp = ThroughputMetrics::new(80.0, 100.0);
        assert_eq!(tp.utilization, 0.8);
        assert!(!tp.is_underutilized());
        assert!(!tp.is_at_capacity());
        assert_eq!(tp.available_capacity(), 20.0);
    }

    #[test]
    fn test_quality_metrics() {
        let quality = QualityMetrics::new(0.9, 5.0, 0.95);
        assert_eq!(quality.first_pass_yield, 0.9);
        assert_eq!(quality.rework_rate, 0.1);
        assert!(quality.is_acceptable());
        assert!(quality.quality_score() > 0.8);
    }

    #[test]
    fn test_overall_metrics() {
        let mut metrics = Metrics::new();
        metrics.process_efficiency = 0.8;
        metrics.throughput = ThroughputMetrics::new(90.0, 100.0);
        metrics.quality = QualityMetrics::new(0.95, 2.0, 1.0);

        let score = metrics.overall_efficiency();
        assert!(score > 0.8);
        assert!(matches!(metrics.efficiency_grade(), 'A' | 'B'));
    }

    #[test]
    fn test_aggregated_metrics() {
        let metrics = vec![
            {
                let mut m = Metrics::new();
                m.timing.lead_time_ms = 1000.0;
                m.timing.process_time_ms = 600.0;
                m.quality.first_pass_yield = 0.9;
                m
            },
            {
                let mut m = Metrics::new();
                m.timing.lead_time_ms = 500.0;
                m.timing.process_time_ms = 400.0;
                m.quality.first_pass_yield = 0.95;
                m
            },
        ];

        let agg = AggregatedMetrics::from_stages(&metrics);
        assert_eq!(agg.total_lead_time_ms, 1500.0);
        assert_eq!(agg.total_process_time_ms, 1000.0);
        assert_eq!(agg.stage_count, 2);
        assert!((agg.average_fpy - 0.925).abs() < 0.001);
    }
}
