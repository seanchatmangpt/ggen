// Flow efficiency metrics for semantic transformations

use super::{WorkflowStage, WorkflowCard, KanbanBoard};
use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Metrics for measuring flow efficiency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetrics {
    /// Cycle time metrics per stage (in seconds)
    pub cycle_times: HashMap<String, CycleTimeMetrics>,
    /// Lead time (end-to-end time)
    pub lead_time_avg: f64,
    /// Throughput (items completed per hour)
    pub throughput: f64,
    /// Work-in-progress (current items in process)
    pub wip: usize,
    /// Flow efficiency ratio (value-added time / total time)
    pub flow_efficiency: f64,
    /// Number of completed items
    pub completed_count: usize,
    /// Number of blocked items
    pub blocked_count: usize,
    /// Timestamp of metrics calculation
    pub timestamp: DateTime<Utc>,
}

/// Cycle time metrics for a specific stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CycleTimeMetrics {
    pub average: f64,
    pub median: f64,
    pub p85: f64, // 85th percentile
    pub p95: f64, // 95th percentile
    pub min: f64,
    pub max: f64,
    pub sample_count: usize,
}

impl CycleTimeMetrics {
    /// Calculate metrics from a list of cycle times
    pub fn from_samples(mut samples: Vec<f64>) -> Self {
        if samples.is_empty() {
            return Self::default();
        }

        samples.sort_by(|a, b| a.partial_cmp(b).unwrap());

        let sum: f64 = samples.iter().sum();
        let average = sum / samples.len() as f64;

        let median = percentile(&samples, 0.5);
        let p85 = percentile(&samples, 0.85);
        let p95 = percentile(&samples, 0.95);
        let min = samples[0];
        let max = samples[samples.len() - 1];

        Self {
            average,
            median,
            p85,
            p95,
            min,
            max,
            sample_count: samples.len(),
        }
    }
}

impl Default for CycleTimeMetrics {
    fn default() -> Self {
        Self {
            average: 0.0,
            median: 0.0,
            p85: 0.0,
            p95: 0.0,
            min: 0.0,
            max: 0.0,
            sample_count: 0,
        }
    }
}

/// Calculate percentile from sorted samples
fn percentile(sorted_samples: &[f64], p: f64) -> f64 {
    if sorted_samples.is_empty() {
        return 0.0;
    }

    let index = (p * (sorted_samples.len() - 1) as f64) as usize;
    sorted_samples[index]
}

/// Metrics specific to semantic transformations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransformationMetrics {
    /// Average number of RDF changes per card
    pub avg_rdf_changes: f64,
    /// Average number of affected templates
    pub avg_affected_templates: f64,
    /// Average number of affected files
    pub avg_affected_files: f64,
    /// Transformation success rate (0.0 to 1.0)
    pub success_rate: f64,
    /// Average time spent in transformation stage (seconds)
    pub avg_transformation_time: f64,
    /// Validation failure rate (0.0 to 1.0)
    pub validation_failure_rate: f64,
}

impl TransformationMetrics {
    /// Calculate transformation metrics from completed cards
    pub fn from_cards(cards: &[&WorkflowCard]) -> Self {
        if cards.is_empty() {
            return Self::default();
        }

        let total_rdf_changes: usize = cards.iter().map(|c| c.rdf_changes.len()).sum();
        let total_templates: usize = cards.iter().map(|c| c.affected_templates.len()).sum();
        let total_files: usize = cards.iter().map(|c| c.affected_files.len()).sum();

        let avg_rdf_changes = total_rdf_changes as f64 / cards.len() as f64;
        let avg_affected_templates = total_templates as f64 / cards.len() as f64;
        let avg_affected_files = total_files as f64 / cards.len() as f64;

        // Calculate transformation time
        let transformation_times: Vec<f64> = cards
            .iter()
            .filter_map(|c| c.cycle_times.get("transformation").copied())
            .collect();

        let avg_transformation_time = if transformation_times.is_empty() {
            0.0
        } else {
            transformation_times.iter().sum::<f64>() / transformation_times.len() as f64
        };

        // Success rate (cards that completed vs total)
        let completed = cards
            .iter()
            .filter(|c| c.stage == WorkflowStage::Done)
            .count();
        let success_rate = completed as f64 / cards.len() as f64;

        // Validation failures (cards that went back from validation)
        let validation_rework = cards
            .iter()
            .filter(|c| {
                c.cycle_times.contains_key("validation")
                    && c.cycle_times.len() > 5 // Went through validation multiple times
            })
            .count();
        let validation_failure_rate = validation_rework as f64 / cards.len() as f64;

        Self {
            avg_rdf_changes,
            avg_affected_templates,
            avg_affected_files,
            success_rate,
            avg_transformation_time,
            validation_failure_rate,
        }
    }
}

impl Default for TransformationMetrics {
    fn default() -> Self {
        Self {
            avg_rdf_changes: 0.0,
            avg_affected_templates: 0.0,
            avg_affected_files: 0.0,
            success_rate: 0.0,
            avg_transformation_time: 0.0,
            validation_failure_rate: 0.0,
        }
    }
}

/// Calculator for flow efficiency metrics
pub struct EfficiencyCalculator;

impl EfficiencyCalculator {
    /// Calculate comprehensive flow metrics from Kanban board and cards
    pub fn calculate(
        board: &KanbanBoard,
        cards: &[WorkflowCard],
    ) -> FlowMetrics {
        let now = Utc::now();

        // Calculate cycle times per stage
        let mut cycle_times = HashMap::new();
        for stage in [
            WorkflowStage::Analysis,
            WorkflowStage::Transformation,
            WorkflowStage::Validation,
            WorkflowStage::Generation,
        ] {
            let samples: Vec<f64> = cards
                .iter()
                .filter_map(|c| c.cycle_times.get(stage.as_str()).copied())
                .collect();

            cycle_times.insert(
                stage.as_str().to_string(),
                CycleTimeMetrics::from_samples(samples),
            );
        }

        // Calculate average lead time
        let lead_times: Vec<f64> = cards
            .iter()
            .filter_map(|c| c.lead_time())
            .map(|d| d.num_seconds() as f64)
            .collect();

        let lead_time_avg = if lead_times.is_empty() {
            0.0
        } else {
            lead_times.iter().sum::<f64>() / lead_times.len() as f64
        };

        // Calculate throughput (completed items in last hour)
        let one_hour_ago = now - Duration::hours(1);
        let completed_last_hour = cards
            .iter()
            .filter(|c| {
                c.completed_at
                    .map(|t| t > one_hour_ago)
                    .unwrap_or(false)
            })
            .count();

        let throughput = completed_last_hour as f64;

        // Calculate WIP
        let wip = board.total_wip();

        // Calculate flow efficiency
        // Flow efficiency = (value-added time) / (total lead time)
        // We consider transformation and generation as value-added stages
        let flow_efficiency = Self::calculate_flow_efficiency(cards);

        // Count completed and blocked items
        let completed_count = cards
            .iter()
            .filter(|c| c.stage == WorkflowStage::Done)
            .count();

        let blocked_count = cards
            .iter()
            .filter(|c| c.status == super::card::CardStatus::Blocked)
            .count();

        FlowMetrics {
            cycle_times,
            lead_time_avg,
            throughput,
            wip,
            flow_efficiency,
            completed_count,
            blocked_count,
            timestamp: now,
        }
    }

    /// Calculate flow efficiency ratio
    fn calculate_flow_efficiency(cards: &[WorkflowCard]) -> f64 {
        let mut total_value_added = 0.0;
        let mut total_lead = 0.0;

        for card in cards {
            // Value-added stages: transformation and generation
            let value_added = card.cycle_times.get("transformation").copied().unwrap_or(0.0)
                + card.cycle_times.get("generation").copied().unwrap_or(0.0);

            let lead = card
                .lead_time()
                .map(|d| d.num_seconds() as f64)
                .unwrap_or(0.0);

            if lead > 0.0 {
                total_value_added += value_added;
                total_lead += lead;
            }
        }

        if total_lead > 0.0 {
            total_value_added / total_lead
        } else {
            0.0
        }
    }

    /// Calculate Little's Law metrics
    /// Little's Law: WIP = Throughput × Lead Time
    pub fn littles_law(
        wip: f64,
        throughput: f64,
        lead_time: f64,
    ) -> LittlesLawMetrics {
        let expected_wip = throughput * lead_time / 3600.0; // Convert to hours
        let expected_lead_time = if throughput > 0.0 {
            (wip * 3600.0) / throughput
        } else {
            0.0
        };

        let wip_variance = ((wip - expected_wip) / expected_wip.max(1.0)) * 100.0;
        let lead_time_variance = if lead_time > 0.0 {
            ((lead_time - expected_lead_time) / lead_time) * 100.0
        } else {
            0.0
        };

        LittlesLawMetrics {
            actual_wip: wip,
            expected_wip,
            actual_lead_time: lead_time,
            expected_lead_time,
            wip_variance_percent: wip_variance,
            lead_time_variance_percent: lead_time_variance,
        }
    }

    /// Identify bottlenecks based on metrics
    pub fn identify_bottlenecks(metrics: &FlowMetrics) -> Vec<Bottleneck> {
        let mut bottlenecks = Vec::new();

        // Check cycle times for outliers
        for (stage, cycle_metrics) in &metrics.cycle_times {
            if cycle_metrics.p95 > cycle_metrics.average * 2.0 {
                bottlenecks.push(Bottleneck {
                    stage: stage.clone(),
                    severity: BottleneckSeverity::High,
                    reason: format!(
                        "High variance in cycle time (P95: {:.1}s, Avg: {:.1}s)",
                        cycle_metrics.p95, cycle_metrics.average
                    ),
                    recommendation: "Investigate outliers and standardize process".to_string(),
                });
            }
        }

        // Check flow efficiency
        if metrics.flow_efficiency < 0.3 {
            bottlenecks.push(Bottleneck {
                stage: "overall".to_string(),
                severity: BottleneckSeverity::Critical,
                reason: format!(
                    "Low flow efficiency: {:.1}% (target: >30%)",
                    metrics.flow_efficiency * 100.0
                ),
                recommendation: "Reduce wait times between stages".to_string(),
            });
        }

        // Check WIP vs throughput ratio
        if metrics.wip > 0 && metrics.throughput > 0.0 {
            let wip_to_throughput = metrics.wip as f64 / metrics.throughput;
            if wip_to_throughput > 5.0 {
                bottlenecks.push(Bottleneck {
                    stage: "overall".to_string(),
                    severity: BottleneckSeverity::Medium,
                    reason: format!(
                        "High WIP-to-throughput ratio: {:.1}",
                        wip_to_throughput
                    ),
                    recommendation: "Reduce WIP limits to improve flow".to_string(),
                });
            }
        }

        bottlenecks
    }
}

/// Little's Law validation metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LittlesLawMetrics {
    pub actual_wip: f64,
    pub expected_wip: f64,
    pub actual_lead_time: f64,
    pub expected_lead_time: f64,
    pub wip_variance_percent: f64,
    pub lead_time_variance_percent: f64,
}

/// Identified bottleneck in the workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bottleneck {
    pub stage: String,
    pub severity: BottleneckSeverity,
    pub reason: String,
    pub recommendation: String,
}

/// Severity of a bottleneck
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BottleneckSeverity {
    Low,
    Medium,
    High,
    Critical,
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::card::{WorkflowCard, Priority};

    #[test]
    fn test_cycle_time_metrics() {
        let samples = vec![10.0, 20.0, 30.0, 40.0, 50.0];
        let metrics = CycleTimeMetrics::from_samples(samples);

        assert_eq!(metrics.average, 30.0);
        assert_eq!(metrics.median, 30.0);
        assert_eq!(metrics.min, 10.0);
        assert_eq!(metrics.max, 50.0);
        assert_eq!(metrics.sample_count, 5);
    }

    #[test]
    fn test_transformation_metrics() {
        let mut card1 = WorkflowCard::new(
            "Card 1".to_string(),
            "Test".to_string(),
            Priority::High,
        );
        card1.add_rdf_change(super::super::card::RdfChangeType::ClassAdded {
            class_uri: "test".to_string(),
        });
        card1.add_affected_template("template1.hbs".to_string());

        let mut card2 = WorkflowCard::new(
            "Card 2".to_string(),
            "Test".to_string(),
            Priority::Medium,
        );
        card2.add_affected_template("template2.hbs".to_string());

        let cards = vec![&card1, &card2];
        let metrics = TransformationMetrics::from_cards(&cards);

        assert_eq!(metrics.avg_rdf_changes, 0.5); // 1 change across 2 cards
        assert_eq!(metrics.avg_affected_templates, 1.0); // 2 templates across 2 cards
    }

    #[test]
    fn test_flow_efficiency_calculation() {
        let mut card = WorkflowCard::new(
            "Test".to_string(),
            "Test".to_string(),
            Priority::High,
        );

        card.cycle_times.insert("transformation".to_string(), 100.0);
        card.cycle_times.insert("generation".to_string(), 100.0);
        card.started_at = Some(Utc::now() - Duration::seconds(500));
        card.completed_at = Some(Utc::now());

        let cards = vec![card];
        let efficiency = EfficiencyCalculator::calculate_flow_efficiency(&cards);

        // Efficiency should be 200 / 500 = 0.4
        assert!((efficiency - 0.4).abs() < 0.1);
    }

    #[test]
    fn test_littles_law() {
        let metrics = EfficiencyCalculator::littles_law(
            10.0,  // WIP
            2.0,   // Throughput (items/hour)
            18000.0, // Lead time (5 hours in seconds)
        );

        // Expected WIP = 2 items/hour * 5 hours = 10 items
        assert!((metrics.expected_wip - 10.0).abs() < 0.1);
    }

    #[test]
    fn test_bottleneck_identification() {
        let mut cycle_times = HashMap::new();
        cycle_times.insert(
            "transformation".to_string(),
            CycleTimeMetrics {
                average: 100.0,
                median: 100.0,
                p85: 150.0,
                p95: 500.0, // Very high P95
                min: 50.0,
                max: 500.0,
                sample_count: 10,
            },
        );

        let metrics = FlowMetrics {
            cycle_times,
            lead_time_avg: 1000.0,
            throughput: 1.0,
            wip: 10,
            flow_efficiency: 0.2, // Low efficiency
            completed_count: 5,
            blocked_count: 2,
            timestamp: Utc::now(),
        };

        let bottlenecks = EfficiencyCalculator::identify_bottlenecks(&metrics);
        assert!(!bottlenecks.is_empty());

        // Should detect low flow efficiency
        assert!(bottlenecks.iter().any(|b| b.severity == BottleneckSeverity::Critical));
    }
}
