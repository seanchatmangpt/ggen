// Cumulative flow diagrams for visualizing generation bottlenecks

use super::{WorkflowStage, KanbanBoard, KanbanError, KanbanResult};
use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, BTreeMap};

/// Data point for cumulative flow diagram
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowDataPoint {
    pub timestamp: DateTime<Utc>,
    pub stage_counts: HashMap<WorkflowStage, usize>,
    pub total_wip: usize,
}

impl FlowDataPoint {
    /// Create a new data point from the current board state
    pub fn from_board(board: &KanbanBoard) -> Self {
        let mut stage_counts = HashMap::new();
        let mut total_wip = 0;

        for (stage, state) in &board.stages {
            let count = state.wip_count();
            stage_counts.insert(*stage, count);
            total_wip += count;
        }

        Self {
            timestamp: Utc::now(),
            stage_counts,
            total_wip,
        }
    }

    /// Get count for a specific stage
    pub fn get_stage_count(&self, stage: WorkflowStage) -> usize {
        self.stage_counts.get(&stage).copied().unwrap_or(0)
    }
}

/// Cumulative flow diagram tracking WIP over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CumulativeFlowDiagram {
    /// Time-series data points
    data_points: BTreeMap<i64, FlowDataPoint>, // Timestamp (seconds) -> DataPoint
    /// Maximum number of data points to retain
    max_data_points: usize,
    /// Sampling interval in seconds
    sampling_interval: u64,
    /// Last sample timestamp
    last_sample: Option<DateTime<Utc>>,
}

impl CumulativeFlowDiagram {
    /// Create a new cumulative flow diagram
    pub fn new(max_data_points: usize, sampling_interval: u64) -> Self {
        Self {
            data_points: BTreeMap::new(),
            max_data_points,
            sampling_interval,
            last_sample: None,
        }
    }

    /// Add a data point from the current board state
    pub fn sample(&mut self, board: &KanbanBoard) -> KanbanResult<()> {
        let now = Utc::now();

        // Check if enough time has passed since last sample
        if let Some(last) = self.last_sample {
            let elapsed = now.signed_duration_since(last).num_seconds() as u64;
            if elapsed < self.sampling_interval {
                return Ok(()); // Skip sampling
            }
        }

        let data_point = FlowDataPoint::from_board(board);
        let timestamp_key = now.timestamp();

        self.data_points.insert(timestamp_key, data_point);
        self.last_sample = Some(now);

        // Trim old data points if exceeded max
        while self.data_points.len() > self.max_data_points {
            if let Some(first_key) = self.data_points.keys().next().copied() {
                self.data_points.remove(&first_key);
            }
        }

        Ok(())
    }

    /// Get all data points in chronological order
    pub fn get_data_points(&self) -> Vec<&FlowDataPoint> {
        self.data_points.values().collect()
    }

    /// Get data points within a time range
    pub fn get_data_points_range(
        &self,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Vec<&FlowDataPoint> {
        let start_ts = start.timestamp();
        let end_ts = end.timestamp();

        self.data_points
            .range(start_ts..=end_ts)
            .map(|(_, dp)| dp)
            .collect()
    }

    /// Calculate average WIP over the entire history
    pub fn average_wip(&self) -> f64 {
        if self.data_points.is_empty() {
            return 0.0;
        }

        let total: usize = self.data_points.values().map(|dp| dp.total_wip).sum();
        total as f64 / self.data_points.len() as f64
    }

    /// Calculate average WIP per stage
    pub fn average_wip_per_stage(&self) -> HashMap<WorkflowStage, f64> {
        if self.data_points.is_empty() {
            return HashMap::new();
        }

        let mut stage_totals: HashMap<WorkflowStage, usize> = HashMap::new();

        for dp in self.data_points.values() {
            for (stage, count) in &dp.stage_counts {
                *stage_totals.entry(*stage).or_insert(0) += count;
            }
        }

        let count = self.data_points.len() as f64;
        stage_totals
            .into_iter()
            .map(|(stage, total)| (stage, total as f64 / count))
            .collect()
    }

    /// Get the trend direction for each stage (growing, shrinking, stable)
    pub fn stage_trends(&self) -> HashMap<WorkflowStage, Trend> {
        if self.data_points.len() < 2 {
            return HashMap::new();
        }

        let data_points: Vec<_> = self.data_points.values().collect();
        let recent_count = (self.data_points.len() / 2).max(1);
        let recent = &data_points[data_points.len() - recent_count..];
        let earlier = &data_points[..data_points.len() - recent_count];

        let mut trends = HashMap::new();

        for stage in [
            WorkflowStage::Backlog,
            WorkflowStage::Analysis,
            WorkflowStage::Transformation,
            WorkflowStage::Validation,
            WorkflowStage::Generation,
            WorkflowStage::Done,
        ] {
            let recent_avg = average_stage_count(recent, stage);
            let earlier_avg = average_stage_count(earlier, stage);

            let trend = if recent_avg > earlier_avg * 1.2 {
                Trend::Growing
            } else if recent_avg < earlier_avg * 0.8 {
                Trend::Shrinking
            } else {
                Trend::Stable
            };

            trends.insert(stage, trend);
        }

        trends
    }

    /// Export data as CSV format
    pub fn to_csv(&self) -> String {
        let mut csv = String::from("Timestamp,Backlog,Analysis,Transformation,Validation,Generation,Done,Total\n");

        for dp in self.data_points.values() {
            csv.push_str(&format!(
                "{},{},{},{},{},{},{},{}\n",
                dp.timestamp.to_rfc3339(),
                dp.get_stage_count(WorkflowStage::Backlog),
                dp.get_stage_count(WorkflowStage::Analysis),
                dp.get_stage_count(WorkflowStage::Transformation),
                dp.get_stage_count(WorkflowStage::Validation),
                dp.get_stage_count(WorkflowStage::Generation),
                dp.get_stage_count(WorkflowStage::Done),
                dp.total_wip,
            ));
        }

        csv
    }

    /// Export data as JSON
    pub fn to_json(&self) -> serde_json::Result<String> {
        let data_points: Vec<_> = self.data_points.values().collect();
        serde_json::to_string_pretty(&data_points)
    }
}

impl Default for CumulativeFlowDiagram {
    fn default() -> Self {
        Self::new(1000, 60) // 1000 points, 60 second intervals
    }
}

/// Trend direction for a workflow stage
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Trend {
    Growing,
    Shrinking,
    Stable,
}

/// Calculate average count for a stage across data points
fn average_stage_count(data_points: &[&FlowDataPoint], stage: WorkflowStage) -> f64 {
    if data_points.is_empty() {
        return 0.0;
    }

    let total: usize = data_points
        .iter()
        .map(|dp| dp.get_stage_count(stage))
        .sum();

    total as f64 / data_points.len() as f64
}

/// Analyzer for detecting bottlenecks from flow data
pub struct BottleneckAnalyzer;

impl BottleneckAnalyzer {
    /// Identify bottlenecks from cumulative flow diagram
    pub fn analyze(cfd: &CumulativeFlowDiagram) -> Vec<BottleneckAnalysis> {
        let mut analyses = Vec::new();

        // Check for growing queues (potential bottlenecks)
        let trends = cfd.stage_trends();
        let avg_wip = cfd.average_wip_per_stage();

        for (stage, trend) in trends {
            if stage == WorkflowStage::Backlog || stage == WorkflowStage::Done {
                continue; // Skip these stages
            }

            let avg = avg_wip.get(&stage).copied().unwrap_or(0.0);

            match trend {
                Trend::Growing => {
                    analyses.push(BottleneckAnalysis {
                        stage,
                        severity: if avg > 5.0 {
                            BottleneckSeverityLevel::High
                        } else {
                            BottleneckSeverityLevel::Medium
                        },
                        description: format!(
                            "Queue growing in {} stage (avg WIP: {:.1})",
                            stage.as_str(),
                            avg
                        ),
                        recommendation: "Increase capacity or reduce WIP limit upstream".to_string(),
                        avg_wip: avg,
                        trend,
                    });
                }
                Trend::Stable => {
                    if avg > 8.0 {
                        analyses.push(BottleneckAnalysis {
                            stage,
                            severity: BottleneckSeverityLevel::Medium,
                            description: format!(
                                "High stable WIP in {} stage ({:.1} items)",
                                stage.as_str(),
                                avg
                            ),
                            recommendation: "Review process capacity".to_string(),
                            avg_wip: avg,
                            trend,
                        });
                    }
                }
                Trend::Shrinking => {
                    // Shrinking is generally good, but check for starvation
                    if avg < 0.5 {
                        analyses.push(BottleneckAnalysis {
                            stage,
                            severity: BottleneckSeverityLevel::Low,
                            description: format!(
                                "Potential starvation in {} stage (avg WIP: {:.1})",
                                stage.as_str(),
                                avg
                            ),
                            recommendation: "Ensure upstream stages are producing work".to_string(),
                            avg_wip: avg,
                            trend,
                        });
                    }
                }
            }
        }

        analyses
    }

    /// Calculate flow debt (accumulated WIP that indicates inefficiency)
    pub fn calculate_flow_debt(cfd: &CumulativeFlowDiagram) -> FlowDebt {
        let avg_wip = cfd.average_wip_per_stage();

        // Ideal WIP per stage (based on Little's Law and typical throughput)
        let ideal_wip: HashMap<WorkflowStage, f64> = [
            (WorkflowStage::Analysis, 2.0),
            (WorkflowStage::Transformation, 2.0),
            (WorkflowStage::Validation, 3.0),
            (WorkflowStage::Generation, 1.0),
        ]
        .iter()
        .cloned()
        .collect();

        let mut total_debt = 0.0;
        let mut stage_debts = HashMap::new();

        for (stage, ideal) in ideal_wip {
            let actual = avg_wip.get(&stage).copied().unwrap_or(0.0);
            let debt = (actual - ideal).max(0.0);
            total_debt += debt;
            stage_debts.insert(stage, debt);
        }

        FlowDebt {
            total_debt,
            stage_debts,
            debt_ratio: if total_debt > 0.0 {
                total_debt / cfd.average_wip()
            } else {
                0.0
            },
        }
    }
}

/// Analysis of a bottleneck in the workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BottleneckAnalysis {
    pub stage: WorkflowStage,
    pub severity: BottleneckSeverityLevel,
    pub description: String,
    pub recommendation: String,
    pub avg_wip: f64,
    pub trend: Trend,
}

/// Severity level for bottleneck analysis
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BottleneckSeverityLevel {
    Low,
    Medium,
    High,
    Critical,
}

/// Flow debt metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowDebt {
    /// Total excess WIP across all stages
    pub total_debt: f64,
    /// Excess WIP per stage
    pub stage_debts: HashMap<WorkflowStage, f64>,
    /// Ratio of debt to total WIP
    pub debt_ratio: f64,
}

/// Time-series data for flow visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowData {
    pub timestamps: Vec<DateTime<Utc>>,
    pub backlog: Vec<usize>,
    pub analysis: Vec<usize>,
    pub transformation: Vec<usize>,
    pub validation: Vec<usize>,
    pub generation: Vec<usize>,
    pub done: Vec<usize>,
}

impl FlowData {
    /// Create flow data from cumulative flow diagram
    pub fn from_cfd(cfd: &CumulativeFlowDiagram) -> Self {
        let mut data = Self {
            timestamps: Vec::new(),
            backlog: Vec::new(),
            analysis: Vec::new(),
            transformation: Vec::new(),
            validation: Vec::new(),
            generation: Vec::new(),
            done: Vec::new(),
        };

        for dp in cfd.get_data_points() {
            data.timestamps.push(dp.timestamp);
            data.backlog.push(dp.get_stage_count(WorkflowStage::Backlog));
            data.analysis.push(dp.get_stage_count(WorkflowStage::Analysis));
            data.transformation.push(dp.get_stage_count(WorkflowStage::Transformation));
            data.validation.push(dp.get_stage_count(WorkflowStage::Validation));
            data.generation.push(dp.get_stage_count(WorkflowStage::Generation));
            data.done.push(dp.get_stage_count(WorkflowStage::Done));
        }

        data
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::{KanbanBoard, KanbanConfig};

    #[test]
    fn test_flow_data_point_creation() {
        let config = KanbanConfig::default();
        let board = KanbanBoard::new(config.wip_limits);

        let data_point = FlowDataPoint::from_board(&board);
        assert_eq!(data_point.total_wip, 0);
    }

    #[test]
    fn test_cumulative_flow_diagram_sampling() {
        let mut cfd = CumulativeFlowDiagram::new(10, 1); // 1 second interval for testing
        let config = KanbanConfig::default();
        let board = KanbanBoard::new(config.wip_limits);

        // First sample should succeed
        assert!(cfd.sample(&board).is_ok());
        assert_eq!(cfd.get_data_points().len(), 1);

        // Immediate second sample should be skipped
        assert!(cfd.sample(&board).is_ok());
        assert_eq!(cfd.get_data_points().len(), 1);
    }

    #[test]
    fn test_average_wip_calculation() {
        let mut cfd = CumulativeFlowDiagram::new(10, 0); // No interval for testing
        let config = KanbanConfig::default();
        let mut board = KanbanBoard::new(config.wip_limits);

        // Add some cards
        board.stages.get_mut(&WorkflowStage::Analysis)
            .unwrap()
            .add_card("card-1".to_string())
            .unwrap();

        cfd.sample(&board).unwrap();

        board.stages.get_mut(&WorkflowStage::Analysis)
            .unwrap()
            .add_card("card-2".to_string())
            .unwrap();

        cfd.last_sample = None; // Force new sample
        cfd.sample(&board).unwrap();

        let avg = cfd.average_wip();
        assert_eq!(avg, 1.5); // (1 + 2) / 2
    }

    #[test]
    fn test_stage_trends() {
        let mut cfd = CumulativeFlowDiagram::new(100, 0);
        let config = KanbanConfig::default();
        let mut board = KanbanBoard::new(config.wip_limits);

        // Create growing trend in Analysis
        for i in 1..=4 {
            board.stages.get_mut(&WorkflowStage::Analysis)
                .unwrap()
                .card_ids
                .push(format!("card-{}", i));

            cfd.last_sample = None;
            cfd.sample(&board).unwrap();
        }

        let trends = cfd.stage_trends();
        assert_eq!(trends.get(&WorkflowStage::Analysis), Some(&Trend::Growing));
    }

    #[test]
    fn test_csv_export() {
        let mut cfd = CumulativeFlowDiagram::new(10, 0);
        let config = KanbanConfig::default();
        let board = KanbanBoard::new(config.wip_limits);

        cfd.sample(&board).unwrap();

        let csv = cfd.to_csv();
        assert!(csv.contains("Timestamp"));
        assert!(csv.contains("Backlog"));
        assert!(csv.contains("Analysis"));
    }

    #[test]
    fn test_bottleneck_analysis() {
        let mut cfd = CumulativeFlowDiagram::new(100, 0);
        let config = KanbanConfig::default();
        let mut board = KanbanBoard::new(config.wip_limits);

        // Create a bottleneck in Transformation
        for i in 1..=6 {
            board.stages.get_mut(&WorkflowStage::Transformation)
                .unwrap()
                .card_ids
                .push(format!("card-{}", i));

            cfd.last_sample = None;
            cfd.sample(&board).unwrap();
        }

        let analyses = BottleneckAnalyzer::analyze(&cfd);
        assert!(!analyses.is_empty());

        // Should detect high WIP in transformation
        assert!(analyses.iter().any(|a| a.stage == WorkflowStage::Transformation));
    }

    #[test]
    fn test_flow_debt_calculation() {
        let mut cfd = CumulativeFlowDiagram::new(10, 0);
        let config = KanbanConfig::default();
        let mut board = KanbanBoard::new(config.wip_limits);

        // Add excessive WIP
        for i in 1..=10 {
            board.stages.get_mut(&WorkflowStage::Analysis)
                .unwrap()
                .card_ids
                .push(format!("card-{}", i));
        }

        cfd.sample(&board).unwrap();

        let debt = BottleneckAnalyzer::calculate_flow_debt(&cfd);
        assert!(debt.total_debt > 0.0); // Should have debt
    }
}
