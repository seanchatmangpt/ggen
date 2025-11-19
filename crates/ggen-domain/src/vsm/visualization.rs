//! Visualization Data Structures for Value Stream Mapping
//!
//! Provides data structures optimized for rendering VSM charts, diagrams, and dashboards.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::bottleneck::{Bottleneck, BottleneckSeverity};
use super::flow::ValueStream;
use super::stage::{Stage, StageType};
use super::swim_lane::StakeholderRole;

/// Complete visualization data for rendering VSM dashboard
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualizationData {
    /// Lead time chart data
    pub lead_time_chart: LeadTimeChart,
    /// Efficiency chart data
    pub efficiency_chart: EfficiencyChart,
    /// Bottleneck chart data
    pub bottleneck_chart: BottleneckChart,
    /// Swim lane view data
    pub swim_lane_view: SwimLaneView,
    /// Process flow diagram
    pub flow_diagram: FlowDiagram,
    /// Summary statistics
    pub summary_stats: SummaryStats,
}

impl VisualizationData {
    /// Generate visualization data from value stream
    pub fn from_value_stream(stream: &ValueStream) -> Self {
        Self {
            lead_time_chart: LeadTimeChart::from_stream(stream),
            efficiency_chart: EfficiencyChart::from_stream(stream),
            bottleneck_chart: BottleneckChart::from_stream(stream),
            swim_lane_view: SwimLaneView::from_stream(stream),
            flow_diagram: FlowDiagram::from_stream(stream),
            summary_stats: SummaryStats::from_stream(stream),
        }
    }
}

/// Lead time breakdown by stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LeadTimeChart {
    /// Stage names (x-axis labels)
    pub stages: Vec<String>,
    /// Lead time values in ms (y-axis)
    pub lead_times: Vec<f64>,
    /// Process time values in ms
    pub process_times: Vec<f64>,
    /// Wait time values in ms
    pub wait_times: Vec<f64>,
    /// Colors for each bar
    pub colors: Vec<String>,
    /// Total lead time
    pub total_lead_time: f64,
}

impl LeadTimeChart {
    fn from_stream(stream: &ValueStream) -> Self {
        let stages: Vec<String> = stream
            .stages
            .iter()
            .map(|s| s.stage_type.to_string())
            .collect();

        let lead_times: Vec<f64> = stream
            .stages
            .iter()
            .map(|s| s.metrics.timing.lead_time_ms)
            .collect();

        let process_times: Vec<f64> = stream
            .stages
            .iter()
            .map(|s| s.metrics.timing.process_time_ms)
            .collect();

        let wait_times: Vec<f64> = stream
            .stages
            .iter()
            .map(|s| s.metrics.timing.wait_time_ms)
            .collect();

        // Color by efficiency: green (high), yellow (medium), red (low)
        let colors: Vec<String> = stream
            .stages
            .iter()
            .map(|s| {
                if s.metrics.process_efficiency > 0.7 {
                    "#2ecc71".to_string() // Green
                } else if s.metrics.process_efficiency > 0.5 {
                    "#f39c12".to_string() // Orange
                } else {
                    "#e74c3c".to_string() // Red
                }
            })
            .collect();

        Self {
            stages,
            lead_times,
            process_times,
            wait_times,
            colors,
            total_lead_time: stream.total_lead_time(),
        }
    }

    /// Get data for stacked bar chart (process + wait)
    pub fn stacked_data(&self) -> Vec<StackedBarData> {
        self.stages
            .iter()
            .enumerate()
            .map(|(i, stage)| StackedBarData {
                label: stage.clone(),
                process_time: self.process_times[i],
                wait_time: self.wait_times[i],
                total: self.lead_times[i],
            })
            .collect()
    }
}

/// Stacked bar data point
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StackedBarData {
    pub label: String,
    pub process_time: f64,
    pub wait_time: f64,
    pub total: f64,
}

/// Process efficiency visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EfficiencyChart {
    /// Stage names
    pub stages: Vec<String>,
    /// Efficiency ratios (0.0 to 1.0)
    pub efficiencies: Vec<f64>,
    /// Efficiency grades (A, B, C, D, F)
    pub grades: Vec<char>,
    /// Overall efficiency
    pub overall_efficiency: f64,
    /// Target efficiency
    pub target_efficiency: f64,
}

impl EfficiencyChart {
    fn from_stream(stream: &ValueStream) -> Self {
        let stages: Vec<String> = stream
            .stages
            .iter()
            .map(|s| s.stage_type.to_string())
            .collect();

        let efficiencies: Vec<f64> = stream
            .stages
            .iter()
            .map(|s| s.metrics.process_efficiency)
            .collect();

        let grades: Vec<char> = stream
            .stages
            .iter()
            .map(|s| s.metrics.efficiency_grade())
            .collect();

        Self {
            stages,
            efficiencies,
            grades,
            overall_efficiency: stream.process_efficiency(),
            target_efficiency: 0.8, // 80% target
        }
    }

    /// Get data for radar chart
    pub fn radar_data(&self) -> Vec<RadarPoint> {
        self.stages
            .iter()
            .enumerate()
            .map(|(i, stage)| RadarPoint {
                axis: stage.clone(),
                value: self.efficiencies[i],
            })
            .collect()
    }
}

/// Radar chart data point
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RadarPoint {
    pub axis: String,
    pub value: f64,
}

/// Bottleneck visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BottleneckChart {
    /// Bottleneck descriptions
    pub bottlenecks: Vec<BottleneckVisualization>,
    /// Total delay from bottlenecks
    pub total_delay_ms: f64,
    /// Total improvement potential
    pub total_improvement_potential_ms: f64,
}

impl BottleneckChart {
    fn from_stream(stream: &ValueStream) -> Self {
        let bottlenecks: Vec<BottleneckVisualization> = stream
            .bottlenecks
            .bottlenecks
            .iter()
            .map(BottleneckVisualization::from_bottleneck)
            .collect();

        Self {
            bottlenecks,
            total_delay_ms: stream.bottlenecks.total_delay_ms,
            total_improvement_potential_ms: stream.bottlenecks.total_improvement_potential_ms,
        }
    }

    /// Get top N bottlenecks for visualization
    pub fn top_bottlenecks(&self, n: usize) -> Vec<&BottleneckVisualization> {
        let mut sorted: Vec<&BottleneckVisualization> = self.bottlenecks.iter().collect();
        sorted.sort_by(|a, b| b.impact_ms.partial_cmp(&a.impact_ms).unwrap());
        sorted.into_iter().take(n).collect()
    }
}

/// Single bottleneck for visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BottleneckVisualization {
    pub stage: String,
    pub bottleneck_type: String,
    pub severity: String,
    pub severity_color: String,
    pub impact_ms: f64,
    pub delay_percentage: f64,
    pub improvement_potential_ms: f64,
    pub description: String,
}

impl BottleneckVisualization {
    fn from_bottleneck(bottleneck: &Bottleneck) -> Self {
        Self {
            stage: bottleneck.stage.to_string(),
            bottleneck_type: bottleneck.bottleneck_type.to_string(),
            severity: bottleneck.severity.to_string(),
            severity_color: bottleneck.severity.color().to_string(),
            impact_ms: bottleneck.impact_ms,
            delay_percentage: bottleneck.delay_percentage,
            improvement_potential_ms: bottleneck.improvement_potential_ms,
            description: bottleneck.description.clone(),
        }
    }
}

/// Swim lane visualization with stakeholder interactions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwimLaneView {
    /// Lanes by stakeholder
    pub lanes: Vec<LaneVisualization>,
    /// Handoffs between lanes
    pub handoffs: Vec<HandoffVisualization>,
    /// Total handoff count
    pub total_handoffs: usize,
    /// Average handoff time
    pub average_handoff_time_ms: f64,
}

impl SwimLaneView {
    fn from_stream(stream: &ValueStream) -> Self {
        let lanes: Vec<LaneVisualization> = stream
            .swim_lanes
            .lanes
            .values()
            .map(|lane| LaneVisualization {
                role: lane.role.to_string(),
                color: lane.role.color().to_string(),
                stage_count: lane.stage_count,
                total_time_ms: lane.total_time_ms,
                utilization: lane.utilization(),
                is_overloaded: lane.is_overloaded(),
            })
            .collect();

        // Extract handoffs from transitions
        let mut handoffs = Vec::new();
        for (i, transition) in stream.transitions.iter().enumerate() {
            if transition.is_stakeholder_handoff() {
                handoffs.push(HandoffVisualization {
                    id: i,
                    from_role: transition.from_role.to_string(),
                    to_role: transition.to_role.to_string(),
                    from_stage: transition.from_stage.to_string(),
                    to_stage: transition.to_stage.to_string(),
                    duration_ms: transition.duration_ms,
                });
            }
        }

        Self {
            lanes,
            handoffs,
            total_handoffs: stream.swim_lanes.total_handoffs,
            average_handoff_time_ms: stream.swim_lanes.average_handoff_time_ms,
        }
    }
}

/// Single swim lane visualization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LaneVisualization {
    pub role: String,
    pub color: String,
    pub stage_count: usize,
    pub total_time_ms: f64,
    pub utilization: f64,
    pub is_overloaded: bool,
}

/// Handoff between swim lanes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HandoffVisualization {
    pub id: usize,
    pub from_role: String,
    pub to_role: String,
    pub from_stage: String,
    pub to_stage: String,
    pub duration_ms: f64,
}

/// Process flow diagram data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowDiagram {
    /// Nodes (stages)
    pub nodes: Vec<FlowNode>,
    /// Edges (transitions)
    pub edges: Vec<FlowEdge>,
}

impl FlowDiagram {
    fn from_stream(stream: &ValueStream) -> Self {
        let nodes: Vec<FlowNode> = stream
            .stages
            .iter()
            .enumerate()
            .map(|(i, stage)| FlowNode {
                id: i,
                label: stage.stage_type.to_string(),
                stage_type: stage.stage_type,
                lead_time_ms: stage.metrics.timing.lead_time_ms,
                efficiency: stage.metrics.process_efficiency,
                is_bottleneck: stage.is_potential_bottleneck(),
                color: if stage.is_potential_bottleneck() {
                    "#e74c3c".to_string() // Red for bottlenecks
                } else if stage.metrics.process_efficiency > 0.7 {
                    "#2ecc71".to_string() // Green for efficient
                } else {
                    "#f39c12".to_string() // Orange for moderate
                },
            })
            .collect();

        let edges: Vec<FlowEdge> = stream
            .transitions
            .iter()
            .enumerate()
            .map(|(i, transition)| {
                let from_idx = stream
                    .stages
                    .iter()
                    .position(|s| s.stage_type == transition.from_stage)
                    .unwrap_or(0);
                let to_idx = stream
                    .stages
                    .iter()
                    .position(|s| s.stage_type == transition.to_stage)
                    .unwrap_or(0);

                FlowEdge {
                    id: i,
                    from: from_idx,
                    to: to_idx,
                    duration_ms: transition.duration_ms,
                    is_handoff: transition.is_stakeholder_handoff(),
                }
            })
            .collect();

        Self { nodes, edges }
    }
}

/// Node in flow diagram
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowNode {
    pub id: usize,
    pub label: String,
    pub stage_type: StageType,
    pub lead_time_ms: f64,
    pub efficiency: f64,
    pub is_bottleneck: bool,
    pub color: String,
}

/// Edge in flow diagram
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowEdge {
    pub id: usize,
    pub from: usize,
    pub to: usize,
    pub duration_ms: f64,
    pub is_handoff: bool,
}

/// Summary statistics for dashboard
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SummaryStats {
    pub total_stages: usize,
    pub total_lead_time_ms: f64,
    pub total_process_time_ms: f64,
    pub total_wait_time_ms: f64,
    pub overall_efficiency: f64,
    pub efficiency_grade: char,
    pub bottleneck_count: usize,
    pub critical_bottleneck_count: usize,
    pub improvement_potential_ms: f64,
    pub improvement_potential_pct: f64,
}

impl SummaryStats {
    fn from_stream(stream: &ValueStream) -> Self {
        let total_lead_time_ms = stream.total_lead_time();
        let total_process_time_ms = stream.total_process_time();
        let total_wait_time_ms = total_lead_time_ms - total_process_time_ms;
        let overall_efficiency = stream.process_efficiency();

        let efficiency_grade = if overall_efficiency >= 0.9 {
            'A'
        } else if overall_efficiency >= 0.8 {
            'B'
        } else if overall_efficiency >= 0.7 {
            'C'
        } else if overall_efficiency >= 0.6 {
            'D'
        } else {
            'F'
        };

        let improvement_potential_pct = if total_lead_time_ms > 0.0 {
            (stream.bottlenecks.total_improvement_potential_ms / total_lead_time_ms) * 100.0
        } else {
            0.0
        };

        Self {
            total_stages: stream.stages.len(),
            total_lead_time_ms,
            total_process_time_ms,
            total_wait_time_ms,
            overall_efficiency,
            efficiency_grade,
            bottleneck_count: stream.bottlenecks.bottlenecks.len(),
            critical_bottleneck_count: stream.bottlenecks.critical().len(),
            improvement_potential_ms: stream.bottlenecks.total_improvement_potential_ms,
            improvement_potential_pct,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vsm::flow::ValueStream;
    use crate::vsm::stage::Stage;

    #[test]
    fn test_visualization_data() {
        let mut stream = ValueStream::new("test");

        let mut stage1 = Stage::new(StageType::CodeGeneration);
        stage1.metrics.timing.lead_time_ms = 1000.0;
        stage1.metrics.timing.process_time_ms = 800.0;
        stage1.metrics.process_efficiency = 0.8;

        let mut stage2 = Stage::new(StageType::Testing);
        stage2.metrics.timing.lead_time_ms = 500.0;
        stage2.metrics.timing.process_time_ms = 300.0;
        stage2.metrics.process_efficiency = 0.6;

        stream.add_stage(stage1);
        stream.add_stage(stage2);

        let viz = VisualizationData::from_value_stream(&stream);

        assert_eq!(viz.lead_time_chart.stages.len(), 2);
        assert_eq!(viz.efficiency_chart.efficiencies.len(), 2);
        assert_eq!(viz.summary_stats.total_stages, 2);
        assert!(viz.summary_stats.total_lead_time_ms > 0.0);
    }

    #[test]
    fn test_lead_time_chart() {
        let mut stream = ValueStream::new("test");

        let mut stage = Stage::new(StageType::Validation);
        stage.metrics.timing.lead_time_ms = 1000.0;
        stage.metrics.timing.process_time_ms = 600.0;
        stage.metrics.timing.wait_time_ms = 400.0;

        stream.add_stage(stage);

        let chart = LeadTimeChart::from_stream(&stream);
        let stacked = chart.stacked_data();

        assert_eq!(stacked.len(), 1);
        assert_eq!(stacked[0].process_time, 600.0);
        assert_eq!(stacked[0].wait_time, 400.0);
        assert_eq!(stacked[0].total, 1000.0);
    }

    #[test]
    fn test_flow_diagram() {
        let mut stream = ValueStream::new("test");

        stream.add_stage(Stage::new(StageType::OntologyDesign));
        stream.add_stage(Stage::new(StageType::CodeGeneration));

        let diagram = FlowDiagram::from_stream(&stream);

        assert_eq!(diagram.nodes.len(), 2);
        assert_eq!(diagram.edges.len(), 1);
        assert_eq!(diagram.edges[0].from, 0);
        assert_eq!(diagram.edges[0].to, 1);
    }
}
