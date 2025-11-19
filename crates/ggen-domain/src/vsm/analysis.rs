//! Value Stream Analysis and Reporting
//!
//! Comprehensive analysis tools for generating VSM reports and insights.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::bottleneck::Bottleneck;
use super::flow::ValueStream;
use super::future_state::FutureState;
use super::metrics::AggregatedMetrics;
use super::stage::{Stage, StageType};
use super::swim_lane::{StakeholderRole, SwimLaneAnalysis};

/// Comprehensive value stream analysis report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisReport {
    /// Report ID
    pub id: String,
    /// Report generation timestamp
    pub generated_at: chrono::DateTime<chrono::Utc>,
    /// Value stream being analyzed
    pub stream_name: String,
    /// Overall metrics
    pub overall_metrics: AggregatedMetrics,
    /// Efficiency analysis
    pub efficiency: EfficiencyAnalysis,
    /// Bottleneck summary
    pub bottleneck_summary: BottleneckSummary,
    /// Swim lane summary
    pub swim_lane_summary: SwimLaneSummary,
    /// Stage performance breakdown
    pub stage_performance: Vec<StagePerformance>,
    /// Recommendations
    pub recommendations: Vec<Recommendation>,
    /// Future state comparison (if available)
    pub future_state_gap: Option<FutureStateGap>,
}

impl AnalysisReport {
    /// Generate report from value stream
    pub fn generate(stream: &ValueStream, future_state: Option<&FutureState>) -> Self {
        let id = uuid::Uuid::new_v4().to_string();
        let generated_at = chrono::Utc::now();

        // Calculate overall metrics
        let overall_metrics = stream.aggregated_metrics();

        // Efficiency analysis
        let efficiency = EfficiencyAnalysis::from_stream(stream);

        // Bottleneck summary
        let bottleneck_summary = BottleneckSummary::from_analysis(&stream.bottlenecks);

        // Swim lane summary
        let swim_lane_summary = SwimLaneSummary::from_analysis(&stream.swim_lanes);

        // Stage performance
        let stage_performance = stream
            .stages
            .iter()
            .map(StagePerformance::from_stage)
            .collect();

        // Generate recommendations
        let recommendations = Self::generate_recommendations(stream, future_state);

        // Future state gap analysis
        let future_state_gap = future_state.map(|fs| FutureStateGap::analyze(stream, fs));

        Self {
            id,
            generated_at,
            stream_name: stream.name.clone(),
            overall_metrics,
            efficiency,
            bottleneck_summary,
            swim_lane_summary,
            stage_performance,
            recommendations,
            future_state_gap,
        }
    }

    /// Generate actionable recommendations
    fn generate_recommendations(
        stream: &ValueStream,
        future_state: Option<&FutureState>,
    ) -> Vec<Recommendation> {
        let mut recommendations = Vec::new();

        // Check overall efficiency
        let efficiency = stream.process_efficiency();
        if efficiency < 0.7 {
            recommendations.push(Recommendation {
                priority: RecommendationPriority::High,
                category: RecommendationCategory::Efficiency,
                title: "Improve Overall Process Efficiency".to_string(),
                description: format!(
                    "Current efficiency is {:.1}%, target should be >70%",
                    efficiency * 100.0
                ),
                expected_impact: "Reduce total lead time by 20-30%".to_string(),
                actions: vec![
                    "Eliminate non-value-added wait times".to_string(),
                    "Streamline handoffs between stages".to_string(),
                    "Automate manual approval steps".to_string(),
                ],
            });
        }

        // Bottleneck recommendations
        for bottleneck in stream.bottlenecks.top_bottlenecks(3) {
            let priority = match bottleneck.severity {
                super::bottleneck::BottleneckSeverity::Critical => RecommendationPriority::Critical,
                super::bottleneck::BottleneckSeverity::High => RecommendationPriority::High,
                super::bottleneck::BottleneckSeverity::Medium => RecommendationPriority::Medium,
                super::bottleneck::BottleneckSeverity::Low => RecommendationPriority::Low,
            };

            recommendations.push(Recommendation {
                priority,
                category: RecommendationCategory::Bottleneck,
                title: format!("Address {} in {}", bottleneck.bottleneck_type, bottleneck.stage),
                description: bottleneck.description.clone(),
                expected_impact: format!(
                    "Save {:.2}ms ({:.1}% of total delay)",
                    bottleneck.improvement_potential_ms, bottleneck.delay_percentage
                ),
                actions: bottleneck.recommendations.clone(),
            });
        }

        // Swim lane recommendations
        for role in stream.swim_lanes.overloaded_stakeholders() {
            recommendations.push(Recommendation {
                priority: RecommendationPriority::Medium,
                category: RecommendationCategory::Capacity,
                title: format!("{} is overloaded", role),
                description: "Stakeholder utilization exceeds 85%".to_string(),
                expected_impact: "Prevent burnout and reduce cycle time".to_string(),
                actions: vec![
                    "Redistribute workload to underutilized roles".to_string(),
                    "Increase automation to reduce manual work".to_string(),
                    "Consider adding capacity".to_string(),
                ],
            });
        }

        // Future state recommendations
        if let Some(fs) = future_state {
            let current_lead_time = stream.total_lead_time();
            if current_lead_time > fs.target_lead_time_ms {
                let gap = current_lead_time - fs.target_lead_time_ms;
                let gap_pct = (gap / current_lead_time) * 100.0;

                recommendations.push(Recommendation {
                    priority: RecommendationPriority::High,
                    category: RecommendationCategory::FutureState,
                    title: "Close Lead Time Gap to Future State".to_string(),
                    description: format!(
                        "Need to reduce lead time by {:.2}ms ({:.1}%) to reach target",
                        gap, gap_pct
                    ),
                    expected_impact: format!("Achieve {} targets", fs.name),
                    actions: vec![
                        "Execute moonshot initiatives".to_string(),
                        "Focus on critical path optimization".to_string(),
                        "Implement parallel processing where possible".to_string(),
                    ],
                });
            }
        }

        // Sort by priority
        recommendations.sort_by_key(|r| r.priority as u8);
        recommendations
    }

    /// Get executive summary
    pub fn executive_summary(&self) -> String {
        format!(
            "Value Stream '{}' Analysis\n\
             Generated: {}\n\
             \n\
             Overall Performance:\n\
             - Total Lead Time: {:.2}ms\n\
             - Process Efficiency: {:.1}%\n\
             - Average First-Pass Yield: {:.1}%\n\
             \n\
             Key Findings:\n\
             - {} stages analyzed\n\
             - {} bottlenecks detected ({} critical)\n\
             - {} recommendations generated\n\
             - Improvement potential: {:.2}ms ({:.1}%)",
            self.stream_name,
            self.generated_at.format("%Y-%m-%d %H:%M:%S UTC"),
            self.overall_metrics.total_lead_time_ms,
            self.overall_metrics.overall_efficiency * 100.0,
            self.overall_metrics.average_fpy * 100.0,
            self.overall_metrics.stage_count,
            self.bottleneck_summary.total_bottlenecks,
            self.bottleneck_summary.critical_count,
            self.recommendations.len(),
            self.bottleneck_summary.total_improvement_potential_ms,
            if self.overall_metrics.total_lead_time_ms > 0.0 {
                (self.bottleneck_summary.total_improvement_potential_ms
                    / self.overall_metrics.total_lead_time_ms)
                    * 100.0
            } else {
                0.0
            }
        )
    }
}

/// Efficiency analysis breakdown
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EfficiencyAnalysis {
    /// Overall process efficiency (0.0 to 1.0)
    pub overall_efficiency: f64,
    /// Value-added time ratio (0.0 to 1.0)
    pub value_added_ratio: f64,
    /// Best performing stage
    pub best_stage: Option<StageType>,
    /// Worst performing stage
    pub worst_stage: Option<StageType>,
    /// Efficiency by stage
    pub stage_efficiency: HashMap<StageType, f64>,
}

impl EfficiencyAnalysis {
    /// Generate efficiency analysis from value stream
    pub fn from_stream(stream: &ValueStream) -> Self {
        let overall_efficiency = stream.process_efficiency();
        let total_lead_time = stream.total_lead_time();
        let total_process_time = stream.total_process_time();

        let value_added_ratio = if total_lead_time > 0.0 {
            total_process_time / total_lead_time
        } else {
            0.0
        };

        let mut stage_efficiency = HashMap::new();
        for stage in &stream.stages {
            stage_efficiency.insert(stage.stage_type, stage.metrics.process_efficiency);
        }

        let best_stage = stream
            .stages
            .iter()
            .max_by(|a, b| {
                a.metrics
                    .process_efficiency
                    .partial_cmp(&b.metrics.process_efficiency)
                    .unwrap()
            })
            .map(|s| s.stage_type);

        let worst_stage = stream
            .stages
            .iter()
            .min_by(|a, b| {
                a.metrics
                    .process_efficiency
                    .partial_cmp(&b.metrics.process_efficiency)
                    .unwrap()
            })
            .map(|s| s.stage_type);

        Self {
            overall_efficiency,
            value_added_ratio,
            best_stage,
            worst_stage,
            stage_efficiency,
        }
    }
}

/// Bottleneck summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BottleneckSummary {
    pub total_bottlenecks: usize,
    pub critical_count: usize,
    pub high_count: usize,
    pub total_delay_ms: f64,
    pub total_improvement_potential_ms: f64,
    pub top_bottleneck: Option<String>,
}

impl BottleneckSummary {
    fn from_analysis(analysis: &super::bottleneck::BottleneckAnalysis) -> Self {
        Self {
            total_bottlenecks: analysis.bottlenecks.len(),
            critical_count: analysis.critical().len(),
            high_count: analysis.high_severity().len(),
            total_delay_ms: analysis.total_delay_ms,
            total_improvement_potential_ms: analysis.total_improvement_potential_ms,
            top_bottleneck: analysis
                .top_bottlenecks(1)
                .first()
                .map(|b| b.summary()),
        }
    }
}

/// Swim lane summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwimLaneSummary {
    pub total_lanes: usize,
    pub total_handoffs: usize,
    pub average_handoff_time_ms: f64,
    pub most_loaded: Option<(StakeholderRole, f64)>,
    pub least_loaded: Option<(StakeholderRole, f64)>,
    pub overloaded_count: usize,
}

impl SwimLaneSummary {
    fn from_analysis(analysis: &SwimLaneAnalysis) -> Self {
        Self {
            total_lanes: analysis.lanes.len(),
            total_handoffs: analysis.total_handoffs,
            average_handoff_time_ms: analysis.average_handoff_time_ms,
            most_loaded: analysis.most_loaded_stakeholder(),
            least_loaded: analysis.least_loaded_stakeholder(),
            overloaded_count: analysis.overloaded_stakeholders().len(),
        }
    }
}

/// Performance metrics for a single stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StagePerformance {
    pub stage_type: StageType,
    pub lead_time_ms: f64,
    pub process_efficiency: f64,
    pub efficiency_grade: char,
    pub is_bottleneck: bool,
}

impl StagePerformance {
    fn from_stage(stage: &Stage) -> Self {
        Self {
            stage_type: stage.stage_type,
            lead_time_ms: stage.metrics.timing.lead_time_ms,
            process_efficiency: stage.metrics.process_efficiency,
            efficiency_grade: stage.metrics.efficiency_grade(),
            is_bottleneck: stage.is_potential_bottleneck(),
        }
    }
}

/// Gap analysis between current and future state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FutureStateGap {
    pub lead_time_gap_ms: f64,
    pub lead_time_gap_pct: f64,
    pub efficiency_gap: f64,
    pub throughput_gap: f64,
    pub moonshot_progress: HashMap<String, f64>,
}

impl FutureStateGap {
    fn analyze(stream: &ValueStream, future_state: &FutureState) -> Self {
        let current_lead_time = stream.total_lead_time();
        let lead_time_gap_ms = current_lead_time - future_state.target_lead_time_ms;
        let lead_time_gap_pct = if current_lead_time > 0.0 {
            (lead_time_gap_ms / current_lead_time) * 100.0
        } else {
            0.0
        };

        let current_efficiency = stream.process_efficiency();
        let efficiency_gap = future_state.target_process_efficiency - current_efficiency;

        // Calculate progress on moonshot targets
        let mut current_metrics = HashMap::new();
        current_metrics.insert("lead_time".to_string(), current_lead_time);
        current_metrics.insert("efficiency".to_string(), current_efficiency);

        let moonshot_progress: HashMap<String, f64> = future_state
            .moonshot_targets
            .iter()
            .filter_map(|target| {
                current_metrics
                    .get(&target.metric_name)
                    .map(|&current| (target.name.clone(), target.progress(current)))
            })
            .collect();

        Self {
            lead_time_gap_ms,
            lead_time_gap_pct,
            efficiency_gap,
            throughput_gap: 0.0, // Would need actual throughput data
            moonshot_progress,
        }
    }
}

/// Actionable recommendation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Recommendation {
    pub priority: RecommendationPriority,
    pub category: RecommendationCategory,
    pub title: String,
    pub description: String,
    pub expected_impact: String,
    pub actions: Vec<String>,
}

/// Recommendation priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RecommendationPriority {
    Critical = 1,
    High = 2,
    Medium = 3,
    Low = 4,
}

impl std::fmt::Display for RecommendationPriority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecommendationPriority::Critical => write!(f, "Critical"),
            RecommendationPriority::High => write!(f, "High"),
            RecommendationPriority::Medium => write!(f, "Medium"),
            RecommendationPriority::Low => write!(f, "Low"),
        }
    }
}

/// Recommendation category
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RecommendationCategory {
    Efficiency,
    Bottleneck,
    Capacity,
    Quality,
    FutureState,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vsm::stage::Stage;

    #[test]
    fn test_efficiency_analysis() {
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

        let analysis = EfficiencyAnalysis::from_stream(&stream);

        assert!(analysis.overall_efficiency > 0.7);
        assert_eq!(analysis.best_stage, Some(StageType::CodeGeneration));
        assert_eq!(analysis.worst_stage, Some(StageType::Testing));
    }

    #[test]
    fn test_analysis_report() {
        let mut stream = ValueStream::new("test-stream");

        let mut stage = Stage::new(StageType::Validation);
        stage.metrics.timing.lead_time_ms = 1000.0;
        stage.metrics.timing.process_time_ms = 400.0;
        stage.metrics.process_efficiency = 0.4;

        stream.add_stage(stage);
        stream.analyze();

        let report = AnalysisReport::generate(&stream, None);

        assert_eq!(report.stream_name, "test-stream");
        assert!(!report.recommendations.is_empty());
        assert!(report.overall_metrics.stage_count > 0);
    }
}
