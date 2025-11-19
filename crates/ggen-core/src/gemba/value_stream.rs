//! Value Stream Mapping for Code Generation
//!
//! Tracks the flow of value through the generation pipeline,
//! identifying value-adding and non-value-adding activities.

use super::*;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::time::Instant;

/// Value Stream Map for a code generation workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueStreamMap {
    /// Stream ID
    pub id: String,

    /// Name of this value stream
    pub name: String,

    /// Stages in the value stream
    pub stages: Vec<ValueStreamStage>,

    /// Total lead time (ms)
    pub total_lead_time_ms: u64,

    /// Total processing time (ms)
    pub total_processing_time_ms: u64,

    /// Flow efficiency (processing / lead time)
    pub flow_efficiency: f64,

    /// Value-added ratio
    pub value_added_ratio: f64,
}

/// A stage in the value stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueStreamStage {
    /// Stage name
    pub name: String,

    /// Pipeline phase this stage represents
    pub phase: PipelinePhase,

    /// Value type of this stage
    pub value_type: ValueType,

    /// Processing time for this stage (ms)
    pub processing_time_ms: u64,

    /// Queue time before this stage (ms)
    pub queue_time_ms: u64,

    /// Number of handoffs to next stage
    pub handoffs: usize,

    /// Defect rate (0.0-1.0)
    pub defect_rate: f64,

    /// Batch size (how many items processed together)
    pub batch_size: usize,

    /// Activities performed in this stage
    pub activities: Vec<String>,

    /// Bottleneck indicator
    pub is_bottleneck: bool,

    /// Improvement opportunities
    pub improvements: Vec<String>,
}

/// Value Stream Mapper - builds value stream maps
pub struct ValueStreamMapper {
    current_stage: Option<ValueStreamStage>,
    stages: Vec<ValueStreamStage>,
    stage_start: Option<Instant>,
    queue_start: Option<Instant>,
    stream_start: Instant,
}

impl ValueStreamMapper {
    /// Create a new value stream mapper
    pub fn new() -> Self {
        Self {
            current_stage: None,
            stages: Vec::new(),
            stage_start: None,
            queue_start: Some(Instant::now()),
            stream_start: Instant::now(),
        }
    }

    /// Start a new stage
    pub fn start_stage(
        &mut self,
        name: String,
        phase: PipelinePhase,
        value_type: ValueType,
    ) {
        // Finish previous stage if any
        if let Some(mut stage) = self.current_stage.take() {
            if let Some(start) = self.stage_start {
                stage.processing_time_ms = start.elapsed().as_millis() as u64;
            }
            self.stages.push(stage);
        }

        // Calculate queue time
        let queue_time_ms = if let Some(queue_start) = self.queue_start {
            queue_start.elapsed().as_millis() as u64
        } else {
            0
        };

        // Start new stage
        self.current_stage = Some(ValueStreamStage {
            name,
            phase,
            value_type,
            processing_time_ms: 0,
            queue_time_ms,
            handoffs: 0,
            defect_rate: 0.0,
            batch_size: 1,
            activities: Vec::new(),
            is_bottleneck: false,
            improvements: Vec::new(),
        });

        self.stage_start = Some(Instant::now());
        self.queue_start = Some(Instant::now());
    }

    /// Add an activity to the current stage
    pub fn add_activity(&mut self, activity: String) {
        if let Some(stage) = &mut self.current_stage {
            stage.activities.push(activity);
        }
    }

    /// Record a defect in the current stage
    pub fn record_defect(&mut self) {
        if let Some(stage) = &mut self.current_stage {
            // Increment defect count (simple model)
            stage.defect_rate += 0.1;
            if stage.defect_rate > 1.0 {
                stage.defect_rate = 1.0;
            }
        }
    }

    /// Mark current stage as bottleneck
    pub fn mark_bottleneck(&mut self) {
        if let Some(stage) = &mut self.current_stage {
            stage.is_bottleneck = true;
        }
    }

    /// Add improvement opportunity to current stage
    pub fn add_improvement(&mut self, improvement: String) {
        if let Some(stage) = &mut self.current_stage {
            stage.improvements.push(improvement);
        }
    }

    /// Build the complete value stream map
    pub fn build(mut self, id: String, name: String) -> ValueStreamMap {
        // Finish final stage
        if let Some(mut stage) = self.current_stage.take() {
            if let Some(start) = self.stage_start {
                stage.processing_time_ms = start.elapsed().as_millis() as u64;
            }
            self.stages.push(stage);
        }

        // Calculate metrics
        let total_processing_time_ms: u64 =
            self.stages.iter().map(|s| s.processing_time_ms).sum();

        let total_queue_time_ms: u64 =
            self.stages.iter().map(|s| s.queue_time_ms).sum();

        let total_lead_time_ms = total_processing_time_ms + total_queue_time_ms;

        let flow_efficiency = if total_lead_time_ms > 0 {
            total_processing_time_ms as f64 / total_lead_time_ms as f64
        } else {
            0.0
        };

        // Calculate value-added time
        let value_added_time_ms: u64 = self
            .stages
            .iter()
            .filter(|s| s.value_type == ValueType::ValueAdding)
            .map(|s| s.processing_time_ms)
            .sum();

        let value_added_ratio = if total_processing_time_ms > 0 {
            value_added_time_ms as f64 / total_processing_time_ms as f64
        } else {
            0.0
        };

        ValueStreamMap {
            id,
            name,
            stages: self.stages,
            total_lead_time_ms,
            total_processing_time_ms,
            flow_efficiency,
            value_added_ratio,
        }
    }

    /// Identify bottlenecks automatically based on processing time
    pub fn identify_bottlenecks(&mut self) {
        if self.stages.is_empty() {
            return;
        }

        // Find max processing time
        let max_time = self
            .stages
            .iter()
            .map(|s| s.processing_time_ms)
            .max()
            .unwrap_or(0);

        // Mark stages that take >= 80% of max time as bottlenecks
        let threshold = (max_time as f64 * 0.8) as u64;
        for stage in &mut self.stages {
            if stage.processing_time_ms >= threshold {
                stage.is_bottleneck = true;
            }
        }
    }
}

impl Default for ValueStreamMapper {
    fn default() -> Self {
        Self::new()
    }
}

/// Value Stream Analyzer - analyzes completed maps
pub struct ValueStreamAnalyzer;

impl ValueStreamAnalyzer {
    /// Analyze a value stream map and generate insights
    pub fn analyze(map: &ValueStreamMap) -> ValueStreamAnalysis {
        let mut insights = Vec::new();
        let mut recommendations = Vec::new();

        // Check flow efficiency
        if map.flow_efficiency < 0.3 {
            insights.push("Low flow efficiency detected - significant waiting time".to_string());
            recommendations.push(
                "Reduce queue times by parallelizing independent stages".to_string(),
            );
        }

        // Check value-added ratio
        if map.value_added_ratio < 0.5 {
            insights.push(
                "Less than 50% of time is spent on value-adding activities".to_string(),
            );
            recommendations.push("Eliminate or reduce non-value-adding steps".to_string());
        }

        // Check for bottlenecks
        let bottlenecks: Vec<_> = map
            .stages
            .iter()
            .filter(|s| s.is_bottleneck)
            .map(|s| s.name.clone())
            .collect();

        if !bottlenecks.is_empty() {
            insights.push(format!("Bottlenecks identified: {}", bottlenecks.join(", ")));
            recommendations.push("Focus improvement efforts on bottleneck stages".to_string());
        }

        // Check defect rates
        let high_defect_stages: Vec<_> = map
            .stages
            .iter()
            .filter(|s| s.defect_rate > 0.1)
            .map(|s| (s.name.clone(), s.defect_rate))
            .collect();

        if !high_defect_stages.is_empty() {
            for (name, rate) in &high_defect_stages {
                insights.push(format!(
                    "High defect rate ({:.1}%) in stage: {}",
                    rate * 100.0,
                    name
                ));
            }
            recommendations.push("Implement error prevention (poka-yoke) in defect-prone stages".to_string());
        }

        // Calculate waste time
        let waste_time_ms: u64 = map
            .stages
            .iter()
            .filter(|s| s.value_type == ValueType::Waste)
            .map(|s| s.processing_time_ms)
            .sum();

        let waste_percentage = if map.total_processing_time_ms > 0 {
            (waste_time_ms as f64 / map.total_processing_time_ms as f64) * 100.0
        } else {
            0.0
        };

        if waste_percentage > 10.0 {
            insights.push(format!(
                "{:.1}% of processing time is pure waste",
                waste_percentage
            ));
            recommendations.push("Eliminate wasteful activities entirely".to_string());
        }

        ValueStreamAnalysis {
            map_id: map.id.clone(),
            insights,
            recommendations,
            flow_efficiency: map.flow_efficiency,
            value_added_ratio: map.value_added_ratio,
            waste_percentage,
            bottleneck_count: bottlenecks.len(),
        }
    }

    /// Compare two value stream maps to track improvement
    pub fn compare(before: &ValueStreamMap, after: &ValueStreamMap) -> ValueStreamComparison {
        let flow_efficiency_improvement =
            ((after.flow_efficiency - before.flow_efficiency) / before.flow_efficiency) * 100.0;

        let value_added_improvement = ((after.value_added_ratio - before.value_added_ratio)
            / before.value_added_ratio)
            * 100.0;

        let lead_time_reduction = ((before.total_lead_time_ms as f64
            - after.total_lead_time_ms as f64)
            / before.total_lead_time_ms as f64)
            * 100.0;

        ValueStreamComparison {
            before_id: before.id.clone(),
            after_id: after.id.clone(),
            flow_efficiency_improvement,
            value_added_improvement,
            lead_time_reduction,
            is_improved: flow_efficiency_improvement > 0.0
                || value_added_improvement > 0.0
                || lead_time_reduction > 0.0,
        }
    }
}

/// Analysis results for a value stream map
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueStreamAnalysis {
    /// ID of the analyzed map
    pub map_id: String,

    /// Insights discovered
    pub insights: Vec<String>,

    /// Improvement recommendations
    pub recommendations: Vec<String>,

    /// Flow efficiency score
    pub flow_efficiency: f64,

    /// Value-added ratio
    pub value_added_ratio: f64,

    /// Waste percentage
    pub waste_percentage: f64,

    /// Number of bottlenecks
    pub bottleneck_count: usize,
}

/// Comparison between two value stream maps
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueStreamComparison {
    /// Before map ID
    pub before_id: String,

    /// After map ID
    pub after_id: String,

    /// Flow efficiency improvement (%)
    pub flow_efficiency_improvement: f64,

    /// Value-added improvement (%)
    pub value_added_improvement: f64,

    /// Lead time reduction (%)
    pub lead_time_reduction: f64,

    /// Whether improvement was achieved
    pub is_improved: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_stream_mapper() {
        let mut mapper = ValueStreamMapper::new();

        mapper.start_stage(
            "Template Loading".to_string(),
            PipelinePhase::TemplateLoading,
            ValueType::NecessaryNonValueAdding,
        );
        mapper.add_activity("Parse YAML frontmatter".to_string());
        std::thread::sleep(Duration::from_millis(10));

        mapper.start_stage(
            "SPARQL Execution".to_string(),
            PipelinePhase::SparqlExecution,
            ValueType::ValueAdding,
        );
        mapper.add_activity("Execute query".to_string());
        std::thread::sleep(Duration::from_millis(20));

        let map = mapper.build("test-1".to_string(), "Test Stream".to_string());

        assert_eq!(map.stages.len(), 2);
        assert_eq!(map.stages[0].name, "Template Loading");
        assert_eq!(map.stages[1].name, "SPARQL Execution");
        assert!(map.total_processing_time_ms >= 30);
    }

    #[test]
    fn test_value_stream_analyzer() {
        let map = ValueStreamMap {
            id: "test-1".to_string(),
            name: "Test".to_string(),
            stages: vec![
                ValueStreamStage {
                    name: "Stage 1".to_string(),
                    phase: PipelinePhase::TemplateLoading,
                    value_type: ValueType::ValueAdding,
                    processing_time_ms: 100,
                    queue_time_ms: 500,
                    handoffs: 1,
                    defect_rate: 0.0,
                    batch_size: 1,
                    activities: vec![],
                    is_bottleneck: false,
                    improvements: vec![],
                },
            ],
            total_lead_time_ms: 600,
            total_processing_time_ms: 100,
            flow_efficiency: 100.0 / 600.0,
            value_added_ratio: 1.0,
        };

        let analysis = ValueStreamAnalyzer::analyze(&map);

        assert!(analysis.flow_efficiency < 0.3);
        assert!(!analysis.insights.is_empty());
    }
}
