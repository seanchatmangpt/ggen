//! Value Stream Flow Management
//!
//! Manages the complete flow through the value stream, including stage transitions
//! and overall metrics tracking.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::bottleneck::{Bottleneck, BottleneckAnalysis, BottleneckSeverity, BottleneckType};
use super::metrics::{AggregatedMetrics, Metrics};
use super::stage::{Stage, StageType};
use super::swim_lane::{StakeholderRole, SwimLaneAnalysis};
use super::touchpoint::TouchpointCollection;

/// A transition between stages in the value stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transition {
    /// Unique identifier
    pub id: String,
    /// Source stage type
    pub from_stage: StageType,
    /// Target stage type
    pub to_stage: StageType,
    /// Time spent in transition (ms)
    pub duration_ms: f64,
    /// When transition occurred
    pub timestamp: DateTime<Utc>,
    /// Handoff between stakeholders
    pub from_role: StakeholderRole,
    pub to_role: StakeholderRole,
}

impl Transition {
    /// Create a new transition
    pub fn new(from_stage: StageType, to_stage: StageType) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            from_stage,
            to_stage,
            duration_ms: 0.0,
            timestamp: Utc::now(),
            from_role: from_stage.primary_stakeholder(),
            to_role: to_stage.primary_stakeholder(),
        }
    }

    /// Set transition duration
    pub fn with_duration(mut self, duration_ms: f64) -> Self {
        self.duration_ms = duration_ms;
        self
    }

    /// Check if transition involves stakeholder handoff
    pub fn is_stakeholder_handoff(&self) -> bool {
        self.from_role != self.to_role
    }
}

/// Complete value stream from ontology to deployment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueStream {
    /// Unique identifier
    pub id: String,
    /// Stream name/description
    pub name: String,
    /// Version of this stream configuration
    pub version: String,
    /// All stages in this stream
    pub stages: Vec<Stage>,
    /// Transitions between stages
    pub transitions: Vec<Transition>,
    /// When stream execution started
    pub start_time: Option<DateTime<Utc>>,
    /// When stream execution completed
    pub end_time: Option<DateTime<Utc>>,
    /// Touchpoint tracking
    pub touchpoints: TouchpointCollection,
    /// Swim lane analysis
    pub swim_lanes: SwimLaneAnalysis,
    /// Detected bottlenecks
    pub bottlenecks: BottleneckAnalysis,
    /// Metadata
    pub metadata: HashMap<String, String>,
}

impl ValueStream {
    /// Create a new value stream
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name: name.into(),
            version: "1.0.0".to_string(),
            stages: Vec::new(),
            transitions: Vec::new(),
            start_time: None,
            end_time: None,
            touchpoints: TouchpointCollection::new(),
            swim_lanes: SwimLaneAnalysis::new(),
            bottlenecks: BottleneckAnalysis::new(),
            metadata: HashMap::new(),
        }
    }

    /// Start value stream execution
    pub fn start(&mut self) {
        self.start_time = Some(Utc::now());
    }

    /// End value stream execution
    pub fn end(&mut self) {
        self.end_time = Some(Utc::now());
        self.analyze();
    }

    /// Add a stage to the stream
    pub fn add_stage(&mut self, stage: Stage) {
        // Update swim lane analysis
        if let Some(lead_time) = stage.lead_time() {
            let lane = self.swim_lanes.get_or_create_lane(stage.swim_lane);
            lane.add_stage_time(lead_time.as_millis() as f64);
        }

        // Create transition if there's a previous stage
        if let Some(last_stage) = self.stages.last() {
            let transition = Transition::new(last_stage.stage_type, stage.stage_type);
            self.transitions.push(transition);

            // Record handoff if stakeholders differ
            if last_stage.swim_lane != stage.swim_lane {
                self.swim_lanes.record_handoff(
                    last_stage.swim_lane,
                    stage.swim_lane,
                    0.0, // Duration unknown
                );
            }
        }

        self.stages.push(stage);
    }

    /// Get stage by ID
    pub fn get_stage(&self, stage_id: &str) -> Option<&Stage> {
        self.stages.iter().find(|s| s.id == stage_id)
    }

    /// Get mutable stage by ID
    pub fn get_stage_mut(&mut self, stage_id: &str) -> Option<&mut Stage> {
        self.stages.iter_mut().find(|s| s.id == stage_id)
    }

    /// Get stages by type
    pub fn stages_by_type(&self, stage_type: StageType) -> Vec<&Stage> {
        self.stages
            .iter()
            .filter(|s| s.stage_type == stage_type)
            .collect()
    }

    /// Calculate total lead time
    pub fn total_lead_time(&self) -> f64 {
        self.stages
            .iter()
            .map(|s| s.metrics.timing.lead_time_ms)
            .sum()
    }

    /// Calculate total process time
    pub fn total_process_time(&self) -> f64 {
        self.stages
            .iter()
            .map(|s| s.metrics.timing.process_time_ms)
            .sum()
    }

    /// Calculate overall process efficiency
    pub fn process_efficiency(&self) -> f64 {
        let lead_time = self.total_lead_time();
        if lead_time > 0.0 {
            self.total_process_time() / lead_time
        } else {
            0.0
        }
    }

    /// Get aggregated metrics across all stages
    pub fn aggregated_metrics(&self) -> AggregatedMetrics {
        let metrics: Vec<Metrics> = self.stages.iter().map(|s| s.metrics.clone()).collect();
        AggregatedMetrics::from_stages(&metrics)
    }

    /// Analyze value stream and detect bottlenecks
    pub fn analyze(&mut self) {
        self.detect_bottlenecks();
    }

    /// Detect bottlenecks in the value stream
    pub fn detect_bottlenecks(&mut self) -> Vec<Bottleneck> {
        let mut bottlenecks = Vec::new();
        let total_lead_time = self.total_lead_time();

        for stage in &self.stages {
            // Check for low process efficiency
            if stage.metrics.process_efficiency < 0.5 {
                let impact = stage.metrics.timing.wait_time_ms;
                let severity = if stage.metrics.process_efficiency < 0.3 {
                    BottleneckSeverity::Critical
                } else {
                    BottleneckSeverity::High
                };

                let mut bottleneck = Bottleneck::new(
                    stage.stage_type,
                    BottleneckType::ProcessInefficiency,
                    severity,
                    impact,
                )
                .with_description(format!(
                    "Low process efficiency: {:.1}%",
                    stage.metrics.process_efficiency * 100.0
                ))
                .with_improvement_potential(impact * 0.5);

                bottleneck.delay_percentage = if total_lead_time > 0.0 {
                    (impact / total_lead_time) * 100.0
                } else {
                    0.0
                };

                bottleneck.add_recommendation("Reduce wait time and streamline handoffs");
                bottleneck.add_recommendation("Automate manual steps where possible");

                bottlenecks.push(bottleneck);
            }

            // Check for high queue length (capacity constraint)
            if stage.queue_length > 10 {
                let impact = stage.metrics.timing.cycle_time_ms * stage.queue_length as f64;
                let severity = if stage.queue_length > 50 {
                    BottleneckSeverity::Critical
                } else if stage.queue_length > 25 {
                    BottleneckSeverity::High
                } else {
                    BottleneckSeverity::Medium
                };

                let mut bottleneck = Bottleneck::new(
                    stage.stage_type,
                    BottleneckType::Capacity,
                    severity,
                    impact,
                )
                .with_description(format!(
                    "High queue length: {} items waiting",
                    stage.queue_length
                ))
                .with_improvement_potential(impact * 0.7);

                bottleneck.delay_percentage = if total_lead_time > 0.0 {
                    (impact / total_lead_time) * 100.0
                } else {
                    0.0
                };

                bottleneck.add_recommendation("Increase processing capacity");
                bottleneck.add_recommendation("Implement parallel processing");

                bottlenecks.push(bottleneck);
            }

            // Check for quality issues (defects/rework)
            if !stage.first_pass && stage.defects > 0 {
                let impact = stage.metrics.timing.lead_time_ms * 0.3; // Assume 30% rework time
                let severity = if stage.defects > 5 {
                    BottleneckSeverity::High
                } else {
                    BottleneckSeverity::Medium
                };

                let mut bottleneck = Bottleneck::new(
                    stage.stage_type,
                    BottleneckType::Quality,
                    severity,
                    impact,
                )
                .with_description(format!("{} defects requiring rework", stage.defects))
                .with_improvement_potential(impact * 0.8);

                bottleneck.delay_percentage = if total_lead_time > 0.0 {
                    (impact / total_lead_time) * 100.0
                } else {
                    0.0
                };

                bottleneck.add_recommendation("Implement quality gates earlier in pipeline");
                bottleneck.add_recommendation("Add automated validation checks");

                bottlenecks.push(bottleneck);
            }

            // Check for approval delays
            if let Some(approval_time) = stage.approval_time_ms {
                if approval_time > 1000.0 {
                    // More than 1 second
                    let severity = if approval_time > 5000.0 {
                        BottleneckSeverity::High
                    } else {
                        BottleneckSeverity::Medium
                    };

                    let mut bottleneck = Bottleneck::new(
                        stage.stage_type,
                        BottleneckType::Approval,
                        severity,
                        approval_time,
                    )
                    .with_description(format!("Slow approval process: {:.2}ms", approval_time))
                    .with_improvement_potential(approval_time * 0.6);

                    bottleneck.delay_percentage = if total_lead_time > 0.0 {
                        (approval_time / total_lead_time) * 100.0
                    } else {
                        0.0
                    };

                    bottleneck.add_recommendation("Automate low-risk approvals");
                    bottleneck.add_recommendation("Implement approval timeouts");

                    bottlenecks.push(bottleneck);
                }
            }
        }

        // Add bottlenecks to analysis
        for bottleneck in &bottlenecks {
            self.bottlenecks.add(bottleneck.clone());
        }

        bottlenecks
    }

    /// Get critical path (stages contributing most to lead time)
    pub fn critical_path(&self) -> Vec<&Stage> {
        let mut stages: Vec<&Stage> = self.stages.iter().collect();
        stages.sort_by(|a, b| {
            b.metrics
                .timing
                .lead_time_ms
                .partial_cmp(&a.metrics.timing.lead_time_ms)
                .unwrap()
        });
        stages.into_iter().take(3).collect() // Top 3 slowest stages
    }

    /// Get value stream summary
    pub fn summary(&self) -> String {
        format!(
            "Value Stream '{}': {} stages, {:.2}ms total lead time, {:.1}% efficiency, {} bottlenecks",
            self.name,
            self.stages.len(),
            self.total_lead_time(),
            self.process_efficiency() * 100.0,
            self.bottlenecks.bottlenecks.len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn test_value_stream_creation() {
        let mut stream = ValueStream::new("test-stream");

        assert_eq!(stream.name, "test-stream");
        assert_eq!(stream.stages.len(), 0);
        assert!(stream.start_time.is_none());

        stream.start();
        assert!(stream.start_time.is_some());
    }

    #[test]
    fn test_add_stages() {
        let mut stream = ValueStream::new("test-stream");

        let mut stage1 = Stage::new(StageType::OntologyDesign);
        stage1.start();
        std::thread::sleep(Duration::from_millis(10));
        stage1.record_process_time(Duration::from_millis(5));
        stage1.end();

        let mut stage2 = Stage::new(StageType::CodeGeneration);
        stage2.start();
        std::thread::sleep(Duration::from_millis(10));
        stage2.record_process_time(Duration::from_millis(8));
        stage2.end();

        stream.add_stage(stage1);
        stream.add_stage(stage2);

        assert_eq!(stream.stages.len(), 2);
        assert_eq!(stream.transitions.len(), 1);
        assert!(stream.total_lead_time() > 0.0);
    }

    #[test]
    fn test_bottleneck_detection() {
        let mut stream = ValueStream::new("test-stream");

        // Add inefficient stage
        let mut stage = Stage::new(StageType::Validation);
        stage.metrics.timing.lead_time_ms = 1000.0;
        stage.metrics.timing.process_time_ms = 200.0; // Only 20% efficient
        stage.metrics.timing.wait_time_ms = 800.0;
        stage.metrics.process_efficiency = 0.2;

        stream.add_stage(stage);
        stream.analyze();

        assert!(!stream.bottlenecks.bottlenecks.is_empty());
        let bottleneck = &stream.bottlenecks.bottlenecks[0];
        assert_eq!(bottleneck.stage, StageType::Validation);
    }

    #[test]
    fn test_process_efficiency() {
        let mut stream = ValueStream::new("test-stream");

        let mut stage1 = Stage::new(StageType::CodeGeneration);
        stage1.metrics.timing.lead_time_ms = 1000.0;
        stage1.metrics.timing.process_time_ms = 800.0;

        let mut stage2 = Stage::new(StageType::Testing);
        stage2.metrics.timing.lead_time_ms = 500.0;
        stage2.metrics.timing.process_time_ms = 400.0;

        stream.add_stage(stage1);
        stream.add_stage(stage2);

        let efficiency = stream.process_efficiency();
        assert!((efficiency - 0.8).abs() < 0.01); // 1200/1500 = 0.8
    }
}
