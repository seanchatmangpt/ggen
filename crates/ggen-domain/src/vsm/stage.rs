//! Value Stream Mapping Stage definitions
//!
//! Defines discrete stages in the semantic code generation pipeline with
//! comprehensive timing and quality metrics.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::time::Duration;

use super::metrics::{Metrics, TimingMetrics};
use super::swim_lane::StakeholderRole;

/// Types of stages in the semantic generation value stream
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StageType {
    /// Ontology design and semantic modeling
    OntologyDesign,
    /// Template creation and authoring
    TemplateCreation,
    /// RDF data binding to template variables
    DataBinding,
    /// Template processing with Tera engine
    TemplateProcessing,
    /// SPARQL query execution against RDF store
    GraphQuerying,
    /// Final code artifact generation
    CodeGeneration,
    /// Quality validation and governance checks
    Validation,
    /// Automated testing
    Testing,
    /// Deployment to target environment
    Deployment,
}

impl StageType {
    /// Get human-readable name for stage type
    pub fn name(&self) -> &'static str {
        match self {
            StageType::OntologyDesign => "Ontology Design",
            StageType::TemplateCreation => "Template Creation",
            StageType::DataBinding => "Data Binding",
            StageType::TemplateProcessing => "Template Processing",
            StageType::GraphQuerying => "Graph Querying",
            StageType::CodeGeneration => "Code Generation",
            StageType::Validation => "Validation",
            StageType::Testing => "Testing",
            StageType::Deployment => "Deployment",
        }
    }

    /// Get RDF URI suffix for this stage type
    pub fn rdf_uri(&self) -> &'static str {
        match self {
            StageType::OntologyDesign => "OntologyDesign",
            StageType::TemplateCreation => "TemplateCreation",
            StageType::DataBinding => "DataBinding",
            StageType::TemplateProcessing => "TemplateProcessing",
            StageType::GraphQuerying => "GraphQuerying",
            StageType::CodeGeneration => "CodeGeneration",
            StageType::Validation => "Validation",
            StageType::Testing => "Testing",
            StageType::Deployment => "Deployment",
        }
    }

    /// Get primary stakeholder role for this stage
    pub fn primary_stakeholder(&self) -> StakeholderRole {
        match self {
            StageType::OntologyDesign => StakeholderRole::OntologyArchitect,
            StageType::TemplateCreation => StakeholderRole::TemplateAuthor,
            StageType::DataBinding => StakeholderRole::DataEngineer,
            StageType::TemplateProcessing => StakeholderRole::AISystem,
            StageType::GraphQuerying => StakeholderRole::AISystem,
            StageType::CodeGeneration => StakeholderRole::AISystem,
            StageType::Validation => StakeholderRole::QualityAssurance,
            StageType::Testing => StakeholderRole::QualityAssurance,
            StageType::Deployment => StakeholderRole::DevOps,
        }
    }

    /// Get all possible stage types in pipeline order
    pub fn all() -> Vec<StageType> {
        vec![
            StageType::OntologyDesign,
            StageType::TemplateCreation,
            StageType::DataBinding,
            StageType::TemplateProcessing,
            StageType::GraphQuerying,
            StageType::CodeGeneration,
            StageType::Validation,
            StageType::Testing,
            StageType::Deployment,
        ]
    }
}

impl std::fmt::Display for StageType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// Status of a stage execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StageStatus {
    /// Not yet started
    Pending,
    /// Currently executing
    InProgress,
    /// Completed successfully
    Completed,
    /// Failed with errors
    Failed,
    /// Skipped (not applicable)
    Skipped,
}

/// A single stage in the value stream with comprehensive metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Stage {
    /// Unique identifier for this stage execution
    pub id: String,
    /// Type of stage
    pub stage_type: StageType,
    /// Current status
    pub status: StageStatus,
    /// Timing and efficiency metrics
    pub metrics: Metrics,
    /// Stakeholder responsible for this stage
    pub swim_lane: StakeholderRole,
    /// When stage execution started
    pub start_time: Option<DateTime<Utc>>,
    /// When stage execution ended
    pub end_time: Option<DateTime<Utc>>,
    /// Number of items in queue waiting to enter stage
    pub queue_length: usize,
    /// Whether this stage passed without requiring rework
    pub first_pass: bool,
    /// Number of defects found in this stage
    pub defects: usize,
    /// Whether governance approval is required
    pub approval_required: bool,
    /// Time spent waiting for approval (ms)
    pub approval_time_ms: Option<f64>,
    /// Additional metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl Stage {
    /// Create a new stage with default metrics
    pub fn new(stage_type: StageType) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            stage_type,
            status: StageStatus::Pending,
            metrics: Metrics::default(),
            swim_lane: stage_type.primary_stakeholder(),
            start_time: None,
            end_time: None,
            queue_length: 0,
            first_pass: true,
            defects: 0,
            approval_required: false,
            approval_time_ms: None,
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Start stage execution (captures start time)
    pub fn start(&mut self) {
        self.start_time = Some(Utc::now());
        self.status = StageStatus::InProgress;
    }

    /// End stage execution (captures end time and calculates metrics)
    pub fn end(&mut self) {
        self.end_time = Some(Utc::now());
        self.status = StageStatus::Completed;
        self.calculate_timing();
    }

    /// Mark stage as failed
    pub fn fail(&mut self) {
        self.end_time = Some(Utc::now());
        self.status = StageStatus::Failed;
        self.first_pass = false;
        self.calculate_timing();
    }

    /// Skip this stage
    pub fn skip(&mut self) {
        self.status = StageStatus::Skipped;
    }

    /// Calculate timing metrics based on start/end times
    fn calculate_timing(&mut self) {
        if let (Some(start), Some(end)) = (self.start_time, self.end_time) {
            let duration = end.signed_duration_since(start);
            let lead_time_ms = duration.num_milliseconds() as f64;

            self.metrics.timing.lead_time_ms = lead_time_ms;

            // If we have process time, calculate wait time
            if self.metrics.timing.process_time_ms > 0.0 {
                self.metrics.timing.wait_time_ms =
                    lead_time_ms - self.metrics.timing.process_time_ms;
            }

            // Calculate process efficiency (process time / lead time)
            if lead_time_ms > 0.0 {
                self.metrics.process_efficiency =
                    self.metrics.timing.process_time_ms / lead_time_ms;
            }
        }
    }

    /// Record process time (actual value-added work)
    pub fn record_process_time(&mut self, duration: Duration) {
        self.metrics.timing.process_time_ms = duration.as_millis() as f64;
        self.calculate_timing();
    }

    /// Record wait time (idle time)
    pub fn record_wait_time(&mut self, duration: Duration) {
        self.metrics.timing.wait_time_ms = duration.as_millis() as f64;
    }

    /// Record touch time (hands-on work)
    pub fn record_touch_time(&mut self, duration: Duration) {
        self.metrics.timing.touch_time_ms = duration.as_millis() as f64;
    }

    /// Add defect to this stage
    pub fn add_defect(&mut self) {
        self.defects += 1;
        self.first_pass = false;
    }

    /// Record approval time
    pub fn record_approval_time(&mut self, duration: Duration) {
        self.approval_time_ms = Some(duration.as_millis() as f64);
    }

    /// Get lead time as duration
    pub fn lead_time(&self) -> Option<Duration> {
        if self.metrics.timing.lead_time_ms > 0.0 {
            Some(Duration::from_millis(
                self.metrics.timing.lead_time_ms as u64
            ))
        } else {
            None
        }
    }

    /// Get process time as duration
    pub fn process_time(&self) -> Option<Duration> {
        if self.metrics.timing.process_time_ms > 0.0 {
            Some(Duration::from_millis(
                self.metrics.timing.process_time_ms as u64,
            ))
        } else {
            None
        }
    }

    /// Check if stage is a bottleneck (low efficiency or high queue)
    pub fn is_potential_bottleneck(&self) -> bool {
        // Low process efficiency
        if self.metrics.process_efficiency < 0.5 {
            return true;
        }

        // High queue length
        if self.queue_length > 10 {
            return true;
        }

        // Low first-pass yield
        if !self.first_pass && self.defects > 0 {
            return true;
        }

        false
    }

    /// Get efficiency score (0.0 to 1.0)
    pub fn efficiency_score(&self) -> f64 {
        let mut score = self.metrics.process_efficiency;

        // Penalize for defects
        if self.defects > 0 {
            score *= 0.8;
        }

        // Penalize for queue length
        if self.queue_length > 5 {
            score *= 0.9;
        }

        score.clamp(0.0, 1.0)
    }

    /// Add metadata key-value pair
    pub fn add_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stage_lifecycle() {
        let mut stage = Stage::new(StageType::CodeGeneration);

        assert_eq!(stage.status, StageStatus::Pending);
        assert!(stage.start_time.is_none());

        stage.start();
        assert_eq!(stage.status, StageStatus::InProgress);
        assert!(stage.start_time.is_some());

        // Simulate some work
        std::thread::sleep(Duration::from_millis(10));

        stage.end();
        assert_eq!(stage.status, StageStatus::Completed);
        assert!(stage.end_time.is_some());
        assert!(stage.metrics.timing.lead_time_ms > 0.0);
    }

    #[test]
    fn test_stage_types() {
        assert_eq!(StageType::OntologyDesign.name(), "Ontology Design");
        assert_eq!(
            StageType::OntologyDesign.primary_stakeholder(),
            StakeholderRole::OntologyArchitect
        );
        assert_eq!(StageType::all().len(), 9);
    }

    #[test]
    fn test_bottleneck_detection() {
        let mut stage = Stage::new(StageType::Validation);

        // Low efficiency
        stage.metrics.process_efficiency = 0.3;
        assert!(stage.is_potential_bottleneck());

        // High queue
        stage.metrics.process_efficiency = 0.8;
        stage.queue_length = 15;
        assert!(stage.is_potential_bottleneck());

        // Defects
        stage.queue_length = 0;
        stage.add_defect();
        assert!(stage.is_potential_bottleneck());
    }

    #[test]
    fn test_efficiency_score() {
        let mut stage = Stage::new(StageType::Testing);
        stage.metrics.process_efficiency = 0.9;

        let score = stage.efficiency_score();
        assert!(score > 0.8);

        stage.add_defect();
        let penalized_score = stage.efficiency_score();
        assert!(penalized_score < score);
    }
}
