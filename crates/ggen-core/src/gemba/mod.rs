//! Gemba Walk Observability for Code Generation
//!
//! Implements lean manufacturing principles for code generation:
//! - Value Stream Mapping: Track value-adding vs non-value-adding activities
//! - Gemba Walk: "Go and see" real-time observation at generation site
//! - Waste Identification: TIMWOOD (Transport, Inventory, Motion, Waiting, Overproduction, Overprocessing, Defects)
//! - Root Cause Analysis: 5 Whys, fishbone diagrams
//! - Kaizen: Continuous improvement culture
//! - Respect for People: Developer feedback loops

pub mod value_stream;
pub mod waste;
pub mod root_cause;
pub mod kaizen;
pub mod feedback;
pub mod observer;

use serde::{Deserialize, Serialize};
use std::time::{Duration, Instant};
use std::collections::BTreeMap;

/// Gemba Walk observation point - "the real place" where work happens
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaObservation {
    /// Unique observation ID
    pub id: String,

    /// Timestamp when observation started
    pub timestamp: String,

    /// Location in the codebase being observed
    pub location: GenerationSite,

    /// Current activity being performed
    pub activity: Activity,

    /// Value classification
    pub value_type: ValueType,

    /// Waste classification if non-value-adding
    pub waste_type: Option<WasteType>,

    /// Duration of this observation
    pub duration: Duration,

    /// Developer context and feedback
    pub developer_context: DeveloperContext,

    /// Metrics collected at this point
    pub metrics: GembaMetrics,
}

/// The actual site where code generation occurs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationSite {
    /// Template being processed
    pub template_path: String,

    /// Output target
    pub output_path: String,

    /// Current pipeline phase
    pub phase: PipelinePhase,

    /// RDF graph context
    pub graph_context: Option<String>,

    /// SPARQL queries involved
    pub sparql_queries: Vec<String>,
}

/// Pipeline phase for tracking
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PipelinePhase {
    /// Parsing template file and frontmatter
    TemplateLoading,

    /// Rendering frontmatter variables
    FrontmatterRendering,

    /// Loading and processing RDF graph
    RdfLoading,

    /// Executing SPARQL queries
    SparqlExecution,

    /// Rendering template body
    BodyRendering,

    /// Executing file operations (write, inject, etc.)
    FileExecution,

    /// Running shell hooks
    HookExecution,
}

/// Activity being performed
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Activity {
    /// Activity name
    pub name: String,

    /// Activity description
    pub description: String,

    /// Whether this is a manual or automated step
    pub automation_level: AutomationLevel,

    /// Dependencies on previous activities
    pub dependencies: Vec<String>,

    /// Output artifacts produced
    pub outputs: Vec<String>,
}

/// Level of automation for an activity
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AutomationLevel {
    /// Fully automated, no human intervention
    FullyAutomated,

    /// Partially automated, requires human input
    PartiallyAutomated,

    /// Manual process
    Manual,

    /// Requires human decision making
    HumanInTheLoop,
}

/// Value classification based on lean principles
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValueType {
    /// Activity directly creates value for the end user
    ValueAdding,

    /// Activity necessary but doesn't add value (e.g., validation, safety checks)
    NecessaryNonValueAdding,

    /// Activity that adds no value and should be eliminated
    Waste,
}

/// TIMWOOD waste classification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum WasteType {
    /// Transport: Moving information or data unnecessarily
    Transport,

    /// Inventory: Excess work in progress, buffered data
    Inventory,

    /// Motion: Unnecessary movement in the process
    Motion,

    /// Waiting: Idle time, blocked on dependencies
    Waiting,

    /// Overproduction: Generating more than needed
    Overproduction,

    /// Overprocessing: Adding unnecessary features or complexity
    Overprocessing,

    /// Defects: Errors requiring rework
    Defects,
}

/// Developer context for feedback and improvement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeveloperContext {
    /// Developer's intent for this generation
    pub intent: String,

    /// Expected outcome
    pub expected_outcome: String,

    /// Actual outcome (filled after completion)
    pub actual_outcome: Option<String>,

    /// Developer satisfaction (1-10)
    pub satisfaction: Option<u8>,

    /// Improvement suggestions
    pub suggestions: Vec<String>,

    /// Pain points encountered
    pub pain_points: Vec<PainPoint>,
}

/// Pain point encountered during generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PainPoint {
    /// Description of the pain point
    pub description: String,

    /// Severity (1-10)
    pub severity: u8,

    /// Frequency of occurrence
    pub frequency: Frequency,

    /// Root cause if known
    pub root_cause: Option<String>,
}

/// Frequency of pain point occurrence
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Frequency {
    /// Happens almost every time
    VeryFrequent,

    /// Happens often
    Frequent,

    /// Happens sometimes
    Occasional,

    /// Rarely happens
    Rare,

    /// First time occurrence
    FirstTime,
}

/// Metrics collected at Gemba observation point
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct GembaMetrics {
    /// Time spent on value-adding activities (ms)
    pub value_adding_time_ms: u64,

    /// Time spent on necessary non-value-adding activities (ms)
    pub necessary_nonvalue_time_ms: u64,

    /// Time spent on waste (ms)
    pub waste_time_ms: u64,

    /// Number of SPARQL queries executed
    pub sparql_query_count: usize,

    /// SPARQL cache hit rate (0.0-1.0)
    pub sparql_cache_hit_rate: f64,

    /// Number of RDF triples processed
    pub rdf_triple_count: usize,

    /// Template rendering time (ms)
    pub template_render_time_ms: u64,

    /// File operation time (ms)
    pub file_operation_time_ms: u64,

    /// Number of template variables
    pub variable_count: usize,

    /// Number of files generated
    pub files_generated: usize,

    /// Total lines of code generated
    pub loc_generated: usize,

    /// Number of errors encountered
    pub error_count: usize,

    /// Number of retries needed
    pub retry_count: usize,

    /// Waiting time (blocked on I/O, network, etc.) in ms
    pub waiting_time_ms: u64,
}

/// Gemba Walk session tracking multiple observations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaSession {
    /// Session ID
    pub session_id: String,

    /// Start timestamp
    pub started_at: String,

    /// End timestamp
    pub ended_at: Option<String>,

    /// All observations in this session
    pub observations: Vec<GembaObservation>,

    /// Aggregated metrics for the session
    pub session_metrics: SessionMetrics,

    /// Improvement opportunities identified
    pub improvements: Vec<ImprovementOpportunity>,
}

/// Aggregated metrics for entire session
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SessionMetrics {
    /// Total time spent (ms)
    pub total_time_ms: u64,

    /// Value-added ratio (value time / total time)
    pub value_added_ratio: f64,

    /// Efficiency score (0.0-1.0)
    pub efficiency_score: f64,

    /// Quality score (0.0-1.0, based on defects)
    pub quality_score: f64,

    /// Flow efficiency (processing time / total lead time)
    pub flow_efficiency: f64,

    /// Waste breakdown by type
    pub waste_breakdown: BTreeMap<WasteType, u64>,

    /// Most common pain points
    pub top_pain_points: Vec<(String, usize)>,
}

/// Improvement opportunity identified through Gemba Walk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementOpportunity {
    /// Unique ID for tracking
    pub id: String,

    /// Title of the improvement
    pub title: String,

    /// Description
    pub description: String,

    /// Type of improvement
    pub improvement_type: ImprovementType,

    /// Waste type this addresses
    pub addresses_waste: Option<WasteType>,

    /// Expected impact (1-10)
    pub expected_impact: u8,

    /// Effort required (1-10)
    pub effort_required: u8,

    /// Priority score (impact / effort)
    pub priority_score: f64,

    /// Current status
    pub status: ImprovementStatus,
}

/// Type of improvement
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ImprovementType {
    /// Remove waste entirely
    Eliminate,

    /// Combine steps
    Combine,

    /// Simplify process
    Simplify,

    /// Automate manual step
    Automate,

    /// Reduce variation
    Standardize,
}

/// Status of improvement
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ImprovementStatus {
    /// Identified but not yet planned
    Identified,

    /// Planned for implementation
    Planned,

    /// Currently being implemented
    InProgress,

    /// Implemented and being validated
    Validating,

    /// Completed and validated
    Completed,

    /// Rejected or deferred
    Rejected,
}

/// Timer for tracking activity duration
pub struct GembaTimer {
    start: Instant,
    activity: String,
}

impl GembaTimer {
    /// Start timing an activity
    pub fn start(activity: String) -> Self {
        Self {
            start: Instant::now(),
            activity,
        }
    }

    /// Stop timer and return duration
    pub fn stop(self) -> (String, Duration) {
        (self.activity, self.start.elapsed())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gemba_timer() {
        let timer = GembaTimer::start("test_activity".to_string());
        std::thread::sleep(Duration::from_millis(10));
        let (activity, duration) = timer.stop();
        assert_eq!(activity, "test_activity");
        assert!(duration.as_millis() >= 10);
    }

    #[test]
    fn test_value_type_classification() {
        let waste = ValueType::Waste;
        let value = ValueType::ValueAdding;
        let necessary = ValueType::NecessaryNonValueAdding;

        assert_ne!(waste, value);
        assert_ne!(waste, necessary);
        assert_ne!(value, necessary);
    }

    #[test]
    fn test_waste_types() {
        let wastes = vec![
            WasteType::Transport,
            WasteType::Inventory,
            WasteType::Motion,
            WasteType::Waiting,
            WasteType::Overproduction,
            WasteType::Overprocessing,
            WasteType::Defects,
        ];

        assert_eq!(wastes.len(), 7, "TIMWOOD should have 7 waste types");
    }
}
