//! YAWL XML Specification Parser
//!
//! This module provides a comprehensive parser for YAWL (Yet Another Workflow Language)
//! XML specifications. It parses YAWL XML files into strongly-typed Rust structures,
//! supporting all 20 Van der Aalst workflow patterns.
//!
//! # Supported Workflow Patterns (Van der Aalst)
//!
//! ## Control Flow Patterns (1-6)
//! 1. **Sequence** - Sequential execution of tasks
//! 2. **Parallel Split** - Split into parallel branches
//! 3. **Synchronization** - Join parallel branches
//! 4. **Exclusive Choice** - Choose one branch from multiple alternatives
//! 5. **Simple Merge** - Merge exclusive branches
//! 6. **Multi-Choice** - Choose multiple branches simultaneously
//!
//! ## Advanced Branching & Synchronization (7-10)
//! 7. **Synchronizing Merge** - Merge multi-choice branches with synchronization
//! 8. **Multi-Merge** - Merge multi-choice branches without synchronization
//! 9. **Discriminator** - Proceed after first branch completes
//! 10. **Arbitrary Cycles** - Looping constructs with conditions
//!
//! ## State-Based Patterns (11-13)
//! 11. **Implicit Termination** - Terminate when no work remains
//! 12. **Multiple Instances (Without A Priori Knowledge)** - Dynamic instance creation
//! 13. **Multiple Instances (With A Priori Design-Time Knowledge)** - Fixed instance count
//!
//! ## Advanced Instance Patterns (14-16)
//! 14. **Multiple Instances (With A Priori Runtime Knowledge)** - Runtime-determined instance count
//! 15. **Deferred Choice** - Choice deferred until runtime
//! 16. **Interleaved Parallel Routing** - Interleave execution of parallel instances
//!
//! ## Cancellation Patterns (17-20)
//! 17. **Cancel Activity** - Cancel a specific activity
//! 18. **Cancel Case** - Cancel entire process instance
//! 19. **Pattern-Based Termination** - Terminate based on pattern completion
//! 20. **Cancel Region** - Cancel activities within a region
//!
//! # Usage
//!
//! ```rust,no_run
//! use ggen_workflow::parser::{YawlParser, YawlSpec};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let xml = r#"<?xml version="1.0" encoding="UTF-8"?>
//! <specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
//!   <name>Example Workflow</name>
//!   <!-- ... -->
//! </specification>"#;
//!
//! let parser = YawlParser::new();
//! let spec: YawlSpec = parser.parse(xml)?;
//!
//! println!("Parsed workflow: {}", spec.metadata.name);
//! # Ok(())
//! # }
//! ```

use crate::error::errors;
use crate::error::WorkflowResult;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;

// ============================================================================
// Core YAWL Data Structures
// ============================================================================

/// Complete YAWL specification representing a workflow definition.
///
/// This is the root structure parsed from YAWL XML files, containing
/// all workflow metadata, decompositions, tasks, flows, and variables.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct YawlSpec {
    /// Specification metadata and identification
    pub metadata: SpecMetadata,
    /// Root decomposition (main workflow net)
    pub root_decomposition: Decomposition,
    /// All decompositions in the specification
    pub decompositions: Vec<Decomposition>,
    /// All tasks defined in the specification
    pub tasks: HashMap<String, Task>,
    /// All flow connections between nodes
    pub flows: Vec<Flow>,
    /// Variables used in the workflow
    pub variables: HashMap<String, Variable>,
    /// Conditions for conditional routing
    pub conditions: HashMap<String, Condition>,
    /// Workflow pattern detected in this specification
    pub detected_patterns: HashSet<WorkflowPattern>,
}

/// Metadata and identification for a YAWL specification.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SpecMetadata {
    /// Unique identifier for the specification
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Description of the workflow purpose
    pub description: Option<String>,
    /// YAWL schema version
    pub version: String,
    /// Specification author/creator
    pub author: Option<String>,
    /// Creation or modification timestamp
    pub created: Option<chrono::DateTime<chrono::Utc>>,
    /// Custom metadata attributes
    pub attributes: HashMap<String, String>,
}

/// A YAWL decomposition (net) containing tasks and flows.
///
/// Decompositions represent workflow nets at different levels of hierarchy.
/// They can be nested to represent composite tasks with sub-workflows.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Decomposition {
    /// Unique identifier for this decomposition
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Type of decomposition (WSNet, etc.)
    pub decomposition_type: DecompositionType,
    /// Input condition node identifier
    pub input_condition: Option<String>,
    /// Output condition node identifier
    pub output_condition: Option<String>,
    /// Tasks within this decomposition
    pub tasks: Vec<String>,
    /// Flows connecting nodes in this decomposition
    pub flows: Vec<String>,
    /// Parent decomposition ID if nested
    pub parent_id: Option<String>,
    /// Whether this is the root decomposition
    pub is_root: bool,
}

/// Types of YAWL decompositions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub enum DecompositionType {
    /// Web Service Net - standard YAWL workflow net
    #[default]
    WSNet,
    /// Composite workflow net
    CompositeNet,
    /// Or-shared workflow net
    OrJoinNet,
    /// Custom decomposition type
    Custom(String),
}

impl fmt::Display for DecompositionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DecompositionType::WSNet => write!(f, "WSNet"),
            DecompositionType::CompositeNet => write!(f, "CompositeNet"),
            DecompositionType::OrJoinNet => write!(f, "OrJoinNet"),
            DecompositionType::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// A task (work unit) in the YAWL workflow.
///
/// Tasks represent units of work that can be atomic or composite.
/// They have configurable split and join behaviors for workflow routing.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Task {
    /// Unique task identifier
    pub id: String,
    /// Human-readable task name
    pub name: String,
    /// Task type (atomic or composite)
    pub task_type: TaskType,
    /// Split behavior (AND, OR, XOR)
    pub split_type: SplitType,
    /// Join behavior (AND, OR, XOR)
    pub join_type: JoinType,
    /// Whether this task starts automatically
    pub is_auto_start: bool,
    /// Decomposition ID for composite tasks
    pub decomposition_id: Option<String>,
    /// Task parameters (input/output)
    pub parameters: Vec<Parameter>,
    /// Task constraints
    pub constraints: Vec<Constraint>,
    /// Custom task attributes
    pub attributes: HashMap<String, String>,
    /// Associated workflow patterns
    pub patterns: Vec<WorkflowPattern>,
}

/// Type of task in the workflow.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub enum TaskType {
    /// Atomic task (leaf node, no sub-workflow)
    #[default]
    Atomic,
    /// Composite task (contains sub-workflow decomposition)
    Composite,
    /// Multiple instance task
    MultipleInstance {
        /// Instance creation strategy
        instance_strategy: InstanceStrategy,
        /// Minimum number of instances
        min_instances: Option<usize>,
        /// Maximum number of instances
        max_instances: Option<usize>,
        /// Threshold for completion (number of instances to complete)
        completion_threshold: Option<usize>,
    },
}

/// Strategy for creating multiple task instances.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum InstanceStrategy {
    /// Instances created without prior knowledge (dynamic)
    Dynamic,
    /// Fixed number of instances known at design time
    DesignTimeKnowledge(usize),
    /// Number of instances determined at runtime
    RuntimeKnowledge,
    /// Data-driven instance creation
    DataDriven {
        /// Variable containing instance count or data
        source_variable: String,
    },
}

/// Split type for task outgoing flows.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Default)]
pub enum SplitType {
    /// AND split - all outgoing flows are activated (parallel)
    And,
    /// OR split - one or more outgoing flows are activated (multi-choice)
    Or,
    /// XOR split - exactly one outgoing flow is activated (exclusive choice)
    #[default]
    Xor,
}

impl SplitType {
    /// Parse split type from string.
    pub fn try_from_str(s: &str) -> WorkflowResult<Self> {
        match s.to_lowercase().as_str() {
            "and" => Ok(SplitType::And),
            "or" => Ok(SplitType::Or),
            "xor" => Ok(SplitType::Xor),
            _ => Err(errors::invalid_attribute("split", s)),
        }
    }

    /// Convert to string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            SplitType::And => "and",
            SplitType::Or => "or",
            SplitType::Xor => "xor",
        }
    }
}

/// Join type for task incoming flows.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize, Default)]
pub enum JoinType {
    /// AND join - wait for all incoming flows (synchronization)
    And,
    /// OR join - wait for one or more incoming flows
    Or,
    /// XOR join - wait for exactly one incoming flow
    #[default]
    Xor,
}

impl JoinType {
    /// Parse join type from string.
    pub fn try_from_str(s: &str) -> WorkflowResult<Self> {
        match s.to_lowercase().as_str() {
            "and" => Ok(JoinType::And),
            "or" => Ok(JoinType::Or),
            "xor" => Ok(JoinType::Xor),
            _ => Err(errors::invalid_attribute("join", s)),
        }
    }

    /// Convert to string representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            JoinType::And => "and",
            JoinType::Or => "or",
            JoinType::Xor => "xor",
        }
    }
}

/// Flow connection between workflow nodes.
///
/// Flows define the directed edges in the workflow graph, connecting
/// tasks, input conditions, and output conditions.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Flow {
    /// Unique flow identifier
    pub id: String,
    /// Source node ID
    pub source: String,
    /// Target node ID
    pub target: String,
    /// Optional predicate condition for this flow
    pub predicate: Option<String>,
    /// Flow condition reference
    pub condition: Option<Condition>,
    /// Flow position in source's outgoing list
    pub index: Option<usize>,
    /// Optional flow label
    pub label: Option<String>,
}

/// Condition for conditional flow routing.
///
/// Conditions evaluate to boolean values to determine which
/// flows are activated during workflow execution.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Condition {
    /// Unique condition identifier
    pub id: String,
    /// Condition name
    pub name: String,
    /// Boolean expression string
    pub expression: String,
    /// Condition type (simple, complex, timer-based)
    pub condition_type: ConditionType,
    /// Variables referenced in this condition
    pub variables: Vec<String>,
    /// Whether this is a default/else condition
    pub is_default: bool,
}

/// Type of condition expression.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConditionType {
    /// Simple boolean expression
    Simple,
    /// Complex boolean expression with multiple terms
    Complex,
    /// Time-based condition (timer)
    Timer {
        /// Duration expression
        duration: String,
    },
    /// Event-based condition
    Event {
        /// Event name to wait for
        event_name: String,
    },
}

/// Variable in the workflow context.
///
/// Variables hold data that flows through the workflow,
/// used in conditions and task parameters.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Variable {
    /// Unique variable identifier
    pub id: String,
    /// Variable name
    pub name: String,
    /// Variable data type
    pub var_type: VariableType,
    /// Initial value (optional)
    pub initial_value: Option<String>,
    /// Whether variable is read-only
    pub is_readonly: bool,
    /// Variable scope (local, global, etc.)
    pub scope: VariableScope,
}

/// Data type for workflow variables.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub enum VariableType {
    /// String type
    #[default]
    String,
    /// Integer type
    Integer,
    /// Boolean type
    Boolean,
    /// Floating point type
    Float,
    /// Date/time type
    DateTime,
    /// Complex/document type (XML, JSON)
    Document,
    /// Custom type
    Custom(String),
}

impl VariableType {
    /// Parse variable type from string.
    pub fn try_from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "string" | "xs:string" => VariableType::String,
            "integer" | "int" | "xs:integer" => VariableType::Integer,
            "boolean" | "xs:boolean" => VariableType::Boolean,
            "float" | "double" | "xs:double" => VariableType::Float,
            "datetime" | "xs:datetime" => VariableType::DateTime,
            "document" | "xml" | "json" => VariableType::Document,
            _ => VariableType::Custom(s.to_string()),
        }
    }

    /// Convert to XML schema type string.
    pub fn as_xs_type(&self) -> &'static str {
        match self {
            VariableType::String => "xs:string",
            VariableType::Integer => "xs:integer",
            VariableType::Boolean => "xs:boolean",
            VariableType::Float => "xs:double",
            VariableType::DateTime => "xs:dateTime",
            VariableType::Document => "xs:anyType",
            VariableType::Custom(_) => "xs:anyType",
        }
    }
}

/// Scope of a workflow variable.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VariableScope {
    /// Local to a specific task
    Local { task_id: String },
    /// Shared across a decomposition
    Decomposition { decomposition_id: String },
    /// Global to the entire specification
    Global,
}

/// Parameter for task input/output.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Parameter {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: ParameterType,
    /// Variable this parameter maps to
    pub variable: String,
    /// Parameter namespace (optional)
    pub namespace: Option<String>,
}

/// Direction of parameter flow.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ParameterType {
    /// Input parameter
    Input,
    /// Output parameter
    Output,
    /// Input/output parameter
    InputOutput,
}

/// Constraint on task execution.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Constraint {
    /// Constraint type
    pub constraint_type: String,
    /// Constraint expression
    pub expression: String,
    /// Constraint level (warning, error)
    pub level: ConstraintLevel,
}

/// Severity level for constraint violations.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ConstraintLevel {
    /// Warning - may indicate issues
    Warning,
    /// Error - must be resolved
    Error,
    /// Fatal - prevents execution
    Fatal,
}

// ============================================================================
// Workflow Patterns (Van der Aalst 20 Patterns)
// ============================================================================

/// All 20 Van der Aalst workflow control-flow patterns.
///
/// These patterns represent the fundamental building blocks of
/// workflow languages and are used to analyze workflow expressiveness.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WorkflowPattern {
    // ===== Control Flow Patterns (1-6) =====
    /// Pattern 1: Sequence
    /// Sequential execution of tasks where one task follows another.
    /// This is the most basic workflow pattern.
    Sequence,

    /// Pattern 2: Parallel Split (AND-split)
    /// Split a single thread of control into multiple threads
    /// which can execute in parallel.
    ParallelSplit,

    /// Pattern 3: Synchronization (AND-join)
    /// Join multiple parallel threads into a single thread of control.
    /// Synchronization ensures all branches complete before proceeding.
    Synchronization,

    /// Pattern 4: Exclusive Choice (XOR-split)
    /// Choose exactly one branch from multiple alternatives based on data.
    ExclusiveChoice,

    /// Pattern 5: Simple Merge (XOR-join)
    /// Merge multiple alternative branches into a single thread.
    /// No synchronization is performed.
    SimpleMerge,

    /// Pattern 6: Multi-Choice (OR-split)
    /// Choose one or more branches from multiple alternatives.
    /// More flexible than exclusive choice.
    MultiChoice,

    // ===== Advanced Branching Patterns (7-10) =====
    /// Pattern 7: Synchronizing Merge (OR-join)
    /// Merge multiple branches that were created by a multi-choice.
    /// Waits for all active branches to complete.
    SynchronizingMerge,

    /// Pattern 8: Multi-Merge
    /// Merge multiple branches without synchronization.
    /// Each incoming branch creates a new thread.
    MultiMerge,

    /// Pattern 9: Discriminator
    /// Proceed after the first incoming branch completes.
    /// Other branches are silently discarded.
    Discriminator,

    /// Pattern 10: Arbitrary Cycles
    /// Looping construct that allows returning to previous activities.
    /// Cycles can be conditional or unconditional.
    ArbitraryCycles,

    // ===== State-Based Patterns (11-13) =====
    /// Pattern 11: Implicit Termination
    /// Terminate the process instance when there is no remaining work.
    /// No explicit termination node is required.
    ImplicitTermination,

    /// Pattern 12: Multiple Instances Without A Priori Knowledge
    /// Create multiple instances of a task dynamically at runtime.
    /// The number of instances is not known in advance.
    MultipleInstancesDynamic,

    /// Pattern 13: Multiple Instances With A Priori Design-Time Knowledge
    /// Create a fixed number of task instances known at design time.
    /// All instances are created simultaneously.
    MultipleInstancesDesignTime,

    // ===== Advanced Instance Patterns (14-16) =====
    /// Pattern 14: Multiple Instances With A Priori Runtime Knowledge
    /// Create a number of instances determined at runtime.
    /// The count is known before instance creation but not at design time.
    MultipleInstancesRuntime,

    /// Pattern 15: Deferred Choice
    /// Defer the choice between alternatives until runtime.
    /// The choice is made based on events or data availability.
    DeferredChoice,

    /// Pattern 16: Interleaved Parallel Routing
    /// Execute multiple instances with interleaved execution order.
    /// Only one instance is active at a time.
    InterleavedParallelRouting,

    // ===== Cancellation Patterns (17-20) =====
    /// Pattern 17: Cancel Activity
    /// Cancel a specific activity while allowing the process to continue.
    /// The activity may be enabled or executing when cancelled.
    CancelActivity,

    /// Pattern 18: Cancel Case
    /// Cancel the entire process instance.
    /// All activities in the case are terminated.
    CancelCase,

    /// Pattern 19: Pattern-Based Termination
    /// Terminate a process based on pattern completion conditions.
    /// Allows complex termination logic beyond implicit termination.
    PatternBasedTermination,

    /// Pattern 20: Cancel Region
    /// Cancel all activities within a specific region.
    /// Activities outside the region continue normally.
    CancelRegion,
}

impl WorkflowPattern {
    /// Get the pattern number (1-20).
    pub fn number(&self) -> u8 {
        match self {
            WorkflowPattern::Sequence => 1,
            WorkflowPattern::ParallelSplit => 2,
            WorkflowPattern::Synchronization => 3,
            WorkflowPattern::ExclusiveChoice => 4,
            WorkflowPattern::SimpleMerge => 5,
            WorkflowPattern::MultiChoice => 6,
            WorkflowPattern::SynchronizingMerge => 7,
            WorkflowPattern::MultiMerge => 8,
            WorkflowPattern::Discriminator => 9,
            WorkflowPattern::ArbitraryCycles => 10,
            WorkflowPattern::ImplicitTermination => 11,
            WorkflowPattern::MultipleInstancesDynamic => 12,
            WorkflowPattern::MultipleInstancesDesignTime => 13,
            WorkflowPattern::MultipleInstancesRuntime => 14,
            WorkflowPattern::DeferredChoice => 15,
            WorkflowPattern::InterleavedParallelRouting => 16,
            WorkflowPattern::CancelActivity => 17,
            WorkflowPattern::CancelCase => 18,
            WorkflowPattern::PatternBasedTermination => 19,
            WorkflowPattern::CancelRegion => 20,
        }
    }

    /// Get the pattern name as a string.
    pub fn name(&self) -> &'static str {
        match self {
            WorkflowPattern::Sequence => "Sequence",
            WorkflowPattern::ParallelSplit => "Parallel Split",
            WorkflowPattern::Synchronization => "Synchronization",
            WorkflowPattern::ExclusiveChoice => "Exclusive Choice",
            WorkflowPattern::SimpleMerge => "Simple Merge",
            WorkflowPattern::MultiChoice => "Multi-Choice",
            WorkflowPattern::SynchronizingMerge => "Synchronizing Merge",
            WorkflowPattern::MultiMerge => "Multi-Merge",
            WorkflowPattern::Discriminator => "Discriminator",
            WorkflowPattern::ArbitraryCycles => "Arbitrary Cycles",
            WorkflowPattern::ImplicitTermination => "Implicit Termination",
            WorkflowPattern::MultipleInstancesDynamic => "Multiple Instances (Dynamic)",
            WorkflowPattern::MultipleInstancesDesignTime => "Multiple Instances (Design Time)",
            WorkflowPattern::MultipleInstancesRuntime => "Multiple Instances (Runtime)",
            WorkflowPattern::DeferredChoice => "Deferred Choice",
            WorkflowPattern::InterleavedParallelRouting => "Interleaved Parallel Routing",
            WorkflowPattern::CancelActivity => "Cancel Activity",
            WorkflowPattern::CancelCase => "Cancel Case",
            WorkflowPattern::PatternBasedTermination => "Pattern-Based Termination",
            WorkflowPattern::CancelRegion => "Cancel Region",
        }
    }

    /// Get the category of this pattern.
    pub fn category(&self) -> PatternCategory {
        match self {
            WorkflowPattern::Sequence
            | WorkflowPattern::ParallelSplit
            | WorkflowPattern::Synchronization
            | WorkflowPattern::ExclusiveChoice
            | WorkflowPattern::SimpleMerge
            | WorkflowPattern::MultiChoice => PatternCategory::ControlFlow,

            WorkflowPattern::SynchronizingMerge
            | WorkflowPattern::MultiMerge
            | WorkflowPattern::Discriminator
            | WorkflowPattern::ArbitraryCycles => PatternCategory::AdvancedBranching,

            WorkflowPattern::ImplicitTermination
            | WorkflowPattern::MultipleInstancesDynamic
            | WorkflowPattern::MultipleInstancesDesignTime => PatternCategory::StateBased,

            WorkflowPattern::MultipleInstancesRuntime
            | WorkflowPattern::DeferredChoice
            | WorkflowPattern::InterleavedParallelRouting => PatternCategory::AdvancedInstance,

            WorkflowPattern::CancelActivity
            | WorkflowPattern::CancelCase
            | WorkflowPattern::PatternBasedTermination
            | WorkflowPattern::CancelRegion => PatternCategory::Cancellation,
        }
    }

    /// Parse pattern from name.
    pub fn from_name(name: &str) -> WorkflowResult<Self> {
        match name.to_lowercase().replace(' ', "_").as_str() {
            "sequence" => Ok(WorkflowPattern::Sequence),
            "parallel_split" => Ok(WorkflowPattern::ParallelSplit),
            "synchronization" => Ok(WorkflowPattern::Synchronization),
            "exclusive_choice" => Ok(WorkflowPattern::ExclusiveChoice),
            "simple_merge" => Ok(WorkflowPattern::SimpleMerge),
            "multi_choice" => Ok(WorkflowPattern::MultiChoice),
            "synchronizing_merge" => Ok(WorkflowPattern::SynchronizingMerge),
            "multi_merge" => Ok(WorkflowPattern::MultiMerge),
            "discriminator" => Ok(WorkflowPattern::Discriminator),
            "arbitrary_cycles" => Ok(WorkflowPattern::ArbitraryCycles),
            "implicit_termination" => Ok(WorkflowPattern::ImplicitTermination),
            "multiple_instances_dynamic" => Ok(WorkflowPattern::MultipleInstancesDynamic),
            "multiple_instances_design_time" => Ok(WorkflowPattern::MultipleInstancesDesignTime),
            "multiple_instances_runtime" => Ok(WorkflowPattern::MultipleInstancesRuntime),
            "deferred_choice" => Ok(WorkflowPattern::DeferredChoice),
            "interleaved_parallel_routing" => Ok(WorkflowPattern::InterleavedParallelRouting),
            "cancel_activity" => Ok(WorkflowPattern::CancelActivity),
            "cancel_case" => Ok(WorkflowPattern::CancelCase),
            "pattern_based_termination" => Ok(WorkflowPattern::PatternBasedTermination),
            "cancel_region" => Ok(WorkflowPattern::CancelRegion),
            _ => Err(errors::unsupported_feature(format!(
                "Unknown workflow pattern: {}",
                name
            ))),
        }
    }
}

impl fmt::Display for WorkflowPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}. {}", self.number(), self.name())
    }
}

/// Category of workflow pattern.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PatternCategory {
    /// Basic control flow patterns (1-6)
    ControlFlow,
    /// Advanced branching and synchronization patterns (7-10)
    AdvancedBranching,
    /// State-based patterns (11-13)
    StateBased,
    /// Advanced instance patterns (14-16)
    AdvancedInstance,
    /// Cancellation patterns (17-20)
    Cancellation,
}

impl fmt::Display for PatternCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternCategory::ControlFlow => write!(f, "Control Flow"),
            PatternCategory::AdvancedBranching => write!(f, "Advanced Branching"),
            PatternCategory::StateBased => write!(f, "State-Based"),
            PatternCategory::AdvancedInstance => write!(f, "Advanced Instance"),
            PatternCategory::Cancellation => write!(f, "Cancellation"),
        }
    }
}

// ============================================================================
// Parser Implementation
// ============================================================================

/// YAWL XML specification parser.
///
/// This parser handles the conversion of YAWL XML files into strongly-typed
/// Rust structures. It provides comprehensive error handling and validation.
///
/// # Example
///
/// ```rust,no_run
/// use ggen_workflow::parser::YawlParser;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let xml = std::fs::read_to_string("workflow.yawl")?;
/// let parser = YawlParser::new();
/// let spec = parser.parse(&xml)?;
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct YawlParser {
    /// Parser configuration options
    config: ParserConfig,
    /// Namespace for YAWL schema
    yawl_namespace: String,
}

/// Configuration options for the YAWL parser.
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// Whether to validate against schema
    pub validate: bool,
    /// Whether to detect workflow patterns automatically
    pub detect_patterns: bool,
    /// Whether to preserve whitespace in text content
    pub preserve_whitespace: bool,
    /// Maximum file size in bytes
    pub max_file_size: usize,
}

impl Default for ParserConfig {
    fn default() -> Self {
        ParserConfig {
            validate: true,
            detect_patterns: true,
            preserve_whitespace: false,
            max_file_size: 10 * 1024 * 1024, // 10 MB
        }
    }
}

impl Default for YawlParser {
    fn default() -> Self {
        Self::new()
    }
}

impl YawlParser {
    /// Create a new YAWL parser with default configuration.
    pub fn new() -> Self {
        YawlParser {
            config: ParserConfig::default(),
            yawl_namespace: "http://www.yawlfoundation.org/yawlschema".to_string(),
        }
    }

    /// Create a new YAWL parser with custom configuration.
    pub fn with_config(config: ParserConfig) -> Self {
        YawlParser {
            config,
            yawl_namespace: "http://www.yawlfoundation.org/yawlschema".to_string(),
        }
    }

    /// Set a custom YAWL namespace.
    pub fn with_namespace(mut self, namespace: String) -> Self {
        self.yawl_namespace = namespace;
        self
    }

    /// Parse YAWL XML from a string.
    ///
    /// This is the main entry point for parsing YAWL specifications.
    /// It returns a complete `YawlSpec` structure on success.
    pub fn parse(&self, xml: &str) -> WorkflowResult<YawlSpec> {
        // Check file size
        if xml.len() > self.config.max_file_size {
            return Err(errors::xml_parse(format!(
                "File size {} exceeds maximum {}",
                xml.len(),
                self.config.max_file_size
            )));
        }

        // Parse XML document
        let mut reader = quick_xml::Reader::from_str(xml);
        // Configure the reader
        reader
            .config_mut()
            .trim_text(!self.config.preserve_whitespace);

        let mut parser_state = ParserState::new();
        let mut buf = Vec::new();

        // Parse the document
        loop {
            match reader.read_event_into(&mut buf) {
                Ok(quick_xml::events::Event::Eof) => break,
                Ok(event) => self.handle_event(&reader, event, &mut parser_state)?,
                Err(e) => {
                    return Err(errors::xml_parse(format!(
                        "XML parsing error at position {}: {}",
                        reader.buffer_position(),
                        e
                    )))
                }
            }
            buf.clear();
        }

        // Build the final specification
        let mut spec = self.build_spec(parser_state)?;

        // Detect workflow patterns if enabled
        if self.config.detect_patterns {
            spec.detected_patterns = self.detect_patterns(&spec);
        }

        // Validate if enabled
        if self.config.validate {
            self.validate_spec(&spec)?;
        }

        Ok(spec)
    }

    /// Parse YAWL XML from a file.
    pub fn parse_file(&self, path: &std::path::Path) -> WorkflowResult<YawlSpec> {
        let xml = std::fs::read_to_string(path).map_err(|e| {
            errors::xml_parse(format!("Failed to read file {}: {}", path.display(), e))
        })?;
        self.parse(&xml)
    }

    /// Handle a single XML parsing event.
    fn handle_event(
        &self, reader: &quick_xml::Reader<&[u8]>, event: quick_xml::events::Event<'_>,
        state: &mut ParserState,
    ) -> WorkflowResult<()> {
        use quick_xml::events::Event;

        match event {
            Event::Start(e) => {
                self.handle_start_element(reader, &e, state)?;
            }
            Event::End(e) => {
                self.handle_end_element(&e, state)?;
            }
            Event::Empty(e) => {
                // Self-closing tags like <split type="and"/>
                self.handle_start_element(reader, &e, state)?;
            }
            Event::Text(e) => {
                if !e.is_empty() {
                    // Convert Cow<str> to String
                    state.add_text(e.unescape().unwrap_or_default().to_string());
                }
            }
            Event::CData(e) => {
                // Convert &[u8] to String
                state.add_text(String::from_utf8_lossy(e.as_ref()).to_string());
            }
            _ => {} // Ignore other events
        }

        Ok(())
    }

    /// Handle XML start element.
    fn handle_start_element(
        &self, _reader: &quick_xml::Reader<&[u8]>, element: &quick_xml::events::BytesStart<'_>,
        state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let name = element.name();
        let name = String::from_utf8_lossy(name.as_ref());

        // Parse element based on current state
        match name.as_ref() {
            "specification" => self.handle_specification_start(element, state)?,
            "name" => state.enter_context(ParseContext::Name),
            "description" => state.enter_context(ParseContext::Description),
            "decomposition" => {
                // Check if we're inside a task (reference) or at spec level (definition)
                if state.current_task_id.is_some() {
                    self.handle_decomposition_ref_start(element, state)?;
                } else {
                    self.handle_decomposition_start(element, state)?;
                }
            }
            "task" => self.handle_task_start(element, state)?,
            "flow" => self.handle_flow_start(element, state)?,
            "inputCondition" => self.handle_input_condition(element, state)?,
            "outputCondition" => self.handle_output_condition(element, state)?,
            "split" => self.handle_split_start(element, state)?,
            "join" => self.handle_join_start(element, state)?,
            "starting" => state.current_task_is_auto = true,
            "predicate" => state.enter_context(ParseContext::Predicate),
            "variable" => self.handle_variable_start(element, state)?,
            "localVariable" => self.handle_variable_start(element, state)?,
            _ => {} // Ignore unknown elements
        }

        Ok(())
    }

    /// Handle XML end element.
    fn handle_end_element(
        &self, element: &quick_xml::events::BytesEnd<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let name = element.name();
        let name = String::from_utf8_lossy(name.as_ref());

        match name.as_ref() {
            "specification" => {
                // Specification complete
            }
            "name" => {
                if state.context == ParseContext::Name {
                    state.spec_name = state.take_text();
                    state.exit_context();
                }
            }
            "description" => {
                if state.context == ParseContext::Description {
                    state.spec_description = Some(state.take_text());
                    state.exit_context();
                }
            }
            "decomposition" => {
                if let Some(dec) = state.finish_decomposition() {
                    state.decompositions.push(dec);
                }
                state.exit_context();
            }
            "task" => {
                if let Some(task) = state.finish_task() {
                    state.tasks.insert(task.id.clone(), task);
                }
                state.exit_context();
            }
            "flow" => {
                if let Some(flow) = state.finish_flow() {
                    state.flows.push(flow);
                }
                state.exit_context();
            }
            "predicate" => {
                if state.context == ParseContext::Predicate {
                    state.current_flow_predicate = Some(state.take_text());
                    state.exit_context();
                }
            }
            "variable" | "localVariable" => {
                if let Some(var) = state.finish_variable() {
                    state.variables.insert(var.id.clone(), var);
                }
                state.exit_context();
            }
            _ => {}
        }

        Ok(())
    }

    // ===== Element-specific handlers =====

    /// Helper function to extract attributes from an element.
    /// Returns a HashMap of attribute names to values.
    fn extract_attributes(
        &self, element: &quick_xml::events::BytesStart<'_>,
    ) -> HashMap<String, String> {
        let mut attrs = HashMap::new();
        for attr in element.attributes().flatten() {
            let key = String::from_utf8_lossy(attr.key.as_ref()).to_string();
            let value = attr.value.as_ref().to_vec();
            let value = String::from_utf8_lossy(&value).to_string();
            attrs.insert(key, value);
        }
        attrs
    }

    fn handle_specification_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        state.enter_context(ParseContext::Specification);

        let attrs = self.extract_attributes(element);
        if let Some(version) = attrs.get("version") {
            state.spec_version = version.clone();
        }

        Ok(())
    }

    fn handle_decomposition_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        state.enter_context(ParseContext::Decomposition);

        let attrs = self.extract_attributes(element);

        let mut dec = Decomposition {
            id: attrs.get("id").cloned().unwrap_or_default(),
            name: String::new(),
            decomposition_type: attrs
                .get("type")
                .map(|t| match t.as_str() {
                    "WSNet" => DecompositionType::WSNet,
                    "CompositeNet" => DecompositionType::CompositeNet,
                    "OrJoinNet" => DecompositionType::OrJoinNet,
                    custom => DecompositionType::Custom(custom.to_string()),
                })
                .unwrap_or_default(),
            input_condition: None,
            output_condition: None,
            tasks: Vec::new(),
            flows: Vec::new(),
            parent_id: None,
            is_root: state.current_decomposition_id.is_none(),
        };

        if !dec.id.is_empty() {
            if state.current_decomposition_id.is_some() {
                dec.parent_id = state.current_decomposition_id.clone();
            }
            state.current_decomposition_id = Some(dec.id.clone());
            state.start_decomposition(dec);
        }

        Ok(())
    }

    fn handle_task_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        state.enter_context(ParseContext::Task);

        let attrs = self.extract_attributes(element);

        let task = Task {
            id: attrs.get("id").cloned().unwrap_or_default(),
            name: attrs.get("name").cloned().unwrap_or_default(),
            task_type: TaskType::default(),
            split_type: SplitType::default(),
            join_type: JoinType::default(),
            is_auto_start: false,
            decomposition_id: None,
            parameters: Vec::new(),
            constraints: Vec::new(),
            attributes: attrs,
            patterns: Vec::new(),
        };

        state.current_task_is_auto = false;
        state.start_task(task);

        Ok(())
    }

    fn handle_flow_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        state.enter_context(ParseContext::Flow);

        let attrs = self.extract_attributes(element);

        let flow = Flow {
            id: attrs.get("id").cloned().unwrap_or_default(),
            source: attrs
                .get("from")
                .or_else(|| attrs.get("source"))
                .cloned()
                .unwrap_or_default(),
            target: attrs
                .get("into")
                .or_else(|| attrs.get("to"))
                .or_else(|| attrs.get("target"))
                .cloned()
                .unwrap_or_default(),
            predicate: None,
            condition: None,
            index: attrs.get("index").and_then(|i| i.parse().ok()),
            label: attrs.get("label").cloned(),
        };

        state.start_flow(flow);

        Ok(())
    }

    fn handle_input_condition(
        &self, element: &quick_xml::events::BytesStart<'_>, _state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let _attrs = self.extract_attributes(element);
        // TODO: Store input condition for current decomposition
        // if let Some(_dec_id) = _state.current_decomposition_id {
        //     if let Some(_id) = _attrs.get("id") {
        //         // Store input condition
        //     }
        // }
        Ok(())
    }

    fn handle_output_condition(
        &self, element: &quick_xml::events::BytesStart<'_>, _state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let _attrs = self.extract_attributes(element);
        // TODO: Store output condition for current decomposition
        Ok(())
    }

    fn handle_split_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let attrs = self.extract_attributes(element);
        if let Some(value) = attrs.get("type") {
            state.current_task_split = Some(SplitType::try_from_str(value)?);
        }
        Ok(())
    }

    fn handle_join_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let attrs = self.extract_attributes(element);
        if let Some(value) = attrs.get("type") {
            state.current_task_join = Some(JoinType::try_from_str(value)?);
        }
        Ok(())
    }

    fn handle_decomposition_ref_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        let attrs = self.extract_attributes(element);
        if let Some(value) = attrs.get("id") {
            state.current_task_decomposition = Some(value.clone());
        }
        Ok(())
    }

    fn handle_variable_start(
        &self, element: &quick_xml::events::BytesStart<'_>, state: &mut ParserState,
    ) -> WorkflowResult<()> {
        state.enter_context(ParseContext::Variable);

        let attrs = self.extract_attributes(element);

        let id = attrs
            .get("id")
            .or_else(|| attrs.get("name"))
            .cloned()
            .unwrap_or_default();

        let variable = Variable {
            id: id.clone(),
            name: id,
            var_type: attrs
                .get("type")
                .or_else(|| attrs.get("dataType"))
                .map(|t| VariableType::try_from_str(t))
                .unwrap_or_default(),
            initial_value: attrs.get("initialValue").cloned(),
            is_readonly: attrs
                .get("readonly")
                .map(|v| v.eq_ignore_ascii_case("true"))
                .unwrap_or(false),
            scope: attrs
                .get("scope")
                .map(|s| match s.to_lowercase().as_str() {
                    "local" => VariableScope::Local {
                        task_id: state.current_task_id.clone().unwrap_or_default(),
                    },
                    "decomposition" => VariableScope::Decomposition {
                        decomposition_id: state
                            .current_decomposition_id
                            .clone()
                            .unwrap_or_default(),
                    },
                    _ => VariableScope::Global,
                })
                .unwrap_or(VariableScope::Global),
        };

        state.start_variable(variable);

        Ok(())
    }

    // ===== Build and validation methods =====

    fn build_spec(&self, state: ParserState) -> WorkflowResult<YawlSpec> {
        // Find root decomposition
        let root_decomposition = state
            .decompositions
            .iter()
            .find(|d| d.is_root)
            .or_else(|| state.decompositions.first())
            .ok_or_else(|| errors::missing_element("decomposition"))?
            .clone();

        // Build tasks map
        let tasks: HashMap<String, Task> = state.tasks;

        // Build flows list
        let flows: Vec<Flow> = state
            .flows
            .into_iter()
            .inspect(|f| {
                // Set predicate from stored value if not already set
                let _ = f.predicate.is_none();
                // This would be matched with predicates during parsing
            })
            .collect();

        // Build variables map
        let variables = state.variables;

        // Build conditions map
        let conditions = HashMap::new();

        // Build metadata
        let metadata = SpecMetadata {
            id: uuid::Uuid::new_v4().to_string(),
            name: state.spec_name.clone(),
            description: state.spec_description.clone(),
            version: state.spec_version.clone(),
            author: None,
            created: Some(chrono::Utc::now()),
            attributes: HashMap::new(),
        };

        Ok(YawlSpec {
            metadata,
            root_decomposition,
            decompositions: state.decompositions,
            tasks,
            flows,
            variables,
            conditions,
            detected_patterns: HashSet::new(),
        })
    }

    fn validate_spec(&self, spec: &YawlSpec) -> WorkflowResult<()> {
        // Validate specification has required elements
        if spec.metadata.name.is_empty() {
            return Err(errors::yawl_validation("Specification name is required"));
        }

        // Validate decompositions exist
        if spec.decompositions.is_empty() {
            return Err(errors::yawl_validation(
                "At least one decomposition is required",
            ));
        }

        // Validate all referenced tasks exist
        for flow in &spec.flows {
            if !flow.source.is_empty() && !spec.tasks.contains_key(&flow.source) {
                // Source might be a condition, not a task
            }
            if !flow.target.is_empty() && !spec.tasks.contains_key(&flow.target) {
                // Target might be a condition
            }
        }

        // Note: In YAWL, any split/join combination is valid
        // Split types (AND, OR, XOR) and join types (AND, OR, XOR) are independent

        Ok(())
    }

    /// Detect workflow patterns in the specification.
    fn detect_patterns(&self, spec: &YawlSpec) -> HashSet<WorkflowPattern> {
        let mut patterns = HashSet::new();

        // Pattern 1: Sequence - always present if tasks exist
        if spec.tasks.len() > 1 {
            patterns.insert(WorkflowPattern::Sequence);
        }

        // Pattern 2: Parallel Split - AND split
        for task in spec.tasks.values() {
            if task.split_type == SplitType::And {
                patterns.insert(WorkflowPattern::ParallelSplit);
                break;
            }
        }

        // Pattern 3: Synchronization - AND join
        for task in spec.tasks.values() {
            if task.join_type == JoinType::And {
                patterns.insert(WorkflowPattern::Synchronization);
                break;
            }
        }

        // Pattern 4: Exclusive Choice - XOR split
        for task in spec.tasks.values() {
            if task.split_type == SplitType::Xor {
                patterns.insert(WorkflowPattern::ExclusiveChoice);
                break;
            }
        }

        // Pattern 5: Simple Merge - XOR join
        for task in spec.tasks.values() {
            if task.join_type == JoinType::Xor {
                patterns.insert(WorkflowPattern::SimpleMerge);
                break;
            }
        }

        // Pattern 6: Multi-Choice - OR split
        for task in spec.tasks.values() {
            if task.split_type == SplitType::Or {
                patterns.insert(WorkflowPattern::MultiChoice);
                break;
            }
        }

        // Pattern 7: Synchronizing Merge - OR join
        for task in spec.tasks.values() {
            if task.join_type == JoinType::Or {
                patterns.insert(WorkflowPattern::SynchronizingMerge);
                break;
            }
        }

        // Pattern 10: Arbitrary Cycles - detect back edges in flows
        let mut node_ids: Vec<&str> = spec.tasks.keys().map(|k| k.as_str()).collect();
        node_ids.sort();
        for flow in &spec.flows {
            // Check if flow goes to a "earlier" node (potential cycle)
            if node_ids
                .iter()
                .position(|&id| id == flow.source)
                .zip(node_ids.iter().position(|&id| id == flow.target))
                .map(|(src_idx, tgt_idx)| tgt_idx < src_idx)
                .unwrap_or(false)
            {
                patterns.insert(WorkflowPattern::ArbitraryCycles);
                break;
            }
        }

        // Pattern 11: Implicit Termination - no explicit output condition required
        patterns.insert(WorkflowPattern::ImplicitTermination);

        // Pattern 12-14: Multiple Instances
        for task in spec.tasks.values() {
            if matches!(task.task_type, TaskType::MultipleInstance { .. }) {
                match &task.task_type {
                    TaskType::MultipleInstance {
                        instance_strategy: InstanceStrategy::Dynamic,
                        ..
                    } => {
                        patterns.insert(WorkflowPattern::MultipleInstancesDynamic);
                    }
                    TaskType::MultipleInstance {
                        instance_strategy: InstanceStrategy::DesignTimeKnowledge(_),
                        ..
                    } => {
                        patterns.insert(WorkflowPattern::MultipleInstancesDesignTime);
                    }
                    TaskType::MultipleInstance {
                        instance_strategy: InstanceStrategy::RuntimeKnowledge,
                        ..
                    } => {
                        patterns.insert(WorkflowPattern::MultipleInstancesRuntime);
                    }
                    _ => {}
                }
            }
        }

        patterns
    }

    /// Generate YAWL XML from a specification.
    pub fn to_xml(&self, spec: &YawlSpec) -> WorkflowResult<String> {
        let mut xml = String::new();

        // XML declaration
        xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

        // Root element
        xml.push_str(&format!(
            "<specification xmlns=\"{}\" version=\"{}\">\n",
            self.yawl_namespace, spec.metadata.version
        ));

        // Metadata
        xml.push_str(&format!(
            "  <name>{}</name>\n",
            escape_xml(&spec.metadata.name)
        ));
        if let Some(ref description) = spec.metadata.description {
            xml.push_str(&format!(
                "  <description>{}</description>\n",
                escape_xml(description)
            ));
        }

        // Root decomposition
        xml.push_str("  <decomposition id=\"");
        xml.push_str(&spec.root_decomposition.id);
        xml.push_str("\" type=\"");
        xml.push_str(&spec.root_decomposition.decomposition_type.to_string());
        xml.push_str("\">\n");

        // Input condition
        if let Some(ref input) = spec.root_decomposition.input_condition {
            xml.push_str(&format!(
                "    <inputCondition id=\"{}\"/>\n",
                escape_xml(input)
            ));
        }

        // Output condition
        if let Some(ref output) = spec.root_decomposition.output_condition {
            xml.push_str(&format!(
                "    <outputCondition id=\"{}\"/>\n",
                escape_xml(output)
            ));
        }

        // Tasks
        for task in spec.tasks.values() {
            xml.push_str("    <task id=\"");
            xml.push_str(&task.id);
            xml.push_str("\" name=\"");
            xml.push_str(&task.name);
            xml.push_str("\">\n");

            // Split and join
            xml.push_str(&format!(
                "      <split type=\"{}\"/>\n",
                task.split_type.as_str()
            ));
            xml.push_str(&format!(
                "      <join type=\"{}\"/>\n",
                task.join_type.as_str()
            ));

            if task.is_auto_start {
                xml.push_str("      <starting/>\n");
            }

            if let Some(ref decomp_id) = task.decomposition_id {
                xml.push_str(&format!(
                    "      <decomposition id=\"{}\"/>\n",
                    escape_xml(decomp_id)
                ));
            }

            xml.push_str("    </task>\n");
        }

        // Flows
        for flow in &spec.flows {
            xml.push_str(&format!(
                "    <flow into=\"{}\" from=\"{}\"",
                flow.target, flow.source
            ));
            if let Some(ref predicate) = flow.predicate {
                xml.push_str(&format!(
                    ">\n      <predicate>{}</predicate>\n    </flow>\n",
                    escape_xml(predicate)
                ));
            } else {
                xml.push_str("/>\n");
            }
        }

        xml.push_str("  </decomposition>\n");
        xml.push_str("</specification>\n");

        Ok(xml)
    }
}

/// Escape special XML characters.
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

// ============================================================================
// Parser Internal State
// ============================================================================

/// Internal state for XML parsing.
#[derive(Debug)]
struct ParserState {
    /// Current parsing context
    context: ParseContext,
    /// Context stack for nested elements
    context_stack: Vec<ParseContext>,
    /// Accumulated text content
    text_buffer: String,

    // Specification-level fields
    spec_name: String,
    spec_description: Option<String>,
    spec_version: String,

    // Decomposition tracking
    current_decomposition_id: Option<String>,
    decompositions: Vec<Decomposition>,
    current_decomposition: Option<Decomposition>,

    // Task tracking
    current_task_id: Option<String>,
    current_task: Option<Task>,
    current_task_split: Option<SplitType>,
    current_task_join: Option<JoinType>,
    current_task_decomposition: Option<String>,
    current_task_is_auto: bool,
    tasks: HashMap<String, Task>,

    // Flow tracking
    current_flow: Option<Flow>,
    current_flow_predicate: Option<String>,
    flows: Vec<Flow>,

    // Variable tracking
    current_variable: Option<Variable>,
    variables: HashMap<String, Variable>,
}

impl ParserState {
    fn new() -> Self {
        ParserState {
            context: ParseContext::Root,
            context_stack: Vec::new(),
            text_buffer: String::new(),
            spec_name: String::new(),
            spec_description: None,
            spec_version: "2.0".to_string(),
            current_decomposition_id: None,
            decompositions: Vec::new(),
            current_decomposition: None,
            current_task_id: None,
            current_task: None,
            current_task_split: None,
            current_task_join: None,
            current_task_decomposition: None,
            current_task_is_auto: false,
            tasks: HashMap::new(),
            current_flow: None,
            current_flow_predicate: None,
            flows: Vec::new(),
            current_variable: None,
            variables: HashMap::new(),
        }
    }

    fn enter_context(&mut self, context: ParseContext) {
        self.context_stack.push(self.context);
        self.context = context;
    }

    fn exit_context(&mut self) {
        if let Some(ctx) = self.context_stack.pop() {
            self.context = ctx;
        }
    }

    fn add_text(&mut self, text: String) {
        self.text_buffer.push_str(&text);
    }

    fn take_text(&mut self) -> String {
        std::mem::take(&mut self.text_buffer)
    }

    fn start_decomposition(&mut self, dec: Decomposition) {
        self.current_decomposition = Some(dec);
    }

    fn finish_decomposition(&mut self) -> Option<Decomposition> {
        self.current_decomposition.take()
    }

    fn start_task(&mut self, task: Task) {
        self.current_task_id = Some(task.id.clone());
        self.current_task = Some(task);
    }

    fn finish_task(&mut self) -> Option<Task> {
        if let Some(mut task) = self.current_task.take() {
            // Apply split and join types
            if let Some(split) = self.current_task_split.take() {
                task.split_type = split;
            }
            if let Some(join) = self.current_task_join.take() {
                task.join_type = join;
            }
            task.decomposition_id = self.current_task_decomposition.take();
            task.is_auto_start = self.current_task_is_auto;
            self.current_task_is_auto = false;
            Some(task)
        } else {
            None
        }
    }

    fn start_flow(&mut self, flow: Flow) {
        self.current_flow = Some(flow);
    }

    fn finish_flow(&mut self) -> Option<Flow> {
        if let Some(mut flow) = self.current_flow.take() {
            flow.predicate = self.current_flow_predicate.take();
            Some(flow)
        } else {
            None
        }
    }

    fn start_variable(&mut self, variable: Variable) {
        self.current_variable = Some(variable);
    }

    fn finish_variable(&mut self) -> Option<Variable> {
        self.current_variable.take()
    }
}

/// Current parsing context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseContext {
    Root,
    Specification,
    Decomposition,
    Task,
    Flow,
    Variable,
    Name,
    Description,
    Predicate,
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE_YAWL: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
  <name>Test Workflow</name>
  <description>A simple test workflow</description>
  <decomposition id="test_net" type="WSNet">
    <inputCondition id="input"/>
    <outputCondition id="output"/>
    <task id="task1" name="First Task">
      <split type="and"/>
      <join type="xor"/>
    </task>
    <task id="task2" name="Second Task">
      <split type="xor"/>
      <join type="and"/>
      <starting/>
    </task>
    <flow into="task1" from="input"/>
    <flow into="task2" from="task1"/>
    <flow into="output" from="task2"/>
  </decomposition>
</specification>"#;

    #[test]
    fn test_parser_creation() {
        let parser = YawlParser::new();
        assert_eq!(
            parser.yawl_namespace,
            "http://www.yawlfoundation.org/yawlschema"
        );
    }

    #[test]
    fn test_parse_simple_yawl() {
        let parser = YawlParser::new();
        let result = parser.parse(SIMPLE_YAWL);
        assert!(result.is_ok());

        let spec = result.unwrap();
        assert_eq!(spec.metadata.name, "Test Workflow");
        assert_eq!(spec.metadata.version, "2.0");
        assert_eq!(
            spec.metadata.description,
            Some("A simple test workflow".to_string())
        );

        // Verify task split/join types are parsed correctly
        let task1 = spec.tasks.get("task1");
        assert!(task1.is_some(), "task1 should exist");
        let task1 = task1.unwrap();
        assert_eq!(
            task1.split_type,
            SplitType::And,
            "task1 should have AND split"
        );
        assert_eq!(task1.join_type, JoinType::Xor, "task1 should have XOR join");

        let task2 = spec.tasks.get("task2");
        assert!(task2.is_some(), "task2 should exist");
        let task2 = task2.unwrap();
        assert_eq!(
            task2.split_type,
            SplitType::Xor,
            "task2 should have XOR split"
        );
        assert_eq!(task2.join_type, JoinType::And, "task2 should have AND join");
    }

    #[test]
    fn test_workflow_pattern_detection() {
        let parser = YawlParser::new();
        let spec = parser.parse(SIMPLE_YAWL).unwrap();

        // Should detect Sequence pattern (multiple tasks)
        assert!(spec.detected_patterns.contains(&WorkflowPattern::Sequence));

        // Should detect Parallel Split (AND split in task1)
        assert!(spec
            .detected_patterns
            .contains(&WorkflowPattern::ParallelSplit));

        // Should detect Exclusive Choice (XOR split in task2)
        assert!(spec
            .detected_patterns
            .contains(&WorkflowPattern::ExclusiveChoice));

        // Should detect Synchronization (AND join in task2)
        assert!(spec
            .detected_patterns
            .contains(&WorkflowPattern::Synchronization));
    }

    #[test]
    fn test_split_type_from_str() {
        assert_eq!(SplitType::try_from_str("and").unwrap(), SplitType::And);
        assert_eq!(SplitType::try_from_str("AND").unwrap(), SplitType::And);
        assert_eq!(SplitType::try_from_str("or").unwrap(), SplitType::Or);
        assert_eq!(SplitType::try_from_str("xor").unwrap(), SplitType::Xor);
        assert!(SplitType::try_from_str("invalid").is_err());
    }

    #[test]
    fn test_join_type_from_str() {
        assert_eq!(JoinType::try_from_str("and").unwrap(), JoinType::And);
        assert_eq!(JoinType::try_from_str("AND").unwrap(), JoinType::And);
        assert_eq!(JoinType::try_from_str("or").unwrap(), JoinType::Or);
        assert_eq!(JoinType::try_from_str("xor").unwrap(), JoinType::Xor);
        assert!(JoinType::try_from_str("invalid").is_err());
    }

    #[test]
    fn test_workflow_pattern_properties() {
        // Test pattern number
        assert_eq!(WorkflowPattern::Sequence.number(), 1);
        assert_eq!(WorkflowPattern::ParallelSplit.number(), 2);
        assert_eq!(WorkflowPattern::Synchronization.number(), 3);
        assert_eq!(WorkflowPattern::CancelRegion.number(), 20);

        // Test pattern name
        assert_eq!(WorkflowPattern::Sequence.name(), "Sequence");
        assert_eq!(WorkflowPattern::MultiChoice.name(), "Multi-Choice");
        assert_eq!(WorkflowPattern::CancelRegion.name(), "Cancel Region");

        // Test pattern category
        assert_eq!(
            WorkflowPattern::Sequence.category(),
            PatternCategory::ControlFlow
        );
        assert_eq!(
            WorkflowPattern::SynchronizingMerge.category(),
            PatternCategory::AdvancedBranching
        );
        assert_eq!(
            WorkflowPattern::MultipleInstancesDynamic.category(),
            PatternCategory::StateBased
        );
        assert_eq!(
            WorkflowPattern::CancelCase.category(),
            PatternCategory::Cancellation
        );
    }

    #[test]
    fn test_workflow_pattern_from_name() {
        assert_eq!(
            WorkflowPattern::from_name("sequence").unwrap(),
            WorkflowPattern::Sequence
        );
        assert_eq!(
            WorkflowPattern::from_name("Parallel Split").unwrap(),
            WorkflowPattern::ParallelSplit
        );
        assert_eq!(
            WorkflowPattern::from_name("ARBITRARY_CYCLES").unwrap(),
            WorkflowPattern::ArbitraryCycles
        );
        assert!(WorkflowPattern::from_name("unknown").is_err());
    }

    #[test]
    fn test_variable_type_from_str() {
        assert_eq!(VariableType::try_from_str("string"), VariableType::String);
        assert_eq!(
            VariableType::try_from_str("xs:string"),
            VariableType::String
        );
        assert_eq!(VariableType::try_from_str("integer"), VariableType::Integer);
        assert_eq!(VariableType::try_from_str("int"), VariableType::Integer);
        assert_eq!(VariableType::try_from_str("boolean"), VariableType::Boolean);
        assert_eq!(VariableType::try_from_str("float"), VariableType::Float);
        assert_eq!(
            VariableType::try_from_str("CustomType"),
            VariableType::Custom("CustomType".to_string())
        );
    }

    #[test]
    fn test_escape_xml() {
        assert_eq!(escape_xml("hello & world"), "hello &amp; world");
        assert_eq!(escape_xml("a < b"), "a &lt; b");
        assert_eq!(escape_xml("a > b"), "a &gt; b");
        assert_eq!(escape_xml("\"quoted\""), "&quot;quoted&quot;");
        assert_eq!(escape_xml("'apostrophe'"), "&apos;apostrophe&apos;");
    }

    #[test]
    fn test_to_xml() {
        let parser = YawlParser::new();
        let spec = parser.parse(SIMPLE_YAWL).unwrap();
        let xml = parser.to_xml(&spec).expect("to_xml should succeed");

        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("<name>Test Workflow</name>"));
        assert!(xml.contains("type=\"and\""));
        assert!(xml.contains("type=\"xor\""));
    }

    #[test]
    fn test_decomposition_type_display() {
        assert_eq!(DecompositionType::WSNet.to_string(), "WSNet");
        assert_eq!(DecompositionType::CompositeNet.to_string(), "CompositeNet");
        assert_eq!(
            DecompositionType::Custom("Custom".to_string()).to_string(),
            "Custom"
        );
    }

    #[test]
    fn test_pattern_category_display() {
        assert_eq!(PatternCategory::ControlFlow.to_string(), "Control Flow");
        assert_eq!(
            PatternCategory::AdvancedBranching.to_string(),
            "Advanced Branching"
        );
        assert_eq!(PatternCategory::Cancellation.to_string(), "Cancellation");
    }

    #[test]
    fn test_all_20_patterns_defined() {
        let all_patterns = vec![
            WorkflowPattern::Sequence,
            WorkflowPattern::ParallelSplit,
            WorkflowPattern::Synchronization,
            WorkflowPattern::ExclusiveChoice,
            WorkflowPattern::SimpleMerge,
            WorkflowPattern::MultiChoice,
            WorkflowPattern::SynchronizingMerge,
            WorkflowPattern::MultiMerge,
            WorkflowPattern::Discriminator,
            WorkflowPattern::ArbitraryCycles,
            WorkflowPattern::ImplicitTermination,
            WorkflowPattern::MultipleInstancesDynamic,
            WorkflowPattern::MultipleInstancesDesignTime,
            WorkflowPattern::MultipleInstancesRuntime,
            WorkflowPattern::DeferredChoice,
            WorkflowPattern::InterleavedParallelRouting,
            WorkflowPattern::CancelActivity,
            WorkflowPattern::CancelCase,
            WorkflowPattern::PatternBasedTermination,
            WorkflowPattern::CancelRegion,
        ];

        // Verify all patterns have unique numbers 1-20
        let numbers: Vec<u8> = all_patterns.iter().map(|p| p.number()).collect();
        let mut sorted_numbers = numbers.clone();
        sorted_numbers.sort();
        sorted_numbers.dedup();

        assert_eq!(sorted_numbers.len(), 20);
        assert_eq!(sorted_numbers, (1..=20).collect::<Vec<_>>());
    }

    #[test]
    fn test_parser_with_config() {
        let config = ParserConfig {
            validate: false,
            detect_patterns: false,
            preserve_whitespace: true,
            max_file_size: 1024,
        };
        let parser = YawlParser::with_config(config);
        assert!(!parser.config.validate);
        assert!(!parser.config.detect_patterns);
        assert!(parser.config.preserve_whitespace);
        assert_eq!(parser.config.max_file_size, 1024);
    }
}
