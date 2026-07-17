//! > 📚 Reference
//!
//! Operator Registry - Chatman Equation YAWL Patterns
//!
//! Registry of all 43 YAWL workflow control patterns, each characterized by
//! the four properties of the Chatman Equation:
//! - Determinism: f(x) = f(x) always
//! - Idempotence: f(f(x)) = f(x)
//! - Type Preservation: Types maintained through execution
//! - Boundedness: Execution time is measurable and bounded
//!
//! This module can be auto-generated from the RDF ontology (ontology/chatman-equation.ttl)
//! using ggen templates.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// > 📚 Reference
///
/// Operator Descriptor - complete specification of a workflow operator.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::operator_registry::{OperatorDescriptor, OperatorProperties, GuardType};
///
/// let descriptor = OperatorDescriptor::new(
///     "sequence_op",
///     1,
///     "Sequence",
///     "Basic Control Flow",
///     OperatorProperties {
///         deterministic: true,
///         idempotent: false,
///         type_preserving: true,
///         bounded: true,
///     },
///     1_000_000_000,
///     vec![GuardType::Chronology],
/// );
///
/// assert_eq!(descriptor.hook_id, "sequence_op");
/// assert_eq!(descriptor.satisfies_all_properties(), false); // idempotent is false
/// ```
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperatorDescriptor {
    /// Unique hook ID (used in tests)
    pub hook_id: String,

    /// YAWL pattern number (1-43)
    pub pattern_number: u32,

    /// Pattern name
    pub pattern_name: String,

    /// Pattern category
    pub pattern_category: String,

    /// Properties of the Chatman Equation
    pub properties: OperatorProperties,

    /// Maximum allowed execution latency (nanoseconds)
    pub max_latency_ns: i64,

    /// Required guards for safe execution
    pub required_guards: Vec<GuardType>,

    /// Service level objective (if any)
    pub slo: Option<String>,
}

/// > 📚 Reference
///
/// The four properties of the Chatman Equation.
///
/// Characterizes workflow operator behavior under:
/// - Determinism
/// - Idempotence
/// - Type Preservation
/// - Boundedness
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::operator_registry::OperatorProperties;
///
/// let properties = OperatorProperties {
///     deterministic: true,
///     idempotent: true,
///     type_preserving: true,
///     bounded: true,
/// };
///
/// assert!(properties.deterministic);
/// ```
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[allow(clippy::struct_excessive_bools)] // Four bools represent the four Chatman properties (intentional design)
pub struct OperatorProperties {
    /// Identical inputs always produce identical outputs
    pub deterministic: bool,

    /// f(f(x)) = f(x) - running twice equals running once
    pub idempotent: bool,

    /// Input types are preserved through execution
    pub type_preserving: bool,

    /// Execution time is measurable and bounded
    pub bounded: bool,
}

/// > 📚 Reference
///
/// Types of guards that can be applied to operators.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::operator_registry::GuardType;
///
/// let guard = GuardType::Legality;
/// assert_eq!(format!("{}", guard), "Legality");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GuardType {
    /// Prevents invalid state transitions
    Legality,

    /// Prevents exceeding resource limits
    Budget,

    /// Enforces proper temporal ordering
    Chronology,

    /// Ensures causal dependencies respected
    Causality,

    /// Bounds recursion depth (Chatman Constant = 8)
    Recursion,
}

impl std::fmt::Display for GuardType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Legality => write!(f, "Legality"),
            Self::Budget => write!(f, "Budget"),
            Self::Chronology => write!(f, "Chronology"),
            Self::Causality => write!(f, "Causality"),
            Self::Recursion => write!(f, "Recursion"),
        }
    }
}

impl OperatorDescriptor {
    /// Create a new operator descriptor
    #[must_use]
    pub fn new(
        hook_id: &str, pattern_number: u32, pattern_name: &str, pattern_category: &str,
        properties: OperatorProperties, max_latency_ns: i64, required_guards: Vec<GuardType>,
    ) -> Self {
        Self {
            hook_id: hook_id.to_string(),
            pattern_number,
            pattern_name: pattern_name.to_string(),
            pattern_category: pattern_category.to_string(),
            properties,
            max_latency_ns,
            required_guards,
            slo: None,
        }
    }

    /// Check if operator satisfies all Chatman properties
    #[must_use]
    pub const fn satisfies_all_properties(&self) -> bool {
        self.properties.deterministic
            && self.properties.idempotent
            && self.properties.type_preserving
            && self.properties.bounded
    }

    /// Check if operator is bounded
    #[must_use]
    pub const fn is_bounded(&self) -> bool {
        self.properties.bounded && self.max_latency_ns > 0
    }

    /// Get maximum latency in milliseconds (for display)
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for display purposes
    pub fn max_latency_ms(&self) -> f64 {
        self.max_latency_ns as f64 / 1_000_000.0
    }

    /// Get maximum latency in seconds (for display)
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for display purposes
    pub fn max_latency_s(&self) -> f64 {
        self.max_latency_ns as f64 / 1_000_000_000.0
    }

    /// Check if operator requires a specific guard
    #[must_use]
    pub fn requires_guard(&self, guard: GuardType) -> bool {
        self.required_guards.contains(&guard)
    }

    /// Add a service level objective
    #[must_use]
    pub fn with_slo(mut self, slo: &str) -> Self {
        self.slo = Some(slo.to_string());
        self
    }
}

/// > 📚 Reference
///
/// Global operator registry containing all YAWL patterns.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::operator_registry::OperatorRegistry;
///
/// let registry = OperatorRegistry::new();
/// let op = registry.get_operator("sequence_op");
/// assert!(op.is_some());
/// ```
pub struct OperatorRegistry {
    operators: HashMap<String, OperatorDescriptor>,
}

impl OperatorRegistry {
    /// Create a new operator registry with all 43 YAWL patterns
    #[must_use]
    #[allow(clippy::too_many_lines)] // Registry initialization requires many pattern definitions
    pub fn new() -> Self {
        let mut operators = HashMap::new();

        // Basic Control Flow Patterns (1-6)

        operators.insert(
            "sequence_op".to_string(),
            OperatorDescriptor::new(
                "sequence_op",
                1,
                "Sequence",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Chronology],
            ),
        );

        operators.insert(
            "parallel_split_op".to_string(),
            OperatorDescriptor::new(
                "parallel_split_op",
                2,
                "Parallel Split",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Causality],
            ),
        );

        operators.insert(
            "synchronization_op".to_string(),
            OperatorDescriptor::new(
                "synchronization_op",
                3,
                "Synchronization",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Causality],
            ),
        );

        operators.insert(
            "exclusive_choice_op".to_string(),
            OperatorDescriptor::new(
                "exclusive_choice_op",
                4,
                "Exclusive Choice",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        operators.insert(
            "simple_merge_op".to_string(),
            OperatorDescriptor::new(
                "simple_merge_op",
                5,
                "Simple Merge",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        operators.insert(
            "multiple_choice_op".to_string(),
            OperatorDescriptor::new(
                "multiple_choice_op",
                6,
                "Multiple Choice",
                "Basic Control Flow",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                2_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        // Advanced Branching and Synchronization (7-15)

        operators.insert(
            "sync_merge_op".to_string(),
            OperatorDescriptor::new(
                "sync_merge_op",
                7,
                "Structured Synchronizing Merge",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Causality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "deferred_choice_op".to_string(),
            OperatorDescriptor::new(
                "deferred_choice_op",
                15,
                "Deferred Choice",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        // Structural Patterns (16-27)

        operators.insert(
            "cycles_op".to_string(),
            OperatorDescriptor::new(
                "cycles_op",
                20,
                "Arbitrary Cycles",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                30_000_000_000,
                vec![GuardType::Recursion, GuardType::Budget],
            ),
        );

        operators.insert(
            "incl_or_join_op".to_string(),
            OperatorDescriptor::new(
                "incl_or_join_op",
                25,
                "Inclusive Or with Multiple Instance Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Causality],
            ),
        );

        // Multiple Instance Patterns (28-32)

        operators.insert(
            "multi_parallel_op".to_string(),
            OperatorDescriptor::new(
                "multi_parallel_op",
                30,
                "Multiple Instance Parallel",
                "Multiple Instance",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                50_000_000_000,
                vec![GuardType::Budget, GuardType::Recursion],
            ),
        );

        // State-Based Patterns (33-38)

        operators.insert(
            "state_concurrency_op".to_string(),
            OperatorDescriptor::new(
                "state_concurrency_op",
                35,
                "State-Based Concurrency",
                "State-Based",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: false,
                },
                0, // Unbounded
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        // Cancellation and Force Completion (39-43)

        operators.insert(
            "cancel_region_op".to_string(),
            OperatorDescriptor::new(
                "cancel_region_op",
                40,
                "Cancellation Region",
                "Cancellation",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        operators.insert(
            "force_complete_op".to_string(),
            OperatorDescriptor::new(
                "force_complete_op",
                43,
                "Force Completion",
                "Cancellation",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        // Advanced Branching and Synchronization continued (8-14)

        operators.insert(
            "multi_merge_op".to_string(),
            OperatorDescriptor::new(
                "multi_merge_op",
                8,
                "Multi-Merge",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Causality],
            ),
        );

        operators.insert(
            "structured_disc_op".to_string(),
            OperatorDescriptor::new(
                "structured_disc_op",
                9,
                "Structured Discriminator",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "blocking_disc_op".to_string(),
            OperatorDescriptor::new(
                "blocking_disc_op",
                10,
                "Blocking Discriminator",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        operators.insert(
            "cancelling_disc_op".to_string(),
            OperatorDescriptor::new(
                "cancelling_disc_op",
                11,
                "Cancelling Discriminator",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget, GuardType::Causality],
            ),
        );

        operators.insert(
            "local_sync_merge_op".to_string(),
            OperatorDescriptor::new(
                "local_sync_merge_op",
                12,
                "Local Synchronizing Merge",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Causality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "general_sync_merge_op".to_string(),
            OperatorDescriptor::new(
                "general_sync_merge_op",
                13,
                "General Synchronizing Merge",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![
                    GuardType::Causality,
                    GuardType::Chronology,
                    GuardType::Budget,
                ],
            ),
        );

        operators.insert(
            "thread_merge_op".to_string(),
            OperatorDescriptor::new(
                "thread_merge_op",
                14,
                "Thread Merge",
                "Advanced Branching",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Causality],
            ),
        );

        // Structural Patterns continued (16-19, 21-24, 26-27)

        operators.insert(
            "implicit_termination_op".to_string(),
            OperatorDescriptor::new(
                "implicit_termination_op",
                16,
                "Implicit Termination",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        operators.insert(
            "explicit_termination_op".to_string(),
            OperatorDescriptor::new(
                "explicit_termination_op",
                17,
                "Explicit Termination",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        operators.insert(
            "transient_trigger_op".to_string(),
            OperatorDescriptor::new(
                "transient_trigger_op",
                18,
                "Transient Trigger",
                "Structural",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                2_000_000_000,
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "persistent_trigger_op".to_string(),
            OperatorDescriptor::new(
                "persistent_trigger_op",
                19,
                "Persistent Trigger",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                2_000_000_000,
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "structured_loop_op".to_string(),
            OperatorDescriptor::new(
                "structured_loop_op",
                21,
                "Structured Loop",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                30_000_000_000,
                vec![GuardType::Recursion, GuardType::Budget],
            ),
        );

        operators.insert(
            "recursion_op".to_string(),
            OperatorDescriptor::new(
                "recursion_op",
                22,
                "Recursion",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                30_000_000_000,
                vec![GuardType::Recursion, GuardType::Budget],
            ),
        );

        operators.insert(
            "transient_partial_join_op".to_string(),
            OperatorDescriptor::new(
                "transient_partial_join_op",
                23,
                "Transient Partial Join",
                "Structural",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Causality],
            ),
        );

        operators.insert(
            "persistent_partial_join_op".to_string(),
            OperatorDescriptor::new(
                "persistent_partial_join_op",
                24,
                "Persistent Partial Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Causality],
            ),
        );

        operators.insert(
            "persistent_partial_and_join_op".to_string(),
            OperatorDescriptor::new(
                "persistent_partial_and_join_op",
                26,
                "Persistent Partial AND-Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![
                    GuardType::Legality,
                    GuardType::Causality,
                    GuardType::Chronology,
                ],
            ),
        );

        operators.insert(
            "generalized_and_join_op".to_string(),
            OperatorDescriptor::new(
                "generalized_and_join_op",
                27,
                "Generalized AND-Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Causality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "blocking_partial_join_op".to_string(),
            OperatorDescriptor::new(
                "blocking_partial_join_op",
                28,
                "Blocking Partial Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget, GuardType::Causality],
            ),
        );

        operators.insert(
            "cancelling_partial_join_op".to_string(),
            OperatorDescriptor::new(
                "cancelling_partial_join_op",
                29,
                "Cancelling Partial Join",
                "Structural",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget, GuardType::Causality],
            ),
        );

        // Multiple Instance Patterns continued (31-32)

        operators.insert(
            "mi_without_sync_op".to_string(),
            OperatorDescriptor::new(
                "mi_without_sync_op",
                31,
                "Multiple Instances Without Synchronization",
                "Multiple Instance",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                50_000_000_000,
                vec![GuardType::Budget, GuardType::Recursion],
            ),
        );

        operators.insert(
            "mi_with_priori_design_op".to_string(),
            OperatorDescriptor::new(
                "mi_with_priori_design_op",
                32,
                "Multiple Instances With A Priori Design Time Knowledge",
                "Multiple Instance",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                30_000_000_000,
                vec![GuardType::Budget, GuardType::Recursion],
            ),
        );

        // State-Based Patterns continued (33-34, 36-38)

        operators.insert(
            "deferred_choice_state_op".to_string(),
            OperatorDescriptor::new(
                "deferred_choice_state_op",
                33,
                "Deferred Choice (State-Based)",
                "State-Based",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        operators.insert(
            "interleaved_parallel_routing_op".to_string(),
            OperatorDescriptor::new(
                "interleaved_parallel_routing_op",
                34,
                "Interleaved Parallel Routing",
                "State-Based",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        operators.insert(
            "milestone_op".to_string(),
            OperatorDescriptor::new(
                "milestone_op",
                36,
                "Milestone",
                "State-Based",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Chronology, GuardType::Legality],
            ),
        );

        operators.insert(
            "critical_section_op".to_string(),
            OperatorDescriptor::new(
                "critical_section_op",
                37,
                "Critical Section",
                "State-Based",
                OperatorProperties {
                    deterministic: true,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        operators.insert(
            "interleaved_routing_op".to_string(),
            OperatorDescriptor::new(
                "interleaved_routing_op",
                38,
                "Interleaved Routing",
                "State-Based",
                OperatorProperties {
                    deterministic: false,
                    idempotent: false,
                    type_preserving: true,
                    bounded: true,
                },
                10_000_000_000,
                vec![GuardType::Legality, GuardType::Chronology],
            ),
        );

        // Cancellation and Force Completion continued (39, 41-42)

        operators.insert(
            "cancel_activity_op".to_string(),
            OperatorDescriptor::new(
                "cancel_activity_op",
                39,
                "Cancel Activity",
                "Cancellation",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                1_000_000_000,
                vec![GuardType::Legality],
            ),
        );

        operators.insert(
            "cancel_case_op".to_string(),
            OperatorDescriptor::new(
                "cancel_case_op",
                41,
                "Cancel Case",
                "Cancellation",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                2_000_000_000,
                vec![GuardType::Legality, GuardType::Budget],
            ),
        );

        operators.insert(
            "cancel_mi_op".to_string(),
            OperatorDescriptor::new(
                "cancel_mi_op",
                42,
                "Cancel Multiple Instance Activity",
                "Cancellation",
                OperatorProperties {
                    deterministic: true,
                    idempotent: true,
                    type_preserving: true,
                    bounded: true,
                },
                5_000_000_000,
                vec![GuardType::Legality, GuardType::Budget, GuardType::Recursion],
            ),
        );

        Self { operators }
    }

    /// Look up an operator by hook ID
    #[must_use]
    pub fn get_operator(&self, hook_id: &str) -> Option<&OperatorDescriptor> {
        self.operators.get(hook_id)
    }

    /// Get all operators
    #[must_use]
    pub fn all_operators(&self) -> Vec<&OperatorDescriptor> {
        let mut ops: Vec<_> = self.operators.values().collect();
        ops.sort_by_key(|op| op.pattern_number);
        ops
    }

    /// Count operators by category
    #[must_use]
    pub fn count_by_category(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for op in self.operators.values() {
            *counts.entry(op.pattern_category.clone()).or_insert(0) += 1;
        }
        counts
    }

    /// Count operators with each property
    #[must_use]
    pub fn count_deterministic(&self) -> usize {
        self.operators
            .values()
            .filter(|op| op.properties.deterministic)
            .count()
    }

    /// Count operators with idempotence property
    #[must_use]
    pub fn count_idempotent(&self) -> usize {
        self.operators
            .values()
            .filter(|op| op.properties.idempotent)
            .count()
    }

    /// Count operators with type preservation property
    #[must_use]
    pub fn count_type_preserving(&self) -> usize {
        self.operators
            .values()
            .filter(|op| op.properties.type_preserving)
            .count()
    }

    /// Count operators with boundedness property
    #[must_use]
    pub fn count_bounded(&self) -> usize {
        self.operators
            .values()
            .filter(|op| op.properties.bounded)
            .count()
    }

    /// Get operators requiring a specific guard
    #[must_use]
    pub fn operators_with_guard(&self, guard: GuardType) -> Vec<&OperatorDescriptor> {
        self.operators
            .values()
            .filter(|op| op.required_guards.contains(&guard))
            .collect()
    }

    /// Get operators satisfying all four properties
    #[must_use]
    pub fn operators_fully_deterministic(&self) -> Vec<&OperatorDescriptor> {
        self.operators
            .values()
            .filter(|op| op.satisfies_all_properties())
            .collect()
    }
}

impl Default for OperatorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// Global registry instance
use std::sync::OnceLock;

static REGISTRY: OnceLock<OperatorRegistry> = OnceLock::new();

/// > 📚 Reference
///
/// Get the global operator registry singleton.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::operator_registry::global_registry;
///
/// let registry = global_registry();
/// let op = registry.get_operator("sequence_op");
/// assert!(op.is_some());
/// ```
pub fn global_registry() -> &'static OperatorRegistry {
    REGISTRY.get_or_init(OperatorRegistry::new)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_initialization() {
        let registry = OperatorRegistry::new();
        assert!(registry.all_operators().len() > 0);
    }

    #[test]
    fn test_operator_lookup() {
        let registry = OperatorRegistry::new();

        let op = registry.get_operator("sequence_op");
        assert!(op.is_some());

        let op = op.unwrap();
        assert_eq!(op.pattern_number, 1);
        assert_eq!(op.pattern_name, "Sequence");
        assert!(op.properties.deterministic);
        assert!(op.properties.type_preserving);
    }

    #[test]
    fn test_operator_properties() {
        let registry = OperatorRegistry::new();

        let sequence = registry.get_operator("sequence_op").unwrap();
        assert!(sequence.is_bounded());
        assert_eq!(sequence.max_latency_ms(), 1000.0);

        let parallel = registry.get_operator("parallel_split_op").unwrap();
        assert!(parallel.is_bounded());
        assert_eq!(parallel.max_latency_ms(), 5000.0);
    }

    #[test]
    fn test_count_by_category() {
        let registry = OperatorRegistry::new();
        let counts = registry.count_by_category();

        assert!(counts.contains_key("Basic Control Flow"));
        assert!(counts.contains_key("Advanced Branching"));
        assert!(counts.contains_key("Cancellation"));
    }

    #[test]
    fn test_guard_filtering() {
        let registry = OperatorRegistry::new();

        let legality = registry.operators_with_guard(GuardType::Legality);
        assert!(legality.len() > 0);

        let recursion = registry.operators_with_guard(GuardType::Recursion);
        assert!(recursion.len() > 0);
    }

    #[test]
    fn test_property_counters() {
        let registry = OperatorRegistry::new();

        let deterministic = registry.count_deterministic();
        let _idempotent = registry.count_idempotent();
        let type_preserving = registry.count_type_preserving();
        let bounded = registry.count_bounded();

        assert!(deterministic > 0);
        assert!(type_preserving > 0);
        assert!(bounded > 0);
    }

    #[test]
    fn test_global_registry() {
        let reg1 = global_registry();
        let reg2 = global_registry();

        // Should be same instance
        assert_eq!(reg1.all_operators().len(), reg2.all_operators().len());
    }
}
