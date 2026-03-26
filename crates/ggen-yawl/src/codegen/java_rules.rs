//! Re-exports of generic framework types from ggen-codegen
//!
//! For backward compatibility, this module re-exports the generic code generation
//! framework types that are now defined in the standalone ggen-codegen crate.
//! All domain-specific implementations (JpaEntityQuery, RepositoryQuery, etc.)
//! remain in this crate.

// Re-export framework types from ggen-codegen
pub use ggen_codegen::{
    GeneratedFile, GenerationMode, Queryable, Renderable, Result, Rule,
};

/// Execution record for a rule
#[derive(Debug, Clone)]
pub struct ExecutedRuleRecord {
    /// Rule name
    pub name: String,
    /// Files generated
    pub files_count: usize,
    /// Execution duration (milliseconds)
    pub duration_ms: u128,
    /// Whether execution succeeded
    pub success: bool,
    /// Error message if failed
    pub error: Option<String>,
}

/// Orchestrator for multiple rules
///
/// Runs rules sequentially and tracks execution state.
/// Enables parallel rule execution with proper batching.
#[allow(dead_code)]
pub struct RuleSet {
    rules: Vec<(String, Box<dyn std::any::Any>)>,
}
