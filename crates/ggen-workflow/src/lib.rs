//! ggen-workflow - Erlang NIF bindings for ggen workflow patterns and ontology processing
//!
//! This crate provides Erlang NIF bindings for executing deterministic workflows
//! based on RDF ontology specifications. It implements a five-stage transformation
//! pipeline with pattern orchestration and cryptographic receipt generation.
//!
//! # Architecture
//!
//! The workflow system is built on the equation A = μ(O) where code precipitates
//! from RDF ontology through five transformation stages:
//! - μ₁ (Normalize): RDF validation and SHAPE constraint resolution
//! - μ₂ (Extract): SPARQL queries and OWL inference execution
//! - μ₃ (Emit): Tera template rendering and code generation
//! - μ₄ (Canonicalize): Deterministic formatting and content hashing
//! - μ₅ (Receipt): Cryptographic proof generation and audit trail
//!
//! # Patterns
//!
//! The system supports four workflow patterns:
//! - **Sequence**: Linear step execution with sequential dependencies
//! - **Parallel**: Concurrent execution with synchronization
//! - **Choice**: Conditional branching based on evaluation results
//! - **Sync**: Barrier synchronization across multiple concurrent streams
//!
//! # Quality Enforcement
//!
//! All operations follow Poka-Yoke principles with comprehensive error handling,
//! Chicago TDD mandatory testing, and deterministic receipts for reproducibility.

// Module declarations
pub mod error;
pub mod nif;
pub mod patterns;
pub mod receipts;

// Re-export commonly used types
pub use error::{WorkflowError, WorkflowResult};
pub use patterns::{
    Choice, ExecutionMetadata, Parallel, Sequence, Sync, SyncConfig, TraceEvent, WorkflowContext,
    WorkflowPattern,
};
pub use receipts::{ReceiptGenerator, ReceiptMetadata, ReceiptStore, WorkflowReceipt};

// Use constants directly from this module

/// Configuration constants for workflow execution
pub struct Constants {
    /// Maximum number of steps in a workflow
    pub max_workflow_steps: usize,
    /// Maximum number of parallel tasks
    pub max_parallel_tasks: usize,
    /// Maximum SPARQL query size in bytes
    pub max_sparql_query_size: usize,
    /// Default timeout for operations in milliseconds
    pub default_timeout_ms: u64,
    /// Length of receipt hash in bytes
    pub receipt_hash_length: usize,
}

impl Default for Constants {
    fn default() -> Self {
        Constants {
            max_workflow_steps: 1000,
            max_parallel_tasks: 100,
            max_sparql_query_size: 1024 * 1024, // 1MB
            default_timeout_ms: 30000,
            receipt_hash_length: 32,
        }
    }
}

/// Global constants instance
pub const CONSTANTS: Constants = Constants {
    max_workflow_steps: 1000,
    max_parallel_tasks: 100,
    max_sparql_query_size: 1024 * 1024,
    default_timeout_ms: 30000,
    receipt_hash_length: 32,
};

/// System information
pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

pub fn description() -> &'static str {
    env!("CARGO_PKG_DESCRIPTION")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!version().is_empty());
    }

    #[test]
    fn test_constants() {
        let constants = Constants::default();
        assert_eq!(constants.max_workflow_steps, 1000);
        assert_eq!(constants.max_parallel_tasks, 100);
        assert_eq!(constants.max_sparql_query_size, 1024 * 1024);
        assert_eq!(constants.default_timeout_ms, 30000);
        assert_eq!(constants.receipt_hash_length, 32);
    }

    #[test]
    fn test_constants_global() {
        assert_eq!(CONSTANTS.max_workflow_steps, 1000);
        assert_eq!(CONSTANTS.max_parallel_tasks, 100);
        assert_eq!(CONSTANTS.max_sparql_query_size, 1024 * 1024);
        assert_eq!(CONSTANTS.default_timeout_ms, 30000);
        assert_eq!(CONSTANTS.receipt_hash_length, 32);
    }
}
