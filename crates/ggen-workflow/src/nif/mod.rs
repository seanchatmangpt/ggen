//! Erlang NIF initialization and module definition
//!
//! This module provides the NIF initialization code for the ggen-workflow crate.
//! It registers all NIF functions for workflow lifecycle, SPARQL queries, and
//! receipt generation.

// Re-export NIF modules
pub mod query;
pub mod receipt;
pub mod workflow;

// Re-export all NIF functions for the single init macro
pub use query::{execute_sparql, parse_sparql, validate_sparql};
pub use receipt::{
    create_test_receipt, generate_receipt, receipt_id, receipt_metadata, receipt_timestamp,
    verify_receipt,
};
pub use workflow::{
    execute_choice, execute_parallel, execute_sequence, execute_sync, workflow_context_add_input,
    workflow_context_create, workflow_context_metadata, workflow_context_output,
};

// Single NIF module definition for all workflow functions
rustler::init!(
    "Elixir.Workflow",
    [
        // Workflow context functions
        workflow_context_create,
        workflow_context_add_input,
        workflow_context_metadata,
        workflow_context_output,
        // Execution functions
        execute_sequence,
        execute_parallel,
        execute_choice,
        execute_sync,
        // SPARQL query functions
        execute_sparql,
        parse_sparql,
        validate_sparql,
        // Receipt functions
        generate_receipt,
        create_test_receipt,
        verify_receipt,
        receipt_metadata,
        receipt_id,
        receipt_timestamp,
    ]
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_context_default() {
        let context = crate::patterns::WorkflowContext::default();
        assert!(!context.metadata.workflow_id.is_empty());
        assert!(context.metadata.trace.is_empty());
    }
}
