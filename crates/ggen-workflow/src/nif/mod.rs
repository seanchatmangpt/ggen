//! Erlang NIF initialization and module definition
//!
//! This module provides the NIF initialization code for the ggen-workflow crate.
//! It registers all NIF functions for workflow lifecycle, SPARQL queries, and
//! receipt generation.

// Re-export NIF modules
pub mod workflow;
pub mod query;
pub mod receipt;

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
