//! RDF Ontology Data Structures
//!
//! This module provides core data structures for RDF ontologies without external dependencies.
//! Ontology loading via Oxigraph is handled in the playground project.
//!
//! ## Structures
//!
//! The RDF module provides:
//! - **`SectorOntology`**: Container for workflow definitions
//! - **`WorkflowStage`**: Individual workflow stages
//! - **`GuardConstraint`**: Safety constraints (Budget, Chronology, etc.)
//! - **`KnowledgeHook`**: Operations within workflows
//! - **`RdfOperationValidator`**: Runtime validation against ontology

pub mod ontology;
pub mod validation;

pub use ontology::{GuardConstraint, KnowledgeHook, SectorOntology, WorkflowStage};
pub use validation::{RdfOperationValidator, RdfValidationError, RdfValidationResult};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ontology_structures_available() {
        // Verify core ontology structures are available
        let ontology = SectorOntology::new("Test".to_string());
        assert_eq!(ontology.sector, "Test");
    }
}
