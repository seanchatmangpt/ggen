//! Ontology Domain - RDF/OWL Ontology Operations
//!
//! This module provides domain logic for working with ontologies:
//! - Extracting schema from TTL/OWL files
//! - Generating code from ontology definitions
//! - Validating ontology structure
//! - Composing multiple ontologies
//! - Discovering ontology packs
//!
//! # Architecture
//!
//! The ontology system follows the pattern:
//!
//! ```text
//! User Request (CLI)
//!        ↓
//! Domain Layer (extract, generate, validate, compose, discover)
//!        ↓
//! Core Components (Graph, Template, SPARQL)
//!        ↓
//! Output (Code, Validation Report, Statistics)
//! ```
//!
//! Each function in this module:
//! - Takes an `Input` struct with required parameters
//! - Performs domain-specific business logic
//! - Returns an `Output` struct with results and statistics
//! - Is testable, reusable, and async-compatible

pub mod compose;
pub mod discover;
pub mod extract;
pub mod generate;
pub mod validate;

// Re-export core functions and types for easy access
pub use compose::{execute_compose, ComposeInput, ComposeOutput};
pub use discover::{execute_discover, DiscoverInput, DiscoverOutput};
pub use extract::{execute_extract, ExtractInput, ExtractOutput};
pub use generate::{execute_generate, GenerateInput, GenerateOutput};
pub use validate::{execute_validate, ValidateInput, ValidateOutput};

/// Common trait for ontology operations
pub trait OntologyOperation: Sized {
    /// Input type for this operation
    type Input;

    /// Output type for this operation
    type Output;

    /// Execute the operation (returns a future)
    fn execute(
        input: &Self::Input,
    ) -> std::pin::Pin<
        Box<dyn std::future::Future<Output = ggen_utils::error::Result<Self::Output>> + Send>,
    >;
}
