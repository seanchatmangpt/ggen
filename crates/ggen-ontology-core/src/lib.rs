//! # ggen-ontology-core - Ontology Handling Layer
//!
//! Core ontology processing module for ggen providing:
//! - RDF/Turtle (TTL) file loading and validation
//! - SPARQL query execution with deterministic results
//! - Entity mapping from domain models to standard ontology classes
//! - Comprehensive error handling with context
//!
//! ## Features
//! - **default**, **oxigraph**: Enables Oxigraph RDF store integration
//! - **sparql**: Enables SPARQL query support
//!
//! ## Architecture
//!
//! The ontology core is built with type-first design:
//! - All operations return `Result<T, OntologyError>` for explicit error handling
//! - Triple store operations are deterministic (same input → same output)
//! - Entity mapper provides confidence-scored ontology matches
//! - SPARQL generators produce consistent query strings
//!
//! ## Module Organization
//!
//! - `errors` - Error types with rich context
//! - `triple_store` - RDF/TTL loading and querying via Oxigraph
//! - `sparql_generator` - Deterministic SPARQL query building
//! - `entity_mapper` - Domain-to-ontology entity mapping
//! - `validators` - Syntax and semantic validation
//!
//! ## Examples
//!
//! ### Load and Query Ontology
//!
//! ```rust,no_run
//! use ggen_ontology_core::triple_store::TripleStore;
//! use ggen_ontology_core::sparql_generator::SparqlGenerator;
//!
//! // Create a new triple store
//! let store = TripleStore::new()?;
//!
//! // Load Turtle file
//! store.load_turtle("ontology.ttl")?;
//!
//! // Execute deterministic SPARQL query
//! let query = SparqlGenerator::find_policies_by_jurisdiction("US");
//! let results = store.query_sparql(&query)?;
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ### Map Entities to Ontology
//!
//! ```rust,no_run
//! use ggen_ontology_core::entity_mapper::EntityMapper;
//!
//! // Map policy to ontology class with confidence score
//! let matches = EntityMapper::match_policy("Privacy Policy")?;
//! for m in matches {
//!     println!("{}: {} (confidence: {})", m.label, m.class, m.score);
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ### Validate Ontology Files
//!
//! ```rust,no_run
//! use ggen_ontology_core::validators::validate_turtle;
//!
//! let report = validate_turtle("ontology.ttl")?;
//! if report.is_valid {
//!     println!("Ontology is valid");
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

#![deny(warnings)]
#![deny(missing_docs)]

pub mod entity_mapper;
pub mod errors;
pub mod sparql_generator;
pub mod triple_store;
pub mod validators;

// Public API re-exports
pub use entity_mapper::{EntityMapper, OntologyMatch, Score};
pub use errors::{OntologyError, Result};
pub use sparql_generator::SparqlGenerator;
pub use triple_store::{TripleStore, ValidationReport};
pub use validators::{validate_ontology, validate_rdf_xml, validate_sparql_query, validate_turtle};

/// Library version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_is_set() {
        assert!(!VERSION.is_empty());
        assert_eq!(VERSION, "0.2.0");
    }
}
