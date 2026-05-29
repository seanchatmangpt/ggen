//! RDF metadata management for templates using Oxigraph
//!
//! This module provides comprehensive RDF support for template metadata, including
//! schema definitions, SHACL validation, SPARQL querying, and template relationship
//! management. It enables semantic understanding of templates and their dependencies.
//!
//! ## Features
//!
//! - **Ggen Ontology**: Type-safe ontology builder with namespace constants
//! - **Template Metadata**: Extract, store, and query template metadata from RDF
//! - **SHACL Validation**: Validate template structure and relationships
//! - **Template Relationships**: Track dependencies, variants, and compositions
//! - **Variable Extraction**: Extract template variables from RDF annotations
//!
//! ## Architecture
//!
//! The module uses Oxigraph as the RDF store and provides:
//! - Schema definitions (`schema.rs`) - Ggen ontology and namespace constants
//! - Metadata management (`template_metadata.rs`) - Store and query template metadata
//! - Validation (`validation.rs`) - SHACL-based validation rules
//!
//! ## Examples
//!
//! ### Using the Ggen Ontology
//!
//! ```rust
//! use crate::rdf::schema::GgenOntology;
//!
//! let template_uri = GgenOntology::template();
//! assert!(template_uri.contains("Template"));
//! ```
//!
//! ### Storing Template Metadata
//!
//! ```rust,no_run
//! use crate::rdf::template_metadata::{TemplateMetadataStore, TemplateMetadata};
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let store = TemplateMetadataStore::new()?;
//!
//! let metadata = TemplateMetadata::new(
//!     "template://example/rust-cli".to_string(),
//!     "Rust CLI Template".to_string(),
//! );
//! store.store_metadata(&metadata)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Validating Template Metadata
//!
//! ```rust,no_run
//! use crate::rdf::validation::Validator;
//! use crate::rdf::template_metadata::TemplateMetadata;
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let validator = Validator::new();
//! let metadata = TemplateMetadata::new(
//!     "template://example/rust-cli".to_string(),
//!     "Rust CLI Template".to_string(),
//! );
//!
//! let result = validator.validate(&metadata)?;
//! if result.is_valid() {
//!     println!("Template metadata is valid");
//! }
//! # Ok(())
//! # }
//! ```

pub mod query;
pub mod query_builder;
pub mod schema;
pub mod template_metadata;
mod template_metadata_helper;
pub mod validation;

pub use query::{CacheStats, QueryCache};
pub use query_builder::{Iri, Literal, SparqlQueryBuilder, Variable};
pub use schema::{GgenOntology, GGEN_NAMESPACE};
pub use template_metadata::{
    TemplateMetadata, TemplateMetadataStore, TemplateRelationship, TemplateVariable,
};
pub use validation::{ValidationReport, ValidationResult, Validator};
