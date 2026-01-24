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
//! use ggen_core::rdf::schema::{GgenOntology, GGEN_NAMESPACE};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let ontology = GgenOntology::new();
//! let template_uri = ontology.template("my-template");
//! assert!(template_uri.contains("my-template"));
//! # Ok(())
//! # }
//! ```
//!
//! ### Storing Template Metadata
//!
//! ```rust,no_run
//! use ggen_core::rdf::template_metadata::TemplateMetadataStore;
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let graph = Graph::new()?;
//! let store = TemplateMetadataStore::new(graph);
//!
//! let metadata = ggen_core::rdf::template_metadata::TemplateMetadata::default();
//! store.store("template.tmpl", &metadata)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Validating Template Metadata
//!
//! ```rust,no_run
//! use ggen_core::rdf::validation::Validator;
//! use ggen_core::rdf::template_metadata::TemplateMetadata;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let validator = Validator::new();
//! let metadata = TemplateMetadata::default();
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
