//! RDF metadata management for templates (v2 domain layer)
//!
//! This module provides comprehensive RDF support for template metadata,
//! including schema definitions, SHACL validation, and SPARQL querying.
//!
//! Refactored from v1 (ggen-core/src/rdf) to v2 domain layer architecture:
//! - Uses ggen_utils::error::Result instead of anyhow::Result
//! - Pure domain logic (no CLI coupling)
//! - Async support preserved for domain layer operations
//! - All SPARQL queries and RDF parsing logic maintained

pub mod metadata;
pub mod schema;
pub mod validation;

pub use metadata::{
    TemplateMetadata, TemplateMetadataStore, TemplateRelationship, TemplateVariable,
};
pub use schema::{GgenOntology, GGEN_NAMESPACE};
pub use validation::{ValidationReport, ValidationResult, Validator};
