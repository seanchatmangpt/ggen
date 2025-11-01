//! RDF metadata management for templates using Oxigraph
//!
//! This module provides comprehensive RDF support for template metadata,
//! including schema definitions, SHACL validation, and SPARQL querying.

pub mod schema;
pub mod template_metadata;
mod template_metadata_helper;
pub mod validation;

pub use schema::{GgenOntology, GGEN_NAMESPACE};
pub use template_metadata::{
    TemplateMetadata, TemplateMetadataStore, TemplateRelationship, TemplateVariable,
};
pub use validation::{ValidationReport, ValidationResult, Validator};
