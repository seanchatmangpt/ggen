//! ggen.toml manifest parsing and validation module
//!
//! This module provides strongly-typed parsing of `ggen.toml` manifests for
//! semantic code generation pipelines. The manifest defines:
//!
//! - **Ontology sources**: RDF files containing domain models
//! - **Inference rules**: CONSTRUCT queries for semantic enrichment
//! - **Generation rules**: SPARQL queries + Tera templates → code files
//! - **Validation**: SHACL shapes and custom constraints
//!
//! ## Example ggen.toml
//!
//! ```toml
//! [project]
//! name = "my-domain"
//! version = "1.0.0"
//!
//! [ontology]
//! source = "domain/model.ttl"
//! base_iri = "http://example.org/"
//!
//! [[inference.rules]]
//! name = "auditable_fields"
//! construct = "CONSTRUCT { ... } WHERE { ... }"
//! order = 1
//!
//! [[generation.rules]]
//! query = { file = "queries/structs.sparql" }
//! template = { file = "templates/struct.tera" }
//! output_file = "src/generated/{{name}}.rs"
//! ```
//!
//! ## Strategic Decision
//!
//! Named CONSTRUCT queries are the canonical rule language for inference.
//! This module does NOT implement full N3 rule execution - CONSTRUCT provides
//! sufficient expressiveness with deterministic execution order.

pub mod parser;
pub mod types;
pub mod validation;

pub use parser::ManifestParser;
pub use types::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig, InferenceRule,
    OntologyConfig, ProjectConfig, QuerySource, TemplateSource, ValidationConfig, ValidationRule,
    ValidationSeverity,
};
pub use validation::ManifestValidator;
