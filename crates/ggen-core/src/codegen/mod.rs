//! Code generation module
//!
//! This module provides generators for different target languages and frameworks,
//! converting OntologySchema into working code for TypeScript, GraphQL, SQL, and more.
//!
//! ## Semantic Code Generation (v5)
//!
//! The new semantic code generation pipeline transforms RDF ontologies through
//! CONSTRUCT queries into typed code graphs, which are then rendered via Tera templates.
//!
//! ### Pipeline Flow
//!
//! ```text
//! ontology.ttl → Graph → CONSTRUCT rules → Code Graph → Tera → .rs files
//! ```
//!
//! ### Key Types
//!
//! - [`GenerationPipeline`] - Orchestrates the full generation flow
//! - [`CodeGraphBuilder`] - Converts SPARQL results to code entities
//! - [`AuditTrailBuilder`] - Tracks execution for determinism verification

pub mod audit;
pub mod code_graph;
pub mod pipeline;
pub mod typescript;

// Re-export key types
pub use audit::{AuditOutput, AuditStep, AuditTrail, AuditTrailBuilder};
pub use code_graph::{
    CodeEnum, CodeField, CodeGraphBuilder, CodeImpl, CodeImport, CodeItem, CodeMethod, CodeModule,
    CodeParam, CodeStruct, CodeTrait, CodeVariant,
};
pub use pipeline::{
    ExecutedRule, GeneratedFile, GenerationPipeline, PipelineState, RuleType, ValidationResult,
    ValidationSeverity,
};
pub use typescript::TypeScriptGenerator;
