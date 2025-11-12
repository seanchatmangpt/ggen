//! CLI Generator for 2026 best practices
//!
//! This module provides generators for creating CLI projects from RDF ontologies
//! following 2026 best practices:
//! - Workspace structure with separate CLI and domain crates
//! - Domain function references for stable contracts
//! - clap-noun-verb v3.3.0 integration
//! - Hyper-Advanced DX (enhanced error messages, live preview, IDE hints)

pub mod cli_layer;
pub mod domain_layer;
pub mod dx;
pub mod ontology_parser;
pub mod types;
pub mod workspace;

pub use cli_layer::CliLayerGenerator;
pub use domain_layer::DomainLayerGenerator;
pub use ontology_parser::OntologyParser;
pub use types::{Argument, ArgumentType, CliProject, Dependency, Noun, Validation, Verb};
pub use workspace::WorkspaceGenerator;
