//! AI tools for MCP - Autonomous AI-powered code generation
//!
//! This module provides AI-powered code generation capabilities via MCP,
//! organized into focused submodules:
//!
//! - `client`: AI client initialization and management
//! - `generators`: Template, SPARQL, and ontology generation
//! - `project_gen`: Project scaffolding and structure generation
//! - `validation`: Template and code validation utilities

pub mod client;
pub mod generators;
pub mod project_gen;
pub mod validation;

// Re-export main functions for backward compatibility
pub use client::list_providers;
pub use generators::{
    generate_template, generate_sparql, generate_ontology,
    extend_graph, validate_and_improve
};
pub use project_gen::generate_project;
