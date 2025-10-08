//! Core graph-aware code generation engine
//! 
//! This crate provides the core functionality for RDF-based code generation,
//! including template processing, RDF handling, and deterministic output generation.

pub mod config;
pub mod e2e_tests;
pub mod generator;
pub mod graph;
pub mod inject;
pub mod pipeline;
pub mod poc;
pub mod register;
pub mod resolver;
pub mod template;
pub mod tera_env;
pub mod validate_frontmatter;

// Re-export commonly used types for convenience
pub use generator::{GenContext, Generator};
pub use graph::Graph;
pub use pipeline::{Pipeline, PipelineBuilder};
pub use template::Template;
