//! Core graph-aware code generation engine
//! 
//! This crate provides the core functionality for RDF-based code generation,
//! including template processing, RDF handling, and deterministic output generation.

pub mod e2e_tests;
pub mod graph;
pub mod pipeline;
pub mod poc;
pub mod register;
pub mod template;

// Re-export commonly used types for convenience
pub use graph::Graph;
pub use pipeline::Pipeline;
pub use template::Template;
