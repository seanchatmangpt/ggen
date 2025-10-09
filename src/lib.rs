//! Rgen - Language-agnostic, deterministic code projection CLI
//!
//! This crate provides the core functionality for RDF-based code generation,
//! including template processing, RDF handling, and deterministic output generation.

// Declare test modules
pub mod mock_registry;

// Re-export commonly used types
pub use rgen_utils::error::Result;
