//! Schema Parser and Code Generators
//!
//! This module provides a unified schema representation for A2A (agent-to-agent)
//! communication, with parsers from multiple formats (JSON Schema, OpenAPI, RDF)
//! and code generators for multiple target languages (Rust, Go, Elixir, Java, TypeScript).

pub mod generators;
pub mod parser;

pub use generators::{
    ElixirGenerator, GoGenerator, JavaGenerator, PythonGenerator, RustGenerator,
    TypeScriptGenerator,
};
pub use parser::{Field, Schema, SchemaParser, SchemaType};
