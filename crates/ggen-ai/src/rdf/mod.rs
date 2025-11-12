//! RDF-based CLI project generation.
//!
//! This module provides functionality to parse RDF/RDFS descriptions of CLI
//! applications and generate complete Rust projects using the Clap library
//! with the noun-verb command pattern.
//!
//! # Architecture
//!
//! The generation process follows these steps:
//!
//! 1. **Parse RDF**: Load and parse Turtle (.ttl) files into an RDF graph (parser.rs)
//! 2. **Query Data**: Execute SPARQL queries to extract structured data (query.rs)
//! 3. **Validate**: Ensure project structure is valid (generator.rs)
//! 4. **Render Templates**: Use Tera to generate Rust source files (template.rs)
//! 5. **Post-Process**: Run cargo fmt and cargo check (generator.rs)
//!
//! # Complete Example (Phase 5)
//!
//! ```no_run
//! use ggen_ai::rdf::CliGenerator;
//! use std::path::{Path, PathBuf};
//!
//! # fn example() -> anyhow::Result<()> {
//! let generator = CliGenerator::new(PathBuf::from("templates"));
//! generator.generate_from_ttl(
//!     Path::new("sample-cli.ttl"),
//!     Path::new("output")
//! )?;
//! # Ok(())
//! # }
//! ```

pub mod generator;
pub mod parser;
pub mod query;
pub mod renderer;
pub mod template;
pub mod types;

// Re-export main types for convenience
pub use generator::CliGenerator;
pub use parser::RdfParser;
pub use query::QueryExecutor;
pub use renderer::TemplateRenderer;
pub use types::{
    Argument, ArgumentType, CliConfig, CliProject, Dependency, Noun, RdfArgument, RdfCommand,
    RdfFlag, TemplateContext, Validation, Verb,
};
