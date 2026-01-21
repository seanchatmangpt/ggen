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
//! # Lean Manufacturing Optimization
//!
//! The `lean` module implements Muda (無駄) waste elimination principles:
//! - **Cached RDF parsing** - eliminates redundant file loads
//! - **Batch SPARQL execution** - removes N+1 query patterns
//! - **Just-in-time compilation** - only compiles templates when needed
//! - **Incremental generation** - skips unchanged files
//! - **Query result memoization** - caches SPARQL results with TTL
//!
//! See [`lean`] module for details.
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
//!
//! # Lean Example
//!
//! ```no_run
//! use ggen_ai::rdf::lean::{LeanRdfParser, LeanQueryExecutor, QueryCache};
//! use std::path::Path;
//! use std::time::Duration;
//!
//! # fn example() -> anyhow::Result<()> {
//! // Just-in-time parsing with caching
//! let mut parser = LeanRdfParser::new()?;
//! parser.load_ttl_cached(Path::new("ontology.ttl"))?; // Loads
//! parser.load_ttl_cached(Path::new("ontology.ttl"))?; // Cached!
//!
//! // Batch SPARQL with memoization
//! let cache = QueryCache::with_ttl(Duration::from_secs(3600));
//! let executor = LeanQueryExecutor::new(parser.get_store(), cache);
//! let project = executor.extract_project_batch()?; // Single query vs N+1
//! # Ok(())
//! # }
//! ```

pub mod generator;
pub mod lean;
pub mod parser;
pub mod query;
pub mod renderer;
pub mod template;
pub mod types;

// Re-export main types for convenience
pub use generator::CliGenerator;
pub use lean::{
    IncrementalTracker, LeanQueryExecutor, LeanRdfParser, OntologyOptimizer, QueryCache,
};
pub use parser::RdfParser;
pub use query::QueryExecutor;
pub use renderer::TemplateRenderer;
pub use types::{
    Argument, ArgumentType, CliConfig, CliProject, Dependency, Noun, RdfArgument, RdfCommand,
    RdfFlag, TemplateContext, Validation, Verb,
};
