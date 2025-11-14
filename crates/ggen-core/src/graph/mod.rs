//! Comprehensive Oxigraph wrapper with full Store API coverage
//!
//! This module provides a complete, type-safe wrapper around Oxigraph's RDF store
//! with intelligent caching, thread-safety, and comprehensive feature coverage.
//!
//! ## Module Structure
//!
//! - [`Graph`] - Main wrapper with SPARQL query caching and epoch-based invalidation
//! - [`GraphStore`] - Storage operations (persistent storage, file I/O)
//! - [`GraphUpdate`] - SPARQL Update operations (INSERT, DELETE, UPDATE, etc.)
//! - [`GraphQuery`] - Advanced query building and execution
//! - [`GraphExport`] - RDF serialization in all formats
//!
//! ## Features
//!
//! - **Full Oxigraph API Coverage**: All Store methods wrapped
//! - **SPARQL 1.1**: Query and Update support
//! - **Named Graphs**: Full support for RDF datasets
//! - **Thread-Safe**: Cheap cloning via Arc for concurrent access
//! - **Query Caching**: LRU cache with epoch-based invalidation
//! - **Multiple RDF Formats**: Turtle, N-Triples, RDF/XML, TriG, N-Quads
//! - **Persistent Storage**: On-disk database support
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_core::graph::{Graph, GraphUpdate, GraphExport};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Create in-memory graph
//! let graph = Graph::new()?;
//!
//! // Load RDF data
//! graph.insert_turtle(r#"
//!     @prefix ex: <http://example.org/> .
//!     ex:alice a ex:Person .
//! "#)?;
//!
//! // Query
//! let results = graph.query("SELECT ?s WHERE { ?s ?p ?o }")?;
//!
//! // Update
//! let update = GraphUpdate::new(&graph);
//! update.insert("INSERT DATA { ex:bob a ex:Person }")?;
//!
//! // Export
//! let export = GraphExport::new(&graph);
//! export.write_to_file("output.ttl", oxigraph::io::RdfFormat::Turtle)?;
//! # Ok(())
//! # }
//! ```

pub mod core;
#[cfg(test)]
mod core_fs_tests;
pub mod export;
#[cfg(test)]
mod export_tests;
pub mod query;
pub mod store;
#[cfg(test)]
mod store_tests;
pub mod types;
pub mod update;

// Re-export main types
pub use core::{build_prolog, Graph};
pub use export::GraphExport;
pub use query::GraphQuery;
pub use store::GraphStore;
pub use types::CachedResult;
pub use update::GraphUpdate;
