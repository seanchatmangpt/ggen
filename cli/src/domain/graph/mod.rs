//! Graph domain layer - RDF operations with real Oxigraph integration
//!
//! This module provides domain logic for graph operations including:
//! - SPARQL query execution with real RDF graphs
//! - RDF data loading and ingestion
//! - Graph export and serialization
//! - Graph validation against SHACL shapes
//!
//! Chicago TDD Principles:
//! - Use REAL Oxigraph in-memory stores for testing
//! - Execute ACTUAL SPARQL queries
//! - Verify REAL RDF state changes
//! - Mock only external AI APIs

pub mod export;
pub mod load;
pub mod query;

// Re-export commonly used types
pub use export::{export_graph, ExportFormat, ExportOptions, ExportStats};
pub use load::{load_rdf, LoadOptions, LoadStats, RdfFormat};
pub use query::{execute_sparql, QueryOptions, QueryResult};
