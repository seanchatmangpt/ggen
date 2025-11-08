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
pub mod visualize;

// Re-export commonly used types
pub use export::{
    execute_export, export_graph, ExportFormat, ExportInput, ExportOptions, ExportOutput,
    ExportStats,
};
pub use load::{execute_load, load_rdf, LoadInput, LoadOptions, LoadOutput, LoadStats, RdfFormat};
pub use query::{execute_query, execute_sparql, QueryInput, QueryOptions, QueryResult};
pub use visualize::{
    execute_visualize, visualize_graph, VisualizeFormat, VisualizeInput, VisualizeOptions,
    VisualizeOutput, VisualizeStats,
};
