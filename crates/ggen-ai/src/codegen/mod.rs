//! Code generation with caching and metrics
//!
//! This module provides specialized code generators for converting RDF ontologies
//! and other sources into Rust code and DSPy Signatures with comprehensive metrics
//! tracking and LRU caching for improved performance.
//!
//! # Features
//!
//! - **Metrics Tracking**: Process metrics including signatures generated, execution time, and cache statistics
//! - **LRU Caching**: Configurable caching for property shapes to improve performance on repeated queries
//! - **Six Sigma Analysis**: Capability level calculation based on error rates
//! - **Thread-Safe**: All shared state protected by Arc<Mutex<>>
//!
//! # Modules
//!
//! - `metrics` - Process metrics and timing utilities
//! - `shacl_parser` - Extract constraints from RDF SHACL shapes for validation and codegen
//! - `ttl_to_signature` - Convert Turtle ontologies with SHACL shapes to DSPy Signatures with caching

pub mod metrics;
pub mod shacl_parser;
// TODO: Fix ttl_to_signature.rs - oxigraph API has deprecated Store::query, needs migration to SparqlEvaluator
// pub mod ttl_to_signature;

pub use metrics::{ProcessMetrics, Timer};
pub use shacl_parser::{map_xsd_to_rust_type, SHACLConstraint, SHACLParser};
// pub use ttl_to_signature::{PropertyShape, TTLToSignatureTranspiler};
