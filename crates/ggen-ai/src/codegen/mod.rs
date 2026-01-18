//! Code generation with caching and metrics
//!
//! This module provides specialized code generators for converting RDF ontologies
//! and other sources into Rust code and DSPy Signatures with comprehensive metrics
//! tracking and LRU caching for improved performance.
//!
//! # Security
//!
//! All SPARQL query construction is protected against injection attacks through:
//! - Strict URI validation in the `validation` module
//! - Safe SPARQL query construction patterns
//! - Comprehensive error handling with `Result<T, E>` types
//!
//! # Features
//!
//! - **Metrics Tracking**: Process metrics including signatures generated, execution time, and cache statistics
//! - **LRU Caching**: Configurable caching for property shapes to improve performance on repeated queries
//! - **Six Sigma Analysis**: Capability level calculation based on error rates
//! - **Thread-Safe**: All shared state protected by Arc<Mutex<>>
//! - **SPARQL Injection Prevention**: Safe validation of all RDF identifiers before query construction
//!
//! # Modules
//!
//! - `datetime` - DateTime validation and conversion from XSD formats to Rust chrono types
//! - `metrics` - Process metrics and timing utilities
//! - `rdf_list_validator` - Validate RDF list chains for well-formedness and integrity
//! - `shacl_parser` - Extract constraints from RDF SHACL shapes for validation and codegen
//! - `ttl_to_signature` - Convert Turtle ontologies with SHACL shapes to DSPy Signatures with caching
//! - `validation` - RDF identifier validation for SPARQL query safety

pub mod datetime;
pub mod metrics;
pub mod rdf_list_validator;
pub mod shacl_parser;
pub mod ttl_to_signature;
pub mod validation;

pub use datetime::{
    parse_date, parse_datetime, parse_duration, parse_time, validate_gday, validate_gmonth,
    validate_gmonth_day, validate_gyear, validate_gyear_month, DateTimeValidation,
};
pub use metrics::{ProcessMetrics, Timer};
pub use rdf_list_validator::{RdfListValidator, ValidationError};
pub use shacl_parser::{map_xsd_to_rust_type, SHACLConstraint, SHACLParser};
pub use ttl_to_signature::{PropertyShape, TTLToSignatureTranspiler};
pub use validation::{validate_rdf_uri, validate_property_name, escape_sparql_property, validate_prefixed_iri};
