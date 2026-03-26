//! SPARQL query execution module
//!
//! Provides utilities for executing SPARQL queries against RDF graphs
//! and converting results to template-friendly formats.

pub mod executor;

pub use executor::{execute_query, execute_query_inline};
