//! Error types for the ontology parser

use thiserror::Error;

/// Result type for ontology operations
pub type Result<T> = std::result::Result<T, OntologyError>;

/// Errors that can occur during ontology parsing and querying
#[derive(Debug, Error)]
pub enum OntologyError {
    /// RDF parsing error
    #[error("RDF parsing error: {0}")]
    ParseError(String),

    /// SPARQL query error
    #[error("SPARQL query error: {0}")]
    QueryError(String),

    /// I/O error
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Invalid IRI
    #[error("Invalid IRI: {0}")]
    InvalidIri(String),

    /// Type mismatch error
    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    /// Binding not found
    #[error("Binding not found: {0}")]
    BindingNotFound(String),

    /// Stream error
    #[error("Stream error: {0}")]
    StreamError(String),

    /// Projection error
    #[error("Projection error: {0}")]
    ProjectionError(String),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

impl From<String> for OntologyError {
    fn from(s: String) -> Self {
        OntologyError::Other(s)
    }
}

impl From<&str> for OntologyError {
    fn from(s: &str) -> Self {
        OntologyError::Other(s.to_string())
    }
}
