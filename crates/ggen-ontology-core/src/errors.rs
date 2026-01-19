//! Error types for ontology handling layer
//!
//! Provides comprehensive error handling with context for RDF/TTL parsing,
//! validation, SPARQL queries, and entity mapping operations.

use std::fmt;
use thiserror::Error;

/// Errors that can occur during ontology operations
#[derive(Error, Debug)]
pub enum OntologyError {
    /// IO errors: file not found, read errors, permission issues
    #[error("IO error: {0}")]
    IoError(String),

    /// RDF/TTL parsing errors with context
    #[error("Parse error in file {file} at line {line}: {message}")]
    ParseError {
        /// Path to the file that failed to parse
        file: String,
        /// Line number where error occurred
        line: u32,
        /// Human-readable error message
        message: String,
    },

    /// Ontology validation errors
    #[error("Validation error(s):\n{}", format_errors(.0))]
    ValidationError(Vec<String>),

    /// SPARQL query errors
    #[error("SPARQL query error: {0}")]
    QueryError(String),

    /// Entity mapping errors
    #[error("Mapping error for entity {entity}: {reason}")]
    MapperError {
        /// Entity identifier or name
        entity: String,
        /// Reason for mapping failure
        reason: String,
    },

    /// Triple store initialization errors
    #[error("Triple store error: {0}")]
    TripleStoreError(String),

    /// Configuration or setup errors
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Unknown or internal errors
    #[error("Internal error: {0}")]
    InternalError(String),
}

/// Helper function to format error lists for display
fn format_errors(errors: &[String]) -> String {
    errors
        .iter()
        .enumerate()
        .map(|(i, e)| format!("  {}. {}", i + 1, e))
        .collect::<Vec<_>>()
        .join("\n")
}

impl OntologyError {
    /// Create an IO error with context
    pub fn io<S: Into<String>>(message: S) -> Self {
        Self::IoError(message.into())
    }

    /// Create a parse error with file and line information
    pub fn parse<S: Into<String>>(file: S, line: u32, message: S) -> Self {
        Self::ParseError {
            file: file.into(),
            line,
            message: message.into(),
        }
    }

    /// Create a validation error from multiple error messages
    pub fn validation(errors: Vec<String>) -> Self {
        Self::ValidationError(errors)
    }

    /// Create a SPARQL query error
    pub fn query<S: Into<String>>(message: S) -> Self {
        Self::QueryError(message.into())
    }

    /// Create a mapper error with context
    pub fn mapper<S: Into<String>>(entity: S, reason: S) -> Self {
        Self::MapperError {
            entity: entity.into(),
            reason: reason.into(),
        }
    }

    /// Create a triple store error
    pub fn triple_store<S: Into<String>>(message: S) -> Self {
        Self::TripleStoreError(message.into())
    }

    /// Create a configuration error
    pub fn config<S: Into<String>>(message: S) -> Self {
        Self::ConfigError(message.into())
    }

    /// Create an internal error
    pub fn internal<S: Into<String>>(message: S) -> Self {
        Self::InternalError(message.into())
    }
}

/// Result type using OntologyError
pub type Result<T> = std::result::Result<T, OntologyError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_io_error_creation() {
        let err = OntologyError::io("file not found");
        assert_eq!(err.to_string(), "IO error: file not found");
    }

    #[test]
    fn test_parse_error_creation() {
        let err = OntologyError::parse("ontology.ttl", 42, "invalid predicate");
        let msg = err.to_string();
        assert!(msg.contains("ontology.ttl"));
        assert!(msg.contains("42"));
        assert!(msg.contains("invalid predicate"));
    }

    #[test]
    fn test_validation_error_formatting() {
        let errors = vec![
            "Missing required field".to_string(),
            "Invalid reference".to_string(),
        ];
        let err = OntologyError::validation(errors);
        let msg = err.to_string();
        assert!(msg.contains("1."));
        assert!(msg.contains("2."));
        assert!(msg.contains("Missing required field"));
    }

    #[test]
    fn test_query_error_creation() {
        let err = OntologyError::query("SPARQL syntax error");
        assert_eq!(err.to_string(), "SPARQL query error: SPARQL syntax error");
    }

    #[test]
    fn test_mapper_error_creation() {
        let err = OntologyError::mapper("policy-001", "no matching ontology class");
        let msg = err.to_string();
        assert!(msg.contains("policy-001"));
        assert!(msg.contains("no matching ontology class"));
    }
}
