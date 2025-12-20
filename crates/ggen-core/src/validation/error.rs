//! Error types for SHACL validation
//!
//! This module defines error types for the validation system following
//! Constitution Principle VII (Error Handling Standards).
//!
//! ## Design Principles
//!
//! - ✓ Uses thiserror for ergonomic error definitions
//! - ✓ Provides context-rich error messages
//! - ✓ NO unwrap() or expect() in production code
//! - ✓ All errors implement std::error::Error

use std::path::PathBuf;
use thiserror::Error;

/// Result type alias for validation operations
///
/// Uses ValidationError as the error type for all validation operations.
/// This follows Constitution Principle VII (Result<T,E> for all fallible operations).
pub type Result<T> = std::result::Result<T, ValidationError>;

/// Validation error types
///
/// Represents all possible errors that can occur during SHACL validation.
/// Each variant provides context-specific information for debugging.
///
/// ## Constitution Compliance
///
/// - ✓ Principle VII: Result<T,E> error handling (no panic in production)
/// - ✓ Uses thiserror for automatic Error trait implementation
/// - ✓ Context-rich error messages with file paths and line numbers where applicable
#[derive(Error, Debug)]
pub enum ValidationError {
    /// TTL syntax parsing error
    ///
    /// Occurs when loading TTL files (ontology or shapes) with invalid Turtle syntax.
    ///
    /// Example:
    /// ```text
    /// ParseError: Invalid Turtle syntax at line 42 in spec.ttl
    /// Caused by: Unexpected token '<' expecting IRI or blank node
    /// ```
    #[error("TTL parse error in {file}: {message}")]
    ParseError {
        /// File path where the parse error occurred
        file: PathBuf,
        /// Detailed error message from the parser
        message: String,
        /// Line number if available (0 if unknown)
        line: usize,
        /// Column number if available (0 if unknown)
        column: usize,
    },

    /// SHACL shapes file loading error
    ///
    /// Occurs when shapes.ttl file cannot be found, read, or contains invalid SHACL definitions.
    ///
    /// Example:
    /// ```text
    /// ShapeLoadError: Could not load SHACL shapes from shapes.ttl
    /// Caused by: File not found
    /// ```
    #[error("Failed to load SHACL shapes from {file}: {reason}")]
    ShapeLoadError {
        /// Path to the shapes file that failed to load
        file: PathBuf,
        /// Reason for the failure (file not found, invalid SHACL, etc.)
        reason: String,
    },

    /// SPARQL query execution error
    ///
    /// Occurs when a SPARQL query (generated from SHACL shapes) fails to execute
    /// against the RDF graph.
    ///
    /// Example:
    /// ```text
    /// SparqlError: SPARQL query failed
    /// Query: SELECT ?s WHERE { ?s a sk:UserStory }
    /// Caused by: Unknown prefix 'sk'
    /// ```
    #[error("SPARQL query execution failed: {message}")]
    SparqlError {
        /// Error message from Oxigraph SPARQL engine
        message: String,
        /// The SPARQL query that failed (for debugging)
        query: String,
    },

    /// I/O error (file system operations)
    ///
    /// Wraps std::io::Error for file operations (reading TTL files, writing reports).
    ///
    /// Example:
    /// ```text
    /// IoError: Failed to read file: Permission denied (os error 13)
    /// ```
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Oxigraph-specific errors
    ///
    /// Wraps Oxigraph errors (graph operations, SPARQL parsing, etc.)
    ///
    /// Example:
    /// ```text
    /// OxigraphError: Failed to insert triple into graph
    /// ```
    #[error("Oxigraph error: {0}")]
    OxigraphError(String),

    /// Invalid SHACL constraint configuration
    ///
    /// Occurs when SHACL shapes contain unsupported or malformed constraints.
    ///
    /// Example:
    /// ```text
    /// InvalidConstraint: Unsupported constraint type 'sh:uniqueLang' in shape :UserStoryShape
    /// Supported: sh:minCount, sh:maxCount, sh:datatype, sh:in, sh:pattern, sh:minLength, sh:maxLength
    /// ```
    #[error("Invalid SHACL constraint: {message}")]
    InvalidConstraint {
        /// Error message describing the invalid constraint
        message: String,
        /// Shape IRI that contains the invalid constraint
        shape_iri: String,
    },

    /// Validation timeout
    ///
    /// Occurs when validation exceeds configured timeout threshold.
    ///
    /// Example:
    /// ```text
    /// TimeoutError: Validation exceeded 30s timeout
    /// ```
    #[error("Validation timeout after {duration_ms}ms (limit: {limit_ms}ms)")]
    TimeoutError {
        /// Actual duration in milliseconds
        duration_ms: u64,
        /// Configured timeout limit in milliseconds
        limit_ms: u64,
    },
}

impl ValidationError {
    /// Create a parse error from file path and message
    pub fn parse_error(
        file: impl Into<PathBuf>,
        message: impl Into<String>,
        line: usize,
        column: usize,
    ) -> Self {
        ValidationError::ParseError {
            file: file.into(),
            message: message.into(),
            line,
            column,
        }
    }

    /// Create a shape load error
    pub fn shape_load_error(file: impl Into<PathBuf>, reason: impl Into<String>) -> Self {
        ValidationError::ShapeLoadError {
            file: file.into(),
            reason: reason.into(),
        }
    }

    /// Create a SPARQL error
    pub fn sparql_error(message: impl Into<String>, query: impl Into<String>) -> Self {
        ValidationError::SparqlError {
            message: message.into(),
            query: query.into(),
        }
    }

    /// Create an invalid constraint error
    pub fn invalid_constraint(message: impl Into<String>, shape_iri: impl Into<String>) -> Self {
        ValidationError::InvalidConstraint {
            message: message.into(),
            shape_iri: shape_iri.into(),
        }
    }

    /// Create a timeout error
    pub fn timeout_error(duration_ms: u64, limit_ms: u64) -> Self {
        ValidationError::TimeoutError {
            duration_ms,
            limit_ms,
        }
    }
}

// Implement From<oxigraph::Error> for ValidationError
impl From<oxigraph::store::LoaderError> for ValidationError {
    fn from(err: oxigraph::store::LoaderError) -> Self {
        ValidationError::OxigraphError(err.to_string())
    }
}

impl From<oxigraph::sparql::QueryEvaluationError> for ValidationError {
    fn from(err: oxigraph::sparql::QueryEvaluationError) -> Self {
        ValidationError::SparqlError {
            message: err.to_string(),
            query: String::new(), // Query context added by caller
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_error_construction() {
        let err = ValidationError::parse_error(
            PathBuf::from("test.ttl"),
            "Unexpected token",
            42,
            15,
        );

        match err {
            ValidationError::ParseError {
                file,
                message,
                line,
                column,
            } => {
                assert_eq!(file, PathBuf::from("test.ttl"));
                assert_eq!(message, "Unexpected token");
                assert_eq!(line, 42);
                assert_eq!(column, 15);
            }
            _ => panic!("Expected ParseError"),
        }
    }

    #[test]
    fn test_shape_load_error_display() {
        let err =
            ValidationError::shape_load_error(PathBuf::from("shapes.ttl"), "File not found");

        let display = format!("{}", err);
        assert!(display.contains("shapes.ttl"));
        assert!(display.contains("File not found"));
    }

    #[test]
    fn test_sparql_error_construction() {
        let err = ValidationError::sparql_error(
            "Unknown prefix 'sk'",
            "SELECT ?s WHERE { ?s a sk:UserStory }",
        );

        match err {
            ValidationError::SparqlError { message, query } => {
                assert_eq!(message, "Unknown prefix 'sk'");
                assert!(query.contains("sk:UserStory"));
            }
            _ => panic!("Expected SparqlError"),
        }
    }

    #[test]
    fn test_invalid_constraint_error() {
        let err =
            ValidationError::invalid_constraint("Unsupported constraint", ":UserStoryShape");

        match err {
            ValidationError::InvalidConstraint {
                message,
                shape_iri,
            } => {
                assert_eq!(message, "Unsupported constraint");
                assert_eq!(shape_iri, ":UserStoryShape");
            }
            _ => panic!("Expected InvalidConstraint"),
        }
    }

    #[test]
    fn test_timeout_error() {
        let err = ValidationError::timeout_error(35000, 30000);

        match err {
            ValidationError::TimeoutError {
                duration_ms,
                limit_ms,
            } => {
                assert_eq!(duration_ms, 35000);
                assert_eq!(limit_ms, 30000);
            }
            _ => panic!("Expected TimeoutError"),
        }
    }

    #[test]
    fn test_result_type_alias() {
        fn returns_result() -> Result<i32> {
            Ok(42)
        }

        assert_eq!(returns_result().unwrap(), 42);
    }

    #[test]
    fn test_error_propagation() {
        fn inner() -> Result<()> {
            Err(ValidationError::parse_error(
                "test.ttl",
                "Syntax error",
                10,
                5,
            ))
        }

        fn outer() -> Result<()> {
            inner()?; // Error propagates via ?
            Ok(())
        }

        assert!(outer().is_err());
    }
}
