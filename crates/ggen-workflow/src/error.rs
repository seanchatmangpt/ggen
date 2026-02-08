//! Workflow error types and result definitions
//!
//! This module defines the comprehensive error handling system for the ggen workflow.
//! All operations use Result<T, WorkflowError> for proper error propagation without
//! unwrap/expect operations in production code.

use thiserror::Error;

/// Comprehensive workflow error types
#[derive(Debug, Error)]
pub enum WorkflowError {
    /// RDF ontology parsing errors
    #[error("RDF parsing failed: {0}")]
    RdfParse(String),

    /// SPARQL query execution errors
    #[error("SPARQL query failed: {error_source}: {query}")]
    SparqlQuery {
        error_source: String,
        query: String,
    },

    /// Validation errors
    #[error("Workflow validation failed: {0}")]
    Validation(String),

    /// Pattern execution errors
    #[error("Pattern execution failed: {pattern}: {message}")]
    PatternExecution {
        pattern: String,
        message: String,
    },

    /// Receipt generation errors
    #[error("Receipt generation failed: {0}")]
    ReceiptGeneration(String),

    /// NIF resource errors
    #[error("NIF resource error: {0}")]
    Resource(String),

    /// Configuration errors
    #[error("Configuration error: {0}")]
    Configuration(String),

    /// Timeout errors
    #[error("Operation timed out after {timeout_ms}ms: {operation}")]
    Timeout {
        operation: String,
        timeout_ms: u64,
    },

    /// IO errors
    #[error("IO operation failed: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization errors
    #[error("Serialization failed: {0}")]
    Serialization(String),

    /// Deserialization errors
    #[error("Deserialization failed: {0}")]
    Deserialization(String),

    /// Unsupported operation
    #[error("Unsupported operation: {0}")]
    Unsupported(String),

    /// System errors
    #[error("System error: {0}")]
    System(String),
}

/// Workflow result type alias for consistent error handling
pub type WorkflowResult<T> = Result<T, WorkflowError>;

// Implement From<serde_json::Error> for WorkflowError
impl From<serde_json::Error> for WorkflowError {
    fn from(err: serde_json::Error) -> Self {
        WorkflowError::Deserialization(err.to_string())
    }
}

/// Common error creation helpers
pub mod errors {
    use super::*;

    /// Create a new RDF parsing error
    pub fn rdf_parse<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::RdfParse(message.into())
    }

    /// Create a new SPARQL query error
    pub fn sparql_query(error_source: String, query: String) -> WorkflowError {
        WorkflowError::SparqlQuery { error_source, query }
    }

    /// Create a new validation error
    pub fn validation<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::Validation(message.into())
    }

    /// Create a new pattern execution error
    pub fn pattern_execution<P: Into<String>, M: Into<String>>(pattern: P, message: M) -> WorkflowError {
        WorkflowError::PatternExecution {
            pattern: pattern.into(),
            message: message.into(),
        }
    }

    /// Create a new receipt generation error
    pub fn receipt_generation<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::ReceiptGeneration(message.into())
    }

    /// Create a new resource error
    pub fn resource<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::Resource(message.into())
    }

    /// Create a new configuration error
    pub fn configuration<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::Configuration(message.into())
    }

    /// Create a new timeout error
    pub fn timeout<O: Into<String>>(operation: O, timeout_ms: u64) -> WorkflowError {
        WorkflowError::Timeout {
            operation: operation.into(),
            timeout_ms,
        }
    }

    /// Create a new serialization error
    pub fn serialization<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::Serialization(message.into())
    }

    /// Create a new deserialization error
    pub fn deserialization<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::Deserialization(message.into())
    }

    /// Create an unsupported operation error
    pub fn unsupported<S: Into<String>>(operation: S) -> WorkflowError {
        WorkflowError::Unsupported(operation.into())
    }

    /// Create a system error
    pub fn system<S: Into<String>>(message: S) -> WorkflowError {
        WorkflowError::System(message.into())
    }
}

/// Error handling utilities
pub mod error_utils {
    use super::*;

    /// Check if an error is recoverable (can be retried)
    pub fn is_recoverable(err: &WorkflowError) -> bool {
        matches!(
            err,
            WorkflowError::RdfParse(_)
                | WorkflowError::SparqlQuery { .. }
                | WorkflowError::Validation(_)
                | WorkflowError::PatternExecution { .. }
                | WorkflowError::ReceiptGeneration(_)
        )
    }

    /// Check if an error requires immediate termination
    pub fn is_terminal(err: &WorkflowError) -> bool {
        matches!(
            err,
            WorkflowError::Resource(_)
                | WorkflowError::Io(_)
                | WorkflowError::Configuration(_)
                | WorkflowError::Timeout { .. }
                | WorkflowError::Unsupported(_)
                | WorkflowError::System(_)
        )
    }

    /// Get error category for classification
    pub fn error_category(err: &WorkflowError) -> &'static str {
        match err {
            WorkflowError::RdfParse(_) | WorkflowError::SparqlQuery { .. } => "parsing",
            WorkflowError::Validation(_) => "validation",
            WorkflowError::PatternExecution { .. } => "execution",
            WorkflowError::ReceiptGeneration(_) => "receipt",
            WorkflowError::Resource(_) => "resource",
            WorkflowError::Configuration(_) => "configuration",
            WorkflowError::Timeout { .. } => "timeout",
            WorkflowError::Io(_) => "io",
            WorkflowError::Serialization(_) | WorkflowError::Deserialization(_) => "serialization",
            WorkflowError::Unsupported(_) => "unsupported",
            WorkflowError::System(_) => "system",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = errors::rdf_parse("Test parsing error");
        assert!(matches!(error, WorkflowError::RdfParse(_)));

        let error = errors::validation("Test validation error");
        assert!(matches!(error, WorkflowError::Validation(_)));

        let error = errors::timeout("test_operation", 1000);
        assert!(matches!(error, WorkflowError::Timeout { .. }));
    }

    #[test]
    fn test_error_category() {
        let error = errors::rdf_parse("Test");
        assert_eq!(error_utils::error_category(&error), "parsing");

        let error = errors::validation("Test");
        assert_eq!(error_utils::error_category(&error), "validation");

        let error = errors::timeout("test", 1000);
        assert_eq!(error_utils::error_category(&error), "timeout");
    }

    #[test]
    fn test_error_classification() {
        let recoverable = errors::rdf_parse("Recoverable error");
        assert!(error_utils::is_recoverable(&recoverable));

        let terminal = errors::resource("Terminal error");
        assert!(error_utils::is_terminal(&terminal));
    }
}