//! Error types for ggen-dspy
//!
//! All errors in this crate use `Result<T, DspyError>` following CLAUDE.md conventions.
//! No unwrap/expect in production code - all fallible operations return Result.

use thiserror::Error;

/// Result type alias for ggen-dspy operations
pub type Result<T> = std::result::Result<T, DspyError>;

/// Main error type for ggen-dspy operations
#[derive(Error, Debug)]
pub enum DspyError {
    /// LLM client error (from ggen-ai)
    #[error("LLM error: {0}")]
    LlmError(#[from] ggen_ai::GgenAiError),

    /// Module execution error
    #[error("Module error: {0}")]
    ModuleError(String),

    /// Optimizer error
    #[error("Optimizer error: {0}")]
    OptimizerError(String),

    /// Evaluation error
    #[error("Evaluation error: {0}")]
    EvaluationError(String),

    /// Assertion failure
    #[error("Assertion failed: {0}")]
    AssertionError(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// I/O error
    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    /// Missing required field
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Invalid input
    #[error("Invalid input: {0}")]
    InvalidInput(String),

    /// Pattern error
    #[error("Pattern error: {0}")]
    PatternError(String),

    /// Cache error
    #[error("Cache error: {0}")]
    CacheError(String),

    /// Tool execution failed
    #[error("Tool '{tool}' execution failed: {reason}")]
    ToolError { tool: String, reason: String },

    /// Retrieval operation failed
    #[error("Retrieval failed: {0}")]
    RetrievalError(String),

    /// Validation failed
    #[error("Validation failed: {0}")]
    ValidationError(String),

    /// Timeout occurred
    #[error("Operation timed out after {0}ms")]
    Timeout(u64),

    /// Missing required input
    #[error("Missing required input: {0}")]
    MissingInput(String),

    /// Invalid input type
    #[error("Invalid input type for field '{field}': expected {expected}")]
    InvalidInputType { field: String, expected: String },

    /// Generic error with context
    #[error("{0}")]
    Other(String),
}

impl From<ggen_ai::dspy::ModuleError> for DspyError {
    fn from(err: ggen_ai::dspy::ModuleError) -> Self {
        match err {
            ggen_ai::dspy::ModuleError::MissingInput(field) => DspyError::MissingInput(field),
            ggen_ai::dspy::ModuleError::InvalidInputType(field, expected) => {
                DspyError::InvalidInputType { field, expected }
            }
            ggen_ai::dspy::ModuleError::LlmError(msg) => DspyError::ModuleError(msg),
            ggen_ai::dspy::ModuleError::Other(msg) => DspyError::Other(msg),
        }
    }
}

impl DspyError {
    /// Create a module error
    pub fn module(msg: impl Into<String>) -> Self {
        Self::ModuleError(msg.into())
    }

    /// Create an optimizer error
    pub fn optimizer(msg: impl Into<String>) -> Self {
        Self::OptimizerError(msg.into())
    }

    /// Create an evaluation error
    pub fn evaluation(msg: impl Into<String>) -> Self {
        Self::EvaluationError(msg.into())
    }

    /// Create an assertion error
    pub fn assertion(msg: impl Into<String>) -> Self {
        Self::AssertionError(msg.into())
    }

    /// Create a configuration error
    pub fn config(msg: impl Into<String>) -> Self {
        Self::ConfigError(msg.into())
    }

    /// Create a pattern error
    pub fn pattern(msg: impl Into<String>) -> Self {
        Self::PatternError(msg.into())
    }

    /// Create a cache error
    pub fn cache(msg: impl Into<String>) -> Self {
        Self::CacheError(msg.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_construction() {
        let err = DspyError::module("test error");
        assert_eq!(err.to_string(), "Module error: test error");

        let err = DspyError::optimizer("optimization failed");
        assert_eq!(err.to_string(), "Optimizer error: optimization failed");
    }

    #[test]
    fn test_missing_field() {
        let err = DspyError::MissingField("test_field".to_string());
        assert_eq!(err.to_string(), "Missing required field: test_field");
    }
}
