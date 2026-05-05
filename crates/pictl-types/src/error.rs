use serde::{Deserialize, Serialize};
use std::fmt;

/// Error types for pm4wasm operations
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Error {
    /// Configuration or input validation failed
    ValidationError(String),

    /// Event log parsing failed
    ParseError(String),

    /// Algorithm execution failed
    ExecutionError(String),

    /// Hash computation failed
    HashError(String),

    /// Provenance validation failed
    ProvenanceError(String),

    /// Budget constraint violated (latency, memory)
    BudgetExceeded(String),

    /// Internal state error
    StateError(String),

    /// Resource not found
    NotFound(String),

    /// Serialization/deserialization error
    SerializationError(String),

    /// Unknown error
    Unknown(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            Error::ParseError(msg) => write!(f, "Parse error: {}", msg),
            Error::ExecutionError(msg) => write!(f, "Execution error: {}", msg),
            Error::HashError(msg) => write!(f, "Hash error: {}", msg),
            Error::ProvenanceError(msg) => write!(f, "Provenance error: {}", msg),
            Error::BudgetExceeded(msg) => write!(f, "Budget exceeded: {}", msg),
            Error::StateError(msg) => write!(f, "State error: {}", msg),
            Error::NotFound(msg) => write!(f, "Not found: {}", msg),
            Error::SerializationError(msg) => write!(f, "Serialization error: {}", msg),
            Error::Unknown(msg) => write!(f, "Unknown error: {}", msg),
        }
    }
}

impl std::error::Error for Error {}

/// Result type for pm4wasm operations
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = Error::ValidationError("test".to_string());
        assert_eq!(err.to_string(), "Validation error: test");
    }

    #[test]
    fn test_error_serialization() {
        let err = Error::ExecutionError("algorithm failed".to_string());
        let json = serde_json::to_string(&err).unwrap();
        assert!(json.contains("ExecutionError"));
        assert!(json.contains("algorithm failed"));
    }
}
