//! Error types for OSIRIS Core

use thiserror::Error;

/// OSIRIS Core errors
#[derive(Debug, Error)]
pub enum OSIRISError {
    /// Domain not found
    #[error("Domain not found: {0}")]
    DomainNotFound(String),

    /// Workflow not found
    #[error("Workflow not found: {0}")]
    WorkflowNotFound(String),

    /// Workflow execution failed
    #[error("Workflow execution failed: {0}")]
    WorkflowExecutionFailed(String),

    /// TPS validation failed
    #[error("TPS validation failed: {0}")]
    TPSValidationFailed(String),

    /// Autonomic decision error
    #[error("Autonomic decision error: {0}")]
    AutonomicDecisionError(String),

    /// Domain coordination error
    #[error("Domain coordination error: {0}")]
    DomainCoordinationError(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    /// Service unavailable
    #[error("Service unavailable: {0}")]
    ServiceUnavailable(String),

    /// Invalid state transition
    #[error("Invalid state transition: {0}")]
    InvalidStateTransition(String),

    /// Timeout error
    #[error("Timeout error: {0}")]
    Timeout(String),

    /// Database error
    #[error("Database error: {0}")]
    DatabaseError(String),

    /// Network error
    #[error("Network error: {0}")]
    NetworkError(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// Unknown error
    #[error("Unknown error: {0}")]
    Unknown(String),
}

/// Result type for OSIRIS operations
pub type Result<T> = std::result::Result<T, OSIRISError>;

impl From<anyhow::Error> for OSIRISError {
    fn from(err: anyhow::Error) -> Self {
        OSIRISError::Unknown(err.to_string())
    }
}

impl From<serde_json::Error> for OSIRISError {
    fn from(err: serde_json::Error) -> Self {
        OSIRISError::SerializationError(err.to_string())
    }
}

impl From<std::io::Error> for OSIRISError {
    fn from(err: std::io::Error) -> Self {
        OSIRISError::Unknown(err.to_string())
    }
}

impl From<tokio::time::error::Elapsed> for OSIRISError {
    fn from(err: tokio::time::error::Elapsed) -> Self {
        OSIRISError::Timeout(err.to_string())
    }
}
