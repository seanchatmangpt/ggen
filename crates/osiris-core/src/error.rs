//! Error types for OSIRIS Core

use thiserror::Error;

/// OSIRIS Core errors
#[derive(Debug, Error)]
pub enum OSIRISError {
    #[error("Domain not found: {0}")]
    DomainNotFound(String),

    #[error("Workflow not found: {0}")]
    WorkflowNotFound(String),

    #[error("Workflow execution failed: {0}")]
    WorkflowExecutionFailed(String),

    #[error("TPS validation failed: {0}")]
    TPSValidationFailed(String),

    #[error("Autonomic decision error: {0}")]
    AutonomicDecisionError(String),

    #[error("Domain coordination error: {0}")]
    DomainCoordinationError(String),

    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    #[error("Service unavailable: {0}")]
    ServiceUnavailable(String),

    #[error("Invalid state transition: {0}")]
    InvalidStateTransition(String),

    #[error("Timeout error: {0}")]
    Timeout(String),

    #[error("Database error: {0}")]
    DatabaseError(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

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
