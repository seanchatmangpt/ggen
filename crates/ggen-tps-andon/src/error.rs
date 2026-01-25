//! Error types for Andon system

use thiserror::Error;

/// Result type for Andon operations
pub type Result<T> = std::result::Result<T, AndonError>;

/// Andon system errors
#[derive(Error, Debug)]
#[non_exhaustive]
pub enum AndonError {
    /// I/O error
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),

    /// JSON error
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::error::Error),

    /// Configuration error
    #[error("Configuration error: {0}")]
    Configuration(String),

    /// Logger error
    #[error("Logger error: {0}")]
    Logger(String),

    /// Metrics error
    #[error("Metrics error: {0}")]
    Metrics(String),

    /// Tracer error
    #[error("Tracer error: {0}")]
    Tracer(String),

    /// Observer error
    #[error("Observer error: {0}")]
    Observer(String),

    /// Alert error
    #[error("Alert error: {0}")]
    Alert(String),

    /// HTTP request error
    #[error("HTTP request failed: {0}")]
    HttpError(String),

    /// Timeout error
    #[error("Operation timed out")]
    Timeout,

    /// Not found error
    #[error("Resource not found: {0}")]
    NotFound(String),

    /// Already exists error
    #[error("Resource already exists: {0}")]
    AlreadyExists(String),

    /// Invalid state error
    #[error("Invalid state: {0}")]
    InvalidState(String),

    /// System error
    #[error("System error: {0}")]
    System(String),
}

impl AndonError {
    /// Create a new configuration error
    pub fn config(msg: impl Into<String>) -> Self {
        Self::Configuration(msg.into())
    }

    /// Create a new logger error
    pub fn logger(msg: impl Into<String>) -> Self {
        Self::Logger(msg.into())
    }

    /// Create a new metrics error
    pub fn metrics(msg: impl Into<String>) -> Self {
        Self::Metrics(msg.into())
    }

    /// Create a new tracer error
    pub fn tracer(msg: impl Into<String>) -> Self {
        Self::Tracer(msg.into())
    }

    /// Create a new observer error
    pub fn observer(msg: impl Into<String>) -> Self {
        Self::Observer(msg.into())
    }

    /// Create a new alert error
    pub fn alert(msg: impl Into<String>) -> Self {
        Self::Alert(msg.into())
    }

    /// Create a new HTTP error
    pub fn http(msg: impl Into<String>) -> Self {
        Self::HttpError(msg.into())
    }

    /// Create a new not found error
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound(msg.into())
    }

    /// Create a new already exists error
    pub fn already_exists(msg: impl Into<String>) -> Self {
        Self::AlreadyExists(msg.into())
    }

    /// Create a new invalid state error
    pub fn invalid_state(msg: impl Into<String>) -> Self {
        Self::InvalidState(msg.into())
    }

    /// Create a new system error
    pub fn system(msg: impl Into<String>) -> Self {
        Self::System(msg.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = AndonError::config("test config error");
        assert!(err.to_string().contains("Configuration error"));

        let err = AndonError::logger("test logger error");
        assert!(err.to_string().contains("Logger error"));
    }
}
