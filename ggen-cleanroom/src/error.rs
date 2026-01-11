//! Error types for cleanroom testing framework

/// Result type for cleanroom operations
pub type Result<T> = std::result::Result<T, CleanroomError>;

/// Cleanroom testing errors
#[derive(Debug, thiserror::Error)]
pub enum CleanroomError {
    /// I/O error during cleanroom setup or teardown
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// Test validation failed
    #[error("Validation failed: {0}")]
    ValidationFailed(String),

    /// Test execution failed
    #[error("Test execution failed: {0}")]
    ExecutionFailed(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Timeout error
    #[error("Test timeout after {0}ms")]
    Timeout(u64),

    /// Assertion failed
    #[error("Assertion failed: {0}")]
    AssertionFailed(String),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

impl CleanroomError {
    /// Create a validation error
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::ValidationFailed(msg.into())
    }

    /// Create an execution error
    pub fn execution(msg: impl Into<String>) -> Self {
        Self::ExecutionFailed(msg.into())
    }

    /// Create a configuration error
    pub fn config(msg: impl Into<String>) -> Self {
        Self::ConfigError(msg.into())
    }

    /// Create a timeout error
    pub fn timeout(duration_ms: u64) -> Self {
        Self::Timeout(duration_ms)
    }

    /// Create an assertion error
    pub fn assertion(msg: impl Into<String>) -> Self {
        Self::AssertionFailed(msg.into())
    }
}
