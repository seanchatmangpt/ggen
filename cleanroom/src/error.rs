//! Error types for cleanroom testing framework
//!
//! This module provides comprehensive error handling following core team best practices:
//! - Structured error types with context
//! - Error chaining and propagation
//! - User-friendly error messages
//! - Debug information for troubleshooting

use std::fmt;
use std::error::Error as StdError;
use serde::{Deserialize, Serialize};

/// Result type alias for cleanroom operations
pub type Result<T> = std::result::Result<T, CleanroomError>;

/// Comprehensive error type for cleanroom operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CleanroomError {
    /// Error kind
    pub kind: ErrorKind,
    /// Error message
    pub message: String,
    /// Additional context
    pub context: Option<String>,
    /// Source error (if any)
    pub source: Option<String>,
    /// Timestamp when error occurred
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Error kinds for different failure scenarios
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ErrorKind {
    /// Container-related errors
    ContainerError,
    /// Network-related errors
    NetworkError,
    /// Resource limit exceeded
    ResourceLimitExceeded,
    /// Timeout errors
    Timeout,
    /// Configuration errors
    ConfigurationError,
    /// Policy violation
    PolicyViolation,
    /// Deterministic execution error
    DeterministicError,
    /// Coverage tracking error
    CoverageError,
    /// Snapshot error
    SnapshotError,
    /// Tracing error
    TracingError,
    /// Redaction error
    RedactionError,
    /// Report generation error
    ReportError,
    /// IO error
    IoError,
    /// Serialization error
    SerializationError,
    /// Validation error
    ValidationError,
    /// Internal error
    InternalError,
}

impl CleanroomError {
    /// Create a new cleanroom error
    pub fn new(kind: ErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            context: None,
            source: None,
            timestamp: chrono::Utc::now(),
        }
    }
    
    /// Create a new cleanroom error with context
    pub fn with_context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }
    
    /// Create a new cleanroom error with source
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }
    
    /// Create a container error
    pub fn container_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ContainerError, message)
    }
    
    /// Create a network error
    pub fn network_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::NetworkError, message)
    }
    
    /// Create a resource limit exceeded error
    pub fn resource_limit_exceeded(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ResourceLimitExceeded, message)
    }
    
    /// Create a timeout error
    pub fn timeout(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Timeout, message)
    }
    
    /// Create a configuration error
    pub fn configuration_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ConfigurationError, message)
    }
    
    /// Create a policy violation error
    pub fn policy_violation(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::PolicyViolation, message)
    }
    
    /// Create a deterministic error
    pub fn deterministic_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::DeterministicError, message)
    }
    
    /// Create a coverage error
    pub fn coverage_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::CoverageError, message)
    }
    
    /// Create a snapshot error
    pub fn snapshot_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::SnapshotError, message)
    }
    
    /// Create a tracing error
    pub fn tracing_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::TracingError, message)
    }
    
    /// Create a redaction error
    pub fn redaction_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::RedactionError, message)
    }
    
    /// Create a report error
    pub fn report_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ReportError, message)
    }
    
    /// Create an IO error
    pub fn io_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::IoError, message)
    }
    
    /// Create a serialization error
    pub fn serialization_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::SerializationError, message)
    }
    
    /// Create a validation error
    pub fn validation_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ValidationError, message)
    }
    
    /// Create an internal error
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::InternalError, message)
    }
}

impl fmt::Display for CleanroomError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}: {}", self.kind, self.message, self.timestamp)?;
        
        if let Some(ref context) = self.context {
            write!(f, " (Context: {})", context)?;
        }
        
        if let Some(ref source) = self.source {
            write!(f, " (Source: {})", source)?;
        }
        
        Ok(())
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::ContainerError => write!(f, "ContainerError"),
            ErrorKind::NetworkError => write!(f, "NetworkError"),
            ErrorKind::ResourceLimitExceeded => write!(f, "ResourceLimitExceeded"),
            ErrorKind::Timeout => write!(f, "Timeout"),
            ErrorKind::ConfigurationError => write!(f, "ConfigurationError"),
            ErrorKind::PolicyViolation => write!(f, "PolicyViolation"),
            ErrorKind::DeterministicError => write!(f, "DeterministicError"),
            ErrorKind::CoverageError => write!(f, "CoverageError"),
            ErrorKind::SnapshotError => write!(f, "SnapshotError"),
            ErrorKind::TracingError => write!(f, "TracingError"),
            ErrorKind::RedactionError => write!(f, "RedactionError"),
            ErrorKind::ReportError => write!(f, "ReportError"),
            ErrorKind::IoError => write!(f, "IoError"),
            ErrorKind::SerializationError => write!(f, "SerializationError"),
            ErrorKind::ValidationError => write!(f, "ValidationError"),
            ErrorKind::InternalError => write!(f, "InternalError"),
        }
    }
}

impl StdError for CleanroomError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        None
    }
}

// Implement From traits for common error types
impl From<std::io::Error> for CleanroomError {
    fn from(err: std::io::Error) -> Self {
        Self::io_error(err.to_string())
    }
}

impl From<serde_json::Error> for CleanroomError {
    fn from(err: serde_json::Error) -> Self {
        Self::serialization_error(err.to_string())
    }
}

impl From<toml::de::Error> for CleanroomError {
    fn from(err: toml::de::Error) -> Self {
        Self::serialization_error(err.to_string())
    }
}

impl From<toml::ser::Error> for CleanroomError {
    fn from(err: toml::ser::Error) -> Self {
        Self::serialization_error(err.to_string())
    }
}

impl From<uuid::Error> for CleanroomError {
    fn from(err: uuid::Error) -> Self {
        Self::validation_error(err.to_string())
    }
}

impl From<chrono::ParseError> for CleanroomError {
    fn from(err: chrono::ParseError) -> Self {
        Self::validation_error(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_error_creation() {
        let error = CleanroomError::container_error("Test error");
        assert_eq!(error.kind, ErrorKind::ContainerError);
        assert_eq!(error.message, "Test error");
    }
    
    #[test]
    fn test_error_with_context() {
        let error = CleanroomError::container_error("Test error")
            .with_context("Additional context");
        assert_eq!(error.context, Some("Additional context".to_string()));
    }
    
    #[test]
    fn test_error_display() {
        let error = CleanroomError::container_error("Test error");
        let display = format!("{}", error);
        assert!(display.contains("ContainerError"));
        assert!(display.contains("Test error"));
    }
    
    #[test]
    fn test_error_from_io() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
        let cleanroom_error: CleanroomError = io_error.into();
        assert_eq!(cleanroom_error.kind, ErrorKind::IoError);
    }
}