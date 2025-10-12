//! Fixed error types for cleanroom testing framework
//!
//! This module provides comprehensive error handling following core team best practices:
//! - Structured error types with context
//! - Error chaining and propagation
//! - User-friendly error messages
//! - Debug information for troubleshooting

use std::fmt;
use std::error::Error as StdError;
use std::str::FromStr;
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
    /// Policy error
    PolicyError,
    /// Resource error
    ResourceError,
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
            ErrorKind::PolicyError => write!(f, "PolicyError"),
            ErrorKind::ResourceError => write!(f, "ResourceError"),
        }
    }
}

impl FromStr for ErrorKind {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "ContainerError" => Ok(ErrorKind::ContainerError),
            "NetworkError" => Ok(ErrorKind::NetworkError),
            "ResourceLimitExceeded" => Ok(ErrorKind::ResourceLimitExceeded),
            "Timeout" => Ok(ErrorKind::Timeout),
            "ConfigurationError" => Ok(ErrorKind::ConfigurationError),
            "PolicyViolation" => Ok(ErrorKind::PolicyViolation),
            "DeterministicError" => Ok(ErrorKind::DeterministicError),
            "CoverageError" => Ok(ErrorKind::CoverageError),
            "SnapshotError" => Ok(ErrorKind::SnapshotError),
            "TracingError" => Ok(ErrorKind::TracingError),
            "RedactionError" => Ok(ErrorKind::RedactionError),
            "ReportError" => Ok(ErrorKind::ReportError),
            "IoError" => Ok(ErrorKind::IoError),
            "SerializationError" => Ok(ErrorKind::SerializationError),
            "ValidationError" => Ok(ErrorKind::ValidationError),
            "InternalError" => Ok(ErrorKind::InternalError),
            "PolicyError" => Ok(ErrorKind::PolicyError),
            "ResourceError" => Ok(ErrorKind::ResourceError),
            _ => Err("Invalid error kind"),
        }
    }
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
    
    /// Chain an error with additional context
    pub fn chain_error(error: CleanroomError, context: impl Into<String>) -> Self {
        Self {
            kind: error.kind,
            message: format!("{}: {}", context.into(), error.message),
            context: error.context,
            source: Some(error.message),
            timestamp: chrono::Utc::now(),
        }
    }
    
    /// Get the error kind
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
    
    /// Get the error message
    pub fn message(&self) -> &str {
        &self.message
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
    pub fn timeout_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Timeout, message)
    }
    
    /// Create a configuration error
    pub fn configuration_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ConfigurationError, message)
    }
    
    /// Create a policy violation error
    pub fn policy_violation_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::PolicyViolation, message)
    }
    
    /// Create a deterministic execution error
    pub fn deterministic_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::DeterministicError, message)
    }
    
    /// Create a coverage tracking error
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
    
    /// Create a report generation error
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
    
    /// Create a policy error
    pub fn policy_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::PolicyError, message)
    }
    
    /// Create a resource error
    pub fn resource_error(message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ResourceError, message)
    }
}

impl fmt::Display for CleanroomError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.message)?;
        if let Some(context) = &self.context {
            write!(f, " (Context: {})", context)?;
        }
        if let Some(source) = &self.source {
            write!(f, " (Source: {})", source)?;
        }
        Ok(())
    }
}

impl StdError for CleanroomError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        // We store source as String, so we can't return it as a trait object directly
        None
    }
}

// Implement From for common error types to convert them to CleanroomError
impl From<std::io::Error> for CleanroomError {
    fn from(err: std::io::Error) -> Self {
        CleanroomError::io_error(err.to_string())
    }
}

impl From<serde_json::Error> for CleanroomError {
    fn from(err: serde_json::Error) -> Self {
        CleanroomError::serialization_error(err.to_string())
    }
}

impl From<testcontainers::TestcontainersError> for CleanroomError {
    fn from(err: testcontainers::TestcontainersError) -> Self {
        CleanroomError::container_error(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = CleanroomError::new(ErrorKind::ConfigurationError, "test message");
        assert_eq!(error.message, "test message");
    }

    #[test]
    fn test_error_with_source() {
        let error = CleanroomError::new(ErrorKind::ContainerError, "test message")
            .with_source("test source");
        assert_eq!(error.message, "test message");
        assert_eq!(error.source, Some("test source".to_string()));
    }

    #[test]
    fn test_error_display() {
        let error = CleanroomError::new(ErrorKind::Timeout, "test message");
        let display = format!("{}", error);
        assert!(display.contains("Timeout"));
        assert!(display.contains("test message"));
    }

    #[test]
    fn test_error_from_io() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
        let error: CleanroomError = io_error.into();
        assert!(matches!(error.kind, ErrorKind::IoError));
    }

    #[test]
    fn test_error_from_json() {
        let json_error = serde_json::from_str::<serde_json::Value>("invalid json");
        let error: CleanroomError = json_error.unwrap_err().into();
        assert!(matches!(error.kind, ErrorKind::SerializationError));
    }

    #[test]
    fn test_helper_functions() {
        let container_error = CleanroomError::container_error("container failed");
        assert!(matches!(container_error.kind, ErrorKind::ContainerError));

        let network_error = CleanroomError::network_error("network failed");
        assert!(matches!(network_error.kind, ErrorKind::NetworkError));

        let timeout_error = CleanroomError::timeout_error("timeout occurred");
        assert!(matches!(timeout_error.kind, ErrorKind::Timeout));
    }

    #[test]
    fn test_error_kind_from_str() {
        assert_eq!(ErrorKind::from_str("ValidationError"), Ok(ErrorKind::ValidationError));
        assert_eq!(ErrorKind::from_str("IoError"), Ok(ErrorKind::IoError));
        assert_eq!(ErrorKind::from_str("ContainerError"), Ok(ErrorKind::ContainerError));
        assert_eq!(ErrorKind::from_str("InvalidError"), Err("Invalid error kind"));
    }

    #[test]
    fn test_error_kind_display() {
        assert_eq!(format!("{}", ErrorKind::ValidationError), "ValidationError");
        assert_eq!(format!("{}", ErrorKind::IoError), "IoError");
        assert_eq!(format!("{}", ErrorKind::ContainerError), "ContainerError");
    }

    #[test]
    fn test_error_chaining() {
        let original_error = CleanroomError::validation_error("Original error");
        let chained_error = CleanroomError::chain_error(original_error, "Additional context");
        assert!(chained_error.message().contains("Additional context"));
        assert!(chained_error.message().contains("Original error"));
    }

    #[test]
    fn test_error_kind_and_message_accessors() {
        let error = CleanroomError::new(ErrorKind::ValidationError, "Test error message");
        assert_eq!(error.kind(), &ErrorKind::ValidationError);
        assert_eq!(error.message(), "Test error message");
    }
}
