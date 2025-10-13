//! Error construction helpers for reducing repetition
//!
//! This module provides helper functions and macros to eliminate repetitive
//! error construction patterns across the codebase.

use crate::error::{CleanroomError, ErrorKind};
use chrono::Utc;

/// Helper function to create container-related errors
///
/// This function eliminates repetitive error construction for container operations.
///
/// # Example
///
/// ```rust
/// return Err(container_error("Failed to start PostgreSQL container", "Container startup timeout"));
/// ```
pub fn container_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::ContainerError,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create network-related errors
pub fn network_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::NetworkError,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create timeout errors
pub fn timeout_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::Timeout,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create configuration errors
pub fn config_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::ConfigurationError,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create policy violation errors
pub fn policy_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::PolicyViolation,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create resource limit errors
pub fn resource_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::ResourceLimitExceeded,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Helper function to create internal errors
pub fn internal_error(message: &str, context: Option<&str>) -> CleanroomError {
    CleanroomError {
        kind: ErrorKind::InternalError,
        message: message.to_string(),
        context: context.map(|s| s.to_string()),
        source: None,
        timestamp: Utc::now(),
    }
}

/// Macro to create errors with automatic context
///
/// This macro eliminates repetitive error construction by automatically
/// adding function context and timestamps.
///
/// # Example
///
/// ```rust
/// return Err(cleanroom_err!(ContainerError, "Failed to start container"));
/// ```
#[macro_export]
macro_rules! cleanroom_err {
    ($kind:ident, $msg:expr) => {
        CleanroomError {
            kind: ErrorKind::$kind,
            message: $msg.to_string(),
            context: Some(format!("{}:{}", file!(), line!())),
            source: None,
            timestamp: chrono::Utc::now(),
        }
    };
    ($kind:ident, $msg:expr, $ctx:expr) => {
        CleanroomError {
            kind: ErrorKind::$kind,
            message: $msg.to_string(),
            context: Some($ctx.to_string()),
            source: None,
            timestamp: chrono::Utc::now(),
        }
    };
}

/// Macro to create errors with source error
///
/// This macro creates errors that wrap another error as the source.
///
/// # Example
///
/// ```rust
/// return Err(cleanroom_err_with_source!(ContainerError, "Failed to start container", io_error));
/// ```
#[macro_export]
macro_rules! cleanroom_err_with_source {
    ($kind:ident, $msg:expr, $source:expr) => {
        CleanroomError {
            kind: ErrorKind::$kind,
            message: $msg.to_string(),
            context: Some(format!("{}:{}", file!(), line!())),
            source: Some($source.to_string()),
            timestamp: chrono::Utc::now(),
        }
    };
    ($kind:ident, $msg:expr, $ctx:expr, $source:expr) => {
        CleanroomError {
            kind: ErrorKind::$kind,
            message: $msg.to_string(),
            context: Some($ctx.to_string()),
            source: Some($source.to_string()),
            timestamp: chrono::Utc::now(),
        }
    };
}

/// Convenience functions for common error patterns
pub mod common {
    use super::*;

    /// Create error for container startup failure
    pub fn container_startup_failed(container_name: &str) -> CleanroomError {
        container_error(
            &format!("Failed to start {} container", container_name),
            Some("Container startup timeout or failure"),
        )
    }

    /// Create error for container not found
    pub fn container_not_found(container_name: &str) -> CleanroomError {
        container_error(
            &format!("Container '{}' not found", container_name),
            Some("Container may have been stopped or never created"),
        )
    }

    /// Create error for network connection failure
    pub fn network_connection_failed(host: &str, port: u16) -> CleanroomError {
        network_error(
            &format!("Failed to connect to {}:{}", host, port),
            Some("Network connection timeout or refused"),
        )
    }

    /// Create error for configuration validation failure
    pub fn config_validation_failed(field: &str, reason: &str) -> CleanroomError {
        config_error(
            &format!("Configuration validation failed for '{}'", field),
            Some(reason),
        )
    }

    /// Create error for policy violation
    pub fn policy_violation(violation: &str) -> CleanroomError {
        policy_error(
            &format!("Policy violation: {}", violation),
            Some("Security policy enforcement"),
        )
    }

    /// Create error for resource limit exceeded
    pub fn resource_limit_exceeded(resource: &str, limit: &str) -> CleanroomError {
        resource_error(
            &format!("Resource limit exceeded for {}: {}", resource, limit),
            Some("Resource monitoring and limits"),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_helpers() {
        let err = container_error("Test error", Some("Test context"));
        assert_eq!(err.kind, ErrorKind::ContainerError);
        assert_eq!(err.message, "Test error");
        assert_eq!(err.context, Some("Test context".to_string()));
    }

    #[test]
    fn test_error_macros() {
        let err = cleanroom_err!(ContainerError, "Test error");
        assert_eq!(err.kind, ErrorKind::ContainerError);
        assert_eq!(err.message, "Test error");
        assert!(err.context.is_some());
    }

    #[test]
    fn test_common_errors() {
        let err = common::container_startup_failed("postgres");
        assert_eq!(err.kind, ErrorKind::ContainerError);
        assert!(err.message.contains("postgres"));
    }
}
