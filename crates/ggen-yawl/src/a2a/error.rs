//! Error types for YAWL to A2A integration.
//!
//! This module defines the error types used throughout the YAWL to A2A
//! integration layer. All operations return [`IntegrationResult<T>`]
//! where [`A2AIntegrationError`] represents the various failure modes.

use crate::Result as YawlResult;
use a2a_generated::task::TaskError;
use std::collections::HashMap;

/// Result type for YAWL to A2A integration operations.
///
/// This type alias is used throughout the integration layer for operations
/// that can fail. It wraps a successful value `T` or an [`A2AIntegrationError`].
pub type IntegrationResult<T> = std::result::Result<T, A2AIntegrationError>;

/// Errors that can occur during YAWL to A2A integration.
///
/// This enum represents all possible error conditions that can occur
/// during workflow conversion, execution, and state synchronization.
#[derive(Debug, thiserror::Error)]
pub enum A2AIntegrationError {
    /// Error converting YAWL workflow to A2A tasks.
    #[error("Workflow conversion error: {message}")]
    Conversion { message: String },

    /// Error during workflow execution.
    #[error("Workflow execution error: {message}")]
    Execution { message: String },

    /// Error during task state synchronization.
    #[error("State synchronization error: {message}")]
    StateSync { message: String },

    /// Error during gateway (split/join) execution.
    #[error("Gateway execution error: {gateway_type} gateway failed: {message}")]
    Gateway {
        gateway_type: String,
        message: String,
    },

    /// Error when task dependencies cannot be resolved.
    #[error("Dependency resolution error: task '{task_id}' depends on '{missing_id}' which does not exist")]
    DependencyResolution {
        task_id: String,
        missing_id: String,
    },

    /// Error when workflow contains circular dependencies.
    #[error("Circular dependency detected in workflow: {cycle}")]
    CircularDependency { cycle: String },

    /// Error when task timeout is exceeded.
    #[error("Task timeout: task '{task_id}' exceeded timeout of {timeout_secs}s")]
    TaskTimeout {
        task_id: String,
        timeout_secs: u64,
    },

    /// Error when composite task decomposition fails.
    #[error("Composite task decomposition failed for '{task_id}': {message}")]
    Decomposition { task_id: String, message: String },

    /// Error when multiple instance task execution fails.
    #[error("Multiple instance task '{task_id}' failed: {message}")]
    MultipleInstance { task_id: String, message: String },

    /// Error when workflow persistence fails.
    #[error("Workflow persistence error: {message}")]
    Persistence { message: String },

    /// Error when workflow recovery fails.
    #[error("Workflow recovery error: {message}")]
    Recovery { message: String },

    /// Error from underlying A2A task execution.
    #[error("A2A task error: {0}")]
    A2ATask(#[from] TaskError),

    /// Error from underlying YAWL generation.
    #[error("YAWL error: {0}")]
    Yawl(String),

    /// IO error during file operations.
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// JSON serialization/deserialization error.
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// Generic error with context.
    #[error("{0}")]
    Other(String),
}

impl A2AIntegrationError {
    /// Create a conversion error with context.
    pub fn conversion(message: impl Into<String>) -> Self {
        Self::Conversion {
            message: message.into(),
        }
    }

    /// Create an execution error with context.
    pub fn execution(message: impl Into<String>) -> Self {
        Self::Execution {
            message: message.into(),
        }
    }

    /// Create a state sync error with context.
    pub fn state_sync(message: impl Into<String>) -> Self {
        Self::StateSync {
            message: message.into(),
        }
    }

    /// Create a gateway error with context.
    pub fn gateway(gateway_type: impl Into<String>, message: impl Into<String>) -> Self {
        Self::Gateway {
            gateway_type: gateway_type.into(),
            message: message.into(),
        }
    }

    /// Create a dependency resolution error.
    pub fn dependency_resolution(task_id: impl Into<String>, missing_id: impl Into<String>) -> Self {
        Self::DependencyResolution {
            task_id: task_id.into(),
            missing_id: missing_id.into(),
        }
    }

    /// Create a circular dependency error.
    pub fn circular_dependency(cycle: impl Into<String>) -> Self {
        Self::CircularDependency {
            cycle: cycle.into(),
        }
    }

    /// Create a task timeout error.
    pub fn task_timeout(task_id: impl Into<String>, timeout_secs: u64) -> Self {
        Self::TaskTimeout {
            task_id: task_id.into(),
            timeout_secs,
        }
    }

    /// Create a decomposition error.
    pub fn decomposition(task_id: impl Into<String>, message: impl Into<String>) -> Self {
        Self::Decomposition {
            task_id: task_id.into(),
            message: message.into(),
        }
    }

    /// Create a multiple instance error.
    pub fn multiple_instance(task_id: impl Into<String>, message: impl Into<String>) -> Self {
        Self::MultipleInstance {
            task_id: task_id.into(),
            message: message.into(),
        }
    }

    /// Create a persistence error.
    pub fn persistence(message: impl Into<String>) -> Self {
        Self::Persistence {
            message: message.into(),
        }
    }

    /// Create a recovery error.
    pub fn recovery(message: impl Into<String>) -> Self {
        Self::Recovery {
            message: message.into(),
        }
    }

    /// Create a YAWL error from a crate::Error.
    pub fn from_yawl_err(err: &crate::Error) -> Self {
        Self::Yawl(err.to_string())
    }

    /// Check if this error is retryable.
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            Self::TaskTimeout { .. }
                | Self::Execution { .. }
                | Self::Persistence { .. }
                | Self::Io(_)
        )
    }

    /// Get error severity for logging/monitoring.
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            Self::TaskTimeout { .. } => ErrorSeverity::Warning,
            Self::CircularDependency { .. } | Self::DependencyResolution { .. } => {
                ErrorSeverity::Critical
            }
            Self::Conversion { .. } | Self::Execution { .. } | Self::StateSync { .. } => {
                ErrorSeverity::Error
            }
            Self::Gateway { .. } | Self::Decomposition { .. } | Self::MultipleInstance { .. } => {
                ErrorSeverity::Error
            }
            Self::Persistence { .. } | Self::Recovery { .. } => ErrorSeverity::Warning,
            Self::A2ATask(_) => ErrorSeverity::Error,
            Self::Yawl(_) => ErrorSeverity::Error,
            Self::Io(_) => ErrorSeverity::Warning,
            Self::Json(_) => ErrorSeverity::Error,
            Self::Other(_) => ErrorSeverity::Error,
        }
    }

    /// Get contextual metadata for error reporting.
    pub fn context(&self) -> HashMap<String, String> {
        let mut ctx = HashMap::new();
        ctx.insert("error_type".to_string(), self.error_type().to_string());
        ctx.insert("severity".to_string(), self.severity().to_string());
        ctx.insert("retryable".to_string(), self.is_retryable().to_string());

        match self {
            Self::TaskTimeout { task_id, timeout_secs } => {
                ctx.insert("task_id".to_string(), task_id.clone());
                ctx.insert("timeout_secs".to_string(), timeout_secs.to_string());
            }
            Self::DependencyResolution { task_id, missing_id } => {
                ctx.insert("task_id".to_string(), task_id.clone());
                ctx.insert("missing_id".to_string(), missing_id.clone());
            }
            Self::Gateway { gateway_type, .. } => {
                ctx.insert("gateway_type".to_string(), gateway_type.clone());
            }
            _ => {}
        }

        ctx
    }

    /// Get the error type name.
    fn error_type(&self) -> &'static str {
        match self {
            Self::Conversion { .. } => "Conversion",
            Self::Execution { .. } => "Execution",
            Self::StateSync { .. } => "StateSync",
            Self::Gateway { .. } => "Gateway",
            Self::DependencyResolution { .. } => "DependencyResolution",
            Self::CircularDependency { .. } => "CircularDependency",
            Self::TaskTimeout { .. } => "TaskTimeout",
            Self::Decomposition { .. } => "Decomposition",
            Self::MultipleInstance { .. } => "MultipleInstance",
            Self::Persistence { .. } => "Persistence",
            Self::Recovery { .. } => "Recovery",
            Self::A2ATask(_) => "A2ATask",
            Self::Yawl(_) => "Yawl",
            Self::Io(_) => "Io",
            Self::Json(_) => "Json",
            Self::Other(_) => "Other",
        }
    }
}

/// Severity level of errors for logging and monitoring.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorSeverity {
    /// Informational - not really an error
    Info,
    /// Warning - something unexpected but not critical
    Warning,
    /// Error - operation failed but system can continue
    Error,
    /// Critical - system cannot operate properly
    Critical,
}

impl std::fmt::Display for ErrorSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Info => write!(f, "info"),
            Self::Warning => write!(f, "warning"),
            Self::Error => write!(f, "error"),
            Self::Critical => write!(f, "critical"),
        }
    }
}

impl From<crate::Error> for A2AIntegrationError {
    fn from(err: crate::Error) -> Self {
        Self::Yawl(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = A2AIntegrationError::conversion("test message");
        assert_eq!(err.to_string(), "Workflow conversion error: test message");
    }

    #[test]
    fn test_error_severity() {
        assert_eq!(
            A2AIntegrationError::TaskTimeout {
                task_id: "test".to_string(),
                timeout_secs: 30,
            }
            .severity(),
            ErrorSeverity::Warning
        );

        assert_eq!(
            A2AIntegrationError::CircularDependency {
                cycle: "a->b->a".to_string(),
            }
            .severity(),
            ErrorSeverity::Critical
        );
    }

    #[test]
    fn test_error_retryable() {
        assert!(A2AIntegrationError::TaskTimeout {
            task_id: "test".to_string(),
            timeout_secs: 30,
        }
        .is_retryable());

        assert!(!A2AIntegrationError::CircularDependency {
            cycle: "a->b->a".to_string(),
        }
        .is_retryable());
    }

    #[test]
    fn test_error_context() {
        let err = A2AIntegrationError::TaskTimeout {
            task_id: "task-123".to_string(),
            timeout_secs: 60,
        };

        let ctx = err.context();
        assert_eq!(ctx.get("task_id"), Some(&"task-123".to_string()));
        assert_eq!(ctx.get("timeout_secs"), Some(&"60".to_string()));
        assert_eq!(ctx.get("retryable"), Some(&"true".to_string()));
    }
}
