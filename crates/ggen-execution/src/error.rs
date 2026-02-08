// Comprehensive error handling for the execution framework
use thiserror::Error;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// ERROR HIERARCHY
// ============================================================================

/// Unified error hierarchy for all execution components
#[derive(Debug, Error)]
pub enum ExecutionError {
    // Configuration errors
    #[error("Configuration error: {0}")]
    Configuration(String),

    // Agent-related errors
    #[error("Agent error: {0}")]
    Agent(String),

    // Task-related errors
    #[error("Task error: {0}")]
    Task(String),

    // Workflow-related errors
    #[error("Workflow error: {0}")]
    Workflow(String),

    // Pipeline-related errors
    #[error("Pipeline error: {0}")]
    Pipeline(String),

    // Message-related errors
    #[error("Message error: {0}")]
    Message(String),

    // Communication errors
    #[error("Communication error: {0}")]
    Communication(String),

    // Resource errors
    #[error("Resource error: {0}")]
    Resource(String),

    // Convergence errors
    #[error("Convergence error: {0}")]
    Convergence(String),

    // Recovery errors
    #[error("Recovery error: {0}")]
    Recovery(String),

    // Timeout errors
    #[error("Timeout error: {0}")]
    Timeout(String),

    // Validation errors
    #[error("Validation error: {0}")]
    Validation(String),

    // Serialization/deserialization errors
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    // I/O errors
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    // UUID errors
    #[error("UUID error: {0}")]
    Uuid(#[from] uuid::Error),

    // Async runtime errors
    #[error("Async error: {0}")]
    Async(String),

    // Error codes for programmatic handling
    #[error("Error code: {code}, message: {message}")]
    Coded { code: ErrorCode, message: String },
}

/// Error codes for programmatic handling
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorCode {
    // Configuration errors (1000-1999)
    ConfigurationInvalid = 1000,
    ConfigurationMissing = 1001,
    ConfigurationValidationFailed = 1002,

    // Agent errors (2000-2999)
    AgentNotFound = 2000,
    AgentNotReady = 2001,
    AgentCapacityExceeded = 2002,
    AgentCommunicationFailed = 2003,

    // Task errors (3000-3999)
    TaskNotFound = 3000,
    TaskAlreadyRunning = 3001,
    TaskDependenciesNotMet = 3002,
    TaskExecutionFailed = 3003,
    TaskTimeout = 3004,
    TaskCancelled = 3005,

    // Workflow errors (4000-4999)
    WorkflowNotFound = 4000,
    WorkflowInvalid = 4001,
    WorkflowTimeout = 4002,
    WorkflowCancelled = 4003,

    // Pipeline errors (5000-5999)
    PipelineNotFound = 5000,
    PipelineInvalid = 5001,
    PipelineStageFailed = 5002,
    PipelineTimeout = 5003,

    // Message errors (6000-6999)
    MessageNotFound = 6000,
    MessageCorrupt = 6001,
    MessageRoutingFailed = 6002,
    MessageTimeout = 6003,

    // Communication errors (7000-7999)
    CommunicationFailed = 7000,
    NetworkError = 7001,
    ProtocolError = 7002,
    AuthenticationFailed = 7003,

    // Resource errors (8000-8999)
    ResourceLimitExceeded = 8000,
    ResourceNotFound = 8001,
    ResourceNotAvailable = 8002,

    // Convergence errors (9000-9999)
    ConvergenceFailed = 9000,
    ConvergenceTimeout = 9001,
    ConvergenceQualityThresholdNotMet = 9002,

    // Recovery errors (10000-10999)
    RecoveryFailed = 10000,
    RecoveryTimeout = 10001,
    RecoveryStrategyNotFound = 10002,

    // Validation errors (12000-12999)
    ValidationError = 12000,
    ValidationRuleFailed = 12001,
    SchemaValidationFailed = 12002,
}

// ============================================================================
// ERROR EXTENSIONS
// ============================================================================

/// Extensions for error handling with context
pub trait ErrorContext {
    fn with_context(self, context: &str) -> Self;
    fn with_metadata(self, key: &str, value: serde_json::Value) -> Self;
}

impl ErrorContext for ExecutionError {
    fn with_context(mut self, context: &str) -> Self {
        match self {
            ExecutionError::Coded { code, mut message } => {
                message = format!("{}: {}", context, message);
                ExecutionError::Coded { code, message }
            }
            ExecutionError::Configuration(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Agent(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Task(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Workflow(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Pipeline(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Message(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Communication(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Resource(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Convergence(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Recovery(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Timeout(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            ExecutionError::Validation(ref mut msg) => {
                *msg = format!("{}: {}", context, msg);
                self
            }
            // Other error types remain unchanged
            _ => self,
        }
    }

    fn with_metadata(self, _key: &str, value: serde_json::Value) -> Self {
        // Store metadata for future use
        match self {
            ExecutionError::Coded { code, mut message } => {
                message = format!("{} (metadata: {})", message, value);
                ExecutionError::Coded { code, message }
            }
            _ => self,
        }
    }
}

// ============================================================================
// ERROR RECOVERY
// ============================================================================

/// Recovery strategy for handling errors
pub trait ErrorRecovery {
    fn can_recover(&self, error: &ExecutionError) -> bool;
    fn recover(&mut self, error: &ExecutionError) -> Result<(), ExecutionError>;
}

/// Basic recovery implementation
pub struct BasicRecovery;

impl ErrorRecovery for BasicRecovery {
    fn can_recover(&self, error: &ExecutionError) -> bool {
        matches!(error,
            ExecutionError::TaskExecutionFailed(_)
            | ExecutionError::CommunicationFailed(_)
            | ExecutionError::ResourceLimitExceeded(_)
        )
    }

    fn recover(&mut self, error: &ExecutionError) -> Result<(), ExecutionError> {
        match error {
            ExecutionError::TaskExecutionFailed(_) => {
                // Implement retry logic
                println!("Retrying failed task...");
                Ok(())
            }
            ExecutionError::CommunicationFailed(_) => {
                // Implement reconnection logic
                println!("Reconnecting...");
                Ok(())
            }
            ExecutionError::ResourceLimitExceeded(_) => {
                // Implement scaling logic
                println!("Scaling resources...");
                Ok(())
            }
            _ => Err(ExecutionError::RecoveryFailed(
                "Recovery not supported for this error type".to_string(),
            )),
        }
    }
}

// ============================================================================
// ERROR METADATA
// ============================================================================

/// Error metadata for debugging and monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorMetadata {
    pub error_type: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub component: String,
    pub operation: String,
    pub severity: ErrorSeverity,
    pub retry_count: u32,
    pub stack_trace: Option<String>,
    pub additional_context: HashMap<String, serde_json::Value>,
}

/// Error severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorSeverity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Error handler for tracking and managing errors
pub struct ErrorHandler {
    errors: Vec<ErrorMetadata>,
    max_errors: usize,
}

impl ErrorHandler {
    pub fn new(max_errors: usize) -> Self {
        Self {
            errors: Vec::new(),
            max_errors,
        }
    }

    pub fn log_error(&mut self, error: &ExecutionError, component: &str, operation: &str) {
        let metadata = ErrorMetadata {
            error_type: error.to_string(),
            timestamp: chrono::Utc::now(),
            component: component.to_string(),
            operation: operation.to_string(),
            severity: ErrorSeverity::Error,
            retry_count: 0,
            stack_trace: None,
            additional_context: HashMap::new(),
        };

        self.errors.push(metadata);
        if self.errors.len() > self.max_errors {
            self.errors.remove(0);
        }
    }

    pub fn get_errors(&self) -> &[ErrorMetadata] {
        &self.errors
    }

    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
}

// ============================================================================
// CONVERSION HELPERS
// ============================================================================

impl From<String> for ExecutionError {
    fn from(s: String) -> Self {
        ExecutionError::Configuration(s)
    }
}

impl From<&str> for ExecutionError {
    fn from(s: &str) -> Self {
        ExecutionError::Configuration(s.to_string())
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = ExecutionError::Configuration("Invalid config".to_string());
        assert!(matches!(error, ExecutionError::Configuration(_)));
    }

    #[test]
    fn test_error_context() {
        let error = ExecutionError::Configuration("Invalid config".to_string());
        let contextual_error = error.with_context("Database connection");
        assert!(matches!(contextual_error, ExecutionError::Configuration(_)));
    }

    #[test]
    fn test_error_metadata() {
        let error = ExecutionError::Configuration("Test error".to_string());
        let mut handler = ErrorHandler::new(100);
        handler.log_error(&error, "test-component", "test-operation");

        assert_eq!(handler.errors.len(), 1);
        assert_eq!(handler.errors[0].component, "test-component");
    }

    #[test]
    fn test_basic_recovery() {
        let mut recovery = BasicRecovery;
        let error = ExecutionError::TaskExecutionFailed("Task failed".to_string());

        assert!(recovery.can_recover(&error));
        let result = recovery.recover(&error);
        assert!(result.is_ok());
    }
}