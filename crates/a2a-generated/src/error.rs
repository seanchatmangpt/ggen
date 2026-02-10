//! A2A-RS Integration Error Handling Module
//!
//! This module provides a unified error type for the A2A-RS integration using
//! `thiserror` for ergonomic error handling and automatic conversions from
//! standard error types.

use std::io;

/// Unified error type for A2A-RS operations
///
/// This error type encompasses all possible errors that can occur during
/// agent-to-agent communication, task execution, and message handling.
/// It provides automatic conversions from standard library and third-party
/// error types for seamless integration.
#[derive(Debug, thiserror::Error)]
pub enum AgentError {
    /// Network or transport layer failures
    ///
    /// These errors occur when there are issues with the underlying transport
    /// mechanism (HTTP, WebSocket, TCP, etc.) connecting agents.
    #[error("Transport error: {0}")]
    TransportError(String),

    /// JSON serialization/deserialization failures
    ///
    /// These errors occur when messages or data structures cannot be
    /// serialized to JSON or deserialized from JSON.
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// Task execution failures
    ///
    /// These errors occur during the execution of tasks, including
    /// timeout, dependency failures, and execution errors.
    #[error("Task execution error: {0}")]
    TaskError(String),

    /// Malformed or invalid messages
    ///
    /// These errors occur when received messages do not conform to the
    /// expected format, schema, or protocol.
    #[error("Invalid message: {0}")]
    InvalidMessage(String),

    /// Generic network errors
    ///
    /// These errors cover general network connectivity issues that are not
    /// specific to the transport layer.
    #[error("Network error: {0}")]
    NetworkError(String),

    /// I/O operation errors
    ///
    /// These errors are automatically converted from `std::io::Error` and
    /// represent failures in file operations, stream operations, etc.
    #[error("I/O error: {0}")]
    IoError(#[from] io::Error),

    /// JSON serde errors (auto-converted)
    ///
    /// These errors are automatically converted from `serde_json::Error`
    /// and represent JSON parsing failures.
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// HTTP client errors (auto-converted, requires `http-adapter` feature)
    ///
    /// These errors are automatically converted from `reqwest::Error`
    /// when the `http-adapter` feature is enabled.
    #[cfg(feature = "http-adapter")]
    #[error("HTTP client error: {0}")]
    HttpError(#[from] reqwest::Error),

    /// Port operation errors
    ///
    /// These errors occur during port initialization, connection, or
    /// message send/receive operations.
    #[error("Port error: {0}")]
    PortError(String),

    /// Adapter operation errors
    ///
    /// These errors occur during data conversion between formats
    /// (JSON, XML, etc.) via adapters.
    #[error("Adapter error: {0}")]
    AdapterError(String),

    /// Agent lifecycle errors
    ///
    /// These errors occur during agent initialization, registration,
    /// or shutdown operations.
    #[error("Agent lifecycle error: {0}")]
    AgentLifecycleError(String),

    /// Timeout errors
    ///
    /// These errors occur when an operation exceeds its time limit.
    #[error("Operation timed out: {0}")]
    TimeoutError(String),

    /// Validation errors
    ///
    /// These errors occur when input validation fails.
    #[error("Validation error: {0}")]
    ValidationError(String),

    /// Configuration errors
    ///
    /// These errors occur when the agent or system configuration is invalid.
    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    /// Unknown or uncategorized errors
    ///
    /// This variant serves as a catch-all for errors that don't fit
    /// into other categories.
    #[error("Unknown error: {0}")]
    Unknown(String),
}

impl AgentError {
    /// Create a new transport error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::transport("Connection refused");
    /// ```
    pub fn transport(msg: impl Into<String>) -> Self {
        Self::TransportError(msg.into())
    }

    /// Create a new serialization error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::serialization("Failed to encode message");
    /// ```
    pub fn serialization(msg: impl Into<String>) -> Self {
        Self::SerializationError(msg.into())
    }

    /// Create a new task error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::task("Task execution failed");
    /// ```
    pub fn task(msg: impl Into<String>) -> Self {
        Self::TaskError(msg.into())
    }

    /// Create a new invalid message error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::invalid_message("Missing required field");
    /// ```
    pub fn invalid_message(msg: impl Into<String>) -> Self {
        Self::InvalidMessage(msg.into())
    }

    /// Create a new network error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::network("Host unreachable");
    /// ```
    pub fn network(msg: impl Into<String>) -> Self {
        Self::NetworkError(msg.into())
    }

    /// Create a new port error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::port("Port not connected");
    /// ```
    pub fn port(msg: impl Into<String>) -> Self {
        Self::PortError(msg.into())
    }

    /// Create a new adapter error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::adapter("Unsupported format");
    /// ```
    pub fn adapter(msg: impl Into<String>) -> Self {
        Self::AdapterError(msg.into())
    }

    /// Create a new agent lifecycle error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::agent_lifecycle("Agent not initialized");
    /// ```
    pub fn agent_lifecycle(msg: impl Into<String>) -> Self {
        Self::AgentLifecycleError(msg.into())
    }

    /// Create a new timeout error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::timeout("Operation exceeded deadline");
    /// ```
    pub fn timeout(msg: impl Into<String>) -> Self {
        Self::TimeoutError(msg.into())
    }

    /// Create a new validation error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::validation("Invalid agent ID format");
    /// ```
    pub fn validation(msg: impl Into<String>) -> Self {
        Self::ValidationError(msg.into())
    }

    /// Create a new configuration error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::configuration("Missing required config field");
    /// ```
    pub fn configuration(msg: impl Into<String>) -> Self {
        Self::ConfigurationError(msg.into())
    }

    /// Create a new unknown error with a message
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::unknown("Unexpected error occurred");
    /// ```
    pub fn unknown(msg: impl Into<String>) -> Self {
        Self::Unknown(msg.into())
    }

    /// Returns a human-readable error category for logging/debugging
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::transport("Connection refused");
    /// assert_eq!(error.category(), "transport");
    /// ```
    pub fn category(&self) -> &str {
        match self {
            Self::TransportError(_) => "transport",
            Self::SerializationError(_) => "serialization",
            Self::TaskError(_) => "task",
            Self::InvalidMessage(_) => "message",
            Self::NetworkError(_) => "network",
            Self::IoError(_) => "io",
            Self::JsonError(_) => "json",
            #[cfg(feature = "http-adapter")]
            Self::HttpError(_) => "http",
            Self::PortError(_) => "port",
            Self::AdapterError(_) => "adapter",
            Self::AgentLifecycleError(_) => "lifecycle",
            Self::TimeoutError(_) => "timeout",
            Self::ValidationError(_) => "validation",
            Self::ConfigurationError(_) => "configuration",
            Self::Unknown(_) => "unknown",
        }
    }

    /// Checks if this error is retryable
    ///
    /// Returns `true` for transient errors that may succeed on retry,
    /// such as network timeouts or temporary unavailability.
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::timeout("Request timed out");
    /// assert!(error.is_retryable());
    ///
    /// let error = AgentError::validation("Invalid input");
    /// assert!(!error.is_retryable());
    /// ```
    pub fn is_retryable(&self) -> bool {
        #[cfg(feature = "http-adapter")]
        {
            matches!(
                self,
                Self::TransportError(_)
                    | Self::NetworkError(_)
                    | Self::TimeoutError(_)
                    | Self::IoError(_)
                    | Self::HttpError(_)
            )
        }
        #[cfg(not(feature = "http-adapter"))]
        {
            matches!(
                self,
                Self::TransportError(_)
                    | Self::NetworkError(_)
                    | Self::TimeoutError(_)
                    | Self::IoError(_)
            )
        }
    }

    /// Checks if this error is permanent (not retryable)
    ///
    /// Returns `true` for errors that will not succeed on retry,
    /// such as validation failures or configuration errors.
    ///
    /// # Example
    /// ```
    /// use a2a_generated::error::AgentError;
    ///
    /// let error = AgentError::validation("Invalid input");
    /// assert!(error.is_permanent());
    /// ```
    pub fn is_permanent(&self) -> bool {
        !self.is_retryable()
    }
}

/// Result type alias for A2A operations
///
/// This is a convenience alias for `Result<T, AgentError>` to reduce
/// boilerplate in function signatures.
///
/// # Example
/// ```
/// use a2a_generated::error::{AgentError, A2AResult};
///
/// fn send_message() -> A2AResult<()> {
///     Ok(())
/// }
/// ```
pub type A2AResult<T> = Result<T, AgentError>;

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;
    use std::io::ErrorKind;

    #[test]
    fn test_transport_error_creation() {
        let error = AgentError::transport("Connection refused");
        assert_eq!(error.to_string(), "Transport error: Connection refused");
        assert_eq!(error.category(), "transport");
        assert!(error.is_retryable());
    }

    #[test]
    fn test_serialization_error_creation() {
        let error = AgentError::serialization("Failed to encode");
        assert_eq!(error.to_string(), "Serialization error: Failed to encode");
        assert_eq!(error.category(), "serialization");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_task_error_creation() {
        let error = AgentError::task("Task execution failed");
        assert_eq!(
            error.to_string(),
            "Task execution error: Task execution failed"
        );
        assert_eq!(error.category(), "task");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_invalid_message_creation() {
        let error = AgentError::invalid_message("Missing required field");
        assert_eq!(error.to_string(), "Invalid message: Missing required field");
        assert_eq!(error.category(), "message");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_network_error_creation() {
        let error = AgentError::network("Host unreachable");
        assert_eq!(error.to_string(), "Network error: Host unreachable");
        assert_eq!(error.category(), "network");
        assert!(error.is_retryable());
    }

    #[test]
    fn test_io_error_auto_conversion() {
        let io_err = io::Error::new(ErrorKind::NotFound, "File not found");
        let agent_err: AgentError = io_err.into();

        assert!(matches!(agent_err, AgentError::IoError(_)));
        assert!(agent_err.to_string().contains("File not found"));
        assert!(agent_err.is_retryable());
    }

    #[test]
    fn test_json_error_auto_conversion() {
        let json_err = serde_json::from_str::<serde_json::Value>("invalid json").unwrap_err();
        let agent_err: AgentError = json_err.into();

        assert!(matches!(agent_err, AgentError::JsonError(_)));
        assert!(!agent_err.is_retryable());
    }

    #[test]
    fn test_port_error_creation() {
        let error = AgentError::port("Port not connected");
        assert_eq!(error.to_string(), "Port error: Port not connected");
        assert_eq!(error.category(), "port");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_adapter_error_creation() {
        let error = AgentError::adapter("Unsupported format");
        assert_eq!(error.to_string(), "Adapter error: Unsupported format");
        assert_eq!(error.category(), "adapter");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_agent_lifecycle_error_creation() {
        let error = AgentError::agent_lifecycle("Agent not initialized");
        assert_eq!(
            error.to_string(),
            "Agent lifecycle error: Agent not initialized"
        );
        assert_eq!(error.category(), "lifecycle");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_timeout_error_creation() {
        let error = AgentError::timeout("Operation exceeded deadline");
        assert_eq!(
            error.to_string(),
            "Operation timed out: Operation exceeded deadline"
        );
        assert_eq!(error.category(), "timeout");
        assert!(error.is_retryable());
    }

    #[test]
    fn test_validation_error_creation() {
        let error = AgentError::validation("Invalid agent ID format");
        assert_eq!(
            error.to_string(),
            "Validation error: Invalid agent ID format"
        );
        assert_eq!(error.category(), "validation");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_configuration_error_creation() {
        let error = AgentError::configuration("Missing required config field");
        assert_eq!(
            error.to_string(),
            "Configuration error: Missing required config field"
        );
        assert_eq!(error.category(), "configuration");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_unknown_error_creation() {
        let error = AgentError::unknown("Unexpected error occurred");
        assert_eq!(
            error.to_string(),
            "Unknown error: Unexpected error occurred"
        );
        assert_eq!(error.category(), "unknown");
        assert!(!error.is_retryable());
    }

    #[test]
    fn test_error_source_for_io() {
        use std::error::Error;

        let io_err = io::Error::new(ErrorKind::PermissionDenied, "Access denied");
        let agent_err: AgentError = io_err.into();

        // Source should be the original io::Error
        assert!(agent_err.source().is_some());
        assert!(agent_err
            .source()
            .unwrap()
            .downcast_ref::<io::Error>()
            .is_some());
    }

    #[test]
    fn test_a2a_result_type_alias() {
        fn returns_ok() -> A2AResult<String> {
            Ok("success".to_string())
        }

        fn returns_err() -> A2AResult<String> {
            Err(AgentError::network("Connection failed"))
        }

        assert!(returns_ok().is_ok());
        assert!(returns_err().is_err());
    }

    #[test]
    fn test_retryable_errors() {
        // All of these should be retryable
        assert!(AgentError::transport("test").is_retryable());
        assert!(AgentError::network("test").is_retryable());
        assert!(AgentError::timeout("test").is_retryable());

        // IO errors should be retryable
        let io_err = io::Error::new(ErrorKind::Interrupted, "Interrupted");
        let agent_err: AgentError = io_err.into();
        assert!(agent_err.is_retryable());
    }

    #[test]
    fn test_permanent_errors() {
        // All of these should be permanent
        assert!(AgentError::serialization("test").is_permanent());
        assert!(AgentError::task("test").is_permanent());
        assert!(AgentError::invalid_message("test").is_permanent());
        assert!(AgentError::port("test").is_permanent());
        assert!(AgentError::adapter("test").is_permanent());
        assert!(AgentError::agent_lifecycle("test").is_permanent());
        assert!(AgentError::validation("test").is_permanent());
        assert!(AgentError::configuration("test").is_permanent());

        // JSON errors should be permanent
        let json_err = serde_json::from_str::<serde_json::Value>("bad").unwrap_err();
        let agent_err: AgentError = json_err.into();
        assert!(agent_err.is_permanent());
    }

    #[test]
    fn test_error_display_includes_context() {
        let error = AgentError::TaskError("Database connection lost".to_string());
        let display = format!("{}", error);
        assert!(display.contains("Database connection lost"));
        assert!(display.contains("Task execution error"));
    }

    #[test]
    fn test_error_debug_formatting() {
        let error = AgentError::InvalidMessage("Missing header".to_string());
        let debug = format!("{:?}", error);
        assert!(debug.contains("InvalidMessage"));
        assert!(debug.contains("Missing header"));
    }

    #[cfg(feature = "http-adapter")]
    #[tokio::test]
    async fn test_http_error_feature() {
        // This test only compiles/runs when http-adapter feature is enabled
        // In a real scenario, this would test reqwest error conversion
        // For now, we just verify the variant exists
        let error = AgentError::HttpError(reqwest::Error::from(reqwest::ErrorKind::Request));
        assert!(matches!(error, AgentError::HttpError(_)));
        assert!(error.is_retryable());
    }

    #[test]
    fn test_error_categories() {
        let test_cases = vec![
            (AgentError::transport("test"), "transport"),
            (AgentError::serialization("test"), "serialization"),
            (AgentError::task("test"), "task"),
            (AgentError::invalid_message("test"), "message"),
            (AgentError::network("test"), "network"),
            (AgentError::port("test"), "port"),
            (AgentError::adapter("test"), "adapter"),
            (AgentError::agent_lifecycle("test"), "lifecycle"),
            (AgentError::timeout("test"), "timeout"),
            (AgentError::validation("test"), "validation"),
            (AgentError::configuration("test"), "configuration"),
            (AgentError::unknown("test"), "unknown"),
        ];

        for (error, expected_category) in test_cases {
            assert_eq!(error.category(), expected_category);
        }
    }
}
