//! Error types for A2A-MCP integration

use thiserror::Error;

/// Errors that can occur in A2A-MCP integration
#[derive(Error, Debug)]
pub enum A2aMcpError {
    /// Error related to A2A protocol
    #[error("A2A error: {0}")]
    A2a(String),

    /// Error related to LLM provider
    #[error("LLM error: {0}")]
    Llm(String),

    /// Error in protocol translation
    #[error("Protocol translation error: {0}")]
    Translation(String),

    /// Task not found
    #[error("Task not found: {0}")]
    TaskNotFound(String),

    /// Error in task processing
    #[error("Task processing error: {0}")]
    TaskProcessing(String),

    /// Agent not found
    #[error("Agent not found: {0}")]
    AgentNotFound(String),

    /// Invalid tool method format
    #[error("Invalid tool method format: {0}")]
    InvalidToolMethod(String),

    /// Server error
    #[error("Server error: {0}")]
    Server(String),

    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// URL parsing error
    #[error("URL error: {0}")]
    Url(#[from] url::ParseError),

    /// Generic error
    #[error("Error: {0}")]
    Generic(#[from] anyhow::Error),

    /// Connection error
    #[error("Connection error: {0}")]
    Connection(String),

    /// Connection timeout
    #[error("Connection timeout after {0:?}")]
    ConnectionTimeout(std::time::Duration),

    /// Request timeout
    #[error("Request timeout: {0}")]
    RequestTimeout(String),

    /// Maximum retries exceeded
    #[error("Maximum retries exceeded: {0}")]
    MaxRetriesExceeded(String),

    /// Health check failed
    #[error("Health check failed: {0}")]
    HealthCheckFailed(String),

    /// Streaming error
    #[error("Streaming error: {0}")]
    Streaming(String),

    /// Agent unavailable
    #[error("Agent unavailable: {0}")]
    AgentUnavailable(String),

    /// Capability not supported
    #[error("Capability not supported: {0}")]
    CapabilityNotSupported(String),

    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    InvalidConfiguration(String),

    /// Rate limit exceeded
    #[error("Rate limit exceeded: try again in {0:?}")]
    RateLimitExceeded(std::time::Duration),

    /// Authentication failed
    #[error("Authentication failed: {0}")]
    AuthenticationFailed(String),

    /// Authorization failed
    #[error("Authorization failed: {0}")]
    AuthorizationFailed(String),
}

/// Result type for A2A-MCP operations
pub type A2aMcpResult<T> = std::result::Result<T, A2aMcpError>;

/// Convenience function to convert a string error to an A2aMcpError
pub fn err<E: ToString>(e: E) -> A2aMcpError {
    A2aMcpError::Translation(e.to_string())
}
