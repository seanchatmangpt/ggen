//! MCP Transport Error types
//!
//! Comprehensive error handling for all MCP transport implementations.
//! Follows the error handling patterns from a2a-rs for consistency.

use std::io;
use thiserror::Error;

/// Standard JSON-RPC 2.0 error codes
pub const PARSE_ERROR: i32 = -32700;
pub const INVALID_REQUEST: i32 = -32600;
pub const METHOD_NOT_FOUND: i32 = -32601;
pub const INVALID_PARAMS: i32 = -32602;
pub const INTERNAL_ERROR: i32 = -32603;

/// MCP specific error codes (custom range -32xxx)
pub const TRANSPORT_ERROR: i32 = -32100;
pub const CONNECTION_FAILED: i32 = -32101;
pub const TIMEOUT: i32 = -32102;
pub const NOT_CONNECTED: i32 = -32103;
pub const SHUTDOWN: i32 = -32104;
pub const PROTOCOL_ERROR: i32 = -32105;
pub const STREAM_CLOSED: i32 = -32106;

/// Error type for MCP transport operations
///
/// This error type encompasses all possible failures that can occur
/// during MCP protocol communication across different transport mechanisms.
#[derive(Error, Debug)]
pub enum TransportError {
    /// JSON-RPC protocol error with code, message, and optional data
    #[error("JSON-RPC error: {code} - {message}")]
    JsonRpc {
        code: i32,
        message: String,
        data: Option<serde_json::Value>,
    },

    /// JSON parsing/serialization error
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// IO error during transport operations
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    /// Transport connection failed
    #[error("Connection failed: {0}")]
    ConnectionFailed(String),

    /// Operation timed out
    #[error("Operation timed out")]
    Timeout,

    /// Not connected to MCP server
    #[error("Not connected to MCP server")]
    NotConnected,

    /// Transport has been shut down
    #[error("Transport shut down")]
    Shutdown,

    /// Protocol violation detected
    #[error("Protocol error: {0}")]
    Protocol(String),

    /// Stream closed unexpectedly
    #[error("Stream closed")]
    StreamClosed,

    /// Invalid request parameters
    #[error("Invalid parameters: {0}")]
    InvalidParams(String),

    /// Method not found on server
    #[error("Method not found: {0}")]
    MethodNotFound(String),

    /// Unsupported operation for this transport
    #[error("Unsupported operation: {0}")]
    UnsupportedOperation(String),

    /// Request ID mismatch in response correlation
    #[error("Request ID mismatch: expected {expected}, got {actual}")]
    IdMismatch { expected: String, actual: String },

    /// HTTP-specific error
    #[error("HTTP error: {status} - {message}")]
    Http { status: u16, message: String },

    /// Stdio process error
    #[error("Process error: {0}")]
    Process(String),

    /// Invalid URI or endpoint configuration
    #[error("Invalid endpoint: {0}")]
    InvalidEndpoint(String),

    /// Authentication failed
    #[error("Authentication failed: {0}")]
    Authentication(String),

    /// Internal transport error
    #[error("Internal error: {0}")]
    Internal(String),
}

impl TransportError {
    /// Create a JSON-RPC error from components
    pub fn json_rpc(code: i32, message: impl Into<String>) -> Self {
        TransportError::JsonRpc {
            code,
            message: message.into(),
            data: None,
        }
    }

    /// Create a JSON-RPC error with data
    pub fn json_rpc_with_data(
        code: i32,
        message: impl Into<String>,
        data: serde_json::Value,
    ) -> Self {
        TransportError::JsonRpc {
            code,
            message: message.into(),
            data: Some(data),
        }
    }

    /// Get the error code for JSON-RPC responses
    pub fn error_code(&self) -> i32 {
        match self {
            TransportError::JsonRpc { code, .. } => *code,
            TransportError::Json(_) => PARSE_ERROR,
            TransportError::InvalidParams(_) => INVALID_PARAMS,
            TransportError::MethodNotFound(_) => METHOD_NOT_FOUND,
            TransportError::ConnectionFailed(_) => CONNECTION_FAILED,
            TransportError::Timeout => TIMEOUT,
            TransportError::NotConnected => NOT_CONNECTED,
            TransportError::Shutdown => SHUTDOWN,
            TransportError::Protocol(_) => PROTOCOL_ERROR,
            TransportError::StreamClosed => STREAM_CLOSED,
            TransportError::UnsupportedOperation(_) => METHOD_NOT_FOUND,
            _ => INTERNAL_ERROR,
        }
    }

    /// Convert to a JSON-RPC error value
    pub fn to_jsonrpc_error(&self) -> serde_json::Value {
        serde_json::json!({
            "code": self.error_code(),
            "message": self.to_string(),
            "data": null,
        })
    }

    /// Check if error is retryable
    pub fn is_retryable(&self) -> bool {
        matches!(
            self,
            TransportError::Timeout
                | TransportError::ConnectionFailed(_)
                | TransportError::Io(_)
                | TransportError::Protocol(_)
        )
    }

    /// Check if error is fatal (requires reconnect)
    pub fn is_fatal(&self) -> bool {
        matches!(
            self,
            TransportError::Shutdown
                | TransportError::StreamClosed
                | TransportError::Protocol(_)
        )
    }
}

/// Result type alias for transport operations
pub type TransportResult<T> = Result<T, TransportError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_code_mapping() {
        assert_eq!(TransportError::Timeout.error_code(), TIMEOUT);
        assert_eq!(TransportError::NotConnected.error_code(), NOT_CONNECTED);
        assert_eq!(
            TransportError::InvalidParams("test".to_string()).error_code(),
            INVALID_PARAMS
        );
    }

    #[test]
    fn test_json_rpc_error() {
        let error = TransportError::json_rpc(METHOD_NOT_FOUND, "method not found");
        assert!(matches!(error, TransportError::JsonRpc { code, .. } if code == METHOD_NOT_FOUND));
    }

    #[test]
    fn test_retryable_errors() {
        assert!(TransportError::Timeout.is_retryable());
        assert!(TransportError::ConnectionFailed("test".to_string()).is_retryable());
        assert!(!TransportError::Shutdown.is_retryable());
        assert!(!TransportError::InvalidParams("test".to_string()).is_retryable());
    }

    #[test]
    fn test_fatal_errors() {
        assert!(TransportError::Shutdown.is_fatal());
        assert!(TransportError::StreamClosed.is_fatal());
        assert!(!TransportError::Timeout.is_fatal());
    }

    #[test]
    fn test_to_jsonrpc_error() {
        let error = TransportError::InvalidParams("missing field".to_string());
        let json_value = error.to_jsonrpc_error();

        assert_eq!(json_value["code"], INVALID_PARAMS);
        assert!(json_value["message"].as_str().unwrap().contains("missing field"));
    }
}
