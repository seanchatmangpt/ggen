#![deny(unsafe_code)]
#![warn(missing_docs)]

//! TAI gRPC - Production-grade service-to-service communication
//!
//! This crate provides:
//! - gRPC server implementation with advanced middleware
//! - gRPC client with resilience patterns (circuit breaker, retries, timeouts)
//! - Distributed tracing and metrics collection
//! - Load balancing and connection pooling
//! - Security features (mTLS, JWT, rate limiting)

pub mod grpc_client;
pub mod grpc_interceptor;
pub mod grpc_server;
pub mod resilience;

pub use grpc_client::{GrpcClient, GrpcClientConfig};
pub use grpc_server::{GrpcServer, GrpcServerConfig};

// Generated protobuf code
pub mod tai {
    tonic::include_proto!("tai");
}

/// Result type for gRPC operations
pub type Result<T> = std::result::Result<T, GrpcError>;

/// Errors that can occur in gRPC operations
#[derive(Debug, thiserror::Error)]
pub enum GrpcError {
    /// Connection error
    #[error("Connection error: {0}")]
    ConnectionError(String),

    /// Timeout error
    #[error("Timeout after {timeout_ms}ms")]
    TimeoutError { timeout_ms: u64 },

    /// Circuit breaker open
    #[error("Circuit breaker is open for {service}: {reason}")]
    CircuitBreakerOpen { service: String, reason: String },

    /// Service unavailable
    #[error("Service unavailable: {0}")]
    ServiceUnavailable(String),

    /// Invalid argument
    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    /// Internal error
    #[error("Internal error: {0}")]
    InternalError(String),

    /// Authentication error
    #[error("Authentication failed: {0}")]
    AuthenticationError(String),

    /// Authorization error
    #[error("Authorization denied: {0}")]
    AuthorizationError(String),

    /// Not found
    #[error("Not found: {0}")]
    NotFound(String),

    /// Already exists
    #[error("Already exists: {0}")]
    AlreadyExists(String),

    /// Rate limit exceeded
    #[error("Rate limit exceeded: {0}")]
    RateLimitExceeded(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// Other errors from tonic
    #[error("gRPC error: {0}")]
    TonicError(String),
}

impl From<tonic::transport::Error> for GrpcError {
    fn from(err: tonic::transport::Error) -> Self {
        GrpcError::ConnectionError(err.to_string())
    }
}

impl From<tonic::Status> for GrpcError {
    fn from(err: tonic::Status) -> Self {
        GrpcError::TonicError(format!("{:?}: {}", err.code(), err.message()))
    }
}

impl From<prost::DecodeError> for GrpcError {
    fn from(err: prost::DecodeError) -> Self {
        GrpcError::SerializationError(err.to_string())
    }
}

impl From<prost::EncodeError> for GrpcError {
    fn from(err: prost::EncodeError) -> Self {
        GrpcError::SerializationError(err.to_string())
    }
}

impl From<anyhow::Error> for GrpcError {
    fn from(err: anyhow::Error) -> Self {
        GrpcError::InternalError(err.to_string())
    }
}
