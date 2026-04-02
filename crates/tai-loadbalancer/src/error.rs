//! Error types for load balancer operations

use std::fmt;
use thiserror::Error;

/// Result type for load balancer operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error types for load balancer
#[derive(Debug, Error)]
pub enum Error {
    /// No healthy endpoints available
    #[error("No healthy endpoints available for service: {service}")]
    NoHealthyEndpoints { service: String },

    /// Service not found in registry
    #[error("Service not found: {0}")]
    ServiceNotFound(String),

    /// Invalid endpoint address
    #[error("Invalid endpoint address: {0}")]
    InvalidEndpoint(String),

    /// Connection pool exhausted
    #[error("Connection pool exhausted for {service}: max {max_connections} connections")]
    PoolExhausted {
        service: String,
        max_connections: usize,
    },

    /// Connection timeout
    #[error("Connection timeout after {timeout_ms}ms")]
    ConnectionTimeout { timeout_ms: u64 },

    /// Health check failed
    #[error("Health check failed: {0}")]
    HealthCheckFailed(String),

    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    InvalidConfiguration(String),

    /// Endpoint removed during operation
    #[error("Endpoint removed during operation: {0}")]
    EndpointRemoved(String),

    /// Operation timeout
    #[error("Operation timeout: {0}")]
    OperationTimeout(String),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),

    /// Other errors
    #[error("{0}")]
    Other(String),
}

impl Error {
    /// Create a new "no healthy endpoints" error
    pub fn no_healthy_endpoints(service: impl Into<String>) -> Self {
        Error::NoHealthyEndpoints {
            service: service.into(),
        }
    }

    /// Create a new "pool exhausted" error
    pub fn pool_exhausted(service: impl Into<String>, max: usize) -> Self {
        Error::PoolExhausted {
            service: service.into(),
            max_connections: max,
        }
    }

    /// Create a new "service not found" error
    pub fn service_not_found(service: impl Into<String>) -> Self {
        Error::ServiceNotFound(service.into())
    }

    /// Create a new "invalid endpoint" error
    pub fn invalid_endpoint(addr: impl Into<String>) -> Self {
        Error::InvalidEndpoint(addr.into())
    }

    /// Create a new "health check failed" error
    pub fn health_check_failed(reason: impl Into<String>) -> Self {
        Error::HealthCheckFailed(reason.into())
    }

    /// Create a new "invalid configuration" error
    pub fn invalid_config(reason: impl Into<String>) -> Self {
        Error::InvalidConfiguration(reason.into())
    }

    /// Create a new "internal error"
    pub fn internal(reason: impl Into<String>) -> Self {
        Error::Internal(reason.into())
    }
}

impl From<anyhow::Error> for Error {
    fn from(err: anyhow::Error) -> Self {
        Error::Other(err.to_string())
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::Other(err.to_string())
    }
}

impl From<tokio::time::error::Elapsed> for Error {
    fn from(err: tokio::time::error::Elapsed) -> Self {
        Error::OperationTimeout(err.to_string())
    }
}
