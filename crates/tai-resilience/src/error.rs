//! Error types for resilience operations

use std::fmt;
use thiserror::Error;

/// Result type for resilience operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error types for tai-resilience
#[derive(Debug, Error)]
pub enum Error {
    /// Circuit breaker is open and requests cannot be processed
    #[error("Circuit breaker is open: {service_name}")]
    CircuitBreakerOpen { service_name: String },

    /// Request timeout exceeded
    #[error("Request timeout after {duration_ms}ms for {service_name}")]
    RequestTimeout {
        service_name: String,
        duration_ms: u64,
    },

    /// Service unavailable or unhealthy
    #[error("Service unavailable: {service_name}")]
    ServiceUnavailable { service_name: String },

    /// Configuration validation failed
    #[error("Configuration validation failed: {reason}")]
    ConfigurationError { reason: String },

    /// Invalid deployment configuration
    #[error("Invalid deployment configuration: {reason}")]
    InvalidDeploymentConfig { reason: String },

    /// Kubernetes API error
    #[error("Kubernetes API error: {reason}")]
    KubernetesError { reason: String },

    /// Istio configuration error
    #[error("Istio configuration error: {reason}")]
    IstioConfigError { reason: String },

    /// Traffic split validation failed
    #[error("Traffic split validation failed: {reason}")]
    InvalidTrafficSplit { reason: String },

    /// Canary deployment error
    #[error("Canary deployment error: {reason}")]
    CanaryDeploymentError { reason: String },

    /// Outlier detection configuration error
    #[error("Outlier detection configuration error: {reason}")]
    OutlierDetectionError { reason: String },

    /// Serialization/deserialization error
    #[error("Serialization error: {0}")]
    SerializationError(String),

    /// HTTP request error
    #[error("HTTP error: {reason}")]
    HttpError { reason: String },

    /// Firestore error (when firestore feature is enabled)
    #[error("Firestore error: {reason}")]
    FirestoreError { reason: String },

    /// Generic internal error
    #[error("Internal error: {reason}")]
    InternalError { reason: String },
}

impl Error {
    /// Create a circuit breaker open error
    pub fn circuit_breaker_open(service_name: impl Into<String>) -> Self {
        Error::CircuitBreakerOpen {
            service_name: service_name.into(),
        }
    }

    /// Create a timeout error
    pub fn timeout(service_name: impl Into<String>, duration_ms: u64) -> Self {
        Error::RequestTimeout {
            service_name: service_name.into(),
            duration_ms,
        }
    }

    /// Create a service unavailable error
    pub fn service_unavailable(service_name: impl Into<String>) -> Self {
        Error::ServiceUnavailable {
            service_name: service_name.into(),
        }
    }

    /// Create a configuration error
    pub fn config_error(reason: impl Into<String>) -> Self {
        Error::ConfigurationError {
            reason: reason.into(),
        }
    }

    /// Create an internal error
    pub fn internal(reason: impl Into<String>) -> Self {
        Error::InternalError {
            reason: reason.into(),
        }
    }

    /// Check if this is a transient error (should retry)
    pub fn is_transient(&self) -> bool {
        matches!(
            self,
            Error::RequestTimeout { .. } | Error::ServiceUnavailable { .. }
        )
    }

    /// Check if this is a permanent error (should not retry)
    pub fn is_permanent(&self) -> bool {
        matches!(
            self,
            Error::CircuitBreakerOpen { .. } | Error::ConfigurationError { .. }
        )
    }
}

/// Convert from anyhow errors
impl From<anyhow::Error> for Error {
    fn from(err: anyhow::Error) -> Self {
        Error::InternalError {
            reason: err.to_string(),
        }
    }
}

/// Convert from serde_json errors
impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Error::SerializationError(err.to_string())
    }
}

/// Convert from serde_yaml errors
impl From<serde_yaml::Error> for Error {
    fn from(err: serde_yaml::Error) -> Self {
        Error::SerializationError(err.to_string())
    }
}

/// Convert from reqwest errors
impl From<reqwest::Error> for Error {
    fn from(err: reqwest::Error) -> Self {
        Error::HttpError {
            reason: err.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_breaker_open_error() {
        let err = Error::circuit_breaker_open("payment-service");
        assert!(matches!(err, Error::CircuitBreakerOpen { .. }));
        assert!(err.is_permanent());
        assert!(!err.is_transient());
    }

    #[test]
    fn test_timeout_error() {
        let err = Error::timeout("auth-service", 5000);
        assert!(err.is_transient());
        assert!(!err.is_permanent());
    }

    #[test]
    fn test_service_unavailable_error() {
        let err = Error::service_unavailable("db-service");
        assert!(err.is_transient());
    }

    #[test]
    fn test_config_error() {
        let err = Error::config_error("Invalid threshold value");
        assert!(err.is_permanent());
    }
}
