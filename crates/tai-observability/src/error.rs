//! Error types for observability operations.
//!
//! This module defines comprehensive error types for Cloud Profiler, Cloud Trace,
//! and Cloud Monitoring operations, ensuring clear error handling and debugging.

use thiserror::Error;

/// Observability system errors
#[derive(Debug, Error)]
pub enum ObservabilityError {
    /// GCP authentication failed
    #[error("GCP authentication failed: {0}")]
    AuthenticationError(String),

    /// Cloud Profiler operation failed
    #[error("Cloud Profiler error: {0}")]
    ProfilerError(String),

    /// Cloud Trace operation failed
    #[error("Cloud Trace error: {0}")]
    TraceError(String),

    /// Cloud Monitoring operation failed
    #[error("Cloud Monitoring error: {0}")]
    MonitoringError(String),

    /// Profile collection failed
    #[error("Profile collection failed: {0}")]
    ProfileCollectionError(String),

    /// Trace span creation failed
    #[error("Trace span creation failed: {0}")]
    SpanCreationError(String),

    /// Metric writing failed
    #[error("Metric writing failed: {0}")]
    MetricWriteError(String),

    /// Profile comparison failed
    #[error("Profile comparison failed: {0}")]
    ProfileComparisonError(String),

    /// Flame graph generation failed
    #[error("Flame graph generation failed: {0}")]
    FlameGraphError(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigurationError(String),

    /// Sampling error
    #[error("Sampling error: {0}")]
    SamplingError(String),

    /// Batch export error
    #[error("Batch export error: {0}")]
    BatchExportError(String),

    /// Regression detection error
    #[error("Regression detection error: {0}")]
    RegressionDetectionError(String),

    /// HTTP request error
    #[error("HTTP request error: {0}")]
    HttpError(#[from] reqwest::Error),

    /// JSON serialization error
    #[error("JSON serialization error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// Chrono error
    #[error("Time error: {0}")]
    TimeError(String),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

/// Result type for observability operations
pub type Result<T> = std::result::Result<T, ObservabilityError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = ObservabilityError::ProfilerError("test error".to_string());
        assert_eq!(
            err.to_string(),
            "Cloud Profiler error: test error"
        );
    }

    #[test]
    fn test_authentication_error() {
        let err = ObservabilityError::AuthenticationError("invalid credentials".to_string());
        assert!(err.to_string().contains("authentication"));
    }

    #[test]
    fn test_error_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<ObservabilityError>();
    }
}
