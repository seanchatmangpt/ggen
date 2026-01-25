//! Error types for the API Gateway with comprehensive error context
//!
//! This module defines all possible error conditions in the gateway, supporting
//! detailed error reporting and debugging while maintaining type safety.

use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use serde::Serialize;
use std::fmt;
use thiserror::Error;

/// Comprehensive error type for gateway operations
#[derive(Error, Debug, Clone)]
pub enum GatewayError {
    #[error("Authentication failed: {0}")]
    AuthenticationFailed(String),

    #[error("Authorization failed: {0}")]
    AuthorizationFailed(String),

    #[error("Rate limit exceeded")]
    RateLimitExceeded,

    #[error("Request routing failed: {0}")]
    RoutingFailed(String),

    #[error("Request transformation failed: {0}")]
    TransformationFailed(String),

    #[error("Invalid request: {0}")]
    InvalidRequest(String),

    #[error("Service unavailable: {0}")]
    ServiceUnavailable(String),

    #[error("Circuit breaker open: {0}")]
    CircuitBreakerOpen(String),

    #[error("Health check failed: {0}")]
    HealthCheckFailed(String),

    #[error("Invalid configuration: {0}")]
    ConfigurationError(String),

    #[error("Internal server error: {0}")]
    InternalError(String),

    #[error("Upstream error: {0}")]
    UpstreamError(String),

    #[error("Timeout: {0}")]
    Timeout(String),

    #[error("Protocol error: {0}")]
    ProtocolError(String),
}

impl GatewayError {
    /// Get the HTTP status code for this error
    pub fn status_code(&self) -> StatusCode {
        match self {
            Self::AuthenticationFailed(_) => StatusCode::UNAUTHORIZED,
            Self::AuthorizationFailed(_) => StatusCode::FORBIDDEN,
            Self::RateLimitExceeded => StatusCode::TOO_MANY_REQUESTS,
            Self::InvalidRequest(_) => StatusCode::BAD_REQUEST,
            Self::RoutingFailed(_) => StatusCode::NOT_FOUND,
            Self::TransformationFailed(_) => StatusCode::UNPROCESSABLE_ENTITY,
            Self::CircuitBreakerOpen(_) => StatusCode::SERVICE_UNAVAILABLE,
            Self::ServiceUnavailable(_) => StatusCode::SERVICE_UNAVAILABLE,
            Self::HealthCheckFailed(_) => StatusCode::SERVICE_UNAVAILABLE,
            Self::Timeout(_) => StatusCode::GATEWAY_TIMEOUT,
            Self::ConfigurationError(_) => StatusCode::INTERNAL_SERVER_ERROR,
            Self::InternalError(_) => StatusCode::INTERNAL_SERVER_ERROR,
            Self::UpstreamError(_) => StatusCode::BAD_GATEWAY,
            Self::ProtocolError(_) => StatusCode::BAD_GATEWAY,
        }
    }

    /// Get the error code identifier for structured logging
    pub fn error_code(&self) -> &'static str {
        match self {
            Self::AuthenticationFailed(_) => "AUTH_FAILED",
            Self::AuthorizationFailed(_) => "AUTHZ_FAILED",
            Self::RateLimitExceeded => "RATE_LIMIT_EXCEEDED",
            Self::RoutingFailed(_) => "ROUTING_FAILED",
            Self::TransformationFailed(_) => "TRANSFORM_FAILED",
            Self::InvalidRequest(_) => "INVALID_REQUEST",
            Self::ServiceUnavailable(_) => "SERVICE_UNAVAILABLE",
            Self::CircuitBreakerOpen(_) => "CIRCUIT_BREAKER_OPEN",
            Self::HealthCheckFailed(_) => "HEALTH_CHECK_FAILED",
            Self::ConfigurationError(_) => "CONFIG_ERROR",
            Self::InternalError(_) => "INTERNAL_ERROR",
            Self::UpstreamError(_) => "UPSTREAM_ERROR",
            Self::Timeout(_) => "TIMEOUT",
            Self::ProtocolError(_) => "PROTOCOL_ERROR",
        }
    }
}

/// Error response JSON structure for API responses
#[derive(Serialize, Debug, Clone)]
pub struct ErrorResponse {
    /// Error code identifier (e.g., "AUTH_FAILED")
    pub code: String,
    /// Human-readable error message
    pub message: String,
    /// Request ID for correlation (if available)
    pub request_id: Option<String>,
    /// Additional context data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<serde_json::Value>,
}

impl ErrorResponse {
    /// Create a new error response
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            request_id: None,
            details: None,
        }
    }

    /// Set the request ID on this error response
    pub fn with_request_id(mut self, request_id: String) -> Self {
        self.request_id = Some(request_id);
        self
    }

    /// Set additional details on this error response
    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

/// Result type alias for gateway operations
pub type GatewayResult<T> = Result<T, GatewayError>;

impl IntoResponse for GatewayError {
    fn into_response(self) -> Response {
        let status = self.status_code();
        let error_response = ErrorResponse::new(self.error_code(), self.to_string());

        (status, Json(error_response)).into_response()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_authentication_error_status() {
        let error = GatewayError::AuthenticationFailed("invalid token".to_string());
        assert_eq!(error.status_code(), StatusCode::UNAUTHORIZED);
        assert_eq!(error.error_code(), "AUTH_FAILED");
    }

    #[test]
    fn test_rate_limit_error_status() {
        let error = GatewayError::RateLimitExceeded;
        assert_eq!(error.status_code(), StatusCode::TOO_MANY_REQUESTS);
        assert_eq!(error.error_code(), "RATE_LIMIT_EXCEEDED");
    }

    #[test]
    fn test_error_response_serialization() {
        let response = ErrorResponse::new("TEST_ERROR", "This is a test error")
            .with_request_id("req-123".to_string());

        let json = serde_json::to_value(&response).unwrap();
        assert_eq!(json["code"], "TEST_ERROR");
        assert_eq!(json["request_id"], "req-123");
    }
}
