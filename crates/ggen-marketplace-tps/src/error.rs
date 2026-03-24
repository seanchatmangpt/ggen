//! Error types for marketplace/workspace economy integration.
//!
//! Type-first design: All errors are strongly typed with context.

use thiserror::Error;

/// Result type for marketplace operations.
pub type Result<T> = std::result::Result<T, MarketplaceError>;

/// Errors that can occur during marketplace operations.
#[derive(Debug, Error)]
pub enum MarketplaceError {
    /// HTTP request failed
    #[error("HTTP request failed: {0}")]
    HttpError(#[from] reqwest::Error),

    /// Authentication failed
    #[error("Authentication failed: {0}")]
    AuthError(String),

    /// Entitlement check failed
    #[error("Entitlement check failed: {0}")]
    EntitlementError(String),

    /// Workspace operation failed
    #[error("Workspace operation failed: {0}")]
    WorkspaceError(String),

    /// CLM proxy operation failed
    #[error("CLM proxy operation failed: {0}")]
    ClmProxyError(String),

    /// JSON serialization/deserialization failed
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    ConfigError(String),

    /// Invalid URL
    #[error("Invalid URL: {0}")]
    UrlError(#[from] url::ParseError),

    /// Resource not found
    #[error("Resource not found: {0}")]
    NotFound(String),

    /// Permission denied
    #[error("Permission denied: {0}")]
    PermissionDenied(String),

    /// Rate limit exceeded
    #[error("Rate limit exceeded: {0}")]
    RateLimitExceeded(String),

    /// Invalid state
    #[error("Invalid state: {0}")]
    InvalidState(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        // Arrange & Act
        let err = MarketplaceError::AuthError("invalid token".to_string());

        // Assert
        assert_eq!(err.to_string(), "Authentication failed: invalid token");
    }

    #[test]
    fn test_error_from_json() {
        // Arrange
        let json_err = serde_json::from_str::<serde_json::Value>("{invalid json")
            .unwrap_err();

        // Act
        let err = MarketplaceError::from(json_err);

        // Assert
        assert!(matches!(err, MarketplaceError::JsonError(_)));
    }
}
