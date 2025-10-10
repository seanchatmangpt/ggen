//! Error types for LLM operations
//!
//! Provides comprehensive error handling for all LLM provider interactions,
//! network issues, configuration problems, and streaming failures.

use thiserror::Error;

/// Result type alias for LLM operations
pub type LlmResult<T> = Result<T, LlmError>;

/// Comprehensive error type for LLM operations
#[derive(Error, Debug)]
pub enum LlmError {
    /// API key is missing or invalid
    #[error("Invalid or missing API key for provider: {provider}")]
    InvalidApiKey { provider: String },

    /// Network or HTTP request failed
    #[error("Network request failed: {message}")]
    NetworkError { message: String },

    /// API returned an error response
    #[error("API error from {provider}: {status_code} - {message}")]
    ApiError {
        provider: String,
        status_code: u16,
        message: String,
    },

    /// Rate limit exceeded
    #[error("Rate limit exceeded for {provider}. Retry after: {retry_after:?}")]
    RateLimitExceeded {
        provider: String,
        retry_after: Option<u64>,
    },

    /// Response parsing failed
    #[error("Failed to parse response: {message}")]
    ParseError { message: String },

    /// Streaming operation failed
    #[error("Streaming error: {message}")]
    StreamError { message: String },

    /// Invalid configuration
    #[error("Configuration error: {message}")]
    ConfigError { message: String },

    /// Model not found or not supported
    #[error("Model '{model}' not found or not supported by {provider}")]
    ModelNotFound { provider: String, model: String },

    /// Request validation failed
    #[error("Request validation failed: {message}")]
    ValidationError { message: String },

    /// Timeout during operation
    #[error("Operation timed out after {seconds} seconds")]
    Timeout { seconds: u64 },

    /// Generic error for other cases
    #[error("LLM error: {0}")]
    Other(String),
}

impl From<reqwest::Error> for LlmError {
    fn from(err: reqwest::Error) -> Self {
        if err.is_timeout() {
            LlmError::Timeout { seconds: 30 }
        } else if err.is_status() {
            let status = err.status().unwrap();
            LlmError::ApiError {
                provider: "unknown".to_string(),
                status_code: status.as_u16(),
                message: err.to_string(),
            }
        } else {
            LlmError::NetworkError {
                message: err.to_string(),
            }
        }
    }
}

impl From<serde_json::Error> for LlmError {
    fn from(err: serde_json::Error) -> Self {
        LlmError::ParseError {
            message: err.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = LlmError::InvalidApiKey {
            provider: "openai".to_string(),
        };
        assert_eq!(
            err.to_string(),
            "Invalid or missing API key for provider: openai"
        );
    }

    #[test]
    fn test_api_error() {
        let err = LlmError::ApiError {
            provider: "anthropic".to_string(),
            status_code: 429,
            message: "Too many requests".to_string(),
        };
        assert!(err.to_string().contains("429"));
    }

    #[test]
    fn test_rate_limit_error() {
        let err = LlmError::RateLimitExceeded {
            provider: "openai".to_string(),
            retry_after: Some(60),
        };
        assert!(err.to_string().contains("Rate limit"));
    }
}
