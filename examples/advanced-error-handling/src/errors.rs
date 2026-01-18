//! Custom error types demonstrating production-ready error handling patterns
//!
//! This module showcases:
//! - Rich, context-aware error types with thiserror
//! - Error conversion and wrapping
//! - Error source chains
//! - Domain-specific error variants
//! - Proper error messages for users

use std::path::PathBuf;
use thiserror::Error;

/// Main application error type
///
/// This demonstrates a well-structured error type that:
/// - Uses thiserror for automatic trait implementations
/// - Provides rich context in error messages
/// - Preserves error sources for debugging
/// - Groups related errors by domain
#[derive(Error, Debug)]
pub enum AppError {
    // === File System Errors ===
    /// File not found error with helpful context
    #[error("File not found: {path}\n  Hint: Check if the file exists and you have read permissions")]
    FileNotFound { path: PathBuf },

    /// File read error with source preservation
    #[error("Failed to read file: {path}")]
    FileReadError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// File write error with source preservation
    #[error("Failed to write to file: {path}")]
    FileWriteError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Directory creation error
    #[error("Failed to create directory: {path}")]
    DirectoryCreateError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    // === Configuration Errors ===
    /// Configuration file load error
    #[error("Failed to load configuration from {path}: {reason}")]
    ConfigLoadError { path: PathBuf, reason: String },

    /// Configuration parsing error
    #[error("Invalid configuration in {path}: {field}")]
    ConfigParseError {
        path: PathBuf,
        field: String,
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Missing required configuration field
    #[error("Required configuration field missing: {field}\n  Expected in: {path}")]
    ConfigMissingField { field: String, path: PathBuf },

    /// Invalid configuration value
    #[error("Invalid value for {field}: {value}\n  Reason: {reason}")]
    ConfigInvalidValue {
        field: String,
        value: String,
        reason: String,
    },

    // === Network Errors ===
    /// Network request failed
    #[error("Network request failed: {url}\n  Status: {status}\n  Reason: {reason}")]
    NetworkError {
        url: String,
        status: Option<u16>,
        reason: String,
    },

    /// Network timeout
    #[error("Request timed out after {timeout_secs} seconds: {url}")]
    NetworkTimeout { url: String, timeout_secs: u64 },

    /// Connection error
    #[error("Failed to connect to {host}:{port}")]
    ConnectionError {
        host: String,
        port: u16,
        #[source]
        source: std::io::Error,
    },

    // === Validation Errors ===
    /// Input validation failed
    #[error("Validation failed for {field}: {reason}")]
    ValidationError { field: String, reason: String },

    /// Data format error
    #[error("Invalid data format for {field}: expected {expected}, got {actual}")]
    FormatError {
        field: String,
        expected: String,
        actual: String,
    },

    /// Range validation error
    #[error("{field} out of range: {value} not in [{min}, {max}]")]
    RangeError {
        field: String,
        value: String,
        min: String,
        max: String,
    },

    // === Processing Errors ===
    /// Graph processing error
    #[error("Graph processing failed: {operation}")]
    GraphProcessingError {
        operation: String,
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Template rendering error
    #[error("Template rendering failed: {template_name}")]
    TemplateRenderError {
        template_name: String,
        #[source]
        source: Box<dyn std::error::Error + Send + Sync>,
    },

    /// Code generation error
    #[error("Code generation failed for {target}: {reason}")]
    CodeGenerationError { target: String, reason: String },

    // === External Errors (with automatic conversion) ===
    /// Standard I/O error
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    /// JSON serialization error
    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    /// TOML parsing error
    #[error("TOML parsing error: {0}")]
    Toml(#[from] toml::de::Error),

    /// HTTP client error
    #[error("HTTP client error: {0}")]
    Http(#[from] reqwest::Error),

    // === Integration Errors ===
    /// ggen-core error
    #[error("ggen-core error: {0}")]
    GgenCore(#[from] ggen_utils::error::Error),

    /// ggen-ai error
    #[error("AI generation error: {0}")]
    GgenAi(#[from] ggen_ai::error::GgenAiError),

    // === Generic Error ===
    /// Generic error for unexpected cases
    #[error("Unexpected error: {message}")]
    Other { message: String },
}

/// Result type alias for this application
pub type Result<T> = std::result::Result<T, AppError>;

impl AppError {
    // === Constructor helpers for better ergonomics ===

    /// Create a file not found error
    pub fn file_not_found(path: impl Into<PathBuf>) -> Self {
        Self::FileNotFound { path: path.into() }
    }

    /// Create a file read error
    pub fn file_read(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::FileReadError {
            path: path.into(),
            source,
        }
    }

    /// Create a file write error
    pub fn file_write(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::FileWriteError {
            path: path.into(),
            source,
        }
    }

    /// Create a directory create error
    pub fn directory_create(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::DirectoryCreateError {
            path: path.into(),
            source,
        }
    }

    /// Create a config load error
    pub fn config_load(path: impl Into<PathBuf>, reason: impl Into<String>) -> Self {
        Self::ConfigLoadError {
            path: path.into(),
            reason: reason.into(),
        }
    }

    /// Create a config parse error
    pub fn config_parse(
        path: impl Into<PathBuf>,
        field: impl Into<String>,
        source: impl std::error::Error + Send + Sync + 'static,
    ) -> Self {
        Self::ConfigParseError {
            path: path.into(),
            field: field.into(),
            source: Box::new(source),
        }
    }

    /// Create a validation error
    pub fn validation(field: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::ValidationError {
            field: field.into(),
            reason: reason.into(),
        }
    }

    /// Create a network error
    pub fn network(url: impl Into<String>, status: Option<u16>, reason: impl Into<String>) -> Self {
        Self::NetworkError {
            url: url.into(),
            status,
            reason: reason.into(),
        }
    }

    /// Create a network timeout error
    pub fn network_timeout(url: impl Into<String>, timeout_secs: u64) -> Self {
        Self::NetworkTimeout {
            url: url.into(),
            timeout_secs,
        }
    }

    /// Create a generic error
    pub fn other(message: impl Into<String>) -> Self {
        Self::Other {
            message: message.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_not_found_error() {
        let err = AppError::file_not_found("/path/to/file.txt");
        let msg = err.to_string();
        assert!(msg.contains("/path/to/file.txt"));
        assert!(msg.contains("Hint"));
    }

    #[test]
    fn test_config_missing_field() {
        let err = AppError::ConfigMissingField {
            field: "api_key".to_string(),
            path: PathBuf::from("config.toml"),
        };
        let msg = err.to_string();
        assert!(msg.contains("api_key"));
        assert!(msg.contains("config.toml"));
    }

    #[test]
    fn test_network_error() {
        let err = AppError::network("https://api.example.com", Some(503), "Service unavailable");
        let msg = err.to_string();
        assert!(msg.contains("503"));
        assert!(msg.contains("https://api.example.com"));
    }

    #[test]
    fn test_validation_error() {
        let err = AppError::validation("email", "Invalid email format");
        assert!(err.to_string().contains("email"));
        assert!(err.to_string().contains("Invalid email format"));
    }

    #[test]
    fn test_error_source_chain() {
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let err = AppError::file_read("/tmp/test.txt", io_err);

        // Test that source is preserved
        let err_ref: &dyn std::error::Error = &err;
        assert!(err_ref.source().is_some());
    }

    #[test]
    fn test_automatic_conversion() {
        // Test that automatic conversions work via From trait
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "test");
        let app_err: AppError = io_err.into();
        assert!(matches!(app_err, AppError::Io(_)));
    }
}
