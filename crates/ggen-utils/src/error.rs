//! Error handling types and utilities
//!
//! This module provides the core error handling infrastructure for the ggen project.
//! It defines a custom `Error` type that supports error chaining, context, and
//! conversion from common error types.
//!
//! ## Features
//!
//! - **Error chaining**: Support for source errors with full error chain
//! - **Context information**: Additional context can be attached to errors
//! - **Type conversions**: Automatic conversion from common error types
//! - **Helper methods**: Convenient constructors for common error scenarios
//!
//! ## Error Type
//!
//! The `Error` type is the primary error type used throughout ggen. It implements
//! `std::error::Error` and provides:
//!
//! - Message: Primary error message
//! - Context: Optional additional context
//! - Source: Optional underlying error (for error chaining)
//!
//! ## Examples
//!
//! ### Creating Errors
//!
//! ```rust
//! use ggen_utils::error::Error;
//!
//! # fn main() {
//! // Simple error
//! let err = Error::new("Something went wrong");
//! assert_eq!(err.to_string(), "Something went wrong");
//!
//! // Error with context
//! let err = Error::with_context("Failed to read file", "config.toml");
//! assert!(err.to_string().contains("Failed to read file"));
//!
//! // Error with source
//! let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
//! let err = Error::with_source("Configuration error", Box::new(io_err));
//! assert!(err.to_string().contains("Configuration error"));
//! # }
//! ```
//!
//! ### Using Result Type
//!
//! ```rust,no_run
//! use ggen_utils::error::{Error, Result};
//!
//! fn read_config() -> Result<String> {
//!     std::fs::read_to_string("config.toml")
//!         .map_err(|e| Error::with_source("Failed to read config", Box::new(e)))
//! }
//!
//! # fn main() -> Result<()> {
//! let _config = read_config()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Helper Methods
//!
//! ```rust
//! use ggen_utils::error::Error;
//! use std::path::PathBuf;
//!
//! # fn main() {
//! // Common error types
//! let err = Error::file_not_found(PathBuf::from("config.toml"));
//! assert!(err.to_string().contains("config.toml"));
//!
//! let err = Error::invalid_input("Invalid project name");
//! assert!(err.to_string().contains("Invalid project name"));
//!
//! let err = Error::network_error("Connection timeout");
//! assert!(err.to_string().contains("Connection timeout"));
//! # }
//! ```

use std::error::Error as StdError;
use std::fmt;

/// Custom error type for the ggen project
#[derive(Debug)]
pub struct Error {
    message: String,
    context: Option<String>,
    source: Option<Box<dyn StdError + Send + Sync>>,
}

impl Error {
    /// Create a new error with a message
    #[must_use]
    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
            context: None,
            source: None,
        }
    }

    /// Create a new error with a formatted message
    #[must_use]
    pub fn new_fmt(args: std::fmt::Arguments) -> Self {
        Self {
            message: args.to_string(),
            context: None,
            source: None,
        }
    }

    /// Create an error with additional context
    #[must_use]
    pub fn with_context(message: &str, context: &str) -> Self {
        Self {
            message: message.to_string(),
            context: Some(context.to_string()),
            source: None,
        }
    }

    /// Create an error with a source error
    #[must_use]
    pub fn with_source(message: &str, source: Box<dyn StdError + Send + Sync>) -> Self {
        Self {
            message: message.to_string(),
            context: None,
            source: Some(source),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;

        if let Some(context) = &self.context {
            write!(f, " (context: {context})")?;
        }

        if let Some(source) = &self.source {
            write!(f, " (caused by: {source})")?;
        }

        Ok(())
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.source
            .as_ref()
            .map(|s| s.as_ref() as &(dyn StdError + 'static))
    }
}

/// Result type alias for the ggen project
pub type Result<T> = std::result::Result<T, Error>;

// Implement From for common error types
impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<serde_yaml::Error> for Error {
    fn from(err: serde_yaml::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<tera::Error> for Error {
    fn from(err: tera::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<config::ConfigError> for Error {
    fn from(err: config::ConfigError) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<log::SetLoggerError> for Error {
    fn from(err: log::SetLoggerError) -> Self {
        Self::new(&err.to_string())
    }
}

impl<T> From<std::sync::PoisonError<T>> for Error {
    fn from(err: std::sync::PoisonError<T>) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<anyhow::Error> for Error {
    fn from(err: anyhow::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<toml::de::Error> for Error {
    fn from(err: toml::de::Error) -> Self {
        Self::new(&err.to_string())
    }
}

impl From<String> for Error {
    fn from(err: String) -> Self {
        Self::new(&err)
    }
}

impl From<&str> for Error {
    fn from(err: &str) -> Self {
        Self::new(err)
    }
}

/// GgenError type alias for backwards compatibility with P2P module
pub type GgenError = Error;

impl Error {
    /// Create an invalid input error
    #[must_use]
    pub fn invalid_input(message: impl Into<String>) -> Self {
        let msg = message.into();
        Self::new(&format!("Invalid input: {}", msg))
    }

    /// Create a network error
    #[must_use]
    pub fn network_error(message: impl Into<String>) -> Self {
        let msg = message.into();
        Self::new(&format!("Network error: {}", msg))
    }

    /// Create a feature not enabled error
    #[must_use]
    pub fn feature_not_enabled(feature: &str, help: &str) -> Self {
        Self::new(&format!("Feature '{}' not enabled. {}", feature, help))
    }

    /// Create a file not found error
    #[must_use]
    pub fn file_not_found(path: std::path::PathBuf) -> Self {
        Self::new(&format!("File not found: {}", path.display()))
    }

    /// Create an IO error
    #[must_use]
    pub fn io_error(message: impl Into<String>) -> Self {
        let msg = message.into();
        Self::new(&format!("IO error: {}", msg))
    }

    /// Create an internal error
    #[must_use]
    pub fn internal_error(message: impl Into<String>) -> Self {
        let msg = message.into();
        Self::new(&format!("Internal error: {}", msg))
    }

    /// Create an invalid state error
    #[must_use]
    pub fn invalid_state(message: impl Into<String>) -> Self {
        let msg = message.into();
        Self::new(&format!("Invalid state: {}", msg))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let error = Error::new("Test error message");
        assert_eq!(error.message, "Test error message");
    }

    #[test]
    fn test_error_display() {
        let error = Error::new("Test error message");
        let display = format!("{error}");
        assert_eq!(display, "Test error message");
    }

    #[test]
    fn test_error_debug() {
        let error = Error::new("Test error message");
        let debug = format!("{error:?}");
        assert!(debug.contains("Test error message"));
    }

    #[test]
    fn test_error_from_io_error() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
        let error: Error = io_error.into();

        assert!(error.to_string().contains("File not found"));
    }

    #[test]
    fn test_error_from_yaml_error() {
        let yaml_content = "invalid: yaml: content: [";
        let yaml_error = serde_yaml::from_str::<serde_yaml::Value>(yaml_content).unwrap_err();
        let error: Error = yaml_error.into();

        assert!(!error.to_string().is_empty());
    }

    #[test]
    fn test_error_from_json_error() {
        let json_content = "invalid json content";
        let json_error = serde_json::from_str::<serde_json::Value>(json_content).unwrap_err();
        let error: Error = json_error.into();

        assert!(!error.to_string().is_empty());
    }

    #[test]
    fn test_error_from_tera_error() {
        let template_content = "{{ invalid template syntax";
        let tera_error = tera::Tera::new("templates/**/*")
            .unwrap()
            .render_str(template_content, &tera::Context::new())
            .unwrap_err();
        let error: Error = tera_error.into();

        assert!(!error.to_string().is_empty());
    }

    #[test]
    fn test_result_type() {
        fn success_function() -> String {
            "success".to_string()
        }

        fn error_function() -> Result<String> {
            Err(Error::new("error"))
        }

        assert_eq!(success_function(), "success");

        assert!(error_function().is_err());
        assert_eq!(error_function().unwrap_err().to_string(), "error");
    }

    #[test]
    fn test_error_chain() {
        let io_error =
            std::io::Error::new(std::io::ErrorKind::PermissionDenied, "Permission denied");
        let error: Error = io_error.into();

        // Test that the error can be used as std::error::Error
        let error_ref: &dyn std::error::Error = &error;
        assert!(!error_ref.to_string().is_empty());
    }
}
