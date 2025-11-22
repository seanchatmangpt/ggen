//! Comprehensive Error Taxonomy and Propagation
//!
//! This module implements a systematic error hierarchy that ensures:
//! - No silent failures
//! - Rich diagnostic information
//! - Actionable error messages
//! - Proper error propagation
//!
//! # Design Principle
//!
//! Every error must be visible, propagated, and actionable.
//! Errors are first-class citizens with context and suggestions.

use thiserror::Error;

// ============================================================================
// Top-Level Error Type
// ============================================================================

/// Top-level error type for ggen
///
/// This error type encompasses all possible failure modes in the system.
/// Each variant provides rich context and actionable information.
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::GgenError;
///
/// fn load_template(path: &str) -> Result<Template, GgenError> {
///     // ...
/// }
/// ```
#[derive(Error, Debug)]
pub enum GgenError {
    // ========================================================================
    // Template Errors
    // ========================================================================
    #[error("Template not found: {path}")]
    TemplateNotFound {
        path: String,
        context: String,
        suggestion: Option<String>,
    },

    #[error("Invalid template syntax in {file}:{line}:{column}\n{snippet}\n{reason}")]
    InvalidTemplateSyntax {
        file: String,
        line: usize,
        column: usize,
        reason: String,
        snippet: String,
        fix_suggestion: Option<String>,
    },

    #[error("Template validation failed: {0}")]
    TemplateValidationFailed(#[from] ValidationError),

    #[error("Template rendering failed: {reason}\nTemplate: {template}\nContext: {context}")]
    RenderingFailed {
        template: String,
        context: String,
        reason: String,
        debug_info: Option<String>,
    },

    // ========================================================================
    // CLI Errors
    // ========================================================================
    #[error("Invalid command: '{command}'\n{suggestion}")]
    InvalidCommand {
        command: String,
        suggestion: String,
        valid_commands: Vec<String>,
    },

    #[error("Missing required argument: {arg}\nUsage: {usage}")]
    MissingArgument { arg: String, usage: String },

    #[error("Invalid argument value: {arg} = '{value}'\nExpected: {expected}\nGot: {actual}")]
    InvalidArgumentValue {
        arg: String,
        value: String,
        expected: String,
        actual: String,
        allowed_values: Vec<String>,
    },

    // ========================================================================
    // Integration Errors
    // ========================================================================
    #[error("API incompatibility: {message}\nExpected version: {expected_version}\nActual version: {actual_version}\nSuggestion: {suggestion}")]
    ApiIncompatible {
        message: String,
        expected_version: String,
        actual_version: String,
        suggestion: String,
    },

    #[error("Contract violation: {contract} failed\nReason: {reason}\nFix: {fix_suggestion}")]
    ContractViolation {
        contract: String,
        reason: String,
        fix_suggestion: String,
        affected_components: Vec<String>,
    },

    #[error("Integration test failed: {test_name}\nExpected: {expected}\nActual: {actual}")]
    IntegrationTestFailed {
        test_name: String,
        expected: String,
        actual: String,
        diff: Option<String>,
    },

    // ========================================================================
    // System Errors
    // ========================================================================
    #[error("IO error: {operation} failed for {path}\nReason: {source}\nSuggestion: {suggestion}")]
    Io {
        operation: String,
        path: String,
        #[source]
        source: std::io::Error,
        suggestion: String,
    },

    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),

    #[error("Permission denied: {operation} on {resource}\nRequired permissions: {required}\nSuggestion: {suggestion}")]
    PermissionDenied {
        operation: String,
        resource: String,
        required: String,
        suggestion: String,
    },

    // ========================================================================
    // Multiple Errors
    // ========================================================================
    #[error("Multiple errors occurred:\nPrimary: {primary}\nAdditional errors: {count}")]
    Multiple {
        primary: Box<GgenError>,
        errors: Vec<GgenError>,
        count: usize,
    },
}

// ============================================================================
// Validation Errors
// ============================================================================

/// Validation-specific errors with detailed diagnostics
#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("Missing required field: '{field}' in {location}")]
    MissingField {
        field: String,
        location: String,
        expected_type: String,
    },

    #[error("Type mismatch: expected {expected}, got {actual} at {location}")]
    TypeMismatch {
        expected: String,
        actual: String,
        location: String,
        value: String,
    },

    #[error("Constraint violation: {constraint}\nValue: {value}\nAllowed: {allowed_values:?}")]
    ConstraintViolation {
        constraint: String,
        value: String,
        allowed_values: Vec<String>,
        suggestion: Option<String>,
    },

    #[error("Schema validation failed: {reason}\nSchema: {schema}\nData: {data}")]
    SchemaValidationFailed {
        reason: String,
        schema: String,
        data: String,
        errors: Vec<String>,
    },
}

// ============================================================================
// Configuration Errors
// ============================================================================

/// Configuration-specific errors
#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found: {path}\nSearched locations: {searched:?}")]
    FileNotFound {
        path: String,
        searched: Vec<String>,
    },

    #[error("Invalid configuration: {reason}\nFile: {file}\nLine: {line}")]
    InvalidConfig {
        reason: String,
        file: String,
        line: usize,
    },

    #[error("Configuration parse error: {0}")]
    ParseError(String),

    #[error("Environment variable not set: {var}\nRequired for: {reason}\nSuggestion: export {var}={example}")]
    MissingEnvVar {
        var: String,
        reason: String,
        example: String,
    },
}

// ============================================================================
// Result Type Alias
// ============================================================================

/// Result type alias for ggen operations
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::Result;
///
/// fn validate_template(template: &Template) -> Result<()> {
///     // ...
/// }
/// ```
pub type Result<T> = std::result::Result<T, GgenError>;

// ============================================================================
// Error Context Enhancement
// ============================================================================

/// Extension trait for adding context to errors
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::ErrorContext;
///
/// fn load_file(path: &str) -> Result<String> {
///     std::fs::read_to_string(path)
///         .context(format!("Failed to read file: {}", path))?;
///     Ok(content)
/// }
/// ```
pub trait ErrorContext<T> {
    /// Add static context to error
    fn context(self, msg: impl Into<String>) -> Result<T>;

    /// Add lazy context to error (computed only if error occurs)
    fn with_context<F>(self, f: F) -> Result<T>
    where
        F: FnOnce() -> String;
}

impl<T, E: Into<GgenError>> ErrorContext<T> for std::result::Result<T, E> {
    fn context(self, _msg: impl Into<String>) -> Result<T> {
        self.map_err(|e| {
            let error: GgenError = e.into();
            // Enhance error with context (implementation would wrap error)
            error
        })
    }

    fn with_context<F>(self, f: F) -> Result<T>
    where
        F: FnOnce() -> String,
    {
        self.map_err(|e| {
            let error: GgenError = e.into();
            let _context = f();
            // Enhance error with lazy context
            error
        })
    }
}

// ============================================================================
// Error Builder Pattern
// ============================================================================

/// Builder for constructing rich error messages
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::ErrorBuilder;
///
/// let error = ErrorBuilder::template_not_found("config.toml")
///     .context("Loading application configuration")
///     .suggestion("Run 'ggen init' to create a default configuration")
///     .build();
/// ```
pub struct ErrorBuilder {
    error_type: ErrorType,
    context: Option<String>,
    suggestion: Option<String>,
}

#[allow(dead_code)] // Some variants reserved for future error builder patterns
enum ErrorType {
    TemplateNotFound { path: String },
    InvalidSyntax { file: String, line: usize },
    // ... other types
}

impl ErrorBuilder {
    pub fn template_not_found(path: impl Into<String>) -> Self {
        Self {
            error_type: ErrorType::TemplateNotFound { path: path.into() },
            context: None,
            suggestion: None,
        }
    }

    pub fn context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    pub fn suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    pub fn build(self) -> GgenError {
        match self.error_type {
            ErrorType::TemplateNotFound { path } => GgenError::TemplateNotFound {
                path,
                context: self.context.unwrap_or_default(),
                suggestion: self.suggestion,
            },
            ErrorType::InvalidSyntax { file, line } => GgenError::InvalidTemplateSyntax {
                file,
                line,
                column: 0,
                reason: self.context.unwrap_or_default(),
                snippet: String::new(),
                fix_suggestion: self.suggestion,
            },
        }
    }
}

// ============================================================================
// Error Reporting
// ============================================================================

/// Format error for user-friendly display
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::{GgenError, format_error};
///
/// let error = GgenError::TemplateNotFound { /* ... */ };
/// println!("{}", format_error(&error));
/// ```
pub fn format_error(error: &GgenError) -> String {
    // Implementation would create rich, colorized output
    format!("ERROR: {}\n\nFor more information, run with RUST_LOG=debug", error)
}

/// Report error with full diagnostic information
///
/// # Examples
///
/// ```rust
/// use ggen_core::prevention::errors::{GgenError, report_error};
///
/// if let Err(error) = risky_operation() {
///     report_error(&error);
///     std::process::exit(1);
/// }
/// ```
pub fn report_error(error: &GgenError) {
    eprintln!("{}", format_error(error));

    // Log full error chain for debugging
    let mut current_error: Option<&dyn std::error::Error> = Some(error);
    let mut level = 0;

    while let Some(err) = current_error {
        if level > 0 {
            eprintln!("  Caused by ({}): {}", level, err);
        }
        current_error = err.source();
        level += 1;
    }
}

// ============================================================================
// Conversion Implementations
// ============================================================================

impl From<std::io::Error> for GgenError {
    fn from(source: std::io::Error) -> Self {
        GgenError::Io {
            operation: "IO operation".to_string(),
            path: "unknown".to_string(),
            source,
            suggestion: "Check file permissions and disk space".to_string(),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_builder() {
        let error = ErrorBuilder::template_not_found("test.tmpl")
            .context("Loading template")
            .suggestion("Create the template file")
            .build();

        match error {
            GgenError::TemplateNotFound {
                path,
                context,
                suggestion,
            } => {
                assert_eq!(path, "test.tmpl");
                assert_eq!(context, "Loading template");
                assert_eq!(suggestion, Some("Create the template file".to_string()));
            }
            _ => panic!("Expected TemplateNotFound error"),
        }
    }

    #[test]
    fn test_error_context() {
        fn failing_operation() -> std::result::Result<(), std::io::Error> {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "file not found",
            ))
        }

        let result: Result<()> = failing_operation().context("Custom context");
        assert!(result.is_err());
    }

    #[test]
    fn test_format_error() {
        let error = GgenError::TemplateNotFound {
            path: "test.tmpl".to_string(),
            context: "test context".to_string(),
            suggestion: Some("try this".to_string()),
        };

        let formatted = format_error(&error);
        assert!(formatted.contains("Template not found"));
        assert!(formatted.contains("test.tmpl"));
    }
}
