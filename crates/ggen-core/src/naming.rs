//! # Naming Convention Validator (Phase 3)
//!
//! This module provides validation for canonical naming conventions used throughout ggen.
//! Enforces rules for types, functions, CLI commands, handlers, and error types.
//!
//! See `.specify/naming-conventions-canonical.ttl` for authoritative naming rules.

use ggen_utils::error::{Error, Result};
use regex::Regex;
use std::fmt;

/// Represents naming convention validation errors
#[derive(Debug, Clone)]
pub struct NamingError {
    message: String,
    violation_type: ViolationType,
    suggestion: Option<String>,
}

impl NamingError {
    /// Create a new naming error with violation type and suggestion
    pub fn new(violation_type: ViolationType, message: String, suggestion: Option<String>) -> Self {
        Self {
            message,
            violation_type,
            suggestion,
        }
    }

    /// Get the violation type
    pub fn violation_type(&self) -> &ViolationType {
        &self.violation_type
    }

    /// Get optional suggestion for correction
    pub fn suggestion(&self) -> Option<&str> {
        self.suggestion.as_deref()
    }

    /// Get error message
    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for NamingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;
        if let Some(suggestion) = &self.suggestion {
            write!(f, " (suggestion: {})", suggestion)?;
        }
        Ok(())
    }
}

impl std::error::Error for NamingError {}

/// Categorizes naming violations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ViolationType {
    /// Type name should be PascalCase
    InvalidTypeName,
    /// Function name should be snake_case
    InvalidFunctionName,
    /// Error type should be PascalCase + Error suffix
    InvalidErrorName,
    /// CLI command should be kebab-case
    InvalidCommandName,
    /// Handler should follow handle_X pattern
    InvalidHandlerName,
    /// Module/file should be snake_case
    InvalidModuleName,
}

impl fmt::Display for ViolationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ViolationType::InvalidTypeName => write!(f, "InvalidTypeName"),
            ViolationType::InvalidFunctionName => write!(f, "InvalidFunctionName"),
            ViolationType::InvalidErrorName => write!(f, "InvalidErrorName"),
            ViolationType::InvalidCommandName => write!(f, "InvalidCommandName"),
            ViolationType::InvalidHandlerName => write!(f, "InvalidHandlerName"),
            ViolationType::InvalidModuleName => write!(f, "InvalidModuleName"),
        }
    }
}

/// Validates naming conventions according to canonical rules
pub struct NamingValidator {
    type_pattern: Regex,
    function_pattern: Regex,
    error_pattern: Regex,
    command_pattern: Regex,
    handler_pattern: Regex,
    module_pattern: Regex,
}

impl NamingValidator {
    /// Create a new naming validator
    pub fn new() -> Result<Self> {
        Ok(Self {
            // PascalCase: starts with uppercase, no separators
            type_pattern: Regex::new(r"^[A-Z][a-zA-Z0-9]*$").map_err(|e| {
                Error::invalid_input(format!("Failed to compile type pattern: {}", e))
            })?,
            // snake_case: lowercase with optional underscores
            function_pattern: Regex::new(r"^[a-z][a-z0-9]*(_[a-z0-9]+)*$").map_err(|e| {
                Error::invalid_input(format!("Failed to compile function pattern: {}", e))
            })?,
            // PascalCase + Error suffix
            error_pattern: Regex::new(r"^[A-Z][a-zA-Z0-9]*Error$").map_err(|e| {
                Error::invalid_input(format!("Failed to compile error pattern: {}", e))
            })?,
            // kebab-case: lowercase with hyphens
            command_pattern: Regex::new(r"^[a-z]([a-z0-9-]*[a-z0-9])?$").map_err(|e| {
                Error::invalid_input(format!("Failed to compile command pattern: {}", e))
            })?,
            // handle_X or X_handler
            handler_pattern: Regex::new(r"^(handle_[a-z][a-z0-9_]*|[a-z][a-z0-9_]*_handler)$")
                .map_err(|e| {
                    Error::invalid_input(format!("Failed to compile handler pattern: {}", e))
                })?,
            // snake_case for modules
            module_pattern: Regex::new(r"^[a-z][a-z0-9]*(_[a-z0-9]+)*$").map_err(|e| {
                Error::invalid_input(format!("Failed to compile module pattern: {}", e))
            })?,
        })
    }

    /// Validate a type name (should be PascalCase)
    pub fn validate_type_name(&self, name: &str) -> Result<()> {
        // Reject all-uppercase names with multiple letters (e.g., GENERATEFILEOPTIONS)
        // Single uppercase letters (e.g., "A") are valid
        if name.len() > 1 && name.chars().all(|c| c.is_uppercase() || c.is_numeric()) && name.chars().any(|c| c.is_alphabetic()) {
            return Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidTypeName,
                    format!("Type name '{}' must be PascalCase, not all uppercase", name),
                    Some("Use PascalCase (e.g., GenerateFileOptions instead of GENERATEFILEOPTIONS)".to_string()),
                )
                .to_string(),
            ));
        }

        if self.type_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidTypeName,
                    format!("Type name '{}' is not valid PascalCase", name),
                    Some(format!("Expected PascalCase, got '{}'", name)),
                )
                .to_string(),
            ))
        }
    }

    /// Validate a function name (should be snake_case)
    pub fn validate_function_name(&self, name: &str) -> Result<()> {
        if self.function_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidFunctionName,
                    format!("Function name '{}' is not valid snake_case", name),
                    Some(
                        "snake_case: lowercase with underscores (e.g., execute_validate)"
                            .to_string(),
                    ),
                )
                .to_string(),
            ))
        }
    }

    /// Validate an error type name (should be PascalCase + Error suffix)
    pub fn validate_error_name(&self, name: &str) -> Result<()> {
        if self.error_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidErrorName,
                    format!(
                        "Error type '{}' does not follow PascalCase + Error pattern",
                        name
                    ),
                    Some(
                        "Format: PascalCase + Error suffix (e.g., ConfigError, ValidationError)"
                            .to_string(),
                    ),
                )
                .to_string(),
            ))
        }
    }

    /// Validate a CLI command name (should be kebab-case)
    pub fn validate_command_name(&self, name: &str) -> Result<()> {
        if self.command_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidCommandName,
                    format!("Command name '{}' is not valid kebab-case", name),
                    Some("kebab-case: lowercase with hyphens (e.g., batch-validate)".to_string()),
                )
                .to_string(),
            ))
        }
    }

    /// Validate a handler function name
    /// Valid patterns: handle_sync, marketplace_install_handler, handle_generate
    pub fn validate_handler_name(&self, name: &str) -> Result<()> {
        if self.handler_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidHandlerName,
                    format!("Handler name '{}' does not follow valid pattern", name),
                    Some("Valid: handle_X or X_handler (e.g., handle_sync, marketplace_install_handler)".to_string()),
                ).to_string()
            ))
        }
    }

    /// Validate a module or file name (should be snake_case)
    pub fn validate_module_name(&self, name: &str) -> Result<()> {
        if self.module_pattern.is_match(name) {
            Ok(())
        } else {
            Err(Error::invalid_input(
                NamingError::new(
                    ViolationType::InvalidModuleName,
                    format!("Module name '{}' is not valid snake_case", name),
                    Some("snake_case: lowercase with underscores (e.g., my_module)".to_string()),
                )
                .to_string(),
            ))
        }
    }
}

impl Default for NamingValidator {
    fn default() -> Self {
        Self::new().expect("Failed to create default NamingValidator")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_type_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid types
        assert!(validator.validate_type_name("GenerateFileOptions").is_ok());
        assert!(validator.validate_type_name("ValidateInput").is_ok());
        assert!(validator.validate_type_name("CodeGraphBuilder").is_ok());
        assert!(validator.validate_type_name("MyStruct").is_ok());
        assert!(validator.validate_type_name("A").is_ok());

        // Invalid types
        assert!(validator.validate_type_name("generateFileOptions").is_err());
        assert!(validator
            .validate_type_name("generate_file_options")
            .is_err());
        assert!(validator.validate_type_name("GENERATEFILEOPTIONS").is_err());
    }

    #[test]
    fn test_validate_function_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid functions
        assert!(validator.validate_function_name("execute_validate").is_ok());
        assert!(validator.validate_function_name("generate_file").is_ok());
        assert!(validator.validate_function_name("parse_manifest").is_ok());
        assert!(validator.validate_function_name("with_var").is_ok());
        assert!(validator.validate_function_name("foo").is_ok());

        // Invalid functions
        assert!(validator.validate_function_name("ExecuteValidate").is_err());
        assert!(validator
            .validate_function_name("EXECUTE_VALIDATE")
            .is_err());
        assert!(validator
            .validate_function_name("_execute_validate")
            .is_err());
    }

    #[test]
    fn test_validate_error_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid errors
        assert!(validator.validate_error_name("ConfigError").is_ok());
        assert!(validator.validate_error_name("ValidationError").is_ok());
        assert!(validator.validate_error_name("NamingError").is_ok());
        assert!(validator.validate_error_name("GenerationError").is_ok());

        // Invalid errors
        assert!(validator.validate_error_name("ConfigFailure").is_err());
        assert!(validator.validate_error_name("config_error").is_err());
        assert!(validator.validate_error_name("Error").is_err());
        assert!(validator.validate_error_name("ConfigError2").is_err());
    }

    #[test]
    fn test_validate_command_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid commands
        assert!(validator.validate_command_name("sync").is_ok());
        assert!(validator.validate_command_name("validate").is_ok());
        assert!(validator.validate_command_name("batch-validate").is_ok());
        assert!(validator.validate_command_name("list").is_ok());

        // Invalid commands
        assert!(validator.validate_command_name("Sync").is_err());
        assert!(validator.validate_command_name("SYNC").is_err());
        assert!(validator.validate_command_name("Sync-Validate").is_err());
        assert!(validator.validate_command_name("sync_validate").is_err());
    }

    #[test]
    fn test_validate_handler_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid handlers
        assert!(validator.validate_handler_name("handle_sync").is_ok());
        assert!(validator.validate_handler_name("handle_validate").is_ok());
        assert!(validator
            .validate_handler_name("handle_marketplace_install")
            .is_ok());
        assert!(validator
            .validate_handler_name("marketplace_install_handler")
            .is_ok());
        assert!(validator.validate_handler_name("sync_handler").is_ok());

        // Invalid handlers
        assert!(validator.validate_handler_name("Sync").is_err());
        assert!(validator.validate_handler_name("sync").is_err());
        assert!(validator.validate_handler_name("handleSync").is_err());
        assert!(validator.validate_handler_name("_handle_sync").is_err());
    }

    #[test]
    fn test_validate_module_names() {
        let validator = NamingValidator::new().unwrap();

        // Valid modules
        assert!(validator.validate_module_name("audit").is_ok());
        assert!(validator.validate_module_name("validation").is_ok());
        assert!(validator.validate_module_name("naming").is_ok());
        assert!(validator.validate_module_name("my_module").is_ok());

        // Invalid modules
        assert!(validator.validate_module_name("MyModule").is_err());
        assert!(validator.validate_module_name("MY_MODULE").is_err());
        assert!(validator.validate_module_name("my-module").is_err());
    }

    #[test]
    fn test_default_instantiation() {
        let _validator = NamingValidator::default();
        // Should not panic
    }
}
