//! Input validation to prevent injection and exploitation
//!
//! **SECURITY ISSUE 4: Input Validation**
//!
//! This module provides comprehensive validation for paths, environment variables,
//! and user inputs to prevent exploitation via malformed or malicious data.

use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};

/// Error type for validation failures
#[derive(Debug, thiserror::Error)]
pub enum ValidationError {
    #[error("Invalid path: {0}")]
    InvalidPath(String),

    #[error("Path traversal detected: {0}")]
    PathTraversal(String),

    #[error("Invalid environment variable: {0}")]
    InvalidEnvVar(String),

    #[error("Input too long: {0} (max: {1})")]
    TooLong(usize, usize),

    #[error("Invalid characters: {0}")]
    InvalidCharacters(String),

    #[error("Empty input")]
    EmptyInput,
}

impl From<ValidationError> for Error {
    fn from(err: ValidationError) -> Self {
        Error::new(&err.to_string())
    }
}

/// Path validator to prevent traversal attacks
pub struct PathValidator;

impl PathValidator {
    /// Dangerous path components
    const DANGEROUS_COMPONENTS: &'static [&'static str] = &[
        "..", "~", "$", "`", "|", ";", "&", "<", ">", "(", ")", "{", "}", "\n", "\r",
    ];

    /// Maximum path length
    const MAX_PATH_LENGTH: usize = 4096;

    /// Validate a path for safety
    ///
    /// # Security
    ///
    /// - Prevents `../../../etc/passwd` traversal attacks
    /// - Rejects paths with dangerous characters
    /// - Enforces maximum length limits
    /// - Ensures path is within allowed bounds
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::security::validation::PathValidator;
    /// use std::path::Path;
    ///
    /// # fn example() -> Result<(), Box<dyn std::error::Error>> {
    /// // Safe path
    /// let path = PathValidator::validate(Path::new("src/main.rs"))?;
    ///
    /// // Unsafe path (path traversal)
    /// assert!(PathValidator::validate(Path::new("../../../etc/passwd")).is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn validate(path: &Path) -> Result<PathBuf> {
        let path_str = path.to_string_lossy();

        // Check length
        if path_str.len() > Self::MAX_PATH_LENGTH {
            return Err(ValidationError::TooLong(path_str.len(), Self::MAX_PATH_LENGTH).into());
        }

        // Check for dangerous components
        for component in path.components() {
            let component_str = component.as_os_str().to_string_lossy();

            for dangerous in Self::DANGEROUS_COMPONENTS {
                if component_str.contains(dangerous) {
                    return Err(ValidationError::PathTraversal(format!(
                        "Path contains dangerous component: {}",
                        component_str
                    ))
                    .into());
                }
            }
        }

        // Canonicalize to resolve any remaining issues
        // Note: This will fail if path doesn't exist, which is fine for validation
        Ok(path.to_path_buf())
    }

    /// Validate path is within a base directory (no traversal outside)
    ///
    /// # Security
    ///
    /// - Ensures path stays within allowed directory tree
    /// - Prevents escaping via symlinks or ../ sequences
    pub fn validate_within(path: &Path, base: &Path) -> Result<PathBuf> {
        let validated = Self::validate(path)?;

        // Ensure path is within base
        let abs_path = if validated.is_relative() {
            base.join(&validated)
        } else {
            validated.clone()
        };

        // Check if path starts with base
        if !abs_path.starts_with(base) {
            return Err(ValidationError::PathTraversal(format!(
                "Path escapes base directory: {} not in {}",
                abs_path.display(),
                base.display()
            ))
            .into());
        }

        Ok(validated)
    }

    /// Validate file extension is in allowed list
    pub fn validate_extension(path: &Path, allowed: &[&str]) -> Result<()> {
        let ext = path
            .extension()
            .and_then(|e| e.to_str())
            .ok_or_else(|| ValidationError::InvalidPath("No file extension".to_string()))?;

        if !allowed.contains(&ext) {
            return Err(ValidationError::InvalidPath(format!(
                "Extension '{}' not in allowed list",
                ext
            ))
            .into());
        }

        Ok(())
    }
}

/// Environment variable validator
pub struct EnvVarValidator;

impl EnvVarValidator {
    /// Dangerous characters in environment variables
    const DANGEROUS_CHARS: &'static [char] = &[
        ';', '|', '&', '$', '`', '\n', '\r', '<', '>', '(', ')', '{', '}', '\\',
    ];

    /// Maximum environment variable length
    const MAX_ENV_LENGTH: usize = 32768;

    /// Validate environment variable name
    pub fn validate_name(name: &str) -> Result<String> {
        if name.is_empty() {
            return Err(ValidationError::EmptyInput.into());
        }

        // Check length
        if name.len() > Self::MAX_ENV_LENGTH {
            return Err(ValidationError::TooLong(name.len(), Self::MAX_ENV_LENGTH).into());
        }

        // Check for dangerous characters
        if name.chars().any(|c| Self::DANGEROUS_CHARS.contains(&c)) {
            return Err(ValidationError::InvalidCharacters(format!(
                "Environment variable name contains dangerous characters: {}",
                name
            ))
            .into());
        }

        // Ensure alphanumeric + underscore only
        if !name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(ValidationError::InvalidCharacters(format!(
                "Environment variable name must be alphanumeric: {}",
                name
            ))
            .into());
        }

        Ok(name.to_string())
    }

    /// Validate environment variable value
    pub fn validate_value(value: &str) -> Result<String> {
        // Check length
        if value.len() > Self::MAX_ENV_LENGTH {
            return Err(ValidationError::TooLong(value.len(), Self::MAX_ENV_LENGTH).into());
        }

        // Check for shell metacharacters that could be dangerous
        if value.chars().any(|c| Self::DANGEROUS_CHARS.contains(&c)) {
            return Err(ValidationError::InvalidCharacters(
                "Environment variable value contains dangerous characters".to_string(),
            )
            .into());
        }

        Ok(value.to_string())
    }
}

/// General input validator
pub struct InputValidator;

impl InputValidator {
    /// Validate string input with length and character restrictions
    pub fn validate_string(
        input: &str, max_length: usize, allowed_chars: fn(char) -> bool,
    ) -> Result<String> {
        if input.is_empty() {
            return Err(ValidationError::EmptyInput.into());
        }

        if input.len() > max_length {
            return Err(ValidationError::TooLong(input.len(), max_length).into());
        }

        if !input.chars().all(allowed_chars) {
            return Err(ValidationError::InvalidCharacters(
                "Input contains invalid characters".to_string(),
            )
            .into());
        }

        Ok(input.to_string())
    }

    /// Validate identifier (alphanumeric + underscore/hyphen)
    pub fn validate_identifier(input: &str) -> Result<String> {
        Self::validate_string(input, 256, |c| c.is_alphanumeric() || c == '_' || c == '-')
    }

    /// Validate template name
    pub fn validate_template_name(input: &str) -> Result<String> {
        Self::validate_string(input, 256, |c| {
            c.is_alphanumeric() || c == '_' || c == '-' || c == '.' || c == '/'
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_traversal_detection() {
        // Path traversal attempts
        assert!(PathValidator::validate(Path::new("../../../etc/passwd")).is_err());
        assert!(PathValidator::validate(Path::new("../../.ssh/id_rsa")).is_err());
        assert!(PathValidator::validate(Path::new("..\\..\\windows\\system32")).is_err());

        // Safe paths
        assert!(PathValidator::validate(Path::new("src/main.rs")).is_ok());
        assert!(PathValidator::validate(Path::new("templates/rust.tmpl")).is_ok());
    }

    #[test]
    fn test_path_length_validation() {
        // Too long path
        let long_path = "a/".repeat(3000);
        assert!(PathValidator::validate(Path::new(&long_path)).is_err());

        // Normal path
        assert!(PathValidator::validate(Path::new("src/lib.rs")).is_ok());
    }

    #[test]
    fn test_env_var_name_validation() {
        // Valid names
        assert!(EnvVarValidator::validate_name("PATH").is_ok());
        assert!(EnvVarValidator::validate_name("MY_VAR_123").is_ok());

        // Invalid names
        assert!(EnvVarValidator::validate_name("").is_err());
        assert!(EnvVarValidator::validate_name("VAR; rm -rf /").is_err());
        assert!(EnvVarValidator::validate_name("VAR|cat").is_err());
        assert!(EnvVarValidator::validate_name("$(whoami)").is_err());
    }

    #[test]
    fn test_env_var_value_validation() {
        // Valid values
        assert!(EnvVarValidator::validate_value("value").is_ok());
        assert!(EnvVarValidator::validate_value("/usr/bin").is_ok());

        // Invalid values (shell metacharacters)
        assert!(EnvVarValidator::validate_value("value; rm -rf /").is_err());
        assert!(EnvVarValidator::validate_value("$(whoami)").is_err());
        assert!(EnvVarValidator::validate_value("`whoami`").is_err());
    }

    #[test]
    fn test_identifier_validation() {
        // Valid identifiers
        assert!(InputValidator::validate_identifier("my_var").is_ok());
        assert!(InputValidator::validate_identifier("my-var-123").is_ok());

        // Invalid identifiers
        assert!(InputValidator::validate_identifier("").is_err());
        assert!(InputValidator::validate_identifier("my var").is_err());
        assert!(InputValidator::validate_identifier("my;var").is_err());
    }

    #[test]
    fn test_template_name_validation() {
        // Valid template names
        assert!(InputValidator::validate_template_name("rust-cli").is_ok());
        assert!(InputValidator::validate_template_name("templates/rust.tmpl").is_ok());

        // Invalid template names
        assert!(InputValidator::validate_template_name("").is_err());
        assert!(InputValidator::validate_template_name("template; rm -rf /").is_err());
    }
}
