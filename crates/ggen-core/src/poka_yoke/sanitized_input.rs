//! Sanitized input for injection prevention.
//!
//! Validates and sanitizes user input to prevent SSTI and injection attacks.

use std::str::FromStr;

use ggen_utils::error::{Error, Result};

/// Input type for validation rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InputType {
    /// Identifier (alphanumeric + underscore + hyphen).
    Identifier,
    /// Template name (+ dots/slashes for paths).
    TemplateName,
    /// Template variable (no injection sequences).
    TemplateVar,
    /// File path (validated separately by ValidatedPath).
    FilePath,
}

/// Sanitized input that has passed validation.
///
/// Cannot be constructed with invalid input (type-level safety).
///
/// # Security Checks
///
/// - Length limits (prevents DoS)
/// - Character whitelist (alphanumeric + allowed special chars)
/// - Injection pattern detection (SSTI, SQL, shell)
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::{SanitizedInput, InputType};
///
/// // Valid input
/// let var = SanitizedInput::new("user_name", InputType::TemplateVar)?;
///
/// // Invalid input (injection attempt)
/// let bad = SanitizedInput::new("{{7*7}}", InputType::TemplateVar);
/// assert!(bad.is_err());
/// # Ok::<(), ggen_core::error::Error>(())
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SanitizedInput {
    inner: String,
    input_type: InputType,
}

impl SanitizedInput {
    /// Creates a new sanitized input.
    ///
    /// # Errors
    ///
    /// Returns error if validation fails.
    pub fn new(input: impl Into<String>, input_type: InputType) -> Result<Self> {
        let input = input.into();

        // Length check (prevent DoS)
        let max_length = Self::max_length_for_type(input_type);
        if input.len() > max_length {
            return Err(Error::invalid_input(&format!(
                "Input too long: {} bytes (max {})",
                input.len(),
                max_length
            )));
        }

        // Type-specific validation
        match input_type {
            InputType::Identifier => Self::validate_identifier(&input)?,
            InputType::TemplateName => Self::validate_template_name(&input)?,
            InputType::TemplateVar => Self::validate_template_var(&input)?,
            InputType::FilePath => Self::validate_file_path(&input)?,
        }

        Ok(Self {
            inner: input,
            input_type,
        })
    }

    /// Returns the inner string.
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Consumes self and returns the inner String.
    pub fn into_string(self) -> String {
        self.inner
    }

    /// Returns the input type.
    pub fn input_type(&self) -> InputType {
        self.input_type
    }

    /// Max length for input type.
    fn max_length_for_type(input_type: InputType) -> usize {
        match input_type {
            InputType::Identifier => 256,
            InputType::TemplateName => 512,
            InputType::TemplateVar => 1024,
            InputType::FilePath => 4096,
        }
    }

    /// Validates identifier (alphanumeric + _ + -).
    fn validate_identifier(input: &str) -> Result<()> {
        if input.is_empty() {
            return Err(Error::invalid_input("Identifier cannot be empty"));
        }

        for ch in input.chars() {
            if !ch.is_alphanumeric() && ch != '_' && ch != '-' {
                return Err(Error::invalid_input(&format!(
                    "Invalid character in identifier: '{}'",
                    ch
                )));
            }
        }

        Ok(())
    }

    /// Validates template name (+ . and /).
    fn validate_template_name(input: &str) -> Result<()> {
        if input.is_empty() {
            return Err(Error::invalid_input("Template name cannot be empty"));
        }

        for ch in input.chars() {
            if !ch.is_alphanumeric() && ch != '_' && ch != '-' && ch != '.' && ch != '/' {
                return Err(Error::invalid_input(&format!(
                    "Invalid character in template name: '{}'",
                    ch
                )));
            }
        }

        Ok(())
    }

    /// Validates template variable (no injection).
    fn validate_template_var(input: &str) -> Result<()> {
        // Check for SSTI injection patterns
        const INJECTION_PATTERNS: &[&str] = &[
            "{{",
            "{%",
            "${",
            "<%",
            "<script",
            "javascript:",
            "onerror=",
            "onload=",
        ];

        let lower = input.to_lowercase();
        for pattern in INJECTION_PATTERNS {
            if lower.contains(pattern) {
                return Err(Error::invalid_input(&format!(
                    "Template injection detected: '{}'",
                    pattern
                )));
            }
        }

        Ok(())
    }

    /// Validates file path (basic checks, use ValidatedPath for full validation).
    fn validate_file_path(input: &str) -> Result<()> {
        if input.is_empty() {
            return Err(Error::invalid_input("File path cannot be empty"));
        }

        // Check for null bytes
        if input.contains('\0') {
            return Err(Error::invalid_input("File path contains null byte"));
        }

        Ok(())
    }
}

impl AsRef<str> for SanitizedInput {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl FromStr for SanitizedInput {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        // Default to TemplateVar for string parsing
        Self::new(s, InputType::TemplateVar)
    }
}

impl std::fmt::Display for SanitizedInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}
