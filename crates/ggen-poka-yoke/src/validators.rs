//! Runtime validators as a last resort.
//!
//! Use only when compile-time validation is impossible.
//!
//! # Examples
//!
//! ```
//! use ggen_poka_yoke::validators::{Email, Url, validate_range};
//!
//! let email = Email::new("user@example.com").unwrap();
//! let url = Url::new("https://example.com").unwrap();
//! assert!(validate_range(50, 0, 100).is_ok());
//! ```

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Validation errors.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum ValidationError {
    #[error("empty string not allowed")]
    EmptyString,

    #[error("invalid email format: {0}")]
    InvalidEmail(String),

    #[error("invalid URL format: {0}")]
    InvalidUrl(String),

    #[error("value {value} out of range [{min}, {max}]")]
    OutOfRange { value: i64, min: i64, max: i64 },

    #[error("string too long: {len} > {max}")]
    StringTooLong { len: usize, max: usize },

    #[error("string too short: {len} < {min}")]
    StringTooShort { len: usize, min: usize },

    #[error("invalid pattern: {0}")]
    InvalidPattern(String),

    #[error("custom validation failed: {0}")]
    Custom(String),
}

/// Result type for validation operations.
pub type ValidationResult<T> = Result<T, ValidationError>;

/// Validated email address.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Email(String);

impl Email {
    /// Creates a validated email address.
    ///
    /// Returns `Err` if format is invalid.
    pub fn new(email: impl Into<String>) -> ValidationResult<Self> {
        let email = email.into();

        if email.is_empty() {
            return Err(ValidationError::EmptyString);
        }

        if !email.contains('@') || !email.contains('.') {
            return Err(ValidationError::InvalidEmail(email));
        }

        let parts: Vec<&str> = email.split('@').collect();
        if parts.len() != 2 || parts[0].is_empty() || parts[1].is_empty() {
            return Err(ValidationError::InvalidEmail(email));
        }

        Ok(Self(email))
    }

    /// Returns the email address as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes and returns the inner string.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl std::fmt::Display for Email {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Validated URL.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Url(String);

impl Url {
    /// Creates a validated URL.
    ///
    /// Returns `Err` if format is invalid.
    pub fn new(url: impl Into<String>) -> ValidationResult<Self> {
        let url = url.into();

        if url.is_empty() {
            return Err(ValidationError::EmptyString);
        }

        if !url.starts_with("http://") && !url.starts_with("https://") {
            return Err(ValidationError::InvalidUrl(url));
        }

        Ok(Self(url))
    }

    /// Returns the URL as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes and returns the inner string.
    pub fn into_string(self) -> String {
        self.0
    }

    /// Checks if the URL uses HTTPS.
    pub fn is_https(&self) -> bool {
        self.0.starts_with("https://")
    }
}

impl std::fmt::Display for Url {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Validates that a value is within range.
pub fn validate_range(value: i64, min: i64, max: i64) -> ValidationResult<i64> {
    if value < min || value > max {
        Err(ValidationError::OutOfRange { value, min, max })
    } else {
        Ok(value)
    }
}

/// Validates string length.
pub fn validate_length(s: &str, min: usize, max: usize) -> ValidationResult<()> {
    let len = s.len();
    if len < min {
        Err(ValidationError::StringTooShort { len, min })
    } else if len > max {
        Err(ValidationError::StringTooLong { len, max })
    } else {
        Ok(())
    }
}

/// Validates that a string matches a pattern.
pub fn validate_pattern(s: &str, pattern: &str) -> ValidationResult<()> {
    // Simple pattern matching (basic implementation)
    if pattern.contains('*') {
        // Wildcard pattern
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() == 2 {
            let (prefix, suffix) = (parts[0], parts[1]);
            if s.starts_with(prefix) && s.ends_with(suffix) {
                return Ok(());
            }
        }
    } else if s == pattern {
        return Ok(());
    }

    Err(ValidationError::InvalidPattern(format!(
        "string '{}' does not match pattern '{}'",
        s, pattern
    )))
}

/// Username validator (3-32 chars, alphanumeric + underscore).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Username(String);

impl Username {
    /// Creates a validated username.
    pub fn new(username: impl Into<String>) -> ValidationResult<Self> {
        let username = username.into();

        validate_length(&username, 3, 32)?;

        if !username
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
        {
            return Err(ValidationError::Custom(
                "username must be alphanumeric with underscore/hyphen".to_string(),
            ));
        }

        Ok(Self(username))
    }

    /// Returns the username as a string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes and returns the inner string.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl std::fmt::Display for Username {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Password validator (minimum 8 chars, at least one number and special char).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Password(String);

impl Password {
    /// Creates a validated password.
    pub fn new(password: impl Into<String>) -> ValidationResult<Self> {
        let password = password.into();

        if password.len() < 8 {
            return Err(ValidationError::StringTooShort {
                len: password.len(),
                min: 8,
            });
        }

        let has_digit = password.chars().any(|c| c.is_ascii_digit());
        let has_special = password
            .chars()
            .any(|c| !c.is_alphanumeric() && !c.is_whitespace());

        if !has_digit {
            return Err(ValidationError::Custom(
                "password must contain at least one digit".to_string(),
            ));
        }

        if !has_special {
            return Err(ValidationError::Custom(
                "password must contain at least one special character".to_string(),
            ));
        }

        Ok(Self(password))
    }

    /// Returns the password as a string (use carefully).
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Consumes and returns the inner string.
    pub fn into_string(self) -> String {
        self.0
    }
}

impl std::fmt::Display for Password {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "********")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_email_validation() {
        assert!(Email::new("user@example.com").is_ok());
        assert!(Email::new("test.user@domain.co.uk").is_ok());

        assert!(Email::new("").is_err());
        assert!(Email::new("invalid").is_err());
        assert!(Email::new("@example.com").is_err());
        assert!(Email::new("user@").is_err());
    }

    #[test]
    fn test_url_validation() {
        assert!(Url::new("http://example.com").is_ok());
        assert!(Url::new("https://example.com").is_ok());

        let url = Url::new("https://secure.example.com").unwrap();
        assert!(url.is_https());

        assert!(Url::new("").is_err());
        assert!(Url::new("example.com").is_err());
        assert!(Url::new("ftp://example.com").is_err());
    }

    #[test]
    fn test_range_validation() {
        assert!(validate_range(50, 0, 100).is_ok());
        assert!(validate_range(0, 0, 100).is_ok());
        assert!(validate_range(100, 0, 100).is_ok());

        assert!(validate_range(-1, 0, 100).is_err());
        assert!(validate_range(101, 0, 100).is_err());
    }

    #[test]
    fn test_length_validation() {
        assert!(validate_length("hello", 3, 10).is_ok());
        assert!(validate_length("hi", 3, 10).is_err());
        assert!(validate_length("hello world!", 3, 10).is_err());
    }

    #[test]
    fn test_pattern_validation() {
        assert!(validate_pattern("test", "test").is_ok());
        assert!(validate_pattern("hello_world", "hello*").is_ok());
        assert!(validate_pattern("hello_world", "*world").is_ok());
        assert!(validate_pattern("hello_world", "hello*world").is_ok());

        assert!(validate_pattern("test", "other").is_err());
        assert!(validate_pattern("hello", "world*").is_err());
    }

    #[test]
    fn test_username_validation() {
        assert!(Username::new("user123").is_ok());
        assert!(Username::new("test_user").is_ok());
        assert!(Username::new("user-name").is_ok());

        assert!(Username::new("ab").is_err()); // Too short
        assert!(Username::new("a".repeat(33)).is_err()); // Too long
        assert!(Username::new("user@name").is_err()); // Invalid char
    }

    #[test]
    fn test_password_validation() {
        assert!(Password::new("password123!").is_ok());
        assert!(Password::new("SecureP@ss1").is_ok());

        assert!(Password::new("short1!").is_err()); // Too short
        assert!(Password::new("nodigits!").is_err()); // No digit
        assert!(Password::new("nospecial1").is_err()); // No special char
    }
}
