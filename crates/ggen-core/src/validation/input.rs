//! Comprehensive input validation framework for Week 4 security hardening
//!
//! This module provides a composable, type-safe validation system for all input types:
//! - String validation (length, pattern, charset, format)
//! - Path validation (SafePath integration, extension whitelist)
//! - URL validation (scheme whitelist, domain validation)
//! - Numeric validation (range, precision, positive/negative)
//!
//! ## Design Principles
//!
//! - **Type-first**: Validation rules are types, compiler verifies correctness
//! - **Zero-cost**: Composable validators with zero-cost abstractions
//! - **Result<T, E>**: All validation returns Result, no unwrap/expect
//! - **Composable**: Validators combine via AND, OR, NOT operators
//!
//! ## Example
//!
//! ```rust
//! use ggen_core::validation::input::{StringValidator, LengthRule, PatternRule};
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Validate username: 3-32 chars, alphanumeric + underscore
//! let username_validator = StringValidator::new()
//!     .with_length(3, 32)
//!     .with_pattern(r"^[a-zA-Z0-9_]+$");
//!
//! let result = username_validator.validate("alice_123")?;
//! assert_eq!(result, "alice_123");
//!
//! // Composable validators
//! let email_validator = StringValidator::new()
//!     .with_length(5, 254)
//!     .with_pattern(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$");
//!
//! let result = email_validator.validate("alice@example.com")?;
//! assert_eq!(result, "alice@example.com");
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result as GgenResult};
use regex::Regex;
use std::fmt;
use std::path::{Path, PathBuf};
use url::Url;

/// Validation error with detailed context
#[derive(Debug, thiserror::Error)]
pub enum InputValidationError {
    #[error("Length validation failed: {field} has length {actual}, expected {constraint}")]
    LengthViolation {
        field: String,
        actual: usize,
        constraint: String,
    },

    #[error("Pattern validation failed: {field} does not match pattern {pattern}")]
    PatternViolation { field: String, pattern: String },

    #[error("Charset validation failed: {field} contains invalid characters")]
    CharsetViolation { field: String },

    #[error("Format validation failed: {field} has invalid format: {reason}")]
    FormatViolation { field: String, reason: String },

    #[error("Range validation failed: {field} value {actual} not in range {constraint}")]
    RangeViolation {
        field: String,
        actual: String,
        constraint: String,
    },

    #[error("Whitelist validation failed: {field} value '{value}' not in whitelist")]
    WhitelistViolation { field: String, value: String },

    #[error("Blacklist validation failed: {field} value '{value}' is blacklisted")]
    BlacklistViolation { field: String, value: String },

    #[error("Path validation failed: {reason}")]
    PathViolation { reason: String },

    #[error("URL validation failed: {reason}")]
    UrlViolation { reason: String },

    #[error("Empty input: {field} cannot be empty")]
    EmptyInput { field: String },

    #[error("Composite validation failed: {reason}")]
    CompositeViolation { reason: String },

    #[error("Regex compilation failed: {0}")]
    RegexError(String),
}

impl From<InputValidationError> for Error {
    fn from(err: InputValidationError) -> Self {
        Error::new(&err.to_string())
    }
}

/// Core validation trait for composable validators
///
/// All validators implement this trait, enabling composition via AND, OR, NOT.
pub trait ValidationRule<T>: Send + Sync {
    /// Validate input and return validated value or error
    fn validate(&self, input: &T, field_name: &str) -> GgenResult<T>;

    /// Combine with another validator using AND logic
    fn and<R: ValidationRule<T> + 'static>(self, other: R) -> AndRule<T>
    where
        Self: Sized + 'static,
    {
        AndRule {
            left: Box::new(self),
            right: Box::new(other),
        }
    }

    /// Combine with another validator using OR logic
    fn or<R: ValidationRule<T> + 'static>(self, other: R) -> OrRule<T>
    where
        Self: Sized + 'static,
    {
        OrRule {
            left: Box::new(self),
            right: Box::new(other),
        }
    }

    /// Negate this validator using NOT logic
    fn not(self) -> NotRule<T>
    where
        Self: Sized + 'static,
    {
        NotRule {
            inner: Box::new(self),
        }
    }
}

// =============================================================================
// Composite Validators (AND, OR, NOT)
// =============================================================================

/// AND composition: both validators must pass
pub struct AndRule<T> {
    left: Box<dyn ValidationRule<T>>,
    right: Box<dyn ValidationRule<T>>,
}

impl<T: Clone> ValidationRule<T> for AndRule<T> {
    fn validate(&self, input: &T, field_name: &str) -> GgenResult<T> {
        let validated_left = self.left.validate(input, field_name)?;
        self.right.validate(&validated_left, field_name)
    }
}

/// OR composition: at least one validator must pass
pub struct OrRule<T> {
    left: Box<dyn ValidationRule<T>>,
    right: Box<dyn ValidationRule<T>>,
}

impl<T: Clone> ValidationRule<T> for OrRule<T> {
    fn validate(&self, input: &T, field_name: &str) -> GgenResult<T> {
        match self.left.validate(input, field_name) {
            Ok(validated) => Ok(validated),
            Err(left_err) => match self.right.validate(input, field_name) {
                Ok(validated) => Ok(validated),
                Err(right_err) => Err(InputValidationError::CompositeViolation {
                    reason: format!("OR failed: {} AND {}", left_err, right_err),
                }
                .into()),
            },
        }
    }
}

/// NOT composition: validator must fail
pub struct NotRule<T> {
    inner: Box<dyn ValidationRule<T>>,
}

impl<T: Clone> ValidationRule<T> for NotRule<T> {
    fn validate(&self, input: &T, field_name: &str) -> GgenResult<T> {
        match self.inner.validate(input, field_name) {
            Ok(_) => Err(InputValidationError::CompositeViolation {
                reason: "NOT validation failed: inner validation succeeded".to_string(),
            }
            .into()),
            Err(_) => Ok(input.clone()),
        }
    }
}

// =============================================================================
// String Validators
// =============================================================================

/// Length validation rule for strings
#[derive(Clone)]
pub struct LengthRule {
    min: Option<usize>,
    max: Option<usize>,
}

impl LengthRule {
    pub fn new(min: Option<usize>, max: Option<usize>) -> Self {
        Self { min, max }
    }

    pub fn exact(length: usize) -> Self {
        Self {
            min: Some(length),
            max: Some(length),
        }
    }

    pub fn min(min: usize) -> Self {
        Self {
            min: Some(min),
            max: None,
        }
    }

    pub fn max(max: usize) -> Self {
        Self {
            min: None,
            max: Some(max),
        }
    }

    pub fn range(min: usize, max: usize) -> Self {
        Self {
            min: Some(min),
            max: Some(max),
        }
    }
}

impl ValidationRule<String> for LengthRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        let len = input.len();

        if let Some(min) = self.min {
            if len < min {
                return Err(InputValidationError::LengthViolation {
                    field: field_name.to_string(),
                    actual: len,
                    constraint: format!("min {}", min),
                }
                .into());
            }
        }

        if let Some(max) = self.max {
            if len > max {
                return Err(InputValidationError::LengthViolation {
                    field: field_name.to_string(),
                    actual: len,
                    constraint: format!("max {}", max),
                }
                .into());
            }
        }

        Ok(input.clone())
    }
}

/// Pattern validation rule using regex
#[derive(Clone)]
pub struct PatternRule {
    pattern: String,
    regex: Regex,
}

impl PatternRule {
    pub fn new(pattern: &str) -> GgenResult<Self> {
        let regex =
            Regex::new(pattern).map_err(|e| InputValidationError::RegexError(e.to_string()))?;

        Ok(Self {
            pattern: pattern.to_string(),
            regex,
        })
    }
}

impl ValidationRule<String> for PatternRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        if !self.regex.is_match(input) {
            return Err(InputValidationError::PatternViolation {
                field: field_name.to_string(),
                pattern: self.pattern.clone(),
            }
            .into());
        }

        Ok(input.clone())
    }
}

/// Charset validation rule - only allowed characters
#[derive(Clone)]
pub struct CharsetRule {
    allowed_chars: fn(char) -> bool,
    description: String,
}

impl CharsetRule {
    pub fn new(allowed_chars: fn(char) -> bool, description: &str) -> Self {
        Self {
            allowed_chars,
            description: description.to_string(),
        }
    }

    pub fn alphanumeric() -> Self {
        Self::new(|c| c.is_alphanumeric(), "alphanumeric")
    }

    pub fn alphanumeric_with_underscore() -> Self {
        Self::new(
            |c| c.is_alphanumeric() || c == '_',
            "alphanumeric with underscore",
        )
    }

    pub fn alphanumeric_with_hyphen() -> Self {
        Self::new(
            |c| c.is_alphanumeric() || c == '-',
            "alphanumeric with hyphen",
        )
    }

    pub fn identifier() -> Self {
        Self::new(
            |c| c.is_alphanumeric() || c == '_' || c == '-',
            "identifier (alphanumeric, underscore, hyphen)",
        )
    }

    pub fn ascii_printable() -> Self {
        Self::new(|c| c.is_ascii() && !c.is_control(), "ASCII printable")
    }
}

impl ValidationRule<String> for CharsetRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        // Suppress warning about unused description field
        let _description = &self.description;

        if !input.chars().all(self.allowed_chars) {
            return Err(InputValidationError::CharsetViolation {
                field: field_name.to_string(),
            }
            .into());
        }

        Ok(input.clone())
    }
}

/// Format validation rule - predefined formats
#[derive(Clone)]
pub enum FormatRule {
    Email,
    Uuid,
    Semver,
    IpAddress,
    Hostname,
}

impl FormatRule {
    fn email_pattern() -> &'static str {
        r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
    }

    fn uuid_pattern() -> &'static str {
        r"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
    }

    fn semver_pattern() -> &'static str {
        r"^\d+\.\d+\.\d+(-[a-zA-Z0-9.-]+)?(\+[a-zA-Z0-9.-]+)?$"
    }

    fn ip_pattern() -> &'static str {
        r"^(\d{1,3}\.){3}\d{1,3}$"
    }

    fn hostname_pattern() -> &'static str {
        r"^[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(\.[a-zA-Z0-9]([a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    }
}

impl ValidationRule<String> for FormatRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        let (pattern, format_name) = match self {
            FormatRule::Email => (Self::email_pattern(), "email"),
            FormatRule::Uuid => (Self::uuid_pattern(), "UUID"),
            FormatRule::Semver => (Self::semver_pattern(), "semver"),
            FormatRule::IpAddress => (Self::ip_pattern(), "IP address"),
            FormatRule::Hostname => (Self::hostname_pattern(), "hostname"),
        };

        let regex =
            Regex::new(pattern).map_err(|e| InputValidationError::RegexError(e.to_string()))?;

        if !regex.is_match(input) {
            return Err(InputValidationError::FormatViolation {
                field: field_name.to_string(),
                reason: format!("does not match {} format", format_name),
            }
            .into());
        }

        Ok(input.clone())
    }
}

/// Whitelist validation rule - value must be in allowed list
#[derive(Clone)]
pub struct WhitelistRule {
    allowed: Vec<String>,
}

impl WhitelistRule {
    pub fn new(allowed: Vec<String>) -> Self {
        Self { allowed }
    }
}

impl ValidationRule<String> for WhitelistRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        if !self.allowed.contains(input) {
            return Err(InputValidationError::WhitelistViolation {
                field: field_name.to_string(),
                value: input.clone(),
            }
            .into());
        }

        Ok(input.clone())
    }
}

/// Blacklist validation rule - value must NOT be in forbidden list
#[derive(Clone)]
pub struct BlacklistRule {
    forbidden: Vec<String>,
}

impl BlacklistRule {
    pub fn new(forbidden: Vec<String>) -> Self {
        Self { forbidden }
    }
}

impl ValidationRule<String> for BlacklistRule {
    fn validate(&self, input: &String, field_name: &str) -> GgenResult<String> {
        if self.forbidden.contains(input) {
            return Err(InputValidationError::BlacklistViolation {
                field: field_name.to_string(),
                value: input.clone(),
            }
            .into());
        }

        Ok(input.clone())
    }
}

/// Fluent builder for string validation
pub struct StringValidator {
    rules: Vec<Box<dyn ValidationRule<String>>>,
}

impl StringValidator {
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    pub fn with_length(mut self, min: usize, max: usize) -> Self {
        self.rules.push(Box::new(LengthRule::range(min, max)));
        self
    }

    pub fn with_min_length(mut self, min: usize) -> Self {
        self.rules.push(Box::new(LengthRule::min(min)));
        self
    }

    pub fn with_max_length(mut self, max: usize) -> Self {
        self.rules.push(Box::new(LengthRule::max(max)));
        self
    }

    pub fn with_pattern(mut self, pattern: &str) -> Self {
        if let Ok(rule) = PatternRule::new(pattern) {
            self.rules.push(Box::new(rule));
        }
        self
    }

    pub fn with_charset(mut self, rule: CharsetRule) -> Self {
        self.rules.push(Box::new(rule));
        self
    }

    pub fn with_format(mut self, format: FormatRule) -> Self {
        self.rules.push(Box::new(format));
        self
    }

    pub fn with_whitelist(mut self, allowed: Vec<String>) -> Self {
        self.rules.push(Box::new(WhitelistRule::new(allowed)));
        self
    }

    pub fn with_blacklist(mut self, forbidden: Vec<String>) -> Self {
        self.rules.push(Box::new(BlacklistRule::new(forbidden)));
        self
    }

    pub fn validate(&self, input: &str) -> GgenResult<String> {
        let mut validated = input.to_string();

        for rule in &self.rules {
            validated = rule.validate(&validated, "input")?;
        }

        Ok(validated)
    }
}

impl Default for StringValidator {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Path Validators
// =============================================================================

/// Path validator with extension whitelist and traversal prevention
pub struct PathValidatorRule {
    allowed_extensions: Option<Vec<String>>,
    prevent_traversal: bool,
    base_dir: Option<PathBuf>,
    max_length: usize,
}

impl PathValidatorRule {
    pub fn new() -> Self {
        Self {
            allowed_extensions: None,
            prevent_traversal: true,
            base_dir: None,
            max_length: 4096,
        }
    }

    pub fn with_extensions(mut self, extensions: Vec<String>) -> Self {
        self.allowed_extensions = Some(extensions);
        self
    }

    pub fn with_base_dir(mut self, base: PathBuf) -> Self {
        self.base_dir = Some(base);
        self
    }

    pub fn allow_traversal(mut self) -> Self {
        self.prevent_traversal = false;
        self
    }

    pub fn with_max_length(mut self, max_length: usize) -> Self {
        self.max_length = max_length;
        self
    }

    pub fn validate(&self, path: &Path) -> GgenResult<PathBuf> {
        let path_str = path.to_string_lossy();

        // Check length
        if path_str.len() > self.max_length {
            return Err(InputValidationError::PathViolation {
                reason: format!(
                    "Path length {} exceeds maximum {}",
                    path_str.len(),
                    self.max_length
                ),
            }
            .into());
        }

        // Check for traversal
        if self.prevent_traversal {
            for component in path.components() {
                let component_str = component.as_os_str().to_string_lossy();
                if component_str == ".." {
                    return Err(InputValidationError::PathViolation {
                        reason: "Path traversal detected (..)".to_string(),
                    }
                    .into());
                }
            }
        }

        // Check extension whitelist
        if let Some(ref allowed_exts) = self.allowed_extensions {
            if let Some(ext) = path.extension() {
                let ext_str = ext.to_string_lossy().to_string();
                if !allowed_exts.contains(&ext_str) {
                    return Err(InputValidationError::PathViolation {
                        reason: format!("Extension '{}' not in whitelist", ext_str),
                    }
                    .into());
                }
            } else {
                return Err(InputValidationError::PathViolation {
                    reason: "No file extension found".to_string(),
                }
                .into());
            }
        }

        // Check base directory constraint
        if let Some(ref base) = self.base_dir {
            let abs_path = if path.is_relative() {
                base.join(path)
            } else {
                path.to_path_buf()
            };

            if !abs_path.starts_with(base) {
                return Err(InputValidationError::PathViolation {
                    reason: format!(
                        "Path '{}' escapes base directory '{}'",
                        abs_path.display(),
                        base.display()
                    ),
                }
                .into());
            }
        }

        Ok(path.to_path_buf())
    }
}

impl Default for PathValidatorRule {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// URL Validators
// =============================================================================

/// URL validator with scheme and domain whitelists
pub struct UrlValidator {
    allowed_schemes: Option<Vec<String>>,
    allowed_domains: Option<Vec<String>>,
    require_https: bool,
}

impl UrlValidator {
    pub fn new() -> Self {
        Self {
            allowed_schemes: None,
            allowed_domains: None,
            require_https: false,
        }
    }

    pub fn with_schemes(mut self, schemes: Vec<String>) -> Self {
        self.allowed_schemes = Some(schemes);
        self
    }

    pub fn with_domains(mut self, domains: Vec<String>) -> Self {
        self.allowed_domains = Some(domains);
        self
    }

    pub fn require_https(mut self) -> Self {
        self.require_https = true;
        self
    }

    pub fn validate(&self, url_str: &str) -> GgenResult<Url> {
        let url = Url::parse(url_str).map_err(|e| InputValidationError::UrlViolation {
            reason: format!("Invalid URL: {}", e),
        })?;

        // Check scheme
        if self.require_https && url.scheme() != "https" {
            return Err(InputValidationError::UrlViolation {
                reason: "HTTPS required".to_string(),
            }
            .into());
        }

        if let Some(ref allowed_schemes) = self.allowed_schemes {
            if !allowed_schemes.contains(&url.scheme().to_string()) {
                return Err(InputValidationError::UrlViolation {
                    reason: format!("Scheme '{}' not in whitelist", url.scheme()),
                }
                .into());
            }
        }

        // Check domain
        if let Some(ref allowed_domains) = self.allowed_domains {
            if let Some(host) = url.host_str() {
                if !allowed_domains.iter().any(|d| host.ends_with(d)) {
                    return Err(InputValidationError::UrlViolation {
                        reason: format!("Domain '{}' not in whitelist", host),
                    }
                    .into());
                }
            } else {
                return Err(InputValidationError::UrlViolation {
                    reason: "No host found in URL".to_string(),
                }
                .into());
            }
        }

        Ok(url)
    }
}

impl Default for UrlValidator {
    fn default() -> Self {
        Self::new()
    }
}

// =============================================================================
// Numeric Validators
// =============================================================================

/// Numeric range validator
#[derive(Clone)]
pub struct RangeRule<T: PartialOrd + fmt::Display + Clone> {
    min: Option<T>,
    max: Option<T>,
}

impl<T: PartialOrd + fmt::Display + Clone> RangeRule<T> {
    pub fn new(min: Option<T>, max: Option<T>) -> Self {
        Self { min, max }
    }

    pub fn min(min: T) -> Self {
        Self {
            min: Some(min),
            max: None,
        }
    }

    pub fn max(max: T) -> Self {
        Self {
            min: None,
            max: Some(max),
        }
    }

    pub fn between(min: T, max: T) -> Self {
        Self {
            min: Some(min),
            max: Some(max),
        }
    }
}

impl<T: PartialOrd + fmt::Display + Clone + Send + Sync + 'static> ValidationRule<T>
    for RangeRule<T>
{
    fn validate(&self, input: &T, field_name: &str) -> GgenResult<T> {
        if let Some(ref min) = self.min {
            if input < min {
                return Err(InputValidationError::RangeViolation {
                    field: field_name.to_string(),
                    actual: format!("{}", input),
                    constraint: format!(">= {}", min),
                }
                .into());
            }
        }

        if let Some(ref max) = self.max {
            if input > max {
                return Err(InputValidationError::RangeViolation {
                    field: field_name.to_string(),
                    actual: format!("{}", input),
                    constraint: format!("<= {}", max),
                }
                .into());
            }
        }

        Ok(input.clone())
    }
}

/// Positive number validator
#[derive(Clone)]
pub struct PositiveRule;

impl ValidationRule<i64> for PositiveRule {
    fn validate(&self, input: &i64, field_name: &str) -> GgenResult<i64> {
        if *input <= 0 {
            return Err(InputValidationError::RangeViolation {
                field: field_name.to_string(),
                actual: input.to_string(),
                constraint: "> 0".to_string(),
            }
            .into());
        }
        Ok(*input)
    }
}

impl ValidationRule<f64> for PositiveRule {
    fn validate(&self, input: &f64, field_name: &str) -> GgenResult<f64> {
        if *input <= 0.0 {
            return Err(InputValidationError::RangeViolation {
                field: field_name.to_string(),
                actual: input.to_string(),
                constraint: "> 0".to_string(),
            }
            .into());
        }
        Ok(*input)
    }
}

/// Negative number validator
#[derive(Clone)]
pub struct NegativeRule;

impl ValidationRule<i64> for NegativeRule {
    fn validate(&self, input: &i64, field_name: &str) -> GgenResult<i64> {
        if *input >= 0 {
            return Err(InputValidationError::RangeViolation {
                field: field_name.to_string(),
                actual: input.to_string(),
                constraint: "< 0".to_string(),
            }
            .into());
        }
        Ok(*input)
    }
}

impl ValidationRule<f64> for NegativeRule {
    fn validate(&self, input: &f64, field_name: &str) -> GgenResult<f64> {
        if *input >= 0.0 {
            return Err(InputValidationError::RangeViolation {
                field: field_name.to_string(),
                actual: input.to_string(),
                constraint: "< 0".to_string(),
            }
            .into());
        }
        Ok(*input)
    }
}

/// Precision validator for floating-point numbers
#[derive(Clone)]
pub struct PrecisionRule {
    max_decimal_places: usize,
}

impl PrecisionRule {
    pub fn new(max_decimal_places: usize) -> Self {
        Self { max_decimal_places }
    }
}

impl ValidationRule<f64> for PrecisionRule {
    fn validate(&self, input: &f64, field_name: &str) -> GgenResult<f64> {
        let s = format!("{}", input);
        if let Some(decimal_pos) = s.find('.') {
            let decimal_places = s.len() - decimal_pos - 1;
            if decimal_places > self.max_decimal_places {
                return Err(InputValidationError::FormatViolation {
                    field: field_name.to_string(),
                    reason: format!(
                        "Precision {} exceeds maximum {}",
                        decimal_places, self.max_decimal_places
                    ),
                }
                .into());
            }
        }
        Ok(*input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // String validator tests
    #[test]
    fn test_length_rule_valid() {
        let rule = LengthRule::range(3, 10);
        assert!(rule.validate(&"hello".to_string(), "test").is_ok());
    }

    #[test]
    fn test_length_rule_too_short() {
        let rule = LengthRule::range(5, 10);
        assert!(rule.validate(&"hi".to_string(), "test").is_err());
    }

    #[test]
    fn test_length_rule_too_long() {
        let rule = LengthRule::range(3, 5);
        assert!(rule.validate(&"toolong".to_string(), "test").is_err());
    }

    #[test]
    fn test_pattern_rule_valid() {
        let rule = PatternRule::new(r"^\d{3}-\d{4}$").expect("valid regex");
        assert!(rule.validate(&"123-4567".to_string(), "test").is_ok());
    }

    #[test]
    fn test_pattern_rule_invalid() {
        let rule = PatternRule::new(r"^\d{3}-\d{4}$").expect("valid regex");
        assert!(rule.validate(&"invalid".to_string(), "test").is_err());
    }

    #[test]
    fn test_charset_rule_alphanumeric() {
        let rule = CharsetRule::alphanumeric();
        assert!(rule.validate(&"abc123".to_string(), "test").is_ok());
        assert!(rule.validate(&"abc-123".to_string(), "test").is_err());
    }

    #[test]
    fn test_format_rule_email_valid() {
        let rule = FormatRule::Email;
        assert!(rule
            .validate(&"alice@example.com".to_string(), "test")
            .is_ok());
    }

    #[test]
    fn test_format_rule_email_invalid() {
        let rule = FormatRule::Email;
        assert!(rule.validate(&"not-an-email".to_string(), "test").is_err());
    }

    #[test]
    fn test_whitelist_rule_valid() {
        let rule = WhitelistRule::new(vec!["foo".to_string(), "bar".to_string()]);
        assert!(rule.validate(&"foo".to_string(), "test").is_ok());
    }

    #[test]
    fn test_whitelist_rule_invalid() {
        let rule = WhitelistRule::new(vec!["foo".to_string(), "bar".to_string()]);
        assert!(rule.validate(&"baz".to_string(), "test").is_err());
    }

    #[test]
    fn test_blacklist_rule_valid() {
        let rule = BlacklistRule::new(vec!["bad".to_string(), "evil".to_string()]);
        assert!(rule.validate(&"good".to_string(), "test").is_ok());
    }

    #[test]
    fn test_blacklist_rule_invalid() {
        let rule = BlacklistRule::new(vec!["bad".to_string(), "evil".to_string()]);
        assert!(rule.validate(&"bad".to_string(), "test").is_err());
    }

    #[test]
    fn test_string_validator_builder() {
        let validator = StringValidator::new()
            .with_length(3, 32)
            .with_charset(CharsetRule::identifier());

        assert!(validator.validate("user_name-123").is_ok());
        assert!(validator.validate("ab").is_err()); // too short
        assert!(validator.validate("a".repeat(33).as_str()).is_err()); // too long
    }

    // Composite validator tests
    #[test]
    fn test_and_rule() {
        let rule1 = LengthRule::min(3);
        let rule2 = LengthRule::max(10);
        let composite = rule1.and(rule2);

        assert!(composite.validate(&"hello".to_string(), "test").is_ok());
        assert!(composite.validate(&"hi".to_string(), "test").is_err());
    }

    #[test]
    fn test_or_rule() {
        let rule1 = PatternRule::new(r"^\d+$").expect("valid regex");
        let rule2 = PatternRule::new(r"^[a-z]+$").expect("valid regex");
        let composite = rule1.or(rule2);

        assert!(composite.validate(&"123".to_string(), "test").is_ok());
        assert!(composite.validate(&"abc".to_string(), "test").is_ok());
        assert!(composite.validate(&"abc123".to_string(), "test").is_err());
    }

    // Path validator tests
    #[test]
    fn test_path_validator_basic() {
        let validator = PathValidatorRule::new();
        assert!(validator.validate(Path::new("src/main.rs")).is_ok());
    }

    #[test]
    fn test_path_validator_traversal() {
        let validator = PathValidatorRule::new();
        assert!(validator.validate(Path::new("../etc/passwd")).is_err());
    }

    #[test]
    fn test_path_validator_extension_whitelist() {
        let validator =
            PathValidatorRule::new().with_extensions(vec!["rs".to_string(), "toml".to_string()]);

        assert!(validator.validate(Path::new("main.rs")).is_ok());
        assert!(validator.validate(Path::new("config.toml")).is_ok());
        assert!(validator.validate(Path::new("script.sh")).is_err());
    }

    // URL validator tests
    #[test]
    fn test_url_validator_basic() {
        let validator = UrlValidator::new();
        assert!(validator.validate("https://example.com").is_ok());
    }

    #[test]
    fn test_url_validator_require_https() {
        let validator = UrlValidator::new().require_https();
        assert!(validator.validate("https://example.com").is_ok());
        assert!(validator.validate("http://example.com").is_err());
    }

    #[test]
    fn test_url_validator_scheme_whitelist() {
        let validator =
            UrlValidator::new().with_schemes(vec!["https".to_string(), "wss".to_string()]);

        assert!(validator.validate("https://example.com").is_ok());
        assert!(validator.validate("wss://example.com").is_ok());
        assert!(validator.validate("http://example.com").is_err());
    }

    #[test]
    fn test_url_validator_domain_whitelist() {
        let validator = UrlValidator::new()
            .with_domains(vec!["example.com".to_string(), "trusted.org".to_string()]);

        assert!(validator.validate("https://example.com/path").is_ok());
        assert!(validator.validate("https://api.example.com").is_ok());
        assert!(validator.validate("https://untrusted.com").is_err());
    }

    // Numeric validator tests
    #[test]
    fn test_range_rule_int() {
        let rule = RangeRule::between(0, 100);
        assert!(rule.validate(&50, "test").is_ok());
        assert!(rule.validate(&-1, "test").is_err());
        assert!(rule.validate(&101, "test").is_err());
    }

    #[test]
    fn test_range_rule_float() {
        let rule = RangeRule::between(0.0, 1.0);
        assert!(rule.validate(&0.5, "test").is_ok());
        assert!(rule.validate(&-0.1, "test").is_err());
        assert!(rule.validate(&1.1, "test").is_err());
    }

    #[test]
    fn test_positive_rule_int() {
        let rule = PositiveRule;
        assert!(rule.validate(&10, "test").is_ok());
        assert!(rule.validate(&0, "test").is_err());
        assert!(rule.validate(&-10, "test").is_err());
    }

    #[test]
    fn test_negative_rule_int() {
        let rule = NegativeRule;
        assert!(rule.validate(&-10, "test").is_ok());
        assert!(rule.validate(&0, "test").is_err());
        assert!(rule.validate(&10, "test").is_err());
    }

    #[test]
    fn test_precision_rule() {
        let rule = PrecisionRule::new(2);
        assert!(rule.validate(&3.14, "test").is_ok());
        assert!(rule.validate(&3.141, "test").is_err());
    }
}
