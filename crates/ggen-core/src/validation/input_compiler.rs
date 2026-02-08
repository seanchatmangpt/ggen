//! Validation rule compiler for declarative rule definitions
//!
//! This module provides a declarative API for defining validation rules
//! and compiling them into efficient validators.
//!
//! ## Example
//!
//! ```rust
//! use ggen_core::validation::input_compiler::{RuleCompiler, RuleDefinition};
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let compiler = RuleCompiler::new();
//!
//! // Define validation rules declaratively
//! let rules = vec![
//!     RuleDefinition::length("username", 3, 32),
//!     RuleDefinition::pattern("username", r"^[a-zA-Z0-9_-]+$"),
//!     RuleDefinition::length("email", 5, 254),
//!     RuleDefinition::format("email", "email"),
//! ];
//!
//! // Compile into efficient validator
//! let validator = compiler.compile(&rules)?;
//!
//! // Validate inputs
//! validator.validate_field("username", "alice")?;
//! validator.validate_field("email", "alice@example.com")?;
//! # Ok(())
//! # }
//! ```

use super::input::{CharsetRule, FormatRule, InputValidationError, StringValidator};
use ggen_utils::error::Result;
use std::collections::HashMap;

/// Rule definition for declarative validation
#[derive(Debug, Clone)]
pub enum RuleDefinition {
    /// Length constraint (field, min, max)
    Length {
        field: String,
        min: usize,
        max: usize,
    },

    /// Pattern constraint (field, regex pattern)
    Pattern { field: String, pattern: String },

    /// Charset constraint (field, charset type)
    Charset { field: String, charset: String },

    /// Format constraint (field, format type)
    Format { field: String, format: String },

    /// Whitelist constraint (field, allowed values)
    Whitelist { field: String, allowed: Vec<String> },

    /// Blacklist constraint (field, forbidden values)
    Blacklist {
        field: String,
        forbidden: Vec<String>,
    },

    /// Composite AND (left, right)
    And {
        left: Box<RuleDefinition>,
        right: Box<RuleDefinition>,
    },

    /// Composite OR (left, right)
    Or {
        left: Box<RuleDefinition>,
        right: Box<RuleDefinition>,
    },

    /// Composite NOT (inner)
    Not { inner: Box<RuleDefinition> },
}

impl RuleDefinition {
    /// Create length constraint
    pub fn length(field: &str, min: usize, max: usize) -> Self {
        Self::Length {
            field: field.to_string(),
            min,
            max,
        }
    }

    /// Create pattern constraint
    pub fn pattern(field: &str, pattern: &str) -> Self {
        Self::Pattern {
            field: field.to_string(),
            pattern: pattern.to_string(),
        }
    }

    /// Create charset constraint
    pub fn charset(field: &str, charset: &str) -> Self {
        Self::Charset {
            field: field.to_string(),
            charset: charset.to_string(),
        }
    }

    /// Create format constraint
    pub fn format(field: &str, format: &str) -> Self {
        Self::Format {
            field: field.to_string(),
            format: format.to_string(),
        }
    }

    /// Create whitelist constraint
    pub fn whitelist(field: &str, allowed: Vec<String>) -> Self {
        Self::Whitelist {
            field: field.to_string(),
            allowed,
        }
    }

    /// Create blacklist constraint
    pub fn blacklist(field: &str, forbidden: Vec<String>) -> Self {
        Self::Blacklist {
            field: field.to_string(),
            forbidden,
        }
    }

    /// Combine with AND
    pub fn and(self, other: RuleDefinition) -> Self {
        Self::And {
            left: Box::new(self),
            right: Box::new(other),
        }
    }

    /// Combine with OR
    pub fn or(self, other: RuleDefinition) -> Self {
        Self::Or {
            left: Box::new(self),
            right: Box::new(other),
        }
    }

    /// Negate with NOT
    pub fn not(self) -> Self {
        Self::Not {
            inner: Box::new(self),
        }
    }

    /// Get field name for this rule
    pub fn field_name(&self) -> Option<&str> {
        match self {
            Self::Length { field, .. }
            | Self::Pattern { field, .. }
            | Self::Charset { field, .. }
            | Self::Format { field, .. }
            | Self::Whitelist { field, .. }
            | Self::Blacklist { field, .. } => Some(field),
            Self::And { left, .. } => left.field_name(),
            Self::Or { left, .. } => left.field_name(),
            Self::Not { inner } => inner.field_name(),
        }
    }
}

/// Compiled validator from rule definitions
pub struct CompiledValidator {
    validators: HashMap<String, StringValidator>,
}

impl CompiledValidator {
    /// Create new compiled validator
    pub fn new(validators: HashMap<String, StringValidator>) -> Self {
        Self { validators }
    }

    /// Validate a field value
    pub fn validate_field(&self, field: &str, value: &str) -> Result<String> {
        if let Some(validator) = self.validators.get(field) {
            validator.validate(value).map_err(|e| {
                InputValidationError::FormatViolation {
                    field: field.to_string(),
                    reason: format!("Validation failed: {}", e),
                }
                .into()
            })
        } else {
            Err(InputValidationError::EmptyInput {
                field: format!("No validator defined for field: {}", field),
            }
            .into())
        }
    }

    /// Validate multiple fields
    pub fn validate_fields(
        &self, fields: &HashMap<String, String>,
    ) -> Result<HashMap<String, String>> {
        let mut validated = HashMap::new();

        for (field, value) in fields {
            let validated_value = self.validate_field(field, value)?;
            validated.insert(field.clone(), validated_value);
        }

        Ok(validated)
    }
}

/// Rule compiler for building validators from definitions
pub struct RuleCompiler {
    // Future: caching, optimization
}

impl RuleCompiler {
    pub fn new() -> Self {
        Self {}
    }

    /// Compile rule definitions into efficient validator
    pub fn compile(&self, rules: &[RuleDefinition]) -> Result<CompiledValidator> {
        let mut validators: HashMap<String, StringValidator> = HashMap::new();

        // Group rules by field
        let mut rules_by_field: HashMap<String, Vec<&RuleDefinition>> = HashMap::new();
        for rule in rules {
            if let Some(field) = rule.field_name() {
                rules_by_field
                    .entry(field.to_string())
                    .or_default()
                    .push(rule);
            }
        }

        // Build validator for each field
        for (field, field_rules) in rules_by_field {
            let mut validator = StringValidator::new();

            for rule in field_rules {
                validator = self.apply_rule(validator, rule)?;
            }

            validators.insert(field, validator);
        }

        Ok(CompiledValidator::new(validators))
    }

    fn apply_rule(
        &self, mut validator: StringValidator, rule: &RuleDefinition,
    ) -> Result<StringValidator> {
        match rule {
            RuleDefinition::Length { min, max, .. } => {
                validator = validator.with_length(*min, *max);
            }
            RuleDefinition::Pattern { pattern, .. } => {
                validator = validator.with_pattern(pattern);
            }
            RuleDefinition::Charset { charset, .. } => {
                let charset_rule = match charset.as_str() {
                    "alphanumeric" => CharsetRule::alphanumeric(),
                    "identifier" => CharsetRule::identifier(),
                    "ascii_printable" => CharsetRule::ascii_printable(),
                    _ => {
                        return Err(InputValidationError::FormatViolation {
                            field: "charset".to_string(),
                            reason: format!("Unknown charset type: {}", charset),
                        }
                        .into())
                    }
                };
                validator = validator.with_charset(charset_rule);
            }
            RuleDefinition::Format { format, .. } => {
                let format_rule = match format.as_str() {
                    "email" => FormatRule::Email,
                    "uuid" => FormatRule::Uuid,
                    "semver" => FormatRule::Semver,
                    "ip" => FormatRule::IpAddress,
                    "hostname" => FormatRule::Hostname,
                    _ => {
                        return Err(InputValidationError::FormatViolation {
                            field: "format".to_string(),
                            reason: format!("Unknown format type: {}", format),
                        }
                        .into())
                    }
                };
                validator = validator.with_format(format_rule);
            }
            RuleDefinition::Whitelist { allowed, .. } => {
                validator = validator.with_whitelist(allowed.clone());
            }
            RuleDefinition::Blacklist { forbidden, .. } => {
                validator = validator.with_blacklist(forbidden.clone());
            }
            RuleDefinition::And { .. } | RuleDefinition::Or { .. } | RuleDefinition::Not { .. } => {
                // Composite rules are not yet supported in builder API
                // This is a limitation of the current implementation
                return Err(InputValidationError::CompositeViolation {
                    reason: "Composite rules (AND/OR/NOT) not yet supported in compiler"
                        .to_string(),
                }
                .into());
            }
        }

        Ok(validator)
    }
}

impl Default for RuleCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rule_definition_length() {
        // Arrange & Act
        let rule = RuleDefinition::length("username", 3, 32);

        // Assert
        assert_eq!(rule.field_name(), Some("username"));
    }

    #[test]
    fn test_rule_definition_pattern() {
        // Arrange & Act
        let rule = RuleDefinition::pattern("email", r"^.+@.+$");

        // Assert
        assert_eq!(rule.field_name(), Some("email"));
    }

    #[test]
    fn test_rule_compiler_single_rule() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![RuleDefinition::length("username", 3, 32)];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");
        let result = validator.validate_field("username", "alice");

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_rule_compiler_multiple_rules_same_field() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![
            RuleDefinition::length("username", 3, 32),
            RuleDefinition::charset("username", "identifier"),
        ];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Assert - valid username
        assert!(validator.validate_field("username", "alice_123").is_ok());

        // Assert - invalid (too short)
        assert!(validator.validate_field("username", "ab").is_err());

        // Assert - invalid (wrong charset)
        assert!(validator.validate_field("username", "alice@123").is_err());
    }

    #[test]
    fn test_rule_compiler_multiple_fields() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![
            RuleDefinition::length("username", 3, 32),
            RuleDefinition::length("email", 5, 254),
            RuleDefinition::format("email", "email"),
        ];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Assert - validate both fields
        assert!(validator.validate_field("username", "alice").is_ok());
        assert!(validator
            .validate_field("email", "alice@example.com")
            .is_ok());
    }

    #[test]
    fn test_rule_compiler_format_email() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![RuleDefinition::format("email", "email")];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Assert
        assert!(validator
            .validate_field("email", "alice@example.com")
            .is_ok());
        assert!(validator.validate_field("email", "not-an-email").is_err());
    }

    #[test]
    fn test_rule_compiler_whitelist() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![RuleDefinition::whitelist(
            "role",
            vec!["admin".to_string(), "user".to_string()],
        )];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Assert
        assert!(validator.validate_field("role", "admin").is_ok());
        assert!(validator.validate_field("role", "user").is_ok());
        assert!(validator.validate_field("role", "guest").is_err());
    }

    #[test]
    fn test_rule_compiler_blacklist() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![RuleDefinition::blacklist(
            "username",
            vec!["admin".to_string(), "root".to_string()],
        )];

        // Act
        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Assert
        assert!(validator.validate_field("username", "alice").is_ok());
        assert!(validator.validate_field("username", "admin").is_err());
        assert!(validator.validate_field("username", "root").is_err());
    }

    #[test]
    fn test_compiled_validator_validate_fields() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![
            RuleDefinition::length("username", 3, 32),
            RuleDefinition::format("email", "email"),
        ];

        let validator = compiler.compile(&rules).expect("compilation succeeds");

        let mut fields = HashMap::new();
        fields.insert("username".to_string(), "alice".to_string());
        fields.insert("email".to_string(), "alice@example.com".to_string());

        // Act
        let result = validator.validate_fields(&fields);

        // Assert
        assert!(result.is_ok());
        let validated = result.unwrap();
        assert_eq!(validated.get("username"), Some(&"alice".to_string()));
        assert_eq!(
            validated.get("email"),
            Some(&"alice@example.com".to_string())
        );
    }

    #[test]
    fn test_compiled_validator_unknown_field() {
        // Arrange
        let compiler = RuleCompiler::new();
        let rules = vec![RuleDefinition::length("username", 3, 32)];

        let validator = compiler.compile(&rules).expect("compilation succeeds");

        // Act
        let result = validator.validate_field("unknown", "value");

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_rule_definition_and() {
        // Arrange & Act
        let rule = RuleDefinition::length("field", 5, 10)
            .and(RuleDefinition::charset("field", "alphanumeric"));

        // Assert
        assert_eq!(rule.field_name(), Some("field"));
    }

    #[test]
    fn test_rule_definition_or() {
        // Arrange & Act
        let rule =
            RuleDefinition::length("field", 5, 10).or(RuleDefinition::length("field", 20, 30));

        // Assert
        assert_eq!(rule.field_name(), Some("field"));
    }

    #[test]
    fn test_rule_definition_not() {
        // Arrange & Act
        let rule = RuleDefinition::length("field", 5, 10).not();

        // Assert
        assert_eq!(rule.field_name(), Some("field"));
    }
}
