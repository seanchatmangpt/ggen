//! Template validation constraints and rules
//!
//! Defines validation constraints that can be applied to templates
//! to ensure they meet quality and consistency standards.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Template validation constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationConstraint {
    /// Constraint name
    pub name: String,
    /// Constraint description
    pub description: String,
    /// Constraint type
    pub constraint_type: ConstraintType,
    /// Constraint parameters
    pub parameters: HashMap<String, String>,
    /// Severity level
    pub severity: ConstraintSeverity,
}

/// Constraint types for validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConstraintType {
    /// Required field presence
    RequiredField,
    /// Format validation
    Format,
    /// Length constraints
    Length,
    /// Pattern matching
    Pattern,
    /// Custom validation function
    Custom,
}

/// Constraint severity levels
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConstraintSeverity {
    /// Warning - non-blocking issue
    Warning,
    /// Error - blocking issue
    Error,
    /// Critical - system integrity issue
    Critical,
}

/// Constraint validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintResult {
    /// Constraint that was validated
    pub constraint: ValidationConstraint,
    /// Validation passed
    pub passed: bool,
    /// Error message if failed
    pub error_message: Option<String>,
    /// Suggestions for fixing
    pub suggestions: Vec<String>,
}

/// Constraint validator trait
pub trait ConstraintValidator {
    /// Validate a constraint against input
    fn validate(&self, input: &str, constraint: &ValidationConstraint) -> ConstraintResult;

    /// Get validator name
    fn name(&self) -> &str;
}

/// Basic constraint validator implementation
pub struct BasicConstraintValidator;

impl ConstraintValidator for BasicConstraintValidator {
    fn validate(&self, input: &str, constraint: &ValidationConstraint) -> ConstraintResult {
        let passed = match constraint.constraint_type {
            ConstraintType::RequiredField => !input.trim().is_empty(),
            ConstraintType::Length => {
                if let Some(min_len_str) = constraint.parameters.get("min_length") {
                    if let Ok(min_len) = min_len_str.parse::<usize>() {
                        input.len() >= min_len
                    } else {
                        true
                    }
                } else {
                    true
                }
            }
            ConstraintType::Pattern => {
                if let Some(pattern) = constraint.parameters.get("pattern") {
                    regex::Regex::new(pattern).map_or(false, |re| re.is_match(input))
                } else {
                    true
                }
            }
            _ => true, // Custom constraints handled elsewhere
        };

        let error_message = if !passed {
            Some(format!("Constraint '{}' failed", constraint.name))
        } else {
            None
        };

        ConstraintResult {
            constraint: constraint.clone(),
            passed,
            error_message,
            suggestions: if !passed {
                vec![format!("Fix constraint: {}", constraint.description)]
            } else {
                vec![]
            },
        }
    }

    fn name(&self) -> &str {
        "basic_validator"
    }
}

/// Constraint engine for applying multiple constraints
pub struct ConstraintEngine {
    /// Registered validators
    validators: HashMap<String, Box<dyn ConstraintValidator>>,
}

impl ConstraintEngine {
    /// Create a new constraint engine
    pub fn new() -> Self {
        let mut validators: HashMap<String, Box<dyn ConstraintValidator>> = HashMap::new();
        validators.insert("basic".to_string(), Box::new(BasicConstraintValidator));

        Self { validators }
    }

    /// Validate input against all applicable constraints
    pub fn validate_input(&self, input: &str, constraints: &[ValidationConstraint]) -> Vec<ConstraintResult> {
        constraints.iter()
            .map(|constraint| {
                if let Some(validator) = self.validators.get("basic") {
                    validator.validate(input, constraint)
                } else {
                    ConstraintResult {
                        constraint: constraint.clone(),
                        passed: false,
                        error_message: Some("No validator available".to_string()),
                        suggestions: vec![],
                    }
                }
            })
            .collect()
    }
}

impl Default for ConstraintEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_constraint_validator() {
        let validator = BasicConstraintValidator;
        let constraint = ValidationConstraint {
            name: "required_field".to_string(),
            description: "Field must not be empty".to_string(),
            constraint_type: ConstraintType::RequiredField,
            parameters: HashMap::new(),
            severity: ConstraintSeverity::Error,
        };

        // Test with empty input
        let result = validator.validate("", &constraint);
        assert!(!result.passed);
        assert!(result.error_message.is_some());

        // Test with non-empty input
        let result = validator.validate("test content", &constraint);
        assert!(result.passed);
        assert!(result.error_message.is_none());
    }

    #[test]
    fn test_constraint_engine() {
        let engine = ConstraintEngine::new();
        let constraints = vec![
            ValidationConstraint {
                name: "required".to_string(),
                description: "Must not be empty".to_string(),
                constraint_type: ConstraintType::RequiredField,
                parameters: HashMap::new(),
                severity: ConstraintSeverity::Error,
            },
        ];

        let results = engine.validate_input("test content", &constraints);
        assert_eq!(results.len(), 1);
        assert!(results[0].passed);
    }
}
