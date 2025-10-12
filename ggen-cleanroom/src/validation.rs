//! Validation utilities for cleanroom testing

use crate::{CleanroomEnv, Result};
use serde::{Deserialize, Serialize};

/// Validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Validation passed
    pub passed: bool,

    /// Validation messages
    pub messages: Vec<String>,

    /// Score (0.0 to 1.0)
    pub score: f64,
}

impl ValidationResult {
    /// Create a new validation result
    pub fn new(passed: bool) -> Self {
        Self {
            passed,
            messages: Vec::new(),
            score: if passed { 1.0 } else { 0.0 },
        }
    }

    /// Add a message
    pub fn add_message(&mut self, message: impl Into<String>) {
        self.messages.push(message.into());
    }

    /// Set score
    pub fn set_score(&mut self, score: f64) {
        self.score = score.clamp(0.0, 1.0);
    }
}

/// Validation test suite
pub struct ValidationSuite {
    /// Suite name
    name: String,

    /// Validators
    validators: Vec<Box<dyn Validator>>,
}

/// Validator trait
pub trait Validator: Send + Sync {
    /// Validate in cleanroom environment
    fn validate(&self, env: &CleanroomEnv) -> Result<ValidationResult>;

    /// Get validator name
    fn name(&self) -> &str;
}

impl ValidationSuite {
    /// Create a new validation suite
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            validators: Vec::new(),
        }
    }

    /// Add a validator
    pub fn add_validator(&mut self, validator: Box<dyn Validator>) {
        self.validators.push(validator);
    }

    /// Run all validators
    pub fn run(&self, env: &CleanroomEnv) -> Result<Vec<ValidationResult>> {
        let mut results = Vec::new();

        for validator in &self.validators {
            let result = validator.validate(env)?;
            results.push(result);
        }

        Ok(results)
    }

    /// Get suite name
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CleanroomConfig;

    struct TestValidator;

    impl Validator for TestValidator {
        fn validate(&self, _env: &CleanroomEnv) -> Result<ValidationResult> {
            Ok(ValidationResult::new(true))
        }

        fn name(&self) -> &str {
            "test_validator"
        }
    }

    #[test]
    fn test_validation_suite() {
        let mut suite = ValidationSuite::new("test_suite");
        suite.add_validator(Box::new(TestValidator));

        let env = CleanroomEnv::new(CleanroomConfig::default())
            .expect("Failed to create cleanroom");

        let results = suite.run(&env).expect("Validation failed");
        assert_eq!(results.len(), 1);
        assert!(results[0].passed);
    }
}
