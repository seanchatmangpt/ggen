//! DSPy Assertion and Validation System
//!
//! This module provides a comprehensive assertion and validation system for DSPy modules,
//! implementing the backtracking and self-correction patterns from DSPy Python.
//!
//! # Core Components
//!
//! - **Validators**: Pluggable validation logic for module outputs
//! - **Assertions**: Hard constraints that trigger retry on failure
//! - **Suggestions**: Soft constraints that log warnings but don't fail
//! - **BacktrackExecutor**: Manages retry logic with dynamic prompt modification
//!
//! # Example Usage
//!
//! ```ignore
//! use ggen_ai::dspy::assertions::*;
//! use ggen_ai::dspy::assertions::validator::*;
//!
//! // Create validators
//! let length_check = LengthValidator::between(10, 100);
//! let not_empty_check = NotEmptyValidator;
//!
//! // Build assertions
//! let assertion = Assertion::assert(length_check)
//!     .with_feedback("Answer must be 10-100 characters")
//!     .max_retries(3);
//!
//! let suggestion = Assertion::suggest(not_empty_check)
//!     .with_feedback("Answer should not be empty")
//!     .max_retries(2);
//!
//! // Execute module with assertions
//! let mut executor = BacktrackExecutor::new(vec![assertion, suggestion]);
//! let result = executor.execute(&module, inputs).await?;
//! ```
//!
//! # Validation Patterns
//!
//! ## Length Constraints
//! ```ignore
//! let validator = LengthValidator::between(5, 100);
//! let assertion = Assertion::assert(validator)
//!     .with_feedback("Output must be 5-100 characters");
//! ```
//!
//! ## Pattern Matching
//! ```ignore
//! let validator = PatternValidator::new(r"^\d{3}-\d{4}$")?;
//! let assertion = Assertion::assert(validator)
//!     .with_feedback("Must match phone number format: 123-4567");
//! ```
//!
//! ## Content Constraints
//! ```ignore
//! let validator = ContainsValidator::new("citation");
//! let assertion = Assertion::suggest(validator)
//!     .with_feedback("Answer should include citations");
//! ```
//!
//! ## Combinator Logic
//! ```ignore
//! let validators = vec![
//!     Box::new(NotEmptyValidator),
//!     Box::new(LengthValidator::min(10)),
//! ];
//! let all_validator = AllValidator::new(validators);
//! let assertion = Assertion::assert(all_validator);
//! ```
//!
//! # Integration with Optimizers
//!
//! Assertions can be used during optimization to guide bootstrapping:
//!
//! ```ignore
//! // Create assertable module
//! let module = MyModule::new()
//!     .with_assertion(Assertion::assert(validator).max_retries(3));
//!
//! // Use in optimizer - teacher will use assertions during bootstrapping
//! let optimizer = BootstrapFewShot::new(metric)
//!     .with_teacher(Arc::new(module));
//!
//! let optimized = optimizer.compile(&student, &trainset).await?;
//! ```
//!
//! # Retry and Self-Correction
//!
//! The BacktrackExecutor automatically:
//! - Executes module with inputs
//! - Validates outputs against assertions
//! - On failure, injects past output + feedback into retry
//! - Retries up to max_attempts
//! - Returns final result or error
//!
//! # Performance Considerations
//!
//! - Each retry requires an additional LLM call
//! - Set reasonable max_retries (2-5 typically)
//! - Use Suggest for non-critical constraints
//! - Use Assert only for critical requirements

pub mod executor;
pub mod module;
pub mod types;
pub mod validator;

pub use executor::{
    Assertion, AssertionError, AssertionResult, BacktrackExecutor, SuggestionWarning,
};
pub use module::{AssertableModule, AssertedModule};
pub use types::{AssertionLevel, BacktrackConfig, RetryContext, RetryStrategy, ValidationResult};
pub use validator::{
    AllValidator, AnyValidator, ArcValidator, BoxedValidator, ContainsValidator, FnValidator,
    ItemCountValidator, LengthValidator, NotEmptyValidator, NotValidator, PatternValidator,
    UniqueItemsValidator, Validator,
};

#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::dspy::{field::*, Module, Signature};
    use serde_json::Value;
    use std::collections::HashMap;

    // Mock module for integration tests
    struct IntegrationTestModule {
        sig: Signature,
        response: String,
    }

    impl IntegrationTestModule {
        fn new(response: String) -> Self {
            let sig = Signature::new("Test", "Test module")
                .with_input(InputField::new("input", "Input", "String"))
                .with_output(OutputField::new("output", "Output", "String"));

            Self { sig, response }
        }
    }

    #[async_trait::async_trait]
    impl Module for IntegrationTestModule {
        fn signature(&self) -> &Signature {
            &self.sig
        }

        async fn forward(
            &self, _inputs: HashMap<String, Value>,
        ) -> Result<HashMap<String, Value>, crate::dspy::ModuleError> {
            let mut outputs = HashMap::new();
            outputs.insert("output".to_string(), Value::String(self.response.clone()));
            Ok(outputs)
        }
    }

    #[tokio::test]
    async fn test_end_to_end_assertion_success() {
        let module = IntegrationTestModule::new("hello world".to_string());

        let assertion = Assertion::assert(LengthValidator::between(5, 20))
            .with_feedback("Output must be 5-20 characters")
            .max_retries(3);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_end_to_end_assertion_failure() {
        let module = IntegrationTestModule::new("hi".to_string());

        let assertion = Assertion::assert(LengthValidator::min(10))
            .with_feedback("Output too short")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_err());
        match result.unwrap_err() {
            AssertionError::AssertionFailed { attempts, feedback } => {
                assert_eq!(attempts, 2);
                assert!(feedback.contains("too short"));
            }
            _ => panic!("Expected AssertionFailed"),
        }
    }

    #[tokio::test]
    async fn test_end_to_end_suggestion_warning() {
        let module = IntegrationTestModule::new("short".to_string());

        let suggestion = Assertion::suggest(LengthValidator::min(20))
            .with_feedback("Output should be detailed")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![suggestion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
        assert_eq!(executor.warnings().len(), 1);
        assert_eq!(executor.warnings()[0].attempts, 2);
    }

    #[tokio::test]
    async fn test_end_to_end_multiple_validators() {
        let module = IntegrationTestModule::new("hello world".to_string());

        let validators: Vec<BoxedValidator> = vec![
            Box::new(NotEmptyValidator),
            Box::new(LengthValidator::between(5, 20)),
            Box::new(ContainsValidator::new("hello")),
        ];

        let all_validator = AllValidator::new(validators);

        let assertion = Assertion::assert(all_validator)
            .with_feedback("Output must pass all checks")
            .max_retries(3);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_end_to_end_pattern_validation() {
        let module = IntegrationTestModule::new("user@example.com".to_string());

        let validator = PatternValidator::new(r"^[\w\.\-]+@[\w\.\-]+\.\w+$").expect("Valid regex");

        let assertion = Assertion::assert(validator)
            .with_feedback("Must be valid email format")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_end_to_end_function_validator() {
        let module = IntegrationTestModule::new("one two three four".to_string());

        let validator = FnValidator::new(
            |value| {
                if let Some(s) = value.as_str() {
                    let word_count = s.split_whitespace().count();
                    if word_count >= 3 {
                        ValidationResult::valid()
                    } else {
                        ValidationResult::invalid(format!(
                            "Need at least 3 words, got {}",
                            word_count
                        ))
                    }
                } else {
                    ValidationResult::invalid("Must be a string")
                }
            },
            "Word count >= 3",
        );

        let assertion = Assertion::assert(validator)
            .with_feedback("Answer must have at least 3 words")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_end_to_end_mixed_assert_and_suggest() {
        let module = IntegrationTestModule::new("hello".to_string());

        let assertions = vec![
            // Hard requirement - must pass
            Assertion::assert(NotEmptyValidator)
                .with_feedback("Output cannot be empty")
                .max_retries(2),
            // Soft suggestion - can fail
            Assertion::suggest(LengthValidator::min(20))
                .with_feedback("Output should be detailed")
                .max_retries(2),
        ];

        let mut executor = BacktrackExecutor::new(assertions);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        // Should succeed (Assert passes) but generate warning (Suggest fails)
        assert!(result.is_ok());
        assert_eq!(executor.warnings().len(), 1);
    }

    #[tokio::test]
    async fn test_end_to_end_or_validator() {
        let module = IntegrationTestModule::new("hi".to_string());

        // Either very short OR very long
        let validators: Vec<BoxedValidator> = vec![
            Box::new(LengthValidator::max(3)),
            Box::new(LengthValidator::min(20)),
        ];

        let any_validator = AnyValidator::new(validators);

        let assertion = Assertion::assert(any_validator)
            .with_feedback("Output must be either very short or very long")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok()); // "hi" is <= 3 chars, so it passes
    }

    #[tokio::test]
    async fn test_end_to_end_not_validator() {
        let module = IntegrationTestModule::new("good content".to_string());

        let not_validator = NotValidator::new(Box::new(ContainsValidator::new("bad")));

        let assertion = Assertion::assert(not_validator)
            .with_feedback("Output must not contain 'bad'")
            .max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }
}
