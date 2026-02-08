//! Backtracking executor for retry logic with assertions

use super::types::{AssertionLevel, BacktrackConfig, RetryContext, ValidationResult};
use super::validator::Validator;
use crate::dspy::{Module, ModuleError};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info, warn};

/// Error type for assertion failures
#[derive(Debug, thiserror::Error)]
pub enum AssertionError {
    #[error("Assertion failed after {attempts} attempts: {feedback}")]
    AssertionFailed { attempts: usize, feedback: String },

    #[error("Module execution failed: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("Validation error: {0}")]
    ValidationError(String),
}

pub type AssertionResult<T> = Result<T, AssertionError>;

/// Warning type for suggestion failures (non-fatal)
#[derive(Debug, Clone)]
pub struct SuggestionWarning {
    pub attempts: usize,
    pub feedback: String,
    pub final_output: HashMap<String, Value>,
}

impl SuggestionWarning {
    /// Log warning to console
    pub fn log(&self) {
        warn!(
            "Suggestion failed after {} attempts: {}",
            self.attempts, self.feedback
        );
    }
}

/// Single assertion with validator and configuration
#[derive(Clone)]
pub struct Assertion {
    /// Validator to check outputs
    validator: Arc<dyn Validator>,

    /// Assertion level (Assert or Suggest)
    level: AssertionLevel,

    /// Custom feedback message
    custom_feedback: Option<String>,

    /// Backtrack configuration
    config: BacktrackConfig,
}

impl Assertion {
    /// Create hard assertion (fail on violation)
    pub fn assert(validator: impl Validator + 'static) -> Self {
        Self {
            validator: Arc::new(validator),
            level: AssertionLevel::Assert,
            custom_feedback: None,
            config: BacktrackConfig::default(),
        }
    }

    /// Create soft suggestion (warn on violation)
    pub fn suggest(validator: impl Validator + 'static) -> Self {
        Self {
            validator: Arc::new(validator),
            level: AssertionLevel::Suggest,
            custom_feedback: None,
            config: BacktrackConfig::default(),
        }
    }

    /// Set custom feedback message
    pub fn with_feedback(mut self, msg: impl Into<String>) -> Self {
        self.custom_feedback = Some(msg.into());
        self
    }

    /// Set maximum retry attempts
    pub fn max_retries(mut self, max: usize) -> Self {
        self.config.max_attempts = max;
        self
    }

    /// Set backtrack configuration
    pub fn with_config(mut self, config: BacktrackConfig) -> Self {
        self.config = config;
        self
    }

    /// Get assertion level
    pub fn level(&self) -> AssertionLevel {
        self.level
    }

    /// Get max attempts
    pub fn max_attempts(&self) -> usize {
        self.config.max_attempts
    }

    /// Validate output
    fn validate(&self, output: &Value) -> ValidationResult {
        let result = self.validator.validate(output);

        // Use custom feedback if validation failed and custom feedback is set
        if result.is_invalid() && self.custom_feedback.is_some() {
            ValidationResult::invalid(self.custom_feedback.as_ref().unwrap())
        } else {
            result
        }
    }
}

/// Backtracking executor for retry logic
///
/// Executes modules with assertion validation and automatic retry on failure.
/// Supports both hard assertions (fail on violation) and soft suggestions
/// (warn on violation).
pub struct BacktrackExecutor {
    /// Assertions to apply
    assertions: Vec<Assertion>,

    /// Warnings collected from suggestions
    warnings: Vec<SuggestionWarning>,
}

impl BacktrackExecutor {
    /// Create new executor with assertions
    pub fn new(assertions: Vec<Assertion>) -> Self {
        Self {
            assertions,
            warnings: Vec::new(),
        }
    }

    /// Execute module with assertions and retry logic
    ///
    /// # Arguments
    /// * `module` - Module to execute
    /// * `inputs` - Input values
    ///
    /// # Returns
    /// Output values if all assertions pass, or error if max retries exceeded
    ///
    /// # Errors
    /// Returns `AssertionError` if:
    /// - Module execution fails
    /// - Hard assertion fails after max retries
    pub async fn execute(
        &mut self, module: &dyn Module, inputs: HashMap<String, Value>,
    ) -> AssertionResult<HashMap<String, Value>> {
        if self.assertions.is_empty() {
            // No assertions - just execute once
            return module.forward(inputs).await.map_err(Into::into);
        }

        // Find max attempts across all assertions
        let max_attempts = self
            .assertions
            .iter()
            .map(|a| a.max_attempts())
            .max()
            .unwrap_or(1);

        let mut context = RetryContext::new();

        for attempt in 0..max_attempts {
            context.attempt = attempt;

            debug!(
                "Executing {} (attempt {}/{})",
                module.name(),
                attempt + 1,
                max_attempts
            );

            // Execute module
            let output = module.forward(inputs.clone()).await?;

            // Validate against all assertions
            let validation_result = self.validate_all(&output);

            match validation_result {
                ValidationResult::Valid => {
                    info!(
                        "Module {} passed all assertions on attempt {}",
                        module.name(),
                        attempt + 1
                    );
                    return Ok(output);
                }
                ValidationResult::Invalid { feedback } => {
                    debug!("Validation failed on attempt {}: {}", attempt + 1, feedback);

                    // Serialize output for context
                    let output_str =
                        serde_json::to_string(&output).unwrap_or_else(|_| format!("{:?}", output));

                    context.add_failure(output_str, feedback.clone());

                    // Check if we should continue retrying
                    if attempt + 1 >= max_attempts {
                        return self.handle_max_retries_reached(output, feedback, attempt + 1);
                    }
                }
            }
        }

        // Should not reach here, but handle gracefully
        Err(AssertionError::ValidationError(
            "Unexpected end of retry loop".to_string(),
        ))
    }

    /// Execute module with retry context injection
    ///
    /// This version modifies the input to include retry context (past outputs
    /// and feedback) to help the model self-correct.
    pub async fn execute_with_context(
        &mut self, module: &dyn Module, mut inputs: HashMap<String, Value>,
    ) -> AssertionResult<HashMap<String, Value>> {
        if self.assertions.is_empty() {
            return module.forward(inputs).await.map_err(Into::into);
        }

        let max_attempts = self
            .assertions
            .iter()
            .map(|a| a.max_attempts())
            .max()
            .unwrap_or(1);

        let mut context = RetryContext::new();

        for attempt in 0..max_attempts {
            context.attempt = attempt;

            // Inject retry context into inputs
            if attempt > 0 {
                let retry_prompt = context.format_for_prompt();
                if !retry_prompt.is_empty() {
                    inputs.insert("__retry_context".to_string(), Value::String(retry_prompt));
                }
            }

            debug!(
                "Executing {} with context (attempt {}/{})",
                module.name(),
                attempt + 1,
                max_attempts
            );

            // Execute module
            let output = module.forward(inputs.clone()).await?;

            // Validate against all assertions
            let validation_result = self.validate_all(&output);

            match validation_result {
                ValidationResult::Valid => {
                    info!(
                        "Module {} passed all assertions with context on attempt {}",
                        module.name(),
                        attempt + 1
                    );
                    return Ok(output);
                }
                ValidationResult::Invalid { feedback } => {
                    debug!("Validation failed on attempt {}: {}", attempt + 1, feedback);

                    let output_str =
                        serde_json::to_string(&output).unwrap_or_else(|_| format!("{:?}", output));

                    context.add_failure(output_str, feedback.clone());

                    if attempt + 1 >= max_attempts {
                        return self.handle_max_retries_reached(output, feedback, attempt + 1);
                    }
                }
            }
        }

        Err(AssertionError::ValidationError(
            "Unexpected end of retry loop".to_string(),
        ))
    }

    /// Validate output against all assertions
    fn validate_all(&self, output: &HashMap<String, Value>) -> ValidationResult {
        for assertion in &self.assertions {
            // For multi-field outputs, validate each field separately
            // Or validate the whole output as JSON
            let output_json =
                serde_json::to_value(output).unwrap_or(Value::Object(serde_json::Map::new()));

            let result = assertion.validate(&output_json);

            // If hard assertion fails, return immediately
            if result.is_invalid() && assertion.level() == AssertionLevel::Assert {
                return result;
            }

            // If suggestion fails, continue checking other assertions
            // We'll handle suggestion warnings separately
        }

        ValidationResult::valid()
    }

    /// Handle max retries reached
    fn handle_max_retries_reached(
        &mut self, output: HashMap<String, Value>, feedback: String, attempts: usize,
    ) -> AssertionResult<HashMap<String, Value>> {
        // Check if any assertion is hard (Assert level)
        let has_hard_assertion = self
            .assertions
            .iter()
            .any(|a| a.level() == AssertionLevel::Assert);

        if has_hard_assertion {
            // Hard assertion failed - return error
            Err(AssertionError::AssertionFailed { attempts, feedback })
        } else {
            // Only suggestions failed - log warning and return output
            let warning = SuggestionWarning {
                attempts,
                feedback,
                final_output: output.clone(),
            };

            warning.log();
            self.warnings.push(warning);

            Ok(output)
        }
    }

    /// Get collected warnings
    pub fn warnings(&self) -> &[SuggestionWarning] {
        &self.warnings
    }

    /// Clear warnings
    pub fn clear_warnings(&mut self) {
        self.warnings.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::{field::*, Signature};

    // Mock module for testing
    struct MockModule {
        sig: Signature,
        outputs: Vec<HashMap<String, Value>>,
        current_attempt: std::sync::Mutex<usize>,
    }

    impl MockModule {
        fn new(sig: Signature, outputs: Vec<HashMap<String, Value>>) -> Self {
            Self {
                sig,
                outputs,
                current_attempt: std::sync::Mutex::new(0),
            }
        }
    }

    #[async_trait::async_trait]
    impl Module for MockModule {
        fn signature(&self) -> &Signature {
            &self.sig
        }

        async fn forward(
            &self, _inputs: HashMap<String, Value>,
        ) -> Result<HashMap<String, Value>, ModuleError> {
            let mut attempt = self.current_attempt.lock().unwrap();
            let output = self
                .outputs
                .get(*attempt)
                .cloned()
                .unwrap_or_else(|| self.outputs.last().unwrap().clone());
            *attempt += 1;
            Ok(output)
        }
    }

    fn create_test_signature() -> Signature {
        Signature::new("Test", "Test module")
            .with_input(InputField::new("input", "Input", "String"))
            .with_output(OutputField::new("output", "Output", "String"))
    }

    // ===== Assertion Builder Tests =====

    #[test]
    fn test_assertion_assert_builder() {
        use super::super::validator::LengthValidator;

        let assertion = Assertion::assert(LengthValidator::min(5))
            .with_feedback("Too short")
            .max_retries(3);

        assert_eq!(assertion.level(), AssertionLevel::Assert);
        assert_eq!(assertion.max_attempts(), 3);
        assert_eq!(assertion.custom_feedback, Some("Too short".to_string()));
    }

    #[test]
    fn test_assertion_suggest_builder() {
        use super::super::validator::NotEmptyValidator;

        let assertion = Assertion::suggest(NotEmptyValidator)
            .with_feedback("Should not be empty")
            .max_retries(2);

        assert_eq!(assertion.level(), AssertionLevel::Suggest);
        assert_eq!(assertion.max_attempts(), 2);
    }

    // ===== BacktrackExecutor Tests =====

    #[tokio::test]
    async fn test_executor_no_assertions() {
        let sig = create_test_signature();
        let mut outputs = HashMap::new();
        outputs.insert("output".to_string(), Value::String("result".to_string()));

        let module = MockModule::new(sig, vec![outputs.clone()]);
        let mut executor = BacktrackExecutor::new(vec![]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().get("output").unwrap(),
            &Value::String("result".to_string())
        );
    }

    #[tokio::test]
    async fn test_executor_assertion_passes_first_attempt() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();
        let mut outputs = HashMap::new();
        outputs.insert(
            "output".to_string(),
            Value::String("hello world".to_string()),
        );

        let module = MockModule::new(sig, vec![outputs.clone()]);

        let assertion = Assertion::assert(LengthValidator::min(5)).max_retries(3);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_executor_assertion_fails_all_attempts() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();

        // All outputs are too short
        let mut output1 = HashMap::new();
        output1.insert("output".to_string(), Value::String("hi".to_string()));

        let module = MockModule::new(sig, vec![output1.clone(), output1.clone(), output1]);

        let assertion = Assertion::assert(LengthValidator::min(10)).max_retries(3);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_err());
        match result.unwrap_err() {
            AssertionError::AssertionFailed { attempts, .. } => {
                assert_eq!(attempts, 3);
            }
            _ => panic!("Expected AssertionFailed error"),
        }
    }

    #[tokio::test]
    async fn test_executor_assertion_passes_on_retry() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();

        // First two attempts fail, third succeeds
        let mut output1 = HashMap::new();
        output1.insert("output".to_string(), Value::String("hi".to_string()));

        let mut output2 = HashMap::new();
        output2.insert("output".to_string(), Value::String("hello".to_string()));

        let mut output3 = HashMap::new();
        output3.insert(
            "output".to_string(),
            Value::String("hello world".to_string()),
        );

        let module = MockModule::new(sig, vec![output1, output2, output3]);

        let assertion = Assertion::assert(LengthValidator::min(10)).max_retries(3);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(
            output.get("output").unwrap(),
            &Value::String("hello world".to_string())
        );
    }

    #[tokio::test]
    async fn test_executor_suggestion_fails_but_continues() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();

        let mut output = HashMap::new();
        output.insert("output".to_string(), Value::String("hi".to_string()));

        let module = MockModule::new(sig, vec![output.clone()]);

        // Soft suggestion - should warn but not fail
        let assertion = Assertion::suggest(LengthValidator::min(10)).max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
        assert_eq!(executor.warnings().len(), 1);
        assert_eq!(executor.warnings()[0].attempts, 2);
    }

    #[tokio::test]
    async fn test_executor_custom_feedback() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();

        let mut output = HashMap::new();
        output.insert("output".to_string(), Value::String("hi".to_string()));

        let module = MockModule::new(sig, vec![output.clone()]);

        let assertion = Assertion::assert(LengthValidator::min(10))
            .with_feedback("Output must be detailed (at least 10 characters)")
            .max_retries(1);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_err());
        match result.unwrap_err() {
            AssertionError::AssertionFailed { feedback, .. } => {
                assert!(feedback.contains("must be detailed"));
            }
            _ => panic!("Expected AssertionFailed error"),
        }
    }

    #[tokio::test]
    async fn test_executor_multiple_assertions() {
        use super::super::validator::{LengthValidator, NotEmptyValidator};

        let sig = create_test_signature();

        let mut output = HashMap::new();
        output.insert(
            "output".to_string(),
            Value::String("hello world".to_string()),
        );

        let module = MockModule::new(sig, vec![output.clone()]);

        let assertions = vec![
            Assertion::assert(NotEmptyValidator).max_retries(2),
            Assertion::assert(LengthValidator::between(5, 20)).max_retries(2),
        ];

        let mut executor = BacktrackExecutor::new(assertions);

        let inputs = HashMap::new();
        let result = executor.execute(&module, inputs).await;

        assert!(result.is_ok());
    }

    // ===== RetryContext Tests =====

    #[test]
    fn test_retry_context_formatting() {
        let mut context = RetryContext::new();
        context.add_failure("bad output".to_string(), "Output too short".to_string());

        let formatted = context.format_for_prompt();
        assert!(formatted.contains("Previous Attempts"));
        assert!(formatted.contains("bad output"));
        assert!(formatted.contains("Output too short"));
    }

    // ===== Warning Collection Tests =====

    #[tokio::test]
    async fn test_executor_warning_collection() {
        use super::super::validator::LengthValidator;

        let sig = create_test_signature();

        let mut output = HashMap::new();
        output.insert("output".to_string(), Value::String("short".to_string()));

        let module = MockModule::new(sig, vec![output.clone()]);

        let assertion = Assertion::suggest(LengthValidator::min(20)).max_retries(2);

        let mut executor = BacktrackExecutor::new(vec![assertion]);

        let inputs = HashMap::new();
        let _ = executor.execute(&module, inputs).await;

        assert_eq!(executor.warnings().len(), 1);

        executor.clear_warnings();
        assert_eq!(executor.warnings().len(), 0);
    }
}
