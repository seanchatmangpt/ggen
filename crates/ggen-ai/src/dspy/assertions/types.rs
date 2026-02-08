//! Core types for DSPy assertion system

use serde::{Deserialize, Serialize};
use std::fmt;

/// Result of a validation check
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValidationResult {
    /// Validation passed
    Valid,

    /// Validation failed with feedback message
    Invalid { feedback: String },
}

impl ValidationResult {
    /// Check if validation passed
    pub fn is_valid(&self) -> bool {
        matches!(self, ValidationResult::Valid)
    }

    /// Check if validation failed
    pub fn is_invalid(&self) -> bool {
        !self.is_valid()
    }

    /// Get feedback message if invalid
    pub fn feedback(&self) -> Option<&str> {
        match self {
            ValidationResult::Valid => None,
            ValidationResult::Invalid { feedback } => Some(feedback),
        }
    }

    /// Create valid result
    pub fn valid() -> Self {
        ValidationResult::Valid
    }

    /// Create invalid result with feedback
    pub fn invalid(feedback: impl Into<String>) -> Self {
        ValidationResult::Invalid {
            feedback: feedback.into(),
        }
    }
}

impl fmt::Display for ValidationResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidationResult::Valid => write!(f, "Valid"),
            ValidationResult::Invalid { feedback } => write!(f, "Invalid: {}", feedback),
        }
    }
}

/// Assertion severity level - determines failure behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AssertionLevel {
    /// Critical constraint - halt execution on failure
    Assert,

    /// Advisory constraint - log warning and continue on failure
    Suggest,
}

impl fmt::Display for AssertionLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssertionLevel::Assert => write!(f, "Assert"),
            AssertionLevel::Suggest => write!(f, "Suggest"),
        }
    }
}

/// Configuration for backtracking retry behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BacktrackConfig {
    /// Maximum number of retry attempts
    pub max_attempts: usize,

    /// Strategy for retrying
    pub strategy: RetryStrategy,
}

impl Default for BacktrackConfig {
    fn default() -> Self {
        Self {
            max_attempts: 2,
            strategy: RetryStrategy::SameModule,
        }
    }
}

impl BacktrackConfig {
    /// Create new config with max attempts
    pub fn new(max_attempts: usize) -> Self {
        Self {
            max_attempts,
            ..Default::default()
        }
    }

    /// Set retry strategy
    pub fn with_strategy(mut self, strategy: RetryStrategy) -> Self {
        self.strategy = strategy;
        self
    }
}

/// Retry strategy for backtracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RetryStrategy {
    /// Retry same module with feedback
    SameModule,

    /// Retry specific target module by name
    TargetModule(String),

    /// Custom retry logic (not serializable)
    #[serde(skip)]
    Custom,
}

/// Context for retry attempts
#[derive(Debug, Clone)]
pub struct RetryContext {
    /// Current attempt number (0-indexed)
    pub attempt: usize,

    /// Previous failed outputs
    pub past_outputs: Vec<String>,

    /// Feedback messages from previous attempts
    pub feedback_history: Vec<String>,
}

impl RetryContext {
    /// Create new retry context
    pub fn new() -> Self {
        Self {
            attempt: 0,
            past_outputs: Vec::new(),
            feedback_history: Vec::new(),
        }
    }

    /// Add failed attempt to context
    pub fn add_failure(&mut self, output: String, feedback: String) {
        self.past_outputs.push(output);
        self.feedback_history.push(feedback);
        self.attempt += 1;
    }

    /// Get most recent feedback
    pub fn latest_feedback(&self) -> Option<&str> {
        self.feedback_history.last().map(|s| s.as_str())
    }

    /// Get most recent output
    pub fn latest_output(&self) -> Option<&str> {
        self.past_outputs.last().map(|s| s.as_str())
    }

    /// Format context for prompt injection
    pub fn format_for_prompt(&self) -> String {
        let mut prompt = String::new();

        if !self.past_outputs.is_empty() {
            prompt.push_str("\n--- Previous Attempts ---\n");
            for (idx, (output, feedback)) in self
                .past_outputs
                .iter()
                .zip(self.feedback_history.iter())
                .enumerate()
            {
                prompt.push_str(&format!(
                    "\nAttempt {}: Output: {}\nFeedback: {}\n",
                    idx + 1,
                    output,
                    feedback
                ));
            }
            prompt.push_str("--- Please try again with corrections ---\n\n");
        }

        prompt
    }
}

impl Default for RetryContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_result_valid() {
        let result = ValidationResult::valid();
        assert!(result.is_valid());
        assert!(!result.is_invalid());
        assert_eq!(result.feedback(), None);
    }

    #[test]
    fn test_validation_result_invalid() {
        let result = ValidationResult::invalid("Too short");
        assert!(!result.is_valid());
        assert!(result.is_invalid());
        assert_eq!(result.feedback(), Some("Too short"));
    }

    #[test]
    fn test_validation_result_display() {
        assert_eq!(ValidationResult::valid().to_string(), "Valid");
        assert_eq!(
            ValidationResult::invalid("Error").to_string(),
            "Invalid: Error"
        );
    }

    #[test]
    fn test_assertion_level_display() {
        assert_eq!(AssertionLevel::Assert.to_string(), "Assert");
        assert_eq!(AssertionLevel::Suggest.to_string(), "Suggest");
    }

    #[test]
    fn test_backtrack_config_default() {
        let config = BacktrackConfig::default();
        assert_eq!(config.max_attempts, 2);
        assert!(matches!(config.strategy, RetryStrategy::SameModule));
    }

    #[test]
    fn test_backtrack_config_builder() {
        let config =
            BacktrackConfig::new(5).with_strategy(RetryStrategy::TargetModule("test".to_string()));

        assert_eq!(config.max_attempts, 5);
        assert!(matches!(config.strategy, RetryStrategy::TargetModule(_)));
    }

    #[test]
    fn test_retry_context_new() {
        let ctx = RetryContext::new();
        assert_eq!(ctx.attempt, 0);
        assert!(ctx.past_outputs.is_empty());
        assert!(ctx.feedback_history.is_empty());
    }

    #[test]
    fn test_retry_context_add_failure() {
        let mut ctx = RetryContext::new();

        ctx.add_failure("output1".to_string(), "feedback1".to_string());
        assert_eq!(ctx.attempt, 1);
        assert_eq!(ctx.past_outputs.len(), 1);
        assert_eq!(ctx.latest_output(), Some("output1"));
        assert_eq!(ctx.latest_feedback(), Some("feedback1"));

        ctx.add_failure("output2".to_string(), "feedback2".to_string());
        assert_eq!(ctx.attempt, 2);
        assert_eq!(ctx.past_outputs.len(), 2);
        assert_eq!(ctx.latest_output(), Some("output2"));
        assert_eq!(ctx.latest_feedback(), Some("feedback2"));
    }

    #[test]
    fn test_retry_context_format_for_prompt() {
        let mut ctx = RetryContext::new();
        ctx.add_failure("bad output".to_string(), "too short".to_string());

        let formatted = ctx.format_for_prompt();
        assert!(formatted.contains("Previous Attempts"));
        assert!(formatted.contains("Attempt 1"));
        assert!(formatted.contains("bad output"));
        assert!(formatted.contains("too short"));
        assert!(formatted.contains("Please try again"));
    }

    #[test]
    fn test_retry_context_format_empty() {
        let ctx = RetryContext::new();
        let formatted = ctx.format_for_prompt();
        assert!(formatted.is_empty());
    }

    #[test]
    fn test_validation_result_serialization() {
        let valid = ValidationResult::valid();
        let json = serde_json::to_string(&valid).unwrap();
        let deserialized: ValidationResult = serde_json::from_str(&json).unwrap();
        assert_eq!(valid, deserialized);

        let invalid = ValidationResult::invalid("error");
        let json = serde_json::to_string(&invalid).unwrap();
        let deserialized: ValidationResult = serde_json::from_str(&json).unwrap();
        assert_eq!(invalid, deserialized);
    }

    #[test]
    fn test_backtrack_config_serialization() {
        let config = BacktrackConfig::new(3)
            .with_strategy(RetryStrategy::TargetModule("module1".to_string()));

        let json = serde_json::to_string(&config).unwrap();
        let deserialized: BacktrackConfig = serde_json::from_str(&json).unwrap();

        assert_eq!(config.max_attempts, deserialized.max_attempts);
    }
}
