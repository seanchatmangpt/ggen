//! Validator trait and built-in validators

use super::types::ValidationResult;
use serde_json::Value;
use std::sync::Arc;

/// Trait for validating module outputs
///
/// Validators are composable and can be combined using logical operators.
/// All validators must be Send + Sync to support async execution.
pub trait Validator: Send + Sync {
    /// Validate output value
    ///
    /// # Arguments
    /// * `output` - The output value to validate
    ///
    /// # Returns
    /// `ValidationResult::Valid` if validation passes,
    /// `ValidationResult::Invalid` with feedback if it fails
    fn validate(&self, output: &Value) -> ValidationResult;

    /// Get validator description (for debugging/logging)
    fn description(&self) -> String {
        "Custom validator".to_string()
    }
}

/// Type alias for boxed validators
pub type BoxedValidator = Box<dyn Validator>;

/// Type alias for Arc validators (for sharing across threads)
pub type ArcValidator = Arc<dyn Validator>;

// ===== Built-in Validators =====

/// Validator that checks string length constraints
#[derive(Debug, Clone)]
pub struct LengthValidator {
    pub min_length: Option<usize>,
    pub max_length: Option<usize>,
}

impl LengthValidator {
    /// Create validator with both min and max length
    pub fn between(min: usize, max: usize) -> Self {
        Self {
            min_length: Some(min),
            max_length: Some(max),
        }
    }

    /// Create validator with only min length
    pub fn min(min: usize) -> Self {
        Self {
            min_length: Some(min),
            max_length: None,
        }
    }

    /// Create validator with only max length
    pub fn max(max: usize) -> Self {
        Self {
            min_length: None,
            max_length: Some(max),
        }
    }
}

impl Validator for LengthValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let text = match output {
            Value::String(s) => s,
            _ => return ValidationResult::invalid("Output must be a string"),
        };

        let len = text.len();

        if let Some(min) = self.min_length {
            if len < min {
                return ValidationResult::invalid(format!(
                    "Output too short: {} chars (minimum: {})",
                    len, min
                ));
            }
        }

        if let Some(max) = self.max_length {
            if len > max {
                return ValidationResult::invalid(format!(
                    "Output too long: {} chars (maximum: {})",
                    len, max
                ));
            }
        }

        ValidationResult::valid()
    }

    fn description(&self) -> String {
        match (self.min_length, self.max_length) {
            (Some(min), Some(max)) => format!("Length between {} and {}", min, max),
            (Some(min), None) => format!("Length >= {}", min),
            (None, Some(max)) => format!("Length <= {}", max),
            (None, None) => "No length constraint".to_string(),
        }
    }
}

/// Validator that checks regex pattern matching
#[derive(Debug, Clone)]
pub struct PatternValidator {
    pattern: regex::Regex,
    pattern_str: String,
}

impl PatternValidator {
    /// Create new pattern validator
    ///
    /// # Errors
    /// Returns error if regex pattern is invalid
    pub fn new(pattern: &str) -> Result<Self, regex::Error> {
        Ok(Self {
            pattern: regex::Regex::new(pattern)?,
            pattern_str: pattern.to_string(),
        })
    }
}

impl Validator for PatternValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let text = match output {
            Value::String(s) => s,
            _ => return ValidationResult::invalid("Output must be a string"),
        };

        if self.pattern.is_match(text) {
            ValidationResult::valid()
        } else {
            ValidationResult::invalid(format!(
                "Output does not match pattern: {}",
                self.pattern_str
            ))
        }
    }

    fn description(&self) -> String {
        format!("Pattern: {}", self.pattern_str)
    }
}

/// Validator that checks if output is non-empty
#[derive(Debug, Clone)]
pub struct NotEmptyValidator;

impl Validator for NotEmptyValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        match output {
            Value::String(s) if s.trim().is_empty() => {
                ValidationResult::invalid("Output cannot be empty")
            }
            Value::String(_) => ValidationResult::valid(),
            Value::Array(arr) if arr.is_empty() => {
                ValidationResult::invalid("Output array cannot be empty")
            }
            Value::Array(_) => ValidationResult::valid(),
            Value::Null => ValidationResult::invalid("Output cannot be null"),
            _ => ValidationResult::valid(),
        }
    }

    fn description(&self) -> String {
        "Non-empty value".to_string()
    }
}

/// Validator that checks if output contains specific substring
#[derive(Debug, Clone)]
pub struct ContainsValidator {
    substring: String,
    case_sensitive: bool,
}

impl ContainsValidator {
    /// Create new contains validator (case-sensitive)
    pub fn new(substring: impl Into<String>) -> Self {
        Self {
            substring: substring.into(),
            case_sensitive: true,
        }
    }

    /// Create case-insensitive contains validator
    pub fn case_insensitive(substring: impl Into<String>) -> Self {
        Self {
            substring: substring.into(),
            case_sensitive: false,
        }
    }
}

impl Validator for ContainsValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let text = match output {
            Value::String(s) => s,
            _ => return ValidationResult::invalid("Output must be a string"),
        };

        let found = if self.case_sensitive {
            text.contains(&self.substring)
        } else {
            text.to_lowercase().contains(&self.substring.to_lowercase())
        };

        if found {
            ValidationResult::valid()
        } else {
            ValidationResult::invalid(format!("Output must contain '{}'", self.substring))
        }
    }

    fn description(&self) -> String {
        format!(
            "Contains '{}' ({})",
            self.substring,
            if self.case_sensitive {
                "case-sensitive"
            } else {
                "case-insensitive"
            }
        )
    }
}

/// Validator that checks array item count
#[derive(Debug, Clone)]
pub struct ItemCountValidator {
    pub min_items: Option<usize>,
    pub max_items: Option<usize>,
}

impl ItemCountValidator {
    /// Create validator with both min and max items
    pub fn between(min: usize, max: usize) -> Self {
        Self {
            min_items: Some(min),
            max_items: Some(max),
        }
    }

    /// Create validator with only min items
    pub fn min(min: usize) -> Self {
        Self {
            min_items: Some(min),
            max_items: None,
        }
    }

    /// Create validator with only max items
    pub fn max(max: usize) -> Self {
        Self {
            min_items: None,
            max_items: Some(max),
        }
    }
}

impl Validator for ItemCountValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let arr = match output {
            Value::Array(a) => a,
            _ => return ValidationResult::invalid("Output must be an array"),
        };

        let count = arr.len();

        if let Some(min) = self.min_items {
            if count < min {
                return ValidationResult::invalid(format!(
                    "Too few items: {} (minimum: {})",
                    count, min
                ));
            }
        }

        if let Some(max) = self.max_items {
            if count > max {
                return ValidationResult::invalid(format!(
                    "Too many items: {} (maximum: {})",
                    count, max
                ));
            }
        }

        ValidationResult::valid()
    }

    fn description(&self) -> String {
        match (self.min_items, self.max_items) {
            (Some(min), Some(max)) => format!("Item count between {} and {}", min, max),
            (Some(min), None) => format!("Item count >= {}", min),
            (None, Some(max)) => format!("Item count <= {}", max),
            (None, None) => "No item count constraint".to_string(),
        }
    }
}

/// Validator that checks for unique items in an array
#[derive(Debug, Clone)]
pub struct UniqueItemsValidator;

impl Validator for UniqueItemsValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let arr = match output {
            Value::Array(a) => a,
            _ => return ValidationResult::invalid("Output must be an array"),
        };

        let unique_count = arr.iter().collect::<std::collections::HashSet<_>>().len();

        if unique_count == arr.len() {
            ValidationResult::valid()
        } else {
            ValidationResult::invalid(format!(
                "Items must be unique (found {} duplicates)",
                arr.len() - unique_count
            ))
        }
    }

    fn description(&self) -> String {
        "All items must be unique".to_string()
    }
}

// ===== Combinator Validators =====

/// Validator that requires all sub-validators to pass (AND logic)
pub struct AllValidator {
    validators: Vec<BoxedValidator>,
}

impl AllValidator {
    /// Create new ALL validator
    pub fn new(validators: Vec<BoxedValidator>) -> Self {
        Self { validators }
    }
}

impl Validator for AllValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        for validator in &self.validators {
            let result = validator.validate(output);
            if result.is_invalid() {
                return result;
            }
        }
        ValidationResult::valid()
    }

    fn description(&self) -> String {
        format!(
            "All of: [{}]",
            self.validators
                .iter()
                .map(|v| v.description())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// Validator that requires at least one sub-validator to pass (OR logic)
pub struct AnyValidator {
    validators: Vec<BoxedValidator>,
}

impl AnyValidator {
    /// Create new ANY validator
    pub fn new(validators: Vec<BoxedValidator>) -> Self {
        Self { validators }
    }
}

impl Validator for AnyValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        let mut all_feedback = Vec::new();

        for validator in &self.validators {
            let result = validator.validate(output);
            if result.is_valid() {
                return ValidationResult::valid();
            }
            if let Some(feedback) = result.feedback() {
                all_feedback.push(feedback.to_string());
            }
        }

        ValidationResult::invalid(format!(
            "None of the validators passed: {}",
            all_feedback.join("; ")
        ))
    }

    fn description(&self) -> String {
        format!(
            "Any of: [{}]",
            self.validators
                .iter()
                .map(|v| v.description())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// Validator that inverts another validator (NOT logic)
pub struct NotValidator {
    validator: BoxedValidator,
}

impl NotValidator {
    /// Create new NOT validator
    pub fn new(validator: BoxedValidator) -> Self {
        Self { validator }
    }
}

impl Validator for NotValidator {
    fn validate(&self, output: &Value) -> ValidationResult {
        match self.validator.validate(output) {
            ValidationResult::Valid => ValidationResult::invalid(
                "Must NOT satisfy: ".to_string() + &self.validator.description(),
            ),
            ValidationResult::Invalid { .. } => ValidationResult::valid(),
        }
    }

    fn description(&self) -> String {
        format!("NOT({})", self.validator.description())
    }
}

/// Function-based validator for custom logic
pub struct FnValidator<F>
where
    F: Fn(&Value) -> ValidationResult + Send + Sync,
{
    func: F,
    description: String,
}

impl<F> FnValidator<F>
where
    F: Fn(&Value) -> ValidationResult + Send + Sync,
{
    /// Create new function-based validator
    pub fn new(func: F, description: impl Into<String>) -> Self {
        Self {
            func,
            description: description.into(),
        }
    }
}

impl<F> Validator for FnValidator<F>
where
    F: Fn(&Value) -> ValidationResult + Send + Sync,
{
    fn validate(&self, output: &Value) -> ValidationResult {
        (self.func)(output)
    }

    fn description(&self) -> String {
        self.description.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // ===== LengthValidator Tests =====

    #[test]
    fn test_length_validator_between() {
        let validator = LengthValidator::between(5, 10);

        assert!(validator.validate(&json!("hello")).is_valid());
        assert!(validator.validate(&json!("hello world")).is_invalid());
        assert!(validator.validate(&json!("hi")).is_invalid());
    }

    #[test]
    fn test_length_validator_min() {
        let validator = LengthValidator::min(5);

        assert!(validator.validate(&json!("hello")).is_valid());
        assert!(validator.validate(&json!("hello world")).is_valid());
        assert!(validator.validate(&json!("hi")).is_invalid());
    }

    #[test]
    fn test_length_validator_max() {
        let validator = LengthValidator::max(10);

        assert!(validator.validate(&json!("hello")).is_valid());
        assert!(validator.validate(&json!("hello world")).is_invalid());
        assert!(validator.validate(&json!("hi")).is_valid());
    }

    #[test]
    fn test_length_validator_non_string() {
        let validator = LengthValidator::between(5, 10);
        let result = validator.validate(&json!(42));

        assert!(result.is_invalid());
        assert!(result.feedback().unwrap().contains("must be a string"));
    }

    // ===== PatternValidator Tests =====

    #[test]
    fn test_pattern_validator_match() {
        let validator = PatternValidator::new(r"^\d{3}-\d{4}$").unwrap();

        assert!(validator.validate(&json!("123-4567")).is_valid());
        assert!(validator.validate(&json!("abc-defg")).is_invalid());
    }

    #[test]
    fn test_pattern_validator_email() {
        let validator = PatternValidator::new(r"^[\w\.\-]+@[\w\.\-]+\.\w+$").unwrap();

        assert!(validator.validate(&json!("user@example.com")).is_valid());
        assert!(validator.validate(&json!("invalid-email")).is_invalid());
    }

    // ===== NotEmptyValidator Tests =====

    #[test]
    fn test_not_empty_validator_string() {
        let validator = NotEmptyValidator;

        assert!(validator.validate(&json!("hello")).is_valid());
        assert!(validator.validate(&json!("")).is_invalid());
        assert!(validator.validate(&json!("   ")).is_invalid());
    }

    #[test]
    fn test_not_empty_validator_array() {
        let validator = NotEmptyValidator;

        assert!(validator.validate(&json!(["item"])).is_valid());
        assert!(validator.validate(&json!([])).is_invalid());
    }

    #[test]
    fn test_not_empty_validator_null() {
        let validator = NotEmptyValidator;
        assert!(validator.validate(&Value::Null).is_invalid());
    }

    // ===== ContainsValidator Tests =====

    #[test]
    fn test_contains_validator_case_sensitive() {
        let validator = ContainsValidator::new("hello");

        assert!(validator.validate(&json!("hello world")).is_valid());
        assert!(validator.validate(&json!("Hello world")).is_invalid());
        assert!(validator.validate(&json!("goodbye")).is_invalid());
    }

    #[test]
    fn test_contains_validator_case_insensitive() {
        let validator = ContainsValidator::case_insensitive("hello");

        assert!(validator.validate(&json!("hello world")).is_valid());
        assert!(validator.validate(&json!("Hello World")).is_valid());
        assert!(validator.validate(&json!("HELLO")).is_valid());
        assert!(validator.validate(&json!("goodbye")).is_invalid());
    }

    // ===== ItemCountValidator Tests =====

    #[test]
    fn test_item_count_validator_between() {
        let validator = ItemCountValidator::between(2, 5);

        assert!(validator.validate(&json!(["a", "b"])).is_valid());
        assert!(validator.validate(&json!(["a"])).is_invalid());
        assert!(validator
            .validate(&json!(["a", "b", "c", "d", "e", "f"]))
            .is_invalid());
    }

    #[test]
    fn test_item_count_validator_min() {
        let validator = ItemCountValidator::min(2);

        assert!(validator.validate(&json!(["a", "b"])).is_valid());
        assert!(validator.validate(&json!(["a", "b", "c"])).is_valid());
        assert!(validator.validate(&json!(["a"])).is_invalid());
    }

    #[test]
    fn test_item_count_validator_max() {
        let validator = ItemCountValidator::max(3);

        assert!(validator.validate(&json!(["a", "b"])).is_valid());
        assert!(validator
            .validate(&json!(["a", "b", "c", "d"]))
            .is_invalid());
    }

    // ===== UniqueItemsValidator Tests =====

    #[test]
    fn test_unique_items_validator() {
        let validator = UniqueItemsValidator;

        assert!(validator.validate(&json!(["a", "b", "c"])).is_valid());
        assert!(validator.validate(&json!(["a", "b", "a"])).is_invalid());
        assert!(validator.validate(&json!([1, 2, 3])).is_valid());
        assert!(validator.validate(&json!([1, 2, 1])).is_invalid());
    }

    // ===== Combinator Validator Tests =====

    #[test]
    fn test_all_validator_all_pass() {
        let validators: Vec<BoxedValidator> = vec![
            Box::new(NotEmptyValidator),
            Box::new(LengthValidator::min(5)),
        ];

        let validator = AllValidator::new(validators);
        assert!(validator.validate(&json!("hello world")).is_valid());
    }

    #[test]
    fn test_all_validator_one_fails() {
        let validators: Vec<BoxedValidator> = vec![
            Box::new(NotEmptyValidator),
            Box::new(LengthValidator::min(20)),
        ];

        let validator = AllValidator::new(validators);
        let result = validator.validate(&json!("hello"));

        assert!(result.is_invalid());
        assert!(result.feedback().unwrap().contains("too short"));
    }

    #[test]
    fn test_any_validator_one_passes() {
        let validators: Vec<BoxedValidator> = vec![
            Box::new(LengthValidator::max(3)),
            Box::new(LengthValidator::min(20)),
        ];

        let validator = AnyValidator::new(validators);
        assert!(validator.validate(&json!("hi")).is_valid());
        assert!(validator
            .validate(&json!("very long string here"))
            .is_valid());
    }

    #[test]
    fn test_any_validator_all_fail() {
        let validators: Vec<BoxedValidator> = vec![
            Box::new(LengthValidator::max(3)),
            Box::new(LengthValidator::min(20)),
        ];

        let validator = AnyValidator::new(validators);
        let result = validator.validate(&json!("medium"));

        assert!(result.is_invalid());
    }

    #[test]
    fn test_not_validator() {
        let validator = NotValidator::new(Box::new(ContainsValidator::new("bad")));

        assert!(validator.validate(&json!("good text")).is_valid());
        assert!(validator.validate(&json!("bad text")).is_invalid());
    }

    // ===== FnValidator Tests =====

    #[test]
    fn test_fn_validator() {
        let validator = FnValidator::new(
            |value| {
                if let Some(s) = value.as_str() {
                    if s.split_whitespace().count() > 3 {
                        ValidationResult::valid()
                    } else {
                        ValidationResult::invalid("Must have more than 3 words")
                    }
                } else {
                    ValidationResult::invalid("Must be a string")
                }
            },
            "Word count > 3",
        );

        assert!(validator.validate(&json!("one two three four")).is_valid());
        assert!(validator.validate(&json!("one two")).is_invalid());
    }

    // ===== Description Tests =====

    #[test]
    fn test_validator_descriptions() {
        let length = LengthValidator::between(5, 10);
        assert!(length.description().contains("5"));
        assert!(length.description().contains("10"));

        let pattern = PatternValidator::new(r"^\d+$").unwrap();
        assert!(pattern.description().contains("Pattern"));

        let not_empty = NotEmptyValidator;
        assert_eq!(not_empty.description(), "Non-empty value");

        let contains = ContainsValidator::new("test");
        assert!(contains.description().contains("test"));

        let items = ItemCountValidator::between(1, 10);
        assert!(items.description().contains("1"));
        assert!(items.description().contains("10"));

        let unique = UniqueItemsValidator;
        assert!(unique.description().contains("unique"));
    }

    #[test]
    fn test_combinator_descriptions() {
        let validators: Vec<BoxedValidator> = vec![
            Box::new(NotEmptyValidator),
            Box::new(LengthValidator::min(5)),
        ];

        let all = AllValidator::new(validators);
        let desc = all.description();
        assert!(desc.contains("All of"));
        assert!(desc.contains("Non-empty"));

        let validators: Vec<BoxedValidator> = vec![
            Box::new(LengthValidator::max(3)),
            Box::new(LengthValidator::min(20)),
        ];

        let any = AnyValidator::new(validators);
        let desc = any.description();
        assert!(desc.contains("Any of"));

        let not = NotValidator::new(Box::new(NotEmptyValidator));
        let desc = not.description();
        assert!(desc.contains("NOT"));
    }
}
