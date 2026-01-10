//! Signature Validator - Runtime Constraint Validation
//!
//! Validates JSON input against Signature field constraints at runtime.
//! Supports type checking, required fields, string/array constraints, and enum values.
//! Collects all validation errors before returning to provide comprehensive feedback.

use crate::dspy::field::FieldConstraints;
use crate::dspy::signature::Signature;
use regex::Regex;
use serde_json::{json, Value};

use super::validation_error::{ValidationError, ValidationErrorDetail, ValidationErrorType};

/// Signature validator for runtime constraint validation
///
/// Validates JSON input against Signature field constraints. Supports:
/// - Required field checking
/// - Type validation
/// - String constraints (min/max length, regex pattern)
/// - Numeric constraints (min/max values)
/// - Array constraints (min/max items)
/// - Enum value validation
///
/// Collects all validation errors before returning, enabling comprehensive error reporting.
#[derive(Debug, Clone)]
pub struct SignatureValidator {
    signature: Signature,
}

impl SignatureValidator {
    /// Create a new validator from a Signature
    pub fn new(signature: Signature) -> Self {
        Self { signature }
    }

    /// Validate JSON input against the signature's input field constraints
    ///
    /// # Arguments
    /// * `input` - JSON value to validate (typically an object with field values)
    ///
    /// # Returns
    /// - `Ok(())` if validation passes
    /// - `Err(ValidationError)` containing all validation errors if validation fails
    ///
    /// # Example
    /// ```ignore
    /// use serde_json::json;
    /// use ggen_ai::dspy::{Signature, InputField, SignatureValidator};
    ///
    /// let sig = Signature::new("QA", "Question answering")
    ///     .with_input(InputField::new("question", "Question text", "String"));
    ///
    /// let validator = SignatureValidator::new(sig);
    ///
    /// // Valid input
    /// let valid = json!({"question": "What is Rust?"});
    /// assert!(validator.validate(&valid).is_ok());
    ///
    /// // Invalid input
    /// let invalid = json!({"question": null});
    /// assert!(validator.validate(&invalid).is_err());
    /// ```
    pub fn validate(&self, input: &Value) -> Result<(), ValidationError> {
        let mut errors = Vec::new();

        // Ensure input is an object
        let obj = match input.as_object() {
            Some(o) => o,
            None => {
                let detail = ValidationErrorDetail::new(
                    "_root",
                    ValidationErrorType::TypeMismatch,
                    "input must be a JSON object",
                );
                return Err(ValidationError::new(detail));
            }
        };

        // Validate each input field
        for field in &self.signature.inputs {
            let field_name = field.name();

            // Check if field is present
            let value = match obj.get(field_name) {
                Some(v) => v,
                None => {
                    if field.constraints.required {
                        errors.push(ValidationErrorDetail::new(
                            field_name,
                            ValidationErrorType::FieldMissing,
                            "required field is missing",
                        ));
                    }
                    continue;
                }
            };

            // Validate the field value against its constraints
            if let Err(validation_err) = self.validate_field(field_name, field.type_annotation(), value, &field.constraints) {
                errors.extend(validation_err.errors);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(ValidationError::multiple(errors))
        }
    }

    /// Validate a single field value against its constraints
    fn validate_field(
        &self,
        field_name: &str,
        type_annotation: &str,
        value: &Value,
        constraints: &FieldConstraints,
    ) -> Result<(), ValidationError> {
        let mut errors = Vec::new();

        // Check null value
        if value.is_null() {
            if constraints.required {
                errors.push(ValidationErrorDetail::new(
                    field_name,
                    ValidationErrorType::NullValue,
                    "field is required but value is null",
                ));
            }
            if errors.is_empty() {
                return Ok(());
            } else {
                return Err(ValidationError::multiple(errors));
            }
        }

        // Type validation
        if let Err(type_err) = self.validate_type(field_name, type_annotation, value) {
            errors.extend(type_err.errors);
        }

        // Enum constraint validation
        if let Some(ref enum_values) = constraints.enum_values {
            if let Err(enum_err) = self.validate_enum(field_name, value, enum_values) {
                errors.extend(enum_err.errors);
            }
        }

        // String constraints
        if let Some(s) = value.as_str() {
            // Min length
            if let Some(min) = constraints.min_length {
                if s.len() < min {
                    errors.push(ValidationErrorDetail::new(
                        field_name,
                        ValidationErrorType::StringTooShort,
                        format!("string length {} is less than minimum {}", s.len(), min),
                    ));
                }
            }

            // Max length
            if let Some(max) = constraints.max_length {
                if s.len() > max {
                    errors.push(ValidationErrorDetail::new(
                        field_name,
                        ValidationErrorType::StringTooLong,
                        format!("string length {} exceeds maximum {}", s.len(), max),
                    ));
                }
            }

            // Pattern constraint
            if let Some(ref pattern) = constraints.pattern {
                if let Err(pattern_err) = self.validate_pattern(field_name, s, pattern) {
                    errors.extend(pattern_err.errors);
                }
            }
        }

        // Array constraints
        if let Some(arr) = value.as_array() {
            // Min items
            if let Some(min) = constraints.min_items {
                if arr.len() < min {
                    errors.push(ValidationErrorDetail::new(
                        field_name,
                        ValidationErrorType::TooFewItems,
                        format!("array length {} is less than minimum {}", arr.len(), min),
                    ));
                }
            }

            // Max items
            if let Some(max) = constraints.max_items {
                if arr.len() > max {
                    errors.push(ValidationErrorDetail::new(
                        field_name,
                        ValidationErrorType::TooManyItems,
                        format!("array length {} exceeds maximum {}", arr.len(), max),
                    ));
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(ValidationError::multiple(errors))
        }
    }

    /// Validate field type matches expected type annotation
    fn validate_type(
        &self,
        field_name: &str,
        type_annotation: &str,
        value: &Value,
    ) -> Result<(), ValidationError> {
        let type_annotation = type_annotation.trim();

        // Handle Vec<T> types
        if type_annotation.starts_with("Vec<") && type_annotation.ends_with('>') {
            if !value.is_array() {
                let detail = ValidationErrorDetail::new(
                    field_name,
                    ValidationErrorType::TypeMismatch,
                    format!("expected array ({}), got {}", type_annotation, self.value_type_name(value)),
                );
                return Err(ValidationError::new(detail));
            }
            return Ok(());
        }

        // Handle Option<T> types - nullable
        if type_annotation.starts_with("Option<") && type_annotation.ends_with('>') {
            if value.is_null() {
                return Ok(());
            }
            let inner_type = &type_annotation[7..type_annotation.len() - 1];
            return self.validate_type(field_name, inner_type, value);
        }

        // Basic type checking
        let expected_json_type = match type_annotation {
            "String" | "str" | "&str" => "string",
            "i32" | "i64" | "i16" | "i8" | "u32" | "u64" | "u16" | "u8" | "isize" | "usize" | "int" => {
                "integer or number"
            }
            "f32" | "f64" | "float" | "double" => "number",
            "bool" | "boolean" => "boolean",
            _ => "value",
        };

        let is_valid = match type_annotation {
            "String" | "str" | "&str" => value.is_string(),
            "i32" | "i64" | "i16" | "i8" | "u32" | "u64" | "u16" | "u8" | "isize" | "usize" | "int" => {
                value.is_number()
            }
            "f32" | "f64" | "float" | "double" => value.is_number(),
            "bool" | "boolean" => value.is_boolean(),
            _ => true, // Unknown types pass type check
        };

        if !is_valid {
            let detail = ValidationErrorDetail::new(
                field_name,
                ValidationErrorType::TypeMismatch,
                format!("expected {}, got {}", expected_json_type, self.value_type_name(value)),
            );
            return Err(ValidationError::new(detail));
        }

        Ok(())
    }

    /// Validate enum constraint
    fn validate_enum(
        &self,
        field_name: &str,
        value: &Value,
        enum_values: &[String],
    ) -> Result<(), ValidationError> {
        let value_str = match value {
            Value::String(s) => s.clone(),
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            _ => {
                let detail = ValidationErrorDetail::new(
                    field_name,
                    ValidationErrorType::EnumConstraintViolation,
                    "enum constraint can only be applied to scalar values",
                );
                return Err(ValidationError::new(detail));
            }
        };

        if !enum_values.contains(&value_str) {
            let detail = ValidationErrorDetail::new(
                field_name,
                ValidationErrorType::EnumConstraintViolation,
                format!(
                    "value '{}' is not in allowed enum values: {}",
                    value_str,
                    enum_values.join(", ")
                ),
            );
            return Err(ValidationError::new(detail));
        }

        Ok(())
    }

    /// Validate regex pattern constraint
    fn validate_pattern(
        &self,
        field_name: &str,
        value: &str,
        pattern: &str,
    ) -> Result<(), ValidationError> {
        match Regex::new(pattern) {
            Ok(regex) => {
                if !regex.is_match(value) {
                    let detail = ValidationErrorDetail::new(
                        field_name,
                        ValidationErrorType::PatternMismatch,
                        format!("value '{}' does not match pattern '{}'", value, pattern),
                    );
                    Err(ValidationError::new(detail))
                } else {
                    Ok(())
                }
            }
            Err(e) => {
                let detail = ValidationErrorDetail::new(
                    field_name,
                    ValidationErrorType::InvalidPattern,
                    format!("invalid regex pattern '{}': {}", pattern, e),
                );
                Err(ValidationError::new(detail))
            }
        }
    }

    /// Get human-readable type name for a JSON value
    fn value_type_name(&self, value: &Value) -> &'static str {
        match value {
            Value::Null => "null",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
        }
    }

    /// Validate input and return JSON error details on failure
    ///
    /// Returns a JSON structure with error details instead of an error type.
    /// Useful for API responses or logging.
    pub fn validate_to_json(&self, input: &Value) -> serde_json::Value {
        match self.validate(input) {
            Ok(()) => json!({
                "valid": true,
                "errors": null
            }),
            Err(err) => json!({
                "valid": false,
                "errors": err.to_json()
            }),
        }
    }

    /// Get reference to the signature being validated
    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    /// Get mutable reference to the signature being validated
    pub fn signature_mut(&mut self) -> &mut Signature {
        &mut self.signature
    }

    /// Create a validator from a Signature (convenience method)
    pub fn from_signature(sig: Signature) -> Self {
        Self::new(sig)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::FieldConstraints;
    use crate::dspy::field::InputField;

    // ===== Creation Tests =====

    #[test]
    fn test_validator_creation() {
        let sig = Signature::new("Test", "Test signature");
        let validator = SignatureValidator::new(sig.clone());

        assert_eq!(validator.signature().name, "Test");
    }

    #[test]
    fn test_validator_from_signature() {
        let sig = Signature::new("Test", "Test signature");
        let validator = SignatureValidator::from_signature(sig.clone());

        assert_eq!(validator.signature().name, "Test");
    }

    // ===== Basic Type Validation Tests =====

    #[test]
    fn test_validate_string_type() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("name", "Name", "String"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"name": "John"});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"name": 123});
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[test]
    fn test_validate_integer_type() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("age", "Age", "i32"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"age": 25});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"age": "twenty-five"});
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[test]
    fn test_validate_float_type() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("score", "Score", "f64"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"score": 3.14});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"score": "3.14"});
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[test]
    fn test_validate_boolean_type() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("active", "Active", "bool"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"active": true});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"active": "yes"});
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[test]
    fn test_validate_array_type() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("items", "Items", "Vec<String>"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"items": ["a", "b", "c"]});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"items": "not an array"});
        assert!(validator.validate(&invalid_input).is_err());
    }

    // ===== Required Field Tests =====

    #[test]
    fn test_required_field_present() {
        let mut field = InputField::new("email", "Email", "String");
        field.constraints = FieldConstraints::new().required(true);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"email": "user@example.com"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_required_field_missing() {
        let mut field = InputField::new("email", "Email", "String");
        field.constraints = FieldConstraints::new().required(true);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.error_count(), 1);
        assert_eq!(err.errors[0].error_type, ValidationErrorType::FieldMissing);
    }

    #[test]
    fn test_required_field_null() {
        let mut field = InputField::new("email", "Email", "String");
        field.constraints = FieldConstraints::new().required(true);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"email": null});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.error_count(), 1);
        assert_eq!(err.errors[0].error_type, ValidationErrorType::NullValue);
    }

    #[test]
    fn test_optional_field_missing() {
        let field = InputField::new("nickname", "Nickname", "String");
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_optional_field_null() {
        let field = InputField::new("nickname", "Nickname", "String");
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"nickname": null});
        assert!(validator.validate(&valid_input).is_ok());
    }

    // ===== String Constraint Tests =====

    #[test]
    fn test_string_min_length_passes() {
        let field = InputField::new("username", "Username", "String")
            .with_min_length(3);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"username": "john"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_string_min_length_fails() {
        let field = InputField::new("username", "Username", "String")
            .with_min_length(5);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"username": "bob"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::StringTooShort);
    }

    #[test]
    fn test_string_max_length_passes() {
        let field = InputField::new("comment", "Comment", "String")
            .with_max_length(100);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"comment": "This is a short comment"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_string_max_length_fails() {
        let field = InputField::new("comment", "Comment", "String")
            .with_max_length(20);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"comment": "This is a very long comment that exceeds the maximum length"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::StringTooLong);
    }

    #[test]
    fn test_string_length_both_bounds() {
        let field = InputField::new("code", "Code", "String")
            .with_min_length(2)
            .with_max_length(5);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"code": "ABC"});
        assert!(validator.validate(&valid_input).is_ok());

        let too_short = json!({"code": "A"});
        assert!(validator.validate(&too_short).is_err());

        let too_long = json!({"code": "ABCDEF"});
        assert!(validator.validate(&too_long).is_err());
    }

    // ===== Pattern Constraint Tests =====

    #[test]
    fn test_pattern_constraint_passes() {
        let field = InputField::new("email", "Email", "String")
            .with_pattern(r"^[a-z]+@[a-z]+\.[a-z]+$");

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"email": "user@example.com"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_pattern_constraint_fails() {
        let field = InputField::new("email", "Email", "String")
            .with_pattern(r"^[a-z]+@[a-z]+\.[a-z]+$");

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"email": "not-an-email"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::PatternMismatch);
    }

    #[test]
    fn test_pattern_constraint_invalid_regex() {
        let field = InputField::new("field", "Field", "String")
            .with_pattern("[invalid(regex");

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let input = json!({"field": "any value"});
        let result = validator.validate(&input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::InvalidPattern);
    }

    // ===== Enum Constraint Tests =====

    #[test]
    fn test_enum_constraint_passes() {
        let field = InputField::new("status", "Status", "String")
            .with_enum_values(vec!["active".to_string(), "inactive".to_string()]);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"status": "active"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_enum_constraint_fails() {
        let field = InputField::new("status", "Status", "String")
            .with_enum_values(vec!["active".to_string(), "inactive".to_string()]);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"status": "pending"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::EnumConstraintViolation);
    }

    #[test]
    fn test_enum_with_numeric_values() {
        let field = InputField::new("level", "Level", "i32")
            .with_enum_values(vec!["1".to_string(), "2".to_string(), "3".to_string()]);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"level": 1});
        assert!(validator.validate(&valid_input).is_ok());

        let invalid_input = json!({"level": 5});
        assert!(validator.validate(&invalid_input).is_err());
    }

    // ===== Array Constraint Tests =====

    #[test]
    fn test_array_min_items_passes() {
        let field = InputField::new("tags", "Tags", "Vec<String>")
            .with_min_items(1);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"tags": ["tag1", "tag2"]});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_array_min_items_fails() {
        let field = InputField::new("tags", "Tags", "Vec<String>")
            .with_min_items(1);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"tags": []});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::TooFewItems);
    }

    #[test]
    fn test_array_max_items_passes() {
        let field = InputField::new("tags", "Tags", "Vec<String>")
            .with_max_items(5);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"tags": ["tag1", "tag2", "tag3"]});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_array_max_items_fails() {
        let field = InputField::new("tags", "Tags", "Vec<String>")
            .with_max_items(3);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"tags": ["a", "b", "c", "d", "e"]});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.errors[0].error_type, ValidationErrorType::TooManyItems);
    }

    #[test]
    fn test_array_items_constraints() {
        let field = InputField::new("scores", "Scores", "Vec<i32>")
            .with_min_items(2)
            .with_max_items(5);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"scores": [1, 2, 3]});
        assert!(validator.validate(&valid_input).is_ok());

        let too_few = json!({"scores": [1]});
        assert!(validator.validate(&too_few).is_err());

        let too_many = json!({"scores": [1, 2, 3, 4, 5, 6]});
        assert!(validator.validate(&too_many).is_err());
    }

    // ===== Multiple Error Tests =====

    #[test]
    fn test_multiple_fields_multiple_errors() {
        let field1 = InputField::new("name", "Name", "String")
            .required(true)
            .with_min_length(2);

        let field2 = InputField::new("email", "Email", "String")
            .required(true)
            .with_pattern(r"^[a-z]+@[a-z]+\.[a-z]+$");

        let sig = Signature::new("Test", "Test")
            .with_input(field1)
            .with_input(field2);

        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"name": "A", "email": "invalid"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        assert_eq!(err.error_count(), 2);
    }

    #[test]
    fn test_multiple_field_errors_multiple_constraints() {
        let field = InputField::new("username", "Username", "String")
            .required(true)
            .with_min_length(3)
            .with_max_length(20)
            .with_pattern(r"^[a-zA-Z0-9_]+$");

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        // Input too short and has invalid characters
        let invalid_input = json!({"username": "a@"});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        // Should have multiple errors for this field
        assert!(err.error_count() >= 1);
    }

    #[test]
    fn test_errors_for_field() {
        let field1 = InputField::new("email", "Email", "String")
            .required(true);

        let field2 = InputField::new("phone", "Phone", "String");

        let sig = Signature::new("Test", "Test")
            .with_input(field1)
            .with_input(field2);

        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"email": null});
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());

        let err = result.unwrap_err();
        let email_errors = err.errors_for_field("email");
        assert!(!email_errors.is_empty());
    }

    // ===== Edge Case Tests =====

    #[test]
    fn test_validate_non_object_input() {
        let sig = Signature::new("Test", "Test");
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!(["array"]);
        let result = validator.validate(&invalid_input);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_empty_object() {
        let sig = Signature::new("Test", "Test");
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_validate_unicode_string() {
        let field = InputField::new("name", "Name", "String")
            .with_min_length(1);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"name": "张三"});
        assert!(validator.validate(&valid_input).is_ok());
    }

    #[test]
    fn test_validate_empty_string_with_min_length() {
        let field = InputField::new("text", "Text", "String")
            .with_min_length(1);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"text": ""});
        assert!(validator.validate(&invalid_input).is_err());
    }

    #[test]
    fn test_validate_empty_array_with_min_items() {
        let field = InputField::new("items", "Items", "Vec<String>")
            .with_min_items(1);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({"items": []});
        assert!(validator.validate(&invalid_input).is_err());
    }

    // ===== JSON Error Response Tests =====

    #[test]
    fn test_validate_to_json_success() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("name", "Name", "String"));
        let validator = SignatureValidator::new(sig);

        let valid_input = json!({"name": "John"});
        let result = validator.validate_to_json(&valid_input);

        assert_eq!(result["valid"], true);
        assert!(result["errors"].is_null());
    }

    #[test]
    fn test_validate_to_json_failure() {
        let field = InputField::new("email", "Email", "String")
            .required(true);

        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let invalid_input = json!({});
        let result = validator.validate_to_json(&invalid_input);

        assert_eq!(result["valid"], false);
        assert!(!result["errors"].is_null());
        assert!(result["errors"]["error_count"].is_number());
    }

    // ===== Complex Integration Tests =====

    #[test]
    fn test_complete_validation_scenario() {
        let fields = vec![
            InputField::new("username", "Username", "String")
                .required(true)
                .with_min_length(3)
                .with_max_length(20),
            InputField::new("email", "Email", "String")
                .required(true)
                .with_pattern(r"^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$"),
            InputField::new("age", "Age", "i32")
                .required(true),
            InputField::new("tags", "Tags", "Vec<String>")
                .with_min_items(1)
                .with_max_items(5),
        ];

        let mut sig = Signature::new("UserRegistration", "Register a new user");
        for field in fields {
            sig = sig.with_input(field);
        }

        let validator = SignatureValidator::new(sig);

        // Valid input
        let valid = json!({
            "username": "johndoe",
            "email": "john@example.com",
            "age": 25,
            "tags": ["active", "premium"]
        });
        assert!(validator.validate(&valid).is_ok());

        // Invalid: username too short
        let invalid1 = json!({
            "username": "ab",
            "email": "john@example.com",
            "age": 25,
            "tags": ["active"]
        });
        assert!(validator.validate(&invalid1).is_err());

        // Invalid: email format
        let invalid2 = json!({
            "username": "johndoe",
            "email": "invalid-email",
            "age": 25,
            "tags": ["active"]
        });
        assert!(validator.validate(&invalid2).is_err());

        // Invalid: missing required field
        let invalid3 = json!({
            "username": "johndoe",
            "age": 25,
            "tags": ["active"]
        });
        assert!(validator.validate(&invalid3).is_err());

        // Valid: missing optional field
        let valid2 = json!({
            "username": "johndoe",
            "email": "john@example.com",
            "age": 25
        });
        assert!(validator.validate(&valid2).is_ok());
    }
}
