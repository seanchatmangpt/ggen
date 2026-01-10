//! Signature Validation Error Types
//!
//! Provides detailed error types for runtime validation of JSON input against Signature constraints.
//! Supports field-level and cross-field validation errors with comprehensive error messages.

use serde::{Deserialize, Serialize};
use std::fmt;

/// Detailed error information for a single validation failure
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ValidationErrorDetail {
    /// Name of the field that failed validation
    pub field: String,

    /// Type of validation error
    pub error_type: ValidationErrorType,

    /// Human-readable error message
    pub message: String,
}

impl ValidationErrorDetail {
    /// Create a new validation error detail
    pub fn new(field: impl Into<String>, error_type: ValidationErrorType, message: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            error_type,
            message: message.into(),
        }
    }
}

impl fmt::Display for ValidationErrorDetail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Field '{}': {} ({})", self.field, self.message, self.error_type)
    }
}

/// Types of validation errors that can occur
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum ValidationErrorType {
    /// Field is required but missing from input
    FieldMissing,

    /// Field value type does not match expected type
    TypeMismatch,

    /// Field value is null but field is required
    NullValue,

    /// String is shorter than minimum allowed length
    StringTooShort,

    /// String is longer than maximum allowed length
    StringTooLong,

    /// String does not match required pattern (regex)
    PatternMismatch,

    /// Value is not in the allowed enum values
    EnumConstraintViolation,

    /// Array/collection has fewer items than minimum
    TooFewItems,

    /// Array/collection has more items than maximum
    TooManyItems,

    /// Invalid regex pattern in constraint
    InvalidPattern,

    /// Other constraint violation
    ConstraintViolation,
}

impl fmt::Display for ValidationErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FieldMissing => write!(f, "field_missing"),
            Self::TypeMismatch => write!(f, "type_mismatch"),
            Self::NullValue => write!(f, "null_value"),
            Self::StringTooShort => write!(f, "string_too_short"),
            Self::StringTooLong => write!(f, "string_too_long"),
            Self::PatternMismatch => write!(f, "pattern_mismatch"),
            Self::EnumConstraintViolation => write!(f, "enum_constraint_violation"),
            Self::TooFewItems => write!(f, "too_few_items"),
            Self::TooManyItems => write!(f, "too_many_items"),
            Self::InvalidPattern => write!(f, "invalid_pattern"),
            Self::ConstraintViolation => write!(f, "constraint_violation"),
        }
    }
}

/// Comprehensive validation error containing one or more validation failures
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    /// All validation errors encountered
    pub errors: Vec<ValidationErrorDetail>,

    /// Overall error summary message
    pub summary: String,
}

impl ValidationError {
    /// Create a new validation error with a single detail
    pub fn new(detail: ValidationErrorDetail) -> Self {
        let summary = format!("Validation failed: {}", detail);
        Self {
            errors: vec![detail],
            summary,
        }
    }

    /// Create a validation error from multiple details
    pub fn multiple(errors: Vec<ValidationErrorDetail>) -> Self {
        let count = errors.len();
        let summary = if count == 1 {
            format!("Validation failed: {}", errors[0])
        } else {
            let field_names: Vec<&str> = errors.iter().map(|e| e.field.as_str()).collect();
            format!("Validation failed on {} field(s): {}", count, field_names.join(", "))
        };

        Self { errors, summary }
    }

    /// Add another validation error to this collection
    pub fn add(&mut self, detail: ValidationErrorDetail) {
        self.errors.push(detail);
        let count = self.errors.len();
        let field_names: Vec<&str> = self.errors.iter().map(|e| e.field.as_str()).collect();
        self.summary = format!("Validation failed on {} field(s): {}", count, field_names.join(", "));
    }

    /// Check if any errors are present
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the number of validation errors
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// Get all errors for a specific field
    pub fn errors_for_field(&self, field: &str) -> Vec<&ValidationErrorDetail> {
        self.errors.iter().filter(|e| e.field == field).collect()
    }

    /// Convert to a structured JSON representation
    pub fn to_json(&self) -> serde_json::Value {
        serde_json::json!({
            "summary": self.summary,
            "error_count": self.error_count(),
            "errors": self.errors.iter().map(|e| serde_json::json!({
                "field": e.field,
                "error_type": e.error_type.to_string(),
                "message": e.message
            })).collect::<Vec<_>>()
        })
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.summary)?;
        for error in &self.errors {
            writeln!(f, "  - {}", error)?;
        }
        Ok(())
    }
}

impl std::error::Error for ValidationError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_error_detail_creation() {
        let detail = ValidationErrorDetail::new(
            "email",
            ValidationErrorType::PatternMismatch,
            "does not match email pattern",
        );

        assert_eq!(detail.field, "email");
        assert_eq!(detail.error_type, ValidationErrorType::PatternMismatch);
        assert!(detail.message.contains("email pattern"));
    }

    #[test]
    fn test_validation_error_single() {
        let detail = ValidationErrorDetail::new(
            "age",
            ValidationErrorType::TypeMismatch,
            "expected integer, got string",
        );
        let err = ValidationError::new(detail);

        assert_eq!(err.error_count(), 1);
        assert!(err.has_errors());
        assert!(err.summary.contains("age"));
    }

    #[test]
    fn test_validation_error_multiple() {
        let errors = vec![
            ValidationErrorDetail::new("email", ValidationErrorType::FieldMissing, "required field"),
            ValidationErrorDetail::new("age", ValidationErrorType::TypeMismatch, "expected integer"),
        ];
        let err = ValidationError::multiple(errors);

        assert_eq!(err.error_count(), 2);
        assert!(err.summary.contains("2 field(s)"));
        assert!(err.summary.contains("email"));
        assert!(err.summary.contains("age"));
    }

    #[test]
    fn test_validation_error_add() {
        let detail1 = ValidationErrorDetail::new("name", ValidationErrorType::FieldMissing, "required");
        let mut err = ValidationError::new(detail1);

        assert_eq!(err.error_count(), 1);

        let detail2 = ValidationErrorDetail::new("email", ValidationErrorType::NullValue, "cannot be null");
        err.add(detail2);

        assert_eq!(err.error_count(), 2);
        assert!(err.summary.contains("2 field(s)"));
    }

    #[test]
    fn test_validation_error_errors_for_field() {
        let errors = vec![
            ValidationErrorDetail::new("email", ValidationErrorType::FieldMissing, "required"),
            ValidationErrorDetail::new("email", ValidationErrorType::PatternMismatch, "invalid format"),
            ValidationErrorDetail::new("age", ValidationErrorType::TypeMismatch, "expected integer"),
        ];
        let err = ValidationError::multiple(errors);

        let email_errors = err.errors_for_field("email");
        assert_eq!(email_errors.len(), 2);

        let age_errors = err.errors_for_field("age");
        assert_eq!(age_errors.len(), 1);

        let other_errors = err.errors_for_field("other");
        assert!(other_errors.is_empty());
    }

    #[test]
    fn test_validation_error_to_json() {
        let detail = ValidationErrorDetail::new(
            "email",
            ValidationErrorType::PatternMismatch,
            "invalid email format",
        );
        let err = ValidationError::new(detail);

        let json = err.to_json();
        assert_eq!(json["error_count"], 1);
        assert!(json["summary"].as_str().unwrap().contains("email"));
        assert_eq!(json["errors"][0]["field"], "email");
        assert_eq!(json["errors"][0]["error_type"], "pattern_mismatch");
    }

    #[test]
    fn test_validation_error_display() {
        let errors = vec![
            ValidationErrorDetail::new("name", ValidationErrorType::FieldMissing, "required field"),
            ValidationErrorDetail::new("age", ValidationErrorType::StringTooShort, "minimum length 1"),
        ];
        let err = ValidationError::multiple(errors);

        let display_str = err.to_string();
        assert!(display_str.contains("2 field(s)"));
        assert!(display_str.contains("name"));
        assert!(display_str.contains("age"));
    }

    #[test]
    fn test_validation_error_type_display() {
        assert_eq!(ValidationErrorType::FieldMissing.to_string(), "field_missing");
        assert_eq!(ValidationErrorType::TypeMismatch.to_string(), "type_mismatch");
        assert_eq!(ValidationErrorType::PatternMismatch.to_string(), "pattern_mismatch");
        assert_eq!(ValidationErrorType::EnumConstraintViolation.to_string(), "enum_constraint_violation");
    }

    #[test]
    fn test_validation_error_serialization() {
        let detail = ValidationErrorDetail::new("field", ValidationErrorType::FieldMissing, "required");
        let err = ValidationError::new(detail);

        let json = serde_json::to_value(&err).expect("serialization failed");
        let deserialized: ValidationError = serde_json::from_value(json)
            .expect("deserialization failed");

        assert_eq!(deserialized.error_count(), err.error_count());
        assert_eq!(deserialized.errors[0].field, "field");
    }
}
