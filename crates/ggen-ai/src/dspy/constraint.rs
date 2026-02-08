//! Constraint Calculus - Formal constraint system for LLM I/O
//!
//! This module formalizes the latent constraint system that DSPy accidentally gestures at.
//! It provides:
//! - Atomic constraints (structural primitives)
//! - Constraint composition (conjunction, disjunction)
//! - Constraint satisfaction checking (single-variable CSP)
//! - Constraint-aware decoding (the missing piece for output validation)
//!
//! Key insight: LLM-centric systems are constraint systems with stochastic solvers.
//! "Prompting" is a compilation phase. Constraints make the nondeterminism cleanly isolated.

use crate::dspy::field::{FieldConstraints, OutputField};
use crate::error::{GgenAiError, Result};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

/// Atomic constraint - the primitive building blocks of the calculus
///
/// These are single, indivisible constraints that can be composed.
/// Each variant maps directly to a SHACL property or JSON Schema constraint.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Constraint {
    /// Value must be present (not null)
    Required,

    /// String minimum length
    MinLength(usize),

    /// String maximum length
    MaxLength(usize),

    /// String must match regex pattern
    Pattern(String),

    /// Value must be one of enumerated values
    OneOf(Vec<String>),

    /// Array minimum cardinality
    MinItems(usize),

    /// Array maximum cardinality
    MaxItems(usize),

    /// Value must have specific JSON type
    TypeIs(JsonType),

    /// Semantic type annotation (for ontology alignment)
    SemanticType(String),

    /// XSD datatype constraint
    Datatype(String),

    /// Numeric minimum (inclusive)
    MinValue(f64),

    /// Numeric maximum (inclusive)
    MaxValue(f64),

    /// Custom constraint with validator function name
    Custom(String),
}

/// JSON type enumeration for type constraints
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum JsonType {
    String,
    Number,
    Integer,
    Boolean,
    Array,
    Object,
    Null,
}

impl JsonType {
    /// Check if a JSON value matches this type
    pub fn matches(&self, value: &Value) -> bool {
        match self {
            JsonType::String => value.is_string(),
            JsonType::Number => value.is_number(),
            JsonType::Integer => value.is_i64() || value.is_u64(),
            JsonType::Boolean => value.is_boolean(),
            JsonType::Array => value.is_array(),
            JsonType::Object => value.is_object(),
            JsonType::Null => value.is_null(),
        }
    }

    /// Get type name for error messages
    pub fn name(&self) -> &'static str {
        match self {
            JsonType::String => "string",
            JsonType::Number => "number",
            JsonType::Integer => "integer",
            JsonType::Boolean => "boolean",
            JsonType::Array => "array",
            JsonType::Object => "object",
            JsonType::Null => "null",
        }
    }
}

/// Result of constraint satisfaction check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintViolation {
    /// The constraint that was violated
    pub constraint: Constraint,

    /// Human-readable explanation
    pub message: String,

    /// The actual value that failed (for debugging)
    pub actual_value: Option<String>,
}

impl ConstraintViolation {
    pub fn new(constraint: Constraint, message: impl Into<String>) -> Self {
        Self {
            constraint,
            message: message.into(),
            actual_value: None,
        }
    }

    pub fn with_value(mut self, value: impl Into<String>) -> Self {
        self.actual_value = Some(value.into());
        self
    }
}

/// Constraint set - a composition of atomic constraints
///
/// Represents a conjunction (AND) of constraints by default.
/// For disjunction (OR), use `ConstraintSet::any_of`.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ConstraintSet {
    /// All constraints must be satisfied (conjunction)
    pub all: Vec<Constraint>,

    /// At least one constraint must be satisfied (disjunction)
    pub any: Vec<Vec<Constraint>>,
}

impl ConstraintSet {
    /// Create an empty constraint set
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a constraint that must be satisfied
    pub fn must(mut self, constraint: Constraint) -> Self {
        self.all.push(constraint);
        self
    }

    /// Add a disjunction - at least one of the constraint groups must be satisfied
    pub fn any_of(mut self, constraints: Vec<Constraint>) -> Self {
        self.any.push(constraints);
        self
    }

    /// Build from FieldConstraints (the existing type)
    pub fn from_field_constraints(fc: &FieldConstraints) -> Self {
        let mut set = Self::new();

        if fc.required {
            set = set.must(Constraint::Required);
        }

        if let Some(min) = fc.min_length {
            set = set.must(Constraint::MinLength(min));
        }

        if let Some(max) = fc.max_length {
            set = set.must(Constraint::MaxLength(max));
        }

        if let Some(ref pattern) = fc.pattern {
            set = set.must(Constraint::Pattern(pattern.clone()));
        }

        if let Some(ref values) = fc.enum_values {
            set = set.must(Constraint::OneOf(values.clone()));
        }

        if let Some(min) = fc.min_items {
            set = set.must(Constraint::MinItems(min));
        }

        if let Some(max) = fc.max_items {
            set = set.must(Constraint::MaxItems(max));
        }

        if let Some(ref semantic) = fc.semantic_type {
            set = set.must(Constraint::SemanticType(semantic.clone()));
        }

        if let Some(ref datatype) = fc.datatype {
            set = set.must(Constraint::Datatype(datatype.clone()));
        }

        set
    }

    /// Check if any constraints are defined
    pub fn is_empty(&self) -> bool {
        self.all.is_empty() && self.any.is_empty()
    }

    /// Check if a value satisfies all constraints
    ///
    /// Returns Ok(()) if satisfied, Err with all violations otherwise.
    pub fn check(&self, value: &Value) -> std::result::Result<(), Vec<ConstraintViolation>> {
        let mut violations = Vec::new();

        // Check all conjunctive constraints
        for constraint in &self.all {
            if let Err(v) = check_single_constraint(constraint, value) {
                violations.push(v);
            }
        }

        // Check disjunctive constraints (any_of groups)
        for group in &self.any {
            let group_satisfied = group
                .iter()
                .any(|c| check_single_constraint(c, value).is_ok());
            if !group_satisfied && !group.is_empty() {
                violations.push(ConstraintViolation::new(
                    group[0].clone(),
                    format!(
                        "None of the {} alternative constraints were satisfied",
                        group.len()
                    ),
                ));
            }
        }

        if violations.is_empty() {
            Ok(())
        } else {
            Err(violations)
        }
    }
}

/// Check a single constraint against a value
fn check_single_constraint(
    constraint: &Constraint, value: &Value,
) -> std::result::Result<(), ConstraintViolation> {
    match constraint {
        Constraint::Required => {
            if value.is_null() {
                Err(ConstraintViolation::new(
                    constraint.clone(),
                    "Value is required but was null",
                ))
            } else {
                Ok(())
            }
        }

        Constraint::MinLength(min) => {
            if let Some(s) = value.as_str() {
                if s.len() < *min {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("String length {} is less than minimum {}", s.len(), min),
                    )
                    .with_value(s))
                } else {
                    Ok(())
                }
            } else {
                Ok(()) // Non-strings pass length checks
            }
        }

        Constraint::MaxLength(max) => {
            if let Some(s) = value.as_str() {
                if s.len() > *max {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("String length {} exceeds maximum {}", s.len(), max),
                    )
                    .with_value(s))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        Constraint::Pattern(pattern) => {
            if let Some(s) = value.as_str() {
                match Regex::new(pattern) {
                    Ok(re) => {
                        if re.is_match(s) {
                            Ok(())
                        } else {
                            Err(ConstraintViolation::new(
                                constraint.clone(),
                                format!("Value does not match pattern '{}'", pattern),
                            )
                            .with_value(s))
                        }
                    }
                    Err(e) => Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("Invalid regex pattern: {}", e),
                    )),
                }
            } else {
                Ok(())
            }
        }

        Constraint::OneOf(values) => {
            let value_str = match value {
                Value::String(s) => s.clone(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => {
                    return Err(ConstraintViolation::new(
                        constraint.clone(),
                        "OneOf constraint requires scalar value",
                    ));
                }
            };

            if values.contains(&value_str) {
                Ok(())
            } else {
                Err(ConstraintViolation::new(
                    constraint.clone(),
                    format!("Value '{}' not in allowed values: {:?}", value_str, values),
                )
                .with_value(&value_str))
            }
        }

        Constraint::MinItems(min) => {
            if let Some(arr) = value.as_array() {
                if arr.len() < *min {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("Array has {} items, minimum is {}", arr.len(), min),
                    ))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        Constraint::MaxItems(max) => {
            if let Some(arr) = value.as_array() {
                if arr.len() > *max {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("Array has {} items, maximum is {}", arr.len(), max),
                    ))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        Constraint::TypeIs(json_type) => {
            if json_type.matches(value) {
                Ok(())
            } else {
                Err(ConstraintViolation::new(
                    constraint.clone(),
                    format!(
                        "Expected type {}, got {}",
                        json_type.name(),
                        value_type_name(value)
                    ),
                ))
            }
        }

        Constraint::MinValue(min) => {
            if let Some(n) = value.as_f64() {
                if n < *min {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("Value {} is less than minimum {}", n, min),
                    ))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        Constraint::MaxValue(max) => {
            if let Some(n) = value.as_f64() {
                if n > *max {
                    Err(ConstraintViolation::new(
                        constraint.clone(),
                        format!("Value {} exceeds maximum {}", n, max),
                    ))
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        // Semantic constraints are metadata - they don't fail validation
        Constraint::SemanticType(_) | Constraint::Datatype(_) | Constraint::Custom(_) => Ok(()),
    }
}

/// Get human-readable type name for a JSON value
fn value_type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "boolean",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

// ============================================================================
// Constraint-Aware Decoding - The Critical Missing Piece
// ============================================================================

/// Decode and validate LLM output against field constraints
///
/// This is the function that makes "teleprompting" become constraint repair
/// and turns retries into algorithmic operations.
///
/// # Arguments
/// * `raw` - Raw string output from LLM
/// * `output_fields` - Output field definitions with constraints
///
/// # Returns
/// * `Ok(HashMap)` - Validated output values
/// * `Err` - Decoding or validation failures
pub fn decode_and_validate(
    raw: &str, output_fields: &[OutputField],
) -> Result<HashMap<String, Value>> {
    let mut outputs = HashMap::new();
    let mut errors = Vec::new();

    // First, try to parse as JSON
    if let Ok(json_value) = serde_json::from_str::<Value>(raw) {
        // JSON parsed successfully - validate each field
        if let Some(obj) = json_value.as_object() {
            for field in output_fields {
                let field_name = field.name();
                let constraints = ConstraintSet::from_field_constraints(&field.constraints);

                match obj.get(field_name) {
                    Some(value) => {
                        // Validate the value against constraints
                        if let Err(violations) = constraints.check(value) {
                            for v in violations {
                                errors.push(format!("{}: {}", field_name, v.message));
                            }
                        } else {
                            outputs.insert(field_name.to_string(), value.clone());
                        }
                    }
                    None => {
                        // Check if field is required
                        if field.constraints.required {
                            errors.push(format!(
                                "{}: required field missing from output",
                                field_name
                            ));
                        }
                    }
                }
            }
        } else {
            // JSON is valid but not an object - try to extract single value
            if output_fields.len() == 1 {
                let field = &output_fields[0];
                let constraints = ConstraintSet::from_field_constraints(&field.constraints);

                if let Err(violations) = constraints.check(&json_value) {
                    for v in violations {
                        errors.push(format!("{}: {}", field.name(), v.message));
                    }
                } else {
                    outputs.insert(field.name().to_string(), json_value);
                }
            }
        }
    } else {
        // Not JSON - parse as structured text
        outputs = parse_structured_text(raw, output_fields)?;

        // Validate parsed outputs
        for field in output_fields {
            if let Some(value) = outputs.get(field.name()) {
                let constraints = ConstraintSet::from_field_constraints(&field.constraints);
                if let Err(violations) = constraints.check(value) {
                    for v in violations {
                        errors.push(format!("{}: {}", field.name(), v.message));
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(outputs)
    } else {
        Err(GgenAiError::validation(errors.join("; ")))
    }
}

/// Parse structured text output (field: value format)
fn parse_structured_text(
    raw: &str, output_fields: &[OutputField],
) -> Result<HashMap<String, Value>> {
    let mut outputs = HashMap::new();

    for field in output_fields {
        let field_name = field.name();

        // Try multiple patterns for field extraction
        let patterns = [
            format!("{}: ", field_name),
            format!("{}:", field_name),
            format!("**{}**:", field_name),
            format!("**{}**: ", field_name),
        ];

        for pattern in &patterns {
            if let Some(start) = raw.find(pattern) {
                let text = &raw[start + pattern.len()..];
                // Take until newline or end of string
                let value = text.lines().next().unwrap_or("").trim().to_string();

                if !value.is_empty() {
                    // Infer type from field annotation
                    let typed_value = coerce_to_type(&value, field.type_annotation());
                    outputs.insert(field_name.to_string(), typed_value);
                    break;
                }
            }
        }

        // If field not found and it's the only output, use entire response
        if !outputs.contains_key(field_name) && output_fields.len() == 1 {
            outputs.insert(
                field_name.to_string(),
                Value::String(raw.trim().to_string()),
            );
        }
    }

    Ok(outputs)
}

/// Coerce a string value to the appropriate JSON type based on type annotation
fn coerce_to_type(value: &str, type_annotation: &str) -> Value {
    let type_annotation = type_annotation.trim();

    match type_annotation {
        "i32" | "i64" | "i16" | "i8" | "u32" | "u64" | "u16" | "u8" | "isize" | "usize" => value
            .parse::<i64>()
            .map(Value::from)
            .unwrap_or_else(|_| Value::String(value.to_string())),

        "f32" | "f64" => value
            .parse::<f64>()
            .map(Value::from)
            .unwrap_or_else(|_| Value::String(value.to_string())),

        "bool" | "boolean" => match value.to_lowercase().as_str() {
            "true" | "yes" | "1" => Value::Bool(true),
            "false" | "no" | "0" => Value::Bool(false),
            _ => Value::String(value.to_string()),
        },

        _ if type_annotation.starts_with("Vec<") => {
            // Try to parse as JSON array
            if let Ok(arr) = serde_json::from_str::<Value>(value) {
                if arr.is_array() {
                    return arr;
                }
            }
            // Fall back to comma-separated
            let items: Vec<Value> = value
                .split(',')
                .map(|s| Value::String(s.trim().to_string()))
                .collect();
            Value::Array(items)
        }

        _ => Value::String(value.to_string()),
    }
}

/// Repair strategy for constraint violations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RepairStrategy {
    /// Retry with more specific instructions
    Retry,

    /// Truncate to fit length constraints
    Truncate,

    /// Use default value
    Default,

    /// Coerce to nearest valid value
    Coerce,

    /// Fail immediately
    Fail,
}

/// Suggest repair strategy for a constraint violation
pub fn suggest_repair(violation: &ConstraintViolation) -> RepairStrategy {
    match &violation.constraint {
        Constraint::MaxLength(_) => RepairStrategy::Truncate,
        Constraint::MinLength(_) => RepairStrategy::Retry,
        Constraint::Pattern(_) => RepairStrategy::Retry,
        Constraint::OneOf(_) => RepairStrategy::Retry,
        Constraint::Required => RepairStrategy::Retry,
        Constraint::TypeIs(_) => RepairStrategy::Coerce,
        Constraint::MinItems(_) | Constraint::MaxItems(_) => RepairStrategy::Retry,
        _ => RepairStrategy::Fail,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // ===== Atomic Constraint Tests =====

    #[test]
    fn test_required_constraint() {
        let constraint = Constraint::Required;
        assert!(check_single_constraint(&constraint, &json!("value")).is_ok());
        assert!(check_single_constraint(&constraint, &json!(null)).is_err());
    }

    #[test]
    fn test_min_length_constraint() {
        let constraint = Constraint::MinLength(5);
        assert!(check_single_constraint(&constraint, &json!("hello")).is_ok());
        assert!(check_single_constraint(&constraint, &json!("hi")).is_err());
        // Non-strings pass
        assert!(check_single_constraint(&constraint, &json!(123)).is_ok());
    }

    #[test]
    fn test_max_length_constraint() {
        let constraint = Constraint::MaxLength(5);
        assert!(check_single_constraint(&constraint, &json!("hello")).is_ok());
        assert!(check_single_constraint(&constraint, &json!("hello world")).is_err());
    }

    #[test]
    fn test_pattern_constraint() {
        let constraint = Constraint::Pattern(r"^\d{3}-\d{4}$".to_string());
        assert!(check_single_constraint(&constraint, &json!("123-4567")).is_ok());
        assert!(check_single_constraint(&constraint, &json!("invalid")).is_err());
    }

    #[test]
    fn test_one_of_constraint() {
        let constraint = Constraint::OneOf(vec!["red".to_string(), "green".to_string()]);
        assert!(check_single_constraint(&constraint, &json!("red")).is_ok());
        assert!(check_single_constraint(&constraint, &json!("blue")).is_err());
    }

    #[test]
    fn test_type_is_constraint() {
        let constraint = Constraint::TypeIs(JsonType::String);
        assert!(check_single_constraint(&constraint, &json!("text")).is_ok());
        assert!(check_single_constraint(&constraint, &json!(123)).is_err());
    }

    #[test]
    fn test_min_max_items_constraints() {
        let min_constraint = Constraint::MinItems(2);
        assert!(check_single_constraint(&min_constraint, &json!([1, 2, 3])).is_ok());
        assert!(check_single_constraint(&min_constraint, &json!([1])).is_err());

        let max_constraint = Constraint::MaxItems(2);
        assert!(check_single_constraint(&max_constraint, &json!([1, 2])).is_ok());
        assert!(check_single_constraint(&max_constraint, &json!([1, 2, 3])).is_err());
    }

    // ===== ConstraintSet Tests =====

    #[test]
    fn test_constraint_set_conjunction() {
        let set = ConstraintSet::new()
            .must(Constraint::Required)
            .must(Constraint::MinLength(3))
            .must(Constraint::MaxLength(10));

        assert!(set.check(&json!("hello")).is_ok());
        assert!(set.check(&json!("hi")).is_err()); // too short
        assert!(set.check(&json!("hello world!")).is_err()); // too long
        assert!(set.check(&json!(null)).is_err()); // null
    }

    #[test]
    fn test_constraint_set_from_field_constraints() {
        let fc = FieldConstraints::new()
            .required(true)
            .min_length(5)
            .max_length(50)
            .pattern(r"^[a-zA-Z]+$");

        let set = ConstraintSet::from_field_constraints(&fc);
        assert_eq!(set.all.len(), 4);

        assert!(set.check(&json!("HelloWorld")).is_ok());
        assert!(set.check(&json!("Hi")).is_err()); // too short
        assert!(set.check(&json!("Hello123")).is_err()); // doesn't match pattern
    }

    // ===== Decode and Validate Tests =====

    #[test]
    fn test_decode_json_output() {
        let output_fields = vec![
            OutputField::new("answer", "The answer", "String"),
            OutputField::new("confidence", "Confidence score", "f64"),
        ];

        let raw = r#"{"answer": "42", "confidence": 0.95}"#;
        let result = decode_and_validate(raw, &output_fields).unwrap();

        assert_eq!(result.get("answer").unwrap(), &json!("42"));
        assert_eq!(result.get("confidence").unwrap(), &json!(0.95));
    }

    #[test]
    fn test_decode_structured_text() {
        let output_fields = vec![OutputField::new("answer", "The answer", "String")];

        let raw = "answer: The meaning of life is 42";
        let result = decode_and_validate(raw, &output_fields).unwrap();

        assert_eq!(
            result.get("answer").unwrap(),
            &json!("The meaning of life is 42")
        );
    }

    #[test]
    fn test_decode_with_constraint_validation() {
        let mut field = OutputField::new("status", "Status", "String");
        field.constraints = FieldConstraints::new()
            .required(true)
            .enum_values(vec!["active".to_string(), "inactive".to_string()]);

        let output_fields = vec![field];

        // Valid value
        let raw = r#"{"status": "active"}"#;
        assert!(decode_and_validate(raw, &output_fields).is_ok());

        // Invalid enum value
        let raw = r#"{"status": "pending"}"#;
        assert!(decode_and_validate(raw, &output_fields).is_err());
    }

    #[test]
    fn test_decode_single_value_response() {
        let output_fields = vec![OutputField::new("result", "Result", "String")];

        let raw = "This is the complete response without field labels.";
        let result = decode_and_validate(raw, &output_fields).unwrap();

        assert!(result
            .get("result")
            .unwrap()
            .as_str()
            .unwrap()
            .contains("complete response"));
    }

    #[test]
    fn test_coerce_to_integer() {
        let value = coerce_to_type("42", "i32");
        assert_eq!(value, json!(42));
    }

    #[test]
    fn test_coerce_to_float() {
        let value = coerce_to_type("3.14", "f64");
        assert_eq!(value, json!(3.14));
    }

    #[test]
    fn test_coerce_to_boolean() {
        assert_eq!(coerce_to_type("true", "bool"), json!(true));
        assert_eq!(coerce_to_type("false", "bool"), json!(false));
        assert_eq!(coerce_to_type("yes", "bool"), json!(true));
        assert_eq!(coerce_to_type("no", "bool"), json!(false));
    }

    #[test]
    fn test_coerce_to_array() {
        let value = coerce_to_type("a, b, c", "Vec<String>");
        assert!(value.is_array());
        let arr = value.as_array().unwrap();
        assert_eq!(arr.len(), 3);
    }

    // ===== Repair Strategy Tests =====

    #[test]
    fn test_repair_strategy_suggestions() {
        let max_len_violation = ConstraintViolation::new(Constraint::MaxLength(10), "too long");
        assert_eq!(suggest_repair(&max_len_violation), RepairStrategy::Truncate);

        let pattern_violation =
            ConstraintViolation::new(Constraint::Pattern(".*".to_string()), "no match");
        assert_eq!(suggest_repair(&pattern_violation), RepairStrategy::Retry);

        let required_violation = ConstraintViolation::new(Constraint::Required, "missing");
        assert_eq!(suggest_repair(&required_violation), RepairStrategy::Retry);
    }
}
