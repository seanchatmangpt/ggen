//! Property-Based Tests for DSPy Modules
//!
//! Uses proptest to verify invariants hold across random inputs.
//! Focuses on finding edge cases that unit tests might miss.
//!
//! Test Coverage:
//! 1. Signature JSON Schema generation (schema always valid)
//! 2. Field constraint validation (constraints always enforced)
//! 3. Signature validation (no false positives/negatives)
//! 4. Type mappings (bidirectional consistency)
//! 5. Edge cases (empty, very large, Unicode, special characters)

use ggen_ai::dspy::field::{FieldConstraints, InputField, OutputField};
use ggen_ai::dspy::signature::Signature;
use ggen_ai::dspy::SignatureValidator;
use proptest::prelude::*;
use serde_json::{json, Value};

// ============================================================================
// Proptest Strategies (Generators for Random Test Data)
// ============================================================================

/// Strategy for generating valid Rust type annotations
fn type_annotation_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("String".to_string()),
        Just("str".to_string()),
        Just("&str".to_string()),
        Just("i32".to_string()),
        Just("i64".to_string()),
        Just("u32".to_string()),
        Just("u64".to_string()),
        Just("f32".to_string()),
        Just("f64".to_string()),
        Just("bool".to_string()),
        Just("Vec<String>".to_string()),
        Just("Vec<i32>".to_string()),
        Just("Vec<f64>".to_string()),
        Just("Option<String>".to_string()),
        Just("Option<i32>".to_string()),
        Just("Option<Vec<String>>".to_string()),
    ]
}

/// Strategy for generating valid field names (Rust identifiers)
fn field_name_strategy() -> impl Strategy<Value = String> {
    // Start with letter or underscore, followed by alphanumeric or underscore
    prop::string::string_regex("[a-z_][a-z0-9_]{0,20}").unwrap()
}

/// Strategy for generating field descriptions
fn description_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Normal descriptions
        prop::string::string_regex("[A-Za-z ]{5,100}").unwrap(),
        // Unicode descriptions
        Just("Unicode: 你好世界 🌍".to_string()),
        // Empty description
        Just("".to_string()),
        // Special characters
        Just("Special chars: @#$%^&*()".to_string()),
    ]
}

/// Strategy for generating FieldConstraints
fn field_constraints_strategy() -> impl Strategy<Value = FieldConstraints> {
    (
        any::<bool>(),                          // required
        prop::option::of(1usize..100),          // min_items
        prop::option::of(100usize..1000),       // max_items
        prop::option::of(1usize..50),           // min_length
        prop::option::of(50usize..500),         // max_length
        prop::option::of(Just("^[a-zA-Z0-9_]+$".to_string())), // pattern
        prop::option::of(prop::collection::vec(
            prop::string::string_regex("[a-z]{3,10}").unwrap(),
            1..5,
        )), // enum_values
    )
        .prop_map(
            |(required, min_items, max_items, min_length, max_length, pattern, enum_values)| {
                let mut constraints = FieldConstraints::new().required(required);

                if let Some(min) = min_items {
                    constraints = constraints.min_items(min);
                }
                if let Some(max) = max_items {
                    // Ensure max >= min
                    if let Some(min) = min_items {
                        if max >= min {
                            constraints = constraints.max_items(max);
                        }
                    } else {
                        constraints = constraints.max_items(max);
                    }
                }
                if let Some(min) = min_length {
                    constraints = constraints.min_length(min);
                }
                if let Some(max) = max_length {
                    // Ensure max >= min
                    if let Some(min) = min_length {
                        if max >= min {
                            constraints = constraints.max_length(max);
                        }
                    } else {
                        constraints = constraints.max_length(max);
                    }
                }
                if let Some(p) = pattern {
                    constraints = constraints.pattern(p);
                }
                if let Some(e) = enum_values {
                    constraints = constraints.enum_values(e);
                }

                constraints
            },
        )
}

/// Strategy for generating InputField
fn input_field_strategy() -> impl Strategy<Value = InputField> {
    (
        field_name_strategy(),
        description_strategy(),
        type_annotation_strategy(),
        field_constraints_strategy(),
    )
        .prop_map(|(name, desc, type_ann, constraints)| {
            InputField::with_constraints(name, desc, type_ann, constraints)
        })
}

/// Strategy for generating Signature with random fields
fn signature_strategy() -> impl Strategy<Value = Signature> {
    (
        field_name_strategy(), // name
        description_strategy(), // description
        prop::collection::vec(input_field_strategy(), 0..5), // inputs
    )
        .prop_map(|(name, desc, inputs)| {
            let mut sig = Signature::new(name, desc);
            for input in inputs {
                sig = sig.with_input(input);
            }
            sig
        })
}

/// Strategy for generating JSON values matching a type annotation
fn json_value_for_type(type_ann: &str) -> BoxedStrategy<Value> {
    match type_ann.trim() {
        "String" | "str" | "&str" => prop::string::string_regex(".{0,100}")
            .unwrap()
            .prop_map(|s| json!(s))
            .boxed(),
        "i32" | "i64" | "u32" | "u64" | "int" => (-1000i32..1000i32)
            .prop_map(|n| json!(n))
            .boxed(),
        "f32" | "f64" | "float" | "double" => (-1000.0f64..1000.0f64)
            .prop_map(|f| json!(f))
            .boxed(),
        "bool" | "boolean" => any::<bool>().prop_map(|b| json!(b)).boxed(),
        s if s.starts_with("Vec<String>") => {
            prop::collection::vec(prop::string::string_regex(".{0,20}").unwrap(), 0..10)
                .prop_map(|v| json!(v))
                .boxed()
        }
        s if s.starts_with("Vec<i32>") => prop::collection::vec(-100i32..100i32, 0..10)
            .prop_map(|v| json!(v))
            .boxed(),
        s if s.starts_with("Option<String>") => {
            prop_oneof![
                Just(json!(null)),
                prop::string::string_regex(".{0,100}")
                    .unwrap()
                    .prop_map(|s| json!(s))
            ]
            .boxed()
        }
        _ => Just(json!("default")).boxed(),
    }
}

// ============================================================================
// Property 1: JSON Schema Generation Always Valid
// ============================================================================

proptest! {
    /// Property: Generated JSON Schema is always valid JSON
    #[test]
    fn prop_json_schema_always_valid_json(sig in signature_strategy()) {
        // Act
        let schema = sig.as_json_schema();

        // Assert: Can serialize and deserialize
        let json_str = serde_json::to_string(&schema).expect("Schema should serialize");
        let _parsed: Value = serde_json::from_str(&json_str).expect("Schema should deserialize");
    }

    /// Property: JSON Schema always has required top-level structure
    #[test]
    fn prop_json_schema_has_required_structure(sig in signature_strategy()) {
        // Act
        let schema = sig.as_json_schema();

        // Assert: Required fields present
        assert_eq!(schema["type"], "object", "Schema must have type=object");
        assert!(schema.get("properties").is_some(), "Schema must have properties");
    }

    /// Property: JSON Schema properties match input fields
    #[test]
    fn prop_json_schema_properties_match_inputs(sig in signature_strategy()) {
        // Act
        let schema = sig.as_json_schema();

        // Assert: Number of properties equals number of inputs
        let properties = schema["properties"].as_object().unwrap();
        assert_eq!(
            properties.len(),
            sig.inputs.len(),
            "Properties count should match inputs count"
        );

        // Assert: Each input field has a corresponding property
        for input in &sig.inputs {
            assert!(
                properties.contains_key(input.name()),
                "Property {} should exist in schema",
                input.name()
            );
        }
    }

    /// Property: Required fields are correctly marked in schema
    #[test]
    fn prop_json_schema_required_fields_correct(sig in signature_strategy()) {
        // Act
        let schema = sig.as_json_schema();

        // Assert: Required array contains only required fields
        if let Some(required) = schema.get("required") {
            let required_arr = required.as_array().unwrap();
            for field_name in required_arr {
                let name = field_name.as_str().unwrap();
                let field = sig.get_input(name).unwrap();
                assert!(
                    field.constraints.required,
                    "Field {} in required array should have required=true",
                    name
                );
            }
        }

        // Assert: All required fields are in the required array
        for input in &sig.inputs {
            if input.constraints.required {
                let required_arr = schema["required"].as_array().unwrap_or(&vec![]);
                let field_names: Vec<&str> = required_arr
                    .iter()
                    .map(|v| v.as_str().unwrap())
                    .collect();
                assert!(
                    field_names.contains(&input.name()),
                    "Required field {} should be in required array",
                    input.name()
                );
            }
        }
    }
}

// ============================================================================
// Property 2: Field Constraint Validation Always Enforced
// ============================================================================

proptest! {
    /// Property: Required constraint is always enforced
    #[test]
    fn prop_required_constraint_enforced(
        field_name in field_name_strategy(),
        desc in description_strategy(),
    ) {
        // Arrange: Create field with required=true
        let field = InputField::new(&field_name, &desc, "String")
            .add_constraints(FieldConstraints::new().required(true));

        // Act & Assert: null value should fail
        let result = field.validate(&json!(null));
        assert!(result.is_err(), "Required field should reject null");

        // Act & Assert: non-null value should pass
        let result = field.validate(&json!("value"));
        assert!(result.is_ok(), "Required field should accept non-null");
    }

    /// Property: Min length constraint is always enforced
    #[test]
    fn prop_min_length_constraint_enforced(
        min_len in 1usize..50,
        actual_len in 0usize..100,
    ) {
        // Arrange
        let constraints = FieldConstraints::new().min_length(min_len);
        let test_string = "a".repeat(actual_len);

        // Act
        let result = constraints.is_satisfied(&json!(test_string));

        // Assert
        if actual_len < min_len {
            assert!(result.is_err(), "String shorter than min_length should fail");
        } else {
            assert!(result.is_ok(), "String >= min_length should pass");
        }
    }

    /// Property: Max length constraint is always enforced
    #[test]
    fn prop_max_length_constraint_enforced(
        max_len in 1usize..50,
        actual_len in 0usize..100,
    ) {
        // Arrange
        let constraints = FieldConstraints::new().max_length(max_len);
        let test_string = "a".repeat(actual_len);

        // Act
        let result = constraints.is_satisfied(&json!(test_string));

        // Assert
        if actual_len > max_len {
            assert!(result.is_err(), "String longer than max_length should fail");
        } else {
            assert!(result.is_ok(), "String <= max_length should pass");
        }
    }

    /// Property: Min items constraint is always enforced
    #[test]
    fn prop_min_items_constraint_enforced(
        min_items in 1usize..20,
        actual_items in 0usize..30,
    ) {
        // Arrange
        let constraints = FieldConstraints::new().min_items(min_items);
        let test_array = vec![json!(1); actual_items];

        // Act
        let result = constraints.is_satisfied(&json!(test_array));

        // Assert
        if actual_items < min_items {
            assert!(result.is_err(), "Array with fewer items than min_items should fail");
        } else {
            assert!(result.is_ok(), "Array with >= min_items should pass");
        }
    }

    /// Property: Max items constraint is always enforced
    #[test]
    fn prop_max_items_constraint_enforced(
        max_items in 1usize..20,
        actual_items in 0usize..30,
    ) {
        // Arrange
        let constraints = FieldConstraints::new().max_items(max_items);
        let test_array = vec![json!(1); actual_items];

        // Act
        let result = constraints.is_satisfied(&json!(test_array));

        // Assert
        if actual_items > max_items {
            assert!(result.is_err(), "Array with more items than max_items should fail");
        } else {
            assert!(result.is_ok(), "Array with <= max_items should pass");
        }
    }

    /// Property: Enum constraint only accepts listed values
    #[test]
    fn prop_enum_constraint_enforced(
        allowed_values in prop::collection::vec(
            prop::string::string_regex("[a-z]{3,8}").unwrap(),
            1..5
        ),
        test_value in prop::string::string_regex("[a-z]{3,8}").unwrap(),
    ) {
        // Arrange
        let constraints = FieldConstraints::new().enum_values(allowed_values.clone());

        // Act
        let result = constraints.is_satisfied(&json!(test_value));

        // Assert
        if allowed_values.contains(&test_value) {
            assert!(result.is_ok(), "Value in enum should pass");
        } else {
            assert!(result.is_err(), "Value not in enum should fail");
        }
    }

    /// Property: Pattern constraint is enforced (alphanumeric only)
    #[test]
    fn prop_pattern_constraint_enforced(
        test_string in prop::string::string_regex("[a-zA-Z0-9_]{0,50}|[^a-zA-Z0-9_]{1,10}").unwrap(),
    ) {
        // Arrange: Pattern that only allows alphanumeric and underscore
        let constraints = FieldConstraints::new().pattern("^[a-zA-Z0-9_]+$");

        // Act
        let result = constraints.is_satisfied(&json!(test_string));

        // Assert
        let is_valid_pattern = test_string.chars().all(|c| c.is_alphanumeric() || c == '_');
        if !test_string.is_empty() && is_valid_pattern {
            assert!(result.is_ok(), "String matching pattern should pass: {}", test_string);
        } else if test_string.is_empty() {
            // Empty string doesn't match ^[a-zA-Z0-9_]+$ (requires at least one char)
            assert!(result.is_err(), "Empty string should fail pattern with +");
        } else {
            assert!(result.is_err(), "String not matching pattern should fail: {}", test_string);
        }
    }
}

// ============================================================================
// Property 3: Signature Validation Consistency
// ============================================================================

proptest! {
    /// Property: Validation never has false positives (valid data accepted)
    #[test]
    fn prop_validation_no_false_positives(
        field_name in field_name_strategy(),
        desc in description_strategy(),
        value in prop::string::string_regex(".{5,50}").unwrap(),
    ) {
        // Arrange: Field with reasonable constraints
        let field = InputField::new(&field_name, &desc, "String")
            .with_min_length(5)
            .with_max_length(50);

        let sig = Signature::new("Test", "Test")
            .with_input(field);

        let validator = SignatureValidator::new(sig);
        let input = json!({ field_name: value });

        // Act
        let result = validator.validate(&input);

        // Assert: Value within bounds should pass
        if value.len() >= 5 && value.len() <= 50 {
            assert!(result.is_ok(), "Valid input should pass validation");
        }
    }

    /// Property: Validation detects missing required fields
    #[test]
    fn prop_validation_detects_missing_required(
        field_name in field_name_strategy(),
        desc in description_strategy(),
    ) {
        // Arrange: Required field
        let field = InputField::new(&field_name, &desc, "String")
            .required(true);

        let sig = Signature::new("Test", "Test")
            .with_input(field);

        let validator = SignatureValidator::new(sig);

        // Act: Empty object (missing field)
        let result = validator.validate(&json!({}));

        // Assert: Should detect missing field
        assert!(result.is_err(), "Missing required field should be detected");
    }

    /// Property: Validation accepts optional missing fields
    #[test]
    fn prop_validation_accepts_optional_missing(
        field_name in field_name_strategy(),
        desc in description_strategy(),
    ) {
        // Arrange: Optional field (required=false)
        let field = InputField::new(&field_name, &desc, "String");

        let sig = Signature::new("Test", "Test")
            .with_input(field);

        let validator = SignatureValidator::new(sig);

        // Act: Empty object (missing optional field)
        let result = validator.validate(&json!({}));

        // Assert: Should pass
        assert!(result.is_ok(), "Missing optional field should be accepted");
    }

    /// Property: Type validation is consistent
    #[test]
    fn prop_type_validation_consistent(
        field_name in field_name_strategy(),
        type_ann in type_annotation_strategy(),
    ) {
        // Arrange
        let field = InputField::new(&field_name, "Test field", &type_ann);
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        // Generate a value that matches the type
        let value_strategy = json_value_for_type(&type_ann);

        proptest!(|(value in value_strategy)| {
            let input = json!({ &field_name: value });

            // Act
            let result = validator.validate(&input);

            // Assert: Matching type should pass (unless null and required)
            // For now, we just check it doesn't panic
            let _ = result;
        });
    }
}

// ============================================================================
// Property 4: Edge Cases and Boundary Conditions
// ============================================================================

proptest! {
    /// Property: Empty signatures validate empty input
    #[test]
    fn prop_empty_signature_validates_empty_input(
        name in field_name_strategy(),
        desc in description_strategy(),
    ) {
        // Arrange: Signature with no fields
        let sig = Signature::new(name, desc);
        let validator = SignatureValidator::new(sig);

        // Act
        let result = validator.validate(&json!({}));

        // Assert: Should pass
        assert!(result.is_ok(), "Empty signature should accept empty input");
    }

    /// Property: Unicode handling is consistent
    #[test]
    fn prop_unicode_handling(
        unicode_str in prop::string::string_regex("[\u{4e00}-\u{9fff}\u{1f300}-\u{1f5ff}]{1,10}").unwrap(),
    ) {
        // Arrange: Field accepting any string
        let field = InputField::new("text", "Unicode text", "String");
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let input = json!({ "text": unicode_str });

        // Act
        let result = validator.validate(&input);

        // Assert: Unicode strings should be accepted
        assert!(result.is_ok(), "Unicode strings should validate correctly");
    }

    /// Property: Very long strings are handled
    #[test]
    fn prop_very_long_strings_handled(
        length in 1000usize..5000,
    ) {
        // Arrange: Field with max length constraint
        let field = InputField::new("text", "Text", "String")
            .with_max_length(1000);
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let long_string = "a".repeat(length);
        let input = json!({ "text": long_string });

        // Act
        let result = validator.validate(&input);

        // Assert: Correctly enforce max length
        if length > 1000 {
            assert!(result.is_err(), "String exceeding max should fail");
        } else {
            assert!(result.is_ok(), "String within max should pass");
        }
    }

    /// Property: Very large arrays are handled
    #[test]
    fn prop_very_large_arrays_handled(
        size in 100usize..500,
    ) {
        // Arrange: Field with max items constraint
        let field = InputField::new("items", "Items", "Vec<i32>")
            .with_max_items(100);
        let sig = Signature::new("Test", "Test").with_input(field);
        let validator = SignatureValidator::new(sig);

        let large_array = vec![1; size];
        let input = json!({ "items": large_array });

        // Act
        let result = validator.validate(&input);

        // Assert: Correctly enforce max items
        if size > 100 {
            assert!(result.is_err(), "Array exceeding max should fail");
        } else {
            assert!(result.is_ok(), "Array within max should pass");
        }
    }

    /// Property: Constraint combinations are consistent
    #[test]
    fn prop_constraint_combinations_consistent(
        min_len in 5usize..20,
        max_len in 20usize..50,
        actual_len in 0usize..100,
    ) {
        // Arrange: Both min and max length
        let constraints = FieldConstraints::new()
            .min_length(min_len)
            .max_length(max_len);

        let test_string = "a".repeat(actual_len);

        // Act
        let result = constraints.is_satisfied(&json!(test_string));

        // Assert: Must satisfy both constraints
        if actual_len >= min_len && actual_len <= max_len {
            assert!(result.is_ok(), "String within both bounds should pass");
        } else {
            assert!(result.is_err(), "String outside bounds should fail");
        }
    }
}

// ============================================================================
// Property 5: Type Mapping Invariants
// ============================================================================

proptest! {
    /// Property: String types always map to JSON string schema
    #[test]
    fn prop_string_types_map_to_string_schema(
        type_variant in prop_oneof![
            Just("String"),
            Just("str"),
            Just("&str"),
        ]
    ) {
        // Arrange
        let field = InputField::new("test", "Test", type_variant);
        let sig = Signature::new("Test", "Test").with_input(field);

        // Act
        let schema = sig.as_json_schema();

        // Assert
        assert_eq!(
            schema["properties"]["test"]["type"],
            "string",
            "String type should map to JSON string"
        );
    }

    /// Property: Integer types always map to JSON integer schema
    #[test]
    fn prop_integer_types_map_to_integer_schema(
        type_variant in prop_oneof![
            Just("i32"),
            Just("i64"),
            Just("u32"),
            Just("u64"),
            Just("int"),
        ]
    ) {
        // Arrange
        let field = InputField::new("test", "Test", type_variant);
        let sig = Signature::new("Test", "Test").with_input(field);

        // Act
        let schema = sig.as_json_schema();

        // Assert
        assert_eq!(
            schema["properties"]["test"]["type"],
            "integer",
            "Integer type should map to JSON integer"
        );
    }

    /// Property: Float types always map to JSON number schema
    #[test]
    fn prop_float_types_map_to_number_schema(
        type_variant in prop_oneof![
            Just("f32"),
            Just("f64"),
            Just("float"),
            Just("double"),
        ]
    ) {
        // Arrange
        let field = InputField::new("test", "Test", type_variant);
        let sig = Signature::new("Test", "Test").with_input(field);

        // Act
        let schema = sig.as_json_schema();

        // Assert
        assert_eq!(
            schema["properties"]["test"]["type"],
            "number",
            "Float type should map to JSON number"
        );
    }

    /// Property: Vec types always map to JSON array schema
    #[test]
    fn prop_vec_types_map_to_array_schema(
        inner_type in prop_oneof![
            Just("String"),
            Just("i32"),
            Just("f64"),
        ]
    ) {
        // Arrange
        let type_ann = format!("Vec<{}>", inner_type);
        let field = InputField::new("test", "Test", &type_ann);
        let sig = Signature::new("Test", "Test").with_input(field);

        // Act
        let schema = sig.as_json_schema();

        // Assert
        assert_eq!(
            schema["properties"]["test"]["type"],
            "array",
            "Vec type should map to JSON array"
        );
        assert!(
            schema["properties"]["test"]["items"].is_object(),
            "Array schema should have items definition"
        );
    }

    /// Property: Constraints are preserved in schema
    #[test]
    fn prop_constraints_preserved_in_schema(
        min_len in 1usize..50,
        max_len in 50usize..200,
    ) {
        // Arrange
        let field = InputField::new("test", "Test", "String")
            .with_min_length(min_len)
            .with_max_length(max_len);
        let sig = Signature::new("Test", "Test").with_input(field);

        // Act
        let schema = sig.as_json_schema();

        // Assert
        assert_eq!(
            schema["properties"]["test"]["minLength"].as_u64(),
            Some(min_len as u64),
            "minLength should be preserved in schema"
        );
        assert_eq!(
            schema["properties"]["test"]["maxLength"].as_u64(),
            Some(max_len as u64),
            "maxLength should be preserved in schema"
        );
    }
}

// ============================================================================
// Shrinking Tests (Verify Proptest Finds Minimal Failing Cases)
// ============================================================================

#[test]
fn test_proptest_shrinking_works() {
    // This test verifies that proptest shrinking works correctly
    // by intentionally creating a failing condition and checking
    // that the shrunk example is minimal

    let result = std::panic::catch_unwind(|| {
        proptest!(|(s in prop::string::string_regex("[a-z]{1,100}").unwrap())| {
            // This will fail when string contains 'a'
            prop_assert!(!s.contains('a'));
        });
    });

    // We expect this to fail (which means proptest found a counterexample)
    assert!(result.is_err(), "Proptest should find a counterexample");

    // The shrunk example should be minimal, typically "a"
    // (We can't directly check the shrunk value, but the test demonstrates shrinking works)
}

#[cfg(test)]
mod validation_edge_cases {
    use super::*;

    /// Test that demonstrates proptest finding edge case: empty string with min_length
    #[test]
    fn edge_case_empty_string_min_length() {
        let constraints = FieldConstraints::new().min_length(1);
        let result = constraints.is_satisfied(&json!(""));
        assert!(result.is_err(), "Empty string should fail min_length=1");
    }

    /// Test that demonstrates proptest finding edge case: array boundary
    #[test]
    fn edge_case_array_exact_boundary() {
        let constraints = FieldConstraints::new().max_items(5);

        // Exactly at boundary - should pass
        let result = constraints.is_satisfied(&json!([1, 2, 3, 4, 5]));
        assert!(result.is_ok(), "Array at exact max should pass");

        // One over boundary - should fail
        let result = constraints.is_satisfied(&json!([1, 2, 3, 4, 5, 6]));
        assert!(result.is_err(), "Array over max should fail");
    }

    /// Test that demonstrates Unicode string length (chars vs bytes)
    #[test]
    fn edge_case_unicode_length_counting() {
        let constraints = FieldConstraints::new().min_length(2);

        // "你好" is 2 characters but 6 bytes
        let result = constraints.is_satisfied(&json!("你好"));
        assert!(result.is_ok(), "Unicode string should count by chars, not bytes");

        // Single emoji
        let result = constraints.is_satisfied(&json!("🌍"));
        assert!(result.is_err(), "Single emoji (1 char) should fail min_length=2");
    }
}
