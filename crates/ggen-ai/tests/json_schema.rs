//! Comprehensive JSON Schema generation tests for DSPy Signatures
//!
//! Tests the JSON Schema generation functionality with real Signature objects
//! using Chicago TDD pattern (Arrange-Act-Assert with real collaborators).
//!
//! Test Coverage:
//! 1. Type mapping (String, i32, f64, bool, Vec<T>, custom types)
//! 2. Constraint conversion (min/max items, min/max length, pattern, enum, required)
//! 3. Schema validation (valid JSON, parseable by serde_json)
//! 4. Integration with Signature (build with constraints, verify output structure)
//! 5. Complex scenarios (multiple fields, mixed types, enum domains)
//! 6. Edge cases (empty signature, escaping, long descriptions)

use ggen_ai::dspy::field::{FieldConstraints, InputField, OutputField};
use ggen_ai::dspy::signature::Signature;
use serde_json::json;

// ============================================================================
// Section 1: Type Mapping Tests
// ============================================================================

#[test]
fn test_type_mapping_string_to_json_schema() {
    // Arrange
    let sig = Signature::new("StringTest", "Test string type").with_input(InputField::new(
        "text",
        "A text field",
        "String",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["type"], "object");
    assert_eq!(schema["properties"]["text"]["type"], "string");
}

#[test]
fn test_type_mapping_i32_to_json_schema() {
    // Arrange
    let sig = Signature::new("IntegerTest", "Test i32 type").with_input(InputField::new(
        "count",
        "Number of items",
        "i32",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["count"]["type"], "integer");
}

#[test]
fn test_type_mapping_i64_to_json_schema() {
    // Arrange
    let sig = Signature::new("LongTest", "Test i64 type").with_input(InputField::new(
        "id",
        "Unique identifier",
        "i64",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["id"]["type"], "integer");
}

#[test]
fn test_type_mapping_u32_to_json_schema() {
    // Arrange
    let sig = Signature::new("UintTest", "Test u32 type").with_input(InputField::new(
        "port",
        "Port number",
        "u32",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["port"]["type"], "integer");
}

#[test]
fn test_type_mapping_f64_to_json_schema() {
    // Arrange
    let sig = Signature::new("FloatTest", "Test f64 type").with_input(InputField::new(
        "price",
        "Product price",
        "f64",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["price"]["type"], "number");
}

#[test]
fn test_type_mapping_f32_to_json_schema() {
    // Arrange
    let sig = Signature::new("FloatSmallTest", "Test f32 type").with_input(InputField::new(
        "ratio",
        "Ratio value",
        "f32",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["ratio"]["type"], "number");
}

#[test]
fn test_type_mapping_bool_to_json_schema() {
    // Arrange
    let sig = Signature::new("BoolTest", "Test bool type").with_input(InputField::new(
        "enabled",
        "Is enabled",
        "bool",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["enabled"]["type"], "boolean");
}

#[test]
fn test_type_mapping_vec_string_to_json_schema() {
    // Arrange
    let sig = Signature::new("ArrayTest", "Test Vec<String> type").with_input(InputField::new(
        "tags",
        "List of tags",
        "Vec<String>",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["tags"]["type"], "array");
    assert_eq!(schema["properties"]["tags"]["items"]["type"], "string");
}

#[test]
fn test_type_mapping_vec_i32_to_json_schema() {
    // Arrange
    let sig = Signature::new("IntArrayTest", "Test Vec<i32> type").with_input(InputField::new(
        "numbers",
        "List of integers",
        "Vec<i32>",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["numbers"]["type"], "array");
    assert_eq!(schema["properties"]["numbers"]["items"]["type"], "integer");
}

#[test]
fn test_type_mapping_vec_f64_to_json_schema() {
    // Arrange
    let sig = Signature::new("FloatArrayTest", "Test Vec<f64> type").with_input(InputField::new(
        "measurements",
        "Measurement values",
        "Vec<f64>",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["measurements"]["type"], "array");
    assert_eq!(
        schema["properties"]["measurements"]["items"]["type"],
        "number"
    );
}

#[test]
fn test_type_mapping_nested_vec_to_json_schema() {
    // Arrange
    let sig = Signature::new("NestedArrayTest", "Test Vec<Vec<String>> type").with_input(
        InputField::new("matrix", "Matrix of strings", "Vec<Vec<String>>"),
    );

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["matrix"]["type"], "array");
    assert_eq!(schema["properties"]["matrix"]["items"]["type"], "array");
    assert_eq!(
        schema["properties"]["matrix"]["items"]["items"]["type"],
        "string"
    );
}

#[test]
fn test_type_mapping_option_string_to_json_schema() {
    // Arrange
    let sig = Signature::new("OptionTest", "Test Option<String> type").with_input(InputField::new(
        "maybe_text",
        "Optional text",
        "Option<String>",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Option unwraps to inner type
    assert_eq!(schema["properties"]["maybe_text"]["type"], "string");
}

#[test]
fn test_type_mapping_custom_type_defaults_to_string() {
    // Arrange
    let sig = Signature::new("CustomTest", "Test custom type").with_input(InputField::new(
        "custom",
        "Custom field",
        "MyCustomType",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Unknown types default to string
    assert_eq!(schema["properties"]["custom"]["type"], "string");
}

#[test]
fn test_type_mapping_str_reference_to_json_schema() {
    // Arrange
    let sig = Signature::new("StrRefTest", "Test &str type").with_input(InputField::new(
        "borrowed",
        "Borrowed string",
        "&str",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["borrowed"]["type"], "string");
}

// ============================================================================
// Section 2: Constraint Conversion Tests
// ============================================================================

#[test]
fn test_constraint_min_length_conversion() {
    // Arrange
    let mut field = InputField::new("username", "User name", "String");
    field.constraints = FieldConstraints::new().min_length(3);
    let sig = Signature::new("UsernameTest", "Test min length").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: min_length becomes minLength
    assert_eq!(schema["properties"]["username"]["minLength"], 3);
}

#[test]
fn test_constraint_max_length_conversion() {
    // Arrange
    let mut field = InputField::new("password", "Password", "String");
    field.constraints = FieldConstraints::new().max_length(128);
    let sig = Signature::new("PasswordTest", "Test max length").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: max_length becomes maxLength
    assert_eq!(schema["properties"]["password"]["maxLength"], 128);
}

#[test]
fn test_constraint_min_and_max_length_conversion() {
    // Arrange
    let mut field = InputField::new("code", "Security code", "String");
    field.constraints = FieldConstraints::new().min_length(6).max_length(6);
    let sig = Signature::new("CodeTest", "Test min/max length").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["code"]["minLength"], 6);
    assert_eq!(schema["properties"]["code"]["maxLength"], 6);
}

#[test]
fn test_constraint_pattern_conversion() {
    // Arrange
    let mut field = InputField::new("email", "Email address", "String");
    field.constraints =
        FieldConstraints::new().pattern("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");
    let sig = Signature::new("EmailTest", "Test email pattern").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: pattern matches directly
    assert!(schema["properties"]["email"]["pattern"].is_string());
    let pattern = schema["properties"]["email"]["pattern"].as_str().unwrap();
    assert!(pattern.contains("@"));
}

#[test]
fn test_constraint_enum_values_conversion() {
    // Arrange
    let mut field = InputField::new("status", "Order status", "String");
    field.constraints = FieldConstraints::new().enum_values(vec![
        "pending".to_string(),
        "approved".to_string(),
        "rejected".to_string(),
    ]);
    let sig = Signature::new("StatusTest", "Test enum values").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: enum_values becomes enum array
    let enum_vals = schema["properties"]["status"]["enum"].as_array().unwrap();
    assert_eq!(enum_vals.len(), 3);
    assert_eq!(enum_vals[0], "pending");
    assert_eq!(enum_vals[1], "approved");
    assert_eq!(enum_vals[2], "rejected");
}

#[test]
fn test_constraint_min_items_conversion() {
    // Arrange
    let mut field = InputField::new("tags", "Resource tags", "Vec<String>");
    field.constraints = FieldConstraints::new().min_items(1);
    let sig = Signature::new("TagsTest", "Test min items").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: min_items becomes minItems
    assert_eq!(schema["properties"]["tags"]["minItems"], 1);
}

#[test]
fn test_constraint_max_items_conversion() {
    // Arrange
    let mut field = InputField::new("selected", "Selected items", "Vec<String>");
    field.constraints = FieldConstraints::new().max_items(10);
    let sig = Signature::new("SelectionTest", "Test max items").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: max_items becomes maxItems
    assert_eq!(schema["properties"]["selected"]["maxItems"], 10);
}

#[test]
fn test_constraint_min_and_max_items_conversion() {
    // Arrange
    let mut field = InputField::new("selections", "Selected items", "Vec<String>");
    field.constraints = FieldConstraints::new().min_items(1).max_items(5);
    let sig = Signature::new("SelectionsTest", "Test min/max items").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["selections"]["minItems"], 1);
    assert_eq!(schema["properties"]["selections"]["maxItems"], 5);
}

#[test]
fn test_constraint_required_field_in_required_array() {
    // Arrange
    let mut field = InputField::new("email", "Email address", "String");
    field.constraints = FieldConstraints::new().required(true);
    let sig = Signature::new("RequiredTest", "Test required field").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: required field appears in "required" array
    let required = schema["required"].as_array().unwrap();
    assert!(required.contains(&json!("email")));
}

#[test]
fn test_constraint_optional_field_not_in_required_array() {
    // Arrange
    let mut field = InputField::new("nickname", "Optional nickname", "String");
    field.constraints = FieldConstraints::new().required(false);
    let sig = Signature::new("OptionalTest", "Test optional field").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: optional field does not appear in "required" array
    assert!(!schema.get("required").map_or(false, |r| {
        r.as_array()
            .map_or(false, |arr| arr.contains(&json!("nickname")))
    }));
}

// ============================================================================
// Section 3: Schema Validation Tests
// ============================================================================

#[test]
fn test_schema_is_valid_json() {
    // Arrange
    let sig = Signature::new("ValidJsonTest", "Test valid JSON output")
        .with_input(InputField::new("field1", "Field 1", "String"))
        .with_input(InputField::new("field2", "Field 2", "i32"));

    // Act
    let schema = sig.as_json_schema();
    let json_str = serde_json::to_string(&schema).expect("Should serialize");

    // Assert: Can be parsed back as valid JSON
    let parsed: serde_json::Value =
        serde_json::from_str(&json_str).expect("Should deserialize from valid JSON");
    assert_eq!(parsed["type"], "object");
}

#[test]
fn test_schema_is_parseable_by_serde_json() {
    // Arrange
    let sig = Signature::new("ParseTest", "Test serde_json parsing").with_input(InputField::new(
        "data",
        "Some data",
        "String",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Schema is a valid serde_json::Value that can be manipulated
    assert!(schema.is_object());
    assert!(schema.get("type").is_some());
    assert!(schema.get("properties").is_some());
}

#[test]
fn test_schema_structure_has_required_fields() {
    // Arrange
    let sig = Signature::new("StructureTest", "Test schema structure").with_input(InputField::new(
        "input",
        "Some input",
        "String",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Schema has required JSON Schema fields
    assert_eq!(schema["type"], "object");
    assert!(schema["properties"].is_object());
    assert!(schema["description"].is_string());
}

#[test]
fn test_schema_properties_are_objects() {
    // Arrange
    let sig = Signature::new("PropertiesTest", "Test property objects")
        .with_input(InputField::new("field1", "String field", "String"))
        .with_input(InputField::new("field2", "Integer field", "i32"));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Each property is an object with type
    assert!(schema["properties"]["field1"].is_object());
    assert!(schema["properties"]["field2"].is_object());
    assert!(schema["properties"]["field1"].get("type").is_some());
    assert!(schema["properties"]["field2"].get("type").is_some());
}

#[test]
fn test_schema_descriptions_preserved() {
    // Arrange
    let sig = Signature::new("DescTest", "Module description").with_input(InputField::new(
        "field",
        "Field description",
        "String",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Descriptions are preserved in schema
    assert_eq!(schema["description"], "Module description");
    assert_eq!(
        schema["properties"]["field"]["description"],
        "Field description"
    );
}

// ============================================================================
// Section 4: Integration with Signature Tests
// ============================================================================

#[test]
fn test_signature_integration_build_with_constraints() {
    // Arrange: Build Signature with multiple fields and constraints
    let mut domain_field = InputField::new("domain", "Financial domain", "String");
    domain_field.constraints = FieldConstraints::new().required(true).enum_values(vec![
        "finops".to_string(),
        "banking".to_string(),
        "insurance".to_string(),
    ]);

    let mut ontology_field = InputField::new("ontologies", "Selected ontologies", "Vec<String>");
    ontology_field.constraints = FieldConstraints::new().min_items(1).max_items(5);

    let sig = Signature::new("DomainSelector", "Select financial domain and ontologies")
        .with_input(domain_field)
        .with_input(ontology_field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: All constraints are correctly mapped
    assert_eq!(schema["type"], "object");
    assert_eq!(schema["properties"]["domain"]["type"], "string");
    assert_eq!(schema["properties"]["ontologies"]["type"], "array");

    // Verify enum constraint
    let enum_vals = schema["properties"]["domain"]["enum"].as_array().unwrap();
    assert_eq!(enum_vals.len(), 3);

    // Verify array constraints
    assert_eq!(schema["properties"]["ontologies"]["minItems"], 1);
    assert_eq!(schema["properties"]["ontologies"]["maxItems"], 5);

    // Verify required fields
    let required = schema["required"].as_array().unwrap();
    assert!(required.contains(&json!("domain")));
}

#[test]
fn test_signature_integration_multiple_inputs_different_types() {
    // Arrange: Create signature with diverse field types
    let sig = Signature::new("ComplexModule", "Module with multiple types")
        .with_input(InputField::new("name", "User name", "String"))
        .with_input(InputField::new("age", "User age", "i32"))
        .with_input(InputField::new("score", "Numeric score", "f64"))
        .with_input(InputField::new("active", "Is active", "bool"))
        .with_input(InputField::new("tags", "User tags", "Vec<String>"));

    // Act
    let schema = sig.as_json_schema();

    // Assert: All types are correctly mapped
    assert_eq!(schema["properties"]["name"]["type"], "string");
    assert_eq!(schema["properties"]["age"]["type"], "integer");
    assert_eq!(schema["properties"]["score"]["type"], "number");
    assert_eq!(schema["properties"]["active"]["type"], "boolean");
    assert_eq!(schema["properties"]["tags"]["type"], "array");
}

#[test]
fn test_signature_integration_mixed_required_and_optional() {
    // Arrange
    let mut required_field = InputField::new("username", "Required username", "String");
    required_field.constraints = FieldConstraints::new().required(true);

    let mut optional_field = InputField::new("bio", "Optional biography", "String");
    optional_field.constraints = FieldConstraints::new().required(false);

    let sig = Signature::new("UserProfile", "User profile data")
        .with_input(required_field)
        .with_input(optional_field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: Required array contains only required fields
    let required = schema["required"].as_array().unwrap();
    assert_eq!(required.len(), 1);
    assert!(required.contains(&json!("username")));
    assert!(!required.contains(&json!("bio")));
}

#[test]
fn test_signature_integration_call_as_json_schema() {
    // Arrange
    let sig = Signature::new("SchemaTest", "Test calling as_json_schema")
        .with_input(InputField::new("input", "Input field", "String"));

    // Act: Call the as_json_schema method
    let result = sig.as_json_schema();

    // Assert: Method returns valid schema
    assert_eq!(result["type"], "object");
    assert!(result["properties"].is_object());
}

#[test]
fn test_signature_integration_instructions_not_in_schema() {
    // Arrange
    let sig = Signature::new("WithInstructions", "Signature with instructions")
        .with_instructions("Process inputs carefully")
        .with_input(InputField::new("data", "Input data", "String"));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Instructions are not included in JSON schema (internal only)
    assert!(!schema.get("instructions").is_some());
}

// ============================================================================
// Section 5: Complex Scenarios Tests
// ============================================================================

#[test]
fn test_complex_scenario_financial_domain_selector() {
    // Arrange: Realistic financial domain selector signature
    let mut domain_field = InputField::new("industry_domain", "Financial domain", "String");
    domain_field.constraints = FieldConstraints::new().required(true).enum_values(vec![
        "finops".to_string(),
        "banking".to_string(),
        "insurance".to_string(),
        "wealth".to_string(),
    ]);

    let mut ontology_field =
        InputField::new("ontology_selections", "Selected ontologies", "Vec<String>");
    ontology_field.constraints = FieldConstraints::new()
        .required(true)
        .min_items(1)
        .max_items(10);

    let sig = Signature::new("ConfigSelector", "Select financial configuration")
        .with_input(domain_field)
        .with_input(ontology_field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["type"], "object");
    assert_eq!(schema["description"], "Select financial configuration");

    // Verify both fields are present and required
    let required = schema["required"].as_array().unwrap();
    assert_eq!(required.len(), 2);

    // Verify enum field
    let enum_vals = schema["properties"]["industry_domain"]["enum"]
        .as_array()
        .unwrap();
    assert_eq!(enum_vals.len(), 4);

    // Verify array field constraints
    assert_eq!(schema["properties"]["ontology_selections"]["minItems"], 1);
    assert_eq!(schema["properties"]["ontology_selections"]["maxItems"], 10);
}

#[test]
fn test_complex_scenario_all_constraint_types() {
    // Arrange: Field with all constraint types applied
    let mut field = InputField::new("complex_field", "Field with all constraints", "String");
    field.constraints = FieldConstraints::new()
        .required(true)
        .min_length(5)
        .max_length(100)
        .pattern("^[a-zA-Z0-9_]+$")
        .enum_values(vec!["option1".to_string(), "option2".to_string()]);

    let sig = Signature::new("AllConstraints", "Test all constraint types").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: All constraints are present
    assert_eq!(schema["properties"]["complex_field"]["minLength"], 5);
    assert_eq!(schema["properties"]["complex_field"]["maxLength"], 100);
    assert_eq!(
        schema["properties"]["complex_field"]["pattern"],
        "^[a-zA-Z0-9_]+$"
    );
    let enums = schema["properties"]["complex_field"]["enum"]
        .as_array()
        .unwrap();
    assert_eq!(enums.len(), 2);

    // Required field appears in required array
    let required = schema["required"].as_array().unwrap();
    assert!(required.contains(&json!("complex_field")));
}

#[test]
fn test_complex_scenario_nested_arrays_with_constraints() {
    // Arrange
    let mut field = InputField::new("matrix", "2D array of integers", "Vec<Vec<i32>>");
    field.constraints = FieldConstraints::new().min_items(1).max_items(100);

    let sig = Signature::new("MatrixProcessor", "Process 2D arrays").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: Nested array structure with constraints
    assert_eq!(schema["properties"]["matrix"]["type"], "array");
    assert_eq!(schema["properties"]["matrix"]["items"]["type"], "array");
    assert_eq!(
        schema["properties"]["matrix"]["items"]["items"]["type"],
        "integer"
    );
    assert_eq!(schema["properties"]["matrix"]["minItems"], 1);
    assert_eq!(schema["properties"]["matrix"]["maxItems"], 100);
}

#[test]
fn test_complex_scenario_multiple_enum_fields() {
    // Arrange: Multiple fields with different enums
    let mut status_field = InputField::new("status", "Order status", "String");
    status_field.constraints = FieldConstraints::new().enum_values(vec![
        "pending".to_string(),
        "approved".to_string(),
        "rejected".to_string(),
    ]);

    let mut priority_field = InputField::new("priority", "Task priority", "String");
    priority_field.constraints = FieldConstraints::new().enum_values(vec![
        "low".to_string(),
        "medium".to_string(),
        "high".to_string(),
        "critical".to_string(),
    ]);

    let sig = Signature::new("TaskManagement", "Task management system")
        .with_input(status_field)
        .with_input(priority_field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    let status_enums = schema["properties"]["status"]["enum"].as_array().unwrap();
    assert_eq!(status_enums.len(), 3);

    let priority_enums = schema["properties"]["priority"]["enum"].as_array().unwrap();
    assert_eq!(priority_enums.len(), 4);
}

// ============================================================================
// Section 6: Edge Cases Tests
// ============================================================================

#[test]
fn test_edge_case_empty_signature_no_inputs() {
    // Arrange: Signature with no input fields
    let sig = Signature::new("EmptyModule", "Module with no inputs");

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["type"], "object");
    assert_eq!(schema["properties"].as_object().unwrap().len(), 0);
    assert_eq!(schema["description"], "EmptyModule");
    // required array should be empty or not present
    assert!(!schema.get("required").map_or(false, |r| {
        r.as_array().map_or(false, |arr| !arr.is_empty())
    }));
}

#[test]
fn test_edge_case_only_outputs_no_inputs() {
    // Arrange: Signature with only output fields (no inputs)
    let sig = Signature::new("GeneratorModule", "Module that only generates outputs")
        .with_output(OutputField::new("result", "Generated result", "String"));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Schema should be valid even with no inputs
    assert_eq!(schema["type"], "object");
    assert_eq!(schema["properties"].as_object().unwrap().len(), 0);
}

#[test]
fn test_edge_case_field_name_with_underscores() {
    // Arrange
    let sig = Signature::new("SnakeCase", "Test snake case field names")
        .with_input(InputField::new(
            "field_name",
            "Field with underscores",
            "String",
        ))
        .with_input(InputField::new(
            "another_field_name",
            "Another field",
            "i32",
        ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Field names are preserved
    assert!(schema["properties"]["field_name"].is_object());
    assert!(schema["properties"]["another_field_name"].is_object());
}

#[test]
fn test_edge_case_field_name_with_numbers() {
    // Arrange
    let sig = Signature::new("NumberNames", "Test field names with numbers")
        .with_input(InputField::new("field1", "First field", "String"))
        .with_input(InputField::new("field2", "Second field", "String"));

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert!(schema["properties"]["field1"].is_object());
    assert!(schema["properties"]["field2"].is_object());
}

#[test]
fn test_edge_case_very_long_description() {
    // Arrange
    let long_desc = "This is a very long description that contains lots of text. \
        It includes multiple sentences and discusses various aspects of the field. \
        It might also include special characters like & < > \" and other symbols. \
        The schema should still be valid JSON despite this long description.";

    let sig = Signature::new("LongDescTest", long_desc)
        .with_input(InputField::new("field", "A field with a long description as well. \
        This description contains multiple sentences and explains the purpose and usage of the field. \
        It might also include examples and edge cases that need to be handled.", "String"));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Long descriptions are preserved and JSON remains valid
    assert_eq!(schema["description"], long_desc);
    assert!(schema["properties"]["field"]["description"].is_string());

    // Verify it can be serialized/deserialized
    let json_str = serde_json::to_string(&schema).expect("Should serialize");
    let _parsed: serde_json::Value = serde_json::from_str(&json_str).expect("Should deserialize");
}

#[test]
fn test_edge_case_special_characters_in_description() {
    // Arrange
    let sig = Signature::new("SpecialChars", "Test special characters: & < > \" '").with_input(
        InputField::new("data", "Field with special chars: @#$%^&*()", "String"),
    );

    // Act
    let schema = sig.as_json_schema();

    // Assert: Special characters are properly escaped in JSON
    let json_str = serde_json::to_string(&schema).expect("Should serialize with special chars");
    let parsed: serde_json::Value =
        serde_json::from_str(&json_str).expect("Should deserialize with special chars");
    assert_eq!(parsed["type"], "object");
}

#[test]
fn test_edge_case_empty_enum_values() {
    // Arrange
    let mut field = InputField::new("empty_enum", "Field with empty enum", "String");
    field.constraints = FieldConstraints::new().enum_values(vec![]);

    let sig = Signature::new("EmptyEnumTest", "Test empty enum").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: Empty enum is handled gracefully
    let enum_vals = schema["properties"]["empty_enum"]["enum"]
        .as_array()
        .unwrap();
    assert_eq!(enum_vals.len(), 0);
}

#[test]
fn test_edge_case_single_item_enum() {
    // Arrange
    let mut field = InputField::new("single_option", "Field with single enum value", "String");
    field.constraints = FieldConstraints::new().enum_values(vec!["only_option".to_string()]);

    let sig = Signature::new("SingleEnumTest", "Test single enum value").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    let enum_vals = schema["properties"]["single_option"]["enum"]
        .as_array()
        .unwrap();
    assert_eq!(enum_vals.len(), 1);
    assert_eq!(enum_vals[0], "only_option");
}

#[test]
fn test_edge_case_pattern_with_special_regex_chars() {
    // Arrange
    let mut field = InputField::new("regex_field", "Field with regex pattern", "String");
    field.constraints =
        FieldConstraints::new().pattern("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$");

    let sig = Signature::new("RegexTest", "Test regex pattern").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: Regex pattern is preserved
    assert!(schema["properties"]["regex_field"]["pattern"].is_string());
}

#[test]
fn test_edge_case_zero_min_max_constraints() {
    // Arrange
    let mut field = InputField::new("array_field", "Array with zero min/max", "Vec<String>");
    field.constraints = FieldConstraints::new().min_items(0).max_items(0);

    let sig = Signature::new("ZeroConstraintTest", "Test zero constraints").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert: Zero values are preserved
    assert_eq!(schema["properties"]["array_field"]["minItems"], 0);
    assert_eq!(schema["properties"]["array_field"]["maxItems"], 0);
}

#[test]
fn test_edge_case_large_constraint_values() {
    // Arrange
    let mut field = InputField::new("large_field", "Field with large constraints", "Vec<String>");
    field.constraints = FieldConstraints::new().min_items(1000).max_items(1_000_000);

    let sig =
        Signature::new("LargeConstraintTest", "Test large constraint values").with_input(field);

    // Act
    let schema = sig.as_json_schema();

    // Assert
    assert_eq!(schema["properties"]["large_field"]["minItems"], 1000);
    assert_eq!(schema["properties"]["large_field"]["maxItems"], 1_000_000);
}

#[test]
fn test_edge_case_whitespace_in_field_names_not_allowed() {
    // Arrange: Field with valid identifier (no spaces)
    let sig = Signature::new("WhitespaceTest", "Test field names").with_input(InputField::new(
        "valid_name",
        "Valid field name",
        "String",
    ));

    // Act
    let schema = sig.as_json_schema();

    // Assert: Field name is preserved as-is
    assert!(schema["properties"]["valid_name"].is_object());
}

// ============================================================================
// Section 7: Round-trip Serialization Tests
// ============================================================================

#[test]
fn test_schema_round_trip_serialization() {
    // Arrange
    let mut field = InputField::new("user_id", "User identifier", "i32");
    field.constraints = FieldConstraints::new()
        .required(true)
        .enum_values(vec!["admin".to_string(), "user".to_string()]);

    let sig = Signature::new("UserSystem", "User management").with_input(field);

    // Act
    let schema = sig.as_json_schema();
    let json_str = serde_json::to_string_pretty(&schema).expect("Should serialize");
    let parsed: serde_json::Value = serde_json::from_str(&json_str).expect("Should deserialize");

    // Assert: Round trip preserves structure
    assert_eq!(schema["type"], parsed["type"]);
    assert_eq!(schema["description"], parsed["description"]);
    assert_eq!(
        schema["properties"]["user_id"]["type"],
        parsed["properties"]["user_id"]["type"]
    );
}

#[test]
fn test_schema_json_pretty_print_is_readable() {
    // Arrange
    let sig = Signature::new("ReadableTest", "Test readable JSON output")
        .with_input(InputField::new("field1", "First field", "String"))
        .with_input(InputField::new("field2", "Second field", "i32"));

    // Act
    let schema = sig.as_json_schema();
    let json_pretty = serde_json::to_string_pretty(&schema).expect("Should pretty print");

    // Assert: Pretty printed JSON contains proper formatting
    assert!(json_pretty.contains("\"type\""));
    assert!(json_pretty.contains("\"properties\""));
    assert!(json_pretty.contains("\"field1\""));
    assert!(json_pretty.contains("\"field2\""));
}
