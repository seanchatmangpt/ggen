//! Assertion Helpers for DSPy Testing
//!
//! Provides custom assertion functions for DSPy-specific testing patterns.

use crate::dspy::optimizer::{Demonstration, Example, MetricFn};
use crate::dspy::signature::Signature;
use serde_json::Value;
use std::collections::HashMap;

/// Assert that an example is valid (has required fields)
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::{assert_example_valid, qa_example};
///
/// let example = qa_example("Q", "A");
/// assert_example_valid(&example, &["question"], &["answer"]);
/// ```
pub fn assert_example_valid(
    example: &Example, required_inputs: &[&str], required_outputs: &[&str],
) {
    for input_key in required_inputs {
        assert!(
            example.inputs.contains_key(*input_key),
            "Example missing required input: {}",
            input_key
        );
    }

    for output_key in required_outputs {
        assert!(
            example.outputs.contains_key(*output_key),
            "Example missing required output: {}",
            output_key
        );
    }
}

/// Assert that a demonstration is valid
pub fn assert_demonstration_valid(demo: &Demonstration, signature: &Signature) {
    // Check all signature inputs are present
    for input_field in &signature.inputs {
        assert!(
            demo.inputs.contains_key(input_field.name()),
            "Demonstration missing input: {}",
            input_field.name()
        );
    }

    // Check all signature outputs are present
    for output_field in &signature.outputs {
        assert!(
            demo.outputs.contains_key(output_field.name()),
            "Demonstration missing output: {}",
            output_field.name()
        );
    }

    // Check formatting works
    let formatted = demo.format(signature);
    assert!(!formatted.is_empty(), "Demonstration format is empty");

    // Check formatted string contains field names
    for input_field in &signature.inputs {
        assert!(
            formatted.contains(input_field.name()),
            "Formatted demo missing input field: {}",
            input_field.name()
        );
    }
}

/// Assert that metric filters examples correctly
///
/// # Example
///
/// ```ignore
/// use std::sync::Arc;
/// use ggen_ai::dspy::testing::assert_metric_filters_correctly;
///
/// let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
///     Ok(example.outputs.get("answer") == output.get("answer"))
/// });
///
/// assert_metric_filters_correctly(&metric, &passing_examples, &failing_examples);
/// ```
pub fn assert_metric_filters_correctly(
    metric: &MetricFn, should_pass: &[(Example, HashMap<String, Value>)],
    should_fail: &[(Example, HashMap<String, Value>)],
) {
    // Check that all expected passing examples pass
    for (i, (example, output)) in should_pass.iter().enumerate() {
        match metric(example, output) {
            Ok(true) => {}
            Ok(false) => panic!("Expected example {} to pass metric, but it failed", i),
            Err(e) => panic!("Metric error on passing example {}: {}", i, e),
        }
    }

    // Check that all expected failing examples fail
    for (i, (example, output)) in should_fail.iter().enumerate() {
        match metric(example, output) {
            Ok(false) => {}
            Ok(true) => panic!("Expected example {} to fail metric, but it passed", i),
            Err(e) => panic!("Metric error on failing example {}: {}", i, e),
        }
    }
}

/// Assert that output matches signature structure
pub fn assert_output_matches_signature(output: &HashMap<String, Value>, signature: &Signature) {
    for output_field in &signature.outputs {
        assert!(
            output.contains_key(output_field.name()),
            "Output missing required field: {}",
            output_field.name()
        );
    }
}

/// Assert that output values are non-empty strings
pub fn assert_non_empty_outputs(output: &HashMap<String, Value>) {
    for (key, value) in output {
        match value {
            Value::String(s) => assert!(!s.is_empty(), "Output field '{}' is empty", key),
            Value::Array(arr) => assert!(!arr.is_empty(), "Output field '{}' is empty array", key),
            Value::Object(obj) => {
                assert!(!obj.is_empty(), "Output field '{}' is empty object", key)
            }
            _ => {} // Numbers, booleans are fine
        }
    }
}

/// Assert that all examples in a dataset are valid
pub fn assert_dataset_valid(
    examples: &[Example], required_inputs: &[&str], required_outputs: &[&str],
) {
    assert!(!examples.is_empty(), "Dataset is empty");

    for (i, example) in examples.iter().enumerate() {
        assert_example_valid(example, required_inputs, required_outputs);

        // Additional check: ensure no empty values
        for (key, value) in &example.inputs {
            if let Value::String(s) = value {
                assert!(
                    !s.is_empty(),
                    "Example {} has empty input field '{}'",
                    i,
                    key
                );
            }
        }
    }
}

/// Assert that demonstrations are well-formed and diverse
pub fn assert_demonstrations_diverse(demos: &[Demonstration]) {
    assert!(!demos.is_empty(), "No demonstrations found");

    // Check for exact duplicates
    for (i, demo1) in demos.iter().enumerate() {
        for (j, demo2) in demos.iter().enumerate().skip(i + 1) {
            let inputs_equal = demo1.inputs == demo2.inputs;
            let outputs_equal = demo1.outputs == demo2.outputs;

            assert!(
                !(inputs_equal && outputs_equal),
                "Demonstrations {} and {} are identical duplicates",
                i,
                j
            );
        }
    }
}

/// Assert that a value matches expected type
pub fn assert_value_type(value: &Value, expected_type: &str) {
    match expected_type {
        "String" | "str" | "&str" => {
            assert!(value.is_string(), "Expected string, got {:?}", value);
        }
        "i32" | "i64" | "u32" | "u64" | "f32" | "f64" => {
            assert!(value.is_number(), "Expected number, got {:?}", value);
        }
        "bool" => {
            assert!(value.is_boolean(), "Expected boolean, got {:?}", value);
        }
        "Array" | "Vec" => {
            assert!(value.is_array(), "Expected array, got {:?}", value);
        }
        "Object" => {
            assert!(value.is_object(), "Expected object, got {:?}", value);
        }
        _ => {} // Unknown type, skip validation
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::testing::{create_test_signature, qa_example};
    use serde_json::json;

    #[test]
    fn test_assert_example_valid_pass() {
        let example = qa_example("Q", "A");
        assert_example_valid(&example, &["question"], &["answer"]);
    }

    #[test]
    #[should_panic(expected = "missing required input")]
    fn test_assert_example_valid_fail_missing_input() {
        let example = qa_example("Q", "A");
        assert_example_valid(&example, &["missing_field"], &["answer"]);
    }

    #[test]
    #[should_panic(expected = "missing required output")]
    fn test_assert_example_valid_fail_missing_output() {
        let example = qa_example("Q", "A");
        assert_example_valid(&example, &["question"], &["missing_field"]);
    }

    #[test]
    fn test_assert_demonstration_valid() {
        let sig = create_test_signature("QA", "Test");

        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), json!("Q"));

        let mut outputs = HashMap::new();
        outputs.insert("answer".to_string(), json!("A"));

        let demo = Demonstration::new(inputs, outputs);
        assert_demonstration_valid(&demo, &sig);
    }

    #[test]
    #[should_panic(expected = "missing input")]
    fn test_assert_demonstration_valid_fail() {
        let sig = create_test_signature("QA", "Test");

        let inputs = HashMap::new(); // Missing required field
        let outputs = HashMap::from([("answer".to_string(), json!("A"))]);

        let demo = Demonstration::new(inputs, outputs);
        assert_demonstration_valid(&demo, &sig);
    }

    #[test]
    fn test_assert_output_matches_signature() {
        let sig = create_test_signature("QA", "Test");
        let output = HashMap::from([("answer".to_string(), json!("Response"))]);

        assert_output_matches_signature(&output, &sig);
    }

    #[test]
    #[should_panic(expected = "missing required field")]
    fn test_assert_output_matches_signature_fail() {
        let sig = create_test_signature("QA", "Test");
        let output = HashMap::new();

        assert_output_matches_signature(&output, &sig);
    }

    #[test]
    fn test_assert_non_empty_outputs() {
        let output = HashMap::from([
            ("string".to_string(), json!("value")),
            ("number".to_string(), json!(42)),
            ("array".to_string(), json!(["item"])),
        ]);

        assert_non_empty_outputs(&output);
    }

    #[test]
    #[should_panic(expected = "is empty")]
    fn test_assert_non_empty_outputs_fail() {
        let output = HashMap::from([("empty".to_string(), json!(""))]);
        assert_non_empty_outputs(&output);
    }

    #[test]
    fn test_assert_dataset_valid() {
        let examples = vec![qa_example("Q1", "A1"), qa_example("Q2", "A2")];

        assert_dataset_valid(&examples, &["question"], &["answer"]);
    }

    #[test]
    #[should_panic(expected = "Dataset is empty")]
    fn test_assert_dataset_valid_empty() {
        assert_dataset_valid(&[], &["question"], &["answer"]);
    }

    #[test]
    fn test_assert_demonstrations_diverse() {
        let demos = vec![
            Demonstration::new(
                HashMap::from([("q".to_string(), json!("Q1"))]),
                HashMap::from([("a".to_string(), json!("A1"))]),
            ),
            Demonstration::new(
                HashMap::from([("q".to_string(), json!("Q2"))]),
                HashMap::from([("a".to_string(), json!("A2"))]),
            ),
        ];

        assert_demonstrations_diverse(&demos);
    }

    #[test]
    #[should_panic(expected = "identical duplicates")]
    fn test_assert_demonstrations_diverse_fail() {
        let demo = Demonstration::new(
            HashMap::from([("q".to_string(), json!("Q"))]),
            HashMap::from([("a".to_string(), json!("A"))]),
        );

        let demos = vec![demo.clone(), demo];
        assert_demonstrations_diverse(&demos);
    }

    #[test]
    fn test_assert_value_type_string() {
        assert_value_type(&json!("text"), "String");
    }

    #[test]
    fn test_assert_value_type_number() {
        assert_value_type(&json!(42), "i32");
    }

    #[test]
    fn test_assert_value_type_bool() {
        assert_value_type(&json!(true), "bool");
    }

    #[test]
    fn test_assert_value_type_array() {
        assert_value_type(&json!(["a", "b"]), "Array");
    }

    #[test]
    fn test_assert_value_type_object() {
        assert_value_type(&json!({"key": "value"}), "Object");
    }

    #[test]
    #[should_panic(expected = "Expected string")]
    fn test_assert_value_type_fail() {
        assert_value_type(&json!(42), "String");
    }
}
