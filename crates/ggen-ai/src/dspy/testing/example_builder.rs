//! ExampleBuilder - Fluent API for Creating Test Examples
//!
//! Provides a builder pattern for creating DSPy examples with less boilerplate.

use crate::dspy::optimizer::Example;
use serde_json::Value;
use std::collections::HashMap;

/// Builder for creating test examples with fluent API
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::ExampleBuilder;
///
/// let example = ExampleBuilder::new()
///     .input("question", "What is Rust?")
///     .output("answer", "A systems programming language")
///     .build();
/// ```
pub struct ExampleBuilder {
    inputs: HashMap<String, Value>,
    outputs: HashMap<String, Value>,
}

impl ExampleBuilder {
    /// Create a new example builder
    pub fn new() -> Self {
        Self {
            inputs: HashMap::new(),
            outputs: HashMap::new(),
        }
    }

    /// Add input field (accepts any type convertible to Value)
    pub fn input<V: Into<Value>>(mut self, key: &str, value: V) -> Self {
        self.inputs.insert(key.to_string(), value.into());
        self
    }

    /// Add output field (accepts any type convertible to Value)
    pub fn output<V: Into<Value>>(mut self, key: &str, value: V) -> Self {
        self.outputs.insert(key.to_string(), value.into());
        self
    }

    /// Add multiple inputs from iterator
    pub fn inputs<I, K, V>(mut self, items: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<Value>,
    {
        for (key, value) in items {
            self.inputs.insert(key.into(), value.into());
        }
        self
    }

    /// Add multiple outputs from iterator
    pub fn outputs<I, K, V>(mut self, items: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: Into<String>,
        V: Into<Value>,
    {
        for (key, value) in items {
            self.outputs.insert(key.into(), value.into());
        }
        self
    }

    /// Build the example
    pub fn build(self) -> Example {
        Example::new(self.inputs, self.outputs)
    }

    /// Build example with only inputs (for inference)
    pub fn build_inputs_only(self) -> Example {
        Example::from_inputs(self.inputs)
    }
}

impl Default for ExampleBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Conversion from tuple pairs for quick example creation
impl<I, O> From<(&str, I, &str, O)> for Example
where
    I: Into<Value>,
    O: Into<Value>,
{
    fn from((input_key, input_val, output_key, output_val): (&str, I, &str, O)) -> Self {
        ExampleBuilder::new()
            .input(input_key, input_val)
            .output(output_key, output_val)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_builder_basic() {
        let example = ExampleBuilder::new()
            .input("question", "What is Rust?")
            .output("answer", "A programming language")
            .build();

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 1);
        assert_eq!(
            example.inputs.get("question").unwrap(),
            &json!("What is Rust?")
        );
        assert_eq!(
            example.outputs.get("answer").unwrap(),
            &json!("A programming language")
        );
    }

    #[test]
    fn test_builder_multiple_fields() {
        let example = ExampleBuilder::new()
            .input("question", "What is 2+2?")
            .input("context", "Math problem")
            .output("answer", "4")
            .output("explanation", "Addition of 2 and 2")
            .build();

        assert_eq!(example.inputs.len(), 2);
        assert_eq!(example.outputs.len(), 2);
    }

    #[test]
    fn test_builder_different_types() {
        let example = ExampleBuilder::new()
            .input("string_field", "text")
            .input("number_field", 42)
            .input("bool_field", true)
            .input("array_field", json!(["a", "b"]))
            .output("result", json!({"key": "value"}))
            .build();

        assert_eq!(example.inputs.len(), 4);
        assert_eq!(example.outputs.len(), 1);
    }

    #[test]
    fn test_builder_inputs_method() {
        let input_pairs = vec![("q1", "Answer 1"), ("q2", "Answer 2")];

        let example = ExampleBuilder::new()
            .inputs(input_pairs)
            .output("summary", "Combined")
            .build();

        assert_eq!(example.inputs.len(), 2);
        assert_eq!(example.outputs.len(), 1);
    }

    #[test]
    fn test_builder_outputs_method() {
        let output_pairs = vec![("a1", "Response 1"), ("a2", "Response 2")];

        let example = ExampleBuilder::new()
            .input("query", "Test")
            .outputs(output_pairs)
            .build();

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 2);
    }

    #[test]
    fn test_builder_inputs_only() {
        let example = ExampleBuilder::new()
            .input("question", "What is DSPy?")
            .build_inputs_only();

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 0);
    }

    #[test]
    fn test_builder_default() {
        let example = ExampleBuilder::default()
            .input("test", "value")
            .build();

        assert_eq!(example.inputs.len(), 1);
    }

    #[test]
    fn test_from_tuple_conversion() {
        let example: Example = ("question", "What is Rust?", "answer", "A language").into();

        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 1);
        assert_eq!(
            example.inputs.get("question").unwrap(),
            &json!("What is Rust?")
        );
        assert_eq!(
            example.outputs.get("answer").unwrap(),
            &json!("A language")
        );
    }

    #[test]
    fn test_builder_chaining() {
        // Test that builder methods can be chained in any order
        let example = ExampleBuilder::new()
            .output("answer", "First output")
            .input("question", "Question")
            .output("confidence", 0.95)
            .input("context", "Context")
            .build();

        assert_eq!(example.inputs.len(), 2);
        assert_eq!(example.outputs.len(), 2);
    }
}
