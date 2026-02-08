//! Comprehensive adapter tests covering format generation, parsing, and fallback
//!
//! Tests organized by adapter type and functionality

use ggen_ai::client::{GenAiClient, LlmConfig};
use ggen_dspy::adapters::{
    AdapterWithFallback, ChatAdapter, Demonstration, JSONAdapter, LlmAdapter,
};
use ggen_dspy::error::DspyError;
use serde_json::Value;
use std::collections::HashMap;

// ============================================================================
// ChatAdapter Tests
// ============================================================================

#[test]
fn test_chat_adapter_format_single_field() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert(
        "prompt".to_string(),
        Value::String("test input".to_string()),
    );

    let output_fields = vec!["response".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("prompt: test input"));
    assert!(prompt.contains("[[ ## response ## ]]"));
}

#[test]
fn test_chat_adapter_format_multiple_fields() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("query".to_string(), Value::String("test query".to_string()));

    let output_fields = vec![
        "title".to_string(),
        "summary".to_string(),
        "tags".to_string(),
    ];

    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("[[ ## title ## ]]"));
    assert!(prompt.contains("[[ ## summary ## ]]"));
    assert!(prompt.contains("[[ ## tags ## ]]"));
}

#[test]
fn test_chat_adapter_format_with_complex_types() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert(
        "data".to_string(),
        serde_json::json!({"nested": {"value": 123}}),
    );

    let output_fields = vec!["result".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("nested"));
    assert!(prompt.contains("123"));
}

#[test]
fn test_chat_adapter_parse_single_field() {
    let adapter = ChatAdapter::new();
    let response = "[[ ## answer ## ]]\nThis is the answer";

    let output_fields = vec!["answer".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("answer").unwrap(),
        &Value::String("This is the answer".to_string())
    );
}

#[test]
fn test_chat_adapter_parse_multiple_fields() {
    let adapter = ChatAdapter::new();
    let response = r#"
[[ ## title ## ]]
Test Title

[[ ## body ## ]]
Test body content

[[ ## author ## ]]
John Doe
    "#;

    let output_fields = vec![
        "title".to_string(),
        "body".to_string(),
        "author".to_string(),
    ];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("title").unwrap(),
        &Value::String("Test Title".to_string())
    );
    assert_eq!(
        result.get("body").unwrap(),
        &Value::String("Test body content".to_string())
    );
    assert_eq!(
        result.get("author").unwrap(),
        &Value::String("John Doe".to_string())
    );
}

#[test]
fn test_chat_adapter_parse_json_field() {
    let adapter = ChatAdapter::new();
    let response = r#"[[ ## metadata ## ]]
{"version": "1.0", "count": 42}"#;

    let output_fields = vec!["metadata".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    match result.get("metadata").unwrap() {
        Value::Object(obj) => {
            assert_eq!(
                obj.get("version").unwrap(),
                &Value::String("1.0".to_string())
            );
            assert_eq!(obj.get("count").unwrap(), &Value::Number(42.into()));
        }
        _ => panic!("Expected object"),
    }
}

#[test]
fn test_chat_adapter_parse_missing_field_error() {
    let adapter = ChatAdapter::new();
    let response = "[[ ## answer ## ]]\nAnswer here";

    let output_fields = vec!["missing_field".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
    match result.unwrap_err() {
        DspyError::FieldError { field } => assert_eq!(field, "missing_field"),
        _ => panic!("Expected FieldError"),
    }
}

#[test]
fn test_chat_adapter_parse_empty_field_error() {
    let adapter = ChatAdapter::new();
    let response = "[[ ## answer ## ]]\n\n[[ ## next ## ]]";

    let output_fields = vec!["answer".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
}

#[test]
fn test_chat_adapter_with_demonstrations() {
    let adapter = ChatAdapter::new();

    let mut demo1_in = HashMap::new();
    demo1_in.insert("question".to_string(), Value::String("2+2?".to_string()));
    let mut demo1_out = HashMap::new();
    demo1_out.insert("answer".to_string(), Value::String("4".to_string()));

    let mut demo2_in = HashMap::new();
    demo2_in.insert("question".to_string(), Value::String("3+3?".to_string()));
    let mut demo2_out = HashMap::new();
    demo2_out.insert("answer".to_string(), Value::String("6".to_string()));

    let demos = vec![
        Demonstration::new(demo1_in, demo1_out),
        Demonstration::new(demo2_in, demo2_out),
    ];

    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), Value::String("5+5?".to_string()));

    let output_fields = vec!["answer".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, Some(&demos))
        .unwrap();

    assert!(prompt.contains("Example 1"));
    assert!(prompt.contains("Example 2"));
    assert!(prompt.contains("2+2?"));
    assert!(prompt.contains("3+3?"));
    assert!(prompt.contains("Your Turn"));
    assert!(prompt.contains("5+5?"));
}

// ============================================================================
// JSONAdapter Tests
// ============================================================================

#[test]
fn test_json_adapter_format_basic() {
    let adapter = JSONAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("query".to_string(), Value::String("test".to_string()));

    let output_fields = vec!["result".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("JSON Schema"));
    assert!(prompt.contains("result"));
    assert!(prompt.contains("\"query\""));
}

#[test]
fn test_json_adapter_format_with_custom_schema() {
    let adapter = JSONAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("input".to_string(), Value::String("test".to_string()));

    let schema = serde_json::json!({
        "type": "object",
        "properties": {
            "output": {"type": "string"},
            "confidence": {"type": "number", "minimum": 0, "maximum": 1}
        },
        "required": ["output", "confidence"]
    });

    let output_fields = vec!["output".to_string(), "confidence".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, Some(&schema), None)
        .unwrap();

    assert!(prompt.contains("minimum"));
    assert!(prompt.contains("maximum"));
    assert!(prompt.contains("confidence"));
}

#[test]
fn test_json_adapter_parse_clean_json() {
    let adapter = JSONAdapter::new();
    let response = r#"{"answer": "test answer", "score": 0.95}"#;

    let output_fields = vec!["answer".to_string(), "score".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("answer").unwrap(),
        &Value::String("test answer".to_string())
    );
    assert_eq!(
        result.get("score").unwrap(),
        &Value::Number(serde_json::Number::from_f64(0.95).unwrap())
    );
}

#[test]
fn test_json_adapter_parse_with_markdown() {
    let adapter = JSONAdapter::new();
    let response = r#"```json
{
    "title": "Test",
    "count": 42
}
```"#;

    let output_fields = vec!["title".to_string(), "count".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("title").unwrap(),
        &Value::String("Test".to_string())
    );
    assert_eq!(result.get("count").unwrap(), &Value::Number(42.into()));
}

#[test]
fn test_json_adapter_parse_nested_object() {
    let adapter = JSONAdapter::new();
    let response = r#"{
        "data": {
            "nested": {
                "value": "test"
            }
        }
    }"#;

    let output_fields = vec!["data".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    match result.get("data").unwrap() {
        Value::Object(obj) => {
            assert!(obj.contains_key("nested"));
        }
        _ => panic!("Expected object"),
    }
}

#[test]
fn test_json_adapter_parse_invalid_json_error() {
    let adapter = JSONAdapter::new();
    let response = "not valid json {{{";

    let output_fields = vec!["field".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
    match result.unwrap_err() {
        DspyError::ParsingError(_) => {}
        _ => panic!("Expected ParsingError"),
    }
}

#[test]
fn test_json_adapter_model_compatibility() {
    let adapter = JSONAdapter::new();

    // Compatible
    assert!(adapter.is_compatible("gpt-4"));
    assert!(adapter.is_compatible("gpt-4-turbo"));
    assert!(adapter.is_compatible("gpt-4o"));

    // Incompatible
    assert!(!adapter.is_compatible("gpt-3.5-turbo"));
    assert!(!adapter.is_compatible("claude-3"));
}

// ============================================================================
// Fallback Mechanism Tests
// ============================================================================

#[test]
fn test_adapter_selection_for_compatible_model() {
    let fallback = AdapterWithFallback::new("gpt-4");
    // Test that correct adapter is selected (would need async test for full coverage)
    assert_eq!(fallback.model, "gpt-4");
}

#[test]
fn test_adapter_selection_for_incompatible_model() {
    let fallback = AdapterWithFallback::new("gpt-3.5-turbo");
    // Test that fallback to ChatAdapter works (would need async test for full coverage)
    assert_eq!(fallback.model, "gpt-3.5-turbo");
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_chat_adapter_empty_output_fields() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("test".to_string(), Value::String("value".to_string()));

    let output_fields: Vec<String> = vec![];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    // Should still include inputs
    assert!(prompt.contains("test: value"));
}

#[test]
fn test_json_adapter_empty_demonstrations() {
    let adapter = JSONAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("test".to_string(), Value::String("value".to_string()));

    let output_fields = vec!["output".to_string()];
    let demos: Vec<Demonstration> = vec![];

    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, Some(&demos))
        .unwrap();

    assert!(!prompt.contains("Example"));
}

#[test]
fn test_chat_adapter_special_characters() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert(
        "text".to_string(),
        Value::String("Special: [[ ## ]] chars".to_string()),
    );

    let output_fields = vec!["result".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("Special: [[ ## ]] chars"));
    assert!(prompt.contains("[[ ## result ## ]]"));
}

#[test]
fn test_json_adapter_unicode() {
    let adapter = JSONAdapter::new();
    let response = r#"{"answer": "こんにちは", "emoji": "😀"}"#;

    let output_fields = vec!["answer".to_string(), "emoji".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("answer").unwrap(),
        &Value::String("こんにちは".to_string())
    );
    assert_eq!(
        result.get("emoji").unwrap(),
        &Value::String("😀".to_string())
    );
}

#[test]
fn test_adapter_name_identification() {
    let chat = ChatAdapter::new();
    let json = JSONAdapter::new();

    assert_eq!(chat.name(), "ChatAdapter");
    assert_eq!(json.name(), "JSONAdapter");
}

#[test]
fn test_demonstration_serialization() {
    let mut inputs = HashMap::new();
    inputs.insert("in".to_string(), Value::String("input".to_string()));

    let mut outputs = HashMap::new();
    outputs.insert("out".to_string(), Value::String("output".to_string()));

    let demo = Demonstration::new(inputs.clone(), outputs.clone());

    // Test that demonstration can be serialized
    let json = serde_json::to_string(&demo).unwrap();
    let deserialized: Demonstration = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.inputs, inputs);
    assert_eq!(deserialized.outputs, outputs);
}
