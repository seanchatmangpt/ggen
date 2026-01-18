//! Standalone adapter tests that don't require full DSPy infrastructure
//!
//! These tests verify adapter functionality independently

use ggen_dspy::adapters::{
    ChatAdapter, CompletionAdapter, Demonstration, JSONAdapter, LlmAdapter, RetryConfig,
    TokenCounter, AdapterWithFallback,
};
use serde_json::Value;
use std::collections::HashMap;

// ============================================================================
// ChatAdapter Tests
// ============================================================================

#[test]
fn test_chat_adapter_basic() {
    let adapter = ChatAdapter::new();
    assert_eq!(adapter.name(), "ChatAdapter");
}

#[test]
fn test_chat_adapter_format_single_field() {
    let adapter = ChatAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("prompt".to_string(), Value::String("test input".to_string()));

    let output_fields = vec!["response".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("prompt: test input"));
    assert!(prompt.contains("[[ ## response ## ]]"));
}

#[test]
fn test_chat_adapter_parse_response() {
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
fn test_chat_adapter_multiple_fields() {
    let adapter = ChatAdapter::new();
    let response = r#"
[[ ## title ## ]]
Test Title

[[ ## body ## ]]
Test body content
    "#;

    let output_fields = vec!["title".to_string(), "body".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert!(result.contains_key("title"));
    assert!(result.contains_key("body"));
}

#[test]
fn test_chat_adapter_with_demonstrations() {
    let adapter = ChatAdapter::new();

    let mut demo_in = HashMap::new();
    demo_in.insert("q".to_string(), Value::String("2+2?".to_string()));
    let mut demo_out = HashMap::new();
    demo_out.insert("a".to_string(), Value::String("4".to_string()));

    let demos = vec![Demonstration::new(demo_in, demo_out)];

    let mut inputs = HashMap::new();
    inputs.insert("q".to_string(), Value::String("3+3?".to_string()));

    let output_fields = vec!["a".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, Some(&demos))
        .unwrap();

    assert!(prompt.contains("Example"));
    assert!(prompt.contains("2+2?"));
    assert!(prompt.contains("Your Turn"));
}

// ============================================================================
// JSONAdapter Tests
// ============================================================================

#[test]
fn test_json_adapter_basic() {
    let adapter = JSONAdapter::new();
    assert_eq!(adapter.name(), "JSONAdapter");
}

#[test]
fn test_json_adapter_compatibility() {
    let adapter = JSONAdapter::new();

    // Compatible models
    assert!(adapter.is_compatible("gpt-4"));
    assert!(adapter.is_compatible("gpt-4-turbo"));
    assert!(adapter.is_compatible("gpt-4o"));
    assert!(adapter.is_compatible("claude-3-opus"));

    // Incompatible models
    assert!(!adapter.is_compatible("gpt-3.5-turbo"));
    assert!(!adapter.is_compatible("llama-2"));
}

#[test]
fn test_json_adapter_format() {
    let adapter = JSONAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("query".to_string(), Value::String("test".to_string()));

    let output_fields = vec!["result".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("JSON"));
    assert!(prompt.contains("schema"));
    assert!(prompt.contains("result"));
}

#[test]
fn test_json_adapter_parse_clean() {
    let adapter = JSONAdapter::new();
    let response = r#"{"answer": "test", "score": 0.95}"#;

    let output_fields = vec!["answer".to_string(), "score".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("answer").unwrap(),
        &Value::String("test".to_string())
    );
    assert!(result.contains_key("score"));
}

#[test]
fn test_json_adapter_parse_markdown() {
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
fn test_json_adapter_parse_invalid() {
    let adapter = JSONAdapter::new();
    let response = "not valid json {{{";

    let output_fields = vec!["field".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
}

// ============================================================================
// CompletionAdapter Tests
// ============================================================================

#[test]
fn test_completion_adapter_basic() {
    let adapter = CompletionAdapter::new();
    assert_eq!(adapter.name(), "CompletionAdapter");
}

#[test]
fn test_completion_adapter_format() {
    let adapter = CompletionAdapter::new();
    let mut inputs = HashMap::new();
    inputs.insert("prompt".to_string(), Value::String("test".to_string()));

    let output_fields = vec!["response".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    assert!(prompt.contains("prompt: test"));
}

#[test]
fn test_completion_adapter_parse_single() {
    let adapter = CompletionAdapter::new();
    let response = "This is the response";

    let output_fields = vec!["output".to_string()];
    let result = adapter.parse_response(response, &output_fields).unwrap();

    assert_eq!(
        result.get("output").unwrap(),
        &Value::String("This is the response".to_string())
    );
}

// ============================================================================
// AdapterWithFallback Tests
// ============================================================================

#[test]
fn test_fallback_json_compatible() {
    let adapter = AdapterWithFallback::new("gpt-4");
    assert_eq!(adapter.model, "gpt-4");
    assert_eq!(adapter.name(), "JSONAdapter");
}

#[test]
fn test_fallback_chat_compatible() {
    let adapter = AdapterWithFallback::new("gpt-3.5-turbo");
    assert_eq!(adapter.model, "gpt-3.5-turbo");
    assert_eq!(adapter.name(), "ChatAdapter");
}

#[test]
fn test_fallback_format_delegates() {
    let adapter = AdapterWithFallback::new("gpt-4");
    let mut inputs = HashMap::new();
    inputs.insert("test".to_string(), Value::String("value".to_string()));

    let output_fields = vec!["result".to_string()];
    let prompt = adapter
        .format_prompt(&inputs, &output_fields, None, None)
        .unwrap();

    // Should use JSON format for GPT-4
    assert!(prompt.contains("JSON"));
}

// ============================================================================
// Demonstration Tests
// ============================================================================

#[test]
fn test_demonstration_creation() {
    let mut inputs = HashMap::new();
    inputs.insert("in".to_string(), Value::String("input".to_string()));

    let mut outputs = HashMap::new();
    outputs.insert("out".to_string(), Value::String("output".to_string()));

    let demo = Demonstration::new(inputs.clone(), outputs.clone());

    assert_eq!(demo.inputs, inputs);
    assert_eq!(demo.outputs, outputs);
}

#[test]
fn test_demonstration_serialization() {
    let mut inputs = HashMap::new();
    inputs.insert("in".to_string(), Value::String("input".to_string()));

    let mut outputs = HashMap::new();
    outputs.insert("out".to_string(), Value::String("output".to_string()));

    let demo = Demonstration::new(inputs.clone(), outputs.clone());

    // Test serialization
    let json = serde_json::to_string(&demo).unwrap();
    let deserialized: Demonstration = serde_json::from_str(&json).unwrap();

    assert_eq!(deserialized.inputs, inputs);
    assert_eq!(deserialized.outputs, outputs);
}

// ============================================================================
// RetryConfig Tests
// ============================================================================

#[test]
fn test_retry_config_default() {
    let config = RetryConfig::default();
    assert_eq!(config.max_retries, 3);
    assert!(config.backoff_multiplier > 1.0);
}

#[test]
fn test_retry_config_backoff() {
    let config = RetryConfig::default();
    let d1 = config.backoff_duration(0);
    let d2 = config.backoff_duration(1);
    let d3 = config.backoff_duration(2);

    // Backoff should increase exponentially
    assert!(d2 > d1);
    assert!(d3 > d2);
    assert!(d3 <= config.max_backoff);
}

// ============================================================================
// TokenCounter Tests
// ============================================================================

#[test]
fn test_token_counter_basic() {
    let counter = TokenCounter::new();
    counter.add_usage(100, 50, "gpt-4".to_string());

    let stats = counter.get_stats();
    assert_eq!(stats.prompt_tokens, 100);
    assert_eq!(stats.completion_tokens, 50);
    assert_eq!(stats.total_tokens, 150);
}

#[test]
fn test_token_counter_multiple_calls() {
    let counter = TokenCounter::new();
    counter.add_usage(100, 50, "gpt-4".to_string());
    counter.add_usage(200, 75, "gpt-4".to_string());

    let stats = counter.get_stats();
    assert_eq!(stats.prompt_tokens, 300);
    assert_eq!(stats.completion_tokens, 125);
    assert_eq!(stats.total_tokens, 425);

    let model_stats = stats.model_usage.get("gpt-4").unwrap();
    assert_eq!(model_stats.request_count, 2);
    assert_eq!(model_stats.prompt_tokens, 300);
    assert_eq!(model_stats.completion_tokens, 125);
}

#[test]
fn test_token_counter_multiple_models() {
    let counter = TokenCounter::new();
    counter.add_usage(100, 50, "gpt-4".to_string());
    counter.add_usage(200, 75, "claude-3".to_string());

    let stats = counter.get_stats();
    assert_eq!(stats.prompt_tokens, 300);
    assert_eq!(stats.completion_tokens, 125);

    assert_eq!(stats.model_usage.len(), 2);
    assert!(stats.model_usage.contains_key("gpt-4"));
    assert!(stats.model_usage.contains_key("claude-3"));
}

// ============================================================================
// Edge Cases and Error Handling
// ============================================================================

#[test]
fn test_chat_adapter_empty_field_error() {
    let adapter = ChatAdapter::new();
    let response = "[[ ## answer ## ]]\n\n[[ ## next ## ]]";

    let output_fields = vec!["answer".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
}

#[test]
fn test_chat_adapter_missing_field_error() {
    let adapter = ChatAdapter::new();
    let response = "[[ ## answer ## ]]\nAnswer here";

    let output_fields = vec!["missing_field".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
}

#[test]
fn test_json_adapter_missing_field_error() {
    let adapter = JSONAdapter::new();
    let response = r#"{"answer": "test"}"#;

    let output_fields = vec!["missing".to_string()];
    let result = adapter.parse_response(response, &output_fields);

    assert!(result.is_err());
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
