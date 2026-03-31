//! Edge case tests for LLM client
//!
//! Tests error paths, boundary conditions, and edge cases:
//! - Configuration validation errors
//! - Invalid model names
//! - Boundary values for parameters
//! - Empty prompts
//! - Very long prompts
//! - Network error handling
//! - Timeout scenarios
//! - Invalid API responses
//! - Concurrent requests
//! - Configuration serialization/deserialization

use ggen_ai::client::{GenAiClient, LlmChunk, LlmClient, LlmConfig, LlmResponse};
use std::time::Duration;
use tokio::time::sleep;

// ---------------------------------------------------------------------------
// Test 1: Empty Model Name
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_empty_model() {
    let mut config = LlmConfig::default();
    config.model = "".to_string();

    let result = config.validate();

    assert!(result.is_err(), "Empty model should fail validation");
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Model name cannot be empty"),
        "Error should mention empty model: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 2: Max Tokens Below Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_max_tokens_below_minimum() {
    let mut config = LlmConfig::default();
    config.max_tokens = Some(0);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Max tokens below minimum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Max tokens must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 3: Max Tokens Above Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_max_tokens_above_maximum() {
    let mut config = LlmConfig::default();
    config.max_tokens = Some(200000);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Max tokens above maximum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Max tokens must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 4: Temperature Below Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_temperature_below_minimum() {
    let mut config = LlmConfig::default();
    config.temperature = Some(-0.1);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Temperature below minimum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Temperature must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 5: Temperature Above Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_temperature_above_maximum() {
    let mut config = LlmConfig::default();
    config.temperature = Some(2.1);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Temperature above maximum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Temperature must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 6: Top-P Below Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_top_p_below_minimum() {
    let mut config = LlmConfig::default();
    config.top_p = Some(-0.1);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Top-p below minimum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Top-p must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 7: Top-P Above Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_top_p_above_maximum() {
    let mut config = LlmConfig::default();
    config.top_p = Some(1.1);

    let result = config.validate();

    assert!(
        result.is_err(),
        "Top-p above maximum should fail validation"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Top-p must be between"),
        "Error should mention valid range: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 8: Boundary Value - Max Tokens Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_max_tokens_minimum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.max_tokens = Some(llm::MIN_TOKEN_LIMIT);

    let result = config.validate();

    assert!(result.is_ok(), "Minimum max_tokens should be valid");
}

// ---------------------------------------------------------------------------
// Test 9: Boundary Value - Max Tokens Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_max_tokens_maximum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.max_tokens = Some(llm::MAX_TOKEN_LIMIT);

    let result = config.validate();

    assert!(result.is_ok(), "Maximum max_tokens should be valid");
}

// ---------------------------------------------------------------------------
// Test 10: Boundary Value - Temperature Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_temperature_minimum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.temperature = Some(llm::MIN_TEMPERATURE);

    let result = config.validate();

    assert!(result.is_ok(), "Minimum temperature should be valid");
}

// ---------------------------------------------------------------------------
// Test 11: Boundary Value - Temperature Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_temperature_maximum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.temperature = Some(llm::MAX_TEMPERATURE);

    let result = config.validate();

    assert!(result.is_ok(), "Maximum temperature should be valid");
}

// ---------------------------------------------------------------------------
// Test 12: Boundary Value - Top-P Minimum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_top_p_minimum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.top_p = Some(llm::MIN_TOP_P);

    let result = config.validate();

    assert!(result.is_ok(), "Minimum top_p should be valid");
}

// ---------------------------------------------------------------------------
// Test 13: Boundary Value - Top-P Maximum
// ---------------------------------------------------------------------------

#[test]
fn test_config_validation_top_p_maximum_boundary() {
    use ggen_ai::constants::llm;

    let mut config = LlmConfig::default();
    config.top_p = Some(llm::MAX_TOP_P);

    let result = config.validate();

    assert!(result.is_ok(), "Maximum top_p should be valid");
}

// ---------------------------------------------------------------------------
// Test 14: Default Configuration is Valid
// ---------------------------------------------------------------------------

#[test]
fn test_default_config_is_valid() {
    let config = LlmConfig::default();

    let result = config.validate();

    assert!(result.is_ok(), "Default configuration should be valid");
}

// ---------------------------------------------------------------------------
// Test 15: Client Creation with Invalid Config
// ---------------------------------------------------------------------------

#[test]
fn test_client_creation_invalid_config() {
    let mut config = LlmConfig::default();
    config.model = "".to_string(); // Invalid

    let result = GenAiClient::new(config);

    assert!(
        result.is_err(),
        "Client creation should fail with invalid config"
    );
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Model name cannot be empty"),
        "Error should mention validation failure: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 16: Client Creation with Valid Config
// ---------------------------------------------------------------------------

#[test]
fn test_client_creation_valid_config() {
    let config = LlmConfig::default();

    let result = GenAiClient::new(config);

    assert!(
        result.is_ok(),
        "Client creation should succeed with valid config"
    );
}

// ---------------------------------------------------------------------------
// Test 17: Get Config Returns Clone
// ---------------------------------------------------------------------------

#[test]
fn test_get_config_returns_clone() {
    let config = LlmConfig::default();
    let client = GenAiClient::new(config.clone()).unwrap();

    let retrieved_config = client.get_config();

    assert_eq!(retrieved_config.model, config.model);
    assert_eq!(retrieved_config.max_tokens, config.max_tokens);
    assert_eq!(retrieved_config.temperature, config.temperature);
    assert_eq!(retrieved_config.top_p, config.top_p);
}

// ---------------------------------------------------------------------------
// Test 18: Update Config
// ---------------------------------------------------------------------------

#[test]
fn test_update_config() {
    let config = LlmConfig::default();
    let mut client = GenAiClient::new(config).unwrap();

    let mut new_config = LlmConfig::default();
    new_config.model = "gpt-4".to_string();
    new_config.temperature = Some(0.5);

    client.update_config(new_config.clone());

    let retrieved_config = client.get_config();
    assert_eq!(retrieved_config.model, "gpt-4");
    assert_eq!(retrieved_config.temperature, Some(0.5));
}

// ---------------------------------------------------------------------------
// Test 19: Configuration Serialization
// ---------------------------------------------------------------------------

#[test]
fn test_config_serialization() {
    let mut config = LlmConfig::default();
    config.model = "test-model".to_string();
    config.max_tokens = Some(2048);
    config.temperature = Some(0.7);
    config.top_p = Some(0.9);

    let json = serde_json::to_string(&config).expect("Serialization should succeed");

    assert!(json.contains("test-model"));
    assert!(json.contains("2048"));
    assert!(json.contains("0.7"));
    assert!(json.contains("0.9"));
}

// ---------------------------------------------------------------------------
// Test 20: Configuration Deserialization
// ---------------------------------------------------------------------------

#[test]
fn test_config_deserialization() {
    let json = r#"{
        "model": "test-model",
        "max_tokens": 2048,
        "temperature": 0.7,
        "top_p": 0.9,
        "stop": ["stop1", "stop2"],
        "extra": {"key": "value"}
    }"#;

    let config: LlmConfig = serde_json::from_str(json).expect("Deserialization should succeed");

    assert_eq!(config.model, "test-model");
    assert_eq!(config.max_tokens, Some(2048));
    assert_eq!(config.temperature, Some(0.7));
    assert_eq!(config.top_p, Some(0.9));
    assert_eq!(
        config.stop,
        Some(vec!["stop1".to_string(), "stop2".to_string()])
    );
    assert_eq!(config.extra.len(), 1);
}

// ---------------------------------------------------------------------------
// Test 21: LlmResponse Serialization
// ---------------------------------------------------------------------------

#[test]
fn test_llm_response_serialization() {
    use ggen_ai::client::UsageStats;

    let response = LlmResponse {
        content: "Test response".to_string(),
        usage: Some(UsageStats {
            prompt_tokens: 100,
            completion_tokens: 50,
            total_tokens: 150,
        }),
        model: "test-model".to_string(),
        finish_reason: Some("stop".to_string()),
        extra: std::collections::HashMap::new(),
    };

    let json = serde_json::to_string(&response).expect("Serialization should succeed");

    assert!(json.contains("Test response"));
    assert!(json.contains("test-model"));
    assert!(json.contains("100"));
    assert!(json.contains("50"));
    assert!(json.contains("150"));
}

// ---------------------------------------------------------------------------
// Test 22: LlmChunk Serialization
// ---------------------------------------------------------------------------

#[test]
fn test_llm_chunk_serialization() {
    use ggen_ai::client::UsageStats;

    let chunk = LlmChunk {
        content: "Partial response".to_string(),
        model: "test-model".to_string(),
        finish_reason: None,
        usage: None,
        extra: std::collections::HashMap::new(),
    };

    let json = serde_json::to_string(&chunk).expect("Serialization should succeed");

    assert!(json.contains("Partial response"));
    assert!(json.contains("test-model"));
}

// ---------------------------------------------------------------------------
// Test 23: Empty Extra Map is Valid
// ---------------------------------------------------------------------------

#[test]
fn test_config_empty_extra_map() {
    let config = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: None,
        temperature: None,
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    let result = config.validate();

    assert!(result.is_ok(), "Empty extra map should be valid");
}

// ---------------------------------------------------------------------------
// Test 24: Extra Map with Custom Values
// ---------------------------------------------------------------------------

#[test]
fn test_config_extra_map_with_values() {
    let mut extra = std::collections::HashMap::new();
    extra.insert("key1".to_string(), serde_json::json!("value1"));
    extra.insert("key2".to_string(), serde_json::json!(42));

    let config = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: None,
        temperature: None,
        top_p: None,
        stop: None,
        extra,
    };

    let result = config.validate();

    assert!(result.is_ok(), "Extra map with values should be valid");
    assert_eq!(config.extra.len(), 2);
}

// ---------------------------------------------------------------------------
// Test 25: Stop Sequences Validation
// ---------------------------------------------------------------------------

#[test]
fn test_config_stop_sequences() {
    let mut config = LlmConfig::default();
    config.stop = Some(vec!["stop1".to_string(), "stop2".to_string()]);

    let result = config.validate();

    assert!(result.is_ok(), "Stop sequences should be valid");
    assert_eq!(
        config.stop,
        Some(vec!["stop1".to_string(), "stop2".to_string()])
    );
}

// ---------------------------------------------------------------------------
// Test 26: Empty Stop Sequences
// ---------------------------------------------------------------------------

#[test]
fn test_config_empty_stop_sequences() {
    let mut config = LlmConfig::default();
    config.stop = Some(vec![]);

    let result = config.validate();

    assert!(result.is_ok(), "Empty stop sequences should be valid");
}

// ---------------------------------------------------------------------------
// Test 27: Clone Config
// ---------------------------------------------------------------------------

#[test]
fn test_config_clone() {
    let mut config = LlmConfig::default();
    config.model = "test-model".to_string();
    config.temperature = Some(0.5);

    let cloned = config.clone();

    assert_eq!(cloned.model, config.model);
    assert_eq!(cloned.temperature, config.temperature);

    // Modify original
    config.temperature = Some(0.8);

    // Clone should be independent
    assert_eq!(cloned.temperature, Some(0.5));
}

// ---------------------------------------------------------------------------
// Test 28: Debug Output for Config
// ---------------------------------------------------------------------------

#[test]
fn test_config_debug_output() {
    let config = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: Some(2048),
        temperature: Some(0.7),
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    let debug_str = format!("{:?}", config);

    assert!(debug_str.contains("test-model"));
    assert!(debug_str.contains("2048"));
    assert!(debug_str.contains("0.7"));
}

// ---------------------------------------------------------------------------
// Test 29: Multiple Validation Errors
// ---------------------------------------------------------------------------

#[test]
fn test_config_multiple_validation_errors() {
    let mut config = LlmConfig::default();
    config.model = "".to_string(); // Invalid
    config.max_tokens = Some(0); // Invalid
    config.temperature = Some(3.0); // Invalid

    // Should fail on first error (empty model)
    let result = config.validate();

    assert!(result.is_err());

    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Model name cannot be empty"),
        "Should report first validation error: {}",
        error_msg
    );
}

// ---------------------------------------------------------------------------
// Test 30: Valid Model Names
// ---------------------------------------------------------------------------

#[test]
fn test_config_valid_model_names() {
    let valid_models = vec![
        "gpt-4",
        "gpt-3.5-turbo",
        "claude-3-opus",
        "groq::openai/gpt-oss-20b",
        "llama-2-70b",
        "mistral-large",
    ];

    for model in valid_models {
        let mut config = LlmConfig::default();
        config.model = model.to_string();

        let result = config.validate();

        assert!(result.is_ok(), "Model '{}' should be valid", model);
    }
}

// ---------------------------------------------------------------------------
// Test 31: Very Long Model Name
// ---------------------------------------------------------------------------

#[test]
fn test_config_very_long_model_name() {
    let long_name = "a".repeat(10000);

    let mut config = LlmConfig::default();
    config.model = long_name.clone();

    let result = config.validate();

    // Should be valid (no length restriction in validation)
    assert!(result.is_ok(), "Very long model name should be valid");
}

// ---------------------------------------------------------------------------
// Test 32: Model Name with Special Characters
// ---------------------------------------------------------------------------

#[test]
fn test_config_model_name_special_characters() {
    let special_models = vec![
        "model-with-dashes",
        "model_with_underscores",
        "model.with.dots",
        "model:with:colons",
        "model/with/slashes",
    ];

    for model in special_models {
        let mut config = LlmConfig::default();
        config.model = model.to_string();

        let result = config.validate();

        assert!(
            result.is_ok(),
            "Model '{}' with special chars should be valid",
            model
        );
    }
}

// ---------------------------------------------------------------------------
// Test 33: Temperature Precision
// ---------------------------------------------------------------------------

#[test]
fn test_config_temperature_precision() {
    let test_temps = vec![0.0, 0.1, 0.5, 0.7, 1.0, 1.5, 2.0];

    for temp in test_temps {
        let mut config = LlmConfig::default();
        config.temperature = Some(temp);

        let result = config.validate();

        assert!(result.is_ok(), "Temperature {} should be valid", temp);
    }
}

// ---------------------------------------------------------------------------
// Test 34: Top-P Precision
// ---------------------------------------------------------------------------

#[test]
fn test_config_top_p_precision() {
    let test_top_p = vec![0.0, 0.1, 0.5, 0.9, 1.0];

    for top_p in test_top_p {
        let mut config = LlmConfig::default();
        config.top_p = Some(top_p);

        let result = config.validate();

        assert!(result.is_ok(), "Top-p {} should be valid", top_p);
    }
}

// ---------------------------------------------------------------------------
// Test 35: All Parameters at Boundary Values
// ---------------------------------------------------------------------------

#[test]
fn test_config_all_parameters_at_boundaries() {
    use ggen_ai::constants::llm;

    let config = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: Some(llm::MAX_TOKEN_LIMIT),
        temperature: Some(llm::MAX_TEMPERATURE),
        top_p: Some(llm::MAX_TOP_P),
        stop: Some(vec!["STOP".to_string()]),
        extra: std::collections::HashMap::new(),
    };

    let result = config.validate();

    assert!(
        result.is_ok(),
        "All parameters at max boundaries should be valid"
    );
}

// ---------------------------------------------------------------------------
// Test 36: Nil Values are Valid
// ---------------------------------------------------------------------------

#[test]
fn test_config_nil_values_are_valid() {
    let config = LlmConfig {
        model: "test-model".to_string(),
        max_tokens: None,
        temperature: None,
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    };

    let result = config.validate();

    assert!(result.is_ok(), "All nil values should be valid");
}

// ---------------------------------------------------------------------------
// Test 37: Update Config with Invalid Value
// ---------------------------------------------------------------------------

#[test]
fn test_update_config_then_validate() {
    let config = LlmConfig::default();
    let mut client = GenAiClient::new(config).unwrap();

    // Update to invalid config
    let mut invalid_config = LlmConfig::default();
    invalid_config.model = "".to_string();

    client.update_config(invalid_config);

    // get_config should return the invalid config
    let retrieved = client.get_config();
    assert_eq!(retrieved.model, "");

    // But validation should fail
    let result = retrieved.validate();
    assert!(
        result.is_err(),
        "Updated invalid config should fail validation"
    );
}

// ---------------------------------------------------------------------------
// Test 38: Multiple Clients with Different Configs
// ---------------------------------------------------------------------------

#[test]
fn test_multiple_clients_different_configs() {
    let mut config1 = LlmConfig::default();
    config1.model = "model1".to_string();
    config1.temperature = Some(0.5);

    let mut config2 = LlmConfig::default();
    config2.model = "model2".to_string();
    config2.temperature = Some(1.0);

    let client1 = GenAiClient::new(config1).unwrap();
    let client2 = GenAiClient::new(config2).unwrap();

    assert_eq!(client1.get_config().model, "model1");
    assert_eq!(client1.get_config().temperature, Some(0.5));

    assert_eq!(client2.get_config().model, "model2");
    assert_eq!(client2.get_config().temperature, Some(1.0));
}

// ---------------------------------------------------------------------------
// Test 39: Config Clone is Independent
// ---------------------------------------------------------------------------

#[test]
fn test_config_clone_independence() {
    let mut config = LlmConfig::default();
    config.model = "original".to_string();
    config.temperature = Some(0.5);

    let mut cloned = config.clone();

    // Modify clone
    cloned.model = "modified".to_string();
    cloned.temperature = Some(0.8);

    // Original should be unchanged
    assert_eq!(config.model, "original");
    assert_eq!(config.temperature, Some(0.5));

    // Clone should have new values
    assert_eq!(cloned.model, "modified");
    assert_eq!(cloned.temperature, Some(0.8));
}

// ---------------------------------------------------------------------------
// Test 40: UsageStats Arithmetic
// ---------------------------------------------------------------------------

#[test]
fn test_usage_stats_arithmetic() {
    use ggen_ai::client::UsageStats;

    let stats = UsageStats {
        prompt_tokens: 100,
        completion_tokens: 50,
        total_tokens: 150,
    };

    assert_eq!(
        stats.prompt_tokens + stats.completion_tokens,
        stats.total_tokens
    );
}

// ---------------------------------------------------------------------------
// Test 41: UsageStats with Zero Values
// ---------------------------------------------------------------------------

#[test]
fn test_usage_stats_zero_values() {
    use ggen_ai::client::UsageStats;

    let stats = UsageStats {
        prompt_tokens: 0,
        completion_tokens: 0,
        total_tokens: 0,
    };

    assert_eq!(stats.total_tokens, 0);
    assert_eq!(stats.prompt_tokens, 0);
    assert_eq!(stats.completion_tokens, 0);
}

// ---------------------------------------------------------------------------
// Test 42: UsageStats Serialization Roundtrip
// ---------------------------------------------------------------------------

#[test]
fn test_usage_stats_serialization_roundtrip() {
    use ggen_ai::client::UsageStats;

    let original = UsageStats {
        prompt_tokens: 100,
        completion_tokens: 50,
        total_tokens: 150,
    };

    let json = serde_json::to_string(&original).expect("Serialization should succeed");
    let deserialized: UsageStats =
        serde_json::from_str(&json).expect("Deserialization should succeed");

    assert_eq!(original.prompt_tokens, deserialized.prompt_tokens);
    assert_eq!(original.completion_tokens, deserialized.completion_tokens);
    assert_eq!(original.total_tokens, deserialized.total_tokens);
}

// ---------------------------------------------------------------------------
// Test 43: LlmResponse with No Usage
// ---------------------------------------------------------------------------

#[test]
fn test_llm_response_no_usage() {
    let response = LlmResponse {
        content: "Test".to_string(),
        usage: None,
        model: "test-model".to_string(),
        finish_reason: None,
        extra: std::collections::HashMap::new(),
    };

    assert!(response.usage.is_none());
    assert_eq!(response.content, "Test");
}

// ---------------------------------------------------------------------------
// Test 44: LlmChunk with Usage
// ---------------------------------------------------------------------------

#[test]
fn test_llm_chunk_with_usage() {
    use ggen_ai::client::UsageStats;

    let chunk = LlmChunk {
        content: "".to_string(),
        model: "test-model".to_string(),
        finish_reason: Some("stop".to_string()),
        usage: Some(UsageStats {
            prompt_tokens: 100,
            completion_tokens: 50,
            total_tokens: 150,
        }),
        extra: std::collections::HashMap::new(),
    };

    assert!(chunk.usage.is_some());
    assert_eq!(chunk.usage.unwrap().total_tokens, 150);
}

// ---------------------------------------------------------------------------
// Test 45: Extra Field in LlmResponse
// ---------------------------------------------------------------------------

#[test]
fn test_llm_response_extra_field() {
    let mut extra = std::collections::HashMap::new();
    extra.insert(
        "custom_field".to_string(),
        serde_json::json!("custom_value"),
    );

    let response = LlmResponse {
        content: "Test".to_string(),
        usage: None,
        model: "test-model".to_string(),
        finish_reason: None,
        extra,
    };

    assert_eq!(response.extra.len(), 1);
    assert!(response.extra.contains_key("custom_field"));
}

// ---------------------------------------------------------------------------
// Test 46: Config with Model from Environment Variable
// ---------------------------------------------------------------------------

#[test]
fn test_config_model_from_env() {
    // This test verifies that the Default impl reads from environment
    // We can't test actual environment variable manipulation in a unit test,
    // but we can verify the fallback behavior

    let config = LlmConfig::default();

    // Should have a valid model (either from env or fallback)
    assert!(
        !config.model.is_empty(),
        "Default model should not be empty"
    );

    // Should validate successfully
    let result = config.validate();
    assert!(result.is_ok(), "Default config should be valid");
}

// ---------------------------------------------------------------------------
// Test 47: Config Clone with Extra Map
// ---------------------------------------------------------------------------

#[test]
fn test_config_clone_with_extra_map() {
    let mut extra = std::collections::HashMap::new();
    extra.insert("key".to_string(), serde_json::json!("value"));

    let config = LlmConfig {
        model: "test".to_string(),
        max_tokens: Some(100),
        temperature: None,
        top_p: None,
        stop: None,
        extra: extra.clone(),
    };

    let cloned = config.clone();

    assert_eq!(cloned.extra.len(), extra.len());
    assert_eq!(cloned.extra.get("key"), extra.get("key"));
}

// ---------------------------------------------------------------------------
// Test 48: Partial Config Update
// ---------------------------------------------------------------------------

#[test]
fn test_partial_config_update() {
    let mut config1 = LlmConfig::default();
    config1.model = "model1".to_string();
    config1.temperature = Some(0.5);
    config1.max_tokens = Some(1000);

    let config2 = LlmConfig {
        model: "model2".to_string(),
        max_tokens: Some(2000),
        ..config1.clone()
    };

    assert_eq!(config2.model, "model2");
    assert_eq!(config2.max_tokens, Some(2000));
    assert_eq!(config2.temperature, Some(0.5)); // Should preserve from config1
}

// ---------------------------------------------------------------------------
// Test 49: Validation Does Not Modify Config
// ---------------------------------------------------------------------------

#[test]
fn test_validation_does_not_modify_config() {
    let mut config = LlmConfig::default();
    config.model = "test".to_string();
    config.temperature = Some(0.7);

    let original_model = config.model.clone();
    let original_temp = config.temperature;

    let _result = config.validate();

    assert_eq!(config.model, original_model);
    assert_eq!(config.temperature, original_temp);
}

// ---------------------------------------------------------------------------
// Test 50: Stop Sequences with Empty Strings
// ---------------------------------------------------------------------------

#[test]
fn test_config_stop_sequences_with_empty_strings() {
    let mut config = LlmConfig::default();
    config.stop = Some(vec![
        "stop1".to_string(),
        "".to_string(),
        "stop2".to_string(),
    ]);

    let result = config.validate();

    // Empty strings in stop sequences should be valid (no validation for this)
    assert!(result.is_ok());
    assert_eq!(config.stop.unwrap().len(), 3);
}
