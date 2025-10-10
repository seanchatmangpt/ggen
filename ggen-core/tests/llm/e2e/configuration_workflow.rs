//! End-to-End Configuration Workflow Tests
//!
//! Tests configuration changes and provider switching workflows.

use ggen_core::llm::{LlmClient, LlmConfig, Message, Role, ChatRequest};
use std::time::Duration;

/// Test provider switching workflow
#[tokio::test]
async fn test_provider_switching_workflow() {
    // Arrange: Start with mock provider
    let config1 = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client1 = LlmClient::new(config1).expect("Failed to create mock client");

    // Act: Switch to different provider
    let config2 = LlmConfig {
        provider: "mock2".to_string(),
        model: "different-model".to_string(),
        api_key: Some("different-key".to_string()),
        ..Default::default()
    };

    let client2 = LlmClient::new(config2).expect("Failed to create second client");

    // Test both clients
    let messages = vec![
        Message {
            role: Role::User,
            content: "Hello".to_string(),
        }
    ];

    let request = ChatRequest {
        messages: messages.clone(),
        ..Default::default()
    };

    let response1 = client1.chat(request.clone()).await;
    let response2 = client2.chat(request).await;

    // Assert: Both providers work
    assert!(response1.is_ok(), "First provider should work");
    assert!(response2.is_ok(), "Second provider should work");
}

/// Test configuration update workflow
#[tokio::test]
async fn test_configuration_update_workflow() {
    // Arrange: Start with basic config
    let mut config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        temperature: Some(0.5),
        max_tokens: Some(100),
        ..Default::default()
    };

    let client1 = LlmClient::new(config.clone()).expect("Failed to create client");

    // Act: Update configuration
    config.temperature = Some(0.9);
    config.max_tokens = Some(200);

    let client2 = LlmClient::new(config).expect("Failed to create updated client");

    // Test with updated config
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate creative text".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let response = client2.chat(request).await;

    // Assert: Updated config works
    assert!(response.is_ok(), "Updated configuration should work");
}

/// Test model switching workflow
#[tokio::test]
async fn test_model_switching_workflow() {
    // Arrange: Test multiple models
    let models = vec!["model-a", "model-b", "model-c"];
    let mut responses = Vec::new();

    // Act: Switch between models
    for model in models {
        let config = LlmConfig {
            provider: "mock".to_string(),
            model: model.to_string(),
            api_key: Some("test-key".to_string()),
            ..Default::default()
        };

        let client = LlmClient::new(config).expect("Failed to create client");

        let messages = vec![
            Message {
                role: Role::User,
                content: "Test".to_string(),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let response = client.chat(request).await;
        responses.push(response);
    }

    // Assert: All models work
    for response in responses {
        assert!(response.is_ok(), "Each model should work");
    }
}

/// Test timeout configuration workflow
#[tokio::test]
async fn test_timeout_configuration_workflow() {
    // Arrange: Configure different timeouts
    let short_timeout = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        timeout: Some(Duration::from_millis(100)),
        ..Default::default()
    };

    let long_timeout = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        timeout: Some(Duration::from_secs(60)),
        ..Default::default()
    };

    let client_short = LlmClient::new(short_timeout).expect("Failed to create short client");
    let client_long = LlmClient::new(long_timeout).expect("Failed to create long client");

    // Act: Test both clients
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test timeout".to_string(),
        }
    ];

    let request = ChatRequest {
        messages: messages.clone(),
        ..Default::default()
    };

    let _response_short = client_short.chat(request.clone()).await;
    let response_long = client_long.chat(request).await;

    // Assert: Different timeouts handled
    assert!(response_long.is_ok(), "Long timeout should work");
}

/// Test API key rotation workflow
#[tokio::test]
async fn test_api_key_rotation_workflow() {
    // Arrange: Simulate key rotation
    let keys = vec!["key-1", "key-2", "key-3"];
    let mut success_count = 0;

    // Act: Rotate through keys
    for key in keys {
        let config = LlmConfig {
            provider: "mock".to_string(),
            model: "test-model".to_string(),
            api_key: Some(key.to_string()),
            ..Default::default()
        };

        if let Ok(client) = LlmClient::new(config) {
            let messages = vec![
                Message {
                    role: Role::User,
                    content: "Test".to_string(),
                }
            ];

            let request = ChatRequest {
                messages,
                ..Default::default()
            };

            if client.chat(request).await.is_ok() {
                success_count += 1;
            }
        }
    }

    // Assert: Key rotation works
    assert!(success_count > 0, "At least one key should work");
}

/// Test environment-based configuration workflow
#[tokio::test]
async fn test_environment_configuration_workflow() {
    // Arrange: Simulate different environments
    let environments = vec![
        ("development", "dev-model", 0.7),
        ("staging", "staging-model", 0.5),
        ("production", "prod-model", 0.3),
    ];

    // Act: Test each environment config
    for (env, model, temp) in environments {
        let config = LlmConfig {
            provider: "mock".to_string(),
            model: model.to_string(),
            api_key: Some(format!("{}-key", env)),
            temperature: Some(temp),
            ..Default::default()
        };

        let client = LlmClient::new(config).expect("Failed to create client");

        let messages = vec![
            Message {
                role: Role::User,
                content: format!("Test {}", env),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let response = client.chat(request).await;

        // Assert: Environment config works
        assert!(response.is_ok(), "Environment {} should work", env);
    }
}

/// Test feature flag configuration workflow
#[tokio::test]
async fn test_feature_flag_configuration_workflow() {
    // Arrange: Test different feature combinations
    let configs = vec![
        LlmConfig {
            provider: "mock".to_string(),
            model: "test-model".to_string(),
            api_key: Some("test-key".to_string()),
            stream: false,
            ..Default::default()
        },
        LlmConfig {
            provider: "mock".to_string(),
            model: "test-model".to_string(),
            api_key: Some("test-key".to_string()),
            stream: true,
            ..Default::default()
        },
    ];

    // Act: Test each configuration
    for config in configs {
        let client = LlmClient::new(config.clone()).expect("Failed to create client");

        let messages = vec![
            Message {
                role: Role::User,
                content: "Test features".to_string(),
            }
        ];

        let request = ChatRequest {
            messages,
            stream: config.stream,
            ..Default::default()
        };

        if config.stream {
            let stream_result = client.chat_stream(request).await;
            assert!(stream_result.is_ok(), "Streaming should work when enabled");
        } else {
            let response = client.chat(request).await;
            assert!(response.is_ok(), "Regular chat should work");
        }
    }
}

/// Test configuration validation workflow
#[tokio::test]
async fn test_configuration_validation_workflow() {
    // Arrange: Test invalid configurations
    let invalid_configs = vec![
        LlmConfig {
            provider: "".to_string(), // Empty provider
            model: "test-model".to_string(),
            api_key: Some("test-key".to_string()),
            ..Default::default()
        },
        LlmConfig {
            provider: "mock".to_string(),
            model: "".to_string(), // Empty model
            api_key: Some("test-key".to_string()),
            ..Default::default()
        },
    ];

    // Act & Assert: Invalid configs rejected
    for config in invalid_configs {
        let result = LlmClient::new(config);
        assert!(result.is_err(), "Invalid configuration should be rejected");
    }
}
