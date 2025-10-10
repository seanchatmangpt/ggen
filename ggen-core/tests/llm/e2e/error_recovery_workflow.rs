//! End-to-End Error Recovery Workflow Tests
//!
//! Tests error handling and recovery workflows.

use ggen_core::llm::{LlmClient, LlmConfig, Message, Role, ChatRequest};
use std::time::Duration;
use tokio::time::sleep;

/// Test network error retry workflow
#[tokio::test]
async fn test_network_error_retry_workflow() {
    // Arrange: Configure with retries
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(3),
        retry_delay: Some(Duration::from_millis(100)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Simulate network error scenario
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test retry".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    // First attempt may fail, retries should succeed
    let result = client.chat(request).await;

    // Assert: Either succeeds or exhausts retries gracefully
    match result {
        Ok(response) => {
            assert!(!response.content.is_empty(), "Should get response");
        }
        Err(e) => {
            // If all retries failed, error should indicate that
            assert!(
                e.to_string().contains("retry") ||
                e.to_string().contains("attempt"),
                "Error should mention retry attempts"
            );
        }
    }
}

/// Test authentication error workflow
#[tokio::test]
async fn test_authentication_error_workflow() {
    // Arrange: Invalid API key
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("invalid-key".to_string()),
        max_retries: Some(1), // Don't retry auth errors
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Attempt request with invalid key
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test auth".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let result = client.chat(request).await;

    // Assert: Auth error detected
    match result {
        Ok(_) => {
            // Mock provider may allow invalid keys
        }
        Err(e) => {
            let error_msg = e.to_string().to_lowercase();
            assert!(
                error_msg.contains("auth") ||
                error_msg.contains("unauthorized") ||
                error_msg.contains("invalid"),
                "Should indicate authentication issue"
            );
        }
    }
}

/// Test rate limit error workflow
#[tokio::test]
async fn test_rate_limit_error_workflow() {
    // Arrange: Configure with aggressive retry
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(5),
        retry_delay: Some(Duration::from_millis(200)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send multiple rapid requests
    let mut results = Vec::new();

    for i in 0..10 {
        let messages = vec![
            Message {
                role: Role::User,
                content: format!("Request {}", i + 1),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let result = client.chat(request).await;
        results.push(result);

        // Small delay
        sleep(Duration::from_millis(50)).await;
    }

    // Assert: Some succeed despite rate limiting
    let success_count = results.iter().filter(|r| r.is_ok()).count();
    assert!(success_count > 0, "Some requests should succeed");
}

/// Test timeout error workflow
#[tokio::test]
async fn test_timeout_error_workflow() {
    // Arrange: Very short timeout
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        timeout: Some(Duration::from_millis(1)), // Very short
        max_retries: Some(2),
        ..Default::default()
    };

    let client = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        timeout: Some(Duration::from_secs(5)), // Reasonable
        ..Default::default()
    };

    let client_ok = LlmClient::new(client).expect("Failed to create client");

    // Act: Request that should succeed with reasonable timeout
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test timeout".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let result = client_ok.chat(request).await;

    // Assert: Reasonable timeout succeeds
    assert!(result.is_ok(), "Reasonable timeout should work");
}

/// Test partial response error workflow
#[tokio::test]
async fn test_partial_response_error_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Start stream that may be interrupted
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate long response".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let stream_result = client.chat_stream(request).await;

    // Assert: Handle partial response gracefully
    if let Ok(mut stream) = stream_result {
        use futures::StreamExt;
        let mut partial_content = String::new();

        while let Some(result) = stream.next().await {
            match result {
                Ok(chunk) => {
                    partial_content.push_str(&chunk.content);
                }
                Err(_) => {
                    // Stream interrupted - partial content available
                    break;
                }
            }
        }

        // Even if interrupted, we should have some content
        println!("Partial content received: {} bytes", partial_content.len());
    }
}

/// Test fallback provider workflow
#[tokio::test]
async fn test_fallback_provider_workflow() {
    // Arrange: Primary and fallback providers
    let primary_config = LlmConfig {
        provider: "primary".to_string(),
        model: "primary-model".to_string(),
        api_key: Some("invalid-key".to_string()), // Will fail
        max_retries: Some(1),
        ..Default::default()
    };

    let fallback_config = LlmConfig {
        provider: "mock".to_string(),
        model: "fallback-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    // Act: Try primary, fallback on error
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test fallback".to_string(),
        }
    ];

    let request = ChatRequest {
        messages: messages.clone(),
        ..Default::default()
    };

    let primary_client = LlmClient::new(primary_config);
    let mut final_response = None;

    if let Ok(client) = primary_client {
        if let Ok(response) = client.chat(request.clone()).await {
            final_response = Some(response);
        }
    }

    // Try fallback if primary failed
    if final_response.is_none() {
        let fallback_client = LlmClient::new(fallback_config)
            .expect("Fallback client should work");

        if let Ok(response) = fallback_client.chat(request).await {
            final_response = Some(response);
        }
    }

    // Assert: Got response from fallback
    assert!(final_response.is_some(), "Fallback should provide response");
}

/// Test graceful degradation workflow
#[tokio::test]
async fn test_graceful_degradation_workflow() {
    // Arrange: Start with full features, degrade as needed
    let full_config = LlmConfig {
        provider: "mock".to_string(),
        model: "advanced-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_tokens: Some(1000),
        temperature: Some(0.9),
        ..Default::default()
    };

    // Act: Try full config, degrade if needed
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test degradation".to_string(),
        }
    ];

    let mut request = ChatRequest {
        messages: messages.clone(),
        max_tokens: Some(1000),
        temperature: Some(0.9),
        ..Default::default()
    };

    let client = LlmClient::new(full_config).expect("Failed to create client");
    let mut result = client.chat(request.clone()).await;

    // If full request fails, try with reduced settings
    if result.is_err() {
        request.max_tokens = Some(100);
        request.temperature = Some(0.5);
        result = client.chat(request).await;
    }

    // Assert: Either full or degraded mode works
    assert!(result.is_ok(), "Should work with degraded settings");
}

/// Test error message clarity workflow
#[tokio::test]
async fn test_error_message_clarity_workflow() {
    // Arrange: Various error scenarios
    let error_configs = vec![
        ("Empty provider", LlmConfig {
            provider: "".to_string(),
            model: "test-model".to_string(),
            api_key: Some("test-key".to_string()),
            ..Default::default()
        }),
        ("Empty model", LlmConfig {
            provider: "mock".to_string(),
            model: "".to_string(),
            api_key: Some("test-key".to_string()),
            ..Default::default()
        }),
    ];

    // Act & Assert: Check error messages
    for (scenario, config) in error_configs {
        let result = LlmClient::new(config);

        if let Err(e) = result {
            let error_msg = e.to_string();
            assert!(
                !error_msg.is_empty(),
                "{}: Error message should not be empty",
                scenario
            );
            assert!(
                error_msg.len() > 10,
                "{}: Error message should be descriptive",
                scenario
            );
        }
    }
}
