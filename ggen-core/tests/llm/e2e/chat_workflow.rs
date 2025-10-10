//! End-to-End Chat Workflow Tests
//!
//! Tests complete user workflows from request to response.

use ggen_core::llm::{LlmClient, LlmConfig, Message, Role, ChatRequest};
use std::time::Duration;
use tokio::time::sleep;

/// Test basic chat request/response workflow
#[tokio::test]
async fn test_chat_request_response_workflow() {
    // Arrange: Setup client with mock provider
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_tokens: Some(100),
        temperature: Some(0.7),
        timeout: Some(Duration::from_secs(30)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send chat request
    let messages = vec![
        Message {
            role: Role::User,
            content: "Hello, how are you?".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        max_tokens: Some(50),
        temperature: Some(0.7),
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert: Verify response received
    assert!(response.is_ok(), "Chat request should succeed");
    let response = response.unwrap();
    assert!(!response.content.is_empty(), "Response should contain content");
    assert!(response.usage.is_some(), "Response should contain usage data");
}

/// Test chat workflow with conversation history
#[tokio::test]
async fn test_multi_turn_conversation_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");
    let mut conversation_history = Vec::new();

    // Act: Turn 1
    conversation_history.push(Message {
        role: Role::User,
        content: "What is 2+2?".to_string(),
    });

    let request1 = ChatRequest {
        messages: conversation_history.clone(),
        ..Default::default()
    };

    let response1 = client.chat(request1).await.expect("Turn 1 failed");
    conversation_history.push(Message {
        role: Role::Assistant,
        content: response1.content.clone(),
    });

    // Turn 2: Reference previous answer
    conversation_history.push(Message {
        role: Role::User,
        content: "Now multiply that by 3".to_string(),
    });

    let request2 = ChatRequest {
        messages: conversation_history.clone(),
        ..Default::default()
    };

    let response2 = client.chat(request2).await.expect("Turn 2 failed");

    // Assert: Both turns successful
    assert!(!response1.content.is_empty());
    assert!(!response2.content.is_empty());
    assert_eq!(conversation_history.len(), 4); // 2 user + 2 assistant
}

/// Test system message workflow
#[tokio::test]
async fn test_system_message_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Chat with system message
    let messages = vec![
        Message {
            role: Role::System,
            content: "You are a helpful coding assistant.".to_string(),
        },
        Message {
            role: Role::User,
            content: "Write a hello world function".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert
    assert!(response.is_ok(), "System message workflow should work");
    let response = response.unwrap();
    assert!(!response.content.is_empty());
}

/// Test parameter customization workflow
#[tokio::test]
async fn test_parameter_customization_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Request with custom parameters
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate creative text".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        max_tokens: Some(200),
        temperature: Some(0.9),
        top_p: Some(0.95),
        frequency_penalty: Some(0.5),
        presence_penalty: Some(0.5),
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert: Custom parameters accepted
    assert!(response.is_ok(), "Custom parameters should work");
    let response = response.unwrap();
    assert!(!response.content.is_empty());
}

/// Test empty message handling
#[tokio::test]
async fn test_empty_message_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send empty messages array
    let request = ChatRequest {
        messages: vec![],
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert: Should fail gracefully
    assert!(response.is_err(), "Empty messages should be rejected");
}

/// Test long conversation workflow
#[tokio::test]
async fn test_long_conversation_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Build long conversation
    let mut messages = vec![
        Message {
            role: Role::System,
            content: "You are a helpful assistant.".to_string(),
        }
    ];

    // Add 10 turns
    for i in 0..10 {
        messages.push(Message {
            role: Role::User,
            content: format!("Question {}", i + 1),
        });
        messages.push(Message {
            role: Role::Assistant,
            content: format!("Answer {}", i + 1),
        });
    }

    messages.push(Message {
        role: Role::User,
        content: "Final question".to_string(),
    });

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert: Long conversation handled
    assert!(response.is_ok(), "Long conversations should work");
}

/// Test concurrent requests workflow
#[tokio::test]
async fn test_concurrent_requests_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send 5 concurrent requests
    let mut handles = vec![];

    for i in 0..5 {
        let client_clone = client.clone();
        let handle = tokio::spawn(async move {
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

            client_clone.chat(request).await
        });
        handles.push(handle);
    }

    // Wait for all to complete
    let results: Vec<_> = futures::future::join_all(handles).await;

    // Assert: All succeeded
    for result in results {
        let response = result.expect("Task panicked").expect("Request failed");
        assert!(!response.content.is_empty());
    }
}

/// Test response metadata workflow
#[tokio::test]
async fn test_response_metadata_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act
    let messages = vec![
        Message {
            role: Role::User,
            content: "Hello".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let response = client.chat(request).await.expect("Request failed");

    // Assert: Verify metadata
    assert!(response.usage.is_some(), "Should include usage");
    if let Some(usage) = response.usage {
        assert!(usage.prompt_tokens > 0, "Should count prompt tokens");
        assert!(usage.completion_tokens > 0, "Should count completion tokens");
        assert_eq!(
            usage.total_tokens,
            usage.prompt_tokens + usage.completion_tokens
        );
    }

    assert!(response.model.is_some(), "Should include model name");
    assert!(response.finish_reason.is_some(), "Should include finish reason");
}
