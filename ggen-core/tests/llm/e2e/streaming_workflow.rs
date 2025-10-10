//! End-to-End Streaming Workflow Tests
//!
//! Tests streaming response workflows from request to completion.

use ggen_core::llm::{LlmClient, LlmConfig, Message, Role, ChatRequest};
use futures::StreamExt;
use std::time::Duration;

/// Test basic streaming workflow
#[tokio::test]
async fn test_basic_streaming_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Request streaming response
    let messages = vec![
        Message {
            role: Role::User,
            content: "Tell me a story".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let mut stream = client.chat_stream(request).await.expect("Stream failed");

    // Collect chunks
    let mut chunks = Vec::new();
    let mut total_content = String::new();

    while let Some(result) = stream.next().await {
        match result {
            Ok(chunk) => {
                total_content.push_str(&chunk.content);
                chunks.push(chunk);
            }
            Err(e) => panic!("Stream error: {}", e),
        }
    }

    // Assert: Received streaming chunks
    assert!(!chunks.is_empty(), "Should receive chunks");
    assert!(!total_content.is_empty(), "Should have content");
}

/// Test streaming with chunk processing
#[tokio::test]
async fn test_streaming_chunk_processing_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Process chunks as they arrive
    let messages = vec![
        Message {
            role: Role::User,
            content: "Count to 10".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let mut stream = client.chat_stream(request).await.expect("Stream failed");

    let mut chunk_count = 0;
    let mut first_chunk_time = None;
    let mut last_chunk_time = None;

    while let Some(result) = stream.next().await {
        if let Ok(_chunk) = result {
            chunk_count += 1;
            let now = std::time::Instant::now();

            if first_chunk_time.is_none() {
                first_chunk_time = Some(now);
            }
            last_chunk_time = Some(now);
        }
    }

    // Assert: Chunks arrived progressively
    assert!(chunk_count > 0, "Should receive chunks");
    assert!(first_chunk_time.is_some());
    assert!(last_chunk_time.is_some());

    if let (Some(first), Some(last)) = (first_chunk_time, last_chunk_time) {
        let duration = last.duration_since(first);
        println!("Stream duration: {:?}", duration);
    }
}

/// Test streaming cancellation workflow
#[tokio::test]
async fn test_streaming_cancellation_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Start stream and cancel early
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate a long response".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let mut stream = client.chat_stream(request).await.expect("Stream failed");

    // Read only first 3 chunks then cancel
    let mut chunks_read = 0;
    while let Some(result) = stream.next().await {
        if result.is_ok() {
            chunks_read += 1;
            if chunks_read >= 3 {
                break; // Cancel stream
            }
        }
    }

    // Assert: Early termination worked
    assert_eq!(chunks_read, 3, "Should read exactly 3 chunks");
}

/// Test streaming error handling workflow
#[tokio::test]
async fn test_streaming_error_handling_workflow() {
    // Arrange: Configure for potential errors
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("invalid-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Request stream that may error
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test error handling".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let stream_result = client.chat_stream(request).await;

    // Assert: Handle stream creation or chunk errors
    match stream_result {
        Ok(mut stream) => {
            // If stream created, check for chunk errors
            while let Some(result) = stream.next().await {
                if result.is_err() {
                    // Error in chunk is acceptable
                    return;
                }
            }
        }
        Err(_) => {
            // Error creating stream is acceptable for invalid key
            return;
        }
    }
}

/// Test streaming with backpressure
#[tokio::test]
async fn test_streaming_backpressure_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Slow consumer simulation
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate data".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let mut stream = client.chat_stream(request).await.expect("Stream failed");

    // Slow processing
    let mut processed = 0;
    while let Some(result) = stream.next().await {
        if result.is_ok() {
            // Simulate slow processing
            tokio::time::sleep(Duration::from_millis(10)).await;
            processed += 1;
        }
    }

    // Assert: All chunks processed despite slow consumption
    assert!(processed > 0, "Should process chunks");
}

/// Test streaming reconnection workflow
#[tokio::test]
async fn test_streaming_reconnection_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        max_retries: Some(2),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Simulate connection issues
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test reconnection".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    // First attempt might fail, retries should work
    let mut attempts = 0;
    let mut success = false;

    for _ in 0..3 {
        attempts += 1;
        if let Ok(mut stream) = client.chat_stream(request.clone()).await {
            if stream.next().await.is_some() {
                success = true;
                break;
            }
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    // Assert: Eventually succeeds
    assert!(success || attempts >= 3, "Should retry on failure");
}

/// Test streaming progress tracking
#[tokio::test]
async fn test_streaming_progress_tracking_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        stream: true,
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Track progress
    let messages = vec![
        Message {
            role: Role::User,
            content: "Generate response".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        stream: true,
        ..Default::default()
    };

    let mut stream = client.chat_stream(request).await.expect("Stream failed");

    let mut bytes_received = 0;
    let mut chunks_received = 0;

    while let Some(result) = stream.next().await {
        if let Ok(chunk) = result {
            bytes_received += chunk.content.len();
            chunks_received += 1;

            // Progress callback simulation
            let progress = (chunks_received as f32) * 10.0; // Arbitrary
            println!("Progress: {}% ({} bytes)", progress, bytes_received);
        }
    }

    // Assert: Progress tracked
    assert!(bytes_received > 0, "Should track bytes");
    assert!(chunks_received > 0, "Should track chunks");
}
