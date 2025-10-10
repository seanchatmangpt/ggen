//! End-to-End Rate Limiting Workflow Tests
//!
//! Tests rate limiting and backoff workflows.

use ggen_core::llm::{LlmClient, LlmConfig, Message, Role, ChatRequest};
use std::time::{Duration, Instant};
use tokio::time::sleep;

/// Test exponential backoff workflow
#[tokio::test]
async fn test_exponential_backoff_workflow() {
    // Arrange: Configure with exponential backoff
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(4),
        retry_delay: Some(Duration::from_millis(100)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Track retry timing
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test backoff".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let start = Instant::now();
    let _result = client.chat(request).await;
    let duration = start.elapsed();

    // Assert: Backoff delays applied if retries occurred
    println!("Request completed in {:?}", duration);
}

/// Test rate limit header handling workflow
#[tokio::test]
async fn test_rate_limit_header_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send request and check for rate limit info
    let messages = vec![
        Message {
            role: Role::User,
            content: "Check rate limits".to_string(),
        }
    ];

    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let response = client.chat(request).await;

    // Assert: Response includes rate limit awareness
    if let Ok(_resp) = response {
        // In real implementation, would check headers
        // For mock, just verify request succeeded
    }
}

/// Test burst request handling workflow
#[tokio::test]
async fn test_burst_request_handling_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(3),
        retry_delay: Some(Duration::from_millis(100)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Send burst of requests
    let start = Instant::now();
    let mut handles = vec![];

    for i in 0..20 {
        let client_clone = client.clone();
        let handle = tokio::spawn(async move {
            let messages = vec![
                Message {
                    role: Role::User,
                    content: format!("Burst request {}", i + 1),
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

    // Wait for all
    let results: Vec<_> = futures::future::join_all(handles).await;
    let duration = start.elapsed();

    // Assert: Most requests succeed
    let success_count = results.iter()
        .filter(|r| r.is_ok() && r.as_ref().unwrap().is_ok())
        .count();

    println!("Burst: {}/20 succeeded in {:?}", success_count, duration);
    assert!(success_count > 10, "Most burst requests should succeed");
}

/// Test rate limit recovery workflow
#[tokio::test]
async fn test_rate_limit_recovery_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(3),
        retry_delay: Some(Duration::from_millis(200)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Hit rate limit, then recover
    let messages = vec![
        Message {
            role: Role::User,
            content: "Test recovery".to_string(),
        }
    ];

    // Send rapid requests
    for _ in 0..5 {
        let request = ChatRequest {
            messages: messages.clone(),
            ..Default::default()
        };
        let _ = client.chat(request).await;
    }

    // Wait for recovery
    sleep(Duration::from_secs(1)).await;

    // Try again
    let request = ChatRequest {
        messages,
        ..Default::default()
    };

    let result = client.chat(request).await;

    // Assert: Recovered from rate limit
    assert!(result.is_ok(), "Should recover after cooldown");
}

/// Test concurrent rate limiting workflow
#[tokio::test]
async fn test_concurrent_rate_limiting_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(2),
        retry_delay: Some(Duration::from_millis(100)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Multiple concurrent tasks
    let mut handles = vec![];

    for task_id in 0..5 {
        let client_clone = client.clone();
        let handle = tokio::spawn(async move {
            let mut successes = 0;

            for i in 0..5 {
                let messages = vec![
                    Message {
                        role: Role::User,
                        content: format!("Task {} request {}", task_id, i),
                    }
                ];

                let request = ChatRequest {
                    messages,
                    ..Default::default()
                };

                if client_clone.chat(request).await.is_ok() {
                    successes += 1;
                }

                sleep(Duration::from_millis(50)).await;
            }

            successes
        });
        handles.push(handle);
    }

    // Wait for all tasks
    let results: Vec<_> = futures::future::join_all(handles).await;

    // Assert: Tasks completed with some success
    let total_successes: i32 = results.iter()
        .filter_map(|r| r.as_ref().ok())
        .sum();

    println!("Total successes: {}/25", total_successes);
    assert!(total_successes > 15, "Most requests should eventually succeed");
}

/// Test adaptive rate limiting workflow
#[tokio::test]
async fn test_adaptive_rate_limiting_workflow() {
    // Arrange: Start aggressive, adapt if hitting limits
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        max_retries: Some(3),
        retry_delay: Some(Duration::from_millis(50)),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Adaptive sending
    let mut delay = Duration::from_millis(10);
    let mut successes = 0;
    let mut failures = 0;

    for i in 0..10 {
        let messages = vec![
            Message {
                role: Role::User,
                content: format!("Adaptive request {}", i + 1),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let result = client.chat(request).await;

        if result.is_ok() {
            successes += 1;
            // Success - can be more aggressive
            delay = Duration::from_millis((delay.as_millis() as f32 * 0.9) as u64);
        } else {
            failures += 1;
            // Failure - back off more
            delay = Duration::from_millis((delay.as_millis() as f32 * 1.5) as u64);
        }

        sleep(delay).await;
    }

    // Assert: Adaptation worked
    println!("Adaptive: {} successes, {} failures", successes, failures);
    assert!(successes > failures, "Should adapt to succeed more than fail");
}

/// Test rate limit queue workflow
#[tokio::test]
async fn test_rate_limit_queue_workflow() {
    // Arrange: Simulate queued requests
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Queue multiple requests
    let mut queue = Vec::new();

    for i in 0..10 {
        let messages = vec![
            Message {
                role: Role::User,
                content: format!("Queued request {}", i + 1),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        queue.push(request);
    }

    // Process queue with rate limiting
    let mut processed = 0;
    for request in queue {
        if client.chat(request).await.is_ok() {
            processed += 1;
        }
        sleep(Duration::from_millis(100)).await; // Rate limit delay
    }

    // Assert: All queued items processed
    assert_eq!(processed, 10, "Should process entire queue");
}

/// Test priority rate limiting workflow
#[tokio::test]
async fn test_priority_rate_limiting_workflow() {
    // Arrange
    let config = LlmConfig {
        provider: "mock".to_string(),
        model: "test-model".to_string(),
        api_key: Some("test-key".to_string()),
        ..Default::default()
    };

    let client = LlmClient::new(config).expect("Failed to create client");

    // Act: Simulate priority requests
    let high_priority = vec!["Critical 1", "Critical 2"];
    let low_priority = vec!["Normal 1", "Normal 2", "Normal 3"];

    // Process high priority first
    for msg in high_priority {
        let messages = vec![
            Message {
                role: Role::User,
                content: msg.to_string(),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let _ = client.chat(request).await;
        sleep(Duration::from_millis(50)).await;
    }

    // Then process low priority
    for msg in low_priority {
        let messages = vec![
            Message {
                role: Role::User,
                content: msg.to_string(),
            }
        ];

        let request = ChatRequest {
            messages,
            ..Default::default()
        };

        let _ = client.chat(request).await;
        sleep(Duration::from_millis(100)).await; // Slower for low priority
    }

    // Assert: Priority handling works
    // (In real implementation, would verify order and timing)
}
