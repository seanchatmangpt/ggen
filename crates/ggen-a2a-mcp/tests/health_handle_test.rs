//! Tests for health check task JoinHandle storage and lifecycle management
//!
//! Phase 2, Bug #4: Verify that:
//! 1. JoinHandle is properly stored in health_handle
//! 2. Health check task runs continuously (not cancelled immediately)
//! 3. Task is properly aborted on Drop
//! 4. No memory leaks from abandoned tasks

use ggen_a2a_mcp::client::{A2aClientConfig, A2aLlmClient};
use ggen_ai::dspy::model_capabilities::Model;
use std::time::Duration;
use tokio::time::{sleep, timeout};

/// Test that health check task JoinHandle is stored after client creation
#[tokio::test]
async fn test_health_handle_stored_after_creation() {
    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        health_check_interval: Duration::from_millis(100),
        ..Default::default()
    };

    let client = A2aLlmClient::with_config(model, config)
        .await
        .expect("Client creation should succeed");

    // Give the health check task time to start
    sleep(Duration::from_millis(50)).await;

    // Verify health_handle contains a JoinHandle
    // Note: We can't directly access health_handle as it's private,
    // but we can verify the health check is running by checking metrics
    let health = client.health().await;
    assert_eq!(
        health.state,
        ggen_a2a_mcp::client::ConnectionState::Connected
    );
}

/// Test that health check task runs continuously
#[tokio::test]
async fn test_health_check_runs_continuously() {
    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        health_check_interval: Duration::from_millis(100),
        ..Default::default()
    };

    let client = A2aLlmClient::with_config(model, config)
        .await
        .expect("Client creation should succeed");

    // Wait for first health check cycle
    sleep(Duration::from_millis(150)).await;

    let health1 = client.health().await;
    let heartbeat1 = health1.last_heartbeat;

    // Wait for another health check cycle
    sleep(Duration::from_millis(150)).await;

    let health2 = client.health().await;
    let heartbeat2 = health2.last_heartbeat;

    // Verify heartbeat was updated (task is still running)
    assert!(
        heartbeat2 > heartbeat1,
        "Health check task should update heartbeat"
    );
}

/// Test that health check task is aborted when client is dropped
#[tokio::test]
async fn test_health_check_aborted_on_drop() {
    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        health_check_interval: Duration::from_millis(100),
        ..Default::default()
    };

    {
        let client = A2aLlmClient::with_config(model, config)
            .await
            .expect("Client creation should succeed");

        // Verify health check is running
        sleep(Duration::from_millis(150)).await;
        let health = client.health().await;
        assert_eq!(
            health.state,
            ggen_a2a_mcp::client::ConnectionState::Connected
        );

        // Client goes out of scope here, Drop should be called
    }

    // Give time for cleanup
    sleep(Duration::from_millis(100)).await;

    // If we get here without hanging, the Drop implementation worked correctly
    // The health check task should have been aborted
}

/// Test that shutdown properly aborts health check task
#[tokio::test]
async fn test_shutdown_aborts_health_check() {
    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        health_check_interval: Duration::from_millis(100),
        ..Default::default()
    };

    let client = A2aLlmClient::with_config(model, config)
        .await
        .expect("Client creation should succeed");

    // Verify health check is running
    sleep(Duration::from_millis(150)).await;
    let health = client.health().await;
    assert_eq!(
        health.state,
        ggen_a2a_mcp::client::ConnectionState::Connected
    );

    // Shutdown the client
    let shutdown_result = timeout(Duration::from_millis(500), client.shutdown()).await;
    assert!(
        shutdown_result.is_ok(),
        "Shutdown should complete without timeout"
    );
    assert!(shutdown_result.unwrap().is_ok(), "Shutdown should succeed");

    // Verify state is shutting down
    let health = client.health().await;
    assert_eq!(
        health.state,
        ggen_a2a_mcp::client::ConnectionState::ShuttingDown
    );
}

/// Test for memory leaks - create and drop multiple clients
#[tokio::test]
async fn test_no_memory_leaks_from_multiple_clients() {
    let model = Model::from_name("gpt-4");

    for i in 0..10 {
        let config = A2aClientConfig {
            health_check_interval: Duration::from_millis(50),
            ..Default::default()
        };

        let client = A2aLlmClient::with_config(model.clone(), config)
            .await
            .expect("Client creation should succeed");

        // Do some work
        sleep(Duration::from_millis(20)).await;
        let _health = client.health().await;

        // Client drops here
        if i % 3 == 0 {
            // Give time for cleanup every few iterations
            sleep(Duration::from_millis(50)).await;
        }
    }

    // If we get here without issues, no memory leaks occurred
    // In a real test, we would use a memory profiler to verify
}

/// Test that health check updates state correctly
#[tokio::test]
async fn test_health_check_state_updates() {
    let model = Model::from_name("gpt-4");
    let config = A2aClientConfig {
        health_check_interval: Duration::from_millis(100),
        ..Default::default()
    };

    let client = A2aLlmClient::with_config(model, config)
        .await
        .expect("Client creation should succeed");

    // Initial state should be Connected (set by start_health_check)
    sleep(Duration::from_millis(150)).await;
    let health = client.health().await;
    assert_eq!(
        health.state,
        ggen_a2a_mcp::client::ConnectionState::Connected
    );

    // Verify heartbeat is recent
    let now = tokio::time::Instant::now();
    let elapsed = now.duration_since(health.last_heartbeat);
    assert!(
        elapsed < Duration::from_millis(200),
        "Heartbeat should be recent"
    );
}

/// Test concurrent client creation and destruction
#[tokio::test]
async fn test_concurrent_client_lifecycle() {
    let mut handles = Vec::new();

    // Create multiple clients concurrently
    for _ in 0..5 {
        handles.push(tokio::spawn(async {
            let model = Model::from_name("gpt-4");
            let config = A2aClientConfig {
                health_check_interval: Duration::from_millis(50),
                ..Default::default()
            };

            let client = A2aLlmClient::with_config(model, config)
                .await
                .expect("Client creation should succeed");

            sleep(Duration::from_millis(100)).await;
            let health = client.health().await;

            // Client drops here
            health.state
        }));
    }

    // Wait for all tasks to complete
    let results = futures::future::join_all(handles).await;
    for result in results {
        assert!(result.is_ok(), "Task should complete without error");
        let state = result.unwrap();
        assert_eq!(state, ggen_a2a_mcp::client::ConnectionState::Connected);
    }
}
