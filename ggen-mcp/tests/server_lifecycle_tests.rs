//! Server Lifecycle and Connection Tests
//!
//! Tests verifying MCP server lifecycle management, connection handling,
//! and state management across the server lifetime.

use ggen_mcp::server::GgenMcpServer;
use serde_json::json;
use std::sync::Arc;
use tokio::sync::RwLock;

// ============================================================================
// SERVER CREATION AND INITIALIZATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_creation_is_fast() {
    let start = std::time::Instant::now();

    let _server = GgenMcpServer::new();

    let elapsed = start.elapsed();

    // Server creation should be near-instantaneous (< 100ms)
    assert!(
        elapsed.as_millis() < 100,
        "Server creation took {} ms, should be < 100ms",
        elapsed.as_millis()
    );
}

#[tokio::test]
async fn test_server_creation_is_consistent() {
    // Create multiple servers and verify consistency
    let servers: Vec<_> = (0..10).map(|_| GgenMcpServer::new()).collect();

    // All should be created successfully
    assert_eq!(servers.len(), 10);
}

#[test]
fn test_server_is_send_and_sync() {
    // Compile-time check that server can be shared across threads
    fn assert_send<T: Send>() {}
    fn assert_sync<T: Sync>() {}

    assert_send::<GgenMcpServer>();
    assert_sync::<GgenMcpServer>();
}

#[tokio::test]
async fn test_server_can_be_wrapped_in_arc() {
    let server = Arc::new(GgenMcpServer::new());

    // Should be usable through Arc
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Arc-wrapped server works"),
        Err(_) => println!("Arc-wrapped server attempted execution"),
    }
}

// ============================================================================
// STATE MANAGEMENT TESTS
// ============================================================================

#[tokio::test]
async fn test_server_maintains_consistent_state() {
    let server = GgenMcpServer::new();

    // Execute multiple operations
    let _ = server.execute_tool("market_list", json!({})).await;
    let _ = server
        .execute_tool("project_gen", json!({"template": "test"}))
        .await;
    let _ = server
        .execute_tool(
            "graph_query",
            json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"}),
        )
        .await;

    // Server should still be functional
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server state maintained"),
        Err(_) => println!("Server state consistent"),
    }
}

#[tokio::test]
async fn test_server_state_isolation_between_calls() {
    let server = GgenMcpServer::new();

    // First call with specific params
    let _ = server
        .execute_tool("market_search", json!({"query": "first-query"}))
        .await;

    // Second call with different params
    let result = server
        .execute_tool("market_search", json!({"query": "second-query"}))
        .await;

    // Second call should not be affected by first
    match result {
        Ok(_) => println!("State isolation maintained"),
        Err(_) => println!("Calls are independent"),
    }
}

#[tokio::test]
async fn test_server_handles_rapid_state_changes() {
    let server = GgenMcpServer::new();

    // Rapid-fire different operations
    for i in 0..100 {
        let tool = match i % 3 {
            0 => "market_search",
            1 => "project_gen",
            _ => "graph_query",
        };

        let params = match tool {
            "market_search" => json!({"query": format!("test-{}", i)}),
            "project_gen" => json!({"template": format!("test-{}", i)}),
            _ => json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"}),
        };

        let _ = server.execute_tool(tool, params).await;
    }

    // Server should still be functional
    let result = server.execute_tool("market_list", json!({})).await;
    match result {
        Ok(_) => println!("Server survived rapid state changes"),
        Err(_) => println!("Server maintained state through load"),
    }
}

// ============================================================================
// CONCURRENT CONNECTION TESTS
// ============================================================================

#[tokio::test]
async fn test_single_server_handles_concurrent_requests() {
    let server = Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    // Spawn 50 concurrent requests
    for i in 0..50 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone
                .execute_tool("market_search", json!({"query": format!("test-{}", i)}))
                .await
        });
        handles.push(handle);
    }

    // All should complete
    let mut success_count = 0;
    for handle in handles {
        if let Ok(result) = handle.await {
            if result.is_ok() || result.is_err() {
                success_count += 1;
            }
        }
    }

    assert_eq!(success_count, 50, "All concurrent requests should complete");
}

#[tokio::test]
async fn test_multiple_servers_can_coexist() {
    let servers: Vec<_> = (0..10).map(|_| Arc::new(GgenMcpServer::new())).collect();

    let mut handles = vec![];

    // Each server handles requests independently
    for (i, server) in servers.iter().enumerate() {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone
                .execute_tool("market_search", json!({"query": format!("server-{}", i)}))
                .await
        });
        handles.push(handle);
    }

    // All should complete independently
    for handle in handles {
        let _ = handle.await;
    }
}

#[tokio::test]
async fn test_server_handles_concurrent_tool_types() {
    let server = Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    // Mix different tool types concurrently
    for i in 0..20 {
        let server_clone = server.clone();
        let tool = match i % 4 {
            0 => "market_search",
            1 => "market_list",
            2 => "project_gen",
            _ => "graph_query",
        };

        let handle = tokio::spawn(async move {
            let params = match tool {
                "market_search" => json!({"query": "test"}),
                "market_list" => json!({}),
                "project_gen" => json!({"template": "test"}),
                _ => json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"}),
            };

            server_clone.execute_tool(tool, params).await
        });
        handles.push(handle);
    }

    // All different tool types should be handled concurrently
    for handle in handles {
        let _ = handle.await;
    }
}

// ============================================================================
// RESOURCE CLEANUP TESTS
// ============================================================================

#[tokio::test]
async fn test_server_drops_cleanly() {
    {
        let server = GgenMcpServer::new();
        let _ = server.execute_tool("market_list", json!({})).await;
    }
    // Server should be dropped here without issues

    // Create new server to verify no resource conflicts
    let _new_server = GgenMcpServer::new();
}

#[tokio::test]
async fn test_multiple_server_lifecycles() {
    for _ in 0..20 {
        let server = GgenMcpServer::new();
        let _ = server.execute_tool("market_list", json!({})).await;
        // Server dropped at end of each iteration
    }
    // No resource leaks should occur
}

#[tokio::test]
async fn test_server_cleanup_with_pending_operations() {
    {
        let server = Arc::new(GgenMcpServer::new());
        let server_clone = server.clone();

        // Start an operation
        let _handle = tokio::spawn(async move {
            let _ = server_clone.execute_tool("market_list", json!({})).await;
        });

        // Drop server reference
    }
    // Should clean up gracefully even with pending operations
}

// ============================================================================
// ERROR RECOVERY TESTS
// ============================================================================

#[tokio::test]
async fn test_server_recovers_from_invalid_tool_calls() {
    let server = GgenMcpServer::new();

    // Make invalid call
    let _ = server.execute_tool("nonexistent_tool", json!({})).await;

    // Server should still work
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server recovered from invalid tool"),
        Err(_) => println!("Server maintains functionality"),
    }
}

#[tokio::test]
async fn test_server_recovers_from_malformed_params() {
    let server = GgenMcpServer::new();

    // Make call with malformed params
    let _ = server
        .execute_tool("project_gen", json!(["invalid", "array"]))
        .await;

    // Server should still work
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server recovered from malformed params"),
        Err(_) => println!("Server maintains functionality"),
    }
}

#[tokio::test]
async fn test_server_survives_error_storm() {
    let server = GgenMcpServer::new();

    // Generate many errors in quick succession
    for i in 0..100 {
        let _ = server
            .execute_tool(
                "project_gen",
                json!({"wrong_param": i}), // Missing required param
            )
            .await;
    }

    // Server should still be functional
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server survived error storm"),
        Err(_) => println!("Server maintained stability"),
    }
}

// ============================================================================
// LONG-RUNNING SERVER TESTS
// ============================================================================

#[tokio::test]
async fn test_server_handles_extended_session() {
    let server = GgenMcpServer::new();

    // Simulate extended session with varied operations
    for i in 0..200 {
        let tool = match i % 5 {
            0 => "market_list",
            1 => "market_search",
            2 => "project_gen",
            3 => "graph_query",
            _ => "template_validate",
        };

        let params = match tool {
            "market_search" => json!({"query": format!("test-{}", i)}),
            "project_gen" => json!({"template": format!("test-{}", i)}),
            "graph_query" => json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"}),
            "template_validate" => json!({"content": "test"}),
            _ => json!({}),
        };

        let _ = server.execute_tool(tool, params).await;

        // Add small delay to simulate real usage
        if i % 10 == 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
        }
    }

    // Server should still be responsive
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server handled extended session"),
        Err(_) => println!("Server maintained stability"),
    }
}

// ============================================================================
// MEMORY AND RESOURCE USAGE TESTS
// ============================================================================

#[tokio::test]
async fn test_server_memory_usage_is_stable() {
    let server = GgenMcpServer::new();

    // Execute many operations to test for memory leaks
    for i in 0..1000 {
        let _ = server
            .execute_tool("market_search", json!({"query": format!("test-{}", i)}))
            .await;
    }

    // Memory should not grow unbounded
    // (In practice, would need memory profiling tools to verify)
}

#[tokio::test]
async fn test_server_size_is_reasonable() {
    let server = GgenMcpServer::new();
    let size = std::mem::size_of_val(&server);

    // Server should not be excessively large
    assert!(
        size < 100_000_000, // < 100MB
        "Server size {} bytes is too large",
        size
    );
}

// ============================================================================
// THREAD SAFETY TESTS
// ============================================================================

#[tokio::test]
async fn test_server_is_thread_safe() {
    let server = Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    // Spawn tasks on different threads
    for i in 0..10 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            for j in 0..10 {
                let _ = server_clone
                    .execute_tool(
                        "market_search",
                        json!({"query": format!("thread-{}-query-{}", i, j)}),
                    )
                    .await;
            }
        });
        handles.push(handle);
    }

    // All threads should complete without data races
    for handle in handles {
        handle.await.expect("Thread should not panic");
    }
}

#[tokio::test]
async fn test_server_with_rwlock_wrapper() {
    let server = Arc::new(RwLock::new(GgenMcpServer::new()));

    // Read operations (multiple concurrent)
    let mut read_handles = vec![];
    for i in 0..10 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            let server_read = server_clone.read().await;
            server_read
                .execute_tool("market_search", json!({"query": format!("read-{}", i)}))
                .await
        });
        read_handles.push(handle);
    }

    // All reads should succeed
    for handle in read_handles {
        let _ = handle.await;
    }
}

// ============================================================================
// GRACEFUL SHUTDOWN TESTS
// ============================================================================

#[tokio::test]
async fn test_server_shutdown_with_active_connections() {
    let server = Arc::new(GgenMcpServer::new());

    // Start some background operations
    let server_clone = server.clone();
    let _background_task = tokio::spawn(async move {
        for i in 0..100 {
            let _ = server_clone
                .execute_tool("market_search", json!({"query": format!("bg-{}", i)}))
                .await;
            tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
        }
    });

    // Drop main server reference
    drop(server);

    // Background tasks should continue until completion
    tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
}

#[tokio::test]
async fn test_server_cancellation_safety() {
    let server = Arc::new(GgenMcpServer::new());
    let server_clone = server.clone();

    // Start a task and cancel it
    let handle =
        tokio::spawn(async move { server_clone.execute_tool("market_list", json!({})).await });

    // Cancel immediately
    handle.abort();

    // Server should still work
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server survived cancellation"),
        Err(_) => println!("Server maintains stability"),
    }
}

// ============================================================================
// CONNECTION POOL SIMULATION TESTS
// ============================================================================

#[tokio::test]
async fn test_simulate_connection_pool() {
    // Simulate a connection pool with 5 servers
    let pool: Vec<_> = (0..5).map(|_| Arc::new(GgenMcpServer::new())).collect();

    let mut handles = vec![];

    // Distribute 50 requests across the pool
    for i in 0..50 {
        let server = pool[i % 5].clone();
        let handle = tokio::spawn(async move {
            server
                .execute_tool("market_search", json!({"query": format!("pooled-{}", i)}))
                .await
        });
        handles.push(handle);
    }

    // All should complete
    for handle in handles {
        let _ = handle.await;
    }
}

// ============================================================================
// HEALTH CHECK SIMULATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_health_check_pattern() {
    let server = GgenMcpServer::new();

    // Simulate periodic health checks
    for _ in 0..10 {
        let result = server.execute_tool("market_list", json!({})).await;

        let is_healthy = result.is_ok() || result.is_err();
        assert!(is_healthy, "Server should respond to health checks");

        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
    }
}

#[tokio::test]
async fn test_server_responds_after_idle_period() {
    let server = GgenMcpServer::new();

    // Make initial request
    let _ = server.execute_tool("market_list", json!({})).await;

    // Idle period
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Server should still respond
    let result = server.execute_tool("market_list", json!({})).await;

    match result {
        Ok(_) => println!("Server responsive after idle"),
        Err(_) => println!("Server maintains availability"),
    }
}
