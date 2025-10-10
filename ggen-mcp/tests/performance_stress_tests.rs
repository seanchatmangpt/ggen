//! Performance and Stress Tests
//!
//! These tests validate performance characteristics and system behavior under load

mod common;

use common::*;
use ggen_mcp::server::GgenMcpServer;
use serde_json::json;
use std::time::{Duration, Instant};

/// Test server creation performance
#[test]
fn test_server_creation_benchmark() {
    let iterations = 1000;
    let start = Instant::now();

    for _ in 0..iterations {
        let _server = GgenMcpServer::new();
    }

    let duration = start.elapsed();
    let avg_per_creation = duration.as_micros() / iterations;

    println!(
        "Created {} servers in {:?} (avg: {}μs per server)",
        iterations, duration, avg_per_creation
    );

    assert!(
        avg_per_creation < 100,
        "Server creation should be fast (< 100μs)"
    );
}

/// Test throughput for market search operations
#[tokio::test]
async fn test_market_search_throughput() {
    let server = GgenMcpServer::new();
    let queries = 100;
    let start = Instant::now();

    for i in 0..queries {
        let _ = server
            .execute_tool(
                "market_search",
                json!({
                    "query": format!("query-{}", i),
                    "limit": 10
                }),
            )
            .await;
    }

    let duration = start.elapsed();
    let qps = queries as f64 / duration.as_secs_f64();

    println!(
        "Executed {} search queries in {:?} ({:.2} QPS)",
        queries, duration, qps
    );

    assert!(qps > 10.0, "Should handle at least 10 queries per second");
}

/// Test concurrent request handling capacity
#[tokio::test]
async fn test_concurrent_request_capacity() {
    let server = std::sync::Arc::new(GgenMcpServer::new());
    let concurrent_requests = 100;
    let mut handles = vec![];

    let start = Instant::now();

    for i in 0..concurrent_requests {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone
                .execute_tool(
                    "market_search",
                    json!({
                        "query": format!("concurrent-{}", i)
                    }),
                )
                .await
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;
    let duration = start.elapsed();

    let success_count = results.iter().filter(|r| {
        r.is_ok() && r.as_ref().unwrap().is_ok()
    }).count();

    println!(
        "Handled {} concurrent requests in {:?} ({} succeeded)",
        concurrent_requests, duration, success_count
    );

    assert!(
        success_count >= 95,
        "Should successfully handle 95%+ of concurrent requests"
    );
}

/// Test memory usage stability over time
#[tokio::test]
#[ignore] // Run with --ignored for full memory test
async fn test_memory_usage_stability() {
    let server = GgenMcpServer::new();
    let iterations = 10000;

    println!("Starting memory stability test with {} iterations", iterations);

    for i in 0..iterations {
        let _ = server
            .execute_tool(
                "market_search",
                json!({
                    "query": format!("query-{}", i),
                    "limit": 5
                }),
            )
            .await;

        if i % 1000 == 0 {
            println!("Completed {} iterations", i);
        }
    }

    println!("Memory stability test completed");
}

/// Test response time consistency
#[tokio::test]
async fn test_response_time_consistency() {
    let server = GgenMcpServer::new();
    let mut latencies = Vec::new();

    for i in 0..50 {
        let start = Instant::now();

        let _ = server
            .execute_tool(
                "market_search",
                json!({
                    "query": format!("test-{}", i)
                }),
            )
            .await;

        latencies.push(start.elapsed().as_millis());
    }

    let avg_latency = latencies.iter().sum::<u128>() / latencies.len() as u128;
    let max_latency = *latencies.iter().max().unwrap();
    let min_latency = *latencies.iter().min().unwrap();

    println!(
        "Latency stats: avg={}ms, min={}ms, max={}ms",
        avg_latency, min_latency, max_latency
    );

    // Check for consistency (max should not be more than 10x avg)
    assert!(
        max_latency < avg_latency * 10,
        "Response times should be consistent"
    );
}

/// Test sustained load handling
#[tokio::test]
#[ignore] // Run with --ignored for sustained load test
async fn test_sustained_load() {
    let server = std::sync::Arc::new(GgenMcpServer::new());
    let duration = Duration::from_secs(30);
    let start = Instant::now();
    let mut request_count = 0;

    println!("Starting 30-second sustained load test");

    while start.elapsed() < duration {
        let server_clone = server.clone();

        tokio::spawn(async move {
            let _ = server_clone
                .execute_tool("market_search", json!({"query": "test"}))
                .await;
        });

        request_count += 1;

        if request_count % 100 == 0 {
            println!("Sent {} requests", request_count);
        }

        tokio::time::sleep(Duration::from_millis(10)).await;
    }

    println!(
        "Sustained load test completed: {} requests in {:?}",
        request_count, duration
    );
}

/// Test large payload handling
#[tokio::test]
async fn test_large_payload_performance() {
    let server = GgenMcpServer::new();

    // Create large variable payload
    let large_vars: serde_json::Value = serde_json::from_str(&format!(
        r#"{{
            "name": "test",
            "data": {}
        }}"#,
        serde_json::json!((0..1000).map(|i| {
            json!({
                "id": i,
                "value": format!("value-{}", i),
                "nested": {
                    "field1": i * 2,
                    "field2": format!("nested-{}", i)
                }
            })
        }).collect::<Vec<_>>())
    )).unwrap();

    let start = Instant::now();
    let result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "vars": large_vars
            }),
        )
        .await;
    let duration = start.elapsed();

    println!("Large payload processed in {:?}", duration);
    assert!(result.is_ok(), "Should handle large payloads");
    assert!(duration.as_secs() < 5, "Should process large payload quickly");
}

/// Test burst traffic handling
#[tokio::test]
async fn test_burst_traffic() {
    let server = std::sync::Arc::new(GgenMcpServer::new());

    // Send burst of requests
    let burst_size = 50;
    let mut handles = vec![];

    let start = Instant::now();

    for i in 0..burst_size {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone
                .execute_tool("market_list", json!({}))
                .await
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;
    let burst_duration = start.elapsed();

    // Wait a bit
    tokio::time::sleep(Duration::from_millis(100)).await;

    // Send another burst
    let mut handles2 = vec![];
    for i in 0..burst_size {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone
                .execute_tool("market_search", json!({"query": format!("burst-{}", i)}))
                .await
        });
        handles2.push(handle);
    }

    let results2 = futures::future::join_all(handles2).await;

    let success1 = results.iter().filter(|r| r.is_ok()).count();
    let success2 = results2.iter().filter(|r| r.is_ok()).count();

    println!(
        "Burst 1: {}/{} succeeded in {:?}",
        success1, burst_size, burst_duration
    );
    println!("Burst 2: {}/{} succeeded", success2, burst_size);

    assert!(success1 >= burst_size * 9 / 10, "Should handle first burst well");
    assert!(success2 >= burst_size * 9 / 10, "Should handle second burst well");
}

/// Test parallel tool execution performance
#[tokio::test]
async fn test_parallel_tool_execution() {
    let server = std::sync::Arc::new(GgenMcpServer::new());

    let tools = vec![
        ("market_search", json!({"query": "test"})),
        ("market_list", json!({})),
        ("project_plan", json!({"template": "test"})),
        ("graph_query", json!({"sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"})),
    ];

    let iterations = 10;
    let start = Instant::now();

    for _ in 0..iterations {
        let mut handles = vec![];

        for (tool, params) in &tools {
            let server_clone = server.clone();
            let tool = tool.to_string();
            let params = params.clone();

            let handle = tokio::spawn(async move {
                server_clone.execute_tool(&tool, params).await
            });
            handles.push(handle);
        }

        let _ = futures::future::join_all(handles).await;
    }

    let duration = start.elapsed();
    let total_calls = iterations * tools.len();

    println!(
        "Executed {} parallel tool calls in {:?} ({:.2} calls/sec)",
        total_calls,
        duration,
        total_calls as f64 / duration.as_secs_f64()
    );
}

/// Test cache performance
#[tokio::test]
async fn test_cache_performance() {
    let server = GgenMcpServer::new();

    // First query (cache miss)
    let start_miss = Instant::now();
    let _ = server
        .execute_tool("market_search", json!({"query": "rust api"}))
        .await;
    let miss_duration = start_miss.elapsed();

    // Second identical query (cache hit, if caching implemented)
    let start_hit = Instant::now();
    let _ = server
        .execute_tool("market_search", json!({"query": "rust api"}))
        .await;
    let hit_duration = start_hit.elapsed();

    println!(
        "Cache performance: miss={}ms, hit={}ms",
        miss_duration.as_millis(),
        hit_duration.as_millis()
    );

    // If caching is implemented, hit should be faster
    if hit_duration < miss_duration {
        println!("Cache is working! Hit is {}x faster",
            miss_duration.as_secs_f64() / hit_duration.as_secs_f64()
        );
    }
}

/// Benchmark tool comparison
#[tokio::test]
async fn test_tool_performance_comparison() {
    let server = GgenMcpServer::new();

    let tools_to_benchmark = vec![
        ("market_search", json!({"query": "test"})),
        ("market_list", json!({"limit": 10})),
        ("project_plan", json!({"template": "test"})),
        ("graph_query", json!({"sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"})),
        ("template_validate", json!({"template": "{{name}}"})),
    ];

    println!("\n=== Tool Performance Benchmark ===");

    for (tool, params) in tools_to_benchmark {
        let start = Instant::now();
        let _ = server.execute_tool(tool, params).await;
        let duration = start.elapsed();

        println!("{:20} {:?}", tool, duration);
    }
    println!("==================================\n");
}

/// Test error handling performance (errors shouldn't slow down system)
#[tokio::test]
async fn test_error_handling_performance() {
    let server = GgenMcpServer::new();
    let mut error_latencies = Vec::new();
    let mut success_latencies = Vec::new();

    // Test error cases
    for i in 0..20 {
        let start = Instant::now();
        let _ = server
            .execute_tool("project_gen", json!({})) // Missing required param
            .await;
        error_latencies.push(start.elapsed().as_micros());
    }

    // Test success cases
    for i in 0..20 {
        let start = Instant::now();
        let _ = server
            .execute_tool("market_list", json!({}))
            .await;
        success_latencies.push(start.elapsed().as_micros());
    }

    let avg_error = error_latencies.iter().sum::<u128>() / error_latencies.len() as u128;
    let avg_success = success_latencies.iter().sum::<u128>() / success_latencies.len() as u128;

    println!(
        "Error handling: avg_error={}μs, avg_success={}μs",
        avg_error, avg_success
    );

    // Error handling should be fast, not significantly slower than success path
    assert!(
        avg_error < avg_success * 5,
        "Error handling should not be significantly slower"
    );
}
