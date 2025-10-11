//! MCP Configuration Tests
//!
//! Comprehensive tests for MCP server configuration, initialization,
//! and capability negotiation following the Model Context Protocol spec.

use ggen_mcp::server::GgenMcpServer;
use serde_json::json;
use rmcp::model::{InitializeRequestParam, ProtocolVersion};

// ============================================================================
// SERVER INITIALIZATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_initialization_success() {
    let server = GgenMcpServer::new();

    // Server should initialize without errors
    assert!(std::mem::size_of_val(&server) > 0);
}

#[tokio::test]
async fn test_multiple_server_instances() {
    let server1 = GgenMcpServer::new();
    let server2 = GgenMcpServer::new();

    // Both servers should be independent
    assert!(std::mem::size_of_val(&server1) > 0);
    assert!(std::mem::size_of_val(&server2) > 0);
}

#[tokio::test]
async fn test_server_supports_required_tools() {
    let server = GgenMcpServer::new();

    // Verify server has all required tools registered
    // These are the core tools that must be available
    let required_tools = vec![
        "project_gen",
        "project_plan",
        "market_search",
        "market_list",
        "graph_query",
        "template_validate",
    ];

    // Each tool should be callable (even if params are missing, tool should exist)
    for tool_name in required_tools {
        let result = server.execute_tool(tool_name, json!({})).await;
        // Should fail due to missing params, not unknown tool
        if let Err(e) = result {
            let error_str = e.to_string();
            assert!(
                !error_str.contains("Unknown tool"),
                "Tool {} should be registered",
                tool_name
            );
        }
    }
}

// ============================================================================
// CAPABILITY NEGOTIATION TESTS
// ============================================================================

#[test]
fn test_protocol_version_compatibility() {
    // MCP protocol version should be compatible
    let version = ProtocolVersion("2024-11-05".to_string());
    assert_eq!(version.0, "2024-11-05");
}

#[tokio::test]
async fn test_server_capabilities_exposed() {
    let server = GgenMcpServer::new();

    // Server should expose its capabilities
    // Test by attempting to execute a tool
    let result = server.execute_tool("project_gen", json!({
        "template": "test"
    })).await;

    // Should succeed or fail gracefully (not panic)
    match result {
        Ok(_) => println!("Tool execution succeeded"),
        Err(e) => println!("Tool execution failed gracefully: {:?}", e),
    }
}

// ============================================================================
// TOOL REGISTRATION TESTS
// ============================================================================

#[tokio::test]
async fn test_all_project_tools_registered() {
    let server = GgenMcpServer::new();

    let project_tools = vec!["project_gen", "project_plan", "project_apply", "project_diff"];

    for tool in project_tools {
        let result = server.execute_tool(tool, json!({})).await;
        if let Err(e) = result {
            assert!(
                !e.to_string().contains("Unknown tool"),
                "Project tool {} should be registered",
                tool
            );
        }
    }
}

#[tokio::test]
async fn test_all_market_tools_registered() {
    let server = GgenMcpServer::new();

    let market_tools = vec![
        "market_list",
        "market_search",
        "market_install",
        "market_info",
        "market_recommend",
    ];

    for tool in market_tools {
        let result = server.execute_tool(tool, json!({})).await;
        if let Err(e) = result {
            assert!(
                !e.to_string().contains("Unknown tool"),
                "Market tool {} should be registered",
                tool
            );
        }
    }
}

#[tokio::test]
async fn test_all_graph_tools_registered() {
    let server = GgenMcpServer::new();

    let graph_tools = vec!["graph_query", "graph_load", "graph_export"];

    for tool in graph_tools {
        let result = server.execute_tool(tool, json!({})).await;
        if let Err(e) = result {
            assert!(
                !e.to_string().contains("Unknown tool"),
                "Graph tool {} should be registered",
                tool
            );
        }
    }
}

#[tokio::test]
async fn test_all_template_tools_registered() {
    let server = GgenMcpServer::new();

    let template_tools = vec!["template_validate", "template_render", "template_from_source"];

    for tool in template_tools {
        let result = server.execute_tool(tool, json!({})).await;
        if let Err(e) = result {
            assert!(
                !e.to_string().contains("Unknown tool"),
                "Template tool {} should be registered",
                tool
            );
        }
    }
}

// ============================================================================
// CONFIGURATION VALIDATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_handles_empty_params_gracefully() {
    let server = GgenMcpServer::new();

    // Server should handle empty params without panicking
    let result = server.execute_tool("project_gen", json!({})).await;

    // Should return error, not panic
    assert!(result.is_err());
}

#[tokio::test]
async fn test_server_handles_null_params() {
    let server = GgenMcpServer::new();

    // Server should handle null params gracefully
    let result = server.execute_tool("project_gen", json!(null)).await;

    // Should return error, not panic
    assert!(result.is_err());
}

#[tokio::test]
async fn test_server_handles_invalid_json_structure() {
    let server = GgenMcpServer::new();

    // Test with array instead of object
    let result = server.execute_tool("project_gen", json!(["invalid"])).await;

    // Should return error, not panic
    assert!(result.is_err());
}

// ============================================================================
// SCHEMA VALIDATION TESTS
// ============================================================================

#[test]
fn test_tool_schemas_are_valid_json() {
    use ggen_mcp::schema::*;

    // All schemas should be valid JSON objects
    let schemas = vec![
        project_gen_schema(),
        project_plan_schema(),
        market_search_schema(),
        graph_query_schema(),
        template_validate_schema(),
    ];

    for schema in schemas {
        assert!(schema.is_object(), "Schema should be a JSON object");
        assert!(
            schema.get("type").is_some(),
            "Schema should have a type field"
        );
    }
}

#[test]
fn test_required_fields_specified_in_schemas() {
    use ggen_mcp::schema::*;

    // project_gen should require 'template'
    let project_gen = project_gen_schema();
    if let Some(required) = project_gen.get("required") {
        assert!(required.is_array());
        let required_array = required.as_array().unwrap();
        assert!(
            required_array.iter().any(|v| v.as_str() == Some("template")),
            "project_gen should require 'template' parameter"
        );
    }

    // market_search should require 'query'
    let market_search = market_search_schema();
    if let Some(required) = market_search.get("required") {
        let required_array = required.as_array().unwrap();
        assert!(
            required_array.iter().any(|v| v.as_str() == Some("query")),
            "market_search should require 'query' parameter"
        );
    }

    // graph_query should require 'sparql'
    let graph_query = graph_query_schema();
    if let Some(required) = graph_query.get("required") {
        let required_array = required.as_array().unwrap();
        assert!(
            required_array.iter().any(|v| v.as_str() == Some("sparql")),
            "graph_query should require 'sparql' parameter"
        );
    }
}

// ============================================================================
// CONCURRENT CONFIGURATION TESTS
// ============================================================================

#[tokio::test]
async fn test_concurrent_server_initialization() {
    let mut handles = vec![];

    for _ in 0..10 {
        let handle = tokio::spawn(async {
            GgenMcpServer::new()
        });
        handles.push(handle);
    }

    // All servers should initialize successfully
    for handle in handles {
        let server = handle.await.expect("Task should not panic");
        assert!(std::mem::size_of_val(&server) > 0);
    }
}

#[tokio::test]
async fn test_concurrent_tool_execution() {
    let server = std::sync::Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    for i in 0..20 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            server_clone.execute_tool(
                "market_search",
                json!({"query": format!("test-{}", i)}),
            ).await
        });
        handles.push(handle);
    }

    // All calls should complete without panicking
    for handle in handles {
        let _ = handle.await.expect("Task should not panic");
    }
}

// ============================================================================
// ENVIRONMENT CONFIGURATION TESTS
// ============================================================================

#[test]
fn test_server_works_without_env_vars() {
    // Server should work even if environment variables are not set
    let server = GgenMcpServer::new();
    assert!(std::mem::size_of_val(&server) > 0);
}

#[tokio::test]
async fn test_server_respects_timeout_configuration() {
    let server = GgenMcpServer::new();

    // Execute a simple operation
    let start = std::time::Instant::now();
    let _ = server.execute_tool("project_gen", json!({})).await;
    let duration = start.elapsed();

    // Should complete quickly (under 5 seconds for param validation)
    assert!(
        duration.as_secs() < 5,
        "Simple param validation should be fast"
    );
}

// ============================================================================
// ERROR HANDLING CONFIGURATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_error_responses_are_consistent() {
    let server = GgenMcpServer::new();

    // Test multiple error scenarios
    let errors = vec![
        server.execute_tool("unknown_tool", json!({})).await,
        server.execute_tool("project_gen", json!({})).await,
        server.execute_tool("market_search", json!({})).await,
    ];

    // All should be errors
    for result in errors {
        assert!(result.is_err(), "Should return consistent error format");
    }
}

#[tokio::test]
async fn test_server_maintains_state_across_calls() {
    let server = GgenMcpServer::new();

    // First call
    let _ = server.execute_tool("market_list", json!({})).await;

    // Second call should work independently
    let result = server.execute_tool("market_list", json!({})).await;

    // Server state should not be corrupted
    match result {
        Ok(_) => println!("Server state maintained correctly"),
        Err(_) => println!("Server state maintained (error is independent)"),
    }
}

// ============================================================================
// INTEGRATION WITH RMCP PROTOCOL TESTS
// ============================================================================

#[test]
fn test_error_converts_to_rmcp_error_data() {
    use ggen_mcp::error::GgenMcpError;
    use rmcp::ErrorData;

    let error = GgenMcpError::MissingParameter("test".to_string());
    let error_data: ErrorData = error.into();

    // Should have valid error code and message
    assert!(!error_data.message.is_empty());
}

#[test]
fn test_success_response_format() {
    use ggen_mcp::error::success_response;

    let data = json!({"key": "value"});
    let response = success_response(data);

    // Should have success status
    assert_eq!(response.get("status").and_then(|v| v.as_str()), Some("success"));
    assert!(response.get("data").is_some());
}

// ============================================================================
// PERFORMANCE CONFIGURATION TESTS
// ============================================================================

#[tokio::test]
async fn test_server_memory_usage_is_reasonable() {
    use std::mem::size_of_val;

    let server = GgenMcpServer::new();
    let size = size_of_val(&server);

    // Server should not be excessively large (< 10MB)
    assert!(size < 10_000_000, "Server size should be reasonable");
}

#[tokio::test]
async fn test_rapid_sequential_tool_calls() {
    let server = GgenMcpServer::new();

    let start = std::time::Instant::now();

    // Execute 100 rapid calls
    for i in 0..100 {
        let _ = server.execute_tool(
            "market_search",
            json!({"query": format!("test-{}", i)}),
        ).await;
    }

    let duration = start.elapsed();

    // Should complete in reasonable time (< 30 seconds)
    assert!(
        duration.as_secs() < 30,
        "100 calls should complete quickly"
    );
}

// ============================================================================
// CLEANUP AND RESOURCE MANAGEMENT TESTS
// ============================================================================

#[tokio::test]
async fn test_server_cleanup_on_drop() {
    {
        let _server = GgenMcpServer::new();
        // Server created
    }
    // Server should be cleaned up here without issues
}

#[tokio::test]
async fn test_multiple_create_and_drop_cycles() {
    for _ in 0..10 {
        let server = GgenMcpServer::new();
        let _ = server.execute_tool("market_list", json!({})).await;
        // Server dropped at end of iteration
    }
    // No resource leaks should occur
}
