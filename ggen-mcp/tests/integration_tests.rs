///! Integration tests for ggen-mcp server
///!
///! Tests the MCP protocol implementation and tool execution
use ggen_mcp::{error::Result, GgenMcpServer};
use serde_json::json;

#[tokio::test]
async fn test_server_creation() {
    let server = GgenMcpServer::new();
    // Server should have all expected tools registered
    assert!(server.tool_count() >= 18, "Expected at least 18 tools");
}

#[tokio::test]
async fn test_tool_routing_project_tools() {
    let server = GgenMcpServer::new();

    // Test that project tools are routable (they will fail without valid inputs, but routing works)
    let tools = vec![
        "project_gen",
        "project_plan",
        "project_apply",
        "project_diff",
    ];

    for tool in tools {
        assert!(
            server.has_tool(tool),
            "Tool '{}' should be registered",
            tool
        );
    }
}

#[tokio::test]
async fn test_tool_routing_market_tools() {
    let server = GgenMcpServer::new();

    let tools = vec![
        "market_list",
        "market_search",
        "market_install",
        "market_recommend",
        "market_info",
        "market_offline_search",
        "market_cache_status",
        "market_sync",
    ];

    for tool in tools {
        assert!(
            server.has_tool(tool),
            "Tool '{}' should be registered",
            tool
        );
    }
}

#[tokio::test]
async fn test_tool_routing_graph_tools() {
    let server = GgenMcpServer::new();

    let tools = vec!["graph_query", "graph_load", "graph_export"];

    for tool in tools {
        assert!(
            server.has_tool(tool),
            "Tool '{}' should be registered",
            tool
        );
    }
}

#[tokio::test]
async fn test_tool_routing_template_tools() {
    let server = GgenMcpServer::new();

    let tools = vec!["template_create", "template_validate"];

    for tool in tools {
        assert!(
            server.has_tool(tool),
            "Tool '{}' should be registered",
            tool
        );
    }
}

#[tokio::test]
async fn test_tool_routing_hook_tools() {
    let server = GgenMcpServer::new();

    assert!(
        server.has_tool("hook_register"),
        "hook_register should be registered"
    );
}

#[tokio::test]
async fn test_unknown_tool_error() {
    let server = GgenMcpServer::new();

    let result = server.execute_tool("unknown_tool", json!({})).await;

    assert!(result.is_err(), "Unknown tool should return error");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("Unknown tool"),
        "Error should mention unknown tool"
    );
}

#[tokio::test]
async fn test_missing_required_parameter() {
    let server = GgenMcpServer::new();

    // project_gen requires 'template' parameter
    let result = server.execute_tool("project_gen", json!({})).await;

    assert!(result.is_err(), "Should fail with missing parameter");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("template") || err_msg.contains("Missing"),
        "Error should mention missing parameter"
    );
}

#[tokio::test]
async fn test_hook_register_success() {
    let server = GgenMcpServer::new();

    let params = json!({
        "event": "pre_gen",
        "command": "echo test",
        "name": "test_hook"
    });

    let result = server.execute_tool("hook_register", params).await;

    // Hook registration should succeed (it's a placeholder implementation)
    assert!(result.is_ok(), "Hook registration should succeed");

    let response = result.unwrap();
    assert!(
        response.get("status").is_some(),
        "Response should have status"
    );
}

#[tokio::test]
async fn test_error_response_format() {
    let server = GgenMcpServer::new();

    // Trigger an error with invalid parameters
    let result = server.execute_tool("market_search", json!({})).await;

    assert!(result.is_err(), "Should fail with missing query parameter");

    // Error should be properly formatted
    let err = result.unwrap_err();
    let err_string = format!("{}", err);
    assert!(!err_string.is_empty(), "Error message should not be empty");
}

#[tokio::test]
async fn test_tool_count_is_correct() {
    let server = GgenMcpServer::new();
    let tool_count = server.tool_count();

    assert_eq!(
        tool_count, 18,
        "Should have exactly 18 tools, got {}",
        tool_count
    );

    // Verify we can get tool names
    let tool_names = server.tool_names();
    assert_eq!(
        tool_names.len(),
        tool_count,
        "tool_names should match tool_count"
    );
}

#[tokio::test]
async fn test_concurrent_tool_execution() {
    let server = GgenMcpServer::new();

    // Test that multiple tools can be executed concurrently
    let futures = vec![
        server.execute_tool(
            "hook_register",
            json!({
                "event": "pre_gen",
                "command": "echo 1"
            }),
        ),
        server.execute_tool(
            "hook_register",
            json!({
                "event": "post_gen",
                "command": "echo 2"
            }),
        ),
        server.execute_tool(
            "hook_register",
            json!({
                "event": "pre_apply",
                "command": "echo 3"
            }),
        ),
    ];

    let results = futures_util::future::join_all(futures).await;

    // All should succeed
    for (i, result) in results.iter().enumerate() {
        assert!(result.is_ok(), "Concurrent execution {} should succeed", i);
    }
}

#[tokio::test]
async fn test_server_default_trait() {
    let server = GgenMcpServer::default();
    assert!(
        server.tool_count() >= 18,
        "Default server should have tools"
    );
}
