//! Integration tests - test full workflow without LLM API calls

use rig_mcp_integration::Config;
use std::fs;
use tempfile::TempDir;

mod mock_mcp_server;

#[tokio::test]
async fn test_full_config_to_manager_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let config_path = temp_dir.path().join("config.toml");

    // Note: We can't actually test MCP manager creation without a real MCP server
    // But we can test that config loads and is ready for manager creation
    let config_content = r#"
deepseek_key = "test-key"
cohere_key = "test-cohere"

[mcp]
[[mcp.server]]
name = "test-filesystem"
protocol = "stdio"
command = "echo"
args = ["test"]
"#;

    fs::write(&config_path, config_content).unwrap();

    let config = Config::retrieve(&config_path).await.unwrap();

    // Verify config loaded successfully
    assert!(config.deepseek_key.is_some());
    assert!(config.cohere_key.is_some());

    // In a real scenario, you would do:
    // let mcp_manager = config.mcp.create_manager().await.unwrap();
    // But that requires actual MCP servers, which costs money to test
}

#[test]
fn test_mock_server_provides_valid_tools() {
    let tools_response = mock_mcp_server::MockMcpServer::mock_tools_list();

    // Verify the mock response structure matches MCP protocol
    assert!(tools_response["tools"].is_array());

    let tools = tools_response["tools"].as_array().unwrap();
    assert!(tools.len() > 0);

    // Check first tool has required fields
    let first_tool = &tools[0];
    assert!(first_tool["name"].is_string());
    assert!(first_tool["description"].is_string());
    assert!(first_tool["inputSchema"].is_object());
}

#[test]
fn test_mock_server_tool_calls_work() {
    use serde_json::json;

    // Test read_file mock
    let response = mock_mcp_server::MockMcpServer::mock_tool_call_response(
        "read_file",
        json!({"path": "/test/file.txt"})
    );
    assert!(response["content"].is_array());

    // Test write_file mock
    let response = mock_mcp_server::MockMcpServer::mock_tool_call_response(
        "write_file",
        json!({"path": "/test/output.txt", "content": "test data"})
    );
    assert!(response["content"].is_array());

    // Test list_directory mock
    let response = mock_mcp_server::MockMcpServer::mock_tool_call_response(
        "list_directory",
        json!({"path": "/test"})
    );
    assert!(response["content"].is_array());
}

#[tokio::test]
async fn test_config_validates_different_transports() {
    let temp_dir = TempDir::new().unwrap();

    // Test stdio transport config
    let config_path = temp_dir.path().join("stdio.toml");
    fs::write(&config_path, r#"
[mcp]
[[mcp.server]]
name = "stdio-server"
protocol = "stdio"
command = "echo"
args = ["test"]
"#).unwrap();
    let config = Config::retrieve(&config_path).await;
    assert!(config.is_ok(), "Stdio transport config should load");

    // Test SSE transport config
    let config_path = temp_dir.path().join("sse.toml");
    fs::write(&config_path, r#"
[mcp]
[[mcp.server]]
name = "sse-server"
protocol = "sse"
url = "http://localhost:3000/sse"
"#).unwrap();
    let config = Config::retrieve(&config_path).await;
    assert!(config.is_ok(), "SSE transport config should load");

    // Test streamable transport config
    let config_path = temp_dir.path().join("streamable.toml");
    fs::write(&config_path, r#"
[mcp]
[[mcp.server]]
name = "http-server"
protocol = "streamable"
url = "http://localhost:3000/mcp"
"#).unwrap();
    let config = Config::retrieve(&config_path).await;
    assert!(config.is_ok(), "Streamable transport config should load");
}
