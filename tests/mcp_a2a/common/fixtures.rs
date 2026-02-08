//! Test fixtures for MCP A2A integration tests
//!
//! Provides reusable test data and fixture builders following
//! Chicago TDD patterns with state-based setup.

use serde_json::json;
use std::collections::HashMap;

use crate::mcp_a2a::{
    MockMcpServer, ToolHandler, ToolHandlerBuilder, McpError, JsonRpcMessage,
    ServerConfig,
};

// ============================================================================
// MCP SERVER FIXTURES
// ============================================================================

/// Create a minimal mock MCP server with no tools
pub fn empty_server() -> MockMcpServer {
    MockMcpServer::new()
}

/// Create a mock server with echo tool only
pub async fn echo_server() -> MockMcpServer {
    let server = MockMcpServer::new();
    server
        .register_tool(
            "echo".to_string(),
            ToolHandlerBuilder::new("echo")
                .description("Echoes the input message")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "message": {"type": "string"}
                    },
                    "required": ["message"]
                }))
                .build_echo(),
        )
        .await;
    server
}

/// Create a mock server with calculator tools
pub async fn calculator_server() -> MockMcpServer {
    let server = MockMcpServer::new();

    // Add tool
    server
        .register_tool(
            "add".to_string(),
            ToolHandlerBuilder::new("add")
                .description("Adds two numbers")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "a": {"type": "number"},
                        "b": {"type": "number"}
                    },
                    "required": ["a", "b"]
                }))
                .build(|params| async move {
                    let a = params.get("a").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    let b = params.get("b").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    Ok(json!({"result": a + b}))
                }),
        )
        .await;

    // Subtract tool
    server
        .register_tool(
            "subtract".to_string(),
            ToolHandlerBuilder::new("subtract")
                .description("Subtracts b from a")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "a": {"type": "number"},
                        "b": {"type": "number"}
                    },
                    "required": ["a", "b"]
                }))
                .build(|params| async move {
                    let a = params.get("a").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    let b = params.get("b").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    Ok(json!({"result": a - b}))
                }),
        )
        .await;

    // Multiply tool
    server
        .register_tool(
            "multiply".to_string(),
            ToolHandlerBuilder::new("multiply")
                .description("Multiplies two numbers")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "a": {"type": "number"},
                        "b": {"type": "number"}
                    },
                    "required": ["a", "b"]
                }))
                .build(|params| async move {
                    let a = params.get("a").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    let b = params.get("b").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    Ok(json!({"result": a * b}))
                }),
        )
        .await;

    // Divide tool
    server
        .register_tool(
            "divide".to_string(),
            ToolHandlerBuilder::new("divide")
                .description("Divides a by b")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "a": {"type": "number"},
                        "b": {"type": "number"}
                    },
                    "required": ["a", "b"]
                }))
                .build(|params| async move {
                    let a = params.get("a").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    let b = params.get("b").and_then(|v| v.as_f64()).unwrap_or(1.0);
                    if b == 0.0 {
                        Err(McpError::invalid_params("Division by zero"))
                    } else {
                        Ok(json!({"result": a / b}))
                    }
                }),
        )
        .await;

    server
}

/// Create a mock server with data transformation tools
pub async fn transformation_server() -> MockMcpServer {
    let server = MockMcpServer::new();

    // Uppercase tool
    server
        .register_tool(
            "to_uppercase".to_string(),
            ToolHandlerBuilder::new("to_uppercase")
                .description("Converts string to uppercase")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "text": {"type": "string"}
                    },
                    "required": ["text"]
                }))
                .build(|params| async move {
                    let text = params
                        .get("text")
                        .and_then(|v| v.as_str())
                        .unwrap_or("");
                    Ok(json!({"result": text.to_uppercase()}))
                }),
        )
        .await;

    // Lowercase tool
    server
        .register_tool(
            "to_lowercase".to_string(),
            ToolHandlerBuilder::new("to_lowercase")
                .description("Converts string to lowercase")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "text": {"type": "string"}
                    },
                    "required": ["text"]
                }))
                .build(|params| async move {
                    let text = params
                        .get("text")
                        .and_then(|v| v.as_str())
                        .unwrap_or("");
                    Ok(json!({"result": text.to_lowercase()}))
                }),
        )
        .await;

    // Reverse tool
    server
        .register_tool(
            "reverse".to_string(),
            ToolHandlerBuilder::new("reverse")
                .description("Reverses a string")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "text": {"type": "string"}
                    },
                    "required": ["text"]
                }))
                .build(|params| async move {
                    let text = params
                        .get("text")
                        .and_then(|v| v.as_str())
                        .unwrap_or("");
                    Ok(json!({"result": text.chars().rev().collect::<String>()}))
                }),
        )
        .await;

    server
}

/// Create a mock server with collection tools
pub async fn collection_server() -> MockMcpServer {
    let server = MockMcpServer::new();

    // Sum array tool
    server
        .register_tool(
            "sum_array".to_string(),
            ToolHandlerBuilder::new("sum_array")
                .description("Sums all numbers in an array")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "numbers": {
                            "type": "array",
                            "items": {"type": "number"}
                        }
                    },
                    "required": ["numbers"]
                }))
                .build(|params| async move {
                    let numbers = params
                        .get("numbers")
                        .and_then(|v| v.as_array())
                        .unwrap_or(&vec![]);
                    let sum: f64 = numbers
                        .iter()
                        .filter_map(|v| v.as_f64())
                        .sum();
                    Ok(json!({"result": sum}))
                }),
        )
        .await;

    // Count tool
    server
        .register_tool(
            "count".to_string(),
            ToolHandlerBuilder::new("count")
                .description("Counts items in an array")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "items": {
                            "type": "array"
                        }
                    },
                    "required": ["items"]
                }))
                .build(|params| async move {
                    let items = params
                        .get("items")
                        .and_then(|v| v.as_array())
                        .unwrap_or(&vec![]);
                    Ok(json!({"result": items.len()}))
                }),
        )
        .await;

    server
}

/// Create a mock server with custom configuration
pub fn configured_server() -> ServerConfig {
    ServerConfig {
        name: "test-mcp-server".to_string(),
        version: "2.0.0".to_string(),
        enable_logging: true,
        max_history: 500,
        timeout_ms: 10000,
    }
}

/// Create a mock server that simulates failures
pub async fn failing_server() -> MockMcpServer {
    let server = MockMcpServer::new();

    // Always-fails tool
    server
        .register_tool(
            "always_fails".to_string(),
            ToolHandlerBuilder::new("always_fails")
                .description("Tool that always fails")
                .fails(McpError::tool_execution("Simulated failure"))
                .build(|_params| async move {
                    Err(McpError::internal_error("Should not reach here"))
                }),
        )
        .await;

    // Timeout tool
    server
        .register_tool(
            "timeout_tool".to_string(),
            ToolHandlerBuilder::new("timeout_tool")
                .description("Tool that times out")
                .duration_ms(6000) // Longer than default timeout
                .build(|_params| async move {
                    // Sleep longer than server timeout
                    tokio::time::sleep(std::time::Duration::from_secs(10)).await;
                    Ok(json!({"result": "should not reach here"}))
                }),
        )
        .await;

    server
}

// ============================================================================
// REQUEST FIXTURES
// ============================================================================

/// Create a standard tools/list request
pub fn tools_list_request() -> JsonRpcMessage {
    JsonRpcMessage::tools_list()
}

/// Create a tools/call request for echo
pub fn echo_request(message: &str) -> JsonRpcMessage {
    JsonRpcMessage::tools_call("echo", json!({"message": message}))
}

/// Create a tools/call request for add
pub fn add_request(a: f64, b: f64) -> JsonRpcMessage {
    JsonRpcMessage::tools_call("add", json!({"a": a, "b": b}))
}

/// Create a tools/call request for subtract
pub fn subtract_request(a: f64, b: f64) -> JsonRpcMessage {
    JsonRpcMessage::tools_call("subtract", json!({"a": a, "b": b}))
}

/// Create a health check request
pub fn health_check_request() -> JsonRpcMessage {
    JsonRpcMessage::health_check()
}

/// Create a resources/list request
pub fn resources_list_request() -> JsonRpcMessage {
    JsonRpcMessage::resources_list()
}

/// Create batch of requests for testing
pub fn batch_requests() -> Vec<JsonRpcMessage> {
    vec![
        tools_list_request(),
        health_check_request(),
        resources_list_request(),
        echo_request("test"),
    ]
}

// ============================================================================
// DATA FIXTURES
// ============================================================================

/// Sample JSON data for testing
pub fn sample_json_data() -> serde_json::Value {
    json!({
        "string": "test",
        "number": 42,
        "float": 3.14,
        "boolean": true,
        "null": null,
        "array": [1, 2, 3],
        "object": {"nested": "value"}
    })
}

/// Sample complex nested JSON
pub fn nested_json_data() -> serde_json::Value {
    json!({
        "user": {
            "id": 123,
            "name": "Test User",
            "email": "test@example.com",
            "roles": ["admin", "user"],
            "preferences": {
                "theme": "dark",
                "notifications": true
            }
        },
        "metadata": {
            "created_at": "2024-01-01T00:00:00Z",
            "updated_at": "2024-01-02T00:00:00Z"
        }
    })
}

/// Sample array data
pub fn sample_array_data() -> Vec<serde_json::Value> {
    vec![
        json!({"id": 1, "name": "Item 1"}),
        json!({"id": 2, "name": "Item 2"}),
        json!({"id": 3, "name": "Item 3"}),
    ]
}

// ============================================================================
// ERROR FIXTURES
// ============================================================================

/// Common error responses for testing
pub fn error_fixtures() -> HashMap<String, McpError> {
    let mut errors = HashMap::new();

    errors.insert("parse_error".to_string(), McpError::parse_error("Invalid JSON"));
    errors.insert(
        "invalid_request".to_string(),
        McpError::invalid_request("Malformed request"),
    );
    errors.insert(
        "method_not_found".to_string(),
        McpError::method_not_found("unknown_method"),
    );
    errors.insert(
        "invalid_params".to_string(),
        McpError::invalid_params("Missing required field"),
    );
    errors.insert(
        "internal_error".to_string(),
        McpError::internal_error("Something went wrong"),
    );
    errors.insert(
        "tool_not_found".to_string(),
        McpError::tool_not_found("nonexistent_tool"),
    );
    errors.insert(
        "tool_execution".to_string(),
        McpError::tool_execution("Tool crashed"),
    );
    errors.insert("timeout".to_string(), McpError::timeout("slow_tool"));

    errors
}

// ============================================================================
// SCENARIO FIXTURES
// ============================================================================

/// Scenario: Standard workflow
pub async fn standard_workflow_scenario() -> MockMcpServer {
    let server = calculator_server().await;
    server
}

/// Scenario: Error handling workflow
pub async fn error_handling_scenario() -> MockMcpServer {
    let server = failing_server().await;
    server
}

/// Scenario: High volume testing
pub async fn high_volume_scenario() -> MockMcpServer {
    let server = MockMcpServer::with_config(ServerConfig {
        max_history: 10000,
        ..Default::default()
    });

    // Register multiple tools
    for i in 0..10 {
        let name = format!("tool_{}", i);
        server
            .register_tool(
                name.clone(),
                ToolHandlerBuilder::new(&name)
                    .description(format!("Tool number {}", i))
                    .build_constant(json!({"tool": i})),
            )
            .await;
    }

    server
}

// ============================================================================
// BUILDER HELPERS
// ============================================================================

/// Builder for creating custom test scenarios
pub struct ScenarioBuilder {
    server: MockMcpServer,
}

impl ScenarioBuilder {
    /// Create a new scenario builder
    pub fn new() -> Self {
        Self {
            server: MockMcpServer::new(),
        }
    }

    /// Add a tool to the scenario
    pub async fn with_tool(mut self, name: &str, handler: ToolHandler) -> Self {
        self.server.register_tool(name.to_string(), handler).await;
        self
    }

    /// Configure error mode
    pub async fn with_error_mode(mut self, mode: crate::mcp_a2a::ErrorMode) -> Self {
        self.server.set_error_mode(mode).await;
        self
    }

    /// Build the final server
    pub fn build(self) -> MockMcpServer {
        self.server
    }
}

impl Default for ScenarioBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TEST SUITE FIXTURES
// ============================================================================

/// Complete fixture set for comprehensive testing
pub struct FixtureSet {
    pub empty_server: MockMcpServer,
    pub echo_server: MockMcpServer,
    pub calculator_server: MockMcpServer,
    pub transformation_server: MockMcpServer,
    pub failing_server: MockMcpServer,
}

impl FixtureSet {
    /// Create all fixtures asynchronously
    pub async fn new() -> Self {
        Self {
            empty_server: empty_server(),
            echo_server: echo_server().await,
            calculator_server: calculator_server().await,
            transformation_server: transformation_server().await,
            failing_server: failing_server().await,
        }
    }

    /// Get a server by name
    pub fn get(&self, name: &str) -> Option<&MockMcpServer> {
        match name {
            "empty" => Some(&self.empty_server),
            "echo" => Some(&self.echo_server),
            "calculator" => Some(&self.calculator_server),
            "transformation" => Some(&self.transformation_server),
            "failing" => Some(&self.failing_server),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_echo_server_fixture() {
        let server = echo_server().await;
        assert!(server.has_tool("echo").await);
    }

    #[tokio::test]
    async fn test_calculator_server_fixture() {
        let server = calculator_server().await;
        let tools = server.list_tools().await;
        assert_eq!(tools.len(), 4);
        assert!(tools.contains(&"add".to_string()));
        assert!(tools.contains(&"subtract".to_string()));
        assert!(tools.contains(&"multiply".to_string()));
        assert!(tools.contains(&"divide".to_string()));
    }

    #[tokio::test]
    async fn test_transformation_server_fixture() {
        let server = transformation_server().await;
        let tools = server.list_tools().await;
        assert_eq!(tools.len(), 3);
    }

    #[tokio::test]
    async fn test_failing_server_fixture() {
        let server = failing_server().await;
        let tools = server.list_tools().await;
        assert_eq!(tools.len(), 2);
    }

    #[tokio::test]
    async fn test_fixture_set() {
        let fixtures = FixtureSet::new().await;
        assert!(fixtures.get("empty").is_some());
        assert!(fixtures.get("echo").is_some());
        assert!(fixtures.get("calculator").is_some());
        assert!(fixtures.get("invalid").is_none());
    }

    #[tokio::test]
    async fn test_scenario_builder() {
        let server = ScenarioBuilder::new()
            .with_tool(
                "test",
                ToolHandlerBuilder::new("test")
                    .description("Test tool")
                    .build_constant(json!({"ok": true})),
            )
            .await;

        assert!(server.has_tool("test").await);
    }

    #[test]
    fn test_error_fixtures() {
        let errors = error_fixtures();
        assert_eq!(errors.len(), 8);
        assert!(errors.contains_key("parse_error"));
        assert!(errors.contains_key("timeout"));
    }

    #[test]
    fn test_sample_json_data() {
        let data = sample_json_data();
        assert_eq!(data.get("string").unwrap(), "test");
        assert_eq!(data.get("number").unwrap(), 42);
    }

    #[test]
    fn test_request_fixtures() {
        let req = echo_request("hello");
        assert_eq!(req.method, "tools/call");
    }
}
