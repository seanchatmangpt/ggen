//! Chicago TDD tests for MCP stdio transport layer
//!
//! Test suite following AAA pattern (Arrange-Act-Assert)
//! Coverage target: 80%+

use ggen_a2a_mcp::transport::{McpErrorCode, McpRequest, McpResponse, McpTransport};
use serde_json::json;

#[tokio::test]
async fn test_transport_creates_success_response() {
    // Arrange
    let transport = McpTransport::new();
    let request_id = 42;
    let result = json!({"status": "success"});

    // Act
    let response = transport.create_response(request_id, result);

    // Assert
    assert_eq!(response.id, request_id);
    assert!(response.result.is_some());
    assert!(response.error.is_none());
}

#[tokio::test]
async fn test_transport_creates_error_response() {
    // Arrange
    let transport = McpTransport::new();
    let request_id = 43;
    let error_code = McpErrorCode::InvalidRequest;
    let error_message = "Invalid request format";

    // Act
    let response = transport.create_error(request_id, error_code, error_message);

    // Assert
    assert_eq!(response.id, request_id);
    assert!(response.result.is_none());
    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, -32600);
}

#[tokio::test]
async fn test_transport_handles_initialize_request() {
    // Arrange
    let transport = McpTransport::new();
    let request = McpRequest {
        id: json!(1),
        method: "initialize".to_string(),
        params: json!({
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "test-client", "version": "1.0.0"}
        }),
    };

    // Act
    let result = transport.handle_request(request).await;

    // Assert
    assert!(result.is_ok());
    let response = result.unwrap();
    assert_eq!(response.id, 1);
    assert!(response.result.is_some());
}

#[tokio::test]
async fn test_transport_handles_list_tools_request() {
    // Arrange
    let transport = McpTransport::new();
    let request = McpRequest {
        id: json!(2),
        method: "tools/list".to_string(),
        params: json!({}),
    };

    // Act
    let result = transport.handle_request(request).await;

    // Assert
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.result.is_some());
    let result_data = response.result.unwrap();
    assert!(result_data["tools"].is_array());
}

#[tokio::test]
async fn test_transport_handles_tool_call() {
    // Arrange
    let transport = McpTransport::new();
    let request = McpRequest {
        id: json!(3),
        method: "tools/call".to_string(),
        params: json!({
            "name": "test_tool",
            "arguments": {"input": "test"}
        }),
    };

    // Act
    let result = transport.handle_request(request).await;

    // Assert
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.result.is_some());
}

#[tokio::test]
async fn test_transport_rejects_invalid_method() {
    // Arrange
    let transport = McpTransport::new();
    let request = McpRequest {
        id: json!(4),
        method: "invalid/method".to_string(),
        params: json!({}),
    };

    // Act
    let result = transport.handle_request(request).await;

    // Assert
    assert!(result.is_ok());
    let response = result.unwrap();
    assert!(response.error.is_some());
    let error = response.error.unwrap();
    assert_eq!(error.code, -32601); // Method not found
}

#[tokio::test]
async fn test_transport_serializes_response() {
    // Arrange
    let transport = McpTransport::new();
    let response = transport.create_response(1, json!({"result": "test"}));

    // Act
    let serialized = transport.serialize_response(&response);

    // Assert
    assert!(serialized.is_ok());
    let json_str = serialized.unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&json_str).unwrap();
    assert_eq!(parsed["jsonrpc"], "2.0");
    assert_eq!(parsed["id"], 1);
}

#[tokio::test]
async fn test_transport_deserializes_request() {
    // Arrange
    let transport = McpTransport::new();
    let json_str = r#"{"jsonrpc":"2.0","id":1,"method":"test","params":{}}"#;

    // Act
    let result = transport.deserialize_request(json_str);

    // Assert
    assert!(result.is_ok());
    let request = result.unwrap();
    assert_eq!(request.method, "test");
}
