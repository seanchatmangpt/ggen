//! Custom assertions for MCP A2A integration tests
//!
//! Provides specialized assertion helpers following Chicago TDD patterns
//! with clear, state-based verification.

use crate::mcp_a2a::{
    JsonRpcMessage, JsonRpcResponse, McpError, MockMcpServer, ErrorMode,
};

// ============================================================================
// RESPONSE ASSERTIONS
// ============================================================================

/// Assert that a response is successful (no error)
pub fn assert_response_success(response: &JsonRpcResponse) {
    assert!(
        response.error.is_none(),
        "Expected successful response, got error: {:?}",
        response.error
    );
}

/// Assert that a response has an error
pub fn assert_response_error(response: &JsonRpcResponse) {
    assert!(
        response.error.is_some(),
        "Expected error response, but got success"
    );
}

/// Assert that a response has a specific error code
pub fn assert_error_code(response: &JsonRpcResponse, expected_code: i32) {
    assert_response_error(response);
    let error = response.error.as_ref().unwrap();
    assert_eq!(
        error.code, expected_code,
        "Expected error code {}, got {}",
        expected_code, error.code
    );
}

/// Assert that a response contains a specific result field
pub fn assert_result_contains(response: &JsonRpcResponse, field: &str) {
    assert_response_success(response);
    let result = response.result.as_ref().unwrap();
    assert!(
        result.get(field).is_some(),
        "Expected result to contain field '{}'",
        field
    );
}

/// Assert that a result field has a specific value
pub fn assert_result_field_eq(response: &JsonRpcResponse, field: &str, expected: serde_json::Value) {
    assert_response_success(response);
    let result = response.result.as_ref().unwrap();
    let actual = result.get(field).unwrap_or(&serde_json::Value::Null);
    assert_eq!(
        actual, &expected,
        "Expected field '{}' to be {:?}, got {:?}",
        field, expected, actual
    );
}

// ============================================================================
// SERVER STATE ASSERTIONS
// ============================================================================

/// Assert that a server has a specific number of tools registered
pub async fn assert_tool_count(server: &MockMcpServer, expected: usize) {
    let count = server.list_tools().await.len();
    assert_eq!(
        count, expected,
        "Expected {} tools, got {}",
        expected, count
    );
}

/// Assert that a server has a specific tool registered
pub async fn assert_has_tool(server: &MockMcpServer, tool_name: &str) {
    assert!(
        server.has_tool(tool_name).await,
        "Expected server to have tool '{}'",
        tool_name
    );
}

/// Assert that a server does not have a specific tool
pub async fn assert_lacks_tool(server: &MockMcpServer, tool_name: &str) {
    assert!(
        !server.has_tool(tool_name).await,
        "Expected server to NOT have tool '{}'",
        tool_name
    );
}

/// Assert that message history has a specific length
pub async fn assert_history_length(server: &MockMcpServer, expected: usize) {
    let len = server.history_len().await;
    assert_eq!(
        len, expected,
        "Expected history length {}, got {}",
        expected, len
    );
}

/// Assert that a specific message exists in history
pub async fn assert_message_exists(server: &MockMcpServer, method: &str) {
    let messages = server.find_messages_by_method(method).await;
    assert!(
        !messages.is_empty(),
        "Expected to find message with method '{}' in history",
        method
    );
}

/// Assert that a message with a specific ID exists
pub async fn assert_message_with_id_exists(server: &MockMcpServer, id: &serde_json::Value) {
    let message = server.find_message_by_id(id).await;
    assert!(
        message.is_some(),
        "Expected to find message with id '{:?}' in history",
        id
    );
}

// ============================================================================
// ERROR ASSERTIONS
// ============================================================================

/// Assert that an error is a timeout error
pub fn assert_is_timeout_error(error: &McpError) {
    assert!(
        error.is_timeout(),
        "Expected timeout error, got {}",
        error.error_type()
    );
}

/// Assert that an error is a tool not found error
pub fn assert_is_tool_not_found(error: &McpError) {
    assert!(
        error.is_tool_not_found(),
        "Expected tool not found error, got {}",
        error.error_type()
    );
}

/// Assert that an error has a specific type
pub fn assert_error_type(error: &McpError, expected_type: &str) {
    assert_eq!(
        error.error_type(),
        expected_type,
        "Expected error type '{}', got '{}'",
        expected_type,
        error.error_type()
    );
}

// ============================================================================
// MESSAGE ASSERTIONS
// ============================================================================

/// Assert that a message has a specific method
pub fn assert_message_method(message: &JsonRpcMessage, expected_method: &str) {
    assert_eq!(
        message.method, expected_method,
        "Expected method '{}', got '{}'",
        expected_method, message.method
    );
}

/// Assert that a message has a specific parameter
pub fn assert_message_has_param(message: &JsonRpcMessage, param: &str) {
    assert!(
        message.params.get(param).is_some(),
        "Expected message to have parameter '{}'",
        param
    );
}

/// Assert that a message parameter has a specific value
pub fn assert_message_param_eq(
    message: &JsonRpcMessage,
    param: &str,
    expected: &serde_json::Value,
) {
    let actual = message.params.get(param).unwrap_or(&serde_json::Value::Null);
    assert_eq!(
        actual, expected,
        "Expected parameter '{}' to be {:?}, got {:?}",
        param, expected, actual
    );
}

// ============================================================================
// SEQUENCE ASSERTIONS
// ============================================================================

/// Assert that messages were received in a specific order
pub async fn assert_message_order(server: &MockMcpServer, expected_methods: &[&str]) {
    let history = server.get_message_history().await;
    assert_eq!(
        history.len(),
        expected_methods.len(),
        "Expected {} messages, got {}",
        expected_methods.len(),
        history.len()
    );

    for (i, (message, expected_method)) in history.iter().zip(expected_methods.iter()).enumerate()
    {
        assert_eq!(
            &message.method,
            expected_method,
            "Message {}: expected method '{}', got '{}'",
            i,
            expected_method,
            message.method
        );
    }
}

/// Assert that a specific method was called N times
pub async fn assert_method_call_count(server: &MockMcpServer, method: &str, expected_count: usize) {
    let messages = server.find_messages_by_method(method).await;
    assert_eq!(
        messages.len(),
        expected_count,
        "Expected method '{}' to be called {} times, got {}",
        method,
        expected_count,
        messages.len()
    );
}

// ============================================================================
// COMPOSITE ASSERTIONS
// ============================================================================

/// Assert a complete successful tool call workflow
pub async fn assert_successful_tool_call(
    server: &MockMcpServer,
    tool_name: &str,
    response: &JsonRpcResponse,
) {
    // Assert response is successful
    assert_response_success(response);

    // Assert tool was called
    assert_method_call_count(server, "tools/call", 1).await;

    // Assert result exists
    assert!(response.result.is_some());

    // Verify the tool exists on the server
    assert_has_tool(server, tool_name).await;
}

/// Assert a complete failed tool call workflow
pub async fn assert_failed_tool_call(
    server: &MockMcpServer,
    response: &JsonRpcResponse,
    expected_error_code: i32,
) {
    // Assert response is an error
    assert_response_error(response);

    // Assert specific error code
    assert_error_code(response, expected_error_code);

    // Assert tool call was attempted
    assert_method_call_count(server, "tools/call", 1).await;
}

/// Assert that server is in healthy state
pub async fn assert_server_healthy(server: &MockMcpServer) {
    let request = crate::mcp_a2a::JsonRpcMessage::health_check();
    let response = server.handle_request(request).await;
    assert_response_success(&response);

    let result = response.result.unwrap();
    assert_eq!(result.get("status").unwrap(), "healthy");
}

// ============================================================================
// BATCH ASSERTIONS
// ============================================================================

/// Assert multiple server states at once
pub async fn assert_server_state(
    server: &MockMcpServer,
    expected_tools: usize,
    expected_messages: usize,
    expected_requests: usize,
) {
    let stats = server.stats().await;
    assert_eq!(
        stats.tools_registered, expected_tools,
        "Expected {} tools, got {}",
        expected_tools, stats.tools_registered
    );
    assert_eq!(
        stats.messages_processed, expected_messages,
        "Expected {} messages, got {}",
        expected_messages, stats.messages_processed
    );
    assert_eq!(
        stats.requests_handled, expected_requests,
        "Expected {} requests, got {}",
        expected_requests, stats.requests_handled
    );
}

/// Assert that error simulation is configured
pub async fn assert_error_mode(server: &MockMcpServer, expected_mode: ErrorMode) {
    let stats = server.stats().await;
    assert_eq!(
        stats.error_mode, expected_mode,
        "Expected error mode {:?}, got {:?}",
        expected_mode, stats.error_mode
    );
}

// ============================================================================
// VALUE ASSERTIONS
// ============================================================================

/// Assert that a JSON value is a string matching the expected value
pub fn assert_json_string(value: &serde_json::Value, expected: &str) {
    assert_eq!(
        value.as_str(),
        Some(expected),
        "Expected string '{}', got {:?}",
        expected, value
    );
}

/// Assert that a JSON value is a number matching the expected value
pub fn assert_json_number(value: &serde_json::Value, expected: f64) {
    let actual = value.as_f64().unwrap_or(f64::NAN);
    assert!(
        (actual - expected).abs() < 0.001,
        "Expected number {}, got {}",
        expected, actual
    );
}

/// Assert that a JSON value is a boolean matching the expected value
pub fn assert_json_bool(value: &serde_json::Value, expected: bool) {
    assert_eq!(
        value.as_bool(),
        Some(expected),
        "Expected bool {}, got {:?}",
        expected, value
    );
}

/// Assert that a JSON value is an array with specific length
pub fn assert_json_array_len(value: &serde_json::Value, expected_len: usize) {
    let arr = value
        .as_array()
        .expect(&format!("Expected array, got {:?}", value));
    assert_eq!(
        arr.len(),
        expected_len,
        "Expected array of length {}, got {}",
        expected_len,
        arr.len()
    );
}

/// Assert that a JSON value is an object with a specific field
pub fn assert_json_has_field(value: &serde_json::Value, field: &str) {
    let obj = value
        .as_object()
        .expect(&format!("Expected object, got {:?}", value));
    assert!(
        obj.contains_key(field),
        "Expected object to have field '{}', got {:?}",
        field, obj
    );
}

// ============================================================================
// MACRO-LIKE HELPERS
// ============================================================================

/// Assert all common success conditions for a response
pub fn assert_response_ok_with_field(
    response: &JsonRpcResponse,
    field: &str,
    expected_value: &serde_json::Value,
) {
    assert_response_success(response);
    assert_result_contains(response, field);
    assert_result_field_eq(response, field, expected_value.clone());
}

/// Assert all conditions for a tool not found error
pub fn assert_tool_not_found(response: &JsonRpcResponse, tool_name: &str) {
    assert_response_error(response);
    let error = response.error.as_ref().unwrap();
    assert!(error.is_tool_not_found());
    assert!(
        error.message.contains(tool_name),
        "Expected error message to contain '{}', got '{}'",
        tool_name,
        error.message
    );
}

// ============================================================================
// TEST HELPERS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mcp_a2a::fixtures::echo_server;

    #[tokio::test]
    async fn test_assert_response_success() {
        let response = JsonRpcResponse::ok(json!(1), json!({"ok": true}));
        assert_response_success(&response);
    }

    #[tokio::test]
    #[should_panic(expected = "Expected successful response")]
    async fn test_assert_response_success_fails() {
        let response = JsonRpcResponse::error(json!(1), McpError::internal_error("test"));
        assert_response_success(&response);
    }

    #[tokio::test]
    async fn test_assert_tool_count() {
        let server = echo_server().await;
        assert_tool_count(&server, 1).await;
    }

    #[tokio::test]
    async fn test_assert_has_tool() {
        let server = echo_server().await;
        assert_has_tool(&server, "echo").await;
    }

    #[tokio::test]
    #[should_panic(expected = "Expected server to have tool")]
    async fn test_assert_has_tool_fails() {
        let server = echo_server().await;
        assert_has_tool(&server, "nonexistent").await;
    }

    #[tokio::test]
    async fn test_assert_history_length() {
        let server = echo_server().await;
        let request = crate::mcp_a2a::JsonRpcMessage::tools_list();
        server.handle_request(request).await;
        assert_history_length(&server, 1).await;
    }

    #[tokio::test]
    async fn test_assert_method_call_count() {
        let server = echo_server().await;
        let request = crate::mcp_a2a::JsonRpcMessage::tools_list();
        server.handle_request(request).await;
        server.handle_request(request).await;
        assert_method_call_count(&server, "tools/list", 2).await;
    }

    #[tokio::test]
    async fn test_assert_server_healthy() {
        let server = echo_server().await;
        assert_server_healthy(&server).await;
    }

    #[tokio::test]
    async fn test_assert_json_string() {
        let value = json!("test");
        assert_json_string(&value, "test");
    }

    #[tokio::test]
    async fn test_assert_json_number() {
        let value = json!(42.5);
        assert_json_number(&value, 42.5);
    }

    #[tokio::test]
    async fn test_assert_json_bool() {
        let value = json!(true);
        assert_json_bool(&value, true);
    }

    #[tokio::test]
    async fn test_assert_json_array_len() {
        let value = json!([1, 2, 3]);
        assert_json_array_len(&value, 3);
    }

    #[tokio::test]
    async fn test_assert_json_has_field() {
        let value = json!({"test": "value"});
        assert_json_has_field(&value, "test");
    }
}
