//! MCP Server Types and Structures
//!
//! Phase 4: These types will be fully utilized in the MCP server implementation.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// MCP Tool Definition
///
/// Represents a tool that can be invoked through the MCP interface.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPToolDefinition {
    /// Tool name (unique identifier)
    pub name: String,

    /// Human-readable description
    pub description: String,

    /// JSON Schema for input parameters
    #[serde(default)]
    pub input_schema: serde_json::Value,

    /// JSON Schema for output (Phase 4)
    #[serde(default)]
    pub output_schema: Option<serde_json::Value>,

    /// Tool metadata (Phase 4)
    #[serde(default)]
    pub metadata: Option<ToolMetadata>,
}

/// Tool Metadata
///
/// Extended information about a tool for discovery and documentation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolMetadata {
    /// Semantic version of the tool
    pub version: String,

    /// Tool author or organization
    #[serde(default)]
    pub author: Option<String>,

    /// Category or domain (e.g., "finance", "weather")
    #[serde(default)]
    pub category: Option<String>,

    /// Tags for discovery and filtering
    #[serde(default)]
    pub tags: Vec<String>,

    /// URL to tool documentation
    #[serde(default)]
    pub documentation_url: Option<String>,

    /// Whether this tool is deprecated
    #[serde(default)]
    pub deprecated: bool,

    /// Custom metadata key-value pairs
    #[serde(default)]
    pub custom: HashMap<String, serde_json::Value>,
}

/// MCP Request (JSON-RPC 2.0)
///
/// Standard JSON-RPC 2.0 request format for MCP methods.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPRequest {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,

    /// Request ID (any JSON value, typically integer or string)
    pub id: serde_json::Value,

    /// Method name (e.g., "tools/list", "tools/call")
    pub method: String,

    /// Method parameters
    #[serde(default)]
    pub params: serde_json::Value,
}

impl MCPRequest {
    /// Create a new request to list tools
    pub fn list_tools() -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "tools/list".to_string(),
            params: serde_json::json!({}),
        }
    }

    /// Create a new request to call a tool
    pub fn call_tool(name: impl Into<String>, arguments: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "tools/call".to_string(),
            params: serde_json::json!({
                "name": name.into(),
                "arguments": arguments,
            }),
        }
    }

    /// Create a new request to validate tool input
    pub fn validate_tool(name: impl Into<String>, arguments: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "tools/validate".to_string(),
            params: serde_json::json!({
                "name": name.into(),
                "arguments": arguments,
            }),
        }
    }
}

/// MCP Response (JSON-RPC 2.0)
///
/// Standard JSON-RPC 2.0 response format.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPResponse {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,

    /// Request ID (matches request)
    pub id: serde_json::Value,

    /// Result data (if successful)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,

    /// Error (if failed)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error: Option<MCPError>,
}

impl MCPResponse {
    /// Create a successful response
    pub fn ok(id: serde_json::Value, result: serde_json::Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    /// Create an error response
    pub fn error(id: serde_json::Value, error: MCPError) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(error),
        }
    }
}

/// MCP Error (JSON-RPC 2.0)
///
/// Standard JSON-RPC 2.0 error format with extended ggen-specific data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPError {
    /// Error code (see ErrorCode)
    pub code: i32,

    /// Error message
    pub message: String,

    /// Additional error data (Phase 4)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<MCPErrorData>,
}

impl MCPError {
    /// Create a parse error
    pub fn parse_error() -> Self {
        Self {
            code: -32700,
            message: "Parse error".to_string(),
            data: None,
        }
    }

    /// Create an invalid request error
    pub fn invalid_request(message: impl Into<String>) -> Self {
        Self {
            code: -32600,
            message: message.into(),
            data: None,
        }
    }

    /// Create a method not found error
    pub fn method_not_found(method: impl Into<String>) -> Self {
        Self {
            code: -32601,
            message: format!("Method not found: {}", method.into()),
            data: None,
        }
    }

    /// Create an invalid params error
    pub fn invalid_params(message: impl Into<String>) -> Self {
        Self {
            code: -32602,
            message: message.into(),
            data: None,
        }
    }

    /// Create an internal error
    pub fn internal_error(message: impl Into<String>) -> Self {
        Self {
            code: -32603,
            message: message.into(),
            data: None,
        }
    }

    /// Create a validation error
    pub fn validation_error(field: String, reason: String) -> Self {
        Self {
            code: -32001,
            message: "Validation error".to_string(),
            data: Some(MCPErrorData {
                error_type: "ValidationError".to_string(),
                field: Some(field),
                reason: Some(reason),
                ..Default::default()
            }),
        }
    }

    /// Create a tool not found error
    pub fn tool_not_found(tool_name: String) -> Self {
        Self {
            code: -32002,
            message: format!("Tool not found: {}", tool_name),
            data: None,
        }
    }

    /// Create a tool execution error
    pub fn tool_execution_error(_tool_name: String, reason: String) -> Self {
        Self {
            code: -32003,
            message: "Tool execution failed".to_string(),
            data: Some(MCPErrorData {
                error_type: "ToolExecution".to_string(),
                reason: Some(reason),
                ..Default::default()
            }),
        }
    }

    /// Create an authentication error
    pub fn auth_error() -> Self {
        Self {
            code: -32004,
            message: "Authentication failed".to_string(),
            data: None,
        }
    }

    /// Create an authorization error
    pub fn authz_error(resource: impl Into<String>) -> Self {
        Self {
            code: -32005,
            message: format!("Access denied to resource: {}", resource.into()),
            data: None,
        }
    }
}

/// Extended Error Data for ggen-specific information
///
/// This structure is used in Phase 4 to provide detailed error information.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MCPErrorData {
    /// Error type (e.g., "ValidationError", "ToolExecution")
    pub error_type: String,

    /// Field name (if applicable)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub field: Option<String>,

    /// Detailed reason
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,

    /// Trace ID for debugging
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_id: Option<String>,

    /// Suggestions for fixing the error
    #[serde(default)]
    pub suggestions: Vec<String>,

    /// URL to documentation
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub documentation_url: Option<String>,
}

/// JSON Schema Representation
///
/// Phase 4: This will be extended with full JSON Schema support.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonSchema {
    #[serde(rename = "type")]
    pub schema_type: String,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub properties: Option<HashMap<String, serde_json::Value>>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub required: Option<Vec<String>>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    #[serde(flatten)]
    pub additional: HashMap<String, serde_json::Value>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_request_list_tools() {
        let req = MCPRequest::list_tools();
        assert_eq!(req.jsonrpc, "2.0");
        assert_eq!(req.method, "tools/list");
    }

    #[test]
    fn test_mcp_request_call_tool() {
        let req = MCPRequest::call_tool("TestTool", serde_json::json!({"param": "value"}));
        assert_eq!(req.method, "tools/call");

        let params = &req.params;
        assert_eq!(params["name"], "TestTool");
        assert_eq!(params["arguments"]["param"], "value");
    }

    #[test]
    fn test_mcp_response_ok() {
        let resp = MCPResponse::ok(serde_json::json!(1), serde_json::json!({"result": "ok"}));
        assert_eq!(resp.jsonrpc, "2.0");
        assert!(resp.result.is_some());
        assert!(resp.error.is_none());
    }

    #[test]
    fn test_mcp_error_validation() {
        let err = MCPError::validation_error("ticker".to_string(), "Invalid format".to_string());
        assert_eq!(err.code, -32001);
        assert!(err.data.is_some());
        assert_eq!(err.data.unwrap().field.unwrap(), "ticker");
    }

    #[test]
    fn test_mcp_tool_definition_serialization() {
        let tool = MCPToolDefinition {
            name: "TestTool".to_string(),
            description: "A test tool".to_string(),
            input_schema: serde_json::json!({"type": "object"}),
            output_schema: None,
            metadata: None,
        };

        let json = serde_json::to_string(&tool).unwrap();
        assert!(json.contains("TestTool"));

        let deserialized: MCPToolDefinition = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized.name, "TestTool");
    }
}
