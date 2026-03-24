//! MCP protocol types for JSON-RPC 2.0

use serde::{Deserialize, Serialize};

/// JSON-RPC 2.0 request base
#[derive(Debug, Deserialize)]
pub struct JsonRpcRequest<T> {
    pub jsonrpc: String,
    pub id: i64,
    pub method: String,
    pub params: T,
}

/// JSON-RPC 2.0 response
#[derive(Debug, Serialize)]
pub struct JsonRpcResponse<T> {
    pub jsonrpc: String,
    pub id: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<T>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

#[derive(Debug, Serialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
}

/// MCP request types
#[derive(Debug, Deserialize)]
#[serde(tag = "method", content = "params")]
pub enum McpRequest {
    #[serde(rename = "initialize")]
    Initialize {
        request_id: i64,
        params: InitializeParams,
    },
    #[serde(rename = "tools/list")]
    ListTools { request_id: i64 },
    #[serde(rename = "tools/call")]
    CallTool {
        request_id: i64,
        name: String,
        arguments: serde_json::Value,
    },
    #[serde(rename = "ping")]
    Ping { request_id: i64 },
}

#[derive(Debug, Deserialize)]
pub struct InitializeParams {
    pub protocol_version: String,
    pub capabilities: ClientCapabilities,
    pub client_info: ClientInfo,
}

#[derive(Debug, Deserialize, Default)]
pub struct ClientCapabilities {
    #[serde(default)]
    pub streaming: bool,
}

#[derive(Debug, Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: String,
}

/// MCP response types
#[derive(Debug, Serialize)]
pub struct McpResponse {
    pub jsonrpc: String,
    pub request_id: i64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<McpError>,
}

#[derive(Debug, Serialize)]
pub struct McpError {
    pub code: i32,
    pub message: String,
}

impl McpResponse {
    pub fn initialize_success(request_id: i64) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            request_id,
            result: Some(serde_json::json!({
                "protocolVersion": "2024-11-05",
                "serverInfo": {
                    "name": "ggen-mcp-server",
                    "version": env!("CARGO_PKG_VERSION")
                },
                "capabilities": {
                    "tools": {},
                }
            })),
            error: None,
        }
    }

    pub fn tools_list_success(request_id: i64, tools: Vec<serde_json::Value>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            request_id,
            result: Some(serde_json::json!({ "tools": tools })),
            error: None,
        }
    }

    pub fn tool_call_success(request_id: i64, content: ToolContent) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            request_id,
            result: Some(serde_json::json!({
                "content": [content]
            })),
            error: None,
        }
    }

    pub fn error(request_id: i64, code: i32, message: &str) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            request_id,
            result: None,
            error: Some(McpError {
                code,
                message: message.to_string(),
            }),
        }
    }

    pub fn ping_success(request_id: i64) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            request_id,
            result: Some(serde_json::json!({})),
            error: None,
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum ToolContent {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "image")]
    Image { data: String, mime_type: String },
    #[serde(rename = "resource")]
    Resource { uri: String },
}
