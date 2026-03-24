//! MCP stdio transport layer
//!
//! Implements JSON-RPC 2.0 protocol over stdin/stdout for Model Context Protocol.
//! Provides bidirectional communication between MCP clients and the ggen A2A server.

use crate::error::{A2aMcpError, A2aMcpResult};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::io::{self, Write};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::sync::RwLock;
use tracing::{debug, error, info};

/// JSON-RPC 2.0 request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpRequest {
    /// JSON-RPC version (must be "2.0")
    #[serde(default = "default_jsonrpc")]
    pub jsonrpc: String,
    /// Request identifier
    pub id: Value,
    /// Method name to invoke
    pub method: String,
    /// Method parameters
    #[serde(default)]
    pub params: Value,
}

fn default_jsonrpc() -> String {
    "2.0".to_string()
}

/// JSON-RPC 2.0 response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpResponse {
    /// JSON-RPC version
    #[serde(default = "default_jsonrpc")]
    pub jsonrpc: String,
    /// Request identifier (must match request)
    pub id: Value,
    /// Result on success
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    /// Error on failure
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<McpError>,
}

/// JSON-RPC 2.0 error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpError {
    /// Error code
    pub code: i32,
    /// Error message
    pub message: String,
    /// Additional error data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// MCP JSON-RPC error codes
#[derive(Debug, Clone, Copy)]
pub enum McpErrorCode {
    /// Invalid JSON was received
    ParseError = -32700,
    /// JSON is not a valid request object
    InvalidRequest = -32600,
    /// Method does not exist
    MethodNotFound = -32601,
    /// Invalid method parameters
    InvalidParams = -32602,
    /// Internal error
    InternalError = -32603,
}

impl McpErrorCode {
    /// Get the error code as i32
    pub fn code(&self) -> i32 {
        *self as i32
    }

    /// Get the default message for this error code
    pub fn message(&self) -> &'static str {
        match self {
            Self::ParseError => "Parse error",
            Self::InvalidRequest => "Invalid Request",
            Self::MethodNotFound => "Method not found",
            Self::InvalidParams => "Invalid params",
            Self::InternalError => "Internal error",
        }
    }
}

/// MCP transport handler for stdio communication
pub struct McpTransport {
    /// Registered tools
    tools: Arc<RwLock<Vec<McpToolDefinition>>>,
    /// Server capabilities
    capabilities: McpServerCapabilities,
    /// Server info
    server_info: McpServerInfo,
}

/// Tool definition for MCP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolDefinition {
    /// Tool name
    pub name: String,
    /// Tool description
    pub description: String,
    /// JSON Schema for input parameters
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

/// Server capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerCapabilities {
    /// Tools capability
    pub tools: McpToolsCapability,
}

/// Tools capability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolsCapability {
    /// Whether tools can be listed
    #[serde(rename = "listChanged", skip_serializing_if = "Option::is_none")]
    pub list_changed: Option<bool>,
}

/// Server information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerInfo {
    /// Server name
    pub name: String,
    /// Server version
    pub version: String,
}

impl Default for McpToolsCapability {
    fn default() -> Self {
        Self { list_changed: None }
    }
}

impl McpTransport {
    /// Create a new MCP transport
    pub fn new() -> Self {
        Self {
            tools: Arc::new(RwLock::new(Vec::new())),
            capabilities: McpServerCapabilities {
                tools: McpToolsCapability::default(),
            },
            server_info: McpServerInfo {
                name: "ggen-a2a-mcp".to_string(),
                version: env!("CARGO_PKG_VERSION").to_string(),
            },
        }
    }

    /// Register a tool
    pub async fn register_tool(&self, tool: McpToolDefinition) -> A2aMcpResult<()> {
        let mut tools = self.tools.write().await;
        tools.push(tool);
        Ok(())
    }

    /// Create a success response
    pub fn create_response(&self, id: Value, result: Value) -> McpResponse {
        McpResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: Some(result),
            error: None,
        }
    }

    /// Create an error response
    pub fn create_error(&self, id: Value, code: McpErrorCode, message: &str) -> McpResponse {
        McpResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: None,
            error: Some(McpError {
                code: code.code(),
                message: message.to_string(),
                data: None,
            }),
        }
    }

    /// Handle an incoming MCP request
    pub async fn handle_request(&self, request: McpRequest) -> A2aMcpResult<McpResponse> {
        debug!("Handling MCP request: {}", request.method);

        match request.method.as_str() {
            "initialize" => self.handle_initialize(request.id, request.params).await,
            "initialized" => self.handle_initialized(request.id).await,
            "tools/list" => self.handle_list_tools(request.id).await,
            "tools/call" => self.handle_tool_call(request.id, request.params).await,
            "shutdown" => self.handle_shutdown(request.id).await,
            _ => Ok(self.create_error(
                request.id,
                McpErrorCode::MethodNotFound,
                &format!("Method '{}' not found", request.method),
            )),
        }
    }

    /// Handle initialize request
    async fn handle_initialize(&self, id: Value, _params: Value) -> A2aMcpResult<McpResponse> {
        info!("Handling initialize request");

        let result = json!({
            "protocolVersion": "2024-11-05",
            "capabilities": self.capabilities,
            "serverInfo": self.server_info
        });

        Ok(self.create_response(id, result))
    }

    /// Handle initialized notification
    async fn handle_initialized(&self, id: Value) -> A2aMcpResult<McpResponse> {
        debug!("Handling initialized notification");
        // Notifications don't expect a response, but return empty result
        Ok(self.create_response(id, json!({})))
    }

    /// Handle tools/list request
    async fn handle_list_tools(&self, id: Value) -> A2aMcpResult<McpResponse> {
        debug!("Handling tools/list request");

        let tools = self.tools.read().await;
        let tools_array: Vec<Value> = tools
            .iter()
            .map(|t| {
                json!({
                    "name": t.name,
                    "description": t.description,
                    "inputSchema": t.input_schema
                })
            })
            .collect();

        let result = json!({ "tools": tools_array });
        Ok(self.create_response(id, result))
    }

    /// Handle tools/call request
    async fn handle_tool_call(&self, id: Value, params: Value) -> A2aMcpResult<McpResponse> {
        debug!("Handling tools/call request");

        let tool_name = params
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or_else(|| A2aMcpError::InvalidToolMethod("Missing tool name".to_string()))?;

        let arguments = params
            .get("arguments")
            .cloned()
            .unwrap_or_else(|| json!({}));

        // Find the tool
        let tools = self.tools.read().await;
        let tool = tools
            .iter()
            .find(|t| t.name == tool_name)
            .ok_or_else(|| A2aMcpError::TaskNotFound(format!("Tool '{}' not found", tool_name)))?;

        // Execute the tool
        let result = self.execute_tool(tool, arguments).await?;

        let response = json!({
            "content": [
                {
                    "type": "text",
                    "text": result
                }
            ]
        });

        Ok(self.create_response(id, response))
    }

    /// Execute a tool
    async fn execute_tool(
        &self, _tool: &McpToolDefinition, _arguments: Value,
    ) -> A2aMcpResult<String> {
        // For now, return a placeholder response
        // In a full implementation, this would dispatch to the actual tool handler
        Ok("Tool executed successfully".to_string())
    }

    /// Handle shutdown request
    async fn handle_shutdown(&self, id: Value) -> A2aMcpResult<McpResponse> {
        info!("Handling shutdown request");
        Ok(self.create_response(id, json!({})))
    }

    /// Serialize a response to JSON
    pub fn serialize_response(&self, response: &McpResponse) -> A2aMcpResult<String> {
        serde_json::to_string(response).map_err(A2aMcpError::from)
    }

    /// Deserialize a request from JSON
    pub fn deserialize_request(&self, json_str: &str) -> A2aMcpResult<McpRequest> {
        serde_json::from_str(json_str).map_err(A2aMcpError::from)
    }

    /// Run the stdio server loop
    pub async fn run_stdio(&self) -> A2aMcpResult<()> {
        info!("Starting MCP stdio server");

        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let reader = BufReader::new(stdin);
        let mut lines = reader.lines();
        let mut stdout_lock = stdout;

        while let Ok(Some(line)) = lines.next_line().await {
            debug!("Received request: {}", line);

            let request = match self.deserialize_request(&line) {
                Ok(req) => req,
                Err(e) => {
                    error!("Failed to parse request: {}", e);
                    let error_response =
                        self.create_error(json!(null), McpErrorCode::ParseError, &e.to_string());
                    let error_json = self.serialize_response(&error_response)?;
                    use tokio::io::AsyncWriteExt;
                    stdout_lock.write_all(error_json.as_bytes()).await?;
                    stdout_lock.write_all(b"\n").await?;
                    stdout_lock.flush().await?;
                    continue;
                }
            };

            let response = match self.handle_request(request).await {
                Ok(resp) => resp,
                Err(e) => {
                    error!("Failed to handle request: {}", e);
                    self.create_error(json!(null), McpErrorCode::InternalError, &e.to_string())
                }
            };

            let response_json = self.serialize_response(&response)?;
            use tokio::io::AsyncWriteExt;
            stdout_lock.write_all(response_json.as_bytes()).await?;
            stdout_lock.write_all(b"\n").await?;
            stdout_lock.flush().await?;
        }

        info!("MCP stdio server shutting down");
        Ok(())
    }
}

impl Default for McpTransport {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_code_messages() {
        assert_eq!(McpErrorCode::ParseError.message(), "Parse error");
        assert_eq!(McpErrorCode::InvalidRequest.message(), "Invalid Request");
        assert_eq!(McpErrorCode::MethodNotFound.message(), "Method not found");
    }

    #[test]
    fn test_error_code_values() {
        assert_eq!(McpErrorCode::ParseError.code(), -32700);
        assert_eq!(McpErrorCode::InvalidRequest.code(), -32600);
        assert_eq!(McpErrorCode::MethodNotFound.code(), -32601);
    }

    #[test]
    fn test_tool_definition_serialization() {
        let tool = McpToolDefinition {
            name: "test_tool".to_string(),
            description: "A test tool".to_string(),
            input_schema: json!({"type": "object"}),
        };

        let serialized = serde_json::to_string(&tool).unwrap();
        let parsed: Value = serde_json::from_str(&serialized).unwrap();

        assert_eq!(parsed["name"], "test_tool");
        assert_eq!(parsed["description"], "A test tool");
    }

    #[test]
    fn test_response_serialization() {
        let response = McpResponse {
            jsonrpc: "2.0".to_string(),
            id: json!(1),
            result: Some(json!({"status": "ok"})),
            error: None,
        };

        let serialized = serde_json::to_string(&response).unwrap();
        let parsed: Value = serde_json::from_str(&serialized).unwrap();

        assert_eq!(parsed["jsonrpc"], "2.0");
        assert_eq!(parsed["id"], 1);
        assert!(parsed["error"].is_null());
    }
}
