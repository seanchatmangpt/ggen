//! MCP Transport Layer
//!
//! This module provides a unified transport abstraction for communicating with
//! Model Context Protocol (MCP) servers over different mechanisms:
//!
//! - **StdioTransport**: For local MCP servers via stdin/stdout (Claude Desktop pattern)
//! - **HttpTransport**: For JSON-RPC over HTTP
//!
//! All transports implement the [`McpTransport`] trait for unified operations.

pub mod a2a;
pub mod error;
pub mod factory;
pub mod http;
pub mod stdio;

pub use a2a::{A2AConfig, A2ATransport, A2ATransportBuilder};
pub use error::{TransportError, TransportResult};
pub use factory::{TransportConfig, TransportFactory, TransportType};
pub use http::{HttpConfig, HttpSseTransport, HttpTransport, HttpTransportBuilder};
pub use stdio::{StdioConfig, StdioTransport, StdioTransportBuilder};

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

/// JSON-RPC 2.0 request identifier
///
/// Can be either a string, number, or null (for notifications).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(untagged)]
pub enum RequestId {
    /// Null ID (used for notifications)
    Null,
    /// String identifier
    String(String),
    /// Numeric identifier
    Number(u64),
}

impl RequestId {
    /// Generate a new unique string ID using UUID
    pub fn new() -> Self {
        RequestId::String(uuid::Uuid::new_v4().to_string())
    }

    /// Generate a new numeric ID
    pub fn new_number(id: u64) -> Self {
        RequestId::Number(id)
    }

    /// Check if this is a notification (null ID)
    pub fn is_notification(&self) -> bool {
        matches!(self, RequestId::Null)
    }
}

impl Default for RequestId {
    fn default() -> Self {
        Self::Null
    }
}

impl From<String> for RequestId {
    fn from(s: String) -> Self {
        RequestId::String(s)
    }
}

impl From<&str> for RequestId {
    fn from(s: &str) -> Self {
        RequestId::String(s.to_string())
    }
}

impl From<u64> for RequestId {
    fn from(n: u64) -> Self {
        RequestId::Number(n)
    }
}

impl From<Option<String>> for RequestId {
    fn from(opt: Option<String>) -> Self {
        match opt {
            Some(s) => RequestId::String(s),
            None => RequestId::Null,
        }
    }
}

/// JSON-RPC 2.0 error object
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    /// The error code
    pub code: i32,
    /// A short description of the error
    pub message: String,
    /// Additional error data (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<JsonValue>,
}

impl JsonRpcError {
    /// Create a new JSON-RPC error
    pub fn new(code: i32, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            data: None,
        }
    }

    /// Create an error with additional data
    pub fn with_data(code: i32, message: impl Into<String>, data: JsonValue) -> Self {
        Self {
            code,
            message: message.into(),
            data: Some(data),
        }
    }
}

impl From<TransportError> for JsonRpcError {
    fn from(err: TransportError) -> Self {
        JsonRpcError {
            code: err.error_code(),
            message: err.to_string(),
            data: Some(err.to_jsonrpc_error()),
        }
    }
}

/// JSON-RPC 2.0 request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcRequest {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,
    /// Request identifier
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<RequestId>,
    /// Method name to invoke
    pub method: String,
    /// Method parameters (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<JsonValue>,
}

impl JsonRpcRequest {
    /// Create a new JSON-RPC request
    pub fn new(method: impl Into<String>, params: Option<JsonValue>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: Some(RequestId::new()),
            method: method.into(),
            params,
        }
    }

    /// Create a notification (request without ID)
    pub fn notification(method: impl Into<String>, params: Option<JsonValue>) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: None,
            method: method.into(),
            params,
        }
    }

    /// Create a request with a specific ID
    pub fn with_id(
        method: impl Into<String>,
        params: Option<JsonValue>,
        id: RequestId,
    ) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: Some(id),
            method: method.into(),
            params,
        }
    }

    /// Check if this is a notification
    pub fn is_notification(&self) -> bool {
        self.id.as_ref().map(|id| id.is_notification()).unwrap_or(false)
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> TransportResult<String> {
        serde_json::to_string(self).map_err(TransportError::from)
    }

    /// Serialize to JSON bytes
    pub fn to_json_bytes(&self) -> TransportResult<Vec<u8>> {
        serde_json::to_vec(self).map_err(TransportError::from)
    }

    /// Parse from JSON string
    pub fn from_json(json: impl AsRef<str>) -> TransportResult<Self> {
        serde_json::from_str(json.as_ref()).map_err(TransportError::from)
    }
}

/// JSON-RPC 2.0 response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    /// JSON-RPC version (always "2.0")
    pub jsonrpc: String,
    /// Request identifier (matches the request)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<RequestId>,
    /// Result value (on success)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<JsonValue>,
    /// Error object (on failure)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

impl JsonRpcResponse {
    /// Create a successful response
    pub fn success(id: RequestId, result: JsonValue) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: Some(id),
            result: Some(result),
            error: None,
        }
    }

    /// Create an error response
    pub fn error(id: RequestId, error: JsonRpcError) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id: Some(id),
            result: None,
            error: Some(error),
        }
    }

    /// Check if response is successful
    pub fn is_success(&self) -> bool {
        self.error.is_none() && self.result.is_some()
    }

    /// Check if response is an error
    pub fn is_error(&self) -> bool {
        self.error.is_some()
    }

    /// Get the result, converting to error if this is an error response
    pub fn into_result(self) -> TransportResult<JsonValue> {
        match (self.result, self.error) {
            (Some(result), None) => Ok(result),
            (None, Some(error)) => Err(TransportError::json_rpc_with_data(
                error.code,
                error.message,
                error.data.unwrap_or(JsonValue::Null),
            )),
            _ => Err(TransportError::Internal(
                "Invalid JSON-RPC response: missing both result and error".to_string(),
            )),
        }
    }

    /// Parse from JSON string
    pub fn from_json(json: impl AsRef<str>) -> TransportResult<Self> {
        serde_json::from_str(json.as_ref()).map_err(TransportError::from)
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> TransportResult<String> {
        serde_json::to_string(self).map_err(TransportError::from)
    }
}

/// MCP protocol request type
///
/// Common MCP methods used across different server implementations.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum McpMethod {
    /// Initialize the MCP session
    Initialize,
    /// List available tools
    ToolsList,
    /// Call a specific tool
    ToolsCall,
    /// List available resources
    ResourcesList,
    /// Read a resource
    ResourcesRead,
    /// List available prompts
    PromptsList,
    /// Get a prompt
    PromptsGet,
    /// Server health check
    Ping,
    /// Custom method
    Custom(String),
}

impl McpMethod {
    /// Get the method name as a string
    pub fn as_str(&self) -> &str {
        match self {
            McpMethod::Initialize => "initialize",
            McpMethod::ToolsList => "tools/list",
            McpMethod::ToolsCall => "tools/call",
            McpMethod::ResourcesList => "resources/list",
            McpMethod::ResourcesRead => "resources/read",
            McpMethod::PromptsList => "prompts/list",
            McpMethod::PromptsGet => "prompts/get",
            McpMethod::Ping => "ping",
            McpMethod::Custom(s) => s.as_str(),
        }
    }

    /// Parse a method string into McpMethod
    pub fn from_str(s: impl AsRef<str>) -> Self {
        match s.as_ref() {
            "initialize" => McpMethod::Initialize,
            "tools/list" => McpMethod::ToolsList,
            "tools/call" => McpMethod::ToolsCall,
            "resources/list" => McpMethod::ResourcesList,
            "resources/read" => McpMethod::ResourcesRead,
            "prompts/list" => McpMethod::PromptsList,
            "prompts/get" => McpMethod::PromptsGet,
            "ping" => McpMethod::Ping,
            s => McpMethod::Custom(s.to_string()),
        }
    }
}

impl From<&str> for McpMethod {
    fn from(s: &str) -> Self {
        McpMethod::from_str(s)
    }
}

impl From<String> for McpMethod {
    fn from(s: String) -> Self {
        McpMethod::from_str(s)
    }
}

impl AsRef<str> for McpMethod {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Unified MCP transport trait
///
/// All MCP transport implementations (stdio, HTTP, etc.) implement this trait
/// to provide a consistent interface for MCP protocol communication.
#[async_trait]
pub trait McpTransport: Send + Sync {
    /// Send a JSON-RPC request and wait for the response
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse>;

    /// Send a notification (no response expected)
    async fn send_notification(&self, notification: JsonRpcRequest) -> TransportResult<()>;

    /// Check if the transport is connected
    fn is_connected(&self) -> bool;

    /// Close the transport connection
    async fn close(&self) -> TransportResult<()>;
}

/// Extension trait with convenience methods for MCP operations
///
/// These are implemented for all McpTransport implementations automatically.
pub trait McpTransportExt: McpTransport {
    /// Initialize the MCP session
    fn initialize<'life0, 'life1, 'async_trait>(
        &'life0 self,
        capabilities: JsonValue,
    ) -> core::pin::Pin<Box<
        dyn core::future::Future<Output = TransportResult<JsonValue>> + core::marker::Send + 'async_trait,
    >>
    where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        Box::pin(async move {
            let request = JsonRpcRequest::new(
                McpMethod::Initialize.as_str(),
                Some(serde_json::json!({
                    "protocolVersion": "2024-11-05",
                    "capabilities": capabilities,
                    "clientInfo": {
                        "name": "rig-mcp",
                        "version": env!("CARGO_PKG_VERSION"),
                    }
                })),
            );

            let response = self.send_request(request).await?;
            response.into_result()
        })
    }

    /// List available tools from the MCP server
    fn list_tools<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<dyn core::future::Future<Output = TransportResult<Vec<Tool>>>
                + core::marker::Send
                + 'async_trait>,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        Box::pin(async move {
            let request = JsonRpcRequest::new(McpMethod::ToolsList.as_str(), None);
            let response = self.send_request(request).await?;
            let result = response.into_result()?;

            serde_json::from_value(result).map_err(TransportError::from)
        })
    }

    /// Call a specific tool
    fn call_tool<'life0, 'life1, 'async_trait>(
        &'life0 self,
        name: &'life1 str,
        arguments: JsonValue,
    ) -> core::pin::Pin<
        Box<dyn core::future::Future<Output = TransportResult<ToolResult>>
                + core::marker::Send
                + 'async_trait>,
    >
    where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        Box::pin(async move {
            let params = serde_json::json!({
                "name": name,
                "arguments": arguments,
            });

            let request = JsonRpcRequest::new(McpMethod::ToolsCall.as_str(), Some(params));
            let response = self.send_request(request).await?;
            let result = response.into_result()?;

            serde_json::from_value(result).map_err(TransportError::from)
        })
    }

    /// List available resources
    fn list_resources<'life0, 'async_trait>(
        &'life0 self,
    ) -> core::pin::Pin<
        Box<dyn core::future::Future<Output = TransportResult<Vec<Resource>>>
                + core::marker::Send
                + 'async_trait>,
    >
    where
        'life0: 'async_trait,
        Self: 'async_trait,
    {
        Box::pin(async move {
            let request = JsonRpcRequest::new(McpMethod::ResourcesList.as_str(), None);
            let response = self.send_request(request).await?;
            let result = response.into_result()?;

            serde_json::from_value(result).map_err(TransportError::from)
        })
    }

    /// Read a specific resource
    fn read_resource<'life0, 'life1, 'async_trait>(
        &'life0 self,
        uri: &'life1 str,
    ) -> core::pin::Pin<
        Box<dyn core::future::Future<Output = TransportResult<ResourceContents>>
                + core::marker::Send
                + 'async_trait>,
    >
    where
        'life0: 'async_trait,
        'life1: 'async_trait,
        Self: 'async_trait,
    {
        Box::pin(async move {
            let params = serde_json::json!({
                "uri": uri,
            });

            let request = JsonRpcRequest::new(McpMethod::ResourcesRead.as_str(), Some(params));
            let response = self.send_request(request).await?;
            let result = response.into_result()?;

            serde_json::from_value(result).map_err(TransportError::from)
        })
    }
}

// Implement the extension trait for all McpTransport implementers
impl<T: McpTransport + ?Sized> McpTransportExt for T {}

/// MCP Tool definition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Tool {
    /// Unique tool identifier
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// JSON Schema for tool input
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_schema: Option<JsonValue>,
}

/// Tool execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolResult {
    /// Content returned by the tool
    pub content: Vec<Content>,
    /// Whether the tool execution is still pending
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_error: Option<bool>,
}

/// Tool content item
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum Content {
    /// Text content
    Text { text: String },
    /// Image content
    Image { data: String, media_type: String },
    /// Resource reference
    Resource { uri: String, #[serde(skip_serializing_if = "Option::is_none")] text: Option<String> },
}

/// MCP Resource definition
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Resource {
    /// Unique resource URI
    pub uri: String,
    /// Human-readable name
    pub name: String,
    /// Description of what the resource provides
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// MIME type of the resource
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mime_type: Option<String>,
}

/// Resource contents
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ResourceContents {
    /// URI of the resource
    pub uri: String,
    /// Content items
    pub contents: Vec<Content>,
}

/// Transport state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransportState {
    /// Transport is disconnected
    Disconnected,
    /// Transport is connecting
    Connecting,
    /// Transport is connected
    Connected,
    /// Transport is shutting down
    ShuttingDown,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_request_id_generation() {
        let id = RequestId::new();
        assert!(matches!(id, RequestId::String(_)));
        assert!(!id.is_notification());
    }

    #[test]
    fn test_notification_detection() {
        let notification = JsonRpcRequest::notification("test", None);
        assert!(notification.is_notification());

        let request = JsonRpcRequest::new("test", None);
        assert!(!request.is_notification());
    }

    #[test]
    fn test_json_rpc_request_serialization() {
        let request = JsonRpcRequest::new("test_method", Some(JsonValue::Null));
        let json = request.to_json().unwrap();

        assert!(json.contains("\"jsonrpc\":\"2.0\""));
        assert!(json.contains("\"method\":\"test_method\""));
    }

    #[test]
    fn test_json_rpc_response_success() {
        let response = JsonRpcResponse::success(RequestId::new(), JsonValue::from(42));
        assert!(response.is_success());
        assert!(!response.is_error());

        let result = response.into_result().unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_json_rpc_response_error() {
        let error = JsonRpcError::new(-32000, "test error");
        let response = JsonRpcResponse::error(RequestId::new(), error);
        assert!(!response.is_success());
        assert!(response.is_error());

        let result = response.into_result();
        assert!(result.is_err());
    }

    #[test]
    fn test_mcp_method_from_str() {
        assert_eq!(McpMethod::from_str("initialize"), McpMethod::Initialize);
        assert_eq!(McpMethod::from_str("tools/list"), McpMethod::ToolsList);
        assert_eq!(McpMethod::from_str("unknown"), McpMethod::Custom("unknown".to_string()));
    }

    #[test]
    fn test_mcp_method_as_str() {
        assert_eq!(McpMethod::Initialize.as_str(), "initialize");
        assert_eq!(McpMethod::ToolsList.as_str(), "tools/list");
        assert_eq!(McpMethod::Custom("test".to_string()).as_str(), "test");
    }
}
