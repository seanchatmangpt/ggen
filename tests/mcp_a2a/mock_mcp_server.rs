//! Mock MCP Server for Integration Testing
//!
//! This module provides a comprehensive mock MCP (Model Context Protocol) server
//! for testing A2A (Agent-to-Agent) integration. Following Chicago TDD patterns,
//! this implementation uses state-based testing with real objects and the AAA pattern
//! (Arrange/Act/Assert).
//!
//! # Design Principle: Test Doubles Wrap Real Implementations
//!
//! This mock server wraps the production rig-mcp transport types where possible,
//! only duplicating test-specific features like message history and error simulation.
//!
//! # Features
//!
//! - Tool registration with dynamic handler assignment
//! - JSON-RPC 2.0 request/response handling (uses rig-mcp JsonRpcRequest/Response)
//! - Message history for test assertions (test-specific feature)
//! - Configurable error responses (test-specific feature)
//! - Graceful shutdown support
//!
//! # Architecture
//!
//! The mock server is designed to simulate a real MCP server while providing
//! testability features:
//!
//! ```text
//! ┌─────────────────────────────────────────────────┐
//! │              MockMcpServer                      │
//! │  ┌───────────────────────────────────────────┐  │
//! │  │ ToolRegistry (HashMap<String, Handler>)   │  │
//! │  └───────────────────────────────────────────┘  │
//! │  ┌───────────────────────────────────────────┐  │
//! │  │ MessageHistory (Vec<JsonRpcMessage>)     │  │
//! │  └───────────────────────────────────────────┘  │
//! │  ┌───────────────────────────────────────────┐  │
//!  │  │ ErrorSimulator (Option<ErrorResponse>)    │  │
//! │  └───────────────────────────────────────────┘  │
//! └─────────────────────────────────────────────────┘
//! ```
//!
//! # Example
//!
//! ```ignore
//! use crate::mcp_a2a::mock_mcp_server::{MockMcpServer, ToolHandler};
//! use rig_mcp_integration::transport::JsonRpcRequest;
//! use serde_json::json;
//!
//! #[tokio::test]
//! async fn test_tool_registration() {
//!     // Arrange
//!     let server = MockMcpServer::new();
//!     let handler = ToolHandler::new(|params| async {
//!         Ok(json!({"result": "success"}))
//!     });
//!
//!     // Act
//!     server.register_tool("test_tool".to_string(), handler);
//!     let tools = server.list_tools();
//!
//!     // Assert
//!     assert_eq!(tools.len(), 1);
//!     assert_eq!(tools[0], "test_tool");
//! }
//! ```

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

// Re-export production types from rig-mcp
pub use rig_mcp_integration::transport::{
    JsonRpcRequest, JsonRpcResponse, JsonRpcError,
    Content, Tool, ToolResult,
};

// ============================================================================
// CORE TYPES
// ============================================================================

/// Mock MCP Server for integration testing
///
/// This server simulates a real MCP server while providing testability
/// features like message history and configurable error responses.
///
/// Wraps production rig-mcp types where possible and only duplicates
/// test-specific functionality.
///
/// # State Management
///
/// The server uses thread-safe (Arc<RwLock<T>>) state to allow concurrent
/// test execution while maintaining test isolation.
#[derive(Clone)]
pub struct MockMcpServer {
    /// Registered tools and their handlers
    tools: Arc<RwLock<HashMap<String, ToolHandler>>>,

    /// Message history for assertions (test-specific feature)
    messages: Arc<RwLock<Vec<JsonRpcRequest>>>,

    /// Current error simulation configuration (test-specific feature)
    error_config: Arc<RwLock<ErrorConfig>>,

    /// Server configuration
    config: ServerConfig,
}

/// Configuration for the mock MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    /// Server name for identification
    pub name: String,

    /// Server version
    pub version: String,

    /// Enable message logging
    pub enable_logging: bool,

    /// Maximum message history size
    pub max_history: usize,

    /// Request timeout in milliseconds
    pub timeout_ms: u64,
}

impl Default for ServerConfig {
    fn default() -> Self {
        Self {
            name: "mock-mcp-server".to_string(),
            version: "1.0.0".to_string(),
            enable_logging: true,
            max_history: 1000,
            timeout_ms: 5000,
        }
    }
}

/// Error simulation configuration (test-specific)
#[derive(Debug, Clone, Default)]
pub struct ErrorConfig {
    /// Optional error to return on next request
    next_error: Option<JsonRpcError>,

    /// Error mode for testing error scenarios
    error_mode: ErrorMode,

    /// Count of requests before triggering error
    error_after: Option<usize>,

    /// Request counter for error_after tracking
    request_count: usize,
}

/// Error simulation modes (test-specific)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ErrorMode {
    /// No errors (default)
    #[default]
    None,

    /// Return error on all requests
    Always,

    /// Return error after N requests
    CountDown,

    /// Return error intermittently
    Intermittent,
}

/// Tool handler type for custom tool behavior
///
/// Handlers are async functions that take tool parameters and return
/// a JSON result or error.
pub type HandlerResult = Result<ToolResult, JsonRpcError>;
pub type HandlerFn = Arc<dyn Fn(Value) -> HandlerFnFuture + Send + Sync>;
pub type HandlerFnFuture = std::pin::Pin<Box<dyn std::future::Future<Output = HandlerResult> + Send>>>;

/// Tool handler with configurable behavior
pub struct ToolHandler {
    /// Tool name
    pub name: String,

    /// Tool description
    pub description: String,

    /// JSON Schema for input validation
    pub input_schema: Value,

    /// Async handler function
    handler: HandlerFn,

    /// Handler configuration
    config: HandlerConfig,
}

/// Tool handler configuration
#[derive(Debug, Clone, Default)]
pub struct HandlerConfig {
    /// Expected execution time in ms (for timeout testing)
    pub expected_duration_ms: Option<u64>,

    /// Whether this handler should fail
    pub should_fail: bool,

    /// Error to return if should_fail is true
    pub failure_error: Option<JsonRpcError>,
}

impl ToolHandler {
    /// Create a new tool handler with the given async function
    pub fn new<F, Fut>(handler: F) -> Self
    where
        F: Fn(Value) -> Fut + Send + Sync + 'static,
        Fut: std::future::Future<Output = HandlerResult> + Send + 'static,
    {
        Self {
            name: String::new(),
            description: String::new(),
            input_schema: json_schema_empty(),
            handler: Arc::new(move |params| Box::pin(handler(params))),
            config: HandlerConfig::default(),
        }
    }

    /// Set the tool name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Set the tool description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set the input schema
    pub fn with_input_schema(mut self, schema: Value) -> Self {
        self.input_schema = schema;
        self
    }

    /// Set expected duration for timeout testing
    pub fn with_expected_duration_ms(mut self, duration_ms: u64) -> Self {
        self.config.expected_duration_ms = Some(duration_ms);
        self
    }

    /// Configure this handler to always fail
    pub fn with_failure(mut self, error: JsonRpcError) -> Self {
        self.config.should_fail = true;
        self.config.failure_error = Some(error);
        self
    }

    /// Execute the handler with the given parameters
    pub async fn execute(&self, params: Value) -> HandlerResult {
        // Check if handler is configured to fail
        if self.config.should_fail {
            if let Some(ref error) = self.config.failure_error {
                return Err(error.clone());
            }
        }

        // Execute the actual handler
        (self.handler)(params).await
    }
}

/// Create an empty JSON schema
fn json_schema_empty() -> Value {
    serde_json::json!({
        "type": "object",
        "properties": {},
        "required": []
    })
}

// ============================================================================
// TEST HELPERS
// ============================================================================

/// Builder for creating test tool handlers
pub struct ToolHandlerBuilder {
    name: String,
    description: String,
    input_schema: Value,
    expected_duration_ms: Option<u64>,
    should_fail: bool,
    failure_error: Option<JsonRpcError>,
}

impl ToolHandlerBuilder {
    /// Create a new builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: String::new(),
            input_schema: json_schema_empty(),
            expected_duration_ms: None,
            should_fail: false,
            failure_error: None,
        }
    }

    /// Set the tool description
    pub fn description(mut self, desc: impl Into<String>) -> Self {
        self.description = desc.into();
        self
    }

    /// Set the input schema
    pub fn input_schema(mut self, schema: Value) -> Self {
        self.input_schema = schema;
        self
    }

    /// Set expected duration
    pub fn duration_ms(mut self, ms: u64) -> Self {
        self.expected_duration_ms = Some(ms);
        self
    }

    /// Configure to fail
    pub fn fails(mut self, error: JsonRpcError) -> Self {
        self.should_fail = true;
        self.failure_error = Some(error);
        self
    }

    /// Build the handler with a simple echo function
    pub fn build_echo(self) -> ToolHandler {
        ToolHandler::new(|params| async move {
            Ok(ToolResult::Success(Content {
                type_: "text".to_string(),
                text: Some(serde_json::to_string_pretty(&params).unwrap_or_default()),
            }))
        })
        .with_name(&self.name)
        .with_description(&self.description)
        .with_input_schema(self.input_schema)
    }

    /// Build the handler with a custom function
    pub fn build<F, Fut>(self, f: F) -> ToolHandler
    where
        F: Fn(Value) -> Fut + Send + Sync + 'static,
        Fut: std::future::Future<Output = HandlerResult> + Send + 'static,
    {
        let mut handler = ToolHandler::new(f);
        handler.name = self.name;
        handler.description = self.description;
        handler.input_schema = self.input_schema;
        if let Some(duration) = self.expected_duration_ms {
            handler.config.expected_duration_ms = Some(duration);
        }
        if self.should_fail {
            handler.config.should_fail = true;
            handler.config.failure_error = self.failure_error;
        }
        handler
    }

    /// Build the handler with a constant result
    pub fn build_constant(self, result: Value) -> ToolHandler {
        self.build(move |_params| {
            let result = result.clone();
            async move { Ok(ToolResult::Success(Content {
                type_: "text".to_string(),
                text: Some(serde_json::to_string(&result).unwrap_or_default()),
            })) }
        })
    }
}

/// Create a standard echo tool for testing
pub fn echo_tool() -> ToolHandler {
    ToolHandlerBuilder::new("echo")
        .description("Echoes the input parameters")
        .input_schema(json!({
            "type": "object",
            "properties": {
                "message": {
                    "type": "string",
                    "description": "Message to echo"
                }
            },
            "required": ["message"]
        }))
        .build_echo()
}

/// Create a tool that returns a constant value
pub fn constant_tool(name: &str, value: Value) -> ToolHandler {
    ToolHandlerBuilder::new(name)
        .description(format!("Returns constant value: {}", name))
        .build_constant(value)
}

/// Create a tool that always fails
pub fn failing_tool(name: &str, error: JsonRpcError) -> ToolHandler {
    ToolHandlerBuilder::new(name)
        .description(format!("Tool that always fails: {}", name))
        .fails(error)
        .build(|_params| async move {
            Err(JsonRpcError {
                code: -32603,
                message: "Should not reach here".to_string(),
                data: None,
            })
        })
}

// ============================================================================
// MOCK MCP SERVER IMPLEMENTATION
// ============================================================================

impl MockMcpServer {
    /// Create a new mock MCP server with default configuration
    pub fn new() -> Self {
        Self::with_config(ServerConfig::default())
    }

    /// Create a new mock MCP server with custom configuration
    pub fn with_config(config: ServerConfig) -> Self {
        Self {
            tools: Arc::new(RwLock::new(HashMap::new())),
            messages: Arc::new(RwLock::new(Vec::new())),
            error_config: Arc::new(RwLock::new(ErrorConfig::default())),
            config,
        }
    }

    /// Register a tool with the given name and handler
    ///
    /// # Example
    ///
    /// ```ignore
    /// let handler = ToolHandler::new(|params| async {
    ///     Ok(json!({"result": "Received: {}", params}))
    /// })
    /// .with_name("echo")
    /// .with_description("Echoes the input");
    ///
    /// server.register_tool("echo".to_string(), handler);
    /// ```
    pub async fn register_tool(&self, name: String, handler: ToolHandler) {
        let mut tools = self.tools.write().await;
        tools.insert(name, handler);
    }

    /// Register multiple tools at once
    pub async fn register_tools(&self, tools: Vec<(String, ToolHandler)>) {
        let mut tool_map = self.tools.write().await;
        for (name, handler) in tools {
            tool_map.insert(name, handler);
        }
    }

    /// Unregister a tool
    pub async fn unregister_tool(&self, name: &str) -> bool {
        let mut tools = self.tools.write().await;
        tools.remove(name).is_some()
    }

    /// Get a list of all registered tool names
    pub async fn list_tools(&self) -> Vec<String> {
        let tools = self.tools.read().await;
        let mut names: Vec<String> = tools.keys().cloned().collect();
        names.sort();
        names
    }

    /// Get tool definitions for all registered tools (as production Tool type)
    pub async fn get_tool_definitions(&self) -> Vec<Tool> {
        let tools = self.tools.read().await;
        tools
            .values()
            .map(|handler| Tool {
                name: handler.name.clone(),
                description: handler.description.clone(),
                input_schema: Some(handler.input_schema.clone()),
            })
            .collect()
    }

    /// Check if a tool is registered
    pub async fn has_tool(&self, name: &str) -> bool {
        let tools = self.tools.read().await;
        tools.contains_key(name)
    }

    /// Get message history for assertions (test-specific)
    pub async fn get_message_history(&self) -> Vec<JsonRpcRequest> {
        let messages = self.messages.read().await;
        messages.clone()
    }

    /// Clear message history (test-specific)
    pub async fn clear_history(&self) {
        let mut messages = self.messages.write().await;
        messages.clear();
    }

    /// Get the number of messages in history
    pub async fn history_len(&self) -> usize {
        let messages = self.messages.read().await;
        messages.len()
    }

    /// Find messages by method name
    pub async fn find_messages_by_method(&self, method: &str) -> Vec<JsonRpcRequest> {
        let messages = self.messages.read().await;
        messages
            .iter()
            .filter(|m| m.method == method)
            .cloned()
            .collect()
    }

    /// Find messages by ID
    pub async fn find_message_by_id(&self, id: &Value) -> Option<JsonRpcRequest> {
        let messages = self.messages.read().await;
        messages.iter().find(|m| &m.id == id).cloned()
    }

    /// Configure error simulation (test-specific)
    pub async fn set_error_mode(&self, mode: ErrorMode) {
        let mut config = self.error_config.write().await;
        config.error_mode = mode;
    }

    /// Set the next error to return (test-specific)
    pub async fn set_next_error(&self, error: JsonRpcError) {
        let mut config = self.error_config.write().await;
        config.next_error = Some(error);
    }

    /// Set error to trigger after N requests (test-specific)
    pub async fn set_error_after(&self, count: usize) {
        let mut config = self.error_config.write().await;
        config.error_after = Some(count);
        config.request_count = 0;
    }

    /// Reset error configuration (test-specific)
    pub async fn reset_error_config(&self) {
        let mut config = self.error_config.write().await;
        *config = ErrorConfig::default();
    }

    /// Get the current request count (test-specific)
    pub async fn request_count(&self) -> usize {
        let config = self.error_config.read().await;
        config.request_count
    }

    /// Handle a JSON-RPC request (using production JsonRpcRequest type)
    ///
    /// This is the main entry point for processing MCP requests.
    /// It handles all standard MCP methods:
    /// - tools/list
    /// - tools/call
    /// - tools/validate
    /// - resources/list
    /// - resources/read
    /// - health
    pub async fn handle_request(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        // Log message if enabled (test-specific)
        if self.config.enable_logging {
            let mut messages = self.messages.write().await;
            if messages.len() < self.config.max_history {
                messages.push(request.clone());
            }
        }

        // Increment request count (test-specific)
        {
            let mut config = self.error_config.write().await;
            config.request_count += 1;
        }

        // Check for simulated errors (test-specific)
        if let Some(error) = self.should_return_error().await {
            return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id.clone(),
                result: None,
                error: Some(error),
            };
        }

        // Route to appropriate handler
        match request.method.as_str() {
            "tools/list" => self.handle_tools_list(request).await,
            "tools/call" => self.handle_tools_call(request).await,
            "tools/validate" => self.handle_tools_validate(request).await,
            "resources/list" => self.handle_resources_list(request).await,
            "resources/read" => self.handle_resources_read(request).await,
            "health" => self.handle_health(request).await,
            _ => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id.clone(),
                result: None,
                error: Some(JsonRpcError {
                    code: -32601,
                    message: format!("Method not found: {}", request.method),
                    data: None,
                }),
            },
        }
    }

    /// Check if we should return an error based on configuration (test-specific)
    async fn should_return_error(&self) -> Option<JsonRpcError> {
        let config = self.error_config.read().await;

        match config.error_mode {
            ErrorMode::None => {
                // Check for next_error
                if config.next_error.is_some() {
                    // Clear it after reading
                    drop(config);
                    let mut cfg = self.error_config.write().await;
                    cfg.next_error.take()
                } else {
                    None
                }
            }
            ErrorMode::Always => Some(JsonRpcError {
                code: -32603,
                message: "Simulated error".to_string(),
                data: None,
            }),
            ErrorMode::CountDown => {
                if let Some(after) = config.error_after {
                    if config.request_count >= after {
                        Some(JsonRpcError {
                            code: -32603,
                            message: "Countdown error".to_string(),
                            data: None,
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ErrorMode::Intermittent => {
                if config.request_count % 2 == 0 {
                    Some(JsonRpcError {
                        code: -32603,
                        message: "Intermittent error".to_string(),
                        data: None,
                    })
                } else {
                    None
                }
            }
        }
    }

    /// Handle tools/list request
    async fn handle_tools_list(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        let tools = self.get_tool_definitions().await;

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "tools": tools,
                "server": {
                    "name": self.config.name,
                    "version": self.config.version
                }
            })),
            error: None,
        }
    }

    /// Handle tools/call request
    async fn handle_tools_call(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        // Extract tool name and arguments from params
        let name = request
            .params
            .as_ref()
            .and_then(|p| p.get("name"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| JsonRpcError {
                code: -32602,
                message: "Missing 'name' parameter".to_string(),
                data: None,
            });

        let arguments = request
            .params
            .as_ref()
            .and_then(|p| p.get("arguments"))
            .cloned()
            .unwrap_or_else(|| json!({}));

        let name = match name {
            Ok(n) => n,
            Err(e) => return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(e),
            },
        };

        // Find and execute the tool
        let tools = self.tools.read().await;
        let handler = match tools.get(name) {
            Some(h) => h,
            None => {
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: request.id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32002,
                        message: format!("Tool not found: {}", name),
                        data: None,
                    }),
                }
            }
        };

        // Execute with timeout
        let timeout_duration = std::time::Duration::from_millis(self.config.timeout_ms);
        let result = tokio::time::timeout(timeout_duration, handler.execute(arguments)).await;

        match result {
            Ok(Ok(tool_result)) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: Some(match tool_result {
                    ToolResult::Success(content) => serde_json::to_value(content).ok(),
                    ToolResult::Error { error, .. } => serde_json::json!({
                        "error": error.message
                    }),
                }),
                error: None,
            },
            Ok(Err(e)) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(e),
            },
            Err(_) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32008,
                    message: format!("Tool execution timeout: {}", name),
                    data: None,
                }),
            },
        }
    }

    /// Handle tools/validate request
    async fn handle_tools_validate(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        let name = request
            .params
            .as_ref()
            .and_then(|p| p.get("name"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| JsonRpcError {
                code: -32602,
                message: "Missing 'name' parameter".to_string(),
                data: None,
            });

        let arguments = request
            .params
            .as_ref()
            .and_then(|p| p.get("arguments"))
            .cloned()
            .unwrap_or_else(|| json!({}));

        let name = match name {
            Ok(n) => n,
            Err(e) => return JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(e),
            },
        };

        // Check if tool exists
        let tools = self.tools.read().await;
        let handler = match tools.get(name) {
            Some(h) => h,
            None => {
                return JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: request.id,
                    result: None,
                    error: Some(JsonRpcError {
                        code: -32002,
                        message: format!("Tool not found: {}", name),
                        data: None,
                    }),
                }
            }
        };

        // Validate against schema
        let validation_result = self.validate_against_schema(&handler.input_schema, &arguments);

        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "valid": validation_result.is_ok(),
                "errors": if let Err(e) = validation_result {
                    vec![e.to_string()]
                } else {
                    vec![]
                }
            })),
            error: None,
        }
    }

    /// Validate arguments against JSON schema
    fn validate_against_schema(&self, schema: &Value, arguments: &Value) -> Result<(), String> {
        // Basic validation - check required fields
        if let Some(required) = schema.get("required").and_then(|v| v.as_array()) {
            for field in required {
                if let Some(field_name) = field.as_str() {
                    if !arguments.get(field_name).is_some() {
                        return Err(format!("Missing required field: {}", field));
                    }
                }
            }
        }
        Ok(())
    }

    /// Handle resources/list request
    async fn handle_resources_list(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        // Return empty resource list for now
        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "resources": []
            })),
            error: None,
        }
    }

    /// Handle resources/read request
    async fn handle_resources_read(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        let uri = request
            .params
            .as_ref()
            .and_then(|p| p.get("uri"))
            .and_then(|v| v.as_str());

        match uri {
            Some(_) => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32603,
                    message: "Resource not found".to_string(),
                    data: None,
                }),
            },
            None => JsonRpcResponse {
                jsonrpc: "2.0".to_string(),
                id: request.id,
                result: None,
                error: Some(JsonRpcError {
                    code: -32602,
                    message: "Missing 'uri' parameter".to_string(),
                    data: None,
                }),
            },
        }
    }

    /// Handle health check request
    async fn handle_health(&self, request: JsonRpcRequest) -> JsonRpcResponse {
        JsonRpcResponse {
            jsonrpc: "2.0".to_string(),
            id: request.id,
            result: Some(serde_json::json!({
                "status": "healthy",
                "server": self.config.name,
                "version": self.config.version,
                "tools_registered": self.tools.read().await.len(),
                "uptime_ms": 0
            })),
            error: None,
        }
    }

    /// Get server statistics
    pub async fn stats(&self) -> ServerStats {
        let tools = self.tools.read().await;
        let messages = self.messages.read().await;
        let config = self.error_config.read().await;

        ServerStats {
            tools_registered: tools.len(),
            messages_processed: messages.len(),
            requests_handled: config.request_count,
            error_mode: config.error_mode,
        }
    }
}

impl Default for MockMcpServer {
    fn default() -> Self {
        Self::new()
    }
}

/// Server statistics
#[derive(Debug, Clone)]
pub struct ServerStats {
    /// Number of registered tools
    pub tools_registered: usize,

    /// Number of messages in history
    pub messages_processed: usize,

    /// Total requests handled
    pub requests_handled: usize,

    /// Current error mode
    pub error_mode: ErrorMode,
}

/// Create a fully configured mock server with common tools
pub async fn create_server_with_standard_tools() -> MockMcpServer {
    let server = MockMcpServer::new();

    // Register standard tools
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
                    Ok(ToolResult::Success(Content {
                        type_: "text".to_string(),
                        text: Some(format!("{}", a + b)),
                    }))
                }),
        )
        .await;

    server
        .register_tool(
            "get_weather".to_string(),
            ToolHandlerBuilder::new("get_weather")
                .description("Get weather for a location")
                .input_schema(json!({
                    "type": "object",
                    "properties": {
                        "location": {"type": "string"}
                    },
                    "required": ["location"]
                }))
                .build(|params| async move {
                    let location = params
                        .get("location")
                        .and_then(|v| v.as_str())
                        .unwrap_or("unknown");
                    Ok(ToolResult::Success(Content {
                        type_: "text".to_string(),
                        text: Some(format!(
                            "Location: {}, Temperature: 22C, Condition: sunny",
                            location
                        )),
                    }))
                }),
        )
        .await;

    server
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_server_creation() {
        // Act
        let server = MockMcpServer::new();

        // Assert
        let stats = server.stats().await;
        assert_eq!(stats.tools_registered, 0);
        assert_eq!(stats.messages_processed, 0);
    }

    #[tokio::test]
    async fn test_tool_registration() {
        // Arrange
        let server = MockMcpServer::new();
        let handler = echo_tool();

        // Act
        server.register_tool("echo".to_string(), handler).await;

        // Assert
        let tools = server.list_tools().await;
        assert_eq!(tools.len(), 1);
        assert_eq!(tools[0], "echo");
        assert!(server.has_tool("echo").await);
    }

    #[tokio::test]
    async fn test_tool_unregistration() {
        // Arrange
        let server = MockMcpServer::new();
        let handler = echo_tool();
        server.register_tool("echo".to_string(), handler).await;

        // Act
        let removed = server.unregister_tool("echo").await;

        // Assert
        assert!(removed);
        assert!(!server.has_tool("echo").await);
        assert_eq!(server.list_tools().await.len(), 0);
    }

    #[tokio::test]
    async fn test_tools_list_request() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        assert!(response.result.is_some());
        let result = response.result.unwrap();
        let tools = result.get("tools").and_then(|t| t.as_array()).unwrap();
        assert_eq!(tools.len(), 3);
    }

    #[tokio::test]
    async fn test_tools_call_success() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(2)),
            method: "tools/call".to_string(),
            params: Some(json!({
                "name": "echo",
                "arguments": {"message": "hello"}
            })),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        assert!(response.result.is_some());
        // Result should contain text content
        let result = response.result.unwrap();
        assert!(result.is_string() || result.is_object());
    }

    #[tokio::test]
    async fn test_tools_call_not_found() {
        // Arrange
        let server = MockMcpServer::new();
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(3)),
            method: "tools/call".to_string(),
            params: Some(json!({
                "name": "nonexistent",
                "arguments": {}
            })),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, -32002); // Tool not found
    }

    #[tokio::test]
    async fn test_health_check() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(0)),
            method: "health".to_string(),
            params: Some(json!({})),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        let result = response.result.unwrap();
        assert_eq!(result.get("status").unwrap(), "healthy");
        assert_eq!(
            result.get("tools_registered").unwrap().as_i64().unwrap(),
            3
        );
    }

    #[tokio::test]
    async fn test_message_history() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request1 = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        };
        let request2 = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(0)),
            method: "health".to_string(),
            params: Some(json!({})),
        };

        // Act
        server.handle_request(request1).await;
        server.handle_request(request2).await;

        // Assert
        let history = server.get_message_history().await;
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].method, "tools/list");
        assert_eq!(history[1].method, "health");
    }

    #[tokio::test]
    async fn test_error_simulation_always() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        server.set_error_mode(ErrorMode::Always).await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_some());
    }

    #[tokio::test]
    async fn test_error_simulation_countdown() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        server.set_error_after(2).await;

        // First request should succeed
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        };
        let response1 = server.handle_request(request.clone()).await;
        assert!(response1.error.is_none());

        // Second request should succeed
        let response2 = server.handle_request(request.clone()).await;
        assert!(response2.error.is_none());

        // Third request should fail
        let response3 = server.handle_request(request).await;
        assert!(response3.error.is_some());
    }

    #[tokio::test]
    async fn test_method_not_found() {
        // Arrange
        let server = MockMcpServer::new();
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "unknown_method".to_string(),
            params: Some(json!({})),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert_eq!(error.code, -32601);
    }

    #[tokio::test]
    async fn test_message_history_clear() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        };
        server.handle_request(request).await;

        // Act
        server.clear_history().await;

        // Assert
        assert_eq!(server.history_len().await, 0);
    }

    #[tokio::test]
    async fn test_find_messages_by_method() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        }).await;
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(0)),
            method: "health".to_string(),
            params: Some(json!({})),
        }).await;
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(2)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        }).await;

        // Act
        let tools_list_msgs = server.find_messages_by_method("tools/list").await;

        // Assert
        assert_eq!(tools_list_msgs.len(), 2);
    }

    #[tokio::test]
    async fn test_server_stats() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        }).await;

        // Act
        let stats = server.stats().await;

        // Assert
        assert_eq!(stats.tools_registered, 3);
        assert_eq!(stats.messages_processed, 1);
        assert_eq!(stats.requests_handled, 1);
        assert_eq!(stats.error_mode, ErrorMode::None);
    }

    #[tokio::test]
    async fn test_tool_handler_builder() {
        // Arrange & Act
        let handler = ToolHandlerBuilder::new("test_tool")
            .description("A test tool")
            .input_schema(json!({"type": "object"}))
            .build_constant(json!({"fixed": "result"}));

        // Assert
        assert_eq!(handler.name, "test_tool");
        assert_eq!(handler.description, "A test tool");
    }

    #[tokio::test]
    async fn test_add_tool() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(2)),
            method: "tools/call".to_string(),
            params: Some(json!({
                "name": "add",
                "arguments": {"a": 5, "b": 3}
            })),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        let result = response.result.unwrap();
        // Result should be a string "8"
        assert!(result.is_string() && result.as_str().unwrap() == "8" ||
                result.is_object() && result.get("result").is_some());
    }

    #[tokio::test]
    async fn test_get_weather_tool() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(3)),
            method: "tools/call".to_string(),
            params: Some(json!({
                "name": "get_weather",
                "arguments": {"location": "San Francisco"}
            })),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        let result = response.result.unwrap();
        assert!(result.is_string() || result.is_object());
    }

    #[tokio::test]
    async fn test_tool_validation() {
        // Arrange
        let server = create_server_with_standard_tools().await;
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(4)),
            method: "tools/validate".to_string(),
            params: Some(json!({
                "name": "echo",
                "arguments": {}
            })),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        let result = response.result.unwrap();
        assert_eq!(result.get("valid").unwrap(), false);
        let errors = result.get("errors").and_then(|e| e.as_array()).unwrap();
        assert!(!errors.is_empty());
    }

    #[tokio::test]
    async fn test_request_count_tracking() {
        // Arrange
        let server = create_server_with_standard_tools().await;

        // Act
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(1)),
            method: "tools/list".to_string(),
            params: Some(json!({})),
        }).await;
        server.handle_request(JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(2)),
            method: "health".to_string(),
            params: Some(json!({})),
        }).await;

        // Assert
        assert_eq!(server.request_count().await, 2);
    }

    #[tokio::test]
    async fn test_resources_list() {
        // Arrange
        let server = MockMcpServer::new();
        let request = JsonRpcRequest {
            jsonrpc: "2.0".to_string(),
            id: Some(json!(5)),
            method: "resources/list".to_string(),
            params: Some(json!({})),
        };

        // Act
        let response = server.handle_request(request).await;

        // Assert
        assert!(response.error.is_none());
        let result = response.result.unwrap();
        let resources = result.get("resources").and_then(|r| r.as_array()).unwrap();
        assert_eq!(resources.len(), 0);
    }

    #[tokio::test]
    async fn test_batch_tool_registration() {
        // Arrange
        let server = MockMcpServer::new();
        let tools = vec![
            ("tool1".to_string(), constant_tool("tool1", json!(1))),
            ("tool2".to_string(), constant_tool("tool2", json!(2))),
            ("tool3".to_string(), constant_tool("tool3", json!(3))),
        ];

        // Act
        server.register_tools(tools).await;

        // Assert
        assert_eq!(server.list_tools().await.len(), 3);
    }
}
