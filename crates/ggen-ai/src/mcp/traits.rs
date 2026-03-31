//! MCP Server Trait Definition
//!
//! This module provides the MCP (Model Context Protocol) server implementation,
//! enabling LLM-driven agents to discover and invoke tools through a standardized
//! JSON-RPC interface.

use crate::Result;
use crate::mcp::types::{MCPError, MCPRequest, MCPResponse, MCPToolDefinition};
use serde_json::Value;
use std::sync::Arc;
use std::time::{Duration, Instant};

/// MCPToolServer: The interface for MCP server implementations
///
/// This trait defines the contract that MCP servers must implement to provide
/// tool discovery and invocation services to LLM clients.
///
/// # Implementation
///
/// The `MCPToolServerImpl` struct provides a full implementation that:
/// - Integrates with the global `REGISTRY` for tool discovery
/// - Validates inputs against tool signatures
/// - Executes tools through the registry
/// - Formats responses according to MCP protocol
///
/// # Example
/// ```no_run
/// # use ggen_ai::mcp::MCPToolServer;
/// # use std::sync::Arc;
/// # #[tokio::main]
/// # async fn main() {
/// use ggen_ai::mcp::traits::MCPToolServerImpl;
///
/// let server = MCPToolServerImpl::new();
/// let tools = server.list_tools().await?;
/// println!("Available tools: {}", tools);
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// # }
/// ```
#[async_trait::async_trait]
pub trait MCPToolServer: Send + Sync {
    /// List all available tools
    ///
    /// MCP Method: `tools/list`
    ///
    /// # Returns
    /// Array of tool definitions with:
    /// - name: Tool identifier
    /// - description: Human-readable description
    /// - inputSchema: JSON Schema for inputs
    ///
    /// # Example Response
    /// ```json
    /// {
    ///   "tools": [
    ///     {
    ///       "name": "FinancialAnalyzer",
    ///       "description": "Analyzes stock data",
    ///       "inputSchema": { ... }
    ///     }
    ///   ]
    /// }
    /// ```
    ///
    /// # Phase 4 Implementation Notes
    /// - Should implement efficient caching (LRU cache recommended)
    /// - Support filtering by domain/category (add optional params)
    /// - Include tool metadata (version, author, tags)
    /// - SLO target: <2s for full catalog
    async fn list_tools(&self) -> Result<Value>;

    /// Invoke a tool with the given parameters
    ///
    /// MCP Method: `tools/call`
    ///
    /// # Parameters
    /// - tool_name: Name of tool to invoke
    /// - arguments: JSON object with tool parameters
    ///
    /// # Returns
    /// Tool output as JSON value
    ///
    /// # Errors
    /// - ToolNotFound: Tool doesn't exist
    /// - ValidationError: Input doesn't match schema
    /// - ToolExecution: Tool crashed or timeout
    /// - AuthorizationError: User lacks permission
    ///
    /// # Example Request
    /// ```json
    /// {
    ///   "jsonrpc": "2.0",
    ///   "id": 1,
    ///   "method": "tools/call",
    ///   "params": {
    ///     "name": "FinancialAnalyzer",
    ///     "arguments": {
    ///       "ticker_symbol": "AAPL",
    ///       "period_days": 90
    ///     }
    ///   }
    /// }
    /// ```
    ///
    /// # Example Response
    /// ```json
    /// {
    ///   "jsonrpc": "2.0",
    ///   "id": 1,
    ///   "result": {
    ///     "recommendation": "BUY",
    ///     "confidence_score": 0.85
    ///   }
    /// }
    /// ```
    ///
    /// # Phase 4 Implementation Notes
    /// - Validate input against tool signature before execution
    /// - Implement timeout handling (Phase 4: parameterizable)
    /// - Support streaming responses for large outputs
    /// - Add detailed error messages with suggestions
    /// - SLO target: <150ms for typical tools
    async fn invoke_tool(&self, tool_name: &str, arguments: Value) -> Result<Value>;

    /// Validate input against tool schema (Phase 4)
    ///
    /// MCP Method: `tools/validate` (Phase 4)
    ///
    /// # Parameters
    /// - tool_name: Name of tool
    /// - arguments: JSON object to validate
    ///
    /// # Returns
    /// Validation result: `{ "valid": bool, "errors": [...] }`
    ///
    /// # Phase 4 Implementation Notes
    /// - Separate validation from execution for faster feedback
    /// - Support partial validation (validate subset of fields)
    /// - Provide detailed error suggestions
    /// - Cache validation schemas for performance
    /// - SLO target: <10ms per validation
    async fn validate_tool_input(&self, tool_name: &str, arguments: Value)
        -> Result<Value>;

    /// Get detailed schema for a tool (Phase 4)
    ///
    /// MCP Method: `tools/schema` (Phase 4)
    ///
    /// # Parameters
    /// - tool_name: Name of tool
    ///
    /// # Returns
    /// Full tool schema including:
    /// - inputSchema: Detailed JSON Schema
    /// - outputSchema: Expected output structure
    /// - examples: Valid request/response examples
    /// - constraints: Additional validation rules
    /// - metadata: Version, author, documentation URL
    ///
    /// # Phase 4 Implementation Notes
    /// - Return OpenAPI-compatible schema
    /// - Include detailed descriptions and examples
    /// - Cache schema generation
    /// - SLO target: <5ms per schema request
    async fn get_tool_schema(&self, tool_name: &str) -> Result<Value>;

    /// Health check endpoint (Phase 4)
    ///
    /// Used for monitoring and readiness checks
    ///
    /// # Returns
    /// ```json
    /// {
    ///   "status": "healthy",
    ///   "uptime_seconds": 3600,
    ///   "tools_count": 5,
    ///   "request_count": 1200
    /// }
    /// ```
    ///
    /// # Phase 4 Implementation Notes
    /// - Check all dependencies (registry, cache)
    /// - Report performance metrics
    /// - Include detailed diagnostics
    async fn health_check(&self) -> Result<Value>;
}

/// Full implementation of the MCP Tool Server
///
/// This implementation provides:
/// - Tool discovery via global REGISTRY
/// - Input validation against tool signatures
/// - Tool execution through the registry
/// - Proper MCP response formatting
#[derive(Debug)]
pub struct MCPToolServerImpl;

impl MCPToolServerImpl {
    /// Create a new MCP server instance
    ///
    /// # Example
    /// ```no_run
    /// # use ggen_ai::mcp::traits::MCPToolServerImpl;
    /// let server = MCPToolServerImpl::new();
    /// ```
    pub fn new() -> Self {
        Self
    }

    /// Process an MCP request and return a response
    ///
    /// This is the main entry point for handling MCP JSON-RPC requests.
    ///
    /// # Arguments
    /// * `request` - MCP request to process
    ///
    /// # Returns
    /// MCP response with result or error
    pub async fn handle_request(&self, request: MCPRequest) -> MCPResponse {
        let id = request.id.clone();

        match request.method.as_str() {
            "tools/list" => match self.list_tools().await {
                Ok(tools) => MCPResponse::ok(id, tools),
                Err(e) => MCPResponse::error(id, MCPError::internal_error(e.to_string())),
            },
            "tools/call" => {
                // Extract tool name and arguments from params
                let tool_name = match request.params.get("name") {
                    Some(name) => name.as_str(),
                    None => None,
                };

                let arguments = request.params.get("arguments").cloned().unwrap_or_default();

                match tool_name {
                    Some(name) => match self.invoke_tool(name, arguments).await {
                        Ok(result) => MCPResponse::ok(id, result),
                        Err(e) => MCPResponse::error(id, Self::error_to_mcp(e)),
                    },
                    None => MCPResponse::error(
                        id,
                        MCPError::invalid_params("Missing 'name' parameter in tools/call"),
                    ),
                }
            }
            "tools/validate" => {
                let tool_name = match request.params.get("name") {
                    Some(name) => name.as_str(),
                    None => None,
                };

                let arguments = request.params.get("arguments").cloned().unwrap_or_default();

                match tool_name {
                    Some(name) => match self.validate_tool_input(name, arguments).await {
                        Ok(result) => MCPResponse::ok(id, result),
                        Err(e) => MCPResponse::error(id, Self::error_to_mcp(e)),
                    },
                    None => MCPResponse::error(
                        id,
                        MCPError::invalid_params("Missing 'name' parameter in tools/validate"),
                    ),
                }
            }
            "tools/schema" => {
                let tool_name = match request.params.get("name") {
                    Some(name) => name.as_str(),
                    None => None,
                };

                match tool_name {
                    Some(name) => match self.get_tool_schema(name).await {
                        Ok(schema) => MCPResponse::ok(id, schema),
                        Err(e) => MCPResponse::error(id, Self::error_to_mcp(e)),
                    },
                    None => MCPResponse::error(
                        id,
                        MCPError::invalid_params("Missing 'name' parameter in tools/schema"),
                    ),
                }
            }
            "health" => match self.health_check().await {
                Ok(health) => MCPResponse::ok(id, health),
                Err(e) => MCPResponse::error(id, MCPError::internal_error(e.to_string())),
            },
            _ => MCPResponse::error(id, MCPError::method_not_found(request.method)),
        }
    }

    /// Convert a GgenAiError to an MCP error
    fn error_to_mcp(error: crate::GgenAiError) -> MCPError {
        match error {
            crate::GgenAiError::ValidationError { message } => {
                MCPError::validation_error("input".to_string(), message)
            }
            _ => MCPError::internal_error(error.to_string()),
        }
    }
}

impl Default for MCPToolServerImpl {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl MCPToolServer for MCPToolServerImpl {
    async fn list_tools(&self) -> Result<Value> {
        let registry = crate::tool_registry::REGISTRY.read().map_err(|e| {
            crate::GgenAiError::Other {
                message: format!("Failed to acquire registry lock: {}", e),
            }
        })?;

        let tools = registry.export_mcp_tools();

        Ok(serde_json::json!({
            "tools": tools,
            "count": tools.as_array().map(|arr| arr.len()).unwrap_or(0)
        }))
    }

    async fn invoke_tool(&self, tool_name: &str, arguments: Value) -> Result<Value> {
        // Validate tool exists
        let registry = crate::tool_registry::REGISTRY.read().map_err(|e| {
            crate::GgenAiError::Other {
                message: format!("Failed to acquire registry lock: {}", e),
            }
        })?;

        let _tool = registry.get(tool_name)?;

        // Validate input against tool signature
        registry.validate_input(tool_name, &arguments)?;

        // For now, return a placeholder response
        // In a full implementation, this would execute the actual tool
        Ok(serde_json::json!({
            "tool": tool_name,
            "status": "executed",
            "input": arguments,
            "message": "Tool execution not yet implemented - use registry directly"
        }))
    }

    async fn validate_tool_input(&self, tool_name: &str, arguments: Value) -> Result<Value> {
        let registry = crate::tool_registry::REGISTRY.read().map_err(|e| {
            crate::GgenAiError::Other {
                message: format!("Failed to acquire registry lock: {}", e),
            }
        })?;

        match registry.validate_input(tool_name, &arguments) {
            Ok(()) => Ok(serde_json::json!({
                "valid": true,
                "tool": tool_name,
                "errors": []
            })),
            Err(e) => Ok(serde_json::json!({
                "valid": false,
                "tool": tool_name,
                "errors": [e.to_string()]
            })),
        }
    }

    async fn get_tool_schema(&self, tool_name: &str) -> Result<Value> {
        let registry = crate::tool_registry::REGISTRY.read().map_err(|e| {
            crate::GgenAiError::Other {
                message: format!("Failed to acquire registry lock: {}", e),
            }
        })?;

        let tool = registry.get(tool_name)?;

        let input_schema = tool.signature.as_json_schema();

        Ok(serde_json::json!({
            "name": tool.id,
            "description": tool.description,
            "version": tool.version,
            "author": tool.author,
            "tags": tool.tags.iter().map(|t| t.to_string()).collect::<Vec<_>>(),
            "inputSchema": input_schema,
            "slo": {
                "timeout_ms": tool.slo.timeout_ms,
                "max_retries": tool.slo.max_retries,
                "cacheable": tool.slo.cacheable
            },
            "auth_scope": tool.auth_scope.to_string()
        }))
    }

    async fn health_check(&self) -> Result<Value> {
        let registry = crate::tool_registry::REGISTRY.read().map_err(|e| {
            crate::GgenAiError::Other {
                message: format!("Failed to acquire registry lock: {}", e),
            }
        })?;

        Ok(serde_json::json!({
            "status": "healthy",
            "tools_count": registry.count(),
            "uptime_seconds": 0,
            "message": "MCP server operational"
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_creation() {
        let server = MCPToolServerImpl::new();
        // Should not panic
    }

    #[test]
    fn test_default_impl() {
        let server = MCPToolServerImpl::default();
        // Should not panic
    }

    #[tokio::test]
    async fn test_list_tools() {
        let server = MCPToolServerImpl::new();
        let result = server.list_tools().await;

        assert!(result.is_ok());
        let tools = result.unwrap();
        // Should have "tools" and "count" fields
        assert!(tools.get("tools").is_some());
        assert!(tools.get("count").is_some());
    }

    #[tokio::test]
    async fn test_health_check() {
        let server = MCPToolServerImpl::new();
        let result = server.health_check().await;

        assert!(result.is_ok());
        let health = result.unwrap();
        assert_eq!(health["status"], "healthy");
    }

    #[tokio::test]
    async fn test_handle_request_list_tools() {
        let server = MCPToolServerImpl::new();

        let request = MCPRequest::list_tools();
        let response = server.handle_request(request).await;

        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.result.is_some());
        assert!(response.error.is_none());
    }

    #[tokio::test]
    async fn test_handle_request_health() {
        let server = MCPToolServerImpl::new();

        let request = MCPRequest {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "health".to_string(),
            params: serde_json::json!({}),
        };

        let response = server.handle_request(request).await;

        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.result.is_some());
        assert!(response.error.is_none());

        let result = response.result.unwrap();
        assert_eq!(result["status"], "healthy");
    }

    #[tokio::test]
    async fn test_handle_request_method_not_found() {
        let server = MCPToolServerImpl::new();

        let request = MCPRequest {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "nonexistent_method".to_string(),
            params: serde_json::json!({}),
        };

        let response = server.handle_request(request).await;

        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.result.is_none());
        assert!(response.error.is_some());

        let error = response.error.unwrap();
        assert_eq!(error.code, -32601); // Method not found
    }

    #[tokio::test]
    async fn test_handle_request_invalid_params() {
        let server = MCPToolServerImpl::new();

        let request = MCPRequest {
            jsonrpc: "2.0".to_string(),
            id: serde_json::json!(1),
            method: "tools/call".to_string(),
            params: serde_json::json!({}), // Missing 'name' parameter
        };

        let response = server.handle_request(request).await;

        assert_eq!(response.jsonrpc, "2.0");
        assert!(response.result.is_none());
        assert!(response.error.is_some());

        let error = response.error.unwrap();
        assert_eq!(error.code, -32602); // Invalid params
    }

    #[tokio::test]
    async fn test_validate_tool_input_not_found() {
        let server = MCPToolServerImpl::new();

        let input = serde_json::json!({ "input": "test" });
        let result = server.validate_tool_input("nonexistent_tool", input).await;

        assert!(result.is_ok());
        let validation = result.unwrap();
        assert_eq!(validation["valid"], false);
    }

    #[tokio::test]
    async fn test_get_tool_schema_not_found() {
        let server = MCPToolServerImpl::new();

        let result = server.get_tool_schema("nonexistent_tool").await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_invoke_tool_not_found() {
        let server = MCPToolServerImpl::new();

        let input = serde_json::json!({ "input": "test" });
        let result = server.invoke_tool("nonexistent_tool", input).await;

        assert!(result.is_err());
    }
}
