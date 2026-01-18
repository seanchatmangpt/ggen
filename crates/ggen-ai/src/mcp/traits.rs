//! MCP Server Trait Definition
//!
//! Phase 4: This trait will be implemented by the full MCP server.
//! For Phase 3, this serves as the interface specification.

use crate::Result;
use serde_json::Value;

/// MCPToolServer: The interface for MCP server implementations
///
/// This trait defines the contract that MCP servers must implement to provide
/// tool discovery and invocation services to LLM clients.
///
/// # Phase 3 Status
/// This trait is defined but not implemented. Phase 4 will provide:
/// - HTTP/WebSocket server implementation
/// - JSON-RPC 2.0 protocol handler
/// - Full error handling and recovery
/// - Performance optimization
///
/// # Phase 4 Implementation Plan
/// 1. Implement HTTP server using axum or actix-web
/// 2. Add JSON-RPC 2.0 protocol handler
/// 3. Implement authentication/authorization
/// 4. Add comprehensive error handling
/// 5. Performance testing and optimization
///
/// # Example (Phase 4)
/// ```no_run
/// # use ggen_ai::mcp::MCPToolServer;
/// # use std::sync::Arc;
/// # #[tokio::main]
/// # async fn main() {
/// // let server = MCPServerImpl::new(config);
/// // server.start().await?;
/// //
/// // Client calls:
/// // POST /rpc
/// // {
/// //   "jsonrpc": "2.0",
/// //   "id": 1,
/// //   "method": "tools/list",
/// //   "params": {}
/// // }
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

/// Stub implementation for Phase 3
///
/// This implementation provides the basic structure for MCP integration.
/// Phase 4 will replace this with a full HTTP server implementation.
#[derive(Debug)]
pub struct MCPServerStub;

#[async_trait::async_trait]
impl MCPToolServer for MCPServerStub {
    async fn list_tools(&self) -> Result<Value> {
        // Phase 4 Implementation:
        // 1. Get all tools from registry
        // 2. Convert to MCP format
        // 3. Apply caching
        // 4. Return JSON array
        Ok(serde_json::json!({
            "tools": [],
            "message": "Phase 4: Full implementation pending"
        }))
    }

    async fn invoke_tool(&self, _tool_name: &str, _arguments: Value) -> Result<Value> {
        // Phase 4 Implementation:
        // 1. Look up tool in registry
        // 2. Validate input against signature
        // 3. Call tool through registry
        // 4. Format and return output
        Err(crate::GgenAiError::Other {
            message: "MCP server implementation deferred to Phase 4".to_string(),
        })
    }

    async fn validate_tool_input(
        &self,
        _tool_name: &str,
        _arguments: Value,
    ) -> Result<Value> {
        // Phase 4 Implementation:
        // 1. Get tool signature
        // 2. Validate input without executing
        // 3. Return detailed validation result
        Ok(serde_json::json!({
            "valid": true,
            "errors": [],
            "message": "Phase 4: Full validation pending"
        }))
    }

    async fn get_tool_schema(&self, _tool_name: &str) -> Result<Value> {
        // Phase 4 Implementation:
        // 1. Get tool signature
        // 2. Convert to JSON Schema
        // 3. Add examples and metadata
        // 4. Return OpenAPI-compatible schema
        Ok(serde_json::json!({
            "schema": {},
            "message": "Phase 4: Full schema export pending"
        }))
    }

    async fn health_check(&self) -> Result<Value> {
        Ok(serde_json::json!({
            "status": "operational",
            "message": "MCP server stub - Phase 4 implementation pending"
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stub_list_tools() {
        let server = MCPServerStub;
        let result = server.list_tools().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_stub_health_check() {
        let server = MCPServerStub;
        let result = server.health_check().await;
        assert!(result.is_ok());
        let value = result.unwrap();
        assert_eq!(value["status"], "operational");
    }

    #[tokio::test]
    async fn test_stub_invoke_tool() {
        let server = MCPServerStub;
        let result = server
            .invoke_tool("TestTool", serde_json::json!({}))
            .await;
        assert!(result.is_err());
    }
}
