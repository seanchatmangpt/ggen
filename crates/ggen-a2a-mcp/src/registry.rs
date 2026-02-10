//! MCP Tool Registry for ggen-a2a-mcp integration
//!
//! Provides bridge between ggen-ai ToolRegistry and MCP tool format.
//! Converts ggen tools with their DSPy Signatures into MCP-compatible
//! tool definitions with proper JSON schemas.

use crate::error::{A2aMcpError, A2aMcpResult};
use ggen_ai::tool::Tool as GgenTool;
use ggen_ai::tool_registry::REGISTRY as GGEN_REGISTRY;
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP tool definition with schema and execution handler
#[derive(Debug, Clone)]
pub struct McpTool {
    /// Tool identifier (must match ggen tool ID)
    pub id: String,

    /// Human-readable name
    pub name: String,

    /// Tool description
    pub description: String,

    /// JSON Schema for input validation
    pub input_schema: serde_json::Value,

    /// Whether tool requires authentication
    pub requires_auth: bool,

    /// Execution timeout in milliseconds
    pub timeout_ms: u64,

    /// Whether execution can be cached
    pub cacheable: bool,
}

impl McpTool {
    /// Create a new MCP tool definition
    pub fn new(
        id: impl Into<String>,
        name: impl Into<String>,
        description: impl Into<String>,
        input_schema: serde_json::Value,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            description: description.into(),
            input_schema,
            requires_auth: false,
            timeout_ms: 30_000,
            cacheable: true,
        }
    }

    /// Set authentication requirement
    pub fn with_auth(mut self, requires_auth: bool) -> Self {
        self.requires_auth = requires_auth;
        self
    }

    /// Set execution timeout
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    /// Set cacheability
    pub fn with_cacheable(mut self, cacheable: bool) -> Self {
        self.cacheable = cacheable;
        self
    }

    /// Convert to MCP protocol tool definition
    pub fn to_mcp_definition(&self) -> serde_json::Value {
        json!({
            "name": self.id,
            "description": self.description,
            "inputSchema": self.input_schema
        })
    }
}

/// Result of tool execution
#[derive(Debug, Clone)]
pub struct ToolExecutionResult {
    /// Tool identifier that was executed
    pub tool_id: String,

    /// Execution result data
    pub result: serde_json::Value,

    /// Whether execution was successful
    pub success: bool,

    /// Error message if execution failed
    pub error: Option<String>,

    /// Execution duration in milliseconds
    pub duration_ms: u64,
}

impl ToolExecutionResult {
    /// Create a successful result
    pub fn success(tool_id: impl Into<String>, result: serde_json::Value, duration_ms: u64) -> Self {
        Self {
            tool_id: tool_id.into(),
            result,
            success: true,
            error: None,
            duration_ms,
        }
    }

    /// Create a failed result
    pub fn failure(tool_id: impl Into<String>, error: impl Into<String>, duration_ms: u64) -> Self {
        Self {
            tool_id: tool_id.into(),
            result: json!(null),
            success: false,
            error: Some(error.into()),
            duration_ms,
        }
    }

    /// Convert to JSON value for MCP response
    pub fn to_json_response(&self) -> serde_json::Value {
        if self.success {
            json!({
                "content": [{
                    "type": "text",
                    "text": serde_json::to_string_pretty(&self.result)
                        .unwrap_or_else(|_| self.result.to_string())
                }]
            })
        } else {
            json!({
                "error": {
                    "code": -32603,
                    "message": self.error.as_deref().unwrap_or("Unknown error"),
                    "data": self.tool_id
                }
            })
        }
    }
}

/// Registry for MCP tools backed by ggen-ai ToolRegistry
///
/// Maintains a cache of MCP tool definitions synchronized with
/// the ggen-ai global tool registry. Provides async-safe access
/// for tool execution and schema generation.
#[derive(Debug)]
pub struct McpToolRegistry {
    /// Cached MCP tool definitions
    tools: RwLock<HashMap<String, McpTool>>,

    /// Whether cache is synchronized with ggen registry
    synced: Arc<std::sync::atomic::AtomicBool>,
}

impl McpToolRegistry {
    /// Create a new MCP tool registry
    pub fn new() -> Self {
        Self {
            tools: RwLock::new(HashMap::new()),
            synced: Arc::new(std::sync::atomic::AtomicBool::new(false)),
        }
    }

    /// Synchronize with ggen-ai tool registry
    ///
    /// Loads all tools from ggen-ai and converts them to MCP format.
    /// This should be called at startup and when tools are updated.
    pub async fn sync(&self) -> A2aMcpResult<usize> {
        let registry = GGEN_REGISTRY
            .read()
            .map_err(|e| A2aMcpError::Translation(format!("Failed to acquire registry lock: {}", e)))?;

        let ggen_tools = registry.list();
        let mut mcp_tools = HashMap::new();

        for (id, ggen_tool) in ggen_tools {
            let mcp_tool = Self::convert_ggen_tool_to_mcp(ggen_tool)?;
            mcp_tools.insert(id, mcp_tool);
        }

        let mut tools = self.tools.write().await;
        *tools = mcp_tools;

        self.synced.store(true, std::sync::atomic::Ordering::Release);

        Ok(tools.len())
    }

    /// Get tool by ID
    pub async fn get(&self, id: &str) -> A2aMcpResult<McpTool> {
        // Auto-sync if not yet synchronized
        if !self.synced.load(std::sync::atomic::Ordering::Acquire) {
            self.sync().await?;
        }

        let tools = self.tools.read().await;
        tools
            .get(id)
            .cloned()
            .ok_or_else(|| A2aMcpError::AgentNotFound(id.to_string()))
    }

    /// List all registered tools
    pub async fn list(&self) -> A2aMcpResult<Vec<McpTool>> {
        // Auto-sync if not yet synchronized
        if !self.synced.load(std::sync::atomic::Ordering::Acquire) {
            self.sync().await?;
        }

        let tools = self.tools.read().await;
        Ok(tools.values().cloned().collect())
    }

    /// Export all tools as MCP tool definitions
    pub async fn export_mcp_tools(&self) -> A2aMcpResult<serde_json::Value> {
        let tools = self.list().await?;
        let definitions: Vec<_> = tools.iter().map(McpTool::to_mcp_definition).collect();

        Ok(json!(definitions))
    }

    /// Validate tool input against schema
    pub async fn validate_input(&self, tool_id: &str, input: &serde_json::Value) -> A2aMcpResult<()> {
        let tool = self.get(tool_id).await?;

        // Basic required field validation
        if let Some(required) = tool.input_schema.get("required") {
            if let Some(required_array) = required.as_array() {
                for field_name in required_array {
                    if let Some(field) = field_name.as_str() {
                        if !input.get(field).is_some() {
                            return Err(A2aMcpError::Translation(format!(
                                "Tool '{}' requires input field '{}'",
                                tool_id, field
                            )));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Execute a tool by ID
    ///
    /// Note: This is a placeholder for actual tool execution.
    /// The actual implementation would dispatch to the appropriate
    /// tool handler based on the tool ID.
    pub async fn execute_tool(
        &self,
        tool_id: &str,
        input: serde_json::Value,
    ) -> A2aMcpResult<ToolExecutionResult> {
        let start = std::time::Instant::now();

        // Validate input first
        self.validate_input(tool_id, &input).await?;

        // Get the ggen tool for execution
        let registry = GGEN_REGISTRY
            .read()
            .map_err(|e| A2aMcpError::Translation(format!("Failed to acquire registry lock: {}", e)))?;

        let ggen_tool = registry.get(tool_id).map_err(|e| {
            A2aMcpError::AgentNotFound(format!("Tool '{}' not found: {}", tool_id, e))
        })?;

        // Execute based on tool type
        // This is where you would dispatch to actual tool implementations
        let result = Self::execute_ggen_tool(ggen_tool, input).await?;

        let duration = start.elapsed().as_millis() as u64;

        Ok(ToolExecutionResult::success(tool_id, result, duration))
    }

    /// Convert ggen Tool to MCP tool format
    fn convert_ggen_tool_to_mcp(ggen_tool: &GgenTool) -> A2aMcpResult<McpTool> {
        let input_schema = ggen_tool.signature.as_json_schema();

        let mcp_tool = McpTool::new(
            &ggen_tool.id,
            &ggen_tool.name,
            &ggen_tool.description,
            input_schema,
        )
        .with_auth(matches!(ggen_tool.auth_scope, ggen_ai::tool::AuthScope::Authenticated | ggen_ai::tool::AuthScope::Admin))
        .with_timeout(ggen_tool.slo.timeout_ms)
        .with_cacheable(ggen_tool.slo.cacheable);

        Ok(mcp_tool)
    }

    /// Execute a ggen tool (placeholder implementation)
    async fn execute_ggen_tool(
        _tool: &GgenTool,
        input: serde_json::Value,
    ) -> A2aMcpResult<serde_json::Value> {
        // This is a placeholder implementation
        // In a real scenario, this would dispatch to the actual tool implementation
        // based on the tool type and ID

        // For now, return the input as the result
        Ok(input)
    }
}

impl Default for McpToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for creating predefined MCP tools
pub struct McpToolBuilder {
    id: String,
    name: String,
    description: String,
    properties: HashMap<String, serde_json::Value>,
    required: Vec<String>,
}

impl McpToolBuilder {
    /// Create a new tool builder
    pub fn new(
        id: impl Into<String>,
        name: impl Into<String>,
        description: impl Into<String>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            description: description.into(),
            properties: HashMap::new(),
            required: Vec::new(),
        }
    }

    /// Add a string property
    pub fn add_string_property(
        mut self,
        name: impl Into<String>,
        description: impl Into<String>,
    ) -> Self {
        let name_str = name.into();
        self.properties.insert(
            name_str.clone(),
            json!({
                "type": "string",
                "description": description.into()
            }),
        );
        self
    }

    /// Add a required string property
    pub fn add_required_string_property(
        mut self,
        name: impl Into<String>,
        description: impl Into<String>,
    ) -> Self {
        let name_str = name.into();
        self.properties.insert(
            name_str.clone(),
            json!({
                "type": "string",
                "description": description.into()
            }),
        );
        self.required.push(name_str);
        self
    }

    /// Add an array property
    pub fn add_array_property(
        mut self,
        name: impl Into<String>,
        description: impl Into<String>,
        item_type: &str,
    ) -> Self {
        self.properties.insert(
            name.into(),
            json!({
                "type": "array",
                "description": description.into(),
                "items": {"type": item_type}
            }),
        );
        self
    }

    /// Build the McpTool
    pub fn build(self) -> McpTool {
        let mut schema = json!({
            "type": "object",
            "properties": serde_json::Map::from_iter(self.properties.into_iter())
        });

        if !self.required.is_empty() {
            if let Some(obj) = schema.as_object_mut() {
                obj.insert("required".to_string(), json!(self.required));
            }
        }

        McpTool::new(self.id, self.name, self.description, schema)
    }
}

/// Register core ggen MCP tools
///
/// Creates the standard tools: agent-list, agent-start, agent-status, workflow-start
pub async fn register_core_tools(registry: &McpToolRegistry) -> A2aMcpResult<Vec<String>> {
    let tool_ids = vec![
        "agent-list",
        "agent-start",
        "agent-status",
        "workflow-start",
    ];

    // Note: These tools should be registered in the ggen-ai ToolRegistry
    // This function ensures they're available for MCP clients
    for id in &tool_ids {
        let _tool = registry.get(id).await
            .map_err(|_| A2aMcpError::AgentNotFound(format!("Core tool '{}' not found in ggen registry", id)))?;
    }

    Ok(tool_ids.into_iter().map(|s| s.to_string()).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_tool_creation() {
        let schema = json!({
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query"
                }
            },
            "required": ["query"]
        });

        let tool = McpTool::new("search", "Search", "Search tool", schema);

        assert_eq!(tool.id, "search");
        assert_eq!(tool.name, "Search");
        assert_eq!(tool.cacheable, true);
    }

    #[test]
    fn test_mcp_tool_with_auth() {
        let schema = json!({
            "type": "object",
            "properties": {}
        });

        let tool = McpTool::new("admin", "Admin", "Admin tool", schema)
            .with_auth(true)
            .with_timeout(60_000);

        assert_eq!(tool.requires_auth, true);
        assert_eq!(tool.timeout_ms, 60_000);
    }

    #[test]
    fn test_mcp_tool_to_mcp_definition() {
        let schema = json!({
            "type": "object",
            "properties": {
                "text": {"type": "string"}
            }
        });

        let tool = McpTool::new("echo", "Echo", "Echo tool", schema);
        let definition = tool.to_mcp_definition();

        assert_eq!(definition["name"], "echo");
        assert_eq!(definition["description"], "Echo tool");
        assert!(definition["inputSchema"].is_object());
    }

    #[test]
    fn test_tool_execution_result_success() {
        let result = ToolExecutionResult::success(
            "test-tool",
            json!({"output": "success"}),
            100,
        );

        assert!(result.success);
        assert_eq!(result.tool_id, "test-tool");
        assert_eq!(result.duration_ms, 100);
        assert!(result.error.is_none());
    }

    #[test]
    fn test_tool_execution_result_failure() {
        let result = ToolExecutionResult::failure(
            "test-tool",
            "Something went wrong",
            50,
        );

        assert!(!result.success);
        assert_eq!(result.error, Some("Something went wrong".to_string()));
    }

    #[test]
    fn test_mcp_tool_builder() {
        let tool = McpToolBuilder::new("search", "Search", "Search tool")
            .add_required_string_property("query", "Search query")
            .add_string_property("limit", "Result limit")
            .build();

        assert_eq!(tool.id, "search");
        assert_eq!(tool.name, "Search");

        let schema = tool.input_schema;
        assert_eq!(schema["type"], "object");
        assert!(schema["properties"]["query"].is_object());
        assert!(schema["properties"]["limit"].is_object());

        let required = schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 1);
        assert_eq!(required[0], "query");
    }

    #[tokio::test]
    async fn test_mcp_registry_creation() {
        let registry = McpToolRegistry::new();
        let tools = registry.list().await;

        assert!(tools.is_ok());
        assert!(tools.unwrap().is_empty());
    }

    #[tokio::test]
    async fn test_mcp_registry_default() {
        let registry = McpToolRegistry::default();
        let tools = registry.list().await;

        assert!(tools.is_ok());
    }
}
