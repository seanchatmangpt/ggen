// MCP Tool definitions and utilities
// This module provides types and utilities for exposing REST endpoints as MCP tools

use serde::{Deserialize, Serialize};

/// Represents a single MCP tool that wraps a REST endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPTool {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
    pub endpoint: String,
    pub method: String,
    pub required_params: Vec<String>,
}

impl MCPTool {
    /// Create a new MCP tool
    pub fn new(
        name: impl Into<String>,
        description: impl Into<String>,
        endpoint: impl Into<String>,
        method: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            input_schema: serde_json::json!({}),
            endpoint: endpoint.into(),
            method: method.into(),
            required_params: vec![],
        }
    }

    /// Add input schema for tool parameters
    pub fn with_schema(mut self, schema: serde_json::Value) -> Self {
        self.input_schema = schema;
        self
    }

    /// Add required parameters
    pub fn with_required_params(mut self, params: Vec<String>) -> Self {
        self.required_params = params;
        self
    }
}

/// Registry for managing available MCP tools
#[derive(Debug, Clone)]
pub struct ToolRegistry {
    tools: Vec<MCPTool>,
}

impl ToolRegistry {
    /// Create a new empty tool registry
    pub fn new() -> Self {
        Self { tools: vec![] }
    }

    /// Register a new tool
    pub fn register(&mut self, tool: MCPTool) -> Result<(), String> {
        if self.tools.iter().any(|t| t.name == tool.name) {
            return Err(format!("Tool {} already registered", tool.name));
        }
        self.tools.push(tool);
        Ok(())
    }

    /// Get all registered tools
    pub fn get_all(&self) -> &[MCPTool] {
        &self.tools
    }

    /// Get a tool by name
    pub fn get_by_name(&self, name: &str) -> Option<&MCPTool> {
        self.tools.iter().find(|t| t.name == name)
    }

    /// Count registered tools
    pub fn count(&self) -> usize {
        self.tools.len()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_tool_creation() {
        let tool = MCPTool::new("test_tool", "A test tool", "/test", "GET");
        assert_eq!(tool.name, "test_tool");
        assert_eq!(tool.endpoint, "/test");
    }

    #[test]
    fn test_mcp_tool_with_schema() {
        let schema = serde_json::json!({
            "type": "object",
            "properties": {
                "id": { "type": "string" }
            }
        });
        let tool = MCPTool::new("get_item", "Get an item", "/items/{id}", "GET")
            .with_schema(schema.clone());
        assert_eq!(tool.input_schema, schema);
    }

    #[test]
    fn test_mcp_tool_with_params() {
        let tool = MCPTool::new("create_item", "Create item", "/items", "POST")
            .with_required_params(vec!["name".to_string(), "email".to_string()]);
        assert_eq!(tool.required_params.len(), 2);
    }

    #[test]
    fn test_tool_registry_new() {
        let registry = ToolRegistry::new();
        assert_eq!(registry.count(), 0);
    }

    #[test]
    fn test_tool_registry_register() {
        let mut registry = ToolRegistry::new();
        let tool = MCPTool::new("list", "List items", "/items", "GET");
        assert!(registry.register(tool).is_ok());
        assert_eq!(registry.count(), 1);
    }

    #[test]
    fn test_tool_registry_duplicate() {
        let mut registry = ToolRegistry::new();
        let tool1 = MCPTool::new("list", "List items", "/items", "GET");
        let tool2 = MCPTool::new("list", "Another list", "/items2", "GET");

        assert!(registry.register(tool1).is_ok());
        assert!(registry.register(tool2).is_err());
    }

    #[test]
    fn test_tool_registry_get_by_name() {
        let mut registry = ToolRegistry::new();
        let tool = MCPTool::new("get_item", "Get item", "/items/{id}", "GET");
        registry.register(tool).unwrap();

        let found = registry.get_by_name("get_item");
        assert!(found.is_some());
        assert_eq!(found.unwrap().endpoint, "/items/{id}");
    }

    #[test]
    fn test_tool_registry_get_all() {
        let mut registry = ToolRegistry::new();
        registry
            .register(MCPTool::new("tool1", "First", "/api/1", "GET"))
            .unwrap();
        registry
            .register(MCPTool::new("tool2", "Second", "/api/2", "POST"))
            .unwrap();

        let all = registry.get_all();
        assert_eq!(all.len(), 2);
    }
}
