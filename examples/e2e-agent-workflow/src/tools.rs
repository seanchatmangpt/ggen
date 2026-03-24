//! MCP tool discovery and integration
//!
//! Manages available tools and enables agent planning with tools.

use crate::error::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Tool capability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCapability {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
    pub output_schema: serde_json::Value,
}

/// Tool information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tool {
    pub id: Uuid,
    pub name: String,
    pub category: String,
    pub capabilities: Vec<ToolCapability>,
    pub is_available: bool,
    pub execution_time_ms: u32,
}

#[async_trait]
pub trait ToolExecutor: Send + Sync {
    async fn execute(&self, tool_id: Uuid, params: serde_json::Value) -> Result<serde_json::Value>;
}

/// Tool registry and discovery
pub struct ToolRegistry {
    tools: HashMap<Uuid, Tool>,
}

impl ToolRegistry {
    /// Create a new tool registry
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    /// Register a tool
    pub fn register_tool(&mut self, tool: Tool) -> Uuid {
        let id = tool.id;
        self.tools.insert(id, tool);
        id
    }

    /// Create a predefined tool
    pub fn create_tool(
        &mut self,
        name: String,
        category: String,
        capabilities: Vec<ToolCapability>,
    ) -> Tool {
        let tool = Tool {
            id: Uuid::new_v4(),
            name,
            category,
            capabilities,
            is_available: true,
            execution_time_ms: 100,
        };

        self.tools.insert(tool.id, tool.clone());
        tool
    }

    /// Get tool by ID
    pub fn get_tool(&self, tool_id: Uuid) -> Result<&Tool> {
        self.tools.get(&tool_id)
            .ok_or_else(|| crate::WorkflowError::ToolError("Tool not found".to_string()))
    }

    /// Get available tools
    pub fn get_available_tools(&self) -> Vec<Tool> {
        self.tools
            .values()
            .filter(|t| t.is_available)
            .cloned()
            .collect()
    }

    /// Search tools by category
    pub fn search_by_category(&self, category: &str) -> Vec<Tool> {
        self.tools
            .values()
            .filter(|t| t.category.contains(category) && t.is_available)
            .cloned()
            .collect()
    }

    /// List all tools
    pub fn list_tools(&self) -> Vec<Tool> {
        self.tools.values().cloned().collect()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Tool discovery engine
pub struct ToolDiscovery {
    registry: ToolRegistry,
}

impl ToolDiscovery {
    /// Create a new tool discovery engine
    pub fn new(registry: ToolRegistry) -> Self {
        Self { registry }
    }

    /// Discover tools matching a query
    pub fn discover(&self, query: &str) -> Vec<Tool> {
        let query_lower = query.to_lowercase();
        self.registry
            .get_available_tools()
            .into_iter()
            .filter(|tool| {
                tool.name.to_lowercase().contains(&query_lower)
                    || tool.category.to_lowercase().contains(&query_lower)
                    || tool
                        .capabilities
                        .iter()
                        .any(|c| c.description.to_lowercase().contains(&query_lower))
            })
            .collect()
    }

    /// Get tools for domain
    pub fn tools_for_domain(&self, domain: &str) -> Vec<Tool> {
        self.registry.search_by_category(domain)
    }

    /// Rank tools by relevance
    pub fn rank_tools(&self, query: &str, tools: &[Tool]) -> Vec<(Tool, f64)> {
        let query_lower = query.to_lowercase();
        let mut ranked: Vec<_> = tools
            .iter()
            .map(|tool| {
                let name_match = if tool.name.to_lowercase() == query_lower { 100.0 } else { 0.0 };
                let category_match = if tool.category.to_lowercase().contains(&query_lower) {
                    50.0
                } else {
                    0.0
                };
                let capability_match = tool
                    .capabilities
                    .iter()
                    .map(|c| {
                        if c.description.to_lowercase().contains(&query_lower) {
                            25.0
                        } else {
                            0.0
                        }
                    })
                    .sum::<f64>()
                    / tool.capabilities.len().max(1) as f64;

                let score = name_match + category_match + capability_match;
                (tool.clone(), score)
            })
            .collect();

        ranked.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        ranked
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_tool() {
        let mut registry = ToolRegistry::new();
        let tool = registry.create_tool(
            "calculator".to_string(),
            "math".to_string(),
            vec![
                ToolCapability {
                    name: "add".to_string(),
                    description: "Add two numbers".to_string(),
                    input_schema: serde_json::json!({}),
                    output_schema: serde_json::json!({}),
                }
            ],
        );

        assert_eq!(tool.name, "calculator");
        assert_eq!(tool.category, "math");
        assert!(tool.is_available);
    }

    #[test]
    fn test_discover_tools() {
        let mut registry = ToolRegistry::new();
        registry.create_tool(
            "calculator".to_string(),
            "math".to_string(),
            vec![],
        );
        registry.create_tool(
            "file_reader".to_string(),
            "file_io".to_string(),
            vec![],
        );

        let discovery = ToolDiscovery::new(registry);
        let found = discovery.discover("math");

        assert_eq!(found.len(), 1);
        assert_eq!(found[0].name, "calculator");
    }

    #[test]
    fn test_tool_ranking() {
        let mut registry = ToolRegistry::new();
        let tool1 = registry.create_tool(
            "calculator".to_string(),
            "math".to_string(),
            vec![],
        );
        let tool2 = registry.create_tool(
            "math_solver".to_string(),
            "utilities".to_string(),
            vec![],
        );

        let discovery = ToolDiscovery::new(registry);
        let all_tools = discovery.registry.get_available_tools();
        let ranked = discovery.rank_tools("calculator", &all_tools);

        assert!(!ranked.is_empty());
        assert_eq!(ranked[0].0.id, tool1.id);
    }
}
