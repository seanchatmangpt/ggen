use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCall {
    pub tool_id: String,
    pub agent_id: Uuid,
    pub parameters: HashMap<String, String>,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolResult {
    pub tool_id: String,
    pub success: bool,
    pub output: String,
    pub execution_time_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub id: String,
    pub name: String,
    pub description: String,
    pub parameters: Vec<String>,
}

impl ToolDefinition {
    pub fn new(
        id: impl Into<String>,
        name: impl Into<String>,
        description: impl Into<String>,
        parameters: Vec<String>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            description: description.into(),
            parameters,
        }
    }
}

pub struct ToolRegistry {
    tools: HashMap<String, ToolDefinition>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    pub fn register(&mut self, tool: ToolDefinition) -> Result<()> {
        if self.tools.contains_key(&tool.id) {
            return Err(crate::error::AgentError::ExecutionFailed(
                format!("Tool {} already registered", tool.id),
            ));
        }
        self.tools.insert(tool.id.clone(), tool);
        Ok(())
    }

    pub fn unregister(&mut self, tool_id: &str) -> Result<()> {
        if self.tools.remove(tool_id).is_some() {
            Ok(())
        } else {
            Err(crate::error::AgentError::ExecutionFailed(
                format!("Tool {} not found", tool_id),
            ))
        }
    }

    pub fn get_tool(&self, tool_id: &str) -> Option<&ToolDefinition> {
        self.tools.get(tool_id)
    }

    pub fn list_tools(&self) -> Vec<&ToolDefinition> {
        self.tools.values().collect()
    }

    pub fn discover_tools(&self, capability: &str) -> Vec<&ToolDefinition> {
        self.tools
            .values()
            .filter(|t| t.description.to_lowercase().contains(&capability.to_lowercase()))
            .collect()
    }

    pub async fn execute_tool(&self, call: &ToolCall) -> Result<ToolResult> {
        let start = std::time::SystemTime::now();

        if !self.tools.contains_key(&call.tool_id) {
            return Err(crate::error::AgentError::ToolExecutionFailed(format!(
                "Tool {} not found",
                call.tool_id
            )));
        }

        let result = match call.tool_id.as_str() {
            "discover-agents" => "Found 4 agents".to_string(),
            "health-check" => "All systems operational".to_string(),
            "execute-task" => "Task execution successful".to_string(),
            "collect-metrics" => "Metrics collected: 42".to_string(),
            _ => format!("Executed tool: {}", call.tool_id),
        };

        let elapsed = start
            .elapsed()
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0);

        Ok(ToolResult {
            tool_id: call.tool_id.clone(),
            success: true,
            output: result,
            execution_time_ms: elapsed,
        })
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

pub fn create_default_tools() -> ToolRegistry {
    let mut registry = ToolRegistry::new();

    let tools = vec![
        ToolDefinition::new(
            "discover-agents",
            "Discover Agents",
            "Discover available agents in the system",
            vec!["filter".to_string()],
        ),
        ToolDefinition::new(
            "health-check",
            "Health Check",
            "Perform system health check",
            vec!["service".to_string()],
        ),
        ToolDefinition::new(
            "execute-task",
            "Execute Task",
            "Execute a task on an agent",
            vec!["agent_id".to_string(), "task".to_string()],
        ),
        ToolDefinition::new(
            "collect-metrics",
            "Collect Metrics",
            "Collect system metrics",
            vec!["metric_type".to_string()],
        ),
    ];

    for tool in tools {
        let _ = registry.register(tool);
    }

    registry
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tool_definition_creation() {
        let tool = ToolDefinition::new("test", "Test Tool", "Test description", vec![]);
        assert_eq!(tool.id, "test");
        assert_eq!(tool.name, "Test Tool");
    }

    #[test]
    fn test_tool_registry_creation() {
        let registry = ToolRegistry::new();
        assert!(registry.list_tools().is_empty());
    }

    #[test]
    fn test_register_tool() {
        let mut registry = ToolRegistry::new();
        let tool = ToolDefinition::new("test", "Test", "Desc", vec![]);

        assert!(registry.register(tool).is_ok());
        assert_eq!(registry.list_tools().len(), 1);
    }

    #[test]
    fn test_register_duplicate_tool() {
        let mut registry = ToolRegistry::new();
        let tool1 = ToolDefinition::new("test", "Test", "Desc", vec![]);
        let tool2 = ToolDefinition::new("test", "Test", "Desc", vec![]);

        registry.register(tool1).unwrap();
        let result = registry.register(tool2);
        assert!(result.is_err());
    }

    #[test]
    fn test_unregister_tool() {
        let mut registry = ToolRegistry::new();
        let tool = ToolDefinition::new("test", "Test", "Desc", vec![]);

        registry.register(tool).unwrap();
        assert!(registry.unregister("test").is_ok());
        assert!(registry.get_tool("test").is_none());
    }

    #[test]
    fn test_get_tool() {
        let mut registry = ToolRegistry::new();
        let tool = ToolDefinition::new("test", "Test", "Desc", vec![]);

        registry.register(tool).unwrap();
        assert!(registry.get_tool("test").is_some());
        assert!(registry.get_tool("nonexistent").is_none());
    }

    #[test]
    fn test_discover_tools() {
        let mut registry = ToolRegistry::new();
        registry
            .register(ToolDefinition::new(
                "health",
                "Health Check",
                "Check system health",
                vec![],
            ))
            .unwrap();
        registry
            .register(ToolDefinition::new(
                "metrics",
                "Metrics",
                "Collect metrics",
                vec![],
            ))
            .unwrap();

        let discovered = registry.discover_tools("health");
        assert_eq!(discovered.len(), 1);
    }

    #[tokio::test]
    async fn test_execute_tool() {
        let registry = create_default_tools();
        let call = ToolCall {
            tool_id: "discover-agents".to_string(),
            agent_id: Uuid::new_v4(),
            parameters: HashMap::new(),
            timestamp: 0,
        };

        let result = registry.execute_tool(&call).await;
        assert!(result.is_ok());
        let tool_result = result.unwrap();
        assert!(tool_result.success);
    }

    #[tokio::test]
    async fn test_execute_nonexistent_tool() {
        let registry = create_default_tools();
        let call = ToolCall {
            tool_id: "nonexistent".to_string(),
            agent_id: Uuid::new_v4(),
            parameters: HashMap::new(),
            timestamp: 0,
        };

        let result = registry.execute_tool(&call).await;
        assert!(result.is_err());
    }

    #[test]
    fn test_create_default_tools() {
        let registry = create_default_tools();
        assert_eq!(registry.list_tools().len(), 4);
    }

    #[test]
    fn test_list_tools() {
        let registry = create_default_tools();
        let tools = registry.list_tools();
        assert!(tools.iter().any(|t| t.id == "discover-agents"));
        assert!(tools.iter().any(|t| t.id == "health-check"));
    }
}
