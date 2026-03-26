//! Agent bridging for MCP tool integration
//!
//! Bridges agents as MCP tools, enabling tool discovery and execution

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tool {
    pub name: String,
    pub description: String,
    pub input_schema: serde_json::Value,
    pub output_schema: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolExecution {
    pub id: String,
    pub tool_name: String,
    pub input: serde_json::Value,
    pub output: Option<serde_json::Value>,
    pub error: Option<String>,
    pub success: bool,
}

/// Bridges an agent as MCP tools
pub struct AgentBridge {
    pub agent_id: String,
    pub agent_name: String,
    tools: HashMap<String, Tool>,
    executions: Vec<ToolExecution>,
}

impl AgentBridge {
    pub fn new(agent_id: impl Into<String>, agent_name: impl Into<String>) -> Self {
        AgentBridge {
            agent_id: agent_id.into(),
            agent_name: agent_name.into(),
            tools: HashMap::new(),
            executions: Vec::new(),
        }
    }

    /// Register a tool that the agent exposes
    pub fn register_tool(&mut self, tool: Tool) -> Result<()> {
        if self.tools.contains_key(&tool.name) {
            return Err(anyhow::anyhow!("Tool already registered: {}", tool.name));
        }
        self.tools.insert(tool.name.clone(), tool);
        Ok(())
    }

    /// Get all available tools
    pub fn tools(&self) -> impl Iterator<Item = &Tool> {
        self.tools.values()
    }

    /// Get a specific tool
    pub fn get_tool(&self, name: &str) -> Option<&Tool> {
        self.tools.get(name)
    }

    /// Execute a tool
    pub fn execute_tool(
        &mut self, tool_name: &str, input: serde_json::Value,
    ) -> Result<ToolExecution> {
        let tool = self
            .tools
            .get(tool_name)
            .ok_or_else(|| anyhow::anyhow!("Tool not found: {}", tool_name))?;

        // Validate input against schema
        self.validate_input(&input, &tool.input_schema)?;

        let execution_id = Uuid::new_v4().to_string();

        // Simulate tool execution
        let output = self.simulate_execution(tool_name, &input)?;

        let execution = ToolExecution {
            id: execution_id,
            tool_name: tool_name.to_string(),
            input,
            output: Some(output),
            error: None,
            success: true,
        };

        self.executions.push(execution.clone());
        Ok(execution)
    }

    /// Execute a tool and handle errors
    pub fn execute_tool_safe(
        &mut self, tool_name: &str, input: serde_json::Value,
    ) -> ToolExecution {
        let execution_id = Uuid::new_v4().to_string();

        match self.execute_tool(tool_name, input.clone()) {
            Ok(mut execution) => {
                execution.id = execution_id;
                self.executions.push(execution.clone());
                execution
            }
            Err(e) => {
                let execution = ToolExecution {
                    id: execution_id,
                    tool_name: tool_name.to_string(),
                    input,
                    output: None,
                    error: Some(e.to_string()),
                    success: false,
                };
                self.executions.push(execution.clone());
                execution
            }
        }
    }

    /// Get execution history
    pub fn executions(&self) -> &[ToolExecution] {
        &self.executions
    }

    /// Get tool count
    pub fn tool_count(&self) -> usize {
        self.tools.len()
    }

    /// Get execution count
    pub fn execution_count(&self) -> usize {
        self.executions.len()
    }

    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        if self.executions.is_empty() {
            return 0.0;
        }

        let successes = self.executions.iter().filter(|e| e.success).count();
        successes as f64 / self.executions.len() as f64
    }

    fn validate_input(&self, input: &serde_json::Value, schema: &serde_json::Value) -> Result<()> {
        // Simple validation: check required fields
        if let Some(required) = schema.get("required") {
            if let Some(required_fields) = required.as_array() {
                for field in required_fields {
                    if let Some(field_name) = field.as_str() {
                        if !input.get(field_name).is_some() {
                            return Err(anyhow::anyhow!("Missing required field: {}", field_name));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn simulate_execution(
        &self, tool_name: &str, input: &serde_json::Value,
    ) -> Result<serde_json::Value> {
        // Simulate different tool behaviors
        match tool_name {
            "get_state" => Ok(serde_json::json!({
                "agent_id": self.agent_id,
                "agent_name": self.agent_name,
                "state": "ready"
            })),
            "execute_task" => Ok(serde_json::json!({
                "task_id": input.get("task_id").unwrap_or(&serde_json::json!("unknown")).clone(),
                "status": "completed",
                "result": "success"
            })),
            "get_metrics" => Ok(serde_json::json!({
                "tools_registered": self.tool_count(),
                "executions": self.execution_count(),
                "success_rate": self.success_rate()
            })),
            _ => Ok(serde_json::json!({
                "result": format!("Tool {} executed", tool_name)
            })),
        }
    }
}

/// Builder for creating agents with tools
pub struct BridgeBuilder {
    agent_id: String,
    agent_name: String,
    tools: Vec<Tool>,
}

impl BridgeBuilder {
    pub fn new(agent_id: impl Into<String>, agent_name: impl Into<String>) -> Self {
        BridgeBuilder {
            agent_id: agent_id.into(),
            agent_name: agent_name.into(),
            tools: Vec::new(),
        }
    }

    pub fn add_tool(mut self, tool: Tool) -> Self {
        self.tools.push(tool);
        self
    }

    pub fn build(self) -> Result<AgentBridge> {
        let mut bridge = AgentBridge::new(self.agent_id, self.agent_name);

        for tool in self.tools {
            bridge.register_tool(tool)?;
        }

        Ok(bridge)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_tool() -> Tool {
        Tool {
            name: "test_tool".to_string(),
            description: "A test tool".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "input": { "type": "string" }
                },
                "required": ["input"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "result": { "type": "string" }
                }
            }),
        }
    }

    #[test]
    fn test_bridge_creation() {
        let bridge = AgentBridge::new("agent-1", "TestAgent");
        assert_eq!(bridge.agent_id, "agent-1");
        assert_eq!(bridge.agent_name, "TestAgent");
        assert_eq!(bridge.tool_count(), 0);
    }

    #[test]
    fn test_tool_registration() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();

        assert!(bridge.register_tool(tool).is_ok());
        assert_eq!(bridge.tool_count(), 1);
    }

    #[test]
    fn test_duplicate_tool_registration() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();

        assert!(bridge.register_tool(tool.clone()).is_ok());
        assert!(bridge.register_tool(tool).is_err());
    }

    #[test]
    fn test_tool_execution() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();
        bridge.register_tool(tool).unwrap();

        let execution = bridge
            .execute_tool(
                "test_tool",
                serde_json::json!({
                    "input": "test_input"
                }),
            )
            .unwrap();

        assert!(execution.success);
        assert!(execution.output.is_some());
    }

    #[test]
    fn test_tool_execution_validation() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();
        bridge.register_tool(tool).unwrap();

        // Missing required field
        let result = bridge.execute_tool("test_tool", serde_json::json!({}));

        assert!(result.is_err());
    }

    #[test]
    fn test_tool_execution_safe() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();
        bridge.register_tool(tool).unwrap();

        let execution = bridge.execute_tool_safe(
            "test_tool",
            serde_json::json!({
                "input": "test"
            }),
        );

        assert!(execution.success);
        assert_eq!(bridge.execution_count(), 1);
    }

    #[test]
    fn test_execution_history() {
        let mut bridge = AgentBridge::new("agent-1", "TestAgent");
        let tool = create_test_tool();
        bridge.register_tool(tool).unwrap();

        for i in 0..5 {
            bridge.execute_tool_safe(
                "test_tool",
                serde_json::json!({
                    "input": format!("input_{}", i)
                }),
            );
        }

        assert_eq!(bridge.execution_count(), 5);
        assert!(bridge.success_rate() > 0.0);
    }

    #[test]
    fn test_bridge_builder() {
        let tool1 = create_test_tool();
        let mut tool2 = create_test_tool();
        tool2.name = "tool2".to_string();

        let bridge = BridgeBuilder::new("agent-1", "TestAgent")
            .add_tool(tool1)
            .add_tool(tool2)
            .build()
            .unwrap();

        assert_eq!(bridge.tool_count(), 2);
    }
}
