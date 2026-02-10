//! Adapters for converting between A2A and LLM tool formats

use crate::error::{A2aMcpError, A2aMcpResult};
use crate::message::A2aMessageConverter;
use a2a_generated::converged::message::{ConvergedMessage, UnifiedContent};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;

/// Tool definition for LLM integration
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Tool {
    pub name: String,
    pub description: String,
    pub parameters: Option<Value>,
}

/// Tool call from LLM
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ToolCall {
    pub method: String,
    pub params: Value,
}

/// Tool response to LLM
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ToolResponse {
    pub result: Value,
}

/// Adapts A2A agent capabilities to LLM tool format
pub struct AgentToToolAdapter {
    converter: Arc<A2aMessageConverter>,
}

impl AgentToToolAdapter {
    /// Create a new adapter
    pub fn new() -> Self {
        Self {
            converter: Arc::new(A2aMessageConverter::new()),
        }
    }

    /// Create a new adapter with a custom converter
    pub fn with_converter(mut self, converter: A2aMessageConverter) -> Self {
        self.converter = Arc::new(converter);
        self
    }

    /// Generate tools from A2A agent capabilities
    pub fn generate_tools(&self, agent_name: &str, capabilities: &[&str]) -> Vec<Tool> {
        capabilities.iter().map(|cap| Tool {
            name: format!("{}:{}", agent_name, cap),
            description: format!("{} capability", cap),
            parameters: None,
        }).collect()
    }

    /// Convert LLM tool call to A2A message
    pub fn tool_call_to_message(&self, call: &ToolCall, agent_id: &str) -> A2aMcpResult<ConvergedMessage> {
        let content = format!(
            "Execute tool: {}\nParameters: {}",
            call.method,
            serde_json::to_string_pretty(&call.params).unwrap_or_else(|_| call.params.to_string())
        );

        Ok(ConvergedMessage::text(
            format!("tool-call-{}", uuid::Uuid::new_v4()),
            agent_id.to_string(),
            content,
        ))
    }

    /// Convert A2A message to tool response
    pub fn message_to_tool_response(&self, message: &ConvergedMessage) -> A2aMcpResult<ToolResponse> {
        let content = match &message.payload.content {
            UnifiedContent::Text { content, .. } => {
                json!({ "result": content })
            }
            UnifiedContent::Data { data, .. } => {
                json!({ "result": data })
            }
            _ => return Err(A2aMcpError::Translation("Unsupported content type for tool response".to_string())),
        };

        Ok(ToolResponse { result: content })
    }
}

impl Default for AgentToToolAdapter {
    fn default() -> Self {
        Self::new()
    }
}

/// Adapts LLM tools to A2A agent format
pub struct ToolToAgentAdapter {
    tools: HashMap<String, Tool>,
    agent_name: String,
    agent_description: String,
}

impl ToolToAgentAdapter {
    /// Create a new adapter
    pub fn new(agent_name: String, agent_description: String) -> Self {
        Self {
            tools: HashMap::new(),
            agent_name,
            agent_description,
        }
    }

    /// Add a tool to the adapter
    pub fn add_tool(&mut self, tool: Tool) {
        self.tools.insert(tool.name.clone(), tool);
    }

    /// Get agent card for A2A protocol
    pub fn agent_card(&self) -> A2aAgentCard {
        A2aAgentCard {
            name: self.agent_name.clone(),
            description: self.agent_description.clone(),
            capabilities: self.tools.keys().cloned().collect(),
        }
    }

    /// Find a tool by name
    pub fn find_tool(&self, name: &str) -> Option<&Tool> {
        self.tools.get(name)
    }
}

/// A2A Agent card for protocol negotiation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct A2aAgentCard {
    pub name: String,
    pub description: String,
    pub capabilities: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_tools() {
        let adapter = AgentToToolAdapter::new();
        let tools = adapter.generate_tools("test-agent", &["read", "write"]);

        assert_eq!(tools.len(), 2);
        assert_eq!(tools[0].name, "test-agent:read");
        assert_eq!(tools[1].name, "test-agent:write");
    }

    #[test]
    fn test_tool_to_agent_adapter() {
        let mut adapter = ToolToAgentAdapter::new(
            "test-agent".to_string(),
            "A test agent".to_string(),
        );

        adapter.add_tool(Tool {
            name: "read".to_string(),
            description: "Read data".to_string(),
            parameters: None,
        });

        let card = adapter.agent_card();
        assert_eq!(card.name, "test-agent");
        assert_eq!(card.capabilities.len(), 1);
        assert!(adapter.find_tool("read").is_some());
    }
}
