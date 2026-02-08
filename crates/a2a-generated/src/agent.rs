//! Agent domain structs and traits
//!
//! This module defines the core abstractions for AI agents in the
//! agent-to-agent (A2A) communication system.

/// Agent represents an intelligent entity in the A2A system
#[derive(Debug, Clone)]
pub struct Agent {
    /// Unique identifier for the agent
    pub id: String,
    /// Human-readable name for the agent
    pub name: String,
    /// Type/class of the agent
    pub agent_type: String,
    /// Configuration for the agent
    pub config: serde_json::Value,
}

/// Trait for agent behavior and capabilities
pub trait AgentBehavior: Send + Sync {
    /// Initialize the agent with given configuration
    fn initialize(&mut self, config: &serde_json::Value) -> Result<(), String>;

    /// Execute the agent's main logic
    fn execute(
        &mut self, input: serde_json::Value,
    ) -> impl std::future::Future<Output = Result<serde_json::Value, String>> + Send;

    /// Shutdown the agent gracefully
    fn shutdown(&mut self) -> Result<(), String>;
}

/// Default implementation for AgentBehavior
pub struct DefaultAgent {
    agent: Agent,
    initialized: bool,
}

impl DefaultAgent {
    pub fn new(agent: Agent) -> Self {
        Self {
            agent,
            initialized: false,
        }
    }
}

impl AgentBehavior for DefaultAgent {
    fn initialize(&mut self, config: &serde_json::Value) -> Result<(), String> {
        self.agent.config = config.clone();
        self.initialized = true;
        Ok(())
    }

    async fn execute(&mut self, input: serde_json::Value) -> Result<serde_json::Value, String> {
        if !self.initialized {
            return Err("Agent not initialized".to_string());
        }

        // Placeholder implementation
        Ok(serde_json::json!({
            "status": "completed",
            "input": input,
            "output": {},
            "agent_id": self.agent.id
        }))
    }

    fn shutdown(&mut self) -> Result<(), String> {
        self.initialized = false;
        Ok(())
    }
}

/// Agent factory for creating agents
pub struct AgentFactory;

impl AgentFactory {
    pub fn create_agent(agent_type: &str, id: &str, name: &str) -> Agent {
        Agent {
            id: id.to_string(),
            name: name.to_string(),
            agent_type: agent_type.to_string(),
            config: serde_json::json!({}),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_creation() {
        let agent = AgentFactory::create_agent("test", "123", "Test Agent");
        assert_eq!(agent.id, "123");
        assert_eq!(agent.name, "Test Agent");
        assert_eq!(agent.agent_type, "test");
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let agent = AgentFactory::create_agent("test", "123", "Test Agent");
        let mut default_agent = DefaultAgent::new(agent);

        default_agent.initialize(&serde_json::json!({})).unwrap();

        let result = default_agent
            .execute(serde_json::json!({"test": "input"}))
            .await
            .unwrap();
        assert_eq!(result["status"].as_str().unwrap(), "completed");
        assert_eq!(result["agent_id"].as_str().unwrap(), "123");
    }
}
