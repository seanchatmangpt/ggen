//! Agent Registry - Discovery and lifecycle management
//!
//! Core team best practice: Simple registry for agent discovery

use super::{Agent, AgentHealth};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Agent registry for managing agent lifecycle
#[derive(Debug, Clone)]
pub struct AgentRegistry {
    agents: Arc<RwLock<HashMap<String, Arc<dyn Agent>>>>,
}

impl AgentRegistry {
    /// Create a new agent registry
    pub fn new() -> Self {
        Self {
            agents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register an agent
    pub async fn register(&self, agent: Arc<dyn Agent>) {
        let name = agent.config().name.clone();
        let mut agents = self.agents.write().await;
        agents.insert(name, agent);
    }

    /// Get agent by name
    pub async fn get(&self, name: &str) -> Option<Arc<dyn Agent>> {
        let agents = self.agents.read().await;
        agents.get(name).cloned()
    }

    /// List all agent names
    pub async fn list(&self) -> Vec<String> {
        let agents = self.agents.read().await;
        agents.keys().cloned().collect()
    }

    /// Get health status of all agents
    pub async fn health_check_all(&self) -> HashMap<String, AgentHealth> {
        let agents = self.agents.read().await;
        let mut health_map = HashMap::new();

        for (name, agent) in agents.iter() {
            let status = agent.status().await;
            let health = AgentHealth {
                status,
                score: 1.0,
                last_check: chrono::Utc::now().to_rfc3339(),
                issues: vec![],
            };
            health_map.insert(name.clone(), health);
        }

        health_map
    }

    /// Remove agent from registry
    pub async fn unregister(&self, name: &str) -> bool {
        let mut agents = self.agents.write().await;
        agents.remove(name).is_some()
    }

    /// Get agent count
    pub async fn count(&self) -> usize {
        let agents = self.agents.read().await;
        agents.len()
    }
}

impl Default for AgentRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agents::{AgentConfig, AgentMessage, AgentRole, AgentStatus};
    use async_trait::async_trait;
    use uuid::Uuid;

    #[derive(Debug)]
    struct TestAgent {
        config: AgentConfig,
        status: AgentStatus,
    }

    #[async_trait]
    impl Agent for TestAgent {
        async fn initialize(
            &mut self,
        ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn start(
            &mut self,
        ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn stop(
            &mut self,
        ) -> std::result::Result<(), Box<dyn std::error::Error + Send + Sync>> {
            Ok(())
        }

        async fn status(&self) -> AgentStatus {
            self.status.clone()
        }

        fn config(&self) -> &AgentConfig {
            &self.config
        }

        async fn handle_message(
            &mut self, message: AgentMessage,
        ) -> std::result::Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
            Ok(message)
        }
    }

    #[tokio::test]
    async fn test_registry_operations() {
        let registry = AgentRegistry::new();

        // Register agent
        let agent = Arc::new(TestAgent {
            config: AgentConfig {
                id: Uuid::new_v4(),
                name: "test-agent".to_string(),
                role: AgentRole::Validator,
                capabilities: vec![],
                max_concurrent_tasks: 10,
            },
            status: AgentStatus::Healthy,
        });
        registry.register(agent).await;

        // Get agent
        let retrieved = registry.get("test-agent").await;
        assert!(retrieved.is_some());

        // List agents
        let list = registry.list().await;
        assert_eq!(list.len(), 1);

        // Count
        assert_eq!(registry.count().await, 1);

        // Unregister
        assert!(registry.unregister("test-agent").await);
        assert_eq!(registry.count().await, 0);
    }
}
