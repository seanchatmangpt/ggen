//! Agent registry for lifecycle management
//!
//! Provides registration, discovery, and health monitoring for validation agents.
//! Uses ggen's A2A registry pattern for agent orchestration.

use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

use super::agent::{AgentHealth, ValidationAgent};

/// Agent registry
pub struct AgentRegistry {
    /// Registered agents (agent_id → agent)
    agents: Arc<RwLock<std::collections::HashMap<String, ValidationAgent>>>,
}

impl AgentRegistry {
    /// Create a new agent registry
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            agents: Arc::new(RwLock::new(std::collections::HashMap::new())),
        })
    }

    /// Register a new agent
    pub async fn register(&self, agent: ValidationAgent) -> Result<(), Box<dyn std::error::Error>> {
        let mut agents = self.agents.write().await;

        tracing::info!(
            agent_id = %agent.id,
            name = %agent.name,
            "Registering agent"
        );

        agents.insert(agent.id.clone(), agent);
        Ok(())
    }

    /// Get an agent by ID
    pub async fn get(&self, agent_id: &str) -> Result<ValidationAgent, Box<dyn std::error::Error>> {
        let agents = self.agents.read().await;

        agents
            .get(agent_id)
            .cloned()
            .ok_or_else(|| format!("Agent not found: {}", agent_id).into())
    }

    /// List all registered agents
    pub async fn list(&self) -> Result<Vec<ValidationAgent>, Box<dyn std::error::Error>> {
        let agents = self.agents.read().await;

        let mut agent_list: Vec<ValidationAgent> = agents.values().cloned().collect();
        agent_list.sort_by(|a, b| a.id.cmp(&b.id));

        Ok(agent_list)
    }

    /// Update agent health status
    pub async fn update_health(
        &self,
        agent_id: &str,
        health: AgentHealth,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut agents = self.agents.write().await;

        if let Some(agent) = agents.get_mut(agent_id) {
            agent.health = health;
            agent.last_heartbeat = Some(chrono::Utc::now());

            tracing::debug!(
                agent_id = %agent_id,
                health = ?health,
                "Updated agent health"
            );

            Ok(())
        } else {
            Err(format!("Agent not found: {}", agent_id).into())
        }
    }

    /// Deregister an agent
    pub async fn deregister(&self, agent_id: &str) -> Result<(), Box<dyn std::error::Error>> {
        let mut agents = self.agents.write().await;

        tracing::info!(
            agent_id = %agent_id,
            "Deregistering agent"
        );

        agents
            .remove(agent_id)
            .map(|_| ())
            .ok_or_else(|| format!("Agent not found: {}", agent_id).into())
    }

    /// Get count of registered agents
    pub async fn count(&self) -> usize {
        self.agents.read().await.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_registry_registration() {
        let registry = AgentRegistry::new().await.unwrap();

        let agent = ValidationAgent::new(
            "agent-test",
            "Test Agent",
            "ggen-core",
            vec!["testing".into()],
        );

        registry.register(agent.clone()).await.unwrap();

        // Check agent exists
        let retrieved = registry.get("agent-test").await.unwrap();
        assert_eq!(retrieved.id, "agent-test");
        assert_eq!(retrieved.name, "Test Agent");

        // Check count
        assert_eq!(registry.count().await, 1);
    }

    #[tokio::test]
    async fn test_registry_list() {
        let registry = AgentRegistry::new().await.unwrap();

        registry.register(
            ValidationAgent::new("agent-1", "Agent 1", "ggen-core", vec![])
        ).await.unwrap();
        registry.register(
            ValidationAgent::new("agent-2", "Agent 2", "ggen-core", vec![])
        ).await.unwrap();

        let agents = registry.list().await.unwrap();
        assert_eq!(agents.len(), 2);
        assert_eq!(agents[0].id, "agent-1"); // Sorted by ID
        assert_eq!(agents[1].id, "agent-2");
    }

    #[tokio::test]
    async fn test_registry_update_health() {
        let registry = AgentRegistry::new().await.unwrap();

        registry.register(
            ValidationAgent::new("agent-1", "Agent 1", "ggen-core", vec![])
        ).await.unwrap();

        // Update health
        registry.update_health("agent-1", AgentHealth::Healthy).await.unwrap();

        let agent = registry.get("agent-1").await.unwrap();
        assert_eq!(agent.health, AgentHealth::Healthy);
        assert!(agent.last_heartbeat.is_some());
    }

    #[tokio::test]
    async fn test_registry_deregister() {
        let registry = AgentRegistry::new().await.unwrap();

        registry.register(
            ValidationAgent::new("agent-1", "Agent 1", "ggen-core", vec![])
        ).await.unwrap();

        assert_eq!(registry.count().await, 1);

        registry.deregister("agent-1").await.unwrap();

        assert_eq!(registry.count().await, 0);

        // Getting deregistered agent should fail
        assert!(registry.get("agent-1").await.is_err());
    }
}
