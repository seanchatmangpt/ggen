//! Agent lifecycle and A2A protocol integration
//!
//! Manages agent creation, initialization, and state transitions.

use crate::error::Result;
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Agent lifecycle states
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AgentState {
    Created,
    Initializing,
    Ready,
    Working,
    Recovering,
    Complete,
    Failed,
}

impl std::fmt::Display for AgentState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Created => write!(f, "Created"),
            Self::Initializing => write!(f, "Initializing"),
            Self::Ready => write!(f, "Ready"),
            Self::Working => write!(f, "Working"),
            Self::Recovering => write!(f, "Recovering"),
            Self::Complete => write!(f, "Complete"),
            Self::Failed => write!(f, "Failed"),
        }
    }
}

/// Agent information and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Agent {
    pub id: Uuid,
    pub name: String,
    pub state: AgentState,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub lifecycle_stage: LifecycleStage,
    pub capabilities: Vec<String>,
    pub error_count: u32,
    pub success_count: u32,
}

/// Lifecycle stages for agents
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LifecycleStage {
    Birth,
    Growth,
    Maturity,
    Mastery,
    Teaching,
    Legacy,
}

impl std::fmt::Display for LifecycleStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Birth => write!(f, "Birth"),
            Self::Growth => write!(f, "Growth"),
            Self::Maturity => write!(f, "Maturity"),
            Self::Mastery => write!(f, "Mastery"),
            Self::Teaching => write!(f, "Teaching"),
            Self::Legacy => write!(f, "Legacy"),
        }
    }
}

#[async_trait]
pub trait AgentLifecycle: Send + Sync {
    async fn initialize(&mut self) -> Result<()>;
    async fn execute_task(&mut self, task_id: Uuid) -> Result<()>;
    async fn handle_error(&mut self, error: String) -> Result<()>;
    async fn recover(&mut self) -> Result<()>;
    async fn complete(&mut self) -> Result<()>;
}

/// Agent manager for coordinating multiple agents
pub struct AgentManager {
    agents: HashMap<Uuid, Agent>,
    max_error_count: u32,
}

impl AgentManager {
    /// Create a new agent manager
    pub fn new() -> Self {
        Self {
            agents: HashMap::new(),
            max_error_count: 5,
        }
    }

    /// Create a new agent
    pub fn create_agent(&mut self, name: String, capabilities: Vec<String>) -> Agent {
        let agent = Agent {
            id: Uuid::new_v4(),
            name,
            state: AgentState::Created,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            lifecycle_stage: LifecycleStage::Birth,
            capabilities,
            error_count: 0,
            success_count: 0,
        };

        self.agents.insert(agent.id, agent.clone());
        agent
    }

    /// Initialize an agent
    pub fn initialize_agent(&mut self, agent_id: Uuid) -> Result<()> {
        let agent = self.agents.get_mut(&agent_id)
            .ok_or_else(|| crate::WorkflowError::AgentError("Agent not found".to_string()))?;

        agent.state = AgentState::Initializing;
        agent.updated_at = Utc::now();

        // Simulate initialization time
        std::thread::sleep(std::time::Duration::from_millis(10));

        agent.state = AgentState::Ready;
        agent.lifecycle_stage = LifecycleStage::Growth;
        agent.updated_at = Utc::now();

        Ok(())
    }

    /// Get agent by ID
    pub fn get_agent(&self, agent_id: Uuid) -> Result<&Agent> {
        self.agents.get(&agent_id)
            .ok_or_else(|| crate::WorkflowError::AgentError("Agent not found".to_string()))
    }

    /// Get mutable agent by ID
    pub fn get_agent_mut(&mut self, agent_id: Uuid) -> Result<&mut Agent> {
        self.agents.get_mut(&agent_id)
            .ok_or_else(|| crate::WorkflowError::AgentError("Agent not found".to_string()))
    }

    /// Record success for agent
    pub fn record_success(&mut self, agent_id: Uuid) -> Result<()> {
        let agent = self.get_agent_mut(agent_id)?;
        agent.success_count += 1;
        agent.updated_at = Utc::now();
        Ok(())
    }

    /// Record error for agent
    pub fn record_error(&mut self, agent_id: Uuid) -> Result<()> {
        let agent = self.get_agent_mut(agent_id)?;
        agent.error_count += 1;
        agent.updated_at = Utc::now();

        if agent.error_count >= self.max_error_count {
            agent.state = AgentState::Failed;
        }

        Ok(())
    }

    /// List all agents
    pub fn list_agents(&self) -> Vec<Agent> {
        self.agents.values().cloned().collect()
    }

    /// Get agents in specific state
    pub fn agents_by_state(&self, state: AgentState) -> Vec<Agent> {
        self.agents
            .values()
            .filter(|a| a.state == state)
            .cloned()
            .collect()
    }
}

impl Default for AgentManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_agent() {
        let mut manager = AgentManager::new();
        let agent = manager.create_agent(
            "test_agent".to_string(),
            vec!["capability1".to_string()],
        );

        assert_eq!(agent.state, AgentState::Created);
        assert_eq!(agent.lifecycle_stage, LifecycleStage::Birth);
        assert_eq!(agent.error_count, 0);
        assert_eq!(agent.success_count, 0);
    }

    #[test]
    fn test_initialize_agent() {
        let mut manager = AgentManager::new();
        let agent = manager.create_agent(
            "test_agent".to_string(),
            vec!["capability1".to_string()],
        );

        assert!(manager.initialize_agent(agent.id).is_ok());

        let initialized = manager.get_agent(agent.id).unwrap();
        assert_eq!(initialized.state, AgentState::Ready);
        assert_eq!(initialized.lifecycle_stage, LifecycleStage::Growth);
    }

    #[test]
    fn test_record_success() {
        let mut manager = AgentManager::new();
        let agent = manager.create_agent(
            "test_agent".to_string(),
            vec!["capability1".to_string()],
        );

        assert!(manager.record_success(agent.id).is_ok());

        let updated = manager.get_agent(agent.id).unwrap();
        assert_eq!(updated.success_count, 1);
    }

    #[test]
    fn test_record_error_threshold() {
        let mut manager = AgentManager::new();
        manager.max_error_count = 2;
        let agent = manager.create_agent(
            "test_agent".to_string(),
            vec!["capability1".to_string()],
        );

        manager.record_error(agent.id).unwrap();
        manager.record_error(agent.id).unwrap();

        let failed = manager.get_agent(agent.id).unwrap();
        assert_eq!(failed.state, AgentState::Failed);
    }
}
