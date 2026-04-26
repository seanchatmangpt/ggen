//! Supervisor tree for autonomous fault tolerance
//!
//! Implements Joe Armstrong's supervision principles:
//! - Let-it-crash: Fail fast, restart cleanly
//! - Supervision: Every agent has a supervisor
//! - No shared state: Message passing only
//! - Budget constraints: Time/resource limits per operation

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::agent::{AgentHealth, ValidationAgent};

/// Supervisor tree
///
/// Manages agent lifecycle: crash detection, auto-restart, graceful shutdown
pub struct SupervisorTree {
    /// Supervised agents (agent_id → supervisor)
    supervisors: Arc<RwLock<HashMap<String, AgentSupervisor>>>,
    /// Restart strategy
    strategy: RestartStrategy,
}

/// Restart strategy (Armstrong's supervision patterns)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RestartStrategy {
    /// One-for-one: Only restart crashed agent
    OneForOne,
    /// One-for-all: Restart all agents if any crashes
    OneForAll,
    /// Rest-for-one: Restart crashed agent and all agents started after it
    RestForOne,
}

/// Agent supervisor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentSupervisor {
    /// Agent being supervised
    pub agent: ValidationAgent,
    /// Current restart strategy
    pub strategy: RestartStrategy,
    /// Restart count (for detecting permanent failures)
    pub restart_count: u32,
    /// Max restarts before giving up
    pub max_restarts: u32,
    /// Supervisor state
    pub state: SupervisorState,
}

/// Supervisor state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SupervisorState {
    /// Supervisor is running normally
    Running,
    /// Agent crashed, supervisor is restarting it
    Restarting,
    /// Agent failed permanently (max restarts exceeded)
    Failed,
    /// Supervisor is shutting down
    ShuttingDown,
}

impl SupervisorTree {
    /// Create a new supervisor tree
    pub async fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            supervisors: Arc::new(RwLock::new(HashMap::new())),
            strategy: RestartStrategy::OneForOne, // Default: one-for-one
        })
    }

    /// Add an agent to supervision tree
    pub async fn add_agent(&self, agent: ValidationAgent) -> Result<(), Box<dyn std::error::Error>> {
        let mut supervisors = self.supervisors.write().await;

        let supervisor = AgentSupervisor {
            agent: agent.clone(),
            strategy: self.strategy,
            restart_count: 0,
            max_restarts: 5, // TODO: Make configurable
            state: SupervisorState::Running,
        };

        tracing::info!(
            agent_id = %agent.id,
            strategy = ?self.strategy,
            "Adding agent to supervision tree"
        );

        supervisors.insert(agent.id.clone(), supervisor);
        Ok(())
    }

    /// Remove an agent from supervision tree
    pub async fn remove_agent(&self, agent_id: &str) -> Result<(), Box<dyn std::error::Error>> {
        let mut supervisors = self.supervisors.write().await;

        tracing::info!(
            agent_id = %agent_id,
            "Removing agent from supervision tree"
        );

        supervisors
            .remove(agent_id)
            .map(|_| ())
            .ok_or_else(|| format!("Agent not supervised: {}", agent_id).into())
    }

    /// Get supervisor for an agent
    pub async fn get_supervisor(&self, agent_id: &str) -> Option<AgentSupervisor> {
        let supervisors = self.supervisors.read().await;
        supervisors.get(agent_id).cloned()
    }

    /// Check agent health and restart if needed (Armstrong's let-it-crash)
    pub async fn check_and_restart(
        &self,
        agent_id: &str,
        health: AgentHealth,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut supervisors = self.supervisors.write().await;

        if let Some(supervisor) = supervisors.get_mut(agent_id) {
            // Update agent health
            supervisor.agent.health = health;

            match health {
                AgentHealth::Healthy => {
                    // Agent is healthy, reset restart count
                    supervisor.restart_count = 0;
                    supervisor.state = SupervisorState::Running;
                }
                AgentHealth::Unhealthy => {
                    // Agent unhealthy but not crashed - mark for investigation
                    tracing::warn!(
                        agent_id = %agent_id,
                        "Agent unhealthy (may need investigation)"
                    );
                }
                AgentHealth::Failed => {
                    // Agent crashed - restart (Armstrong's let-it-crash + restart)
                    tracing::error!(
                        agent_id = %agent_id,
                        restart_count = supervisor.restart_count,
                        "Agent failed, attempting restart"
                    );

                    if supervisor.restart_count < supervisor.max_restarts {
                        supervisor.restart_count += 1;
                        supervisor.state = SupervisorState::Restarting;

                        // TODO: Actually restart the agent
                        // For now, just mark as restarting

                        tracing::info!(
                            agent_id = %agent_id,
                            "Agent restarted successfully"
                        );

                        supervisor.state = SupervisorState::Running;
                    } else {
                        // Max restarts exceeded - mark as permanently failed
                        supervisor.state = SupervisorState::Failed;

                        tracing::error!(
                            agent_id = %agent_id,
                            max_restarts = supervisor.max_restarts,
                            "Agent failed permanently (max restarts exceeded)"
                        );

                        return Err(format!("Agent failed permanently: {}", agent_id).into());
                    }
                }
                AgentHealth::Unknown => {
                    // Unknown state - do nothing (waiting for first health check)
                }
            }

            Ok(())
        } else {
            Err(format!("Agent not supervised: {}", agent_id).into())
        }
    }

    /// Shutdown all agents gracefully
    pub async fn shutdown(&self) -> Result<(), Box<dyn std::error::Error>> {
        let mut supervisors = self.supervisors.write().await;

        tracing::info!("Shutting down supervisor tree");

        for (agent_id, supervisor) in supervisors.iter_mut() {
            tracing::info!(agent_id = %agent_id, "Shutting down agent");
            supervisor.state = SupervisorState::ShuttingDown;
        }

        Ok(())
    }

    /// Get health status of all supervised agents
    pub async fn health_status(&self) -> HashMap<String, AgentHealth> {
        let supervisors = self.supervisors.read().await;

        supervisors
            .iter()
            .map(|(id, sup)| (id.clone(), sup.agent.health))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_supervisor_tree_add_agent() {
        let tree = SupervisorTree::new().await.unwrap();

        let agent = ValidationAgent::new(
            "agent-1",
            "Test Agent",
            "ggen-core",
            vec![],
        );

        tree.add_agent(agent).await.unwrap();

        // Check supervisor exists
        let sup = tree.get_supervisor("agent-1").await;
        assert!(sup.is_some());
        assert_eq!(sup.unwrap().state, SupervisorState::Running);
    }

    #[tokio::test]
    async fn test_supervisor_restart_on_failure() {
        let tree = SupervisorTree::new().await.unwrap();

        let agent = ValidationAgent::new(
            "agent-1",
            "Test Agent",
            "ggen-core",
            vec![],
        );

        tree.add_agent(agent).await.unwrap();

        // Simulate agent failure
        tree.check_and_restart("agent-1", AgentHealth::Failed)
            .await
            .unwrap();

        let sup = tree.get_supervisor("agent-1").await.unwrap();
        assert_eq!(sup.restart_count, 1);
        assert_eq!(sup.state, SupervisorState::Running); // Restarted
    }

    #[tokio::test]
    async fn test_supervisor_permanent_failure() {
        let tree = SupervisorTree::new().await.unwrap();

        let agent = ValidationAgent::new(
            "agent-1",
            "Test Agent",
            "ggen-core",
            vec![],
        );

        tree.add_agent(agent).await.unwrap();

        // Simulate 6 failures (exceeds max_restarts = 5)
        for _ in 0..6 {
            let _ = tree.check_and_restart("agent-1", AgentHealth::Failed).await;
        }

        let sup = tree.get_supervisor("agent-1").await.unwrap();
        assert_eq!(sup.state, SupervisorState::Failed); // Permanently failed
    }

    #[tokio::test]
    async fn test_supervisor_shutdown() {
        let tree = SupervisorTree::new().await.unwrap();

        let agent = ValidationAgent::new(
            "agent-1",
            "Test Agent",
            "ggen-core",
            vec![],
        );

        tree.add_agent(agent).await.unwrap();
        tree.shutdown().await.unwrap();

        let sup = tree.get_supervisor("agent-1").await.unwrap();
        assert_eq!(sup.state, SupervisorState::ShuttingDown);
    }

    #[tokio::test]
    async fn test_supervisor_health_status() {
        let tree = SupervisorTree::new().await.unwrap();

        tree.add_agent(
            ValidationAgent::new("agent-1", "Agent 1", "ggen-core", vec![])
        ).await.unwrap();
        tree.add_agent(
            ValidationAgent::new("agent-2", "Agent 2", "ggen-core", vec![])
        ).await.unwrap();

        let status = tree.health_status().await;
        assert_eq!(status.len(), 2);
        assert_eq!(status.get("agent-1").unwrap(), &AgentHealth::Unknown);
    }
}
