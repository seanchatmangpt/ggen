use crate::agents::{Agent, AgentState};
use crate::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CoordinatorState {
    Idle,
    Coordinating,
    WaitingForConsensus,
    Executing,
    Complete,
    Error,
}

#[derive(Debug, Clone)]
pub struct Coordinator {
    id: Uuid,
    state: CoordinatorState,
    agents: HashMap<Uuid, Agent>,
    active_tasks: Vec<String>,
}

impl Coordinator {
    pub fn new() -> Self {
        Self {
            id: Uuid::new_v4(),
            state: CoordinatorState::Idle,
            agents: HashMap::new(),
            active_tasks: Vec::new(),
        }
    }

    pub fn register_agent(&mut self, agent: Agent) -> Result<()> {
        if self.agents.contains_key(&agent.id) {
            return Err(crate::error::AgentError::ExecutionFailed(
                "Agent already registered".to_string(),
            ));
        }
        self.agents.insert(agent.id, agent);
        Ok(())
    }

    pub fn unregister_agent(&mut self, agent_id: &Uuid) -> Result<()> {
        if self.agents.remove(agent_id).is_some() {
            Ok(())
        } else {
            Err(crate::error::AgentError::ExecutionFailed(
                "Agent not found".to_string(),
            ))
        }
    }

    pub fn get_agent(&self, agent_id: &Uuid) -> Option<&Agent> {
        self.agents.get(agent_id)
    }

    pub fn get_agent_mut(&mut self, agent_id: &Uuid) -> Option<&mut Agent> {
        self.agents.get_mut(agent_id)
    }

    pub fn add_task(&mut self, task: String) -> Result<()> {
        self.active_tasks.push(task);
        Ok(())
    }

    pub fn complete_task(&mut self, task: &str) -> Result<()> {
        if let Some(pos) = self.active_tasks.iter().position(|t| t == task) {
            self.active_tasks.remove(pos);
            Ok(())
        } else {
            Err(crate::error::AgentError::ExecutionFailed(
                "Task not found".to_string(),
            ))
        }
    }

    pub fn get_active_tasks(&self) -> &[String] {
        &self.active_tasks
    }

    pub fn transition_to(&mut self, new_state: CoordinatorState) -> Result<()> {
        match (self.state, new_state) {
            (CoordinatorState::Idle, CoordinatorState::Coordinating) => {
                self.state = new_state;
                Ok(())
            }
            (CoordinatorState::Coordinating, CoordinatorState::WaitingForConsensus) => {
                self.state = new_state;
                Ok(())
            }
            (CoordinatorState::WaitingForConsensus, CoordinatorState::Executing) => {
                self.state = new_state;
                Ok(())
            }
            (CoordinatorState::Executing, CoordinatorState::Complete) => {
                self.state = new_state;
                Ok(())
            }
            (_, CoordinatorState::Error) => {
                self.state = new_state;
                Ok(())
            }
            (CoordinatorState::Error, CoordinatorState::Idle) => {
                self.state = new_state;
                Ok(())
            }
            _ => Err(crate::error::AgentError::InvalidStateTransition(
                format!("Cannot transition from {:?} to {:?}", self.state, new_state),
            )),
        }
    }

    pub fn get_state(&self) -> CoordinatorState {
        self.state
    }

    pub fn get_agents(&self) -> &HashMap<Uuid, Agent> {
        &self.agents
    }

    pub fn healthy_agents(&self) -> Vec<&Agent> {
        self.agents
            .values()
            .filter(|a| a.state != AgentState::Failed && a.error_rate() < 0.5)
            .collect()
    }

    pub fn failed_agents(&self) -> Vec<&Agent> {
        self.agents
            .values()
            .filter(|a| a.state == AgentState::Failed)
            .collect()
    }
}

impl Default for Coordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_coordinator_creation() {
        let coordinator = Coordinator::new();
        assert_eq!(coordinator.state, CoordinatorState::Idle);
        assert!(coordinator.agents.is_empty());
    }

    #[test]
    fn test_register_agent() {
        let mut coordinator = Coordinator::new();
        let agent = Agent::new("test-agent");

        assert!(coordinator.register_agent(agent).is_ok());
        assert_eq!(coordinator.agents.len(), 1);
    }

    #[test]
    fn test_register_duplicate_agent() {
        let mut coordinator = Coordinator::new();
        let agent = Agent::new("test-agent");

        coordinator.register_agent(agent.clone()).unwrap();
        let result = coordinator.register_agent(agent);
        assert!(result.is_err());
    }

    #[test]
    fn test_unregister_agent() {
        let mut coordinator = Coordinator::new();
        let agent = Agent::new("test-agent");
        let agent_id = agent.id;

        coordinator.register_agent(agent).unwrap();
        assert!(coordinator.unregister_agent(&agent_id).is_ok());
        assert_eq!(coordinator.agents.len(), 0);
    }

    #[test]
    fn test_get_agent() {
        let mut coordinator = Coordinator::new();
        let agent = Agent::new("test-agent");
        let agent_id = agent.id;

        coordinator.register_agent(agent).unwrap();
        let retrieved = coordinator.get_agent(&agent_id);
        assert!(retrieved.is_some());
    }

    #[test]
    fn test_add_task() {
        let mut coordinator = Coordinator::new();
        assert!(coordinator.add_task("task1".to_string()).is_ok());
        assert_eq!(coordinator.get_active_tasks().len(), 1);
    }

    #[test]
    fn test_complete_task() {
        let mut coordinator = Coordinator::new();
        coordinator.add_task("task1".to_string()).unwrap();
        assert!(coordinator.complete_task("task1").is_ok());
        assert_eq!(coordinator.get_active_tasks().len(), 0);
    }

    #[test]
    fn test_coordinator_state_transition() {
        let mut coordinator = Coordinator::new();
        assert!(coordinator.transition_to(CoordinatorState::Coordinating).is_ok());
        assert_eq!(coordinator.state, CoordinatorState::Coordinating);

        assert!(coordinator.transition_to(CoordinatorState::WaitingForConsensus).is_ok());
        assert_eq!(coordinator.state, CoordinatorState::WaitingForConsensus);
    }

    #[test]
    fn test_invalid_coordinator_transition() {
        let mut coordinator = Coordinator::new();
        let result = coordinator.transition_to(CoordinatorState::Executing);
        assert!(result.is_err());
    }

    #[test]
    fn test_healthy_agents() {
        let mut coordinator = Coordinator::new();
        let mut agent1 = Agent::new("agent1");
        let mut agent2 = Agent::new("agent2");

        agent1.record_success();
        agent2.record_failure();
        agent2.record_failure();

        coordinator.register_agent(agent1).unwrap();
        coordinator.register_agent(agent2).unwrap();

        let healthy = coordinator.healthy_agents();
        assert_eq!(healthy.len(), 1);
    }

    #[test]
    fn test_multiple_tasks() {
        let mut coordinator = Coordinator::new();
        coordinator.add_task("task1".to_string()).unwrap();
        coordinator.add_task("task2".to_string()).unwrap();
        coordinator.add_task("task3".to_string()).unwrap();

        assert_eq!(coordinator.get_active_tasks().len(), 3);

        coordinator.complete_task("task2").unwrap();
        assert_eq!(coordinator.get_active_tasks().len(), 2);
    }
}
