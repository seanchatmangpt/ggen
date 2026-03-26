use crate::error::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use std::sync::Arc;
use tokio::sync::RwLock;
use async_trait::async_trait;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentState {
    Idle,
    Running,
    Paused,
    Failed,
    Recovered,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMessage {
    pub id: String,
    pub agent_id: Uuid,
    pub content: String,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Agent {
    pub id: Uuid,
    pub name: String,
    pub state: AgentState,
    pub error_count: usize,
    pub success_count: usize,
}

impl Agent {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            state: AgentState::Idle,
            error_count: 0,
            success_count: 0,
        }
    }

    pub fn transition_to(&mut self, new_state: AgentState) -> Result<()> {
        match (self.state, new_state) {
            (AgentState::Idle, AgentState::Running) => {
                self.state = new_state;
                Ok(())
            }
            (AgentState::Running, AgentState::Paused) => {
                self.state = new_state;
                Ok(())
            }
            (AgentState::Running, AgentState::Failed) => {
                self.state = new_state;
                self.error_count += 1;
                Ok(())
            }
            (AgentState::Failed, AgentState::Recovered) => {
                self.state = new_state;
                Ok(())
            }
            (AgentState::Recovered, AgentState::Running) => {
                self.state = new_state;
                Ok(())
            }
            (AgentState::Paused, AgentState::Running) => {
                self.state = new_state;
                Ok(())
            }
            _ => Err(crate::error::AgentError::InvalidStateTransition(
                format!(
                    "Cannot transition from {:?} to {:?}",
                    self.state, new_state
                ),
            )),
        }
    }

    pub fn record_success(&mut self) {
        self.success_count += 1;
    }

    pub fn record_failure(&mut self) {
        self.error_count += 1;
    }

    pub fn error_rate(&self) -> f64 {
        let total = self.error_count + self.success_count;
        if total == 0 {
            0.0
        } else {
            self.error_count as f64 / total as f64
        }
    }
}

#[async_trait]
pub trait AgentExecutor: Send + Sync {
    async fn execute(&self) -> Result<String>;
}

#[derive(Debug, Clone)]
pub struct CoordinatorAgent {
    pub agent: Agent,
}

impl CoordinatorAgent {
    pub fn new() -> Self {
        Self {
            agent: Agent::new("Coordinator"),
        }
    }
}

impl Default for CoordinatorAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl AgentExecutor for CoordinatorAgent {
    async fn execute(&self) -> Result<String> {
        Ok("Coordinator executed".to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ExecutorAgent {
    pub agent: Agent,
}

impl ExecutorAgent {
    pub fn new() -> Self {
        Self {
            agent: Agent::new("Executor"),
        }
    }
}

impl Default for ExecutorAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl AgentExecutor for ExecutorAgent {
    async fn execute(&self) -> Result<String> {
        Ok("Executor completed task".to_string())
    }
}

#[derive(Debug, Clone)]
pub struct ValidatorAgent {
    pub agent: Agent,
}

impl ValidatorAgent {
    pub fn new() -> Self {
        Self {
            agent: Agent::new("Validator"),
        }
    }

    pub fn validate(&mut self, result: &str) -> Result<bool> {
        self.agent.transition_to(AgentState::Running)?;
        let is_valid = !result.is_empty();
        self.agent.transition_to(AgentState::Idle)?;
        Ok(is_valid)
    }
}

impl Default for ValidatorAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl AgentExecutor for ValidatorAgent {
    async fn execute(&self) -> Result<String> {
        Ok("Validation passed".to_string())
    }
}

#[derive(Debug, Clone)]
pub struct MonitorAgent {
    pub agent: Agent,
    pub monitored_metrics: Arc<RwLock<Vec<String>>>,
}

impl MonitorAgent {
    pub fn new() -> Self {
        Self {
            agent: Agent::new("Monitor"),
            monitored_metrics: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub async fn record_metric(&self, metric: String) {
        let mut metrics = self.monitored_metrics.write().await;
        metrics.push(metric);
    }

    pub async fn get_metrics(&self) -> Vec<String> {
        self.monitored_metrics.read().await.clone()
    }
}

impl Default for MonitorAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl AgentExecutor for MonitorAgent {
    async fn execute(&self) -> Result<String> {
        let metrics = self.get_metrics().await;
        Ok(format!("Monitored {} metrics", metrics.len()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_creation() {
        let agent = Agent::new("test-agent");
        assert_eq!(agent.name, "test-agent");
        assert_eq!(agent.state, AgentState::Idle);
        assert_eq!(agent.error_count, 0);
        assert_eq!(agent.success_count, 0);
    }

    #[test]
    fn test_agent_state_transitions() {
        let mut agent = Agent::new("test");
        assert!(agent.transition_to(AgentState::Running).is_ok());
        assert_eq!(agent.state, AgentState::Running);

        assert!(agent.transition_to(AgentState::Paused).is_ok());
        assert_eq!(agent.state, AgentState::Paused);

        assert!(agent.transition_to(AgentState::Running).is_ok());
        assert_eq!(agent.state, AgentState::Running);

        assert!(agent.transition_to(AgentState::Failed).is_ok());
        assert_eq!(agent.state, AgentState::Failed);
    }

    #[test]
    fn test_invalid_state_transition() {
        let mut agent = Agent::new("test");
        assert!(agent.transition_to(AgentState::Failed).is_err());
    }

    #[test]
    fn test_agent_success_tracking() {
        let mut agent = Agent::new("test");
        agent.record_success();
        agent.record_success();
        assert_eq!(agent.success_count, 2);
    }

    #[test]
    fn test_error_rate_calculation() {
        let mut agent = Agent::new("test");
        agent.record_success();
        agent.record_success();
        agent.record_failure();
        let rate = agent.error_rate();
        assert!((rate - 1.0 / 3.0).abs() < 0.01);
    }

    #[test]
    fn test_coordinator_agent_creation() {
        let agent = CoordinatorAgent::new();
        assert_eq!(agent.agent.name, "Coordinator");
    }

    #[test]
    fn test_executor_agent_creation() {
        let agent = ExecutorAgent::new();
        assert_eq!(agent.agent.name, "Executor");
    }

    #[test]
    fn test_validator_agent_creation() {
        let agent = ValidatorAgent::new();
        assert_eq!(agent.agent.name, "Validator");
    }

    #[test]
    fn test_monitor_agent_creation() {
        let agent = MonitorAgent::new();
        assert_eq!(agent.agent.name, "Monitor");
    }

    #[tokio::test]
    async fn test_coordinator_executor() {
        let agent = CoordinatorAgent::new();
        let result = agent.execute().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_executor_agent_executor() {
        let agent = ExecutorAgent::new();
        let result = agent.execute().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_validator_agent_executor() {
        let agent = ValidatorAgent::new();
        let result = agent.execute().await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_monitor_agent_metrics() {
        let agent = MonitorAgent::new();
        agent.record_metric("metric1".to_string()).await;
        agent.record_metric("metric2".to_string()).await;

        let metrics = agent.get_metrics().await;
        assert_eq!(metrics.len(), 2);
    }

    #[test]
    fn test_validator_validate() {
        let mut agent = ValidatorAgent::new();
        // Move to running state first
        let _ = agent.agent.transition_to(AgentState::Idle);
        let result = agent.validate("valid_result");
        assert!(result.is_ok());
        assert!(result.unwrap());
    }
}
