//! Agent state machine implementation with lifecycle management
//!
//! State transitions:
//! Initializing → Ready → Processing → Idle → Processing (cycle)
//!             → Error → Ready (recovery)
//!           → Terminated (final)

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentState {
    Initializing,
    Ready,
    Processing,
    Idle,
    Error,
    Terminated,
}

impl AgentState {
    pub fn code(&self) -> &'static str {
        match self {
            AgentState::Initializing => "INITIALIZING",
            AgentState::Ready => "READY",
            AgentState::Processing => "PROCESSING",
            AgentState::Idle => "IDLE",
            AgentState::Error => "ERROR",
            AgentState::Terminated => "TERMINATED",
        }
    }

    pub fn is_recoverable(&self) -> bool {
        matches!(self, AgentState::Error)
    }

    pub fn is_terminal(&self) -> bool {
        matches!(self, AgentState::Terminated)
    }
}

#[derive(Debug, Clone)]
pub struct StateTransition {
    pub from: AgentState,
    pub to: AgentState,
    pub trigger: String,
    pub guard: Option<String>,
    pub timeout_ms: Option<u64>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug)]
pub struct Agent {
    pub id: String,
    pub name: String,
    state: AgentState,
    state_history: Vec<StateTransition>,
    message_queue: VecDeque<String>,
    created_at: DateTime<Utc>,
    updated_at: DateTime<Utc>,
    error_count: u32,
    max_retries: u32,
}

impl Agent {
    pub fn new(name: impl Into<String>) -> Self {
        let id = Uuid::new_v4().to_string();
        let name = name.into();
        let now = Utc::now();

        Agent {
            id,
            name,
            state: AgentState::Initializing,
            state_history: Vec::new(),
            message_queue: VecDeque::new(),
            created_at: now,
            updated_at: now,
            error_count: 0,
            max_retries: 3,
        }
    }

    pub fn state(&self) -> AgentState {
        self.state
    }

    pub fn state_code(&self) -> &'static str {
        self.state.code()
    }

    pub fn uptime_ms(&self) -> u128 {
        (Utc::now() - self.created_at).num_milliseconds() as u128
    }

    pub fn state_history(&self) -> &[StateTransition] {
        &self.state_history
    }

    pub fn error_count(&self) -> u32 {
        self.error_count
    }

    pub fn message_queue_len(&self) -> usize {
        self.message_queue.len()
    }

    /// Transition to a new state with guard validation
    pub fn transition(&mut self, to: AgentState, trigger: &str) -> Result<()> {
        let from = self.state;

        // Validate transition is allowed
        self.validate_transition(from, to)?;

        // Apply transition
        self.state = to;
        self.updated_at = Utc::now();

        let transition = StateTransition {
            from,
            to,
            trigger: trigger.to_string(),
            guard: None,
            timeout_ms: None,
            timestamp: self.updated_at,
        };

        self.state_history.push(transition);

        Ok(())
    }

    /// Validate state transition according to state machine rules
    fn validate_transition(&self, from: AgentState, to: AgentState) -> Result<()> {
        let valid = match (from, to) {
            (AgentState::Initializing, AgentState::Ready) => true,
            (AgentState::Ready, AgentState::Processing) => true,
            (AgentState::Ready, AgentState::Error) => true,
            (AgentState::Processing, AgentState::Idle) => true,
            (AgentState::Processing, AgentState::Error) => true,
            (AgentState::Idle, AgentState::Processing) => true,
            (AgentState::Error, AgentState::Ready) => self.error_count < self.max_retries,
            (_, AgentState::Terminated) => true,
            _ => false,
        };

        if !valid {
            return Err(anyhow::anyhow!(
                "Invalid transition: {} -> {}",
                from.code(),
                to.code()
            ));
        }

        Ok(())
    }

    /// Mark agent as ready (transition from Initializing)
    pub fn mark_ready(&mut self) -> Result<()> {
        self.transition(AgentState::Ready, "initialization_complete")
    }

    /// Mark agent as processing a task (transition from Ready/Idle)
    pub fn mark_processing(&mut self) -> Result<()> {
        self.transition(AgentState::Processing, "task_accepted")
    }

    /// Mark agent as idle after task completion
    pub fn mark_idle(&mut self) -> Result<()> {
        self.transition(AgentState::Idle, "task_completed")
    }

    /// Record an error and attempt recovery
    pub fn mark_error(&mut self) -> Result<()> {
        self.error_count += 1;
        self.transition(AgentState::Error, "task_failed")
    }

    /// Recover from error (transition to Ready)
    pub fn recover(&mut self) -> Result<()> {
        if self.error_count >= self.max_retries {
            return Err(anyhow::anyhow!(
                "Max retries exceeded: {} attempts",
                self.error_count
            ));
        }

        self.error_count = 0;
        self.transition(AgentState::Ready, "recovery_successful")
    }

    /// Terminate agent
    pub fn terminate(&mut self) -> Result<()> {
        self.transition(AgentState::Terminated, "shutdown_requested")
    }

    /// Queue a message for processing
    pub fn enqueue_message(&mut self, msg: String) {
        self.message_queue.push_back(msg);
    }

    /// Dequeue next message (FIFO)
    pub fn dequeue_message(&mut self) -> Option<String> {
        self.message_queue.pop_front()
    }

    /// Peek at next message without removing
    pub fn peek_message(&self) -> Option<&String> {
        self.message_queue.front()
    }

    /// Get current state information
    pub fn state_info(&self) -> StateInfo {
        StateInfo {
            id: self.id.clone(),
            name: self.name.clone(),
            state: self.state_code().to_string(),
            created_at: self.created_at,
            uptime_ms: self.uptime_ms(),
            message_queue_len: self.message_queue_len(),
            error_count: self.error_count,
            state_transitions: self.state_history.len(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateInfo {
    pub id: String,
    pub name: String,
    pub state: String,
    pub created_at: DateTime<Utc>,
    pub uptime_ms: u128,
    pub message_queue_len: usize,
    pub error_count: u32,
    pub state_transitions: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_creation() {
        let agent = Agent::new("TestAgent");
        assert_eq!(agent.state, AgentState::Initializing);
        assert_eq!(agent.error_count, 0);
        assert_eq!(agent.message_queue_len(), 0);
    }

    #[test]
    fn test_valid_transition_initializing_to_ready() {
        let mut agent = Agent::new("TestAgent");
        assert!(agent.mark_ready().is_ok());
        assert_eq!(agent.state, AgentState::Ready);
    }

    #[test]
    fn test_valid_transition_ready_to_processing() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();
        assert!(agent.mark_processing().is_ok());
        assert_eq!(agent.state, AgentState::Processing);
    }

    #[test]
    fn test_valid_transition_processing_to_idle() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();
        agent.mark_processing().unwrap();
        assert!(agent.mark_idle().is_ok());
        assert_eq!(agent.state, AgentState::Idle);
    }

    #[test]
    fn test_invalid_transition_direct_to_processing() {
        let mut agent = Agent::new("TestAgent");
        // Try to go to processing from Initializing (invalid)
        assert!(agent.mark_processing().is_err());
    }

    #[test]
    fn test_error_transition() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();
        agent.mark_processing().unwrap();
        assert!(agent.mark_error().is_ok());
        assert_eq!(agent.state, AgentState::Error);
        assert_eq!(agent.error_count, 1);
    }

    #[test]
    fn test_error_recovery() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();
        agent.mark_processing().unwrap();
        agent.mark_error().unwrap();
        assert!(agent.recover().is_ok());
        assert_eq!(agent.state, AgentState::Ready);
        assert_eq!(agent.error_count, 0);
    }

    #[test]
    fn test_max_retries_exceeded() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();

        // Simulate max retries
        for _ in 0..agent.max_retries {
            agent.mark_processing().unwrap();
            agent.mark_error().unwrap();
            if agent.error_count < agent.max_retries {
                agent.recover().unwrap();
            }
        }

        assert_eq!(agent.error_count, agent.max_retries);
        assert!(agent.recover().is_err());
    }

    #[test]
    fn test_message_queue() {
        let mut agent = Agent::new("TestAgent");
        agent.enqueue_message("msg1".to_string());
        agent.enqueue_message("msg2".to_string());

        assert_eq!(agent.message_queue_len(), 2);
        assert_eq!(agent.dequeue_message(), Some("msg1".to_string()));
        assert_eq!(agent.message_queue_len(), 1);
        assert_eq!(agent.dequeue_message(), Some("msg2".to_string()));
        assert_eq!(agent.dequeue_message(), None);
    }

    #[test]
    fn test_state_history() {
        let mut agent = Agent::new("TestAgent");
        agent.mark_ready().unwrap();
        agent.mark_processing().unwrap();
        agent.mark_idle().unwrap();

        assert_eq!(agent.state_history.len(), 3);
        assert_eq!(agent.state_history[0].from, AgentState::Initializing);
        assert_eq!(agent.state_history[0].to, AgentState::Ready);
    }

    #[test]
    fn test_terminate_from_any_state() {
        let mut agent = Agent::new("TestAgent");
        assert!(agent.terminate().is_ok());
        assert_eq!(agent.state, AgentState::Terminated);
    }
}
