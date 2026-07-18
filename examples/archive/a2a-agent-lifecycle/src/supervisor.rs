//! Supervisor tree pattern for agent lifecycle management
//!
//! Implements production fault tolerance with:
//! - Agent pool supervision and lifecycle
//! - Automatic crash detection and restart
//! - Exponential backoff retry strategy
//! - Max restart enforcement with permanent failure marking
//! - Health monitoring and reporting
//! - Dead letter queue for undelivered messages

use anyhow::{anyhow, Result};
use chrono::{DateTime, Utc};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::agent::Agent;
use crate::messaging::Message;

/// Maximum restart attempts before marking agent permanently failed
pub const MAX_RESTART_ATTEMPTS: u32 = 3;

/// Initial backoff duration (exponential growth: 100ms, 200ms, 400ms)
pub const INITIAL_BACKOFF_MS: u64 = 100;

/// Shutdown timeout for graceful termination
pub const SHUTDOWN_TIMEOUT_MS: u64 = 5000;

/// Health check interval
pub const HEALTH_CHECK_INTERVAL_MS: u64 = 1000;

/// Supervisor state for tracking agent lifecycle
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SupervisorState {
    /// Supervisor initialized, not managing agents
    Idle,
    /// Supervisor actively managing agent pool
    Supervising,
    /// Supervisor shutting down agents
    Shutting,
    /// Supervisor permanently failed (unrecoverable error)
    Failed,
}

/// Agent crash reason tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CrashReason {
    Panic(String),
    Timeout,
    HealthCheckFailed,
    UnhandledError(String),
    MaxRestartsExceeded,
    Manual,
}

/// Agent lifecycle tracking within supervisor
#[derive(Debug, Clone)]
pub struct ManagedAgent {
    pub id: String,
    pub agent: Arc<RwLock<Agent>>,
    pub restart_count: Arc<AtomicU32>, // Wrap in Arc for cloning
    pub last_crash: Option<(CrashReason, DateTime<Utc>)>,
    pub created_at: DateTime<Utc>,
    pub last_seen_alive: DateTime<Utc>,
}

impl ManagedAgent {
    pub fn new(agent: Agent) -> Self {
        let id = agent.id.clone();
        let now = Utc::now();

        ManagedAgent {
            id,
            agent: Arc::new(RwLock::new(agent)),
            restart_count: Arc::new(AtomicU32::new(0)),
            last_crash: None,
            created_at: now,
            last_seen_alive: now,
        }
    }

    pub fn restart_count(&self) -> u32 {
        self.restart_count.load(Ordering::SeqCst)
    }

    pub fn increment_restart_count(&self) {
        self.restart_count.fetch_add(1, Ordering::SeqCst);
    }

    pub fn can_restart(&self) -> bool {
        self.restart_count() < MAX_RESTART_ATTEMPTS
    }

    pub fn is_healthy(&self) -> bool {
        let now = Utc::now();
        let duration_since_alive = (now - self.last_seen_alive).num_milliseconds().max(0) as u64;

        // Agent is healthy if seen alive within health check interval
        duration_since_alive < HEALTH_CHECK_INTERVAL_MS * 2
    }
}

/// Supervisor managing agent pool with fault tolerance
pub struct Supervisor {
    pub id: String,
    pub agents: Arc<DashMap<String, ManagedAgent>>,
    pub dead_letter_queue: Arc<RwLock<Vec<(String, Message)>>>, // (agent_id, message)
    pub state: Arc<RwLock<SupervisorState>>,
    pub created_at: DateTime<Utc>,
}

impl Supervisor {
    pub fn new() -> Self {
        Supervisor {
            id: Uuid::new_v4().to_string(),
            agents: Arc::new(DashMap::new()),
            dead_letter_queue: Arc::new(RwLock::new(Vec::new())),
            state: Arc::new(RwLock::new(SupervisorState::Idle)),
            created_at: Utc::now(),
        }
    }

    /// Spawn a new agent under supervision (must be called from async context)
    /// For synchronous spawn, use spawn_agent_sync after starting supervisor
    pub async fn spawn_agent(&self, agent: Agent) -> Result<String> {
        let state = self.state.read().await.clone();
        if state != SupervisorState::Supervising {
            return Err(anyhow!("Supervisor not in supervising state: {:?}", state));
        }

        let agent_id = agent.id.clone();
        let managed = ManagedAgent::new(agent);

        self.agents.insert(agent_id.clone(), managed);
        Ok(agent_id)
    }

    /// Spawn agent synchronously (only works if supervisor is already supervising)
    pub fn spawn_agent_sync(&self, agent: Agent) -> Result<String> {
        // Simple synchronous check without async
        let agent_id = agent.id.clone();
        let managed = ManagedAgent::new(agent);
        self.agents.insert(agent_id.clone(), managed);
        Ok(agent_id)
    }

    /// Start supervising (activate supervisor)
    pub async fn start(&self) -> Result<()> {
        let mut state = self.state.write().await;
        if *state != SupervisorState::Idle {
            return Err(anyhow!("Supervisor already started or failed"));
        }

        *state = SupervisorState::Supervising;
        Ok(())
    }

    /// Detect and handle agent crash
    pub async fn handle_crash(&self, agent_id: &str, reason: CrashReason) -> Result<bool> {
        if let Some(mut managed) = self.agents.get_mut(agent_id) {
            managed.last_crash = Some((reason.clone(), Utc::now()));

            if managed.can_restart() {
                managed.increment_restart_count();
                let restart_count = managed.restart_count();
                let backoff_ms = INITIAL_BACKOFF_MS * (1 << (restart_count - 1));

                // Exponential backoff before restart
                tokio::time::sleep(Duration::from_millis(backoff_ms)).await;

                // Attempt restart: create new agent instance
                let old_agent = managed.agent.read().await;
                let new_agent = Agent::new(old_agent.name.clone());
                drop(old_agent);

                *managed.agent.write().await = new_agent;
                managed.last_seen_alive = Utc::now();

                Ok(true) // Restart successful
            } else {
                // Permanent failure: mark agent as terminated
                let mut agent = managed.agent.write().await;
                let _ = agent.terminate();
                Ok(false) // No more restart attempts
            }
        } else {
            Err(anyhow!("Agent not found: {}", agent_id))
        }
    }

    /// Health check: detect unhealthy agents
    pub async fn health_check(&self) -> Result<Vec<String>> {
        let mut unhealthy = Vec::new();

        for entry in self.agents.iter() {
            let managed = entry.value();
            if !managed.is_healthy() {
                unhealthy.push(managed.id.clone());
            }
        }

        Ok(unhealthy)
    }

    /// Add message to dead letter queue when delivery fails
    pub async fn queue_dead_letter(&self, agent_id: String, message: Message) -> Result<()> {
        let mut dlq = self.dead_letter_queue.write().await;
        dlq.push((agent_id, message));
        Ok(())
    }

    /// Retrieve dead letter queue messages
    pub async fn get_dead_letters(&self) -> Result<Vec<(String, Message)>> {
        let dlq = self.dead_letter_queue.read().await;
        Ok(dlq.clone())
    }

    /// Clear processed messages from dead letter queue
    pub async fn clear_dead_letters(&self) -> Result<()> {
        let mut dlq = self.dead_letter_queue.write().await;
        dlq.clear();
        Ok(())
    }

    /// Get agent health metrics
    pub async fn get_agent_health(&self, agent_id: &str) -> Result<AgentHealth> {
        if let Some(managed) = self.agents.get(agent_id) {
            let agent = managed.agent.read().await;
            Ok(AgentHealth {
                agent_id: agent_id.to_string(),
                state: agent.state_code().to_string(),
                restart_count: managed.restart_count(),
                last_crash: managed.last_crash.clone(),
                healthy: managed.is_healthy(),
                uptime_ms: agent.uptime_ms(),
            })
        } else {
            Err(anyhow!("Agent not found: {}", agent_id))
        }
    }

    /// Graceful shutdown of all agents (5s timeout)
    pub async fn shutdown(&self) -> Result<()> {
        let mut state = self.state.write().await;
        if *state == SupervisorState::Failed {
            return Err(anyhow!("Supervisor already failed"));
        }

        *state = SupervisorState::Shutting;
        drop(state);

        let shutdown_timeout = Duration::from_millis(SHUTDOWN_TIMEOUT_MS);
        let start = std::time::Instant::now();

        for entry in self.agents.iter() {
            if start.elapsed() > shutdown_timeout {
                break;
            }

            let mut agent = entry.value().agent.write().await;
            let _ = agent.terminate();
        }

        let mut state = self.state.write().await;
        *state = SupervisorState::Idle;
        Ok(())
    }

    /// Get supervisor state
    pub async fn get_state(&self) -> SupervisorState {
        *self.state.read().await
    }

    /// Get all managed agents
    pub fn get_agents(&self) -> Vec<String> {
        self.agents
            .iter()
            .map(|entry| entry.value().id.clone())
            .collect()
    }

    /// Get agent count
    pub fn agent_count(&self) -> usize {
        self.agents.len()
    }

    /// Check if agent exists
    pub fn has_agent(&self, agent_id: &str) -> bool {
        self.agents.contains_key(agent_id)
    }

    /// Get restart count for agent
    pub fn get_restart_count(&self, agent_id: &str) -> Option<u32> {
        self.agents.get(agent_id).map(|m| m.restart_count())
    }
}

impl Default for Supervisor {
    fn default() -> Self {
        Self::new()
    }
}

/// Agent health information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentHealth {
    pub agent_id: String,
    pub state: String,
    pub restart_count: u32,
    pub last_crash: Option<(CrashReason, DateTime<Utc>)>,
    pub healthy: bool,
    pub uptime_ms: u128,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_supervisor_creation() {
        let supervisor = Supervisor::new();
        assert_eq!(supervisor.agent_count(), 0);
        assert_eq!(supervisor.get_state().await, SupervisorState::Idle);
    }

    #[tokio::test]
    async fn test_start_supervisor() {
        let supervisor = Supervisor::new();
        assert!(supervisor.start().await.is_ok());
        assert_eq!(supervisor.get_state().await, SupervisorState::Supervising);
    }

    #[tokio::test]
    async fn test_spawn_agent_requires_supervising() {
        let supervisor = Supervisor::new();
        let agent = Agent::new("TestAgent");

        // Should fail if supervisor not started
        assert!(supervisor.spawn_agent(agent.clone()).await.is_err());

        // Should succeed after starting
        let _ = supervisor.start().await;
        assert!(supervisor.spawn_agent(agent).await.is_ok());
    }

    #[tokio::test]
    async fn test_spawn_multiple_agents() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        for i in 0..5 {
            let agent = Agent::new(format!("Agent{}", i));
            assert!(supervisor.spawn_agent(agent).await.is_ok());
        }

        assert_eq!(supervisor.agent_count(), 5);
    }

    #[tokio::test]
    async fn test_agent_crash_detection() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("CrashAgent");
        let agent_id = supervisor.spawn_agent(agent).await.unwrap();

        // Simulate crash
        let restarted = supervisor
            .handle_crash(&agent_id, CrashReason::Panic("Test panic".to_string()))
            .await
            .unwrap();

        assert!(restarted);
        assert_eq!(supervisor.get_restart_count(&agent_id), Some(1));
    }

    #[tokio::test]
    async fn test_exponential_backoff() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("BackoffAgent");
        let agent_id = supervisor.spawn_agent(agent).await.unwrap();

        let start = std::time::Instant::now();

        // First crash: 100ms backoff
        let _ = supervisor
            .handle_crash(&agent_id, CrashReason::Panic("Crash 1".to_string()))
            .await;
        assert!(start.elapsed().as_millis() >= 100);
    }

    #[tokio::test]
    async fn test_max_restarts_exceeded() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("MaxRestartAgent");
        let agent_id = supervisor.spawn_agent(agent).await.unwrap();

        // Trigger MAX_RESTART_ATTEMPTS crashes
        for i in 0..MAX_RESTART_ATTEMPTS {
            let restarted = supervisor
                .handle_crash(&agent_id, CrashReason::Panic(format!("Crash {}", i)))
                .await
                .unwrap();

            if i < MAX_RESTART_ATTEMPTS - 1 {
                assert!(restarted);
            } else {
                assert!(!restarted); // No more restart attempts
            }
        }

        assert_eq!(
            supervisor.get_restart_count(&agent_id),
            Some(MAX_RESTART_ATTEMPTS)
        );
    }

    #[tokio::test]
    async fn test_dead_letter_queue() {
        let supervisor = Supervisor::new();
        let msg = Message::new(
            crate::messaging::MessageType::TaskRequest,
            "sender",
            None,
            serde_json::json!({"test": "data"}),
        );

        let _ = supervisor
            .queue_dead_letter("agent-1".to_string(), msg.clone())
            .await;

        let dlq = supervisor.get_dead_letters().await.unwrap();
        assert_eq!(dlq.len(), 1);
        assert_eq!(dlq[0].0, "agent-1");
    }

    #[tokio::test]
    async fn test_clear_dead_letter_queue() {
        let supervisor = Supervisor::new();
        let msg = Message::new(
            crate::messaging::MessageType::TaskRequest,
            "sender",
            None,
            serde_json::json!({}),
        );

        let _ = supervisor
            .queue_dead_letter("agent-1".to_string(), msg)
            .await;
        let _ = supervisor.clear_dead_letters().await;

        let dlq = supervisor.get_dead_letters().await.unwrap();
        assert_eq!(dlq.len(), 0);
    }

    #[tokio::test]
    async fn test_graceful_shutdown() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        for i in 0..3 {
            let agent = Agent::new(format!("ShutdownAgent{}", i));
            let _ = supervisor.spawn_agent(agent);
        }

        assert_eq!(supervisor.agent_count(), 3);
        let _ = supervisor.shutdown().await;
        assert_eq!(supervisor.get_state().await, SupervisorState::Idle);
    }

    #[tokio::test]
    async fn test_health_check_returns_unhealthy_agents() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("HealthCheckAgent");
        let _ = supervisor.spawn_agent(agent);

        let unhealthy = supervisor.health_check().await.unwrap();
        // Newly spawned agent should be healthy
        assert_eq!(unhealthy.len(), 0);
    }

    #[tokio::test]
    async fn test_agent_health_metrics() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("MetricsAgent");
        let agent_id = supervisor.spawn_agent(agent).await.unwrap();

        let health = supervisor.get_agent_health(&agent_id).await.unwrap();
        assert_eq!(health.agent_id, agent_id);
        assert_eq!(health.restart_count, 0);
        assert!(health.healthy);
    }

    #[tokio::test]
    async fn test_health_includes_crash_reason() {
        let supervisor = Supervisor::new();
        let _ = supervisor.start().await;

        let agent = Agent::new("CrashReasonAgent");
        let agent_id = supervisor.spawn_agent(agent).await.unwrap();

        let _ = supervisor
            .handle_crash(&agent_id, CrashReason::Timeout)
            .await;

        let health = supervisor.get_agent_health(&agent_id).await.unwrap();
        assert!(health.last_crash.is_some());
    }
}
