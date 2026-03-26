//! Agent lifecycle management with health tracking and recovery

use std::sync::Arc;
use std::time::{Duration, Instant};
use uuid::Uuid;
use dashmap::DashMap;
use serde::{Serialize, Deserialize};

/// Health status of an agent
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HealthStatus {
    /// Agent is operational
    Healthy,
    /// Agent is degraded but operational
    Degraded,
    /// Agent has failed
    Failed,
    /// Agent is recovering
    Recovering,
}

impl HealthStatus {
    /// Convert to string code
    pub fn code(&self) -> &'static str {
        match self {
            HealthStatus::Healthy => "HEALTHY",
            HealthStatus::Degraded => "DEGRADED",
            HealthStatus::Failed => "FAILED",
            HealthStatus::Recovering => "RECOVERING",
        }
    }
}

/// Agent health metrics
#[derive(Debug, Clone)]
pub struct AgentHealth {
    /// Current health status
    pub status: HealthStatus,
    /// Last heartbeat timestamp
    pub last_heartbeat: Instant,
    /// Number of failures
    pub failure_count: usize,
    /// Number of successful operations
    pub success_count: usize,
    /// Message latency in milliseconds
    pub message_latency_ms: u64,
}

impl AgentHealth {
    /// Create new health metrics
    pub fn new() -> Self {
        Self {
            status: HealthStatus::Healthy,
            last_heartbeat: Instant::now(),
            failure_count: 0,
            success_count: 0,
            message_latency_ms: 0,
        }
    }

    /// Update heartbeat timestamp
    pub fn heartbeat(&mut self) {
        self.last_heartbeat = Instant::now();
        if self.status == HealthStatus::Recovering {
            self.status = HealthStatus::Healthy;
        }
    }

    /// Record a success
    pub fn record_success(&mut self) {
        self.success_count += 1;
        if self.status == HealthStatus::Degraded {
            self.status = HealthStatus::Healthy;
        }
    }

    /// Record a failure
    pub fn record_failure(&mut self) {
        self.failure_count += 1;
        if self.failure_count >= 3 {
            self.status = HealthStatus::Failed;
        } else if self.failure_count >= 1 {
            self.status = HealthStatus::Degraded;
        }
    }

    /// Check if agent is unresponsive
    pub fn is_unresponsive(&self, timeout_secs: u64) -> bool {
        self.last_heartbeat.elapsed() > Duration::from_secs(timeout_secs)
    }

    /// Success rate as percentage
    pub fn success_rate(&self) -> f64 {
        let total = self.success_count + self.failure_count;
        if total == 0 {
            100.0
        } else {
            (self.success_count as f64 / total as f64) * 100.0
        }
    }
}

impl Default for AgentHealth {
    fn default() -> Self {
        Self::new()
    }
}

/// A single agent instance
#[derive(Debug, Clone)]
pub struct AgentInstance {
    /// Unique agent ID
    pub id: Uuid,
    /// Agent name
    pub name: String,
    /// Health metrics
    pub health: AgentHealth,
    /// Tasks assigned to this agent
    pub task_count: usize,
    /// Creation time
    pub created_at: Instant,
}

impl AgentInstance {
    /// Create a new agent instance
    pub fn new(name: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            name,
            health: AgentHealth::new(),
            task_count: 0,
            created_at: Instant::now(),
        }
    }

    /// Get agent uptime in milliseconds
    pub fn uptime_ms(&self) -> u128 {
        self.created_at.elapsed().as_millis()
    }
}

/// Pool of managed agents with health tracking
pub struct AgentPool {
    agents: Arc<DashMap<Uuid, AgentInstance>>,
    health_check_interval: Duration,
    unresponsive_timeout_secs: u64,
}

impl AgentPool {
    /// Create a new agent pool
    pub fn new(num_agents: usize) -> Self {
        let agents = Arc::new(DashMap::new());
        for i in 0..num_agents {
            let agent = AgentInstance::new(format!("Agent-{}", i));
            agents.insert(agent.id, agent);
        }

        Self {
            agents,
            health_check_interval: Duration::from_secs(1),
            unresponsive_timeout_secs: 5,
        }
    }

    /// Get agent by ID
    pub fn get(&self, id: Uuid) -> Option<AgentInstance> {
        self.agents.get(&id).map(|r| r.clone())
    }

    /// Get all agents
    pub fn all(&self) -> Vec<AgentInstance> {
        self.agents.iter().map(|r| r.value().clone()).collect()
    }

    /// Update agent health
    pub fn update_health(&self, id: Uuid, health: AgentHealth) {
        if let Some(mut agent) = self.agents.get_mut(&id) {
            agent.health = health;
        }
    }

    /// Get healthy agents
    pub fn healthy_agents(&self) -> Vec<AgentInstance> {
        self.all()
            .into_iter()
            .filter(|a| a.health.status == HealthStatus::Healthy)
            .collect()
    }

    /// Get failed agents
    pub fn failed_agents(&self) -> Vec<AgentInstance> {
        self.all()
            .into_iter()
            .filter(|a| a.health.status == HealthStatus::Failed)
            .collect()
    }

    /// Perform health check on all agents
    pub async fn perform_health_check(&self) {
        for mut agent_ref in self.agents.iter_mut() {
            if agent_ref.health.is_unresponsive(self.unresponsive_timeout_secs) {
                agent_ref.health.status = HealthStatus::Failed;
            }
        }
    }

    /// Attempt recovery of failed agent
    pub fn recover_agent(&self, id: Uuid) -> bool {
        if let Some(mut agent) = self.agents.get_mut(&id) {
            if agent.health.status == HealthStatus::Failed {
                agent.health.status = HealthStatus::Recovering;
                agent.health.failure_count = 0;
                return true;
            }
        }
        false
    }

    /// Get system health summary
    pub fn health_summary(&self) -> HealthSummary {
        let agents = self.all();
        let total = agents.len();
        let healthy = agents
            .iter()
            .filter(|a| a.health.status == HealthStatus::Healthy)
            .count();
        let degraded = agents
            .iter()
            .filter(|a| a.health.status == HealthStatus::Degraded)
            .count();
        let failed = agents
            .iter()
            .filter(|a| a.health.status == HealthStatus::Failed)
            .count();

        let total_successes: usize = agents.iter().map(|a| a.health.success_count).sum();
        let total_failures: usize = agents.iter().map(|a| a.health.failure_count).sum();

        HealthSummary {
            total_agents: total,
            healthy_count: healthy,
            degraded_count: degraded,
            failed_count: failed,
            total_successes,
            total_failures,
            system_health_percentage: (healthy as f64 / total as f64) * 100.0,
        }
    }
}

/// System health summary
#[derive(Debug, Clone)]
pub struct HealthSummary {
    /// Total number of agents
    pub total_agents: usize,
    /// Number of healthy agents
    pub healthy_count: usize,
    /// Number of degraded agents
    pub degraded_count: usize,
    /// Number of failed agents
    pub failed_count: usize,
    /// Total successful operations
    pub total_successes: usize,
    /// Total failed operations
    pub total_failures: usize,
    /// System health as percentage
    pub system_health_percentage: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_health_status_codes() {
        assert_eq!(HealthStatus::Healthy.code(), "HEALTHY");
        assert_eq!(HealthStatus::Failed.code(), "FAILED");
    }

    #[test]
    fn test_agent_creation() {
        let agent = AgentInstance::new("test-agent".to_string());
        assert_eq!(agent.name, "test-agent");
        assert_eq!(agent.health.status, HealthStatus::Healthy);
    }

    #[test]
    fn test_health_record_success() {
        let mut health = AgentHealth::new();
        health.record_success();
        assert_eq!(health.success_count, 1);
        assert_eq!(health.status, HealthStatus::Healthy);
    }

    #[test]
    fn test_health_record_failures() {
        let mut health = AgentHealth::new();
        health.record_failure();
        assert_eq!(health.status, HealthStatus::Degraded);
        health.record_failure();
        assert_eq!(health.status, HealthStatus::Degraded);
        health.record_failure();
        assert_eq!(health.status, HealthStatus::Failed);
    }

    #[test]
    fn test_agent_pool_creation() {
        let pool = AgentPool::new(6);
        assert_eq!(pool.all().len(), 6);
    }

    #[test]
    fn test_agent_pool_get_healthy() {
        let pool = AgentPool::new(3);
        let healthy = pool.healthy_agents();
        assert_eq!(healthy.len(), 3);
    }

    #[test]
    fn test_health_summary() {
        let pool = AgentPool::new(3);
        let summary = pool.health_summary();
        assert_eq!(summary.total_agents, 3);
        assert_eq!(summary.healthy_count, 3);
    }

    #[test]
    fn test_success_rate() {
        let mut health = AgentHealth::new();
        health.success_count = 90;
        health.failure_count = 10;
        assert_eq!(health.success_rate(), 90.0);
    }
}
