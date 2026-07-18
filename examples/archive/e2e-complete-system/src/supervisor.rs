/// Supervisor Tree Implementation
/// Monitors agents and automatically restarts them on crash
use crate::agents::DomainAgent;
use crate::orchestrator::LifeDomain;
use anyhow::Result;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time::{sleep, Duration};
use tracing::{error, info, warn};

/// Supervision strategy
#[derive(Clone, Copy, Debug)]
pub enum SupervisionStrategy {
    OneForOne,  // Restart only the failed child
    AllForOne,  // Restart all children if one fails
    RestForOne, // Restart failed child and all that depend on it
}

/// Supervisor for an agent
pub struct Supervisor {
    domain: LifeDomain,
    agent: Arc<Mutex<DomainAgent>>,
    is_running: Arc<AtomicBool>,
    restart_count: Arc<AtomicUsize>,
    max_restarts: usize,
    restart_delay: Duration,
    strategy: SupervisionStrategy,
    persistent_state: Arc<Mutex<Option<f64>>>,
}

impl Supervisor {
    /// Create a new supervisor for an agent
    pub async fn new(domain: LifeDomain, agent: DomainAgent) -> Result<Self> {
        info!("[SUPERVISOR-{}] Initializing supervisor", domain);

        Ok(Self {
            domain,
            agent: Arc::new(Mutex::new(agent)),
            is_running: Arc::new(AtomicBool::new(true)),
            restart_count: Arc::new(AtomicUsize::new(0)),
            max_restarts: 5,
            restart_delay: Duration::from_millis(100),
            strategy: SupervisionStrategy::OneForOne,
            persistent_state: Arc::new(Mutex::new(None)),
        })
    }

    /// Start monitoring the agent
    pub async fn start_monitoring(&self) {
        let _domain = self.domain.clone();
        let is_running = self.is_running.clone();
        let _agent = self.agent.clone();

        tokio::spawn(async move {
            while is_running.load(Ordering::Relaxed) {
                // Simulate health check
                sleep(Duration::from_millis(500)).await;

                // Check if agent is still responsive
                // In real implementation, would send heartbeat
            }
        });
    }

    /// Simulate agent crash
    pub async fn crash(&self) {
        info!("[SUPERVISOR-{}] Agent crashed!", self.domain);
        self.is_running.store(false, Ordering::Relaxed);

        // Save state before crash
        if let Ok(score) = self.save_state().await {
            let mut state = self.persistent_state.lock().await;
            *state = Some(score);
        }
    }

    /// Restart the agent
    pub async fn restart_agent(&self) -> Result<()> {
        let restart_count = self.restart_count.fetch_add(1, Ordering::SeqCst);

        if restart_count >= self.max_restarts {
            error!(
                "[SUPERVISOR-{}] Max restarts exceeded ({})",
                self.domain, self.max_restarts
            );
            return Err(anyhow::anyhow!("Max restarts exceeded"));
        }

        info!(
            "[SUPERVISOR-{}] Restarting agent (attempt {}/{})",
            self.domain,
            restart_count + 1,
            self.max_restarts
        );

        // Wait before restarting (exponential backoff would be better)
        sleep(self.restart_delay).await;

        // Restore state
        if let Some(saved_score) = *self.persistent_state.lock().await {
            self.agent.lock().await.restore_state(saved_score).await?;
            info!(
                "[SUPERVISOR-{}] State restored: {:.2}",
                self.domain, saved_score
            );
        }

        // Mark as running
        self.is_running.store(true, Ordering::Relaxed);
        info!("[SUPERVISOR-{}] Agent restarted successfully", self.domain);

        Ok(())
    }

    /// Save agent state before crash
    async fn save_state(&self) -> Result<f64> {
        let score = self.agent.lock().await.get_health_score().await;
        info!("[SUPERVISOR-{}] State saved: {:.2}", self.domain, score);
        Ok(score)
    }

    /// Check if agent is healthy
    pub async fn is_healthy(&self) -> bool {
        self.is_running.load(Ordering::Relaxed)
    }

    /// Get restart count
    pub fn get_restart_count(&self) -> usize {
        self.restart_count.load(Ordering::SeqCst)
    }

    /// Reset restart count (after successful operation)
    pub fn reset_restart_count(&self) {
        self.restart_count.store(0, Ordering::SeqCst);
    }

    /// Get the supervised agent
    pub fn get_agent(&self) -> Arc<Mutex<DomainAgent>> {
        self.agent.clone()
    }

    /// Get domain
    pub fn get_domain(&self) -> LifeDomain {
        self.domain.clone()
    }
}

/// Hierarchical supervisor tree
pub struct SupervisorTree {
    root: Arc<Supervisor>,
    children: Arc<std::sync::Mutex<Vec<Arc<Supervisor>>>>,
}

impl SupervisorTree {
    /// Create a new supervisor tree
    pub async fn new(domain: LifeDomain, agent: DomainAgent) -> Result<Self> {
        let supervisor = Supervisor::new(domain, agent).await?;

        Ok(Self {
            root: Arc::new(supervisor),
            children: Arc::new(std::sync::Mutex::new(Vec::new())),
        })
    }

    /// Add a child supervisor
    pub fn add_child(&self, supervisor: Arc<Supervisor>) {
        self.children.lock().unwrap().push(supervisor);
    }

    /// Restart all children
    pub async fn restart_all_children(&self) -> Result<()> {
        for supervisor in self.children.lock().unwrap().iter() {
            supervisor.restart_agent().await?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_supervisor_creation() {
        let agent = DomainAgent::new(LifeDomain::Health)
            .await
            .expect("Failed to create agent");
        let supervisor = Supervisor::new(LifeDomain::Health, agent)
            .await
            .expect("Failed to create supervisor");
        assert!(supervisor.is_healthy().await);
    }

    #[tokio::test]
    async fn test_supervisor_crash_and_restart() {
        let agent = DomainAgent::new(LifeDomain::Health)
            .await
            .expect("Failed to create agent");
        let supervisor = Supervisor::new(LifeDomain::Health, agent)
            .await
            .expect("Failed to create supervisor");

        // Simulate crash
        supervisor.crash().await;
        assert!(!supervisor.is_healthy().await);

        // Restart
        supervisor.restart_agent().await.expect("Failed to restart");
        assert!(supervisor.is_healthy().await);
    }

    #[tokio::test]
    async fn test_restart_count() {
        let agent = DomainAgent::new(LifeDomain::Career)
            .await
            .expect("Failed to create agent");
        let supervisor = Supervisor::new(LifeDomain::Career, agent)
            .await
            .expect("Failed to create supervisor");

        assert_eq!(supervisor.get_restart_count(), 0);
        supervisor.crash().await;
        supervisor.restart_agent().await.expect("Failed to restart");
        assert_eq!(supervisor.get_restart_count(), 1);
    }

    #[tokio::test]
    async fn test_state_persistence() {
        let agent = DomainAgent::new(LifeDomain::Learning)
            .await
            .expect("Failed to create agent");
        let supervisor = Supervisor::new(LifeDomain::Learning, agent)
            .await
            .expect("Failed to create supervisor");

        // Set initial state
        supervisor
            .agent
            .lock()
            .await
            .restore_state(0.75)
            .await
            .expect("Failed to restore");

        // Crash and save
        supervisor.crash().await;

        // Restart and verify state recovered
        supervisor.restart_agent().await.expect("Failed to restart");
        let score = supervisor.agent.lock().await.get_health_score().await;
        assert_eq!(score, 0.75);
    }
}
