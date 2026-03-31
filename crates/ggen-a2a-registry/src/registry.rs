//! `AgentRegistry` -- the primary public API for multi-agent orchestration.
//!
//! It wraps a pluggable `AgentStore` and an optional `HealthMonitor` to
//! provide registration, discovery, health checking, listing, and shutdown.

use std::sync::Arc;
use tracing::{debug, info};

use crate::error::{RegistryError, RegistryResult};
use crate::health::{ping_agent, HealthConfig, HealthMonitor};
use crate::query::{matches_query, AgentQuery};
use crate::store::AgentStore;
use crate::types::{AgentEntry, HealthStatus, Registration};

/// Central registry for managing A2A agent lifecycle.
pub struct AgentRegistry {
    store: Arc<dyn AgentStore>,
    health_monitor: Arc<HealthMonitor>,
}

impl AgentRegistry {
    /// Create a registry with the given store and default health-monitor config.
    #[must_use]
    pub fn new(store: Arc<dyn AgentStore>) -> Self {
        let monitor = Arc::new(HealthMonitor::new(
            Arc::clone(&store),
            HealthConfig::default(),
        ));
        Self {
            store,
            health_monitor: monitor,
        }
    }

    /// Create a registry with a custom health-monitor configuration.
    #[must_use]
    pub fn with_health_config(store: Arc<dyn AgentStore>, config: HealthConfig) -> Self {
        let monitor = Arc::new(HealthMonitor::new(Arc::clone(&store), config));
        Self {
            store,
            health_monitor: monitor,
        }
    }

    /// Register a new agent.
    ///
    /// Returns a `Registration` on success, or `AlreadyRegistered` if the ID
    /// already exists in the store.
    ///
    /// # Errors
    ///
    /// Returns `RegistryError::AlreadyRegistered` if the agent ID already exists.
    /// Returns `RegistryError::StoreError` if the underlying store operation fails.
    pub async fn register(&self, agent: AgentEntry) -> RegistryResult<Registration> {
        let agent_id = agent.id.clone();
        let registered_at = agent.registered_at;

        self.store.register(agent).await.map_err(|e| {
            if matches!(e, RegistryError::AlreadyRegistered(_)) {
                e
            } else {
                RegistryError::StoreError(e.to_string())
            }
        })?;

        info!(agent_id = %agent_id, "agent registered");
        Ok(Registration {
            agent_id,
            registered_at,
        })
    }

    /// Discover agents matching a query.
    ///
    /// # Errors
    ///
    /// Returns `RegistryError::StoreError` if the underlying store operation fails.
    /// Returns `RegistryError::NoMatch` if no agents match the query.
    pub async fn discover(&self, query: AgentQuery) -> RegistryResult<Vec<AgentEntry>> {
        let all = self
            .store
            .list()
            .await
            .map_err(|e| RegistryError::StoreError(e.to_string()))?;

        let mut matched: Vec<AgentEntry> = all
            .into_iter()
            .filter(|e| matches_query(e, &query))
            .collect();

        if matched.is_empty() {
            return Err(RegistryError::NoMatch(format!("{query:?}")));
        }

        if query.limit > 0 && matched.len() > query.limit {
            matched.truncate(query.limit);
        }

        Ok(matched)
    }

    /// Run a one-shot health check against a specific agent.
    ///
    /// This makes a real HTTP GET to the agent's endpoint and returns the
    /// derived `HealthStatus`.  The result is also persisted in the store.
    ///
    /// # Errors
    ///
    /// Returns `RegistryError::AgentNotFound` if the agent ID does not exist.
    /// Returns `RegistryError::StoreError` if the underlying store operation fails.
    /// Returns `RegistryError::HttpError` or `RegistryError::Timeout` from the HTTP ping.
    pub async fn health_check(&self, agent_id: &str) -> RegistryResult<HealthStatus> {
        let agent = self
            .store
            .get(agent_id)
            .await
            .map_err(|e| RegistryError::StoreError(e.to_string()))?
            .ok_or_else(|| RegistryError::AgentNotFound(agent_id.to_string()))?;

        let status = ping_agent(&agent, std::time::Duration::from_secs(10)).await?;
        self.store
            .update_health(agent_id, status)
            .await
            .map_err(|e| RegistryError::StoreError(e.to_string()))?;

        debug!(agent_id = %agent_id, health = %status, "one-shot health check");
        Ok(status)
    }

    /// List all registered agents.
    ///
    /// # Errors
    ///
    /// Returns `RegistryError::StoreError` if the underlying store operation fails.
    pub async fn list(&self) -> RegistryResult<Vec<AgentEntry>> {
        self.store
            .list()
            .await
            .map_err(|e| RegistryError::StoreError(e.to_string()))
    }

    /// Remove an agent from the registry.
    ///
    /// # Errors
    ///
    /// Returns `RegistryError::AgentNotFound` if the agent ID does not exist.
    /// Returns `RegistryError::StoreError` if the underlying store operation fails.
    pub async fn deregister(&self, agent_id: &str) -> RegistryResult<()> {
        self.store
            .remove(agent_id)
            .await
            .map_err(|e| RegistryError::StoreError(e.to_string()))?;
        info!(agent_id = %agent_id, "agent deregistered");
        Ok(())
    }

    /// Start the background health monitor.
    pub async fn start_health_monitor(&self) {
        self.health_monitor.start().await;
    }

    /// Stop the background health monitor.
    pub async fn stop_health_monitor(&self) {
        self.health_monitor.stop().await;
    }

    /// Check if the health monitor is running.
    #[must_use]
    pub fn is_health_monitor_running(&self) -> bool {
        self.health_monitor.is_running()
    }

    /// Shut down the registry: stop the health monitor.
    ///
    /// The store itself is not cleared -- that is left to the caller or
    /// the store's own Drop implementation.
    ///
    /// # Errors
    ///
    /// This method currently never returns an error, but the `Result` return
    /// type is preserved for forward compatibility.
    pub async fn shutdown(&self) -> RegistryResult<()> {
        info!("shutting down agent registry");
        self.health_monitor.stop().await;
        Ok(())
    }
}
