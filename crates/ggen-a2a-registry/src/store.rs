//! Agent store trait and an in-memory implementation.
//!
//! The trait is intentionally minimal so that alternative backends
//! (`SQLite`, `Redis`, etc.) can be plugged in without changing registry logic.

use async_trait::async_trait;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::debug;

use crate::error::{RegistryError, RegistryResult};
use crate::types::{AgentEntry, HealthStatus};

/// Persistence / retrieval contract for agent entries.
#[async_trait]
pub trait AgentStore: Send + Sync {
    /// Insert a new agent.  Returns `AlreadyRegistered` if the ID exists.
    async fn register(&self, agent: AgentEntry) -> RegistryResult<()>;

    /// Retrieve an agent by ID.
    async fn get(&self, id: &str) -> RegistryResult<Option<AgentEntry>>;

    /// List every registered agent.
    async fn list(&self) -> RegistryResult<Vec<AgentEntry>>;

    /// Update only the health status (and last-heartbeat timestamp) for an agent.
    async fn update_health(&self, id: &str, status: HealthStatus) -> RegistryResult<()>;

    /// Remove an agent from the store.
    async fn remove(&self, id: &str) -> RegistryResult<()>;

    /// Find all agents that advertise a given capability.
    async fn find_by_capability(&self, capability: &str) -> RegistryResult<Vec<AgentEntry>>;
}

/// In-memory store backed by a `HashMap` protected by `RwLock`.
///
/// Suitable for single-process use and testing.  Thread-safe and async-friendly.
#[derive(Debug, Clone, Default)]
pub struct MemoryStore {
    inner: Arc<RwLock<HashMap<String, AgentEntry>>>,
}

impl MemoryStore {
    /// Create a new empty in-memory store.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

#[async_trait]
impl AgentStore for MemoryStore {
    async fn register(&self, agent: AgentEntry) -> RegistryResult<()> {
        {
            let mut map = self.inner.write().await;
            if map.contains_key(&agent.id) {
                return Err(RegistryError::AlreadyRegistered(agent.id));
            }
            debug!(agent_id = %agent.id, "registered agent");
            map.insert(agent.id.clone(), agent);
        }
        Ok(())
    }

    async fn get(&self, id: &str) -> RegistryResult<Option<AgentEntry>> {
        let map = self.inner.read().await;
        Ok(map.get(id).cloned())
    }

    async fn list(&self) -> RegistryResult<Vec<AgentEntry>> {
        let map = self.inner.read().await;
        Ok(map.values().cloned().collect())
    }

    async fn update_health(&self, id: &str, status: HealthStatus) -> RegistryResult<()> {
        {
            let mut map = self.inner.write().await;
            let entry = map
                .get_mut(id)
                .ok_or_else(|| RegistryError::AgentNotFound(id.to_string()))?;
            entry.health = status;
            entry.last_heartbeat = chrono::Utc::now();
            debug!(agent_id = %id, new_health = %status, "updated health");
        }
        Ok(())
    }

    async fn remove(&self, id: &str) -> RegistryResult<()> {
        {
            let mut map = self.inner.write().await;
            map.remove(id)
                .ok_or_else(|| RegistryError::AgentNotFound(id.to_string()))?;
            debug!(agent_id = %id, "removed agent");
        }
        Ok(())
    }

    async fn find_by_capability(&self, capability: &str) -> RegistryResult<Vec<AgentEntry>> {
        let map = self.inner.read().await;
        let results: Vec<AgentEntry> = map
            .values()
            .filter(|e| e.capabilities.iter().any(|c| c == capability))
            .cloned()
            .collect();
        Ok(results)
    }
}
