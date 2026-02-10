//! Persistence Service for OSIRIS

use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

use crate::{OSIRISError, Result};

/// Persisted state for workflows
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistedState {
    pub workflow_id: String,
    pub data: Value,
    pub timestamp: Instant,
    pub version: String,
}

/// Persistence service for OSIRIS
pub struct PersistenceService {
    storage_path: String,
    cache: Arc<RwLock<HashMap<String, PersistedState>>>,
    max_cache_size: usize,
}

impl PersistenceService {
    /// Create a new persistence service
    pub fn new() -> Self {
        Self::with_path(".osiris/persistence")
    }

    /// Create with custom storage path
    pub fn with_path(path: &str) -> Self {
        fs::create_dir_all(path).unwrap_or_else(|_| {
            warn!("Failed to create persistence directory: {}", path);
        });

        Self {
            storage_path: path.to_string(),
            cache: Arc::new(RwLock::new(HashMap::new())),
            max_cache_size: 1000,
        }
    }

    /// Persist workflow state
    pub async fn persist_workflow_state(&self, workflow: &Workflow) -> Result<()> {
        let state = PersistedState {
            workflow_id: workflow.id.clone(),
            data: self.workflow_to_value(workflow),
            timestamp: Instant::now(),
            version: "1.0".to_string(),
        };

        self.persist_state(&state).await
    }

    /// Persist generic state
    pub async fn persist_state(&self, state: &PersistedState) -> Result<()> {
        debug!("Persisting state for workflow: {}", state.workflow_id);

        // Cache the state
        let mut cache = self.cache.write().await;
        cache.insert(state.workflow_id.clone(), state.clone());

        // Keep cache size limited
        if cache.len() > self.max_cache_size {
            let oldest_key = cache.keys().next().unwrap().clone();
            cache.remove(&oldest_key);
        }

        // Write to disk
        let file_path = self.get_state_file_path(&state.workflow_id);
        let json = serde_json::to_string_pretty(state)
            .map_err(|e| OSIRISError::SerializationError(e.to_string()))?;

        tokio::fs::write(&file_path, json)
            .await
            .map_err(|e| OSIRISError::Unknown(e.to_string()))?;

        info!("Persisted workflow state: {}", state.workflow_id);
        Ok(())
    }

    /// Get persisted workflow state
    pub async fn get_workflow_state(&self, workflow_id: &str) -> Result<PersistedState> {
        debug!("Getting persisted state for workflow: {}", workflow_id);

        // Check cache first
        {
            let cache = self.cache.read().await;
            if let Some(cached) = cache.get(workflow_id) {
                if cached.timestamp.elapsed() < Duration::from_secs(60) {
                    return Ok(cached.clone());
                }
            }
        }

        // Load from disk
        let file_path = self.get_state_file_path(workflow_id);
        let json = tokio::fs::read_to_string(&file_path)
            .await
            .map_err(|e| {
                if e.kind() == std::io::ErrorKind::NotFound {
                    OSIRISError::WorkflowNotFound(workflow_id.to_string())
                } else {
                    OSIRISError::Unknown(e.to_string())
                }
            })?;

        let state: PersistedState = serde_json::from_str(&json)
            .map_err(|e| OSIRISError::SerializationError(e.to_string()))?;

        // Cache the state
        let mut cache = self.cache.write().await;
        cache.insert(workflow_id.to_string(), state.clone());

        Ok(state)
    }

    /// Get persisted state
    pub async fn get_persisted_state(&self, workflow_id: &str) -> Option<PersistedState> {
        match self.get_workflow_state(workflow_id).await {
            Ok(state) => Some(state),
            Err(_) => None,
        }
    }

    /// List all persisted states
    pub async fn list_states(&self) -> Result<Vec<PersistedState>> {
        let mut states = Vec::new();

        for entry in fs::read_dir(&self.storage_path)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Ok(json) = fs::read_to_string(&path) {
                    if let Ok(state) = serde_json::from_str::<PersistedState>(&json) {
                        states.push(state);
                    }
                }
            }
        }

        Ok(states)
    }

    /// Delete persisted state
    pub async fn delete_state(&self, workflow_id: &str) -> Result<()> {
        debug!("Deleting persisted state for workflow: {}", workflow_id);

        // Remove from cache
        let mut cache = self.cache.write().await;
        cache.remove(workflow_id);

        // Delete from disk
        let file_path = self.get_state_file_path(workflow_id);
        tokio::fs::remove_file(&file_path)
            .await
            .map_err(|e| OSIRISError::Unknown(e.to_string()))?;

        info!("Deleted persisted state: {}", workflow_id);
        Ok(())
    }

    /// Cleanup old states
    pub async fn cleanup_old_states(&self, max_age: Duration) -> Result<usize> {
        debug!("Cleaning up states older than {:?}", max_age);

        let mut deleted_count = 0;
        let cutoff = Instant::now() - max_age;

        // List all states
        let all_states = self.list_states().await?;

        for state in all_states {
            if state.timestamp < cutoff {
                self.delete_state(&state.workflow_id).await?;
                deleted_count += 1;
            }
        }

        info!("Cleaned up {} old states", deleted_count);
        Ok(deleted_count)
    }

    /// Get storage statistics
    pub async fn get_stats(&self) -> Value {
        let states = match self.list_states().await {
            Ok(states) => states,
            Err(_) => Vec::new(),
        };

        let cache_size = self.cache.read().await.len();

        let total_size: u64 = states.iter()
            .map(|state| {
                let file_path = self.get_state_file_path(&state.workflow_id);
                match std::fs::metadata(&file_path) {
                    Ok(metadata) => metadata.len(),
                    Err(_) => 0,
                }
            })
            .sum();

        json!({
            "storage_path": self.storage_path,
            "total_states": states.len(),
            "cache_size": cache_size,
            "total_bytes": total_size,
            "oldest_state": states.iter()
                .map(|s| s.timestamp.elapsed().as_secs())
                .min()
                .unwrap_or(0),
            "newest_state": states.iter()
                .map(|s| s.timestamp.elapsed().as_secs())
                .max()
                .unwrap_or(0),
        })
    }

    /// Convert workflow to value for persistence
    fn workflow_to_value(&self, workflow: &Workflow) -> Value {
        json!({
            "id": workflow.id,
            "name": workflow.name,
            "description": workflow.description,
            "status": format!("{:?}", workflow.status),
            "priority": format!("{:?}", workflow.priority),
            "created_at": workflow.created_at.elapsed().as_secs(),
            "started_at": workflow.started_at.map(|t| t.elapsed().as_secs()),
            "completed_at": workflow.completed_at.map(|t| t.elapsed().as_secs()),
            "execution_times": workflow.execution_times.iter().map(|t| t.as_millis()).collect::<Vec<_>>(),
            "error_rate": workflow.error_rate,
            "data": workflow.data,
            "metadata": workflow.metadata,
            "steps_count": workflow.steps.len(),
            "verification_passed": workflow.verification_passed,
            "retry_count": workflow.retry_count,
            "max_retries": workflow.max_retries,
        })
    }

    /// Get file path for a workflow state
    fn get_state_file_path(&self, workflow_id: &str) -> String {
        let safe_id = workflow_id.replace('/', "_").replace('\\', "_");
        format!("{}/{}.json", self.storage_path, safe_id)
    }
}

/// State manager for OSIRIS
pub struct StateManager {
    persistence: Arc<PersistenceService>,
}

impl StateManager {
    pub fn new() -> Self {
        Self {
            persistence: Arc::new(PersistenceService::new()),
        }
    }

    pub fn with_persistence(persistence: Arc<PersistenceService>) -> Self {
        Self { persistence }
    }

    /// Persist state
    pub async fn persist_state(&self, state: PersistedState) -> Result<()> {
        self.persistence.persist_state(&state).await
    }

    /// Get state
    pub async fn get_state(&self, workflow_id: &str) -> Result<PersistedState> {
        self.persistence.get_workflow_state(workflow_id).await
    }

    /// Initialize state for a new workflow
    pub async fn initialize_workflow_state(&self, workflow_id: &str) -> Result<PersistedState> {
        let state = PersistedState {
            workflow_id: workflow_id.to_string(),
            data: json!({
                "initialized_at": chrono::Utc::now().to_rfc3339(),
                "status": "initialized"
            }),
            timestamp: Instant::now(),
            version: "1.0".to_string(),
        };

        self.persist_state(state.clone()).await?;
        Ok(state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_persistence_service() {
        let service = PersistenceService::with_path(".test_persistence");

        let state = PersistedState {
            workflow_id: "test-workflow".to_string(),
            data: json!({"test": "data"}),
            timestamp: Instant::now(),
            version: "1.0".to_string(),
        };

        let result = service.persist_state(&state).await;
        assert!(result.is_ok());

        let retrieved = service.get_workflow_state("test-workflow").await;
        assert!(retrieved.is_ok());

        // Cleanup
        let _ = service.delete_state("test-workflow").await;
        let _ = fs::remove_dir_all(".test_persistence");
    }

    #[tokio::test]
    async fn test_state_manager() {
        let manager = StateManager::new();

        let state = manager.initialize_workflow_state("test-manager").await;
        assert!(state.is_ok());

        let retrieved = manager.get_state("test-manager").await;
        assert!(retrieved.is_ok());
    }
}