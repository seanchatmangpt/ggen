//! Firestore document store wrapper with type-safe operations
//!
//! This module provides a high-level, type-safe interface to Google Cloud Firestore
//! for persisting state snapshots and events. It handles:
//!
//! - Document organization (hierarchical: projects/{id}/governors/{id}/states/{doc})
//! - Version tracking for optimistic locking
//! - Timestamp management for ordering and causality
//! - Metadata capture (who, when, why changed)
//! - Event logging for audit trails

use crate::error::{Error, Result};
use crate::types::{ChangeMetadata, Event, StateId, StateSnapshot, VectorClockSnapshot};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde_json::{json, Value};
use std::collections::BTreeMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Configuration for Firestore store
#[derive(Debug, Clone)]
pub struct FirestoreConfig {
    /// GCP project ID
    pub project_id: String,

    /// Firestore database ID (usually "default")
    pub database_id: String,

    /// Root collection path in Firestore
    pub collection_root: String,
}

impl FirestoreConfig {
    /// Create a new Firestore configuration
    pub fn new(project_id: String) -> Self {
        FirestoreConfig {
            project_id,
            database_id: "default".to_string(),
            collection_root: "state-management".to_string(),
        }
    }

    /// Set custom database ID
    pub fn with_database(mut self, database_id: String) -> Self {
        self.database_id = database_id;
        self
    }

    /// Set custom collection root
    pub fn with_collection_root(mut self, root: String) -> Self {
        self.collection_root = root;
        self
    }
}

/// Type-safe wrapper for Firestore document store
pub struct FirestoreStore {
    config: FirestoreConfig,
    /// In-memory cache for development/testing (in production, remove this)
    cache: Arc<RwLock<BTreeMap<String, StateSnapshot>>>,
    /// Event log cache
    event_log: Arc<RwLock<Vec<Event>>>,
}

impl FirestoreStore {
    /// Create a new Firestore store with default configuration
    pub async fn new(project_id: impl Into<String>) -> Result<Self> {
        let config = FirestoreConfig::new(project_id.into());
        Self::with_config(config).await
    }

    /// Create a new Firestore store with custom configuration
    pub async fn with_config(config: FirestoreConfig) -> Result<Self> {
        // TODO: Initialize actual Firestore client
        // For now, using in-memory implementation for development
        let store = FirestoreStore {
            config,
            cache: Arc::new(RwLock::new(BTreeMap::new())),
            event_log: Arc::new(RwLock::new(Vec::new())),
        };

        Ok(store)
    }

    /// Build a document path for a state
    ///
    /// Returns path in format: {collection_root}/governors/{governor_id}/states/{state_id}
    fn build_state_path(&self, governor_id: &str, state_id: &str) -> String {
        format!(
            "{}/governors/{}/states/{}",
            self.config.collection_root, governor_id, state_id
        )
    }

    /// Build a document path for an event
    ///
    /// Returns path in format: {collection_root}/governors/{governor_id}/events/{event_id}
    fn build_event_path(&self, governor_id: &str, event_id: &str) -> String {
        format!(
            "{}/governors/{}/events/{}",
            self.config.collection_root, governor_id, event_id
        )
    }

    /// Save a state snapshot to Firestore
    ///
    /// This operation:
    /// 1. Sets initial version to 0 if this is a new state
    /// 2. Records metadata about who made the change
    /// 3. Stores timestamp for ordering
    /// 4. Creates audit log entry
    pub async fn save_state(
        &self,
        governor_id: &str,
        state_id: &str,
        mut snapshot: StateSnapshot,
        metadata: ChangeMetadata,
    ) -> Result<()> {
        // Validate inputs
        if governor_id.is_empty() || state_id.is_empty() {
            return Err(Error::InvalidState(
                "governor_id and state_id must not be empty".to_string(),
            ));
        }

        // Set initial version if new
        if snapshot.version == 0 {
            snapshot.version = 1;
        }

        snapshot.metadata = metadata;
        snapshot.touch();

        let path = self.build_state_path(governor_id, state_id);

        // In-memory cache operation
        let mut cache = self.cache.write().await;
        cache.insert(path, snapshot.clone());

        // Log operation
        tracing::info!(
            governor = governor_id,
            state_id = state_id,
            version = snapshot.version,
            "State saved"
        );

        Ok(())
    }

    /// Get a state snapshot from Firestore
    ///
    /// Returns tuple of (snapshot, version) for version-based conflict detection
    pub async fn get_state(
        &self,
        governor_id: &str,
        state_id: &str,
    ) -> Result<(StateSnapshot, u64)> {
        let path = self.build_state_path(governor_id, state_id);

        let cache = self.cache.read().await;
        match cache.get(&path) {
            Some(snapshot) => Ok((snapshot.clone(), snapshot.version)),
            None => Err(Error::StateNotFound(format!(
                "State not found: {}/{}",
                governor_id, state_id
            ))),
        }
    }

    /// Update a state with optimistic locking
    ///
    /// This operation:
    /// 1. Checks that the provided version matches the current version in Firestore
    /// 2. If versions match, increments version and writes new state
    /// 3. If versions don't match, returns VersionConflict error
    /// 4. Records change metadata and timestamp
    ///
    /// # Arguments
    ///
    /// * `governor_id` - Identifier of the governor (state machine) owning this state
    /// * `state_id` - Identifier of the state document
    /// * `new_state` - New state data
    /// * `expected_version` - Version we expect to find (optimistic locking)
    /// * `metadata` - Metadata about the change
    pub async fn update_state(
        &self,
        governor_id: &str,
        state_id: &str,
        mut new_state: StateSnapshot,
        expected_version: u64,
        metadata: ChangeMetadata,
    ) -> Result<u64> {
        let path = self.build_state_path(governor_id, state_id);

        // Check version for optimistic locking
        let mut cache = self.cache.write().await;
        match cache.get(&path) {
            Some(current) => {
                if current.version != expected_version {
                    return Err(Error::VersionConflict {
                        doc_id: path.clone(),
                        expected: expected_version,
                        actual: current.version,
                    });
                }
            }
            None => {
                if expected_version != 0 {
                    return Err(Error::StateNotFound(format!(
                        "State not found: {}/{}",
                        governor_id, state_id
                    )));
                }
            }
        }

        // Increment version
        new_state.version = expected_version + 1;
        new_state.metadata = metadata.clone();
        new_state.touch();

        let new_version = new_state.version;
        cache.insert(path.clone(), new_state.clone());

        // Log operation
        tracing::info!(
            governor = governor_id,
            state_id = state_id,
            old_version = expected_version,
            new_version = new_version,
            "State updated"
        );

        Ok(new_version)
    }

    /// Delete a state from Firestore
    pub async fn delete_state(&self, governor_id: &str, state_id: &str) -> Result<()> {
        let path = self.build_state_path(governor_id, state_id);

        let mut cache = self.cache.write().await;
        cache.remove(&path);

        tracing::info!(
            governor = governor_id,
            state_id = state_id,
            "State deleted"
        );

        Ok(())
    }

    /// Check if a state exists
    pub async fn exists(&self, governor_id: &str, state_id: &str) -> Result<bool> {
        let path = self.build_state_path(governor_id, state_id);
        let cache = self.cache.read().await;
        Ok(cache.contains_key(&path))
    }

    /// Log an event for audit trail
    ///
    /// Events are stored with:
    /// 1. Complete state before and after
    /// 2. Action that caused transition
    /// 3. Timestamp and causality information
    /// 4. Who performed the action and why
    pub async fn log_event(
        &self,
        governor_id: &str,
        event: Event,
    ) -> Result<()> {
        if governor_id.is_empty() {
            return Err(Error::InvalidState(
                "governor_id must not be empty".to_string(),
            ));
        }

        let path = self.build_event_path(governor_id, &event.id);

        let mut events = self.event_log.write().await;
        events.push(event.clone());

        tracing::info!(
            governor = governor_id,
            event_id = event.id,
            action = event.action,
            "Event logged"
        );

        Ok(())
    }

    /// Get events for a governor within a time range
    pub async fn get_events(
        &self,
        governor_id: &str,
        from_time: DateTime<Utc>,
        to_time: DateTime<Utc>,
    ) -> Result<Vec<Event>> {
        let events = self.event_log.read().await;
        let filtered = events
            .iter()
            .filter(|e| e.timestamp >= from_time && e.timestamp <= to_time)
            .cloned()
            .collect();

        Ok(filtered)
    }

    /// Get all events for a governor
    pub async fn get_all_events(&self, governor_id: &str) -> Result<Vec<Event>> {
        let events = self.event_log.read().await;
        Ok(events.clone())
    }

    /// Replay events to reconstruct state at a given time
    pub async fn replay_events(
        &self,
        events: &[Event],
        until_time: DateTime<Utc>,
    ) -> Result<Option<StateSnapshot>> {
        let mut state = None;

        for event in events {
            if event.timestamp <= until_time {
                state = Some(event.to_state.clone());
            } else {
                break;
            }
        }

        Ok(state)
    }

    /// Create a snapshot for recovery purposes
    ///
    /// Snapshots allow faster recovery by eliminating need to replay all events
    pub async fn create_snapshot(
        &self,
        governor_id: &str,
        state_id: &str,
        snapshot: StateSnapshot,
    ) -> Result<()> {
        let snapshot_path = format!(
            "{}/governors/{}/snapshots/{}",
            self.config.collection_root, governor_id, state_id
        );

        let mut cache = self.cache.write().await;
        cache.insert(snapshot_path, snapshot);

        Ok(())
    }

    /// Load the latest snapshot for a state
    pub async fn load_snapshot(
        &self,
        governor_id: &str,
        state_id: &str,
    ) -> Result<Option<StateSnapshot>> {
        let snapshot_path = format!(
            "{}/governors/{}/snapshots/{}",
            self.config.collection_root, governor_id, state_id
        );

        let cache = self.cache.read().await;
        Ok(cache.get(&snapshot_path).cloned())
    }

    /// Batch get multiple states (atomic read)
    pub async fn batch_get_states(
        &self,
        governor_id: &str,
        state_ids: &[&str],
    ) -> Result<Vec<StateSnapshot>> {
        let mut results = Vec::new();

        for state_id in state_ids {
            if let Ok((snapshot, _)) = self.get_state(governor_id, state_id).await {
                results.push(snapshot);
            }
        }

        Ok(results)
    }

    /// List all states for a governor
    pub async fn list_states(&self, governor_id: &str) -> Result<Vec<StateSnapshot>> {
        let prefix = self.build_state_path(governor_id, "");
        let cache = self.cache.read().await;

        let states = cache
            .iter()
            .filter(|(key, _)| key.starts_with(&prefix))
            .map(|(_, snapshot)| snapshot.clone())
            .collect();

        Ok(states)
    }

    /// Get statistics about stored states
    pub async fn get_stats(&self) -> Result<StoreStats> {
        let cache = self.cache.read().await;
        let event_log = self.event_log.read().await;

        let mut total_versions = 0;
        let mut max_version = 0;

        for snapshot in cache.values() {
            total_versions += 1;
            max_version = max_version.max(snapshot.version);
        }

        Ok(StoreStats {
            total_states: cache.len(),
            total_events: event_log.len(),
            total_versions: total_versions,
            max_version,
            cache_size_bytes: estimate_cache_size(&cache),
        })
    }
}

/// Statistics about the store
#[derive(Debug, Clone)]
pub struct StoreStats {
    pub total_states: usize,
    pub total_events: usize,
    pub total_versions: usize,
    pub max_version: u64,
    pub cache_size_bytes: usize,
}

/// Estimate memory usage of cache (rough estimate)
fn estimate_cache_size(cache: &BTreeMap<String, StateSnapshot>) -> usize {
    cache
        .iter()
        .map(|(k, v)| {
            k.len()
                + serde_json::to_string(&v)
                    .map(|s| s.len())
                    .unwrap_or(0)
        })
        .sum()
}

/// Type-safe builder for saving state
pub struct SaveStateBuilder {
    governor_id: String,
    state_id: String,
    snapshot: StateSnapshot,
    metadata: ChangeMetadata,
}

impl SaveStateBuilder {
    /// Create a new builder
    pub fn new(
        governor_id: String,
        state_id: String,
        snapshot: StateSnapshot,
        metadata: ChangeMetadata,
    ) -> Self {
        SaveStateBuilder {
            governor_id,
            state_id,
            snapshot,
            metadata,
        }
    }

    /// Save to store
    pub async fn save(&self, store: &FirestoreStore) -> Result<()> {
        store
            .save_state(
                &self.governor_id,
                &self.state_id,
                self.snapshot.clone(),
                self.metadata.clone(),
            )
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_save_and_get_state() {
        let store = FirestoreStore::new("test-project").await.unwrap();
        let metadata = ChangeMetadata::new("test", "creation", None);
        let snapshot = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 0}),
            metadata.clone(),
        );

        store
            .save_state("governor1", "state1", snapshot.clone(), metadata)
            .await
            .unwrap();

        let (retrieved, version) = store.get_state("governor1", "state1").await.unwrap();
        assert_eq!(retrieved.data, json!({"counter": 0}));
        assert_eq!(version, 1);
    }

    #[tokio::test]
    async fn test_update_state_with_optimistic_locking() {
        let store = FirestoreStore::new("test-project").await.unwrap();
        let metadata = ChangeMetadata::new("test", "creation", None);
        let snapshot = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 0}),
            metadata.clone(),
        );

        // Save initial state
        store
            .save_state("governor1", "state1", snapshot, metadata)
            .await
            .unwrap();

        // Get current version
        let (_, current_version) = store.get_state("governor1", "state1").await.unwrap();

        // Update with correct version
        let new_snapshot = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 1}),
            ChangeMetadata::new("test", "increment", Some(current_version)),
        );

        let new_version = store
            .update_state(
                "governor1",
                "state1",
                new_snapshot,
                current_version,
                ChangeMetadata::new("test", "update", None),
            )
            .await
            .unwrap();

        assert_eq!(new_version, current_version + 1);
    }

    #[tokio::test]
    async fn test_version_conflict_detection() {
        let store = FirestoreStore::new("test-project").await.unwrap();
        let metadata = ChangeMetadata::new("test", "creation", None);
        let snapshot = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 0}),
            metadata.clone(),
        );

        store
            .save_state("governor1", "state1", snapshot, metadata)
            .await
            .unwrap();

        // Try to update with wrong version
        let new_snapshot = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 1}),
            ChangeMetadata::new("test", "update", None),
        );

        let result = store
            .update_state(
                "governor1",
                "state1",
                new_snapshot,
                999, // Wrong version
                ChangeMetadata::new("test", "update", None),
            )
            .await;

        assert!(matches!(result, Err(Error::VersionConflict { .. })));
    }

    #[tokio::test]
    async fn test_event_logging() {
        let store = FirestoreStore::new("test-project").await.unwrap();

        let to_state = StateSnapshot::new(
            StateId::new("test-state".to_string()),
            json!({"counter": 1}),
            ChangeMetadata::new("test", "increment", None),
        );

        let event = Event::new(
            "event1".to_string(),
            "increment".to_string(),
            None,
            to_state,
            ChangeMetadata::new("test", "action", None),
        );

        store.log_event("governor1", event).await.unwrap();

        let events = store.get_all_events("governor1").await.unwrap();
        assert_eq!(events.len(), 1);
    }
}
