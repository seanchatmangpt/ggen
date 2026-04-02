//! Cache invalidation patterns for keeping caches consistent across application state changes.
//!
//! This module provides:
//! - Time-based invalidation (TTL management)
//! - Event-based invalidation (when data changes, invalidate cache)
//! - Dependency tracking (cache A depends on cache B)
//! - Cascading invalidation (invalidate dependents recursively)
//! - Broadcast invalidation (notify all instances to clear cache)
//! - Selective invalidation (delete only affected keys matching patterns)

use anyhow::{anyhow, Result};
use dashmap::DashMap;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tracing::{debug, info, warn};

/// Invalidation event type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InvalidationEvent {
    /// Time-based expiration
    Expiration(String),
    /// Data changed event
    DataChanged(String),
    /// Custom event
    Custom(String),
}

/// Cache dependency tracker
/// Tracks which cache entries depend on others
#[derive(Clone)]
pub struct DependencyGraph {
    /// Map from key to its dependents (other keys that depend on it)
    dependents: Arc<DashMap<String, Vec<String>>>,
    /// Map from key to its dependencies (keys it depends on)
    dependencies: Arc<DashMap<String, Vec<String>>>,
}

impl DependencyGraph {
    /// Create a new dependency graph
    pub fn new() -> Self {
        Self {
            dependents: Arc::new(DashMap::new()),
            dependencies: Arc::new(DashMap::new()),
        }
    }

    /// Add a dependency relationship: dependent depends on dependency
    pub fn add_dependency(&self, dependent: &str, dependency: &str) {
        // Add to dependencies map
        self.dependencies
            .entry(dependent.to_string())
            .or_insert_with(Vec::new)
            .push(dependency.to_string());

        // Add to dependents map
        self.dependents
            .entry(dependency.to_string())
            .or_insert_with(Vec::new)
            .push(dependent.to_string());

        debug!(
            "Added dependency: {} depends on {}",
            dependent, dependency
        );
    }

    /// Remove a dependency relationship
    pub fn remove_dependency(&self, dependent: &str, dependency: &str) {
        if let Some(mut deps) = self.dependencies.get_mut(dependent) {
            deps.retain(|d| d != dependency);
        }

        if let Some(mut depts) = self.dependents.get_mut(dependency) {
            depts.retain(|d| d != dependent);
        }
    }

    /// Get all keys that depend on the given key
    pub fn get_dependents(&self, key: &str) -> Vec<String> {
        self.dependents
            .get(key)
            .map(|entry| entry.clone())
            .unwrap_or_default()
    }

    /// Get all keys that the given key depends on
    pub fn get_dependencies(&self, key: &str) -> Vec<String> {
        self.dependencies
            .get(key)
            .map(|entry| entry.clone())
            .unwrap_or_default()
    }

    /// Get all keys that need to be invalidated when the given key changes
    /// This includes direct dependents and transitive dependents (cascading)
    pub fn get_cascade_targets(&self, key: &str) -> Vec<String> {
        let mut targets = Vec::new();
        let mut queue = vec![key.to_string()];
        let mut visited = std::collections::HashSet::new();

        while let Some(current) = queue.pop() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.clone());

            let dependents = self.get_dependents(&current);
            targets.extend(dependents.iter().cloned());
            queue.extend(dependents);
        }

        targets
    }

    /// Clear all dependency information
    pub fn clear(&self) {
        self.dependents.clear();
        self.dependencies.clear();
    }
}

impl Default for DependencyGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Invalidation strategy
#[derive(Debug, Clone)]
pub enum InvalidationStrategy {
    /// Lazy invalidation: mark as invalid but don't delete until accessed
    Lazy,
    /// Eager invalidation: delete immediately
    Eager,
    /// Event-based: invalidate based on specific events
    EventBased,
    /// TTL-based: invalidate after time expires
    TTL(Duration),
}

/// Cache invalidation manager
pub struct InvalidationManager {
    /// Dependency graph for tracking cache relationships
    dependencies: DependencyGraph,
    /// Map of keys to their last invalidation time
    invalidation_times: Arc<DashMap<String, SystemTime>>,
    /// Map of keys to their invalidation strategies
    strategies: Arc<DashMap<String, InvalidationStrategy>>,
    /// Callback function called on invalidation
    on_invalidate: Arc<tokio::sync::Mutex<Option<Box<dyn Fn(String) + Send + Sync>>>>,
}

impl InvalidationManager {
    /// Create a new invalidation manager
    pub fn new() -> Self {
        Self {
            dependencies: DependencyGraph::new(),
            invalidation_times: Arc::new(DashMap::new()),
            strategies: Arc::new(DashMap::new()),
            on_invalidate: Arc::new(tokio::sync::Mutex::new(None)),
        }
    }

    /// Register a key with a specific invalidation strategy
    pub fn register(&self, key: &str, strategy: InvalidationStrategy) {
        self.strategies.insert(key.to_string(), strategy);
        debug!("Registered invalidation strategy for key: {}", key);
    }

    /// Add a dependency relationship
    pub fn add_dependency(&self, dependent: &str, dependency: &str) {
        self.dependencies.add_dependency(dependent, dependency);
    }

    /// Invalidate a key due to data change
    /// Returns list of all invalidated keys (including cascading)
    pub fn invalidate(&self, key: &str) -> Vec<String> {
        let mut invalidated = vec![key.to_string()];

        // Get all cascading targets
        let cascade_targets = self.dependencies.get_cascade_targets(key);
        invalidated.extend(cascade_targets);

        // Remove duplicates
        invalidated.sort();
        invalidated.dedup();

        // Record invalidation time
        for k in &invalidated {
            self.invalidation_times.insert(k.clone(), SystemTime::now());
        }

        debug!("Invalidated {} keys (including cascading)", invalidated.len());
        invalidated
    }

    /// Check if a key is still valid
    pub fn is_valid(&self, key: &str) -> bool {
        // Check if key has a TTL strategy
        if let Some(strategy) = self.strategies.get(key) {
            if let InvalidationStrategy::TTL(duration) = strategy.value() {
                if let Some(invalidation_time) = self.invalidation_times.get(key) {
                    if let Ok(elapsed) = invalidation_time.elapsed() {
                        return elapsed < *duration;
                    }
                }
                return false;
            }
        }
        true
    }

    /// Get keys that have expired based on TTL
    pub fn get_expired_keys(&self) -> Vec<String> {
        let mut expired = Vec::new();

        for strategy_entry in self.strategies.iter() {
            let key = strategy_entry.key().clone();
            if let InvalidationStrategy::TTL(duration) = strategy_entry.value() {
                if let Some(inv_time) = self.invalidation_times.get(&key) {
                    if let Ok(elapsed) = inv_time.elapsed() {
                        if elapsed > *duration {
                            expired.push(key);
                        }
                    }
                }
            }
        }

        expired
    }

    /// Get cascading invalidation targets
    pub fn get_cascade_targets(&self, key: &str) -> Vec<String> {
        self.dependencies.get_cascade_targets(key)
    }

    /// Clear all invalidation state
    pub fn clear(&self) {
        self.invalidation_times.clear();
        self.strategies.clear();
        self.dependencies.clear();
    }

    /// Selective invalidation: invalidate all keys matching a pattern
    pub fn invalidate_pattern(&self, pattern: &str) -> Vec<String> {
        let regex = match regex::Regex::new(pattern) {
            Ok(r) => r,
            Err(e) => {
                warn!("Invalid invalidation pattern '{}': {}", pattern, e);
                return Vec::new();
            }
        };

        let mut invalidated = Vec::new();
        for strategy_entry in self.strategies.iter() {
            let key = strategy_entry.key();
            if regex.is_match(key) {
                invalidated.push(key.clone());
                self.invalidation_times
                    .insert(key.clone(), SystemTime::now());
            }
        }

        debug!("Invalidated {} keys matching pattern '{}'", invalidated.len(), pattern);
        invalidated
    }
}

impl Default for InvalidationManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Broadcast invalidation for distributed caching
/// Notifies all cache instances to invalidate a key
pub struct BroadcastInvalidator {
    /// List of subscribed listeners
    listeners: Arc<DashMap<String, Arc<tokio::sync::broadcast::Sender<String>>>>,
}

impl BroadcastInvalidator {
    /// Create a new broadcast invalidator
    pub fn new() -> Self {
        Self {
            listeners: Arc::new(DashMap::new()),
        }
    }

    /// Subscribe to invalidation events for a channel
    pub async fn subscribe(&self, channel: &str) -> tokio::sync::broadcast::Receiver<String> {
        let sender = self
            .listeners
            .entry(channel.to_string())
            .or_insert_with(|| {
                let (tx, _) = tokio::sync::broadcast::channel(1024);
                Arc::new(tx)
            })
            .clone();

        sender.subscribe()
    }

    /// Broadcast an invalidation event
    pub fn broadcast(&self, channel: &str, key: &str) -> Result<usize> {
        if let Some(sender) = self.listeners.get(channel) {
            let count = sender.receiver_count();
            sender
                .send(key.to_string())
                .map_err(|_| anyhow!("Failed to broadcast invalidation"))?;
            info!(
                "Broadcasted invalidation for key '{}' to {} listeners",
                key, count
            );
            Ok(count)
        } else {
            debug!("No listeners for channel '{}'", channel);
            Ok(0)
        }
    }

    /// Get number of listeners for a channel
    pub fn listener_count(&self, channel: &str) -> usize {
        self.listeners
            .get(channel)
            .map(|sender| sender.receiver_count())
            .unwrap_or(0)
    }

    /// Clear all listeners
    pub fn clear(&self) {
        self.listeners.clear();
    }
}

impl Default for BroadcastInvalidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Invalidation receipt tracking
/// Tracks when invalidations occur for auditing
#[derive(Debug, Clone)]
pub struct InvalidationReceipt {
    /// Key that was invalidated
    pub key: String,
    /// Reason for invalidation
    pub reason: String,
    /// Timestamp of invalidation
    pub timestamp: SystemTime,
    /// All keys invalidated (including cascading)
    pub affected_keys: Vec<String>,
}

/// Invalidation audit log
pub struct InvalidationAuditLog {
    receipts: Arc<DashMap<String, Vec<InvalidationReceipt>>>,
}

impl InvalidationAuditLog {
    /// Create a new audit log
    pub fn new() -> Self {
        Self {
            receipts: Arc::new(DashMap::new()),
        }
    }

    /// Record an invalidation
    pub fn record(&self, receipt: InvalidationReceipt) {
        self.receipts
            .entry(receipt.key.clone())
            .or_insert_with(Vec::new)
            .push(receipt);
    }

    /// Get invalidation history for a key
    pub fn get_history(&self, key: &str) -> Vec<InvalidationReceipt> {
        self.receipts
            .get(key)
            .map(|entry| entry.clone())
            .unwrap_or_default()
    }

    /// Get all recorded receipts
    pub fn get_all_receipts(&self) -> HashMap<String, Vec<InvalidationReceipt>> {
        let mut result = HashMap::new();
        for entry in self.receipts.iter() {
            result.insert(entry.key().clone(), entry.value().clone());
        }
        result
    }

    /// Clear all audit logs
    pub fn clear(&self) {
        self.receipts.clear();
    }
}

impl Default for InvalidationAuditLog {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dependency_graph_basic() {
        let graph = DependencyGraph::new();
        graph.add_dependency("page", "user_data");
        graph.add_dependency("page", "settings");

        let dependents = graph.get_dependents("user_data");
        assert_eq!(dependents, vec!["page"]);
    }

    #[test]
    fn test_cascade_invalidation() {
        let graph = DependencyGraph::new();
        graph.add_dependency("b", "a");
        graph.add_dependency("c", "b");

        let targets = graph.get_cascade_targets("a");
        assert!(targets.contains(&"b".to_string()));
        assert!(targets.contains(&"c".to_string()));
    }

    #[test]
    fn test_invalidation_manager() {
        let manager = InvalidationManager::new();
        manager.register("key1", InvalidationStrategy::Eager);
        manager.register("key2", InvalidationStrategy::Lazy);

        let invalidated = manager.invalidate("key1");
        assert!(invalidated.contains(&"key1".to_string()));
    }

    #[test]
    fn test_ttl_invalidation() {
        let manager = InvalidationManager::new();
        manager.register("key", InvalidationStrategy::TTL(Duration::from_secs(60)));

        // Key should be valid immediately after registration
        assert!(manager.is_valid("key"));
    }
}
