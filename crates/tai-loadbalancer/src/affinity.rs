//! Session affinity and sticky routing

use crate::service_registry::Endpoint;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

/// Affinity strategy for session stickiness
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AffinityStrategy {
    /// No affinity - each request is load balanced independently
    None,
    /// Source IP based affinity
    SourceIP,
    /// User ID based affinity
    UserId,
    /// Cookie based affinity
    Cookie,
    /// Custom key based affinity
    CustomKey,
}

/// Configuration for session affinity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AffinityConfig {
    /// Affinity strategy
    pub strategy: AffinityStrategy,
    /// TTL for affinity entries
    pub ttl: Duration,
    /// Maximum number of affinity entries to cache
    pub max_entries: usize,
}

impl Default for AffinityConfig {
    fn default() -> Self {
        Self {
            strategy: AffinityStrategy::None,
            ttl: Duration::from_secs(3600),
            max_entries: 100_000,
        }
    }
}

/// Affinity entry mapping a key to an endpoint
#[derive(Debug, Clone)]
struct AffinityEntry {
    endpoint: Endpoint,
    created_at: SystemTime,
    last_accessed: SystemTime,
    access_count: usize,
}

/// Manages session affinity for sticky routing
pub struct AffinityManager {
    config: AffinityConfig,
    // Map of service → key → affinity entry
    affinity_cache: Arc<DashMap<String, DashMap<String, AffinityEntry>>>,
    // Cleanup task handle
    cleanup_handle: Arc<parking_lot::Mutex<Option<tokio::task::JoinHandle<()>>>>,
}

impl AffinityManager {
    /// Create a new affinity manager
    pub fn new(config: AffinityConfig) -> Self {
        let mgr = Self {
            config,
            affinity_cache: Arc::new(DashMap::new()),
            cleanup_handle: Arc::new(parking_lot::Mutex::new(None)),
        };

        // Start cleanup task for expired entries
        if config.strategy != AffinityStrategy::None {
            let cache = mgr.affinity_cache.clone();
            let ttl = config.ttl;
            let handle = tokio::spawn(async move {
                let mut interval = tokio::time::interval(Duration::from_secs(60));
                loop {
                    interval.tick().await;

                    // Remove expired entries
                    for mut service_entry in cache.iter_mut() {
                        let service_cache = service_entry.value_mut();
                        service_cache.retain(|_, entry| {
                            entry
                                .last_accessed
                                .elapsed()
                                .map(|elapsed| elapsed < ttl)
                                .unwrap_or(false)
                        });
                    }
                }
            });

            *mgr.cleanup_handle.lock() = Some(handle);
        }

        mgr
    }

    /// Get affinity endpoint for a key
    pub async fn get_affinity(&self, service: &str, key: &str) -> Option<Endpoint> {
        if self.config.strategy == AffinityStrategy::None {
            return None;
        }

        if let Some(mut entry) = self
            .affinity_cache
            .get_mut(service)
            .and_then(|s| s.get_mut(key))
        {
            // Check if entry is expired
            if entry
                .last_accessed
                .elapsed()
                .map(|elapsed| elapsed < self.config.ttl)
                .unwrap_or(false)
            {
                entry.last_accessed = SystemTime::now();
                entry.access_count += 1;
                return Some(entry.endpoint.clone());
            } else {
                // Entry expired, remove it
                drop(entry);
                if let Some(s) = self.affinity_cache.get_mut(service) {
                    s.remove(key);
                }
            }
        }

        None
    }

    /// Set affinity endpoint for a key
    pub async fn set_affinity(&self, service: &str, key: &str, endpoint: Endpoint) {
        if self.config.strategy == AffinityStrategy::None {
            return;
        }

        let now = SystemTime::now();
        let service_cache = self
            .affinity_cache
            .entry(service.to_string())
            .or_insert_with(DashMap::new);

        // Check cache size and evict if needed
        if service_cache.len() >= self.config.max_entries {
            // Simple eviction: remove oldest accessed entry
            if let Some((k, _)) = service_cache
                .iter()
                .min_by_key(|entry| entry.value().last_accessed)
                .map(|e| (e.key().clone(), e.value().clone()))
            {
                service_cache.remove(&k);
            }
        }

        service_cache.insert(
            key.to_string(),
            AffinityEntry {
                endpoint,
                created_at: now,
                last_accessed: now,
                access_count: 0,
            },
        );
    }

    /// Remove affinity for a key
    pub async fn remove_affinity(&self, service: &str, key: &str) {
        if let Some(mut service_cache) = self.affinity_cache.get_mut(service) {
            service_cache.remove(key);
        }
    }

    /// Clear all affinity entries for a service
    pub async fn clear_service_affinity(&self, service: &str) {
        self.affinity_cache.remove(service);
    }

    /// Clear all affinity entries
    pub async fn clear_all(&self) {
        self.affinity_cache.clear();
    }

    /// Get affinity cache statistics
    pub async fn get_stats(&self) -> AffinityCacheStats {
        let mut total_entries = 0;
        let mut total_services = self.affinity_cache.len();

        for entry in self.affinity_cache.iter() {
            total_entries += entry.value().len();
        }

        AffinityCacheStats {
            services: total_services,
            entries: total_entries,
            strategy: self.config.strategy,
        }
    }
}

impl Drop for AffinityManager {
    fn drop(&mut self) {
        if let Some(handle) = self.cleanup_handle.lock().take() {
            handle.abort();
        }
    }
}

/// Statistics for affinity cache
#[derive(Debug, Clone)]
pub struct AffinityCacheStats {
    /// Number of services tracked
    pub services: usize,
    /// Total affinity entries
    pub entries: usize,
    /// Current affinity strategy
    pub strategy: AffinityStrategy,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_affinity_set_and_get() {
        let config = AffinityConfig {
            strategy: AffinityStrategy::CustomKey,
            ..Default::default()
        };

        let manager = AffinityManager::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        manager
            .set_affinity("test-service", "user-123", endpoint.clone())
            .await;

        let retrieved = manager.get_affinity("test-service", "user-123").await;
        assert_eq!(retrieved, Some(endpoint));
    }

    #[tokio::test]
    async fn test_no_affinity_when_disabled() {
        let config = AffinityConfig {
            strategy: AffinityStrategy::None,
            ..Default::default()
        };

        let manager = AffinityManager::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        manager
            .set_affinity("test-service", "user-123", endpoint.clone())
            .await;

        let retrieved = manager.get_affinity("test-service", "user-123").await;
        assert!(retrieved.is_none());
    }

    #[tokio::test]
    async fn test_affinity_expiration() {
        let config = AffinityConfig {
            strategy: AffinityStrategy::CustomKey,
            ttl: Duration::from_millis(100),
            max_entries: 1000,
        };

        let manager = AffinityManager::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        manager
            .set_affinity("test-service", "user-123", endpoint.clone())
            .await;

        // Should exist immediately
        assert!(manager.get_affinity("test-service", "user-123").await.is_some());

        // Wait for expiration
        tokio::time::sleep(Duration::from_millis(150)).await;

        // Should be expired now
        let retrieved = manager.get_affinity("test-service", "user-123").await;
        assert!(retrieved.is_none());
    }

    #[tokio::test]
    async fn test_remove_affinity() {
        let config = AffinityConfig {
            strategy: AffinityStrategy::CustomKey,
            ..Default::default()
        };

        let manager = AffinityManager::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        manager
            .set_affinity("test-service", "user-123", endpoint.clone())
            .await;

        manager.remove_affinity("test-service", "user-123").await;

        let retrieved = manager.get_affinity("test-service", "user-123").await;
        assert!(retrieved.is_none());
    }

    #[tokio::test]
    async fn test_affinity_cache_stats() {
        let config = AffinityConfig {
            strategy: AffinityStrategy::CustomKey,
            ..Default::default()
        };

        let manager = AffinityManager::new(config);
        let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

        manager
            .set_affinity("service-1", "user-1", endpoint.clone())
            .await;
        manager
            .set_affinity("service-1", "user-2", endpoint.clone())
            .await;
        manager
            .set_affinity("service-2", "user-3", endpoint.clone())
            .await;

        let stats = manager.get_stats().await;
        assert_eq!(stats.services, 2);
        assert_eq!(stats.entries, 3);
    }
}
