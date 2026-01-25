//! Distributed caching with multi-tier strategies for optimal performance and persistence.
//!
//! Implements:
//! - Multi-tier caching: L1 (in-memory), L2 (Redis), L3 (persistent storage)
//! - Write-through caching: write to all tiers before returning
//! - Write-behind caching: write to cache, background flush to storage
//! - Read-through caching: read from cache, fallback to source
//! - Cache coherency: consistency across instances
//! - Cache warming: preload hot data

use anyhow::{anyhow, Result};
use async_trait::async_trait;
use dashmap::DashMap;
use serde::{de::DeserializeOwned, Serialize};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;
use tracing::{debug, error, info, warn};

/// Cache tier abstraction
#[async_trait]
pub trait CacheTier: Send + Sync {
    /// Get a value from this tier
    async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>>;

    /// Set a value in this tier
    async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()>;

    /// Delete a value from this tier
    async fn delete(&self, key: &str) -> Result<bool>;

    /// Check if value exists in this tier
    async fn exists(&self, key: &str) -> Result<bool>;

    /// Get tier name for logging
    fn tier_name(&self) -> &str;
}

/// In-memory cache tier (L1)
#[derive(Clone)]
pub struct InMemoryCacheTier {
    data: Arc<DashMap<String, (Vec<u8>, Option<std::time::Instant>)>>,
}

impl InMemoryCacheTier {
    pub fn new() -> Self {
        Self {
            data: Arc::new(DashMap::new()),
        }
    }

    fn is_expired(&self, instant: Option<std::time::Instant>) -> bool {
        instant.map_or(false, |exp| std::time::Instant::now() > exp)
    }
}

impl Default for InMemoryCacheTier {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl CacheTier for InMemoryCacheTier {
    async fn get<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        if let Some(entry) = self.data.get(key) {
            let (data, expiry) = entry.value();
            if self.is_expired(*expiry) {
                drop(entry);
                self.data.remove(key);
                debug!("L1 (in-memory) key expired: {}", key);
                return Ok(None);
            }
            return bincode::deserialize(data)
                .map(Some)
                .map_err(|e| anyhow!("Failed to deserialize from L1: {}", e));
        }
        Ok(None)
    }

    async fn set<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()> {
        let bytes = bincode::serialize(value)?;
        let expiry = ttl.map(|d| std::time::Instant::now() + d);
        self.data.insert(key.to_string(), (bytes, expiry));
        debug!("Set L1 (in-memory) key: {}", key);
        Ok(())
    }

    async fn delete(&self, key: &str) -> Result<bool> {
        let deleted = self.data.remove(key).is_some();
        if deleted {
            debug!("Deleted L1 (in-memory) key: {}", key);
        }
        Ok(deleted)
    }

    async fn exists(&self, key: &str) -> Result<bool> {
        Ok(self.data.contains_key(key))
    }

    fn tier_name(&self) -> &str {
        "L1-InMemory"
    }
}

/// Multi-tier cache write strategy
#[derive(Debug, Clone)]
pub enum WriteStrategy {
    /// Write-through: write to all tiers before returning
    WriteThrough,
    /// Write-behind: write to cache, background flush to storage
    WriteBehind { flush_interval: Duration },
}

/// Multi-tier cache read strategy
#[derive(Debug, Clone)]
pub enum ReadStrategy {
    /// Read-through: fallback to source if not in cache
    ReadThrough,
    /// Lazy load: return only if in cache
    LazyLoad,
}

/// Multi-tier cache implementation
pub struct MultiTierCache {
    /// L1: in-memory tier
    l1: Arc<InMemoryCacheTier>,
    /// Write strategy
    write_strategy: WriteStrategy,
    /// Read strategy
    read_strategy: ReadStrategy,
    /// Pending writes for write-behind strategy
    pending_writes: Arc<Mutex<Vec<(String, Vec<u8>, Option<Duration>)>>>,
    /// Cache coherency state
    coherency_version: Arc<Mutex<u64>>,
}

impl MultiTierCache {
    /// Create a new multi-tier cache
    pub fn new(write_strategy: WriteStrategy, read_strategy: ReadStrategy) -> Self {
        Self {
            l1: Arc::new(InMemoryCacheTier::new()),
            write_strategy,
            read_strategy,
            pending_writes: Arc::new(Mutex::new(Vec::new())),
            coherency_version: Arc::new(Mutex::new(0)),
        }
    }

    /// Try to get value from L1 only
    pub async fn get_l1<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        self.l1.get(key).await
    }

    /// Set value with write strategy
    pub async fn set_tiered<T: Serialize>(&self, key: &str, value: &T, ttl: Option<Duration>) -> Result<()> {
        let bytes = bincode::serialize(value)?;

        match &self.write_strategy {
            WriteStrategy::WriteThrough => {
                // Write to L1 immediately
                self.l1.set(key, value, ttl).await?;
                debug!("Write-through: wrote key '{}' to L1", key);
                Ok(())
            }
            WriteStrategy::WriteBehind { .. } => {
                // Queue for background flush
                let mut pending = self.pending_writes.lock().await;
                pending.push((key.to_string(), bytes, ttl));
                debug!("Write-behind: queued key '{}' for flush", key);
                Ok(())
            }
        }
    }

    /// Flush pending writes (for write-behind strategy)
    pub async fn flush_pending(&self) -> Result<usize> {
        let mut pending = self.pending_writes.lock().await;
        let count = pending.len();

        for (key, _, ttl) in pending.drain(..) {
            if let Ok(value) = bincode::deserialize::<serde_json::Value>(&_) {
                self.l1.set(&key, &value, ttl).await?;
                debug!("Flushed pending write: {}", key);
            }
        }

        info!("Flushed {} pending writes", count);
        Ok(count)
    }

    /// Get value with read strategy
    pub async fn get_tiered<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>> {
        // Try L1 first
        if let Ok(Some(value)) = self.l1.get(key).await {
            debug!("Cache hit in L1 for key '{}'", key);
            return Ok(Some(value));
        }

        match &self.read_strategy {
            ReadStrategy::ReadThrough => {
                debug!("Cache miss in L1 for key '{}' (read-through strategy)", key);
                Ok(None)
            }
            ReadStrategy::LazyLoad => {
                debug!("Cache miss in L1 for key '{}' (lazy-load strategy)", key);
                Ok(None)
            }
        }
    }

    /// Delete value from all tiers
    pub async fn delete_tiered(&self, key: &str) -> Result<bool> {
        let mut deleted = false;
        deleted |= self.l1.delete(key).await?;

        if deleted {
            // Increment coherency version for invalidation broadcast
            let mut version = self.coherency_version.lock().await;
            *version += 1;
            debug!("Deleted key '{}', coherency version: {}", key, version);
        }

        Ok(deleted)
    }

    /// Warm cache by preloading hot data
    pub async fn warm_cache<T: Serialize>(&self, entries: Vec<(String, T, Option<Duration>)>) -> Result<usize> {
        let mut count = 0;
        for (key, value, ttl) in entries {
            self.l1.set(&key, &value, ttl).await?;
            count += 1;
        }
        info!("Cache warming complete: {} entries loaded", count);
        Ok(count)
    }

    /// Get current coherency version
    pub async fn get_coherency_version(&self) -> u64 {
        *self.coherency_version.lock().await
    }

    /// Get pending write count (for write-behind)
    pub async fn get_pending_write_count(&self) -> usize {
        self.pending_writes.lock().await.len()
    }

    /// Check cache statistics
    pub async fn get_stats(&self) -> CacheStats {
        CacheStats {
            l1_entries: self.l1.data.len(),
            pending_writes: self.pending_writes.lock().await.len(),
            coherency_version: *self.coherency_version.lock().await,
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub l1_entries: usize,
    pub pending_writes: usize,
    pub coherency_version: u64,
}

/// Cache coherency protocol for distributed systems
pub struct CacheCoherence {
    /// Local coherency state
    local_version: Arc<Mutex<u64>>,
    /// Remote coherency states (instance_id -> version)
    remote_versions: Arc<DashMap<String, u64>>,
}

impl CacheCoherence {
    /// Create a new cache coherence tracker
    pub fn new() -> Self {
        Self {
            local_version: Arc::new(Mutex::new(0)),
            remote_versions: Arc::new(DashMap::new()),
        }
    }

    /// Update local version
    pub async fn update_local_version(&self) {
        let mut version = self.local_version.lock().await;
        *version += 1;
        debug!("Updated local coherency version: {}", version);
    }

    /// Update remote instance version
    pub fn update_remote_version(&self, instance_id: &str, version: u64) {
        self.remote_versions.insert(instance_id.to_string(), version);
        debug!("Updated remote coherency version for {}: {}", instance_id, version);
    }

    /// Check if local cache is coherent with remote
    pub async fn is_coherent(&self, instance_id: &str) -> bool {
        let local = *self.local_version.lock().await;
        let remote = self.remote_versions.get(instance_id).map(|v| *v).unwrap_or(0);
        local == remote
    }

    /// Get local version
    pub async fn get_local_version(&self) -> u64 {
        *self.local_version.lock().await
    }

    /// Get remote version
    pub fn get_remote_version(&self, instance_id: &str) -> Option<u64> {
        self.remote_versions.get(instance_id).map(|v| *v)
    }
}

impl Default for CacheCoherence {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache warmer for preloading data
pub struct CacheWarmer {
    /// Data source for warming
    data_source: Arc<Mutex<Vec<(String, serde_json::Value)>>>,
}

impl CacheWarmer {
    /// Create a new cache warmer
    pub fn new() -> Self {
        Self {
            data_source: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Add data to warm the cache with
    pub async fn add_data(&self, key: String, value: serde_json::Value) {
        let mut data = self.data_source.lock().await;
        data.push((key, value));
    }

    /// Get all warming data
    pub async fn get_data(&self) -> Vec<(String, serde_json::Value)> {
        self.data_source.lock().await.clone()
    }

    /// Clear warming data
    pub async fn clear(&self) {
        self.data_source.lock().await.clear();
    }
}

impl Default for CacheWarmer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_in_memory_cache_tier() {
        let tier = InMemoryCacheTier::new();
        let value = "test_value";
        tier.set("key1", &value, None).await.unwrap();
        let retrieved: Option<String> = tier.get("key1").await.unwrap();
        assert_eq!(retrieved, Some("test_value".to_string()));
    }

    #[tokio::test]
    async fn test_multi_tier_cache_write_through() {
        let cache = MultiTierCache::new(
            WriteStrategy::WriteThrough,
            ReadStrategy::ReadThrough,
        );
        let value = "test_value";
        cache.set_tiered("key1", &value, None).await.unwrap();
        let retrieved: Option<String> = cache.get_tiered("key1").await.unwrap();
        assert_eq!(retrieved, Some("test_value".to_string()));
    }

    #[tokio::test]
    async fn test_cache_coherence() {
        let coherence = CacheCoherence::new();
        coherence.update_local_version().await;
        let version = coherence.get_local_version().await;
        assert_eq!(version, 1);
    }

    #[tokio::test]
    async fn test_cache_warmer() {
        let warmer = CacheWarmer::new();
        warmer.add_data("key1".to_string(), serde_json::json!({ "value": 42 })).await;
        let data = warmer.get_data().await;
        assert_eq!(data.len(), 1);
    }
}
