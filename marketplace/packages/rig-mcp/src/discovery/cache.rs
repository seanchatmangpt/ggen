//! Schema cache with TTL support for MCP tool discovery
//!
//! Provides thread-safe caching for tool schemas with configurable TTL,
//! supporting both in-memory and persistent storage options.

use crate::discovery::Result;
use crate::discovery::{DiscoveryError, ToolSchema};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::RwLock;

/// Configuration for the schema cache
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Time-to-live for cache entries
    pub ttl: Duration,
    /// Maximum number of entries in the cache
    pub max_entries: usize,
    /// Whether to persist cache to disk
    pub persist: bool,
    /// Path for cache persistence
    pub persist_path: Option<PathBuf>,
    /// Whether to automatically cleanup expired entries
    pub auto_cleanup: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            ttl: Duration::from_secs(300), // 5 minutes
            max_entries: 1000,
            persist: false,
            persist_path: None,
            auto_cleanup: true,
        }
    }
}

impl CacheConfig {
    /// Create a new cache config with the specified TTL
    pub fn with_ttl(ttl: Duration) -> Self {
        Self {
            ttl,
            ..Default::default()
        }
    }

    /// Set the maximum number of entries
    pub fn with_max_entries(mut self, max: usize) -> Self {
        self.max_entries = max;
        self
    }

    /// Enable persistence to disk
    pub fn with_persistence(mut self, path: PathBuf) -> Self {
        self.persist = true;
        self.persist_path = Some(path);
        self
    }

    /// Disable automatic cleanup
    pub fn without_auto_cleanup(mut self) -> Self {
        self.auto_cleanup = false;
        self
    }
}

/// A single cache entry with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry {
    /// The tool schema
    pub schema: ToolSchema,
    /// When the entry was created (Unix timestamp)
    pub created_at: u64,
    /// When the entry was last accessed (Unix timestamp)
    pub last_accessed: u64,
    /// Access count
    pub access_count: u64,
}

impl CacheEntry {
    /// Create a new cache entry
    pub fn new(schema: ToolSchema) -> Self {
        let now = Self::now();
        Self {
            schema,
            created_at: now,
            last_accessed: now,
            access_count: 0,
        }
    }

    /// Check if this entry has expired
    pub fn is_expired(&self, ttl: Duration) -> bool {
        let now = Self::now();
        let age = Duration::from_secs(now.saturating_sub(self.created_at));
        age > ttl
    }

    /// Record an access
    pub fn record_access(&mut self) {
        self.last_accessed = Self::now();
        self.access_count += 1;
    }

    /// Get current time as Unix timestamp
    fn now() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0)
    }
}

/// Cache statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CacheStats {
    /// Total number of entries
    pub total_entries: usize,
    /// Number of expired entries
    pub expired_entries: usize,
    /// Total cache hits
    pub hits: u64,
    /// Total cache misses
    pub misses: u64,
    /// Hit rate (0.0 to 1.0)
    pub hit_rate: f64,
}

impl CacheStats {
    /// Calculate hit rate from hits and misses
    pub fn calculate_hit_rate(&mut self) {
        let total = self.hits + self.misses;
        self.hit_rate = if total > 0 {
            self.hits as f64 / total as f64
        } else {
            0.0
        };
    }
}

/// Thread-safe schema cache with TTL support
pub struct SchemaCache {
    /// Cache entries indexed by server URL and tool name
    entries: RwLock<HashMap<String, CacheEntry>>,
    /// Cache configuration
    config: CacheConfig,
    /// Cache statistics
    stats: RwLock<CacheStats>,
}

impl SchemaCache {
    /// Create a new schema cache
    pub fn new(config: CacheConfig) -> Self {
        Self {
            entries: RwLock::new(HashMap::new()),
            config,
            stats: RwLock::new(CacheStats::default()),
        }
    }

    /// Create a new cache with default configuration
    pub fn with_ttl(ttl: Duration) -> Self {
        Self::new(CacheConfig::with_ttl(ttl))
    }

    /// Get a cached schema by key
    pub async fn get(&self, key: &str) -> Result<Option<ToolSchema>> {
        // Auto-cleanup if enabled
        if self.config.auto_cleanup {
            self.cleanup_expired().await;
        }

        let mut entries = self.entries.write().await;
        let mut stats = self.stats.write().await;

        match entries.get(key) {
            Some(entry) if !entry.is_expired(self.config.ttl) => {
                // Update the entry to record access
                let schema = entry.schema.clone();
                let mut entry = entry.clone();
                entry.record_access();
                entries.insert(key.to_string(), entry);

                stats.hits += 1;
                stats.calculate_hit_rate();

                Ok(schema.into())
            }
            Some(_) => {
                // Entry exists but is expired
                stats.misses += 1;
                stats.calculate_hit_rate();
                entries.remove(key);
                Ok(None)
            }
            None => {
                stats.misses += 1;
                stats.calculate_hit_rate();
                Ok(None)
            }
        }
    }

    /// Insert a schema into the cache
    pub async fn insert(&self, key: String, schema: ToolSchema) -> Result<()> {
        let mut entries = self.entries.write().await;

        // Check if we're at capacity
        if entries.len() >= self.config.max_entries {
            // Remove the oldest entry or an expired one
            let oldest_key = entries
                .iter()
                .filter(|(_, e)| !e.is_expired(self.config.ttl))
                .min_by_key(|(_, e)| e.last_accessed)
                .map(|(k, _)| k.clone());

            if let Some(key_to_remove) = oldest_key {
                entries.remove(&key_to_remove);
            } else {
                // All entries are expired, just remove the first one
                let key_to_remove = entries.keys().next().cloned();
                if let Some(k) = key_to_remove {
                    entries.remove(&k);
                }
            }
        }

        let entry = CacheEntry::new(schema);
        entries.insert(key, entry);

        // Update stats
        let mut stats = self.stats.write().await;
        stats.total_entries = entries.len();
        stats.expired_entries = entries
            .values()
            .filter(|e| e.is_expired(self.config.ttl))
            .count();

        Ok(())
    }

    /// Remove an entry from the cache
    pub async fn remove(&self, key: &str) -> Result<bool> {
        let mut entries = self.entries.write().await;
        let removed = entries.remove(key).is_some();

        // Update stats
        let mut stats = self.stats.write().await;
        stats.total_entries = entries.len();

        Ok(removed)
    }

    /// Clear all entries from the cache
    pub async fn clear(&self) -> Result<()> {
        let mut entries = self.entries.write().await;
        entries.clear();

        // Reset stats
        let mut stats = self.stats.write().await;
        *stats = CacheStats::default();

        Ok(())
    }

    /// Clean up expired entries
    pub async fn cleanup_expired(&self) -> usize {
        let mut entries = self.entries.write().await;
        let before_count = entries.len();

        entries.retain(|_, entry| !entry.is_expired(self.config.ttl));

        let after_count = entries.len();
        let removed = before_count - after_count;

        // Update stats
        let mut stats = self.stats.write().await;
        stats.total_entries = after_count;
        stats.expired_entries = entries
            .values()
            .filter(|e| e.is_expired(self.config.ttl))
            .count();

        removed
    }

    /// Get cache statistics
    pub async fn stats(&self) -> CacheStats {
        let entries = self.entries.read().await;
        let mut stats = self.stats.write().await;

        stats.total_entries = entries.len();
        stats.expired_entries = entries
            .values()
            .filter(|e| e.is_expired(self.config.ttl))
            .count();

        stats.clone()
    }

    /// Check if a key exists in the cache
    pub async fn contains_key(&self, key: &str) -> bool {
        let entries = self.entries.read().await;
        entries
            .get(key)
            .map(|e| !e.is_expired(self.config.ttl))
            .unwrap_or(false)
    }

    /// Get all keys in the cache
    pub async fn keys(&self) -> Vec<String> {
        let entries = self.entries.read().await;
        entries.keys().cloned().collect()
    }

    /// Get all non-expired entries
    pub async fn entries(&self) -> Vec<(String, ToolSchema)> {
        let entries = self.entries.read().await;
        entries
            .iter()
            .filter(|(_, e)| !e.is_expired(self.config.ttl))
            .map(|(k, e)| (k.clone(), e.schema.clone()))
            .collect()
    }

    /// Persist cache to disk if persistence is enabled
    pub async fn persist(&self) -> Result<()> {
        if !self.config.persist {
            return Ok(());
        }

        let path = self.config.persist_path.as_ref().ok_or_else(|| {
            DiscoveryError::Cache("Persistence enabled but no path configured".to_string())
        })?;

        let entries = self.entries.read().await;
        let json = serde_json::to_string_pretty(&*entries)
            .map_err(|e| DiscoveryError::Cache(format!("Serialization failed: {}", e)))?;

        tokio::fs::write(path, json)
            .await
            .map_err(|e| DiscoveryError::Cache(format!("Failed to write cache: {}", e)))?;

        Ok(())
    }

    /// Load cache from disk if persistence is enabled
    pub async fn load(&self) -> Result<()> {
        if !self.config.persist {
            return Ok(());
        }

        let path = self.config.persist_path.as_ref().ok_or_else(|| {
            DiscoveryError::Cache("Persistence enabled but no path configured".to_string())
        })?;

        // Read file if it exists
        let json = match tokio::fs::read_to_string(path).await {
            Ok(content) => content,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                // Cache file doesn't exist yet, that's okay
                return Ok(());
            }
            Err(e) => {
                return Err(DiscoveryError::Cache(format!(
                    "Failed to read cache: {}",
                    e
                )))
            }
        };

        let loaded: HashMap<String, CacheEntry> = serde_json::from_str(&json)
            .map_err(|e| DiscoveryError::Cache(format!("Deserialization failed: {}", e)))?;

        let mut entries = self.entries.write().await;
        *entries = loaded;

        // Update stats
        let mut stats = self.stats.write().await;
        stats.total_entries = entries.len();

        Ok(())
    }

    /// Create a cache key from server URL and tool name
    pub fn make_key(server_url: &str, tool_name: &str) -> String {
        format!("{}::{}", server_url, tool_name)
    }

    /// Parse a cache key into server URL and tool name
    pub fn parse_key(key: &str) -> Option<(String, String)> {
        key.split_once("::")
            .map(|(server, tool)| (server.to_string(), tool.to_string()))
    }
}

impl Clone for SchemaCache {
    fn clone(&self) -> Self {
        // Note: This creates a new cache with the same config but empty entries.
        // For a true clone, we'd need to block on the async locks.
        Self {
            entries: RwLock::new(HashMap::new()),
            config: self.config.clone(),
            stats: RwLock::new(CacheStats::default()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_cache_config_default() {
        let config = CacheConfig::default();
        assert_eq!(config.ttl, Duration::from_secs(300));
        assert_eq!(config.max_entries, 1000);
        assert!(!config.persist);
        assert!(config.auto_cleanup);
    }

    #[tokio::test]
    async fn test_cache_insert_and_get() {
        let cache = SchemaCache::with_ttl(Duration::from_secs(60));
        let schema = ToolSchema::new("test_tool", "A test tool");

        let key = "server::test_tool";
        cache.insert(key.to_string(), schema.clone()).await.unwrap();

        let retrieved = cache.get(key).await.unwrap();
        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().name, "test_tool");
    }

    #[tokio::test]
    async fn test_cache_miss() {
        let cache = SchemaCache::with_ttl(Duration::from_secs(60));

        let result = cache.get("nonexistent_key").await.unwrap();
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_cache_remove() {
        let cache = SchemaCache::with_ttl(Duration::from_secs(60));
        let schema = ToolSchema::new("test_tool", "A test tool");

        let key = "server::test_tool";
        cache.insert(key.to_string(), schema).await.unwrap();

        let removed = cache.remove(key).await.unwrap();
        assert!(removed);

        let retrieved = cache.get(key).await.unwrap();
        assert!(retrieved.is_none());
    }

    #[tokio::test]
    async fn test_cache_clear() {
        let cache = SchemaCache::with_ttl(Duration::from_secs(60));

        cache
            .insert("key1".to_string(), ToolSchema::new("tool1", "Tool 1"))
            .await
            .unwrap();
        cache
            .insert("key2".to_string(), ToolSchema::new("tool2", "Tool 2"))
            .await
            .unwrap();

        cache.clear().await.unwrap();

        assert!(!cache.contains_key("key1").await);
        assert!(!cache.contains_key("key2").await);
    }

    #[tokio::test]
    async fn test_cache_stats() {
        let cache = SchemaCache::with_ttl(Duration::from_secs(60));
        let schema = ToolSchema::new("test_tool", "A test tool");

        let key = "server::test_tool";
        cache.insert(key.to_string(), schema).await.unwrap();

        // Hit
        cache.get(key).await.unwrap();

        // Miss
        cache.get("nonexistent").await.unwrap();

        let stats = cache.stats().await;
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
        assert_eq!(stats.total_entries, 1);
        assert!((stats.hit_rate - 0.5).abs() < 0.01); // ~50%
    }

    #[tokio::test]
    async fn test_cache_key_utils() {
        let key = SchemaCache::make_key("http://localhost:3000", "my_tool");
        assert_eq!(key, "http://localhost:3000::my_tool");

        let parsed = SchemaCache::parse_key(&key);
        assert_eq!(
            parsed,
            Some(("http://localhost:3000".to_string(), "my_tool".to_string()))
        );
    }

    #[tokio::test]
    async fn test_cache_entry_is_expired() {
        let schema = ToolSchema::new("test", "Test");
        let mut entry = CacheEntry::new(schema);

        // Fresh entry should not be expired
        assert!(!entry.is_expired(Duration::from_secs(60)));

        // Simulate old entry
        entry.created_at = CacheEntry::now().saturating_sub(61);
        assert!(entry.is_expired(Duration::from_secs(60)));
    }

    #[tokio::test]
    async fn test_cache_max_entries() {
        let cache = SchemaCache::new(CacheConfig {
            ttl: Duration::from_secs(60),
            max_entries: 3,
            ..Default::default()
        });

        // Insert more entries than max
        for i in 0..5 {
            let key = format!("tool{}", i);
            let schema = ToolSchema::new(&key, &format!("Tool {}", i));
            cache.insert(key, schema).await.unwrap();
        }

        let stats = cache.stats().await;
        assert!(stats.total_entries <= 3);
    }

    #[tokio::test]
    async fn test_cache_auto_cleanup() {
        let cache = SchemaCache::new(CacheConfig {
            ttl: Duration::from_millis(100),
            auto_cleanup: true,
            ..Default::default()
        });

        let key = "test_tool";
        let schema = ToolSchema::new("test", "Test");
        cache.insert(key.to_string(), schema).await.unwrap();

        // Should be present immediately
        assert!(cache.contains_key(key).await);

        // Wait for expiration
        tokio::time::sleep(Duration::from_millis(150)).await;

        // Auto-cleanup should remove it on next access
        cache.get(key).await.unwrap();
        assert!(!cache.contains_key(key).await);
    }
}
