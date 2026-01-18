//! Cache manager with memory and disk tiers
//!
//! Provides two-tier caching:
//! - Memory cache (fast, volatile) using moka
//! - Disk cache (persistent, slower) using sled

use crate::DspyError;
use moka::future::Cache as MokaCache;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// Cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheConfig {
    /// Enable in-memory caching
    pub enable_memory: bool,

    /// Enable disk caching
    pub enable_disk: bool,

    /// Maximum entries in memory cache
    pub memory_max_entries: usize,

    /// Disk cache directory
    pub disk_cache_dir: PathBuf,

    /// Disk cache size limit in bytes
    pub disk_size_limit_bytes: u64,

    /// Time-to-live in seconds (None = no expiration)
    pub ttl_seconds: Option<u64>,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            enable_memory: true,
            enable_disk: true,
            memory_max_entries: 1000,
            disk_cache_dir: PathBuf::from(".dspy_cache"),
            disk_size_limit_bytes: 1_000_000_000, // 1GB
            ttl_seconds: Some(86400),              // 24 hours
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CacheStats {
    /// Total cache hits
    pub hits: u64,

    /// Memory cache hits
    pub memory_hits: u64,

    /// Disk cache hits
    pub disk_hits: u64,

    /// Cache misses
    pub misses: u64,

    /// Current memory entries
    pub memory_entries: u64,

    /// Current disk entries
    pub disk_entries: u64,
}

impl CacheStats {
    /// Calculate hit rate (0.0-1.0)
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }

    /// Calculate cache hit rate (0.0-1.0)
    pub fn cache_hit_rate(&self) -> f64 {
        self.hit_rate()
    }
}

/// Cache manager with memory and disk tiers
pub struct CacheManager {
    /// Memory cache (fast tier)
    pub memory_cache: Option<Arc<MokaCache<String, String>>>,

    /// Disk cache (persistent tier)
    pub disk_cache: Option<Arc<sled::Db>>,

    /// Cache configuration
    #[allow(dead_code)]
    config: CacheConfig,

    /// Statistics
    stats: Arc<CacheStatsInner>,
}

#[derive(Debug, Default)]
struct CacheStatsInner {
    hits: AtomicU64,
    memory_hits: AtomicU64,
    disk_hits: AtomicU64,
    misses: AtomicU64,
}

impl CacheManager {
    /// Create new cache manager
    pub fn new(config: CacheConfig) -> Result<Self, DspyError> {
        let memory_cache = if config.enable_memory {
            let mut builder = MokaCache::builder()
                .max_capacity(config.memory_max_entries as u64);

            if let Some(ttl) = config.ttl_seconds {
                builder = builder.time_to_live(std::time::Duration::from_secs(ttl));
            }

            Some(Arc::new(builder.build()))
        } else {
            None
        };

        let disk_cache = if config.enable_disk {
            std::fs::create_dir_all(&config.disk_cache_dir).map_err(|e| {
                DspyError::config_error(format!("Failed to create cache directory: {}", e))
            })?;

            let db = sled::open(&config.disk_cache_dir).map_err(|e| {
                DspyError::config_error(format!("Failed to open disk cache: {}", e))
            })?;

            Some(Arc::new(db))
        } else {
            None
        };

        Ok(Self {
            memory_cache,
            disk_cache,
            config,
            stats: Arc::new(CacheStatsInner::default()),
        })
    }

    /// Get value from cache
    pub async fn get(&self, key: &str) -> Option<String> {
        // Try memory cache first
        if let Some(ref cache) = self.memory_cache {
            if let Some(value) = cache.get(key).await {
                self.stats.hits.fetch_add(1, Ordering::Relaxed);
                self.stats.memory_hits.fetch_add(1, Ordering::Relaxed);
                return Some(value);
            }
        }

        // Try disk cache
        if let Some(ref cache) = self.disk_cache {
            if let Ok(Some(value)) = cache.get(key.as_bytes()) {
                let value_str = String::from_utf8_lossy(&value).to_string();

                // Promote to memory cache
                if let Some(ref mem) = self.memory_cache {
                    mem.insert(key.to_string(), value_str.clone()).await;
                }

                self.stats.hits.fetch_add(1, Ordering::Relaxed);
                self.stats.disk_hits.fetch_add(1, Ordering::Relaxed);
                return Some(value_str);
            }
        }

        // Cache miss
        self.stats.misses.fetch_add(1, Ordering::Relaxed);
        None
    }

    /// Set value in cache
    pub async fn set(&self, key: String, value: String) -> Result<(), DspyError> {
        // Set in memory cache
        if let Some(ref cache) = self.memory_cache {
            cache.insert(key.clone(), value.clone()).await;
        }

        // Set in disk cache
        if let Some(ref cache) = self.disk_cache {
            cache
                .insert(key.as_bytes(), value.as_bytes())
                .map_err(|e| DspyError::cache(format!("Failed to write to disk cache: {}", e)))?;
            cache
                .flush()
                .map_err(|e| DspyError::cache(format!("Failed to flush disk cache: {}", e)))?;
        }

        Ok(())
    }

    /// Remove value from cache
    pub async fn remove(&self, key: &str) -> Result<(), DspyError> {
        // Remove from memory cache
        if let Some(ref cache) = self.memory_cache {
            cache.invalidate(key).await;
        }

        // Remove from disk cache
        if let Some(ref cache) = self.disk_cache {
            cache.remove(key.as_bytes()).map_err(|e| {
                DspyError::cache(format!("Failed to remove from disk cache: {}", e))
            })?;
        }

        Ok(())
    }

    /// Clear all cache entries
    pub async fn clear(&self) -> Result<(), DspyError> {
        // Clear memory cache
        if let Some(ref cache) = self.memory_cache {
            cache.invalidate_all();
        }

        // Clear disk cache
        if let Some(ref cache) = self.disk_cache {
            cache
                .clear()
                .map_err(|e| DspyError::cache(format!("Failed to clear disk cache: {}", e)))?;
        }

        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let memory_entries = self
            .memory_cache
            .as_ref()
            .map(|c| c.entry_count())
            .unwrap_or(0);

        let disk_entries = self
            .disk_cache
            .as_ref()
            .map(|c| c.len() as u64)
            .unwrap_or(0);

        CacheStats {
            hits: self.stats.hits.load(Ordering::Relaxed),
            memory_hits: self.stats.memory_hits.load(Ordering::Relaxed),
            disk_hits: self.stats.disk_hits.load(Ordering::Relaxed),
            misses: self.stats.misses.load(Ordering::Relaxed),
            memory_entries,
            disk_entries,
        }
    }

    /// Reset statistics
    pub fn reset_stats(&self) {
        self.stats.hits.store(0, Ordering::Relaxed);
        self.stats.memory_hits.store(0, Ordering::Relaxed);
        self.stats.disk_hits.store(0, Ordering::Relaxed);
        self.stats.misses.store(0, Ordering::Relaxed);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    async fn create_test_cache() -> (CacheManager, TempDir) {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            enable_memory: true,
            enable_disk: true,
            memory_max_entries: 100,
            disk_cache_dir: temp_dir.path().to_path_buf(),
            disk_size_limit_bytes: 1_000_000,
            ttl_seconds: None,
        };

        let manager = CacheManager::new(config).unwrap();
        (manager, temp_dir)
    }

    #[tokio::test]
    async fn test_cache_set_and_get() {
        let (manager, _temp_dir) = create_test_cache().await;

        manager
            .set("test_key".to_string(), "test_value".to_string())
            .await
            .unwrap();

        let value = manager.get("test_key").await;
        assert_eq!(value, Some("test_value".to_string()));
    }

    #[tokio::test]
    async fn test_cache_statistics() {
        let (manager, _temp_dir) = create_test_cache().await;

        manager
            .set("key1".to_string(), "value1".to_string())
            .await
            .unwrap();

        // 2 hits, 1 miss
        manager.get("key1").await;
        manager.get("key1").await;
        manager.get("key2").await;

        let stats = manager.stats();
        assert_eq!(stats.hits, 2);
        assert_eq!(stats.misses, 1);
        assert!(stats.hit_rate() > 0.5);
    }
}
