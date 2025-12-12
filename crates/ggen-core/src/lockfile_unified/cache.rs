//! Cache coherency for unified lockfile system
//!
//! Provides shared LRU caching with statistics tracking and
//! cross-manager coherency for dependency resolution.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};

/// Cache statistics for monitoring and debugging
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Total cache hits
    pub hits: u64,
    /// Total cache misses
    pub misses: u64,
    /// Current number of entries
    pub entries: usize,
    /// Total bytes used (estimated)
    pub bytes_used: usize,
    /// Cache evictions
    pub evictions: u64,
    /// Last cleanup timestamp
    pub last_cleanup: Option<Instant>,
}

impl CacheStats {
    /// Calculate hit rate as percentage
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            (self.hits as f64 / total as f64) * 100.0
        }
    }

    /// Reset statistics
    pub fn reset(&mut self) {
        self.hits = 0;
        self.misses = 0;
        self.evictions = 0;
    }
}

/// Cache entry with TTL support
#[derive(Debug, Clone)]
struct CacheEntry<V> {
    value: V,
    created_at: Instant,
    ttl: Duration,
    size_estimate: usize,
}

impl<V> CacheEntry<V> {
    fn is_expired(&self) -> bool {
        self.created_at.elapsed() > self.ttl
    }
}

/// Coherent cache for lockfile operations
///
/// Provides thread-safe caching with:
/// - TTL-based expiration
/// - LRU eviction when capacity exceeded
/// - Statistics tracking
/// - Cross-manager coherency
#[derive(Debug)]
pub struct CoherentCache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    /// Internal storage
    storage: Arc<RwLock<HashMap<K, CacheEntry<V>>>>,
    /// Maximum entries before eviction
    max_entries: usize,
    /// Default TTL for entries
    default_ttl: Duration,
    /// Statistics
    stats: Arc<RwLock<CacheStats>>,
}

impl<K, V> Clone for CoherentCache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            storage: Arc::clone(&self.storage),
            max_entries: self.max_entries,
            default_ttl: self.default_ttl,
            stats: Arc::clone(&self.stats),
        }
    }
}

impl<K, V> Default for CoherentCache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    fn default() -> Self {
        // Default: 1000 entries, 5 minute TTL
        Self::new(1000, Duration::from_secs(300))
    }
}

impl<K, V> CoherentCache<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    /// Create new cache with specified capacity and TTL
    pub fn new(max_entries: usize, default_ttl: Duration) -> Self {
        Self {
            storage: Arc::new(RwLock::new(HashMap::new())),
            max_entries,
            default_ttl,
            stats: Arc::new(RwLock::new(CacheStats::default())),
        }
    }

    /// Get value from cache
    pub fn get(&self, key: &K) -> Option<V> {
        let storage = self.storage.read().ok()?;

        match storage.get(key) {
            Some(entry) if !entry.is_expired() => {
                if let Ok(mut stats) = self.stats.write() {
                    stats.hits += 1;
                }
                Some(entry.value.clone())
            }
            _ => {
                if let Ok(mut stats) = self.stats.write() {
                    stats.misses += 1;
                }
                None
            }
        }
    }

    /// Insert value with default TTL
    pub fn insert(&self, key: K, value: V) {
        self.insert_with_ttl(key, value, self.default_ttl);
    }

    /// Insert value with custom TTL
    pub fn insert_with_ttl(&self, key: K, value: V, ttl: Duration) {
        let size_estimate = std::mem::size_of::<V>();

        if let Ok(mut storage) = self.storage.write() {
            // Check if eviction needed
            if storage.len() >= self.max_entries {
                self.evict_expired(&mut storage);
            }

            // If still at capacity, evict oldest
            if storage.len() >= self.max_entries {
                self.evict_oldest(&mut storage);
            }

            storage.insert(
                key,
                CacheEntry {
                    value,
                    created_at: Instant::now(),
                    ttl,
                    size_estimate,
                },
            );

            if let Ok(mut stats) = self.stats.write() {
                stats.entries = storage.len();
                stats.bytes_used = storage.values().map(|e| e.size_estimate).sum();
            }
        }
    }

    /// Remove entry from cache
    pub fn remove(&self, key: &K) -> Option<V> {
        let mut storage = self.storage.write().ok()?;
        let entry = storage.remove(key)?;

        if let Ok(mut stats) = self.stats.write() {
            stats.entries = storage.len();
            stats.bytes_used = storage.values().map(|e| e.size_estimate).sum();
        }

        Some(entry.value)
    }

    /// Clear all entries
    pub fn clear(&self) {
        if let Ok(mut storage) = self.storage.write() {
            storage.clear();
            if let Ok(mut stats) = self.stats.write() {
                stats.entries = 0;
                stats.bytes_used = 0;
                stats.last_cleanup = Some(Instant::now());
            }
        }
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        self.stats.read().map(|s| s.clone()).unwrap_or_default()
    }

    /// Reset statistics
    pub fn reset_stats(&self) {
        if let Ok(mut stats) = self.stats.write() {
            stats.reset();
        }
    }

    /// Run cleanup to remove expired entries
    pub fn cleanup(&self) {
        if let Ok(mut storage) = self.storage.write() {
            self.evict_expired(&mut storage);
            if let Ok(mut stats) = self.stats.write() {
                stats.entries = storage.len();
                stats.bytes_used = storage.values().map(|e| e.size_estimate).sum();
                stats.last_cleanup = Some(Instant::now());
            }
        }
    }

    /// Check if cache contains key (and not expired)
    pub fn contains(&self, key: &K) -> bool {
        self.storage
            .read()
            .ok()
            .map(|s| s.get(key).map(|e| !e.is_expired()).unwrap_or(false))
            .unwrap_or(false)
    }

    /// Get current entry count
    pub fn len(&self) -> usize {
        self.storage.read().ok().map(|s| s.len()).unwrap_or(0)
    }

    /// Check if cache is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // Internal: evict expired entries
    fn evict_expired(&self, storage: &mut HashMap<K, CacheEntry<V>>) {
        let before = storage.len();
        storage.retain(|_, entry| !entry.is_expired());
        let evicted = before - storage.len();

        if evicted > 0 {
            if let Ok(mut stats) = self.stats.write() {
                stats.evictions += evicted as u64;
            }
        }
    }

    // Internal: evict oldest entry (simple LRU approximation)
    fn evict_oldest(&self, storage: &mut HashMap<K, CacheEntry<V>>) {
        if let Some(oldest_key) = storage
            .iter()
            .min_by_key(|(_, entry)| entry.created_at)
            .map(|(k, _)| k.clone())
        {
            storage.remove(&oldest_key);
            if let Ok(mut stats) = self.stats.write() {
                stats.evictions += 1;
            }
        }
    }
}

/// Dependency resolution cache key
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DepCacheKey {
    /// Package/entry ID
    pub id: String,
    /// Version constraint
    pub version: String,
}

impl DepCacheKey {
    /// Create new cache key
    pub fn new(id: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            version: version.into(),
        }
    }
}

/// Pre-configured cache for dependency resolution
pub type DependencyCache = CoherentCache<DepCacheKey, Vec<String>>;

/// Create a new dependency resolution cache with sensible defaults
pub fn new_dependency_cache() -> DependencyCache {
    CoherentCache::new(500, Duration::from_secs(600)) // 500 entries, 10 min TTL
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_cache_basic_operations() {
        let cache: CoherentCache<String, i32> = CoherentCache::default();

        cache.insert("key1".into(), 42);
        assert_eq!(cache.get(&"key1".into()), Some(42));
        assert!(cache.contains(&"key1".into()));

        cache.remove(&"key1".into());
        assert_eq!(cache.get(&"key1".into()), None);
    }

    #[test]
    fn test_cache_expiration() {
        let cache: CoherentCache<String, i32> = CoherentCache::new(100, Duration::from_millis(50));

        cache.insert("key1".into(), 42);
        assert_eq!(cache.get(&"key1".into()), Some(42));

        thread::sleep(Duration::from_millis(100));
        assert_eq!(cache.get(&"key1".into()), None);
    }

    #[test]
    fn test_cache_stats() {
        let cache: CoherentCache<String, i32> = CoherentCache::default();

        cache.insert("key1".into(), 42);
        let _ = cache.get(&"key1".into()); // hit
        let _ = cache.get(&"key2".into()); // miss

        let stats = cache.stats();
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
        assert_eq!(stats.hit_rate(), 50.0);
    }

    #[test]
    fn test_cache_eviction() {
        let cache: CoherentCache<i32, i32> = CoherentCache::new(3, Duration::from_secs(60));

        cache.insert(1, 10);
        cache.insert(2, 20);
        cache.insert(3, 30);
        cache.insert(4, 40); // Should evict oldest

        assert!(cache.len() <= 3);
    }

    #[test]
    fn test_dependency_cache() {
        let cache = new_dependency_cache();

        let key = DepCacheKey::new("io.ggen.test", "1.0.0");
        cache.insert(key.clone(), vec!["dep1".into(), "dep2".into()]);

        let deps = cache.get(&key).unwrap();
        assert_eq!(deps.len(), 2);
    }

    #[test]
    fn test_cache_thread_safety() {
        let cache: CoherentCache<i32, i32> = CoherentCache::default();
        let cache_clone = cache.clone();

        let handle = thread::spawn(move || {
            for i in 0..100 {
                cache_clone.insert(i, i * 2);
            }
        });

        for i in 100..200 {
            cache.insert(i, i * 2);
        }

        handle.join().unwrap();
        assert!(cache.len() > 0);
    }
}
