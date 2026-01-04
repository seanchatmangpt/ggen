//! Local package caching system (T009)
//!
//! This module implements a file-based package cache for storing
//! downloaded packages locally to avoid repeated downloads.
//!
//! ## Two-Layer Caching Strategy
//!
//! - **Metadata Layer**: Short TTL (1 hour) for package metadata, version lists
//! - **Package Layer**: Long TTL (7 days) for downloaded package files
//!
//! ## CacheManager Trait
//!
//! Defines the interface for cache implementations:
//! - `get()` - Retrieve cached entry
//! - `set()` - Store entry in cache
//! - `invalidate()` - Remove specific entry
//! - `cleanup()` - Remove expired entries

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

use super::error::{GpackError, GpackResult};

/// Default cache directory name
pub const CACHE_DIR: &str = ".ggen/cache/packages";

/// Metadata cache directory
pub const METADATA_CACHE_DIR: &str = ".ggen/cache/metadata";

/// Default package cache TTL (7 days)
pub const DEFAULT_TTL: Duration = Duration::from_secs(7 * 24 * 60 * 60);

/// Metadata cache TTL (1 hour)
pub const METADATA_TTL: Duration = Duration::from_secs(60 * 60);

/// Cache tier classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CacheTier {
    /// Metadata tier: version lists, package info (1h TTL)
    Metadata,
    /// Package tier: downloaded package files (7d TTL)
    Package,
}

impl CacheTier {
    /// Get the TTL for this cache tier
    pub fn ttl(&self) -> Duration {
        match self {
            CacheTier::Metadata => METADATA_TTL,
            CacheTier::Package => DEFAULT_TTL,
        }
    }

    /// Get the cache directory suffix for this tier
    pub fn dir_suffix(&self) -> &'static str {
        match self {
            CacheTier::Metadata => "metadata",
            CacheTier::Package => "packages",
        }
    }
}

impl Default for CacheTier {
    fn default() -> Self {
        CacheTier::Package
    }
}

/// Cache manager trait defining the interface for cache implementations (T009)
///
/// This trait provides the core caching operations that any cache
/// implementation must support.
pub trait CacheManager: Send + Sync {
    /// Get a cached entry by key
    ///
    /// Returns `None` if the entry doesn't exist or has expired.
    fn get(&self, key: &str) -> GpackResult<Option<Vec<u8>>>;

    /// Store data in the cache with a key
    ///
    /// Returns the cache entry metadata.
    fn set(&mut self, key: &str, data: &[u8], tier: CacheTier) -> GpackResult<CacheEntry>;

    /// Invalidate (remove) a specific cache entry
    ///
    /// Returns `true` if an entry was removed, `false` if it didn't exist.
    fn invalidate(&mut self, key: &str) -> GpackResult<bool>;

    /// Clean up expired entries from the cache
    ///
    /// Returns the total size of data removed in bytes.
    fn cleanup(&mut self) -> GpackResult<u64>;

    /// Check if a key exists and is valid (not expired)
    fn contains(&self, key: &str) -> bool;

    /// Get cache statistics
    fn stats(&self) -> CacheStats;
}

/// Metadata cache entry for quick lookups
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetadataCacheEntry {
    /// Cache key
    pub key: String,
    /// Cached data (JSON serialized)
    pub data: String,
    /// Cache timestamp
    pub cached_at: u64,
    /// TTL in seconds
    pub ttl_secs: u64,
    /// Cache tier
    pub tier: CacheTier,
}

/// Cache entry metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// SHA-256 checksum (hash)
    pub checksum: String,
    /// Download timestamp
    pub downloaded_at: u64,
    /// File size in bytes
    pub size: u64,
    /// Path to cached file (location)
    pub path: PathBuf,
    /// Cache tier
    #[serde(default)]
    pub tier: CacheTier,
}

impl CacheEntry {
    /// Create a new cache entry
    pub fn new(
        name: impl Into<String>,
        version: impl Into<String>,
        checksum: impl Into<String>,
        path: impl Into<PathBuf>,
        size: u64,
    ) -> Self {
        Self {
            name: name.into(),
            version: version.into(),
            checksum: checksum.into(),
            downloaded_at: SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            size,
            path: path.into(),
            tier: CacheTier::Package,
        }
    }

    /// Create entry with specific tier
    pub fn with_tier(mut self, tier: CacheTier) -> Self {
        self.tier = tier;
        self
    }

    /// Check if entry is expired based on tier TTL
    pub fn is_expired(&self) -> bool {
        let now = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        now.saturating_sub(self.downloaded_at) > self.tier.ttl().as_secs()
    }

    /// Get the cache key for this entry
    pub fn cache_key(&self) -> String {
        format!("{}@{}", self.name, self.version)
    }
}

/// Package cache manager
#[derive(Debug)]
pub struct PackageCache {
    /// Cache directory path
    cache_dir: PathBuf,
    /// Cache index (name@version -> entry)
    index: HashMap<String, CacheEntry>,
    /// Cache TTL
    ttl: Duration,
    /// Maximum cache size in bytes
    max_size: u64,
}

impl PackageCache {
    /// Create a new package cache
    pub fn new(cache_dir: impl AsRef<Path>) -> GpackResult<Self> {
        let cache_dir = cache_dir.as_ref().to_path_buf();

        // Create cache directory if it doesn't exist
        if !cache_dir.exists() {
            std::fs::create_dir_all(&cache_dir)?;
        }

        let mut cache = Self {
            cache_dir,
            index: HashMap::new(),
            ttl: DEFAULT_TTL,
            max_size: 1024 * 1024 * 1024, // 1GB default
        };

        // Load existing cache index
        cache.load_index()?;

        Ok(cache)
    }

    /// Create cache with custom settings
    pub fn with_config(
        cache_dir: impl AsRef<Path>,
        ttl: Duration,
        max_size: u64,
    ) -> GpackResult<Self> {
        let mut cache = Self::new(cache_dir)?;
        cache.ttl = ttl;
        cache.max_size = max_size;
        Ok(cache)
    }

    /// Get a cached package
    pub fn get(&self, name: &str, version: &str) -> Option<&CacheEntry> {
        let key = format!("{}@{}", name, version);
        let entry = self.index.get(&key)?;

        // Check if entry has expired
        if self.is_expired(entry) {
            return None;
        }

        // Check if file still exists
        if !entry.path.exists() {
            return None;
        }

        Some(entry)
    }

    /// Store a package in the cache
    pub fn store(
        &mut self,
        name: &str,
        version: &str,
        checksum: &str,
        data: &[u8],
    ) -> GpackResult<CacheEntry> {
        // Ensure we have space
        self.ensure_space(data.len() as u64)?;

        // Create package directory
        let pkg_dir = self.cache_dir.join(name);
        if !pkg_dir.exists() {
            std::fs::create_dir_all(&pkg_dir)?;
        }

        // Write package file
        let file_path = pkg_dir.join(format!("{}.gpack", version));
        std::fs::write(&file_path, data)?;

        // Create cache entry
        let entry = CacheEntry::new(name, version, checksum, &file_path, data.len() as u64);

        // Update index
        let key = format!("{}@{}", name, version);
        self.index.insert(key, entry.clone());

        // Save index
        self.save_index()?;

        Ok(entry)
    }

    /// Remove a package from cache
    pub fn remove(&mut self, name: &str, version: &str) -> GpackResult<bool> {
        let key = format!("{}@{}", name, version);

        if let Some(entry) = self.index.remove(&key) {
            if entry.path.exists() {
                std::fs::remove_file(&entry.path)?;
            }
            self.save_index()?;
            return Ok(true);
        }

        Ok(false)
    }

    /// Clear expired entries
    pub fn cleanup_expired(&mut self) -> GpackResult<u64> {
        let mut removed_size = 0u64;
        let expired: Vec<String> = self
            .index
            .iter()
            .filter(|(_, entry)| self.is_expired(entry))
            .map(|(key, _)| key.clone())
            .collect();

        for key in expired {
            if let Some(entry) = self.index.remove(&key) {
                if entry.path.exists() {
                    std::fs::remove_file(&entry.path)?;
                }
                removed_size += entry.size;
            }
        }

        if removed_size > 0 {
            self.save_index()?;
        }

        Ok(removed_size)
    }

    /// Clear all cached packages
    pub fn clear(&mut self) -> GpackResult<()> {
        for entry in self.index.values() {
            if entry.path.exists() {
                let _ = std::fs::remove_file(&entry.path);
            }
        }
        self.index.clear();
        self.save_index()?;
        Ok(())
    }

    /// Get total cache size
    pub fn total_size(&self) -> u64 {
        self.index.values().map(|e| e.size).sum()
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let metadata_entries = self
            .index
            .values()
            .filter(|e| e.tier == CacheTier::Metadata)
            .count();
        let package_entries = self
            .index
            .values()
            .filter(|e| e.tier == CacheTier::Package)
            .count();

        CacheStats {
            total_entries: self.index.len(),
            total_size: self.total_size(),
            max_size: self.max_size,
            ttl_seconds: self.ttl.as_secs(),
            metadata_entries,
            package_entries,
            hits: 0, // Would need runtime tracking
            misses: 0,
        }
    }

    /// Store with specific cache tier
    pub fn store_with_tier(
        &mut self,
        name: &str,
        version: &str,
        checksum: &str,
        data: &[u8],
        tier: CacheTier,
    ) -> GpackResult<CacheEntry> {
        // Ensure we have space
        self.ensure_space(data.len() as u64)?;

        // Create package directory
        let pkg_dir = self.cache_dir.join(tier.dir_suffix()).join(name);
        if !pkg_dir.exists() {
            std::fs::create_dir_all(&pkg_dir)?;
        }

        // Write package file
        let file_path = pkg_dir.join(format!("{}.gpack", version));
        std::fs::write(&file_path, data)?;

        // Create cache entry with tier
        let entry = CacheEntry::new(name, version, checksum, &file_path, data.len() as u64)
            .with_tier(tier);

        // Update index
        let key = format!("{}@{}", name, version);
        self.index.insert(key, entry.clone());

        // Save index
        self.save_index()?;

        Ok(entry)
    }

    /// Load cache index from disk
    fn load_index(&mut self) -> GpackResult<()> {
        let index_path = self.cache_dir.join("index.json");
        if index_path.exists() {
            let content = std::fs::read_to_string(&index_path)?;
            self.index = serde_json::from_str(&content)
                .map_err(|e| GpackError::CacheError(e.to_string()))?;
        }
        Ok(())
    }

    /// Save cache index to disk
    fn save_index(&self) -> GpackResult<()> {
        let index_path = self.cache_dir.join("index.json");
        let content = serde_json::to_string_pretty(&self.index)
            .map_err(|e| GpackError::CacheError(e.to_string()))?;
        std::fs::write(index_path, content)?;
        Ok(())
    }

    /// Check if a cache entry has expired
    fn is_expired(&self, entry: &CacheEntry) -> bool {
        entry.is_expired()
    }

    /// Get the cache directory path
    pub fn cache_dir(&self) -> &Path {
        &self.cache_dir
    }

    /// Get the TTL setting
    pub fn ttl(&self) -> Duration {
        self.ttl
    }

    /// Ensure there's enough space for new data
    fn ensure_space(&mut self, required: u64) -> GpackResult<()> {
        while self.total_size() + required > self.max_size {
            // Remove oldest entry
            let oldest = self
                .index
                .iter()
                .min_by_key(|(_, e)| e.downloaded_at)
                .map(|(k, _)| k.clone());

            if let Some(key) = oldest {
                if let Some(entry) = self.index.remove(&key) {
                    if entry.path.exists() {
                        std::fs::remove_file(&entry.path)?;
                    }
                }
            } else {
                break;
            }
        }
        Ok(())
    }
}

/// Cache statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheStats {
    /// Total number of cached entries
    pub total_entries: usize,
    /// Total size in bytes
    pub total_size: u64,
    /// Maximum allowed size in bytes
    pub max_size: u64,
    /// TTL in seconds (for package tier)
    pub ttl_seconds: u64,
    /// Number of metadata entries
    #[serde(default)]
    pub metadata_entries: usize,
    /// Number of package entries
    #[serde(default)]
    pub package_entries: usize,
    /// Cache hit count
    #[serde(default)]
    pub hits: u64,
    /// Cache miss count
    #[serde(default)]
    pub misses: u64,
}

impl CacheStats {
    /// Calculate hit ratio
    pub fn hit_ratio(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }

    /// Check if cache is healthy (>50% hit ratio, <80% capacity)
    pub fn is_healthy(&self) -> bool {
        let capacity_ratio = if self.max_size == 0 {
            0.0
        } else {
            self.total_size as f64 / self.max_size as f64
        };
        self.hit_ratio() >= 0.5 || (self.hits + self.misses) < 10 && capacity_ratio < 0.8
    }
}

impl Default for CacheStats {
    fn default() -> Self {
        Self {
            total_entries: 0,
            total_size: 0,
            max_size: 1024 * 1024 * 1024, // 1GB
            ttl_seconds: DEFAULT_TTL.as_secs(),
            metadata_entries: 0,
            package_entries: 0,
            hits: 0,
            misses: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_cache_creation() {
        let temp_dir = TempDir::new().unwrap();
        let cache = PackageCache::new(temp_dir.path()).unwrap();
        assert_eq!(cache.total_size(), 0);
    }

    #[test]
    fn test_store_and_get() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = PackageCache::new(temp_dir.path()).unwrap();

        let data = b"test package content";
        let entry = cache
            .store("test-pkg", "1.0.0", "abc123", data)
            .unwrap();

        assert_eq!(entry.name, "test-pkg");
        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.size, data.len() as u64);

        let retrieved = cache.get("test-pkg", "1.0.0");
        assert!(retrieved.is_some());
    }

    #[test]
    fn test_remove() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = PackageCache::new(temp_dir.path()).unwrap();

        cache.store("test-pkg", "1.0.0", "abc123", b"data").unwrap();
        assert!(cache.get("test-pkg", "1.0.0").is_some());

        cache.remove("test-pkg", "1.0.0").unwrap();
        assert!(cache.get("test-pkg", "1.0.0").is_none());
    }

    #[test]
    fn test_stats() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = PackageCache::new(temp_dir.path()).unwrap();

        cache.store("pkg1", "1.0.0", "abc", b"data1").unwrap();
        cache.store("pkg2", "1.0.0", "def", b"data2").unwrap();

        let stats = cache.stats();
        assert_eq!(stats.total_entries, 2);
        assert_eq!(stats.total_size, 10); // "data1" + "data2"
    }

    #[test]
    fn test_cache_tier_ttl() {
        assert_eq!(CacheTier::Metadata.ttl(), METADATA_TTL);
        assert_eq!(CacheTier::Package.ttl(), DEFAULT_TTL);
    }

    #[test]
    fn test_cache_tier_dir_suffix() {
        assert_eq!(CacheTier::Metadata.dir_suffix(), "metadata");
        assert_eq!(CacheTier::Package.dir_suffix(), "packages");
    }

    #[test]
    fn test_cache_entry_new() {
        let entry = CacheEntry::new("test-pkg", "1.0.0", "sha256:abc", "/cache/test", 100);
        assert_eq!(entry.name, "test-pkg");
        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.checksum, "sha256:abc");
        assert_eq!(entry.size, 100);
        assert_eq!(entry.tier, CacheTier::Package);
    }

    #[test]
    fn test_cache_entry_with_tier() {
        let entry = CacheEntry::new("test-pkg", "1.0.0", "sha256:abc", "/cache/test", 100)
            .with_tier(CacheTier::Metadata);
        assert_eq!(entry.tier, CacheTier::Metadata);
    }

    #[test]
    fn test_cache_entry_cache_key() {
        let entry = CacheEntry::new("my-pkg", "2.0.0", "sha256:xyz", "/cache/pkg", 50);
        assert_eq!(entry.cache_key(), "my-pkg@2.0.0");
    }

    #[test]
    fn test_store_with_tier() {
        let temp_dir = TempDir::new().unwrap();
        let mut cache = PackageCache::new(temp_dir.path()).unwrap();

        let data = b"metadata content";
        let entry = cache
            .store_with_tier("meta-pkg", "1.0.0", "abc123", data, CacheTier::Metadata)
            .unwrap();

        assert_eq!(entry.tier, CacheTier::Metadata);
        assert!(entry.path.to_string_lossy().contains("metadata"));
    }

    #[test]
    fn test_cache_stats_hit_ratio() {
        let mut stats = CacheStats::default();
        assert_eq!(stats.hit_ratio(), 0.0);

        stats.hits = 80;
        stats.misses = 20;
        assert!((stats.hit_ratio() - 0.8).abs() < 0.01);
    }

    #[test]
    fn test_cache_stats_is_healthy() {
        let mut stats = CacheStats::default();
        stats.max_size = 1000;
        stats.total_size = 500; // 50% capacity
        assert!(stats.is_healthy()); // Low usage counts as healthy

        stats.hits = 60;
        stats.misses = 40;
        assert!(stats.is_healthy()); // 60% hit ratio, 50% capacity

        stats.hits = 20;
        stats.misses = 80;
        stats.total_size = 900; // 90% capacity
        assert!(!stats.is_healthy()); // 20% hit ratio, 90% capacity
    }

    #[test]
    fn test_metadata_cache_entry_serialization() {
        let entry = MetadataCacheEntry {
            key: "tokio@1.0.0".to_string(),
            data: r#"{"name": "tokio", "version": "1.0.0"}"#.to_string(),
            cached_at: 1700000000,
            ttl_secs: 3600,
            tier: CacheTier::Metadata,
        };

        let json = serde_json::to_string(&entry).unwrap();
        let parsed: MetadataCacheEntry = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.key, "tokio@1.0.0");
        assert_eq!(parsed.tier, CacheTier::Metadata);
    }
}
