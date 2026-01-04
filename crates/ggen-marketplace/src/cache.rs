//! Multi-layer caching system for marketplace packages
//!
//! Features:
//! - Metadata cache: 1 hour TTL for crate_index entries
//! - Package cache: 7 days TTL for downloaded .tar.gz files
//! - Storage: ~/.ggen/cache/ with organized structure
//! - Cleanup: remove expired entries on startup
//! - Validation: verify SHA256 on read

use crate::error::{Error, Result};
use crate::models::{Package, PackageId, PackageVersion};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{debug, info, warn};

/// Cache entry metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntryMeta {
    /// When the entry was cached
    pub cached_at: DateTime<Utc>,
    /// When the entry expires
    pub expires_at: DateTime<Utc>,
    /// SHA256 checksum of the cached data
    pub checksum: String,
    /// Size in bytes
    pub size: u64,
    /// Number of times accessed
    pub access_count: u64,
    /// Last accessed time
    pub last_accessed: DateTime<Utc>,
}

impl CacheEntryMeta {
    /// Check if the entry has expired
    pub fn is_expired(&self) -> bool {
        Utc::now() > self.expires_at
    }

    /// Create new metadata with TTL
    pub fn new(checksum: String, size: u64, ttl: Duration) -> Self {
        let now = Utc::now();
        Self {
            cached_at: now,
            expires_at: now + ttl,
            checksum,
            size,
            access_count: 0,
            last_accessed: now,
        }
    }

    /// Record an access
    pub fn record_access(&mut self) {
        self.access_count += 1;
        self.last_accessed = Utc::now();
    }
}

/// Cached package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedPackageMetadata {
    /// Package information
    pub package: Package,
    /// Cache metadata
    pub meta: CacheEntryMeta,
}

/// Cached package data (binary)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedPackageData {
    /// Package ID
    pub id: PackageId,
    /// Version
    pub version: PackageVersion,
    /// File path (relative to cache root)
    pub file_path: String,
    /// Cache metadata
    pub meta: CacheEntryMeta,
}

/// Cache layer type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CacheLayer {
    /// Metadata cache (1 hour TTL)
    Metadata,
    /// Package cache (7 days TTL)
    Package,
}

impl CacheLayer {
    /// Get TTL for this layer
    pub fn ttl(&self) -> Duration {
        match self {
            Self::Metadata => Duration::hours(1),
            Self::Package => Duration::days(7),
        }
    }

    /// Get directory name for this layer
    pub fn dir_name(&self) -> &'static str {
        match self {
            Self::Metadata => "metadata",
            Self::Package => "packages",
        }
    }
}

/// Multi-layer cache system
pub struct MultiLayerCache {
    /// Cache root directory
    cache_root: PathBuf,
    /// In-memory metadata cache
    metadata_cache: Arc<RwLock<BTreeMap<String, CachedPackageMetadata>>>,
    /// In-memory package index
    package_index: Arc<RwLock<BTreeMap<String, CachedPackageData>>>,
    /// Maximum cache size in bytes
    max_size: u64,
    /// Current cache size
    current_size: Arc<RwLock<u64>>,
}

impl MultiLayerCache {
    /// Create a new multi-layer cache
    pub fn new(cache_root: impl Into<PathBuf>) -> Result<Self> {
        let cache_root = cache_root.into();

        // Create cache directories
        std::fs::create_dir_all(cache_root.join("metadata"))?;
        std::fs::create_dir_all(cache_root.join("packages"))?;

        let cache = Self {
            cache_root,
            metadata_cache: Arc::new(RwLock::new(BTreeMap::new())),
            package_index: Arc::new(RwLock::new(BTreeMap::new())),
            max_size: 1024 * 1024 * 1024, // 1GB default
            current_size: Arc::new(RwLock::new(0)),
        };

        // Load existing cache index
        cache.load_index()?;

        Ok(cache)
    }

    /// Create cache with custom home directory
    pub fn with_home() -> Result<Self> {
        let home = dirs::home_dir()
            .ok_or_else(|| Error::ConfigError("Could not find home directory".to_string()))?;
        let cache_root = home.join(".ggen").join("cache");
        Self::new(cache_root)
    }

    /// Set maximum cache size
    pub fn with_max_size(mut self, size_bytes: u64) -> Self {
        self.max_size = size_bytes;
        self
    }

    /// Get cache root path
    pub fn cache_root(&self) -> &Path {
        &self.cache_root
    }

    /// Load cache index from disk
    fn load_index(&self) -> Result<()> {
        let index_path = self.cache_root.join("index.json");

        if index_path.exists() {
            let content = std::fs::read_to_string(&index_path)?;

            #[derive(Deserialize)]
            struct CacheIndex {
                metadata: BTreeMap<String, CachedPackageMetadata>,
                packages: BTreeMap<String, CachedPackageData>,
                total_size: u64,
            }

            if let Ok(index) = serde_json::from_str::<CacheIndex>(&content) {
                *self.metadata_cache.write() = index.metadata;
                *self.package_index.write() = index.packages;
                *self.current_size.write() = index.total_size;
                debug!("Loaded cache index with {} metadata entries and {} packages",
                    self.metadata_cache.read().len(),
                    self.package_index.read().len()
                );
            }
        }

        Ok(())
    }

    /// Save cache index to disk
    fn save_index(&self) -> Result<()> {
        #[derive(Serialize)]
        struct CacheIndex<'a> {
            metadata: &'a BTreeMap<String, CachedPackageMetadata>,
            packages: &'a BTreeMap<String, CachedPackageData>,
            total_size: u64,
        }

        let index = CacheIndex {
            metadata: &self.metadata_cache.read(),
            packages: &self.package_index.read(),
            total_size: *self.current_size.read(),
        };

        let content = serde_json::to_string_pretty(&index)?;
        let index_path = self.cache_root.join("index.json");
        std::fs::write(&index_path, content)?;

        Ok(())
    }

    /// Generate cache key for a package
    fn cache_key(id: &PackageId, version: Option<&PackageVersion>) -> String {
        match version {
            Some(v) => format!("{}@{}", id, v),
            None => id.to_string(),
        }
    }

    /// Calculate SHA256 of data
    fn calculate_checksum(data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        hex::encode(hasher.finalize())
    }

    // ========== Metadata Cache Operations ==========

    /// Get package metadata from cache
    pub fn get_metadata(&self, id: &PackageId) -> Option<Package> {
        let key = Self::cache_key(id, None);
        let mut cache = self.metadata_cache.write();

        if let Some(entry) = cache.get_mut(&key) {
            if entry.meta.is_expired() {
                debug!(package = %id, "Metadata cache expired");
                cache.remove(&key);
                return None;
            }

            entry.meta.record_access();
            debug!(package = %id, "Metadata cache hit");
            return Some(entry.package.clone());
        }

        debug!(package = %id, "Metadata cache miss");
        None
    }

    /// Store package metadata in cache
    pub fn set_metadata(&self, package: &Package) -> Result<()> {
        let key = Self::cache_key(&package.metadata.id, None);
        let data = serde_json::to_vec(package)?;
        let checksum = Self::calculate_checksum(&data);
        let size = data.len() as u64;

        let entry = CachedPackageMetadata {
            package: package.clone(),
            meta: CacheEntryMeta::new(checksum, size, CacheLayer::Metadata.ttl()),
        };

        self.metadata_cache.write().insert(key.clone(), entry);

        // Write to disk
        let path = self.cache_root
            .join(CacheLayer::Metadata.dir_name())
            .join(format!("{}.json", package.metadata.id));
        std::fs::write(&path, &data)?;

        debug!(package = %package.metadata.id, "Cached metadata");

        Ok(())
    }

    // ========== Package Cache Operations ==========

    /// Get cached package file path
    pub fn get_package(&self, id: &PackageId, version: &PackageVersion) -> Option<PathBuf> {
        let key = Self::cache_key(id, Some(version));
        let mut index = self.package_index.write();

        if let Some(entry) = index.get_mut(&key) {
            if entry.meta.is_expired() {
                debug!(package = %id, version = %version, "Package cache expired");
                index.remove(&key);
                return None;
            }

            let file_path = self.cache_root.join(&entry.file_path);

            if file_path.exists() {
                // Verify checksum
                if let Ok(data) = std::fs::read(&file_path) {
                    let checksum = Self::calculate_checksum(&data);
                    if checksum == entry.meta.checksum {
                        entry.meta.record_access();
                        debug!(package = %id, version = %version, "Package cache hit");
                        return Some(file_path);
                    } else {
                        warn!(
                            package = %id,
                            version = %version,
                            expected = %entry.meta.checksum,
                            actual = %checksum,
                            "Package checksum mismatch, removing"
                        );
                        index.remove(&key);
                        let _ = std::fs::remove_file(&file_path);
                    }
                }
            } else {
                // File missing, remove from index
                index.remove(&key);
            }
        }

        debug!(package = %id, version = %version, "Package cache miss");
        None
    }

    /// Store package data in cache
    pub fn set_package(
        &self,
        id: &PackageId,
        version: &PackageVersion,
        data: &[u8],
    ) -> Result<PathBuf> {
        let key = Self::cache_key(id, Some(version));
        let checksum = Self::calculate_checksum(data);
        let size = data.len() as u64;

        // Check cache size limit
        self.ensure_space(size)?;

        // Create file path
        let relative_path = format!(
            "{}/{}/{}-{}.tar.gz",
            CacheLayer::Package.dir_name(),
            id,
            id,
            version
        );
        let file_path = self.cache_root.join(&relative_path);

        // Ensure parent directory exists
        if let Some(parent) = file_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        // Write file
        std::fs::write(&file_path, data)?;

        // Update index
        let entry = CachedPackageData {
            id: id.clone(),
            version: version.clone(),
            file_path: relative_path,
            meta: CacheEntryMeta::new(checksum, size, CacheLayer::Package.ttl()),
        };

        self.package_index.write().insert(key, entry);
        *self.current_size.write() += size;

        // Save index
        self.save_index()?;

        debug!(package = %id, version = %version, size = %size, "Cached package");

        Ok(file_path)
    }

    // ========== Cache Management ==========

    /// Ensure there's enough space for new data
    fn ensure_space(&self, needed: u64) -> Result<()> {
        let mut current = self.current_size.write();

        while *current + needed > self.max_size {
            // Evict least recently used entry
            if !self.evict_lru()? {
                return Err(Error::ConfigError(
                    "Cache is full and cannot evict entries".to_string()
                ));
            }
        }

        Ok(())
    }

    /// Evict least recently used entry
    fn evict_lru(&self) -> Result<bool> {
        // Find LRU in package cache (larger entries)
        let mut index = self.package_index.write();

        if let Some((key, entry)) = index
            .iter()
            .min_by_key(|(_, e)| e.meta.last_accessed)
            .map(|(k, e)| (k.clone(), e.clone()))
        {
            let file_path = self.cache_root.join(&entry.file_path);
            if file_path.exists() {
                std::fs::remove_file(&file_path)?;
            }
            *self.current_size.write() -= entry.meta.size;
            index.remove(&key);
            debug!(key = %key, "Evicted LRU entry");
            return Ok(true);
        }

        // Try metadata cache
        let mut cache = self.metadata_cache.write();

        if let Some((key, entry)) = cache
            .iter()
            .min_by_key(|(_, e)| e.meta.last_accessed)
            .map(|(k, e)| (k.clone(), e.clone()))
        {
            *self.current_size.write() -= entry.meta.size;
            cache.remove(&key);
            debug!(key = %key, "Evicted LRU metadata entry");
            return Ok(true);
        }

        Ok(false)
    }

    /// Clean up expired entries
    pub fn cleanup_expired(&self) -> Result<CleanupStats> {
        let mut removed_count = 0;
        let mut freed_bytes = 0u64;

        // Cleanup metadata cache
        {
            let mut cache = self.metadata_cache.write();
            let expired_keys: Vec<String> = cache
                .iter()
                .filter(|(_, e)| e.meta.is_expired())
                .map(|(k, _)| k.clone())
                .collect();

            for key in expired_keys {
                if let Some(entry) = cache.remove(&key) {
                    freed_bytes += entry.meta.size;
                    removed_count += 1;

                    // Remove file
                    let path = self.cache_root
                        .join(CacheLayer::Metadata.dir_name())
                        .join(format!("{}.json", key));
                    let _ = std::fs::remove_file(path);
                }
            }
        }

        // Cleanup package cache
        {
            let mut index = self.package_index.write();
            let expired_keys: Vec<String> = index
                .iter()
                .filter(|(_, e)| e.meta.is_expired())
                .map(|(k, _)| k.clone())
                .collect();

            for key in expired_keys {
                if let Some(entry) = index.remove(&key) {
                    freed_bytes += entry.meta.size;
                    removed_count += 1;

                    // Remove file
                    let file_path = self.cache_root.join(&entry.file_path);
                    let _ = std::fs::remove_file(file_path);
                }
            }
        }

        *self.current_size.write() -= freed_bytes;

        // Save updated index
        self.save_index()?;

        info!(
            removed = %removed_count,
            freed_mb = %(freed_bytes / 1024 / 1024),
            "Cache cleanup complete"
        );

        Ok(CleanupStats {
            removed_count,
            freed_bytes,
        })
    }

    /// Clear all cache
    pub fn clear(&self) -> Result<()> {
        // Clear in-memory caches
        self.metadata_cache.write().clear();
        self.package_index.write().clear();
        *self.current_size.write() = 0;

        // Remove cache directories
        let _ = std::fs::remove_dir_all(self.cache_root.join("metadata"));
        let _ = std::fs::remove_dir_all(self.cache_root.join("packages"));

        // Recreate empty directories
        std::fs::create_dir_all(self.cache_root.join("metadata"))?;
        std::fs::create_dir_all(self.cache_root.join("packages"))?;

        // Remove index
        let index_path = self.cache_root.join("index.json");
        let _ = std::fs::remove_file(index_path);

        info!("Cache cleared");

        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let metadata_count = self.metadata_cache.read().len();
        let package_count = self.package_index.read().len();
        let total_size = *self.current_size.read();

        let mut total_accesses = 0u64;

        for entry in self.metadata_cache.read().values() {
            total_accesses += entry.meta.access_count;
        }

        for entry in self.package_index.read().values() {
            total_accesses += entry.meta.access_count;
        }

        CacheStats {
            metadata_entries: metadata_count,
            package_entries: package_count,
            total_size_bytes: total_size,
            max_size_bytes: self.max_size,
            total_accesses,
        }
    }
}

/// Cache cleanup statistics
#[derive(Debug, Clone)]
pub struct CleanupStats {
    /// Number of entries removed
    pub removed_count: usize,
    /// Bytes freed
    pub freed_bytes: u64,
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    /// Number of metadata entries
    pub metadata_entries: usize,
    /// Number of package entries
    pub package_entries: usize,
    /// Total cache size in bytes
    pub total_size_bytes: u64,
    /// Maximum cache size in bytes
    pub max_size_bytes: u64,
    /// Total number of cache accesses
    pub total_accesses: u64,
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cache: {} metadata, {} packages, {:.2} MB / {:.2} MB, {} accesses",
            self.metadata_entries,
            self.package_entries,
            self.total_size_bytes as f64 / 1024.0 / 1024.0,
            self.max_size_bytes as f64 / 1024.0 / 1024.0,
            self.total_accesses
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::PackageMetadata;
    use tempfile::TempDir;

    fn create_test_package(id: &str) -> Package {
        let pkg_id = PackageId::new(id).unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let metadata = PackageMetadata::new(pkg_id.clone(), id, format!("Test {}", id), "MIT");

        Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version],
            releases: indexmap::IndexMap::new(),
        }
    }

    #[test]
    fn test_cache_entry_meta_expiry() {
        let meta = CacheEntryMeta::new(
            "test".to_string(),
            100,
            Duration::seconds(0),
        );

        // Should be expired immediately
        std::thread::sleep(std::time::Duration::from_millis(10));
        assert!(meta.is_expired());
    }

    #[test]
    fn test_cache_entry_meta_not_expired() {
        let meta = CacheEntryMeta::new(
            "test".to_string(),
            100,
            Duration::hours(1),
        );

        assert!(!meta.is_expired());
    }

    #[test]
    fn test_cache_layer_ttl() {
        assert_eq!(CacheLayer::Metadata.ttl(), Duration::hours(1));
        assert_eq!(CacheLayer::Package.ttl(), Duration::days(7));
    }

    #[test]
    fn test_checksum_calculation() {
        let data = b"test data";
        let checksum = MultiLayerCache::calculate_checksum(data);

        // SHA256 produces 64 hex characters
        assert_eq!(checksum.len(), 64);

        // Same data should produce same checksum
        let checksum2 = MultiLayerCache::calculate_checksum(data);
        assert_eq!(checksum, checksum2);

        // Different data should produce different checksum
        let checksum3 = MultiLayerCache::calculate_checksum(b"different data");
        assert_ne!(checksum, checksum3);
    }

    #[test]
    fn test_metadata_cache_operations() {
        let temp_dir = TempDir::new().unwrap();
        let cache = MultiLayerCache::new(temp_dir.path()).unwrap();

        let package = create_test_package("test-pkg");

        // Initially not cached
        let pkg_id = PackageId::new("test-pkg").unwrap();
        assert!(cache.get_metadata(&pkg_id).is_none());

        // Cache it
        cache.set_metadata(&package).unwrap();

        // Now should be cached
        let cached = cache.get_metadata(&pkg_id);
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().metadata.id, pkg_id);
    }

    #[test]
    fn test_package_cache_operations() {
        let temp_dir = TempDir::new().unwrap();
        let cache = MultiLayerCache::new(temp_dir.path()).unwrap();

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let data = b"fake package data";

        // Initially not cached
        assert!(cache.get_package(&pkg_id, &version).is_none());

        // Cache it
        let path = cache.set_package(&pkg_id, &version, data).unwrap();
        assert!(path.exists());

        // Now should be cached
        let cached_path = cache.get_package(&pkg_id, &version);
        assert!(cached_path.is_some());
        assert_eq!(cached_path.unwrap(), path);
    }

    #[test]
    fn test_cache_checksum_validation() {
        let temp_dir = TempDir::new().unwrap();
        let cache = MultiLayerCache::new(temp_dir.path()).unwrap();

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let data = b"original data";

        // Cache the package
        let path = cache.set_package(&pkg_id, &version, data).unwrap();

        // Corrupt the file
        std::fs::write(&path, b"corrupted data").unwrap();

        // Should fail validation and return None
        assert!(cache.get_package(&pkg_id, &version).is_none());
    }

    #[test]
    fn test_cache_stats() {
        let temp_dir = TempDir::new().unwrap();
        let cache = MultiLayerCache::new(temp_dir.path()).unwrap();

        let stats = cache.stats();
        assert_eq!(stats.metadata_entries, 0);
        assert_eq!(stats.package_entries, 0);

        // Add some entries
        let package = create_test_package("test-pkg");
        cache.set_metadata(&package).unwrap();

        let pkg_id = PackageId::new("test-pkg-2").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        cache.set_package(&pkg_id, &version, b"data").unwrap();

        let stats = cache.stats();
        assert_eq!(stats.metadata_entries, 1);
        assert_eq!(stats.package_entries, 1);
    }

    #[test]
    fn test_cache_clear() {
        let temp_dir = TempDir::new().unwrap();
        let cache = MultiLayerCache::new(temp_dir.path()).unwrap();

        // Add entries
        let package = create_test_package("test-pkg");
        cache.set_metadata(&package).unwrap();

        let pkg_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        cache.set_package(&pkg_id, &version, b"data").unwrap();

        // Clear
        cache.clear().unwrap();

        // Should be empty
        let stats = cache.stats();
        assert_eq!(stats.metadata_entries, 0);
        assert_eq!(stats.package_entries, 0);
    }
}
