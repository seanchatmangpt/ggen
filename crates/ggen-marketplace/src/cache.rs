//! Pack cache with LRU eviction and metadata tracking.
//!
//! Features:
//! - LRU (Least Recently Used) eviction policy
//! - SHA-256 digest verification
//! - Metadata tracking (download time, access time, size)
//! - Persistent storage on disk
//! - Atomic operations for consistency

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;
use std::sync::{Arc, RwLock};
use tracing::{debug, info, warn};

use crate::error::{Error, Result};
use crate::models::{PackageId, PackageVersion};

/// Cached pack metadata
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CachedPack {
    /// Package ID
    pub package_id: PackageId,
    /// Package version
    pub version: PackageVersion,
    /// SHA-256 digest of the pack contents
    pub digest: String,
    /// Size in bytes
    pub size_bytes: u64,
    /// When the pack was downloaded
    pub downloaded_at: DateTime<Utc>,
    /// When the pack was last accessed
    pub last_accessed: DateTime<Utc>,
    /// Path to cached files
    pub cache_path: PathBuf,
    /// Number of times this pack has been accessed
    pub access_count: u64,
}

impl CachedPack {
    /// Create a new cached pack entry
    #[must_use]
    pub fn new(
        package_id: PackageId, version: PackageVersion, digest: String, size_bytes: u64,
        cache_path: PathBuf,
    ) -> Self {
        let now = Utc::now();
        Self {
            package_id,
            version,
            digest,
            size_bytes,
            downloaded_at: now,
            last_accessed: now,
            cache_path,
            access_count: 0,
        }
    }

    /// Update last accessed time and increment access count
    pub fn record_access(&mut self) {
        self.last_accessed = Utc::now();
        self.access_count += 1;
    }

    /// Get the cache key for this pack
    #[must_use]
    pub fn cache_key(&self) -> String {
        format!("{}@{}", self.package_id, self.version)
    }
}

/// Cache configuration
#[derive(Clone, Debug)]
pub struct CacheConfig {
    /// Maximum cache size in bytes
    pub max_size_bytes: u64,
    /// Maximum number of packs to store
    pub max_packs: usize,
    /// Cache directory path
    pub cache_dir: PathBuf,
    /// Whether to persist cache metadata to disk
    pub persistent: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            max_size_bytes: 2_000_000_000, // 2GB default
            max_packs: 100,
            cache_dir: dirs::cache_dir()
                .unwrap_or_else(|| PathBuf::from(".cache"))
                .join("ggen")
                .join("packs"),
            persistent: true,
        }
    }
}

/// Pack cache with LRU eviction
pub struct PackCache {
    /// Cache configuration
    config: CacheConfig,
    /// Cached packs indexed by cache key
    packs: Arc<RwLock<HashMap<String, CachedPack>>>,
    /// Current total cache size in bytes
    current_size: Arc<RwLock<u64>>,
}

impl PackCache {
    /// Create a new pack cache
    ///
    /// # Errors
    ///
    /// Returns error if cache directory cannot be created or metadata cannot be loaded.
    pub fn new(config: CacheConfig) -> Result<Self> {
        // Create cache directory if it doesn't exist
        fs::create_dir_all(&config.cache_dir)?;

        let packs = Arc::new(RwLock::new(HashMap::new()));
        let current_size = Arc::new(RwLock::new(0));

        let cache = Self {
            config,
            packs,
            current_size,
        };

        // Load persistent metadata if enabled
        if cache.config.persistent {
            cache.load_metadata()?;
        }

        info!("Pack cache initialized at {:?}", cache.config.cache_dir);
        Ok(cache)
    }

    /// Create a cache with default configuration
    ///
    /// # Errors
    ///
    /// Returns error if cache directory cannot be created or metadata cannot be loaded.
    pub fn with_default_config() -> Result<Self> {
        Self::new(CacheConfig::default())
    }

    /// Get a pack from cache
    ///
    /// # Errors
    ///
    /// Returns error if cache lock is poisoned.
    pub fn get(&self, package_id: &PackageId, version: &PackageVersion) -> Option<CachedPack> {
        let cache_key = format!("{}@{}", package_id, version);

        let mut packs = self.packs.write().unwrap();
        if let Some(mut pack) = packs.remove(&cache_key) {
            // Update access statistics
            pack.record_access();
            debug!(
                "Cache hit: {} (accessed {} times)",
                cache_key, pack.access_count
            );
            packs.insert(cache_key, pack.clone());
            Some(pack)
        } else {
            debug!("Cache miss: {}", cache_key);
            None
        }
    }

    /// Insert a pack into cache
    ///
    /// # Errors
    ///
    /// Returns error if cache lock is poisoned or eviction fails.
    pub fn insert(&self, pack: CachedPack) -> Result<()> {
        let cache_key = pack.cache_key();

        // Check if pack already exists
        let existing_size = {
            let packs = self.packs.read().unwrap();
            if let Some(existing) = packs.get(&cache_key) {
                existing.size_bytes
            } else {
                0
            }
        };

        // Evict packs if necessary to make room
        let new_size = *self.current_size.read().unwrap() - existing_size + pack.size_bytes;
        {
            let packs = self.packs.read().unwrap();
            if new_size > self.config.max_size_bytes || packs.len() >= self.config.max_packs {
                drop(packs);
                self.evict_if_needed(pack.size_bytes)?;
            }
        }

        // Insert the pack
        let size_bytes = pack.size_bytes;
        let cache_key_clone = cache_key.clone();

        let mut packs = self.packs.write().unwrap();
        let mut current_size = self.current_size.write().unwrap();
        *current_size = *current_size - existing_size + pack.size_bytes;

        packs.insert(cache_key, pack);

        info!(
            "Cached pack: {} (size: {} bytes, total: {} bytes)",
            cache_key_clone,
            size_bytes,
            *current_size
        );

        // Persist metadata if enabled
        if self.config.persistent {
            drop(packs);
            drop(current_size);
            self.save_metadata()?;
        }

        Ok(())
    }

    /// Evict packs if necessary to make room for a new pack
    ///
    /// # Errors
    ///
    /// Returns error if cache lock is poisoned or file deletion fails.
    fn evict_if_needed(&self, required_size: u64) -> Result<()> {
        let mut packs = self.packs.write().unwrap();
        let mut current_size = self.current_size.write().unwrap();

        // Check if eviction is needed
        let size_pressure = *current_size + required_size > self.config.max_size_bytes;
        let count_pressure = packs.len() >= self.config.max_packs;

        if !size_pressure && !count_pressure {
            return Ok(());
        }

        // Sort packs by last accessed time (LRU)
        let mut packs_vec: Vec<(String, CachedPack)> = packs.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        packs_vec.sort_by(|a, b| a.1.last_accessed.cmp(&b.1.last_accessed));

        let mut evicted = 0;
        let mut freed_bytes = 0;

        for (cache_key, pack) in packs_vec {
            let within_size =
                *current_size + required_size - freed_bytes <= self.config.max_size_bytes;
            // Leave room for one new entry after this eviction pass (strict `<`, not `<=`).
            let within_count = packs.len() < self.config.max_packs;
            if within_size && within_count {
                break;
            }

            // Remove pack from cache
            packs.remove(&cache_key);
            *current_size -= pack.size_bytes;
            freed_bytes += pack.size_bytes;
            evicted += 1;

            // Delete cached files
            if pack.cache_path.exists() {
                fs::remove_dir_all(&pack.cache_path).map_err(|e| {
                    Error::InstallationFailed {
                        reason: format!("Failed to remove cached pack directory: {}", e),
                    }
                })?;
            }

            info!(
                "Evicted pack: {} (freed {} bytes)",
                cache_key, pack.size_bytes
            );
        }

        debug!(
            "Evicted {} packs, freed {} bytes",
            evicted, freed_bytes
        );

        Ok(())
    }

    /// Remove a pack from cache
    ///
    /// # Errors
    ///
    /// Returns error if cache lock is poisoned or file deletion fails.
    pub fn remove(&self, package_id: &PackageId, version: &PackageVersion) -> Result<bool> {
        let cache_key = format!("{}@{}", package_id, version);

        let mut packs = self.packs.write().unwrap();
        let mut current_size = self.current_size.write().unwrap();

        if let Some(pack) = packs.remove(&cache_key) {
            *current_size -= pack.size_bytes;

            // Delete cached files
            if pack.cache_path.exists() {
                fs::remove_dir_all(&pack.cache_path).map_err(|e| {
                    Error::InstallationFailed {
                        reason: format!("Failed to remove cached pack directory: {}", e),
                    }
                })?;
            }

            info!("Removed pack from cache: {}", cache_key);

            // Persist metadata if enabled
            if self.config.persistent {
                drop(packs);
                drop(current_size);
                self.save_metadata()?;
            }

            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Clear all packs from cache
    ///
    /// # Errors
    ///
    /// Returns error if file operations fail.
    pub fn clear(&self) -> Result<()> {
        let mut packs = self.packs.write().unwrap();
        let mut current_size = self.current_size.write().unwrap();

        // Delete all cached pack directories
        for pack in packs.values() {
            if pack.cache_path.exists() {
                fs::remove_dir_all(&pack.cache_path).map_err(|e| {
                    Error::InstallationFailed {
                        reason: format!("Failed to remove cached pack directory: {}", e),
                    }
                })?;
            }
        }

        packs.clear();
        *current_size = 0;

        info!("Cache cleared");

        // Persist metadata if enabled
        if self.config.persistent {
            drop(packs);
            drop(current_size);
            self.save_metadata()?;
        }

        Ok(())
    }

    /// Get cache statistics
    ///
    /// # Errors
    ///
    /// Returns error if cache lock is poisoned.
    pub fn stats(&self) -> CacheStats {
        let packs = self.packs.read().unwrap();
        let current_size = *self.current_size.read().unwrap();

        CacheStats {
            total_packs: packs.len(),
            total_size_bytes: current_size,
            max_size_bytes: self.config.max_size_bytes,
            max_packs: self.config.max_packs,
            utilization_percent: if self.config.max_size_bytes > 0 {
                (current_size as f64 / self.config.max_size_bytes as f64) * 100.0
            } else {
                0.0
            },
        }
    }

    /// Save cache metadata to disk
    ///
    /// # Errors
    ///
    /// Returns error if serialization or file I/O fails.
    fn save_metadata(&self) -> Result<()> {
        let metadata_path = self.config.cache_dir.join("cache_metadata.json");
        let packs = self.packs.read().unwrap();

        let file = fs::File::create(&metadata_path).map_err(|e| Error::IoError(e))?;
        let writer = BufWriter::new(file);
        serde_json::to_writer(writer, &*packs).map_err(|e| Error::SerializationError(e))?;

        debug!("Saved cache metadata to {:?}", metadata_path);
        Ok(())
    }

    /// Load cache metadata from disk
    ///
    /// # Errors
    ///
    /// Returns error if deserialization or file I/O fails.
    fn load_metadata(&self) -> Result<()> {
        let metadata_path = self.config.cache_dir.join("cache_metadata.json");

        if !metadata_path.exists() {
            debug!("No existing cache metadata found");
            return Ok(());
        }

        let file = fs::File::open(&metadata_path).map_err(|e| Error::IoError(e))?;
        let reader = BufReader::new(file);
        let loaded_packs: HashMap<String, CachedPack> =
            serde_json::from_reader(reader).map_err(|e| Error::SerializationError(e))?;

        let mut packs = self.packs.write().unwrap();
        let mut current_size = self.current_size.write().unwrap();

        *current_size = 0;
        for (key, pack) in loaded_packs {
            *current_size += pack.size_bytes;
            packs.insert(key, pack);
        }

        info!(
            "Loaded cache metadata: {} packs, {} bytes",
            packs.len(),
            *current_size
        );

        Ok(())
    }

    /// Check if a pack is cached
    ///
    /// # Returns
    ///
    /// `true` if the pack exists in cache, `false` otherwise
    #[must_use]
    pub fn is_cached(&self, package_id: &PackageId, version: &PackageVersion) -> bool {
        let cache_key = format!("{}@{}", package_id, version);
        let packs = self.packs.read().unwrap();
        packs.contains_key(&cache_key)
    }

    /// Get all cached pack keys
    ///
    /// # Returns
    ///
    /// Vector of cache keys for all cached packs
    #[must_use]
    pub fn cached_packs(&self) -> Vec<String> {
        let packs = self.packs.read().unwrap();
        packs.keys().cloned().collect()
    }

    /// Verify that a cached pack's digest matches
    ///
    /// # Errors
    ///
    /// Returns error if digest verification fails.
    pub fn verify_digest(&self, pack: &CachedPack) -> Result<bool> {
        use sha2::{Digest, Sha256};

        let mut hasher = Sha256::new();
        let mut verified = true;

        // Walk the pack directory and hash all files
        if pack.cache_path.exists() {
            for entry in walkdir::WalkDir::new(&pack.cache_path)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.file_type().is_file() {
                    if let Ok(contents) = fs::read(entry.path()) {
                        hasher.update(&contents);
                    } else {
                        verified = false;
                        break;
                    }
                }
            }
        }

        let calculated_digest = hex::encode(hasher.finalize());
        let matches = calculated_digest == pack.digest;

        if !matches {
            warn!(
                "Digest mismatch for {}: expected {}, got {}",
                pack.cache_key(),
                pack.digest,
                calculated_digest
            );
        }

        Ok(verified && matches)
    }
}

/// Cache statistics
#[derive(Clone, Debug)]
pub struct CacheStats {
    /// Total number of packs in cache
    pub total_packs: usize,
    /// Total size of all cached packs in bytes
    pub total_size_bytes: u64,
    /// Maximum cache size in bytes
    pub max_size_bytes: u64,
    /// Maximum number of packs
    pub max_packs: usize,
    /// Cache utilization percentage
    pub utilization_percent: f64,
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Cache Statistics")?;
        writeln!(f, "  Packs: {} / {}", self.total_packs, self.max_packs)?;
        writeln!(
            f,
            "  Size: {} MB / {} MB ({:.1}%)",
            self.total_size_bytes / (1024 * 1024),
            self.max_size_bytes / (1024 * 1024),
            self.utilization_percent
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageId, PackageVersion};
    use tempfile::TempDir;

    #[test]
    fn test_cache_config_default() {
        let config = CacheConfig::default();
        assert_eq!(config.max_size_bytes, 2_000_000_000);
        assert_eq!(config.max_packs, 100);
        assert!(config.persistent);
    }

    #[test]
    fn test_cached_pack_creation() {
        let temp_dir = TempDir::new().unwrap();
        let cache_path = temp_dir.path().join("test-pack");

        let package_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let digest = "abc123".to_string();

        let pack = CachedPack::new(
            package_id.clone(),
            version.clone(),
            digest.clone(),
            1024,
            cache_path.clone(),
        );

        assert_eq!(pack.package_id, package_id);
        assert_eq!(pack.version, version);
        assert_eq!(pack.digest, digest);
        assert_eq!(pack.size_bytes, 1024);
        assert_eq!(pack.access_count, 0);
        assert_eq!(pack.cache_key(), "test-pkg@1.0.0");
    }

    #[test]
    fn test_cached_pack_record_access() {
        let temp_dir = TempDir::new().unwrap();
        let cache_path = temp_dir.path().join("test-pack");

        let package_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();

        let mut pack = CachedPack::new(
            package_id,
            version,
            "abc123".to_string(),
            1024,
            cache_path,
        );

        assert_eq!(pack.access_count, 0);
        pack.record_access();
        assert_eq!(pack.access_count, 1);
        pack.record_access();
        assert_eq!(pack.access_count, 2);
    }

    #[test]
    fn test_pack_cache_insert_and_get() {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };

        let cache = PackCache::new(config).unwrap();

        let package_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let cache_path = temp_dir.path().join("test-pack");

        let pack = CachedPack::new(
            package_id.clone(),
            version.clone(),
            "abc123".to_string(),
            1024,
            cache_path,
        );

        cache.insert(pack.clone()).unwrap();

        let retrieved = cache.get(&package_id, &version);
        assert!(retrieved.is_some());
        let retrieved_pack = retrieved.unwrap();
        assert_eq!(retrieved_pack.package_id, package_id);
        assert_eq!(retrieved_pack.version, version);
        assert_eq!(retrieved_pack.access_count, 1); // Should be incremented on get
    }

    #[test]
    fn test_pack_cache_remove() {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            ..Default::default()
        };

        let cache = PackCache::new(config).unwrap();

        let package_id = PackageId::new("test-pkg").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let cache_path = temp_dir.path().join("test-pack");

        let pack = CachedPack::new(
            package_id.clone(),
            version.clone(),
            "abc123".to_string(),
            1024,
            cache_path,
        );

        cache.insert(pack).unwrap();

        let removed = cache.remove(&package_id, &version).unwrap();
        assert!(removed);

        let retrieved = cache.get(&package_id, &version);
        assert!(retrieved.is_none());
    }

    #[test]
    fn test_pack_cache_stats() {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            max_size_bytes: 10_000,
            max_packs: 5,
            ..Default::default()
        };

        let cache = PackCache::new(config).unwrap();

        let stats = cache.stats();
        assert_eq!(stats.total_packs, 0);
        assert_eq!(stats.total_size_bytes, 0);
        assert_eq!(stats.max_size_bytes, 10_000);
        assert_eq!(stats.max_packs, 5);
    }

    #[test]
    fn test_pack_cache_lru_eviction() {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            cache_dir: temp_dir.path().join("cache"),
            max_size_bytes: 3000, // Only fits 3 packs of 1000 bytes each
            max_packs: 10,
            ..Default::default()
        };

        let cache = PackCache::new(config).unwrap();

        // Insert 3 packs
        for i in 1..=3 {
            let package_id = PackageId::new(&format!("test-pkg-{}", i)).unwrap();
            let version = PackageVersion::new("1.0.0").unwrap();
            let cache_path = temp_dir.path().join(&format!("pack-{}", i));

            let pack = CachedPack::new(
                package_id,
                version,
                format!("digest{}", i),
                1000,
                cache_path,
            );

            cache.insert(pack).unwrap();
        }

        // Access pack 1 to make it more recently used
        let package_id = PackageId::new("test-pkg-1").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        cache.get(&package_id, &version);

        // Insert a 4th pack (should evict pack 2, which is least recently used)
        let package_id = PackageId::new("test-pkg-4").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let cache_path = temp_dir.path().join("pack-4");

        let pack = CachedPack::new(
            package_id.clone(),
            version.clone(),
            "digest4".to_string(),
            1000,
            cache_path,
        );

        cache.insert(pack).unwrap();

        let stats = cache.stats();
        assert_eq!(stats.total_packs, 3); // Should still be 3 packs
        assert_eq!(stats.total_size_bytes, 3000);

        // Pack 1 should still be there (was accessed)
        let package_id1 = PackageId::new("test-pkg-1").unwrap();
        let version1 = PackageVersion::new("1.0.0").unwrap();
        assert!(cache.get(&package_id1, &version1).is_some());

        // Pack 4 should be there
        assert!(cache.get(&package_id, &version).is_some());

        // Pack 2 should have been evicted
        let package_id2 = PackageId::new("test-pkg-2").unwrap();
        let version2 = PackageVersion::new("1.0.0").unwrap();
        assert!(cache.get(&package_id2, &version2).is_none());
    }
}
