//! High-performance async registry with concurrent access patterns
//!
//! Uses DashMap for lock-free concurrent access and advanced async patterns.

use async_trait::async_trait;
use dashmap::DashMap;
use moka::future::Cache as AsyncCache;
use std::sync::Arc;
use tracing::{debug, info, warn};

use crate::error::Result;
use crate::models::{Package, PackageId, PackageVersion};
use crate::traits::AsyncRepository;

/// High-performance marketplace registry
///
/// Features:
/// - Lock-free concurrent access (DashMap)
/// - LRU caching with moka
/// - Async I/O operations
/// - Distributed tracing support
pub struct Registry {
    // Primary storage: concurrent hash map
    packages: Arc<DashMap<PackageId, Package>>,

    // Secondary storage: version index for faster lookups
    version_index: Arc<DashMap<String, PackageId>>,

    // LRU cache for frequent queries
    query_cache: Arc<AsyncCache<String, Vec<Package>>>,

    // Metrics
    cache_hits: Arc<std::sync::atomic::AtomicU64>,
    cache_misses: Arc<std::sync::atomic::AtomicU64>,
}

impl Registry {
    /// Create a new registry
    pub async fn new(cache_size: u64) -> Self {
        let cache = AsyncCache::new(cache_size);

        Self {
            packages: Arc::new(DashMap::new()),
            version_index: Arc::new(DashMap::new()),
            query_cache: Arc::new(cache),
            cache_hits: Arc::new(std::sync::atomic::AtomicU64::new(0)),
            cache_misses: Arc::new(std::sync::atomic::AtomicU64::new(0)),
        }
    }

    /// Insert a package into the registry
    ///
    /// # Errors
    ///
    /// This function currently always returns `Ok(())`. Future implementations may return:
    /// * [`Error::PackageAlreadyExists`] - When a package with the same ID already exists
    /// * [`Error::InvalidPackageId`] - When the package ID format is invalid
    #[must_use]
    pub fn insert(&self, package: Package) -> Result<()> {
        let package_id = package.metadata.id.clone();

        // Add to primary storage
        self.packages.insert(package_id.clone(), package.clone());
        debug!("Inserted package: {}", package_id);

        // Add to version index for all versions
        for version in &package.versions {
            let key = format!("{}@{}", package_id, version);
            self.version_index.insert(key, package_id.clone());
        }

        // Invalidate relevant caches
        self.query_cache.invalidate_all();

        Ok(())
    }

    /// Remove a package from the registry
    ///
    /// # Errors
    ///
    /// This function currently always returns `Ok`. Future implementations may return:
    /// * [`Error::PackageNotFound`] - When attempting to remove a non-existent package
    #[must_use]
    pub fn remove(&self, id: &PackageId) -> Result<Option<Package>> {
        let removed = self.packages.remove(id).map(|(_, pkg)| pkg);

        if removed.is_some() {
            debug!("Removed package: {}", id);
            // Remove from version index
            let mut keys_to_remove = Vec::new();
            for entry in self.version_index.iter() {
                if entry.value() == id {
                    keys_to_remove.push(entry.key().clone());
                }
            }
            for key in keys_to_remove {
                self.version_index.remove(&key);
            }

            // Invalidate caches
            self.query_cache.invalidate_all();
        }

        Ok(removed)
    }

    /// Update a package
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package ID does not exist in the registry
    #[must_use]
    pub fn update(&self, id: &PackageId, package: Package) -> Result<()> {
        if !self.packages.contains_key(id) {
            return Err(crate::error::Error::package_not_found(id.to_string()));
        }

        self.packages.insert(id.clone(), package);
        debug!("Updated package: {}", id);

        // Invalidate caches
        self.query_cache.invalidate_all();

        Ok(())
    }

    /// Get cache statistics
    #[must_use]
    pub fn cache_stats(&self) -> CacheStats {
        let hits = self.cache_hits.load(std::sync::atomic::Ordering::Relaxed);
        let misses = self.cache_misses.load(std::sync::atomic::Ordering::Relaxed);

        #[allow(clippy::cast_precision_loss)]
        let hit_rate = if hits + misses > 0 {
            hits as f64 / (hits + misses) as f64
        } else {
            0.0
        };

        CacheStats {
            hits,
            misses,
            hit_rate,
        }
    }

    /// Number of packages in registry
    #[must_use]
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Check if registry is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }

    /// Clear the entire registry
    pub fn clear(&self) {
        self.packages.clear();
        self.version_index.clear();
        self.query_cache.invalidate_all();
        info!("Registry cleared");
    }
}

#[async_trait]
impl AsyncRepository for Registry {
    type PackageIterator = std::vec::IntoIter<Package>;

    /// Get a package by ID
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package ID does not exist in the registry
    #[must_use]
    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        // Check if in primary storage
        if let Some(package) = self.packages.get(id) {
            self.cache_hits
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            debug!("Cache hit for package: {}", id);
            return Ok(package.clone());
        }

        self.cache_misses
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        warn!("Package not found: {}", id);
        Err(crate::error::Error::package_not_found(id.to_string()))
    }

    /// Get a package with only the specified version
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package ID does not exist in the registry
    /// * [`Error::InvalidVersion`] - When the version does not exist for the package
    #[must_use]
    async fn get_package_version(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Package> {
        let mut package = self.get_package(id).await?;

        // Filter to only the requested version
        if !package.versions.contains(version) {
            return Err(crate::error::Error::InvalidVersion {
                version: version.to_string(),
                reason: format!("Version not found for package {}", id),
            });
        }

        // Keep only the requested version in releases
        let mut releases = indexmap::IndexMap::new();
        if let Some(release) = package.releases.shift_remove(version) {
            releases.insert(version.clone(), release);
        }
        package.releases = releases;
        package.versions = vec![version.clone()];

        Ok(package)
    }

    /// Get all packages in the registry
    ///
    /// # Errors
    ///
    /// This function currently always returns `Ok`. Future implementations may return:
    /// * [`Error::RegistryError`] - When the registry is unavailable or corrupted
    /// * [`Error::ConcurrencyError`] - When concurrent access causes inconsistency
    #[must_use]
    async fn all_packages(&self) -> Result<Vec<Package>> {
        // Try cache first
        let cache_key = "all_packages".to_string();
        if let Some(cached) = self.query_cache.get(&cache_key).await {
            self.cache_hits
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            debug!("Cache hit for all_packages");
            return Ok(cached);
        }

        self.cache_misses
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Fetch from primary storage
        let packages: Vec<Package> = self
            .packages
            .iter()
            .map(|entry| entry.value().clone())
            .collect();

        // Cache result
        if packages.len() < 10000 {
            // Only cache if reasonable size
            self.query_cache.insert(cache_key, packages.clone()).await;
        }

        Ok(packages)
    }

    /// List all versions of a package
    ///
    /// # Errors
    ///
    /// * [`Error::PackageNotFound`] - When the package ID does not exist in the registry
    #[must_use]
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>> {
        let package = self.get_package(id).await?;
        Ok(package.versions)
    }

    /// Check if a package exists in the registry
    ///
    /// # Errors
    ///
    /// This function currently always returns `Ok`. Future implementations may return:
    /// * [`Error::RegistryError`] - When the registry is unavailable
    #[must_use]
    async fn package_exists(&self, id: &PackageId) -> Result<bool> {
        Ok(self.packages.contains_key(id))
    }
}

/// Registry cache statistics
#[derive(Clone, Debug)]
pub struct CacheStats {
    /// Number of cache hits
    pub hits: u64,
    /// Number of cache misses
    pub misses: u64,
    /// Cache hit rate (0.0-1.0)
    pub hit_rate: f64,
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cache stats: {} hits, {} misses, {:.2}% hit rate",
            self.hits,
            self.misses,
            self.hit_rate * 100.0
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageId, PackageMetadata};

    #[tokio::test]
    async fn test_registry_insert_and_get() {
        let registry = Registry::new(100).await;

        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id.clone(), "Test Package", "A test package", "MIT");
        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        };

        registry.insert(package.clone()).unwrap();
        let retrieved = registry.get_package(&id).await.unwrap();

        assert_eq!(retrieved.metadata.id, id);
    }

    #[tokio::test]
    async fn test_registry_remove() {
        let registry = Registry::new(100).await;

        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id.clone(), "Test Package", "A test package", "MIT");
        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        };

        registry.insert(package).unwrap();
        let removed = registry.remove(&id).unwrap();

        assert!(removed.is_some());
        assert!(registry.get_package(&id).await.is_err());
    }

    #[tokio::test]
    async fn test_cache_stats() {
        let registry = Registry::new(100).await;
        let stats = registry.cache_stats();

        assert_eq!(stats.hits, 0);
        assert_eq!(stats.misses, 0);
    }
}
