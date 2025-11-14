//! Smart caching system for marketplace operations
//!
//! This module provides a high-performance caching layer using the `moka` crate
//! for caching package metadata, search results, download counts, and version
//! information. It includes automatic expiration, hit/miss statistics, and
//! cache warming strategies.
//!
//! ## Features
//!
//! - **Multi-tier caching**: Separate caches for packages, searches, downloads, versions
//! - **TTL and TTI**: Time-to-live and time-to-idle expiration policies
//! - **Statistics**: Track cache hits, misses, and hit rates
//! - **Cache warming**: Pre-populate cache with popular packages and searches
//! - **Memory management**: Monitor and manage cache memory usage
//! - **Async operations**: Full async/await support for non-blocking operations
//!
//! ## Cache Types
//!
//! - **Package Cache**: Metadata for individual packages (1 hour TTL)
//! - **Search Cache**: Cached search results (10 minutes TTL)
//! - **Download Cache**: Download counts (5 minutes TTL)
//! - **Version Cache**: Available versions for packages (30 minutes TTL)
//!
//! ## Examples
//!
//! ### Basic Cache Usage
//!
//! ```rust,no_run
//! use ggen_marketplace::cache::SmartCache;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let cache = SmartCache::new();
//!
//! // Cache a package
//! let package = Package {
//!     id: "io.ggen.rust.api".to_string(),
//!     name: "Rust API".to_string(),
//!     version: "1.0.0".to_string(),
//!     description: "Rust API templates".to_string(),
//!     downloads: 1000,
//! };
//! cache.set_package(package.id.clone(), package.clone()).await;
//!
//! // Retrieve from cache
//! let cached = cache.get_package(&package.id).await;
//! # Ok(())
//! # }
//! ```
//!
//! ### Cache Statistics
//!
//! ```rust,no_run
//! use ggen_marketplace::cache::SmartCache;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let cache = SmartCache::new();
//! // ... use cache ...
//!
//! let stats = cache.get_stats().await;
//! println!("Hit rate: {:.2}%", stats.hit_rate * 100.0);
//! println!("Total entries: {}", stats.entries);
//! # Ok(())
//! # }
//! ```

#![allow(clippy::unwrap_used)] // Test code uses unwrap
// Smart Caching using moka
use anyhow::Result;
use moka::future::Cache;
use serde::{Deserialize, Serialize};
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheStats {
    pub hits: u64,
    pub misses: u64,
    pub entries: u64,
    pub hit_rate: f64,
}

pub type PackageId = String;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub id: PackageId,
    pub name: String,
    pub version: String,
    pub description: String,
    pub downloads: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct SearchQuery {
    pub query: String,
    pub filters: Vec<String>,
    pub limit: usize,
}

pub struct SmartCache {
    // Package metadata cache
    packages: Cache<PackageId, Arc<Package>>,

    // Search results cache
    search_results: Cache<SearchQuery, Arc<Vec<Package>>>,

    // Download counts cache (frequently accessed)
    download_counts: Cache<PackageId, u64>,

    // Version info cache
    versions: Cache<PackageId, Arc<Vec<String>>>,

    // Statistics
    stats: Arc<tokio::sync::RwLock<CacheStatsInternal>>,
}

#[derive(Debug, Default)]
struct CacheStatsInternal {
    package_hits: u64,
    package_misses: u64,
    search_hits: u64,
    search_misses: u64,
}

impl SmartCache {
    pub fn new() -> Self {
        /// Package cache TTL: 1 hour
        const PACKAGE_CACHE_TTL_SECS: u64 = 3600;
        /// Package cache TTI: 30 minutes
        const PACKAGE_CACHE_TTI_SECS: u64 = 1800;
        /// Search results cache TTL: 10 minutes
        const SEARCH_CACHE_TTL_SECS: u64 = 600;
        /// Search results cache TTI: 5 minutes
        const SEARCH_CACHE_TTI_SECS: u64 = 300;
        /// Download counts cache TTL: 5 minutes
        const DOWNLOAD_CACHE_TTL_SECS: u64 = 300;
        /// Version cache TTL: 30 minutes
        const VERSION_CACHE_TTL_SECS: u64 = 1800;
        
        Self {
            packages: Cache::builder()
                .max_capacity(10_000)
                .time_to_live(Duration::from_secs(PACKAGE_CACHE_TTL_SECS))
                .time_to_idle(Duration::from_secs(PACKAGE_CACHE_TTI_SECS))
                .build(),

            search_results: Cache::builder()
                .max_capacity(5_000)
                .time_to_live(Duration::from_secs(SEARCH_CACHE_TTL_SECS))
                .time_to_idle(Duration::from_secs(SEARCH_CACHE_TTI_SECS))
                .build(),

            download_counts: Cache::builder()
                .max_capacity(50_000)
                .time_to_live(Duration::from_secs(DOWNLOAD_CACHE_TTL_SECS))
                .build(),

            versions: Cache::builder()
                .max_capacity(10_000)
                .time_to_live(Duration::from_secs(VERSION_CACHE_TTL_SECS))
                .build(),

            stats: Arc::new(tokio::sync::RwLock::new(CacheStatsInternal::default())),
        }
    }

    pub fn with_custom_config(
        package_capacity: u64,
        search_capacity: u64,
        ttl_seconds: u64,
    ) -> Self {
        Self {
            packages: Cache::builder()
                .max_capacity(package_capacity)
                .time_to_live(Duration::from_secs(ttl_seconds))
                .build(),

            search_results: Cache::builder()
                .max_capacity(search_capacity)
                .time_to_live(Duration::from_secs(ttl_seconds))
                .build(),

            download_counts: Cache::builder()
                .max_capacity(package_capacity * 5)
                .time_to_live(Duration::from_secs(ttl_seconds / 2))
                .build(),

            versions: Cache::builder()
                .max_capacity(package_capacity)
                .time_to_live(Duration::from_secs(ttl_seconds))
                .build(),

            stats: Arc::new(tokio::sync::RwLock::new(CacheStatsInternal::default())),
        }
    }

    // Package cache operations
    pub async fn get_package(&self, id: &PackageId) -> Option<Arc<Package>> {
        let result = self.packages.get(id).await;

        let mut stats = self.stats.write().await;
        if result.is_some() {
            stats.package_hits += 1;
        } else {
            stats.package_misses += 1;
        }

        result
    }

    pub async fn set_package(&self, id: PackageId, package: Package) {
        self.packages.insert(id, Arc::new(package)).await;
    }

    pub async fn invalidate_package(&self, id: &PackageId) {
        self.packages.invalidate(id).await;
    }

    // Search cache operations
    pub async fn get_search_results(&self, query: &SearchQuery) -> Option<Arc<Vec<Package>>> {
        let result = self.search_results.get(query).await;

        let mut stats = self.stats.write().await;
        if result.is_some() {
            stats.search_hits += 1;
        } else {
            stats.search_misses += 1;
        }

        result
    }

    pub async fn set_search_results(&self, query: SearchQuery, results: Vec<Package>) {
        self.search_results.insert(query, Arc::new(results)).await;
    }

    pub async fn invalidate_search(&self, query: &SearchQuery) {
        self.search_results.invalidate(query).await;
    }

    pub async fn invalidate_all_searches(&self) {
        self.search_results.invalidate_all();
    }

    // Download counts cache operations
    pub async fn get_download_count(&self, id: &PackageId) -> Option<u64> {
        self.download_counts.get(id).await
    }

    pub async fn set_download_count(&self, id: PackageId, count: u64) {
        self.download_counts.insert(id, count).await;
    }

    pub async fn increment_download_count(&self, id: &PackageId) -> u64 {
        if let Some(current) = self.download_counts.get(id).await {
            let new_count = current + 1;
            self.download_counts.insert(id.clone(), new_count).await;
            new_count
        } else {
            self.download_counts.insert(id.clone(), 1).await;
            1
        }
    }

    // Version cache operations
    pub async fn get_versions(&self, id: &PackageId) -> Option<Arc<Vec<String>>> {
        self.versions.get(id).await
    }

    pub async fn set_versions(&self, id: PackageId, versions: Vec<String>) {
        self.versions.insert(id, Arc::new(versions)).await;
    }

    // Cache management
    pub async fn clear_all(&self) {
        self.packages.invalidate_all();
        self.search_results.invalidate_all();
        self.download_counts.invalidate_all();
        self.versions.invalidate_all();
    }

    pub async fn get_stats(&self) -> CacheStats {
        let stats = self.stats.read().await;

        let package_total = stats.package_hits + stats.package_misses;
        let search_total = stats.search_hits + stats.search_misses;
        let total_hits = stats.package_hits + stats.search_hits;
        let total_misses = stats.package_misses + stats.search_misses;
        let total = total_hits + total_misses;

        let hit_rate = if total > 0 {
            total_hits as f64 / total as f64
        } else {
            0.0
        };

        let entries = self.packages.entry_count()
            + self.search_results.entry_count()
            + self.download_counts.entry_count()
            + self.versions.entry_count();

        CacheStats {
            hits: total_hits,
            misses: total_misses,
            entries,
            hit_rate,
        }
    }

    pub async fn reset_stats(&self) {
        let mut stats = self.stats.write().await;
        *stats = CacheStatsInternal::default();
    }

    // Warming strategies
    pub async fn warm_popular_packages(&self, packages: Vec<Package>) {
        for package in packages {
            self.set_package(package.id.clone(), package).await;
        }
    }

    pub async fn warm_trending_searches(&self, queries: Vec<(SearchQuery, Vec<Package>)>) {
        for (query, results) in queries {
            self.set_search_results(query, results).await;
        }
    }

    // Advanced cache operations
    pub async fn get_or_insert_with<F, Fut>(
        &self,
        id: PackageId,
        fetch_fn: F,
    ) -> Result<Arc<Package>>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<Package>>,
    {
        if let Some(package) = self.get_package(&id).await {
            return Ok(package);
        }

        let package = fetch_fn().await?;
        let arc_package = Arc::new(package);
        self.packages.insert(id, arc_package.clone()).await;

        Ok(arc_package)
    }

    pub async fn get_search_or_insert_with<F, Fut>(
        &self,
        query: SearchQuery,
        search_fn: F,
    ) -> Result<Arc<Vec<Package>>>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<Vec<Package>>>,
    {
        if let Some(results) = self.get_search_results(&query).await {
            return Ok(results);
        }

        let results = search_fn().await?;
        let arc_results = Arc::new(results);
        self.search_results.insert(query, arc_results.clone()).await;

        Ok(arc_results)
    }

    // Memory management
    pub async fn memory_usage(&self) -> MemoryUsage {
        MemoryUsage {
            package_entries: self.packages.entry_count(),
            search_entries: self.search_results.entry_count(),
            download_entries: self.download_counts.entry_count(),
            version_entries: self.versions.entry_count(),
            // Approximate memory usage (rough estimate)
            estimated_bytes: (self.packages.entry_count() * 1024)
                + (self.search_results.entry_count() * 2048)
                + (self.download_counts.entry_count() * 16)
                + (self.versions.entry_count() * 512),
        }
    }

    pub async fn run_maintenance(&self) {
        self.packages.run_pending_tasks().await;
        self.search_results.run_pending_tasks().await;
        self.download_counts.run_pending_tasks().await;
        self.versions.run_pending_tasks().await;
    }
}

impl Default for SmartCache {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryUsage {
    pub package_entries: u64,
    pub search_entries: u64,
    pub download_entries: u64,
    pub version_entries: u64,
    pub estimated_bytes: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_package_cache() {
        let cache = SmartCache::new();

        let package = Package {
            id: "test-pkg".to_string(),
            name: "Test Package".to_string(),
            version: "1.0.0".to_string(),
            description: "A test package".to_string(),
            downloads: 1000,
        };

        cache.set_package("test-pkg".to_string(), package.clone()).await;

        let cached = cache.get_package(&"test-pkg".to_string()).await;
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().name, "Test Package");
    }

    #[tokio::test]
    async fn test_search_cache() {
        let cache = SmartCache::new();

        let query = SearchQuery {
            query: "rust web".to_string(),
            filters: vec![],
            limit: 10,
        };

        let results = vec![
            Package {
                id: "pkg1".to_string(),
                name: "Package 1".to_string(),
                version: "1.0.0".to_string(),
                description: "Test".to_string(),
                downloads: 100,
            },
        ];

        cache.set_search_results(query.clone(), results.clone()).await;

        let cached = cache.get_search_results(&query).await;
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().len(), 1);
    }

    #[tokio::test]
    async fn test_download_counter() {
        let cache = SmartCache::new();
        let pkg_id = "test-pkg".to_string();

        let count1 = cache.increment_download_count(&pkg_id).await;
        assert_eq!(count1, 1);

        let count2 = cache.increment_download_count(&pkg_id).await;
        assert_eq!(count2, 2);
    }

    #[tokio::test]
    async fn test_cache_stats() {
        let cache = SmartCache::new();

        let package = Package {
            id: "test".to_string(),
            name: "Test".to_string(),
            version: "1.0.0".to_string(),
            description: "Test".to_string(),
            downloads: 0,
        };

        cache.set_package("test".to_string(), package).await;

        // Hit
        cache.get_package(&"test".to_string()).await;

        // Miss
        cache.get_package(&"nonexistent".to_string()).await;

        let stats = cache.get_stats().await;
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
        assert_eq!(stats.hit_rate, 0.5);
    }
}
