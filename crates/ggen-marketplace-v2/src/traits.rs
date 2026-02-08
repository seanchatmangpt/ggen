//! Core traits using Generic Associated Types (GATs) and Higher-Ranked Trait Bounds (HRTB)
//!
//! These traits define the behavior of marketplace components with maximum flexibility
//! and type safety through advanced Rust patterns.

use async_trait::async_trait;

use crate::error::Result;
use crate::models::{
    InstallationManifest, Manifest, Package, PackageId, PackageVersion, SearchResult,
};

/// Generic associated types for async operations (using async_trait for now)
/// Note: Can be migrated to native GATs when Rust stabilizes them
/// A repository that can be queried for packages
#[async_trait]
pub trait AsyncRepository: Send + Sync {
    /// Type for the iterator over packages
    type PackageIterator: Iterator<Item = Package> + Send;

    /// Get a package by ID
    #[must_use]
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Get a specific version of a package
    #[must_use]
    async fn get_package_version(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Package>;

    /// Get all packages in the repository
    #[must_use]
    async fn all_packages(&self) -> Result<Vec<Package>>;

    /// List all versions of a package
    #[must_use]
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<PackageVersion>>;

    /// Check if a package exists
    #[must_use]
    async fn package_exists(&self, id: &PackageId) -> Result<bool>;
}

/// Trait for queryable data sources
#[async_trait]
pub trait Queryable: Send + Sync {
    /// Type of query accepted
    type Query: Send + Sync;

    /// Type of query result
    type QueryResult: Send + Sync;

    /// Execute a query
    #[must_use]
    async fn query(&self, query: Self::Query) -> Result<Self::QueryResult>;

    /// Explain a query (for debugging)
    #[must_use]
    fn explain_query(&self, query: &Self::Query) -> String;
}

/// Trait for installable packages with dependency resolution
#[async_trait]
pub trait Installable: Send + Sync {
    /// Install a package
    #[must_use]
    async fn install(&self, manifest: InstallationManifest) -> Result<InstallationManifest>;

    /// Resolve dependencies for a package
    #[must_use]
    async fn resolve_dependencies(
        &self, id: &PackageId, version: &PackageVersion,
    ) -> Result<Vec<(PackageId, PackageVersion)>>;

    /// Dry-run installation without actual changes
    #[must_use]
    async fn dry_run_install(&self, manifest: &InstallationManifest) -> Result<String>;
}

/// Trait for package validation
#[async_trait]
pub trait Validatable: Send + Sync {
    /// Validation result
    type ValidationResult: Send + Sync;

    /// Validate a package
    #[must_use]
    async fn validate(&self, package: &Package) -> Result<Self::ValidationResult>;

    /// Validate a manifest
    #[must_use]
    async fn validate_manifest(&self, manifest: &Manifest) -> Result<Self::ValidationResult>;

    /// Check if validation passes
    #[must_use]
    fn validation_passes(&self, result: &Self::ValidationResult) -> bool;
}

/// Trait for cryptographic operations with HRTB
pub trait Signable {
    /// Sign data and return signature
    #[must_use]
    fn sign(&self, data: &[u8]) -> Result<String>;

    /// Verify a signature
    #[must_use]
    fn verify(&self, data: &[u8], signature: &str) -> Result<bool>;

    /// Get the public key
    #[must_use]
    fn public_key(&self) -> String;
}

/// Trait for observable metrics collection
#[async_trait]
pub trait Observable: Send + Sync {
    /// Record a metric
    async fn record_metric(&self, name: &str, value: f64) -> Result<()>;

    /// Record an event
    async fn record_event(&self, name: &str, data: &str) -> Result<()>;

    /// Get metrics summary
    #[must_use]
    async fn get_metrics(&self) -> Result<String>;
}

/// A cache with generic key-value operations
pub trait Cache<K, V>: Send + Sync
where
    K: Send + Sync,
    V: Send + Sync,
{
    /// Get a value from cache
    fn get(&self, key: &K) -> Option<V>;

    /// Insert a value into cache
    fn insert(&self, key: K, value: V);

    /// Remove a value from cache
    fn remove(&self, key: &K) -> Option<V>;

    /// Clear the cache
    fn clear(&self);

    /// Get cache size
    fn size(&self) -> usize;
}

/// A builder trait with compile-time safety using HRTB
/// This allows for flexible builder implementations with lifetime tracking
pub trait Builder<T>: Sized {
    /// Build the final value
    fn build(self) -> Result<T>;

    /// Validate intermediate state
    fn validate(&self) -> Result<()>;
}

/// A filter for query results
pub trait Filter<T>: Send + Sync {
    /// Apply filter to item
    fn matches(&self, item: &T) -> bool;

    /// Filter a collection
    fn filter_items(&self, items: Vec<T>) -> Vec<T> {
        items
            .into_iter()
            .filter(|item| self.matches(item))
            .collect()
    }
}

/// A transformer for items with generic mapping
pub trait Transformer<T, U>: Send + Sync {
    /// Transform an item
    fn transform(&self, item: T) -> Result<U>;

    /// Transform multiple items
    #[allow(async_fn_in_trait)]
    async fn transform_batch(&self, items: Vec<T>) -> Result<Vec<U>> {
        let mut results = Vec::with_capacity(items.len());
        for item in items {
            results.push(self.transform(item)?);
        }
        Ok(results)
    }
}

/// Trait for search result ranking
pub trait Ranker {
    /// Rank search results
    fn rank(&self, results: &mut [SearchResult]) {
        results.sort_by(|a, b| {
            b.relevance
                .partial_cmp(&a.relevance)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
    }
}

/// Default ranker implementation
pub struct DefaultRanker;

impl Ranker for DefaultRanker {
    fn rank(&self, results: &mut [SearchResult]) {
        results.sort_by(|a, b| {
            // First compare by relevance
            let rel_cmp = b
                .relevance
                .partial_cmp(&a.relevance)
                .unwrap_or(std::cmp::Ordering::Equal);
            if rel_cmp != std::cmp::Ordering::Equal {
                return rel_cmp;
            }

            // Then by download count
            b.package
                .metadata
                .downloads
                .cmp(&a.package.metadata.downloads)
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_ranker() {
        let mut results = vec![];
        let ranker = DefaultRanker;
        ranker.rank(&mut results);
        assert!(results.is_empty());
    }
}
