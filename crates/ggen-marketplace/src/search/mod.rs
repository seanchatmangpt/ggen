pub mod query_parser;
pub mod scoring;
pub mod tantivy_engine;

pub use tantivy_engine::TantivySearchEngine;

use crate::types::{Package, SearchQuery, SearchResults};
use anyhow::Result;
use async_trait::async_trait;

/// Core search engine trait
#[async_trait]
pub trait SearchEngine: Send + Sync {
    /// Search for packages
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults>;

    /// Index a package
    async fn index(&self, package: &Package) -> Result<()>;

    /// Bulk index packages
    async fn bulk_index(&self, packages: Vec<Package>) -> Result<()>;

    /// Remove a package from index
    async fn remove(&self, package_id: &str) -> Result<()>;

    /// Update a package
    async fn update(&self, package: &Package) -> Result<()>;

    /// Commit pending changes
    async fn commit(&self) -> Result<()>;

    /// Get index statistics
    async fn stats(&self) -> Result<IndexStats>;
}

#[derive(Debug, Clone)]
pub struct IndexStats {
    pub total_documents: usize,
    pub index_size_bytes: u64,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}
