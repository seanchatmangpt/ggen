//! Search engine implementations
//!
//! This module provides implementations of the `SearchEngine` trait for full-text
//! search over package metadata. The primary implementation uses Tantivy for
//! high-performance search with advanced querying capabilities.
//!
//! ## Features
//!
//! - **Full-text search**: Search across package names, descriptions, and metadata
//! - **Query parsing**: Advanced query parsing with filters and facets
//! - **Scoring**: Relevance scoring for search results
//! - **Indexing**: Efficient indexing and bulk operations
//! - **Fuzzy matching**: Typo-tolerant search with configurable fuzziness
//! - **Faceted search**: Filter results by category, language, license, etc.
//!
//! ## Implementations
//!
//! - **TantivySearchEngine**: High-performance full-text search using Tantivy
//!   - BM25 relevance ranking
//!   - Incremental indexing
//!   - Faceted filtering
//!   - Query parsing and optimization
//!
//! ## Examples
//!
//! ### Creating a Search Engine
//!
//! ```rust,no_run
//! use ggen_marketplace::search::TantivySearchEngine;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let engine = TantivySearchEngine::new(Path::new("/tmp/search_index"))?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Indexing Packages
//!
//! ```rust,no_run
//! use ggen_marketplace::search::TantivySearchEngine;
//! use ggen_marketplace::types::Package;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let engine = TantivySearchEngine::new("/tmp/index")?;
//! let package = Package { /* ... */ };
//!
//! engine.index(&package).await?;
//! # Ok(())
//! # }
//! ```

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
