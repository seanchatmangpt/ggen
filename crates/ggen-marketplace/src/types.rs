//! Marketplace type definitions
//!
//! This module provides the core type definitions for the ggen marketplace,
//! including package structures, search queries, filters, and result types.
//!
//! ## Types
//!
//! - **Package**: Represents a package in the marketplace with metadata
//! - **SearchQuery**: Advanced search query with filters, pagination, and sorting
//! - **SearchFilters**: Faceted filters for categories, languages, licenses, etc.
//! - **SearchResults**: Search results with packages, facets, and metadata
//! - **ScoredPackage**: Package with relevance score and highlights
//! - **Facet**: Facet value with count for filtering UI
//!
//! ## Examples
//!
//! ### Creating a Search Query
//!
//! ```rust,no_run
//! use ggen_marketplace::types::{SearchQuery, SearchFilters, SortOption};
//!
//! let query = SearchQuery {
//!     query: "rust cli".to_string(),
//!     filters: SearchFilters {
//!         categories: vec!["cli".to_string()],
//!         languages: vec!["rust".to_string()],
//!         min_downloads: Some(100),
//!         min_rating: Some(4.0),
//!         ..Default::default()
//!     },
//!     offset: 0,
//!     limit: 20,
//!     sort_by: SortOption::Downloads,
//!     fuzzy: true,
//!     min_score: 0.5,
//! };
//! ```
//!
//! ### Working with Packages
//!
//! ```rust,no_run
//! use ggen_marketplace::types::Package;
//!
//! let package = Package {
//!     id: "io.ggen.rust.cli".to_string(),
//!     name: "Rust CLI Generator".to_string(),
//!     description: "Generate CLI applications in Rust".to_string(),
//!     version: "1.0.0".to_string(),
//!     category: "cli".to_string(),
//!     language: "rust".to_string(),
//!     license: "MIT".to_string(),
//!     tags: vec!["cli".to_string(), "rust".to_string()],
//!     downloads: 1000,
//!     rating: 4.5,
//!     created_at: chrono::Utc::now(),
//!     updated_at: chrono::Utc::now(),
//!     author: "example".to_string(),
//!     repository_url: Some("https://github.com/example/rust-cli".to_string()),
//! };
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a package in the marketplace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub id: String,
    pub name: String,
    pub description: String,
    pub version: String,
    pub category: String,
    pub language: String,
    pub license: String,
    pub tags: Vec<String>,
    pub downloads: u64,
    pub rating: f32,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub updated_at: chrono::DateTime<chrono::Utc>,
    pub author: String,
    pub repository_url: Option<String>,
}

/// Search query with filters and options
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchQuery {
    /// Full-text search query
    pub query: String,

    /// Faceted filters
    pub filters: SearchFilters,

    /// Pagination
    pub offset: usize,
    pub limit: usize,

    /// Sorting
    pub sort_by: SortOption,

    /// Enable fuzzy matching
    pub fuzzy: bool,

    /// Minimum relevance score (0.0 - 1.0)
    pub min_score: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SearchFilters {
    pub categories: Vec<String>,
    pub languages: Vec<String>,
    pub licenses: Vec<String>,
    pub min_downloads: Option<u64>,
    pub min_rating: Option<f32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub enum SortOption {
    #[default]
    Relevance,
    Downloads,
    Rating,
    UpdatedAt,
    CreatedAt,
}

/// Search results with facets and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResults {
    pub packages: Vec<ScoredPackage>,
    pub total: usize,
    pub facets: HashMap<String, Vec<Facet>>,
    pub query_time_ms: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScoredPackage {
    pub package: Package,
    pub score: f32,
    pub highlights: HashMap<String, Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Facet {
    pub value: String,
    pub count: usize,
}

impl Default for SearchQuery {
    fn default() -> Self {
        Self {
            query: String::new(),
            filters: SearchFilters::default(),
            offset: 0,
            limit: 20,
            sort_by: SortOption::default(),
            fuzzy: false,
            min_score: 0.0,
        }
    }
}
