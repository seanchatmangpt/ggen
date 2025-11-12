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
