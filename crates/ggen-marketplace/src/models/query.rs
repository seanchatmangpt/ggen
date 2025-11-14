//! Search query models and types
//!
//! This module provides data structures for constructing and executing search queries
//! against the marketplace. It includes simple queries, advanced queries with filters,
//! and search result types.
//!
//! ## Query Types
//!
//! - **Query**: Simple text-based search query with optional filters
//! - **SearchQuery**: Advanced query with ranking, faceting, and pagination
//! - **SearchResults**: Paginated search results with metadata
//!
//! ## Examples
//!
//! ### Creating a Simple Query
//!
//! ```rust
//! use ggen_marketplace::models::{Query, Category};
//!
//! # fn main() {
//! let query = Query::new("rust web service")
//!     .with_category(Category::WebService)
//!     .with_tag("async")
//!     .with_limit(20);
//! # }
//! ```
//!
//! ### Creating an Advanced Search Query
//!
//! ```rust,no_run
//! use ggen_marketplace::models::{SearchQuery, Query, Category};
//!
//! # fn main() {
//! let base_query = Query::new("rust");
//! let search_query = SearchQuery {
//!     query: base_query,
//!     min_score: 0.5,
//!     fuzzy: true,
//!     // ... other options
//! };
//! # }
//! ```

use super::{Category, Package, Version};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Simple search query for package discovery
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Query {
    pub text: String,
    pub categories: Vec<Category>,
    pub tags: Vec<String>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

impl Query {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            categories: Vec::new(),
            tags: Vec::new(),
            limit: None,
            offset: None,
        }
    }

    pub fn with_category(mut self, category: Category) -> Self {
        self.categories.push(category);
        self
    }

    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }
}

/// Advanced search query with filters and ranking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchQuery {
    pub query: Query,
    pub filters: SearchFilters,
    pub sort: SortOrder,
    pub facets: Vec<Facet>,
}

impl SearchQuery {
    pub fn new(query: Query) -> Self {
        Self {
            query,
            filters: SearchFilters::default(),
            sort: SortOrder::Relevance,
            facets: Vec::new(),
        }
    }

    pub fn with_filters(mut self, filters: SearchFilters) -> Self {
        self.filters = filters;
        self
    }

    pub fn with_sort(mut self, sort: SortOrder) -> Self {
        self.sort = sort;
        self
    }

    pub fn with_facet(mut self, facet: Facet) -> Self {
        self.facets.push(facet);
        self
    }
}

/// Search filters for narrowing results
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchFilters {
    pub min_downloads: Option<u64>,
    pub min_stars: Option<u32>,
    pub version_range: Option<VersionRange>,
    pub license: Option<Vec<String>>,
    pub updated_after: Option<chrono::DateTime<chrono::Utc>>,
    pub verified_only: bool,
    pub custom: HashMap<String, serde_json::Value>,
}

/// Version range filter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionRange {
    pub min: Option<Version>,
    pub max: Option<Version>,
}

/// Sort order for search results
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum SortOrder {
    Relevance,
    Downloads,
    Stars,
    RecentlyUpdated,
    RecentlyCreated,
    Name,
}

/// Facet for aggregating search results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Facet {
    Category,
    License,
    Author,
    Tag,
    Custom(String),
}

/// Search results with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResults {
    pub packages: Vec<SearchResult>,
    pub total_count: usize,
    pub facets: HashMap<String, FacetResults>,
    pub query_time_ms: u64,
}

impl SearchResults {
    pub fn new(packages: Vec<SearchResult>, total_count: usize, query_time_ms: u64) -> Self {
        Self {
            packages,
            total_count,
            facets: HashMap::new(),
            query_time_ms,
        }
    }

    pub fn with_facets(mut self, facets: HashMap<String, FacetResults>) -> Self {
        self.facets = facets;
        self
    }
}

/// Individual search result with relevance scoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub package: Package,
    pub score: f64,
    pub highlights: Vec<Highlight>,
}

impl SearchResult {
    pub fn new(package: Package, score: f64) -> Self {
        Self {
            package,
            score,
            highlights: Vec::new(),
        }
    }

    pub fn with_highlight(mut self, highlight: Highlight) -> Self {
        self.highlights.push(highlight);
        self
    }
}

/// Highlighted text snippet from search
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Highlight {
    pub field: String,
    pub snippet: String,
    pub start: usize,
    pub end: usize,
}

/// Facet aggregation results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FacetResults {
    pub values: Vec<FacetValue>,
}

/// Individual facet value with count
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FacetValue {
    pub value: String,
    pub count: usize,
}
