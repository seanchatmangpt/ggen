use crate::error::Result;
use crate::models::{Category, Package, PackageId};
use async_trait::async_trait;

/// Extended search engine trait with advanced features
#[async_trait]
pub trait SearchEngineExt: super::SearchEngine {
    /// Suggest search queries based on partial input
    async fn suggest(&self, partial_query: &str, limit: usize) -> Result<Vec<String>>;

    /// Get related packages
    async fn related(&self, id: &PackageId, limit: usize) -> Result<Vec<Package>>;

    /// Get popular search terms
    async fn popular_searches(&self, limit: usize) -> Result<Vec<SearchTerm>>;

    /// Index multiple packages in batch
    async fn index_batch(&self, packages: &[Package]) -> Result<BatchIndexResult>;

    /// Search with autocorrect
    async fn search_with_autocorrect(
        &self,
        query: &crate::models::SearchQuery,
    ) -> Result<AutocorrectSearchResults>;

    /// Get search analytics
    async fn analytics(&self, days: u32) -> Result<SearchAnalytics>;
}

/// Search term with popularity
#[derive(Debug, Clone)]
pub struct SearchTerm {
    pub term: String,
    pub count: u64,
    pub last_searched: chrono::DateTime<chrono::Utc>,
}

/// Batch indexing result
#[derive(Debug, Clone)]
pub struct BatchIndexResult {
    pub total_indexed: usize,
    pub failed: Vec<IndexFailure>,
    pub duration_ms: u64,
}

/// Index failure information
#[derive(Debug, Clone)]
pub struct IndexFailure {
    pub package_id: PackageId,
    pub error: String,
}

/// Search results with autocorrection
#[derive(Debug, Clone)]
pub struct AutocorrectSearchResults {
    pub results: crate::models::SearchResults,
    pub original_query: String,
    pub corrected_query: Option<String>,
    pub suggestions: Vec<String>,
}

/// Search analytics data
#[derive(Debug, Clone)]
pub struct SearchAnalytics {
    pub total_searches: u64,
    pub unique_queries: u64,
    pub avg_results_per_query: f64,
    pub top_categories: Vec<CategoryStats>,
    pub top_queries: Vec<SearchTerm>,
}

/// Category statistics
#[derive(Debug, Clone)]
pub struct CategoryStats {
    pub category: Category,
    pub search_count: u64,
    pub package_count: usize,
}

/// Full-text search trait for content indexing
#[async_trait]
pub trait FullTextSearch: Send + Sync {
    /// Index text content
    async fn index_text(&self, id: &PackageId, field: &str, text: &str) -> Result<()>;

    /// Search within text fields
    async fn search_text(&self, query: &str, fields: &[String]) -> Result<Vec<TextSearchResult>>;

    /// Highlight matching terms in text
    async fn highlight(&self, text: &str, query: &str) -> Result<Vec<HighlightSpan>>;
}

/// Text search result
#[derive(Debug, Clone)]
pub struct TextSearchResult {
    pub package_id: PackageId,
    pub field: String,
    pub score: f64,
    pub highlights: Vec<HighlightSpan>,
}

/// Highlighted span in text
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    pub start: usize,
    pub end: usize,
    pub text: String,
}
