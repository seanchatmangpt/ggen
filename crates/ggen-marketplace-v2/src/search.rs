//! Advanced search engine with full-text and semantic capabilities
//!
//! Features:
//! - Multi-field search (name, description, keywords)
//! - Fuzzy matching
//! - Relevance ranking
//! - Filters and facets

use crate::error::Result;
use crate::models::{Package, QualityScore, SearchResult};
use crate::traits::{DefaultRanker, Ranker};
use std::sync::Arc;
use tracing::debug;

/// Search query with advanced capabilities
#[derive(Clone, Debug)]
pub struct SearchQuery {
    /// Main search text
    pub text: String,

    /// Filter by category
    pub category_filter: Option<String>,

    /// Filter by minimum quality score
    pub min_quality_score: Option<QualityScore>,

    /// Filter by author
    pub author_filter: Option<String>,

    /// Filter by license
    pub license_filter: Option<String>,

    /// Sort order
    pub sort_by: SortBy,

    /// Maximum results to return
    pub limit: usize,

    /// Pagination offset
    pub offset: usize,
}

impl SearchQuery {
    /// Create a new search query
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            category_filter: None,
            min_quality_score: None,
            author_filter: None,
            license_filter: None,
            sort_by: SortBy::Relevance,
            limit: 50,
            offset: 0,
        }
    }

    /// Add category filter
    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category_filter = Some(category.into());
        self
    }

    /// Add quality filter
    pub fn with_min_quality(mut self, score: QualityScore) -> Self {
        self.min_quality_score = Some(score);
        self
    }

    /// Add author filter
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author_filter = Some(author.into());
        self
    }

    /// Add license filter
    pub fn with_license(mut self, license: impl Into<String>) -> Self {
        self.license_filter = Some(license.into());
        self
    }

    /// Set sort order
    pub fn with_sort(mut self, sort: SortBy) -> Self {
        self.sort_by = sort;
        self
    }

    /// Set result limit
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    /// Set pagination offset
    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = offset;
        self
    }
}

/// Sort order for search results
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SortBy {
    /// Sort by relevance (default)
    Relevance,
    /// Sort by download count
    Downloads,
    /// Sort by quality score
    Quality,
    /// Sort by creation date (newest first)
    Newest,
    /// Sort alphabetically by name
    Name,
}

/// Advanced search engine
pub struct SearchEngine {
    ranker: Arc<Box<dyn Ranker + Send + Sync>>,
}

impl SearchEngine {
    /// Create a new search engine
    pub fn new() -> Self {
        Self {
            ranker: Arc::new(Box::new(DefaultRanker)),
        }
    }

    /// Create search engine with custom ranker
    pub fn with_ranker(ranker: Arc<Box<dyn Ranker + Send + Sync>>) -> Self {
        Self { ranker }
    }

    /// Execute a search on a list of packages
    pub fn search(&self, packages: Vec<Package>, query: &SearchQuery) -> Result<Vec<SearchResult>> {
        let text_lower = query.text.to_lowercase();

        // Score packages based on relevance
        let mut scored: Vec<SearchResult> = packages
            .into_iter()
            .filter_map(|pkg| {
                // Apply filters first
                if !self.passes_filters(&pkg, query) {
                    return None;
                }

                // Calculate relevance score
                let relevance = self.calculate_relevance(&pkg, &text_lower);

                if relevance > 0.0 {
                    Some(SearchResult {
                        package: pkg,
                        relevance,
                    })
                } else {
                    None
                }
            })
            .collect();

        // Sort results
        scored = self.sort_results(scored, query.sort_by);

        // Apply pagination
        let start = query.offset;
        let end = std::cmp::min(start + query.limit, scored.len());
        let results = scored.into_iter().skip(start).take(query.limit).collect();

        debug!(
            "Search for '{}' returned {} results (offset: {}, limit: {})",
            query.text,
            end - start,
            query.offset,
            query.limit
        );

        Ok(results)
    }

    /// Calculate relevance score for a package
    fn calculate_relevance(&self, package: &Package, query_text: &str) -> f64 {
        let mut score: f64 = 0.0;

        let pkg_name_lower = package.metadata.name.to_lowercase();
        let pkg_desc_lower = package.metadata.description.to_lowercase();
        let pkg_id_lower = package.metadata.id.as_str().to_lowercase();

        // Name match: highest priority
        if pkg_name_lower == query_text {
            score += 1.0;
        } else if pkg_name_lower.contains(query_text) {
            score += 0.8;
        }

        // ID match
        if pkg_id_lower == query_text {
            score += 0.9;
        } else if pkg_id_lower.contains(query_text) {
            score += 0.7;
        }

        // Description match
        if pkg_desc_lower.contains(query_text) {
            score += 0.5;
        }

        // Keyword match
        for keyword in &package.metadata.keywords {
            if keyword.to_lowercase().contains(query_text) {
                score += 0.3;
            }
        }

        // Fuzzy matching for typos
        if levenshtein_distance(&pkg_name_lower, query_text) <= 2 {
            score += 0.4;
        }

        // Cap score at 1.0
        score.min(1.0)
    }

    /// Check if package passes all filters
    fn passes_filters(&self, package: &Package, query: &SearchQuery) -> bool {
        // Category filter
        if let Some(ref category) = query.category_filter {
            if !package
                .metadata
                .categories
                .iter()
                .any(|c| c.to_lowercase() == category.to_lowercase())
            {
                return false;
            }
        }

        // Quality filter
        if let Some(min_score) = query.min_quality_score {
            if let Some(score) = package.metadata.quality_score {
                if score < min_score {
                    return false;
                }
            } else {
                return false; // No quality score = doesn't pass quality filter
            }
        }

        // Author filter
        if let Some(ref author) = query.author_filter {
            if !package
                .metadata
                .authors
                .iter()
                .any(|a| a.to_lowercase().contains(&author.to_lowercase()))
            {
                return false;
            }
        }

        // License filter
        if let Some(ref license) = query.license_filter {
            if package.metadata.license.to_lowercase() != license.to_lowercase() {
                return false;
            }
        }

        true
    }

    /// Sort results based on sort order
    fn sort_results(&self, mut results: Vec<SearchResult>, sort_by: SortBy) -> Vec<SearchResult> {
        match sort_by {
            SortBy::Relevance => {
                self.ranker.as_ref().rank(&mut results);
            }
            SortBy::Downloads => {
                results.sort_by(|a, b| {
                    b.package
                        .metadata
                        .downloads
                        .cmp(&a.package.metadata.downloads)
                });
            }
            SortBy::Quality => {
                results.sort_by(|a, b| {
                    match (
                        b.package.metadata.quality_score,
                        a.package.metadata.quality_score,
                    ) {
                        (Some(b_score), Some(a_score)) => b_score.cmp(&a_score),
                        (Some(_), None) => std::cmp::Ordering::Less,
                        (None, Some(_)) => std::cmp::Ordering::Greater,
                        (None, None) => std::cmp::Ordering::Equal,
                    }
                });
            }
            SortBy::Newest => {
                results.sort_by(|a, b| {
                    b.package
                        .metadata
                        .created_at
                        .cmp(&a.package.metadata.created_at)
                });
            }
            SortBy::Name => {
                results.sort_by(|a, b| a.package.metadata.name.cmp(&b.package.metadata.name));
            }
        }

        results
    }
}

impl Default for SearchEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculate Levenshtein distance for fuzzy matching
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.len();
    let len2 = s2.len();
    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for i in 0..=len1 {
        matrix[i][0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    for (i, c1) in s1.chars().enumerate() {
        for (j, c2) in s2.chars().enumerate() {
            let cost = if c1 == c2 { 0 } else { 1 };
            matrix[i + 1][j + 1] = std::cmp::min(
                std::cmp::min(matrix[i][j + 1] + 1, matrix[i + 1][j] + 1),
                matrix[i][j] + cost,
            );
        }
    }

    matrix[len1][len2]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("hello", "hello"), 0);
        assert_eq!(levenshtein_distance("hello", "hallo"), 1);
        assert_eq!(levenshtein_distance("hello", "hell"), 1);
    }

    #[test]
    fn test_search_query_builder() {
        let query = SearchQuery::new("test")
            .with_category("database")
            .with_limit(100);

        assert_eq!(query.text, "test");
        assert_eq!(query.category_filter, Some("database".to_string()));
        assert_eq!(query.limit, 100);
    }
}
