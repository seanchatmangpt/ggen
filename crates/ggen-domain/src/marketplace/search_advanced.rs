//! Advanced marketplace search with ontology-based filtering and discovery
//!
//! Implements semantic search capabilities leveraging the marketplace ontology
//! for intelligent package discovery across sector bundles.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Advanced search query with semantic filters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdvancedSearchQuery {
    /// Free-text search query
    pub query: String,

    /// Filter by package quality score range (0-100)
    pub score_min: Option<f64>,
    pub score_max: Option<f64>,

    /// Filter by production-ready status
    pub production_ready: Option<bool>,

    /// Filter by sector bundles (e.g., "sector-enterprise-saas")
    pub bundles: Vec<String>,

    /// Filter by package categories (from ontology)
    pub categories: Vec<String>,

    /// Filter by tags
    pub tags: Vec<String>,

    /// Sort by: score, name, updated_at, downloads
    pub sort_by: SortField,

    /// Limit results
    pub limit: usize,
}

/// Sort field options for search results
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum SortField {
    /// Sort by quality score (highest first)
    Score,
    /// Sort by package name (A-Z)
    Name,
    /// Sort by last update (newest first)
    UpdatedAt,
    /// Sort by download count (most first)
    Downloads,
    /// Sort by relevance (best match first)
    Relevance,
}

impl Default for SortField {
    fn default() -> Self {
        SortField::Relevance
    }
}

impl Default for AdvancedSearchQuery {
    fn default() -> Self {
        Self {
            query: String::new(),
            score_min: None,
            score_max: None,
            production_ready: None,
            bundles: Vec::new(),
            categories: Vec::new(),
            tags: Vec::new(),
            sort_by: SortField::default(),
            limit: 20,
        }
    }
}

/// Package search result with relevance metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResultEntry {
    /// Package identifier
    pub package_id: String,

    /// Package version
    pub version: String,

    /// Package description
    pub description: String,

    /// Quality/validation score (0-100)
    pub score: f64,

    /// Whether package is production-ready
    pub production_ready: bool,

    /// Category from ontology
    pub category: String,

    /// Sector bundles this package belongs to
    pub bundles: Vec<String>,

    /// Relevance score for search match (0-100)
    pub relevance: f64,

    /// Last update timestamp
    pub updated_at: String,

    /// Download count
    pub downloads: usize,

    /// Tags/keywords
    pub tags: Vec<String>,
}

/// Advanced search results with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdvancedSearchResults {
    /// Total matches before limiting
    pub total_matches: usize,

    /// Number of results returned
    pub returned_count: usize,

    /// Search results
    pub results: Vec<SearchResultEntry>,

    /// Quality statistics
    pub stats: SearchStatistics,

    /// Recommended packages not in results
    pub recommendations: Vec<String>,
}

/// Statistics about search results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchStatistics {
    /// Average score of results
    pub avg_score: f64,

    /// Number of production-ready packages
    pub production_ready_count: usize,

    /// Distribution of scores: 95-100, 80-94, 60-79, <60
    pub score_distribution: ScoreDistribution,

    /// Top 3 categories in results
    pub top_categories: Vec<(String, usize)>,

    /// Top 3 bundles in results
    pub top_bundles: Vec<(String, usize)>,
}

/// Score distribution for analytics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScoreDistribution {
    pub excellent: usize, // 95-100
    pub good: usize,      // 80-94
    pub fair: usize,      // 60-79
    pub poor: usize,      // <60
}

impl Default for SearchStatistics {
    fn default() -> Self {
        Self {
            avg_score: 0.0,
            production_ready_count: 0,
            score_distribution: ScoreDistribution {
                excellent: 0,
                good: 0,
                fair: 0,
                poor: 0,
            },
            top_categories: Vec::new(),
            top_bundles: Vec::new(),
        }
    }
}

/// Search engine implementation with advanced filtering
pub struct SearchEngine;

impl SearchEngine {
    /// Perform advanced search with multiple filters and scoring
    pub fn search(
        query: &AdvancedSearchQuery, packages: Vec<SearchResultEntry>,
    ) -> AdvancedSearchResults {
        let mut filtered = packages
            .into_iter()
            .filter(|pkg| Self::matches_filters(pkg, query))
            .collect::<Vec<_>>();

        // Calculate relevance scores
        for pkg in &mut filtered {
            pkg.relevance = Self::calculate_relevance(&query.query, pkg);
        }

        // Sort results
        Self::sort_results(&mut filtered, query.sort_by);

        // Calculate statistics
        let stats = Self::calculate_statistics(&filtered);

        let total = filtered.len();
        let returned = filtered.iter().take(query.limit).count();

        let results = filtered.into_iter().take(query.limit).collect::<Vec<_>>();

        AdvancedSearchResults {
            total_matches: total,
            returned_count: returned,
            recommendations: Vec::new(),
            stats,
            results,
        }
    }

    /// Check if package matches all query filters
    fn matches_filters(pkg: &SearchResultEntry, query: &AdvancedSearchQuery) -> bool {
        // Score range filter
        if let Some(min) = query.score_min {
            if pkg.score < min {
                return false;
            }
        }
        if let Some(max) = query.score_max {
            if pkg.score > max {
                return false;
            }
        }

        // Production-ready filter
        if let Some(prod_ready) = query.production_ready {
            if pkg.production_ready != prod_ready {
                return false;
            }
        }

        // Bundles filter (if specified, must match at least one)
        if !query.bundles.is_empty() {
            let matches = pkg.bundles.iter().any(|b| query.bundles.contains(b));
            if !matches {
                return false;
            }
        }

        // Categories filter (if specified, must match at least one)
        if !query.categories.is_empty() {
            if !query.categories.contains(&pkg.category) {
                return false;
            }
        }

        // Tags filter (if specified, must match at least one)
        if !query.tags.is_empty() {
            let matches = pkg.tags.iter().any(|t| query.tags.contains(t));
            if !matches {
                return false;
            }
        }

        true
    }

    /// Calculate relevance score for query matching
    fn calculate_relevance(query: &str, pkg: &SearchResultEntry) -> f64 {
        let query_lower = query.to_lowercase();

        let mut score: f64 = 0.0;

        // Exact name match: 50 points
        if pkg.package_id.to_lowercase() == query_lower {
            score += 50.0;
        }
        // Name contains query: 25 points
        else if pkg.package_id.to_lowercase().contains(&query_lower) {
            score += 25.0;
        }

        // Description contains: 15 points per match
        if pkg.description.to_lowercase().contains(&query_lower) {
            score += 15.0;
        }

        // Tags/category match: 10 points each
        for tag in &pkg.tags {
            if tag.to_lowercase().contains(&query_lower)
                || query_lower.contains(&tag.to_lowercase())
            {
                score += 10.0;
            }
        }

        // Normalize to 0-100
        score.min(100.0)
    }

    /// Sort results by specified field
    fn sort_results(results: &mut [SearchResultEntry], sort_by: SortField) {
        match sort_by {
            SortField::Score => {
                results.sort_by(|a, b| {
                    b.score
                        .partial_cmp(&a.score)
                        .unwrap_or(std::cmp::Ordering::Equal)
                });
            }
            SortField::Name => {
                results.sort_by(|a, b| a.package_id.cmp(&b.package_id));
            }
            SortField::UpdatedAt => {
                results.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
            }
            SortField::Downloads => {
                results.sort_by(|a, b| b.downloads.cmp(&a.downloads));
            }
            SortField::Relevance => {
                results.sort_by(|a, b| {
                    b.relevance
                        .partial_cmp(&a.relevance)
                        .unwrap_or(std::cmp::Ordering::Equal)
                });
            }
        }
    }

    /// Calculate search result statistics
    fn calculate_statistics(results: &[SearchResultEntry]) -> SearchStatistics {
        let mut stats = SearchStatistics::default();

        if results.is_empty() {
            return stats;
        }

        // Calculate average score and production ready count
        let mut total_score = 0.0;
        let mut category_counts: HashMap<String, usize> = HashMap::new();
        let mut bundle_counts: HashMap<String, usize> = HashMap::new();

        for pkg in results {
            total_score += pkg.score;
            if pkg.production_ready {
                stats.production_ready_count += 1;
            }

            // Score distribution
            match pkg.score {
                s if s >= 95.0 => stats.score_distribution.excellent += 1,
                s if s >= 80.0 => stats.score_distribution.good += 1,
                s if s >= 60.0 => stats.score_distribution.fair += 1,
                _ => stats.score_distribution.poor += 1,
            }

            // Category counts
            *category_counts.entry(pkg.category.clone()).or_insert(0) += 1;

            // Bundle counts
            for bundle in &pkg.bundles {
                *bundle_counts.entry(bundle.clone()).or_insert(0) += 1;
            }
        }

        stats.avg_score = total_score / results.len() as f64;

        // Top 3 categories and bundles
        let mut categories: Vec<_> = category_counts.into_iter().collect();
        categories.sort_by_key(|(_k, v)| std::cmp::Reverse(*v));
        stats.top_categories = categories.into_iter().take(3).collect();

        let mut bundles: Vec<_> = bundle_counts.into_iter().collect();
        bundles.sort_by_key(|(_k, v)| std::cmp::Reverse(*v));
        stats.top_bundles = bundles.into_iter().take(3).collect();

        stats
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_relevance_calculation() {
        let pkg = SearchResultEntry {
            package_id: "rust-project".to_string(),
            version: "1.0.0".to_string(),
            description: "A Rust project for data processing".to_string(),
            score: 85.0,
            production_ready: true,
            category: "rust".to_string(),
            bundles: vec!["sector-data-pipelines".to_string()],
            relevance: 0.0,
            updated_at: "2025-01-01T00:00:00Z".to_string(),
            downloads: 100,
            tags: vec!["rust".to_string(), "data".to_string()],
        };

        let rel = SearchEngine::calculate_relevance("rust", &pkg);
        assert!(rel > 0.0);
    }

    #[test]
    fn test_filter_matching() {
        let pkg = SearchResultEntry {
            package_id: "test-package".to_string(),
            version: "1.0.0".to_string(),
            description: "Test".to_string(),
            score: 90.0,
            production_ready: true,
            category: "test".to_string(),
            bundles: vec!["sector-enterprise-saas".to_string()],
            relevance: 50.0,
            updated_at: "2025-01-01T00:00:00Z".to_string(),
            downloads: 50,
            tags: vec!["test".to_string()],
        };

        let mut query = AdvancedSearchQuery::default();
        query.score_min = Some(85.0);
        assert!(SearchEngine::matches_filters(&pkg, &query));

        query.score_min = Some(95.0);
        assert!(!SearchEngine::matches_filters(&pkg, &query));
    }
}
