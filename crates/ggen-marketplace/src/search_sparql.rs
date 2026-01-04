//! SPARQL-based semantic search engine
//!
//! Implements intelligent package discovery using SPARQL queries against
//! the RDF knowledge graph. Enables semantic search, filtering, and ranking.
//!
//! # Quality Tiers (Story 3 - Search & Discover)
//!
//! Packages are classified into quality tiers for better discoverability:
//! - **Gold**: FMEA passed + >100 downloads + <30 days old
//! - **Silver**: FMEA passed + 10-100 downloads OR 30-90 days old
//! - **Bronze**: Basic validation only
//!
//! # Performance Target
//!
//! SC-004: Search latency <1 second for 100 concurrent queries

use crate::error::Result;
use crate::ontology::{Namespaces, Properties, Queries};
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Instant;
use tracing::{debug, info};

/// SPARQL-powered search engine
///
/// Uses semantic queries to find packages based on:
/// - Full-text search (name, description, keywords)
/// - Quality filtering
/// - Author filtering
/// - Trending/recent packages
/// - Dependency relationships
pub struct SparqlSearchEngine {
    /// RDF triplestore
    store: Arc<Store>,
}

impl SparqlSearchEngine {
    /// Create a new SPARQL search engine
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }

    /// Search packages by name (semantic search)
    pub fn search_by_name(&self, name: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_name(name);
        self.execute_query(&query)
    }

    /// Search packages by description content
    pub fn search_by_description(&self, text: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_description(text);
        self.execute_query(&query)
    }

    /// Search packages by keyword/category
    pub fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_keyword(keyword);
        self.execute_query(&query)
    }

    /// Find packages by author
    pub fn search_by_author(&self, author: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_author(author);
        self.execute_query(&query)
    }

    /// Get trending packages (sorted by downloads)
    pub fn trending_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::trending_packages(limit);
        self.execute_query(&query)
    }

    /// Get recent packages (newly added)
    pub fn recent_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::recent_packages(limit);
        self.execute_query(&query)
    }

    /// Find high-quality packages (quality score >= threshold)
    pub fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>> {
        let query = Queries::packages_by_quality(min_score);
        self.execute_query(&query)
    }

    /// Get all packages
    pub fn all_packages(&self) -> Result<Vec<String>> {
        let query = Queries::all_packages();
        self.execute_query(&query)
    }

    /// Advanced search with quality tier filtering
    ///
    /// Returns enhanced search results with quality tier classification.
    /// Meets SC-004 latency target of <1 second.
    pub fn search_with_quality(
        &self, query_text: &str, filters: &SearchFilters,
    ) -> Result<(Vec<EnhancedSearchResult>, SearchMetrics)> {
        let start = Instant::now();

        // Execute base search query
        let sparql = self.build_quality_search_query(query_text, filters);
        let raw_results = self.execute_query_with_metadata(&sparql)?;

        // Convert to enhanced results with quality tier calculation
        let mut enhanced: Vec<EnhancedSearchResult> = raw_results
            .into_iter()
            .map(|(uri, downloads, fmea, age, category)| {
                EnhancedSearchResult::new(uri, downloads, fmea, age, category)
            })
            .collect();

        let total_results = enhanced.len();

        // Apply filters
        enhanced.retain(|r| filters.matches(r));

        // Sort by quality tier (Gold first), then by quality score
        enhanced.sort_by(|a, b| {
            // First by tier (Gold=0, Silver=1, Bronze=2, Unrated=3)
            match (a.tier as u8).cmp(&(b.tier as u8)) {
                std::cmp::Ordering::Equal => {
                    // Then by quality score (descending)
                    b.quality_score
                        .partial_cmp(&a.quality_score)
                        .unwrap_or(std::cmp::Ordering::Equal)
                }
                other => other,
            }
        });

        // Apply limit
        enhanced.truncate(filters.limit);

        let elapsed = start.elapsed();
        let query_time_ms = elapsed.as_millis() as u64;
        let latency_target_met = elapsed.as_secs() < 1;

        if !latency_target_met {
            info!(
                "SC-004 latency target exceeded: {}ms for query '{}'",
                query_time_ms, query_text
            );
        }

        let metrics = SearchMetrics {
            query_time_ms,
            total_results,
            filtered_results: enhanced.len(),
            latency_target_met,
        };

        Ok((enhanced, metrics))
    }

    /// Search packages by quality tier
    pub fn search_by_tier(&self, tier: QualityTier, limit: usize) -> Result<Vec<EnhancedSearchResult>> {
        let filters = SearchFilters::new()
            .with_quality_tier(tier)
            .with_limit(limit);
        let (results, _) = self.search_with_quality("", &filters)?;
        Ok(results)
    }

    /// Search for FMEA-validated packages only
    pub fn search_fmea_only(&self, query_text: &str, limit: usize) -> Result<Vec<EnhancedSearchResult>> {
        let filters = SearchFilters::new()
            .with_fmea_only(true)
            .with_limit(limit);
        let (results, _) = self.search_with_quality(query_text, &filters)?;
        Ok(results)
    }

    /// Build SPARQL query for quality-aware search
    fn build_quality_search_query(&self, query_text: &str, filters: &SearchFilters) -> String {
        let mut query = format!(
            r#"
            PREFIX ggen: <{}>
            PREFIX rdf: <{}>

            SELECT ?package ?downloads ?fmeaPassed ?lastUpdated ?category WHERE {{
                ?package rdf:type ggen:Package .
                OPTIONAL {{ ?package ggen:downloads ?downloads }}
                OPTIONAL {{ ?package ggen:fmeaPassed ?fmeaPassed }}
                OPTIONAL {{ ?package ggen:lastUpdated ?lastUpdated }}
                OPTIONAL {{ ?package ggen:category ?category }}
            "#,
            Namespaces::GGEN,
            Namespaces::RDF
        );

        // Add text search filter if provided
        if !query_text.is_empty() {
            query.push_str(&format!(
                r#"
                ?package ggen:name ?name .
                FILTER(CONTAINS(LCASE(str(?name)), LCASE("{}")))
                "#,
                query_text.replace('"', "\\\"")
            ));
        }

        // Add author filter if provided
        if let Some(ref author) = filters.author {
            query.push_str(&format!(
                r#"
                ?package ggen:hasAuthor ?author .
                ?author ggen:name ?authorName .
                FILTER(CONTAINS(LCASE(str(?authorName)), LCASE("{}")))
                "#,
                author.replace('"', "\\\"")
            ));
        }

        // Add quality score filter if provided
        if let Some(min_quality) = filters.min_quality {
            query.push_str(&format!(
                r#"
                ?package ggen:qualityScore ?score .
                FILTER(?score >= {})
                "#,
                min_quality
            ));
        }

        // Close query and add ordering/limit
        query.push_str(&format!(
            r#"
            }}
            ORDER BY DESC(?downloads)
            LIMIT {}
            "#,
            filters.limit * 2 // Fetch extra for post-filtering
        ));

        query
    }

    /// Execute a SPARQL query with metadata extraction
    fn execute_query_with_metadata(
        &self, query: &str,
    ) -> Result<Vec<(String, u32, bool, u32, Option<String>)>> {
        let results = self
            .store
            .query(query)
            .map_err(|e| crate::error::Error::SearchError(format!("SPARQL query failed: {}", e)))?;

        let mut packages = Vec::new();

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(solution) => {
                        let package_uri = solution
                            .get("package")
                            .and_then(|t| match t {
                                oxigraph::model::Term::NamedNode(n) => Some(n.as_str().to_string()),
                                _ => None,
                            })
                            .unwrap_or_default();

                        let downloads = solution
                            .get("downloads")
                            .and_then(|t| match t {
                                oxigraph::model::Term::Literal(l) => l.value().parse().ok(),
                                _ => None,
                            })
                            .unwrap_or(0);

                        let fmea_passed = solution
                            .get("fmeaPassed")
                            .and_then(|t| match t {
                                oxigraph::model::Term::Literal(l) => {
                                    Some(l.value() == "true" || l.value() == "1")
                                }
                                _ => None,
                            })
                            .unwrap_or(false);

                        // Parse lastUpdated to calculate age_days
                        let age_days = solution
                            .get("lastUpdated")
                            .and_then(|t| match t {
                                oxigraph::model::Term::Literal(l) => {
                                    Self::parse_age_from_timestamp(l.value())
                                }
                                _ => None,
                            })
                            .unwrap_or(365); // Default to 1 year if unknown

                        let category = solution
                            .get("category")
                            .and_then(|t| match t {
                                oxigraph::model::Term::Literal(l) => Some(l.value().to_string()),
                                _ => None,
                            });

                        if !package_uri.is_empty() {
                            packages.push((package_uri, downloads, fmea_passed, age_days, category));
                        }
                    }
                    Err(e) => {
                        debug!("Error processing SPARQL solution: {}", e);
                    }
                }
            }
        }

        Ok(packages)
    }

    /// Parse age in days from ISO 8601 timestamp
    fn parse_age_from_timestamp(timestamp: &str) -> Option<u32> {
        use chrono::{DateTime, Utc};

        // Try RFC 3339 format
        if let Ok(dt) = DateTime::parse_from_rfc3339(timestamp) {
            let now = Utc::now();
            let duration = now.signed_duration_since(dt.with_timezone(&Utc));
            return Some(duration.num_days().max(0) as u32);
        }

        // Try date-only format
        if let Ok(date) = chrono::NaiveDate::parse_from_str(timestamp, "%Y-%m-%d") {
            let today = Utc::now().date_naive();
            let duration = today.signed_duration_since(date);
            return Some(duration.num_days().max(0) as u32);
        }

        None
    }

    /// Execute a SPARQL query and extract results
    fn execute_query(&self, query: &str) -> Result<Vec<String>> {
        let results = self
            .store
            .query(query)
            .map_err(|e| crate::error::Error::SearchError(format!("SPARQL query failed: {}", e)))?;

        let mut packages = Vec::new();

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(solution) => {
                        for (_, term) in solution.iter() {
                            if let oxigraph::model::Term::NamedNode(node) = term {
                                packages.push(node.as_str().to_string());
                            }
                        }
                    }
                    Err(e) => {
                        debug!("Error processing SPARQL solution: {}", e);
                    }
                }
            }
        }

        Ok(packages)
    }
}

/// Quality tier classification for search results
///
/// Tiers are ordered from highest to lowest quality for sorting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QualityTier {
    /// Highest quality - FMEA passed, high downloads, recently updated
    Gold = 0,
    /// Good quality - FMEA passed with moderate activity
    Silver = 1,
    /// Basic quality - passes basic validation
    Bronze = 2,
    /// Not yet rated or doesn't meet Bronze criteria
    Unrated = 3,
}

impl QualityTier {
    /// Get display symbol for the tier
    pub fn symbol(&self) -> &'static str {
        match self {
            QualityTier::Gold => "🥇",
            QualityTier::Silver => "🥈",
            QualityTier::Bronze => "🥉",
            QualityTier::Unrated => "○",
        }
    }

    /// Parse tier from string (case-insensitive)
    pub fn from_str_loose(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "gold" => QualityTier::Gold,
            "silver" => QualityTier::Silver,
            "bronze" => QualityTier::Bronze,
            _ => QualityTier::Unrated,
        }
    }

    /// Calculate tier from package metrics
    ///
    /// # Tier Definitions
    /// - **Gold**: FMEA passed + >100 downloads + <30 days old
    /// - **Silver**: FMEA passed + 10-100 downloads OR 30-90 days old
    /// - **Bronze**: Basic validation (any downloads)
    /// - **Unrated**: No downloads or doesn't meet criteria
    pub fn calculate(downloads: u32, fmea_passed: bool, age_days: u32) -> Self {
        // Gold: FMEA passed + >100 downloads + <30 days old
        if fmea_passed && downloads >= 100 && age_days <= 30 {
            return QualityTier::Gold;
        }

        // Silver: FMEA passed + 10-100 downloads OR 30-90 days old
        if fmea_passed && downloads >= 10 && age_days <= 90 {
            return QualityTier::Silver;
        }

        // Bronze: Any downloads
        if downloads > 0 {
            return QualityTier::Bronze;
        }

        QualityTier::Unrated
    }
}

impl Default for QualityTier {
    fn default() -> Self {
        QualityTier::Unrated
    }
}

impl std::fmt::Display for QualityTier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QualityTier::Gold => write!(f, "Gold"),
            QualityTier::Silver => write!(f, "Silver"),
            QualityTier::Bronze => write!(f, "Bronze"),
            QualityTier::Unrated => write!(f, "Unrated"),
        }
    }
}

/// Enhanced search result with quality tier and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnhancedSearchResult {
    /// Package URI or identifier
    pub package_uri: String,
    /// Package name (extracted from URI)
    pub name: String,
    /// Quality tier classification
    pub tier: QualityTier,
    /// Download count
    pub downloads: u32,
    /// Whether FMEA validation passed
    pub fmea_passed: bool,
    /// Days since last update
    pub age_days: u32,
    /// Quality score (0-100)
    pub quality_score: f64,
    /// Category/keywords
    pub category: Option<String>,
}

impl EnhancedSearchResult {
    /// Create a new enhanced search result
    pub fn new(
        package_uri: String, downloads: u32, fmea_passed: bool, age_days: u32,
        category: Option<String>,
    ) -> Self {
        let tier = QualityTier::calculate(downloads, fmea_passed, age_days);
        let quality_score = Self::compute_quality_score(downloads, fmea_passed, age_days);

        // Extract name from URI (last segment)
        let name = package_uri
            .split('/')
            .last()
            .unwrap_or(&package_uri)
            .to_string();

        Self {
            package_uri,
            name,
            tier,
            downloads,
            fmea_passed,
            age_days,
            quality_score,
            category,
        }
    }

    /// Compute quality score (0-100)
    fn compute_quality_score(downloads: u32, fmea_passed: bool, age_days: u32) -> f64 {
        // Downloads: up to 40 points (log scale)
        let download_score = if downloads > 0 {
            (f64::from(downloads).ln() / 10.0 * 40.0).min(40.0)
        } else {
            0.0
        };

        // FMEA: 30 points if passed
        let fmea_score = if fmea_passed { 30.0 } else { 0.0 };

        // Freshness: up to 30 points
        let freshness_score = if age_days <= 7 {
            30.0
        } else if age_days <= 30 {
            25.0
        } else if age_days <= 90 {
            15.0
        } else if age_days <= 365 {
            5.0
        } else {
            0.0
        };

        (download_score + fmea_score + freshness_score).min(100.0)
    }
}

/// Search filters for advanced queries
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SearchFilters {
    /// Minimum quality score (0-100)
    pub min_quality: Option<u32>,

    /// Filter by quality tier (Gold, Silver, Bronze)
    pub quality_tier: Option<QualityTier>,

    /// Author filter
    pub author: Option<String>,

    /// Category/keyword filter
    pub keyword: Option<String>,

    /// Only show FMEA-passed packages
    pub fmea_only: bool,

    /// Maximum results
    pub limit: usize,

    /// Show only best matches (Gold + Silver tiers)
    pub best_match: bool,
}

impl SearchFilters {
    /// Create empty filters
    pub fn new() -> Self {
        Self {
            min_quality: None,
            quality_tier: None,
            author: None,
            keyword: None,
            fmea_only: false,
            limit: 50,
            best_match: false,
        }
    }

    /// Set quality filter
    pub fn with_quality(mut self, min_score: u32) -> Self {
        self.min_quality = Some(min_score);
        self
    }

    /// Set quality tier filter
    pub fn with_quality_tier(mut self, tier: QualityTier) -> Self {
        self.quality_tier = Some(tier);
        self
    }

    /// Set author filter
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set keyword filter
    pub fn with_keyword(mut self, keyword: impl Into<String>) -> Self {
        self.keyword = Some(keyword.into());
        self
    }

    /// Filter to FMEA-only packages
    pub fn with_fmea_only(mut self, fmea_only: bool) -> Self {
        self.fmea_only = fmea_only;
        self
    }

    /// Set result limit
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    /// Enable best-match mode (Gold + Silver only)
    pub fn with_best_match(mut self, best_match: bool) -> Self {
        self.best_match = best_match;
        self
    }

    /// Check if a result passes the filters
    pub fn matches(&self, result: &EnhancedSearchResult) -> bool {
        // Quality score filter
        if let Some(min_quality) = self.min_quality {
            if result.quality_score < f64::from(min_quality) {
                return false;
            }
        }

        // Quality tier filter
        if let Some(ref tier) = self.quality_tier {
            if result.tier != *tier {
                return false;
            }
        }

        // FMEA-only filter
        if self.fmea_only && !result.fmea_passed {
            return false;
        }

        // Best match filter (Gold + Silver only)
        if self.best_match
            && result.tier != QualityTier::Gold
            && result.tier != QualityTier::Silver
        {
            return false;
        }

        // Category/keyword filter
        if let Some(ref keyword) = self.keyword {
            if let Some(ref category) = result.category {
                if !category.to_lowercase().contains(&keyword.to_lowercase()) {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}

impl Default for SearchFilters {
    fn default() -> Self {
        Self::new()
    }
}

/// Search metrics for performance tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchMetrics {
    /// Query execution time in milliseconds
    pub query_time_ms: u64,
    /// Total results before filtering
    pub total_results: usize,
    /// Results after filtering
    pub filtered_results: usize,
    /// Whether latency target (<1s) was met
    pub latency_target_met: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_filters() {
        let filters = SearchFilters::new()
            .with_quality(80)
            .with_author("Alice")
            .with_keyword("database")
            .with_limit(100);

        assert_eq!(filters.min_quality, Some(80));
        assert_eq!(filters.author, Some("Alice".to_string()));
        assert_eq!(filters.keyword, Some("database".to_string()));
        assert_eq!(filters.limit, 100);
    }

    #[test]
    fn test_quality_tier_ordering() {
        // Gold < Silver < Bronze < Unrated (for sorting)
        assert!(QualityTier::Gold < QualityTier::Silver);
        assert!(QualityTier::Silver < QualityTier::Bronze);
        assert!(QualityTier::Bronze < QualityTier::Unrated);
    }

    #[test]
    fn test_quality_tier_symbols() {
        assert_eq!(QualityTier::Gold.symbol(), "🥇");
        assert_eq!(QualityTier::Silver.symbol(), "🥈");
        assert_eq!(QualityTier::Bronze.symbol(), "🥉");
        assert_eq!(QualityTier::Unrated.symbol(), "○");
    }

    #[test]
    fn test_quality_tier_from_str() {
        assert_eq!(QualityTier::from_str_loose("gold"), QualityTier::Gold);
        assert_eq!(QualityTier::from_str_loose("GOLD"), QualityTier::Gold);
        assert_eq!(QualityTier::from_str_loose("Silver"), QualityTier::Silver);
        assert_eq!(QualityTier::from_str_loose("BRONZE"), QualityTier::Bronze);
        assert_eq!(QualityTier::from_str_loose("unknown"), QualityTier::Unrated);
    }

    #[test]
    fn test_quality_tier_calculate_gold() {
        // Gold: FMEA passed + >100 downloads + <30 days
        assert_eq!(QualityTier::calculate(150, true, 15), QualityTier::Gold);
        assert_eq!(QualityTier::calculate(100, true, 30), QualityTier::Gold);
        assert_eq!(QualityTier::calculate(500, true, 0), QualityTier::Gold);
    }

    #[test]
    fn test_quality_tier_calculate_silver() {
        // Silver: FMEA passed + 10-100 downloads OR 30-90 days
        assert_eq!(QualityTier::calculate(50, true, 45), QualityTier::Silver);
        assert_eq!(QualityTier::calculate(10, true, 90), QualityTier::Silver);
        assert_eq!(QualityTier::calculate(20, true, 60), QualityTier::Silver);
    }

    #[test]
    fn test_quality_tier_calculate_bronze() {
        // Bronze: Any downloads but doesn't meet Silver/Gold
        assert_eq!(QualityTier::calculate(5, true, 100), QualityTier::Bronze);
        assert_eq!(QualityTier::calculate(1, false, 365), QualityTier::Bronze);
        assert_eq!(QualityTier::calculate(50, false, 45), QualityTier::Bronze); // No FMEA
    }

    #[test]
    fn test_quality_tier_calculate_unrated() {
        // Unrated: No downloads
        assert_eq!(QualityTier::calculate(0, true, 10), QualityTier::Unrated);
        assert_eq!(QualityTier::calculate(0, false, 0), QualityTier::Unrated);
    }

    #[test]
    fn test_enhanced_search_result() {
        let result = EnhancedSearchResult::new(
            "https://ggen.io/packages/test-pkg".to_string(),
            150,
            true,
            10,
            Some("database".to_string()),
        );

        assert_eq!(result.name, "test-pkg");
        assert_eq!(result.tier, QualityTier::Gold);
        assert_eq!(result.downloads, 150);
        assert!(result.fmea_passed);
        assert!(result.quality_score > 50.0);
    }

    #[test]
    fn test_search_filters_with_quality_tier() {
        let filters = SearchFilters::new()
            .with_quality_tier(QualityTier::Gold)
            .with_fmea_only(true)
            .with_best_match(true);

        assert_eq!(filters.quality_tier, Some(QualityTier::Gold));
        assert!(filters.fmea_only);
        assert!(filters.best_match);
    }

    #[test]
    fn test_search_filters_matches() {
        let filters = SearchFilters::new()
            .with_fmea_only(true)
            .with_best_match(true);

        // Gold with FMEA should match
        let gold_result = EnhancedSearchResult::new(
            "pkg1".to_string(),
            150,
            true,
            10,
            None,
        );
        assert!(filters.matches(&gold_result));

        // Bronze should not match (best_match = true)
        let bronze_result = EnhancedSearchResult::new(
            "pkg2".to_string(),
            5,
            true,
            100,
            None,
        );
        assert!(!filters.matches(&bronze_result));

        // No FMEA should not match
        let no_fmea = EnhancedSearchResult::new(
            "pkg3".to_string(),
            150,
            false,
            10,
            None,
        );
        assert!(!filters.matches(&no_fmea));
    }

    #[test]
    fn test_search_filters_keyword_match() {
        let filters = SearchFilters::new().with_keyword("database");

        let matching = EnhancedSearchResult::new(
            "pkg1".to_string(),
            50,
            true,
            30,
            Some("database-tools".to_string()),
        );
        assert!(filters.matches(&matching));

        let not_matching = EnhancedSearchResult::new(
            "pkg2".to_string(),
            50,
            true,
            30,
            Some("web-framework".to_string()),
        );
        assert!(!filters.matches(&not_matching));

        let no_category = EnhancedSearchResult::new(
            "pkg3".to_string(),
            50,
            true,
            30,
            None,
        );
        assert!(!filters.matches(&no_category));
    }

    #[test]
    fn test_quality_score_computation() {
        // High quality: many downloads, FMEA, fresh
        let high = EnhancedSearchResult::new(
            "high".to_string(),
            1000,
            true,
            5,
            None,
        );
        assert!(high.quality_score > 80.0);

        // Low quality: few downloads, no FMEA, old
        let low = EnhancedSearchResult::new(
            "low".to_string(),
            1,
            false,
            400,
            None,
        );
        assert!(low.quality_score < 30.0);
    }

    #[test]
    fn test_quality_tier_serialization() {
        let tier = QualityTier::Gold;
        let json = serde_json::to_string(&tier).unwrap();
        assert_eq!(json, r#""gold""#);

        let deserialized: QualityTier = serde_json::from_str(&json).unwrap();
        assert_eq!(deserialized, QualityTier::Gold);
    }

    #[test]
    fn test_search_metrics_structure() {
        let metrics = SearchMetrics {
            query_time_ms: 150,
            total_results: 100,
            filtered_results: 25,
            latency_target_met: true,
        };

        assert_eq!(metrics.query_time_ms, 150);
        assert!(metrics.latency_target_met);
    }
}
