//! SPARQL-based semantic search engine
//!
//! Implements intelligent package discovery using SPARQL queries against
//! the RDF knowledge graph. Enables semantic search, filtering, and ranking.

use crate::error::Result;
use crate::models::{Package, QualityScore};
use crate::ontology::Queries;
use oxigraph::store::Store;
use std::sync::Arc;
use tracing::debug;

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
    pub async fn search_by_name(&self, name: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_name(name);
        self.execute_query(&query).await
    }

    /// Search packages by description content
    pub async fn search_by_description(&self, text: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_description(text);
        self.execute_query(&query).await
    }

    /// Search packages by keyword/category
    pub async fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_keyword(keyword);
        self.execute_query(&query).await
    }

    /// Find packages by author
    pub async fn search_by_author(&self, author: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_author(author);
        self.execute_query(&query).await
    }

    /// Get trending packages (sorted by downloads)
    pub async fn trending_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::trending_packages(limit);
        self.execute_query(&query).await
    }

    /// Get recent packages (newly added)
    pub async fn recent_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::recent_packages(limit);
        self.execute_query(&query).await
    }

    /// Find high-quality packages (quality score >= threshold)
    pub async fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>> {
        let query = Queries::packages_by_quality(min_score);
        self.execute_query(&query).await
    }

    /// Get all packages
    pub async fn all_packages(&self) -> Result<Vec<String>> {
        let query = Queries::all_packages();
        self.execute_query(&query).await
    }

    /// Execute a SPARQL query and extract results
    async fn execute_query(&self, query: &str) -> Result<Vec<String>> {
        let results = self
            .store
            .query(query)
            .map_err(|e| crate::error::Error::SearchError(format!("SPARQL query failed: {}", e)))?;

        let mut packages = Vec::new();

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(solution) => {
                        for binding in solution.iter() {
                            if let Some(term) = binding.value() {
                                if let oxigraph::model::Term::NamedNode(node) = term {
                                    packages.push(node.as_str().to_string());
                                }
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

/// Search filters for advanced queries
#[derive(Clone, Debug)]
pub struct SearchFilters {
    /// Minimum quality score (0-100)
    pub min_quality: Option<u32>,

    /// Author filter
    pub author: Option<String>,

    /// Category/keyword filter
    pub keyword: Option<String>,

    /// Maximum results
    pub limit: usize,
}

impl SearchFilters {
    /// Create empty filters
    pub fn new() -> Self {
        Self {
            min_quality: None,
            author: None,
            keyword: None,
            limit: 50,
        }
    }

    /// Set quality filter
    pub fn with_quality(mut self, min_score: u32) -> Self {
        self.min_quality = Some(min_score);
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

    /// Set result limit
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

impl Default for SearchFilters {
    fn default() -> Self {
        Self::new()
    }
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
}
