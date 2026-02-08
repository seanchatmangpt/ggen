//! SPARQL-based semantic search engine
//!
//! Implements intelligent package discovery using SPARQL queries against
//! the RDF knowledge graph. Enables semantic search, filtering, and ranking.

use crate::error::Result;
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
    #[must_use]
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }

    /// Search packages by name (semantic search)
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn search_by_name(&self, name: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_name(name);
        self.execute_query(&query)
    }

    /// Search packages by description content
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn search_by_description(&self, text: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_description(text);
        self.execute_query(&query)
    }

    /// Search packages by keyword/category
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_keyword(keyword);
        self.execute_query(&query)
    }

    /// Find packages by author
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn search_by_author(&self, author: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_author(author);
        self.execute_query(&query)
    }

    /// Get trending packages (sorted by downloads)
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn trending_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::trending_packages(limit);
        self.execute_query(&query)
    }

    /// Get recent packages (newly added)
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn recent_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::recent_packages(limit);
        self.execute_query(&query)
    }

    /// Find high-quality packages (quality score >= threshold)
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>> {
        let query = Queries::packages_by_quality(min_score);
        self.execute_query(&query)
    }

    /// Get all packages
    ///
    /// # Errors
    ///
    /// * [`Error::SparqlError`] - When the SPARQL query syntax is invalid
    /// * [`Error::SearchError`] - When querying the RDF store fails
    #[must_use]
    pub fn all_packages(&self) -> Result<Vec<String>> {
        let query = Queries::all_packages();
        self.execute_query(&query)
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
    #[must_use]
    pub fn new() -> Self {
        Self {
            min_quality: None,
            author: None,
            keyword: None,
            limit: 50,
        }
    }

    /// Set quality filter
    #[must_use]
    pub fn with_quality(mut self, min_score: u32) -> Self {
        self.min_quality = Some(min_score);
        self
    }

    /// Set author filter
    #[must_use]
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set keyword filter
    #[must_use]
    pub fn with_keyword(mut self, keyword: impl Into<String>) -> Self {
        self.keyword = Some(keyword.into());
        self
    }

    /// Set result limit
    #[must_use]
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
