//! Marketplace search and discovery
//!
//! This module provides search functionality for discovering packages
//! in the marketplace using various criteria and filters.

use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use super::error::GpackResult;

/// Search result for a package query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    /// Package name
    pub name: String,
    /// Latest version
    pub version: String,
    /// Package description
    pub description: Option<String>,
    /// Download count
    pub downloads: u64,
    /// Search relevance score
    pub score: f64,
    /// Package keywords
    pub keywords: Vec<String>,
    /// Package categories
    pub categories: Vec<String>,
    /// Whether package is verified
    pub verified: bool,
}

/// Search query parameters
#[derive(Debug, Clone, Default)]
pub struct SearchQuery {
    /// Search text (name, description, keywords)
    pub text: Option<String>,
    /// Filter by category
    pub category: Option<String>,
    /// Filter by keyword
    pub keywords: HashSet<String>,
    /// Minimum downloads
    pub min_downloads: Option<u64>,
    /// Only verified packages
    pub verified_only: bool,
    /// Sort order
    pub sort_by: SortOrder,
    /// Results per page
    pub per_page: u32,
    /// Page offset
    pub page: u32,
}

/// Sort order for search results
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum SortOrder {
    /// Sort by relevance score (default)
    #[default]
    Relevance,
    /// Sort by download count
    Downloads,
    /// Sort by name alphabetically
    Name,
    /// Sort by recently updated
    RecentlyUpdated,
    /// Sort by recently added
    RecentlyAdded,
}

/// Package search engine
#[derive(Debug)]
pub struct PackageSearch {
    /// Index of searchable packages
    packages: Vec<SearchablePackage>,
    /// Inverted index for text search
    text_index: std::collections::HashMap<String, Vec<usize>>,
}

/// Internal searchable package representation
#[derive(Debug, Clone)]
struct SearchablePackage {
    name: String,
    version: String,
    description: Option<String>,
    downloads: u64,
    keywords: Vec<String>,
    categories: Vec<String>,
    verified: bool,
    updated_at: u64,
    created_at: u64,
}

impl Default for PackageSearch {
    fn default() -> Self {
        Self::new()
    }
}

impl PackageSearch {
    /// Create a new empty search engine
    pub fn new() -> Self {
        Self {
            packages: Vec::new(),
            text_index: std::collections::HashMap::new(),
        }
    }

    /// Add a package to the search index
    pub fn index_package(&mut self, result: SearchResult) {
        let idx = self.packages.len();

        // Index text tokens
        let tokens = self.tokenize(&result.name);
        for token in tokens {
            self.text_index.entry(token).or_default().push(idx);
        }

        if let Some(ref desc) = result.description {
            for token in self.tokenize(desc) {
                self.text_index.entry(token).or_default().push(idx);
            }
        }

        for keyword in &result.keywords {
            for token in self.tokenize(keyword) {
                self.text_index.entry(token).or_default().push(idx);
            }
        }

        // Store package
        self.packages.push(SearchablePackage {
            name: result.name,
            version: result.version,
            description: result.description,
            downloads: result.downloads,
            keywords: result.keywords,
            categories: result.categories,
            verified: result.verified,
            updated_at: 0,
            created_at: 0,
        });
    }

    /// Search packages with query
    pub fn search(&self, query: &SearchQuery) -> GpackResult<Vec<SearchResult>> {
        let mut results: Vec<(usize, f64)> = Vec::new();

        // Text search
        if let Some(ref text) = query.text {
            let tokens = self.tokenize(text);
            let mut matching_indices: std::collections::HashMap<usize, usize> =
                std::collections::HashMap::new();

            for token in &tokens {
                if let Some(indices) = self.text_index.get(token) {
                    for &idx in indices {
                        *matching_indices.entry(idx).or_insert(0) += 1;
                    }
                }
            }

            // Calculate relevance score
            for (idx, match_count) in matching_indices {
                let score = match_count as f64 / tokens.len() as f64;
                results.push((idx, score));
            }
        } else {
            // Return all packages with base score
            for idx in 0..self.packages.len() {
                results.push((idx, 1.0));
            }
        }

        // Apply filters
        results.retain(|(idx, _)| {
            let pkg = &self.packages[*idx];

            // Category filter
            if let Some(ref cat) = query.category {
                if !pkg.categories.contains(cat) {
                    return false;
                }
            }

            // Keywords filter
            if !query.keywords.is_empty() {
                let pkg_keywords: HashSet<_> = pkg.keywords.iter().collect();
                if !query.keywords.iter().any(|k| pkg_keywords.contains(k)) {
                    return false;
                }
            }

            // Min downloads filter
            if let Some(min) = query.min_downloads {
                if pkg.downloads < min {
                    return false;
                }
            }

            // Verified filter
            if query.verified_only && !pkg.verified {
                return false;
            }

            true
        });

        // Sort results
        match query.sort_by {
            SortOrder::Relevance => {
                results.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
            }
            SortOrder::Downloads => {
                results.sort_by(|a, b| {
                    self.packages[b.0]
                        .downloads
                        .cmp(&self.packages[a.0].downloads)
                });
            }
            SortOrder::Name => {
                results.sort_by(|a, b| self.packages[a.0].name.cmp(&self.packages[b.0].name));
            }
            SortOrder::RecentlyUpdated => {
                results.sort_by(|a, b| {
                    self.packages[b.0]
                        .updated_at
                        .cmp(&self.packages[a.0].updated_at)
                });
            }
            SortOrder::RecentlyAdded => {
                results.sort_by(|a, b| {
                    self.packages[b.0]
                        .created_at
                        .cmp(&self.packages[a.0].created_at)
                });
            }
        }

        // Pagination
        let start = (query.page * query.per_page) as usize;
        let end = start + query.per_page as usize;
        let paginated: Vec<_> = results
            .into_iter()
            .skip(start)
            .take(end - start)
            .collect();

        // Convert to SearchResult
        Ok(paginated
            .into_iter()
            .map(|(idx, score)| {
                let pkg = &self.packages[idx];
                SearchResult {
                    name: pkg.name.clone(),
                    version: pkg.version.clone(),
                    description: pkg.description.clone(),
                    downloads: pkg.downloads,
                    score,
                    keywords: pkg.keywords.clone(),
                    categories: pkg.categories.clone(),
                    verified: pkg.verified,
                }
            })
            .collect())
    }

    /// Tokenize text for indexing
    fn tokenize(&self, text: &str) -> Vec<String> {
        text.to_lowercase()
            .split(|c: char| !c.is_alphanumeric() && c != '-' && c != '_')
            .filter(|s| !s.is_empty() && s.len() >= 2)
            .map(String::from)
            .collect()
    }

    /// Get number of indexed packages
    pub fn len(&self) -> usize {
        self.packages.len()
    }

    /// Check if index is empty
    pub fn is_empty(&self) -> bool {
        self.packages.is_empty()
    }
}

impl SearchQuery {
    /// Create a new search query
    pub fn new() -> Self {
        Self {
            per_page: 20,
            ..Default::default()
        }
    }

    /// Set search text
    pub fn text(mut self, text: &str) -> Self {
        self.text = Some(text.to_string());
        self
    }

    /// Set category filter
    pub fn category(mut self, category: &str) -> Self {
        self.category = Some(category.to_string());
        self
    }

    /// Add keyword filter
    pub fn keyword(mut self, keyword: &str) -> Self {
        self.keywords.insert(keyword.to_string());
        self
    }

    /// Set sort order
    pub fn sort_by(mut self, order: SortOrder) -> Self {
        self.sort_by = order;
        self
    }

    /// Set verified only
    pub fn verified_only(mut self) -> Self {
        self.verified_only = true;
        self
    }

    /// Set pagination
    pub fn paginate(mut self, page: u32, per_page: u32) -> Self {
        self.page = page;
        self.per_page = per_page;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_result(name: &str, downloads: u64) -> SearchResult {
        SearchResult {
            name: name.to_string(),
            version: "1.0.0".to_string(),
            description: Some(format!("{} description", name)),
            downloads,
            score: 0.0,
            keywords: vec!["test".to_string()],
            categories: vec!["development".to_string()],
            verified: true,
        }
    }

    #[test]
    fn test_search_by_name() {
        let mut search = PackageSearch::new();
        search.index_package(create_test_result("my-package", 100));
        search.index_package(create_test_result("other-pkg", 200));

        let query = SearchQuery::new().text("my-package");
        let results = search.search(&query).unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "my-package");
    }

    #[test]
    fn test_sort_by_downloads() {
        let mut search = PackageSearch::new();
        search.index_package(create_test_result("pkg-a", 100));
        search.index_package(create_test_result("pkg-b", 500));
        search.index_package(create_test_result("pkg-c", 200));

        let query = SearchQuery::new().sort_by(SortOrder::Downloads);
        let results = search.search(&query).unwrap();

        assert_eq!(results[0].name, "pkg-b");
        assert_eq!(results[1].name, "pkg-c");
        assert_eq!(results[2].name, "pkg-a");
    }

    #[test]
    fn test_verified_filter() {
        let mut search = PackageSearch::new();
        search.index_package(create_test_result("verified-pkg", 100));

        let mut unverified = create_test_result("unverified-pkg", 200);
        unverified.verified = false;
        search.index_package(unverified);

        let query = SearchQuery::new().verified_only();
        let results = search.search(&query).unwrap();

        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "verified-pkg");
    }

    #[test]
    fn test_pagination() {
        let mut search = PackageSearch::new();
        for i in 0..25 {
            search.index_package(create_test_result(&format!("pkg-{}", i), 100));
        }

        let query = SearchQuery::new().paginate(0, 10);
        let page1 = search.search(&query).unwrap();
        assert_eq!(page1.len(), 10);

        let query = SearchQuery::new().paginate(1, 10);
        let page2 = search.search(&query).unwrap();
        assert_eq!(page2.len(), 10);

        let query = SearchQuery::new().paginate(2, 10);
        let page3 = search.search(&query).unwrap();
        assert_eq!(page3.len(), 5);
    }
}
