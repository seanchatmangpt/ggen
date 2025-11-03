//! Domain logic for marketplace package search
//!
//! This module contains the core business logic for searching packages,
//! separated from CLI concerns for better testability and reusability.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Search input options (pure domain type - no CLI dependencies)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchInput {
    /// Search query
    pub query: String,

    /// Filter by category
    pub category: Option<String>,

    /// Filter by keyword
    pub keyword: Option<String>,

    /// Filter by author
    pub author: Option<String>,

    /// Enable fuzzy search
    pub fuzzy: bool,

    /// Show detailed information
    pub detailed: bool,

    /// Output as JSON
    pub json: bool,

    /// Limit number of results
    pub limit: usize,
}

impl SearchInput {
    pub fn new(query: String) -> Self {
        Self {
            query,
            limit: 10,
            ..Default::default()
        }
    }
}

/// Search filters for package discovery
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SearchFilters {
    pub category: Option<String>,
    pub keyword: Option<String>,
    pub author: Option<String>,
    pub license: Option<String>,
    pub min_stars: Option<u32>,
    pub min_downloads: Option<u32>,
    pub sort: String,
    pub order: String,
    pub fuzzy: bool,
    pub limit: usize,
}

impl SearchFilters {
    pub fn new() -> Self {
        Self {
            sort: "relevance".to_string(),
            order: "desc".to_string(),
            limit: 10,
            ..Default::default()
        }
    }

    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.category = Some(category.into());
        self
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }

    pub fn with_fuzzy(mut self, fuzzy: bool) -> Self {
        self.fuzzy = fuzzy;
        self
    }
}

/// Search result representing a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub stars: u32,
    pub downloads: u32,
}

/// Registry index structure matching ~/.ggen/registry/index.json
#[derive(Debug, Clone, Deserialize)]
struct RegistryIndex {
    updated: String,
    packs: HashMap<String, PackageMetadata>,
}

/// Package metadata from registry
#[derive(Debug, Clone, Deserialize)]
struct PackageMetadata {
    id: String,
    name: String,
    description: String,
    tags: Vec<String>,
    keywords: Vec<String>,
    category: String,
    author: String,
    latest_version: String,
    downloads: u32,
    #[serde(default)]
    stars: u32,
    license: Option<String>,
}

/// Calculate Levenshtein distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1 = s1.to_lowercase();
    let s2 = s2.to_lowercase();
    let len1 = s1.len();
    let len2 = s2.len();

    if len1 == 0 {
        return len2;
    }
    if len2 == 0 {
        return len1;
    }

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

/// Calculate relevance score for a package match
fn calculate_relevance(pkg: &PackageMetadata, query: &str, fuzzy: bool) -> f64 {
    let query_lower = query.to_lowercase();
    let name_lower = pkg.name.to_lowercase();
    let desc_lower = pkg.description.to_lowercase();

    let mut score = 0.0;

    // Exact name match: highest score
    if name_lower == query_lower {
        score += 100.0;
    }
    // Name contains query
    else if name_lower.contains(&query_lower) {
        score += 50.0;
    }
    // Fuzzy name match
    else if fuzzy {
        let distance = levenshtein_distance(&name_lower, &query_lower);
        let max_len = std::cmp::max(name_lower.len(), query_lower.len());
        let similarity = 1.0 - (distance as f64 / max_len as f64);
        if similarity > 0.7 {
            score += similarity * 30.0;
        }
    }

    // Description contains query
    if desc_lower.contains(&query_lower) {
        score += 20.0;
    }

    // Query words match tags or keywords
    for word in query.split_whitespace() {
        let word_lower = word.to_lowercase();

        for tag in &pkg.tags {
            if tag.to_lowercase().contains(&word_lower) {
                score += 10.0;
            }
        }

        for keyword in &pkg.keywords {
            if keyword.to_lowercase().contains(&word_lower) {
                score += 10.0;
            }
        }
    }

    // Only boost by popularity if there's already some relevance
    if score > 0.0 {
        score += (pkg.downloads as f64 / 1000.0).min(10.0);
        score += (pkg.stars as f64 / 10.0).min(5.0);
    }

    score
}

/// Load registry index from file system
fn load_registry_index() -> Result<RegistryIndex> {
    let registry_path = dirs::home_dir()
        .ok_or_else(|| anyhow::anyhow!("Could not determine home directory"))?
        .join(".ggen")
        .join("registry")
        .join("index.json");

    // Fallback to project registry for development/testing
    let registry_path = if registry_path.exists() {
        registry_path
    } else {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Could not determine parent directory"))?
            .join("registry")
            .join("index.json")
    };

    if !registry_path.exists() {
        // Return empty index if no registry found
        return Ok(RegistryIndex {
            updated: chrono::Utc::now().to_rfc3339(),
            packs: HashMap::new(),
        });
    }

    let content = std::fs::read_to_string(&registry_path)
        .map_err(|e| anyhow::anyhow!("Failed to read registry index: {}", e))?;

    let index: RegistryIndex = serde_json::from_str(&content)
        .map_err(|e| anyhow::anyhow!("Failed to parse registry index: {}", e))?;

    Ok(index)
}

/// Search for packages in the marketplace
///
/// Implements full-text search with:
/// - Keyword matching across name, description, tags, keywords
/// - Fuzzy search with Levenshtein distance
/// - Relevance scoring and ranking
/// - Multiple filter options (category, author, license, stars, downloads)
/// - Sorting by relevance, stars, or downloads
pub async fn search_packages(
    query: &str,
    filters: &SearchFilters,
) -> Result<Vec<SearchResult>> {
    // Load registry index
    let index = load_registry_index()?;

    // Score and filter packages
    let mut scored_packages: Vec<(PackageMetadata, f64)> = index
        .packs
        .into_values()
        .filter_map(|pkg| {
            // Apply filters
            if let Some(ref category) = filters.category {
                if !pkg.category.eq_ignore_ascii_case(category) {
                    return None;
                }
            }

            if let Some(ref author) = filters.author {
                if !pkg.author.eq_ignore_ascii_case(author) {
                    return None;
                }
            }

            if let Some(ref license) = filters.license {
                if pkg.license.as_ref().map(|l| !l.eq_ignore_ascii_case(license)).unwrap_or(true) {
                    return None;
                }
            }

            if let Some(min_stars) = filters.min_stars {
                if pkg.stars < min_stars {
                    return None;
                }
            }

            if let Some(min_downloads) = filters.min_downloads {
                if pkg.downloads < min_downloads {
                    return None;
                }
            }

            if let Some(ref keyword) = filters.keyword {
                let keyword_lower = keyword.to_lowercase();
                let has_keyword = pkg.keywords.iter().any(|k| k.to_lowercase().contains(&keyword_lower))
                    || pkg.tags.iter().any(|t| t.to_lowercase().contains(&keyword_lower));
                if !has_keyword {
                    return None;
                }
            }

            // Calculate relevance score
            let score = calculate_relevance(&pkg, query, filters.fuzzy);

            // Only include packages with some relevance (score > 0)
            if score > 0.0 {
                Some((pkg, score))
            } else {
                None
            }
        })
        .collect();

    // Sort by criteria
    match filters.sort.as_str() {
        "stars" => {
            scored_packages.sort_by(|a, b| {
                let cmp = b.0.stars.cmp(&a.0.stars);
                if filters.order == "asc" {
                    cmp.reverse()
                } else {
                    cmp
                }
            });
        }
        "downloads" => {
            scored_packages.sort_by(|a, b| {
                let cmp = b.0.downloads.cmp(&a.0.downloads);
                if filters.order == "asc" {
                    cmp.reverse()
                } else {
                    cmp
                }
            });
        }
        _ => {
            // Default: sort by relevance
            scored_packages.sort_by(|a, b| {
                let cmp = b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal);
                if filters.order == "asc" {
                    cmp.reverse()
                } else {
                    cmp
                }
            });
        }
    }

    // Apply limit and convert to SearchResult
    let results: Vec<SearchResult> = scored_packages
        .into_iter()
        .take(filters.limit)
        .map(|(pkg, _score)| SearchResult {
            id: pkg.id,
            name: pkg.name,
            version: pkg.latest_version,
            description: pkg.description,
            author: Some(pkg.author),
            category: Some(pkg.category),
            tags: pkg.tags,
            stars: pkg.stars,
            downloads: pkg.downloads,
        })
        .collect();

    Ok(results)
}

/// Search for packages and display results
///
/// This function bridges the CLI to the domain layer.
pub async fn search_and_display(
    query: &str,
    category: Option<&str>,
    keyword: Option<&str>,
    author: Option<&str>,
    fuzzy: bool,
    detailed: bool,
    json: bool,
    limit: usize,
) -> Result<()> {
    // Build search filters
    let mut filters = SearchFilters::new()
        .with_limit(limit)
        .with_fuzzy(fuzzy);

    if let Some(cat) = category {
        filters = filters.with_category(cat);
    }
    if let Some(kw) = keyword {
        filters.keyword = Some(kw.to_string());
    }
    if let Some(auth) = author {
        filters.author = Some(auth.to_string());
    }

    // Search packages
    let results = search_packages(query, &filters).await?;

    // Display results
    if json {
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else if results.is_empty() {
        println!("No packages found matching '{}'", query);
        println!("\nTry:");
        println!("  - Using broader search terms");
        println!("  - Removing filters");
        println!("  - Using --fuzzy for typo tolerance");
    } else {
        println!("Found {} package(s) matching '{}':\n", results.len(), query);

        for result in results {
            println!("📦 {} v{}", result.name, result.version);
            println!("   {}", result.description);

            if detailed {
                if let Some(author) = result.author {
                    println!("   Author: {}", author);
                }
                if let Some(category) = result.category {
                    println!("   Category: {}", category);
                }
                if !result.tags.is_empty() {
                    println!("   Tags: {}", result.tags.join(", "));
                }
                println!("   ⭐ {} stars  📥 {} downloads", result.stars, result.downloads);
            }

            println!();
        }
    }

    Ok(())
}

/// Execute search with input (pure domain function)
pub async fn execute_search(input: SearchInput) -> Result<Vec<SearchResult>> {
    let filters = SearchFilters {
        category: input.category,
        keyword: input.keyword,
        author: input.author,
        fuzzy: input.fuzzy,
        limit: input.limit,
        ..Default::default()
    };
    
    search_packages(&input.query, &filters).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_filters_builder() {
        let filters = SearchFilters::new()
            .with_category("web")
            .with_limit(5)
            .with_fuzzy(true);

        assert_eq!(filters.category, Some("web".to_string()));
        assert_eq!(filters.limit, 5);
        assert!(filters.fuzzy);
        assert_eq!(filters.sort, "relevance");
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("hello", "hello"), 0);
        assert_eq!(levenshtein_distance("hello", "helo"), 1);
        assert_eq!(levenshtein_distance("rust", "rest"), 1);
        assert_eq!(levenshtein_distance("python", "pyton"), 1);
        assert_eq!(levenshtein_distance("javascript", "java"), 6);
    }

    #[test]
    fn test_relevance_calculation_exact_match() {
        let pkg = PackageMetadata {
            id: "test".to_string(),
            name: "Rust CLI".to_string(),
            description: "A Rust CLI tool".to_string(),
            tags: vec!["rust".to_string(), "cli".to_string()],
            keywords: vec!["command-line".to_string()],
            category: "rust".to_string(),
            author: "test".to_string(),
            latest_version: "1.0.0".to_string(),
            downloads: 1000,
            stars: 50,
            license: Some("MIT".to_string()),
        };

        // Exact name match should give highest score
        let score = calculate_relevance(&pkg, "rust cli", false);
        assert!(score >= 100.0, "Exact match should have high score: {}", score);
    }

    #[test]
    fn test_relevance_calculation_fuzzy_match() {
        let pkg = PackageMetadata {
            id: "test".to_string(),
            name: "Rust Web".to_string(),
            description: "A Rust web framework".to_string(),
            tags: vec!["rust".to_string(), "web".to_string()],
            keywords: vec!["http".to_string()],
            category: "rust".to_string(),
            author: "test".to_string(),
            latest_version: "1.0.0".to_string(),
            downloads: 500,
            stars: 25,
            license: Some("MIT".to_string()),
        };

        // Fuzzy match with typo should still match
        let score_fuzzy = calculate_relevance(&pkg, "rst web", true);
        assert!(score_fuzzy > 0.0, "Fuzzy match should have some score");

        // Without fuzzy, no match
        let score_no_fuzzy = calculate_relevance(&pkg, "xyz", false);
        assert!(score_no_fuzzy < 10.0, "Unrelated query should have low score");
    }

    #[tokio::test]
    async fn test_search_packages_real_index() {
        let filters = SearchFilters::new();
        let results = search_packages("rust", &filters).await;

        // Should not error even if index doesn't exist (returns empty)
        assert!(results.is_ok());
    }

    #[tokio::test]
    async fn test_search_packages_with_limit() {
        let filters = SearchFilters::new().with_limit(1);
        let results = search_packages("cli", &filters).await.unwrap();

        // Should respect limit
        assert!(results.len() <= 1);
    }

    #[tokio::test]
    async fn test_search_packages_with_category_filter() {
        let filters = SearchFilters::new().with_category("rust");
        let results = search_packages("web", &filters).await.unwrap();

        // All results should be in rust category
        for result in results {
            assert_eq!(result.category, Some("rust".to_string()));
        }
    }

    #[tokio::test]
    async fn test_search_packages_with_fuzzy() {
        let filters = SearchFilters::new().with_fuzzy(true);
        let results = search_packages("pyton", &filters).await;

        // Fuzzy search should not error
        assert!(results.is_ok());
    }
}
