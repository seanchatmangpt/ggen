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

/// Registry index structure matching ~/.ggen/registry/index.json or marketplace/registry/index.json
#[derive(Debug, Clone, Serialize, Deserialize)]
struct RegistryIndex {
    #[serde(rename = "updated_at")]
    updated: String,
    packages: Vec<PackageInfo>,
    #[serde(default)]
    search_index: HashMap<String, Vec<String>>,
}

/// Package info from registry index
#[derive(Debug, Clone, Serialize, Deserialize)]
struct PackageInfo {
    name: String,
    version: String,
    category: String,
    description: String,
    tags: Vec<String>,
    #[serde(default)]
    keywords: Vec<String>,
    #[serde(default)]
    author: Option<String>,
    #[serde(default)]
    license: Option<String>,
    #[serde(default)]
    downloads: u32,
    #[serde(default)]
    stars: u32,
    #[serde(default)]
    production_ready: bool,
    #[serde(default)]
    dependencies: Vec<String>,
    #[serde(default)]
    path: Option<String>, // Path in GitHub repo (e.g., "marketplace/packages/agent-cli-copilot")
    #[serde(default)]
    download_url: Option<String>, // GitHub archive URL
    #[serde(default)]
    checksum: Option<String>, // SHA256 checksum
}

/// Package metadata converted from PackageInfo for search
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
    stars: u32,
    license: Option<String>,
}

impl From<PackageInfo> for PackageMetadata {
    fn from(info: PackageInfo) -> Self {
        Self {
            id: info.name.clone(),
            name: info.name,
            description: info.description,
            tags: info.tags,
            keywords: info.keywords,
            category: info.category,
            author: info.author.unwrap_or_else(|| "unknown".to_string()),
            latest_version: info.version,
            downloads: info.downloads,
            stars: info.stars,
            license: info.license,
        }
    }
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

/// Load registry index from GitHub Pages or local filesystem
///
/// Priority order:
/// 1. GitHub Pages (production): https://seanchatmangpt.github.io/ggen/marketplace/registry/index.json
/// 2. Environment variable override: GGEN_REGISTRY_URL
/// 3. Local cache: ~/.ggen/registry/index.json
/// 4. Development: marketplace/registry/index.json (for developers)
async fn load_registry_index() -> Result<RegistryIndex> {
    // Determine registry URL
    let registry_url = std::env::var("GGEN_REGISTRY_URL").unwrap_or_else(|_| {
        "https://seanchatmangpt.github.io/ggen/marketplace/registry/index.json".to_string()
    });

    // Try fetching from URL first (for binary users)
    if registry_url.starts_with("http://") || registry_url.starts_with("https://") {
        match fetch_registry_from_url(&registry_url).await {
            Ok(index) => {
                // Cache the fetched registry for offline use
                if let Err(e) = cache_registry_index(&index).await {
                    // Log but don't fail - caching is optional
                    tracing::warn!("Failed to cache registry index: {}", e);
                }
                return Ok(index);
            }
            Err(e) => {
                tracing::debug!("Failed to fetch registry from {}: {}", registry_url, e);
                // Fall through to try local filesystem
            }
        }
    }

    // Fallback to local filesystem (for development and offline use)
    let possible_paths = vec![
        // 1. User's home directory cache (production cache)
        dirs::home_dir().map(|h| h.join(".ggen").join("registry").join("index.json")),
        // 2. Workspace root marketplace/registry/index.json (development)
        std::env::current_dir().ok().and_then(|cwd| {
            // Try current directory and parent directories
            let mut path = cwd.clone();
            for _ in 0..5 {
                let registry = path.join("marketplace").join("registry").join("index.json");
                if registry.exists() {
                    return Some(registry);
                }
                path = match path.parent() {
                    Some(p) => p.to_path_buf(),
                    None => break,
                };
            }
            None
        }),
        // 3. Relative to CARGO_MANIFEST_DIR (compile-time path)
        {
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            let workspace_root = manifest_dir
                .parent()
                .and_then(|p| p.parent())
                .map(|p| p.join("marketplace").join("registry").join("index.json"));
            workspace_root
        },
        // 4. Old registry location for backwards compatibility
        {
            let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
            manifest_dir
                .parent()
                .map(|p| p.join("registry").join("index.json"))
        },
    ];

    // Find first existing registry path
    let registry_path = possible_paths.into_iter().flatten().find(|p| p.exists());

    let registry_path = match registry_path {
        Some(path) => path,
        None => {
            // Return empty index if no registry found
            return Ok(RegistryIndex {
                updated: chrono::Utc::now().to_rfc3339(),
                packages: Vec::new(),
                search_index: HashMap::new(),
            });
        }
    };

    let content = tokio::fs::read_to_string(&registry_path)
        .await
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!(
                "Failed to read registry index {}: {}",
                registry_path.display(),
                e
            ))
        })?;

    // Validate JSON structure before parsing
    let json_value: serde_json::Value = serde_json::from_str(&content).map_err(|e| {
        ggen_utils::error::Error::new(&format!(
            "Invalid JSON in registry index {}: {}. Please check registry format.",
            registry_path.display(),
            e
        ))
    })?;

    // Validate required fields
    if !json_value.is_object() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Invalid registry format: expected object, got {}",
            json_value
        )));
    }

    let index: RegistryIndex = serde_json::from_value(json_value).map_err(|e| {
        ggen_utils::error::Error::new(&format!(
            "Invalid registry structure {}: {}. Expected 'packages' array and 'updated' timestamp.",
            registry_path.display(),
            e
        ))
    })?;

    Ok(index)
}

/// Fetch registry index from HTTP/HTTPS URL with retry logic
async fn fetch_registry_from_url(url: &str) -> Result<RegistryIndex> {
    use reqwest::Client;

    let client = Client::builder()
        .timeout(std::time::Duration::from_secs(30))
        .user_agent(format!("ggen/{}", env!("CARGO_PKG_VERSION")))
        .build()
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create HTTP client: {}", e))
        })?;

    const MAX_RETRIES: u32 = 3;
    let mut last_error = None;

    for attempt in 1..=MAX_RETRIES {
        match client.get(url).send().await {
            Ok(response) => {
                if !response.status().is_success() {
                    let status = response.status();
                    if status.is_client_error() {
                        // Don't retry on 4xx errors
                        return Err(ggen_utils::error::Error::new(&format!(
                            "Registry returned client error {} for URL: {}",
                            status, url
                        )));
                    }
                    last_error = Some(ggen_utils::error::Error::new(&format!(
                        "Registry returned status {} for URL: {}",
                        status, url
                    )));
                } else {
                    match response.json::<RegistryIndex>().await {
                        Ok(index) => {
                            tracing::info!("Successfully fetched registry index from {}", url);
                            return Ok(index);
                        }
                        Err(e) => {
                            last_error = Some(ggen_utils::error::Error::new(&format!(
                                "Failed to parse registry index JSON: {}",
                                e
                            )));
                        }
                    }
                }
            }
            Err(e) => {
                last_error = Some(ggen_utils::error::Error::new(&format!(
                    "Network error fetching registry from {}: {}. Check your internet connection. Falling back to local cache if available.",
                    url, e
                )));
            }
        }

        // Exponential backoff: wait 1s, 2s, 4s
        if attempt < MAX_RETRIES {
            let delay = std::time::Duration::from_secs(1 << (attempt - 1));
            tokio::time::sleep(delay).await;
        }
    }

    Err(last_error.unwrap_or_else(|| {
        ggen_utils::error::Error::new(&format!(
            "Failed to fetch registry after {} attempts",
            MAX_RETRIES
        ))
    }))
}

/// Cache registry index to user's home directory for offline use
async fn cache_registry_index(index: &RegistryIndex) -> Result<()> {
    let cache_dir = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("Could not determine home directory"))?
        .join(".ggen")
        .join("registry");

    // Create cache directory if it doesn't exist
    tokio::fs::create_dir_all(&cache_dir).await.map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create cache directory: {}", e))
    })?;

    let cache_path = cache_dir.join("index.json");
    let content = serde_json::to_string_pretty(index).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to serialize registry index: {}", e))
    })?;

    tokio::fs::write(&cache_path, content).await.map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to write cache file: {}", e))
    })?;

    tracing::debug!("Cached registry index to {}", cache_path.display());
    Ok(())
}

/// Search for packages in the marketplace
///
/// Implements full-text search with:
/// - Keyword matching across name, description, tags, keywords
/// - Fuzzy search with Levenshtein distance
/// - Relevance scoring and ranking
/// - Multiple filter options (category, author, license, stars, downloads)
/// - Sorting by relevance, stars, or downloads
pub async fn search_packages(query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>> {
    // Load registry index (fetches from GitHub Pages or local filesystem)
    let index = load_registry_index().await?;

    // Convert PackageInfo to PackageMetadata and score/filter packages
    let mut scored_packages: Vec<(PackageMetadata, f64)> = index
        .packages
        .into_iter()
        .map(|info| PackageMetadata::from(info))
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
                if pkg
                    .license
                    .as_ref()
                    .map(|l| !l.eq_ignore_ascii_case(license))
                    .unwrap_or(true)
                {
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
                let has_keyword = pkg
                    .keywords
                    .iter()
                    .any(|k| k.to_lowercase().contains(&keyword_lower))
                    || pkg
                        .tags
                        .iter()
                        .any(|t| t.to_lowercase().contains(&keyword_lower));
                if !has_keyword {
                    return None;
                }
            }

            // Calculate relevance score
            let score = if query.is_empty() {
                // Empty query: give all packages a base score so they're included
                1.0
            } else {
                calculate_relevance(&pkg, query, filters.fuzzy)
            };

            // Only include packages with some relevance (score > 0)
            // For empty queries, include all packages
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
    query: &str, category: Option<&str>, keyword: Option<&str>, author: Option<&str>, fuzzy: bool,
    detailed: bool, json: bool, limit: usize,
) -> Result<()> {
    // Build search filters
    let mut filters = SearchFilters::new().with_limit(limit).with_fuzzy(fuzzy);

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
        ggen_utils::alert_info!("{}", json_output);
    } else if results.is_empty() {
        ggen_utils::alert_info!("No packages found matching '{}'", query);
        ggen_utils::alert_info!("\nTry:");
        ggen_utils::alert_info!("  - Using broader search terms");
        ggen_utils::alert_info!("  - Removing filters");
        ggen_utils::alert_info!("  - Using --fuzzy for typo tolerance");
    } else {
        ggen_utils::alert_info!("Found {} package(s) matching '{}':\n", results.len(), query);

        for result in results {
            ggen_utils::alert_info!("📦 {} v{}", result.name, result.version);
            ggen_utils::alert_info!("   {}", result.description);

            if detailed {
                if let Some(author) = result.author {
                    ggen_utils::alert_info!("   Author: {}", author);
                }
                if let Some(category) = result.category {
                    ggen_utils::alert_info!("   Category: {}", category);
                }
                if !result.tags.is_empty() {
                    ggen_utils::alert_info!("   Tags: {}", result.tags.join(", "));
                }
                ggen_utils::alert_info!(
                    "   ⭐ {} stars  📥 {} downloads",
                    result.stars,
                    result.downloads
                );
            }
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
        assert!(
            score >= 100.0,
            "Exact match should have high score: {}",
            score
        );
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
        assert!(
            score_no_fuzzy < 10.0,
            "Unrelated query should have low score"
        );
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
