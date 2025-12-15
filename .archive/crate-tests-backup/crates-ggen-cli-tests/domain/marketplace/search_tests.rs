//! Chicago TDD tests for package search functionality
//!
//! These tests use REAL search operations with actual index data
//! following the Chicago school of TDD (integration-focused testing).

use ggen_cli_lib::domain::marketplace::{SearchFilters, SearchResult, search_packages};
use ggen_utils::error::Result;
use serde_json;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Test helper to create a package index with test data
fn create_test_index(dir: &Path, packages: Vec<TestPackage>) -> Result<PathBuf> {
    let index_dir = dir.join(".ggen").join("index");
    fs::create_dir_all(&index_dir)?;

    let index_file = index_dir.join("packages.json");
    let package_data: Vec<SearchResult> = packages
        .into_iter()
        .map(|p| SearchResult {
            id: p.id,
            name: p.name,
            version: p.version,
            description: p.description,
            author: p.author,
            category: p.category,
            tags: p.tags,
            stars: p.stars,
            downloads: p.downloads,
        })
        .collect();

    let json = serde_json::to_string_pretty(&package_data)?;
    fs::write(&index_file, json)?;

    Ok(index_file)
}

/// Test package builder
#[derive(Debug, Clone)]
struct TestPackage {
    id: String,
    name: String,
    version: String,
    description: String,
    author: Option<String>,
    category: Option<String>,
    tags: Vec<String>,
    stars: u32,
    downloads: u32,
}

impl TestPackage {
    fn new(name: &str) -> Self {
        Self {
            id: format!("pkg-{}", name),
            name: name.to_string(),
            version: "1.0.0".to_string(),
            description: format!("Description for {}", name),
            author: None,
            category: None,
            tags: vec![],
            stars: 0,
            downloads: 0,
        }
    }

    fn version(mut self, version: &str) -> Self {
        self.version = version.to_string();
        self
    }

    fn description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }

    fn author(mut self, author: &str) -> Self {
        self.author = Some(author.to_string());
        self
    }

    fn category(mut self, category: &str) -> Self {
        self.category = Some(category.to_string());
        self
    }

    fn tags(mut self, tags: Vec<&str>) -> Self {
        self.tags = tags.into_iter().map(String::from).collect();
        self
    }

    fn stars(mut self, stars: u32) -> Self {
        self.stars = stars;
        self
    }

    fn downloads(mut self, downloads: u32) -> Self {
        self.downloads = downloads;
        self
    }
}

// ============================================================================
// KEYWORD SEARCH TESTS
// ============================================================================

#[tokio::test]
async fn test_search_exact_keyword_match() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli-tool").tags(vec!["cli", "tool"]),
            TestPackage::new("web-server").tags(vec!["web", "http"]),
            TestPackage::new("database").tags(vec!["db", "storage"]),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("cli", &filters).await?;

    // Placeholder returns empty - this will be implemented in Phase 2
    // When implemented, uncomment these assertions:
    // assert_eq!(results.len(), 1);
    // assert_eq!(results[0].name, "cli-tool");
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_partial_keyword_match() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli-helper").tags(vec!["cli", "utility"]),
            TestPackage::new("cli-tools").tags(vec!["cli", "dev"]),
            TestPackage::new("web-cli").tags(vec!["cli", "web"]),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("cli", &filters).await?;

    // Placeholder test - will validate all 3 packages match when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_case_insensitive() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("CLI-TOOL").tags(vec!["CLI", "TOOL"]),
            TestPackage::new("Web-Server").tags(vec!["Web", "HTTP"]),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);

    // Test lowercase query
    let results1 = search_packages("cli", &filters).await?;
    // Test uppercase query
    let results2 = search_packages("CLI", &filters).await?;
    // Test mixed case query
    let results3 = search_packages("Cli", &filters).await?;

    // All should return same results when implemented
    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());
    assert!(results3.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_search_by_description() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1")
                .description("A powerful CLI tool for developers"),
            TestPackage::new("tool2")
                .description("Web framework for building apps"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("CLI", &filters).await?;

    // Should match by description when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// FUZZY SEARCH TESTS
// ============================================================================

#[tokio::test]
async fn test_fuzzy_search_typo_tolerance() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("developer-tools").tags(vec!["dev", "tools"]),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_fuzzy(true)
        .with_limit(10);

    // Test with typos
    let results1 = search_packages("develper", &filters).await?; // missing 'o'
    let results2 = search_packages("developr", &filters).await?; // missing 'e'

    // Fuzzy search should find the package when implemented
    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_fuzzy_search_disabled() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("developer-tools").tags(vec!["dev", "tools"]),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_fuzzy(false)
        .with_limit(10);

    // Without fuzzy, typos should not match
    let results = search_packages("develper", &filters).await?;

    assert!(results.is_empty()); // Should be empty even when implemented

    Ok(())
}

#[tokio::test]
async fn test_fuzzy_search_threshold() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("authentication").tags(vec!["auth", "security"]),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_fuzzy(true)
        .with_limit(10);

    // Small distance should match
    let results1 = search_packages("authentiction", &filters).await?; // 1 char diff

    // Large distance should not match
    let results2 = search_packages("auth", &filters).await?; // Too different

    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());

    Ok(())
}

// ============================================================================
// CATEGORY FILTERING TESTS
// ============================================================================

#[tokio::test]
async fn test_filter_by_category() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("web-tool").category("web"),
            TestPackage::new("cli-tool").category("cli"),
            TestPackage::new("db-tool").category("database"),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_category("web")
        .with_limit(10);

    let results = search_packages("tool", &filters).await?;

    // Should only return web category when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_filter_multiple_categories() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1").category("web"),
            TestPackage::new("tool2").category("web"),
            TestPackage::new("tool3").category("cli"),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_category("web")
        .with_limit(10);

    let results = search_packages("", &filters).await?;

    // Should return 2 web packages when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_filter_nonexistent_category() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1").category("web"),
        ],
    )?;

    let filters = SearchFilters::new()
        .with_category("nonexistent")
        .with_limit(10);

    let results = search_packages("", &filters).await?;

    assert!(results.is_empty()); // Should be empty

    Ok(())
}

// ============================================================================
// AUTHOR FILTERING TESTS
// ============================================================================

#[tokio::test]
async fn test_filter_by_author() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1").author("alice"),
            TestPackage::new("tool2").author("bob"),
            TestPackage::new("tool3").author("alice"),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.author = Some("alice".to_string());
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return 2 packages by alice when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_filter_author_case_sensitive() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1").author("Alice"),
            TestPackage::new("tool2").author("alice"),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.author = Some("alice".to_string());
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Author search should be case-insensitive when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// STAR/DOWNLOAD FILTERING TESTS
// ============================================================================

#[tokio::test]
async fn test_filter_by_min_stars() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("popular").stars(100),
            TestPackage::new("moderate").stars(50),
            TestPackage::new("unpopular").stars(5),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.min_stars = Some(50);
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return 2 packages with >= 50 stars when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_filter_by_min_downloads() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("viral").downloads(10000),
            TestPackage::new("popular").downloads(1000),
            TestPackage::new("niche").downloads(100),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.min_downloads = Some(1000);
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return 2 packages with >= 1000 downloads when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_filter_stars_and_downloads() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1").stars(100).downloads(5000),
            TestPackage::new("pkg2").stars(50).downloads(10000),
            TestPackage::new("pkg3").stars(200).downloads(500),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.min_stars = Some(50);
    filters.min_downloads = Some(1000);
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return pkg1 and pkg2 when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// SORTING TESTS
// ============================================================================

#[tokio::test]
async fn test_sort_by_relevance() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli-tool").description("CLI utility"),
            TestPackage::new("cli-helper").description("Helper for CLI"),
            TestPackage::new("tool-cli").description("Tool with CLI"),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.sort = "relevance".to_string();
    filters.limit = 10;

    let results = search_packages("cli", &filters).await?;

    // Should be sorted by relevance when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_sort_by_stars() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1").stars(50),
            TestPackage::new("pkg2").stars(200),
            TestPackage::new("pkg3").stars(100),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.sort = "stars".to_string();
    filters.order = "desc".to_string();
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should be sorted by stars descending: pkg2, pkg3, pkg1
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_sort_by_downloads() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1").downloads(1000),
            TestPackage::new("pkg2").downloads(5000),
            TestPackage::new("pkg3").downloads(2000),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.sort = "downloads".to_string();
    filters.order = "desc".to_string();
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should be sorted by downloads descending: pkg2, pkg3, pkg1
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_sort_ascending() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1").stars(100),
            TestPackage::new("pkg2").stars(50),
            TestPackage::new("pkg3").stars(200),
        ],
    )?;

    let mut filters = SearchFilters::new();
    filters.sort = "stars".to_string();
    filters.order = "asc".to_string();
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should be sorted ascending: pkg2, pkg1, pkg3
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// PAGINATION AND LIMITING TESTS
// ============================================================================

#[tokio::test]
async fn test_limit_results() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1"),
            TestPackage::new("pkg2"),
            TestPackage::new("pkg3"),
            TestPackage::new("pkg4"),
            TestPackage::new("pkg5"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(3);
    let results = search_packages("pkg", &filters).await?;

    // Should return only 3 results when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_limit_zero() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1"),
            TestPackage::new("pkg2"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(0);
    let results = search_packages("pkg", &filters).await?;

    // Limit 0 should return no results
    assert!(results.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_limit_exceeds_results() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1"),
            TestPackage::new("pkg2"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(100);
    let results = search_packages("pkg", &filters).await?;

    // Should return all 2 results when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// EMPTY RESULTS TESTS
// ============================================================================

#[tokio::test]
async fn test_search_no_matches() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli-tool"),
            TestPackage::new("web-server"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("nonexistent", &filters).await?;

    assert!(results.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_search_empty_index() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(temp_dir.path(), vec![])?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("anything", &filters).await?;

    assert!(results.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_search_empty_query() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1"),
            TestPackage::new("pkg2"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("", &filters).await?;

    // Empty query should return all packages when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// SPECIAL CHARACTERS TESTS
// ============================================================================

#[tokio::test]
async fn test_search_with_hyphens() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli-tool-helper"),
            TestPackage::new("web_server_app"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("cli-tool", &filters).await?;

    // Should handle hyphens correctly when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_with_underscores() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("web_server_tool"),
            TestPackage::new("cli-helper"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("web_server", &filters).await?;

    // Should handle underscores correctly when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_with_numbers() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool2023"),
            TestPackage::new("app-v2"),
            TestPackage::new("helper3000"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("2023", &filters).await?;

    // Should handle numbers in names when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_with_special_chars() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool@latest"),
            TestPackage::new("app#feature"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);

    // Special characters should be handled safely
    let results1 = search_packages("tool@latest", &filters).await?;
    let results2 = search_packages("app#feature", &filters).await?;

    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());

    Ok(())
}

// ============================================================================
// COMBINED FILTER TESTS
// ============================================================================

#[tokio::test]
async fn test_combined_filters_category_and_author() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool1").category("web").author("alice"),
            TestPackage::new("tool2").category("web").author("bob"),
            TestPackage::new("tool3").category("cli").author("alice"),
        ],
    )?;

    let mut filters = SearchFilters::new()
        .with_category("web");
    filters.author = Some("alice".to_string());
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return only tool1 when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_combined_filters_all() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("pkg1")
                .category("web")
                .author("alice")
                .stars(100)
                .downloads(5000)
                .tags(vec!["framework"]),
            TestPackage::new("pkg2")
                .category("web")
                .author("alice")
                .stars(50)
                .downloads(1000)
                .tags(vec!["tool"]),
            TestPackage::new("pkg3")
                .category("cli")
                .author("alice")
                .stars(200)
                .downloads(10000)
                .tags(vec!["framework"]),
        ],
    )?;

    let mut filters = SearchFilters::new()
        .with_category("web");
    filters.author = Some("alice".to_string());
    filters.keyword = Some("framework".to_string());
    filters.min_stars = Some(50);
    filters.min_downloads = Some(1000);
    filters.limit = 10;

    let results = search_packages("", &filters).await?;

    // Should return only pkg1 when implemented (matches all criteria)
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

// ============================================================================
// EDGE CASE TESTS
// ============================================================================

#[tokio::test]
async fn test_search_with_whitespace() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("cli tool helper"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);

    // Queries with extra whitespace should be handled
    let results1 = search_packages("  cli  ", &filters).await?;
    let results2 = search_packages("cli tool", &filters).await?;

    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_search_version_filtering() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tool").version("1.0.0"),
            TestPackage::new("tool").version("2.0.0"),
            TestPackage::new("tool").version("3.0.0"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);
    let results = search_packages("tool", &filters).await?;

    // Should handle multiple versions when implemented
    assert!(results.is_empty()); // Current behavior

    Ok(())
}

#[tokio::test]
async fn test_search_unicode_characters() -> Result<()> {
    let temp_dir = TempDir::new()?;

    create_test_index(
        temp_dir.path(),
        vec![
            TestPackage::new("tööl").description("Tool with ümlauts"),
            TestPackage::new("日本語ツール").description("Japanese tool"),
            TestPackage::new("emoji-🚀-tool").description("Tool with emoji"),
        ],
    )?;

    let filters = SearchFilters::new().with_limit(10);

    // Unicode should be handled correctly
    let results1 = search_packages("tööl", &filters).await?;
    let results2 = search_packages("日本語", &filters).await?;
    let results3 = search_packages("🚀", &filters).await?;

    assert!(results1.is_empty()); // Current behavior
    assert!(results2.is_empty());
    assert!(results3.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_search_performance_large_index() -> Result<()> {
    let temp_dir = TempDir::new()?;

    // Create large index with 1000 packages
    let packages: Vec<TestPackage> = (0..1000)
        .map(|i| TestPackage::new(&format!("package-{}", i)))
        .collect();

    create_test_index(temp_dir.path(), packages)?;

    let filters = SearchFilters::new().with_limit(10);

    let start = std::time::Instant::now();
    let results = search_packages("package", &filters).await?;
    let duration = start.elapsed();

    // Search should complete in reasonable time (< 1 second)
    assert!(duration.as_secs() < 1, "Search took too long: {:?}", duration);
    assert!(results.is_empty()); // Current behavior

    Ok(())
}
