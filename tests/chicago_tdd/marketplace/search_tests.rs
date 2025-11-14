//! Chicago TDD Tests for Marketplace Search
//!
//! These tests use REAL registry files and verify actual search behavior
//! following the Classicist School of TDD.

use chicago_tdd_tools::async_test;
use chicago_tdd_tools::prelude::*;
use ggen_domain::marketplace::search::{search_packages, SearchFilters};
use ggen_utils::error::Result;
use std::fs;
use tempfile::TempDir;

/// Create a test registry with real packages
/// Returns the temp dir - caller must set CARGO_MANIFEST_DIR before calling search_packages
fn create_test_registry() -> Result<TempDir> {
    let temp_dir = TempDir::new().unwrap();

    // Create registry in parent dir to match expected structure
    // search_packages looks for: CARGO_MANIFEST_DIR/../registry/index.json
    let parent_dir = temp_dir.path();
    let registry_path = parent_dir.join("registry");
    fs::create_dir_all(&registry_path).unwrap();

    let index_content = r#"{
  "updated": "2025-01-15T00:00:00Z",
  "packs": {
    "io.ggen.rust.cli": {
      "id": "io.ggen.rust.cli",
      "name": "Rust CLI Template",
      "description": "Generate Rust CLI applications with clap",
      "tags": ["rust", "cli", "clap"],
      "keywords": ["command-line", "terminal", "argument-parsing"],
      "category": "rust",
      "author": "ggen-team",
      "latest_version": "1.0.0",
      "downloads": 5000,
      "stars": 100,
      "license": "MIT"
    },
    "io.ggen.rust.web": {
      "id": "io.ggen.rust.web",
      "name": "Rust Web Service",
      "description": "Generate Rust web services with Axum framework",
      "tags": ["rust", "web", "axum", "http"],
      "keywords": ["web-service", "api", "rest", "async"],
      "category": "rust",
      "author": "ggen-team",
      "latest_version": "2.1.0",
      "downloads": 8500,
      "stars": 250,
      "license": "MIT"
    },
    "io.ggen.python.api": {
      "id": "io.ggen.python.api",
      "name": "Python FastAPI",
      "description": "Generate Python REST APIs with FastAPI",
      "tags": ["python", "api", "fastapi", "rest"],
      "keywords": ["web-api", "rest", "async", "openapi"],
      "category": "python",
      "author": "python-dev",
      "latest_version": "3.0.0",
      "downloads": 12000,
      "stars": 500,
      "license": "Apache-2.0"
    },
    "io.ggen.typescript.react": {
      "id": "io.ggen.typescript.react",
      "name": "TypeScript React App",
      "description": "Generate React applications with TypeScript",
      "tags": ["typescript", "react", "frontend", "spa"],
      "keywords": ["web", "frontend", "ui", "components"],
      "category": "typescript",
      "author": "frontend-team",
      "latest_version": "1.5.0",
      "downloads": 15000,
      "stars": 800,
      "license": "MIT"
    },
    "io.ggen.rust.microservice": {
      "id": "io.ggen.rust.microservice",
      "name": "Rust Microservice",
      "description": "Generate production-ready Rust microservices",
      "tags": ["rust", "microservice", "kubernetes", "docker"],
      "keywords": ["cloud", "distributed", "scalable", "production"],
      "category": "rust",
      "author": "cloud-team",
      "latest_version": "2.0.0",
      "downloads": 3500,
      "stars": 150,
      "license": "MIT"
    }
  }
}"#;

    fs::write(registry_path.join("index.json"), index_content).unwrap();

    // Create a fake cli subdirectory to match the CARGO_MANIFEST_DIR/../registry structure
    let cli_dir = parent_dir.join("cli");
    fs::create_dir_all(&cli_dir).unwrap();

    // Create a fake home directory with .ggen/registry to override ~/.ggen
    let fake_home = parent_dir.join("home");
    let home_registry = fake_home.join(".ggen").join("registry");
    fs::create_dir_all(&home_registry).unwrap();
    fs::write(home_registry.join("index.json"), index_content).unwrap();

    // Set HOME to fake home so ~/.ggen/registry/index.json is found first
    std::env::set_var("HOME", fake_home.to_str().unwrap());

    // Set CARGO_MANIFEST_DIR as fallback
    std::env::set_var("CARGO_MANIFEST_DIR", cli_dir.to_str().unwrap());

    Ok(temp_dir)
}

async_test!(test_search_exact_name_match, {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act
    let filters = SearchFilters::new();
    let results = search_packages("Rust CLI Template", &filters)
        .await
        .unwrap();

    // Assert: Should find exact match first
    assert!(!results.is_empty(), "Should find exact match");
    assert_eq!(results[0].id, "io.ggen.rust.cli");
    assert_eq!(results[0].name, "Rust CLI Template");
});

async_test!(test_search_partial_match, {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act
    let filters = SearchFilters::new();
    let results = search_packages("rust", &filters).await.unwrap();

    // Assert: Should find all rust packages
    assert!(results.len() >= 3, "Should find at least 3 rust packages");

    // All results should be rust-related
    for result in &results {
        let is_rust = result.name.to_lowercase().contains("rust")
            || result.description.to_lowercase().contains("rust")
            || result
                .tags
                .iter()
                .any(|t| t.to_lowercase().contains("rust"));
        assert!(is_rust, "Result {} should be rust-related", result.name);
    }
});

async_test!(test_search_fuzzy_matching, {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act: Search with typo
    let filters = SearchFilters::new().with_fuzzy(true);
    let results = search_packages("rst cli", &filters).await.unwrap();

    // Assert: Should still find rust CLI packages with fuzzy matching
    assert!(
        !results.is_empty(),
        "Fuzzy search should find results despite typo"
    );
});

async_test!(test_search_category_filter, {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act
    let filters = SearchFilters::new().with_category("rust");
    let results = search_packages("web", &filters).await.unwrap();

    // Assert: Should only find rust web packages
    for result in &results {
        assert_eq!(result.category, Some("rust".to_string()));
    }
});

async_test!(test_search_keyword_filter, {
    // Arrange
    let _temp = create_test_registry().unwrap();

    // Act
    let mut filters = SearchFilters::new();
    filters.keyword = Some("rest".to_string());

    let results = search_packages("api", &filters).await.unwrap();

    // Assert: Should only find packages with "rest" keyword
    assert!(
        !results.is_empty(),
        "Should find packages with rest keyword"
    );
});

async_test!(test_search_author_filter, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.author = Some("ggen-team".to_string());

    let results = search_packages("rust", &filters).await.unwrap();

    // All results should be by ggen-team
    for result in &results {
        assert_eq!(result.author, Some("ggen-team".to_string()));
    }

    // Assert
});

async_test!(test_search_min_stars_filter, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.min_stars = Some(200);

    let results = search_packages("rust", &filters).await.unwrap();

    // All results should have >= 200 stars
    for result in &results {
        assert!(
            result.stars >= 200,
            "Package {} has {} stars, expected >= 200",
            result.name,
            result.stars
        );
    }

    // Assert
});

async_test!(test_search_min_downloads_filter, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.min_downloads = Some(10000);

    let results = search_packages("api", &filters).await.unwrap();

    // All results should have >= 10000 downloads
    for result in &results {
        assert!(
            result.downloads >= 10000,
            "Package {} has {} downloads, expected >= 10000",
            result.name,
            result.downloads
        );
    }

    // Assert
});

async_test!(test_search_sort_by_downloads, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.sort = "downloads".to_string();
    filters.order = "desc".to_string();
    filters.limit = 10;

    let results = search_packages("rust", &filters).await.unwrap();

    // Results should be sorted by downloads descending
    for i in 0..results.len().saturating_sub(1) {
        assert!(
            results[i].downloads >= results[i + 1].downloads,
            "Downloads not sorted: {} ({}) vs {} ({})",
            results[i].name,
            results[i].downloads,
            results[i + 1].name,
            results[i + 1].downloads
        );
    }

    // Assert
});

async_test!(test_search_sort_by_stars, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.sort = "stars".to_string();
    filters.order = "desc".to_string();
    filters.limit = 10;

    let results = search_packages("rust", &filters).await.unwrap();

    // Results should be sorted by stars descending
    for i in 0..results.len().saturating_sub(1) {
        assert!(
            results[i].stars >= results[i + 1].stars,
            "Stars not sorted: {} ({}) vs {} ({})",
            results[i].name,
            results[i].stars,
            results[i + 1].name,
            results[i + 1].stars
        );
    }

    // Assert
});

async_test!(test_search_limit, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new().with_limit(2);
    let results = search_packages("rust", &filters).await.unwrap();

    // Should respect limit
    assert!(results.len() <= 2, "Results should be limited to 2");

    // Assert
});

async_test!(test_search_relevance_ranking, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new();
    let results = search_packages("rust web", &filters).await.unwrap();

    // "Rust Web Service" should rank higher than generic rust packages
    assert!(!results.is_empty());

    // First result should be the most relevant (contains both "rust" and "web")
    let first = &results[0];
    let has_rust =
        first.name.to_lowercase().contains("rust") || first.tags.contains(&"rust".to_string());
    let has_web =
        first.name.to_lowercase().contains("web") || first.tags.contains(&"web".to_string());

    assert!(
        has_rust && has_web,
        "Most relevant result should contain both rust and web"
    );

    // Assert
});

async_test!(test_search_no_results, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new();
    let results = search_packages("nonexistent-language-xyz", &filters)
        .await
        .unwrap();

    // Should return empty results, not error
    assert!(
        results.is_empty(),
        "Nonexistent query should return empty results"
    );

    // Assert
});

async_test!(test_search_performance, {
    let _temp = create_test_registry().unwrap();

    let start = std::time::Instant::now();
    let filters = SearchFilters::new();
    let _results = search_packages("rust", &filters).await.unwrap();
    let elapsed = start.elapsed();

    // Performance target: <100ms for small registry
    // For larger registries (1000+), this should still be under 100ms
    assert!(
        elapsed.as_millis() < 100,
        "Search took {}ms, expected <100ms",
        elapsed.as_millis()
    );

    // Assert
});

async_test!(test_search_multiple_filters_combined, {
    let _temp = create_test_registry().unwrap();

    let mut filters = SearchFilters::new();
    filters.category = Some("rust".to_string());
    filters.min_stars = Some(100);
    filters.sort = "downloads".to_string();
    filters.limit = 5;

    let results = search_packages("web", &filters).await.unwrap();

    // All results should match all filters
    for result in &results {
        assert_eq!(result.category, Some("rust".to_string()));
        assert!(result.stars >= 100);
    }

    // Should be sorted by downloads
    for i in 0..results.len().saturating_sub(1) {
        assert!(results[i].downloads >= results[i + 1].downloads);
    }

    // Assert
});

async_test!(test_search_empty_query, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new();
    let results = search_packages("", &filters).await.unwrap();

    // Empty query might return all packages or none, but shouldn't error
    // Result is already unwrapped, so just check it exists
    let _ = results;

    // Assert
});

async_test!(test_search_tag_matching, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new();
    let results = search_packages("axum", &filters).await.unwrap();

    // Should find packages tagged with "axum"
    let has_axum = results.iter().any(|r| r.tags.contains(&"axum".to_string()));
    assert!(has_axum, "Should find packages with axum tag");

    // Assert
});

async_test!(test_search_case_insensitive, {
    let _temp = create_test_registry().unwrap();

    let filters = SearchFilters::new();

    let results_lower = search_packages("rust", &filters).await.unwrap();
    let results_upper = search_packages("RUST", &filters).await.unwrap();
    let results_mixed = search_packages("RuSt", &filters).await.unwrap();

    // All should return same results (case insensitive)
    assert_eq!(results_lower.len(), results_upper.len());
    assert_eq!(results_lower.len(), results_mixed.len());

    // Assert
});
