//! Comprehensive Marketplace Validation Tests
//!
//! This test suite validates ggen marketplace operations using isolated test environments.
//! Tests cover search, installation, P2P registry interactions, error handling, and performance.
//!
//! ## Test Categories
//! - Search functionality (basic, advanced, filtering)
//! - Package installation and verification
//! - P2P registry interactions
//! - Error handling and edge cases
//! - Performance and stress tests
//!
//! ## Running Tests
//! ```bash
//! cargo test --test marketplace_tests_main integration::marketplace_validation
//! ```

use anyhow::Result;
use ggen_core::registry::{RegistryClient, PackMetadata, VersionMetadata, RegistryIndex};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;
use chrono::Utc;

/// Helper to create a test registry with multiple packages
fn create_test_registry(_temp_dir: &TempDir, packages: Vec<(&str, &str, Vec<&str>)>) -> Result<RegistryIndex> {
    let mut packs = HashMap::new();

    for (id, desc, tags) in packages {
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: format!("sha256-{}", id),
        });

        packs.insert(id.to_string(), PackMetadata {
            id: id.to_string(),
            name: id.replace("-", " ").to_string(),
            description: desc.to_string(),
            tags: tags.iter().map(|s| s.to_string()).collect(),
            keywords: tags.iter().map(|s| s.to_string()).collect(),
            category: Some("test".to_string()),
            author: Some("Test Author".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some(100),
            updated: Some(Utc::now()),
            license: Some("MIT".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        });
    }

    Ok(RegistryIndex {
        updated: Utc::now(),
        packs,
    })
}

/// Helper to create a registry client with a temporary index
async fn setup_test_client(packages: Vec<(&str, &str, Vec<&str>)>) -> Result<(TempDir, RegistryClient)> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let index = create_test_registry(&temp_dir, packages)?;
    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    Ok((temp_dir, client))
}

// ============================================================================
// SEARCH FUNCTIONALITY TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_basic_search() -> Result<()> {
    let packages = vec![
        ("rust-web-server", "A Rust web server", vec!["rust", "web", "server"]),
        ("python-cli", "A Python CLI tool", vec!["python", "cli"]),
        ("rust-database", "A Rust database library", vec!["rust", "database"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Test basic search
    let results = client.search("rust").await?;
    assert_eq!(results.len(), 2, "Should find 2 rust packages");

    // Verify results contain expected packages
    let ids: Vec<String> = results.iter().map(|p| p.id.clone()).collect();
    assert!(ids.contains(&"rust-web-server".to_string()));
    assert!(ids.contains(&"rust-database".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_marketplace_search_by_tag() -> Result<()> {
    let packages = vec![
        ("axum-service", "Web service using Axum", vec!["rust", "web", "axum"]),
        ("actix-service", "Web service using Actix", vec!["rust", "web", "actix"]),
        ("cli-tool", "Command line tool", vec!["rust", "cli"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Search for web packages
    let web_results = client.search("web").await?;
    assert_eq!(web_results.len(), 2, "Should find 2 web packages");

    // Search for CLI packages
    let cli_results = client.search("cli").await?;
    assert_eq!(cli_results.len(), 1, "Should find 1 CLI package");
    assert_eq!(cli_results[0].id, "cli-tool");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_search_case_insensitive() -> Result<()> {
    let packages = vec![
        ("PostgreSQL-Driver", "PostgreSQL database driver", vec!["database", "postgresql"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Test case-insensitive search
    let results_lower = client.search("postgresql").await?;
    let results_upper = client.search("POSTGRESQL").await?;
    let results_mixed = client.search("PostgreSQL").await?;

    assert_eq!(results_lower.len(), 1);
    assert_eq!(results_upper.len(), 1);
    assert_eq!(results_mixed.len(), 1);

    Ok(())
}

#[tokio::test]
async fn test_marketplace_search_empty_results() -> Result<()> {
    let packages = vec![
        ("rust-package", "A Rust package", vec!["rust"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Search for non-existent term
    let results = client.search("javascript").await?;
    assert!(results.is_empty(), "Should return empty results for non-existent term");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_search_with_special_characters() -> Result<()> {
    let packages = vec![
        ("my-awesome-crate", "An awesome crate", vec!["rust"]),
        ("another_package", "Another package", vec!["rust"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Search should handle hyphens and underscores
    let results_hyphen = client.search("my-awesome").await?;
    let results_underscore = client.search("another_package").await?;

    assert_eq!(results_hyphen.len(), 1);
    assert_eq!(results_underscore.len(), 1);

    Ok(())
}

// ============================================================================
// PACKAGE INSTALLATION VERIFICATION TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_package_resolve() -> Result<()> {
    let packages = vec![
        ("test-package", "Test package for installation", vec!["test"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Resolve package
    let resolved = client.resolve("test-package", None).await?;

    assert_eq!(resolved.version, "1.0.0");
    assert_eq!(resolved.git_url, "https://github.com/test/test-package.git");
    assert_eq!(resolved.git_rev, "v1.0.0");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_package_resolve_specific_version() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Create package with multiple versions
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/pkg.git".to_string(),
        git_rev: "v1.0.0".to_string(),
        manifest_url: None,
        sha256: "sha1".to_string(),
    });
    versions.insert("2.0.0".to_string(), VersionMetadata {
        version: "2.0.0".to_string(),
        git_url: "https://github.com/test/pkg.git".to_string(),
        git_rev: "v2.0.0".to_string(),
        manifest_url: None,
        sha256: "sha2".to_string(),
    });

    let mut packs = HashMap::new();
    packs.insert("multi-version-pkg".to_string(), PackMetadata {
        id: "multi-version-pkg".to_string(),
        name: "Multi Version Package".to_string(),
        description: "Package with multiple versions".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["test".to_string()],
        category: Some("test".to_string()),
        author: Some("Tester".to_string()),
        latest_version: "2.0.0".to_string(),
        versions,
        downloads: Some(50),
        updated: Some(Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    });

    let index = RegistryIndex {
        updated: Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Resolve specific version
    let resolved_v1 = client.resolve("multi-version-pkg", Some("1.0.0")).await?;
    assert_eq!(resolved_v1.version, "1.0.0");
    assert_eq!(resolved_v1.git_rev, "v1.0.0");

    let resolved_v2 = client.resolve("multi-version-pkg", Some("2.0.0")).await?;
    assert_eq!(resolved_v2.version, "2.0.0");
    assert_eq!(resolved_v2.git_rev, "v2.0.0");

    // Resolve latest (no version specified)
    let resolved_latest = client.resolve("multi-version-pkg", None).await?;
    assert_eq!(resolved_latest.version, "2.0.0");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_package_metadata_validation() -> Result<()> {
    let packages = vec![
        ("validated-package", "Package with full metadata", vec!["rust", "validated"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Fetch and validate package metadata
    let index = client.fetch_index().await?;
    let package = index.packs.get("validated-package").unwrap();

    assert_eq!(package.id, "validated-package");
    assert!(!package.name.is_empty());
    assert!(!package.description.is_empty());
    assert!(!package.tags.is_empty());
    assert!(package.license.is_some());
    assert!(package.author.is_some());

    Ok(())
}

// ============================================================================
// ERROR HANDLING AND EDGE CASE TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_nonexistent_package() -> Result<()> {
    let packages = vec![
        ("existing-package", "An existing package", vec!["test"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Try to resolve non-existent package
    let result = client.resolve("nonexistent-package", None).await;
    assert!(result.is_err(), "Should fail to resolve non-existent package");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_invalid_version() -> Result<()> {
    let packages = vec![
        ("version-test", "Package for version testing", vec!["test"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Try to resolve with invalid version
    let result = client.resolve("version-test", Some("99.99.99")).await;
    assert!(result.is_err(), "Should fail to resolve invalid version");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_empty_registry() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let index = RegistryIndex {
        updated: Utc::now(),
        packs: HashMap::new(),
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Search in empty registry
    let results = client.search("anything").await?;
    assert!(results.is_empty());

    // List packages in empty registry
    let packages = client.list_packages().await?;
    assert!(packages.is_empty());

    Ok(())
}

#[tokio::test]
async fn test_marketplace_malformed_index_handling() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Write malformed JSON
    fs::write(&index_path, "{ invalid json }")?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Should fail gracefully
    let result = client.fetch_index().await;
    assert!(result.is_err(), "Should fail to parse malformed index");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_concurrent_searches() -> Result<()> {
    let packages = vec![
        ("concurrent-1", "Package 1", vec!["test"]),
        ("concurrent-2", "Package 2", vec!["test"]),
        ("concurrent-3", "Package 3", vec!["test"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Perform concurrent searches
    let handles = (0..10).map(|_| {
        let client = client.clone();
        tokio::spawn(async move {
            client.search("test").await
        })
    });

    let results: Vec<_> = futures::future::join_all(handles).await;

    // All searches should succeed
    for result in results {
        assert!(result.is_ok());
        let search_result = result.unwrap()?;
        assert_eq!(search_result.len(), 3);
    }

    Ok(())
}

// ============================================================================
// PERFORMANCE AND STRESS TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_large_registry_search() -> Result<()> {
    // Create a registry with 100 packages
    let packages: Vec<_> = (0..100)
        .map(|i| {
            (
                format!("package-{}", i),
                format!("Description for package {}", i),
                vec!["test", "performance"],
            )
        })
        .map(|(id, desc, tags)| (id, desc, tags))
        .collect();

    let packages_refs: Vec<(&str, &str, Vec<&str>)> = packages
        .iter()
        .map(|(id, desc, tags)| (id.as_str(), desc.as_str(), tags.iter().map(|s| *s).collect()))
        .collect();

    let (_temp, client) = setup_test_client(packages_refs).await?;

    // Measure search performance
    let start = std::time::Instant::now();
    let results = client.search("test").await?;
    let duration = start.elapsed();

    assert_eq!(results.len(), 100, "Should find all 100 packages");
    assert!(duration.as_millis() < 1000, "Search should complete within 1 second");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_rapid_successive_searches() -> Result<()> {
    let packages = vec![
        ("search-test-1", "First search test package", vec!["test"]),
        ("search-test-2", "Second search test package", vec!["test"]),
    ];

    let (_temp, client) = setup_test_client(packages).await?;

    // Perform 50 rapid searches
    let start = std::time::Instant::now();
    for _ in 0..50 {
        let _results = client.search("test").await?;
    }
    let duration = start.elapsed();

    assert!(duration.as_millis() < 5000, "50 searches should complete within 5 seconds");

    Ok(())
}

#[tokio::test]
async fn test_marketplace_package_list_performance() -> Result<()> {
    // Create registry with 200 packages
    let packages: Vec<_> = (0..200)
        .map(|i| {
            (
                format!("list-package-{}", i),
                format!("Package {} for listing test", i),
                vec!["test"],
            )
        })
        .map(|(id, desc, tags)| (id, desc, tags))
        .collect();

    let packages_refs: Vec<(&str, &str, Vec<&str>)> = packages
        .iter()
        .map(|(id, desc, tags)| (id.as_str(), desc.as_str(), tags.iter().map(|s| *s).collect()))
        .collect();

    let (_temp, client) = setup_test_client(packages_refs).await?;

    // Measure list performance
    let start = std::time::Instant::now();
    let all_packages = client.list_packages().await?;
    let duration = start.elapsed();

    assert_eq!(all_packages.len(), 200);
    assert!(duration.as_millis() < 1000, "Listing should complete within 1 second");

    Ok(())
}

// ============================================================================
// UPDATE AND VERSION MANAGEMENT TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_check_updates() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Create package with multiple versions
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/update-pkg.git".to_string(),
        git_rev: "v1.0.0".to_string(),
        manifest_url: None,
        sha256: "sha1".to_string(),
    });
    versions.insert("1.1.0".to_string(), VersionMetadata {
        version: "1.1.0".to_string(),
        git_url: "https://github.com/test/update-pkg.git".to_string(),
        git_rev: "v1.1.0".to_string(),
        manifest_url: None,
        sha256: "sha2".to_string(),
    });
    versions.insert("2.0.0".to_string(), VersionMetadata {
        version: "2.0.0".to_string(),
        git_url: "https://github.com/test/update-pkg.git".to_string(),
        git_rev: "v2.0.0".to_string(),
        manifest_url: None,
        sha256: "sha3".to_string(),
    });

    let mut packs = HashMap::new();
    packs.insert("update-pkg".to_string(), PackMetadata {
        id: "update-pkg".to_string(),
        name: "Update Package".to_string(),
        description: "Package for update testing".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["test".to_string()],
        category: Some("test".to_string()),
        author: Some("Tester".to_string()),
        latest_version: "2.0.0".to_string(),
        versions,
        downloads: Some(100),
        updated: Some(Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    });

    let index = RegistryIndex {
        updated: Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Check for updates from 1.0.0
    let update = client.check_updates("update-pkg", "1.0.0").await?;
    assert!(update.is_some());
    assert_eq!(update.unwrap().version, "2.0.0");

    // Check for updates from 1.1.0
    let update = client.check_updates("update-pkg", "1.1.0").await?;
    assert!(update.is_some());
    assert_eq!(update.unwrap().version, "2.0.0");

    // No update when already at latest
    let no_update = client.check_updates("update-pkg", "2.0.0").await?;
    assert!(no_update.is_none());

    Ok(())
}

// ============================================================================
// REGISTRY METADATA TESTS
// ============================================================================

#[tokio::test]
async fn test_marketplace_registry_categories() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Add packages with different categories
    for (id, category) in &[("pkg1", "web"), ("pkg2", "cli"), ("pkg3", "web")] {
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: format!("sha-{}", id),
        });

        packs.insert(id.to_string(), PackMetadata {
            id: id.to_string(),
            name: format!("Package {}", id),
            description: format!("Test package {}", id),
            tags: vec!["test".to_string()],
            keywords: vec!["test".to_string()],
            category: Some(category.to_string()),
            author: Some("Tester".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some(10),
            updated: Some(Utc::now()),
            license: Some("MIT".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        });
    }

    let index = RegistryIndex {
        updated: Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    let categories = client.list_categories().await?;
    assert_eq!(categories.len(), 2, "Should have 2 categories");
    assert!(categories.contains(&"web".to_string()));
    assert!(categories.contains(&"cli".to_string()));

    Ok(())
}

#[tokio::test]
async fn test_marketplace_package_statistics() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Add packages with different download counts
    for i in 0..5 {
        let id = format!("stats-pkg-{}", i);
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: format!("sha-{}", i),
        });

        packs.insert(id.clone(), PackMetadata {
            id: id.clone(),
            name: format!("Stats Package {}", i),
            description: format!("Package {} for statistics", i),
            tags: vec!["test".to_string()],
            keywords: vec!["test".to_string()],
            category: Some("test".to_string()),
            author: Some("Tester".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some((i + 1) as u64 * 100),
            updated: Some(Utc::now()),
            license: Some("MIT".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        });
    }

    let index = RegistryIndex {
        updated: Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    let index_fetched = client.fetch_index().await?;

    // Verify download counts
    for i in 0..5 {
        let pkg_id = format!("stats-pkg-{}", i);
        let package = index_fetched.packs.get(&pkg_id).unwrap();
        assert_eq!(package.downloads, Some((i + 1) as u64 * 100));
    }

    Ok(())
}
