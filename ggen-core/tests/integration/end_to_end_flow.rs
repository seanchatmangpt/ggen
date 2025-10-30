//! End-to-end integration tests for package lifecycle

use anyhow::Result;
use ggen_core::registry::{RegistryClient, PackMetadata, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

#[tokio::test]
async fn test_complete_package_lifecycle() -> Result<()> {
    // Setup: Create a temporary registry
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Step 1: Create initial registry with one package
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/package.git".to_string(),
        git_rev: "v1.0.0".to_string(),
        manifest_url: None,
        sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string(),
    });

    let mut packs = HashMap::new();
    packs.insert("test-package".to_string(), PackMetadata {
        id: "test-package".to_string(),
        name: "Test Package".to_string(),
        description: "A test package for E2E testing".to_string(),
        tags: vec!["test".to_string(), "e2e".to_string()],
        keywords: vec!["testing".to_string()],
        category: Some("development".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "1.0.0".to_string(),
        versions,
        downloads: Some(0),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: Some("https://example.com".to_string()),
        repository: Some("https://github.com/test/package".to_string()),
        documentation: Some("https://docs.example.com".to_string()),
    });

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    // Step 2: Create client and fetch index
    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    let fetched_index = client.fetch_index().await?;
    assert_eq!(fetched_index.packs.len(), 1);

    // Step 3: Search for the package
    let search_results = client.search("test").await?;
    assert_eq!(search_results.len(), 1);
    assert_eq!(search_results[0].id, "test-package");

    // Step 4: Resolve the package
    let resolved = client.resolve("test-package", None).await?;
    assert_eq!(resolved.version, "1.0.0");
    assert_eq!(resolved.git_url, "https://github.com/test/package.git");

    // Step 5: Resolve specific version
    let resolved_specific = client.resolve("test-package", Some("1.0.0")).await?;
    assert_eq!(resolved_specific.version, "1.0.0");

    Ok(())
}

#[tokio::test]
async fn test_package_update_flow() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Create package with two versions
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/package.git".to_string(),
        git_rev: "v1.0.0".to_string(),
        manifest_url: None,
        sha256: "abc123".to_string(),
    });
    versions.insert("2.0.0".to_string(), VersionMetadata {
        version: "2.0.0".to_string(),
        git_url: "https://github.com/test/package.git".to_string(),
        git_rev: "v2.0.0".to_string(),
        manifest_url: None,
        sha256: "def456".to_string(),
    });

    let mut packs = HashMap::new();
    packs.insert("update-test".to_string(), PackMetadata {
        id: "update-test".to_string(),
        name: "Update Test".to_string(),
        description: "Package for testing updates".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["update".to_string()],
        category: Some("test".to_string()),
        author: Some("Tester".to_string()),
        latest_version: "2.0.0".to_string(),
        versions,
        downloads: Some(10),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    });

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Check for updates from 1.0.0
    let update = client.check_updates("update-test", "1.0.0").await?;
    assert!(update.is_some());
    assert_eq!(update.unwrap().version, "2.0.0");

    // No update when already at latest
    let no_update = client.check_updates("update-test", "2.0.0").await?;
    assert!(no_update.is_none());

    Ok(())
}

#[tokio::test]
async fn test_multi_package_search() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Add multiple packages with different tags
    for i in 0..5 {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: format!("hash{}", i),
        });

        packs.insert(id.clone(), PackMetadata {
            id: id.clone(),
            name: format!("Package {}", i),
            description: if i % 2 == 0 {
                "A Rust package".to_string()
            } else {
                "A Python package".to_string()
            },
            tags: if i % 2 == 0 {
                vec!["rust".to_string(), "cli".to_string()]
            } else {
                vec!["python".to_string(), "web".to_string()]
            },
            keywords: vec!["test".to_string()],
            category: Some("development".to_string()),
            author: Some("Tester".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some((i as u64 + 1) * 10),
            updated: Some(chrono::Utc::now()),
            license: Some("MIT".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        });
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Search for Rust packages
    let rust_results = client.search("rust").await?;
    assert!(rust_results.len() >= 3); // Should find packages 0, 2, 4

    // Search for Python packages
    let python_results = client.search("python").await?;
    assert!(python_results.len() >= 2); // Should find packages 1, 3

    // List all packages
    let all_packages = client.list_packages().await?;
    assert_eq!(all_packages.len(), 5);

    Ok(())
}

#[tokio::test]
async fn test_error_handling_flow() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs: HashMap::new(),
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    // Try to resolve non-existent package
    let result = client.resolve("nonexistent", None).await;
    assert!(result.is_err());

    // Search should return empty results
    let results = client.search("nothing").await?;
    assert!(results.is_empty());

    Ok(())
}
