//! Integration tests for search functionality

use anyhow::Result;
use ggen_core::registry::{RegistryClient, SearchParams, PackMetadata, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

#[tokio::test]
async fn test_basic_search_integration() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Create diverse packages for testing search
    let packages = vec![
        ("rust-cli-tool", "Rust CLI Tool", "cli", vec!["rust", "cli", "tool"]),
        ("python-web-framework", "Python Web Framework", "web", vec!["python", "web", "framework"]),
        ("rust-web-server", "Rust Web Server", "web", vec!["rust", "web", "server"]),
    ];

    for (id, name, category, tags) in packages {
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        });

        packs.insert(id.to_string(), PackMetadata {
            id: id.to_string(),
            name: name.to_string(),
            description: format!("Description for {}", name),
            tags: tags.iter().map(|s| s.to_string()).collect(),
            keywords: tags.iter().map(|s| s.to_string()).collect(),
            category: Some(category.to_string()),
            author: Some("Test Author".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some(100),
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

    // Test 1: Search by language
    let rust_results = client.search("rust").await?;
    assert_eq!(rust_results.len(), 2);

    let python_results = client.search("python").await?;
    assert_eq!(python_results.len(), 1);

    // Test 2: Search by functionality
    let web_results = client.search("web").await?;
    assert_eq!(web_results.len(), 2);

    // Test 3: Search by exact name
    let cli_results = client.search("CLI").await?; // Case insensitive
    assert!(cli_results.len() >= 1);

    Ok(())
}

#[tokio::test]
async fn test_advanced_search_with_filters() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Create packages with different attributes
    for i in 0..10 {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        });

        packs.insert(id.clone(), PackMetadata {
            id: id.clone(),
            name: format!("Package {}", i),
            description: "Test package".to_string(),
            tags: vec!["test".to_string()],
            keywords: vec![format!("keyword{}", i % 3)],
            category: Some(if i % 2 == 0 { "tools" } else { "libraries" }.to_string()),
            author: Some(if i < 5 { "Author A" } else { "Author B" }.to_string()),
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

    // Test 1: Filter by category
    let params = SearchParams {
        query: "package",
        category: Some("tools"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await?;
    assert_eq!(results.len(), 5); // packages 0, 2, 4, 6, 8

    // Test 2: Filter by author
    let params = SearchParams {
        query: "package",
        category: None,
        keyword: None,
        author: Some("Author A"),
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await?;
    assert_eq!(results.len(), 5); // packages 0-4

    // Test 3: Filter by keyword
    let params = SearchParams {
        query: "package",
        category: None,
        keyword: Some("keyword1"),
        author: None,
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await?;
    assert!(results.len() >= 3); // packages 1, 4, 7

    // Test 4: Limit results
    let params = SearchParams {
        query: "package",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 3,
    };

    let results = client.advanced_search(&params).await?;
    assert_eq!(results.len(), 3);

    Ok(())
}

#[tokio::test]
async fn test_search_relevance_ranking() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Create packages with different relevance scores
    let packages = vec![
        ("rust", "Rust", 1000u64), // Exact match, high downloads
        ("rust-tools", "Rust Tools", 500),
        ("tools-rust", "Tools Rust", 200),
        ("rust-cli", "Rust CLI", 100),
    ];

    for (id, name, downloads) in packages {
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        });

        packs.insert(id.to_string(), PackMetadata {
            id: id.to_string(),
            name: name.to_string(),
            description: format!("Description for {}", name),
            tags: vec!["rust".to_string()],
            keywords: vec!["rust".to_string()],
            category: Some("tools".to_string()),
            author: Some("Test".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some(downloads),
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

    // Search for "rust" - should prioritize exact match and high downloads
    let results = client.search("rust").await?;

    assert!(results.len() >= 4);

    // First result should be exact match
    assert_eq!(results[0].id, "rust");

    Ok(())
}

#[tokio::test]
async fn test_empty_search_results() -> Result<()> {
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

    let results = client.search("anything").await?;
    assert!(results.is_empty());

    let params = SearchParams {
        query: "test",
        category: Some("nonexistent"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    let advanced_results = client.advanced_search(&params).await?;
    assert!(advanced_results.is_empty());

    Ok(())
}
