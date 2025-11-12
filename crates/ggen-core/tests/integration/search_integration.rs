//! Integration tests for search functionality

use chicago_tdd_tools::prelude::*;
use ggen_core::registry::{PackMetadata, RegistryClient, SearchParams, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

async_test_with_timeout!(test_basic_search_integration, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    let packages = vec![
        (
            "rust-cli-tool",
            "Rust CLI Tool",
            "cli",
            vec!["rust", "cli", "tool"],
        ),
        (
            "python-web-framework",
            "Python Web Framework",
            "web",
            vec!["python", "web", "framework"],
        ),
        (
            "rust-web-server",
            "Rust Web Server",
            "web",
            vec!["rust", "web", "server"],
        ),
    ];

    for (id, name, category, tags) in packages {
        let mut versions = HashMap::new();
        versions.insert(
            "1.0.0".to_string(),
            VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: format!("https://github.com/test/{}.git", id),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            },
        );

        packs.insert(
            id.to_string(),
            PackMetadata {
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
            },
        );
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index).unwrap()).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act & Assert
    let rust_results = client.search("rust").await.unwrap();
    assert_eq!(rust_results.len(), 2);

    let python_results = client.search("python").await.unwrap();
    assert_eq!(python_results.len(), 1);

    let web_results = client.search("web").await.unwrap();
    assert_eq!(web_results.len(), 2);

    let cli_results = client.search("CLI").await.unwrap();
    assert!(cli_results.len() >= 1);
});

async_test_with_timeout!(test_advanced_search_with_filters, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    for i in 0..10 {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();
        versions.insert(
            "1.0.0".to_string(),
            VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: format!("https://github.com/test/{}.git", id),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            },
        );

        packs.insert(
            id.clone(),
            PackMetadata {
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
            },
        );
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index).unwrap()).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act & Assert
    let params = SearchParams {
        query: "package",
        category: Some("tools"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await.unwrap();
    assert_eq!(results.len(), 5);

    let params = SearchParams {
        query: "package",
        category: None,
        keyword: None,
        author: Some("Author A"),
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await.unwrap();
    assert_eq!(results.len(), 5);

    let params = SearchParams {
        query: "package",
        category: None,
        keyword: Some("keyword1"),
        author: None,
        stable_only: false,
        limit: 100,
    };

    let results = client.advanced_search(&params).await.unwrap();
    assert!(results.len() >= 3);

    let params = SearchParams {
        query: "package",
        category: None,
        keyword: None,
        author: None,
        stable_only: false,
        limit: 3,
    };

    let results = client.advanced_search(&params).await.unwrap();
    assert_eq!(results.len(), 3);
});

async_test_with_timeout!(test_search_relevance_ranking, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    let packages = vec![
        ("rust", "Rust", 1000u64),
        ("rust-tools", "Rust Tools", 500),
        ("tools-rust", "Tools Rust", 200),
        ("rust-cli", "Rust CLI", 100),
    ];

    for (id, name, downloads) in packages {
        let mut versions = HashMap::new();
        versions.insert(
            "1.0.0".to_string(),
            VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: format!("https://github.com/test/{}.git", id),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            },
        );

        packs.insert(
            id.to_string(),
            PackMetadata {
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
            },
        );
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index).unwrap()).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act
    let results = client.search("rust").await.unwrap();

    // Assert
    assert!(results.len() >= 4);
    assert_eq!(results[0].id, "rust");
});

async_test_with_timeout!(test_empty_search_results, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs: HashMap::new(),
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index).unwrap()).unwrap();

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();
    let client = RegistryClient::with_base_url(base_url).unwrap();

    // Act & Assert
    let results = client.search("anything").await.unwrap();
    assert!(results.is_empty());

    let params = SearchParams {
        query: "test",
        category: Some("nonexistent"),
        keyword: None,
        author: None,
        stable_only: false,
        limit: 10,
    };

    let advanced_results = client.advanced_search(&params).await.unwrap();
    assert!(advanced_results.is_empty());
});
