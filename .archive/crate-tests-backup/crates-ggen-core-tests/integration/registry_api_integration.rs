//! Registry API integration tests

use chicago_tdd_tools::prelude::*;
use ggen_core::registry::{PackMetadata, RegistryClient, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

async_test_with_timeout!(test_get_popular_categories, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    let categories = vec![
        "tools",
        "libraries",
        "frameworks",
        "tools",
        "libraries",
        "tools",
    ];

    for (i, category) in categories.iter().enumerate() {
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
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: Some(category.to_string()),
                author: Some("Test".to_string()),
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: None,
                updated: None,
                license: None,
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
    let categories = client.get_popular_categories().await.unwrap();

    // Assert
    assert_eq!(categories.len(), 3);
    assert_eq!(categories[0].0, "tools");
    assert_eq!(categories[0].1, 3);
    assert_eq!(categories[1].0, "libraries");
    assert_eq!(categories[1].1, 2);
    assert_eq!(categories[2].0, "frameworks");
    assert_eq!(categories[2].1, 1);
});

async_test_with_timeout!(test_get_popular_keywords, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    let keyword_sets = vec![
        vec!["rust", "cli"],
        vec!["rust", "web"],
        vec!["rust", "cli", "tool"],
        vec!["python", "web"],
        vec!["rust"],
    ];

    for (i, keywords) in keyword_sets.iter().enumerate() {
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
                description: "Test".to_string(),
                tags: vec![],
                keywords: keywords.iter().map(|k| k.to_string()).collect(),
                category: None,
                author: None,
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: None,
                updated: None,
                license: None,
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
    let keywords = client.get_popular_keywords().await.unwrap();

    // Assert
    assert!(keywords.len() >= 3);
    assert_eq!(keywords[0].0, "rust");
    assert_eq!(keywords[0].1, 4);
});

async_test_with_timeout!(test_list_all_packages, 30, async {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    for i in 0..20 {
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
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: Some("test".to_string()),
                author: Some("Test".to_string()),
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: Some(i as u64 * 10),
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
    let packages = client.list_packages().await.unwrap();

    // Assert
    assert_eq!(packages.len(), 20);
    for i in 0..20 {
        let id = format!("package-{}", i);
        assert!(packages.iter().any(|p| p.id == id));
    }
});
