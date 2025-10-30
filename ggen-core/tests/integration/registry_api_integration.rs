//! Registry API integration tests

use anyhow::Result;
use ggen_core::registry::{RegistryClient, PackMetadata, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

#[tokio::test]
async fn test_get_popular_categories() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Create packages in different categories
    let categories = vec!["tools", "libraries", "frameworks", "tools", "libraries", "tools"];

    for (i, category) in categories.iter().enumerate() {
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

    let categories = client.get_popular_categories().await?;

    // Should have 3 categories: tools (3), libraries (2), frameworks (1)
    assert_eq!(categories.len(), 3);

    // Most popular should be first
    assert_eq!(categories[0].0, "tools");
    assert_eq!(categories[0].1, 3);

    assert_eq!(categories[1].0, "libraries");
    assert_eq!(categories[1].1, 2);

    assert_eq!(categories[2].0, "frameworks");
    assert_eq!(categories[2].1, 1);

    Ok(())
}

#[tokio::test]
async fn test_get_popular_keywords() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    // Create packages with various keywords
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

    let keywords = client.get_popular_keywords().await?;

    // "rust" appears 4 times, "cli" 2 times, "web" 2 times, "tool" 1 time, "python" 1 time
    assert!(keywords.len() >= 3);

    // Most popular should be first
    assert_eq!(keywords[0].0, "rust");
    assert_eq!(keywords[0].1, 4);

    Ok(())
}

#[tokio::test]
async fn test_list_all_packages() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    for i in 0..20 {
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

    let packages = client.list_packages().await?;

    assert_eq!(packages.len(), 20);

    // Verify all packages are present
    for i in 0..20 {
        let id = format!("package-{}", i);
        assert!(packages.iter().any(|p| p.id == id));
    }

    Ok(())
}
