//! Multi-node scenario tests
//!
//! These tests simulate multiple registry nodes and client interactions

use anyhow::Result;
use ggen_core::registry::{PackMetadata, RegistryClient, VersionMetadata};
use std::collections::HashMap;
use std::fs;
use tempfile::TempDir;
use url::Url;

#[tokio::test]
async fn test_multiple_registry_instances() -> Result<()> {
    // Create two separate registry instances
    let registry1_dir = TempDir::new()?;
    let registry2_dir = TempDir::new()?;

    // Setup registry 1 with package A
    let index1_path = registry1_dir.path().join("index.json");
    let mut packs1 = HashMap::new();
    let mut versions1 = HashMap::new();
    versions1.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/package-a.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        },
    );

    packs1.insert(
        "package-a".to_string(),
        PackMetadata {
            id: "package-a".to_string(),
            name: "Package A".to_string(),
            description: "Package from registry 1".to_string(),
            tags: vec!["registry1".to_string()],
            keywords: vec!["test".to_string()],
            category: Some("tools".to_string()),
            author: Some("Registry 1".to_string()),
            latest_version: "1.0.0".to_string(),
            versions: versions1,
            downloads: Some(100),
            updated: Some(chrono::Utc::now()),
            license: Some("MIT".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        },
    );

    let index1 = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs: packs1,
    };

    fs::write(&index1_path, serde_json::to_string_pretty(&index1)?)?;

    // Setup registry 2 with package B
    let index2_path = registry2_dir.path().join("index.json");
    let mut packs2 = HashMap::new();
    let mut versions2 = HashMap::new();
    versions2.insert(
        "2.0.0".to_string(),
        VersionMetadata {
            version: "2.0.0".to_string(),
            git_url: "https://github.com/test/package-b.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "def456".to_string(),
        },
    );

    packs2.insert(
        "package-b".to_string(),
        PackMetadata {
            id: "package-b".to_string(),
            name: "Package B".to_string(),
            description: "Package from registry 2".to_string(),
            tags: vec!["registry2".to_string()],
            keywords: vec!["test".to_string()],
            category: Some("libraries".to_string()),
            author: Some("Registry 2".to_string()),
            latest_version: "2.0.0".to_string(),
            versions: versions2,
            downloads: Some(200),
            updated: Some(chrono::Utc::now()),
            license: Some("Apache-2.0".to_string()),
            homepage: None,
            repository: None,
            documentation: None,
        },
    );

    let index2 = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs: packs2,
    };

    fs::write(&index2_path, serde_json::to_string_pretty(&index2)?)?;

    // Create clients for both registries
    let base_url1 = Url::from_file_path(registry1_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client1 = RegistryClient::with_base_url(base_url1)?;

    let base_url2 = Url::from_file_path(registry2_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client2 = RegistryClient::with_base_url(base_url2)?;

    // Verify isolation - each registry only sees its own packages
    let packages1 = client1.list_packages().await?;
    assert_eq!(packages1.len(), 1);
    assert_eq!(packages1[0].id, "package-a");

    let packages2 = client2.list_packages().await?;
    assert_eq!(packages2.len(), 1);
    assert_eq!(packages2[0].id, "package-b");

    // Search in registry 1
    let results1 = client1.search("package").await?;
    assert_eq!(results1.len(), 1);
    assert_eq!(results1[0].id, "package-a");

    // Search in registry 2
    let results2 = client2.search("package").await?;
    assert_eq!(results2.len(), 1);
    assert_eq!(results2[0].id, "package-b");

    Ok(())
}

#[tokio::test]
async fn test_concurrent_registry_access() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    // Create a registry with multiple packages
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
                sha256: format!("hash{}", i),
            },
        );

        packs.insert(
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: format!("Test package {}", i),
                tags: vec!["test".to_string()],
                keywords: vec!["concurrent".to_string()],
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

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;

    // Create multiple clients and perform concurrent operations
    let mut handles = vec![];

    for i in 0..5 {
        let base_url_clone = base_url.clone();
        let handle = tokio::spawn(async move {
            let client = RegistryClient::with_base_url(base_url_clone).unwrap();

            // Each task performs different operations
            match i % 3 {
                0 => {
                    // Search operation
                    client.search("package").await.unwrap()
                }
                1 => {
                    // List operation
                    let packages = client.list_packages().await.unwrap();
                    packages
                        .into_iter()
                        .map(|p| ggen_core::registry::SearchResult {
                            id: p.id,
                            name: p.name,
                            description: p.description,
                            tags: p.tags,
                            keywords: p.keywords,
                            category: p.category,
                            author: p.author,
                            latest_version: p.latest_version,
                            downloads: p.downloads,
                            updated: p.updated,
                            license: p.license,
                            homepage: p.homepage,
                            repository: p.repository,
                            documentation: p.documentation,
                        })
                        .collect()
                }
                _ => {
                    // Fetch index operation
                    let index = client.fetch_index().await.unwrap();
                    index
                        .packs
                        .into_iter()
                        .map(|(_, p)| ggen_core::registry::SearchResult {
                            id: p.id,
                            name: p.name,
                            description: p.description,
                            tags: p.tags,
                            keywords: p.keywords,
                            category: p.category,
                            author: p.author,
                            latest_version: p.latest_version,
                            downloads: p.downloads,
                            updated: p.updated,
                            license: p.license,
                            homepage: p.homepage,
                            repository: p.repository,
                            documentation: p.documentation,
                        })
                        .collect()
                }
            }
        });

        handles.push(handle);
    }

    // Wait for all tasks to complete
    for handle in handles {
        let results = handle.await?;
        assert!(!results.is_empty());
    }

    Ok(())
}

#[tokio::test]
async fn test_registry_failover_simulation() -> Result<()> {
    // Primary registry
    let primary_dir = TempDir::new()?;
    let primary_index_path = primary_dir.path().join("index.json");

    // Fallback registry
    let fallback_dir = TempDir::new()?;
    let fallback_index_path = fallback_dir.path().join("index.json");

    // Setup both registries with same package but different versions
    let mut packs = HashMap::new();
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/package.git".to_string(),
            git_rev: "v1.0.0".to_string(),
            manifest_url: None,
            sha256: "primary".to_string(),
        },
    );

    packs.insert(
        "shared-package".to_string(),
        PackMetadata {
            id: "shared-package".to_string(),
            name: "Shared Package".to_string(),
            description: "Package available in multiple registries".to_string(),
            tags: vec!["shared".to_string()],
            keywords: vec!["test".to_string()],
            category: Some("test".to_string()),
            author: Some("Test".to_string()),
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

    let primary_index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs: packs.clone(),
    };

    fs::write(
        &primary_index_path,
        serde_json::to_string_pretty(&primary_index)?,
    )?;
    fs::write(
        &fallback_index_path,
        serde_json::to_string_pretty(&primary_index)?,
    )?;

    // Test primary registry
    let primary_url = Url::from_file_path(primary_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let primary_client = RegistryClient::with_base_url(primary_url)?;

    let primary_result = primary_client.resolve("shared-package", None).await?;
    assert_eq!(primary_result.version, "1.0.0");

    // Test fallback registry
    let fallback_url = Url::from_file_path(fallback_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let fallback_client = RegistryClient::with_base_url(fallback_url)?;

    let fallback_result = fallback_client.resolve("shared-package", None).await?;
    assert_eq!(fallback_result.version, "1.0.0");

    // Simulate primary failure by removing index
    fs::remove_file(&primary_index_path)?;

    // Primary should fail
    let primary_fail = primary_client.fetch_index().await;
    assert!(primary_fail.is_err());

    // Fallback should still work
    let fallback_index = fallback_client.fetch_index().await?;
    assert_eq!(fallback_index.packs.len(), 1);

    Ok(())
}
