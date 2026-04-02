//! Unit tests for RegistryClient

use chicago_tdd_tools::prelude::*;
use ggen_core::registry::{PackMetadata, RegistryClient, RegistryIndex, VersionMetadata};
use std::collections::HashMap;
use tempfile::TempDir;
use url::Url;

test!(test_registry_client_creation, {
    // Arrange & Act
    let client = RegistryClient::new().unwrap();

    // Assert
    assert!(format!("{:?}", client).contains("RegistryClient"));
});

test!(test_registry_client_custom_url, {
    // Arrange
    let custom_url = Url::parse("https://custom-registry.example.com/").unwrap();

    // Act
    let client = RegistryClient::with_base_url(custom_url.clone()).unwrap();

    // Assert
    assert!(format!("{:?}", client).contains("RegistryClient"));
});

test!(test_registry_client_invalid_url, {
    // Arrange & Act
    let result = Url::parse("not-a-valid-url");

    // Assert
    assert_err!(&result);
});

test!(test_registry_client_file_url, {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let file_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))
        .unwrap();

    // Act
    let client = RegistryClient::with_base_url(file_url).unwrap();

    // Assert
    assert!(format!("{:?}", client).contains("RegistryClient"));
});

test!(test_registry_index_serialization, {
    // Arrange
    let mut packs = HashMap::new();
    let mut versions = HashMap::new();

    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/repo.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        },
    );

    packs.insert(
        "test-pack".to_string(),
        PackMetadata {
            id: "test-pack".to_string(),
            name: "Test Package".to_string(),
            description: "A test package".to_string(),
            tags: vec!["test".to_string()],
            keywords: vec!["testing".to_string()],
            category: Some("development".to_string()),
            author: Some("Test Author".to_string()),
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: Some(100),
            updated: Some(chrono::Utc::now()),
            license: Some("MIT".to_string()),
            homepage: Some("https://example.com".to_string()),
            repository: Some("https://github.com/test/repo".to_string()),
            documentation: Some("https://docs.example.com".to_string()),
        },
    );

    let index = RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    // Act
    let json = serde_json::to_string(&index).unwrap();
    let parsed: RegistryIndex = serde_json::from_str(&json).unwrap();

    // Assert
    assert!(json.contains("test-pack"));
    assert!(json.contains("Test Package"));
    assert_eq!(parsed.packs.len(), 1);
    assert!(parsed.packs.contains_key("test-pack"));
});

test!(test_pack_metadata_validation, {
    // Arrange
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/repo.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        },
    );

    let pack = PackMetadata {
        id: "test-pack".to_string(),
        name: "Test Package".to_string(),
        description: "A test package".to_string(),
        tags: vec!["test".to_string()],
        keywords: vec!["testing".to_string()],
        category: Some("development".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "1.0.0".to_string(),
        versions: versions.clone(),
        downloads: Some(100),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: Some("https://example.com".to_string()),
        repository: Some("https://github.com/test/repo".to_string()),
        documentation: Some("https://docs.example.com".to_string()),
    };

    // Assert
    assert!(!pack.id.is_empty());
    assert!(!pack.name.is_empty());
    assert!(!pack.description.is_empty());
    assert!(!pack.latest_version.is_empty());
    assert!(pack.versions.contains_key(&pack.latest_version));
    assert!(!pack.tags.is_empty());
    assert!(!pack.keywords.is_empty());
});

test!(test_version_metadata_validation, {
    // Arrange
    let version = VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "abc123def456".to_string(),
        manifest_url: Some("https://registry.example.com/manifest.json".to_string()),
        sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string(),
    };

    // Assert
    assert!(!version.version.is_empty());
    assert!(version.git_url.starts_with("https://"));
    assert!(!version.git_rev.is_empty());
    assert_eq!(version.sha256.len(), 64);
});

test!(test_pack_metadata_optional_fields, {
    // Arrange
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/repo.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        },
    );

    let pack = PackMetadata {
        id: "minimal-pack".to_string(),
        name: "Minimal Package".to_string(),
        description: "Minimal package with no optional fields".to_string(),
        tags: vec![],
        keywords: vec![],
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
    };

    // Assert
    assert!(pack.category.is_none());
    assert!(pack.author.is_none());
    assert!(pack.downloads.is_none());
    assert!(pack.updated.is_none());
});

test!(test_empty_collections, {
    // Arrange
    let mut versions = HashMap::new();
    versions.insert(
        "1.0.0".to_string(),
        VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: "https://github.com/test/repo.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        },
    );

    let pack = PackMetadata {
        id: "empty-collections".to_string(),
        name: "Empty Collections".to_string(),
        description: "Package with empty collections".to_string(),
        tags: vec![],
        keywords: vec![],
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
    };

    // Assert
    assert!(pack.tags.is_empty());
    assert!(pack.keywords.is_empty());
});

test!(test_boundary_values, {
    // Arrange
    let mut versions = HashMap::new();
    for i in 0..100 {
        versions.insert(
            format!("{}.0.0", i),
            VersionMetadata {
                version: format!("{}.0.0", i),
                git_url: "https://github.com/test/repo.git".to_string(),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            },
        );
    }

    let pack = PackMetadata {
        id: "max-versions".to_string(),
        name: "Maximum Versions".to_string(),
        description: "Package with many versions".to_string(),
        tags: (0..50).map(|i| format!("tag{}", i)).collect(),
        keywords: (0..50).map(|i| format!("keyword{}", i)).collect(),
        category: Some("test".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: "99.0.0".to_string(),
        versions,
        downloads: Some(u64::MAX),
        updated: Some(chrono::Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    };

    // Assert
    assert_eq!(pack.versions.len(), 100);
    assert_eq!(pack.tags.len(), 50);
    assert_eq!(pack.keywords.len(), 50);
    assert_eq!(pack.downloads, Some(u64::MAX));
});
