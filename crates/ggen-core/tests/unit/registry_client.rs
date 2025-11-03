//! Unit tests for RegistryClient

use anyhow::Result;
use ggen_core::registry::{RegistryClient, RegistryIndex, PackMetadata, VersionMetadata};
use std::collections::HashMap;
use tempfile::TempDir;
use url::Url;

#[test]
fn test_registry_client_creation() -> Result<()> {
    // Test default client creation
    let client = RegistryClient::new()?;
    assert!(format!("{:?}", client).contains("RegistryClient"));
    Ok(())
}

#[test]
fn test_registry_client_custom_url() -> Result<()> {
    let custom_url = Url::parse("https://custom-registry.example.com/")?;
    let client = RegistryClient::with_base_url(custom_url.clone())?;
    assert!(format!("{:?}", client).contains("RegistryClient"));
    Ok(())
}

#[test]
fn test_registry_client_invalid_url() {
    let result = Url::parse("not-a-valid-url");
    assert!(result.is_err());
}

#[test]
fn test_registry_client_file_url() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let file_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;

    let client = RegistryClient::with_base_url(file_url)?;
    assert!(format!("{:?}", client).contains("RegistryClient"));
    Ok(())
}

#[test]
fn test_registry_index_serialization() -> Result<()> {
    let mut packs = HashMap::new();
    let mut versions = HashMap::new();

    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "main".to_string(),
        manifest_url: None,
        sha256: "abc123".to_string(),
    });

    packs.insert("test-pack".to_string(), PackMetadata {
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
    });

    let index = RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    // Test serialization
    let json = serde_json::to_string(&index)?;
    assert!(json.contains("test-pack"));
    assert!(json.contains("Test Package"));

    // Test deserialization
    let parsed: RegistryIndex = serde_json::from_str(&json)?;
    assert_eq!(parsed.packs.len(), 1);
    assert!(parsed.packs.contains_key("test-pack"));

    Ok(())
}

#[test]
fn test_pack_metadata_validation() {
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "main".to_string(),
        manifest_url: None,
        sha256: "abc123".to_string(),
    });

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

    // Validate required fields
    assert!(!pack.id.is_empty());
    assert!(!pack.name.is_empty());
    assert!(!pack.description.is_empty());
    assert!(!pack.latest_version.is_empty());
    assert!(pack.versions.contains_key(&pack.latest_version));
    assert!(!pack.tags.is_empty());
    assert!(!pack.keywords.is_empty());
}

#[test]
fn test_version_metadata_validation() {
    let version = VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "abc123def456".to_string(),
        manifest_url: Some("https://registry.example.com/manifest.json".to_string()),
        sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string(),
    };

    // Validate fields
    assert!(!version.version.is_empty());
    assert!(version.git_url.starts_with("https://"));
    assert!(!version.git_rev.is_empty());
    assert_eq!(version.sha256.len(), 64); // SHA256 hex string length
}

#[test]
fn test_pack_metadata_optional_fields() {
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "main".to_string(),
        manifest_url: None,
        sha256: "abc123".to_string(),
    });

    // Test with minimal fields (no optional fields)
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

    assert!(pack.category.is_none());
    assert!(pack.author.is_none());
    assert!(pack.downloads.is_none());
    assert!(pack.updated.is_none());
}

#[test]
fn test_empty_collections() {
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), VersionMetadata {
        version: "1.0.0".to_string(),
        git_url: "https://github.com/test/repo.git".to_string(),
        git_rev: "main".to_string(),
        manifest_url: None,
        sha256: "abc123".to_string(),
    });

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

    assert!(pack.tags.is_empty());
    assert!(pack.keywords.is_empty());
}

#[test]
fn test_boundary_values() {
    // Test with maximum reasonable values
    let mut versions = HashMap::new();
    for i in 0..100 {
        versions.insert(format!("{}.0.0", i), VersionMetadata {
            version: format!("{}.0.0", i),
            git_url: "https://github.com/test/repo.git".to_string(),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        });
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

    assert_eq!(pack.versions.len(), 100);
    assert_eq!(pack.tags.len(), 50);
    assert_eq!(pack.keywords.len(), 50);
    assert_eq!(pack.downloads, Some(u64::MAX));
}
