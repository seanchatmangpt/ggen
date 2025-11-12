//! Mock implementations for testing

use chrono::Utc;
use ggen_core::registry::{PackMetadata, RegistryIndex, VersionMetadata};
use std::collections::HashMap;

/// Create a mock VersionMetadata for testing
pub fn create_mock_version(version: &str) -> VersionMetadata {
    VersionMetadata {
        version: version.to_string(),
        git_url: format!("https://github.com/test/{}.git", version),
        git_rev: "main".to_string(),
        manifest_url: Some(format!("https://registry.test/manifest-{}.json", version)),
        sha256: format!("{:0<64}", version.replace('.', "")), // Pad to 64 chars
    }
}

/// Create a mock PackMetadata for testing
pub fn create_mock_pack(id: &str, name: &str, latest_version: &str) -> PackMetadata {
    let mut versions = HashMap::new();
    versions.insert(
        latest_version.to_string(),
        create_mock_version(latest_version),
    );

    PackMetadata {
        id: id.to_string(),
        name: name.to_string(),
        description: format!("Description for {}", name),
        tags: vec!["test".to_string(), "mock".to_string()],
        keywords: vec!["testing".to_string(), "mocking".to_string()],
        category: Some("development".to_string()),
        author: Some("Test Author".to_string()),
        latest_version: latest_version.to_string(),
        versions,
        downloads: Some(42),
        updated: Some(Utc::now()),
        license: Some("MIT".to_string()),
        homepage: Some(format!("https://{}.example.com", id)),
        repository: Some(format!("https://github.com/test/{}", id)),
        documentation: Some(format!("https://docs.example.com/{}", id)),
    }
}

/// Create a mock RegistryIndex with multiple packs
pub fn create_mock_registry_index(pack_count: usize) -> RegistryIndex {
    let mut packs = HashMap::new();

    for i in 0..pack_count {
        let id = format!("mock-pack-{}", i);
        let name = format!("Mock Package {}", i);
        let version = format!("{}.0.0", i);

        packs.insert(id.clone(), create_mock_pack(&id, &name, &version));
    }

    RegistryIndex {
        updated: Utc::now(),
        packs,
    }
}

/// Create a minimal mock pack with only required fields
pub fn create_minimal_mock_pack(id: &str) -> PackMetadata {
    let mut versions = HashMap::new();
    versions.insert("1.0.0".to_string(), create_mock_version("1.0.0"));

    PackMetadata {
        id: id.to_string(),
        name: format!("Pack {}", id),
        description: "Minimal package".to_string(),
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
    }
}

/// Create a pack with multiple versions
pub fn create_multiver_mock_pack(id: &str, versions_list: &[&str]) -> PackMetadata {
    let mut versions = HashMap::new();

    for version in versions_list {
        versions.insert(version.to_string(), create_mock_version(version));
    }

    let latest_version = versions_list.last().unwrap().to_string();

    PackMetadata {
        id: id.to_string(),
        name: format!("MultiVer Pack {}", id),
        description: "Package with multiple versions".to_string(),
        tags: vec!["multiver".to_string()],
        keywords: vec!["versioning".to_string()],
        category: Some("tools".to_string()),
        author: Some("Test Author".to_string()),
        latest_version,
        versions,
        downloads: Some(100),
        updated: Some(Utc::now()),
        license: Some("MIT".to_string()),
        homepage: None,
        repository: None,
        documentation: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_mock_version() {
        let version = create_mock_version("1.2.3");
        assert_eq!(version.version, "1.2.3");
        assert!(version.git_url.contains("1.2.3"));
        assert_eq!(version.sha256.len(), 64);
    }

    #[test]
    fn test_create_mock_pack() {
        let pack = create_mock_pack("test-id", "Test Name", "1.0.0");
        assert_eq!(pack.id, "test-id");
        assert_eq!(pack.name, "Test Name");
        assert_eq!(pack.latest_version, "1.0.0");
        assert!(pack.versions.contains_key("1.0.0"));
    }

    #[test]
    fn test_create_mock_registry_index() {
        let index = create_mock_registry_index(5);
        assert_eq!(index.packs.len(), 5);

        for i in 0..5 {
            let id = format!("mock-pack-{}", i);
            assert!(index.packs.contains_key(&id));
        }
    }

    #[test]
    fn test_create_minimal_mock_pack() {
        let pack = create_minimal_mock_pack("minimal");
        assert_eq!(pack.id, "minimal");
        assert!(pack.tags.is_empty());
        assert!(pack.keywords.is_empty());
        assert!(pack.category.is_none());
    }

    #[test]
    fn test_create_multiver_mock_pack() {
        let versions = vec!["1.0.0", "1.1.0", "2.0.0"];
        let pack = create_multiver_mock_pack("multiver", &versions);

        assert_eq!(pack.versions.len(), 3);
        assert_eq!(pack.latest_version, "2.0.0");

        for version in versions {
            assert!(pack.versions.contains_key(version));
        }
    }
}
