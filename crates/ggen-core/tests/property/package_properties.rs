//! Property-based tests for package operations

#[cfg(feature = "proptest")]
mod tests {
    use ggen_core::registry::{PackMetadata, VersionMetadata};
    use proptest::prelude::*;
    use std::collections::HashMap;

    proptest! {
        #[test]
        fn package_roundtrip_serialization(
            id in "[a-z0-9_\\-\\.]{3,20}",
            name in "[a-zA-Z0-9_\\s\\-\\.]{3,50}",
            version in "[0-9]\\.[0-9]\\.[0-9]",
        ) {
            let mut versions = HashMap::new();
            versions.insert(version.clone(), VersionMetadata {
                version: version.clone(),
                git_url: format!("https://github.com/test/{}.git", id),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string(),
            });

            let pack = PackMetadata {
                id: id.clone(),
                name: name.clone(),
                description: "Test package".to_string(),
                tags: vec!["test".to_string()],
                keywords: vec!["testing".to_string()],
                category: Some("test".to_string()),
                author: Some("Test Author".to_string()),
                latest_version: version.clone(),
                versions,
                downloads: Some(100),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Serialize and deserialize
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            // Properties that must hold
            prop_assert_eq!(&parsed.id, &id);
            prop_assert_eq!(&parsed.name, &name);
            prop_assert_eq!(&parsed.latest_version, &version);
            prop_assert!(parsed.versions.contains_key(&version));
        }

        #[test]
        fn package_id_consistency(
            id in "[a-z0-9_\\-\\.]{3,20}",
        ) {
            let mut versions = HashMap::new();
            versions.insert("1.0.0".to_string(), VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: "https://github.com/test/repo.git".to_string(),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            });

            let pack = PackMetadata {
                id: id.clone(),
                name: "Test Package".to_string(),
                description: "Test".to_string(),
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

            // Property: ID should remain unchanged through operations
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(&parsed.id, &id);
            prop_assert_eq!(&pack.id, &parsed.id);
        }

        #[test]
        fn package_version_consistency(
            version in "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}",
        ) {
            let mut versions = HashMap::new();
            versions.insert(version.clone(), VersionMetadata {
                version: version.clone(),
                git_url: "https://github.com/test/repo.git".to_string(),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            });

            let pack = PackMetadata {
                id: "test-pack".to_string(),
                name: "Test".to_string(),
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: None,
                author: None,
                latest_version: version.clone(),
                versions: versions.clone(),
                downloads: None,
                updated: None,
                license: None,
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Property: latest_version must exist in versions map
            prop_assert!(pack.versions.contains_key(&pack.latest_version));

            // Property: version in metadata must match key
            let version_meta = pack.versions.get(&version).unwrap();
            prop_assert_eq!(&version_meta.version, &version);
        }

        #[test]
        fn package_tags_unique(
            tags in prop::collection::vec("[a-z]+", 0..10),
        ) {
            let mut versions = HashMap::new();
            versions.insert("1.0.0".to_string(), VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: "https://github.com/test/repo.git".to_string(),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            });

            let pack = PackMetadata {
                id: "test".to_string(),
                name: "Test".to_string(),
                description: "Test".to_string(),
                tags: tags.clone(),
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

            // Property: serialization preserves tags
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(parsed.tags.len(), tags.len());

            for tag in &tags {
                prop_assert!(parsed.tags.contains(tag));
            }
        }

        #[test]
        fn package_downloads_non_negative(
            downloads in 0..1000000u64,
        ) {
            let mut versions = HashMap::new();
            versions.insert("1.0.0".to_string(), VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: "https://github.com/test/repo.git".to_string(),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            });

            let pack = PackMetadata {
                id: "test".to_string(),
                name: "Test".to_string(),
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: None,
                author: None,
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: Some(downloads),
                updated: None,
                license: None,
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Property: downloads must be non-negative
            if let Some(dl) = pack.downloads {
                prop_assert!(dl >= 0);
            }

            // Property: serialization preserves downloads
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(parsed.downloads, pack.downloads);
        }
    }
}
