//! Property-based tests for serialization and deserialization

#[cfg(feature = "proptest")]
mod tests {
    use ggen_core::registry::{PackMetadata, RegistryIndex, VersionMetadata};
    use proptest::prelude::*;
    use std::collections::HashMap;

    proptest! {
        #[test]
        fn registry_index_serialization_idempotent(
            pack_count in 0..10usize,
        ) {
            // Create registry with multiple packages
            let mut packs = HashMap::new();

            for i in 0..pack_count {
                let id = format!("pack-{}", i);
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
                    keywords: vec!["testing".to_string()],
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

            let index = RegistryIndex {
                updated: chrono::Utc::now(),
                packs,
            };

            // Property: Serialization should be idempotent
            let json1 = serde_json::to_string(&index).unwrap();
            let parsed1: RegistryIndex = serde_json::from_str(&json1).unwrap();

            let json2 = serde_json::to_string(&parsed1).unwrap();
            let parsed2: RegistryIndex = serde_json::from_str(&json2).unwrap();

            // Both parsed results should have the same structure
            prop_assert_eq!(parsed1.packs.len(), parsed2.packs.len());

            for (id, pack1) in &parsed1.packs {
                let pack2 = parsed2.packs.get(id).unwrap();
                prop_assert_eq!(&pack1.id, &pack2.id);
                prop_assert_eq!(&pack1.name, &pack2.name);
                prop_assert_eq!(&pack1.latest_version, &pack2.latest_version);
            }
        }

        #[test]
        fn json_preserves_special_characters(
            description in "[a-zA-Z0-9\\s!@#$%^&*()\\-_=+\\[\\]{}|;:',.<>?/]{10,100}",
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
                id: "test-pack".to_string(),
                name: "Test Package".to_string(),
                description: description.clone(),
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

            // Property: Special characters should be preserved
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(&parsed.description, &description);
        }

        #[test]
        fn unicode_preservation(
            name in "[\u{0041}-\u{007A}\u{4E00}-\u{9FFF}\u{0400}-\u{04FF}]{3,20}",
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
                id: "unicode-test".to_string(),
                name: name.clone(),
                description: "Unicode test".to_string(),
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

            // Property: Unicode should be preserved through serialization
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(&parsed.name, &name);
        }

        #[test]
        fn empty_collections_preserved(
            has_tags in proptest::bool::ANY,
            has_keywords in proptest::bool::ANY,
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
                id: "empty-test".to_string(),
                name: "Empty Collections Test".to_string(),
                description: "Test".to_string(),
                tags: if has_tags { vec!["tag".to_string()] } else { vec![] },
                keywords: if has_keywords { vec!["keyword".to_string()] } else { vec![] },
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

            // Property: Empty collections should remain empty
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(parsed.tags.is_empty(), !has_tags);
            prop_assert_eq!(parsed.keywords.is_empty(), !has_keywords);
        }

        #[test]
        fn optional_fields_preserved(
            has_category in proptest::bool::ANY,
            has_author in proptest::bool::ANY,
            has_downloads in proptest::bool::ANY,
            has_license in proptest::bool::ANY,
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
                id: "optional-test".to_string(),
                name: "Optional Fields Test".to_string(),
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: if has_category { Some("test".to_string()) } else { None },
                author: if has_author { Some("Test Author".to_string()) } else { None },
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: if has_downloads { Some(100) } else { None },
                updated: None,
                license: if has_license { Some("MIT".to_string()) } else { None },
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Property: Optional fields should preserve their None/Some state
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(parsed.category.is_some(), has_category);
            prop_assert_eq!(parsed.author.is_some(), has_author);
            prop_assert_eq!(parsed.downloads.is_some(), has_downloads);
            prop_assert_eq!(parsed.license.is_some(), has_license);
        }

        #[test]
        fn large_version_maps_preserved(
            version_count in 1..20usize,
        ) {
            let mut versions = HashMap::new();

            for i in 0..version_count {
                let version = format!("{}.0.0", i);
                versions.insert(version.clone(), VersionMetadata {
                    version: version.clone(),
                    git_url: "https://github.com/test/repo.git".to_string(),
                    git_rev: format!("v{}", version),
                    manifest_url: None,
                    sha256: format!("hash{}", i),
                });
            }

            let pack = PackMetadata {
                id: "multiver-test".to_string(),
                name: "Multiple Versions Test".to_string(),
                description: "Test".to_string(),
                tags: vec![],
                keywords: vec![],
                category: None,
                author: None,
                latest_version: format!("{}.0.0", version_count - 1),
                versions: versions.clone(),
                downloads: None,
                updated: None,
                license: None,
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Property: All versions should be preserved
            let json = serde_json::to_string(&pack).unwrap();
            let parsed: PackMetadata = serde_json::from_str(&json).unwrap();

            prop_assert_eq!(parsed.versions.len(), version_count);

            for i in 0..version_count {
                let version = format!("{}.0.0", i);
                prop_assert!(parsed.versions.contains_key(&version));
            }
        }
    }
}
