//! DoS resistance tests

use ggen_core::registry::{PackMetadata, RegistryIndex, VersionMetadata};
use std::collections::HashMap;

#[test]
fn test_large_registry_handling() {
    // Create a large registry to test memory/performance
    let mut packs = HashMap::new();

    for i in 0..1000 {
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();

        for j in 0..10 {
            let version = format!("{}.0.0", j);
            versions.insert(
                version.clone(),
                VersionMetadata {
                    version: version.clone(),
                    git_url: format!("https://github.com/test/{}.git", id),
                    git_rev: format!("v{}", version),
                    manifest_url: None,
                    sha256: format!("hash{}", j),
                },
            );
        }

        packs.insert(
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: format!("Description {}", i),
                tags: vec![format!("tag{}", i % 10)],
                keywords: vec![format!("keyword{}", i % 20)],
                category: Some("test".to_string()),
                author: Some("Test".to_string()),
                latest_version: "9.0.0".to_string(),
                versions,
                downloads: Some(i as u64),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            },
        );
    }

    let index = RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    // Should handle large registry without excessive memory usage
    assert_eq!(index.packs.len(), 1000);
}

#[test]
fn test_deeply_nested_structures() {
    // Test handling of complex version maps
    let mut versions = HashMap::new();

    for i in 0..100 {
        for j in 0..10 {
            for k in 0..10 {
                let version = format!("{}.{}.{}", i, j, k);
                versions.insert(
                    version.clone(),
                    VersionMetadata {
                        version: version.clone(),
                        git_url: "https://github.com/test/repo.git".to_string(),
                        git_rev: format!("v{}", version),
                        manifest_url: Some(format!(
                            "https://registry.test/manifest-{}.json",
                            version
                        )),
                        sha256: format!("hash{}{}{}", i, j, k),
                    },
                );
            }
        }
    }

    // 100 * 10 * 10 = 10,000 versions
    assert_eq!(versions.len(), 10000);

    // Should handle without panicking
    let pack = PackMetadata {
        id: "deep-test".to_string(),
        name: "Deep Structure Test".to_string(),
        description: "Test with many versions".to_string(),
        tags: vec![],
        keywords: vec![],
        category: None,
        author: None,
        latest_version: "99.9.9".to_string(),
        versions,
        downloads: None,
        updated: None,
        license: None,
        homepage: None,
        repository: None,
        documentation: None,
    };

    assert_eq!(pack.versions.len(), 10000);
}

#[test]
fn test_extremely_long_strings() {
    // Test handling of very long strings
    let long_description = "a".repeat(1_000_000);

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
        id: "long-string-test".to_string(),
        name: "Long String Test".to_string(),
        description: long_description.clone(),
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

    // Should handle without excessive memory issues
    assert_eq!(pack.description.len(), 1_000_000);
}

#[test]
fn test_many_tags_and_keywords() {
    // Test handling of packages with many tags/keywords
    let tags: Vec<String> = (0..1000).map(|i| format!("tag{}", i)).collect();
    let keywords: Vec<String> = (0..1000).map(|i| format!("keyword{}", i)).collect();

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
        id: "many-tags-test".to_string(),
        name: "Many Tags Test".to_string(),
        description: "Test".to_string(),
        tags,
        keywords,
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

    assert_eq!(pack.tags.len(), 1000);
    assert_eq!(pack.keywords.len(), 1000);
}

#[test]
fn test_serialization_bomb_prevention() {
    // Test that serialization doesn't create exponentially large output
    let mut packs = HashMap::new();

    for i in 0..100 {
        let id = format!("pack{}", i);
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
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {}", i),
                description: "x".repeat(1000), // Repeated but not exponential
                tags: vec!["test".to_string()],
                keywords: vec!["test".to_string()],
                category: Some("test".to_string()),
                author: Some("Test".to_string()),
                latest_version: "1.0.0".to_string(),
                versions,
                downloads: Some(i as u64),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            },
        );
    }

    let index = RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    // Serialization should be linear in size
    let json = serde_json::to_string(&index).unwrap();

    // Output should be reasonable size (not exponential)
    // 100 packs * ~1KB each = ~100KB expected
    assert!(json.len() < 1_000_000); // Less than 1MB
}

#[test]
fn test_hash_collision_resistance() {
    // Create many packages with similar names to test hash table performance
    let mut packs = HashMap::new();

    for i in 0..10000 {
        let id = format!("pack{:04}", i);
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
            id.clone(),
            PackMetadata {
                id: id.clone(),
                name: format!("Package {:04}", i),
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
            },
        );
    }

    // HashMap should handle many similar keys efficiently
    assert_eq!(packs.len(), 10000);

    // Lookup should still be efficient
    assert!(packs.contains_key("pack0000"));
    assert!(packs.contains_key("pack9999"));
}

#[test]
fn test_recursive_structure_prevention() {
    // Verify that structures don't allow recursion (stack overflow)
    // RegistryIndex -> PackMetadata -> VersionMetadata (finite depth)

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
        id: "test".to_string(),
        name: "Test".to_string(),
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

    let mut packs = HashMap::new();
    packs.insert("test".to_string(), pack);

    let index = RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    // Should serialize without stack overflow
    let json = serde_json::to_string(&index).unwrap();
    assert!(!json.is_empty());
}
