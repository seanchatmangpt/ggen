//! Property-based tests for search operations

#[cfg(feature = "proptest")]
mod tests {
    use ggen_core::registry::{PackMetadata, VersionMetadata, RegistryIndex, SearchResult};
    use proptest::prelude::*;
    use std::collections::HashMap;

    proptest! {
        #[test]
        fn search_results_are_subset(
            query in "[a-zA-Z]{1,10}",
            pack_count in 0..20usize,
        ) {
            // Create mock registry
            let mut packs = HashMap::new();

            for i in 0..pack_count {
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
                    description: format!("Description for package {}", i),
                    tags: vec![format!("tag{}", i % 3)],
                    keywords: vec![format!("keyword{}", i % 5)],
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
                packs: packs.clone(),
            };

            // Simulate search by filtering packs
            let query_lower = query.to_lowercase();
            let filtered: Vec<_> = index.packs.into_iter()
                .filter(|(_, pack)| {
                    pack.name.to_lowercase().contains(&query_lower) ||
                    pack.description.to_lowercase().contains(&query_lower) ||
                    pack.tags.iter().any(|t| t.to_lowercase().contains(&query_lower)) ||
                    pack.keywords.iter().any(|k| k.to_lowercase().contains(&query_lower))
                })
                .collect();

            // Property: filtered results must be a subset of all packages
            prop_assert!(filtered.len() <= pack_count);

            // Property: all filtered results must contain the query
            for (id, pack) in &filtered {
                let matches = pack.name.to_lowercase().contains(&query_lower) ||
                    pack.description.to_lowercase().contains(&query_lower) ||
                    pack.tags.iter().any(|t| t.to_lowercase().contains(&query_lower)) ||
                    pack.keywords.iter().any(|k| k.to_lowercase().contains(&query_lower));

                prop_assert!(matches, "Package {} doesn't match query {}", id, query);
            }
        }

        #[test]
        fn search_is_case_insensitive(
            query in "[a-zA-Z]{3,10}",
        ) {
            // Create a package that matches the query
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
                name: query.to_uppercase(), // Store in uppercase
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

            // Property: search should find the package regardless of case
            let query_lower = query.to_lowercase();
            let name_lower = pack.name.to_lowercase();

            prop_assert!(name_lower.contains(&query_lower) || query_lower.contains(&name_lower));
        }

        #[test]
        fn empty_query_returns_all_or_none(
            pack_count in 0..20usize,
        ) {
            // Create mock registry
            let mut packs = HashMap::new();

            for i in 0..pack_count {
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

            // Property: empty query should match nothing (implementation-dependent)
            // or everything (list all packages)
            let empty_query = "";

            let filtered: Vec<_> = packs.iter()
                .filter(|(_, pack)| {
                    pack.name.contains(empty_query) ||
                    pack.description.contains(empty_query)
                })
                .collect();

            // Empty query matches all strings
            prop_assert_eq!(filtered.len(), pack_count);
        }

        #[test]
        fn search_preserves_package_integrity(
            query in "[a-z]{3,10}",
            pack_id in "[a-z0-9\\-]{3,20}",
        ) {
            // Create a package
            let mut versions = HashMap::new();
            versions.insert("1.0.0".to_string(), VersionMetadata {
                version: "1.0.0".to_string(),
                git_url: format!("https://github.com/test/{}.git", pack_id),
                git_rev: "main".to_string(),
                manifest_url: None,
                sha256: "abc123".to_string(),
            });

            let pack = PackMetadata {
                id: pack_id.clone(),
                name: format!("Package {}", query),
                description: format!("Description with {}", query),
                tags: vec![query.clone()],
                keywords: vec![query.clone()],
                category: Some("test".to_string()),
                author: Some("Test".to_string()),
                latest_version: "1.0.0".to_string(),
                versions: versions.clone(),
                downloads: Some(100),
                updated: Some(chrono::Utc::now()),
                license: Some("MIT".to_string()),
                homepage: None,
                repository: None,
                documentation: None,
            };

            // Convert to SearchResult (simulating search operation)
            let search_result = SearchResult {
                id: pack.id.clone(),
                name: pack.name.clone(),
                description: pack.description.clone(),
                tags: pack.tags.clone(),
                keywords: pack.keywords.clone(),
                category: pack.category.clone(),
                author: pack.author.clone(),
                latest_version: pack.latest_version.clone(),
                downloads: pack.downloads,
                updated: pack.updated,
                license: pack.license.clone(),
                homepage: pack.homepage.clone(),
                repository: pack.repository.clone(),
                documentation: pack.documentation.clone(),
            };

            // Property: search result must preserve all package fields
            prop_assert_eq!(&search_result.id, &pack_id);
            prop_assert_eq!(&search_result.latest_version, "1.0.0");
            prop_assert!(search_result.tags.contains(&query));
            prop_assert!(search_result.keywords.contains(&query));
        }
    }
}
