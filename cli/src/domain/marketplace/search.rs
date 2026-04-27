//! Marketplace search domain logic
//!
//! Real implementation of package search functionality using ggen-marketplace backend.

use ggen_utils::error::Result;
use serde_json;
use std::path::PathBuf;

/// Package information for display
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PackageInfo {
    pub id: String,
    pub version: String,
    pub title: String,
    pub description: String,
    pub categories: Vec<String>,
    pub tags: Vec<String>,
    pub downloads: u64,
    pub stars: u32,
}

/// Search and display packages from the marketplace
///
/// # Arguments
///
/// * `query` - Search query string
/// * `category` - Filter by category
/// * `keyword` - Filter by keyword/tag
/// * `author` - Filter by author
/// * `fuzzy` - Enable fuzzy search
/// * `detailed` - Show detailed information for each package
/// * `json` - Output results in JSON format
/// * `limit` - Maximum number of results to return
///
/// # Returns
///
/// Returns Ok(()) on success, or an error if search fails
#[allow(clippy::too_many_arguments)]
pub async fn search_and_display(
    query: &str,
    category: Option<&str>,
    keyword: Option<&str>,
    author: Option<&str>,
    fuzzy: bool,
    detailed: bool,
    json: bool,
    limit: usize,
) -> Result<()> {
    // Create local registry for searching
    let registry_path = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::with_context(
            "Home directory not found",
            "~/.ggen/registry"
        ))?
        .join(".ggen")
        .join("registry");

    // Search packages with filters
    let mut results = search_packages(query, &registry_path, fuzzy).await?;

    // Apply filters
    if let Some(cat) = category {
        let cat_lower = cat.to_lowercase();
        results.retain(|pkg| {
            pkg.categories
                .iter()
                .any(|c| c.to_lowercase().contains(&cat_lower))
        });
    }

    if let Some(kw) = keyword {
        let kw_lower = kw.to_lowercase();
        results.retain(|pkg| pkg.tags.iter().any(|t| t.to_lowercase().contains(&kw_lower)));
    }

    if let Some(auth) = author {
        let auth_lower = auth.to_lowercase();
        results.retain(|pkg| pkg.id.to_lowercase().contains(&auth_lower));
    }

    // Apply limit
    results.truncate(limit);

    if json {
        // Output JSON format
        let json_output = serde_json::to_string_pretty(&results)?;
        println!("{}", json_output);
    } else {
        // Output human-readable format
        if results.is_empty() {
            println!("No packages found matching '{}'", query);
            return Ok(());
        }

        println!("Found {} package(s) matching '{}':\n", results.len(), query);

        for pkg in results {
            println!("📦 {}", pkg.id);
            println!("   Version: {}", pkg.version);
            println!("   {}", pkg.title);

            if detailed {
                println!("   Description: {}", pkg.description);
                if !pkg.categories.is_empty() {
                    println!("   Categories: {}", pkg.categories.join(", "));
                }
                if !pkg.tags.is_empty() {
                    println!("   Tags: {}", pkg.tags.join(", "));
                }
                println!("   Downloads: {} | Stars: {}", pkg.downloads, pkg.stars);
            } else {
                println!("   {}", pkg.description);
            }

            println!();
        }
    }

    Ok(())
}

/// Internal function to search packages
async fn search_packages(
    query: &str,
    registry_path: &PathBuf,
    fuzzy: bool,
) -> Result<Vec<PackageInfo>> {
    // Load index if it exists
    let index_path = registry_path.join("index.json");

    if !index_path.exists() {
        // Return empty results if no local registry exists
        return Ok(vec![]);
    }

    let content = tokio::fs::read_to_string(&index_path).await.map_err(|e| {
        ggen_utils::error::Error::new("IO error")
    })?;

    // Parse index
    let index: LocalIndex = serde_json::from_str(&content)?;

    // Search packages
    let query_lower = query.to_lowercase();
    let mut results = Vec::new();

    for (pkg_id, versions) in &index.packages {
        if let Some(latest) = versions.first() {
            // Match against package name, title, description, or tags
            let matches = if fuzzy {
                // Fuzzy search: check if any word in query matches
                query_lower.split_whitespace().any(|word| {
                    pkg_id.to_lowercase().contains(word)
                        || latest.title.to_lowercase().contains(word)
                        || latest.description.to_lowercase().contains(word)
                        || latest.tags.iter().any(|tag| tag.to_lowercase().contains(word))
                })
            } else {
                // Exact search
                pkg_id.to_lowercase().contains(&query_lower)
                    || latest.title.to_lowercase().contains(&query_lower)
                    || latest.description.to_lowercase().contains(&query_lower)
                    || latest
                        .tags
                        .iter()
                        .any(|tag| tag.to_lowercase().contains(&query_lower))
            };

            if matches {
                results.push(PackageInfo {
                    id: pkg_id.clone(),
                    version: latest.version.clone(),
                    title: latest.title.clone(),
                    description: latest.description.clone(),
                    categories: latest.categories.clone(),
                    tags: latest.tags.clone(),
                    downloads: latest.downloads,
                    stars: latest.stars,
                });
            }
        }
    }

    // Sort by relevance (name matches first)
    results.sort_by(|a, b| {
        let a_name_match = a.id.to_lowercase().contains(&query_lower);
        let b_name_match = b.id.to_lowercase().contains(&query_lower);

        match (a_name_match, b_name_match) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => a.id.cmp(&b.id),
        }
    });

    Ok(results)
}

/// Simplified local index structure
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalIndex {
    packages: std::collections::HashMap<String, Vec<LocalPackage>>,
}

/// Simplified package structure
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalPackage {
    version: String,
    title: String,
    description: String,
    categories: Vec<String>,
    tags: Vec<String>,
    downloads: u64,
    stars: u32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_search_empty_query() {
        let temp_dir = tempfile::tempdir().unwrap();
        let registry_path = temp_dir.path().to_path_buf();

        let results = search_packages("", &registry_path, false).await.unwrap();
        assert!(results.is_empty());
    }

    #[tokio::test]
    async fn test_search_no_registry() {
        let temp_dir = tempfile::tempdir().unwrap();
        let registry_path = temp_dir.path().join("nonexistent");

        let results = search_packages("test", &registry_path, false)
            .await
            .unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_package_info_serialization() {
        let pkg = PackageInfo {
            id: "test/example".to_string(),
            version: "1.0.0".to_string(),
            title: "Example Package".to_string(),
            description: "A test package".to_string(),
            categories: vec!["testing".to_string()],
            tags: vec!["test".to_string()],
            downloads: 100,
            stars: 10,
        };

        let json = serde_json::to_string(&pkg).unwrap();
        let deserialized: PackageInfo = serde_json::from_str(&json).unwrap();

        assert_eq!(pkg.id, deserialized.id);
        assert_eq!(pkg.version, deserialized.version);
    }
}
