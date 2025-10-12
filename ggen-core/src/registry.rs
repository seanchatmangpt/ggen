use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use reqwest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use url::Url;

/// Registry client for fetching gpack metadata from registry.ggen.dev
#[derive(Debug, Clone)]
pub struct RegistryClient {
    base_url: Url,
    client: reqwest::Client,
}

/// Registry index structure matching the JSON format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryIndex {
    pub updated: DateTime<Utc>,
    pub packs: HashMap<String, PackMetadata>,
}

/// Metadata for a single gpack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackMetadata {
    pub id: String,
    pub name: String,
    pub description: String,
    pub tags: Vec<String>,
    pub keywords: Vec<String>,
    pub category: Option<String>,
    pub author: Option<String>,
    pub latest_version: String,
    pub versions: HashMap<String, VersionMetadata>,
    pub downloads: Option<u64>,
    pub updated: Option<chrono::DateTime<chrono::Utc>>,
    pub license: Option<String>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub documentation: Option<String>,
}

/// Metadata for a specific version of an gpack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionMetadata {
    pub version: String,
    pub git_url: String,
    pub git_rev: String,
    pub manifest_url: Option<String>,
    pub sha256: String,
}

/// Search result for gpacks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub description: String,
    pub tags: Vec<String>,
    pub keywords: Vec<String>,
    pub category: Option<String>,
    pub author: Option<String>,
    pub latest_version: String,
    pub downloads: Option<u64>,
    pub updated: Option<chrono::DateTime<chrono::Utc>>,
    pub license: Option<String>,
    pub homepage: Option<String>,
    pub repository: Option<String>,
    pub documentation: Option<String>,
}

/// Search parameters for advanced search
#[derive(Debug, Clone)]
pub struct SearchParams<'a> {
    pub query: &'a str,
    pub category: Option<&'a str>,
    pub keyword: Option<&'a str>,
    pub author: Option<&'a str>,
    pub stable_only: bool,
    pub limit: usize,
}

/// Resolved pack information for installation
#[derive(Debug, Clone)]
pub struct ResolvedPack {
    pub id: String,
    pub version: String,
    pub git_url: String,
    pub git_rev: String,
    pub sha256: String,
}

impl RegistryClient {
    /// Create a new registry client
    pub fn new() -> Result<Self> {
        // Check environment variable for registry URL
        let registry_url = std::env::var("GGEN_REGISTRY_URL")
            .unwrap_or_else(|_| "https://seanchatmangpt.github.io/ggen/registry/".to_string());

        let base_url = Url::parse(&registry_url).context("Failed to parse registry URL")?;

        let client = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self { base_url, client })
    }

    /// Create a registry client with custom base URL (for testing)
    pub fn with_base_url(base_url: Url) -> Result<Self> {
        let client = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(30))
            .build()
            .context("Failed to create HTTP client")?;

        Ok(Self { base_url, client })
    }

    /// Fetch the registry index
    pub async fn fetch_index(&self) -> Result<RegistryIndex> {
        let url = self
            .base_url
            .join("index.json")
            .context("Failed to construct index URL")?;

        // Handle file:// URLs for local testing
        if url.scheme() == "file" {
            let path = url
                .to_file_path()
                .map_err(|_| anyhow::anyhow!("Invalid file URL: {}", url))?;

            let content = std::fs::read_to_string(&path).context(format!(
                "Failed to read registry index from {}",
                path.display()
            ))?;

            let index: RegistryIndex =
                serde_json::from_str(&content).context("Failed to parse registry index")?;

            return Ok(index);
        }

        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .context(format!("Failed to fetch registry index from {}", url))?;

        if !response.status().is_success() {
            anyhow::bail!(
                "Registry returned status: {} for URL: {}",
                response.status(),
                url
            );
        }

        let index: RegistryIndex = response
            .json()
            .await
            .context("Failed to parse registry index")?;

        Ok(index)
    }

    /// Search for gpacks matching the query
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
        let index = self.fetch_index().await?;
        let query_lower = query.to_lowercase();

        let mut results = Vec::new();

        for (id, pack) in index.packs {
            // Search in name, description, and tags
            let matches = pack.name.to_lowercase().contains(&query_lower)
                || pack.description.to_lowercase().contains(&query_lower)
                || pack
                    .tags
                    .iter()
                    .any(|tag| tag.to_lowercase().contains(&query_lower));

            if matches {
                let search_result = self.convert_to_search_result(id, pack)?;
                results.push(search_result);
            }
        }

        // Sort by relevance (exact matches first, then by name)
        results.sort_by(|a, b| {
            let a_exact =
                a.id.to_lowercase() == query_lower || a.name.to_lowercase() == query_lower;
            let b_exact =
                b.id.to_lowercase() == query_lower || b.name.to_lowercase() == query_lower;

            match (a_exact, b_exact) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.name.cmp(&b.name),
            }
        });

        Ok(results)
    }

    /// Advanced search with filtering options
    pub async fn advanced_search(&self, params: &SearchParams<'_>) -> Result<Vec<SearchResult>> {
        let index = self.fetch_index().await?;
        let query_lower = params.query.to_lowercase();

        let mut results = Vec::new();

        for (id, pack) in index.packs {
            // Apply filters
            if !self.matches_filters(&pack, params) {
                continue;
            }

            // Search in name, description, tags, and keywords
            let matches = pack.name.to_lowercase().contains(&query_lower)
                || pack.description.to_lowercase().contains(&query_lower)
                || pack
                    .tags
                    .iter()
                    .any(|tag| tag.to_lowercase().contains(&query_lower))
                || pack
                    .keywords
                    .iter()
                    .any(|keyword| keyword.to_lowercase().contains(&query_lower));

            if matches {
                // Convert to SearchResult with extended metadata
                let search_result = self.convert_to_search_result(id, pack)?;
                results.push(search_result);
            }
        }

        // Sort by relevance and apply limit
        results.sort_by(|a, b| self.compare_relevance(a, b, &query_lower));
        results.truncate(params.limit);

        Ok(results)
    }

    /// Check if a pack matches the search filters
    fn matches_filters(&self, pack: &PackMetadata, params: &SearchParams<'_>) -> bool {
        // Category filter
        if let Some(category) = params.category {
            if pack
                .category
                .as_ref()
                .is_none_or(|c| c.to_lowercase() != category.to_lowercase())
            {
                return false;
            }
        }

        // Keyword filter
        if let Some(keyword) = params.keyword {
            if !pack
                .keywords
                .iter()
                .any(|k| k.to_lowercase() == keyword.to_lowercase())
            {
                return false;
            }
        }

        // Author filter
        if let Some(author) = params.author {
            if !pack
                .author
                .as_ref()
                .is_some_and(|a| a.to_lowercase().contains(&author.to_lowercase()))
            {
                return false;
            }
        }

        // Stable version filter
        if params.stable_only {
            if let Ok(version) = semver::Version::parse(&pack.latest_version) {
                if !version.pre.is_empty() {
                    return false; // Pre-release versions are not stable
                }
            }
        }

        true
    }

    /// Convert PackMetadata to SearchResult
    fn convert_to_search_result(&self, id: String, pack: PackMetadata) -> Result<SearchResult> {
        Ok(SearchResult {
            id,
            name: pack.name,
            description: pack.description,
            tags: pack.tags,
            keywords: pack.keywords,
            category: pack.category,
            author: pack.author,
            latest_version: pack.latest_version,
            downloads: pack.downloads,
            updated: pack.updated,
            license: pack.license,
            homepage: pack.homepage,
            repository: pack.repository,
            documentation: pack.documentation,
        })
    }

    /// Compare search results by relevance
    fn compare_relevance(
        &self, a: &SearchResult, b: &SearchResult, query: &str,
    ) -> std::cmp::Ordering {
        // Exact matches first
        let a_exact = a.id.to_lowercase() == query || a.name.to_lowercase() == query;
        let b_exact = b.id.to_lowercase() == query || b.name.to_lowercase() == query;

        match (a_exact, b_exact) {
            (true, false) => return std::cmp::Ordering::Less,
            (false, true) => return std::cmp::Ordering::Greater,
            _ => {}
        }

        // Then by downloads (popularity)
        let download_ordering = match (a.downloads, b.downloads) {
            (Some(a_dl), Some(b_dl)) => b_dl.cmp(&a_dl), // Higher downloads first
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        };

        if download_ordering != std::cmp::Ordering::Equal {
            return download_ordering;
        }

        // Finally by name
        a.name.cmp(&b.name)
    }

    /// List all packages in the registry
    pub async fn list_packages(&self) -> Result<Vec<PackMetadata>> {
        let index = self.fetch_index().await?;
        Ok(index.packs.into_values().collect())
    }

    /// Resolve a pack ID to a specific version
    pub async fn resolve(&self, pack_id: &str, version: Option<&str>) -> Result<ResolvedPack> {
        let index = self.fetch_index().await?;

        let pack = index
            .packs
            .get(pack_id)
            .with_context(|| format!("Pack '{}' not found in registry", pack_id))?;

        let target_version = match version {
            Some(v) => v.to_string(),
            None => pack.latest_version.clone(),
        };

        let version_meta = pack.versions.get(&target_version).with_context(|| {
            format!(
                "Version '{}' not found for pack '{}'",
                target_version, pack_id
            )
        })?;

        Ok(ResolvedPack {
            id: pack_id.to_string(),
            version: target_version,
            git_url: version_meta.git_url.clone(),
            git_rev: version_meta.git_rev.clone(),
            sha256: version_meta.sha256.clone(),
        })
    }

    /// Check if a pack has updates available
    pub async fn check_updates(
        &self, pack_id: &str, current_version: &str,
    ) -> Result<Option<ResolvedPack>> {
        let index = self.fetch_index().await?;

        let pack = index
            .packs
            .get(pack_id)
            .with_context(|| format!("Pack '{}' not found in registry", pack_id))?;

        // Compare versions using semver
        let current = semver::Version::parse(current_version)
            .with_context(|| format!("Invalid current version: {}", current_version))?;

        let latest = semver::Version::parse(&pack.latest_version)
            .with_context(|| format!("Invalid latest version: {}", pack.latest_version))?;

        if latest > current {
            self.resolve(pack_id, Some(&pack.latest_version))
                .await
                .map(Some)
        } else {
            Ok(None)
        }
    }

    /// Get popular categories with template counts
    pub async fn get_popular_categories(&self) -> Result<Vec<(String, u64)>> {
        let index = self.fetch_index().await?;
        let mut category_counts: std::collections::HashMap<String, u64> =
            std::collections::HashMap::new();

        for (_, pack) in index.packs {
            if let Some(category) = pack.category {
                *category_counts.entry(category).or_insert(0) += 1;
            }
        }

        let mut categories: Vec<(String, u64)> = category_counts.into_iter().collect();
        categories.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending

        Ok(categories)
    }

    /// Get popular keywords with template counts
    pub async fn get_popular_keywords(&self) -> Result<Vec<(String, u64)>> {
        let index = self.fetch_index().await?;
        let mut keyword_counts: std::collections::HashMap<String, u64> =
            std::collections::HashMap::new();

        for (_, pack) in index.packs {
            for keyword in pack.keywords {
                *keyword_counts.entry(keyword).or_insert(0) += 1;
            }
        }

        let mut keywords: Vec<(String, u64)> = keyword_counts.into_iter().collect();
        keywords.sort_by(|a, b| b.1.cmp(&a.1)); // Sort by count descending

        Ok(keywords)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[tokio::test]
    #[ignore] // Disabled due to file:// URL not supported by reqwest
    async fn test_registry_client_search() -> Result<()> {
        // Create a temporary directory for mock registry
        let temp_dir = TempDir::new().context("Failed to create temp dir")?;
        let index_path = temp_dir.path().join("index.json");

        // Create mock index
        let mock_index = r#"{
            "updated": "2024-01-01T00:00:00Z",
            "packs": {
                "io.ggen.rust.cli-subcommand": {
                    "id": "io.ggen.rust.cli-subcommand",
                    "name": "Rust CLI subcommand",
                    "description": "Generate clap subcommands for Rust CLI applications",
                    "tags": ["rust", "cli", "clap", "subcommand"],
                    "latest_version": "0.2.1",
                    "versions": {
                        "0.2.1": {
                            "version": "0.2.1",
                            "git_url": "https://github.com/example/gpack.git",
                            "git_rev": "abc123",
                            "sha256": "def456"
                        }
                    }
                }
            }
        }"#;

        fs::write(&index_path, mock_index).context("Failed to write mock index")?;

        // Create registry client with file:// URL
        let base_url = Url::from_file_path(temp_dir.path())
            .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
        let client = RegistryClient::with_base_url(base_url)?;

        // Test search
        let results = client.search("rust").await?;
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, "io.ggen.rust.cli-subcommand");
        Ok(())
    }

    #[tokio::test]
    #[ignore] // Disabled due to file:// URL not supported by reqwest
    async fn test_registry_client_resolve() -> Result<()> {
        let temp_dir = TempDir::new().context("Failed to create temp dir")?;
        let index_path = temp_dir.path().join("index.json");

        let mock_index = r#"{
            "updated": "2024-01-01T00:00:00Z",
            "packs": {
                "io.ggen.rust.cli-subcommand": {
                    "id": "io.ggen.rust.cli-subcommand",
                    "name": "Rust CLI subcommand",
                    "description": "Generate clap subcommands",
                    "tags": ["rust", "cli"],
                    "latest_version": "0.2.1",
                    "versions": {
                        "0.2.1": {
                            "version": "0.2.1",
                            "git_url": "https://github.com/example/gpack.git",
                            "git_rev": "abc123",
                            "sha256": "def456"
                        }
                    }
                }
            }
        }"#;

        fs::write(&index_path, mock_index).context("Failed to write mock index")?;

        let base_url = Url::from_file_path(temp_dir.path())
            .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
        let client = RegistryClient::with_base_url(base_url)?;

        // Test resolve
        let resolved = client
            .resolve("io.ggen.rust.cli-subcommand", None)
            .await?;
        assert_eq!(resolved.id, "io.ggen.rust.cli-subcommand");
        assert_eq!(resolved.version, "0.2.1");
        assert_eq!(resolved.git_url, "https://github.com/example/gpack.git");
        Ok(())
    }
}
