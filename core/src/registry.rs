use anyhow::{Context, Result};
use chrono::{DateTime, Utc};
use reqwest;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use url::Url;

/// Registry client for fetching rpack metadata from registry.rgen.dev
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

/// Metadata for a single rpack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackMetadata {
    pub id: String,
    pub name: String,
    pub description: String,
    pub tags: Vec<String>,
    pub latest_version: String,
    pub versions: HashMap<String, VersionMetadata>,
}

/// Metadata for a specific version of an rpack
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionMetadata {
    pub version: String,
    pub git_url: String,
    pub git_rev: String,
    pub manifest_url: Option<String>,
    pub sha256: String,
}

/// Search result for rpacks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub id: String,
    pub name: String,
    pub description: String,
    pub tags: Vec<String>,
    pub latest_version: String,
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
        let base_url = Url::parse("https://registry.rgen.dev")
            .context("Failed to parse registry base URL")?;
        
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
        let url = self.base_url.join("index.json")
            .context("Failed to construct index URL")?;

        let response = self.client
            .get(url)
            .send()
            .await
            .context("Failed to fetch registry index")?;

        if !response.status().is_success() {
            anyhow::bail!("Registry returned status: {}", response.status());
        }

        let index: RegistryIndex = response
            .json()
            .await
            .context("Failed to parse registry index")?;

        Ok(index)
    }

    /// Search for rpacks matching the query
    pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
        let index = self.fetch_index().await?;
        let query_lower = query.to_lowercase();
        
        let mut results = Vec::new();
        
        for (id, pack) in index.packs {
            // Search in name, description, and tags
            let matches = pack.name.to_lowercase().contains(&query_lower)
                || pack.description.to_lowercase().contains(&query_lower)
                || pack.tags.iter().any(|tag| tag.to_lowercase().contains(&query_lower));
            
            if matches {
                results.push(SearchResult {
                    id,
                    name: pack.name,
                    description: pack.description,
                    tags: pack.tags,
                    latest_version: pack.latest_version,
                });
            }
        }
        
        // Sort by relevance (exact matches first, then by name)
        results.sort_by(|a, b| {
            let a_exact = a.id.to_lowercase() == query_lower || a.name.to_lowercase() == query_lower;
            let b_exact = b.id.to_lowercase() == query_lower || b.name.to_lowercase() == query_lower;
            
            match (a_exact, b_exact) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.name.cmp(&b.name),
            }
        });
        
        Ok(results)
    }

    /// Resolve a pack ID to a specific version
    pub async fn resolve(&self, pack_id: &str, version: Option<&str>) -> Result<ResolvedPack> {
        let index = self.fetch_index().await?;
        
        let pack = index.packs.get(pack_id)
            .with_context(|| format!("Pack '{}' not found in registry", pack_id))?;
        
        let target_version = match version {
            Some(v) => v.to_string(),
            None => pack.latest_version.clone(),
        };
        
        let version_meta = pack.versions.get(&target_version)
            .with_context(|| format!("Version '{}' not found for pack '{}'", target_version, pack_id))?;
        
        Ok(ResolvedPack {
            id: pack_id.to_string(),
            version: target_version,
            git_url: version_meta.git_url.clone(),
            git_rev: version_meta.git_rev.clone(),
            sha256: version_meta.sha256.clone(),
        })
    }

    /// Check if a pack has updates available
    pub async fn check_updates(&self, pack_id: &str, current_version: &str) -> Result<Option<ResolvedPack>> {
        let index = self.fetch_index().await?;
        
        let pack = index.packs.get(pack_id)
            .with_context(|| format!("Pack '{}' not found in registry", pack_id))?;
        
        // Compare versions using semver
        let current = semver::Version::parse(current_version)
            .with_context(|| format!("Invalid current version: {}", current_version))?;
        
        let latest = semver::Version::parse(&pack.latest_version)
            .with_context(|| format!("Invalid latest version: {}", pack.latest_version))?;
        
        if latest > current {
            self.resolve(pack_id, Some(&pack.latest_version)).await
                .map(Some)
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;

    #[tokio::test]
    async fn test_registry_client_search() {
        // Create a temporary directory for mock registry
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("index.json");
        
        // Create mock index
        let mock_index = r#"{
            "updated": "2024-01-01T00:00:00Z",
            "packs": {
                "io.rgen.rust.cli-subcommand": {
                    "id": "io.rgen.rust.cli-subcommand",
                    "name": "Rust CLI subcommand",
                    "description": "Generate clap subcommands for Rust CLI applications",
                    "tags": ["rust", "cli", "clap", "subcommand"],
                    "latest_version": "0.2.1",
                    "versions": {
                        "0.2.1": {
                            "version": "0.2.1",
                            "git_url": "https://github.com/example/rpack.git",
                            "git_rev": "abc123",
                            "sha256": "def456"
                        }
                    }
                }
            }
        }"#;
        
        fs::write(&index_path, mock_index).unwrap();
        
        // Create registry client with file:// URL
        let base_url = Url::from_file_path(temp_dir.path()).unwrap();
        let client = RegistryClient::with_base_url(base_url).unwrap();
        
        // Test search
        let results = client.search("rust").await.unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, "io.rgen.rust.cli-subcommand");
    }

    #[tokio::test]
    async fn test_registry_client_resolve() {
        let temp_dir = TempDir::new().unwrap();
        let index_path = temp_dir.path().join("index.json");
        
        let mock_index = r#"{
            "updated": "2024-01-01T00:00:00Z",
            "packs": {
                "io.rgen.rust.cli-subcommand": {
                    "id": "io.rgen.rust.cli-subcommand",
                    "name": "Rust CLI subcommand",
                    "description": "Generate clap subcommands",
                    "tags": ["rust", "cli"],
                    "latest_version": "0.2.1",
                    "versions": {
                        "0.2.1": {
                            "version": "0.2.1",
                            "git_url": "https://github.com/example/rpack.git",
                            "git_rev": "abc123",
                            "sha256": "def456"
                        }
                    }
                }
            }
        }"#;
        
        fs::write(&index_path, mock_index).unwrap();
        
        let base_url = Url::from_file_path(temp_dir.path()).unwrap();
        let client = RegistryClient::with_base_url(base_url).unwrap();
        
        // Test resolve
        let resolved = client.resolve("io.rgen.rust.cli-subcommand", None).await.unwrap();
        assert_eq!(resolved.id, "io.rgen.rust.cli-subcommand");
        assert_eq!(resolved.version, "0.2.1");
        assert_eq!(resolved.git_url, "https://github.com/example/rpack.git");
    }
}