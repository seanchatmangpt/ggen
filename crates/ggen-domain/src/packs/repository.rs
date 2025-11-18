//! Pack repository trait and implementations
//!
//! This module provides abstraction for pack storage and retrieval,
//! allowing multiple backends (filesystem, remote registry, etc.)

use crate::packs::types::Pack;
use async_trait::async_trait;
use ggen_utils::error::{Error, Result};
use std::path::PathBuf;

/// Repository trait for pack storage operations
#[async_trait]
pub trait PackRepository: Send + Sync {
    /// Load a pack by ID
    async fn load(&self, pack_id: &str) -> Result<Pack>;

    /// List all available packs, optionally filtered by category
    async fn list(&self, category: Option<&str>) -> Result<Vec<Pack>>;

    /// Save a pack (for writable repositories)
    async fn save(&self, pack: &Pack) -> Result<()>;

    /// Check if a pack exists
    async fn exists(&self, pack_id: &str) -> Result<bool>;

    /// Delete a pack (for writable repositories)
    async fn delete(&self, pack_id: &str) -> Result<()>;
}

/// Filesystem-based pack repository
///
/// Stores packs as TOML files in a directory structure:
/// ```text
/// marketplace/packs/
///   web-api-stack.toml
///   data-science-pack.toml
///   ...
/// ```
#[derive(Debug, Clone)]
pub struct FileSystemRepository {
    base_path: PathBuf,
}

impl FileSystemRepository {
    /// Create new filesystem repository
    ///
    /// # Arguments
    /// * `base_path` - Base directory containing packs (e.g., "marketplace/packs")
    pub fn new(base_path: impl Into<PathBuf>) -> Self {
        Self {
            base_path: base_path.into(),
        }
    }

    /// Create repository with automatic path discovery
    ///
    /// Tries multiple common paths to find the packs directory
    pub fn discover() -> Result<Self> {
        let possible_paths = vec![
            PathBuf::from("marketplace/packs"),
            PathBuf::from("../marketplace/packs"),
            PathBuf::from("../../marketplace/packs"),
            dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".ggen/packs"),
        ];

        for path in possible_paths {
            if path.exists() && path.is_dir() {
                tracing::debug!("Found packs directory at: {}", path.display());
                return Ok(Self::new(path));
            }
        }

        Err(Error::new(
            "Packs directory not found. Expected marketplace/packs/ or ~/.ggen/packs/",
        ))
    }

    /// Get pack file path
    fn pack_path(&self, pack_id: &str) -> PathBuf {
        self.base_path.join(format!("{}.toml", pack_id))
    }

    /// Validate pack ID for safety
    fn validate_pack_id(&self, pack_id: &str) -> Result<()> {
        // Prevent path traversal
        if pack_id.contains("..") || pack_id.contains('/') || pack_id.contains('\\') {
            return Err(Error::new(
                "Invalid pack ID: must not contain path separators or traversal sequences",
            ));
        }

        // Validate characters
        if !pack_id
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(Error::new(
                "Invalid pack ID: must contain only alphanumeric characters, hyphens, and underscores",
            ));
        }

        Ok(())
    }
}

#[async_trait]
impl PackRepository for FileSystemRepository {
    async fn load(&self, pack_id: &str) -> Result<Pack> {
        self.validate_pack_id(pack_id)?;

        let pack_path = self.pack_path(pack_id);

        if !pack_path.exists() {
            return Err(Error::new(&format!(
                "Pack '{}' not found at {}",
                pack_id,
                pack_path.display()
            )));
        }

        let content = tokio::fs::read_to_string(&pack_path).await?;

        let pack_file: crate::packs::types::PackFile = toml::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse pack '{}': {}", pack_id, e)))?;

        Ok(pack_file.pack)
    }

    async fn list(&self, category: Option<&str>) -> Result<Vec<Pack>> {
        let mut packs = Vec::new();

        let mut entries = tokio::fs::read_dir(&self.base_path).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("toml") {
                match tokio::fs::read_to_string(&path).await {
                    Ok(content) => {
                        match toml::from_str::<crate::packs::types::PackFile>(&content) {
                            Ok(pack_file) => {
                                let pack = pack_file.pack;

                                // Filter by category if specified
                                if let Some(cat) = category {
                                    if pack.category == cat {
                                        packs.push(pack);
                                    }
                                } else {
                                    packs.push(pack);
                                }
                            }
                            Err(e) => {
                                tracing::warn!("Failed to parse pack {}: {}", path.display(), e);
                            }
                        }
                    }
                    Err(e) => {
                        tracing::warn!("Failed to read pack {}: {}", path.display(), e);
                    }
                }
            }
        }

        Ok(packs)
    }

    async fn save(&self, pack: &Pack) -> Result<()> {
        self.validate_pack_id(&pack.id)?;

        let pack_path = self.pack_path(&pack.id);

        // Create directory if it doesn't exist
        if let Some(parent) = pack_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let pack_file = crate::packs::types::PackFile { pack: pack.clone() };

        let content = toml::to_string_pretty(&pack_file)
            .map_err(|e| Error::new(&format!("Failed to serialize pack: {}", e)))?;

        tokio::fs::write(&pack_path, content).await?;

        tracing::info!("Saved pack '{}' to {}", pack.id, pack_path.display());

        Ok(())
    }

    async fn exists(&self, pack_id: &str) -> Result<bool> {
        self.validate_pack_id(pack_id)?;

        let pack_path = self.pack_path(pack_id);
        Ok(pack_path.exists())
    }

    async fn delete(&self, pack_id: &str) -> Result<()> {
        self.validate_pack_id(pack_id)?;

        let pack_path = self.pack_path(pack_id);

        if !pack_path.exists() {
            return Err(Error::new(&format!("Pack '{}' not found", pack_id)));
        }

        tokio::fs::remove_file(&pack_path).await?;

        tracing::info!("Deleted pack '{}'", pack_id);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackMetadata, PackTemplate};
    use std::collections::HashMap;

    #[tokio::test]
    async fn test_filesystem_repo_validates_pack_id() {
        let repo = FileSystemRepository::new("/tmp/test-packs");

        // Path traversal attempts should fail
        assert!(repo.validate_pack_id("../etc/passwd").is_err());
        assert!(repo.validate_pack_id("pack/with/slash").is_err());

        // Valid pack IDs should pass
        assert!(repo.validate_pack_id("web-api-stack").is_ok());
        assert!(repo.validate_pack_id("data_science_pack").is_ok());
        assert!(repo.validate_pack_id("pack123").is_ok());
    }

    #[tokio::test]
    async fn test_filesystem_repo_save_and_load() {
        let temp_dir = tempfile::tempdir().unwrap();
        let repo = FileSystemRepository::new(temp_dir.path());

        let pack = Pack {
            id: "test-pack".to_string(),
            name: "Test Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "A test pack".to_string(),
            category: "test".to_string(),
            author: Some("Test Author".to_string()),
            repository: None,
            license: Some("MIT".to_string()),
            packages: vec!["package1".to_string()],
            templates: vec![PackTemplate {
                name: "main".to_string(),
                path: "templates/main.tmpl".to_string(),
                description: "Main template".to_string(),
                variables: vec!["project_name".to_string()],
            }],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec!["test".to_string()],
            keywords: vec!["testing".to_string()],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        // Save pack
        repo.save(&pack).await.unwrap();

        // Load pack
        let loaded = repo.load("test-pack").await.unwrap();

        assert_eq!(loaded.id, "test-pack");
        assert_eq!(loaded.name, "Test Pack");
        assert_eq!(loaded.packages.len(), 1);
        assert_eq!(loaded.templates.len(), 1);
    }

    #[tokio::test]
    async fn test_filesystem_repo_list() {
        let temp_dir = tempfile::tempdir().unwrap();
        let repo = FileSystemRepository::new(temp_dir.path());

        // Create multiple packs
        for i in 1..=3 {
            let pack = Pack {
                id: format!("pack{}", i),
                name: format!("Pack {}", i),
                version: "1.0.0".to_string(),
                description: format!("Pack {}", i),
                category: if i == 1 {
                    "web".to_string()
                } else {
                    "cli".to_string()
                },
                author: None,
                repository: None,
                license: None,
                packages: vec![],
                templates: vec![],
                sparql_queries: HashMap::new(),
                dependencies: vec![],
                tags: vec![],
                keywords: vec![],
                production_ready: true,
                metadata: PackMetadata::default(),
            };

            repo.save(&pack).await.unwrap();
        }

        // List all packs
        let all_packs = repo.list(None).await.unwrap();
        assert_eq!(all_packs.len(), 3);

        // List packs by category
        let web_packs = repo.list(Some("web")).await.unwrap();
        assert_eq!(web_packs.len(), 1);
        assert_eq!(web_packs[0].id, "pack1");
    }

    #[tokio::test]
    async fn test_filesystem_repo_exists() {
        let temp_dir = tempfile::tempdir().unwrap();
        let repo = FileSystemRepository::new(temp_dir.path());

        let pack = Pack {
            id: "exists-pack".to_string(),
            name: "Exists Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "Test".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        // Initially doesn't exist
        assert!(!repo.exists("exists-pack").await.unwrap());

        // After saving, exists
        repo.save(&pack).await.unwrap();
        assert!(repo.exists("exists-pack").await.unwrap());
    }

    #[tokio::test]
    async fn test_filesystem_repo_delete() {
        let temp_dir = tempfile::tempdir().unwrap();
        let repo = FileSystemRepository::new(temp_dir.path());

        let pack = Pack {
            id: "delete-pack".to_string(),
            name: "Delete Pack".to_string(),
            version: "1.0.0".to_string(),
            description: "Test".to_string(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        };

        repo.save(&pack).await.unwrap();
        assert!(repo.exists("delete-pack").await.unwrap());

        repo.delete("delete-pack").await.unwrap();
        assert!(!repo.exists("delete-pack").await.unwrap());
    }
}
