use crate::error::{MarketplaceError, Result};
use crate::models::{Package, PackageId, Query, RegistryMetadata};
use crate::traits::Registry;
use async_trait::async_trait;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Local filesystem-based registry
///
/// Stores package metadata in a local SQLite database and provides
/// offline-first package discovery and management.
///
/// # Example
///
/// ```no_run
/// use ggen_marketplace::backend::LocalRegistry;
/// use ggen_marketplace::traits::Registry;
/// use std::path::PathBuf;
///
/// # async fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let registry = LocalRegistry::new(PathBuf::from("~/.ggen/registry")).await?;
/// let packages = registry.search(&Query::new("rust web")).await?;
/// # Ok(())
/// # }
/// ```
pub struct LocalRegistry {
    db_path: PathBuf,
    packages: Arc<RwLock<HashMap<PackageId, Vec<Package>>>>,
}

impl LocalRegistry {
    /// Create a new local registry
    ///
    /// # Arguments
    ///
    /// * `db_path` - Path to the local database directory
    ///
    /// # Errors
    ///
    /// Returns an error if the database cannot be initialized.
    pub async fn new(db_path: PathBuf) -> Result<Self> {
        // Create directory if it doesn't exist
        if let Some(parent) = db_path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(|e| {
                    MarketplaceError::io_error(e.to_string(), db_path.to_string_lossy().as_ref())
                })?;
        }

        let packages = Arc::new(RwLock::new(HashMap::new()));

        let registry = Self { db_path, packages };

        // Load existing packages from disk
        registry.load_from_disk().await?;

        Ok(registry)
    }

    /// Load packages from disk
    async fn load_from_disk(&self) -> Result<()> {
        let index_path = self.db_path.join("index.json");

        if !index_path.exists() {
            return Ok(());
        }

        let content = tokio::fs::read_to_string(&index_path)
            .await
            .map_err(|e| {
                MarketplaceError::io_error(e.to_string(), index_path.to_string_lossy().as_ref())
            })?;

        let index: LocalIndex = serde_json::from_str(&content).map_err(|e| {
            MarketplaceError::parse_error(e.to_string(), "local index")
        })?;

        let mut packages = self.packages.write().await;
        *packages = index.packages;

        Ok(())
    }

    /// Save packages to disk
    async fn save_to_disk(&self) -> Result<()> {
        let index_path = self.db_path.join("index.json");

        let packages = self.packages.read().await;
        let index = LocalIndex {
            version: "1.0".to_string(),
            packages: packages.clone(),
        };

        let content = serde_json::to_string_pretty(&index).map_err(|e| {
            MarketplaceError::serialize_error(e.to_string(), "local index")
        })?;

        tokio::fs::write(&index_path, content)
            .await
            .map_err(|e| {
                MarketplaceError::io_error(e.to_string(), index_path.to_string_lossy().as_ref())
            })?;

        Ok(())
    }

    /// Add a package to the local registry
    pub async fn add_package(&self, package: Package) -> Result<()> {
        let mut packages = self.packages.write().await;

        let versions = packages.entry(package.id.clone()).or_insert_with(Vec::new);

        // Check if version already exists
        if versions.iter().any(|p| p.version == package.version) {
            return Err(MarketplaceError::already_exists(
                "package version",
                &format!("{}@{}", package.id, package.version),
            ));
        }

        versions.push(package);

        // Sort by version (newest first)
        versions.sort_by(|a, b| b.version.cmp(&a.version));

        drop(packages);

        self.save_to_disk().await?;

        Ok(())
    }

    /// Remove a package version from the local registry
    pub async fn remove_package(&self, id: &PackageId, version: &str) -> Result<()> {
        let mut packages = self.packages.write().await;

        if let Some(versions) = packages.get_mut(id) {
            let original_len = versions.len();
            versions.retain(|p| p.version.to_string() != version);

            if versions.len() == original_len {
                return Err(MarketplaceError::not_found(
                    "package version",
                    &format!("{}@{}", id, version),
                ));
            }

            if versions.is_empty() {
                packages.remove(id);
            }
        } else {
            return Err(MarketplaceError::not_found("package", &id.to_string()));
        }

        drop(packages);

        self.save_to_disk().await?;

        Ok(())
    }
}

#[async_trait]
impl Registry for LocalRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        let packages = self.packages.read().await;

        let mut results = Vec::new();
        let query_lower = query.text.to_lowercase();

        for versions in packages.values() {
            if let Some(latest) = versions.first() {
                // Search in name, title, description, and tags
                let matches = latest.id.name.to_lowercase().contains(&query_lower)
                    || latest
                        .metadata
                        .title
                        .to_lowercase()
                        .contains(&query_lower)
                    || latest
                        .metadata
                        .description
                        .to_lowercase()
                        .contains(&query_lower)
                    || latest
                        .metadata
                        .tags
                        .iter()
                        .any(|tag| tag.to_lowercase().contains(&query_lower));

                if matches {
                    results.push(latest.clone());
                }
            }
        }

        // Sort by relevance (simple: by name match first, then title match)
        results.sort_by(|a, b| {
            let a_name_match = a.id.name.to_lowercase().contains(&query_lower);
            let b_name_match = b.id.name.to_lowercase().contains(&query_lower);

            match (a_name_match, b_name_match) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.id.name.cmp(&b.id.name),
            }
        });

        Ok(results)
    }

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        let packages = self.packages.read().await;

        packages
            .get(id)
            .and_then(|versions| versions.first())
            .cloned()
            .ok_or_else(|| MarketplaceError::not_found("package", &id.to_string()))
    }

    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package> {
        let packages = self.packages.read().await;

        packages
            .get(id)
            .and_then(|versions| versions.iter().find(|p| p.version.to_string() == version))
            .cloned()
            .ok_or_else(|| {
                MarketplaceError::not_found("package version", &format!("{}@{}", id, version))
            })
    }

    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>> {
        let packages = self.packages.read().await;

        packages
            .get(id)
            .cloned()
            .ok_or_else(|| MarketplaceError::not_found("package", &id.to_string()))
    }

    async fn publish(&self, package: Package) -> Result<()> {
        self.add_package(package).await
    }

    async fn delete(&self, id: &PackageId, version: &str) -> Result<()> {
        self.remove_package(id, version).await
    }

    async fn exists(&self, id: &PackageId) -> Result<bool> {
        let packages = self.packages.read().await;
        Ok(packages.contains_key(id))
    }

    async fn metadata(&self) -> Result<RegistryMetadata> {
        let packages = self.packages.read().await;

        Ok(RegistryMetadata {
            name: "Local Registry".to_string(),
            description: "Local filesystem-based package registry".to_string(),
            version: "1.0.0".to_string(),
            package_count: packages.len(),
            api_version: "v1".to_string(),
            features: vec!["offline".to_string(), "local-storage".to_string()],
        })
    }
}

/// Local index file structure
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct LocalIndex {
    pub version: String,
    pub packages: HashMap<PackageId, Vec<Package>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{ContentId, HashAlgorithm, Version};

    #[tokio::test]
    async fn test_local_registry_new() {
        let temp_dir = tempfile::tempdir().unwrap();
        let db_path = temp_dir.path().join("registry");

        let registry = LocalRegistry::new(db_path).await;
        assert!(registry.is_ok());
    }

    #[tokio::test]
    async fn test_add_and_search_package() {
        let temp_dir = tempfile::tempdir().unwrap();
        let db_path = temp_dir.path().join("registry");

        let registry = LocalRegistry::new(db_path).await.unwrap();

        let package = Package::builder(
            PackageId::new("test", "example"),
            Version::new(1, 0, 0),
        )
        .title("Example Package")
        .description("A test package")
        .license("MIT")
        .content_id(ContentId::new("abc123", HashAlgorithm::Sha256))
        .build()
        .unwrap();

        registry.add_package(package).await.unwrap();

        let results = registry.search(&Query::new("example")).await.unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id.name, "example");
    }
}
