//! Pack Registry for publishing and discovering packs
//!
//! This module provides a registry interface for pack publishing, search, and version management.

use crate::packs::repository::PackRepository;
use crate::packs::types::Pack;
use async_trait::async_trait;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::{info, warn};

/// Pack registry trait
#[async_trait]
pub trait PackRegistry: Send + Sync {
    /// Publish a pack to the registry
    async fn publish(&self, pack: &Pack, metadata: PublishMetadata) -> Result<PublishReceipt>;

    /// Unpublish a specific version
    async fn unpublish(&self, pack_id: &str, version: &str) -> Result<()>;

    /// Search for packs
    async fn search(&self, query: &SearchQuery) -> Result<Vec<Pack>>;

    /// Get all versions of a pack
    async fn get_versions(&self, pack_id: &str) -> Result<Vec<Version>>;
}

/// Publish metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishMetadata {
    /// Version being published
    pub version: String,
    /// Changelog for this version
    pub changelog: String,
    /// Tags for discovery
    pub tags: Vec<String>,
    /// Documentation URL
    pub documentation_url: Option<String>,
}

/// Publish receipt
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishReceipt {
    /// Pack ID
    pub pack_id: String,
    /// Published version
    pub version: String,
    /// Registry URL
    pub registry_url: String,
    /// Publication timestamp
    pub published_at: String,
}

/// Search query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchQuery {
    /// Text search query
    pub text: Option<String>,
    /// Category filter
    pub category: Option<String>,
    /// Tags filter
    pub tags: Vec<String>,
    /// Author filter
    pub author: Option<String>,
    /// Production ready only
    pub production_ready_only: bool,
    /// Maximum results
    pub limit: usize,
}

/// Version information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Version {
    /// Version number
    pub version: String,
    /// Publication date
    pub published_at: String,
    /// Is this the latest version
    pub is_latest: bool,
    /// Download count
    pub downloads: u64,
}

/// In-memory registry implementation (for testing/development)
pub struct InMemoryRegistry {
    #[allow(dead_code)]
    packs: HashMap<String, Vec<Pack>>,
    repository: Box<dyn PackRepository>,
}

impl InMemoryRegistry {
    /// Create new in-memory registry
    pub fn new(repository: Box<dyn PackRepository>) -> Self {
        Self {
            packs: HashMap::new(),
            repository,
        }
    }
}

#[async_trait]
impl PackRegistry for InMemoryRegistry {
    async fn publish(&self, pack: &Pack, metadata: PublishMetadata) -> Result<PublishReceipt> {
        info!("Publishing pack '{}' version {}", pack.id, metadata.version);

        // Validate pack
        if pack.id.is_empty() || pack.name.is_empty() {
            return Err(Error::new("Pack ID and name are required"));
        }

        // In production, this would:
        // 1. Validate license
        // 2. Check version uniqueness
        // 3. Upload to CDN
        // 4. Update registry index

        // Save to repository
        self.repository.save(pack).await?;

        Ok(PublishReceipt {
            pack_id: pack.id.clone(),
            version: metadata.version,
            registry_url: format!("https://registry.ggen.io/packs/{}", pack.id),
            published_at: chrono::Utc::now().to_rfc3339(),
        })
    }

    async fn unpublish(&self, pack_id: &str, version: &str) -> Result<()> {
        info!("Unpublishing pack '{}' version {}", pack_id, version);

        // In production, this would:
        // 1. Check permissions
        // 2. Remove from CDN
        // 3. Update registry index
        // 4. Notify users

        warn!("Unpublish functionality not yet fully implemented");

        Ok(())
    }

    async fn search(&self, query: &SearchQuery) -> Result<Vec<Pack>> {
        let mut results = self.repository.list(query.category.as_deref()).await?;

        // Apply filters
        if let Some(text) = &query.text {
            let text_lower = text.to_lowercase();
            results.retain(|p| {
                p.name.to_lowercase().contains(&text_lower)
                    || p.description.to_lowercase().contains(&text_lower)
            });
        }

        if !query.tags.is_empty() {
            results.retain(|p| query.tags.iter().any(|tag| p.tags.contains(tag)));
        }

        if let Some(author) = &query.author {
            results.retain(|p| p.author.as_ref() == Some(author));
        }

        if query.production_ready_only {
            results.retain(|p| p.production_ready);
        }

        // Limit results
        results.truncate(query.limit);

        Ok(results)
    }

    async fn get_versions(&self, pack_id: &str) -> Result<Vec<Version>> {
        // In production, this would query the registry for all versions
        // For now, return current version only

        let pack = self.repository.load(pack_id).await?;

        Ok(vec![Version {
            version: pack.version,
            published_at: chrono::Utc::now().to_rfc3339(),
            is_latest: true,
            downloads: 0,
        }])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::repository::FileSystemRepository;
    use crate::packs::types::PackMetadata;

    async fn create_test_registry() -> InMemoryRegistry {
        let temp_dir = tempfile::tempdir().unwrap();
        let repo = FileSystemRepository::new(temp_dir.path());
        InMemoryRegistry::new(Box::new(repo))
    }

    fn create_test_pack(id: &str) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Pack {}", id),
            version: "1.0.0".to_string(),
            description: "Test pack".to_string(),
            category: "test".to_string(),
            author: Some("Test Author".to_string()),
            repository: None,
            license: Some("MIT".to_string()),
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec!["test".to_string()],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[tokio::test]
    async fn test_publish_pack() {
        let registry = create_test_registry().await;
        let pack = create_test_pack("test-pack");

        let metadata = PublishMetadata {
            version: "1.0.0".to_string(),
            changelog: "Initial release".to_string(),
            tags: vec!["test".to_string()],
            documentation_url: None,
        };

        let result = registry.publish(&pack, metadata).await;
        assert!(result.is_ok());

        let receipt = result.unwrap();
        assert_eq!(receipt.pack_id, "test-pack");
        assert_eq!(receipt.version, "1.0.0");
    }

    #[tokio::test]
    async fn test_search_packs() {
        let registry = create_test_registry().await;

        // Publish a pack first
        let pack = create_test_pack("searchable-pack");
        let metadata = PublishMetadata {
            version: "1.0.0".to_string(),
            changelog: "Initial release".to_string(),
            tags: vec!["test".to_string()],
            documentation_url: None,
        };
        registry.publish(&pack, metadata).await.unwrap();

        // Search for it
        let query = SearchQuery {
            text: Some("searchable".to_string()),
            category: None,
            tags: vec![],
            author: None,
            production_ready_only: false,
            limit: 10,
        };

        let results = registry.search(&query).await.unwrap();
        assert!(!results.is_empty());
    }
}
