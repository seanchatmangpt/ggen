use crate::error::{MarketplaceError, Result};
use crate::models::ContentId;
use crate::traits::{ContentMetadata, PackageStore};
use async_trait::async_trait;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::io::AsyncReadExt;
use tokio::sync::RwLock;

/// In-memory package storage (for testing and caching)
///
/// Stores package content in memory. Useful for testing and as a cache layer.
pub struct MemoryStore {
    storage: Arc<RwLock<HashMap<ContentId, Vec<u8>>>>,
    metadata: Arc<RwLock<HashMap<ContentId, ContentMetadata>>>,
}

impl MemoryStore {
    /// Create a new memory store
    pub fn new() -> Self {
        Self {
            storage: Arc::new(RwLock::new(HashMap::new())),
            metadata: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Calculate SHA-256 hash of content
    fn calculate_hash(content: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content);
        hex::encode(hasher.finalize())
    }
}

impl Default for MemoryStore {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl PackageStore for MemoryStore {
    async fn store(&self, content: &[u8]) -> Result<ContentId> {
        let hash = Self::calculate_hash(content);
        let id = ContentId::new(hash, crate::models::HashAlgorithm::Sha256);

        let mut storage = self.storage.write().await;
        storage.insert(id.clone(), content.to_vec());

        let mut metadata_map = self.metadata.write().await;
        metadata_map.insert(
            id.clone(),
            ContentMetadata {
                size: content.len() as u64,
                content_type: None,
                created_at: chrono::Utc::now(),
            },
        );

        Ok(id)
    }

    async fn retrieve(&self, id: &ContentId) -> Result<Vec<u8>> {
        let storage = self.storage.read().await;

        storage
            .get(id)
            .cloned()
            .ok_or_else(|| MarketplaceError::not_found("content", &id.to_string()))
    }

    async fn exists(&self, id: &ContentId) -> Result<bool> {
        let storage = self.storage.read().await;
        Ok(storage.contains_key(id))
    }

    async fn delete(&self, id: &ContentId) -> Result<()> {
        let mut storage = self.storage.write().await;

        if storage.remove(id).is_none() {
            return Err(MarketplaceError::not_found("content", &id.to_string()));
        }

        let mut metadata_map = self.metadata.write().await;
        metadata_map.remove(id);

        Ok(())
    }

    async fn metadata(&self, id: &ContentId) -> Result<ContentMetadata> {
        let metadata_map = self.metadata.read().await;

        metadata_map
            .get(id)
            .cloned()
            .ok_or_else(|| MarketplaceError::not_found("content metadata", &id.to_string()))
    }

    async fn store_stream(
        &self,
        mut stream: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    ) -> Result<ContentId> {
        let mut buffer = Vec::new();
        stream
            .read_to_end(&mut buffer)
            .await
            .map_err(|e| MarketplaceError::io_error(e.to_string(), "stream read"))?;

        self.store(&buffer).await
    }

    async fn retrieve_stream(
        &self,
        id: &ContentId,
    ) -> Result<Box<dyn tokio::io::AsyncRead + Send + Unpin>> {
        let content = self.retrieve(id).await?;
        Ok(Box::new(tokio::io::BufReader::new(std::io::Cursor::new(
            content,
        ))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_memory_store() {
        let store = MemoryStore::new();

        let content = b"Hello, Memory!";
        let id = store.store(content).await.unwrap();

        assert!(store.exists(&id).await.unwrap());

        let retrieved = store.retrieve(&id).await.unwrap();
        assert_eq!(retrieved, content);

        let metadata = store.metadata(&id).await.unwrap();
        assert_eq!(metadata.size, content.len() as u64);

        store.delete(&id).await.unwrap();
        assert!(!store.exists(&id).await.unwrap());
    }

    #[tokio::test]
    async fn test_content_deduplication() {
        let store = MemoryStore::new();

        let content = b"Same content";

        let id1 = store.store(content).await.unwrap();
        let id2 = store.store(content).await.unwrap();

        // Same content should produce same ID (content-addressable storage)
        assert_eq!(id1.hash, id2.hash);

        // But only one copy should be stored (implicit deduplication)
        let storage = store.storage.read().await;
        assert_eq!(storage.len(), 1);
    }
}
