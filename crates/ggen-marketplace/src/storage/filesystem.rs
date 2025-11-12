use crate::error::{MarketplaceError, Result};
use crate::models::ContentId;
use crate::traits::{ContentMetadata, PackageStore};
use async_trait::async_trait;
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use tokio::io::AsyncReadExt;

/// Filesystem-based package storage
///
/// Stores package content on the local filesystem using content-addressable
/// storage (CAS) where each file is stored by its SHA-256 hash.
///
/// # Layout
///
/// ```text
/// storage_root/
/// ├── objects/
/// │   ├── ab/
/// │   │   └── cdef123... (content file)
/// │   └── 12/
/// │       └── 3456789... (content file)
/// └── metadata/
///     └── package_metadata.json
/// ```
pub struct FilesystemStore {
    root_path: PathBuf,
}

impl FilesystemStore {
    /// Create a new filesystem store
    ///
    /// # Arguments
    ///
    /// * `root_path` - Root directory for package storage
    ///
    /// # Errors
    ///
    /// Returns an error if the directory cannot be created.
    pub async fn new(root_path: PathBuf) -> Result<Self> {
        // Create root directory structure
        let objects_path = root_path.join("objects");
        let metadata_path = root_path.join("metadata");

        tokio::fs::create_dir_all(&objects_path)
            .await
            .map_err(|e| MarketplaceError::io_error("create_dir_all", e))?;

        tokio::fs::create_dir_all(&metadata_path)
            .await
            .map_err(|e| MarketplaceError::io_error("create_dir_all", e))?;

        Ok(Self { root_path })
    }

    /// Get the path for a content ID using sharding
    ///
    /// Uses the first 2 characters of the hash as a subdirectory for sharding.
    fn content_path(&self, id: &ContentId) -> PathBuf {
        let hash = &id.hash;
        let (prefix, suffix) = hash.split_at(2.min(hash.len()));

        self.root_path.join("objects").join(prefix).join(suffix)
    }

    /// Calculate SHA-256 hash of content
    fn calculate_hash(content: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content);
        hex::encode(hasher.finalize())
    }

    /// Get metadata path for a content ID
    fn metadata_path(&self, id: &ContentId) -> PathBuf {
        self.root_path
            .join("metadata")
            .join(format!("{}.json", id.hash))
    }

    /// Store content metadata
    async fn store_metadata(&self, id: &ContentId, metadata: &ContentMetadata) -> Result<()> {
        let path = self.metadata_path(id);

        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(|e| MarketplaceError::io_error("create_dir_all", e))?;
        }

        let json = serde_json::to_string_pretty(metadata)
            .map_err(|e| MarketplaceError::serialize_error(format!("content metadata: {}", e)))?;

        tokio::fs::write(&path, json)
            .await
            .map_err(|e| MarketplaceError::io_error("write", e))?;

        Ok(())
    }

    /// Load content metadata
    async fn load_metadata(&self, id: &ContentId) -> Result<ContentMetadata> {
        let path = self.metadata_path(id);

        let json = tokio::fs::read_to_string(&path).await.map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                MarketplaceError::not_found(format!("content metadata {}", id))
            } else {
                MarketplaceError::io_error("read_to_string", e)
            }
        })?;

        serde_json::from_str(&json)
            .map_err(|e| MarketplaceError::parse_error(format!("content metadata: {}", e)))
    }
}

#[async_trait]
impl PackageStore for FilesystemStore {
    async fn store(&self, content: &[u8]) -> Result<ContentId> {
        let hash = Self::calculate_hash(content);
        let id = ContentId::new(hash, crate::models::HashAlgorithm::Sha256);

        let path = self.content_path(&id);

        // Create parent directory if it doesn't exist
        if let Some(parent) = path.parent() {
            tokio::fs::create_dir_all(parent)
                .await
                .map_err(|e| MarketplaceError::io_error("create_dir_all", e))?;
        }

        // Write content to file
        tokio::fs::write(&path, content)
            .await
            .map_err(|e| MarketplaceError::io_error("write", e))?;

        // Store metadata
        let metadata = ContentMetadata {
            size: content.len() as u64,
            content_type: None,
            created_at: chrono::Utc::now(),
        };

        self.store_metadata(&id, &metadata).await?;

        Ok(id)
    }

    async fn retrieve(&self, id: &ContentId) -> Result<Vec<u8>> {
        let path = self.content_path(id);

        tokio::fs::read(&path).await.map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                MarketplaceError::package_not_found(id.to_string(), "content not found")
            } else {
                MarketplaceError::io_error("read", e)
            }
        })
    }

    async fn exists(&self, id: &ContentId) -> Result<bool> {
        let path = self.content_path(id);
        Ok(path.exists())
    }

    async fn delete(&self, id: &ContentId) -> Result<()> {
        let path = self.content_path(id);

        tokio::fs::remove_file(&path).await.map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                MarketplaceError::package_not_found(id.to_string(), "content not found")
            } else {
                MarketplaceError::io_error("remove_file", e)
            }
        })?;

        // Also delete metadata
        let metadata_path = self.metadata_path(id);
        let _ = tokio::fs::remove_file(&metadata_path).await;

        Ok(())
    }

    async fn metadata(&self, id: &ContentId) -> Result<ContentMetadata> {
        self.load_metadata(id).await
    }

    async fn store_stream(
        &self, mut stream: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    ) -> Result<ContentId> {
        // Read entire stream into memory
        // FUTURE: Implement true streaming with temp file and hash calculation
        let mut buffer = Vec::new();
        stream
            .read_to_end(&mut buffer)
            .await
            .map_err(|e| MarketplaceError::io_error("stream read", e))?;

        self.store(&buffer).await
    }

    async fn retrieve_stream(
        &self, id: &ContentId,
    ) -> Result<Box<dyn tokio::io::AsyncRead + Send + Unpin>> {
        let path = self.content_path(id);

        let file = tokio::fs::File::open(&path).await.map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                MarketplaceError::package_not_found(id.to_string(), "content not found")
            } else {
                MarketplaceError::io_error("open", e)
            }
        })?;

        Ok(Box::new(file))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_filesystem_store() {
        let temp_dir = tempfile::tempdir().unwrap();
        let store = FilesystemStore::new(temp_dir.path().to_path_buf())
            .await
            .unwrap();

        let content = b"Hello, World!";
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
    async fn test_content_hash_deterministic() {
        let temp_dir = tempfile::tempdir().unwrap();
        let store = FilesystemStore::new(temp_dir.path().to_path_buf())
            .await
            .unwrap();

        let content = b"Same content";

        let id1 = store.store(content).await.unwrap();
        let id2 = store.store(content).await.unwrap();

        assert_eq!(id1.hash, id2.hash);
    }
}
