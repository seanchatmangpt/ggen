use crate::error::Result;
use crate::models::ContentId;
use async_trait::async_trait;

/// Extended storage trait with additional functionality
#[async_trait]
pub trait PackageStoreExt: super::PackageStore {
    /// Copy content to another storage location
    async fn copy(&self, source: &ContentId, destination: &ContentId) -> Result<()>;

    /// List all stored content IDs
    async fn list_all(&self) -> Result<Vec<ContentId>>;

    /// Get storage statistics
    async fn storage_stats(&self) -> Result<StorageStats>;

    /// Verify content integrity
    async fn verify_integrity(&self, id: &ContentId) -> Result<bool>;

    /// Compact storage (remove unused content)
    async fn compact(&self) -> Result<CompactionStats>;

    /// Create a snapshot of current storage state
    async fn create_snapshot(&self) -> Result<String>;

    /// Restore from a snapshot
    async fn restore_snapshot(&self, snapshot_id: &str) -> Result<()>;
}

/// Storage statistics
#[derive(Debug, Clone)]
pub struct StorageStats {
    pub total_size_bytes: u64,
    pub total_items: usize,
    pub available_space_bytes: Option<u64>,
    pub fragmentation_percent: f64,
}

/// Compaction statistics
#[derive(Debug, Clone)]
pub struct CompactionStats {
    pub items_removed: usize,
    pub bytes_freed: u64,
    pub duration_ms: u64,
}

/// Content caching trait for performance optimization
#[async_trait]
pub trait ContentCache: Send + Sync {
    /// Get content from cache
    async fn get(&self, id: &ContentId) -> Result<Option<Vec<u8>>>;

    /// Put content in cache
    async fn put(&self, id: &ContentId, content: Vec<u8>) -> Result<()>;

    /// Remove content from cache
    async fn remove(&self, id: &ContentId) -> Result<()>;

    /// Clear entire cache
    async fn clear(&self) -> Result<()>;

    /// Get cache statistics
    async fn stats(&self) -> Result<CacheStats>;
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub total_entries: usize,
    pub total_size_bytes: u64,
    pub hit_count: u64,
    pub miss_count: u64,
    pub hit_rate: f64,
}
