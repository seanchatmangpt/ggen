//! Core trait definitions for marketplace operations
//!
//! This module defines the core traits that form the foundation of the marketplace
//! architecture. These traits enable pluggable implementations of registry, storage,
//! search, and cryptographic operations.
//!
//! ## Trait Architecture
//!
//! The marketplace is built on four core traits:
//!
//! - **Registry**: Package discovery and management operations
//! - **PackageStore**: Content-addressable storage for package binaries
//! - **SearchEngine**: Full-text search with advanced querying
//! - **CryptoVerifier**: Cryptographic signature verification
//!
//! Each trait has extension traits in submodules that provide additional functionality
//! and convenience methods.

use crate::error::Result;
use crate::models::{ContentId, Package, PackageId, Query, SearchQuery, SearchResults, Signature};
use async_trait::async_trait;

pub mod crypto;
pub mod registry;
pub mod search;
pub mod storage;

pub use crypto::CryptoVerifierExt;
pub use registry::RegistryExt;
pub use search::SearchEngineExt;
pub use storage::PackageStoreExt;

/// Trait for package discovery and management
#[async_trait]
pub trait Registry: Send + Sync {
    /// Search for packages matching the query
    async fn search(&self, query: &Query) -> Result<Vec<Package>>;

    /// Retrieve a specific package by ID
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Get a specific version of a package
    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package>;

    /// List all versions of a package
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>>;

    /// Publish a new package or version
    async fn publish(&self, package: Package) -> Result<()>;

    /// Delete a package version (if allowed)
    async fn delete(&self, id: &PackageId, version: &str) -> Result<()>;

    /// Check if a package exists
    async fn exists(&self, id: &PackageId) -> Result<bool>;

    /// Get registry metadata and capabilities
    async fn metadata(&self) -> Result<crate::models::RegistryMetadata>;
}

/// Trait for content storage and retrieval
#[async_trait]
pub trait PackageStore: Send + Sync {
    /// Store package content and return content-addressable ID
    async fn store(&self, content: &[u8]) -> Result<ContentId>;

    /// Retrieve package content by ID
    async fn retrieve(&self, id: &ContentId) -> Result<Vec<u8>>;

    /// Check if content exists
    async fn exists(&self, id: &ContentId) -> Result<bool>;

    /// Delete content (if allowed)
    async fn delete(&self, id: &ContentId) -> Result<()>;

    /// Get content metadata (size, hash, etc.)
    async fn metadata(&self, id: &ContentId) -> Result<ContentMetadata>;

    /// Store with streaming for large files
    async fn store_stream(
        &self, stream: Box<dyn tokio::io::AsyncRead + Send + Unpin>,
    ) -> Result<ContentId>;

    /// Retrieve with streaming for large files
    async fn retrieve_stream(
        &self, id: &ContentId,
    ) -> Result<Box<dyn tokio::io::AsyncRead + Send + Unpin>>;
}

/// Metadata about stored content
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ContentMetadata {
    pub size: u64,
    pub content_type: Option<String>,
    pub created_at: chrono::DateTime<chrono::Utc>,
}

/// Trait for advanced search functionality
#[async_trait]
pub trait SearchEngine: Send + Sync {
    /// Index a package for search
    async fn index(&self, package: &Package) -> Result<()>;

    /// Remove a package from the search index
    async fn remove(&self, id: &PackageId) -> Result<()>;

    /// Perform an advanced search with filters and facets
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults>;

    /// Update the index for a package
    async fn update(&self, package: &Package) -> Result<()>;

    /// Rebuild the entire search index
    async fn rebuild_index(&self) -> Result<()>;

    /// Get search statistics
    async fn stats(&self) -> Result<SearchStats>;
}

/// Search engine statistics
#[derive(Debug, Clone)]
pub struct SearchStats {
    pub total_documents: usize,
    pub index_size_bytes: u64,
    pub last_updated: chrono::DateTime<chrono::Utc>,
}

/// Trait for cryptographic operations
#[async_trait]
pub trait CryptoVerifier: Send + Sync {
    /// Sign content with a private key
    fn sign(&self, content: &[u8]) -> Result<Signature>;

    /// Verify a signature against content
    fn verify(&self, content: &[u8], signature: &Signature) -> Result<bool>;

    /// Generate a new key pair
    fn generate_keypair(&self) -> Result<crate::models::signature::KeyPair>;

    /// Import a public key for verification
    fn import_public_key(&self, pem: &str) -> Result<crate::models::PublicKey>;

    /// Export a public key as PEM
    fn export_public_key(&self, key: &crate::models::PublicKey) -> Result<String>;

    /// Compute a content hash
    fn hash_content(&self, content: &[u8]) -> Result<String>;
}
