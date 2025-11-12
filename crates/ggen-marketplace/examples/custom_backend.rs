//! Example of implementing custom storage and registry backends
//!
//! This example demonstrates:
//! - Implementing the Registry trait
//! - Implementing the PackageStore trait
//! - Using custom backends with the marketplace
//!
//! NOTE: This example is currently disabled due to API changes.
//! It will be updated to match the current trait API.

#![cfg(never)] // Disabled until updated for current API

use anyhow::Result;
use async_trait::async_trait;
use bytes::Bytes;
use ggen_marketplace::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// In-memory registry implementation for testing/development
pub struct InMemoryRegistry {
    packages: Arc<RwLock<HashMap<PackageId, PackageMetadata>>>,
    versions: Arc<RwLock<HashMap<String, Vec<semver::Version>>>>,
}

impl InMemoryRegistry {
    pub fn new() -> Self {
        Self {
            packages: Arc::new(RwLock::new(HashMap::new())),
            versions: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

#[async_trait]
impl Registry for InMemoryRegistry {
    async fn register(
        &self, metadata: &PackageMetadata, _content_hash: ContentHash,
    ) -> Result<PackageId> {
        let mut packages = self.packages.write().await;
        let mut versions = self.versions.write().await;

        // Check if package already exists
        if packages.contains_key(&metadata.id) {
            return Err(anyhow::anyhow!("Package version already exists"));
        }

        // Register package
        packages.insert(metadata.id.clone(), metadata.clone());

        // Update versions list
        versions
            .entry(metadata.id.name.clone())
            .or_insert_with(Vec::new)
            .push(metadata.id.version.clone());

        Ok(metadata.id.clone())
    }

    async fn get(&self, id: &PackageId) -> Result<PackageMetadata> {
        let packages = self.packages.read().await;

        packages
            .get(id)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Package not found"))
    }

    async fn list_versions(
        &self, name: &str, _namespace: Option<&str>,
    ) -> Result<Vec<semver::Version>> {
        let versions = self.versions.read().await;

        let mut result = versions.get(name).cloned().unwrap_or_default();

        result.sort();
        result.reverse(); // Latest first

        Ok(result)
    }

    async fn resolve_dependencies(
        &self, id: &PackageId, transitive: bool,
    ) -> Result<DependencyGraph> {
        let metadata = self.get(id).await?;

        let mut dependencies = Vec::new();

        if transitive {
            for (dep_name, dep_req) in &metadata.dependencies {
                if let Some(dep_version) = self.resolve_version(dep_name, dep_req).await? {
                    let dep_graph = self.resolve_dependencies(&dep_version, true).await?;
                    dependencies.push(dep_graph);
                }
            }
        }

        Ok(DependencyGraph {
            package: id.clone(),
            dependencies,
        })
    }

    async fn resolve_version(
        &self, name: &str, req: &semver::VersionReq,
    ) -> Result<Option<PackageId>> {
        let versions = self.list_versions(name, None).await?;

        for version in versions {
            if req.matches(&version) {
                return Ok(Some(PackageId {
                    name: name.to_string(),
                    version,
                    namespace: None,
                }));
            }
        }

        Ok(None)
    }

    async fn update_metadata(&self, id: &PackageId, updates: MetadataUpdates) -> Result<()> {
        let mut packages = self.packages.write().await;

        let metadata = packages
            .get_mut(id)
            .ok_or_else(|| anyhow::anyhow!("Package not found"))?;

        // Apply updates
        if let Some(description) = updates.description {
            metadata.description = description;
        }
        if let Some(keywords) = updates.keywords {
            metadata.keywords = keywords;
        }

        Ok(())
    }

    async fn yank(&self, id: &PackageId, _reason: Option<&str>) -> Result<()> {
        // For in-memory implementation, we just verify the package exists
        let packages = self.packages.read().await;
        if !packages.contains_key(id) {
            return Err(anyhow::anyhow!("Package not found"));
        }

        // In a real implementation, we'd mark the package as yanked
        Ok(())
    }

    async fn unyank(&self, id: &PackageId) -> Result<()> {
        let packages = self.packages.read().await;
        if !packages.contains_key(id) {
            return Err(anyhow::anyhow!("Package not found"));
        }

        Ok(())
    }

    async fn is_yanked(&self, _id: &PackageId) -> Result<bool> {
        // In-memory implementation doesn't track yanked status
        Ok(false)
    }

    async fn list_by_author(
        &self, author: &str, limit: usize, offset: usize,
    ) -> Result<Vec<PackageMetadata>> {
        let packages = self.packages.read().await;

        let mut results: Vec<_> = packages
            .values()
            .filter(|pkg| pkg.authors.iter().any(|a| a.contains(author)))
            .cloned()
            .collect();

        results.sort_by(|a, b| b.published_at.cmp(&a.published_at));

        Ok(results.into_iter().skip(offset).take(limit).collect())
    }

    async fn get_stats(&self, _id: &PackageId) -> Result<PackageStats> {
        // Return dummy stats for in-memory implementation
        Ok(PackageStats {
            total_downloads: 0,
            downloads_last_30_days: 0,
            average_rating: 0.0,
            rating_count: 0,
        })
    }

    async fn record_download(&self, _id: &PackageId) -> Result<()> {
        // No-op for in-memory implementation
        Ok(())
    }
}

/// In-memory package store implementation
pub struct InMemoryPackageStore {
    storage: Arc<RwLock<HashMap<ContentHash, Bytes>>>,
}

impl InMemoryPackageStore {
    pub fn new() -> Self {
        Self {
            storage: Arc::new(RwLock::new(HashMap::new())),
        }
    }
}

#[async_trait]
impl PackageStore for InMemoryPackageStore {
    async fn store(&self, package_bytes: Bytes) -> Result<ContentHash> {
        let hash = ContentHash::sha256(&package_bytes);
        let mut storage = self.storage.write().await;
        storage.insert(hash.clone(), package_bytes);
        Ok(hash)
    }

    async fn get(&self, hash: &ContentHash) -> Result<Bytes> {
        let storage = self.storage.read().await;
        storage
            .get(hash)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Package not found"))
    }

    async fn stream(&self, hash: &ContentHash) -> Result<BoxStream<Result<Bytes>>> {
        // For in-memory store, just return the whole content as a single chunk
        let bytes = self.get(hash).await?;
        Ok(Box::pin(futures::stream::once(async move { Ok(bytes) })))
    }

    async fn upload_stream(
        &self, mut reader: impl tokio::io::AsyncRead + Unpin + Send, _size_hint: Option<u64>,
    ) -> Result<ContentHash> {
        use tokio::io::AsyncReadExt;

        let mut buffer = Vec::new();
        reader.read_to_end(&mut buffer).await?;

        self.store(Bytes::from(buffer)).await
    }

    async fn exists(&self, hash: &ContentHash) -> Result<bool> {
        let storage = self.storage.read().await;
        Ok(storage.contains_key(hash))
    }

    async fn delete(&self, hash: &ContentHash) -> Result<()> {
        let mut storage = self.storage.write().await;
        storage
            .remove(hash)
            .ok_or_else(|| anyhow::anyhow!("Package not found"))?;
        Ok(())
    }

    async fn metadata(&self, hash: &ContentHash) -> Result<StorageMetadata> {
        let storage = self.storage.read().await;
        let bytes = storage
            .get(hash)
            .ok_or_else(|| anyhow::anyhow!("Package not found"))?;

        Ok(StorageMetadata {
            size: bytes.len() as u64,
            created_at: chrono::Utc::now(),
            modified_at: chrono::Utc::now(),
        })
    }

    async fn copy_to(
        &self, hash: &ContentHash, destination: &dyn PackageStore,
    ) -> Result<ContentHash> {
        let bytes = self.get(hash).await?;
        destination.store(bytes).await
    }

    async fn list(&self, prefix: Option<&str>, limit: usize) -> Result<Vec<ContentHash>> {
        let storage = self.storage.read().await;

        let mut hashes: Vec<_> = storage
            .keys()
            .filter(|hash| {
                if let Some(p) = prefix {
                    hash.to_string().starts_with(p)
                } else {
                    true
                }
            })
            .cloned()
            .take(limit)
            .collect();

        hashes.sort();

        Ok(hashes)
    }

    async fn stats(&self) -> Result<StorageStats> {
        let storage = self.storage.read().await;

        let total_size: usize = storage.values().map(|b| b.len()).sum();

        Ok(StorageStats {
            total_size: total_size as u64,
            package_count: storage.len() as u64,
            utilization_percent: 0.0, // Not applicable for in-memory
        })
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== Custom Backend Example ===\n");

    // Create marketplace with custom backends
    let marketplace = MarketplaceBuilder::new()
        .registry(InMemoryRegistry::new())
        .storage(InMemoryPackageStore::new())
        .search(MemorySearchEngine::new()) // Assuming this exists
        .crypto(Ed25519CryptoVerifier::new())
        .build()
        .await?;

    println!("✓ Marketplace initialized with custom backends\n");

    // Use the marketplace with custom backends
    let package_metadata = PackageMetadata {
        id: PackageId {
            name: "custom-backend-demo".to_string(),
            version: semver::Version::parse("1.0.0")?,
            namespace: None,
        },
        description: "Demo package using custom backends".to_string(),
        authors: vec!["demo@example.com".to_string()],
        license: "MIT".to_string(),
        repository: None,
        homepage: None,
        keywords: vec!["demo".to_string()],
        categories: vec!["examples".to_string()],
        dependencies: std::collections::HashMap::new(),
        checksums: Checksums::default(),
        signature: Signature::default(),
        published_at: chrono::Utc::now(),
    };

    let package_bytes = Bytes::from_static(b"Custom backend content");

    let package_id = marketplace
        .publish(&package_metadata, package_bytes)
        .await?;
    println!(
        "✓ Published package: {} v{}",
        package_id.name, package_id.version
    );

    // Retrieve the package
    let retrieved = marketplace.download(&package_id).await?;
    println!("✓ Retrieved {} bytes", retrieved.len());

    // List versions
    let versions = marketplace
        .list_versions("custom-backend-demo", None)
        .await?;
    println!("✓ Found {} versions", versions.len());

    println!("\n✓ Custom backend example completed successfully!");

    Ok(())
}
