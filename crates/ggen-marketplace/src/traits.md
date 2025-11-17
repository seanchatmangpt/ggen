# Ggen-Marketplace Core Traits

This document provides comprehensive documentation for all core traits in the ggen-marketplace library.

## Table of Contents

1. [Registry Trait](#registry-trait)
2. [PackageStore Trait](#packagestore-trait)
3. [SearchEngine Trait](#searchengine-trait)
4. [CryptoVerifier Trait](#cryptoverifier-trait)
5. [MetricsCollector Trait](#metricscollector-trait)
6. [Replication Trait](#replication-trait-optional)

---

## Registry Trait

The `Registry` trait manages package metadata and version information. It is the source of truth for what packages exist and their metadata.

### Trait Definition

```rust
use async_trait::async_trait;
use semver::{Version, VersionReq};
use chrono::{DateTime, Utc};

#[async_trait]
pub trait Registry: Send + Sync {
    /// Register a new package or update existing package metadata
    ///
    /// # Arguments
    /// * `metadata` - Package metadata to register
    /// * `content_hash` - Hash of the package content for verification
    ///
    /// # Returns
    /// * `PackageId` - Unique identifier for the registered package
    ///
    /// # Errors
    /// * `RegistryError::Conflict` - Package version already exists
    /// * `RegistryError::Validation` - Invalid metadata
    async fn register(
        &self,
        metadata: &PackageMetadata,
        content_hash: ContentHash,
    ) -> Result<PackageId>;

    /// Retrieve package metadata by ID
    ///
    /// # Arguments
    /// * `id` - Package identifier
    ///
    /// # Returns
    /// * `PackageMetadata` - Full package metadata
    ///
    /// # Errors
    /// * `RegistryError::NotFound` - Package does not exist
    async fn get(&self, id: &PackageId) -> Result<PackageMetadata>;

    /// List all versions of a package
    ///
    /// # Arguments
    /// * `name` - Package name
    /// * `namespace` - Optional namespace
    ///
    /// # Returns
    /// * `Vec<Version>` - Sorted list of available versions
    async fn list_versions(
        &self,
        name: &str,
        namespace: Option<&str>,
    ) -> Result<Vec<Version>>;

    /// Resolve dependencies for a package
    ///
    /// # Arguments
    /// * `id` - Package identifier
    /// * `transitive` - If true, resolve transitive dependencies
    ///
    /// # Returns
    /// * `DependencyGraph` - Resolved dependency graph
    ///
    /// # Errors
    /// * `RegistryError::DependencyConflict` - Unresolvable dependency conflict
    async fn resolve_dependencies(
        &self,
        id: &PackageId,
        transitive: bool,
    ) -> Result<DependencyGraph>;

    /// Find the latest version matching a version requirement
    ///
    /// # Arguments
    /// * `name` - Package name
    /// * `req` - Semantic version requirement (e.g., "^1.0.0")
    ///
    /// # Returns
    /// * `Option<PackageId>` - Latest matching version, or None if no match
    async fn resolve_version(
        &self,
        name: &str,
        req: &VersionReq,
    ) -> Result<Option<PackageId>>;

    /// Update package metadata (excluding version)
    ///
    /// # Arguments
    /// * `id` - Package identifier
    /// * `updates` - Metadata fields to update
    ///
    /// # Errors
    /// * `RegistryError::NotFound` - Package does not exist
    /// * `RegistryError::Unauthorized` - Caller lacks permission
    async fn update_metadata(
        &self,
        id: &PackageId,
        updates: MetadataUpdates,
    ) -> Result<()>;

    /// Mark a package version as yanked (unpublished)
    ///
    /// Yanked packages are not returned in searches or dependency resolution,
    /// but existing installations continue to work.
    ///
    /// # Arguments
    /// * `id` - Package identifier
    /// * `reason` - Optional reason for yanking
    async fn yank(&self, id: &PackageId, reason: Option<&str>) -> Result<()>;

    /// Reverse a yank operation
    ///
    /// # Arguments
    /// * `id` - Package identifier
    async fn unyank(&self, id: &PackageId) -> Result<()>;

    /// Check if a package version is yanked
    ///
    /// # Arguments
    /// * `id` - Package identifier
    ///
    /// # Returns
    /// * `bool` - True if yanked
    async fn is_yanked(&self, id: &PackageId) -> Result<bool>;

    /// List all packages by author
    ///
    /// # Arguments
    /// * `author` - Author name or email
    /// * `limit` - Maximum number of results
    /// * `offset` - Pagination offset
    ///
    /// # Returns
    /// * `Vec<PackageMetadata>` - List of packages
    async fn list_by_author(
        &self,
        author: &str,
        limit: usize,
        offset: usize,
    ) -> Result<Vec<PackageMetadata>>;

    /// Get package download statistics
    ///
    /// # Arguments
    /// * `id` - Package identifier
    ///
    /// # Returns
    /// * `PackageStats` - Download counts and trends
    async fn get_stats(&self, id: &PackageId) -> Result<PackageStats>;

    /// Record a package download (for statistics)
    ///
    /// # Arguments
    /// * `id` - Package identifier
    async fn record_download(&self, id: &PackageId) -> Result<()>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Create a local registry backed by SQLite
    let registry = LocalRegistry::new("./registry.db").await?;

    // Register a new package
    let metadata = PackageMetadata {
        id: PackageId {
            name: "web-framework".to_string(),
            version: Version::parse("1.0.0")?,
            namespace: Some("acme".to_string()),
        },
        description: "A fast web framework".to_string(),
        authors: vec!["alice@example.com".to_string()],
        license: "MIT".to_string(),
        repository: Some("https://github.com/acme/framework".parse()?),
        homepage: None,
        keywords: vec!["web".to_string(), "framework".to_string()],
        categories: vec!["web".to_string()],
        dependencies: HashMap::new(),
        checksums: Checksums::default(),
        signature: Signature::default(),
        published_at: Utc::now(),
    };

    let content_hash = ContentHash::sha256(&package_bytes);
    let id = registry.register(&metadata, content_hash).await?;
    println!("Registered package: {:?}", id);

    // Resolve dependencies
    let deps = registry.resolve_dependencies(&id, true).await?;
    println!("Dependencies: {:?}", deps);

    // Get statistics
    let stats = registry.get_stats(&id).await?;
    println!("Downloads: {}", stats.total_downloads);

    Ok(())
}
```

---

## PackageStore Trait

The `PackageStore` trait handles binary package storage and retrieval. It abstracts over different storage backends (filesystem, S3, IPFS, etc.).

### Trait Definition

```rust
use bytes::Bytes;
use tokio::io::{AsyncRead, AsyncWrite};
use futures::stream::Stream;

#[async_trait]
pub trait PackageStore: Send + Sync {
    /// Store a package and return its content hash
    ///
    /// # Arguments
    /// * `package_bytes` - Raw package data
    ///
    /// # Returns
    /// * `ContentHash` - Content-addressed hash for retrieval
    async fn store(&self, package_bytes: Bytes) -> Result<ContentHash>;

    /// Retrieve a package by its content hash
    ///
    /// # Arguments
    /// * `hash` - Content hash
    ///
    /// # Returns
    /// * `Bytes` - Package data
    ///
    /// # Errors
    /// * `StorageError::NotFound` - Package does not exist
    async fn get(&self, hash: &ContentHash) -> Result<Bytes>;

    /// Stream a large package without loading it entirely into memory
    ///
    /// # Arguments
    /// * `hash` - Content hash
    ///
    /// # Returns
    /// * `BoxStream<Result<Bytes>>` - Async stream of chunks
    async fn stream(&self, hash: &ContentHash) -> Result<BoxStream<Result<Bytes>>>;

    /// Upload a package from an async reader
    ///
    /// # Arguments
    /// * `reader` - Async reader providing package data
    /// * `size_hint` - Optional expected size for optimization
    ///
    /// # Returns
    /// * `ContentHash` - Content-addressed hash
    async fn upload_stream(
        &self,
        reader: impl AsyncRead + Unpin + Send,
        size_hint: Option<u64>,
    ) -> Result<ContentHash>;

    /// Check if a package exists in storage
    ///
    /// # Arguments
    /// * `hash` - Content hash to check
    ///
    /// # Returns
    /// * `bool` - True if package exists
    async fn exists(&self, hash: &ContentHash) -> Result<bool>;

    /// Delete a package from storage
    ///
    /// # Arguments
    /// * `hash` - Content hash
    ///
    /// # Errors
    /// * `StorageError::NotFound` - Package does not exist
    /// * `StorageError::PermissionDenied` - Caller lacks permission
    async fn delete(&self, hash: &ContentHash) -> Result<()>;

    /// Get storage metadata for a package
    ///
    /// # Arguments
    /// * `hash` - Content hash
    ///
    /// # Returns
    /// * `StorageMetadata` - Size, timestamps, etc.
    async fn metadata(&self, hash: &ContentHash) -> Result<StorageMetadata>;

    /// Copy a package to another storage backend
    ///
    /// # Arguments
    /// * `hash` - Content hash
    /// * `destination` - Target storage backend
    ///
    /// # Returns
    /// * `ContentHash` - Hash in destination storage (usually same)
    async fn copy_to(
        &self,
        hash: &ContentHash,
        destination: &dyn PackageStore,
    ) -> Result<ContentHash>;

    /// List all stored packages (for maintenance/backup)
    ///
    /// # Arguments
    /// * `prefix` - Optional hash prefix filter
    /// * `limit` - Maximum number of results
    ///
    /// # Returns
    /// * `Vec<ContentHash>` - List of stored package hashes
    async fn list(
        &self,
        prefix: Option<&str>,
        limit: usize,
    ) -> Result<Vec<ContentHash>>;

    /// Get storage statistics
    ///
    /// # Returns
    /// * `StorageStats` - Total size, package count, etc.
    async fn stats(&self) -> Result<StorageStats>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;
use tokio::fs::File;

#[tokio::main]
async fn main() -> Result<()> {
    // File-based storage
    let file_store = FilePackageStore::new("./packages").await?;

    // Store a package
    let package_bytes = Bytes::from_static(b"package content");
    let hash = file_store.store(package_bytes).await?;
    println!("Stored with hash: {}", hash);

    // Retrieve the package
    let retrieved = file_store.get(&hash).await?;
    assert_eq!(retrieved, b"package content");

    // Stream a large file
    let mut file = File::open("large-package.tar.gz").await?;
    let hash = file_store.upload_stream(file, None).await?;

    // Stream download
    let mut stream = file_store.stream(&hash).await?;
    while let Some(chunk) = stream.next().await {
        let chunk = chunk?;
        // Process chunk...
    }

    // S3-based storage (with feature flag)
    #[cfg(feature = "s3-storage")]
    {
        let s3_store = S3PackageStore::new(
            "my-bucket",
            "us-east-1",
            aws_credentials,
        ).await?;

        // Copy between storage backends
        file_store.copy_to(&hash, &s3_store).await?;
    }

    Ok(())
}
```

---

## SearchEngine Trait

The `SearchEngine` trait provides fast package discovery through full-text search and filtering.

### Trait Definition

```rust
#[async_trait]
pub trait SearchEngine: Send + Sync {
    /// Index a package for search
    ///
    /// # Arguments
    /// * `metadata` - Package metadata to index
    ///
    /// # Errors
    /// * `SearchError::IndexingFailed` - Failed to index package
    async fn index(&self, metadata: &PackageMetadata) -> Result<()>;

    /// Remove a package from the search index
    ///
    /// # Arguments
    /// * `id` - Package identifier
    async fn remove(&self, id: &PackageId) -> Result<()>;

    /// Update an existing index entry
    ///
    /// # Arguments
    /// * `metadata` - Updated package metadata
    async fn update(&self, metadata: &PackageMetadata) -> Result<()>;

    /// Search for packages
    ///
    /// # Arguments
    /// * `query` - Search query with filters
    ///
    /// # Returns
    /// * `Vec<SearchResult>` - Ranked search results with highlights
    async fn search(&self, query: &SearchQuery) -> Result<Vec<SearchResult>>;

    /// Suggest package names based on partial input (autocomplete)
    ///
    /// # Arguments
    /// * `prefix` - Partial package name
    /// * `limit` - Maximum number of suggestions
    ///
    /// # Returns
    /// * `Vec<String>` - Suggested package names
    async fn suggest(&self, prefix: &str, limit: usize) -> Result<Vec<String>>;

    /// Get faceted search results (counts by category/tag)
    ///
    /// # Arguments
    /// * `query` - Search query
    ///
    /// # Returns
    /// * `Facets` - Counts for each category, tag, etc.
    async fn facets(&self, query: &SearchQuery) -> Result<Facets>;

    /// Perform a more-like-this search (find similar packages)
    ///
    /// # Arguments
    /// * `id` - Reference package identifier
    /// * `limit` - Maximum number of results
    ///
    /// # Returns
    /// * `Vec<SearchResult>` - Similar packages
    async fn similar(&self, id: &PackageId, limit: usize) -> Result<Vec<SearchResult>>;

    /// Rebuild the entire search index
    ///
    /// This is a maintenance operation typically run offline.
    ///
    /// # Arguments
    /// * `packages` - Iterator of all packages to index
    async fn rebuild(
        &self,
        packages: impl Iterator<Item = PackageMetadata> + Send,
    ) -> Result<()>;

    /// Optimize the search index (compact, merge segments)
    async fn optimize(&self) -> Result<()>;

    /// Get search index statistics
    ///
    /// # Returns
    /// * `IndexStats` - Document count, size, etc.
    async fn stats(&self) -> Result<IndexStats>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Create Tantivy search engine
    let search = TantivySearchEngine::new("./search-index").await?;

    // Index a package
    let metadata = create_package_metadata();
    search.index(&metadata).await?;

    // Basic search
    let query = SearchQuery {
        text: "web framework".to_string(),
        categories: vec![],
        tags: vec![],
        min_downloads: None,
        limit: 10,
        offset: 0,
    };

    let results = search.search(&query).await?;
    for result in results {
        println!("{}: {} (score: {})",
            result.package.id.name,
            result.package.description,
            result.score
        );
    }

    // Autocomplete
    let suggestions = search.suggest("web-", 5).await?;
    println!("Suggestions: {:?}", suggestions);

    // Faceted search
    let facets = search.facets(&query).await?;
    println!("Categories: {:?}", facets.categories);

    // Find similar packages
    let similar = search.similar(&PackageId::new("web-framework", "1.0.0"), 5).await?;

    Ok(())
}
```

---

## CryptoVerifier Trait

The `CryptoVerifier` trait ensures package authenticity and integrity through cryptographic operations.

### Trait Definition

```rust
#[async_trait]
pub trait CryptoVerifier: Send + Sync {
    /// Sign package metadata
    ///
    /// # Arguments
    /// * `metadata` - Package metadata to sign
    /// * `private_key` - Signing private key
    ///
    /// # Returns
    /// * `Signature` - Digital signature
    async fn sign(
        &self,
        metadata: &PackageMetadata,
        private_key: &PrivateKey,
    ) -> Result<Signature>;

    /// Verify a package signature
    ///
    /// # Arguments
    /// * `metadata` - Package metadata
    /// * `signature` - Signature to verify
    ///
    /// # Returns
    /// * `VerificationResult` - Verification outcome with details
    async fn verify_signature(
        &self,
        metadata: &PackageMetadata,
        signature: &Signature,
    ) -> Result<VerificationResult>;

    /// Compute checksum for package bytes
    ///
    /// # Arguments
    /// * `bytes` - Package data
    /// * `algorithm` - Hash algorithm (SHA256, BLAKE3, etc.)
    ///
    /// # Returns
    /// * `ContentHash` - Cryptographic hash
    async fn checksum(
        &self,
        bytes: &[u8],
        algorithm: HashAlgorithm,
    ) -> Result<ContentHash>;

    /// Verify package integrity using checksum
    ///
    /// # Arguments
    /// * `bytes` - Package data
    /// * `expected` - Expected hash
    ///
    /// # Returns
    /// * `bool` - True if checksum matches
    async fn verify_checksum(
        &self,
        bytes: &[u8],
        expected: &ContentHash,
    ) -> Result<bool>;

    /// Verify trust chain for a public key
    ///
    /// Checks if the public key is trusted (directly or via chain of trust).
    ///
    /// # Arguments
    /// * `public_key` - Public key to verify
    ///
    /// # Returns
    /// * `TrustResult` - Trust status and chain information
    async fn verify_trust(&self, public_key: &PublicKey) -> Result<TrustResult>;

    /// Add a trusted public key
    ///
    /// # Arguments
    /// * `public_key` - Public key to trust
    /// * `trust_level` - Level of trust (full, partial, etc.)
    async fn add_trusted_key(
        &self,
        public_key: &PublicKey,
        trust_level: TrustLevel,
    ) -> Result<()>;

    /// Revoke trust for a public key
    ///
    /// # Arguments
    /// * `public_key` - Public key to revoke
    /// * `reason` - Reason for revocation
    async fn revoke_key(&self, public_key: &PublicKey, reason: &str) -> Result<()>;

    /// Check if a key has been revoked
    ///
    /// # Arguments
    /// * `public_key` - Public key to check
    ///
    /// # Returns
    /// * `bool` - True if revoked
    async fn is_revoked(&self, public_key: &PublicKey) -> Result<bool>;

    /// Generate a new key pair
    ///
    /// # Returns
    /// * `KeyPair` - Generated public/private key pair
    async fn generate_keypair(&self) -> Result<KeyPair>;

    /// Import a key from PEM format
    ///
    /// # Arguments
    /// * `pem_data` - PEM-encoded key
    ///
    /// # Returns
    /// * `PublicKey` or `PrivateKey` - Imported key
    async fn import_key_pem(&self, pem_data: &str) -> Result<Key>;

    /// Export a key to PEM format
    ///
    /// # Arguments
    /// * `key` - Key to export
    ///
    /// # Returns
    /// * `String` - PEM-encoded key
    async fn export_key_pem(&self, key: &Key) -> Result<String>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Create Ed25519 verifier
    let crypto = Ed25519CryptoVerifier::new();

    // Generate a key pair
    let keypair = crypto.generate_keypair().await?;
    println!("Public key: {}", keypair.public_key);

    // Sign package metadata
    let metadata = create_package_metadata();
    let signature = crypto.sign(&metadata, &keypair.private_key).await?;

    // Verify signature
    let result = crypto.verify_signature(&metadata, &signature).await?;
    assert!(result.is_valid);

    // Compute checksum
    let package_bytes = b"package content";
    let hash = crypto.checksum(package_bytes, HashAlgorithm::SHA256).await?;
    println!("SHA256: {}", hash);

    // Verify checksum
    let valid = crypto.verify_checksum(package_bytes, &hash).await?;
    assert!(valid);

    // Trust management
    crypto.add_trusted_key(&keypair.public_key, TrustLevel::Full).await?;
    let trust = crypto.verify_trust(&keypair.public_key).await?;
    assert!(trust.is_trusted);

    // Export key for storage
    let pem = crypto.export_key_pem(&keypair.public_key.into()).await?;
    std::fs::write("public_key.pem", pem)?;

    Ok(())
}
```

---

## MetricsCollector Trait

The `MetricsCollector` trait collects operational metrics and telemetry.

### Trait Definition

```rust
use std::time::Duration;

#[async_trait]
pub trait MetricsCollector: Send + Sync {
    /// Record a counter metric
    ///
    /// # Arguments
    /// * `name` - Metric name
    /// * `value` - Value to add to counter
    /// * `labels` - Optional labels for categorization
    async fn counter(&self, name: &str, value: u64, labels: &[(&str, &str)]);

    /// Record a gauge metric (current value)
    ///
    /// # Arguments
    /// * `name` - Metric name
    /// * `value` - Current value
    /// * `labels` - Optional labels
    async fn gauge(&self, name: &str, value: f64, labels: &[(&str, &str)]);

    /// Record a histogram metric (distribution)
    ///
    /// # Arguments
    /// * `name` - Metric name
    /// * `value` - Observed value
    /// * `labels` - Optional labels
    async fn histogram(&self, name: &str, value: f64, labels: &[(&str, &str)]);

    /// Record an operation timing
    ///
    /// # Arguments
    /// * `operation` - Operation name
    /// * `duration` - Time taken
    /// * `labels` - Optional labels
    async fn timing(&self, operation: &str, duration: Duration, labels: &[(&str, &str)]);

    /// Record an error occurrence
    ///
    /// # Arguments
    /// * `error_type` - Error category
    /// * `message` - Error message
    /// * `labels` - Optional labels
    async fn error(&self, error_type: &str, message: &str, labels: &[(&str, &str)]);

    /// Get current metric values
    ///
    /// # Returns
    /// * `HashMap<String, MetricValue>` - Current metrics
    async fn snapshot(&self) -> Result<HashMap<String, MetricValue>>;

    /// Reset all metrics
    async fn reset(&self) -> Result<()>;

    /// Export metrics in Prometheus format
    ///
    /// # Returns
    /// * `String` - Prometheus text exposition format
    async fn export_prometheus(&self) -> Result<String>;

    /// Export metrics as JSON
    ///
    /// # Returns
    /// * `serde_json::Value` - JSON representation
    async fn export_json(&self) -> Result<serde_json::Value>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;
use std::time::Instant;

#[tokio::main]
async fn main() -> Result<()> {
    // Create Prometheus metrics collector
    let metrics = PrometheusMetricsCollector::new();

    // Record operations
    metrics.counter("packages_published", 1, &[("author", "alice")]).await;
    metrics.gauge("active_connections", 42.0, &[]).await;

    // Time an operation
    let start = Instant::now();
    perform_search().await?;
    metrics.timing("search_query", start.elapsed(), &[("type", "fulltext")]).await;

    // Record errors
    if let Err(e) = risky_operation().await {
        metrics.error("storage_error", &e.to_string(), &[]).await;
    }

    // Export for Prometheus scraping
    let prometheus_format = metrics.export_prometheus().await?;
    println!("{}", prometheus_format);

    // Or export as JSON for custom dashboards
    let json_format = metrics.export_json().await?;
    println!("{}", serde_json::to_string_pretty(&json_format)?);

    Ok(())
}
```

---

## Replication Trait (Optional)

The `Replication` trait enables distributed registry replication using consensus algorithms like Raft.

### Trait Definition

```rust
#[async_trait]
pub trait Replication: Send + Sync {
    /// Initialize replication node
    ///
    /// # Arguments
    /// * `node_id` - Unique node identifier
    /// * `peers` - List of peer nodes
    async fn initialize(&self, node_id: &str, peers: Vec<PeerInfo>) -> Result<()>;

    /// Replicate a write operation across the cluster
    ///
    /// # Arguments
    /// * `operation` - Operation to replicate
    ///
    /// # Returns
    /// * `CommitResult` - Result after consensus
    async fn replicate(&self, operation: Operation) -> Result<CommitResult>;

    /// Get current node role (leader, follower, candidate)
    ///
    /// # Returns
    /// * `NodeRole` - Current role in the cluster
    async fn role(&self) -> Result<NodeRole>;

    /// Get cluster status
    ///
    /// # Returns
    /// * `ClusterStatus` - Health and membership information
    async fn cluster_status(&self) -> Result<ClusterStatus>;

    /// Add a new node to the cluster
    ///
    /// # Arguments
    /// * `peer` - New peer information
    async fn add_peer(&self, peer: PeerInfo) -> Result<()>;

    /// Remove a node from the cluster
    ///
    /// # Arguments
    /// * `node_id` - Node to remove
    async fn remove_peer(&self, node_id: &str) -> Result<()>;

    /// Sync state from leader (for new or recovering nodes)
    async fn sync_from_leader(&self) -> Result<()>;

    /// Get replication lag (how far behind leader)
    ///
    /// # Returns
    /// * `Duration` - Estimated lag time
    async fn replication_lag(&self) -> Result<Duration>;
}
```

### Usage Examples

```rust
use ggen_marketplace::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize Raft replication
    let replication = RaftReplication::new();

    let peers = vec![
        PeerInfo::new("node-1", "10.0.0.1:5000"),
        PeerInfo::new("node-2", "10.0.0.2:5000"),
        PeerInfo::new("node-3", "10.0.0.3:5000"),
    ];

    replication.initialize("node-1", peers).await?;

    // Wait for leader election
    loop {
        let role = replication.role().await?;
        if role == NodeRole::Leader {
            println!("This node is the leader");
            break;
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }

    // Replicate an operation
    let operation = Operation::RegisterPackage {
        metadata: create_package_metadata(),
        hash: ContentHash::default(),
    };

    let result = replication.replicate(operation).await?;
    println!("Operation committed: {:?}", result);

    // Monitor cluster health
    let status = replication.cluster_status().await?;
    println!("Cluster members: {}", status.member_count);
    println!("Healthy: {}", status.is_healthy);

    Ok(())
}
```

---

## Implementing Custom Traits

### Example: Custom Registry Implementation

```rust
use ggen_marketplace::prelude::*;
use async_trait::async_trait;

pub struct CustomRegistry {
    // Your custom fields
}

#[async_trait]
impl Registry for CustomRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        // Your custom implementation: Search packages in your storage backend
        // Example: Query your database, filter by query terms, return matching packages
        let _ = query;
        Ok(Vec::new())
    }

    async fn get_package(&self, id: &PackageId) -> Result<Package> {
        // Your custom implementation: Retrieve package from your storage
        // Example: Look up package in your database by ID
        let _ = id;
        Err(MarketplaceError::package_not_found(
            &id.to_string(),
            "Package not found in custom registry",
        ))
    }

    async fn publish(&self, package: Package) -> Result<()> {
        // Your custom implementation: Store package in your backend
        // Example: Save package metadata to your database, index for search
        let _ = package;
        Ok(())
    }

    // Implement other required methods...
}
```

### Example: Composing Traits

```rust
use ggen_marketplace::prelude::*;

pub struct CompositeStore {
    primary: Box<dyn PackageStore>,
    backup: Box<dyn PackageStore>,
}

#[async_trait]
impl PackageStore for CompositeStore {
    async fn store(&self, package_bytes: Bytes) -> Result<ContentHash> {
        // Store in both primary and backup
        let hash = self.primary.store(package_bytes.clone()).await?;

        // Backup asynchronously (fire and forget)
        let backup = self.backup.clone();
        let bytes = package_bytes.clone();
        tokio::spawn(async move {
            let _ = backup.store(bytes).await;
        });

        Ok(hash)
    }

    // Other methods...
}
```

---

## Summary

These traits form the foundation of the ggen-marketplace library:

- **Registry**: Package metadata and versioning
- **PackageStore**: Binary storage and retrieval
- **SearchEngine**: Fast package discovery
- **CryptoVerifier**: Security and integrity
- **MetricsCollector**: Observability
- **Replication**: Distributed consensus (optional)

Each trait is designed for:
- **Async-first**: Non-blocking I/O
- **Composability**: Traits can be combined
- **Extensibility**: Easy to implement custom backends
- **Type safety**: Leverages Rust's type system
- **Production-ready**: Comprehensive error handling

For integration examples, see [Integration Guide](integration.md).
