# P2P Marketplace API Contracts

**Version:** 2.4.0
**Date:** 2025-11-02

This document defines the API contracts between layers in the ggen marketplace system with P2P integration.

---

## Table of Contents

1. [Layer Boundaries](#layer-boundaries)
2. [CLI to Domain Contracts](#cli-to-domain-contracts)
3. [Domain to Backend Contracts](#domain-to-backend-contracts)
4. [Error Handling Contracts](#error-handling-contracts)
5. [Data Structure Contracts](#data-structure-contracts)
6. [P2P Network Contracts](#p2p-network-contracts)

---

## Layer Boundaries

### Architecture Overview

```
┌──────────────────────────────────────────────┐
│  CLI Layer (clap commands)                   │
│  Responsibility: Parse args, format output   │
└──────────────────────────────────────────────┘
                    │
                    │ Domain API (public functions)
                    ▼
┌──────────────────────────────────────────────┐
│  Domain Layer (business logic)               │
│  Responsibility: Orchestration, validation   │
└──────────────────────────────────────────────┘
                    │
                    │ Backend Trait (Registry)
                    ▼
┌──────────────────────────────────────────────┐
│  Backend Layer (storage & network)           │
│  Responsibility: I/O, protocols, caching     │
└──────────────────────────────────────────────┘
```

**Key Principles:**
1. **CLI never calls Backend directly** - Always goes through Domain
2. **Domain is Backend-agnostic** - Uses traits, not concrete types
3. **Backend never knows about CLI** - No clap types, no terminal formatting

---

## CLI to Domain Contracts

### Search Command

```rust
// CLI Layer
// File: cli/src/cmds/marketplace.rs
#[derive(Debug, Args)]
pub struct SearchArgs {
    /// Search query text
    pub query: String,

    /// Filter by category
    #[arg(short = 'c', long)]
    pub category: Option<String>,

    /// Filter by tags
    #[arg(short = 't', long)]
    pub tags: Vec<String>,

    /// Maximum results
    #[arg(short = 'l', long, default_value = "20")]
    pub limit: usize,
}

// Domain Layer
// File: cli/src/domain/marketplace/search.rs
pub async fn search_and_display(
    query: &str,
    category: Option<&str>,
    tags: &[String],
    limit: usize,
) -> Result<()>;

// Contract:
// - CLI parses arguments
// - CLI calls domain function with primitive types
// - Domain handles business logic
// - Domain formats and prints output
// - Returns Result<()> for success/error
```

**Invariants:**
- `query` is never empty (enforced by clap required)
- `limit` is always > 0 (default: 20)
- `tags` can be empty (no filtering)
- `category` is `None` or non-empty string

---

### Install Command

```rust
// CLI Layer
#[derive(Debug, Args)]
pub struct InstallArgs {
    /// Package specification (name@version)
    pub package: String,

    /// Target directory
    #[arg(short = 't', long)]
    pub target: Option<String>,

    /// Force overwrite
    #[arg(short = 'f', long)]
    pub force: bool,

    /// Skip dependencies
    #[arg(long)]
    pub no_dependencies: bool,

    /// Dry run (simulate)
    #[arg(long)]
    pub dry_run: bool,
}

// Domain Layer
pub async fn install_and_report(
    package: &str,
    target: Option<&str>,
    force: bool,
    with_dependencies: bool,
    dry_run: bool,
) -> Result<()>;

// Contract:
// - package format: "name@version" or "name" (latest)
// - target is None (default) or absolute/relative path
// - force: true = overwrite existing, false = error if exists
// - with_dependencies: true = install deps, false = skip
// - dry_run: true = simulate only, false = execute
```

**Invariants:**
- `package` is never empty
- If `package` contains `@`, version part is non-empty
- `target` is None or valid path (validated in domain)
- Dry run never modifies filesystem

---

### P2P Commands

```rust
// CLI Layer
#[derive(Debug, Args)]
pub struct P2PArgs {
    #[command(subcommand)]
    pub command: P2PCommand,
}

#[derive(Debug, Clone, Subcommand)]
pub enum P2PCommand {
    Start(StartArgs),
    Publish(PublishArgs),
    Search(SearchArgs),
    PeerList(PeerListArgs),
    PeerInfo(PeerInfoArgs),
    Bootstrap(BootstrapArgs),
    Status,
}

// Domain Layer
pub async fn execute_p2p_command(command: P2PCommand) -> Result<()>;

// Contract:
// - CLI dispatches to execute_p2p_command
// - Domain matches on enum variant
// - Each variant handled by specific function
// - Returns unified Result<()>
```

**Feature Gate:**
```rust
#[cfg(not(feature = "p2p"))]
pub async fn execute_p2p_command(_: P2PCommand) -> Result<()> {
    Err(GgenError::feature_not_enabled(
        "p2p",
        "Rebuild with --features p2p to enable P2P"
    ))
}
```

---

## Domain to Backend Contracts

### Registry Trait (Core Abstraction)

```rust
// File: ggen-marketplace/src/traits/mod.rs
use async_trait::async_trait;
use crate::models::{Package, PackageId, Query, RegistryMetadata};
use crate::error::Result;

#[async_trait]
pub trait Registry: Send + Sync {
    /// Search for packages matching query criteria
    ///
    /// # Arguments
    /// * `query` - Search parameters (text, categories, tags, limit)
    ///
    /// # Returns
    /// * `Ok(Vec<Package>)` - Matching packages (may be empty)
    /// * `Err(MarketplaceError)` - Network or storage failure
    ///
    /// # Guarantees
    /// - Results are sorted by relevance (implementation-defined)
    /// - Respects query.limit (returns at most limit packages)
    /// - Never returns duplicate PackageIds
    /// - Empty query matches all packages
    async fn search(&self, query: &Query) -> Result<Vec<Package>>;

    /// Retrieve specific package by ID
    ///
    /// # Arguments
    /// * `id` - Unique package identifier
    ///
    /// # Returns
    /// * `Ok(Package)` - Package metadata
    /// * `Err(PackageNotFound)` - Package does not exist
    /// * `Err(NetworkError)` - P2P network unreachable (P2P only)
    /// * `Err(StorageError)` - Filesystem read failure (File only)
    ///
    /// # Guarantees
    /// - Returns latest version if package has multiple versions
    /// - Package.id == id (exact match)
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Get specific version of a package
    ///
    /// # Arguments
    /// * `id` - Package identifier
    /// * `version` - Exact version string (e.g., "1.0.0")
    ///
    /// # Returns
    /// * `Ok(Package)` - Package with matching version
    /// * `Err(PackageNotFound)` - Package or version not found
    ///
    /// # Guarantees
    /// - Package.version == version (exact match)
    /// - Fails if version does not exist (no fallback)
    async fn get_package_version(
        &self,
        id: &PackageId,
        version: &str
    ) -> Result<Package>;

    /// List all versions of a package
    ///
    /// # Arguments
    /// * `id` - Package identifier
    ///
    /// # Returns
    /// * `Ok(Vec<Package>)` - All versions (may be empty if package not found)
    /// * `Err(MarketplaceError)` - Storage or network error
    ///
    /// # Guarantees
    /// - Results sorted by version (ascending: 1.0.0, 1.1.0, 2.0.0)
    /// - All packages have same id, different versions
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>>;

    /// Publish a new package or version
    ///
    /// # Arguments
    /// * `package` - Complete package metadata and content reference
    ///
    /// # Returns
    /// * `Ok(())` - Successfully published
    /// * `Err(ValidationError)` - Invalid metadata
    /// * `Err(AlreadyExists)` - Package version already published
    /// * `Err(NetworkError)` - P2P broadcast failure (P2P only)
    ///
    /// # Side Effects
    /// - File: Writes to ~/.ggen/registry/, updates index.json
    /// - P2P: Stores in DHT, announces via Gossipsub
    ///
    /// # Guarantees
    /// - Atomic: Success means fully published
    /// - Idempotent: Re-publishing same version updates metadata
    async fn publish(&self, package: Package) -> Result<()>;

    /// Delete a package version
    ///
    /// # Arguments
    /// * `id` - Package identifier
    /// * `version` - Version to delete
    ///
    /// # Returns
    /// * `Ok(())` - Successfully deleted
    /// * `Err(PackageNotFound)` - Package or version not found
    /// * `Err(PermissionDenied)` - Not authorized to delete (future)
    ///
    /// # Side Effects
    /// - File: Removes from ~/.ggen/registry/, updates index.json
    /// - P2P: Stops serving locally (cannot truly delete from DHT)
    ///
    /// # Guarantees
    /// - After success, get_package_version returns NotFound
    /// - Other versions unaffected
    async fn delete(&self, id: &PackageId, version: &str) -> Result<()>;

    /// Check if package exists
    ///
    /// # Arguments
    /// * `id` - Package identifier
    ///
    /// # Returns
    /// * `Ok(true)` - Package exists (any version)
    /// * `Ok(false)` - Package does not exist
    /// * `Err(MarketplaceError)` - Storage or network error
    ///
    /// # Guarantees
    /// - Faster than get_package (may use cache/index only)
    /// - Returns true if any version exists
    async fn exists(&self, id: &PackageId) -> Result<bool>;

    /// Get registry metadata and capabilities
    ///
    /// # Returns
    /// * `Ok(RegistryMetadata)` - Registry information
    ///
    /// # Guarantees
    /// - Never fails (registry always knows its own metadata)
    /// - Metadata includes: name, URL, supports_publish, requires_auth
    async fn metadata(&self) -> Result<RegistryMetadata>;
}
```

---

### Domain Layer Usage Patterns

#### Pattern 1: Search with Fallback

```rust
// Domain Layer Implementation
pub async fn search_packages(filters: SearchFilters) -> Result<Vec<PackageInfo>> {
    // Build query
    let query = Query {
        text: filters.query.clone(),
        categories: filters.category.into_iter().collect(),
        tags: filters.tags.clone().into_iter().collect(),
        limit: Some(filters.limit),
    };

    // Determine which registry to use
    let results = if cfg!(feature = "p2p") && filters.use_p2p {
        // Try P2P first
        match try_p2p_search(&query).await {
            Ok(pkgs) => pkgs,
            Err(e) => {
                warn!("P2P search failed: {}, falling back to file", e);
                file_search(&query).await?
            }
        }
    } else {
        // Use file registry
        file_search(&query).await?
    };

    // Convert Backend models to Domain models
    let package_infos = results.into_iter()
        .map(|pkg| PackageInfo {
            name: pkg.metadata.title,
            version: pkg.version.to_string(),
            description: pkg.metadata.description,
            category: pkg.metadata.categories.first().cloned(),
        })
        .collect();

    Ok(package_infos)
}
```

#### Pattern 2: Install with Verification

```rust
// Domain Layer Implementation
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult> {
    // Step 1: Get package from registry
    let package = registry.get_package_version(
        &options.package_name,
        options.version.as_deref().unwrap_or("latest")
    ).await?;

    // Step 2: Download tarball (from package.download_url or P2P peer)
    let tarball_bytes = download_tarball(&package).await?;

    // Step 3: CRITICAL - Verify checksum
    let computed_hash = sha256(&tarball_bytes);
    if computed_hash != package.metadata.checksum {
        return Err(SecurityError::ChecksumMismatch {
            expected: package.metadata.checksum,
            actual: computed_hash,
        });
    }

    // Step 4: Extract to temporary location first
    let temp_dir = tempfile::tempdir()?;
    extract_tarball(&tarball_bytes, temp_dir.path()).await?;

    // Step 5: Atomic move to final location
    let install_path = get_install_path(&options.package_name)?;
    atomic_move(temp_dir.path(), &install_path)?;

    // Step 6: Update lockfile
    update_lockfile(&package, &install_path).await?;

    Ok(InstallResult {
        package_name: options.package_name.clone(),
        version: package.version.to_string(),
        install_path,
        dependencies_installed: vec![],
    })
}
```

---

## Error Handling Contracts

### Error Type Hierarchy

```rust
// Backend Layer Errors
// File: ggen-marketplace/src/error.rs
#[derive(Debug, thiserror::Error)]
pub enum MarketplaceError {
    #[error("Package not found: {id} in {registry}")]
    PackageNotFound {
        id: String,
        registry: String,
    },

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Storage error: {0}")]
    StorageError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Dependency resolution failed: {0}")]
    DependencyError(String),

    #[error("Security check failed: {0}")]
    SecurityError(String),

    #[error("Already exists: {0}")]
    AlreadyExists(String),

    #[error("Permission denied: {0}")]
    PermissionDenied(String),
}

// Domain Layer Errors
// File: ggen-utils/src/error.rs
#[derive(Debug, thiserror::Error)]
pub enum GgenError {
    #[error("Not found: {0}")]
    NotFound(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Feature not enabled: {0} - {1}")]
    FeatureNotEnabled {
        feature: String,
        help: String,
    },

    #[error("{0}")]
    Other(String),
}

// Error Conversion (Backend → Domain)
impl From<MarketplaceError> for GgenError {
    fn from(err: MarketplaceError) -> Self {
        match err {
            MarketplaceError::PackageNotFound { id, registry } => {
                GgenError::NotFound(format!("Package {} not found in {}", id, registry))
            }
            MarketplaceError::NetworkError(msg) => {
                GgenError::NetworkError(msg)
            }
            MarketplaceError::StorageError(e) => {
                GgenError::IoError(e)
            }
            MarketplaceError::ValidationError(msg) => {
                GgenError::InvalidInput(msg)
            }
            MarketplaceError::SecurityError(msg) => {
                GgenError::Other(format!("Security error: {}", msg))
            }
            // ... other conversions
            _ => GgenError::Other(err.to_string()),
        }
    }
}
```

### Error Handling Guidelines

**Backend Layer:**
- Return specific `MarketplaceError` variants
- Include context (package ID, registry name, etc.)
- Never panic (always return Result)

**Domain Layer:**
- Convert `MarketplaceError` → `GgenError`
- Add user-friendly context
- Log detailed errors, return simplified messages

**CLI Layer:**
- Print user-facing error messages
- Suggest corrective actions
- Exit with appropriate code (0 = success, 1 = error)

---

## Data Structure Contracts

### Package Model

```rust
// File: ggen-marketplace/src/models/mod.rs
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Package {
    /// Unique package identifier (e.g., "ggen-tools/web-framework")
    pub id: PackageId,

    /// Semantic version (e.g., "1.2.3")
    pub version: Version,

    /// Package metadata
    pub metadata: PackageMetadata,

    /// Content reference (IPFS CID, HTTP URL, or local path)
    pub content: ContentId,

    /// Digital signature (optional, for verification)
    pub signature: Option<Signature>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct PackageId(pub String);

impl PackageId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

impl std::fmt::Display for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Version(pub String);

impl Version {
    pub fn parse(s: &str) -> Result<Self> {
        // Simplified semver validation
        // TODO: Use semver crate for full validation
        if s.is_empty() {
            return Err(MarketplaceError::ValidationError(
                "Version cannot be empty".to_string()
            ));
        }
        Ok(Self(s.to_string()))
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PackageMetadata {
    /// Display name
    pub title: String,

    /// Short description
    pub description: String,

    /// Long-form README (Markdown)
    pub readme: Option<String>,

    /// Publisher information
    pub publisher: Publisher,

    /// Categories (e.g., ["web", "framework"])
    pub categories: Vec<String>,

    /// Tags (e.g., ["async", "http", "rest"])
    pub tags: Vec<String>,

    /// License (SPDX identifier)
    pub license: Option<String>,

    /// Repository URL
    pub repository: Option<String>,

    /// Homepage URL
    pub homepage: Option<String>,

    /// Dependencies (name → version requirement)
    pub dependencies: HashMap<String, String>,

    /// SHA256 checksum of content
    pub checksum: String,

    /// Size in bytes
    pub size_bytes: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Publisher {
    /// Publisher display name
    pub name: String,

    /// Publisher email (optional)
    pub email: Option<String>,

    /// Publisher public key (for signature verification)
    pub public_key: Option<PublicKey>,
}
```

### Query Model

```rust
#[derive(Debug, Clone, Default)]
pub struct Query {
    /// Free-text search query
    pub text: String,

    /// Filter by categories (OR logic)
    pub categories: HashSet<String>,

    /// Filter by tags (AND logic)
    pub tags: HashSet<String>,

    /// Maximum results to return
    pub limit: Option<usize>,
}

impl Query {
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            ..Default::default()
        }
    }

    pub fn with_category(mut self, category: impl Into<String>) -> Self {
        self.categories.insert(category.into());
        self
    }

    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.insert(tag.into());
        self
    }

    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }
}
```

### Registry Metadata

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryMetadata {
    /// Registry display name
    pub name: String,

    /// Registry URL or identifier
    pub url: String,

    /// Description (optional)
    pub description: Option<String>,

    /// Whether registry supports publishing
    pub supports_publish: bool,

    /// Whether registry requires authentication
    pub requires_auth: bool,
}
```

---

## P2P Network Contracts

### P2P Configuration

```rust
// File: ggen-marketplace/src/backend/p2p.rs
#[derive(Debug, Clone)]
pub struct P2PConfig {
    /// Bootstrap nodes for initial peer discovery
    pub bootstrap_nodes: Vec<Multiaddr>,

    /// Gossipsub topic for package announcements
    pub packages_topic: String,

    /// Enable DHT server mode (serve DHT queries)
    pub dht_server_mode: bool,

    /// Local listen addresses
    pub listen_addresses: Vec<Multiaddr>,
}

impl Default for P2PConfig {
    fn default() -> Self {
        let default_addr = "/ip4/0.0.0.0/tcp/0"
            .parse()
            .expect("Valid multiaddr");

        Self {
            bootstrap_nodes: Vec::new(),
            packages_topic: "/ggen/packages/v1".to_string(),
            dht_server_mode: true,
            listen_addresses: vec![default_addr],
        }
    }
}
```

### P2P Network Events

```rust
/// Events emitted by P2P registry
pub enum P2PEvent {
    /// New peer connected
    PeerConnected(PeerId),

    /// Peer disconnected
    PeerDisconnected(PeerId),

    /// Package announcement received
    PackageAnnounced {
        package_id: PackageId,
        provider: PeerId,
    },

    /// DHT query completed
    DHTQueryComplete {
        key: String,
        value: Option<Vec<u8>>,
    },

    /// DHT store completed
    DHTStoreComplete {
        key: String,
        success: bool,
    },

    /// Gossipsub message received
    GossipReceived {
        topic: String,
        data: Vec<u8>,
    },
}
```

### Peer Reputation Model

```rust
/// Peer reputation tracking
#[derive(Debug, Clone)]
pub struct PeerReputation {
    /// Peer identifier
    pub peer_id: PeerId,

    /// Successful package retrievals from this peer
    pub successful_retrievals: u64,

    /// Failed package retrievals from this peer
    pub failed_retrievals: u64,

    /// Last seen timestamp
    pub last_seen: chrono::DateTime<chrono::Utc>,
}

impl PeerReputation {
    /// Calculate reputation score (0.0 to 1.0)
    pub fn score(&self) -> f64 {
        let total = self.successful_retrievals + self.failed_retrievals;
        if total == 0 {
            return 1.0; // Neutral reputation for new peers
        }
        self.successful_retrievals as f64 / total as f64
    }

    /// Check if peer should be trusted
    pub fn is_trusted(&self, threshold: f64) -> bool {
        self.score() >= threshold
    }
}
```

---

## Versioning and Compatibility

### API Versioning Strategy

**Contract Version:** 1.0.0

**Breaking Changes (require major version bump):**
- Removing or renaming trait methods
- Changing method signatures (parameters, return types)
- Changing error types in a non-additive way

**Non-Breaking Changes (minor version):**
- Adding new trait methods with default implementations
- Adding new optional fields to structs
- Adding new error variants

**Backward Compatibility Guarantees:**
- Registry trait stable for v2.x.x series
- File registry format stable (index.json schema)
- P2P network protocol stable (/ggen/packages/v1 topic)

---

**Document Version:** 1.0
**Last Updated:** 2025-11-02
**Next Review:** 2025-12-02
