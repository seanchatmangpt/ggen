# P2P Marketplace Integration Architecture

**Version:** 2.4.0
**Date:** 2025-11-02
**Status:** Design Phase

## Executive Summary

This document defines the system architecture for integrating P2P (libp2p-based) marketplace functionality into ggen. The design emphasizes clean separation of concerns, resilience, and backward compatibility with the existing file-based registry.

### Key Architectural Decisions

1. **Dual-Registry Pattern**: Support both file-based and P2P registries simultaneously
2. **Clean Layering**: CLI → Domain → Backend with clear interfaces
3. **Feature-Gated P2P**: P2P functionality behind `p2p` feature flag
4. **Trait-Based Abstraction**: `Registry` trait enables polymorphic backend selection
5. **Graceful Degradation**: System functions without P2P when unavailable

---

## 1. System Context (C4 Level 1)

```
┌─────────────────────────────────────────────────────────────┐
│                      Ggen Marketplace                        │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐   │
│  │         CLI Layer (clap commands)                    │   │
│  └───────────────────────┬─────────────────────────────┘   │
│                          │                                   │
│  ┌───────────────────────▼─────────────────────────────┐   │
│  │     Domain Layer (business logic)                    │   │
│  │   • Search  • Install  • Publish  • P2P Commands    │   │
│  └───────────────────────┬─────────────────────────────┘   │
│                          │                                   │
│  ┌───────────────────────▼─────────────────────────────┐   │
│  │      Backend Layer (storage & networking)            │   │
│  │   • File Registry  • P2P Registry  • Cache          │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
         │                              │
         │                              │
         ▼                              ▼
┌─────────────────┐          ┌──────────────────────┐
│ Local Filesystem│          │  P2P Network (libp2p)│
│  ~/.ggen/       │          │  • Kademlia DHT      │
│  • registry/    │          │  • Gossipsub         │
│  • packages/    │          │  • Peer Discovery    │
└─────────────────┘          └──────────────────────┘
```

### External Dependencies

- **Local Filesystem**: Primary storage for registry index and installed packages
- **P2P Network**: Distributed package discovery and retrieval via libp2p
- **User CLI**: Command-line interface for all operations

---

## 2. Container Architecture (C4 Level 2)

### 2.1 Core Containers

```
┌──────────────────────────────────────────────────────────────┐
│                         CLI Container                         │
│  ┌────────────────────────────────────────────────────────┐  │
│  │  marketplace::MarketplaceCmd                            │  │
│  │  • search  • install  • list  • publish  • p2p         │  │
│  └────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────┘
                              │
                              │ executes domain functions
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                       Domain Container                        │
│  ┌─────────────────────┐  ┌────────────────────────────┐    │
│  │  Search Domain      │  │  Install Domain             │    │
│  │  • SearchFilters    │  │  • InstallOptions           │    │
│  │  • search_packages  │  │  • dependency resolution    │    │
│  └─────────────────────┘  │  • install_package          │    │
│                            └────────────────────────────┘    │
│  ┌─────────────────────┐  ┌────────────────────────────┐    │
│  │  P2P Domain         │  │  Registry Domain            │    │
│  │  • execute_p2p_cmd  │  │  • Registry struct          │    │
│  │  • start_node       │  │  • CacheManager             │    │
│  │  • publish_package  │  │  • RegistryIndex            │    │
│  └─────────────────────┘  └────────────────────────────┘    │
└──────────────────────────────────────────────────────────────┘
                              │
                              │ uses backend traits
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                      Backend Container                        │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  ggen_marketplace::traits::Registry (trait)              │ │
│  │  • search(&Query) -> Vec<Package>                        │ │
│  │  • get_package(&PackageId) -> Package                    │ │
│  │  • publish(Package) -> Result<()>                        │ │
│  └─────────────────────────────────────────────────────────┘ │
│                              │                                │
│              ┌───────────────┴──────────────┐                │
│              ▼                               ▼                │
│  ┌─────────────────────┐        ┌──────────────────────┐    │
│  │  FileRegistry       │        │  P2PRegistry          │    │
│  │  • Local index.json │        │  • libp2p Swarm       │    │
│  │  • Package cache    │        │  • Kademlia DHT       │    │
│  │  • LRU eviction     │        │  • Gossipsub pubsub   │    │
│  └─────────────────────┘        │  • Peer reputation    │    │
│                                  └──────────────────────┘    │
└──────────────────────────────────────────────────────────────┘
```

### 2.2 Key Interfaces

#### Registry Trait (Backend Abstraction)

```rust
#[async_trait]
pub trait Registry: Send + Sync {
    async fn search(&self, query: &Query) -> Result<Vec<Package>>;
    async fn get_package(&self, id: &PackageId) -> Result<Package>;
    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package>;
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>>;
    async fn publish(&self, package: Package) -> Result<()>;
    async fn delete(&self, id: &PackageId, version: &str) -> Result<()>;
    async fn exists(&self, id: &PackageId) -> Result<bool>;
    async fn metadata(&self) -> Result<RegistryMetadata>;
}
```

#### Domain API (CLI to Domain)

```rust
// Search
pub async fn search_packages(filters: SearchFilters) -> Result<Vec<PackageInfo>>;

// Install
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult>;

// P2P Operations
pub async fn execute_p2p_command(command: P2PCommand) -> Result<()>;
```

---

## 3. Component Architecture (C4 Level 3)

### 3.1 P2P Backend Integration

```
┌──────────────────────────────────────────────────────────────┐
│                      P2PRegistry Component                    │
│                                                               │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  P2PRegistry                                             │ │
│  │  Fields:                                                 │ │
│  │  • swarm: Arc<RwLock<Swarm<P2PBehaviour>>>              │ │
│  │  • peer_id: PeerId                                       │ │
│  │  • local_packages: Arc<RwLock<HashMap<PackageId, Pkg>>> │ │
│  │  • discovered_packages: Arc<RwLock<HashMap<...>>>       │ │
│  │  • peer_reputation: Arc<RwLock<HashMap<PeerId, Rep>>>   │ │
│  └─────────────────────────────────────────────────────────┘ │
│                              │                                │
│              ┌───────────────┼──────────────┐                │
│              ▼               ▼               ▼                │
│  ┌──────────────┐ ┌──────────────┐ ┌─────────────────┐     │
│  │  Kademlia    │ │  Gossipsub   │ │ Peer Reputation │     │
│  │  DHT         │ │  PubSub      │ │ Tracker         │     │
│  │              │ │              │ │                 │     │
│  │ • DHT store  │ │ • Topics     │ │ • Success rate  │     │
│  │ • DHT query  │ │ • Broadcast  │ │ • Last seen     │     │
│  │ • Bootstrap  │ │ • Subscribe  │ │ • Filtering     │     │
│  └──────────────┘ └──────────────┘ └─────────────────┘     │
└──────────────────────────────────────────────────────────────┘
```

### 3.2 Registry Coordination

```
┌──────────────────────────────────────────────────────────────┐
│              Registry Selection & Fallback Logic              │
│                                                               │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  RegistryCoordinator (future enhancement)                │ │
│  │                                                           │ │
│  │  Decision Logic:                                         │ │
│  │  1. Check --p2p flag or P2P_ENABLED env var             │ │
│  │  2. If P2P requested:                                    │ │
│  │     a. Try P2PRegistry::new()                            │ │
│  │     b. On failure, fallback to FileRegistry              │ │
│  │  3. Else: Use FileRegistry (default)                     │ │
│  │                                                           │ │
│  │  Hybrid Mode (future):                                   │ │
│  │  • Query both registries in parallel                     │ │
│  │  • Merge results by PackageId                            │ │
│  │  • Prefer P2P for newer versions                         │ │
│  └─────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```

### 3.3 Package Installation Flow

```
┌──────────────────────────────────────────────────────────────┐
│                   Package Installation Pipeline               │
│                                                               │
│  User: ggen marketplace install pkg@1.0.0                    │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 1. Parse Package Spec                                    │ │
│  │    pkg@1.0.0 → (name: "pkg", version: "1.0.0")          │ │
│  └─────────────────────────────────────────────────────────┘ │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 2. Registry.get_package_version("pkg", "1.0.0")         │ │
│  │    • Check local cache                                   │ │
│  │    • Query registry (file or P2P)                        │ │
│  │    • Return Package metadata                             │ │
│  └─────────────────────────────────────────────────────────┘ │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 3. Resolve Dependencies                                  │ │
│  │    • Build dependency graph                              │ │
│  │    • Detect circular dependencies                        │ │
│  │    • Topological sort for install order                  │ │
│  └─────────────────────────────────────────────────────────┘ │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 4. Download & Verify                                     │ │
│  │    • Download tarball from download_url                  │ │
│  │    • Verify checksum (SHA256)                            │ │
│  │    • Validate signature (future)                         │ │
│  └─────────────────────────────────────────────────────────┘ │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 5. Extract & Install                                     │ │
│  │    • Extract tarball to ~/.ggen/packages/pkg            │ │
│  │    • Update ggen.lock file                               │ │
│  │    • Run post-install hooks (future)                     │ │
│  └─────────────────────────────────────────────────────────┘ │
│         │                                                     │
│         ▼                                                     │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │ 6. Rollback on Failure                                   │ │
│  │    • Remove partially installed packages                 │ │
│  │    • Revert lockfile changes                             │ │
│  │    • Report error to user                                │ │
│  └─────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```

---

## 4. Data Flow Architecture

### 4.1 Search Flow (P2P vs File)

#### File-Based Search
```
User Query → SearchFilters → Registry.search() → FileRegistry
                                                       │
                                                       ▼
                                   Load index.json → Filter packages
                                                       │
                                                       ▼
                                   Return Vec<Package> → Display Results
```

#### P2P Search
```
User Query → SearchFilters → Registry.search() → P2PRegistry
                                                       │
                                       ┌───────────────┴───────────────┐
                                       ▼                               ▼
                          Search local_packages         Query Kademlia DHT
                                       │                               │
                                       └───────────┬───────────────────┘
                                                   │
                                                   ▼
                                   Filter by min_reputation → Merge results
                                                   │
                                                   ▼
                                   Return Vec<Package> → Display Results
```

### 4.2 Package Publishing Flow

#### File-Based Publish
```
User: ggen marketplace publish ./my-package
         │
         ▼
Parse package.json → Validate metadata → Create tarball
         │
         ▼
Copy to ~/.ggen/registry/pkg/1.0.0/ → Update index.json
         │
         ▼
Success ✓
```

#### P2P Publish
```
User: ggen marketplace p2p publish ./my-package
         │
         ▼
Parse package.json → Validate metadata → Create tarball
         │
         ▼
P2PRegistry.publish(package)
         │
         ┌────────────┴────────────┐
         ▼                         ▼
Store in DHT             Announce via Gossipsub
 (Kademlia)                  (to /ggen/packages/v1 topic)
         │                         │
         └────────────┬────────────┘
                      │
                      ▼
         Store in local_packages (cache)
                      │
                      ▼
         Success ✓ (announced to N peers)
```

### 4.3 Peer Discovery Flow

```
P2P Node Startup
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 1. Load/Generate Keypair                        │
│    • Check ~/.ggen/p2p/keypair                  │
│    • Generate Ed25519 keypair if missing        │
└─────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 2. Initialize libp2p Swarm                      │
│    • Kademlia DHT                               │
│    • Gossipsub (topic: /ggen/packages/v1)      │
│    • Identify protocol                          │
└─────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 3. Start Listening                              │
│    • Bind to /ip4/0.0.0.0/tcp/0 (random port)  │
│    • Log local PeerId and multiaddr             │
└─────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 4. Bootstrap DHT                                │
│    • Connect to bootstrap nodes                 │
│    • Announce self to DHT                       │
│    • Begin Kademlia routing table population    │
└─────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 5. Subscribe to Gossipsub                       │
│    • Subscribe to /ggen/packages/v1             │
│    • Listen for package announcements           │
└─────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────┐
│ 6. Event Loop (continuous)                      │
│    • Process swarm events                       │
│    • Handle peer connections/disconnections     │
│    • Update peer reputation                     │
│    • Cache discovered packages                  │
└─────────────────────────────────────────────────┘
```

---

## 5. Module Boundaries & Responsibilities

### 5.1 Layer Responsibilities

#### CLI Layer (`cli/src/cmds/marketplace.rs`)
**Responsibilities:**
- Parse command-line arguments via clap
- Dispatch to domain functions
- Handle synchronous execution context
- Format output for terminal display

**Does NOT:**
- Contain business logic
- Directly access filesystem or network
- Manage state or cache

#### Domain Layer (`cli/src/domain/marketplace/`)
**Responsibilities:**
- Implement business logic (search, install, publish)
- Orchestrate backend operations
- Validate inputs and handle errors
- Manage dependency resolution
- Coordinate transactions (install + rollback)

**Does NOT:**
- Know about CLI argument parsing
- Implement low-level I/O or networking
- Know about libp2p details

#### Backend Layer (`ggen-marketplace/src/`)
**Responsibilities:**
- Implement `Registry` trait for different backends
- Manage network protocols (libp2p, HTTP)
- Handle storage (filesystem, DHT)
- Provide caching and optimization
- Track peer reputation (P2P only)

**Does NOT:**
- Implement dependency resolution
- Handle rollback logic
- Format user-facing output

### 5.2 Module Interfaces

```rust
// CLI to Domain
cli::cmds::marketplace::MarketplaceArgs::execute()
    → domain::marketplace::search::run(&SearchArgs)
    → domain::marketplace::install::run(&InstallArgs)
    → domain::marketplace::execute_p2p_command(P2PCommand)

// Domain to Backend
domain::search::search_packages(filters)
    → backend::Registry::search(&Query)

domain::install::install_package(options)
    → backend::Registry::get_package(&PackageId)
    → backend::Registry::get_package_version(&PackageId, &str)

domain::p2p::start_node(args)
    → backend::P2PRegistry::new(config)
    → backend::P2PRegistry::start_listening()
    → backend::P2PRegistry::bootstrap()
```

---

## 6. Failure Modes & Resilience

### 6.1 P2P Network Failures

| Failure Scenario | Detection | Mitigation | User Impact |
|-----------------|-----------|------------|-------------|
| **No bootstrap nodes available** | Connection timeout after 30s | Fallback to file registry | Warning: "P2P unavailable, using local registry" |
| **DHT query timeout** | No response after 10s | Try alternate peers, then return cached results | Partial results with warning |
| **All peers unreachable** | Connection refused / timeout | Use local packages only | "Operating in offline mode" |
| **Malicious peer (bad package)** | Checksum verification fails | Ban peer, try next provider | Retry with different peer |
| **Network partition** | No new peer connections for 60s | Continue with known peers, retry bootstrap | Reduced package availability |
| **Gossipsub flood** | High message rate (>1000/s) | Rate limiting, drop excess messages | Possible delayed announcements |

### 6.2 File Registry Failures

| Failure Scenario | Detection | Mitigation | User Impact |
|-----------------|-----------|------------|-------------|
| **index.json corrupted** | JSON parse error | Create new empty index | "Registry index reset, run update" |
| **Registry directory deleted** | File not found | Recreate directory structure | Initialize new registry |
| **Disk full** | Write error | Abort operation, cleanup partial writes | Error: "Insufficient disk space" |
| **Concurrent writes** | File lock conflict | Retry with exponential backoff (3 attempts) | Brief delay, transparent retry |

### 6.3 Installation Failures

| Failure Scenario | Detection | Mitigation | User Impact |
|-----------------|-----------|------------|-------------|
| **Circular dependency** | DFS cycle detection | Abort with dependency chain | Clear error with cycle path |
| **Missing dependency** | Package not found in registry | Prompt user or abort | "Missing dependency: pkg@1.0.0" |
| **Checksum mismatch** | Hash comparison fails | Delete download, mark peer bad, retry | Automatic retry with different source |
| **Extract fails mid-way** | Tar error | Rollback all installed packages | "Installation failed, rolled back" |
| **Insufficient permissions** | File write error | Check ~/.ggen/ permissions | "Permission denied: ~/.ggen/" |

### 6.4 Graceful Degradation

**P2P Feature Not Compiled:**
```rust
#[cfg(not(feature = "p2p"))]
async fn start_node(_args: StartArgs) -> Result<()> {
    Err(GgenError::feature_not_enabled(
        "p2p",
        "Rebuild with --features p2p to enable P2P functionality"
    ))
}
```

**P2P Network Unreachable (at runtime):**
```rust
match P2PRegistry::new(config).await {
    Ok(registry) => use_p2p(registry),
    Err(e) => {
        warn!("P2P initialization failed: {}, falling back to file registry", e);
        use_file_registry()
    }
}
```

---

## 7. Scalability Considerations

### 7.1 Local Registry Scaling

| Metric | Limit | Impact at Limit | Mitigation |
|--------|-------|-----------------|------------|
| **Packages in index** | ~10,000 | Slow search (linear scan) | Add SQLite index (future) |
| **Cache size** | 100 packages (LRU) | More disk reads | Increase capacity or add L2 cache |
| **Concurrent installs** | 1 (lockfile) | Queuing | Parallel install support (future) |
| **Index file size** | ~10MB at 10k packages | Slow startup load | Lazy loading or chunked index |

### 7.2 P2P Network Scaling

| Metric | Limit | Impact at Limit | Mitigation |
|--------|-------|-----------------|------------|
| **Connected peers** | ~200 (libp2p default) | DHT routing table full | Increase kbucket size |
| **DHT records** | ~10,000 (memory) | High memory usage | LRU eviction for DHT records |
| **Gossipsub topics** | 1 (/ggen/packages/v1) | All announcements to all peers | Add category-based topics (future) |
| **Package announcement size** | 1MB max | Large packages not announced | Announce metadata only, not tarball |
| **Peer reputation entries** | Unlimited | Memory leak risk | Prune peers not seen in 24h |

### 7.3 Performance Targets

| Operation | Target Latency | Acceptable at Scale |
|-----------|----------------|---------------------|
| Search (local) | <100ms | <500ms at 10k packages |
| Search (P2P) | <3s | <10s with 50 peers |
| Install (no deps) | <2s | <5s for 10MB package |
| Install (with deps) | <10s | <30s for 10 dependencies |
| P2P node startup | <5s | <15s with 10 bootstrap nodes |
| DHT query | <2s | <8s with network partition |

---

## 8. Security Architecture

### 8.1 Threat Model

| Threat | Attack Vector | Mitigation | Status |
|--------|--------------|------------|--------|
| **Malicious package** | Publish compromised package to P2P | Checksum verification, future: signature verification | Partial (checksum only) |
| **Sybil attack** | Flood network with fake peers | Peer reputation tracking, connection limits | Implemented |
| **Eclipse attack** | Isolate node from honest peers | Multiple bootstrap nodes, peer diversity | Implemented |
| **Man-in-the-middle** | Intercept DHT queries | TLS for bootstrap, future: QUIC transport | Partial (no TLS yet) |
| **Dependency confusion** | Publish similar package name | Namespace verification, future: publisher signatures | Not implemented |
| **Denial of service** | Flood with DHT queries | Rate limiting, peer scoring | Implemented |

### 8.2 Trust Boundaries

```
┌────────────────────────────────────────────────────┐
│              Trusted Zone                          │
│  • Local filesystem (~/.ggen/)                     │
│  • In-memory cache                                 │
│  • CLI user input (after validation)               │
└────────────────────────────────────────────────────┘
         │
         │ Checksum verification
         │ Signature checks (future)
         ▼
┌────────────────────────────────────────────────────┐
│           Semi-Trusted Zone                        │
│  • Known bootstrap nodes                           │
│  • High-reputation peers (>0.8)                    │
│  • Packages with valid checksums                   │
└────────────────────────────────────────────────────┘
         │
         │ Reputation filtering
         │ Redundant queries
         ▼
┌────────────────────────────────────────────────────┐
│           Untrusted Zone                           │
│  • Random P2P peers                                │
│  • DHT records from unknown sources                │
│  • Packages without signatures                     │
└────────────────────────────────────────────────────┘
```

### 8.3 Verification Pipeline

```rust
// Package installation verification pipeline
pub async fn install_package_verified(options: &InstallOptions) -> Result<InstallResult> {
    // 1. Retrieve package metadata
    let package = registry.get_package_version(&id, &version).await?;

    // 2. Download tarball
    let tarball_bytes = download(&package.download_url).await?;

    // 3. CRITICAL: Verify checksum
    let computed_hash = sha256(&tarball_bytes);
    if computed_hash != package.checksum {
        return Err(SecurityError::ChecksumMismatch {
            expected: package.checksum,
            actual: computed_hash,
        });
    }

    // 4. FUTURE: Verify signature
    #[cfg(feature = "signatures")]
    {
        let signature = package.signature.ok_or(SecurityError::MissingSignature)?;
        if !verify_signature(&tarball_bytes, &signature, &publisher_public_key)? {
            return Err(SecurityError::InvalidSignature);
        }
    }

    // 5. Extract to temporary directory first
    let temp_dir = tempfile::tempdir()?;
    extract_tarball(&tarball_bytes, temp_dir.path()).await?;

    // 6. Validate extracted files (no path traversal, etc.)
    validate_package_structure(temp_dir.path())?;

    // 7. Move to final location atomically
    atomic_move(temp_dir.path(), install_path)?;

    Ok(InstallResult { ... })
}
```

---

## 9. API Contracts

### 9.1 CLI to Domain Contract

```rust
// Search Command
pub fn run(args: &SearchArgs) -> Result<()> {
    runtime::block_on(async {
        search_and_display(
            &args.query,
            args.category.as_deref(),
            &args.tags,
            args.limit,
        ).await
    })
}

// Install Command
pub fn run(args: &InstallArgs) -> Result<()> {
    runtime::block_on(async {
        install_and_report(
            &args.package,
            args.target.as_deref(),
            args.force,
            !args.no_dependencies,
            args.dry_run,
        ).await
    })
}

// P2P Command
pub fn execute_p2p_command(command: P2PCommand) -> Result<()> {
    match command {
        P2PCommand::Start(args) => start_node(args).await,
        P2PCommand::Publish(args) => publish_package(args).await,
        P2PCommand::Search(args) => search_packages(args).await,
        // ... other commands
    }
}
```

### 9.2 Domain to Backend Contract

```rust
// Registry Trait (backend interface)
#[async_trait]
pub trait Registry: Send + Sync {
    /// Search for packages matching the query
    ///
    /// # Arguments
    /// * `query` - Search query with text, categories, tags, and limit
    ///
    /// # Returns
    /// * `Ok(Vec<Package>)` - List of matching packages
    /// * `Err(MarketplaceError)` - Network or storage error
    async fn search(&self, query: &Query) -> Result<Vec<Package>>;

    /// Retrieve a specific package by ID
    ///
    /// # Errors
    /// * `PackageNotFound` - Package does not exist in registry
    /// * `NetworkError` - P2P network unreachable (P2P only)
    /// * `StorageError` - Local storage read failure (File only)
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Publish a new package or version
    ///
    /// # Behavior
    /// * File: Copies to ~/.ggen/registry/, updates index.json
    /// * P2P: Stores in DHT, announces via Gossipsub
    ///
    /// # Errors
    /// * `ValidationError` - Invalid package metadata
    /// * `AlreadyExists` - Package version already published
    /// * `NetworkError` - P2P announcement failed
    async fn publish(&self, package: Package) -> Result<()>;

    /// Check if a package exists
    ///
    /// # Returns
    /// * `Ok(true)` - Package found in registry
    /// * `Ok(false)` - Package not found
    /// * `Err(...)` - Network or storage error
    async fn exists(&self, id: &PackageId) -> Result<bool>;
}
```

### 9.3 Error Handling Contract

```rust
/// Marketplace error types
#[derive(Debug, thiserror::Error)]
pub enum MarketplaceError {
    #[error("Package not found: {id} in {registry}")]
    PackageNotFound { id: String, registry: String },

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Storage error: {0}")]
    StorageError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Dependency error: {0}")]
    DependencyError(String),

    #[error("Security error: {0}")]
    SecurityError(String),
}

// Usage at boundaries
impl From<MarketplaceError> for GgenError {
    fn from(err: MarketplaceError) -> Self {
        match err {
            MarketplaceError::PackageNotFound { .. } => {
                GgenError::NotFound(err.to_string())
            }
            MarketplaceError::NetworkError(_) => {
                GgenError::NetworkError(err.to_string())
            }
            // ... other conversions
        }
    }
}
```

---

## 10. Migration Path & Backward Compatibility

### 10.1 Phased Rollout

**Phase 1: Current (v2.3.0)**
- ✅ File-based registry fully functional
- ✅ Install with dependency resolution
- ✅ Search with filters
- ❌ No P2P functionality

**Phase 2: P2P Foundation (v2.4.0)**
- ✅ P2P backend implementation (`ggen-marketplace/src/backend/p2p.rs`)
- ✅ CLI commands (`ggen marketplace p2p ...`)
- ✅ Feature flag (`--features p2p`)
- ✅ Graceful fallback when P2P unavailable
- ❌ Hybrid mode not yet implemented

**Phase 3: Enhanced P2P (v2.5.0)**
- 🔄 Signature verification for packages
- 🔄 Hybrid search (query both registries)
- 🔄 P2P package installation (download from peers)
- 🔄 Persistent keypair storage

**Phase 4: Production P2P (v3.0.0)**
- 🔄 DHT record expiration and cleanup
- 🔄 Advanced peer reputation (ML-based scoring)
- 🔄 Category-based Gossipsub topics
- 🔄 QUIC transport for P2P
- 🔄 Mirror synchronization (file ↔ P2P)

### 10.2 Backward Compatibility Guarantees

**File Format Stability:**
```rust
// index.json schema (v1.0.0)
{
  "version": "1.0.0",
  "updated_at": "2025-11-02T12:00:00Z",
  "packages": {
    "my-package": {
      "name": "my-package",
      "versions": [...],
      // Future fields additive, not breaking
    }
  }
}
```

**CLI Compatibility:**
- Old commands continue working: `ggen marketplace search`, `install`, etc.
- P2P commands are additive: `ggen marketplace p2p ...`
- Feature flag required for P2P: `--features p2p` (opt-in)

**API Stability:**
- `Registry` trait methods: Breaking changes require major version bump
- Domain functions: Additive changes only (new optional parameters)

---

## 11. Observability & Monitoring

### 11.1 Logging Strategy

```rust
use tracing::{debug, info, warn, error, instrument};

// High-level operations (INFO)
#[instrument(skip(self))]
pub async fn install_package(&self, options: &InstallOptions) -> Result<InstallResult> {
    info!("Installing package: {}@{:?}", options.package_name, options.version);
    // ...
    info!("Successfully installed package: {}", result.package_name);
    Ok(result)
}

// Internal details (DEBUG)
#[instrument(skip(self))]
async fn resolve_version(&self, name: &str, spec: &str) -> Result<String> {
    debug!("Resolving version: {} {}", name, spec);
    let resolved = /* ... */;
    debug!("Resolved {} {} -> {}", name, spec, resolved);
    Ok(resolved)
}

// Errors (WARN/ERROR)
if let Err(e) = registry.get_package(id).await {
    warn!("Failed to retrieve package from registry: {}", e);
    // Fallback logic
}

// Performance (TRACE)
let start = Instant::now();
let result = heavy_operation().await;
trace!("Operation completed in {:?}", start.elapsed());
```

### 11.2 Metrics (Future)

| Metric | Type | Purpose |
|--------|------|---------|
| `marketplace.search.duration_ms` | Histogram | Search performance |
| `marketplace.install.success_count` | Counter | Installation success rate |
| `marketplace.install.failure_count` | Counter | Installation failures |
| `marketplace.p2p.peer_count` | Gauge | Connected peers |
| `marketplace.p2p.dht_queries` | Counter | DHT query volume |
| `marketplace.p2p.peer_reputation_avg` | Gauge | Average peer reputation |
| `marketplace.cache.hit_rate` | Gauge | Cache effectiveness |
| `marketplace.cache.size` | Gauge | Cache memory usage |

---

## 12. Testing Strategy

### 12.1 Unit Tests

```rust
// Registry trait implementation tests
#[tokio::test]
async fn test_file_registry_search() {
    let registry = create_test_registry().await;
    let query = Query { text: "test".to_string(), ... };
    let results = registry.search(&query).await.unwrap();
    assert!(!results.is_empty());
}

#[tokio::test]
async fn test_p2p_registry_peer_reputation() {
    let registry = P2PRegistry::new(test_config()).await.unwrap();
    let peer_id = test_peer_id();
    registry.record_peer_success(peer_id).await;
    let reputation = registry.get_peer_reputation(&peer_id).await;
    assert_eq!(reputation, 1.0);
}
```

### 12.2 Integration Tests

```rust
// CLI to domain integration
#[test]
fn test_marketplace_install_command() {
    let args = InstallArgs {
        package: "test-pkg@1.0.0".to_string(),
        target: Some("/tmp/test".to_string()),
        force: false,
        no_dependencies: false,
        dry_run: false,
    };
    let result = marketplace::install::run(&args);
    assert!(result.is_ok());
}
```

### 12.3 E2E Tests

```rust
// Full installation flow with real registry
#[tokio::test]
async fn test_e2e_package_installation() {
    setup_test_registry().await;
    publish_test_package("test-pkg", "1.0.0").await;

    let result = install_package_e2e("test-pkg@1.0.0").await;
    assert!(result.is_ok());

    let installed_path = PathBuf::from("~/.ggen/packages/test-pkg");
    assert!(installed_path.exists());
}
```

### 12.4 P2P Network Tests

```rust
// Multi-node P2P simulation
#[tokio::test]
async fn test_p2p_package_discovery() {
    let node1 = start_p2p_node(config1).await;
    let node2 = start_p2p_node(config2).await;
    let node3 = start_p2p_node(config3).await;

    // Node1 publishes package
    node1.publish(test_package()).await.unwrap();

    // Wait for gossip propagation
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Node3 should discover package via DHT
    let results = node3.search(&Query { text: "test-package", ... }).await.unwrap();
    assert!(!results.is_empty());
}
```

---

## 13. Deployment Architecture

### 13.1 Bootstrap Node Infrastructure

```
┌───────────────────────────────────────────────────────┐
│              Bootstrap Node (Tier 1)                  │
│  • High availability (99.9% uptime)                   │
│  • Static multiaddrs                                  │
│  • DHT server mode enabled                            │
│  • No package publishing (routing only)               │
│  • Locations: us-east, eu-west, ap-southeast         │
└───────────────────────────────────────────────────────┘
         │
         │ bootstrap connection
         ▼
┌───────────────────────────────────────────────────────┐
│           Community Nodes (Tier 2)                    │
│  • User-run nodes                                     │
│  • Variable availability                              │
│  • Publish and consume packages                       │
│  • Contribute to DHT                                  │
└───────────────────────────────────────────────────────┘
         │
         │ peer-to-peer connections
         ▼
┌───────────────────────────────────────────────────────┐
│           Client Nodes (Tier 3)                       │
│  • Short-lived (ephemeral)                            │
│  • Primary use: search & install                      │
│  • May publish packages                               │
│  • Do not serve as bootstrap nodes                    │
└───────────────────────────────────────────────────────┘
```

### 13.2 Configuration Management

**Default Configuration (`~/.ggen/config.toml`):**
```toml
[marketplace]
# File registry (always enabled)
registry_path = "~/.ggen/registry"
packages_path = "~/.ggen/packages"
cache_capacity = 100

[marketplace.p2p]
# P2P registry (optional, requires --features p2p)
enabled = false
listen_address = "/ip4/0.0.0.0/tcp/0"
dht_server_mode = true

[[marketplace.p2p.bootstrap_nodes]]
address = "/dnsaddr/bootstrap-us.ggen.io/p2p/12D3KooW..."
priority = 1

[[marketplace.p2p.bootstrap_nodes]]
address = "/dnsaddr/bootstrap-eu.ggen.io/p2p/12D3KooW..."
priority = 2

[marketplace.p2p.reputation]
min_reputation = 0.5
prune_threshold = 0.2
prune_after_days = 7
```

---

## 14. Future Enhancements

### 14.1 Short-Term (v2.5.0 - 6 months)

1. **Package Signatures**
   - Ed25519 signature generation and verification
   - Publisher public key registry
   - Signature validation in install pipeline

2. **Hybrid Registry Mode**
   - Query both file and P2P registries simultaneously
   - Merge and deduplicate results
   - Prefer P2P for latest versions

3. **Persistent Keypair**
   - Store node keypair in `~/.ggen/p2p/keypair`
   - Stable PeerId across sessions
   - Optional keypair encryption

4. **P2P Package Download**
   - Download tarballs directly from peers
   - Fallback to HTTP download if P2P fails
   - Parallel multi-peer download

### 14.2 Medium-Term (v3.0.0 - 12 months)

1. **Advanced Peer Reputation**
   - ML-based reputation scoring
   - Collaboration with known good peers
   - Automatic peer blacklisting

2. **DHT Record Expiration**
   - Time-to-live for DHT records
   - Periodic re-announcement of packages
   - Cleanup of stale records

3. **Category-Based Topics**
   - Gossipsub topics per category (e.g., `/ggen/packages/web`)
   - Selective subscription to reduce bandwidth
   - Topic-based routing optimization

4. **QUIC Transport**
   - Replace TCP with QUIC for P2P
   - Better NAT traversal
   - Reduced connection setup time

### 14.3 Long-Term (v4.0.0+ - 18+ months)

1. **Package Mirrors**
   - Automated mirror synchronization (file ↔ P2P)
   - Geographic distribution of packages
   - CDN integration

2. **Decentralized Identity**
   - Publisher verification via DID
   - Reputation tied to identity
   - Trust network establishment

3. **Smart Contracts (Optional)**
   - Package licensing enforcement
   - Payment for premium packages
   - Bounty system for package development

4. **Package Provenance**
   - Full audit trail from source to installation
   - Build reproducibility verification
   - Supply chain security

---

## Appendix A: Architecture Decision Records (ADRs)

### ADR-001: Use Trait Abstraction for Registry Backend

**Status:** Accepted
**Date:** 2025-11-02

**Context:** Need to support both file-based and P2P registries without code duplication.

**Decision:** Implement `Registry` trait in `ggen-marketplace::traits` with two implementations: `FileRegistry` (implicit, via CLI) and `P2PRegistry` (libp2p-based).

**Consequences:**
- ✅ Clean separation of concerns
- ✅ Easy to add new backends (HTTP, SQL, etc.)
- ✅ Testable via mock implementations
- ❌ Requires async trait (adds complexity)
- ❌ Harder to optimize backend-specific operations

---

### ADR-002: Feature-Gate P2P Functionality

**Status:** Accepted
**Date:** 2025-11-02

**Context:** libp2p adds significant dependencies and binary size. Not all users need P2P.

**Decision:** P2P functionality behind `p2p` feature flag in `Cargo.toml`.

**Consequences:**
- ✅ Smaller binary for non-P2P users
- ✅ Faster compile times without P2P
- ✅ Opt-in adoption of P2P features
- ❌ Requires conditional compilation (`#[cfg(feature = "p2p")]`)
- ❌ More complex CI/CD (test both configurations)

---

### ADR-003: Use Gossipsub for Package Announcements

**Status:** Accepted
**Date:** 2025-11-02

**Context:** Need efficient broadcast of new packages to all peers.

**Decision:** Use libp2p Gossipsub protocol with topic `/ggen/packages/v1`.

**Consequences:**
- ✅ Efficient broadcast (flood-subscribe with optimization)
- ✅ Built-in message deduplication
- ✅ Scalable to 1000+ peers
- ❌ All peers receive all announcements (future: category topics)
- ❌ No guaranteed delivery (best-effort)

---

### ADR-004: Implement Peer Reputation Tracking

**Status:** Accepted
**Date:** 2025-11-02

**Context:** Malicious or unreliable peers could degrade user experience.

**Decision:** Track per-peer success/failure rates and filter by `min_reputation`.

**Consequences:**
- ✅ Protects against Sybil attacks
- ✅ Improves user experience (reliable peers prioritized)
- ✅ Enables automatic blacklisting
- ❌ Memory overhead (store reputation per peer)
- ❌ Cold start problem (new peers default to 1.0)

---

### ADR-005: Defer Signature Verification to v2.5.0

**Status:** Accepted
**Date:** 2025-11-02

**Context:** Signature verification is critical for security but adds complexity.

**Decision:** Implement checksum verification in v2.4.0, defer signatures to v2.5.0.

**Consequences:**
- ✅ Faster time-to-market for P2P MVP
- ✅ Simpler initial implementation
- ❌ Reduced security (checksums prevent corruption, not tampering)
- ❌ Must educate users about security limitations

---

## Appendix B: Glossary

| Term | Definition |
|------|------------|
| **Kademlia DHT** | Distributed hash table protocol for key-value storage across peers |
| **Gossipsub** | Publish-subscribe protocol for efficient message broadcasting |
| **PeerId** | Unique identifier for a peer in the P2P network (derived from public key) |
| **Multiaddr** | Composable network address format (e.g., `/ip4/127.0.0.1/tcp/8080/p2p/12D3...`) |
| **Bootstrap Node** | Well-known peer used to join the P2P network |
| **Reputation Score** | Metric (0.0 to 1.0) indicating peer reliability |
| **Lockfile** | `ggen.lock` file tracking installed package versions |
| **Registry Index** | `index.json` file containing package metadata |
| **Tarball** | Compressed package archive (`.tar.gz`) |
| **Dependency Graph** | Graph structure representing package dependencies |
| **LRU Cache** | Least Recently Used cache eviction policy |

---

**Document Version:** 1.0
**Last Updated:** 2025-11-02
**Next Review:** 2025-12-02
**Approved By:** System Architect Agent
