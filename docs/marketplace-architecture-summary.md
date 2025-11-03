# Marketplace Registry & CacheManager - Quick Reference

**Full Documentation**: See `marketplace-registry-cache-architecture.md` (1,417 lines)

## Core Components

### 1. Registry (Metadata & Discovery)
- **Location**: `cli/src/domain/marketplace/mod.rs`
- **Purpose**: Package discovery, version resolution, dependency management
- **Backend**: Delegates to `ggen_core::registry::RegistryClient`

```rust
pub struct Registry {
    client: RegistryClient,
    index_cache: Option<RegistryIndex>,
    cache_ttl: Duration,
    last_fetch: Option<Instant>,
}
```

**Key Methods**:
- `search(query)` - Search packages
- `resolve(package_id, version)` - Resolve to specific version
- `resolve_dependencies(package_id)` - Build dependency graph
- `list_all()` - List all packages

### 2. CacheManager (Local Storage + LRU)
- **Location**: `cli/src/domain/marketplace/mod.rs`
- **Purpose**: Local package caching with LRU eviction
- **Backend**: Uses `lru` crate for O(1) eviction

```rust
pub struct CacheManager {
    cache_dir: PathBuf,
    max_size: u64,
    lru: Arc<Mutex<LruCache<String, CacheEntry>>>,
    current_size: Arc<AtomicU64>,
}
```

**Key Methods**:
- `write(package_id, version, content)` - Cache package
- `read(package_id, version)` - Read from cache
- `evict_lru()` - Evict least recently used
- `verify(package_id, version, sha256)` - Verify integrity

## Data Structures

### PackageMetadata (Full Schema)
```rust
pub struct PackageMetadata {
    // Core
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,

    // Discovery
    pub tags: Vec<String>,
    pub keywords: Vec<String>,
    pub category: Option<String>,

    // Versioning
    pub latest_version: String,
    pub versions: HashMap<String, VersionMetadata>,

    // Dependencies
    pub dependencies: Vec<Dependency>,

    // Repository
    pub git_url: String,
    pub homepage: Option<String>,

    // Statistics
    pub downloads: Option<u64>,
    pub stars: Option<u32>,
    pub updated: Option<DateTime<Utc>>,
}
```

### VersionMetadata
```rust
pub struct VersionMetadata {
    pub version: String,
    pub git_url: String,
    pub git_rev: String,
    pub sha256: String,
    pub published: DateTime<Utc>,
    pub yanked: bool,
}
```

### DependencyGraph
```rust
pub struct DependencyGraph {
    pub root: DependencyNode,
    pub nodes: HashMap<String, DependencyNode>,
    pub resolved: HashMap<String, String>,
}
```

## Index File Format (JSON)

```json
{
  "version": "1.0.0",
  "updated": "2025-11-02T10:00:00Z",
  "packages": {
    "io.ggen.rust.cli": {
      "id": "io.ggen.rust.cli",
      "name": "Rust CLI Template",
      "latest_version": "1.2.0",
      "versions": {
        "1.2.0": {
          "version": "1.2.0",
          "git_url": "https://github.com/ggen/templates.git",
          "git_rev": "v1.2.0",
          "sha256": "abc123..."
        }
      },
      "dependencies": [
        {
          "package_id": "io.ggen.rust.common",
          "version": "^1.0.0",
          "optional": false
        }
      ]
    }
  }
}
```

## Version Resolution (Semver)

| Constraint | Example Match |
|------------|---------------|
| `1.2.3` | Exact: `1.2.3` |
| `^1.2.3` | Compatible: `1.2.3`, `1.9.9` (not `2.0.0`) |
| `~1.2.3` | Tilde: `1.2.3`, `1.2.9` (not `1.3.0`) |
| `>=1.2.0` | Range: `1.2.0`, `2.0.0` |
| `*` or `latest` | Latest non-yanked version |

**Algorithm**: Parse semver constraint → Filter non-yanked versions → Sort descending → Return first match

## Error Handling

```rust
#[derive(Error, Debug)]
pub enum MarketplaceError {
    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("Version not found: {package}@{version}")]
    VersionNotFound { package: String, version: String },

    #[error("Hash mismatch: expected {expected}, got {actual}")]
    HashMismatch { expected: String, actual: String },

    #[error("Circular dependency: {0}")]
    CircularDependency(String),

    #[error("Cache full: {used}/{max} bytes")]
    CacheFull { used: u64, max: u64 },
}
```

**Pattern**: Use `anyhow::Context` for error chains:
```rust
self.fetch_index()
    .await
    .context("Failed to fetch registry index")?;
```

## Performance Targets

| Operation | Target | Strategy |
|-----------|--------|----------|
| Search (cached) | <10ms | In-memory index |
| Search (cold) | <300ms | Network + parse |
| Cache check | <1ms | Filesystem stat |
| Hash (1MB) | <50ms | Streaming hash |
| LRU eviction | <10ms | O(1) with `lru` crate |
| Dep resolution (10 deps) | <100ms | BFS traversal |

## Implementation Phases

### Phase 1: Registry Core ✓
1. Implement `Registry` struct wrapping `RegistryClient`
2. Add index caching with TTL
3. Implement search & version resolution
4. Unit tests for semver constraints

### Phase 2: CacheManager ✓
1. Implement `CacheManager` with LRU
2. Add read/write operations
3. SHA-256 verification
4. Eviction strategies

### Phase 3: Dependency Resolution
1. `DependencyGraph` struct
2. Topological sort for install order
3. Conflict detection
4. Circular dependency detection

### Phase 4: Integration
1. Wire Registry & CacheManager to CLI commands
2. Error handling throughout
3. Integration tests
4. Performance benchmarks

## Dependencies Required

```toml
[dependencies]
# Version resolution
semver = "1.0"

# LRU cache
lru = "0.12"

# Hashing
sha2 = "0.10"

# From ggen-core
ggen-core = { path = "../ggen-core" }

# From workspace
tokio = { workspace = true }
reqwest = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
anyhow = { workspace = true }
thiserror = { workspace = true }
```

## File Structure

```
cli/src/domain/marketplace/
├── mod.rs              # Registry & CacheManager
├── install.rs          # Install command logic
├── search.rs           # Search command logic
├── list.rs             # List command logic
├── update.rs           # Update command logic
└── publish.rs          # Publish command logic
```

## Key Algorithms

### 1. Version Resolution
```rust
// Parse constraint → Filter non-yanked → Sort desc → First match
pub async fn resolve_version_constraint(
    &mut self,
    package_id: &str,
    constraint: &str,
) -> Result<String>
```

### 2. Dependency Resolution (BFS)
```rust
// BFS traversal building DependencyGraph
pub async fn resolve_dependencies(
    &mut self,
    package_id: &str,
    version: Option<&str>,
) -> Result<DependencyGraph>
```

### 3. Topological Sort (Install Order)
```rust
// DFS with cycle detection → Reverse order
pub fn topological_sort(&self) -> Result<Vec<String>>
```

### 4. LRU Eviction
```rust
// Pop LRU entries until under 80% max_size
pub async fn evict_lru(&self) -> Result<Vec<String>>
```

## Testing Strategy

1. **Unit Tests**: Version resolution, semver matching, LRU logic
2. **Integration Tests**: Registry fetch, cache operations, dependency resolution
3. **Property Tests**: Semver constraints, graph algorithms
4. **Performance Tests**: Search latency, cache throughput, hash calculation

## Related Files

- **Full Architecture**: `/Users/sac/ggen/docs/marketplace-registry-cache-architecture.md`
- **Existing Registry**: `/Users/sac/ggen/ggen-core/src/registry.rs`
- **Existing Cache**: `/Users/sac/ggen/ggen-core/src/cache.rs`
- **Target Location**: `/Users/sac/ggen/cli/src/domain/marketplace/mod.rs`
- **Example Usage**: `/Users/sac/ggen/examples/advanced-cache-registry/`

---

**Status**: Implementation-ready architecture design complete. All structs, methods, and algorithms specified. Ready for implementation by coding agents.
