# Marketplace Implementation Guide

**Target**: Implement Registry and CacheManager for ggen marketplace CLI

## Quick Start

### 1. Read Architecture Documents (5 min)
- **Summary** (299 lines): `marketplace-architecture-summary.md` - Start here
- **Full Spec** (1,417 lines): `marketplace-registry-cache-architecture.md` - Reference
- **Diagrams**: `marketplace-architecture-diagram.md` - Visual guide

### 2. Review Existing Code (10 min)
- **Existing Registry**: `ggen-core/src/registry.rs` (828 lines) - Reuse RegistryClient
- **Existing Cache**: `ggen-core/src/cache.rs` (313 lines) - Reference implementation
- **Target Location**: `cli/src/domain/marketplace/mod.rs` (lines 35-38)

### 3. Study Examples (10 min)
- **Cache Demo**: `examples/advanced-cache-registry/src/cache_demo.rs`
- **Registry Demo**: `examples/advanced-cache-registry/src/registry_demo.rs`

## Implementation Order

### Phase 1: Registry Wrapper (2-3 hours)

**File**: `cli/src/domain/marketplace/mod.rs`

```rust
use ggen_core::registry::{RegistryClient, RegistryIndex};
use std::time::{Duration, Instant};

/// Registry for marketplace package discovery
#[derive(Debug, Clone)]
pub struct Registry {
    client: RegistryClient,
    index_cache: Option<RegistryIndex>,
    cache_ttl: Duration,
    last_fetch: Option<Instant>,
}

impl Registry {
    pub fn new() -> Result<Self> {
        Ok(Self {
            client: RegistryClient::new()?,
            index_cache: None,
            cache_ttl: Duration::from_secs(300), // 5 minutes
            last_fetch: None,
        })
    }

    pub async fn fetch_index(&mut self) -> Result<&RegistryIndex> {
        // Check cache validity
        if self.is_cache_valid() {
            return Ok(self.index_cache.as_ref().unwrap());
        }

        // Fetch fresh index
        let index = self.client.fetch_index().await?;
        self.index_cache = Some(index);
        self.last_fetch = Some(Instant::now());

        Ok(self.index_cache.as_ref().unwrap())
    }

    fn is_cache_valid(&self) -> bool {
        if let (Some(cache), Some(last)) = (&self.index_cache, self.last_fetch) {
            last.elapsed() < self.cache_ttl
        } else {
            false
        }
    }

    // Add more methods as needed...
}
```

**Tests**: `cli/tests/domain/marketplace/registry_tests.rs`

```rust
#[tokio::test]
async fn test_registry_caching() {
    let mut registry = Registry::new().unwrap();

    // First fetch (cold)
    let start = Instant::now();
    let index1 = registry.fetch_index().await.unwrap();
    let cold_time = start.elapsed();

    // Second fetch (cached)
    let start = Instant::now();
    let index2 = registry.fetch_index().await.unwrap();
    let warm_time = start.elapsed();

    assert!(warm_time < cold_time / 10); // Cached should be 10x faster
}
```

### Phase 2: CacheManager with LRU (3-4 hours)

**Dependencies** (add to `cli/Cargo.toml`):
```toml
[dependencies]
lru = "0.12"
sha2 = "0.10"
semver = "1.0"
```

**Implementation**:

```rust
use lru::LruCache;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicU64, Ordering};
use std::num::NonZeroUsize;

/// Cache manager with LRU eviction
#[derive(Debug, Clone)]
pub struct CacheManager {
    cache_dir: PathBuf,
    max_size: u64,
    lru: Arc<Mutex<LruCache<String, CacheEntry>>>,
    current_size: Arc<AtomicU64>,
}

#[derive(Debug, Clone)]
struct CacheEntry {
    package_id: String,
    version: String,
    size: u64,
    last_accessed: Instant,
}

impl CacheManager {
    pub fn new() -> Result<Self> {
        let cache_dir = dirs::cache_dir()
            .ok_or_else(|| Error::new("Failed to find cache directory"))?
            .join("ggen")
            .join("marketplace");

        fs::create_dir_all(&cache_dir)?;

        let capacity = NonZeroUsize::new(1000).unwrap();
        let lru = Arc::new(Mutex::new(LruCache::new(capacity)));

        Ok(Self {
            cache_dir,
            max_size: 5 * 1024 * 1024 * 1024, // 5GB
            lru,
            current_size: Arc::new(AtomicU64::new(0)),
        })
    }

    pub async fn write(
        &self,
        package_id: &str,
        version: &str,
        content: &[u8],
    ) -> Result<CachedPackage> {
        let path = self.get_path(package_id, version);
        fs::create_dir_all(path.parent().unwrap())?;
        fs::write(&path, content)?;

        // Update LRU
        let entry = CacheEntry {
            package_id: package_id.to_string(),
            version: version.to_string(),
            size: content.len() as u64,
            last_accessed: Instant::now(),
        };

        let key = format!("{}@{}", package_id, version);
        self.lru.lock().unwrap().put(key, entry.clone());

        // Update size
        self.current_size.fetch_add(content.len() as u64, Ordering::Relaxed);

        // Check if eviction needed
        if self.current_size.load(Ordering::Relaxed) > self.max_size {
            self.evict_lru().await?;
        }

        Ok(CachedPackage {
            package_id: package_id.to_string(),
            version: version.to_string(),
            path,
            sha256: self.calculate_hash(&path).await?,
            size: content.len() as u64,
            cached_at: chrono::Utc::now(),
            last_accessed: chrono::Utc::now(),
            manifest: None,
        })
    }

    pub async fn evict_lru(&self) -> Result<Vec<String>> {
        let target_size = (self.max_size as f64 * 0.8) as u64;
        let mut evicted = Vec::new();

        while self.current_size.load(Ordering::Relaxed) > target_size {
            let entry = {
                let mut lru = self.lru.lock().unwrap();
                lru.pop_lru()
            };

            if let Some((key, entry)) = entry {
                let path = self.get_path(&entry.package_id, &entry.version);
                if path.exists() {
                    fs::remove_dir_all(&path)?;
                    self.current_size.fetch_sub(entry.size, Ordering::Relaxed);
                }
                evicted.push(key);
            } else {
                break;
            }
        }

        Ok(evicted)
    }

    fn get_path(&self, package_id: &str, version: &str) -> PathBuf {
        self.cache_dir.join(package_id).join(version)
    }

    async fn calculate_hash(&self, path: &Path) -> Result<String> {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        let content = fs::read(path)?;
        hasher.update(&content);
        Ok(format!("{:x}", hasher.finalize()))
    }
}
```

**Tests**: `cli/tests/domain/marketplace/cache_tests.rs`

```rust
#[tokio::test]
async fn test_lru_eviction() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().to_path_buf(), 1000).unwrap();

    // Write entries exceeding max size
    cache.write("pkg1", "1.0.0", &vec![0u8; 400]).await.unwrap();
    cache.write("pkg2", "1.0.0", &vec![0u8; 400]).await.unwrap();
    cache.write("pkg3", "1.0.0", &vec![0u8; 400]).await.unwrap(); // Should trigger eviction

    // pkg1 should be evicted (LRU)
    assert!(!cache.is_cached("pkg1", "1.0.0"));
    assert!(cache.is_cached("pkg2", "1.0.0"));
    assert!(cache.is_cached("pkg3", "1.0.0"));
}
```

### Phase 3: Version Resolution (2 hours)

**Add to Registry**:

```rust
impl Registry {
    pub async fn resolve_version_constraint(
        &mut self,
        package_id: &str,
        constraint: &str,
    ) -> Result<String> {
        // Handle special cases
        if constraint == "latest" || constraint == "*" {
            let index = self.fetch_index().await?;
            let pkg = index.packs.get(package_id)
                .ok_or_else(|| Error::new(&format!("Package not found: {}", package_id)))?;
            return Ok(pkg.latest_version.clone());
        }

        // Parse semver constraint
        let requirement = semver::VersionReq::parse(constraint)
            .map_err(|e| Error::new(&format!("Invalid version: {}", e)))?;

        // Get package
        let index = self.fetch_index().await?;
        let pkg = index.packs.get(package_id)
            .ok_or_else(|| Error::new(&format!("Package not found: {}", package_id)))?;

        // Find matching version
        let mut versions: Vec<_> = pkg.versions.keys()
            .filter_map(|v| semver::Version::parse(v).ok())
            .collect();

        versions.sort_by(|a, b| b.cmp(a)); // Descending

        for version in versions {
            if requirement.matches(&version) {
                return Ok(version.to_string());
            }
        }

        Err(Error::new(&format!(
            "No version of '{}' matches '{}'",
            package_id, constraint
        )))
    }
}
```

**Tests**:

```rust
#[tokio::test]
async fn test_version_resolution() {
    let mut registry = Registry::new().unwrap();

    // Caret constraint
    let version = registry
        .resolve_version_constraint("io.ggen.rust.cli", "^1.0.0")
        .await
        .unwrap();
    assert!(version.starts_with("1."));

    // Latest
    let latest = registry
        .resolve_version_constraint("io.ggen.rust.cli", "latest")
        .await
        .unwrap();
    assert!(!latest.is_empty());
}
```

### Phase 4: Dependency Resolution (4-5 hours)

**Add DependencyGraph**:

```rust
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    pub root: DependencyNode,
    pub nodes: HashMap<String, DependencyNode>,
    pub resolved: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct DependencyNode {
    pub package_id: String,
    pub version: String,
    pub dependencies: Vec<DependencyEdge>,
    pub depth: usize,
}

#[derive(Debug, Clone)]
pub struct DependencyEdge {
    pub target: String,
    pub constraint: String,
    pub optional: bool,
}

impl DependencyGraph {
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut sorted = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_mark = HashSet::new();

        for package_id in self.nodes.keys() {
            if !visited.contains(package_id) {
                self.visit(package_id, &mut visited, &mut temp_mark, &mut sorted)?;
            }
        }

        sorted.reverse();
        Ok(sorted)
    }

    fn visit(
        &self,
        package_id: &str,
        visited: &mut HashSet<String>,
        temp_mark: &mut HashSet<String>,
        sorted: &mut Vec<String>,
    ) -> Result<()> {
        if temp_mark.contains(package_id) {
            return Err(Error::new(&format!(
                "Circular dependency detected: {}",
                package_id
            )));
        }

        if visited.contains(package_id) {
            return Ok(());
        }

        temp_mark.insert(package_id.to_string());

        if let Some(node) = self.nodes.get(package_id) {
            for edge in &node.dependencies {
                if !edge.optional {
                    self.visit(&edge.target, visited, temp_mark, sorted)?;
                }
            }
        }

        temp_mark.remove(package_id);
        visited.insert(package_id.to_string());
        sorted.push(package_id.to_string());

        Ok(())
    }
}
```

**Tests**:

```rust
#[test]
fn test_topological_sort() {
    let mut graph = DependencyGraph::new("app".to_string(), "1.0.0".to_string());

    // app -> framework -> base
    // Expected order: [base, framework, app]

    let order = graph.topological_sort().unwrap();
    assert_eq!(order, vec!["base", "framework", "app"]);
}

#[test]
fn test_circular_dependency_detection() {
    let mut graph = DependencyGraph::new("app".to_string(), "1.0.0".to_string());

    // app -> lib1 -> lib2 -> lib1 (circular!)

    let result = graph.topological_sort();
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Circular dependency"));
}
```

### Phase 5: Integration with Commands (2-3 hours)

**Update existing command files** to use Registry and CacheManager:

**`cli/src/domain/marketplace/install.rs`**:

```rust
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult> {
    let mut registry = Registry::new()?;
    let cache = CacheManager::new()?;

    // 1. Resolve version
    let resolved = registry.resolve(
        &options.package_name,
        options.version.as_deref(),
    ).await?;

    // 2. Check cache
    if let Some(cached) = cache.read(&resolved.id, &resolved.version).await? {
        println!("Using cached package: {}@{}", resolved.id, resolved.version);
        // Install from cache
        return install_from_cache(&cached, options);
    }

    // 3. Resolve dependencies
    let graph = if options.with_dependencies {
        registry.resolve_dependencies(&resolved.id, Some(&resolved.version)).await?
    } else {
        DependencyGraph::new(resolved.id.clone(), resolved.version.clone())
    };

    // 4. Get install order
    let install_order = graph.topological_sort()?;

    // 5. Download and cache each package
    for package_id in &install_order {
        let version = graph.resolved.get(package_id).unwrap();
        // Download logic here...
        cache.write(package_id, version, &content).await?;
    }

    // 6. Install from cache
    Ok(InstallResult {
        package_name: resolved.id,
        version: resolved.version,
        install_path: /* ... */,
        dependencies_installed: install_order[..install_order.len()-1].to_vec(),
    })
}
```

**`cli/src/domain/marketplace/search.rs`**:

```rust
pub async fn search_packages(
    query: &str,
    filters: &SearchFilters,
) -> Result<Vec<SearchResult>> {
    let mut registry = Registry::new()?;

    // Delegate to registry search
    registry.search_with_filters(query, filters).await
}
```

## Testing Strategy

### Unit Tests
- Version constraint resolution
- LRU eviction logic
- Hash calculation
- Index caching

### Integration Tests
- End-to-end search
- End-to-end install
- Dependency resolution with multiple packages
- Cache eviction under load

### Property Tests (optional)
- Semver constraint matching
- Graph algorithms (topological sort)
- LRU invariants

## Performance Benchmarks

Add to `cli/benches/marketplace_bench.rs`:

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_version_resolution(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let mut registry = Registry::new().unwrap();

    c.bench_function("resolve_version", |b| {
        b.iter(|| {
            rt.block_on(async {
                registry
                    .resolve_version_constraint(
                        black_box("io.ggen.rust.cli"),
                        black_box("^1.0.0"),
                    )
                    .await
            })
        });
    });
}

fn bench_cache_read(c: &mut Criterion) {
    let cache = CacheManager::new().unwrap();
    let rt = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("cache_read", |b| {
        b.iter(|| {
            rt.block_on(async {
                cache.read(black_box("test-pkg"), black_box("1.0.0")).await
            })
        });
    });
}

criterion_group!(benches, bench_version_resolution, bench_cache_read);
criterion_main!(benches);
```

## Common Pitfalls

### 1. Index Caching Race Conditions
**Problem**: Multiple concurrent fetches of index
**Solution**: Use `Mutex` or `RwLock` around index_cache

### 2. Cache Size Overflow
**Problem**: current_size becomes inaccurate
**Solution**: Recalculate size periodically with `calculate_size()`

### 3. Dependency Cycles Not Detected
**Problem**: Infinite loop in resolution
**Solution**: Use temp_mark set in DFS (see topological_sort)

### 4. Yanked Versions Still Resolved
**Problem**: User gets unstable versions
**Solution**: Filter out yanked versions in resolution

### 5. Hash Calculation Memory Issues
**Problem**: Loading 1GB file into memory
**Solution**: Use streaming hash calculation (see architecture doc)

## Debugging Tips

### Enable Tracing
```rust
use tracing::{info, warn, error};

#[tracing::instrument]
pub async fn resolve(&mut self, package_id: &str) -> Result<ResolvedPackage> {
    info!(package_id, "Resolving package");
    // ...
}
```

### Cache Inspection
```bash
# View cache contents
ls -lh ~/.cache/ggen/marketplace/

# Check cache size
du -sh ~/.cache/ggen/marketplace/

# Clear cache
rm -rf ~/.cache/ggen/marketplace/
```

### Registry Debugging
```bash
# Test registry directly
curl https://seanchatmangpt.github.io/ggen/registry/index.json | jq .

# Set custom registry
export GGEN_REGISTRY_URL=file:///path/to/test/registry/
```

## Next Steps After Implementation

1. **Documentation**: Add rustdoc comments to all public APIs
2. **Examples**: Create usage examples in `examples/`
3. **CLI Help**: Update help text for marketplace commands
4. **Error Messages**: Improve user-facing error messages
5. **Performance**: Run benchmarks and optimize hot paths
6. **Security**: Audit dependency resolution for supply chain attacks

## Resources

- **Cargo Implementation**: Study cargo's registry/cache (for inspiration)
- **Semver Spec**: https://semver.org/
- **LRU Crate**: https://docs.rs/lru/
- **SHA2 Crate**: https://docs.rs/sha2/

## Questions?

Refer to:
1. Full architecture doc (marketplace-registry-cache-architecture.md)
2. Existing implementations (ggen-core/src/registry.rs, ggen-core/src/cache.rs)
3. Examples (examples/advanced-cache-registry/)
4. Visual diagrams (marketplace-architecture-diagram.md)

---

**Estimated Total Time**: 15-20 hours for complete implementation + testing

**Completion Checklist**:
- [ ] Phase 1: Registry wrapper
- [ ] Phase 2: CacheManager with LRU
- [ ] Phase 3: Version resolution
- [ ] Phase 4: Dependency resolution
- [ ] Phase 5: Command integration
- [ ] Unit tests (>80% coverage)
- [ ] Integration tests
- [ ] Performance benchmarks
- [ ] Documentation
