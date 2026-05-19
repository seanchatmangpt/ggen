# Marketplace Architecture Documentation Index

**Version**: 1.0.0
**Date**: 2025-11-02
**Status**: Implementation-Ready
**Total Documentation**: 2,798 lines across 4 documents

---

## Quick Navigation

### For Architects & Reviewers
Start with → **Architecture Summary** (5-10 min read)

### For Implementers
Start with → **Implementation Guide** (15-20 min read)

### For Visual Learners
Start with → **Architecture Diagrams** (10-15 min read)

### For Deep Dive
Read → **Full Architecture Specification** (45-60 min read)

---

## Documentation Files

### 1. Architecture Summary (Quick Reference)
**File**: `marketplace-architecture-summary.md` (299 lines, 7.5KB)

**Contents**:
- Core components overview (Registry, CacheManager)
- Data structures at a glance
- Index file format (JSON)
- Version resolution rules (semver)
- Error handling patterns
- Performance targets
- Dependencies required
- Implementation phases checklist

**Best For**: Quick reference, onboarding new team members

**Reading Time**: 5-10 minutes

---

### 2. Full Architecture Specification
**File**: `marketplace-registry-cache-architecture.md` (1,417 lines, 41KB)

**Contents**:
1. Architecture Overview
2. Registry Architecture
   - Complete struct definition
   - 40+ method signatures
   - Supporting types
3. CacheManager Architecture
   - Complete struct definition
   - LRU implementation details
   - Supporting types
4. Package Metadata Schema
   - Complete PackageMetadata struct
   - VersionMetadata
   - Dependency specification
5. Index File Format
   - Full JSON structure
   - JSON Schema validation
6. Version Resolution Logic
   - Semver constraint algorithm
   - Constraint examples table
7. Dependency Graph Structure
   - Graph representation
   - Topological sort algorithm
   - Conflict detection
8. Error Handling Strategy
   - MarketplaceError enum
   - Error context patterns
   - Retry logic
9. Performance Considerations
   - Index caching strategy
   - LRU eviction (O(1))
   - Parallel downloads
   - Streaming hash calculation
   - Performance benchmarks table
10. Implementation Checklist
    - 6 phases with tasks

**Best For**: Complete reference, design reviews, technical specifications

**Reading Time**: 45-60 minutes

---

### 3. Architecture Diagrams (Visual Guide)
**File**: `marketplace-architecture-diagram.md` (433 lines, 34KB)

**Contents**:
- System Architecture Overview (ASCII diagram)
- Registry Architecture Details (box diagram)
- CacheManager Architecture Details (box diagram)
- Dependency Resolution Flow (flowchart)
- Version Resolution Algorithm (flowchart)
- Topological Sort (DFS visualization)
- LRU Eviction Strategy (visual explanation)
- Data Flow: Install Command (step-by-step diagram)

**Best For**: Understanding system flow, visual learners, presentations

**Reading Time**: 10-15 minutes

---

### 4. Implementation Guide (Step-by-Step)
**File**: `marketplace-implementation-guide.md` (649 lines, 17KB)

**Contents**:
1. Quick Start (3-step guide)
2. Implementation Order
   - Phase 1: Registry Wrapper (2-3h) with code examples
   - Phase 2: CacheManager + LRU (3-4h) with code examples
   - Phase 3: Version Resolution (2h) with code examples
   - Phase 4: Dependency Resolution (4-5h) with code examples
   - Phase 5: Command Integration (2-3h) with code examples
3. Testing Strategy
   - Unit test examples
   - Integration test examples
   - Property test guidelines
4. Performance Benchmarks
   - Criterion benchmark code
5. Common Pitfalls
   - 5 major issues and solutions
6. Debugging Tips
   - Tracing, cache inspection, registry debugging
7. Next Steps After Implementation
8. Resources & Links

**Best For**: Implementers, coding agents, step-by-step development

**Reading Time**: 15-20 minutes

**Estimated Implementation Time**: 15-20 hours total

---

## Key Design Principles

### 1. Separation of Concerns
- **Registry**: Metadata discovery, version resolution, dependency management
- **CacheManager**: Local storage, LRU eviction, integrity verification

### 2. Backward Compatibility
- Wraps existing `ggen_core::RegistryClient` (no duplication)
- Compatible with current index format
- Extends functionality without breaking changes

### 3. Performance First
- In-memory index caching (5 min TTL)
- O(1) LRU eviction using `lru` crate
- Streaming hash calculation (constant memory)
- Parallel downloads (4 concurrent)

### 4. Deterministic & Secure
- SHA-256 verification for all packages
- Semver constraint resolution
- Circular dependency detection
- Conflict detection

---

## Implementation Phases Summary

| Phase | Component | Time | Complexity | Tests |
|-------|-----------|------|------------|-------|
| 1 | Registry wrapper | 2-3h | Low | Unit |
| 2 | CacheManager + LRU | 3-4h | Medium | Unit + Integration |
| 3 | Version resolution | 2h | Medium | Unit + Property |
| 4 | Dependency graph | 4-5h | High | Unit + Integration |
| 5 | CLI integration | 2-3h | Low | Integration + E2E |
| **Total** | **Complete system** | **15-20h** | - | **Full suite** |

---

## Data Structures Overview

### Registry
```rust
struct Registry {
    client: RegistryClient,          // Delegates to ggen-core
    index_cache: Option<RegistryIndex>, // 5-min TTL cache
    cache_ttl: Duration,
    last_fetch: Option<Instant>,
}
```

### CacheManager
```rust
struct CacheManager {
    cache_dir: PathBuf,              // ~/.cache/ggen/marketplace
    max_size: u64,                   // Default: 5GB
    lru: Arc<Mutex<LruCache>>,       // O(1) eviction
    current_size: Arc<AtomicU64>,    // Atomic size tracking
}
```

### DependencyGraph
```rust
struct DependencyGraph {
    root: DependencyNode,
    nodes: HashMap<String, DependencyNode>,
    resolved: HashMap<String, String>,
}
```

---

## Performance Targets

| Operation | Target Latency | Strategy |
|-----------|----------------|----------|
| Search (cached) | <10ms | In-memory index |
| Search (cold) | <300ms | Network + parse |
| Resolve version | <5ms | Semver matching |
| Cache check | <1ms | Filesystem stat |
| Download (1MB) | <2s | Network dependent |
| Hash (1MB) | <50ms | Streaming SHA-256 |
| LRU eviction | <10ms | O(1) with lru crate |
| Dep resolution (10) | <100ms | BFS traversal |

---

## Dependencies Required

Add to `cli/Cargo.toml`:

```toml
[dependencies]
# Version resolution
semver = "1.0"

# LRU cache
lru = "0.12"

# Hashing
sha2 = "0.10"

# From workspace (already present)
ggen-core = { path = "../ggen-core", version = "2.2.0" }
tokio = { workspace = true }
reqwest = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
anyhow = { workspace = true }
thiserror = { workspace = true }
chrono = { workspace = true }
```

---

## Target Implementation Location

**File**: `/Users/sac/ggen/cli/src/domain/marketplace/mod.rs`

**Current State** (lines 35-38):
```rust
#[derive(Debug, Clone)]
pub struct Registry;

#[derive(Debug, Clone)]
pub struct CacheManager;
```

**After Implementation**:
- Replace with full Registry struct (~150 lines)
- Replace with full CacheManager struct (~200 lines)
- Add supporting types (~100 lines)
- Total: ~450 lines of implementation

---

## Related Existing Code

### Reference Implementations
1. **ggen-core/src/registry.rs** (828 lines)
   - RegistryClient implementation
   - Index fetching with retry
   - Search functionality
   - Used as backend by our Registry

2. **ggen-core/src/cache.rs** (313 lines)
   - Basic CacheManager for gpacks
   - SHA-256 verification
   - Git cloning logic
   - Reference for our marketplace cache

### Usage Examples
3. **examples/advanced-cache-registry/src/cache_demo.rs** (284 lines)
   - Cache usage patterns
   - Performance demonstrations
   - Invalidation strategies

4. **examples/advanced-cache-registry/src/registry_demo.rs** (349 lines)
   - Registry usage patterns
   - Version management
   - Dependency demonstrations

---

## Testing Strategy

### Unit Tests (~40 tests)
- Version constraint resolution (exact, caret, tilde, range, latest)
- LRU eviction logic
- Hash calculation
- Index caching with TTL
- Topological sort
- Circular dependency detection

### Integration Tests (~15 tests)
- End-to-end search flow
- End-to-end install flow
- Dependency resolution with real packages
- Cache eviction under load
- Multi-package installations

### Property Tests (~5 tests)
- Semver constraint matching (proptest)
- Graph algorithms invariants
- LRU ordering properties

### Performance Benchmarks (~8 benchmarks)
- Search latency (cached vs cold)
- Version resolution speed
- Cache read/write throughput
- Hash calculation speed
- LRU eviction performance
- Dependency resolution scaling

**Total Test Coverage Target**: >80%

---

## Documentation Standards

### Code Comments
- All public APIs must have rustdoc comments
- Include examples in doc comments
- Document error conditions
- Link to architecture docs

### Example
```rust
/// Resolve a package to a specific version.
///
/// # Arguments
/// * `package_id` - Package identifier (e.g., "io.ggen.rust.cli")
/// * `version` - Version constraint (e.g., "^1.0.0", "latest", "1.2.3")
///
/// # Returns
/// Resolved package with specific version and download metadata
///
/// # Errors
/// - `PackageNotFound` if package doesn't exist in registry
/// - `VersionNotFound` if no version matches constraint
/// - `NetworkError` if registry is unreachable
///
/// # Examples
/// ```
/// let mut registry = Registry::new()?;
/// let resolved = registry.resolve("io.ggen.rust.cli", Some("^1.0.0")).await?;
/// println!("Resolved to version: {}", resolved.version);
/// ```
///
/// See: [Architecture Documentation](../docs/marketplace-registry-cache-architecture.md)
pub async fn resolve(
    &mut self,
    package_id: &str,
    version: Option<&str>,
) -> Result<ResolvedPackage>
```

---

## Common Questions

### Q: Why wrap RegistryClient instead of using it directly?
**A**: Separation of concerns. CLI domain layer adds caching, filtering, and higher-level abstractions while ggen-core provides low-level registry access.

### Q: Why use `lru` crate instead of custom implementation?
**A**: Battle-tested, O(1) operations, thread-safe. Custom LRU would take 2-3 hours with potential bugs.

### Q: How is this different from Cargo's registry?
**A**: Similar concepts but adapted for ggen templates. Key differences: git-based packages, template merging, knowledge graph metadata.

### Q: What happens on cache corruption?
**A**: SHA-256 verification detects corruption. Corrupted entries are removed and re-downloaded.

### Q: How are circular dependencies handled?
**A**: Detected during topological sort via temp_mark set. Returns error with cycle information.

---

## Status & Next Steps

### ✅ Completed
- [x] Architecture design
- [x] All struct definitions
- [x] All method signatures
- [x] Algorithm specifications
- [x] Error handling design
- [x] Performance targets
- [x] Testing strategy
- [x] Implementation guide with code examples
- [x] Visual diagrams

### 🔄 Ready for Implementation
- [ ] Phase 1: Registry wrapper
- [ ] Phase 2: CacheManager + LRU
- [ ] Phase 3: Version resolution
- [ ] Phase 4: Dependency graph
- [ ] Phase 5: CLI integration
- [ ] Unit tests
- [ ] Integration tests
- [ ] Performance benchmarks
- [ ] Documentation

### 🎯 Success Criteria
1. All 40+ methods implemented and tested
2. >80% test coverage
3. All performance targets met
4. Zero compilation warnings
5. Full rustdoc documentation
6. Passing CI/CD pipeline

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-11-02 | System Architect | Initial architecture design complete |

---

## Contact & Support

For questions about this architecture:
1. Read the relevant documentation section
2. Check existing implementations in ggen-core
3. Review examples in examples/advanced-cache-registry
4. Consult the implementation guide for specific code examples

---

**End of Index**
