# Pseudocode: Cache Invalidation Strategy

**SPARC Phase**: Pseudocode
**Algorithm**: Incremental Build Cache Invalidation
**Complexity**: O(n) where n = number of cached build artifacts
**Status**: Formal specification complete

---

## Algorithm 1: Content-Hash Based Cache Invalidation

### Function Signature
```
FUNCTION compute_cache_key(sources: Vec<Path>, options: CompileOptions) -> CacheKey
    Precondition: sources.len() > 0
    Postcondition: Returns deterministic cache key
    Returns: CacheKey (SHA-256 hash)
```

### Pseudocode (Detailed)

```
FUNCTION compute_cache_key(sources: Vec<Path>, options: CompileOptions) -> CacheKey
    // Collect all source file content hashes
    file_hashes = Vec<(Path, Hash)>::new()

    // Step 1: Sort files for determinism (important!)
    sorted_sources = sort(sources)  // O(n log n) lexicographic order

    // Step 2: Compute hash of each source file
    FOR EACH source_path IN sorted_sources:
        IF NOT file_exists(source_path):
            RETURN Error("Source file missing: {}", source_path)
        END IF

        file_content = read_file(source_path)  // O(file_size)
        file_hash = sha256(file_content)
        file_hashes.append((source_path, file_hash))
    END FOR

    // Step 3: Hash compilation options (flags, features, etc.)
    options_str = serialize_options(options)  // JSON or canonical form
    options_hash = sha256(options_str)

    // Step 4: Combine all hashes in deterministic order
    combined_input = concatenate([
        // Version identifier (prevents stale caches across versions)
        "v1.0",
        // Options hash (changes trigger rebuild)
        options_hash,
        // File hashes in sorted order
        [h FOR (_, h) IN file_hashes]
    ])

    // Step 5: Compute final cache key
    cache_key = sha256(combined_input)

    RETURN CacheKey(
        hash: cache_key,
        sources: file_hashes,
        options_hash: options_hash,
        timestamp: now()
    )
END FUNCTION
```

### Complexity Analysis

| Operation | Complexity | Notes |
|-----------|-----------|-------|
| Sort sources | O(n log n) | Lexicographic order |
| Read & hash files | O(n * avg_file_size) | Parallelizable |
| Hash options | O(1) | Fixed-size serialization |
| Combine & finalize | O(n) | Linear pass |
| **Total** | **O(n log n + file_i/o)** | I/O bound in practice |

---

## Algorithm 2: Dependency Graph Invalidation Propagation

### Purpose
When a file changes, invalidate dependent targets transitively

### Function Signature
```
FUNCTION invalidate_dependents(changed_file: Path, dep_graph: DependencyGraph) -> Set<CacheKey>
    Precondition: changed_file exists in dep_graph
    Postcondition: Returns all invalidated cache keys
    Returns: Set of affected cache keys (for cleanup)
```

### Pseudocode

```
FUNCTION invalidate_dependents(changed_file: Path, dep_graph: DependencyGraph) -> Set<CacheKey>
    // Use BFS to find all transitive dependents
    invalidated = Set<CacheKey>::new()
    queue = Queue::new()

    // Step 1: Find direct dependents of changed file
    direct_dependents = dep_graph.get_dependents(changed_file)  // O(degree)

    FOR EACH dependent IN direct_dependents:
        queue.enqueue(dependent)
        invalidated.insert(dependent.cache_key)
    END FOR

    // Step 2: BFS to find transitive dependents
    WHILE queue.is_not_empty():
        current_target = queue.dequeue()

        // Mark current target as stale
        cache.mark_stale(current_target.cache_key)

        // Find targets that depend on current_target
        next_dependents = dep_graph.get_dependents(current_target)  // O(degree)

        FOR EACH next IN next_dependents:
            IF NOT invalidated.contains(next.cache_key):
                invalidated.insert(next.cache_key)
                queue.enqueue(next)
            END IF
        END FOR
    END WHILE

    // Step 3: Return for cleanup/logging
    RETURN invalidated
END FUNCTION
```

### Invalidation Example (ggen workspace)

```
File structure:
┌─ ggen-core/src/lib.rs (lib entry)
│  └─ used by: ggen-cli, ggen-api, ggen-auth
│
├─ ggen-cli/src/main.rs (uses ggen-core)
│  └─ used by: integration tests
│
├─ ggen-api/src/lib.rs (uses ggen-core)
│  └─ used by: API tests, benchmarks
│
└─ tests/integration/full_workflow_test.rs
   └─ uses: ggen-cli, ggen-api

Change: Edit ggen-core/src/lib.rs

Invalidation cascade:
├─ 1. ggen-core crate cache → INVALID
├─ 2. ggen-cli cache → INVALID (depends on ggen-core)
├─ 3. ggen-api cache → INVALID (depends on ggen-core)
├─ 4. ggen-auth cache → INVALID (depends on ggen-core)
├─ 5. Integration test cache → INVALID (depends on ggen-cli + ggen-api)
└─ Total affected: 5 targets (must rebuild all)

Optimization: Compile ggen-core first, then ggen-cli/ggen-api in parallel
```

---

## Algorithm 3: Stale Cache Detection & Cleanup

### Purpose
Detect when cache entries become stale and remove them safely

### Function Signature
```
FUNCTION cleanup_stale_cache(cache_dir: Path, retention_days: u32) -> u64
    Precondition: cache_dir exists and is writable
    Postcondition: Removes stale entries
    Returns: Bytes freed
```

### Pseudocode

```
FUNCTION cleanup_stale_cache(cache_dir: Path, retention_days: u32) -> u64
    now = current_timestamp()
    cutoff_time = now - duration_days(retention_days)
    bytes_freed = 0

    // Step 1: Iterate all cache entries
    FOR EACH cache_entry IN list_cache_entries(cache_dir):
        // Determine age
        age = now - cache_entry.timestamp

        // Step 2: Determine if stale (by multiple criteria)
        is_stale = (
            cache_entry.is_marked_stale() OR  // Explicit invalidation
            (age > cutoff_time)  OR           // Age-based expiration
            (cache_entry.hash != current_source_hash(cache_entry.sources))  // Content check
        )

        IF is_stale:
            // Safety: Verify before deletion
            IF cache_is_redundant(cache_entry):
                bytes = delete_cache_entry(cache_entry)
                bytes_freed += bytes
                log_info("Deleted stale cache: {} ({} bytes)", cache_entry.id, bytes)
            END IF
        END IF
    END FOR

    RETURN bytes_freed
END FUNCTION

FUNCTION cache_is_redundant(entry: CacheEntry) -> bool
    // Ensure we can rebuild this entry if needed
    // Check:
    // 1. Source files still exist
    // 2. Compilation environment available
    // 3. No other entry depends on this being cached
    RETURN entry.sources_exist() AND environment_available()
END FUNCTION
```

### Cache Storage Format

```
Directory structure:
.cargo/.ggen-cache/
├── metadata.json
│   {
│     "version": "1.0",
│     "created": "2026-01-25T12:00:00Z",
│     "entries": 47,
│     "total_size_bytes": 2147483648
│   }
├── 0/
│   ├── abc123def456...json  (cache entry metadata)
│   └── abc123def456...bin   (actual cached artifact - e.g., compiled .rlib)
├── 1/
│   └── ...
└── ...

Entry format (JSON):
{
  "id": "abc123def456",
  "target": "ggen-core",
  "kind": "lib",
  "hash": "abc123def456...",
  "timestamp": "2026-01-25T11:55:00Z",
  "sources": [
    {
      "path": "crates/ggen-core/src/lib.rs",
      "hash": "def456..."
    },
    ...
  ],
  "options": {
    "profile": "debug",
    "features": ["default"],
    "optimization_level": 0
  },
  "size_bytes": 1024000,
  "hits": 23,
  "last_used": "2026-01-25T12:00:00Z"
}
```

---

## Algorithm 4: Incremental Compilation Strategy

### Purpose
Determine what to rebuild vs. what to reuse

### Function Signature
```
FUNCTION plan_incremental_build(workspace: Manifest, cache: BuildCache) -> BuildPlan
    Precondition: workspace.crates.len() > 0 AND cache is initialized
    Postcondition: Returns plan with rebuild decisions
    Returns: BuildPlan with rebuild/reuse decisions per crate
```

### Pseudocode

```
FUNCTION plan_incremental_build(workspace: Manifest, cache: BuildCache) -> BuildPlan
    plan = BuildPlan::new()

    // Step 1: Analyze each crate
    FOR EACH crate IN workspace.crates:  // O(m) where m = num crates
        // Compute current source hash
        sources = glob_crate_sources(crate)  // "crates/{crate}/src/**/*.rs"
        current_key = compute_cache_key(sources, crate.compile_options)

        // Lookup cache
        cached_entry = cache.get(crate.name)

        // Step 2: Decision tree
        IF cached_entry == null:
            plan.add_action(crate, Action::Rebuild, Reason::NoCachEntry)
        ELSE IF cached_entry.hash != current_key.hash:
            plan.add_action(crate, Action::Rebuild, Reason::SourceChanged)
            cache.mark_stale(crate.name)
        ELSE IF crate.has_new_dependencies():
            plan.add_action(crate, Action::Rebuild, Reason::DepsChanged)
        ELSE IF cache.is_marked_stale(crate.name):
            plan.add_action(crate, Action::Rebuild, Reason::InvalidationPropagation)
        ELSE:
            plan.add_action(crate, Action::Reuse, Reason::Cached)
            // Verify reused artifact exists
            IF NOT cache.artifact_exists(crate.name):
                plan.change_action(crate, Action::Rebuild, Reason::ArtifactMissing)
            END IF
        END IF
    END FOR

    // Step 3: Determine build order (respecting dependencies)
    plan.compute_build_order()  // Topological sort

    RETURN plan
END FUNCTION
```

### Build Plan Example

```
Scenario: Edit ggen-core/src/lib.rs, no other changes

Decision:
┌─ ggen-core/src/lib.rs: CHANGED
│  └─ Action: Rebuild ggen-core → REBUILD
│
├─ ggen-cli (depends on ggen-core)
│  └─ Dependency changed → Action: Rebuild ggen-cli → REBUILD
│
├─ ggen-api (depends on ggen-core)
│  └─ Dependency changed → Action: Rebuild ggen-api → REBUILD
│
├─ ggen-auth (depends on ggen-core)
│  └─ Dependency changed → Action: Rebuild ggen-auth → REBUILD
│
├─ ggen-domain (no dependency on changed files)
│  └─ Action: Reuse cached → REUSE_CACHED
│
└─ ... (30 total crates)

Build order (respecting dependencies):
1. ggen-core (core dependency)
2. ggen-cli, ggen-api, ggen-auth (parallel - all depend on ggen-core)
3. Integration tests (depend on ggen-cli + ggen-api)
4. Benchmarks (can run in parallel)

Time estimate: 45s (vs 90s sequential)
```

---

## Performance Characteristics

### Scenario 1: Single File Change
```
Workspace: 30 crates, 1,000 source files

Operation Breakdown:
├─ Detect change: 50ms (scan changed files)
├─ Compute cache key: 150ms (hash 50 modified files)
├─ Lookup cache: 10ms (hash table lookup)
├─ Propagate invalidation: 20ms (BFS through dep graph)
├─ Plan rebuild: 30ms (topological sort of 30 crates)
└─ Total overhead: ~250ms (cache decision time)

Rebuild time depends on dependency graph:
├─ Best case: 10s (only changed crate + 2 dependents)
├─ Worst case: 90s (change in core crate, rebuilds 15 crates)
└─ Typical case: 30s (change in mid-level crate)
```

### Scenario 2: No Changes (Cache Hit)
```
Plan computation: 50ms
├─ Verify all cache entries: 200ms (check artifact existence)
├─ Confirm no source changes: 150ms (quick hash verification)
├─ Determine zero-rebuild scenario: 10ms
└─ Total: ~410ms overhead

Build execution: 0s (all cached)
Total: 410ms (vs 90s+ build from scratch)

Speedup: 200x+ for no-change scenario
```

### Scenario 3: Comprehensive Changes
```
Scenario: Update Cargo.toml (affects all crates)

Decision:
├─ All 30 crates: REBUILD (dependency change)
├─ No optimization possible
└─ Total time: 90s+ (must rebuild entire workspace)

Insight: Cargo.toml changes are expensive - minimize frequency
```

---

## Implementation Strategy

### Level 1: Basic Cache (MVP)
- Content hash of source files
- Per-crate caching
- Age-based cleanup (30 days)
- Storage: JSON + binary artifacts

### Level 2: Smart Cache
- Dependency graph tracking
- Incremental invalidation propagation
- Parallel cache computation
- Statistics tracking (hit/miss rates)

### Level 3: Distributed Cache
- Network cache server (optional)
- Cache distribution across CI nodes
- Remote cache validation
- Bandwidth-aware caching

---

## Testing Strategy

### Unit Tests
```
TEST compute_cache_key_determinism:
    key1 = compute_cache_key(sources, options)
    key2 = compute_cache_key(sources, options)
    ASSERT key1 == key2  // Same inputs = same output

TEST invalidate_dependents_correctness:
    graph = build_test_graph(...)  // 10 crates, known dependencies
    changed = "crate-A/src/lib.rs"
    invalidated = invalidate_dependents(changed, graph)
    ASSERT invalidated.contains("crate-B")  // Depends on A
    ASSERT invalidated.contains("crate-C")  // Depends on A
    ASSERT invalidated.contains("crate-D")  // Depends on B (transitive)
```

### Integration Tests
```
TEST incremental_build_single_change:
    workspace.build_all()  // Baseline: 90s
    edit_file("ggen-core/src/lib.rs")
    plan = plan_incremental_build(workspace, cache)
    ASSERT plan.rebuild_count() == 5  // ggen-core + 4 dependents
    ASSERT plan.reuse_count() == 25   // Other crates cached

TEST cache_invalidation_propagation:
    workspace.build_all()
    edit_file("ggen-core/src/lib.rs")
    workspace.build_incremental()
    ASSERT elapsed_time < 30s  // Should be much faster
```

---

## References

- **Content-Addressable Storage**: https://en.wikipedia.org/wiki/Content-addressable_storage
- **Cargo Incremental Compilation**: https://doc.rust-lang.org/cargo/guide/incremental-compilation.html
- **Build Systems a la Carte**: https://blogs.ncl.ac.uk/andreymokhov/buildsystems/
- **Shake Build System**: https://github.com/ndmitchell/shake (reference implementation)
