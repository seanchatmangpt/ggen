# Architecture: Build Cache Layer Design

**SPARC Phase**: Architecture
**Component**: Incremental Compilation Caching
**Status**: Design complete and approved
**Implementation**: Planned for Phase 2 (future optimization)

---

## 1. Overview

### Purpose
Enable incremental builds by caching compilation artifacts and skipping unchanged tasks.

### Key Metrics
- Cache hit rate target: 80% in typical workflows
- Cache overhead: <500ms (validation time)
- Storage: ~2GB per workspace
- Time saved: 60% reduction in incremental builds

---

## 2. Cache Architecture

### 2.1 Storage Layer

```
Directory Structure:
.cargo/.ggen-cache/
├── metadata.json          # Cache index and statistics
├── v1/                    # Version 1 schema
│   ├── {hash}/
│   │   ├── manifest       # JSON metadata
│   │   ├── artifacts/     # Compiled .rlib, .so files
│   │   └── hashes.json    # Content verification
│   └── ...
└── cleanup-log.json       # Audit trail for cache maintenance
```

### 2.2 Metadata Format

```json
{
  "version": "1.0",
  "created": "2026-01-25T12:00:00Z",
  "crate": "ggen-core",
  "source_hash": "abc123def456...",
  "options": {
    "profile": "debug",
    "features": ["default", "logging"],
    "target": "x86_64-unknown-linux-gnu"
  },
  "artifacts": {
    "rlib": {
      "path": "v1/{hash}/artifacts/libggen_core.rlib",
      "size_bytes": 2048000,
      "hash": "xyz789..."
    },
    "metadata": {
      "path": "v1/{hash}/artifacts/libggen_core.rmeta",
      "size_bytes": 512000,
      "hash": "uvw456..."
    }
  },
  "dependencies": {
    "ggen-utils": "abc123...",
    "serde": "def456..."
  },
  "timestamps": {
    "cached": "2026-01-25T11:55:00Z",
    "last_used": "2026-01-25T12:00:00Z",
    "accessed_count": 5
  }
}
```

---

## 3. Cache Decision Workflow

### 3.1 Cache Lookup Path

```
FUNCTION should_use_cache(crate: Crate) -> bool:
    1. Compute source hash
       ├─ sha256(all *.rs files in crate)
       └─ O(num_files * avg_file_size) ≈ 150ms for ggen-core

    2. Lookup in cache index
       ├─ Hash table lookup
       └─ O(1) amortized

    3. Verify cache validity
       ├─ Source files still exist
       ├─ Dependencies match
       └─ Cache entry not marked stale

    4. Verify artifact integrity
       ├─ Check cached .rlib file exists
       ├─ Verify content hash matches
       └─ Check permissions/accessibility

    5. Decision:
       ├─ ALL VALID → Use cached artifacts (skip compilation)
       └─ ANY INVALID → Recompile and update cache
```

### 3.2 Cache Hit/Miss Scenarios

```
Scenario 1: No changes (best case)
├─ Source hash: SAME
├─ Dependencies: SAME
├─ Artifacts: PRESENT
└─ Result: CACHE HIT → Skip compilation (saves 20s for ggen-core)

Scenario 2: Edit single file (common case)
├─ Source hash: DIFFERENT
├─ Dependencies: SAME
├─ Artifacts: OUTDATED
└─ Result: CACHE MISS → Recompile (but can use dep artifacts)

Scenario 3: Dependency changed (transitive invalidation)
├─ Source hash: SAME (didn't edit this crate)
├─ Dependencies: DIFFERENT (dependency was rebuilt)
├─ Artifacts: OUTDATED (incompatible)
└─ Result: CACHE MISS → Recompile (necessary)

Scenario 4: Feature flags changed (Cargo.toml modified)
├─ Source hash: SAME
├─ Options hash: DIFFERENT (features changed)
├─ Artifacts: INCOMPATIBLE
└─ Result: CACHE MISS → Recompile
```

---

## 4. Incremental Invalidation

### 4.1 Dependency Graph Tracking

```
Cache entry includes dependency tree:

ggen-core cached:
├─ Depends on: ggen-utils (hash: abc123)
├─ Depends on: serde (hash: def456)
├─ Depends on: tokio (hash: ghi789)
└─ Timestamp: 2026-01-25T11:55:00Z

When ggen-utils rebuilt:
├─ Hash changes from abc123 → xyz999
├─ Invalidate: All crates that depend on ggen-utils
│  ├─ ggen-cli → INVALIDATE
│  ├─ ggen-api → INVALIDATE
│  └─ ggen-auth → INVALIDATE
└─ Pattern: Transitive invalidation via dependency graph
```

### 4.2 Stale Cache Cleanup

```
CRON JOB (hourly or on-demand):

FUNCTION cleanup_stale_cache():
    1. Scan all cache entries
    2. For each entry:
       ├─ Check: Is source still accessible?
       ├─ Check: Age > 30 days?
       ├─ Check: Last used > 7 days ago?
       └─ If ANY true → MARK for deletion
    3. Verify safety
       ├─ Can rebuild from sources? YES → Safe to delete
       └─ Cannot rebuild? NO → Keep cache
    4. Delete marked entries
    5. Log cleanup audit trail
```

---

## 5. Integration Points

### 5.1 Cargo Make Integration

```toml
[tasks.cache-check]
description = "Verify cache integrity and rebuild if needed"
workspace = false
script_runner = "@shell"
script = '''
#!/bin/bash
# Check each crate's cache status
for crate in ggen-core ggen-cli ggen-domain; do
    hash=$(sha256sum crates/$crate/src/**/*.rs | sha256sum | cut -d' ' -f1)
    cached=$(jq -r ".entries.$crate.hash" .cargo/.ggen-cache/metadata.json)
    if [ "$hash" = "$cached" ]; then
        echo "✓ $crate: CACHE HIT (skip compilation)"
    else
        echo "✗ $crate: CACHE MISS (recompile)"
    fi
done
'''

[tasks.cache-clean]
description = "Clean up stale cache entries"
command = "timeout"
args = ["10s", "cargo", "clean"]
```

### 5.2 CI/CD Integration

```yaml
# .github/workflows/ci.yml

- name: Restore build cache
  uses: actions/cache@v3
  with:
    path: .cargo/.ggen-cache
    key: ${{ runner.os }}-ggen-${{ hashFiles('Cargo.lock') }}
    restore-keys: |
      ${{ runner.os }}-ggen-

- name: Build with cache
  run: cargo make build

- name: Upload cache
  uses: actions/cache/save@v3
  if: always()
```

---

## 6. Performance Characteristics

### 6.1 Cache Overhead Analysis

```
Clean build (no cache):
├─ ggen-core: 20s
├─ ggen-cli: 5s
└─ Total: 25s (core only)

Incremental build with cache:
├─ Source hash computation: 150ms
├─ Cache lookup: 10ms
├─ Artifact verification: 50ms
├─ Cache check total: 210ms
├─ Actual build: 25s (only changed crates)
└─ Total overhead: 0.2s (negligible)

Speedup factors:
├─ Cache hit (all cached): 25s / 0.2s = 125x
├─ Partial hit (1 of 4 crates changed): 25s / (5s + 0.2s) = 4.6x
└─ Worst case (all changed): 25s / 25s = 1.0x (no benefit)
```

### 6.2 Storage Growth

```
Per-crate cache size:
├─ ggen-core: 50MB (.rlib + .rmeta + metadata)
├─ ggen-cli: 30MB
├─ ggen-domain: 20MB
├─ (25 other crates): 500MB
└─ Total: ~2GB

Cleanup strategy:
├─ Keep only recent 10 builds per crate
├─ Age-based: Delete entries >30 days old
├─ Space-based: Keep total <5GB
└─ Result: Bounded growth, automatic maintenance
```

---

## 7. Implementation Phases

### Phase 1 (Months 1-2): Foundation
- [ ] Implement cache metadata storage (JSON)
- [ ] Add source hash computation
- [ ] Basic cache hit/miss decision logic
- [ ] Artifact verification

### Phase 2 (Months 2-3): Dependency Tracking
- [ ] Dependency graph tracking
- [ ] Transitive invalidation logic
- [ ] Cache cleanup automation

### Phase 3 (Months 3-4): Integration
- [ ] Cargo make integration
- [ ] GitHub Actions CI/CD cache integration
- [ ] Performance monitoring and tuning

---

## 8. References

- **Cargo Incremental Compilation**: https://doc.rust-lang.org/cargo/guide/incremental-compilation.html
- **Bazel Build Cache**: https://bazel.build/docs/bazel-user-manual.html#caching
- **Content-Addressable Storage**: https://en.wikipedia.org/wiki/Content-addressable_storage
