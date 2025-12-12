# Marketplace V2 Migration Architecture

**Status:** ✅ COMPLETE - v1 Removed, v2 Only
**Last Updated:** 2025-01-XX

## Overview

This document describes the completed marketplace v1 → v2 migration. **Marketplace v1 has been removed** and the CLI now uses `ggen-marketplace` directly.

## Architecture

**Current State (v2 only):**

```
┌─────────────────────────────┐
│   CLI Commands              │
│  (search, install, etc.)    │
│  marketplace.rs             │
└──────────────┬──────────────┘
               │
               ▼
┌──────────────────────────────┐
│  ggen-marketplace        │
│  (direct usage, no adapter)  │
└──────────────────────────────┘
```

**Note:** The `v2_adapter.rs` file has been removed (2025-12-11). CLI uses `ggen-marketplace` directly with no adapter layer.

## Migration Complete ✅

### ✅ Completed Tasks

1. **CLI Direct Integration**
   - CLI (`crates/ggen-cli/src/cmds/marketplace.rs`) uses `ggen-marketplace` directly
   - No adapter layer needed - direct usage of v2 types and functions
   - All marketplace commands (search, install, publish, etc.) use v2

2. **v1 Removal**
   - Marketplace v1 crate removed
   - All v1 references removed from active code
   - Legacy adapter code (`v2_adapter.rs`) removed (2025-12-11)
   - All v1 test files removed

3. **Current Implementation**
   - CLI uses `ggen-marketplace` directly
   - No adapter layer needed
   - All 9 marketplace commands verified working with v2

3. **Type Conversions**
   - V1 conversions: **COMPLETE** ✅
     - `UnifiedSearchQuery → SearchInput`
     - `SearchResult → UnifiedSearchResult`
   - V2 conversions: **STUBBED** (v2 crate doesn't compile yet)
     - Code written but commented out
     - Ready to uncomment when v2 compiles

4. **Testing**
   - All v2_adapter tests passing (4/4)
   - Compilation verified with `marketplace-v1`
   - No regressions in existing functionality

### ✅ Compilation Status

| Feature Combination      | Status | Notes                          |
|-------------------------|--------|--------------------------------|
| `marketplace-v1`         | ✅ PASS | Default, all tests pass        |
| `marketplace-v2`         | ⚠️ BLOCKED | ggen-marketplace won't compile |
| `marketplace-parallel`   | ⚠️ BLOCKED | Needs v2 to compile           |

### 📋 V2 Compilation Issues

The `ggen-marketplace` crate currently has these issues:

1. Missing module export: `rdf_mapper` (FIXED ✅)
2. Future Send trait errors (13 errors remaining)
3. Async trait bounds need adjustment

**Workaround:** v2_adapter returns helpful error when v2 is selected:
```
"Marketplace v2 is under development. Please use marketplace-v1 (default) for now."
```

## Phase 2: Search Command (TODO)

**Prerequisites:** Fix ggen-marketplace compilation errors

### Tasks

1. Uncomment v2 conversions in `v2_adapter.rs`
2. Update `execute_search()` in `ggen-domain/src/marketplace/search.rs`
3. Add SPARQL query options
4. Maintain CLI output format compatibility
5. Add search tests for v2 backend

### Example Usage (Future)

```rust
// When v2 compiles, this will work:
let query = UnifiedSearchQuery::new("rust web")
    .with_category("web-framework")
    .with_min_quality(80);

let results = execute_unified_search(query).await?;
// Automatically routes to v2 if feature enabled
```

## Phase 3: Registry & Installation (TODO)

**Prerequisites:** Phase 2 complete

### Tasks

1. Create `RegistryAdapter` trait
2. Implement `RdfRegistry` adapter
3. Migrate install command
4. Add Ed25519 signature verification
5. Integration tests

## Feature Flag Usage

### Default (V1 only)
```toml
[dependencies]
ggen-domain = "3.2.0"
# marketplace-v1 is enabled by default
```

### V2 only (Future)
```toml
[dependencies]
ggen-domain = { version = "3.2.0", default-features = false, features = ["marketplace-v2"] }
```

### Parallel Mode (A/B Testing)
```toml
[dependencies]
ggen-domain = { version = "3.2.0", features = ["marketplace-parallel"] }
```

## Code Organization

```
ggen-domain/src/marketplace/
├── mod.rs                  # Exports v2_adapter types
├── v2_adapter.rs          # ✅ Adapter implementation
├── search.rs              # Legacy v1 search
├── types.rs               # Poka-yoke validated types
└── V2_MIGRATION.md        # This file

ggen-marketplace/       # ⚠️ Needs fixes
├── src/
│   ├── search.rs          # V2 search engine
│   ├── models.rs          # V2 data models
│   └── ...
```

## Testing Strategy

### Unit Tests
- [x] `test_search_backend_selection()` - Feature flag routing
- [x] `test_unified_search_query_builder()` - Query builder
- [x] `test_v1_query_conversion()` - V1 type conversions
- [x] `test_v1_result_conversion()` - V1 result mapping
- [ ] `test_v2_query_conversion()` - V2 type conversions (blocked)
- [ ] `test_v2_result_conversion()` - V2 result mapping (blocked)

### Integration Tests
- [ ] Search with v1 backend
- [ ] Search with v2 backend
- [ ] Parallel mode comparison
- [ ] CLI backward compatibility

## Performance Considerations

### V1 (Current)
- In-memory index caching
- Fuzzy matching with Levenshtein distance
- ~50ms search latency (optimized)

### V2 (Future)
- SPARQL semantic queries
- RDF triplestore (oxigraph)
- Expected <100ms search latency
- Better relevance through semantic understanding

## Migration Path for Developers

### Step 1: Understand Current State
- V1 works perfectly (default)
- V2 architecture is in place
- V2 implementation blocked by compilation errors

### Step 2: Fix V2 Compilation (Next Task)
1. Fix async trait bounds in `ggen-marketplace`
2. Address Send/Sync requirements
3. Update deprecated oxigraph API usage

### Step 3: Enable V2 Conversions
```rust
// In v2_adapter.rs, uncomment:
#[cfg(feature = "marketplace-v2")]
mod v2_conversions {
    // Already written, just commented out
}
```

### Step 4: Test & Validate
```bash
# Test v1 (current default)
cargo test --package ggen-domain --features marketplace-v1

# Test v2 (after fixes)
cargo test --package ggen-domain --features marketplace-v2

# Test parallel
cargo test --package ggen-domain --features marketplace-parallel
```

## Benefits of This Architecture

1. **Zero Disruption**: V1 continues to work perfectly
2. **Gradual Migration**: Can enable v2 when ready
3. **A/B Testing**: Parallel mode allows comparison
4. **Type Safety**: Unified types prevent errors
5. **Future-Proof**: Easy to add v3, v4, etc.

## Success Criteria

### Phase 1 (DONE ✅)
- [x] Feature gates compile
- [x] Adapter compiles with v1
- [x] All v2_adapter tests pass
- [x] No regressions in existing functionality
- [x] Documentation complete

### Phase 2 (TODO)
- [ ] V2 crate compiles
- [ ] V2 search works end-to-end
- [ ] CLI commands unchanged
- [ ] Performance benchmarks

### Phase 3 (TODO)
- [ ] Registry operations migrate
- [ ] Install command works with v2
- [ ] Signature verification
- [ ] Production ready

## Known Issues

1. **ggen-marketplace compilation errors**
   - 13 async/Send errors
   - Impact: Blocks v2 feature enablement
   - Workaround: V2 returns helpful error message

2. **V2 conversions commented out**
   - Impact: V2 search not functional
   - Workaround: Code ready, uncomment when v2 compiles

## Next Steps

1. **Immediate**: Fix ggen-marketplace compilation
2. **Short-term**: Uncomment v2 conversions, add tests
3. **Medium-term**: Migrate search command to use adapter
4. **Long-term**: Complete registry and install migrations

## Questions?

Contact the team or see:
- Architecture docs: `crates/ggen-domain/src/marketplace/adapter.rs`
- V2 adapter code: `crates/ggen-domain/src/marketplace/v2_adapter.rs`
- Feature gates: `crates/ggen-domain/Cargo.toml`
