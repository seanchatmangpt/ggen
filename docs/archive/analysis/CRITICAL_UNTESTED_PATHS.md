<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Critical Untested Code Paths - Quick Reference](#critical-untested-code-paths---quick-reference)
  - [CRITICAL PRIORITY - Application Crashes](#critical-priority---application-crashes)
    - [1. Graph Type Conversion Panics](#1-graph-type-conversion-panics)
    - [2. Template Parsing Panics](#2-template-parsing-panics)
    - [3. AI Governance Decision Panic](#3-ai-governance-decision-panic)
  - [HIGH PRIORITY - Race Conditions](#high-priority---race-conditions)
    - [1. Query Cache Invalidation Race](#1-query-cache-invalidation-race)
    - [2. Template Cache Lock Across I/O](#2-template-cache-lock-across-io)
    - [3. Task Spawning Without Join Validation](#3-task-spawning-without-join-validation)
  - [MEDIUM PRIORITY - Error Path Testing](#medium-priority---error-path-testing)
    - [1. Network Failures Not Tested](#1-network-failures-not-tested)
    - [2. Marketplace Registry Failures](#2-marketplace-registry-failures)
    - [3. JSON/TOML Parsing Edge Cases](#3-jsontoml-parsing-edge-cases)
  - [LOW PRIORITY - Edge Cases](#low-priority---edge-cases)
    - [1. Empty Collections](#1-empty-collections)
    - [2. Large Data Sets](#2-large-data-sets)
    - [3. Special Characters](#3-special-characters)
    - [4. Time-Based Issues](#4-time-based-issues)
  - [Files with Test Infrastructure Present](#files-with-test-infrastructure-present)
  - [Summary Statistics](#summary-statistics)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Critical Untested Code Paths - Quick Reference

## CRITICAL PRIORITY - Application Crashes

### 1. Graph Type Conversion Panics
**File**: `/home/user/ggen/crates/ggen-core/src/graph/types.rs`

**Lines 109-250**: 8 panic macros in conversion methods
```rust
109:  _ => panic!("Expected Boolean variant"),
131:  _ => panic!("Expected Solutions variant"),
151:  _ => panic!("Expected Graph variant"),
185:  _ => panic!("Expected JSON array"),
201:  _ => panic!("Expected JSON string"),
217:  _ => panic!("Expected Boolean variants"),
239:  _ => panic!("Expected Solutions variant"),
250:  _ => panic!("Expected Graph variant"),
```

**How to trigger**: Send malformed SPARQL query results or RDF data with wrong type
**Test case needed**:
```rust
#[test]
fn test_invalid_cached_result_type_conversion() {
    // Create CachedResult with wrong type
    // Try to convert to unexpected variant
    // Should NOT panic - should return error
}
```

### 2. Template Parsing Panics
**File**: `/home/user/ggen/crates/ggen-core/src/template.rs`

**Line 837**: Non-idempotent parsing panic
```rust
837: panic!("Template parsing is not idempotent");
```

**Line 879**: Missing path preservation panic
```rust
879: panic!("Expected path to be preserved, but got None");
```

**How to trigger**: 
- Parse template multiple times
- Parse template with missing path field

**Test case needed**:
```rust
#[test]
fn test_template_parsing_idempotency() {
    // Parse same template twice
    // Should produce identical results
}

#[test]
fn test_template_path_preservation() {
    // Parse template without explicit path
    // Should handle gracefully
}
```

### 3. AI Governance Decision Panic
**File**: `/home/user/ggen/crates/ggen-ai/src/governance/mod.rs`

**Line 230**: Governance decision panic (exact panic message in line 230)

**Impact**: AI swarm decision-making crashes

**Test case needed**:
```rust
#[tokio::test]
async fn test_governance_invalid_decision_state() {
    // Create governance with invalid state
    // Make decision request
    // Should handle error gracefully
}
```

---

## HIGH PRIORITY - Race Conditions

### 1. Query Cache Invalidation Race
**File**: `/home/user/ggen/crates/ggen-core/src/graph/core.rs`

**Issue**: Epoch-based cache invalidation with concurrent readers/writers
- Writer increments `epoch` (AtomicU64)
- Reader checks epoch and returns cached result
- Window where stale cache could be returned

**Lines**: 36-46 (INITIAL_EPOCH and EPOCH_INCREMENT constants)

**Test case needed**:
```rust
#[tokio::test]
async fn test_concurrent_insert_and_query_cache_invalidation() {
    let graph = Graph::new()?;
    
    let graph_insert = graph.clone();
    let insert_handle = tokio::spawn(async move {
        // Insert new data repeatedly
    });
    
    let graph_query = graph.clone();
    let query_handle = tokio::spawn(async move {
        // Query repeatedly and check results
    });
    
    // Both run concurrently - should NOT return stale cache
    tokio::try_join!(insert_handle, query_handle)?;
}
```

### 2. Template Cache Lock Across I/O
**File**: `/home/user/ggen/crates/ggen-core/src/template_cache.rs`

**Line 46**: `cache: Arc<Mutex<LruCache<String, Arc<Template>>>>`

**Issue**: Mutex held during template parsing (I/O operation)
- Long parsing time could block other cache accessors
- Potential deadlock if parsing fails

**Test case needed**:
```rust
#[tokio::test]
async fn test_concurrent_cache_access_during_parse() {
    let cache = TemplateCache::new(10);
    
    // Spawn many concurrent cache accesses
    // Some while parsing slow templates
    // Should not deadlock
}
```

### 3. Task Spawning Without Join Validation
**File**: `/home/user/ggen/crates/ggen-ai/src/swarm/orchestration.rs` (and others)

**Issue**: 38 tokio::spawn calls, but no panic handling for spawned tasks

**Test case needed**:
```rust
#[tokio::test]
async fn test_spawned_task_panic_is_handled() {
    // Spawn task that panics
    // Should not crash the orchestrator
    // Should be detectable
}
```

---

## MEDIUM PRIORITY - Error Path Testing

### 1. Network Failures Not Tested
**File**: `/home/user/ggen/crates/ggen-core/src/cache.rs`

**Methods**:
- `ensure()` - Downloads and caches packs (async)
- `calculate_sha256()` - Verifies pack integrity

**Error scenarios missing**:
- Git clone timeout
- Git authentication failure
- SHA256 mismatch (corruption)
- Disk space exhausted
- Permission denied on cache directory

**Test case needed**:
```rust
#[tokio::test]
async fn test_cache_ensure_git_clone_timeout() {
    let cache = CacheManager::with_dir(temp_dir)?;
    // Mock git clone to timeout
    // Should return CacheError::GitOperationFailed
}

#[tokio::test]
async fn test_cache_sha256_mismatch() {
    // Create pack with wrong SHA256
    // Should return CacheError
}
```

### 2. Marketplace Registry Failures
**File**: `/home/user/ggen/crates/ggen-core/src/registry.rs` (38 pub fns)

**Missing tests**:
- Concurrent registry access (26 Err() returns indicate error paths)
- Malformed registry data
- Registry file corruption
- Network failures during registry sync

### 3. JSON/TOML Parsing Edge Cases
**Files**: 
- `/home/user/ggen/crates/ggen-marketplace/src/models/`
- `/home/user/ggen/crates/ggen-domain/src/marketplace/`

**Missing tests**:
- Malformed JSON structures
- TOML with invalid syntax
- Very large payloads (>100MB)
- Deeply nested structures
- Invalid UTF-8 sequences

---

## LOW PRIORITY - Edge Cases

### 1. Empty Collections
**Missing tests**:
- Empty template set generation
- Empty marketplace registry queries
- Empty RDF graph operations
- Empty SPARQL query results

### 2. Large Data Sets
**Missing tests**:
- 10,000+ items in LRU cache
- 100MB+ RDF graphs
- SPARQL queries returning 1M+ results
- Template with 1000+ variables

### 3. Special Characters
**Missing tests**:
- Unicode file paths
- newlines in template variables
- TOML/JSON with escaped characters
- Path traversal attempts (../../../etc/passwd)

### 4. Time-Based Issues
**File**: `/home/user/ggen/crates/ggen-core/src/cleanroom/mod.rs` (line 240)
**Missing tests**:
- Frozen time transitions
- Clock wrap-around
- Concurrent time queries

---

## Files with Test Infrastructure Present

These can be used as templates for writing missing tests:

1. **Template Cache Tests** (Good example of concurrent testing)
   - `/home/user/ggen/crates/ggen-core/src/template_cache.rs` (lines 232-277)
   - Uses: chicago_tdd_tools framework, Arc<> pointer equality testing

2. **Cache Manager Tests** (Good example of async testing)
   - `/home/user/ggen/crates/ggen-core/src/cache.rs` (lines 581+)
   - Uses: tempfile, filesystem operations

3. **Marketplace Integration Tests** (Good example of complex scenarios)
   - `/home/user/ggen/crates/ggen-marketplace/tests/error_scenarios.rs` (10KB)
   - `/home/user/ggen/crates/ggen-marketplace/tests/integration_critical_paths.rs` (14KB)

4. **Security Tests** (Good example of edge case testing)
   - `/home/user/ggen/crates/ggen-core/tests/security/injection_prevention.rs`
   - `/home/user/ggen/crates/ggen-core/tests/security/input_validation.rs`

---

## Summary Statistics

| Category | Count | Files |
|----------|-------|-------|
| Panic macros | 11 | 3 files |
| Orphaned test files | 4 | lifecycle/ |
| tokio::spawn calls | 38 | AI modules |
| Unwrap/expect calls | 761 | Mostly in tests |
| Error returns | 26+ | Cache, Registry |
| Critical untested race conditions | 3 | Graph, Cache, Tasks |

