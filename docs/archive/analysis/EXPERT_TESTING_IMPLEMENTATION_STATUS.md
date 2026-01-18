# Expert Testing Patterns - Implementation Status

## Overview

This document tracks the implementation of expert-level testing patterns across the ggen codebase, following the 80/20 rule: **Test the 20% of cases that catch 80% of bugs**.

## Core Principle: 80/20 Rule

Expert testing focuses on:
- **Error paths** (not just happy path) - 80% of bugs
- **Boundary conditions** (not just normal values) - 15% of bugs
- **Resource cleanup** (not just normal execution) - 3% of bugs
- **Concurrency** (not just single-threaded) - 2% of bugs

## Implementation Status by Module

### ✅ Marketplace Module (`crates/ggen-domain/src/marketplace/expert_tests.rs`)

**Status**: ✅ Complete

**Patterns Implemented**:
1. **Resource Cleanup** ✅
   - `test_lockfile_cleanup_on_error` - Lockfile cleanup verification
   - `test_registry_save_cleanup_on_error` - Registry temp file cleanup

2. **Concurrency** ✅
   - `test_concurrent_lockfile_writes` - Concurrent lockfile writes with file locking
   - `test_concurrent_registry_saves` - Concurrent registry saves

3. **Error Recovery** ✅
   - `test_lockfile_corruption_recovery` - Lockfile corruption recovery from backup
   - `test_registry_corruption_recovery` - Registry corruption error handling

4. **Real Dependencies** ✅
   - `test_real_file_io_lockfile_operations` - Real file I/O for lockfiles
   - `test_real_json_parsing_registry_operations` - Real JSON parsing for registry
   - `test_validated_types_with_real_validation` - Real validation for Poka-Yoke types

5. **Integration** ✅
   - `test_end_to_end_error_scenarios` - End-to-end error scenarios

**Coverage**: 10 expert tests covering all 5 patterns

---

### ✅ Graph Export Module (`crates/ggen-domain/src/graph/export_expert_tests.rs`)

**Status**: ✅ Complete (Just Added)

**Patterns Implemented**:
1. **Error Paths** ✅
   - `test_export_error_invalid_output_path` - Invalid output path handling
   - `test_export_error_permission_denied` - Permission error handling
   - `test_export_error_unsupported_format` - Unsupported format (JSON-LD, N3) errors

2. **Boundary Conditions** ✅
   - `test_export_boundary_empty_graph` - Empty graph export
   - `test_export_boundary_very_large_graph` - Large graph export (100+ triples)
   - `test_export_boundary_max_path_length` - Very long file paths

3. **Resource Cleanup** ✅
   - `test_export_resource_cleanup_temp_files` - Temp file cleanup verification
   - `test_export_resource_cleanup_file_handles` - File handle cleanup (multiple exports)

4. **Concurrency** ✅
   - `test_export_concurrent_writes` - Concurrent exports to different files

5. **Error Recovery** ✅
   - `test_export_error_recovery_after_failure` - Recovery after initial failure

**Coverage**: 9 expert tests covering all 5 patterns

---

### ⚠️ Graph Core Module (`crates/ggen-core/src/graph/`)

**Status**: ⚠️ Partial

**Existing Tests**: Basic unit tests exist, but expert patterns are limited

**Missing Patterns**:
- Error paths for Store creation failures
- Boundary conditions for empty graphs, very large graphs
- Resource cleanup for persistent stores
- Concurrency tests for Arc<Mutex<Store>> access patterns
- Error recovery for corrupted persistent stores

**Priority**: 🔴 High (critical for graph operations)

---

### ⚠️ Template Render Module (`crates/ggen-domain/src/template/render_with_rdf.rs`)

**Status**: ⚠️ Partial

**Existing Tests**: Basic tests exist

**Missing Patterns**:
- Error paths for template parsing failures
- Boundary conditions for very large templates, empty templates
- Resource cleanup for temp files during rendering
- Concurrency tests for concurrent template renders
- Error recovery for partial render failures

**Priority**: 🟡 Medium (important for template generation)

---

### ⚠️ Cache Module (`crates/ggen-core/src/cache.rs`)

**Status**: ⚠️ Basic

**Missing Patterns**:
- Error paths for cache directory creation failures
- Boundary conditions for cache at capacity, LRU eviction
- Resource cleanup for cache file handles
- Concurrency tests for concurrent cache access
- Error recovery for corrupted cache files

**Priority**: 🔴 High (critical for performance)

---

## Test Execution Summary

### Marketplace Expert Tests
```bash
cargo test --lib --package ggen-domain marketplace::expert_tests
```
**Result**: ✅ All 10 tests passing

### Graph Export Expert Tests
```bash
cargo test --lib --package ggen-domain graph::export_expert_tests
```
**Result**: ✅ All 9 tests passing

---

## Next Steps (Recommended Priority)

### Phase 1: Critical Error Paths (High Priority)
1. **Graph Core Module** - Add error path tests for Store operations
2. **Cache Module** - Add error path tests for cache operations
3. **Template Render Module** - Add error path tests for template parsing

### Phase 2: Boundary Conditions (High Priority)
1. **Graph Core Module** - Add boundary tests for empty/large graphs
2. **Cache Module** - Add boundary tests for cache capacity
3. **Template Render Module** - Add boundary tests for template sizes

### Phase 3: Resource Management (Medium Priority)
1. **Graph Core Module** - Add resource cleanup tests for persistent stores
2. **Cache Module** - Add resource cleanup tests for cache files
3. **Template Render Module** - Add resource cleanup tests for temp files

### Phase 4: Concurrency (Medium Priority)
1. **Graph Core Module** - Add concurrency tests for Arc<Mutex<Store>>
2. **Cache Module** - Add concurrency tests for cache access
3. **Template Render Module** - Add concurrency tests for concurrent renders

---

## Key Metrics

- **Total Expert Tests**: 19 (10 marketplace + 9 graph export)
- **Modules with Expert Tests**: 2 (marketplace ✅, graph export ✅)
- **Modules Needing Expert Tests**: 3+ (graph core, cache, template render)
- **Coverage**: ~40% of critical modules have expert tests

---

## References

- **[Expert Testing Patterns Command](.cursor/commands/expert-testing-patterns.md)** - Complete workflow guide
- **[Chicago TDD Guide](../../docs/testing/chicago-tdd-guide.md)** - Testing best practices
- **[Marketplace Expert Tests](../crates/ggen-domain/src/marketplace/expert_tests.rs)** - Reference implementation
- **[Graph Export Expert Tests](../crates/ggen-domain/src/graph/export_expert_tests.rs)** - Reference implementation

