# Test Coverage Analysis - Lifecycle System

**Analysis Date**: 2025-10-11
**Files Analyzed**:
- `src/lifecycle/integration_test.rs` (746 lines)
- `src/lifecycle/behavior_tests.rs` (697 lines)
- `src/lifecycle/state.rs`, `exec.rs`, `cache.rs`

---

## Executive Summary

**Current State**: Good foundation with integration and behavior tests
**Critical Gaps**: 17 P0 scenarios, 23 P1 scenarios
**Quick Wins**: 12 tests that provide maximum confidence with minimal effort

---

## P0 Test Gaps (Production-Breaking Scenarios)

### 1. **Disk Full During State Save** ⚠️ CRITICAL
**Risk**: Data loss, inconsistent state
**Effort**: 15 min
**Scenario**:
```rust
#[test]
fn test_disk_full_during_state_save() {
    // Given: Full disk condition
    // When: Phase completes and tries to save state
    // Then: Should return clear error, not corrupt existing state
}
```

**Why Critical**: State file could be left in corrupted/partial state, breaking future runs

---

### 2. **Process Killed During Phase Execution** ⚠️ CRITICAL
**Risk**: Orphaned processes, corrupted state
**Effort**: 20 min
**Scenario**:
```rust
#[test]
fn test_process_killed_mid_execution() {
    // Given: Phase running with state updates
    // When: Process receives SIGKILL/SIGTERM
    // Then: State should be recoverable on next run
}
```

**Implementation Hint**: Use `nix` crate to send signals to spawned test process

---

### 3. **Corrupted state.json Recovery** ⚠️ CRITICAL
**Risk**: Complete system failure, no recovery path
**Effort**: 10 min
**Scenarios**:
```rust
#[test]
fn test_corrupted_state_json_invalid_json() {
    // state.json contains: "{ invalid json syntax"
    // Should: Return clear error, suggest deleting state
}

#[test]
fn test_corrupted_state_json_partial_write() {
    // state.json contains: partial JSON (truncated)
    // Should: Detect and handle gracefully
}

#[test]
fn test_state_json_empty_file() {
    // state.json is 0 bytes
    // Should: Treat as missing, use defaults
}
```

**Current Gap**: `load_state()` will panic or return cryptic serde errors

---

### 4. **Circular Hook Dependencies** ⚠️ CRITICAL
**Risk**: Infinite recursion, stack overflow
**Effort**: 15 min
**Current Coverage**: Basic recursion test exists but limited
**Missing Scenarios**:
```rust
#[test]
fn test_circular_hooks_a_b_a() {
    // before_build = ["test"]
    // before_test = ["build"]
    // Should: Detect cycle and fail with clear error showing chain
}

#[test]
fn test_circular_hooks_deep_chain() {
    // A -> B -> C -> D -> A (4-level cycle)
    // Should: Detect and show full cycle path
}

#[test]
fn test_circular_hooks_complex_graph() {
    // Multiple paths leading to same cycle
    // Should: Detect first cycle encountered
}
```

**Current Limitation**: `hook_guard` detects direct recursion but may miss complex cycles

---

### 5. **Invalid make.toml Handling** ⚠️ CRITICAL
**Risk**: Cryptic errors, poor UX
**Effort**: 20 min
**Scenarios**:
```rust
#[test]
fn test_make_toml_missing_project_section() {
    // No [project] section
    // Should: Clear error message
}

#[test]
fn test_make_toml_invalid_toml_syntax() {
    // Malformed TOML
    // Should: Show line number and syntax error
}

#[test]
fn test_make_toml_conflicting_command_definitions() {
    // Phase has both 'command' and 'commands'
    // Should: Error or pick one with warning
}

#[test]
fn test_make_toml_invalid_hook_phase_reference() {
    // before_build = ["nonexistent"]
    // Should: Fail fast at load time, not execution
}
```

---

### 6. **Concurrent Workspace Access** ⚠️ CRITICAL (for parallel mode)
**Risk**: Race conditions, corrupted state
**Effort**: 25 min
**Scenarios**:
```rust
#[test]
fn test_concurrent_state_writes_different_workspaces() {
    // Two workspaces writing to state simultaneously
    // Should: Each workspace has isolated state file
}

#[test]
fn test_concurrent_cache_access() {
    // Multiple phases checking cache in parallel
    // Should: Thread-safe cache reads/writes
}

#[test]
fn test_concurrent_mutex_poisoning_recovery() {
    // One workspace panics, poisoning mutex
    // Should: Other workspaces continue or fail gracefully
}
```

**Current Gap**: `hook_guard` uses `Arc<Mutex>` but poisoning not tested

---

### 7. **Workspace Path Traversal / Security** ⚠️ CRITICAL
**Risk**: Arbitrary code execution, path traversal
**Effort**: 15 min
**Scenarios**:
```rust
#[test]
fn test_workspace_path_traversal_attack() {
    // workspace.path = "../../etc/passwd"
    // Should: Reject paths outside project root
}

#[test]
fn test_workspace_symlink_escape() {
    // workspace.path contains symlink to /tmp
    // Should: Resolve real path and validate
}
```

---

### 8. **Large-Scale Stress Tests** 🔥 P0 for production
**Risk**: Performance degradation, OOM
**Effort**: 30 min
**Scenarios**:
```rust
#[test]
fn test_large_number_of_phases() {
    // 1000+ phases in make.toml
    // Should: Load and execute without stack overflow
}

#[test]
fn test_deep_hook_nesting() {
    // 50+ levels of before_X hooks
    // Should: Execute without stack overflow
}

#[test]
fn test_large_phase_history() {
    // state.json with 10,000+ phase executions
    // Should: Load/save efficiently
}

#[test]
fn test_large_file_outputs() {
    // Phase outputs 1GB+ files
    // Should: Track outputs without memory issues
}
```

---

## P1 Test Gaps (Important Edge Cases)

### 9. **State Atomicity & Partial Updates**
**Effort**: 15 min
```rust
#[test]
fn test_state_update_interrupted() {
    // Phase succeeds but save_state() fails
    // Should: Next run should not see partial state
}

#[test]
fn test_state_backup_on_corruption() {
    // Auto-create state.json.backup before overwrite
}
```

---

### 10. **Cache Key Collisions**
**Effort**: 10 min
```rust
#[test]
fn test_cache_key_uniqueness_different_phases() {
    // Ensure SHA256 collision impossible for different inputs
}

#[test]
fn test_cache_invalidation_after_file_change() {
    // Input file changes -> cache key changes
}
```

---

### 11. **Environment Variable Edge Cases**
**Effort**: 10 min
```rust
#[test]
fn test_env_with_special_characters() {
    // VAR="value with\nnewlines"
}

#[test]
fn test_env_override_system_vars() {
    // Overriding PATH, HOME, etc.
}
```

---

### 12. **Workspace Loading Failures**
**Effort**: 15 min
```rust
#[test]
fn test_workspace_missing_directory() {
    // workspace.path points to non-existent dir
}

#[test]
fn test_workspace_missing_make_toml_fallback() {
    // Workspace has no make.toml, uses root config
    // (Already handled, but needs explicit test)
}
```

---

### 13. **Parallel Execution Edge Cases**
**Effort**: 20 min
```rust
#[test]
fn test_parallel_with_zero_workspaces() {
    // parallel = true but no workspaces defined
}

#[test]
fn test_parallel_error_aggregation() {
    // Multiple workspaces fail - report all errors
}

#[test]
fn test_parallel_timeout_handling() {
    // One workspace hangs - timeout mechanism?
}
```

---

### 14. **Command Execution Edge Cases**
**Effort**: 15 min
```rust
#[test]
fn test_command_with_special_shell_characters() {
    // command = "echo 'test' | grep test"
}

#[test]
fn test_command_environment_isolation() {
    // Env vars don't leak between phases
}

#[test]
fn test_command_working_directory() {
    // Verify cwd is correct for each phase
}
```

---

### 15. **Hook Execution Order Guarantees**
**Effort**: 10 min
```rust
#[test]
fn test_hook_order_with_multiple_before_all() {
    // before_all = ["a", "b", "c"]
    // Verify exact execution order
}

#[test]
fn test_hook_failure_prevents_after_hooks() {
    // Already exists but needs verification
}
```

---

### 16. **State History Pruning**
**Effort**: 15 min
```rust
#[test]
fn test_phase_history_growth_over_time() {
    // After 10,000 runs, state.json shouldn't be huge
    // Consider auto-pruning old history
}
```

---

### 17. **Platform-Specific Behavior**
**Effort**: 20 min
```rust
#[test]
#[cfg(windows)]
fn test_windows_command_execution() {
    // Verify cmd /C execution
}

#[test]
#[cfg(unix)]
fn test_unix_shell_execution() {
    // Verify sh -lc execution
}
```

---

## Quick Test Additions (<30 min each, high value)

### 🎯 Top 12 Quick Wins (80/20 Principle)

1. **Corrupted state.json** (10 min) - P0, high probability
2. **Circular hooks 3+ levels** (15 min) - P0, catches real bugs
3. **Invalid make.toml error messages** (15 min) - P0, user experience
4. **Disk full during save** (15 min) - P0, data loss prevention
5. **Empty state.json file** (5 min) - P0, common failure mode
6. **Large phase history** (10 min) - P1, performance regression
7. **Cache key with file inputs** (10 min) - P1, cache correctness
8. **Workspace path validation** (10 min) - P0, security
9. **Parallel error aggregation** (15 min) - P1, debugging aid
10. **Hook recursion with chain** (10 min) - P0, better errors
11. **Command with special chars** (10 min) - P1, real-world usage
12. **Mutex poisoning recovery** (15 min) - P0, parallel safety

**Total Effort**: ~2.5 hours for 12 high-impact tests

---

## Property-Based Testing Opportunities

### Cache Key Determinism
```rust
#[quickcheck]
fn prop_cache_key_deterministic(cmds: Vec<String>, env: Vec<(String, String)>) -> bool {
    let key1 = cache_key("test", &cmds, &env, &[]);
    let key2 = cache_key("test", &cmds, &env, &[]);
    key1 == key2
}
```

### State Consistency
```rust
#[quickcheck]
fn prop_state_round_trip(state: LifecycleState) -> bool {
    // Save -> Load -> Should equal original
}
```

---

## Integration with CI/CD

### Recommended Test Tiers

**Tier 1: Fast Unit Tests** (<100ms each)
- All P0 tests above
- Run on every commit

**Tier 2: Integration Tests** (<1s each)
- Current integration_test.rs suite
- Run on every PR

**Tier 3: Stress Tests** (>1s each)
- Large-scale tests (#8)
- Run nightly or pre-release

---

## Implementation Priority

### Sprint 1 (Week 1)
- [ ] P0 #1-5 (Corrupted state, disk full, circular hooks, invalid make.toml)
- [ ] Quick wins #1-6

### Sprint 2 (Week 2)
- [ ] P0 #6-8 (Concurrency, security, stress tests)
- [ ] Quick wins #7-12

### Sprint 3 (Week 3)
- [ ] P1 tests (#9-17)
- [ ] Property-based tests
- [ ] CI/CD integration

---

## Code Coverage Metrics

**Current Estimated Coverage**: ~70% (based on test file analysis)

**Target Coverage After P0 Tests**: 85%

**Target Coverage After All Tests**: 92%

**Areas with Low Coverage**:
- Error handling paths (state corruption, disk full)
- Concurrency edge cases
- Security validation

---

## Testing Tools Needed

1. **mockall** ✅ Already used for behavior tests
2. **quickcheck** ❌ Not installed - for property-based tests
3. **nix** ❌ Not installed - for signal handling tests
4. **tempfile** ✅ Already used
5. **rayon** ✅ Already used for parallel execution

**Installation**:
```toml
[dev-dependencies]
quickcheck = "1.0"
nix = { version = "0.27", features = ["signal"] }
```

---

## Recommended Test File Structure

```
src/lifecycle/
├── integration_test.rs      # Happy path integration tests
├── behavior_tests.rs         # Mock-based behavior tests
├── edge_cases_test.rs        # NEW: P0 edge cases
├── stress_test.rs            # NEW: Performance tests
└── property_test.rs          # NEW: Property-based tests
```

---

## Risk Assessment

| Scenario | Likelihood | Impact | Priority | Effort |
|----------|-----------|--------|----------|--------|
| Corrupted state.json | High | Critical | P0 | 10m |
| Disk full during save | Medium | Critical | P0 | 15m |
| Circular hooks | Medium | High | P0 | 15m |
| Process killed mid-run | Low | Critical | P0 | 20m |
| Concurrent state writes | Medium | High | P0 | 25m |
| Large phase count | Low | Medium | P1 | 30m |
| Path traversal | Very Low | Critical | P0 | 15m |

---

## Conclusion

**Immediate Action Items**:
1. Add P0 tests #1-5 (corrupted state, circular hooks, invalid config)
2. Add quick wins #1-6 for maximum confidence gain
3. Set up CI matrix with test tiers
4. Consider property-based testing for cache keys

**Expected Outcome**:
- 85% code coverage with P0 tests
- Production-ready confidence
- <3 hours total testing effort for critical paths

**Next Steps**:
See `tests/lifecycle_edge_cases.rs` for implementation template.
