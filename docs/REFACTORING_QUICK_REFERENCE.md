# Refactoring Quick Reference - ggen v2.0 Async/Sync

## Critical Numbers

| Metric | Value | Status |
|--------|-------|--------|
| **Total async functions** | 317 | - |
| **Non-test async functions** | 203 | 🔴 Need wrappers |
| **Command files (cmds/)** | 74 | 🔴 Must be sync |
| **Unwrap/expect (prod)** | 17 | 🔴 Must fix |
| **Unwrap/expect (tests)** | 87 | ✅ Acceptable |
| **Files >500 lines** | 6 | 🟡 Should split |
| **Total LOC** | 25,637 | - |
| **Error handling (? op)** | 658 | ✅ Excellent |

## Critical Path (172.5 hours)

1. **Fix unwrap/expect** (8.5h)
   - 17 production occurrences
   - Files: `domain/graph/export.rs`, `lib.rs`

2. **Convert commands to sync** (80h)
   - 74 command files
   - Pattern: `runtime::execute(async { ... })`

3. **Add sync wrappers** (60h)
   - 203 non-test async functions
   - Commands layer only

4. **Split oversized files** (24h)
   - 6 files >500 lines
   - Target: <300 lines per file

## Refactoring Pattern

### BEFORE (Async - Won't Work)
```rust
// cmds/market/search.rs
pub async fn run(args: &SearchArgs) -> Result<()> {
    domain::marketplace::search::search_and_display(
        &args.query,
        args.category.as_deref(),
        args.keyword.as_deref(),
        args.author.as_deref(),
        args.fuzzy,
        args.detailed,
        args.json,
        args.limit,
    ).await
}
```

### AFTER (Sync - Compatible with clap-noun-verb v3.0.0)
```rust
// cmds/market/search.rs
pub fn run(args: &SearchArgs) -> Result<()> {
    crate::runtime::execute(async {
        domain::marketplace::search::search_and_display(
            &args.query,
            args.category.as_deref(),
            args.keyword.as_deref(),
            args.author.as_deref(),
            args.fuzzy,
            args.detailed,
            args.json,
            args.limit,
        ).await
    })
}

// domain/marketplace/search.rs (UNCHANGED - stays async)
pub async fn search_and_display(...) -> Result<()> {
    // Existing implementation
}
```

## Complexity Breakdown

| Category | Count | Lines | Effort/Func | Total |
|----------|-------|-------|-------------|-------|
| **Simple** | 145 | 5-30 | 1-2h | 217h |
| **Medium** | 48 | 30-100 | 3-5h | 192h |
| **Complex** | 10 | 100+ | 8-12h | 100h |
| **TOTAL** | 203 | - | - | **509h** |

## Files Requiring Refactoring

### High Priority (Week 1-2)
- `cmds/hook/` - 18 functions
- `cmds/template/` - 22 functions
- `cmds/project/` - 25 functions
- `domain/marketplace/` - 6 functions

### Medium Priority (Week 3-4)
- `cmds/market/` - 15 functions
- `cmds/graph/` - 20 functions
- `cmds/ai/` - 15 functions

### Complex Files (Week 5-6)
- `cmds/ci/release.rs` - 786 lines, 19 funcs
- `cmds/lifecycle/mod.rs` - 656 lines, 10+ funcs
- `cmds/market/search.rs` - 635 lines, 4 funcs
- `cmds/ci/workflow.rs` - 601 lines, 17 funcs
- `cmds/ci/pages.rs` - 585 lines, 16 funcs
- `cmds/shell/init.rs` - 489 lines, 13 funcs

## Validation Commands

```bash
# Check no async in commands layer
rg "pub async fn run" cli/src/cmds/ && echo "FAIL" || echo "PASS"

# Check runtime::execute usage
rg "runtime::execute" cli/src/cmds/ | wc -l  # Should be 74+

# Check unwrap count in production
rg "\.unwrap\(\)" cli/src/{cmds,domain} --glob '!*test*' | wc -l  # Should be 0

# Ensure compilation
cargo check --all-features

# Run tests
cargo test --all-features
```

## Key Insights

1. **Domain layer stays async** ✅ (correct design)
2. **Commands layer must be sync** 🔴 (clap-noun-verb requirement)
3. **Runtime bridge is simple** ✅ (`runtime::execute()`)
4. **Tests stay async** ✅ (`#[tokio::test]` is fine)
5. **Can parallelize work** ✅ (multiple developers)

## Error Handling Fix Pattern

### BEFORE (unwrap - unsafe)
```rust
let graph = Graph::new().expect("Failed to create empty graph");
let lock = mutex.lock().unwrap();
let runtime = Runtime::new().unwrap();
```

### AFTER (proper error handling)
```rust
let graph = Graph::new()
    .map_err(|e| Error::with_context(
        &format!("Failed to create graph: {}", e),
        "graph_export"
    ))?;

let lock = mutex.lock()
    .map_err(|e| Error::with_context(
        &format!("Failed to acquire lock: {}", e),
        "sync"
    ))?;

let runtime = Runtime::new()
    .map_err(|e| Error::with_context(
        &format!("Failed to create runtime: {}", e),
        "async_bridge"
    ))?;
```

## Timeline Estimates

### 1 Developer
- **Phase 1**: 8 weeks (critical blockers)
- **Phase 2**: 4 weeks (medium priority)
- **Phase 3**: 2 weeks (quality)
- **Total**: 14 weeks

### 3 Developers
- **Phase 1**: 3 weeks (parallel modules)
- **Phase 2**: 2 weeks (parallel refactoring)
- **Phase 3**: 1 week (integration)
- **Total**: 6 weeks

### 6 Developers
- **Phase 1**: 2 weeks (max parallelization)
- **Phase 2**: 1 week (integration)
- **Phase 3**: 1 week (testing)
- **Total**: 4 weeks

## Risk Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking changes | Critical | Comprehensive test suite, CI/CD |
| Performance regression | Medium | Benchmarks before/after |
| Scope creep | High | Strict phased approach |
| Integration issues | Medium | Incremental commits, PR reviews |

## Success Criteria

- ✅ Zero async functions in `cmds/*/run()` methods
- ✅ Zero unwrap/expect in production code
- ✅ All tests passing
- ✅ No performance regression (benchmarks)
- ✅ clap-noun-verb v3.0.0 compatibility verified
- ✅ Documentation updated

## Resources

- **Main Report**: `/Users/sac/ggen/docs/CODE_QUALITY_ANALYSIS_REPORT.md` (18KB)
- **Complexity Matrix**: `/Users/sac/ggen/docs/ASYNC_FUNCTION_COMPLEXITY_MATRIX.md` (8.5KB)
- **Runtime Bridge**: `/Users/sac/ggen/cli/src/runtime.rs` (38 lines)

---

**Last Updated**: 2025-11-01
**Status**: Analysis complete, ready for Phase 1
