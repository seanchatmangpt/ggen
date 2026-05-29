# CLI Marketplace Stress Test - Quick Reference Card

**Agent**: Analyst Beta | **Version**: 1.0.0 | **Date**: 2025-11-01

---

## 🎯 At a Glance

| Metric | Value |
|--------|-------|
| **Total Test Scenarios** | 432 |
| **P0 Critical Tests** | 35 (must-pass) |
| **P1 Important Tests** | 52 (high-value) |
| **Stress Test Vectors** | 45 (edge cases) |
| **Performance Benchmarks** | 6 (latency/throughput) |
| **Full Suite Execution Time** | 77 minutes |
| **Smoke Tests Only** | 2 minutes |
| **Target Coverage** | 80% |

---

## 🚀 Quick Start - Run Tests

```bash
# Smoke tests (2 minutes)
cargo test --test smoke_tests

# P0 critical tests only (12 minutes)
cargo test --test critical_tests

# Full permutation suite (77 minutes)
cargo test --test permutation_tests

# Stress tests only (30 minutes)
cargo test --test stress_tests

# Performance benchmarks (20 minutes)
cargo bench --bench marketplace_benchmarks
```

---

## 📋 Test Priority Breakdown

### P0 - Critical (35 tests) - RUN FIRST ⚡
**Why**: Must-pass for production readiness

- ✅ Basic search functionality
- ✅ Package add/remove
- ✅ Lifecycle init/build/test/deploy
- ✅ Production validation
- ✅ Input validation (all commands)
- ✅ Error handling (network, disk, permissions)

**Command**: `cargo test --test critical_tests`

---

### P1 - Important (52 tests) - RUN SECOND 🎯
**Why**: High-value features and edge cases

- ✅ Filtered searches
- ✅ Cache management
- ✅ Offline operations
- ✅ Pipeline execution
- ✅ Readiness tracking
- ✅ Concurrent operations

**Command**: `cargo test --test important_tests`

---

### P2 - Nice-to-Have (30 tests) - RUN THIRD 💡
**Why**: Advanced features and optimizations

- ✅ Fuzzy search
- ✅ Natural language search
- ✅ GraphQL API
- ✅ Advanced caching
- ✅ Performance benchmarks

**Command**: `cargo test --test nice_to_have_tests`

---

## 🔥 Top 10 High-Risk Scenarios

| # | Scenario | Command | Priority |
|---|----------|---------|----------|
| 1 | Concurrent lockfile modifications | Multiple `add`/`remove` simultaneously | P0 |
| 2 | Cache corruption under load | 50+ concurrent cache operations | P0 |
| 3 | Network timeout handling | 30s marketplace API delay | P0 |
| 4 | Disk full scenarios | Write operations when disk full | P0 |
| 5 | Unicode in package metadata | Package names with 🦀 工具 | P1 |
| 6 | Cross-platform path handling | 260+ char paths on Windows | P1 |
| 7 | Large cache compaction | 10,000 packages in cache | P1 |
| 8 | Empty query validation | `ggen market search ""` | P0 |
| 9 | Circular dependencies | Pkg A → Pkg B → Pkg A | P1 |
| 10 | Malformed metadata | Invalid Cargo.toml in package | P1 |

---

## 📊 Performance Targets (SLA)

| Operation | P50 | P95 | P99 | Throughput | Memory |
|-----------|-----|-----|-----|------------|--------|
| `market search` | <100ms | <500ms | <1s | 100/sec | <100MB |
| `market add` | <200ms | <1s | <2s | 50/sec | <200MB |
| `market sync` | <5s | <15s | <30s | 10/min | <500MB |
| `lifecycle run` | <1s | <5s | <10s | 20/min | <300MB |
| `lifecycle pipeline` | <10s | <30s | <60s | 5/min | <500MB |
| `cache ops` | <50ms | <200ms | <500ms | 200/sec | <50MB |

**Validation**: `cargo bench --bench marketplace_benchmarks`

---

## 🧪 Sample Test Commands

### Basic Functionality
```bash
# Valid search
ggen market search "rust cli"

# Filtered search
ggen market search "web" --category api --min-stars 100 --detailed

# JSON output
ggen market search "database" --json --limit 20

# Package installation
ggen market add "rust-cli-template@1.2.0"

# Lifecycle execution
ggen lifecycle run init
ggen lifecycle pipeline init setup build test
```

### Error Cases (Should Fail Gracefully)
```bash
# Empty query
ggen market search ""
# Expected: Error: Search query cannot be empty

# Query too long
ggen market search "$(python3 -c 'print("a" * 1001)')"
# Expected: Error: Search query too long (max 1000)

# Invalid package ID
ggen market add "invalid/package"
# Expected: Error: Invalid gpack ID format

# Non-existent phase
ggen lifecycle run invalid-phase
# Expected: Error: Phase 'invalid-phase' not found
```

### Stress Tests
```bash
# Concurrent searches
ggen market search "rust" & \
ggen market search "web" & \
ggen market search "database" & \
wait

# Large result set
ggen market search "*" --limit 100

# Deep pipeline
ggen lifecycle pipeline init setup build test lint security-scan deploy clean
```

---

## 🔧 Test Implementation Checklist

### Phase 1: P0 Critical Tests (Week 1)
- [ ] Basic search functionality (5 tests)
- [ ] Input validation for all commands (10 tests)
- [ ] Package add/remove (5 tests)
- [ ] Lifecycle run phases (5 tests)
- [ ] Error handling (10 tests)

### Phase 2: P1 Important Tests (Week 2-3)
- [ ] Filtered searches (8 tests)
- [ ] Cache management (6 tests)
- [ ] Offline operations (6 tests)
- [ ] Pipeline execution (8 tests)
- [ ] Readiness tracking (8 tests)
- [ ] Concurrent operations (6 tests)

### Phase 3: Stress Tests (Week 4)
- [ ] Concurrent operations (6 tests)
- [ ] Resource exhaustion (8 tests)
- [ ] Edge cases (15 tests)
- [ ] Boundary conditions (6 tests)

### Phase 4: Performance Benchmarks (Week 5)
- [ ] Latency benchmarks (6 benchmarks)
- [ ] Throughput tests (4 load profiles)
- [ ] Memory profiling (6 operations)

### Phase 5: CI/CD Integration (Week 5)
- [ ] GitHub Actions workflow
- [ ] Nightly test runs
- [ ] Pre-release validation
- [ ] Performance regression detection

---

## 📁 File Locations

| File | Path | Purpose |
|------|------|---------|
| **JSON Matrix** | `/tests/stress/permutation_test_matrix.json` | Full permutation data |
| **Summary** | `/tests/stress/TEST_MATRIX_SUMMARY.md` | Detailed analysis |
| **Visualization** | `/tests/stress/test_matrix_visualization.txt` | ASCII art overview |
| **Quick Reference** | `/tests/stress/QUICK_REFERENCE.md` | This file |
| **Hive Memory** | `.swarm/memory.db` | Key: `hive/analysis/test_matrix` |

---

## 🎓 Testing Best Practices

### 1. London TDD Pattern (Already in Use)
```rust
#[cfg_attr(test, mockall::automock)]
pub trait MarketplaceClient {
    fn search(&self, query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>>;
}

#[tokio::test]
async fn test_search_calls_client() {
    let mut mock_client = MockMarketplaceClient::new();
    mock_client
        .expect_search()
        .with(eq(String::from("rust")), always())
        .times(1)
        .returning(|_, _| Ok(vec![/* ... */]));

    let result = run_with_deps(&args, &mock_client).await;
    assert!(result.is_ok());
}
```

### 2. Property-Based Testing (For Permutations)
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_search_query_validation(query in "\\PC{1,1000}") {
        let args = SearchArgs {
            query,
            limit: 10,
            // ... other fields
        };

        let result = validate_search_input(&args);
        // Should not panic, should return Ok or valid error
        prop_assert!(result.is_ok() || result.is_err());
    }
}
```

### 3. OpenTelemetry Instrumentation (Already in Use)
```rust
#[tracing::instrument(name = "ggen.market.search", skip(args), fields(
    query = %args.query,
    limit = args.limit
))]
pub async fn run(args: &SearchArgs) -> Result<()> {
    let _validate_span = tracing::info_span!("validate_input").entered();
    validate_search_input(args)?;
    drop(_validate_span);

    // ... rest of implementation
}
```

---

## 🚨 Critical Path Coverage

**These 8 paths MUST have 100% test coverage:**

1. `cli/src/cmds/market/search.rs::run`
2. `cli/src/cmds/market/add.rs::run`
3. `cli/src/cmds/lifecycle/mod.rs::run_single_phase`
4. `ggen-marketplace/src/backend/centralized.rs::search`
5. `ggen-marketplace/src/storage/filesystem.rs`
6. `cli/src/cmds/market/search.rs::validate_search_input`
7. `cli/src/cmds/market/add.rs::validate_gpack_input`
8. `cli/src/cmds/lifecycle/mod.rs::validate_for_deployment`

**Validation**:
```bash
cargo tarpaulin --out Html --output-dir coverage
open coverage/index.html
```

---

## 📞 Contact & Coordination

**Matrix Owner**: Analyst Agent Beta (Hive Mind Collective)
**Hive Memory Key**: `hive/analysis/test_matrix`
**Status**: ✅ Ready for Implementation

**Next Actions**:
1. **Researcher Alpha**: Validate approach, identify gaps
2. **Coordinator**: Assign implementation tasks
3. **Tester Agent**: Implement P0 tests first
4. **Performance Agent**: Set up benchmark infrastructure
5. **Integration Agent**: Build CI/CD pipeline

---

## 💡 Quick Tips

1. **Start with smoke tests** - 2 minutes to verify environment
2. **Focus on P0 first** - 35 tests that matter most
3. **Use proptest for combinatorics** - Don't write 2048 manual tests
4. **Mock external dependencies** - Marketplace API, filesystem
5. **Instrument with tracing** - Performance insights for free
6. **Run benchmarks regularly** - Catch regressions early
7. **Test concurrent scenarios** - Use `loom` or `tokio::test`
8. **Validate on multiple platforms** - Linux, macOS, Windows

---

**Last Updated**: 2025-11-01T18:20:00Z
**Status**: ✅ Analysis Complete, Ready for Implementation
