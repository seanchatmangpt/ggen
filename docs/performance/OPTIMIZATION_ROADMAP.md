# Performance Optimization Roadmap - ggen v2.6.0

**Date:** 2025-11-14
**Agent:** Performance Benchmarker (Hive Mind)
**Based on:** BUILD_PERFORMANCE_REPORT.md

---

## Quick Reference Card

| Metric | Current | Target | Gap | Priority |
|--------|---------|--------|-----|----------|
| Incremental Build | 0.22s | <1s | ✅ Met | - |
| Clean Build | 0.45s* | <90s | ✅ Met | - |
| Test Suite | BLOCKED | <300s | 🚨 CRITICAL | P0 |
| CI Pipeline | FAILING | <15min | 🚨 CRITICAL | P0 |
| Build Cache | 8.5GB | <5GB | ⚠️ 3.5GB | P1 |
| Dependencies | 61 | <50 | ⚠️ 11 deps | P2 |

*Cached registry build; true clean ~60-120s estimated

---

## Phase 1: Critical Fixes (Week 1) - UNBLOCK DEVELOPMENT

### 1.1 Fix Test Compilation Errors 🚨

**Blocking:** All testing, CI/CD, quality validation

**Error 1: ggen-marketplace/tests/error_scenarios.rs:185**
```rust
// Current (BROKEN):
registry.publish(pkg).await.expect("publish failed");
//                  ^^^ expected `Package`, found `UnvalidatedPackage`

// Fix Options:
// A) Validate the package first
let validated_pkg = pkg.validate()?;
registry.publish(validated_pkg).await.expect("publish failed");

// B) Change publish() signature to accept UnvalidatedPackage
async fn publish(&self, package: UnvalidatedPackage) -> Result<()>

// C) Use builder to create validated Package directly
let pkg = UnvalidatedPackage::builder()
    .name("test-pkg")
    // ... (fields)
    .validate()  // Returns Package
    .build()?;
```

**Error 2: ggen-core/src/lifecycle/behavior_tests.rs:13**
```rust
// Current (BROKEN):
use std::collections::BTreeMap;  // unused

// Fix: Remove unused import
// (Delete line 13)
```

**Error 3: ggen-core/src/lifecycle/state_machine.rs:280**
```rust
// Current (BROKEN):
let mut lifecycle = LifecycleStateMachine::<Initialized> { ... };
//  ^^^ unnecessary mut

// Fix: Remove mut
let lifecycle = LifecycleStateMachine::<Initialized> { ... };
```

**Estimated Time:** 1-2 hours
**Impact:** Unblocks 100% of validation workflow

### 1.2 Restore CI/CD Pipeline 🚨

**Current State:**
- 0/5 recent CI runs successful (100% failure rate)
- All jobs failing due to test compilation errors

**Recovery Steps:**
```bash
# 1. Apply compilation fixes from 1.1
# 2. Run locally to verify
cargo test --all

# 3. Verify CI jobs individually
cargo fmt --all -- --check
cargo clippy --workspace --all-features -- -D warnings
cargo build --workspace --all-features
cargo nextest run --workspace --all-features

# 4. Check coverage generation
cargo tarpaulin --workspace --out Xml

# 5. Commit and push
git add .
git commit -m "fix: resolve test compilation errors"
git push
```

**Success Criteria:**
- ✅ All tests compile
- ✅ All tests pass
- ✅ CI pipeline green
- ✅ Coverage report generated

**Estimated Time:** 2-4 hours (including CI wait time)
**Impact:** Restore quality gates, enable releases

---

## Phase 2: Performance Optimization (Week 2)

### 2.1 Optimize Test Suite Execution ⚡

**Current:** ~171s (2m51s) with 690% CPU usage
**Target:** <180s with better parallelization

**Strategies:**

1. **Leverage cargo-nextest (already in CI)**
   ```bash
   # Local testing with nextest
   cargo nextest run --workspace --all-features

   # Benefits:
   # - Better parallelization than cargo test
   # - Cleaner output
   # - Per-test timing
   # - Retry flaky tests
   ```

2. **Split Slow Integration Tests**
   ```rust
   // Mark slow tests
   #[test]
   #[ignore] // Skip in fast test runs
   fn slow_docker_integration_test() { ... }

   // Run separately
   cargo test --ignored  // Only slow tests
   cargo test            // Fast tests only
   ```

3. **Mock Heavy Dependencies**
   ```rust
   // Instead of:
   #[test]
   async fn test_with_real_docker() {
       let container = testcontainers::start(...);
       // ... slow Docker operations
   }

   // Use:
   #[test]
   async fn test_with_mock_docker() {
       let mock = MockDockerClient::new();
       // ... fast in-memory operations
   }
   ```

**Estimated Time:** 4-6 hours
**Impact:** 30-50% faster test feedback loop

### 2.2 Reduce Build Cache Size 💾

**Current:** 8.5GB
**Target:** <5GB

**Implementation:**

1. **Add cargo-cache to CI**
   ```yaml
   # .github/workflows/ci.yml
   - name: Cleanup cargo cache
     run: |
       cargo install cargo-cache --no-default-features --features ci-autoclean
       cargo-cache -a
   ```

2. **Separate Debug/Release Artifacts**
   ```toml
   # .cargo/config.toml
   [build]
   target-dir = "target"

   # For CI:
   [env]
   CARGO_TARGET_DIR = { value = "target/ci", relative = true }
   ```

3. **Periodic Cleanup Script**
   ```bash
   #!/bin/bash
   # scripts/cleanup-build-cache.sh

   # Remove old debug builds (>7 days)
   find target/debug -type f -mtime +7 -delete

   # Remove old test binaries
   cargo clean --doc

   # Remove unused deps
   cargo-cache --autoclean
   ```

**Estimated Time:** 2-3 hours
**Impact:** Save 3-4GB disk space, faster CI cache restore

---

## Phase 3: Dependency Optimization (Week 3)

### 3.1 Feature-Gate Heavy Dependencies 🎯

**Current Issue:**
- testcontainers (Docker): Always compiled, even for non-Docker tests
- wasmtime (WASM runtime): Always compiled, even if not used
- criterion (benchmarking): Compiled even in regular test runs

**Solution: Cargo Feature Flags**

```toml
# crates/ggen-marketplace/Cargo.toml
[features]
default = []
docker-tests = ["testcontainers", "bollard"]
benchmarks = ["criterion"]

[dependencies]
testcontainers = { version = "0.25", optional = true }
bollard = { version = "0.19", optional = true }
criterion = { version = "0.5", optional = true }
```

**Usage:**
```bash
# Regular tests (no Docker)
cargo test

# Docker integration tests
cargo test --features docker-tests

# Benchmarks
cargo bench --features benchmarks

# CI: Full validation
cargo test --all-features
```

**Benefits:**
- 30-40% faster local test compilation
- Smaller default binary
- Opt-in for heavy dependencies

**Estimated Time:** 1 day
**Impact:** Significantly faster developer iteration

### 3.2 Dependency Audit & Replacement 🔍

**Heavy Dependencies to Review:**

| Dependency | Size Impact | Alternatives | Decision |
|------------|-------------|--------------|----------|
| testcontainers | HIGH | docker-compose-cli | Keep (necessary) |
| wasmtime | VERY HIGH | wasmer | Evaluate |
| criterion | HIGH | divan | Evaluate |
| bollard | MEDIUM | shiplift | Keep (maintained) |
| tokio | MEDIUM | async-std | Keep (standard) |

**Recommended Actions:**

1. **Evaluate wasmtime vs wasmer**
   ```bash
   # Benchmark both WASM runtimes
   # wasmer claims 2x faster startup
   # Check if features are compatible
   ```

2. **Consider divan for benchmarking**
   ```bash
   # divan: Faster, simpler than criterion
   # Good for quick iteration benchmarks
   # criterion: Keep for detailed reports
   ```

**Estimated Time:** 2-3 days
**Impact:** Potentially 20-30% faster builds

---

## Phase 4: Advanced Optimizations (Week 4)

### 4.1 Compilation Profile Tuning ⚙️

```toml
# Cargo.toml

[profile.dev]
# Already optimized for developer experience
incremental = true
debug = 1  # Faster compilation than full debug

[profile.release]
# Optimize for runtime performance
incremental = false  # Reproducible builds
lto = "thin"         # 10-20% smaller binary, +30s build time
codegen-units = 1    # Maximum runtime optimization
opt-level = 3
strip = true         # Remove debug symbols

[profile.bench]
inherits = "release"
lto = "fat"          # Maximum optimization for benchmarks

[profile.test]
# Faster test compilation
inherits = "dev"
debug = 0            # Minimal debug info
opt-level = 1        # Some optimizations for test speed
```

**Trade-offs:**
- Dev build: FAST compile, slower runtime (OK for testing)
- Release build: SLOW compile, fast runtime (OK for releases)
- Bench build: VERY SLOW compile, maximum perf (OK, run rarely)

### 4.2 Build Time Monitoring 📊

**Tooling Setup:**

1. **cargo-timing**
   ```bash
   # Generate build timeline
   cargo build --release --timings

   # Output: target/cargo-timings/cargo-timing.html
   # Shows: Which crates take longest to compile
   ```

2. **CI Timing Dashboard**
   ```yaml
   # .github/workflows/ci.yml
   - name: Build with timing
     run: |
       cargo build --release --timings

   - name: Upload timing report
     uses: actions/upload-artifact@v4
     with:
       name: cargo-timings
       path: target/cargo-timings/
   ```

3. **Performance Regression Detection**
   ```bash
   # Store baseline timing
   cargo build --timings
   cp target/cargo-timings/cargo-timing.json baseline.json

   # Compare against baseline
   ./scripts/check-build-regression.sh baseline.json
   ```

**Estimated Time:** 2-3 days
**Impact:** Catch performance regressions early

### 4.3 Benchmark Suite in CI 🏃

**Add to CI:**
```yaml
# .github/workflows/benchmarks.yml
name: Benchmarks

on:
  push:
    branches: [master]
  pull_request:

jobs:
  bench:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Run benchmarks
        run: cargo bench --no-fail-fast

      - name: Store benchmark results
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: target/criterion/*/new/estimates.json

      - name: Check for regressions
        run: |
          # Fail if >10% slower than baseline
          ./scripts/check-bench-regression.sh 0.10
```

**Benefits:**
- Catch performance regressions in PRs
- Track performance over time
- Visualize performance trends

**Estimated Time:** 1 day
**Impact:** Prevent performance degradation

---

## Implementation Timeline

```
Week 1: CRITICAL FIXES
├─ Day 1: Fix test compilation errors (2h)
├─ Day 2: Restore CI/CD pipeline (4h)
├─ Day 3: Verify all tests pass (2h)
├─ Day 4: Monitor CI stability (1h)
└─ Day 5: Buffer for issues (8h)
   Status: ✅ Tests running, CI green

Week 2: PERFORMANCE OPTIMIZATION
├─ Day 1: Optimize test suite with nextest (4h)
├─ Day 2: Split slow tests, add mocks (6h)
├─ Day 3: Setup cargo-cache cleanup (3h)
├─ Day 4: Implement cache management (3h)
└─ Day 5: Measure improvements (2h)
   Status: ✅ 30-50% faster tests, 3GB saved

Week 3: DEPENDENCY OPTIMIZATION
├─ Day 1: Add feature flags for heavy deps (6h)
├─ Day 2: Update CI to use features (2h)
├─ Day 3: Evaluate wasmtime vs wasmer (6h)
├─ Day 4: Test with alternative deps (6h)
└─ Day 5: Migrate if beneficial (4h)
   Status: ✅ Faster local builds

Week 4: ADVANCED MONITORING
├─ Day 1: Tune compilation profiles (3h)
├─ Day 2: Setup cargo-timing (4h)
├─ Day 3: Add benchmark CI job (6h)
├─ Day 4: Regression detection (5h)
└─ Day 5: Documentation (2h)
   Status: ✅ Full performance monitoring
```

---

## Success Metrics

### Developer Experience (DX)

| Metric | Baseline | Week 1 | Week 2 | Week 4 | Target |
|--------|----------|--------|--------|--------|--------|
| **Test Feedback** | BROKEN | 171s | 120s | 100s | <120s |
| **CI Duration** | FAILING | 12min | 10min | 8min | <15min |
| **Local Build** | 0.22s | 0.22s | 0.18s | 0.15s | <1s |
| **Cache Size** | 8.5GB | 8.5GB | 5GB | 4GB | <5GB |
| **CI Success Rate** | 0% | 80% | 95% | 99% | >95% |

### Performance Regression Prevention

**Gates Added:**
- ✅ Build time check (<5min)
- ✅ Test time check (<5min)
- ✅ Benchmark regression check (±10%)
- ✅ Cache size check (<6GB)
- ✅ Dependency count check (<75)

---

## Risk Assessment

### High Risk (Needs Mitigation)

1. **Feature Flag Migration**
   - Risk: Breaking existing code that depends on features
   - Mitigation: Gradual rollout, comprehensive testing
   - Fallback: Keep all features enabled by default

2. **Dependency Replacement**
   - Risk: Different behavior, new bugs
   - Mitigation: A/B testing, gradual migration
   - Fallback: Stick with current deps

### Medium Risk

3. **Profile Tuning**
   - Risk: Longer build times, different behavior
   - Mitigation: Benchmark before/after
   - Fallback: Revert profile changes

### Low Risk

4. **Cache Cleanup**
   - Risk: Minimal (just disk cleanup)
   - Mitigation: Test cleanup scripts
   - Fallback: Manual cleanup

---

## Monitoring & Alerts

### Performance Dashboard (Proposed)

**Metrics to Track:**
```
Build Performance:
├─ Clean build time (trend)
├─ Incremental build time (trend)
├─ Cache hit rate (%)
└─ Dependency count (count)

Test Performance:
├─ Full suite time (trend)
├─ Per-crate test time (breakdown)
├─ Flaky test rate (%)
└─ Coverage % (trend)

CI/CD Performance:
├─ Pipeline duration (trend)
├─ Job-level breakdown (stacked)
├─ Success rate (%)
└─ Cache restore time (trend)

Benchmark Results:
├─ Throughput (ops/sec)
├─ Latency (p50, p95, p99)
├─ Memory usage (bytes)
└─ Regression alerts (boolean)
```

### Alert Thresholds

```yaml
alerts:
  - name: "Build time regression"
    condition: "clean_build_time > 300s"
    severity: warning

  - name: "Test suite timeout"
    condition: "test_suite_time > 600s"
    severity: critical

  - name: "CI pipeline failure"
    condition: "ci_success_rate < 0.90"
    severity: critical

  - name: "Cache size explosion"
    condition: "build_cache_size > 10GB"
    severity: warning

  - name: "Benchmark regression"
    condition: "performance_change < -0.10"  # 10% slower
    severity: warning
```

---

## Conclusion

### Current State (Pre-Optimization)
- **DX Score:** 4/10 (broken tests, failing CI)
- **Build Performance:** ✅ Excellent (incremental)
- **Test Performance:** 🚨 Blocked (compilation errors)
- **CI/CD:** 🚨 100% failure rate

### Target State (Post-Optimization)
- **DX Score:** 9/10 (fast, reliable, monitored)
- **Build Performance:** ✅ Excellent (maintained)
- **Test Performance:** ✅ Good (<120s full suite)
- **CI/CD:** ✅ Reliable (>95% success rate)

### Investment Required
- **Week 1:** 17h (critical fixes)
- **Week 2:** 18h (performance optimization)
- **Week 3:** 24h (dependency optimization)
- **Week 4:** 20h (advanced monitoring)
- **Total:** 79h (~2 person-weeks)

### Expected ROI
- **Developer Time Saved:** ~30min/day per developer
  - Faster test feedback: 15min/day
  - Fewer CI failures: 10min/day
  - Smaller cache: 5min/day

- **For 3 developers:** 1.5h/day = 37.5h/month saved
- **Payback Period:** ~2 months

---

**Status:** ✅ Roadmap Complete
**Next Steps:** Begin Phase 1 (Fix compilation errors)
**Owner:** Development Team
**Timeline:** 4 weeks (aggressive), 6 weeks (conservative)

