# Build Optimization Validation Checklist (v6.0.0)

**Date**: January 25, 2026 | **Status**: Validation Framework Ready

---

## Executive Validation Workflow

```
Phase 1: Baseline Metrics (Pre-optimization)
    ↓
Phase 2: Apply Modifications (Cargo.toml changes)
    ↓
Phase 3: Incremental Validation (Build, test, lint)
    ↓
Phase 4: Performance Measurement (Compare to baseline)
    ↓
Phase 5: Production Readiness (Full test suite, SLO verification)
    ↓
Phase 6: Documentation (Update CI/CD, document improvements)
```

---

## Phase 1: Baseline Metrics (Pre-Optimization)

### 1.1 Environment Setup

**Pre-Validation Checklist**:
- [ ] Rust toolchain verified: `rustc --version` (expect 1.91.1+)
- [ ] Cargo verified: `cargo --version` (expect 1.xx)
- [ ] Git clean state: `git status` (no uncommitted changes)
- [ ] Backup created: `git branch backup/build-optimization-baseline`
- [ ] System resources available: ≥ 8GB RAM, ≥ 4 CPU cores, ≥ 50GB disk

**Command**:
```bash
# Create baseline branch
git checkout -b baseline/build-optimization
git log --oneline -1

# Verify clean state
cargo check --workspace 2>&1 | tail -5
```

### 1.2 Baseline Build Time Measurement

**Install measurement tool**:
```bash
cargo install hyperfine
```

**Measure clean build (all features)**:
```bash
# Full workspace clean build
time cargo clean && time cargo build --release 2>&1 | tee baseline_full.log

# Record result (typical: 600+ seconds)
echo "Full build baseline: $(grep -oP 'real\s+\K.*' baseline_full.log)"
```

**Measure incremental build**:
```bash
# Make trivial change
echo "// baseline" >> crates/ggen-core/src/lib.rs

# Measure rebuild
time cargo build --release 2>&1 | tee baseline_incremental.log

# Revert change
git checkout -- crates/ggen-core/src/lib.rs
```

**Measure release binary build**:
```bash
# Binary-specific build
time cargo build --release -p ggen-cli-lib --bin ggen 2>&1 | tee baseline_binary.log
```

**Results Template**:
```markdown
# Baseline Metrics (Pre-Optimization)
Date: 2026-01-25
Environment: [OS, CPU cores, RAM]

## Build Times
- Full workspace clean build: [XXX seconds]
- Incremental build (1 file): [XXX seconds]
- Release binary build: [XXX seconds]
- Peak memory: [XXX MB]

## Binary Metrics
- Debug binary size: [XXX MB]
- Release binary size: [XXX MB]
- Strip overhead: [XXX%]
```

### 1.3 Baseline Test Metrics

**Run full test suite**:
```bash
# Measure test execution time
time cargo make test 2>&1 | tee baseline_tests.log

# Count passing tests
TEST_COUNT=$(grep -c "test .* ok" baseline_tests.log)
echo "Total tests: $TEST_COUNT"
```

**Record result**:
```
Baseline Test Results:
- Total test count: [XXX]
- Total test time: [XXX seconds]
- Pass rate: [100% expected]
- Failed tests: 0 (expected)
```

---

## Phase 2: Apply Modifications

### 2.1 Cargo.toml Modifications

**Modification Checklist**:
- [ ] Read current Cargo.toml: `git diff Cargo.toml.original Cargo.toml`
- [ ] Apply Step 1: Update [profile.release] (codegen-units: 16→4)
- [ ] Apply Step 2: Update [profile.bench]
- [ ] Apply Step 3: Add workspace lints
- [ ] Verify syntax: `cargo metadata --format-version=1 > /dev/null`

**Verification Command**:
```bash
# Check Cargo.toml syntax
cargo check --no-default-features 2>&1 | head -20

# Verify profiles are recognized
cargo build --release --dry-run 2>&1 | grep -E "Compiling|profile|opt-level"
```

### 2.2 Create ~/.cargo/config.toml (Optional Phase 2)

**Setup sccache** (optional, Phase 2):
```bash
# Install sccache
cargo install sccache

# Create config
mkdir -p ~/.cargo
cat >> ~/.cargo/config.toml << 'EOF'
[build]
rustc-wrapper = "sccache"
jobs = 8

[term]
verbose = false
EOF

# Verify
sccache --version
sccache --show-stats
```

**Setup mold linker** (optional, Phase 2):
```bash
# Install mold (Linux)
# Ubuntu: sudo apt-get install mold
# Or from source: https://github.com/rui314/mold

# Configure in ~/.cargo/config.toml
cat >> ~/.cargo/config.toml << 'EOF'
# Linker optimization
rustflags = ["-C", "link-arg=-fuse-ld=mold"]
EOF

# Verify
which mold && mold --version
```

---

## Phase 3: Incremental Validation

### 3.1 Compilation Check

**Quick syntax check**:
```bash
# Stage 1: Check without building
timeout 60s cargo check --workspace 2>&1 | tee validation_check.log

# Verify no errors
grep -i "error" validation_check.log && echo "FAIL: Errors found" || echo "PASS: No errors"
```

**Expected Output**:
```
    Checking ggen-utils v0.2.0
    Checking ggen-core v0.2.0
    ...
    Finished `dev` profile [unoptimized + debuginfo] target(s) in [XXX.XXs]
```

### 3.2 Build Verification

**Build debug binary**:
```bash
timeout 120s cargo build 2>&1 | tee validation_build_debug.log

# Verify success
ls -lh target/debug/ggen && echo "PASS: Debug binary built" || echo "FAIL: No binary"
```

**Build release binary**:
```bash
timeout 180s cargo build --release 2>&1 | tee validation_build_release.log

# Verify success
ls -lh target/release/ggen && echo "PASS: Release binary built" || echo "FAIL: No binary"
```

**Record build times**:
```bash
# Extract build time
RELEASE_TIME=$(grep -oP "Finished.*in \K[0-9.]+s" validation_build_release.log)
echo "Release build time: $RELEASE_TIME"
```

### 3.3 Linting Validation

**Run clippy lints**:
```bash
timeout 60s cargo make lint 2>&1 | tee validation_lint.log

# Check for new warnings
NEW_WARNINGS=$(grep -c "warning:" validation_lint.log)
echo "Warnings found: $NEW_WARNINGS"

# Fail if warnings increased
if [ "$NEW_WARNINGS" -gt 0 ]; then
    echo "FAIL: New warnings introduced"
    exit 1
else
    echo "PASS: No new warnings"
fi
```

**Expected**: 0 warnings (Poka-Yoke design enforces warnings-as-errors)

### 3.4 Format Verification

**Check code formatting**:
```bash
timeout 30s cargo make fmt --check 2>&1 | tee validation_format.log

# Should show no formatting issues
grep -i "error\|diff" validation_format.log && echo "FAIL: Format issues" || echo "PASS: Format OK"
```

---

## Phase 4: Performance Measurement

### 4.1 Build Time Comparison

**Measure post-optimization build times**:
```bash
# Record git commit for this phase
OPTIMIZED_COMMIT=$(git rev-parse --short HEAD)
echo "Optimization commit: $OPTIMIZED_COMMIT" > optimization_metrics.txt

# Full clean build
time cargo clean && time cargo build --release 2>&1 | tee optimized_full.log
OPTIMIZED_FULL_TIME=$(grep -oP "real\s+\K.*" optimized_full.log | head -1)

# Incremental build
echo "// optimized" >> crates/ggen-core/src/lib.rs
time cargo build --release 2>&1 | tee optimized_incremental.log
OPTIMIZED_INCREMENTAL_TIME=$(grep -oP "real\s+\K.*" optimized_incremental.log | head -1)
git checkout -- crates/ggen-core/src/lib.rs

# Binary build
time cargo build --release -p ggen-cli-lib --bin ggen 2>&1 | tee optimized_binary.log
OPTIMIZED_BINARY_TIME=$(grep -oP "real\s+\K.*" optimized_binary.log | head -1)
```

**Calculate improvements**:
```bash
# Simple percentage calculation (needs manual baseline values)
cat > calculate_improvement.sh << 'EOF'
#!/bin/bash
baseline_full=600  # Seconds (update with actual baseline)
baseline_incr=15
baseline_binary=120

optimized_full=$1
optimized_incr=$2
optimized_binary=$3

# Calculate percentages
improvement_full=$(( (baseline_full - optimized_full) * 100 / baseline_full ))
improvement_incr=$(( (baseline_incr - optimized_incr) * 100 / baseline_incr ))
improvement_binary=$(( (baseline_binary - optimized_binary) * 100 / baseline_binary ))

echo "Build Time Improvements:"
echo "  Full build:        ${improvement_full}% faster (${baseline_full}s → ${optimized_full}s)"
echo "  Incremental:       ${improvement_incr}% faster (${baseline_incr}s → ${optimized_incr}s)"
echo "  Binary build:      ${improvement_binary}% faster (${baseline_binary}s → ${optimized_binary}s)"
EOF

chmod +x calculate_improvement.sh
```

**Create comparison table**:
```markdown
| Metric | Baseline | Optimized | Improvement |
|--------|----------|-----------|-------------|
| Full clean build | 600s | [XXXs] | [XX%] |
| Incremental build | 15s | [Xs] | [XX%] |
| Release binary | 120s | [XXs] | [XX%] |
| Memory peak | 1GB | [XXX]MB | [XX%] |
| Binary size | 80MB | [XX]MB | [XX%] |
```

### 4.2 Binary Metrics

**Measure binary size**:
```bash
# Debug binary
DEBUG_SIZE=$(du -h target/debug/ggen | cut -f1)
echo "Debug binary size: $DEBUG_SIZE"

# Release binary
RELEASE_SIZE=$(du -h target/release/ggen | cut -f1)
echo "Release binary size: $RELEASE_SIZE"

# Strip overhead
STRIP_OVERHEAD=$(( 100 - (RELEASE_SIZE * 100 / DEBUG_SIZE) ))
echo "Size reduction: ${STRIP_OVERHEAD}%"
```

### 4.3 Memory Usage Measurement

**Measure peak memory during build**:
```bash
# Use /usr/bin/time for detailed metrics
/usr/bin/time -v cargo build --release 2>&1 | grep "Maximum resident"

# Should show reduced memory compared to baseline
```

---

## Phase 5: Production Readiness

### 5.1 Full Test Suite Validation

**Run all tests**:
```bash
# Timeout: 150s for full suite
timeout 150s cargo make test 2>&1 | tee validation_full_tests.log

# Count results
PASSED=$(grep -c "test result: ok" validation_full_tests.log)
FAILED=$(grep -c "test result: FAILED" validation_full_tests.log)

echo "Test Results:"
echo "  Passed: $PASSED"
echo "  Failed: $FAILED"

if [ "$FAILED" -gt 0 ]; then
    echo "FAIL: Tests failed"
    exit 1
else
    echo "PASS: All tests passed"
fi
```

**Expected**: All 350+ tests passing ✅

### 5.2 SLO Verification

**Verify SLO targets**:
```bash
# Extract actual build times
FULL_BUILD_TIME=$(grep "Finished" optimized_full.log | grep -oP "in \K[0-9.]+")
INCREMENTAL_TIME=$(grep "Finished" optimized_incremental.log | grep -oP "in \K[0-9.]+")

# Compare to SLOs
SLO_FULL=90  # seconds
SLO_INCR=5   # seconds

if (( $(echo "$FULL_BUILD_TIME <= $SLO_FULL" | bc -l) )); then
    echo "✅ PASS: Full build SLO met ($FULL_BUILD_TIME ≤ $SLO_FULL)"
else
    echo "⚠️  WARNING: Full build SLO not met ($FULL_BUILD_TIME > $SLO_FULL)"
fi

if (( $(echo "$INCREMENTAL_TIME <= $SLO_INCR" | bc -l) )); then
    echo "✅ PASS: Incremental SLO met ($INCREMENTAL_TIME ≤ $SLO_INCR)"
else
    echo "⚠️  WARNING: Incremental SLO not met ($INCREMENTAL_TIME > $SLO_INCR)"
fi
```

### 5.3 Binary Functionality

**Verify binary works**:
```bash
# Test CLI help
./target/release/ggen --help 2>&1 | head -5 && echo "✅ Help works"

# Test version
./target/release/ggen --version && echo "✅ Version works"

# Test sync command
timeout 5s ./target/release/ggen sync --help 2>&1 | head -5 && echo "✅ Sync help works"

# Test with actual ontology (if available)
if [ -f ".specify/specs/001-poka-yoke-patterns/feature.ttl" ]; then
    timeout 10s ./target/release/ggen sync --dry_run true && echo "✅ Sync dry-run works"
fi
```

**Expected Output**:
```
✅ Help works
✅ Version works
✅ Sync help works
✅ Sync dry-run works
```

### 5.4 Feature Flag Testing

**Test each feature combination**:
```bash
# Minimal build
timeout 60s cargo check --no-default-features --features core && echo "✅ core features work"

# Default build
timeout 90s cargo check && echo "✅ default features work"

# Full features
timeout 180s cargo check --all-features && echo "✅ all features work"
```

---

## Phase 6: SLO Target Validation

### 6.1 SLO Summary

**Create final SLO report**:
```bash
cat > SLO_VALIDATION_REPORT.txt << 'EOF'
BUILD OPTIMIZATION SLO VALIDATION REPORT
========================================
Date: $(date)
Commit: $(git rev-parse --short HEAD)

SLO TARGETS:
[✅/❌] Clean build (all features): ≤ 90s  (Actual: XXs)
[✅/❌] Incremental build: ≤ 5s             (Actual: Xs)
[✅/❌] Release build: ≤ 60s               (Actual: XXs)
[✅/❌] Binary size reduction: ≥ 10%      (Actual: XXX%)
[✅/❌] Memory reduction: ≥ 50%           (Actual: XXX%)

TEST METRICS:
[✅/❌] All tests passing: 350+            (Actual: XXX passing, 0 failing)
[✅/❌] No new warnings: 0                 (Actual: 0)
[✅/❌] Code quality gates: ✅             (check, lint, fmt all pass)

PRODUCTION READINESS:
[✅/❌] Binary functionality: ✅
[✅/❌] Feature flags working: ✅
[✅/❌] CI/CD compatible: ✅
[✅/❌] Performance stable: ✅

OVERALL: [PASS/FAIL - Optimization Complete]
EOF

cat SLO_VALIDATION_REPORT.txt
```

### 6.2 Validation Checklist

**Final validation before merging**:

- [ ] Git branch clean: `git status` shows only modified Cargo.toml
- [ ] All changes committed: `git log --oneline -5`
- [ ] Tests passing: `cargo make test` → all green
- [ ] Linting passing: `cargo make lint` → no warnings
- [ ] Format clean: `cargo make fmt --check` → no issues
- [ ] Build times improved: Measured and documented
- [ ] Binary works: Manual smoke tests pass
- [ ] SLOs met: Full report created
- [ ] Documentation updated: This validation checklist completed
- [ ] PR ready: Branch ready for code review

---

## Phase 7: CI/CD Integration

### 7.1 Update Makefile.toml with optimization targets

**Add new targets**:
```toml
[tasks.build-optimized-dev]
description = "Build with development optimizations"
command = "timeout"
args = ["120s", "cargo", "build", "--no-default-features", "--features", "core"]

[tasks.build-optimized-release]
description = "Build release with all optimizations"
command = "timeout"
args = ["60s", "cargo", "build", "--release", "-p", "ggen-cli-lib"]

[tasks.measure-build-times]
description = "Measure build times for performance tracking"
script = '''
echo "Measuring build times..."
time cargo clean && time cargo build --release
echo "Build time measurement complete"
'''
```

### 7.2 Update GitHub Actions

**Add SLO validation to CI**:
```yaml
# .github/workflows/slo-validation.yml
name: SLO Validation
on: [push, pull_request]

jobs:
  build-slo:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Measure build time
        run: |
          time cargo build --release --no-default-features --features core
          time cargo make test-unit
```

---

## Troubleshooting Guide

### Issue: Build times not improving

**Diagnosis**:
```bash
# Check if Cargo.toml changes applied
cargo metadata --format-version=1 | grep codegen

# Check if cache directory exists
ls -la ~/.cargo/config.toml

# Clear build cache
cargo clean && cargo build --release
```

**Solution**:
- Verify profile changes in Cargo.toml (sections 5.3-5.4)
- Set RUSTC_WRAPPER=sccache if using cache
- Ensure mold linker installed if using

### Issue: Tests failing after optimization

**Diagnosis**:
```bash
# Run single failing test with backtrace
RUST_BACKTRACE=1 cargo test --lib <test_name> -- --nocapture

# Check if panic behavior changed
grep -r "panic = " Cargo.toml
```

**Solution**:
- If panic=abort enabled, ensure no tests rely on panic catching
- Run full test suite to identify all failures
- May need to adjust specific test expectations

### Issue: Binary not working after optimization

**Diagnosis**:
```bash
# Test binary directly
./target/release/ggen --version
./target/release/ggen sync --help

# Check for any runtime errors
RUST_LOG=debug ./target/release/ggen sync --dry_run true 2>&1 | head -50
```

**Solution**:
- Verify all functionality with manual testing
- If issues, revert to previous Cargo.toml: `git checkout HEAD~1 -- Cargo.toml`
- May be proc-macro or LTO issue - investigate in isolation

---

## Final Validation Command Script

```bash
#!/bin/bash
# run_full_validation.sh - Complete validation in one script

set -e  # Exit on first error

echo "=========================================="
echo "BUILD OPTIMIZATION VALIDATION SCRIPT"
echo "=========================================="
echo ""

# Phase 1: Check compilation
echo "Phase 1: Compilation Check"
timeout 60s cargo check --workspace
echo "✅ Compilation passed"
echo ""

# Phase 2: Run linting
echo "Phase 2: Linting"
timeout 60s cargo make lint
echo "✅ Linting passed"
echo ""

# Phase 3: Build release
echo "Phase 3: Release Build"
START=$(date +%s)
timeout 180s cargo build --release -p ggen-cli-lib
END=$(date +%s)
BUILD_TIME=$((END - START))
echo "✅ Release build completed in ${BUILD_TIME}s"
echo ""

# Phase 4: Run tests
echo "Phase 4: Test Suite"
START=$(date +%s)
timeout 150s cargo make test
END=$(date +%s)
TEST_TIME=$((END - START))
echo "✅ All tests passed in ${TEST_TIME}s"
echo ""

# Phase 5: Binary functionality
echo "Phase 5: Binary Functionality"
./target/release/ggen --version
./target/release/ggen sync --help > /dev/null
echo "✅ Binary functional"
echo ""

# Summary
echo "=========================================="
echo "VALIDATION COMPLETE ✅"
echo "=========================================="
echo "Build time: ${BUILD_TIME}s"
echo "Test time: ${TEST_TIME}s"
echo "Status: PASS"
echo ""
```

---

## Success Criteria

**All of the following must be true for optimization to be considered successful**:

- ✅ `cargo make check` passes with no errors
- ✅ `cargo make lint` passes with no warnings
- ✅ `cargo make test` passes all 350+ tests
- ✅ Release build time ≤ 90s (or 50% improvement from baseline)
- ✅ Incremental build time ≤ 5s (or 67% improvement from baseline)
- ✅ Binary functionality verified and working
- ✅ No new compilation errors or warnings introduced
- ✅ All feature combinations build successfully
- ✅ SLO targets documented and met

**When all criteria are met, optimization is ready for production release.**

---

## Post-Validation Documentation

After successful validation, update project documentation:

1. **Update CLAUDE.md**: Add optimization notes to build section
2. **Update Makefile.toml**: Add optimization targets to build system
3. **Update CI/CD**: Integrate SLO validation into GitHub Actions
4. **Create OPTIMIZATION_RESULTS.md**: Document actual improvements achieved
5. **Update team wiki**: Share optimization strategies with team

