<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [OTEL Optional Feature - Validation Checklist](#otel-optional-feature---validation-checklist)
  - [Implementation Complete](#implementation-complete)
  - [Validation Tasks](#validation-tasks)
    - [Compilation Validation](#compilation-validation)
    - [Test Validation](#test-validation)
    - [Linting Validation](#linting-validation)
    - [Dependency Validation](#dependency-validation)
    - [Performance Validation](#performance-validation)
    - [Documentation Validation](#documentation-validation)
    - [Regression Testing](#regression-testing)
    - [CI/CD Integration](#cicd-integration)
  - [Automated Validation Script](#automated-validation-script)
  - [Success Criteria](#success-criteria)
  - [Validation Evidence](#validation-evidence)
    - [Compilation Results](#compilation-results)
    - [Test Results](#test-results)
    - [Performance Results](#performance-results)
    - [Dependency Results](#dependency-results)
  - [Sign-Off](#sign-off)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# OTEL Optional Feature - Validation Checklist

## Implementation Complete

All code changes have been implemented to make OpenTelemetry optional.

## Validation Tasks

### Compilation Validation

- [ ] **Check compilation without otel feature**
  - Command: `cargo check --package ggen-core --no-default-features`
  - Expected: Clean compilation with no errors
  - Validates: Feature gates work correctly, no OTEL code in default build

- [ ] **Check compilation with otel feature**
  - Command: `cargo check --package ggen-core --features otel`
  - Expected: Clean compilation with OTEL dependencies
  - Validates: OTEL integration still works when enabled

- [ ] **Verify workspace-level compilation**
  - Command: `cargo check --workspace --no-default-features`
  - Expected: All crates compile without OTEL
  - Validates: No broken dependencies across workspace

### Test Validation

- [ ] **Run unit tests without otel**
  - Command: `cargo test --package ggen-core --lib --no-default-features`
  - Expected: All tests pass
  - Validates: Core functionality works without OTEL

- [ ] **Run unit tests with otel**
  - Command: `cargo test --package ggen-core --lib --features otel`
  - Expected: All tests pass including OTEL-specific tests
  - Validates: OTEL instrumentation works correctly

- [ ] **Run integration tests**
  - Command: `cargo test --package ggen-core --test '*' --no-default-features`
  - Expected: Integration tests pass without OTEL
  - Validates: No runtime dependencies on OTEL

### Linting Validation

- [ ] **Run clippy without otel**
  - Command: `cargo clippy --package ggen-core --no-default-features -- -D warnings`
  - Expected: No warnings or errors
  - Validates: Code quality maintained without OTEL

- [ ] **Run clippy with otel**
  - Command: `cargo clippy --package ggen-core --features otel -- -D warnings`
  - Expected: No warnings or errors
  - Validates: OTEL code meets quality standards

- [ ] **Run rustfmt**
  - Command: `cargo fmt --check`
  - Expected: Code properly formatted
  - Validates: Formatting consistent across all changes

### Dependency Validation

- [ ] **Count dependencies without otel**
  - Command: `cargo tree --package ggen-core --no-default-features | wc -l`
  - Expected: ~300 dependencies (reduced from ~500)
  - Validates: OTEL dependencies excluded

- [ ] **Count dependencies with otel**
  - Command: `cargo tree --package ggen-core --features otel | wc -l`
  - Expected: ~500 dependencies (includes OTEL stack)
  - Validates: OTEL dependencies included when enabled

- [ ] **Verify no duplicate OTEL versions**
  - Command: `cargo tree --package ggen-core --features otel -d`
  - Expected: Single version of each OTEL crate
  - Validates: Clean dependency resolution

### Performance Validation

- [ ] **Measure clean build time without otel**
  - Command: `cargo clean && time cargo build --package ggen-core --no-default-features`
  - Expected: Baseline build time recorded
  - Validates: Build performance baseline

- [ ] **Measure clean build time with otel**
  - Command: `cargo clean && time cargo build --package ggen-core --features otel`
  - Expected: Significantly longer than baseline (~50% slower)
  - Validates: OTEL adds build time overhead

- [ ] **Measure incremental build time without otel**
  - Command: `touch crates/ggen-core/src/lib.rs && time cargo build --package ggen-core --no-default-features`
  - Expected: Fast incremental rebuild
  - Validates: Incremental compilation works without OTEL

- [ ] **Compare binary sizes**
  - Command: `ls -lh target/release/ggen` (with and without otel)
  - Expected: Minimal size difference (code is conditionally compiled)
  - Validates: No runtime overhead when disabled

### Documentation Validation

- [ ] **Generate documentation without otel**
  - Command: `cargo doc --package ggen-core --no-default-features --no-deps`
  - Expected: Documentation builds successfully
  - Validates: Doc comments don't reference OTEL when disabled

- [ ] **Generate documentation with otel**
  - Command: `cargo doc --package ggen-core --features otel --no-deps`
  - Expected: Full documentation including OTEL APIs
  - Validates: OTEL documentation accessible when enabled

- [ ] **Verify README accuracy**
  - Review: `/docs/features/otel-optional-feature.md`
  - Expected: Accurate usage examples and migration guide
  - Validates: User-facing documentation is clear

### Regression Testing

- [ ] **Run full test suite**
  - Command: `cargo make test`
  - Expected: All workspace tests pass
  - Validates: No regressions introduced

- [ ] **Run clippy on workspace**
  - Command: `cargo make lint`
  - Expected: No new warnings across workspace
  - Validates: Code quality maintained globally

- [ ] **Verify SLO compliance**
  - Command: `cargo make slo-check`
  - Expected: All SLOs met (build times, memory usage)
  - Validates: Performance targets maintained

### CI/CD Integration

- [ ] **Update CI configuration**
  - File: `.github/workflows/*.yml`
  - Change: Add `--features otel` to production builds
  - Validates: CI builds with correct features

- [ ] **Test development build in CI**
  - Command: CI runs `cargo check` (default features)
  - Expected: Fast CI feedback without OTEL
  - Validates: CI benefits from faster builds

- [ ] **Test production build in CI**
  - Command: CI runs `cargo build --release --features otel`
  - Expected: Production artifacts include OTEL
  - Validates: Production has full observability

## Automated Validation Script

Run the comprehensive validation script:

```bash
# Quick validation (compilation + tests)
./scripts/validate-otel-feature.sh

# Full validation (includes build time benchmarks)
FULL_BENCHMARK=1 ./scripts/validate-otel-feature.sh
```

## Success Criteria

All validation tasks must pass before merging:

1. ✅ Compiles cleanly with and without `otel` feature
2. ✅ All tests pass in both configurations
3. ✅ No clippy warnings or errors
4. ✅ Dependency count reduced by ~200 crates without `otel`
5. ✅ Build time improved by ~50% without `otel`
6. ✅ Documentation accurate and complete
7. ✅ No regressions in workspace tests
8. ✅ CI/CD configured correctly

## Validation Evidence

### Compilation Results

```
[ ] cargo check --no-default-features: PASS/FAIL
[ ] cargo check --features otel: PASS/FAIL
```

### Test Results

```
[ ] cargo test --no-default-features: PASS/FAIL (X/Y tests passed)
[ ] cargo test --features otel: PASS/FAIL (X/Y tests passed)
```

### Performance Results

```
[ ] Clean build without otel: X.XXs
[ ] Clean build with otel: Y.YYs
[ ] Improvement: Z.ZZ% faster
```

### Dependency Results

```
[ ] Dependencies without otel: XXX crates
[ ] Dependencies with otel: YYY crates
[ ] Reduction: ZZZ crates (~ZZ%)
```

## Sign-Off

- [ ] **Developer**: All validation tasks completed
- [ ] **Reviewer**: Code changes reviewed and approved
- [ ] **CI/CD**: Automated checks passing
- [ ] **Documentation**: User-facing docs updated

## References

- [OTEL Optional Feature Documentation](/docs/features/otel-optional-feature.md)
- [Validation Script](/scripts/validate-otel-feature.sh)
- [CLAUDE.md - Build Commands](/CLAUDE.md#build-commands)
- [Dependency Deduplication Plan](/examples/factory-paas/DEPENDENCY_DEDUPLICATION_PLAN.md)
