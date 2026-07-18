# Build Issues Fix Plan

**Status**: 🔴 CRITICAL - Build blockers identified
**Last Updated**: 2026-01-24
**Estimated Effort**: 6-8 weeks

---

## Critical Issues Summary

### Issue #1: 7,490 unwrap/expect Violations ⚠️

**Severity**: CRITICAL
**Impact**: Production panics, violates ggen constitutional rules
**Files Affected**: 571 files across workspace
**Estimated Effort**: 2-3 developer-weeks

#### Root Cause
Historical codebase evolution before strict `Result<T,E>` enforcement.

#### Fix Strategy

**Phase 1: Add Clippy Enforcement** (1 day)
```toml
# Add to workspace Cargo.toml [workspace.lints.clippy]
unwrap_used = "deny"
expect_used = "deny"
```

**Phase 2: Automated Refactoring** (1 week)
```bash
# Use cargo-fix to automatically migrate simple cases
cargo fix --all --allow-dirty --broken-code

# Use rust-analyzer code actions for complex cases
# (manual review required)
```

**Phase 3: Manual Review** (1-2 weeks)
Focus on these high-priority modules:
- `ggen-marketplace/src/security.rs` (security-critical)
- `ggen-core/src/codegen/` (generation pipeline)
- `ggen-cli/src/` (user-facing errors)

**Migration Pattern**:
```rust
// BEFORE (violates rules)
let value = some_operation().unwrap();

// AFTER (compliant)
let value = some_operation()
    .map_err(|e| Error::OperationFailed(e.to_string()))?;
```

**Validation**:
```bash
# Verify no violations remain
cargo make lint
grep -r "unwrap()" crates/*/src/ --include="*.rs"  # Should return 0 results
grep -r "expect(" crates/*/src/ --include="*.rs"   # Should return 0 results
```

---

### Issue #2: Build Time >600s (40x over SLO) ⚠️

**Severity**: CRITICAL
**Impact**: Development velocity, CI/CD pipeline timeouts
**Current**: >600s
**Target**: ≤15s
**Estimated Effort**: 1-2 developer-weeks

#### Root Cause Analysis

Run build with timing information:
```bash
cargo build --release --timings
# Open target/cargo-timings/cargo-timing.html
```

**Suspected Bottlenecks**:
1. No dependency caching in CI
2. Excessive proc-macro usage
3. Large workspace with unnecessary rebuilds
4. Debug artifacts in release builds

#### Fix Strategy

**Phase 1: Implement sccache** (2 hours)
```bash
# Install sccache
cargo install sccache

# Configure Cargo to use it
export RUSTC_WRAPPER=sccache

# Verify caching works
sccache --show-stats
```

**Expected Improvement**: 5-10x faster incremental builds

**Phase 2: Optimize Dependencies** (2-3 days)
```toml
# Use cargo-udeps to find unused dependencies
cargo install cargo-udeps
cargo +nightly udeps

# Use cargo-tree to identify duplicate dependencies
cargo tree --duplicates
```

**Phase 3: Workspace Optimization** (3-5 days)
```toml
# Split large crates into smaller focused crates
# Enable parallel compilation
[profile.dev]
split-debuginfo = "unpacked"  # Faster linking
incremental = true             # Enable incremental compilation

[profile.release]
lto = "thin"                   # Faster than "fat" LTO
codegen-units = 16             # Parallel codegen
```

**Phase 4: CI/CD Caching** (1-2 days)
```yaml
# GitHub Actions example
- uses: actions/cache@v3
  with:
    path: |
      ~/.cargo/registry
      ~/.cargo/git
      target
    key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
```

**Validation**:
```bash
# Measure first build
time cargo clean && cargo build --release

# Measure incremental build (should be <2s)
touch crates/ggen-core/src/lib.rs
time cargo build --release

# Verify SLO
cargo make slo-check
```

---

## Non-Critical Issues (Post-Launch)

### Issue #3: Missing SEO Content Publishing Implementation

**Agent**: backend-dev (not found during parallel execution)
**Impact**: SEO workflow incomplete
**Estimated Effort**: 1 week

**Workaround**: Manual content publishing via API
**Fix**: Implement missing agent or use alternative (rust-coder)

---

### Issue #4: C4 Diagrams Not Updated

**Agent**: system-architect (not found during parallel execution)
**Impact**: Documentation completeness
**Estimated Effort**: 2-3 days

**Workaround**: Use existing generic C4 diagrams
**Fix**: Update templates/c4_*.mmd.tera with FactoryPaaS-specific diagrams

---

## Validation Checklist

Before declaring build issues resolved:

- [ ] **Compilation**: `cargo make check` passes cleanly (<15s)
- [ ] **Linting**: `cargo make lint` passes with zero warnings
- [ ] **Testing**: `cargo make test` passes all tests (<30s)
- [ ] **SLO Compliance**: `cargo make slo-check` confirms all targets met
- [ ] **No Violations**: Zero unwrap/expect in production code
- [ ] **CI/CD**: GitHub Actions workflow completes <5 minutes

---

## Timeline Estimate

### Optimistic (4-6 weeks)
- Week 1-2: unwrap/expect refactoring (automated + priority modules)
- Week 3: Build performance optimization
- Week 4: Validation and documentation
- Week 5-6: SEO + C4 diagram fixes

### Realistic (6-8 weeks)
- Week 1-3: unwrap/expect refactoring (comprehensive review)
- Week 4-5: Build performance optimization + profiling
- Week 6: Full test suite validation
- Week 7: SEO + C4 diagram implementation
- Week 8: Production deployment validation

### Pessimistic (8-10 weeks)
- Includes discovery of additional issues during refactoring
- Multiple rounds of validation failures
- Dependency update cascades
- Security audit findings

---

## Success Criteria

✅ **Build SLOs Met**:
- First build: ≤15s
- Incremental build: ≤2s
- CI/CD pipeline: <5 minutes

✅ **Code Quality**:
- Zero unwrap/expect in production code
- Zero compiler warnings
- All tests passing
- Property test coverage >80%

✅ **Documentation Complete**:
- All Diataxis categories populated
- C4 diagrams updated for FactoryPaaS
- SEO workflow documented

---

## References

- [VALIDATION_REPORT.md](VALIDATION_REPORT.md) - Complete validation findings
- [ggen Constitutional Rules](../../CLAUDE.md) - Code quality requirements
- [Cargo Book - Build Performance](https://doc.rust-lang.org/cargo/guide/build-cache.html)
- [Clippy Lints](https://rust-lang.github.io/rust-clippy/master/index.html)

---

**Next Steps**: Begin Phase 1 of Issue #1 (Add Clippy enforcement)
