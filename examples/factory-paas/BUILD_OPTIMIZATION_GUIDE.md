# Build Optimization Guide - FactoryPaaS

**Date**: 2026-01-24
**Status**: Implementation in progress
**Goal**: Reduce build time from >600s to ≤15s (40x improvement)

---

## Current State

### Build Performance Issues

**Measured**: >600 seconds (confirmed during investigation)
**Target**: ≤15 seconds (first build), ≤2 seconds (incremental)
**Impact**: Development velocity, CI/CD pipeline timeouts

**Evidence**:
```bash
# Attempted cargo check on ggen-core
cd crates/ggen-core && timeout 30s cargo check
# Result: Exit code 143 - Timeout after 2 minutes
```

### Workspace Complexity

- **27 crates** with complex interdependencies
- **OpenTelemetry** dependencies with heavy proc-macros
- **Multiple RDF libraries** (Oxigraph, SPARQL processing)
- **Large test suite** with testcontainers and property tests

---

## Optimization Strategy

### Phase 1: sccache (IMMEDIATE - 5-10x improvement)

**Installation** (in progress):
```bash
# Install sccache for shared compilation cache
cargo install sccache

# Configure environment
export RUSTC_WRAPPER=sccache
export SCCACHE_DIR=$HOME/.cache/sccache

# Verify installation
sccache --show-stats
```

**Expected Impact**:
- First build: Still slow (compiling + caching)
- Incremental builds: 5-10x faster (cache hits)
- CI/CD: Massive improvement with shared cache

**Configuration for Bash/Zsh** (~/.bashrc or ~/.zshrc):
```bash
# Add to shell rc file
export RUSTC_WRAPPER=sccache
export SCCACHE_DIR=$HOME/.cache/sccache
```

**Validation**:
```bash
# Clear cache and rebuild
sccache --stop-server
rm -rf ~/.cache/sccache
cargo clean

# First build (slow, creates cache)
time cargo build

# Touch a file and rebuild (should be fast)
touch crates/ggen-core/src/lib.rs
time cargo build  # Should be <5s

# Check cache stats
sccache --show-stats
```

---

### Phase 2: Workspace Profiles (ALREADY CONFIGURED)

**Current Configuration** (Cargo.toml:L264-291):

#### Development Profile
```toml
[profile.dev]
opt-level = 0              # No optimization = fast compilation
debug = true
lto = false                # No link-time optimization
debug-assertions = true
codegen-units = 256        # Maximum parallelism (already optimal)
incremental = true         # Incremental compilation (already enabled)
split-debuginfo = "unpacked"  # Faster debug builds on macOS (already enabled)
```

**Analysis**: Dev profile is already well-optimized for fast compilation.

#### Test Profile
```toml
[profile.test]
opt-level = 0              # Fast test compilation
codegen-units = 256        # Parallel codegen (already optimal)
incremental = true         # Incremental (already enabled)
```

**Status**: ✅ Test profile already optimized

---

### Phase 3: Dependency Optimization (2-3 days)

#### Find Unused Dependencies
```bash
# Install cargo-udeps
cargo install cargo-udeps

# Run analysis (requires nightly)
cargo +nightly udeps

# Remove unused dependencies from Cargo.toml
```

#### Find Duplicate Dependencies
```bash
# Identify duplicates
cargo tree --duplicates

# Common issues:
# - Multiple versions of base64, tokio, serde
# - OpenTelemetry dependency conflicts
```

**Current Known Issues** (from Cargo.toml:L146-149):
```toml
# Force base64 version to resolve duplicate dependency issue
# config -> ron -> base64 0.21.7 conflicts with reqwest -> base64 0.22.1
base64 = "0.22"
```

**Action**: Use `cargo tree --duplicates` to find more duplicates, then add version pins to `[workspace.dependencies]`.

---

### Phase 4: Build Caching in CI/CD (1-2 days)

#### GitHub Actions Configuration
```yaml
name: CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      # Cache Cargo registry and git dependencies
      - uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
          restore-keys: |
            ${{ runner.os }}-cargo-

      # Cache sccache compilation cache
      - uses: mozilla-actions/sccache-action@v0.0.3
        with:
          version: "latest"

      # Set environment variables
      - name: Configure sccache
        run: |
          echo "RUSTC_WRAPPER=sccache" >> $GITHUB_ENV
          echo "SCCACHE_DIR=$HOME/.cache/sccache" >> $GITHUB_ENV

      # Build with caching
      - name: Build
        run: cargo build --all-features

      # Show cache statistics
      - name: Cache stats
        run: sccache --show-stats
```

**Expected Impact**:
- First CI run: Slow (builds cache)
- Subsequent runs: 5-10x faster (cache hits)
- Parallel jobs: Share compilation cache

---

### Phase 5: Profiling and Bottleneck Analysis (1-2 days)

#### Generate Timing Report
```bash
# Build with timing information
cargo build --timings

# Open HTML report
open target/cargo-timings/cargo-timing.html  # macOS
xdg-open target/cargo-timings/cargo-timing.html  # Linux
```

**Analyze**:
1. Which crates take longest to compile?
2. Are proc-macros causing bottlenecks?
3. Are there circular dependencies?

#### Common Bottlenecks

1. **OpenTelemetry dependencies**: Heavy proc-macros
   - Solution: Feature-gate OTEL dependencies
   - Only enable in production builds

2. **Testcontainers**: Large test dependencies
   - Already isolated in dev-dependencies ✅
   - Could move to separate test crate

3. **Oxigraph**: Large RDF library
   - Core dependency, can't remove
   - Ensure using latest version

---

### Phase 6: Advanced Optimizations (Optional)

#### Feature Gating for Development
```toml
[features]
default = []
otel = ["opentelemetry", "opentelemetry-otlp", "tracing-opentelemetry"]
full = ["otel", "testcontainers"]
```

**Benefit**: Fast development builds without OTEL overhead

#### Split Large Crates
If any crate >10k LOC:
- Split into focused sub-crates
- Reduce compilation unit size
- Enable better parallelism

#### Use Mold Linker (Linux)
```bash
# Install mold (fast linker)
sudo apt install mold  # Ubuntu/Debian

# Configure Cargo
mkdir -p ~/.cargo
cat >> ~/.cargo/config.toml <<'EOF'
[target.x86_64-unknown-linux-gnu]
linker = "clang"
rustflags = ["-C", "link-arg=-fuse-ld=mold"]
EOF
```

**Expected**: 2-3x faster linking on large projects

---

## Validation Checklist

Before declaring build optimization complete:

- [ ] sccache installed and configured
- [ ] sccache --show-stats shows >80% cache hit rate on incremental builds
- [ ] First build (clean): Measured baseline time
- [ ] Incremental build: ≤2 seconds (touch single file)
- [ ] cargo build --timings: HTML report generated and analyzed
- [ ] cargo tree --duplicates: All duplicates resolved or documented
- [ ] cargo +nightly udeps: No unused dependencies
- [ ] CI/CD: Caching configured in GitHub Actions
- [ ] CI/CD: Pipeline completes in <5 minutes

---

## Success Metrics

### Build Times (with sccache)

| Build Type | Target | Actual | Status |
|-----------|--------|--------|--------|
| First build (clean) | ≤15s | TBD | 🟡 Pending |
| Incremental (1 file change) | ≤2s | TBD | 🟡 Pending |
| Full workspace rebuild | ≤30s | TBD | 🟡 Pending |
| CI/CD pipeline | <5 min | TBD | 🟡 Pending |

### Cache Efficiency

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Cache hit rate (incremental) | >80% | TBD | 🟡 Pending |
| Cache size | <5 GB | TBD | 🟡 Pending |
| sccache overhead | <100ms | TBD | 🟡 Pending |

---

## Implementation Timeline

### Immediate (This Session - 1-2 hours)

1. ✅ Install sccache (in progress)
2. Configure RUSTC_WRAPPER environment variable
3. Run baseline build with `cargo build --timings`
4. Document bottlenecks from timing report

### Short-Term (Next 1-2 days)

1. Resolve duplicate dependencies (cargo tree --duplicates)
2. Remove unused dependencies (cargo +nightly udeps)
3. Configure CI/CD caching (GitHub Actions)
4. Measure and document improvements

### Medium-Term (Next 1-2 weeks)

1. Feature-gate OTEL dependencies for development
2. Split large crates if needed
3. Implement mold linker on Linux systems
4. Full workspace profiling and optimization

---

## Troubleshooting

### sccache Not Caching

**Symptom**: Cache hit rate <10%
**Diagnosis**:
```bash
# Check sccache logs
export SCCACHE_LOG=debug
cargo clean && cargo build

# Check cache directory
ls -lh ~/.cache/sccache/
```

**Solutions**:
1. Ensure RUSTC_WRAPPER is set correctly
2. Check disk space (cache requires several GB)
3. Verify sccache server is running: `sccache --show-stats`

### Compilation Still Slow

**Symptom**: Incremental builds >10s even with sccache
**Diagnosis**:
```bash
# Generate timing report
cargo build --timings

# Check for problematic dependencies
cargo tree --depth 1
```

**Solutions**:
1. Identify slow crates from timing report
2. Consider feature-gating heavy dependencies
3. Check for circular dependencies
4. Profile with `cargo build -vv` for verbose output

### CI/CD Cache Not Working

**Symptom**: Every CI run is slow
**Diagnosis**:
1. Check GitHub Actions logs for cache hits/misses
2. Verify cache key matches Cargo.lock hash
3. Ensure sccache is enabled in CI environment

**Solutions**:
1. Use `actions/cache@v3` with correct paths
2. Use `mozilla-actions/sccache-action` for CI
3. Verify RUSTC_WRAPPER is set in CI environment

---

## References

- [Cargo Build Performance](https://doc.rust-lang.org/cargo/guide/build-cache.html)
- [sccache Documentation](https://github.com/mozilla/sccache)
- [Cargo Timings](https://doc.rust-lang.org/cargo/reference/timings.html)
- [Fast Rust Builds](https://fasterthanli.me/articles/why-is-my-rust-build-so-slow)
- [BUILD_FIX_PLAN.md](BUILD_FIX_PLAN.md) - Original remediation plan
- [BUILD_ISSUES_ASSESSMENT.md](BUILD_ISSUES_ASSESSMENT.md) - Corrected findings

---

**Next Steps**:
1. Wait for sccache installation to complete
2. Configure RUSTC_WRAPPER in shell rc file
3. Run baseline build with `--timings`
4. Analyze bottlenecks and implement optimizations
5. Measure improvements and update this document

---

**Last Updated**: 2026-01-24
**Status**: sccache installation in progress, baseline measurements pending
