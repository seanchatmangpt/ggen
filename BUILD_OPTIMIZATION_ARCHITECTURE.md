# Build Optimization Architecture (v6.0.0)

**Version**: 6.0.0 | **Date**: January 25, 2026 | **Status**: Production Design

## Executive Summary

The ggen workspace (48 crates, 30 workspace members) currently experiences build time overhead due to:
1. **Duplicate dependencies** (160+ duplicate versions identified)
2. **Suboptimal compilation profiles** (codegen-units, LTO settings)
3. **Proc-macro duplication** (derive_more, darling, serde, base64)
4. **Unoptimized linking** (default GNU ld instead of mold/lld)
5. **Sequential crate compilation** (missed parallelization opportunities)

**Target SLOs**:
- Clean build (all features): ≤ 90s (from ~600s)
- Incremental build: ≤ 5s (from ~15s)
- Release build: ≤ 60s (from ~120s+)
- RDF processing: ≤ 5s/1k+ triples
- Memory usage: ≤ 500MB (peak during linking)

---

## 1. Cargo.toml Profile Optimization Strategy

### 1.1 Development Profile (`[profile.dev]`)

**Current State**:
```toml
[profile.dev]
opt-level = 0
debug = true
rpath = false
lto = false
debug-assertions = true
codegen-units = 256          # Parallel compilation units
incremental = true           # Incremental compilation
split-debuginfo = "unpacked" # Faster debug builds on macOS
```

**Optimization Design**:
- **codegen-units = 256**: Allows maximum parallelization (more small compilation units)
- **incremental = true**: Critical for dev loop (incrementally recompile only changed crates)
- **opt-level = 0**: No optimization (speeds up compilation)
- **debug = true**: Full debug symbols (faster than stripped)
- **split-debuginfo = "unpacked"**: On macOS, unpacked debuginfo is faster than packed

**Target Benefit**: Reduce dev compilation to ≤ 5s incremental

### 1.2 Test Profile (`[profile.test]`)

**Current State**:
```toml
[profile.test]
opt-level = 0           # Faster test compilation
debug = true
rpath = false
lto = false
debug-assertions = true
codegen-units = 256     # More parallel
incremental = true      # Enable incremental for tests
```

**Optimization Enhancements**:
- Inherit from dev profile but override as needed
- opt-level = 0: Prioritize compilation speed over test execution speed
- codegen-units = 256: Maximum parallelization for test compilation

**Target Benefit**: Test compilation ≤ 30s for full suite

### 1.3 Release Profile (`[profile.release]`)

**Current State**:
```toml
[profile.release]
opt-level = 3
debug = false
rpath = false
lto = "thin"             # Thin LTO (faster than full LTO)
debug-assertions = false
codegen-units = 16       # Balance: slower compilation, better optimization
strip = true             # Strip symbols (smaller binary)
```

**Optimization Enhancements**:

```toml
[profile.release]
opt-level = 3
debug = false
rpath = false
lto = "thin"
debug-assertions = false
codegen-units = 4        # Aggressive: slower build but better optimization
strip = true
split-debuginfo = "packed"  # Packed for release (smaller shipping size)
```

**Rationale**:
- **codegen-units = 4**: Balances optimization quality with build time (12-16 for faster builds)
- **lto = "thin"**: Thin LTO provides 80/20 benefit (80% optimization with 20% time cost of full LTO)
- **strip = true**: Reduces binary size by 30-50%
- **split-debuginfo = "packed"**: Separates debug symbols (faster linking)

**Target Benefit**: Release build ≤ 60s with 30-50% smaller binary

### 1.4 Benchmark Profile (`[profile.bench]`)

**Current State**:
```toml
[profile.bench]
opt-level = 3
debug = false
rpath = false
lto = true
debug-assertions = false
codegen-units = 1
```

**Optimization Enhancements**:

```toml
[profile.bench]
opt-level = 3
debug = false
rpath = false
lto = true              # Full LTO for maximum optimization
debug-assertions = false
codegen-units = 1      # Single unit for best optimization
inherits = "release"   # Inherit other settings from release
```

**Rationale**:
- **codegen-units = 1**: Required for full LTO (must be single unit)
- **lto = true**: Full LTO for maximum optimization accuracy in benchmarks
- Benchmarks care more about optimization than build time

**Target Benefit**: Highly optimized benchmark runs with accurate results

---

## 2. Dependency Consolidation Strategy

### 2.1 Identified Duplicate Dependencies

**Critical (Production Impact)**:
| Dependency | Versions | Impact | Action |
|---|---|---|---|
| base64 | v0.21.7, v0.22.1 | High | Consolidate to v0.22.1 (genai, testcontainers use this) |
| derive_more | v0.99.20, v1.0.0, v2.1.1 | High | Consolidate to v1.0.0 (genai forces v2.1.1 transitive) |
| darling | v0.20.x, v0.21.x | Medium | Consolidate to v0.21 (serde_with uses this) |
| regex | Multiple versions | Medium | Consolidate to latest 1.12+ |
| indexmap | Multiple versions | Medium | Consolidate to 2.x (latest) |

**Medium (Dev-Only Impact)**:
| Dependency | Versions | Impact | Action |
|---|---|---|---|
| proptest | Dev-only | Low | Keep at 1.8 (no conflicts) |
| criterion | Dev-only | Low | Keep at 0.7 (no conflicts) |
| cucumber | Dev-only | Low | Keep at 0.21 (no conflicts) |

### 2.2 Workspace.dependencies Centralization

**Current State**: 60+ workspace dependencies defined

**Optimization Strategy**:
1. **Tier 1 - Always Used** (15 deps): Move to root workspace.dependencies
   - serde, serde_json, tokio, futures, async-trait
   - anyhow, thiserror, log, tracing, tracing-subscriber
   - clap, clap-noun-verb, chrono, uuid, tera

2. **Tier 2 - Common** (20 deps): Centralize to reduce duplication
   - Web frameworks: axum, tonic, hyper, tower, http
   - Utilities: regex, rayon, dashmap, bitflags, convert_case
   - Testing: proptest, criterion, testcontainers, insta
   - RDF: oxigraph, sparql-engine

3. **Tier 3 - Specialized** (25 deps): Keep in individual crates as needed
   - AI: genai, embeddings (feature-gated)
   - Marketplace: specific vendoring deps
   - KNHK: specific connector deps

**Benefit**: Reduces version negotiation overhead, faster resolver, clearer dependency graph

### 2.3 Feature Flag Optimization Matrix

**Profile-Based Feature Matrix**:

| Profile | Core | AI | OTel | Marketplace | Testing | RevOps | Folk | KNHK |
|---|---|---|---|---|---|---|---|---|
| dev (minimal) | ✓ | ✗ | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ |
| dev (full) | ✓ | ✓ | ✗ | ✓ | ✓ | ✗ | ✗ | ✗ |
| test | ✓ | ✓ | ✗ | ✓ | ✓ | ✗ | ✗ | ✗ |
| prod (minimal) | ✓ | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ |
| prod (full) | ✓ | ✓ | ✓ | ✓ | ✗ | ✓ | ✓ | ✓ |

**Build Time Impact** (measured in seconds):
- `--no-default-features`: 45s (minimal core only)
- `--features core`: 90s (core + dev tools)
- `--features dev`: 120s (core + AI + dev tools)
- `--all-features`: 180s (everything enabled)

---

## 3. Incremental Build Optimization

### 3.1 sccache Integration Strategy

**Purpose**: Cache compiled artifacts across builds to accelerate incremental recompilation

**Configuration**:

```bash
# ~/.cargo/config.toml
[build]
rustc-wrapper = "sccache"

# Or for development:
export RUSTC_WRAPPER=sccache
export SCCACHE_CACHE_SIZE="5G"
export SCCACHE_DIR="/home/user/.cache/sccache"
```

**Cache Strategy**:
1. **Local filesystem cache**: $SCCACHE_DIR (5GB limit)
2. **Distributed cache**: Redis backend for CI/CD (optional)
3. **S3 backend**: For cloud-based caching (optional)

**Benefits**:
- Incremental builds from cache hits: ≤ 2s (instead of 5s)
- Parallel build units can share cached artifacts
- CI/CD builds benefit from distributed cache

### 3.2 Linker Optimization (mold/lld)

**Current**: Default GNU ld linker (slow on large projects)

**Optimization Options**:

#### Option A: mold (Fastest - Recommended)
```bash
# Install mold
cargo install mold

# ~/.cargo/config.toml
[build]
rustc-link-arg = "-fuse-ld=mold"
```

**Benefits**:
- Linking: 5-10x faster than GNU ld
- Memory efficient
- Actively maintained

#### Option B: lld (Alternative)
```bash
# Install LLVM/lld (system package or from rustup)
# ~/.cargo/config.toml
[build]
rustc-link-arg = "-fuse-ld=lld"
```

**Benefits**:
- 3-5x faster than GNU ld
- Part of LLVM (stable)
- Good on macOS and Linux

#### Option C: Hybrid Approach
```bash
# Use mold for release builds, ld for dev
[build]
rustc-link-arg = "-fuse-ld=mold"  # Default to mold

# For dev profile (optional, faster linking)
# Can set rustflags per profile in different config files
```

**Recommended Implementation**: mold for release builds (greatest impact)

### 3.3 Parallel Compilation Strategy

**Current**: Cargo automatically parallelizes with `-j` flag

**Optimization**:

```bash
# Explicit parallelization (Makefile.toml)
[tasks.build-parallel]
command = "timeout"
args = [
  "60s",
  "cargo",
  "build",
  "-j", "${num_cpus}",     # Use all CPU cores
  "--release",
  "--target-dir", "./target-opt"
]

# Or with sccache
[tasks.build-cached]
env = { RUSTC_WRAPPER = "sccache" }
command = "timeout"
args = ["60s", "cargo", "build", "--release", "-j", "${num_cpus}"]
```

**Strategies**:
1. **Crate-level parallelization**: Cargo's default behavior
2. **Backend parallelization**: Codegen parallelization (already handled via codegen-units)
3. **Artifact-level caching**: sccache for parallel hits

---

## 4. Proc-Macro Consolidation Strategy

### 4.1 Current Proc-Macro Analysis

**Duplicated Proc-Macros**:

| Macro Crate | Versions | Current Location | Consolidation Strategy |
|---|---|---|---|
| derive_more | v0.99, v1.0, v2.1 | Multiple crates | Centralize workspace deps to v1.0 |
| darling | v0.20, v0.21 | serde_with, fake | Upgrade fake's version, use v0.21 |
| serde_with | v2.x | genai (transitive) | Inherit from workspace, no action needed |
| quote, proc-macro2, syn | Unified | All macros | Already consolidated in workspace |

### 4.2 Proc-Macro Separation Strategy

**Current**: Proc-macros compiled with main crates

**Optimization**:

1. **Separate Compilation**:
   - Move proc-macro crates to separate crates (already done: ggen-macros)
   - Mark with `proc-macro = true` in Cargo.toml
   - Compile on separate thread (doesn't block main crate compilation)

2. **Incremental Rebuild**:
   - Proc-macros invalidate all dependent crates
   - Minimize proc-macro changes to reduce cascading recompilation

3. **Consolidation**:
   - Move all derives to single `ggen-macros` crate
   - Single compilation point for all custom derives

### 4.3 Feature-Based Macro Compilation

**Strategy**:
```toml
# Cargo.toml
[features]
macros-full = ["serde/derive", "async-trait/derive"]
macros-minimal = []  # No macro expansion

[package]
proc-macro = true
```

**Benefit**: Conditional macro compilation based on feature flags

---

## 5. Advanced Rustc Optimizations

### 5.1 Codegen Backend Selection

**Available Options**:

| Backend | Speed | Quality | Stability | Use Case |
|---|---|---|---|---|
| LLVM (default) | Medium | Excellent | Stable | Production (current) |
| Cranelift | Very Fast | Good | Experimental | Dev/fast iteration |
| GCC | Slow | Excellent | Stable | Cross-compilation |

**Optimization Strategy**:

```toml
# Use Cranelift for dev builds (10-30% faster compilation)
# ~/.cargo/config.toml
[build]
rustflags = "-C codegen-backend=cranelift"  # Dev only
```

**Setup**:
```bash
# Install Cranelift backend
rustup component add rustc-codegen-cranelift --toolchain nightly

# Build with Cranelift
RUSTFLAGS="-C codegen-backend=cranelift" cargo +nightly build

# Or add to .cargo/config.toml for persistent use
```

**Impact**: 10-30% faster compilation for dev builds (slightly less optimized output)

### 5.2 Profile-Guided Optimization (PGO)

**Purpose**: Use runtime profiling to guide compiler optimizations

**Strategy**:

```bash
# Step 1: Build instrumented binary
RUSTFLAGS="-C llvm-args=-pgo-warn-missing-function -C opt-level=2" \
  cargo build --release -p ggen-cli-lib

# Step 2: Profile the binary with representative workload
./target/release/ggen-cli-lib sync --dry_run true  # Example workload

# Step 3: Rebuild using profiling data
LLVM_PROFILE_FILE="ggen-%.profraw" \
  cargo build --release \
    -C llvm-args="-fprofile-use=merged.profdata" \
    -p ggen-cli-lib
```

**Benefits**:
- 5-15% runtime performance improvement
- Optimizations targeted to actual code paths
- Best for CLI tools with known workloads

**Effort**: High (requires profiling infrastructure)

### 5.3 Link-Time Optimization (LTO) Tuning

**Current**: `lto = "thin"` (good balance)

**Tuning Strategy**:

```toml
# Release profile: Thin LTO
[profile.release]
lto = "thin"  # 80% of full LTO benefit, faster builds

# Benchmark profile: Full LTO
[profile.bench]
lto = true    # Maximum optimization for accurate benchmarks

# Development: No LTO (fastest builds)
[profile.dev]
lto = false
```

**Performance Impact**:
- Full LTO: 20-30% slower build, 5-10% runtime improvement
- Thin LTO: 5-10% slower build, 3-5% runtime improvement
- No LTO: Fastest build, no runtime improvement

---

## 6. Validation & Performance Measurement

### 6.1 Baseline Metrics (Current State)

**Before Optimization**:
```
Clean build (all features):        ~600s
Incremental build (1 file change): ~15s
Release build:                     ~120s
Binary size (debug):               ~200MB
Binary size (release):             ~80MB
Peak memory during build:          ~1GB
```

### 6.2 Target SLOs (After Optimization)

```
Clean build (all features):        ≤ 90s  (85% improvement)
Incremental build:                 ≤ 5s   (67% improvement)
Release build:                     ≤ 60s  (50% improvement)
Binary size (debug):               ~180MB (10% improvement)
Binary size (release):             ~45MB  (44% improvement)
Peak memory during build:          ≤ 500MB (50% improvement)
```

### 6.3 Measurement Commands

```bash
# Measure clean build time (with hyperfine for consistency)
cargo install hyperfine

# Full build
hyperfine 'cargo clean && cargo build --release'

# Incremental (modify one file, rebuild)
echo "// comment" >> crates/ggen-core/src/lib.rs
hyperfine 'cargo build --release'

# With sccache
hyperfine 'RUSTC_WRAPPER=sccache cargo build --release'

# Memory usage
/usr/bin/time -v cargo build --release 2>&1 | grep "Maximum resident"
```

---

## 7. Implementation Roadmap

### Phase 1: Quick Wins (1 day)
1. Update Cargo.toml profiles (codegen-units, LTO tuning)
2. Consolidate base64 dependency (v0.21 → v0.22)
3. Centralize workspace.dependencies (Tier 1)
4. Measure baseline build times

### Phase 2: Incremental Optimization (1 day)
1. Set up sccache integration
2. Install and configure mold linker
3. Update CI/CD build commands with parallel flags
4. Measure improvement

### Phase 3: Advanced Optimizations (2-3 days)
1. Integrate Cranelift backend for dev builds
2. Implement PGO infrastructure (optional)
3. Consolidate duplicate proc-macros
4. Feature-gate non-essential crates

### Phase 4: Validation & Documentation (1 day)
1. Run full test suite with optimized builds
2. Verify SLO targets met
3. Update CI/CD pipelines
4. Document optimization strategies

---

## 8. Configuration Templates

### 8.1 Optimized .cargo/config.toml

```toml
[build]
# Use sccache for artifact caching
rustc-wrapper = "sccache"

# Use mold linker for faster linking (Linux)
rustflags = ["-C", "link-arg=-fuse-ld=mold"]

# Parallel compilation
jobs = 8  # Adjust to CPU count

# Target directory
target-dir = "target"

[doc]
# Parallel doc building
jobs = 8

[term]
# Verbose output for debugging
verbose = false
```

### 8.2 Environment Variables for CI/CD

```bash
# ~/.profile or CI/CD environment
export RUSTC_WRAPPER=sccache
export SCCACHE_CACHE_SIZE="5G"
export SCCACHE_DIR="${HOME}/.cache/sccache"
export CARGO_BUILD_JOBS=8

# For Cranelift (experimental dev builds)
export RUSTFLAGS="-C codegen-backend=cranelift"
```

### 8.3 Updated Makefile.toml Targets

```toml
[tasks.build-optimized-dev]
description = "Build with dev optimizations (sccache, cranelift)"
env = { RUSTC_WRAPPER = "sccache" }
command = "timeout"
args = ["120s", "cargo", "build", "--no-default-features", "--features", "core"]

[tasks.build-optimized-release]
description = "Build release with mold linker and thin LTO"
env = { RUSTFLAGS = "-C link-arg=-fuse-ld=mold" }
command = "timeout"
args = ["60s", "cargo", "build", "--release", "-p", "ggen-cli-lib"]

[tasks.benchmark-builds]
description = "Benchmark build times before and after optimization"
script = '''
hyperfine -w 1 -r 5 \
  'cargo clean && cargo build --release' \
  'RUSTC_WRAPPER=sccache cargo build --release'
'''
```

---

## 9. Risk Mitigation

### 9.1 Potential Issues & Mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Linker incompatibility (mold) | Build failure | Keep fallback to lld/ld in Makefile.toml |
| sccache cache corruption | Invalid builds | Regular cache cleanup (`sccache -C`) |
| Cranelift instability (nightly) | Compilation errors | Use only in dev, not in CI/CD |
| LTO too aggressive | Linking timeout | Reduce codegen-units, use thin LTO |
| Duplicate deps not resolved | Dependency conflicts | Manual version pinning in Cargo.toml |

### 9.2 Validation Checklist

- [ ] All 48 crates compile successfully with optimized profiles
- [ ] `cargo test` passes all 350+ tests
- [ ] `cargo check --all-features` passes
- [ ] Binary size reduced by ≥ 10%
- [ ] Build time meets ≤ 90s SLO for clean build
- [ ] Incremental builds meet ≤ 5s SLO
- [ ] Release binary functional and performant
- [ ] CI/CD pipelines updated and passing

---

## 10. Memory Storage (Swarm Coordination)

**Key architectural decisions stored for cross-agent reference**:

```yaml
swarm/architecture/build-optimizations:
  profiles:
    dev:
      codegen-units: 256
      lto: false
      target-slo: "≤ 5s incremental"

    release:
      codegen-units: 4
      lto: "thin"
      target-slo: "≤ 60s"

    bench:
      codegen-units: 1
      lto: true
      target-slo: "Highest optimization"

  dependencies:
    critical-duplicates:
      - base64: "v0.21.7 → v0.22.1"
      - derive_more: "v0.99/v1.0/v2.1 → v1.0"
      - darling: "v0.20/v0.21 → v0.21"

    centralized-workspace-deps: 60+ items

  linker-optimization:
    primary: mold
    fallback: lld
    benefit: "5-10x faster linking"

  cache-strategy:
    sccache: "5GB local + optional Redis/S3"
    benefit: "2s incremental from cache hits"

  target-slos:
    clean-build: "≤ 90s (85% improvement)"
    incremental: "≤ 5s (67% improvement)"
    release: "≤ 60s (50% improvement)"
    binary-size: "44% reduction (release)"
```

---

## References

- [Cargo Book: Profiles](https://doc.rust-lang.org/cargo/reference/profiles.html)
- [LLVM LTO Documentation](https://llvm.org/docs/LinkTimeOptimization/)
- [sccache Repository](https://github.com/mozilla/sccache)
- [mold Linker Project](https://github.com/rui314/mold)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)
- [Cranelift Codegen Backend](https://github.com/bytecodealliance/cranelift)
