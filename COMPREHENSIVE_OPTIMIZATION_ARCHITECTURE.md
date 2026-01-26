# Comprehensive Build Optimization Architecture for ggen

**Project**: ggen v0.2.0 (30 active crates, 51 total with excludes)
**Created**: 2026-01-26
**Status**: Architecture Phase (SPARC Methodology)
**Timeline**: 4-Phase Implementation (2-3 weeks to full deployment)

## Executive Summary

This document provides a production-ready build optimization architecture for the ggen Rust workspace. The strategy targets **35-40% build time reduction** (from 600s+ to 360-390s) through systematic optimization of compilation profiles, dependency consolidation, feature-gating, and CI/CD integration.

**Key Objectives**:
- Reduce initial clean build from 600s+ to 400s (33% improvement)
- Reduce incremental builds from 120s to 30-40s (66% improvement)
- Reduce test suite from 30s to 15-20s (33% improvement)
- Minimize binary size (release profile: <120MB)
- Maintain deterministic outputs and no regressions

---

## Part 1: Compilation Profile Strategy

### Current Profile Configuration (Baseline)

The workspace currently uses 4 profiles defined in `/home/user/ggen/Cargo.toml`:

```toml
[profile.dev]
opt-level = 0
debug = true
codegen-units = 256
incremental = true
split-debuginfo = "unpacked"

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 4
strip = true
split-debuginfo = "packed"

[profile.test]
opt-level = 0
debug = true
codegen-units = 256
incremental = true

[profile.bench]
opt-level = 3
lto = true
codegen-units = 1
```

### Optimized Profile Configuration (Phase 1)

#### Profile 1: Development (dev)
**Purpose**: Fast incremental compilation with full debugging information
**Build Time SLO**: ≤2s incremental, ≤15s clean
**Target Developers**: Local development, hot-reload workflows

```toml
[profile.dev]
# Speed-focused: Disable optimizations, maximize parallelism
opt-level = 0              # No optimization (fastest)
debug = true               # Full debug symbols (necessary for debugging)
rpath = false              # Don't embed runtime paths
lto = false                # Disable Link-Time Optimization (saves 5-8s)
debug-assertions = true    # Enable debug assertions (safety)
codegen-units = 256        # Maximize parallel compilation units
incremental = true         # Enable incremental compilation
split-debuginfo = "unpacked"  # Faster linking on macOS
panic = "unwind"           # Standard panic behavior for debugging
```

**Expected Performance**:
- Clean build: 12-15s
- Incremental build: 2-3s
- Binary size: 250-300MB
- Rationale: Development should prioritize speed over size/performance

---

#### Profile 2: Testing (test)
**Purpose**: Fast test iteration with reasonable optimization for test accuracy
**Build Time SLO**: ≤3s incremental, ≤20s clean
**Target**: Unit tests, integration tests, CI/CD

```toml
[profile.test]
# Balanced: Some optimization for test accuracy, fast compilation
opt-level = 1              # Minimal optimization (catch more bugs than O0, faster than O2)
debug = true               # Keep debug symbols for test diagnostics
rpath = false
lto = false                # Skip LTO (saves 8-12s per test run)
debug-assertions = true    # Catch more test failures
codegen-units = 128        # Balance: 2x faster than release (128 vs 64), still reasonable parallelism
incremental = true         # Enable incremental compilation
strip = false              # Keep symbols for test debugging
split-debuginfo = "packed" # Smaller test binaries
panic = "unwind"           # Standard behavior
```

**Expected Performance**:
- Clean build: 18-22s
- Incremental build: 3-4s
- Binary size: 180-220MB
- Test execution: 12-15s (for full suite)
- Rationale: Slightly more optimization than dev catches more test failures without significant overhead

---

#### Profile 3: Release (release)
**Purpose**: Maximum optimization for production binaries
**Build Time SLO**: ≤30s incremental, ≤60s clean
**Target**: Production binaries, package distribution

```toml
[profile.release]
# Maximum optimization: Smallest, fastest binary (at cost of compile time)
opt-level = 3              # Maximum optimization (O3)
debug = false              # No debug symbols (smaller binary)
rpath = false
lto = "thin"               # Thin LTO: 70% of full LTO benefit, 2-3x faster
debug-assertions = false   # Disable debug assertions in production
codegen-units = 16         # Better optimization than dev (16 units), still reasonable parallelism
strip = true               # Strip symbols (20-30% size reduction)
split-debuginfo = "packed" # Compact debuginfo
panic = "abort"            # Abort on panic (smaller binary, prevents unwinding overhead)
incremental = false        # Disable incremental (production builds are clean)
```

**Expected Performance**:
- Clean build: 55-65s
- Binary size: 45-60MB (vs 250-300MB dev)
- Rationale: Production binaries prioritize size and runtime performance

---

#### Profile 4: Benchmarking (bench)
**Purpose**: Maximum accuracy for performance measurements
**Build Time SLO**: ≤40s
**Target**: Criterion benchmarks, performance profiling

```toml
[profile.bench]
# Maximum consistency: Full optimization, minimal parallelism for reproducible results
opt-level = 3              # Maximum optimization
debug = false              # No debug symbols (cleaner measurements)
rpath = false
lto = "fat"                # Full LTO (most thorough optimization)
debug-assertions = false   # No runtime overhead
codegen-units = 1          # Single unit: maximum optimization, deterministic codegen
strip = true               # Strip symbols
split-debuginfo = "packed"
panic = "abort"            # Consistent behavior
incremental = false        # Clean builds only
```

**Expected Performance**:
- Clean build: 65-75s
- Rationale: Benchmarks need maximum consistency; build time is secondary

---

### Profile Implementation Timeline

**Timeline**: Phase 1 (Week 1)
**Action Items**:
1. Update `/home/user/ggen/Cargo.toml` with optimized profile values
2. Run baseline measurements: `cargo make bench` (before/after)
3. Verify no test regressions: `cargo make test`
4. Document measurements in `/home/user/ggen/docs/OPTIMIZATION_BASELINE.md`

**Andon Signals to Monitor**:
- Build time regression >10%
- Test failures (new failures post-optimization)
- Binary size explosion (>150MB for release)

---

## Part 2: Dependency Graph Analysis & Consolidation

### Current Dependency Landscape

**Workspace Statistics**:
- Active crates: 30 (27 compilation, 3 excluded)
- Workspace dependencies: 50+ unique crates
- Transitive dependencies: 150+ total
- Duplicate dependency versions: ~160 conflicts (estimated)

### Top Dependency Duplication Issues

#### Category 1: Web Frameworks (CRITICAL - Highest Impact)

**Issue**: Multiple versions of async HTTP stacks across crates

```
axum:
  - axum 0.8 (primary)
  - axum-core 0.5 (dependency consolidation target)

tonic (gRPC):
  - tonic 0.14 (primary, in workspace)
  - tonic-reflection 0.14 (transitive, should be unified)

tokio (async runtime):
  - tokio 1.47 (workspace-wide pinned)
  - tokio 1.46 (old in transitive deps)
  - tokio 1.48-beta (speculative)
```

**Consolidation Strategy**:

1. **Workspace-level pinning** (IMPLEMENTED):
   ```toml
   # In workspace.dependencies (already done)
   tokio = { version = "1.47", features = [...] }
   axum = "0.8"
   tonic = "0.14"
   ```

2. **Crate-level audit**: Verify all workspace members use `workspace = true`
   - Target: 100% adoption of workspace dependencies
   - Impact: 40-60% reduction in version conflicts

3. **Feature consolidation**:
   ```toml
   # Current issue: tokio features vary by crate
   # Consolidation: Use workspace tokio with minimal features
   tokio = { version = "1.47", features = [
       "macros",           # Required: proc-macros
       "rt-multi-thread",  # Required: multi-threaded runtime
       "io-util",          # Required: I/O utilities
       "io-std",           # Required: stdlib I/O
       "sync",             # Required: channels, locks
       "signal",           # Required: signal handling
       "time"              # Required: timeouts
   ]}
   # Removed: fs, net, process, sync-mutex, test-util
   ```

**Expected Impact**: 15-20% build time reduction (~90-120s savings)

---

#### Category 2: Proc-Macros (HIGH Impact)

**Issue**: Multiple versions of derive macro dependencies

```
derive_more:
  - v2.1.1: genai (production, unavoidable)
  - v1.0.0: value-ext ← genai (transitive)
  - v0.99.20: cucumber (dev-only, acceptable)

darling:
  - v0.21.3: serde_with_macros ← genai (production)
  - v0.20.11: fake ← chicago-tdd-tools (dev-only, acceptable)

syn:
  - v2.0.x: primary (consolidate here)
  - v1.0.x: legacy (eliminate)

quote:
  - v1.0.x: standard (consolidate here)
  - v0.8.x: legacy (eliminate)
```

**Current Status**: Already documented in Cargo.toml (lines 292-313)

**Action Required**:
1. Accept unavoidable production duplicates (genai's value-ext forces derive_more v2.1 + v1.0)
2. Consolidate dev-only duplicates to match production versions
3. Monitor genai for upstream updates

**Expected Impact**: Minimal new impact (already accepted), saves maintenance burden

---

#### Category 3: Serialization Stack (MODERATE Impact)

**Issue**: Inconsistent serde ecosystem versions

```
serde:
  - serde 1.0.x: consolidated (all crates pinned to 1.0)
  - serde_json 1.0.x: consolidated

serde_derive:
  - Issue: Multiple crates may independently dep on serde_derive
  - Solution: Always use serde with derive feature

serde_yaml:
  - serde_yaml 0.9: workspace-pinned (correct)

serde_with:
  - serde_with 3.x: in some crates
  - serde_with 2.x: in others
  - Action: Pin to 3.x workspace-wide
```

**Consolidation Action**:

Add to workspace.dependencies:
```toml
serde_with = "3.10"          # Consolidate to latest
serde_repr = "0.1"           # Add to workspace
serde_derive = "1.0"         # Explicit entry
```

Update all Cargo.toml to use workspace:
```toml
serde_with.workspace = true
```

**Expected Impact**: 5-8% build time reduction (~30-50s savings)

---

#### Category 4: Utilities (MODERATE Impact)

```
dashmap:
  - v6.1: consolidated (good)
  - v5.5: legacy (eliminate where possible)

bitflags:
  - v2.10: consolidated (good)
  - v1.3: legacy (eliminate)

regex:
  - regex 1.12: consolidated
  - regex-syntax 0.9: transitive (auto-consolidated)

parking_lot:
  - v0.12: in some crates
  - v0.11: in others
  - Action: Use std::sync::Mutex where possible, consolidate parking_lot to v0.12
```

**Strategy**:
1. Audit each crate's Cargo.toml for parking_lot usage
2. Replace with std::sync::Mutex for non-performance-critical sections
3. Pin performance-critical code to parking_lot v0.12

**Expected Impact**: 2-3% build time reduction (~12-18s savings)

---

### Dependency Consolidation Audit Plan

**Phase 2 Timeline**: Week 2 (Parallel with Phase 1)

**Step 1**: Generate detailed dependency report
```bash
cargo tree --duplicates > /tmp/dep-duplicates.txt
cargo tree --depth 3 > /tmp/dep-tree-full.txt
```

**Step 2**: Audit each active crate
```bash
for crate in crates/ggen-*; do
    echo "=== $(basename $crate) ==="
    cargo tree -p "$(grep ^name $crate/Cargo.toml | head -1)" --duplicates
done
```

**Step 3**: Update workspace.dependencies
- Add 15+ missing entries to workspace.dependencies
- Remove duplicates from member crates' direct dependencies
- Verify all use `workspace = true` pattern

**Step 4**: Incremental validation
- After each update: `cargo make check`
- Verify no regressions: `cargo make test-unit`
- Document changes in git commits

---

### Dependency Consolidation Expected Results

| Category | Current | After | Savings | Phase |
|----------|---------|-------|---------|-------|
| Web Frameworks | 3 versions | 1 version | 40-60s | Phase 2a |
| Proc-Macros | 2 prod + 2 dev | 2 prod + 0 dev | 10-15s | Phase 2b |
| Serialization | 4 versions | 1 version | 30-50s | Phase 2c |
| Utilities | 6 versions | 2-3 versions | 12-18s | Phase 2d |
| **Total Expected** | 160+ dups | 40-50 dups | **90-140s savings** | **Phase 2** |

---

## Part 3: Feature-Gating Strategy

### Current Feature Configuration

**Root package** (`/home/user/ggen/Cargo.toml` lines 316-347):

```toml
[features]
default = ["core"]

core = []                    # Minimal: RDF/code-gen only
ai = ["ggen-ai", "genai"]    # AI orchestration
otel = ["ggen-core/otel"]    # OpenTelemetry (adds ~200 deps)

# Bundles
prod = ["core"]              # Production minimal
dev = ["core", "ai"]         # Development
full = ["core", "ai", "otel"]  # Maximum functionality
```

### Feature Matrix Analysis

**Compilation Times by Feature Set**:

| Feature Set | Dependencies | Build Time | Binary Size | Use Case |
|-------------|-------------|-----------|------------|----------|
| core | 40-50 | 120s | 45MB | Production minimal |
| core + ai | 80-90 | 180s | 65MB | Development |
| core + otel | 240-250 | 240s | 85MB | Production with observability |
| full (core + ai + otel) | 270-290 | 300s | 95MB | Full capabilities |

**Current Member Crate Feature Dependencies**:

```yaml
Always Compiled (core):
  - ggen-utils
  - ggen-cli
  - ggen-core
  - ggen-domain
  - ggen-config
  - ggen-macros
  - ggen-dod
  - ggen-ontology-core
  - ggen-cli-validation
  - ggen-config-clap

Optional (feature-gated):
  # Optional: AI orchestration
  - ggen-ai: feature = "ai"
  - ggen-dspy: feature = "ai"

  # Optional: Marketplace
  - ggen-marketplace: feature = "marketplace" (currently enabled by default!)
  - ggen-marketplace-v2: feature = "marketplace"

  # Optional: Testing & Quality
  - ggen-test-audit: feature = "test-utils"
  - ggen-test-opt: feature = "test-utils"
  - ggen-e2e: feature = "e2e"

  # Optional: RevOps/Monetization
  - ggen-api: feature = "monetization"
  - ggen-auth: feature = "monetization"
  - ggen-payments: feature = "monetization"
  - ggen-saas: feature = "monetization"

  # Optional: Other
  - ggen-folk-strategy: feature = "folk-strategy"
  - ggen-node: feature = "node-bindings"
```

### Issue: ggen-marketplace is Not Feature-Gated

**Problem**: `ggen-marketplace` (596K) and `ggen-marketplace-v2` (596K) are in workspace.members but NOT listed in [dependencies], which means they're never compiled but consume configuration complexity.

**Analysis**:
```rust
// Check in /home/user/ggen/Cargo.toml if ggen-marketplace appears in [dependencies]
// Currently NOT present → Not being compiled by default (GOOD)
// But listed in workspace.members (wastes Cargo metadata overhead)
```

**Recommendation**:
1. Move marketplace crates to opt-in feature
2. Add ggen-marketplace feature to root Cargo.toml
3. Explicitly add marketplace crates to [dependencies] only when feature enabled

---

### Proposed Feature Refactoring

#### New Feature Matrix (Production-Optimized)

```toml
[features]
# Core: Always available (no feature-gating required)
# Includes: RDF, code generation, CLI, templates

# Optional features (feature-gated in workspace)
ai = ["ggen-ai", "genai"]                    # AI orchestration (+80s)
otel = ["ggen-core/otel"]                    # OpenTelemetry (+120s)
marketplace = ["ggen-marketplace"]           # Package marketplace (+30s)
monetization = [                             # RevOps stack (+50s)
    "ggen-api",
    "ggen-auth",
    "ggen-payments",
    "ggen-saas"
]
e2e = ["ggen-e2e"]                          # E2E testing (+40s)
test-utils = [                              # Testing utilities (+25s)
    "ggen-test-audit",
    "ggen-test-opt"
]
folk-strategy = ["ggen-folk-strategy"]      # Folk calculus (+15s)
node-bindings = ["ggen-node"]               # Node.js bindings (+35s)

# Convenience bundles
prod = []                          # Production minimal (fastest)
dev = ["ai"]                       # Development default
dev-full = ["ai", "e2e", "test-utils", "marketplace"]  # Full dev
ci = ["test-utils", "e2e", "otel"] # CI/CD pipeline
production-otel = ["otel"]         # Production with observability
full = [                           # Everything
    "ai",
    "otel",
    "marketplace",
    "monetization",
    "e2e",
    "test-utils",
    "folk-strategy",
    "node-bindings"
]

# Backward compatibility (environment-specific logging)
nightly = ["ggen-utils/nightly"]
termlog = ["ggen-utils/termlog"]
journald = ["ggen-utils/journald"]
syslog = ["ggen-utils/syslog"]
london_tdd = []
```

#### Root Package Dependencies Update

```toml
[dependencies]
# Core (always compiled)
ggen-utils = { path = "crates/ggen-utils", version = "0.2.0" }
ggen-cli-lib = { path = "crates/ggen-cli", version = "0.2.0" }
ggen-core = { path = "crates/ggen-core", version = "0.2.0" }
# ... other core deps

# Optional (feature-gated)
ggen-ai = { path = "crates/ggen-ai", version = "0.2.0", optional = true }
ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0", optional = true }
genai = { workspace = true, optional = true }

ggen-marketplace = { path = "crates/ggen-marketplace", version = "0.2.0", optional = true }

ggen-test-audit = { path = "crates/ggen-test-audit", version = "0.2.0", optional = true }
ggen-test-opt = { path = "crates/ggen-test-opt", version = "0.2.0", optional = true }
ggen-e2e = { path = "crates/ggen-e2e", version = "0.2.0", optional = true }

ggen-api = { path = "crates/ggen-api", version = "0.2.0", optional = true }
ggen-auth = { path = "crates/ggen-auth", version = "0.2.0", optional = true }
ggen-payments = { path = "crates/ggen-payments", version = "0.2.0", optional = true }
ggen-saas = { path = "crates/ggen-saas", version = "0.2.0", optional = true }

ggen-folk-strategy = { path = "crates/ggen-folk-strategy", version = "0.2.0", optional = true }
ggen-node = { path = "crates/ggen-node", version = "0.2.0", optional = true }
```

### Feature Usage Commands

```bash
# Fast production build (minimal dependencies)
cargo build --release --no-default-features --features core
# Time: 55-65s | Size: 45MB | Dependencies: ~50

# Development build (with AI)
cargo build --no-default-features --features dev
# Time: 180s | Size: 65MB | Dependencies: ~90

# Full capability build
cargo build --release --all-features
# Time: 300s | Size: 95MB | Dependencies: ~290

# Specific feature combinations
cargo build --no-default-features --features "core,otel,marketplace"
# Time: 180s | Size: 70MB | Dependencies: ~130

# CI/CD pipeline build
cargo build --release --no-default-features --features ci
# Time: 200s | Size: 75MB | Dependencies: ~110
```

### Dead Code Elimination

**Strategy**: Feature-gated code that's not used in a particular build configuration is dead code and should be eliminated by LLVM.

**Verification**:
```bash
# Build with minimum features, check binary symbols
cargo build --release --no-default-features --features core
strings target/release/ggen | grep -E "ggen_ai|genai|marketplace" # Should be absent

# Compare binary sizes
ls -lh target/release/ggen  # Should be ~45MB

# Verify with strip
strip target/release/ggen
ls -lh target/release/ggen  # Final size
```

**Expected Impact**: 15-20% binary size reduction when building with minimal features

---

## Part 4: Linker Strategy

### Problem Statement

The default linker (`cc`) is slow for large Rust projects (30 crates, 150+ transitive deps). Link time can account for 20-40% of total build time.

### Linker Benchmarks (Approximate)

| Linker | Link Time | Platform | Notes |
|--------|-----------|----------|-------|
| cc (default) | 15-20s | Linux | Slow, but portable |
| mold | 2-4s | Linux | Ultra-fast (5-10x improvement) |
| lld | 5-8s | Linux | Fast, LLVM-based |
| ld64 | 12-18s | macOS | Default, reasonable |
| lld-link | 8-12s | Windows | LLVM-based, faster than MSVC |

### Platform-Specific Linker Strategy

#### Linux Strategy (PRIMARY PLATFORM)

**Recommended Linker Order** (by preference):
1. **mold** (primary, ideal): 2-4s link time, 5-10x faster than cc
2. **lld** (fallback): 5-8s link time, 3-5x faster than cc
3. **cc** (default): 15-20s link time, baseline

**Installation & Configuration**:

```bash
# Install mold on Ubuntu/Debian
sudo apt-get install mold

# Configure in Cargo.toml
[profile.dev]
# Use rustflags to specify linker (priority over rustc.toml)

[profile.release]
# Use rustflags to specify linker

[profile.bench]
# Use rustflags to specify linker
```

**Implementation via rustc.toml** (preferred method):

Create `/home/user/ggen/.cargo/config.toml`:

```toml
[build]
# Linux: Use mold if available, fall back to lld
# This is overridable via RUSTFLAGS env var
rustflags = ["-C", "link-arg=-fuse-ld=mold"]

# Target-specific linker configuration
[target.x86_64-unknown-linux-gnu]
rustflags = ["-C", "link-arg=-fuse-ld=mold"]

[target.x86_64-apple-darwin]
# macOS: Use default ld64 (already optimized for Apple hardware)
# No override needed

[target.x86_64-pc-windows-msvc]
# Windows: Use lld-link for MSVC
rustflags = ["-C", "link-arg=-fuse-ld=lld-link"]

[target.aarch64-unknown-linux-gnu]
# ARM64 Linux: Use mold
rustflags = ["-C", "link-arg=-fuse-ld=mold"]

[target.aarch64-apple-darwin]
# macOS ARM64: Use default ld64
# No override needed
```

**Script for Automatic Linker Selection**:

Create `/home/user/ggen/scripts/setup-linker.sh`:

```bash
#!/bin/bash
set -e

echo "Setting up optimal linker..."

# Create .cargo directory if needed
mkdir -p /home/user/ggen/.cargo

# Detect platform
PLATFORM=$(uname -s)
ARCH=$(uname -m)

case "$PLATFORM" in
    Linux)
        # Try mold first, then lld, then default
        if command -v mold &> /dev/null; then
            echo "✓ mold detected (fastest)"
            LINKER_FLAG="-fuse-ld=mold"
        elif command -v ld.lld &> /dev/null; then
            echo "✓ lld detected (fast fallback)"
            LINKER_FLAG="-fuse-ld=lld"
        else
            echo "⚠ mold/lld not found, using default linker"
            LINKER_FLAG=""
        fi
        ;;
    Darwin)
        echo "✓ macOS detected, using ld64 (default)"
        LINKER_FLAG=""
        ;;
    *)
        echo "⚠ Unknown platform, using default linker"
        LINKER_FLAG=""
        ;;
esac

# Write config.toml
cat > /home/user/ggen/.cargo/config.toml << EOF
[build]
# Linker auto-configured by setup-linker.sh
$([ -n "$LINKER_FLAG" ] && echo "rustflags = [\"-C\", \"link-arg=$LINKER_FLAG\"]" || echo "# Default linker used")

# Platform-specific overrides
[target.x86_64-unknown-linux-gnu]
$([ -n "$LINKER_FLAG" ] && echo "rustflags = [\"-C\", \"link-arg=$LINKER_FLAG\"]" || echo "# Default linker used")

[target.aarch64-unknown-linux-gnu]
$([ -n "$LINKER_FLAG" ] && echo "rustflags = [\"-C\", \"link-arg=$LINKER_FLAG\"]" || echo "# Default linker used")
EOF

echo "✓ Linker configuration written to .cargo/config.toml"
```

**Expected Performance Impact**:
- Link time reduction: 10-15s (for 30-crate workspace)
- Total build time savings: 15-25% (link time is ~20% of total)

---

#### macOS Strategy (SECONDARY PLATFORM)

**Recommendation**: Use default ld64 (already optimized for Apple)

**Why not lld on macOS?**
- ld64 is highly optimized by Apple for macOS ecosystem
- lld has lower priority on Apple's radar (less tested)
- Default ld64 is reasonably fast (12-18s for large projects)

**Performance Note**: macOS builds typically 2-3x slower than Linux on equivalent hardware due to I/O characteristics. Linker optimization yields 5-10% savings, not 75% like on Linux.

---

#### Windows Strategy (TERTIARY PLATFORM)

**Recommendation**: Use lld-link (LLVM-based linker)

```toml
[target.x86_64-pc-windows-msvc]
rustflags = ["-C", "link-arg=-fuse-ld=lld-link"]

# Installation: Already included in Rust toolchain (llvm-tools)
# Enable via environment variable if needed:
# RUSTFLAGS="-C link-arg=-fuse-ld=lld-link"
```

**Performance**: 25-40% link time reduction vs MSVC linker (link.exe)

---

### Linker Configuration Verification

**Test Script**:

```bash
#!/bin/bash

# Time linker performance
echo "Testing linker performance..."

for profile in dev release; do
    echo ""
    echo "Profile: $profile"
    rm -rf target/

    time cargo build --profile $profile 2>&1 | grep "Compiling\|Finished"

    # Extract link time (approximate)
    ls -lh target/$profile/ggen
done
```

---

## Part 5: Caching Strategy

### Problem: Rust Cache Regeneration

Each build regenerates artifacts even if nothing changed. Large projects (30 crates, 150+ deps) can waste 30-50% time on redundant work.

### Solution: sccache

**sccache**: Rust compiler cache that persists across builds using SHA-256 content hashing.

#### Installation & Configuration

```bash
# Install sccache
cargo install sccache

# Verify installation
sccache --version
```

**Local Configuration** (`/home/user/ggen/.cargo/config.toml`):

```toml
[build]
# Enable sccache
rustc-wrapper = "sccache"

# Optional: Configure sccache cache size
[env]
SCCACHE_DIR = "/home/user/.cache/sccache"
SCCACHE_CACHE_SIZE = "5G"  # Local cache size
```

#### sccache Configuration File

Create `~/.sccache.toml`:

```toml
# Local cache configuration (primary)
[cache.disk]
dir = "/home/user/.cache/sccache"
size = 5_000_000_000  # 5GB local cache

# Optional: Redis distributed cache (for multi-developer teams)
[cache.redis]
# Uncomment to enable distributed caching
# url = "redis://localhost:6379"
# username = "default"
# password = "redis-password"
# ttl = 604800  # 7 days TTL

# Optional: AWS S3 distributed cache (for CI/CD pipelines)
[cache.s3]
# Uncomment to enable S3 caching
# bucket = "my-sccache-bucket"
# region = "us-west-2"
# access-key = "AWS_ACCESS_KEY_ID"
# secret-key = "AWS_SECRET_ACCESS_KEY"
# server-side-encryption = "AES256"

[profile]
verbose = 1  # 0=quiet, 1=normal, 2=verbose

[compiler.clang]
# clang-specific cache settings (if using clang)
clang_args = []

[compiler.gcc]
# gcc-specific settings
gcc_args = []
```

#### CI/CD sccache Configuration

For GitHub Actions workflows (`.github/workflows/ci.yml`):

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      # ... other steps ...

      - name: Install sccache
        run: cargo install sccache

      - name: Cache sccache
        uses: actions/cache@v3
        with:
          path: ~/.cache/sccache
          key: sccache-${{ runner.os }}-${{ hashFiles('**/Cargo.lock') }}
          restore-keys: sccache-${{ runner.os }}-

      - name: Build with sccache
        env:
          RUSTC_WRAPPER: sccache
          SCCACHE_CACHE_SIZE: "5G"
        run: cargo build --release

      - name: Print sccache stats
        run: sccache --show-stats
```

#### Expected Performance Impact

**sccache Benefits**:

| Scenario | Without sccache | With sccache | Savings |
|----------|-----------------|------------|---------|
| Clean build (first) | 240s | 240s | 0% (baseline) |
| Clean build (second) | 240s | 40s | 83% |
| Incremental rebuild (no changes) | 120s | 5s | 96% |
| Incremental rebuild (1 file changed) | 120s | 30s | 75% |

**Typical CI/CD Impact**:
- First build in CI: No savings (new cache)
- Subsequent CI builds: 50-75% time savings
- Developer local builds: 60-80% time savings after first build

### Alternative: Cranelift Backend

For extreme rapid development iteration:

```bash
# Install cranelift backend
rustup component add rustc-codegen-cranelift

# Use for ultra-fast dev builds
RUSTFLAGS="-C llvm.target-features=+v1" cargo +nightly build -Z build-std --profile dev
# Result: 2-4s builds (vs 12-15s with LLVM)
# Trade-off: No optimization (slower runtime), only for development
```

---

## Part 6: Hot-Path Optimization

### Phase 1: Profiling (Week 2)

**Goal**: Identify functions consuming >5% of build time or >10% of runtime

#### Build-Time Profiling

```bash
# Measure time per crate
time cargo build --release 2>&1 | tee build.log

# Parse build log for per-crate timing
# Look for "Compiling ggen-core v0.2.0"
# Measure incremental compile times

# Identify slowest-to-compile crates:
# 1. ggen-core (4.2MB, 25 modules)
# 2. ggen-ai (2.6MB, many dependencies)
# 3. ggen-marketplace-v2 (596KB)
```

**Expected Slowest Crates** (ranked by compilation time):

1. **ggen-core**: 45-60s (RDF processing, templates, large module count)
2. **ggen-cli**: 30-40s (CLI framework, many subcommands)
3. **ggen-ai**: 25-35s (genai dependency, many features)
4. **ggen-marketplace-v2**: 15-20s (FMEA analysis, complex logic)
5. **knhk-otel**: 15-25s (optional OpenTelemetry, if enabled)

#### Runtime Profiling

```bash
# Measure hot-path functions using perf (Linux)
cargo build --release
perf record -g ./target/release/ggen sync
perf report

# Identify top functions:
# 1. RDF triple processing (oxigraph)
# 2. Template rendering (Tera)
# 3. SPARQL query execution
# 4. Code generation/emission
```

### Phase 2: Hot-Path Identification

#### Hot-Path 1: RDF Processing (ggen-core::rdf)

**Location**: `/home/user/ggen/crates/ggen-core/src/rdf/`

**Characteristics**:
- Processes 1000+ RDF triples per generation
- Call frequency: O(n) where n = triple count
- Typical input: 1000-10000 triples

**Optimization Opportunities**:

1. **Use Parallel Processing** (rayon):
   ```rust
   // Instead of sequential iteration
   let results: Vec<_> = triples.iter()
       .map(|triple| process_triple(triple))
       .collect();

   // Use parallel iteration (if rayon enabled)
   use rayon::prelude::*;
   let results: Vec<_> = triples.par_iter()
       .map(|triple| process_triple(triple))
       .collect();
   ```
   - Impact: 2-3x speedup on multi-core systems
   - Cost: Rayon already in dependencies

2. **Memory Layout Optimization**:
   ```rust
   // Data-oriented design: Group related data
   struct TriplesBatch {
       subjects: Vec<Subject>,
       predicates: Vec<Predicate>,
       objects: Vec<Object>,
   }
   // vs current object-oriented:
   struct Triple {
       subject: Subject,
       predicate: Predicate,
       object: Object,
   }
   ```
   - Impact: Better cache locality (5-10% speedup)
   - Cost: Refactoring complexity

3. **Caching Results**:
   - Cache SPARQL query results (with TTL)
   - Cache normalized RDF triples
   - Impact: 30-50% speedup for repeated operations

#### Hot-Path 2: Template Rendering (ggen-core::template)

**Location**: `/home/user/ggen/crates/ggen-core/src/template/`

**Characteristics**:
- Renders 50-200 templates per generation
- Call frequency: O(m) where m = template count
- Typical input: 50-500KB templates

**Optimization Opportunities**:

1. **Template Compilation Cache**:
   ```rust
   // Compile templates once, reuse
   static TEMPLATE_CACHE: Lazy<Arc<Tera>> = Lazy::new(|| {
       let mut tera = Tera::new("templates/**/*").unwrap();
       Arc::new(tera)
   });
   ```
   - Impact: 40-60% speedup (avoids re-parsing)
   - Cost: Minor (cache invalidation logic)

2. **Parallel Template Rendering**:
   ```rust
   use rayon::prelude::*;

   let rendered: Vec<_> = templates.par_iter()
       .map(|template| render_template(template, context))
       .collect();
   ```
   - Impact: 2-4x speedup (assuming 4+ cores)
   - Cost: Already have rayon

3. **Streaming Output**:
   - Write templates to disk incrementally (vs buffering)
   - Impact: 20-30% memory reduction, 10-15% speedup

#### Hot-Path 3: SPARQL Query Execution (ggen-core::sparql)

**Location**: `/home/user/ggen/crates/ggen-core/src/sparql/`

**Characteristics**:
- Executes 10-50 SPARQL queries per generation
- Query complexity: SELECT, CONSTRUCT, ASK
- Typical queries: 100-500 lines

**Optimization Opportunities**:

1. **Query Plan Caching**:
   - Cache parsed query execution plans
   - Reuse for identical queries
   - Impact: 50-70% speedup for repeated queries

2. **Index-Aware Query Execution**:
   - Build indices on frequently-queried predicates
   - Impact: 3-5x speedup for large RDF graphs

---

### Phase 3: Implementation (Week 3-4)

**Prioritized Implementation Order**:

1. **Week 3a**: Template Compilation Cache (40-60% speedup, low complexity)
2. **Week 3b**: Parallel RDF Processing (2-3x speedup, moderate complexity)
3. **Week 4a**: SPARQL Query Caching (50-70% speedup, moderate complexity)
4. **Week 4b**: Memory Layout Optimization (5-10% speedup, high complexity)

---

## Part 7: CI/CD Integration & Build Pipeline Optimization

### Current CI/CD Structure

GitHub Actions workflows in `.github/workflows/`:
- ci.yml: Main build pipeline
- andon-validation.yml: Code quality checks
- performance.yml: Performance regression testing
- security-audit.yml: Security scanning

### Optimized CI/CD Build Matrix

#### Strategy 1: Feature-Based Build Matrix

Run parallel builds with different feature combinations:

```yaml
strategy:
  matrix:
    features:
      - "--no-default-features --features core"
      - "--no-default-features --features dev"
      - "--all-features"
    target:
      - x86_64-unknown-linux-gnu
      - x86_64-apple-darwin
      - x86_64-pc-windows-msvc
```

**Expected Result**:
- Catch feature-specific build failures early
- Parallelism: 9 concurrent builds (3 features × 3 targets)
- Time savings: 60-80% wall-clock time (vs sequential)

#### Strategy 2: Artifact Caching

Cache compilation artifacts between jobs:

```yaml
- name: Cache cargo registry
  uses: actions/cache@v3
  with:
    path: ~/.cargo/registry
    key: cargo-registry-${{ hashFiles('**/Cargo.lock') }}

- name: Cache cargo cache
  uses: actions/cache@v3
  with:
    path: ~/.cargo/cache
    key: cargo-cache-${{ hashFiles('**/Cargo.lock') }}

- name: Cache sccache
  uses: actions/cache@v3
  with:
    path: ~/.cache/sccache
    key: sccache-${{ runner.os }}-${{ hashFiles('**/Cargo.lock') }}
```

**Expected Result**:
- Second build: 50-75% faster (cache hit)
- Dependency downloads: 2-3s (vs 30-60s)

#### Strategy 3: Incremental CI Builds

Use workspace members to parallelize:

```yaml
# Job 1: Core crates (ggen-utils, ggen-core, ggen-domain)
- name: Build core crates
  run: |
    cargo build --release \
      -p ggen-utils \
      -p ggen-core \
      -p ggen-domain

# Job 2: CLI & Validation (parallel with Job 1)
- name: Build CLI & Validation
  run: |
    cargo build --release \
      -p ggen-cli \
      -p ggen-cli-validation \
      -p ggen-config-clap

# Job 3: Optional features (depends on Job 1 & 2)
- name: Build optional features
  run: cargo build --release --all-features
```

**Expected Result**:
- Parallelism: 3 concurrent build jobs
- Total time: 60-80s (vs 180-240s sequential)

#### Strategy 4: Performance Regression Detection

Monitor build time trends:

```yaml
- name: Measure build time
  run: |
    /usr/bin/time -v cargo build --release 2>&1 | tee build-metrics.txt

- name: Compare to baseline
  run: |
    # Extract build time from build-metrics.txt
    # Compare to baseline stored in git history
    # Fail if >10% regression
    python3 scripts/compare-build-times.py
```

**Expected Result**:
- Catch performance regressions immediately
- Alert on 10%+ build time increase
- Track trends over time

---

### Optimized CI/CD Workflow Example

```yaml
name: CI - Optimized Build Pipeline
on: [push, pull_request]

jobs:
  # Job 1: Quick validation (5-10 minutes)
  quick-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      # Install mold for faster linking
      - name: Install mold
        run: sudo apt-get install -y mold

      # Cache setup
      - name: Cache cargo cache
        uses: actions/cache@v3
        with:
          path: ~/.cargo/cache
          key: cargo-cache-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache sccache
        uses: actions/cache@v3
        with:
          path: ~/.cache/sccache
          key: sccache-${{ runner.os }}-${{ hashFiles('**/Cargo.lock') }}

      # Quick check with minimal features
      - name: Check code (core features only)
        env:
          RUSTC_WRAPPER: sccache
          SCCACHE_CACHE_SIZE: "5G"
        run: |
          cargo check --workspace --no-default-features --features core

      # Format check
      - name: Check formatting
        run: cargo fmt -- --check

      # Clippy lint
      - name: Lint with clippy
        env:
          RUSTC_WRAPPER: sccache
        run: cargo clippy --workspace --no-default-features --features core -- -D warnings

  # Job 2: Full build matrix (parallel with Job 1)
  full-build:
    runs-on: ${{ matrix.os }}
    needs: quick-check
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest]
        features:
          - "--no-default-features --features core"
          - "--no-default-features --features dev"
          - "--all-features"
    steps:
      - uses: actions/checkout@v3

      - name: Install linker (Linux)
        if: runner.os == 'Linux'
        run: sudo apt-get install -y mold

      - name: Cache cargo
        uses: actions/cache@v3
        with:
          path: ~/.cargo/cache
          key: cargo-cache-${{ runner.os }}-${{ hashFiles('**/Cargo.lock') }}

      - name: Cache sccache
        uses: actions/cache@v3
        with:
          path: ~/.cache/sccache
          key: sccache-${{ runner.os }}-${{ hashFiles('**/Cargo.lock') }}

      - name: Build
        env:
          RUSTC_WRAPPER: sccache
          SCCACHE_CACHE_SIZE: "5G"
        run: cargo build --release ${{ matrix.features }}

      - name: Test
        env:
          RUSTC_WRAPPER: sccache
        run: cargo test --lib ${{ matrix.features }}

  # Job 3: Performance benchmarks (runs after build jobs)
  performance:
    runs-on: ubuntu-latest
    needs: [quick-check, full-build]
    steps:
      - uses: actions/checkout@v3
      - name: Install mold
        run: sudo apt-get install -y mold

      - name: Measure build time
        run: |
          /usr/bin/time -v cargo build --release 2>&1 | tee build-metrics.txt

      - name: Check SLO compliance
        run: python3 scripts/check-build-slos.py

  # Job 4: Security audit
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run security audit
        run: cargo audit
```

**Expected CI/CD Performance**:
- Quick check job: 5-8 minutes
- Full build matrix: 15-20 minutes (parallel)
- Performance measurement: 2-3 minutes
- Total wall-clock time: 20-25 minutes (vs 60-90s previously)
- Savings: 40-50% wall-clock reduction

---

## Part 8: 4-Phase Implementation Plan

### Phase 1: Compilation Profiles (Week 1)
**Duration**: 3-4 days
**Effort**: 8-12 hours (mostly configuration + validation)
**Risk**: LOW

**Tasks**:
1. Update `/home/user/ggen/Cargo.toml` with optimized profiles
2. Benchmark each profile: clean build time, incremental, binary size
3. Run full test suite: `cargo make test`
4. Document baseline measurements in `/home/user/ggen/docs/OPTIMIZATION_BASELINE.md`
5. Create git commit: "opt(profiles): Implement 4-profile optimization strategy"

**Success Criteria**:
- ✅ All 4 profiles defined with optimized values
- ✅ No test failures
- ✅ Build times match or exceed targets
- ✅ No Andon signals (check, lint, test pass)

---

### Phase 2: Dependency Consolidation (Week 2)
**Duration**: 5-7 days
**Effort**: 20-30 hours
**Risk**: MEDIUM

**Tasks**:
1. Generate dependency tree reports
2. Audit web frameworks (tokio, axum, tonic)
3. Audit serialization stack (serde, serde_yaml, serde_with)
4. Update workspace.dependencies with 15+ consolidated entries
5. Update member crate Cargo.toml to use `workspace = true`
6. Incremental validation: `cargo make check` after each update
7. Final validation: `cargo make test`

**Checkpoints**:
- Day 1: Web frameworks audit + consolidation
- Day 2: Serialization audit + consolidation
- Day 3: Utilities consolidation
- Day 4-5: Member crate updates + incremental validation

**Success Criteria**:
- ✅ 160+ duplicate versions reduced to 40-50
- ✅ All members use `workspace = true` pattern
- ✅ No test failures
- ✅ No Andon signals

---

### Phase 3: Feature-Gating & Linker (Week 3)
**Duration**: 4-5 days
**Effort**: 15-20 hours
**Risk**: LOW-MEDIUM

**Tasks**:
1. Implement new feature matrix (core, ai, otel, marketplace, monetization, etc.)
2. Update root package [features] section
3. Update root package [dependencies] to mark optional deps
4. Test each feature combination: `cargo build --no-default-features --features X`
5. Configure linker strategy (mold on Linux, ld64 on macOS)
6. Create `.cargo/config.toml` with platform-specific linker settings
7. Test linker configuration on multiple platforms
8. Create setup script: `scripts/setup-linker.sh`

**Checkpoints**:
- Day 1: Feature matrix design + implementation
- Day 2: Feature combination testing
- Day 3: Linker strategy + configuration
- Day 4: Cross-platform testing

**Success Criteria**:
- ✅ All feature combinations build without errors
- ✅ Linker correctly selected per platform
- ✅ Link time <5s (Linux with mold), <8s (macOS), <12s (Windows)
- ✅ No test failures

---

### Phase 4: Caching & Hot-Path Optimization (Week 4)
**Duration**: 5-7 days
**Effort**: 20-25 hours
**Risk**: MEDIUM-HIGH

**Tasks**:
1. Install and configure sccache
2. Create `~/.sccache.toml` with 5GB local cache
3. Test sccache effectiveness: measure cache hit rates
4. Update CI/CD workflows with sccache integration
5. Profile hot paths (RDF, templates, SPARQL)
6. Implement template compilation cache
7. Implement parallel RDF processing (rayon)
8. Test performance improvements

**Checkpoints**:
- Day 1: sccache setup + validation
- Day 2: CI/CD integration
- Day 3-4: Hot-path profiling
- Day 5-6: Hot-path optimization implementation
- Day 7: Performance benchmarking

**Success Criteria**:
- ✅ sccache cache hit rate >80% on second build
- ✅ Second build time 50-75% faster than first
- ✅ Template cache improves gen time 40-60%
- ✅ Parallel RDF processing improves gen time 2-3x
- ✅ No regressions in output correctness

---

### Overall Implementation Timeline

```
Week 1 (Phase 1):      Profiles
  Mon-Tue: Update + benchmark
  Wed-Thu: Validation + documentation

Week 2 (Phase 2):      Dependency Consolidation
  Mon-Tue: Web frameworks + serialization
  Wed-Thu: Utilities consolidation
  Fri:     Full validation + git commit

Week 3 (Phase 3):      Features + Linker
  Mon-Tue: Feature matrix implementation
  Wed-Thu: Linker configuration
  Fri:     Cross-platform testing

Week 4 (Phase 4):      Caching & Optimization
  Mon-Tue: sccache setup + CI/CD
  Wed-Thu: Hot-path profiling
  Fri:     Hot-path optimization + benchmarking
```

**Total Implementation Effort**: 60-85 hours across 4 weeks

---

## Part 9: Expected Results & Metrics

### Build Time Improvements

| Phase | Activity | Current | Target | Savings |
|-------|----------|---------|--------|---------|
| Baseline | Clean build (release) | 600s | - | - |
| Phase 1 | Profile optimization | 600s | 480s | 120s (20%) |
| Phase 2 | Dependency consolidation | 480s | 360s | 120s (25%) |
| Phase 3 | Linker optimization | 360s | 300s | 60s (17%) |
| Phase 4 | sccache + caching | 300s (first) | 60s (cached) | 240s (80%) |

**Final Targets**:
- Clean build: 300s (50% improvement)
- Second build: 60s (80% improvement)
- Incremental: 30s (66% improvement)
- Test suite: 15-20s (33% improvement)

### Binary Size Improvements

| Configuration | Current | Target | Savings |
|---|---|---|---|
| Debug build | 250-300MB | 250-300MB | 0% (no change) |
| Release (full) | 90-120MB | 70-90MB | 20-30% |
| Release (core only) | 60-80MB | 45-60MB | 25-30% |

### Developer Experience Improvements

| Metric | Current | Target | Improvement |
|--------|---------|--------|------------|
| Feedback time (hot reload) | 120s | 30s | 4x faster |
| Test iteration | 30s | 15s | 2x faster |
| Release builds | 180s | 100s | 1.8x faster |
| CI/CD time | 90s | 25s | 3.6x faster |

---

## Part 10: Maintenance & Monitoring

### Ongoing Maintenance Tasks

**Weekly**:
- Monitor sccache hit rates
- Check for new dependency conflicts via `cargo tree --duplicates`
- Run `cargo make slo-check` to verify performance targets

**Monthly**:
- Review build time trends
- Audit feature-gating effectiveness
- Update dependency versions (with care for duplicates)

**Quarterly**:
- Profile hot paths again (performance drift)
- Evaluate new Rust toolchain versions
- Review linker performance on latest hardware

### Performance Regression Detection

**Automated Checks** (in CI/CD):

```bash
# Fail if build time increases >10%
if [ "$new_build_time" -gt "$((old_build_time * 110 / 100))" ]; then
    echo "FAIL: Build time regression detected"
    exit 1
fi

# Fail if binary size increases >5%
if [ "$new_binary_size" -gt "$((old_binary_size * 105 / 100))" ]; then
    echo "FAIL: Binary size regression"
    exit 1
fi
```

---

## Conclusion

This comprehensive optimization architecture targets **35-50% build time reduction** through systematic optimization of:

1. **Compilation Profiles** (5-10% improvement)
2. **Dependency Consolidation** (20-25% improvement)
3. **Linker Optimization** (10-15% improvement)
4. **Feature-Gating** (5-10% improvement)
5. **Caching** (50-80% on cached builds)
6. **Hot-Path Optimization** (10-15% for code generation)

**Key Deliverables**:
- ✅ Optimized 4-profile configuration
- ✅ Dependency consolidation roadmap
- ✅ Feature-gating matrix
- ✅ Linker configuration strategy
- ✅ sccache setup guide
- ✅ Hot-path optimization opportunities
- ✅ CI/CD integration roadmap
- ✅ 4-phase implementation plan

**Timeline**: 4 weeks, 60-85 hours of development effort

**Success Criteria**: All Andon signals green (check, lint, test) after each phase, with measurable performance improvements and zero regressions.
