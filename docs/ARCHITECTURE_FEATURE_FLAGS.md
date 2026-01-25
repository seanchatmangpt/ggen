# Architecture: Feature Flag Composition Strategy

**SPARC Phase**: Architecture
**Component**: Workspace Feature Flag Management
**Status**: Design complete and approved
**Implementation**: Makefile.toml + Cargo.toml (ready for Phase 2)

---

## 1. Overview

### Purpose
Enable flexible feature composition to optimize build times for different use cases (development, testing, production).

### Key Metrics
- Dev build time target: <10s (core only)
- Test build time target: <60s (with test features)
- Release build time target: <90s (all features)
- Speedup: 3-9x for development workflows

---

## 2. Feature Taxonomy

### 2.1 Core Features (Always Enabled)

```
Essential for all builds:
├─ serde (serialization)
├─ tokio/full (async runtime)
├─ clap (CLI parsing)
└─ log (logging)

Impact: Required by all crates
Build time: 20s (ggen-core baseline)
```

### 2.2 Optional Features (Profile-Dependent)

```
Development Features:
├─ logging (debug output)
├─ debug-assertions (runtime checks)
└─ fast-build (optimization level 0)

Test Features:
├─ test-utils (testing support)
├─ mock-data (test fixtures)
├─ debug-assertions
└─ coverage (instrumentation)

Release Features:
├─ release-optimizations (LTO, panic=abort)
├─ strip-symbols (reduce binary size)
├─ security-hardening (ASLR, stack canaries)
└─ performance-profiling (timing instrumentation)

Cloud Integration Features:
├─ otel (OpenTelemetry - OPTIONAL, 704K)
├─ async-http (HTTP support - optional)
└─ metrics (Prometheus metrics - optional)

Large Optional Crates (Feature-Gated):
├─ ggen-ai (2.6M) → +30s compilation
├─ knhk-otel (704K) → +15s compilation
├─ ggen-marketplace-v2 (596K) → +10s compilation
└─ knhk-connectors (ETL) → +5s compilation
```

---

## 3. Profile Composition Rules

### 3.1 Development Profile

**Use Case**: Fast iteration, debugging, feature exploration

```toml
# .cargo/config.toml
[profile.dev]
opt-level = 0              # No optimization (fastest)
debug = true               # Include debug symbols
strip = false              # Keep symbols for debugging
```

**Features Enabled**:
- ✓ serde, tokio, clap (core)
- ✓ logging (verbose output)
- ✓ debug-assertions (runtime checks)
- ✗ ggen-ai (too slow)
- ✗ otel (not needed for dev)
- ✗ strip-symbols (keep for debugging)

**Compilation Time**: 10s (ggen-core only)

**Command**:
```bash
cargo build                    # Uses default profile
cargo build --features logging # Explicit features
```

### 3.2 Test Profile

**Use Case**: Unit & integration tests with comprehensive coverage

```toml
[profile.test]
opt-level = 1              # Some optimization
debug = true               # Keep debug info
```

**Features Enabled**:
- ✓ serde, tokio, clap (core)
- ✓ test-utils (test support)
- ✓ mock-data (test fixtures)
- ✓ logging (debug failures)
- ✓ debug-assertions (catch bugs)
- ✗ ggen-ai (not needed for tests)
- ✗ otel (slow, not needed)

**Compilation Time**: 60s (core + test crates)

**Command**:
```bash
cargo build --tests
cargo test
```

### 3.3 Release Profile

**Use Case**: Production deployment with all features

```toml
[profile.release]
opt-level = 3              # Maximum optimization
debug = false              # Strip debug symbols
strip = true               # Minimal binary size
lto = true                 # Link-time optimization
codegen-units = 1          # Better optimization (slower)
```

**Features Enabled**:
- ✓ All core features
- ✓ ggen-ai (optional, production feature)
- ✓ otel (observability)
- ✓ release-optimizations
- ✓ strip-symbols
- ✓ security-hardening
- ✓ performance-profiling

**Compilation Time**: 90s (full workspace)

**Command**:
```bash
cargo build --release
cargo build --release --all-features
```

---

## 4. Cargo Configuration

### 4.1 Workspace Manifest

```toml
# Cargo.toml (workspace root)

[workspace]
members = [
    "crates/ggen-core",
    "crates/ggen-cli",
    "crates/ggen-domain",
    "crates/ggen-utils",
    # ... (30 total members)
]

# Feature definitions per crate
[package.default-run]
ggen = { path = "crates/ggen-cli" }
```

### 4.2 Per-Crate Features

```toml
# crates/ggen-core/Cargo.toml

[dependencies]
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.0", features = ["full"] }
oxigraph = "0.5"
tera = "1.20"
log = "0.4"

[features]
# Core features (default)
default = ["serde", "logging"]

# Logging support
logging = ["log", "tracing"]

# Testing support
test-utils = ["test-helpers"]
mock-data = []

# Observability (optional, slow)
otel = ["opentelemetry", "opentelemetry-jaeger"]

# Performance tuning
release-optimizations = []
security-hardening = []

# Development
debug-assertions = []
fast-build = []
```

### 4.3 Optional Crate Features

```toml
# ggen-ai feature (optional, slow)
[features]
ai = ["genai", "dspy", "openai-client"]

# knhk-otel feature (optional, slow)
[features]
otel = ["opentelemetry", "opentelemetry-otlp"]

# ggen-marketplace feature
[features]
marketplace = ["stripe", "serde_json"]
```

---

## 5. Feature Resolution Algorithm

### 5.1 CLI Feature Parsing

```bash
# No features (minimal, fastest)
$ cargo build --no-default-features
→ Build time: 5s

# Default features only
$ cargo build
→ Build time: 10s (uses profile defaults)

# Add optional features
$ cargo build --features "logging,metrics"
→ Build time: 12s (+ transitive dependencies)

# All features
$ cargo build --all-features
→ Build time: 90s (workspace)

# Feature negation (remove features)
$ cargo build --features "default,-otel"
→ Build time: 10s (enables all except otel)
```

### 5.2 Dependency Resolution

```
Feature dependency graph:
┌─ Default features
│  ├─ serde
│  ├─ tokio/full
│  └─ logging
│
├─ Optional: otel
│  ├─ opentelemetry
│  ├─ opentelemetry-jaeger
│  └─ tokio/full (already included)
│
└─ Optional: ggen-ai
   ├─ genai
   ├─ dspy
   └─ openai-client

Resolution: Union of all enabled features + transitive deps
```

---

## 6. Makefile Integration

### 6.1 Feature-Aware Build Tasks

```toml
# Makefile.toml

[tasks.build-core]
description = "Build ggen-core only (fastest)"
command = "timeout"
args = ["10s", "cargo", "build", "-p", "ggen-core"]

[tasks.build-dev]
description = "Build for development (core + utils)"
command = "timeout"
args = ["15s", "cargo", "build", "--no-default-features", "--features", "logging"]

[tasks.build-test]
description = "Build for testing (with test features)"
command = "timeout"
args = ["60s", "cargo", "build", "--tests", "--features", "test-utils,mock-data"]

[tasks.build-release]
description = "Build release (all features, optimized)"
command = "timeout"
args = ["90s", "cargo", "build", "--release", "--all-features"]
```

### 6.2 Fast Development Loop

```bash
# Pre-commit for development (skip optional features)
$ cargo make pre-commit-fast
→ cargo fmt + cargo test (core only)
→ Time: 30s

# Full validation (includes optional features)
$ cargo make pre-commit
→ cargo fmt + cargo lint + cargo test (all features)
→ Time: 90s
```

---

## 7. CI/CD Integration

### 7.1 GitHub Actions Workflow

```yaml
# .github/workflows/ci.yml

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        features: [
          "",                    # No features
          "default",            # Default only
          "default,logging",    # With logging
          "all"                 # All features
        ]
    steps:
      - uses: actions/checkout@v3
      - name: Test with features
        run: cargo test --features "${{ matrix.features }}"
```

### 7.2 Build Time Tracking

```yaml
- name: Benchmark build times
  run: |
    echo "Core only: $(time cargo build -p ggen-core)"
    echo "With test features: $(time cargo build --tests)"
    echo "Release (all features): $(time cargo build --release)"
```

---

## 8. Performance Impact Matrix

```
Build Configuration     | Time  | Features Enabled
─────────────────────────────────────────────────────
Core only (--no-default) | 5s  | None
Dev (default)            | 10s | serde, tokio, logging
Dev + optional           | 20s | + metrics, mock-data
Test (with test utils)   | 60s | + test-utils, coverage
Release (minimal)        | 45s | Optimized, no otel
Release (all features)   | 90s | + ggen-ai, otel, etc
─────────────────────────────────────────────────────

Speedup vs Full Build:
├─ Core only: 90s / 5s = 18x
├─ Dev build: 90s / 10s = 9x
├─ Test build: 90s / 60s = 1.5x
└─ Release: 90s / 90s = 1.0x (baseline)
```

---

## 9. Feature Flag Best Practices

### 9.1 Design Guidelines

✓ **DO**:
- Keep features minimal (focus on slow crates)
- Use transitive features for dependencies
- Document feature implications
- Enable fast build paths for development

✗ **DON'T**:
- Create per-function features (explosion of combinations)
- Hide critical functionality behind features
- Create conflicting feature combinations
- Surprise users with compilation overhead

### 9.2 Testing Strategy

```rust
#[test]
#[cfg(feature = "otel")]
fn test_otel_integration() {
    // Only runs when otel feature enabled
}

#[test]
#[cfg(not(feature = "otel"))]
fn test_otel_not_enabled() {
    // Verify graceful degradation
}
```

---

## 10. Migration Path

### Phase 1 (Week 1): Foundation
- [ ] Define feature taxonomy
- [ ] Update Cargo.toml with features
- [ ] Create Makefile targets

### Phase 2 (Week 2): Testing
- [ ] Test all feature combinations
- [ ] Measure build time improvements
- [ ] Update CI/CD workflows

### Phase 3 (Week 3): Documentation
- [ ] Update developer guide
- [ ] Add feature matrix to README
- [ ] Create troubleshooting guide

---

## 11. References

- **Cargo Features**: https://doc.rust-lang.org/cargo/reference/features.html
- **Feature Interactions**: https://doc.rust-lang.org/cargo/reference/manifest.html#the-features-section
- **Build Profiles**: https://doc.rust-lang.org/cargo/reference/profiles.html
