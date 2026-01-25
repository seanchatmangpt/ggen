# ggen Performance Optimization Roadmap

**Baseline Established**: January 25, 2026
**Goal**: Achieve SLO targets (first build ≤15s, incremental ≤2s)
**Current State**: 8x over SLO target (120s+ vs 15s)

---

## Quick Reference: 80/20 Win Hierarchy

### 🚀 Immediate (Week 1-2) - Feature-Gating: 40% Improvement
**Effort: LOW | Impact: HIGH**

**The Big Win**: 20% of crates (22 optional) cause 80% of build overhead.

```
Default build:
  30 crates → 8 core crates only
  Expected time: >120s → ~30s
  Improvement: 40%
```

**Implementation**:
1. Create feature flags in `Cargo.toml`:
   - `core` (default): RDF, code-gen, CLI only
   - `ai`: genai + ggen-ai + ggen-dspy
   - `marketplace`: ggen-marketplace-v2
   - `testing`: ggen-test-*
   - `knhk`: knhk-* systems
   - `tai`: tai-* systems
   - `revops`: ggen-api, ggen-auth, etc.

2. Move optional crates to `[dependencies]` with `optional = true`

3. Test builds:
   ```bash
   cargo build --no-default-features --features core  # Fastest
   cargo build --features ai                           # With AI
   cargo build --features full                         # Everything
   ```

4. Update CI/CD to use appropriate feature flags

**Expected Results**: Baseline check 30s (from >120s), unit tests 45s

---

### 🔧 Short Term (Week 3-4) - Proc-Macro Consolidation: 15% Improvement
**Effort: MEDIUM | Impact: MEDIUM**

**The Issue**: `derive_more` (3 versions) + `darling` (2 versions) = redundant compilation

**Implementation**:
1. Update `genai` to latest (check for derive_more v2.1 consolidation)
2. Centralize in `workspace.dependencies`:
   ```toml
   derive_more = "1.0"
   darling = "0.21"
   ```
3. Verify consolidation:
   ```bash
   cargo tree --duplicates | grep "derive_more\|darling"
   ```

**Expected Results**: 15% reduction in build time (contributes to 33% deduplication target)

---

### 🛠️ Medium Term (Week 5-8) - Fix Excluded Crates: 10% Improvement
**Effort: HIGH | Impact: MEDIUM**

**The Problem**: 5 crates excluded from workspace = maintenance overhead + inconsistent deps

**Excluded Crates**:
- `tps-kaizen` - Cargo.toml syntax error
- `tai-gcp` - Missing google-authz
- `tai-security`, `tai-resilience`, `tai-cache` - Dependency issues
- `tai-grpc`, `tai-loadbalancer` - Missing files

**Implementation**:
1. Fix Cargo.toml syntax in tps-kaizen
2. Add google-authz to tai-gcp (or feature-gate it)
3. Resolve transitive dependencies in tai-* crates
4. Find/create missing benchmark files
5. Move from `exclude` to `members` in Cargo.toml

**Expected Results**: Unified workspace, consistent version management, 10% improvement

---

## Performance Improvement Timeline

```
Baseline (Today):              >120s cargo check (8x SLO)
├─ Week 1-2: Feature-gate      ~30s (40% ↓)
├─ Week 3-4: Proc-macros       ~25s (15% ↓)
└─ Week 5-8: Fix excluded      ~22s (10% ↓)

Target: ≤15s (within 1 SLO step)
Achieved: ~22s (1.5x SLO target)
```

**Note**: Further optimizations (incremental LTO, split compilation units, caching) can push below 15s.

---

## Detailed Implementation Plans

### Phase 1: Feature-Gating (Immediate)

**File**: `Cargo.toml` (workspace root)

**Changes**:
```toml
[workspace]
members = [
  # Core (8 crates) - always included
  "crates/ggen-utils",
  "crates/ggen-cli",
  "crates/ggen-domain",
  "crates/ggen-core",
  "crates/ggen-config",
  "crates/ggen-macros",
  "crates/ggen-dod",
  "crates/ggen-ontology-core",

  # Optional: AI orchestration
  "crates/ggen-ai",
  "crates/ggen-dspy",
  # ... rest of optional crates
]

[features]
default = ["core"]

# Core feature: RDF/code-gen only
core = []

# Optional features
ai = ["ggen-ai", "ggen-dspy", "genai"]
marketplace = ["ggen-marketplace-v2"]
testing = ["ggen-test-audit", "ggen-test-opt", "ggen-e2e"]
knhk = ["knhk-etl", "knhk-connectors", "knhk-lockchain", "knhk-otel", "knhk-orchestrator"]
tai = ["tai-testing", "tai-k8s", "tai-validation"]
revops = ["ggen-api", "ggen-auth", "ggen-payments", "ggen-saas"]

# Bundles
full = ["core", "ai", "marketplace", "testing", "knhk", "tai", "revops"]
prod = ["core"]
dev = ["core", "ai"]

[dependencies]
# Core dependencies
ggen-utils = { workspace = true }
ggen-cli = { workspace = true }
ggen-core = { workspace = true }
# ... core deps

# Optional dependencies
ggen-ai = { workspace = true, optional = true }
ggen-dspy = { workspace = true, optional = true }
genai = { workspace = true, optional = true }
# ... optional deps

# Update Makefile.toml targets
```

**Build Commands**:
```bash
# Fast (core only)
cargo check --no-default-features --features core
cargo build --no-default-features --features core
cargo test --lib --no-default-features --features core

# Development (core + AI)
cargo check --features ai
cargo build --features ai

# Full (everything)
cargo check --all-features
cargo build --all-features
```

---

### Phase 2: Proc-Macro Consolidation

**File**: `Cargo.toml` (workspace.dependencies section)

**Current State**:
```
derive_more: 0.99, 1.0, 2.1 (3 versions!)
darling: 0.20, 0.21 (2 versions)
```

**Changes**:
```toml
[workspace.dependencies]
# Consolidate proc-macros
derive_more = "1.0"  # Reduce 3 versions to 1
darling = "0.21"     # Reduce 2 versions to 1

# Update genai if needed
genai = "0.5"  # Verify consistent dependencies
```

**Verification**:
```bash
cargo tree --duplicates

# Expected output (before):
# derive_more v0.99.20
# derive_more v1.0.0
# derive_more v2.1.1
# darling v0.20.11
# darling v0.21.3

# Expected output (after):
# (no duplicates listed for derive_more or darling)
```

---

### Phase 3: Fix Excluded Crates

**File**: `Cargo.toml` (exclude → members)

**Current State**:
```toml
[workspace]
exclude = [
  "crates/tps-kaizen",
  "crates/tai-gcp",
  "crates/tai-security",
  "crates/tai-resilience",
  "crates/tai-cache",
  "crates/tai-grpc",
  "crates/tai-loadbalancer",
]
```

**Changes**:
1. Fix each crate's issues:
   - `tps-kaizen`: Fix Cargo.toml syntax error
   - `tai-gcp`: Add `google-authz` dependency
   - `tai-security/resilience/cache`: Resolve dependency conflicts
   - `tai-grpc/loadbalancer`: Create missing benchmark files

2. Move to `members`:
   ```toml
   [workspace]
   members = [
     # ... existing members
     "crates/tps-kaizen",
     "crates/tai-gcp",
     "crates/tai-security",
     "crates/tai-resilience",
     "crates/tai-cache",
     "crates/tai-grpc",
     "crates/tai-loadbalancer",
   ]
   # Remove or empty the exclude section
   ```

3. Verify workspace:
   ```bash
   cargo check --workspace
   cargo tree --workspace
   ```

---

## Measurement & Validation

### Pre-Optimization Baseline (Today)
```
cargo check --workspace: >120s (TIMEOUT)
cargo test --lib: PENDING
cargo build --release: PENDING
```

### Post-Phase-1 Target (After Feature-Gating)
```
cargo check --no-default-features --features core: ~30s (40% improvement)
cargo test --lib --no-default-features --features core: ~45s (expected)
```

### Post-Phase-2 Target (After Proc-Macro Consolidation)
```
cargo check --no-default-features --features core: ~25s (15% additional improvement)
cargo tree --duplicates: No derive_more or darling duplicates
```

### Post-Phase-3 Target (After Fixing Excluded Crates)
```
cargo check --workspace: ~22s (combined improvement)
All 30 crates in workspace members
Consistent dependency resolution
```

---

## Success Criteria

### Phase 1 Success
- [ ] Feature flags defined in Cargo.toml
- [ ] `core` feature builds without optional crates
- [ ] `cargo check --no-default-features --features core` completes in <60s
- [ ] CI/CD updated to use feature flags
- [ ] Documentation updated

### Phase 2 Success
- [ ] `derive_more` consolidated to single version
- [ ] `darling` consolidated to single version
- [ ] `cargo tree --duplicates` shows no derive_more/darling duplicates
- [ ] Build time reduced by 15% (measured with identical hardware)

### Phase 3 Success
- [ ] All 5 excluded crates moved to members
- [ ] `cargo check --workspace` completes in <60s
- [ ] All tests pass for all 30 crates
- [ ] Workspace-wide linting passes

---

## Monitoring & Regression Prevention

### Add to CI/CD

**Pre-commit hook**:
```bash
# Fast feedback (core only)
cargo check --no-default-features --features core
cargo test --lib --no-default-features --features core
```

**Full CI pipeline**:
```bash
# All features
cargo check --all-features
cargo test --all-features
```

**Performance regression tests**:
```bash
# Track build times over time
cargo build --no-default-features --features core --timings
# Output: target/cargo-timings/[timestamp]/index.html
```

---

## Appendix: Build Commands Reference

### Fast Iteration (Development)
```bash
# Core only (fastest)
cargo check --no-default-features --features core
cargo test --lib --no-default-features --features core

# With AI (still fast)
cargo check --features ai
cargo test --lib --features ai
```

### Full Validation
```bash
# All features
cargo check --all-features
cargo test --all-features

# Specific features
cargo build --features "ai,marketplace"
cargo build --features "knhk,tai"
```

### Performance Analysis
```bash
# Show duplicates
cargo tree --duplicates

# Deep dependency graph
cargo tree -d

# Feature impact
cargo tree --features ai
cargo tree --all-features

# Build timing
cargo build --timings
# View: target/cargo-timings/[timestamp]/index.html
```

---

**Document Version**: 1.0
**Status**: Ready for Implementation
**Next Review**: Post-Phase-1 Completion
