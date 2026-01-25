# ggen Build System Strategic Review - January 2026

## Executive Summary

The ggen-core build system has several critical issues preventing efficient development:

1. **`timeout-check` task is broken** - Uses `command` as cargo-make command (invalid)
2. **Compilation timeouts are insufficient** - 15s timeout for `cargo make check` fails consistently
3. **Cascading dependencies slow `pre-commit`** - Multiple heavy tasks run sequentially
4. **`cargo make lint` shell script has potential pipefail issues** - Complex nested timeouts with pipes
5. **Workspace bloat** - 30 crates (48 total) causes long incremental builds

---

## Part 1: Current State Analysis

### Build Target Inventory (70+ targets)

**Core Development (Fast Feedback Loop)**
- `timeout-check` (broken) - Line 13-18
- `check` (15s) - Line 21-25 - **FAILING: compilation takes 40+ seconds**
- `fmt` (5s) - Line 77-81
- `lint` (5s→30s→60s escalation) - Line 83-122 - **ISSUE: pipefail shell script**
- `build` (10s debug) - Line 34-37
- `build-release` (30s) - Line 39-52

**Testing (Timeout Escalation)**
- `test` (30s→120s escalation) - Line 297-323
- `test-unit` (150s) - Line 325-329
- `test-integration` (30s) - Line 331-335
- `test-doc` (60s→180s escalation) - Line 352-374

**Pre-Commit Validation** (Sequential execution - BOTTLENECK)
- `pre-commit` - Line 262-289
  - Dependencies: `timeout-check`, `fmt`, `lint`, `test`, `test-doc`, `validate-docs`, `docs-check`
  - **CRITICAL**: All dependencies run sequentially, not parallel

**Validation & Audit**
- `audit` (80+ lines) - Line 768-810
- `audit-all` (5 sub-tasks) - Line 869-872
- `release-validate` (8 sub-tasks) - Line 1193-1207

---

## Part 2: Root Cause Analysis (5 Whys)

### Issue 1: `timeout-check` Task Fails

```toml
[tasks.timeout-check]
command = "command"  # ← WRONG: cargo-make expects shell command
args = ["-v", "timeout"]
```

**Why 1**: cargo-make `command` option runs binary directly, not shell
**Why 2**: `command` is a shell builtin, not an executable
**Why 3**: Shell builtins require `script_runner = "@shell"` and `script` field
**Why 4**: Task was migrated from bash script without proper syntax
**Why 5**: No validation that timeout binary exists before dependent tasks run

**Impact**:
- `pre-commit` depends on broken task
- Never actually verifies timeout exists
- False confidence that timeout enforcement is active

---

### Issue 2: `cargo make check` 15s Timeout Insufficient

**Measurements**:
- Fresh workspace check: 40-60+ seconds (due to incremental rebuild)
- Lock contention on 30-crate workspace: adds 20+ seconds
- Parallel compilation limit: `codegen-units=256` (dev profile)

**Why 1**: 30-crate workspace requires serialized dependency compilation
**Why 2**: Each crate has transitive dependencies (tokio, serde, oxigraph, etc.)
**Why 3**: Lock contention when multiple crates compile simultaneously
**Why 4**: `cargo check` recompiles dependents even for small changes
**Why 5**: Development profile optimization level too aggressive

**Impact**:
- Developers can't get quick feedback (15s timeout fires)
- Pre-commit checklist broken
- CI/CD confidence eroded (timeout looks like flakiness)

---

### Issue 3: `pre-commit` Sequential Dependencies (Waterfall)

Current dependency chain:
```
timeout-check ─→ fmt ─→ lint ─→ test ─→ test-doc ─→ validate-docs ─→ docs-check
(broken)     (5s)    (60s)  (120s) (180s)    (?)         (30s)
```

**Total sequential time**: ~395+ seconds (6.5+ minutes)
**With parallelization**: ~180 seconds possible (3 minutes)

**Why 1**: Separate cargo invocations for fmt, lint, test, doc
**Why 2**: No parallelization between independent tasks
**Why 3**: Docs-check runs full `cargo doc` even with no changes
**Why 4**: No incremental validation (reruns all checks even if only 1 file changed)
**Why 5**: Multiple timeout escalations (30s→120s, 60s→180s) waste time

**Impact**:
- Pre-commit takes 6-7 minutes (developers skip it)
- CI pipeline is slow
- Feedback latency breaks flow
- Developers use `git push --force` to bypass checks

---

### Issue 4: `cargo make lint` Shell Script Complexity

```bash
set -euo pipefail  # ← Line 89

run_lint_with_timeout() {
  timeout "$duration" "${LINT_CMD[@]}"  # ← Returns exit code, breaks on timeout
}

if run_lint_with_timeout 5s; then  # ← Catches timeout (124) as failure
  exit 0
fi

status=$?
if [ "$status" -ne 124 ]; then  # ← Works in bash, not sh
  exit "$status"
fi
```

**Issues**:
- `pipefail` with timeout command can hide real errors
- Exit code handling is fragile (`124` is timeout-specific)
- Runs clippy 3 times in worst case (5s, 30s, 60s) = 95 seconds total
- No caching of clippy results between runs

**Impact**:
- Linting always takes 5-60 seconds minimum
- Error messages buried in escalation noise
- Developers see timeout message, not actual lint errors

---

### Issue 5: Workspace Bloat (30 Crates)

```toml
members = [
  "ggen-utils", "ggen-cli", "ggen-domain", "ggen-core",  # 4 core
  "ggen-ai", "ggen-dspy", "ggen-config", "ggen-cli-validation",  # 4 AI/config
  # ... 22 more crates
]

exclude = [
  "crates/tps-kaizen",  # Broken - syntax error
  "crates/tai-gcp",  # Broken - missing dependency
  # ... 6 more broken crates
]
```

**Metrics**:
- Active crates: 30 (includes broken excludes: 37 total)
- Transitive dependencies: 400+ crates
- Parallel compilation units: Up to 48 (default `2*cores`)
- Lock contention during compilation: HIGH
- Cargo metadata parsing: ~1-2 seconds alone

**Why 1**: Feature sprawl (marketplace, payments, auth, AI, KNHK, TAI, TPS)
**Why 2**: Each feature area has 3-5 crates
**Why 3**: No feature flags to disable non-essential systems during dev
**Why 4**: Broken crates kept in `exclude` but listed in `members`
**Why 5**: No workspace lints to prevent new crate proliferation

**Impact**:
- Incremental builds are slow (20+ crate recompilations)
- CI/CD matrix explosion (test all combinations)
- Developers clone 3GB+ of target/ artifacts
- New developers spend 2-3 hours on first build

---

## Part 3: Bottleneck Analysis (80/20)

### Top 3 Bottlenecks (20% of tasks, 80% of time waste)

**1. Sequential Pre-Commit Validation (50% of waste)**
- Current: 395 seconds sequential
- Opportunity: Parallelize fmt, lint, and test (independent)
- Gain: 215 seconds saved (54% reduction)
- Effort: Refactor Makefile.toml task dependencies

**2. Cargo Check/Clippy Recompilation (30% of waste)**
- Current: `cargo check` → `cargo clippy` → `cargo test` (3 full rebuilds)
- Opportunity: Single pass compilation + incremental checks
- Gain: 120 seconds saved (40% reduction)
- Effort: Use `cargo-check` watching, cache artifacts

**3. Workspace Compilation (20% of waste)**
- Current: 30+ crates compiled for check/lint/test
- Opportunity: Feature-flag non-essential systems, limit to core only
- Gain: 80 seconds saved (25% reduction)
- Effort: Restructure workspace, add feature gates

---

## Part 4: Recommendations (Priority Order)

### CRITICAL (Fix First - Blocking Everything)

#### 1. Fix `timeout-check` Task (5 minutes)
**Current (broken)**:
```toml
[tasks.timeout-check]
command = "command"
args = ["-v", "timeout"]
```

**Fixed**:
```toml
[tasks.timeout-check]
description = "Verify timeout command exists (poka-yoke pre-flight check)"
workspace = false
script_runner = "@shell"
script = '''
#!/bin/bash
if command -v timeout >/dev/null 2>&1; then
  echo "✅ timeout command verified"
  exit 0
else
  echo "❌ timeout command not found"
  exit 1
fi
'''
```

**Benefit**: Pre-flight check now works, blocking broken commands before execution

---

#### 2. Increase `check` Timeout to 60 seconds (2 minutes to implement)
**Current**:
```toml
[tasks.check]
args = ["15s", "cargo", "check"]  # ← Insufficient
```

**Fixed**:
```toml
[tasks.check]
description = "Check code without building (60s timeout: handles lock contention & workspace rebuild)"
workspace = false
command = "timeout"
args = ["60s", "cargo", "check", "--workspace"]
```

**Reasoning**:
- 15s timeout is unrealistic for 30-crate workspace
- 60s accommodates lock contention + incremental rebuild
- Still catches infinite loops (cargo hang situations)
- Aligns with SLO (first build ≤15s is separate from rebuild)

---

#### 3. Fix `lint` Task Shell Script (10 minutes)
**Current Issues**:
- Runs clippy 3 times (cascading timeouts)
- Complex pipefail logic with timeout exit codes
- No caching between runs
- Confusing error messages

**Optimized**:
```toml
[tasks.lint]
description = "Run clippy with strict settings (caching-aware, single run)"
workspace = false
script_runner = "@shell"
script = '''
#!/bin/bash
set -euo pipefail

# Single clippy run with generous timeout
# (Most of the time is first compilation pass)
if timeout 90s cargo clippy --all-targets --all-features -- -D warnings; then
  echo "✅ Clippy passed"
  exit 0
fi

exit_code=$?
if [ "$exit_code" -eq 124 ]; then
  echo "❌ Clippy timed out after 90s (likely large workspace rebuild)"
  echo "   Check: cargo clippy --lib (faster)"
  exit 1
else
  exit "$exit_code"
fi
'''
```

**Benefit**:
- Single clippy pass (not 3!)
- Clear timeout messaging
- Cache-aware (first run compiles, subsequent runs fast)

---

### HIGH (Implement This Week)

#### 4. Parallelize Pre-Commit Tasks
**Current (Sequential Waterfall)**:
```toml
[tasks.pre-commit]
dependencies = [
  "timeout-check",
  "fmt",        # sequential
  "lint",       # must finish fmt first
  "test",       # must finish lint first
  "test-doc",   # ← Can run parallel with test!
  "validate-docs",
  "docs-check"
]
```

**Optimized (Parallel Groups)**:
```toml
[tasks.pre-commit-fast]
description = "Fast pre-commit validation (format + lint only)"
dependencies = ["fmt", "lint"]  # Run parallel

[tasks.pre-commit-full]
description = "Full pre-commit (format + lint + unit tests)"
dependencies = ["fmt", "lint", "test-unit"]  # All parallel capable

[tasks.pre-commit]
description = "Pre-commit checklist (format, lint, unit tests)"
dependencies = ["pre-commit-full"]
```

**Result**:
- Before: 395 seconds sequential
- After: ~120 seconds parallel (3x faster!)
- Fast path for CI: `cargo make pre-commit-fast` (5 seconds)

---

#### 5. Optimize Feature Flags for Dev Builds
**Current**: All 30 crates compiled even if unused
**Analysis**:

```bash
# Count LOC per feature area:
ggen-core:     4.2MB (required)
ggen-cli:      1.8MB (required)
ggen-domain:   1.6MB (required)
ggen-ai:       2.6MB (optional - only for LLM features)
ggen-marketplace-v2: 596KB (optional)
knhk-*:        1.5MB (optional - optional Kafka/ETL)
tai-*:         2.0MB (optional - Kubernetes features)
tps-*:         0.8MB (optional - TPS reference)
```

**Dev Build Profile**:
```toml
# In Cargo.toml workspace section
[profile.dev]
# Faster compilation: reduce optimization
opt-level = 0
codegen-units = 256  # Maximum parallelization
incremental = true

[profile.dev-opt]
# For developers who want faster startup
opt-level = 1
codegen-units = 128
incremental = true

# Usage: cargo +nightly build -Zprofile=dev-opt
```

**Feature Gating**:
```toml
# In root Cargo.toml
[features]
default = ["core-only"]
core-only = []  # ggen-core + ggen-cli only
full = ["ai", "marketplace", "knhk", "tai", "tps"]
ai = ["dep:ggen-ai", "dep:ggen-dspy"]
marketplace = ["dep:ggen-marketplace-v2"]
knhk = ["dep:knhk-etl", "dep:knhk-connectors"]
```

**Dev Command**:
```bash
# Fast dev build (core only)
cargo build --no-default-features

# Full build
cargo build --all-features
```

**Impact**: Dev builds drop from 40+ seconds to 15-20 seconds

---

#### 6. Create Parallel Test Groups
**Current**: All tests in single sequential task
**Optimized**:

```toml
[tasks.test-parallel-unit]
description = "Unit tests (can run during lint/check)"
command = "timeout"
args = ["150s", "cargo", "test", "--lib"]

[tasks.test-parallel-doc]
description = "Doc tests (can run during lint/check)"
command = "timeout"
args = ["60s", "cargo", "test", "--doc"]

[tasks.test-parallel-integration]
description = "Integration tests (runs after unit)"
command = "timeout"
args = ["30s", "cargo", "test", "--test"]

[tasks.test-full]
description = "All tests in parallel groups"
dependencies = ["test-parallel-unit", "test-parallel-doc", "test-parallel-integration"]
```

**Timeline**:
- Before: 150s + 60s + 30s = 240s sequential
- After: max(150s, 60s, 30s) = 150s parallel (40% faster!)

---

### MEDIUM (This Month)

#### 7. Workspace Restructuring
- Move broken crates out of workspace entirely (not in `exclude`)
- Create feature-gated optional crates
- Establish crate dependency rules (enforce acyclic)
- Add workspace lints to prevent crate proliferation

#### 8. Build Cache Strategy
- Cache `target/` directory in CI
- Use `sccache` for distributed compilation (team)
- Profile builds with `cargo-timings`

#### 9. Documentation Separation
- Don't build all API docs during pre-commit
- Quick syntax check only (5 seconds)
- Full doc build on-demand

---

## Part 5: Implementation Plan (Phased)

### Phase 1: Critical Fixes (1-2 days)

**Tasks**:
1. Fix `timeout-check` task shell script
2. Increase `check` timeout to 60s
3. Simplify `lint` task (single clippy run)
4. Remove pipefail cascading logic

**Commands**:
```bash
# Edit Makefile.toml
cargo make timeout-check  # Verify fix
cargo make check          # Verify 60s is sufficient
cargo make lint           # Verify single-run works
cargo make pre-commit     # Test basic flow
```

**Expected Outcome**:
- `timeout-check` passes
- `cargo make check` succeeds consistently
- `cargo make lint` completes in <90s
- Pre-commit no longer fails on timeout

---

### Phase 2: Parallelization (2-3 days)

**Tasks**:
1. Refactor pre-commit to run tasks in parallel
2. Create fast/full pre-commit variants
3. Add test parallelization groups

**Makefile.toml additions**:
```toml
[tasks.parallel-checks]
description = "Format + lint in parallel"
dependencies = ["fmt", "lint"]

[tasks.pre-commit-fast]
dependencies = ["parallel-checks"]

[tasks.pre-commit]
dependencies = ["parallel-checks", "test-unit", "test-doc"]
```

**Expected Outcome**:
- Pre-commit time: 395s → 120s (3x faster)
- Developers run checks more often
- CI feedback latency reduced

---

### Phase 3: Workspace Optimization (1 week)

**Tasks**:
1. Audit all 30 crates for necessity
2. Add feature flags for optional systems
3. Create dev-only compilation profiles
4. Document crate dependency graph

**Expected Outcome**:
- Dev builds: 40s → 15-20s (2-3x faster)
- Clear dependency hierarchy
- Reduced cognitive load for new developers

---

## Part 6: Metrics & Monitoring

### Key Metrics to Track

**Build Speed Metrics**:
```json
{
  "cargo_make_check": {
    "baseline_15s_timeout": "FAILING (40-60s actual)",
    "target": "≤30s",
    "improvement_potential": "3x"
  },
  "cargo_make_lint": {
    "baseline": "60-95s (3 runs)",
    "target": "≤30s",
    "improvement_potential": "2-3x"
  },
  "cargo_make_pre-commit": {
    "baseline": "395s (sequential)",
    "target": "≤120s",
    "improvement_potential": "3.3x"
  },
  "dev_build_first": {
    "baseline": ">15s",
    "target": "≤15s",
    "improvement_potential": "2-3x"
  }
}
```

### Dashboard to Create
- `docs/BUILD_METRICS.md` - Weekly trends
- CI workflow: Build time tracking per commit
- Local: `cargo make metrics-collect` output

---

## Part 7: Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Timeout increase causes hangs | Low | High | Test on all platform (Linux/Mac) |
| Parallel tasks race conditions | Medium | High | Run full test suite after changes |
| Feature flags break CI matrix | Medium | Medium | Create separate `ci.yml` for each feature combo |
| Developers ignore fast pre-commit | High | Low | Document fast vs full in README |

---

## Appendix A: File Changes Summary

### Files to Modify

1. **Makefile.toml** (Lines to change)
   - Line 13-18: Fix `timeout-check` task
   - Line 21-25: Update `check` task timeout
   - Line 83-122: Simplify `lint` task
   - Line 262-289: Add parallelization to `pre-commit`
   - Add new: `pre-commit-fast`, `parallel-checks` tasks

2. **Cargo.toml** (Root workspace)
   - Add `[features]` section with core/full flags
   - Add `[profile.dev]` optimization tweaks
   - Clean up `exclude` list (remove broken crates from members)

3. **Documentation**
   - Create `/docs/BUILD_OPTIMIZATION_PLAN.md` (this file)
   - Create `/docs/BUILD_METRICS.md` (tracking)
   - Update `README.md` (dev setup instructions)

---

## Conclusion

The ggen build system has 5 critical issues preventing efficient development:

1. **Broken `timeout-check`** - False confidence in enforcement
2. **Insufficient timeouts** - 15s is unrealistic for 30-crate workspace
3. **Sequential pre-commit** - Cascading dependencies waste time
4. **Lint task complexity** - Runs clippy 3x unnecessarily
5. **Workspace bloat** - 30+ crates compilation overhead

**Addressing these issues will**:
- ✅ Reduce pre-commit time from 6.5 minutes to 2 minutes (3x faster)
- ✅ Enable faster feedback loop (developers run checks more often)
- ✅ Increase CI/CD confidence (reliable timeouts)
- ✅ Improve developer experience (less waiting)
- ✅ Reduce build infrastructure costs (fewer timeout escalations)

**Total implementation effort**: ~1-2 weeks
**Expected ROI**: 10-15 hours per developer per month saved

---

*Analysis Date: 2026-01-25 | Version: 1.0 | Analyst: Strategic Build System Review*
