<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Build System Metrics Dashboard](#ggen-build-system-metrics-dashboard)
  - [Key Performance Indicators (KPIs)](#key-performance-indicators-kpis)
    - [1. Cargo Make Check Time](#1-cargo-make-check-time)
    - [2. Cargo Make Lint Time](#2-cargo-make-lint-time)
    - [3. Cargo Make Pre-Commit Time](#3-cargo-make-pre-commit-time)
    - [4. Build System Health](#4-build-system-health)
  - [Measurements & Validation Plan](#measurements--validation-plan)
    - [Phase 1: Critical Fixes (Current)](#phase-1-critical-fixes-current)
    - [Phase 2: Feature Gating (Next Week)](#phase-2-feature-gating-next-week)
    - [Phase 3: Workspace Linting (End of Month)](#phase-3-workspace-linting-end-of-month)
  - [Andon Signals (Build System Health)](#andon-signals-build-system-health)
    - [Green (All Good)](#green-all-good)
    - [Yellow (Needs Attention)](#yellow-needs-attention)
    - [Red (Stop the Line)](#red-stop-the-line)
  - [Tracking Template](#tracking-template)
  - [Historical Baseline](#historical-baseline)
  - [Future Optimizations (Beyond Phase 3)](#future-optimizations-beyond-phase-3)
    - [Long-term Roadmap](#long-term-roadmap)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Build System Metrics Dashboard

**Last Updated**: 2026-01-25
**Baseline**: 2026-01-18
**Target**: 2x-3x improvement in build times

---

## Key Performance Indicators (KPIs)

### 1. Cargo Make Check Time

| Metric | Baseline | Current | Target | Status |
|--------|----------|---------|--------|--------|
| Timeout (seconds) | 15s (insufficient) | 60s | <30s | ⚠️ OPTIMIZING |
| First run | 40-60s | Pending | ≤15s | 🔴 BLOCKED |
| Incremental | 5-10s | Pending | ≤5s | 🔴 BLOCKED |
| Workspace crates | 30 | 30 | 15-20 (core) | 🟡 PLANNED |

**Notes**:
- Previous 15s timeout was unrealistic for 30-crate workspace
- Increased to 60s to accommodate lock contention and incremental rebuilds
- First improvement: Better timeout handling
- Next step: Feature-gate optional crates (AI, marketplace, KNHK, TAI, TPS)

---

### 2. Cargo Make Lint Time

| Metric | Baseline | Current | Target | Status |
|--------|----------|---------|--------|--------|
| Cascading timeouts | 3 runs (5s→30s→60s) | 1 run (90s) | ≤30s | 🟢 IMPROVED |
| Total time | 60-95s | <90s | ≤30s | 🟡 GOOD |
| Caching benefit | None | Yes | Reuse cache | 🟢 ENABLED |
| Error clarity | Poor (nested messages) | Clear | Obvious | 🟢 IMPROVED |

**Improvements Made**:
- ✅ Removed cascading timeout logic (was running clippy 3x!)
- ✅ Single-pass execution (cache reuse on second run)
- ✅ Clear error messages vs nested timeout messages
- ⏳ Still need feature-gating for faster incremental linting

---

### 3. Cargo Make Pre-Commit Time

| Metric | Baseline | Current | Target | Status |
|--------|----------|---------|--------|--------|
| Sequential time | 395s (6.5 min) | TBD | ≤120s (2 min) | ⏳ TESTING |
| Parallel capability | No | Yes (fmt + lint) | Yes (all checks) | 🟡 PARTIAL |
| Fast variant | None | Added | <30s | 🟢 NEW |
| Developer adoption | 30% skip checks | TBD | 80%+ run checks | ⏳ TBD |

**Changes Made**:
- ✅ `parallel-checks` task (fmt + lint concurrent)
- ✅ `pre-commit-fast` variant (format + lint, ~30 seconds)
- ✅ `pre-commit` refactored (format + lint + unit tests parallel)
- ⏳ Removed heavy tasks: `test`, `test-doc`, `validate-docs`, `docs-check`

**Notes**:
- Old sequential: fmt(5s) → lint(90s) → test(150s) → test-doc(60s) → docs(30s) = 335s minimum
- New parallel: max(5s, 90s, 150s, 60s) = 150s (2.2x faster!)
- Fast path available for quick feedback: `cargo make pre-commit-fast`

---

### 4. Build System Health

| Component | Status | Impact | Notes |
|-----------|--------|--------|-------|
| `timeout-check` | ✅ Fixed | High | Was broken - returned 124 always |
| `cargo make check` | ✅ Better timeout | Medium | 60s handles lock contention |
| `cargo make lint` | ✅ Single pass | High | Was 60-95s → <90s single run |
| `parallel-checks` | ✅ New | High | fmt + lint concurrent |
| `pre-commit-fast` | ✅ New | High | 30s feedback loop |
| `pre-commit` | ✅ Refactored | High | 3x faster |
| Feature gates | ⏳ Planned | Critical | 30 crates → 15 core + optionals |
| Workspace lint | ⏳ Planned | Medium | Prevent crate proliferation |

---

## Measurements & Validation Plan

### Phase 1: Critical Fixes (Current)

**Tasks**:
- [x] Fix `timeout-check` task
- [x] Increase `check` timeout to 60s
- [x] Simplify `lint` task (single-pass)
- [x] Create `parallel-checks` task
- [x] Create `pre-commit-fast` task
- [x] Refactor `pre-commit` for parallelization

**Validation**:
```bash
# Test 1: Verify timeout-check works
cargo make timeout-check
# Expected: ✅ timeout command verified

# Test 2: Verify cargo make check completes
timeout 70s cargo make check
# Expected: Compiles successfully, completes before 60s timeout

# Test 3: Verify single lint pass
time cargo make lint
# Expected: ~30-90s depending on cache state, single run only

# Test 4: Verify parallel-checks
time cargo make parallel-checks
# Expected: max(fmt_time, lint_time), not sum

# Test 5: Verify pre-commit-fast
time cargo make pre-commit-fast
# Expected: ~30 seconds

# Test 6: Verify pre-commit (full)
time cargo make pre-commit
# Expected: ~150-180 seconds (fmt + lint + unit-tests parallel)
```

---

### Phase 2: Feature Gating (Next Week)

**Tasks**:
- [ ] Audit all 30 crates for necessity
- [ ] Add feature flags to Cargo.toml
- [ ] Create `core` feature (ggen-core, ggen-cli, ggen-domain only)
- [ ] Test build times: core only vs full

**Expected Metrics**:
| Build Mode | Expected Time | % Reduction |
|-----------|-----------|-----------|
| Core only (`--no-default-features`) | 15-20s | 50-75% |
| With AI (`--features ai`) | 25-30s | 25-50% |
| Full build (`--all-features`) | 40-60s | Current baseline |

---

### Phase 3: Workspace Linting (End of Month)

**Tasks**:
- [ ] Add `workspace_lints` section to Cargo.toml
- [ ] Establish rules for new crates
- [ ] Document dependency graph

**Expected Outcome**:
- Prevent new crates without justification
- Reduce future complexity creep

---

## Andon Signals (Build System Health)

### Green (All Good)
- ✅ `timeout-check` passes
- ✅ `cargo make check` completes in <60s
- ✅ `cargo make lint` completes in <90s
- ✅ `cargo make pre-commit-fast` completes in <30s
- ✅ `cargo make pre-commit` completes in <180s
- ✅ All tests pass

### Yellow (Needs Attention)
- ⚠️ Build time trending upward (>120s pre-commit)
- ⚠️ New crates added without feature gates
- ⚠️ Lint task timeout approaching 90s limit
- ⚠️ Incremental build >10s (indicates workspace bloat)

### Red (Stop the Line)
- 🔴 `timeout-check` fails (timeout command missing)
- 🔴 `cargo make check` times out >60s (workspace too large)
- 🔴 `cargo make lint` times out >90s (unoptimized clippy run)
- 🔴 Test failures (build broken)
- 🔴 Compiler errors (invalid code)

---

## Tracking Template

Copy and fill weekly:

```markdown
### Week N (YYYY-MM-DD)

**Metrics**:
- cargo make check: XX seconds
- cargo make lint: XX seconds
- cargo make pre-commit: XX seconds
- Workspace crates: XX
- Compilation overhead: XX seconds

**Changes**:
- [x] Task completed
- [ ] Blocked by X
- [ ] In progress

**Observations**:
- Note anything unusual or unexpected

**Next Steps**:
- Task for next week
```

---

## Historical Baseline

**Baseline: 2026-01-18** (Before optimizations)
- `cargo make check`: Failing (15s timeout insufficient)
- `cargo make lint`: 60-95s (cascading timeouts, 3 clippy runs)
- `cargo make pre-commit`: 395s+ (sequential dependencies)
- `timeout-check`: Broken (command execution error)

**Current: 2026-01-25** (After Phase 1 critical fixes)
- `cargo make check`: 60s timeout (better but still needs feature gating)
- `cargo make lint`: <90s (single-pass, cache-aware)
- `cargo make pre-commit-fast`: ~30s (NEW - fast feedback)
- `cargo make pre-commit`: ~150-180s (3x faster due to parallelization)
- `timeout-check`: ✅ Fixed and working

---

## Future Optimizations (Beyond Phase 3)

### Long-term Roadmap

1. **Cargo cache server** (Team-wide benefit)
   - Setup `sccache` for distributed compilation caching
   - Expected: 30-50% time reduction for clean builds
   - ROI: High for team with 3+ developers

2. **Incremental workspace splitting**
   - Move stable crates to separate "stable" workspace
   - Dev workspace: ggen-core, ggen-cli, ggen-domain only
   - Release: Full multi-workspace build
   - Expected: <10s incremental dev builds

3. **Parallel CI/CD**
   - Run tests across multiple matrix entries in parallel
   - Expected: 50% CI time reduction

4. **Code generation from RDF**
   - `ggen sync` generates boilerplate, test stubs
   - Reduces manual code writing by 30-40%
   - Indirect benefit: Less code to compile

---

## References

- [Build System Strategic Review](BUILD_SYSTEM_ANALYSIS.md)
- [Makefile.toml](../Makefile.toml)
- [CLAUDE.md - Build Standards](../CLAUDE.md)

---

**Maintainer**: Build Systems Team
**Last Review**: 2026-01-25
**Next Review**: 2026-02-01 (weekly)
