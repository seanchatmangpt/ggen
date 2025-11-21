# FMEA (Failure Mode and Effects Analysis) - ggen Technical Debt

## Executive Summary

FMEA systematically identifies how the ggen codebase can fail, what impact those failures have, and what controls exist to prevent them. Using Risk Priority Number (RPN = Severity × Occurrence × Detection), we identify the highest-risk issues requiring immediate mitigation.

**Analysis Date**: 2025-11-21
**Scope**: Critical technical debt items from kaizen audit
**Methodology**: ISO 16949 / AIAG FMEA Standard

---

## Critical FMEA Items (RPN > 100)

### FMEA-001: Vendored OpenSSL Build Race Condition
**Failure Mode**: OpenSSL source compilation fails with touch/mv race condition
**Effects**:
- Entire CI/CD pipeline blocked (P1 severity)
- Unable to merge code or deploy releases
- Team unable to work (complete stop)
- SLO breach: 109.6s build vs 15s target

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 10 | Complete system failure, blocks all work |
| **Occurrence (O)** | 9 | Happens on every clean build, every CI run |
| **Detection (D)** | 2 | Caught immediately by build system |
| **RPN** | **180** | CRITICAL - Requires immediate action |

**Current Controls**:
- Build fails fast (detected immediately)
- Pre-commit hook validation catches early

**Recommended Actions** (Priority Order):
1. Remove `vendored-openssl` feature, use system OpenSSL
2. Set `OPENSSL_DIR=/opt/homebrew/opt/openssl@3` in CI
3. Add build timeout handling to prevent hanging
4. Document workaround in TROUBLESHOOTING.md

**Prevention**: Use system libraries instead of vendoring C dependencies

---

### FMEA-002: 30+ Panic Calls in Production Code
**Failure Mode**: Production code panics due to unwrap()/expect() on recoverable errors
**Effects**:
- User-facing application crashes unexpectedly (P1 severity)
- Data loss possible if not properly cleaned up
- No graceful error handling or recovery
- Violates "fail soft" principle
- Production incidents guaranteed

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 10 | Complete application failure, data loss risk |
| **Occurrence (O)** | 7 | Will happen when error condition encountered |
| **Detection (D)** | 8 | Hard to detect before reaching production |
| **RPN** | **560** | CRITICAL - Highest risk item |

**Current Controls**:
- Pre-push hook Gate 2.5 checks for expect() calls
- Clippy linting warns about panic-prone code
- Unit tests may catch some cases

**Recommended Actions** (Priority Order):
1. Replace all `panic!()` with `Result<T,E>` returns
2. Replace `unwrap()` with `?` operator in error paths
3. Replace `expect()` with `map_err()` + context
4. Add runtime error handling at system boundaries
5. Test error paths in integration tests

**Prevention**: Use Result<T,E> pattern exclusively in libraries

---

### FMEA-003: 150+ Expect() Violations Without Documentation
**Failure Mode**: expect() calls in code without #[allow(clippy::expect_used)] documentation
**Effects**:
- Violates code standards (P2 severity)
- Lacks documentation of why expect() is safe
- Pre-push hook blocks merges
- Technical debt accumulates

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 6 | Code quality issue, not system failure |
| **Occurrence (O)** | 10 | 150+ instances across codebase |
| **Detection (D)** | 1 | Pre-push hook catches 100% |
| **RPN** | **60** | HIGH - Systematic issue but well-controlled |

**Current Controls**:
- Pre-push hook Gate 2.5 validates each expect() call
- Requires #[allow(clippy::expect_used)] with 5-line context check
- Hook prevents commits without documentation

**Recommended Actions** (Priority Order):
1. Audit each expect() call systematically
2. For test code: add #[allow(clippy::expect_used)] with justification
3. For production code: replace with proper error handling
4. Document which expect() calls are acceptable
5. Maintain register of allowed violations with timeline

**Prevention**: Systematic remediation plan per crate

---

### FMEA-004: First Build SLO Failure (109.6s vs 15s)
**Failure Mode**: Build takes 730% longer than SLO target
**Effects**:
- Developer experience severely degraded (P2 severity)
- CI/CD pipeline slow and expensive
- Resource waste (compute, electricity)
- Team productivity reduced
- Feedback loops extended (slower iteration)

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 7 | Blocks productivity but not catastrophic |
| **Occurrence (O)** | 10 | Happens on every clean build |
| **Detection (D)** | 1 | Build metrics visible immediately |
| **RPN** | **70** | HIGH - Systemic performance issue |

**Current Controls**:
- `cargo make slo-check` monitors build times
- Pre-commit hook includes timeout-based checks
- CI metrics track build performance

**Recommended Actions** (Priority Order):
1. Remove vendored-openssl (Issue FMEA-001, saves ~60s)
2. Make git2 optional feature (saves ~30s)
3. Unify axum versions (saves ~10s)
4. Add sccache to CI for distributed caching
5. Feature-gate telemetry dependencies
6. Profile and optimize hot crates

**Prevention**: Dependency management and build optimization

---

### FMEA-005: 10 Modules Over 500 Lines
**Failure Mode**: Source files exceed maintainability threshold (500 lines)
**Effects**:
- Code difficult to understand and modify (P2 severity)
- Higher bug rate in large files
- Slower code review and navigation
- Incremental build slower for those files
- Testing complexity increases

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 5 | Maintainability issue, not functional failure |
| **Occurrence (O)** | 10 | 10 files confirmed over limit |
| **Detection (D)** | 2 | Easy to measure with line count |
| **RPN** | **100** | MEDIUM-HIGH - Systemic code quality issue |

**Current Controls**:
- Code review guidelines recommend <500 lines
- Linting could catch this (not yet enabled)
- Architecture guidelines document best practice

**Recommended Actions** (Priority Order):
1. Enable `#![warn(missing_docs)]` per-crate to catch large modules
2. Split `marketplace-v2/lifecycle/install.rs` (1,649 lines)
3. Split `marketplace-v2/lifecycle/production.rs` (1,385 lines)
4. Split `marketplace-v2/lifecycle/search.rs` (1,370 lines)
5. Create module organization plan for each crate
6. Add line-count checks to CI

**Prevention**: Enforce module size limits during code review

---

### FMEA-006: Inconsistent Error Types Across Crates
**Failure Mode**: Multiple incompatible error types (struct vs enum vs thiserror)
**Effects**:
- Error handling code fragmented and inconsistent (P2 severity)
- Difficult to propagate errors across crate boundaries
- Boilerplate `.map_err()` code everywhere
- Harder to debug and trace error origins
- Poor error context loss between conversions

| Metric | Rating | Justification |
|--------|--------|---------------|
| **Severity (S)** | 6 | Makes error handling harder, not broken |
| **Occurrence (O)** | 10 | All error calls affected |
| **Detection (D)** | 3 | Visible during type checking |
| **RPN** | **180** | CRITICAL - Architectural inconsistency |

**Current Controls**:
- Each crate has error module documentation
- Clippy catches some error handling issues
- Type system prevents cross-crate errors

**Recommended Actions** (Priority Order):
1. Create unified `ggen-error` crate
2. Define all error types as single thiserror enum
3. Implement From traits for all error conversions
4. Update all crates to depend on `ggen-error`
5. Remove duplicate error definitions
6. Add error context middleware

**Prevention**: Single error type defined upfront, shared across all crates

---

## Medium FMEA Items (RPN 30-100)

### FMEA-007: 59+ Dead Code Suppressions
**Severity: 6 | Occurrence: 8 | Detection: 1 | RPN: 48**
- Mix of legitimate (phantom types) and actual dead code
- Clutters codebase, makes code harder to understand
- **Action**: Audit and remove actual dead code, document intentional suppressions

### FMEA-008: 3 Duplicate Axum Versions in Dependency Tree
**Severity: 5 | Occurrence: 10 | Detection: 1 | RPN: 50**
- Three incompatible axum versions compiled separately
- Binary bloat, slower builds, potential version conflicts
- **Action**: Pin single version in workspace dependencies

### FMEA-009: Fragmented MAPE-K Implementations (3 locations)
**Severity: 6 | Occurrence: 9 | Detection: 3 | RPN: 162**
- Core autonomic loop implemented three separate ways
- Maintenance burden, inconsistent behavior risk
- **Action**: Consolidate into single authoritative implementation

### FMEA-010: ggen-core "God Crate" (35+ modules)
**Severity: 5 | Occurrence: 10 | Detection: 2 | RPN: 100**
- Single crate handles templates, graphs, lifecycle, security, etc.
- Violates single responsibility principle
- Slow incremental builds (22.4s for single file change)
- **Action**: Split into focused crates

---

## Low Risk Items (RPN < 30)

### FMEA-011: Async/Sync API Boundary Confusion
**Severity: 4 | Occurrence: 8 | Detection: 5 | RPN: 160**
- Inconsistent async/sync APIs across crates
- Documentation unclear on which to use
- **Action**: Standardize async-first with blocking variants

### FMEA-012: Public API Documentation Gaps
**Severity: 3 | Occurrence: 8 | Detection: 4 | RPN: 96**
- Many public items lack documentation
- Users struggle to understand API surface
- **Action**: Enable `missing_docs` lint, add docs

---

## RPN Summary Table

| ID | Failure Mode | Severity | Occurrence | Detection | RPN | Priority |
|-----|------|----------|-----------|-----------|-----|----------|
| FMEA-002 | Panics in production | 10 | 7 | 8 | **560** | **CRITICAL** |
| FMEA-001 | OpenSSL build failure | 10 | 9 | 2 | **180** | **CRITICAL** |
| FMEA-006 | Inconsistent error types | 6 | 10 | 3 | **180** | **CRITICAL** |
| FMEA-009 | Fragmented MAPE-K | 6 | 9 | 3 | **162** | HIGH |
| FMEA-011 | Async/sync confusion | 4 | 8 | 5 | **160** | HIGH |
| FMEA-003 | 150+ expect() violations | 6 | 10 | 1 | **60** | HIGH |
| FMEA-004 | Build SLO failure | 7 | 10 | 1 | **70** | HIGH |
| FMEA-008 | Duplicate axum versions | 5 | 10 | 1 | **50** | HIGH |
| FMEA-007 | Dead code suppressions | 6 | 8 | 1 | **48** | MEDIUM |
| FMEA-010 | God crate (ggen-core) | 5 | 10 | 2 | **100** | MEDIUM |
| FMEA-012 | Documentation gaps | 3 | 8 | 4 | **96** | MEDIUM |

---

## Mitigation Strategy

### Immediate (RPN > 150): This Week
1. Fix OpenSSL build (FMEA-001) - Unblocks everything
2. Replace panics with Result<T,E> (FMEA-002) - Safety critical
3. Create unified error type (FMEA-006) - Architecture fix
4. Consolidate MAPE-K (FMEA-009) - Core logic

### Short-term (RPN 50-150): Week 1-2
1. Standardize async/sync boundaries (FMEA-011)
2. Remediate expect() violations (FMEA-003)
3. Fix build SLO (FMEA-004)
4. Unify dependencies (FMEA-008)

### Medium-term (RPN < 50): Week 3-4
1. Clean up dead code (FMEA-007)
2. Split god crate (FMEA-010)
3. Add documentation (FMEA-012)

---

## Success Criteria

✅ All RPN > 150 items resolved
✅ All RPN > 50 items have action plans in progress
✅ New issues kept to RPN < 30 (low risk)
✅ No new panics introduced
✅ Build time restored to <20s

---

**FMEA Status**: Analysis Complete - Ready for Mitigation Planning
**Next Steps**: Execute Phase 1 actions from Remediation Strategy
