# Technical Debt Register - ggen Project

## Executive Summary

Comprehensive audit of ggen codebase identified **37 technical debt items** across code quality, architecture, performance, and production readiness. Using 80/20 Pareto principle, the **top 20% of issues (7 items)** account for **80% of impact**.

**Audit Date**: 2025-11-21
**Scope**: All 8 crates (ggen-core, ggen-cli, ggen-domain, ggen-utils, ggen-ai, ggen-dod, ggen-marketplace, ggen-config)
**Auditors**: code-analyzer, production-validator, performance-benchmarker, system-architect

---

## Critical Issues (STOP - Must Fix Immediately)

### 1. **Vendored OpenSSL Build Failure** (BUILD BLOCKER)
**Severity**: CRITICAL | **Effort**: 2h | **Impact**: Build unblocked + 60s reduction

- First build takes **109.6s** (730% over **15s SLO**)
- `git2 v0.20` with `vendored-openssl` causes race condition
- OpenSSL C code compilation fails
- Blocks all CI/CD pipelines

**Solution**: Remove vendored-openssl, use system OpenSSL

---

### 2. **10 Modules Over 500 Lines** (MAINTAINABILITY)
**Severity**: HIGH | **Effort**: 16h | **Impact**: Improves maintainability + build reduction

**Problem**: Files violate architecture guidelines (target: <500 lines)
1. `marketplace-v2/src/lifecycle/production.rs` - **1,385 lines**
2. `marketplace-v2/src/lifecycle/install.rs` - **1,649 lines**
3. `marketplace-v2/src/lifecycle/search.rs` - **1,370 lines**
4. `marketplace-v2/src/marketplace/registry.rs` - **1,107 lines**
5. Plus 6 more files over 600 lines

---

### 3. **Inconsistent Error Type Proliferation** (ARCHITECTURE)
**Severity**: HIGH | **Effort**: 12h | **Impact**: Unified error handling + debugging

**Problem**: Multiple incompatible error types:
- `ggen-utils::error::Error` (custom struct)
- `ggen-marketplace::error::Error` (thiserror enum)
- `ggen-ai::error::GgenAiError` (thiserror enum)
- `ggen-dod::error::DoDError` (thiserror enum)

**Solution**: Create unified `ggen-error` crate with standardized error handling

---

### 4. **First Build SLO Failure** (PERFORMANCE)
**Severity**: CRITICAL | **Effort**: 12h | **Impact**: 73-86% build time reduction

| Metric | Current | SLO | Status |
|--------|---------|-----|--------|
| First Build | **109.6s** | 15s | ❌ **+730%** |
| Incremental (warm) | 0.2s | 2s | ✅ |
| RDF Processing | <100ms | 5s | ✅ |

**Root Causes**:
1. OpenSSL source compilation (~90s)
2. SHACL dependency chain (~12s)
3. Three axum versions (~10s)
4. oxigraph RDF store (~5s)
5. Target directory bloat (19GB)

---

### 5. **59+ Dead Code Suppressions** (CODE QUALITY)
**Severity**: HIGH | **Effort**: 8h | **Impact**: Cleaner codebase + reduced compilation

- 59+ `#[allow(dead_code)]` instances
- Mix of legitimate (phantom types) and actual dead code
- Need audit to remove actual unused functions

---

### 6. **30+ Panic Calls in Production Code** (SAFETY)
**Severity**: HIGH | **Effort**: 10h | **Impact**: Production stability

- `panic!()`, `unwrap()`, `expect()` in production code
- Process crashes with no recovery
- Must replace with `Result<T,E>` error returns

---

### 7. **Three Duplicate Axum Versions** (DEPENDENCY)
**Severity**: HIGH | **Effort**: 8h | **Impact**: 10s build reduction + smaller binary

- axum v0.6.20 (via tonic@0.9.2 → opentelemetry-otlp)
- axum v0.7.9 (via ggen-marketplace direct)
- axum v0.8.6 (via tonic@0.14.2 → bollard → testcontainers)

**Solution**: Pin single version, update transitive dependencies

---

## High Priority Issues (Next 2 Weeks)

### Additional Critical Items:
- 150+ Expect() violations (documented in ANDON_SIGNAL_ENFORCEMENT.md)
- Fragmented MAPE-K implementations (3 separate implementations)
- Async/sync API boundary confusion
- Unsafe Send/Sync implementations need documentation
- ggen-core God Crate (35+ modules, should be split)
- Circular dependency risk (ggen-domain ↔ ggen-core)
- Duplicate RDF/Ontology modules (across 5 crates)
- Public API documentation gaps
- Breaking API change risk in public re-exports

---

## 80/20 Pareto Analysis - Top 7 Items

### Summary Table

| Rank | Issue | Category | Effort | Impact | Timeline |
|------|-------|----------|--------|--------|----------|
| 1 | Vendored OpenSSL Build | Performance | 2h | CRITICAL | **TODAY** |
| 2 | 10 Modules >500 lines | Maintainability | 16h | HIGH | Week 1 |
| 3 | Error Type Proliferation | Architecture | 12h | HIGH | Week 1 |
| 4 | First Build SLO Failure | Performance | 12h | CRITICAL | Week 1 |
| 5 | 59+ Dead Code Suppressions | Code Quality | 8h | HIGH | Week 1 |
| 6 | 30+ Panic Calls | Safety | 10h | HIGH | Week 2 |
| 7 | 3 Duplicate Axum Versions | Dependency | 8h | HIGH | **TODAY** |

**Total Effort**: 68 hours
**Total Impact**: 80% of technical debt resolved

---

## Remediation Roadmap

### Phase 1: IMMEDIATE (Today - This Week) - 12 hours
**Goal**: Fix blockers, restore CI/CD functionality
- [ ] Fix vendored-openssl build failure (2h)
- [ ] Unify axum versions (8h)
- [ ] Verify CI passes (2h)

### Phase 2: Week 1 - 36 hours
**Goal**: Improve maintainability and code quality
- [ ] Split 10 modules >500 lines (16h)
- [ ] Standardize error types (12h)
- [ ] Remove dead code suppressions (8h)

### Phase 3: Week 2 - 20 hours
**Goal**: Improve safety and reliability
- [ ] Replace 30+ panics with Result<T,E> (10h)
- [ ] Consolidate MAPE-K implementations (10h)

### Phase 4: Week 3-4 - Ongoing
**Goal**: Architecture improvements and documentation
- [ ] Resolve circular dependencies
- [ ] Consolidate RDF modules
- [ ] Clarify async/sync boundaries
- [ ] Add missing documentation

---

## Success Criteria

✅ All CRITICAL issues resolved
✅ Phase 1 passes all gates (cargo make check, test, lint, slo-check)
✅ CI/CD pipeline fully functional
✅ Build time <20s (Phase 1) → <15s (Phase 2)
✅ SLOs restored across all metrics
✅ Zero production panics
✅ Unified error types across crates

---

**Last Updated**: 2025-11-21
**Next Review**: Post-Phase-1 (1 week)
**Status**: Technical debt audit complete, remediation plan ready for Phase 1 execution
