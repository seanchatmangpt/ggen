# ggen 25-Item Action Plan - Manufacturing-Grade Quality Framework

## Executive Summary

This action plan consolidates findings from FMEA (Failure Mode and Effects Analysis), TRIZ (Theory of Inventive Problem Solving), Andon Signals, and Gemba Walk observations into a prioritized 25-item roadmap for improving code quality, safety, and performance.

**Total Estimated Effort**: 244 hours (6-week execution)
**Critical Path Items**: 5 items (RPN > 150) = 38 hours
**High Priority Items**: 5 items (RPN 50-150) = 50 hours
**Innovation Items**: 5 items from TRIZ principles = 30 hours
**Andon Enhancements**: 5 items for quality gates = 19 hours
**Gemba Remediation**: 5 items addressing current-state waste = 32 hours

---

## PHASE 1: CRITICAL (RPN > 150) - IMMEDIATE ACTION REQUIRED

These items block productivity and pose production safety risks. Execute in order within 1 week.

### Item 1: [CRITICAL] Replace 30+ Panic Calls with Result<T,E>
- **Source**: FMEA-002 (RPN 560 - HIGHEST RISK)
- **Impact**: Production crashes prevented
- **Effort**: 10 hours
- **Timeline**: Days 1-2
- **Success Criteria**:
  - All `panic!()` calls in library code replaced with `Result<T,E>`
  - All `unwrap()` in error paths replaced with `?` operator
  - All `expect()` replaced with `map_err()` + context
  - `cargo make check` passes with no panics remaining
  - Integration tests validate error handling paths
- **Dependency**: None
- **Owner**: Core team (crates/ggen-core, crates/ggen-utils)
- **Verification**: `cargo make lint` + `cargo make test`

### Item 2: [CRITICAL] Fix Vendored OpenSSL Build Bottleneck
- **Source**: FMEA-001 (RPN 180), TRIZ #35 Parameter Changes
- **Impact**: Build time 109.6s → ~50s (54% reduction)
- **Effort**: 2 hours
- **Timeline**: Day 1 (morning)
- **Success Criteria**:
  - Remove `vendored-openssl` feature from git2 dependency
  - Set `OPENSSL_DIR` environment variable in CI/CD
  - First build completes in <60s
  - `cargo make check` verifies build speedup
- **Dependency**: None (blocks other build optimizations)
- **Owner**: Build/DevOps team
- **Verification**: `time cargo make check` shows <60s

### Item 3: [CRITICAL] Create Unified ggen-error Crate
- **Source**: FMEA-006 (RPN 180), TRIZ #4 Asymmetry
- **Impact**: Consistent error handling across all crates
- **Effort**: 12 hours
- **Timeline**: Days 2-3
- **Success Criteria**:
  - New `crates/ggen-error/src/lib.rs` with unified error enum
  - All error types use `#[derive(Error, Debug)]` from thiserror
  - Implement `From<T>` traits for automatic conversions
  - All crates depend on `ggen-error`
  - Remove duplicate error definitions from 4 crates
  - `cargo make check` passes for all crates
  - Error propagation uses `?` operator consistently
- **Dependency**: Item 1 (panic replacement provides context)
- **Owner**: Architecture team
- **Verification**: Type checking shows single error type used everywhere

### Item 4: [CRITICAL] Consolidate Fragmented MAPE-K Implementations
- **Source**: FMEA-009 (RPN 162)
- **Impact**: Single authoritative autonomic loop implementation
- **Effort**: 8 hours
- **Timeline**: Days 3-4
- **Success Criteria**:
  - Identify 3 MAPE-K implementations (Monitor, Analyze, Plan, Execute, Knowledge)
  - Create `crates/ggen-domain/src/mape_k.rs` as authoritative implementation
  - Remove duplicate implementations
  - All autonomic components delegate to single implementation
  - `cargo make test` validates behavior equivalence
  - Documentation clearly identifies the MAPE-K pattern
- **Dependency**: Item 3 (unified error type needed for MAPE-K)
- **Owner**: Domain architecture team
- **Verification**: `cargo tree | grep -i mape` shows single dependency

### Item 5: [CRITICAL] Standardize Async/Sync API Boundary
- **Source**: FMEA-011 (RPN 160)
- **Impact**: Clear, consistent API design
- **Effort**: 6 hours
- **Timeline**: Day 4
- **Success Criteria**:
  - Document async-first philosophy (all primary APIs are async)
  - Add blocking wrapper variants for sync callers
  - Create `_sync()` and `_blocking()` variants where needed
  - Update API documentation
  - `cargo make test` validates all variants work correctly
  - Clippy linting passes with consistent async patterns
- **Dependency**: None
- **Owner**: API design team
- **Verification**: No clippy warnings about async/sync confusion

---

## PHASE 2: HIGH PRIORITY (RPN 50-150) - WEEK 2

High-impact issues that affect code quality and maintainability.

### Item 6: [HIGH] Remediate 150+ Expect() Violations Systematically
- **Source**: FMEA-003 (RPN 60)
- **Impact**: All expect() calls properly documented or replaced
- **Effort**: 16 hours (4 hours per crate)
- **Timeline**: Days 5-8
- **Success Criteria**:
  - For test code: Add `#[allow(clippy::expect_used)]` with justification comment
  - For production code: Replace with proper error handling
  - Create register: `docs/EXPECT_VIOLATIONS_REGISTER.md`
  - Document which expect() calls are acceptable and why
  - Pre-push hook Gate 2.5 validates all violations
  - `cargo make test` passes
- **Crates**: ggen-core, ggen-cli, ggen-marketplace-v2, ggen-ai
- **Owner**: Each crate owner
- **Verification**: `git grep -n "\.expect(" | wc -l` shows remediated count

### Item 7: [HIGH] Optimize Build Performance SLO (109.6s → 15s Target)
- **Source**: FMEA-004 (RPN 70), Multiple TRIZ principles
- **Impact**: Build time restored to 15s SLO
- **Effort**: 18 hours (phased approach)
- **Timeline**: Days 5-12 (spans multiple phases)
- **Success Criteria**:
  - Item 2: OpenSSL fix = 60s savings
  - Item 8: Axum unification = 10s savings
  - Add sccache to CI = 30-40% incremental speedup
  - Feature-gate heavy dependencies = 5s savings
  - Total build time <15s
  - `cargo make slo-check` verifies target met
- **Dependency**: Item 2 (must complete first)
- **Owner**: Build/Performance team
- **Verification**: `time cargo build --release` shows <15s

### Item 8: [HIGH] Unify Axum Version Conflicts
- **Source**: FMEA-008 (RPN 50)
- **Impact**: Single version (0.7.9), smaller binary, faster builds
- **Effort**: 4 hours
- **Timeline**: Day 5
- **Success Criteria**:
  - Pin single axum version in workspace `Cargo.toml`
  - Update all dependencies to compatible versions
  - Remove duplicate versions from dependency tree
  - `cargo tree | grep axum` shows single entry
  - `cargo make test` passes
- **Dependency**: None
- **Owner**: Dependency management team
- **Verification**: Binary size reduction, faster incremental builds

### Item 9: [HIGH] Clean Up Dead Code and Suppressions
- **Source**: FMEA-007 (RPN 48)
- **Impact**: Clearer codebase, easier navigation
- **Effort**: 8 hours
- **Timeline**: Days 6-7
- **Success Criteria**:
  - Audit 59+ `#[allow(dead_code)]` suppressions
  - Remove actual dead code (delete it completely)
  - Document intentional suppressions (PhantomData, etc.)
  - Create `docs/INTENTIONAL_SUPPRESSIONS.md`
  - `cargo make lint` has zero dead code warnings
- **Dependency**: None
- **Owner**: Code quality team
- **Verification**: `cargo clippy` shows no dead code warnings

### Item 10: [HIGH] Begin God Crate Modularization (ggen-core)
- **Source**: FMEA-010 (RPN 100), TRIZ #1 Segmentation
- **Impact**: Clearer architecture, faster incremental builds
- **Effort**: 40 hours (Phase 1 of multi-phase effort)
- **Timeline**: Days 8-22 (spans weeks 2-3)
- **Success Criteria**:
  - Create modularization plan: ggen-core → 5 focused crates
  - Proposed crates: ggen-template, ggen-graph, ggen-registry, ggen-lifecycle, ggen-security
  - Complete Phase 1: Extract ggen-template crate (8h)
  - Verify single-file changes rebuild in <2s (vs current 22.4s)
  - `cargo make test` passes for all new crates
  - Update workspace dependencies
- **Dependency**: Items 3, 4, 5 (need unified error, MAPE-K, async patterns)
- **Owner**: Architecture team
- **Verification**: Incremental build time <2s for single-file changes

---

## PHASE 3: INNOVATION (TRIZ PRINCIPLES) - WEEK 3

Implement elegant solutions from TRIZ inventive problem-solving framework.

### Item 11: [TRIZ] Implement System OpenSSL (Principle #35: Parameter Changes)
- **Source**: TRIZ Analysis - Build Speed vs Correctness
- **Impact**: Deterministic builds using system-provided OpenSSL
- **Effort**: 2 hours
- **Timeline**: Day 15
- **Success Criteria**:
  - Remove `vendored-openssl` feature flag
  - Use system OpenSSL (assume pre-installed)
  - Set `OPENSSL_DIR` in CI pipeline
  - Build verifies system OpenSSL availability
  - Performance gain: 60s → integrated in Item 2
- **Dependency**: Item 2 (overlaps)
- **Owner**: DevOps team
- **Verification**: Build completes without C compilation

### Item 12: [TRIZ] Design Unified Error Type with Variants (Principle #4: Asymmetry)
- **Source**: TRIZ Analysis - Error Handling vs Consistency
- **Impact**: Unified external boundary, flexible internal structure
- **Effort**: 4 hours
- **Timeline**: Day 16
- **Success Criteria**:
  - Document asymmetric design: single `GgenError` external type
  - Internal: rich variant structure for domain errors
  - Domain variants: RdfError, TemplateError, LifecycleError, RegistryError, SecurityError
  - Implement auto-conversion via macros
  - Type-safe error handling throughout
- **Dependency**: Item 3 (builds on unified error crate)
- **Owner**: Architecture team
- **Verification**: All crates use single error type with domain variants

### Item 13: [TRIZ] Enable Compiler-Enforced Safety (Principle #25: Self-Service)
- **Source**: TRIZ Analysis - Production Safety vs Development Speed
- **Impact**: Type system makes panics impossible, zero runtime overhead
- **Effort**: 2 hours
- **Timeline**: Day 17
- **Success Criteria**:
  - Leverage Rust's Result<T,E> type system
  - No unchecked conversions or panics
  - Every fallible operation returns Result
  - Type checker prevents invalid states
  - Zero performance overhead from safety checks
- **Dependency**: Item 1 (panic replacement)
- **Owner**: Core team
- **Verification**: Type checking shows all error paths typed

### Item 14: [TRIZ] Begin Code Segmentation (Principle #1: Segmentation)
- **Source**: TRIZ Analysis - Code Organization vs Compilation Speed
- **Impact**: Small, independent crates, fast incremental builds
- **Effort**: 16 hours (Phase 1 of Item 10)
- **Timeline**: Days 18-21
- **Success Criteria**:
  - Extract ggen-template as first independent crate
  - Clear dependency boundaries
  - No circular dependencies
  - Separate compilation (faster rebuilds)
  - Test coverage maintained
- **Dependency**: Item 10 (core modularization plan)
- **Owner**: Architecture team
- **Verification**: `cargo tree` shows clear hierarchy

### Item 15: [TRIZ] Feature-Gate Heavy Dependencies (Principle #36: Phase Transition)
- **Source**: TRIZ Analysis - Feature Coverage vs Maintenance Burden
- **Impact**: Users only pay for features they use
- **Effort**: 6 hours
- **Timeline**: Days 22-23
- **Success Criteria**:
  - Identify heavy dependencies: oxigraph, tera, serde_yaml, etc.
  - Add Cargo feature flags for optional dependencies
  - Document feature combinations
  - Users can opt-out of unused features
  - Build size reduction for minimal configs
- **Dependency**: None
- **Owner**: Dependency management team
- **Verification**: Minimal build (no optional features) <10MB

---

## PHASE 4: QUALITY GATES (ANDON ENHANCEMENTS) - WEEK 3

Enhance Andon signal system to catch more issues earlier.

### Item 16: [ANDON] Add Panic Detection Gate (Gate 2.7)
- **Source**: Andon Signal Summary - Enhancement 1
- **Impact**: Catch all panic!() calls before push
- **Effort**: 2 hours
- **Timeline**: Day 24
- **Success Criteria**:
  - Add pre-push Gate 2.7: panic/unwrap detection
  - `grep -n "panic!\|unwrap()" crates/ggen-*/src/lib.rs`
  - Block any unhandled panics in library code
  - Allow only with documented `#[allow(panic)]`
- **Dependency**: None
- **Owner**: Quality/DevOps team
- **Verification**: Pre-push hook catches all panics

### Item 17: [ANDON] Add Documentation Coverage Check (Gate 3.5)
- **Source**: Andon Signal Summary - Enhancement 2
- **Impact**: All public APIs documented
- **Effort**: 3 hours
- **Timeline**: Day 25
- **Success Criteria**:
  - Enable `#![warn(missing_docs)]` per crate
  - Enforce documentation on all public items
  - Integration test validates missing_docs lint
  - Gate 3.5 validates documentation completeness
- **Dependency**: None
- **Owner**: Documentation team
- **Verification**: `cargo doc --no-deps` shows 100% coverage

### Item 18: [ANDON] Add Performance SLO Monitoring Gate (Gate 4.5)
- **Source**: Andon Signal Summary - Enhancement 3
- **Impact**: Build time SLOs monitored automatically
- **Effort**: 4 hours
- **Timeline**: Days 25-26
- **Success Criteria**:
  - Implement `cargo make slo-check` command
  - Monitor: build time <15s, RDF processing <5s, memory <100MB
  - Gate blocks push if SLOs violated
  - Dashboard shows current metrics
- **Dependency**: Item 2, 7 (need to optimize for SLO compliance)
- **Owner**: Performance team
- **Verification**: `cargo make slo-check` passes

### Item 19: [ANDON] Implement Security Scanning (Gate 5.5)
- **Source**: Andon Signal Summary - Enhancement 4
- **Impact**: Comprehensive security checks
- **Effort**: 4 hours
- **Timeline**: Days 26-27
- **Success Criteria**:
  - Use `cargo audit` for known vulnerabilities
  - Add `cargo deny` for supply chain validation
  - Add semver-checks for breaking API changes
  - Gate 5.5 runs comprehensive security suite
  - All advisories addressed
- **Dependency**: None
- **Owner**: Security team
- **Verification**: `cargo make audit` shows no advisories

### Item 20: [ANDON] Create Real-Time Quality Dashboard
- **Source**: Andon Signal Summary - Phase 3
- **Impact**: Visual quality metrics and signal status
- **Effort**: 6 hours
- **Timeline**: Days 27-28
- **Success Criteria**:
  - Dashboard shows: build time, test coverage, SLO status
  - Andon signals visualized: red (critical), yellow (high), green (clear)
  - Metrics updated after each commit
  - Team can see quality trends
  - Gate performance metrics logged
- **Dependency**: Items 16-19 (gate implementations)
- **Owner**: DevOps/Metrics team
- **Verification**: Dashboard accessible and updating

---

## PHASE 5: GEMBA REMEDIATION - WEEK 4

Address current-state waste and maintainability issues identified through Gemba walk.

### Item 21: [GEMBA] Modularize marketplace-v2/lifecycle/install.rs (1,649 lines)
- **Source**: Gemba Walk - Code Organization issue
- **Impact**: Reduced file size, clearer module structure
- **Effort**: 6 hours
- **Timeline**: Days 29-31
- **Success Criteria**:
  - Split install.rs into 3-4 focused modules
  - Module size: <400 lines each
  - Clear module responsibilities
  - Test coverage maintained
  - `cargo make lint` passes
  - Navigation time reduced 50%
- **Dependency**: Item 3, 4 (unified error, MAPE-K)
- **Owner**: Marketplace team
- **Verification**: File line counts all <400 lines

### Item 22: [GEMBA] Modularize marketplace-v2/lifecycle/production.rs (1,385 lines)
- **Source**: Gemba Walk - Code Organization issue
- **Impact**: Reduced file size, clearer module structure
- **Effort**: 6 hours
- **Timeline**: Days 31-32
- **Success Criteria**:
  - Split production.rs into 3 focused modules
  - Module size: <400 lines each
  - Test coverage maintained
  - `cargo make test` passes
- **Dependency**: Item 21 (same approach)
- **Owner**: Marketplace team
- **Verification**: File line counts all <400 lines

### Item 23: [GEMBA] Modularize marketplace-v2/lifecycle/search.rs (1,370 lines)
- **Source**: Gemba Walk - Code Organization issue
- **Impact**: Reduced file size, clearer module structure
- **Effort**: 6 hours
- **Timeline**: Days 32-33
- **Success Criteria**:
  - Split search.rs into 3 focused modules
  - Module size: <400 lines each
  - Test coverage maintained
  - All tests pass
- **Dependency**: Item 21 (same approach)
- **Owner**: Marketplace team
- **Verification**: File line counts all <400 lines

### Item 24: [GEMBA] Consolidate MAPE-K Implementations
- **Source**: Gemba Walk - Fragmentation issue (overlaps with Item 4)
- **Impact**: Single authoritative MAPE-K pattern
- **Effort**: 8 hours
- **Timeline**: Days 33-35
- **Success Criteria**:
  - All autonomic components use single MAPE-K
  - Clear Monitor → Analyze → Plan → Execute → Knowledge flow
  - Type-safe state transitions
  - Integration tests verify behavior
- **Dependency**: Item 4 (coordinate, not duplicate)
- **Owner**: Domain team
- **Verification**: Single MAPE-K implementation used everywhere

### Item 25: [CONTINUOUS] Establish Kaizen 2-Week Review Cycle
- **Source**: Gemba Walk - Continuous Improvement philosophy
- **Impact**: Systematic, ongoing technical debt management
- **Effort**: 2 hours (initial setup)
- **Timeline**: Day 36 (ongoing every 2 weeks)
- **Success Criteria**:
  - Bi-weekly Kaizen reviews scheduled
  - Review agenda: Andon signals, new technical debt, SLO compliance
  - Document findings in KAIZEN_FINDINGS.md
  - Create new action items with RPN scoring
  - Team discusses root causes (5 Whys)
  - Implementation of top items tracked
  - Process becomes sustainable
- **Dependency**: All prior items (foundation for ongoing improvement)
- **Owner**: Quality/Architecture leadership
- **Verification**: Kaizen meetings scheduled, findings documented

---

## Summary Table: All 25 Items

| # | Priority | Item | RPN/Source | Effort | Week | Owner |
|---|----------|------|-----------|--------|------|-------|
| 1 | CRITICAL | Replace 30+ Panics | FMEA-002 (560) | 10h | W1 | Core |
| 2 | CRITICAL | Fix OpenSSL Build | FMEA-001 (180) | 2h | W1 | DevOps |
| 3 | CRITICAL | Unified Error Crate | FMEA-006 (180) | 12h | W1 | Architecture |
| 4 | CRITICAL | Consolidate MAPE-K | FMEA-009 (162) | 8h | W1 | Domain |
| 5 | CRITICAL | Async/Sync Boundary | FMEA-011 (160) | 6h | W1 | API |
| 6 | HIGH | Expect() Remediation | FMEA-003 (60) | 16h | W2 | All Teams |
| 7 | HIGH | Build SLO Optimization | FMEA-004 (70) | 18h | W2-3 | Performance |
| 8 | HIGH | Unify Axum Version | FMEA-008 (50) | 4h | W2 | Dependencies |
| 9 | HIGH | Dead Code Cleanup | FMEA-007 (48) | 8h | W2 | Quality |
| 10 | HIGH | Modularize ggen-core | FMEA-010 (100) | 40h | W2-3 | Architecture |
| 11 | TRIZ | System OpenSSL | #35 Parameter | 2h | W3 | DevOps |
| 12 | TRIZ | Error Asymmetry | #4 Asymmetry | 4h | W3 | Architecture |
| 13 | TRIZ | Compiler Safety | #25 Self-Service | 2h | W3 | Core |
| 14 | TRIZ | Code Segmentation | #1 Segmentation | 16h | W3 | Architecture |
| 15 | TRIZ | Feature Gates | #36 Phase Transition | 6h | W3 | Dependencies |
| 16 | ANDON | Panic Detection | Gate 2.7 | 2h | W4 | Quality |
| 17 | ANDON | Doc Coverage | Gate 3.5 | 3h | W4 | Docs |
| 18 | ANDON | SLO Monitoring | Gate 4.5 | 4h | W4 | Performance |
| 19 | ANDON | Security Scanning | Gate 5.5 | 4h | W4 | Security |
| 20 | ANDON | Quality Dashboard | Phase 3 | 6h | W4 | DevOps |
| 21 | GEMBA | Modularize install.rs | 1,649 lines | 6h | W4 | Marketplace |
| 22 | GEMBA | Modularize production.rs | 1,385 lines | 6h | W4 | Marketplace |
| 23 | GEMBA | Modularize search.rs | 1,370 lines | 6h | W4 | Marketplace |
| 24 | GEMBA | Consolidate MAPE-K | Fragmentation | 8h | W4 | Domain |
| 25 | CONTINUOUS | Kaizen 2-Week Cycle | Continuous | 2h + | W4+ | Leadership |

---

## Execution Timeline

### Week 1: CRITICAL (38 hours)
- Days 1-4: Fix panics, OpenSSL, create unified error, consolidate MAPE-K, async/sync
- **Gating Criteria**: All CRITICAL items must complete before Week 2 begins
- **Verification**: `cargo make ci` passes, zero panics, single error type in use

### Week 2: HIGH Priority & Begins TRIZ (50 hours)
- Days 5-8: Expect() remediation, build optimization begins, axum unification, dead code cleanup
- Days 9-14: Begin god crate modularization, start TRIZ implementations
- **Gating Criteria**: All HIGH items completed or in-progress, SLO progress tracked

### Week 3: Innovation & Continuation (30 hours + 40 hours from Item 10)
- Days 15-23: Complete TRIZ implementations, continue modularization
- Days 24-28: Start Andon enhancements, begin Gemba remediation
- **Gating Criteria**: TRIZ principles proven, modularization showing <2s incremental builds

### Week 4: Quality & Completion (19 hours + 32 hours from Items 21-24)
- Days 29-36: Complete Andon gates, complete Gemba remediation, establish Kaizen cycle
- **Final Verification**: All 25 items completed or documented with clear completion path
- **Gating Criteria**: Production ready, zero CRITICAL signals, SLOs met, quality dashboard operational

---

## Success Metrics (Definition of Done)

### Critical Success Factors:
- ✅ All CRITICAL items (1-5) completed within Week 1
- ✅ Build time restored to 15s SLO (Item 7)
- ✅ Zero panic calls in production code (Item 1)
- ✅ Single unified error type in use (Item 3)
- ✅ All Andon signal gates operational (Items 16-20)

### Quality Metrics:
- ✅ `cargo make ci` passes cleanly
- ✅ `cargo make slo-check` shows all SLOs met
- ✅ `cargo make audit` shows no security advisories
- ✅ Code coverage maintained >80%
- ✅ Test pass rate 100%
- ✅ Zero compiler warnings
- ✅ Zero clippy warnings
- ✅ Zero pre-push hook signals

### Performance Metrics:
- ✅ First build: <15s (target)
- ✅ Incremental build: <2s (target)
- ✅ RDF processing: <5s for 1k+ triples (target)
- ✅ Memory usage: <100MB (target)
- ✅ CLI scaffolding: <3s end-to-end (target)

### Organizational Metrics:
- ✅ Kaizen cycle established (bi-weekly reviews)
- ✅ Technical debt register maintained
- ✅ All team members trained on Andon/TRIZ/Gemba principles
- ✅ Quality culture shift visible in code reviews and discussions

---

## Document References

This action plan consolidates findings from:
1. **docs/FMEA_ANALYSIS.md** - Risk Priority Numbers and mitigation strategies
2. **docs/TRIZ_INNOVATION.md** - Inventive principles for elegant solutions
3. **docs/ANDON_SIGNAL_SUMMARY.md** - Quality gate framework and enhancements
4. **docs/GEMBA_WALK_REPORT.md** - Current-state observations and waste analysis
5. **docs/TECHNICAL_DEBT_REGISTER.md** - Comprehensive debt inventory

---

**Status**: Ready for execution
**Created**: 2025-11-21
**Next Step**: Assign owners, schedule teams, begin Week 1 critical items
**Review Cycle**: Bi-weekly Kaizen reviews (Item 25) to track progress and identify new opportunities
