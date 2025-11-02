# v2.0.0 Migration Timeline

**Method**: Critical Path Analysis + 80/20 Principle
**Date**: 2025-11-02
**Baseline**: Current state (72/100 scorecard)

---

## Timeline Overview

| Phase | Modules | Hours | Coverage | Value | Cumulative |
|-------|---------|-------|----------|-------|------------|
| **P0** | Utils, Graph fix | 2-3 | 67% | 80% | 3h |
| **P1** | Hook, CI | 2 | 89% | 95% | 5h |
| **P2** | Audit | 1 | 100% | 100% | 6h |

---

## Phase 1 (P0) - Critical Path [2-3 hours]

**Goal**: Production release with core features
**Modules**: Utils + Graph fix
**Coverage**: 67% (6/9 modules)
**Value**: 80% of user workflows

### Hour 1: Domain Layer Fixes

```
00:00-00:05 ✅ Fix GraphCmd → GraphArgs typo
├─ File: cli/src/cmds/mod.rs:40
├─ Change: 1 line
├─ Build: cargo build (validate)
└─ Time: 5 minutes

00:05-00:35 ✅ Fix domain/utils/doctor.rs
├─ Task: Add type exports to domain/utils/mod.rs
├─ Pattern: Copy from domain/marketplace/mod.rs
├─ Types:
│  - SystemCheck
│  - SystemChecker
│  - CheckStatus
│  - CheckSummary
│  - SystemCheckResult
│  - EnvironmentInfo
├─ Build: cargo build (validate)
└─ Time: 30 minutes

00:35-01:00 ✅ Fix domain/utils/env.rs
├─ Task: Add type exports to domain/utils/mod.rs
├─ Pattern: Same as doctor.rs
├─ Types:
│  - EnvironmentManager
│  - GgenEnvironment
│  - DefaultEnvironmentManager
├─ Build: cargo build (validate)
└─ Time: 25 minutes

Checkpoint: Domain layer complete, 0 compilation errors
```

### Hour 2: CLI Wrapper

```
01:00-01:30 ✅ Add cmds/utils.rs CLI wrapper
├─ Template: Copy from cmds/marketplace.rs
├─ Structure:
│  - UtilsArgs struct
│  - UtilsCommands enum
│  - Verb: Doctor
│  - Verb: Env
│  - run() implementation
├─ Build: cargo build --release
└─ Time: 30 minutes

01:30-01:45 ✅ Build validation
├─ Command: cargo build --release
├─ Success criteria:
│  - 0 compilation errors
│  - Binary size 8-10 MB
│  - Build time <60 seconds
└─ Time: 15 minutes

01:45-02:00 ✅ Library test validation
├─ Command: cargo test --lib
├─ Success criteria:
│  - 127+ tests passing
│  - 0 failures
│  - Execution time <30 seconds
└─ Time: 15 minutes

Checkpoint: Clean build, all tests passing
```

### Hour 3: Runtime Validation

```
02:00-02:30 ✅ Command smoke tests
├─ ggen template list
├─ ggen marketplace search rust
├─ ggen ai generate "create API"
├─ ggen project new test-proj
├─ ggen graph query "SELECT..."
├─ ggen utils doctor
├─ ggen utils env
└─ Time: 30 minutes

02:30-02:45 ✅ Performance benchmarks
├─ cargo bench --bench template_generation
├─ cargo bench --bench runtime_overhead
├─ Validate: All SLOs met
└─ Time: 15 minutes

02:45-03:00 ✅ Documentation
├─ Update RELEASE_NOTES.md
├─ Update README.md (features)
├─ Git commit + tag v2.0.0
└─ Time: 15 minutes

Checkpoint: v2.0.0 PRODUCTION READY 🎉
```

### Phase 1 Milestones

- **30 min**: Graph typo fixed, building
- **1 hour**: Domain layer errors resolved
- **2 hours**: Clean build achieved
- **3 hours**: Production release ready

---

## Phase 2 (P1) - Extended Features [+2 hours]

**Goal**: Developer tooling and automation
**Modules**: Hook + CI
**Coverage**: 89% (8/9 modules)
**Value**: 95% of workflows

### Hour 4: Hook Commands

```
03:00-03:30 ✅ Add cmds/hook.rs CLI wrapper
├─ Template: Copy from cmds/template.rs
├─ Structure:
│  - HookArgs struct
│  - HookCommands enum
│  - Verbs: Create, List, Remove, Monitor (4 total)
│  - run() implementation
├─ Build: cargo build
└─ Time: 30 minutes

03:30-03:45 ✅ Hook verb implementations
├─ Create: Map to domain::hook::create
├─ List: Map to domain::hook::list
├─ Remove: Map to domain::hook::remove
├─ Monitor: Map to domain::hook::monitor
└─ Time: 15 minutes

03:45-04:00 ✅ Hook testing
├─ ggen hook list
├─ ggen hook create pre-commit
├─ ggen hook monitor
├─ ggen hook remove {id}
└─ Time: 15 minutes

Checkpoint: Hook commands working
```

### Hour 5: CI Commands

```
04:00-04:30 ✅ Add cmds/ci.rs CLI wrapper
├─ Template: Copy from cmds/ai.rs (single verb)
├─ Structure:
│  - CiArgs struct
│  - CiCommands enum
│  - Verb: Workflow (1 total)
│  - run() implementation
├─ Build: cargo build
└─ Time: 30 minutes

04:30-04:45 ✅ CI verb implementation
├─ Workflow: Map to domain::ci::workflow
├─ Args: workflow file path
└─ Time: 15 minutes

04:45-05:00 ✅ CI testing
├─ ggen ci workflow .github/workflows/test.yml
├─ Validate: Workflow runs locally
└─ Time: 15 minutes

Checkpoint: CI commands working
```

### Phase 2 Milestones

- **4 hours**: Hook module complete (4 verbs)
- **5 hours**: CI module complete (1 verb)
- **Coverage**: 89% (8/9 modules)

---

## Phase 3 (P2) - Complete Coverage [+1 hour]

**Goal**: Security and auditing
**Modules**: Audit
**Coverage**: 100% (9/9 modules)
**Value**: 100% of features

### Hour 6: Audit Commands

```
05:00-05:30 ✅ Add cmds/audit.rs CLI wrapper
├─ Template: Copy from cmds/ai.rs
├─ Structure:
│  - AuditArgs struct
│  - AuditCommands enum
│  - Verb: Security (1 total)
│  - run() implementation
├─ Build: cargo build
└─ Time: 30 minutes

05:30-05:45 ✅ Audit verb implementation
├─ Security: Map to domain::audit::security
├─ Args: scan path
└─ Time: 15 minutes

05:45-06:00 ✅ Audit testing
├─ ggen audit security ./
├─ Validate: Security scan runs
├─ Update documentation
└─ Time: 15 minutes

Checkpoint: v2.0.0 100% COMPLETE 🎉
```

### Phase 3 Milestones

- **6 hours**: All 9 modules complete
- **Coverage**: 100% (9/9 modules)
- **Value**: 100% of features

---

## Critical Path Dependencies

```
Graph Fix ─────────────────────────────────┐
  (5 min)                                   │
                                            ├──→ Build OK
Utils Domain ──────────────────────────────┤    (Hour 1)
  (1 hour)                                  │
                                            │
Utils CLI ─────────────────────────────────┤
  (30 min)                                  │
                                            ├──→ Tests OK
Library Tests ─────────────────────────────┤    (Hour 2)
  (15 min)                                  │
                                            │
Runtime Tests ─────────────────────────────┼──→ v2.0.0 READY
  (30 min)                                  │    (Hour 3)
                                            │
Hook CLI ──────────────────────────────────┤
  (1 hour)                                  ├──→ Extended Release
                                            │    (Hour 5)
CI CLI ────────────────────────────────────┤
  (1 hour)                                  │
                                            │
Audit CLI ─────────────────────────────────┼──→ Complete Release
  (1 hour)                                       (Hour 6)
```

**No parallelization opportunities**: Each task depends on previous build success.

---

## Buffer Time Analysis

### Optimistic Timeline (Best Case)

| Phase | Est. Hours | Buffer | Total |
|-------|------------|--------|-------|
| P0 | 2.0 | 0.5 | 2.5 |
| P1 | 1.5 | 0.5 | 2.0 |
| P2 | 0.75 | 0.25 | 1.0 |
| **Total** | **4.25** | **1.25** | **5.5** |

### Realistic Timeline (Expected Case)

| Phase | Est. Hours | Buffer | Total |
|-------|------------|--------|-------|
| P0 | 2.5 | 0.5 | 3.0 |
| P1 | 2.0 | 0.5 | 2.5 |
| P2 | 1.0 | 0.25 | 1.25 |
| **Total** | **5.5** | **1.25** | **6.75** |

### Pessimistic Timeline (Worst Case)

| Phase | Est. Hours | Buffer | Total |
|-------|------------|--------|-------|
| P0 | 3.5 | 1.0 | 4.5 |
| P1 | 2.5 | 1.0 | 3.5 |
| P2 | 1.5 | 0.5 | 2.0 |
| **Total** | **7.5** | **2.5** | **10.0** |

**Confidence Intervals**:
- 50% chance: 5.5-6.75 hours
- 90% chance: 4.5-10 hours
- 95% chance: <12 hours

---

## Velocity Assumptions

**Based on**:
- Prior v2 migration work (5 modules completed)
- Chicago TDD validation (10/11 E2E tests passing)
- Reference implementations (marketplace, template patterns)

**Assumptions**:
1. **Copy-paste velocity**: 15-20 minutes per CLI wrapper
2. **Domain fixes**: 30-60 minutes per module (with reference)
3. **Build time**: 5-15 seconds (incremental)
4. **Test time**: 15-30 seconds (library tests)
5. **Debug time**: 10-20% buffer for unexpected issues

**Risks**:
- Hidden type dependencies (mitigated by compiler guidance)
- Test failures (mitigated by E2E baseline at 91%)
- Build cache invalidation (mitigated by incremental builds)

---

## Progress Tracking

### Hour-by-Hour Checklist

**Hour 1**: Domain Layer
- [ ] 00:05 - Graph typo fixed
- [ ] 00:35 - doctor.rs exports added
- [ ] 01:00 - env.rs exports added
- [ ] Milestone: 0 compilation errors

**Hour 2**: Build Validation
- [ ] 01:30 - utils.rs CLI wrapper added
- [ ] 01:45 - Release build succeeds
- [ ] 02:00 - Library tests pass
- [ ] Milestone: Clean build

**Hour 3**: Runtime Validation
- [ ] 02:30 - All commands smoke tested
- [ ] 02:45 - Benchmarks run
- [ ] 03:00 - Documentation updated
- [ ] Milestone: v2.0.0 READY

**Hour 4**: Hook Module
- [ ] 03:30 - hook.rs wrapper added
- [ ] 03:45 - All 4 verbs implemented
- [ ] 04:00 - Hook tests pass
- [ ] Milestone: Hook commands working

**Hour 5**: CI Module
- [ ] 04:30 - ci.rs wrapper added
- [ ] 04:45 - Workflow verb implemented
- [ ] 05:00 - CI tests pass
- [ ] Milestone: 89% coverage

**Hour 6**: Audit Module
- [ ] 05:30 - audit.rs wrapper added
- [ ] 05:45 - Security verb implemented
- [ ] 06:00 - Audit tests pass
- [ ] Milestone: 100% coverage

---

## Decision Points

### After Hour 1: Domain Layer Complete

**Decision**: Continue to Hour 2 or stop?

**Criteria**:
- ✅ 0 compilation errors → Continue
- ❌ >2 errors remaining → Debug or rollback

### After Hour 2: Build Validation Complete

**Decision**: Continue to Hour 3 or ship?

**Criteria**:
- ✅ Clean build + tests passing → Continue
- ⚠️ Tests 80-90% → Ship now, fix in patch
- ❌ Tests <80% → Debug before ship

### After Hour 3: v2.0.0 Ready

**Decision**: Ship now or continue to Phase 2?

**Options**:
1. **Ship v2.0.0** (67% coverage, 80% value)
   - Fastest to production
   - Lowest risk
   - Can add features in v2.1.0

2. **Continue to Phase 2** (+2 hours, 89% coverage)
   - Better developer experience
   - More complete release
   - Higher risk of delays

**Recommendation**: Ship v2.0.0 after Phase 1, add P2/P3 in v2.1.0

---

## Rollback Triggers

**Stop work and rollback if**:
- More than 4 hours spent on Phase 1
- More than 3 new critical bugs discovered
- Build fails after 10+ attempts
- E2E test pass rate drops below 80%

**Rollback time**: 15 minutes (git revert + rebuild)

---

## Success Metrics

### Phase 1 (P0) Success

- [ ] Timeline: ≤3 hours
- [ ] Build: 0 errors
- [ ] Tests: ≥90% pass rate
- [ ] Commands: 6/9 working (67%)
- [ ] Performance: SLOs met

### Phase 2 (P1) Success

- [ ] Timeline: ≤5 hours cumulative
- [ ] Commands: 8/9 working (89%)
- [ ] No regressions in Phase 1 features

### Phase 3 (P2) Success

- [ ] Timeline: ≤6 hours cumulative
- [ ] Commands: 9/9 working (100%)
- [ ] Documentation complete

---

## Recommended Execution

### Option A: Fast to Production (3 hours)

```
Execute Phase 1 → Ship v2.0.0
Timeline: 2-3 hours
Coverage: 67%
Value: 80%
Risk: LOW
```

**Best for**: Immediate production release

---

### Option B: Extended Release (5 hours)

```
Execute Phase 1 + Phase 2 → Ship v2.0.0
Timeline: 4-5 hours
Coverage: 89%
Value: 95%
Risk: MEDIUM
```

**Best for**: More complete developer experience

---

### Option C: Complete Release (6 hours)

```
Execute All Phases → Ship v2.0.0
Timeline: 5-6 hours
Coverage: 100%
Value: 100%
Risk: MEDIUM-HIGH
```

**Best for**: Feature-complete release

---

## Conclusion

**Recommended**: **Option A** (Fast to Production)

**Rationale**:
- Delivers 80% value in 33% time (3h vs 9h total)
- Lowest risk (95% success probability)
- Can add remaining features in v2.1.0
- Faster user feedback cycle
- Incremental release strategy

**Next Steps**:
1. Execute Phase 1 timeline (3 hours)
2. Ship v2.0.0 if successful
3. Plan v2.1.0 with Phase 2/3 features
