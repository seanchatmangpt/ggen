# v2.0.0 Migration Priority Matrix

**Method**: Complexity × Value × Dependencies Analysis
**Date**: 2025-11-02

---

## Priority Scoring Formula

```
Priority Score = (User Value × 10) + (Simplicity × 5) - (Dependencies × 3)

Where:
- User Value: 1-10 (10 = critical feature)
- Simplicity: 1-10 (10 = trivial to implement)
- Dependencies: 0-10 (0 = no deps, 10 = many complex deps)
```

---

## Command Module Scores

| Module | User Value | Simplicity | Deps | Score | Priority | Hours |
|--------|------------|------------|------|-------|----------|-------|
| **Utils** | 8 | 7 | 0 | **115** | P0 🔴 | 2-3 |
| **Graph** (fix) | 6 | 10 | 1 | **107** | P0 🔴 | 0.5 |
| **Hook** | 5 | 8 | 2 | **84** | P1 🟡 | 1-2 |
| **CI** | 4 | 7 | 2 | **69** | P1 🟡 | 1-2 |
| **Audit** | 3 | 6 | 1 | **57** | P2 🟢 | 1 |

### Already Complete (No Migration Needed)

| Module | User Value | Status |
|--------|------------|--------|
| **Template** | 10 | ✅ COMPLETE |
| **Project** | 10 | ✅ COMPLETE |
| **Marketplace** | 9 | ✅ COMPLETE |
| **AI** | 8 | ✅ COMPLETE |

---

## Detailed Analysis

### P0 (Critical) - Ship Blockers

#### 1. Utils (Score: 115) 🔴

**Why P0**:
- Health checking is critical for troubleshooting
- Environment management is user-facing
- Blocks production release confidence

**Complexity**:
- Domain layer 67% complete (2/3 files work)
- Missing type exports in mod.rs
- Reference implementation exists (marketplace)

**Value**:
- `utils doctor`: System health diagnostics
- `utils env`: Environment variable management
- **User Impact**: HIGH (troubleshooting, debugging)

**Dependencies**:
- None (pure Rust std lib)

**Estimated Hours**: 2-3
- 1h: Fix domain/utils/doctor.rs exports
- 1h: Fix domain/utils/env.rs exports
- 0.5h: Add cmds/utils.rs CLI wrapper
- 0.5h: Testing and validation

**Risk**: LOW (pattern exists, no external deps)

---

#### 2. Graph Fix (Score: 107) 🔴

**Why P0**:
- **One-line fix** blocks entire graph subsystem
- RDF/SPARQL queries are core feature
- High value, trivial complexity

**Complexity**:
- Typo: `GraphCmd` → `GraphArgs`
- File: `cmds/mod.rs:40`
- Time: 5 minutes

**Value**:
- Unblocks 4 graph verbs (load, query, export, visualize)
- SPARQL is central to ggen architecture
- **User Impact**: HIGH (knowledge graph operations)

**Dependencies**:
- oxigraph (already working)

**Estimated Hours**: 0.5
- 5 min: Fix typo
- 10 min: Build validation
- 15 min: Test graph commands

**Risk**: TRIVIAL (one-line change)

---

### P1 (High) - Extended Features

#### 3. Hook (Score: 84) 🟡

**Why P1**:
- Developer experience feature
- Git hooks automation valuable
- Not blocking core workflows

**Complexity**:
- Domain layer 100% complete (5 files)
- CLI wrapper missing
- Pattern exists (marketplace, template)

**Value**:
- `hook create`: Create pre-commit, post-commit hooks
- `hook list`: List installed hooks
- `hook remove`: Remove hooks
- `hook monitor`: Monitor hook execution
- **User Impact**: MEDIUM (dev productivity)

**Dependencies**:
- tokio (already in workspace)

**Estimated Hours**: 1-2
- 1h: Add cmds/hook.rs CLI wrapper
- 0.5h: Testing
- 0.5h: Documentation

**Risk**: LOW (domain layer complete)

---

#### 4. CI (Score: 69) 🟡

**Why P1**:
- CI/CD automation helpful
- Workflow management useful
- Internal tooling, not user-critical

**Complexity**:
- Domain layer 100% complete (2 files)
- CLI wrapper missing
- Single verb (workflow)

**Value**:
- `ci workflow`: Run CI workflows locally
- **User Impact**: MEDIUM (CI testing)

**Dependencies**:
- tokio (already in workspace)

**Estimated Hours**: 1-2
- 1h: Add cmds/ci.rs CLI wrapper
- 0.5h: Testing
- 0.5h: Documentation

**Risk**: LOW (simple single-verb command)

---

### P2 (Medium) - Optional Features

#### 5. Audit (Score: 57) 🟢

**Why P2**:
- Security scanning nice-to-have
- Optional feature for advanced users
- Can defer to v2.1.0

**Complexity**:
- Domain layer 100% complete (2 files)
- CLI wrapper missing
- Single verb (security)

**Value**:
- `audit security`: Security vulnerability scanning
- **User Impact**: LOW (advanced feature)

**Dependencies**:
- tokio (already in workspace)

**Estimated Hours**: 1
- 0.5h: Add cmds/audit.rs CLI wrapper
- 0.25h: Testing
- 0.25h: Documentation

**Risk**: LOW (single verb, simple wrapper)

---

## Dependency Analysis

### External Dependencies (All Working)

```
oxigraph ──→ Graph ✅ (1 typo to fix)
genai ─────→ AI ✅ (complete)
reqwest ───→ Marketplace ✅ (complete)
ggen-core ─→ Template, Project ✅ (complete)
tokio ─────→ Hook, CI, Audit (domain complete, no CLI)
```

**Insight**: No dependency blockers. All external crates already integrated.

### Internal Dependencies

```
runtime (94 LOC) ──→ All commands (working)
domain (8,619 LOC) → All commands (98% complete)
cmds (1,052 LOC) ──→ CLI routing (56% complete)
```

**Insight**: Runtime bridge is stable. Domain layer is robust. CLI wrappers are the only gap.

---

## Value × Complexity Quadrants

```
High Value, Low Complexity (DO FIRST)
┌──────────────────────────────┐
│ • Graph fix (1 line)    🔴   │
│ • Utils (reference exists) 🔴│
└──────────────────────────────┘

High Value, High Complexity (DEFER)
┌──────────────────────────────┐
│ (None - all high-value items │
│  are low complexity!)         │
└──────────────────────────────┘

Low Value, Low Complexity (QUICK WINS)
┌──────────────────────────────┐
│ • Hook 🟡                     │
│ • CI 🟡                       │
│ • Audit 🟢                    │
└──────────────────────────────┘

Low Value, High Complexity (SKIP)
┌──────────────────────────────┐
│ (None identified)             │
└──────────────────────────────┘
```

---

## 80/20 Analysis

### Phase 1 (P0): 80% Value, 20% Effort

**Modules**: Utils + Graph fix
**Coverage**: 67% (6/9 modules)
**Hours**: 2-3
**Value Delivered**:
- Template generation ✅
- Project workflows ✅
- Marketplace integration ✅
- AI code generation ✅
- Graph queries ✅
- System health checks ✅

**User Stories Covered**:
- "Generate code from RDF graphs" ✅
- "Search and install marketplace packages" ✅
- "Use AI to generate templates" ✅
- "Query knowledge graphs with SPARQL" ✅
- "Check system health and dependencies" ✅

**What's Missing**:
- Hook automation (dev tooling)
- CI workflows (internal)
- Security audits (advanced)

**Trade-off**: 67% coverage delivers 80%+ of user value

---

### Phase 2 (P1): +15% Value, +20% Effort

**Modules**: Hook + CI
**Coverage**: 89% (8/9 modules)
**Hours**: +2 (cumulative 5)
**Additional Value**:
- Git hook automation
- Local CI testing

**User Stories Added**:
- "Automate pre-commit checks" ✅
- "Run CI workflows locally" ✅

---

### Phase 3 (P2): +5% Value, +10% Effort

**Modules**: Audit
**Coverage**: 100% (9/9 modules)
**Hours**: +1 (cumulative 6)
**Additional Value**:
- Security scanning

**User Stories Added**:
- "Scan for vulnerabilities" ✅

---

## Risk vs Reward Matrix

```
High Reward, Low Risk (MUST DO)
┌────────────────────────────────┐
│ Graph fix    (107 score, 0.5h) │ 🔴
│ Utils        (115 score, 2-3h) │ 🔴
└────────────────────────────────┘

Medium Reward, Low Risk (SHOULD DO)
┌────────────────────────────────┐
│ Hook         (84 score, 1-2h)  │ 🟡
│ CI           (69 score, 1-2h)  │ 🟡
└────────────────────────────────┘

Low Reward, Low Risk (NICE TO HAVE)
┌────────────────────────────────┐
│ Audit        (57 score, 1h)    │ 🟢
└────────────────────────────────┘

(No high-risk items identified)
```

---

## Recommended Execution Order

### Option A: Fastest to Production (3 hours)

```
1. Graph fix     (5 min)   🔴
2. Utils domain  (2 hours) 🔴
3. Utils CLI     (30 min)  🔴
4. Validation    (30 min)

Result: v2.0.0 SHIPPED (67% coverage, 80% value)
```

### Option B: Extended Release (5 hours)

```
1. Graph fix     (5 min)   🔴
2. Utils domain  (2 hours) 🔴
3. Utils CLI     (30 min)  🔴
4. Hook CLI      (1 hour)  🟡
5. CI CLI        (1 hour)  🟡
6. Validation    (30 min)

Result: v2.0.0 SHIPPED (89% coverage, 95% value)
```

### Option C: Complete Release (6 hours)

```
1. Graph fix     (5 min)   🔴
2. Utils domain  (2 hours) 🔴
3. Utils CLI     (30 min)  🔴
4. Hook CLI      (1 hour)  🟡
5. CI CLI        (1 hour)  🟡
6. Audit CLI     (1 hour)  🟢
7. Validation    (30 min)

Result: v2.0.0 SHIPPED (100% coverage, 100% value)
```

---

## Success Probability

| Phase | Probability | Confidence |
|-------|-------------|------------|
| P0 (Graph fix) | 99% | Very High |
| P0 (Utils) | 95% | High |
| P1 (Hook) | 90% | High |
| P1 (CI) | 90% | High |
| P2 (Audit) | 85% | Medium |

**Compound Probability**:
- Phase 1 success: 94% (0.99 × 0.95)
- Phase 2 success: 76% (0.94 × 0.90 × 0.90)
- Phase 3 success: 65% (0.76 × 0.85)

**Interpretation**: Phase 1 is very likely to succeed. Phase 2-3 carry incremental risk but don't block v2.0.0 release.

---

## Blockers and Mitigation

### Identified Blockers

1. **Utils domain layer incomplete**
   - **Mitigation**: Copy pattern from marketplace (working reference)
   - **Fallback**: Ship v2.0.0 without utils, mark as v2.1.0 feature

2. **Graph typo**
   - **Mitigation**: One-line fix
   - **Fallback**: None needed (trivial fix)

### No Blockers Identified

- Hook, CI, Audit: Domain layers complete, only CLI wrappers needed
- All external dependencies working
- Runtime bridge stable
- Test infrastructure validated

---

## Conclusion

**Recommendation**: Execute **Phase 1 (P0)** to achieve production release in 2-3 hours.

**Rationale**:
- Highest value/effort ratio (80% value, 20% effort)
- Lowest risk (95% success probability)
- Unblocks critical features (graph, utils)
- Delivers complete user workflows
- Allows immediate v2.0.0 release

**Next Steps**: Execute STRATEGY.md Phase 1 plan.
