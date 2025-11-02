# ggen v2 Migration: Quick Reference

**Status**: DRAFT | **Last Updated**: 2025-11-01

---

## TL;DR

**Problem**: 67 commands to migrate, build broken, security issue
**Solution**: 3-phase rollout with automation
**Timeline**: 12 weeks | **Effort**: 36 hours (vs 188 manual)

---

## Current State (2025-11-01)

```
✅ 36 commands exist (commands + domain layers)
❌ Build fails (8 compilation errors)
❌ Security: RUSTSEC-2025-0111 (critical)
❌ Marketplace: 0/5 commands working (stubs only)
```

---

## 3-Phase Rollout

### v2.0.0 MVP (2 weeks)
```
Commands: 20 (core only)
Focus:    Fix blockers + stabilize
Effort:   16 hours
Goal:     Production-ready foundation
```

**Critical Fixes**:
- ✅ Build failures (2h)
- ✅ Security vulnerability (4h)
- ✅ Marketplace stubs → real implementations (6h)
- ✅ Integration tests (2h)
- ✅ Documentation (2h)

**Commands**:
```
Template      (7): generate, list, show, lint, generate-tree, regenerate, new
Marketplace   (5): search, install, list, publish, update
Project       (3): init, build, new
Utils         (5): doctor, completion, env, hook apply, ci validate
```

---

### v2.1.0 Features (4 weeks)
```
Commands: 45 total (20 + 25 new)
Focus:    AI + Graph + Shell
Effort:   20 hours
Goal:     Advanced features for power users
```

**New Commands**:
```
AI          (8): gen, analyze, plan, refactor, template create/optimize/validate/suggest
Graph       (9): load, query, visualize, export, merge, workflow create/run/list/validate
Shell       (8): init, aliases, functions, completions, env set/get/list/export
```

---

### v2.2.0 Complete (6 weeks)
```
Commands: 67+ total (45 + 22 new)
Focus:    Feature parity + v1 compatibility
Effort:   24 hours
Goal:     100% v1 migration
```

**New Commands**:
```
Template    (5): validate --schema, test, benchmark, migrate, diff
Marketplace (5): sync, verify, stats, author, trending
Project     (6): status, test, deploy, watch, clean, dependencies
CI/CD       (6): test, lint, build, deploy, rollback, status
```

---

## Automation ROI

### Manual Approach
```
47 remaining commands × 4 hours = 188 hours
```

### Automated Approach
```
Build automation:  16 hours
Write YAML specs:   8 hours
Parallel agents:    4 hours (wall time)
Quality/docs:       8 hours
Total:             36 hours

Savings: 152 hours (81% reduction)
ROI:     543% (5.4x return)
```

---

## Critical Path

### Week 1-2: Fix Blockers
```bash
# 1. Fix build (2h)
cargo build --release  # Should succeed

# 2. Fix security (4h)
cargo audit            # 0 critical vulnerabilities

# 3. Implement marketplace (6h)
ggen marketplace search "test"    # Real results
ggen marketplace install "test"   # Downloads template

# 4. Release v2.0.0 MVP (2h)
git tag v2.0.0
cargo publish
```

### Week 3-6: Build Automation
```bash
# 1. Build generators (16h)
cargo run --bin gen-command
cargo run --bin gen-tests
cargo run --bin gen-docs

# 2. Generate 47 commands (8h specs + 4h agents)
# Write YAML specs
# Spawn parallel agents via Claude Code
# Auto-generate tests + docs

# 3. Release v2.1.0 (2h)
git tag v2.1.0
cargo publish
```

### Week 7-12: Complete Migration
```bash
# 1. Implement remaining 22 commands (12h)
# Use automation pipeline

# 2. v1 compatibility layer (8h)
ggen doctor --migrate-config

# 3. Final validation (4h)
cargo test --workspace
cargo audit
cargo bench

# 4. Release v2.2.0 (2h)
git tag v2.2.0
cargo publish
```

---

## Command Breakdown by Phase

### v2.0.0 MVP (20 commands) - CRITICAL

| Category | Count | Status |
|----------|-------|--------|
| Template | 7 | ✅ Working |
| Marketplace | 5 | ❌ Stubs (fix first) |
| Project | 3 | ✅ Working |
| Utils | 5 | ✅ Working |

**Priority**: Fix marketplace stubs immediately

---

### v2.1.0 Additions (25 commands)

| Category | Count | Status |
|----------|-------|--------|
| AI | 8 | 📝 Spec phase |
| Graph | 9 | 📝 Spec phase |
| Shell | 8 | 📝 Spec phase |

**Strategy**: Generate via automation + parallel agents

---

### v2.2.0 Additions (22 commands)

| Category | Count | Status |
|----------|-------|--------|
| Template Advanced | 5 | 📋 Planning |
| Marketplace Advanced | 5 | 📋 Planning |
| Project Management | 6 | 📋 Planning |
| CI/CD | 6 | 📋 Planning |

**Strategy**: Same automation pipeline as v2.1.0

---

## Quality Gates

### v2.0.0 Release Criteria
```
✅ cargo build --release (succeeds)
✅ cargo audit (0 critical vulnerabilities)
✅ cargo test --workspace (80%+ pass rate)
✅ All 20 commands work end-to-end
✅ Binary size < 20MB
✅ Startup time < 100ms
```

### v2.1.0 Release Criteria
```
✅ All v2.0.0 criteria
✅ All 45 commands work end-to-end
✅ Test coverage 80%+
✅ AI generation < 5s (with caching)
✅ Graph queries < 1s (10k nodes)
```

### v2.2.0 Release Criteria
```
✅ All v2.1.0 criteria
✅ All 67+ commands work end-to-end
✅ Test coverage 85%+
✅ 100% v1 feature parity
✅ v1 compatibility layer working
✅ Performance ≥ v1
```

---

## Risk Mitigation

### High-Risk Items
```
❌ Build failures        → Fix immediately (2h)
❌ Security vulnerability → Fix before release (4h)
❌ Marketplace stubs     → Implement real versions (6h)
```

### Medium-Risk Items
```
⚠️  AI API costs         → Local caching + rate limiting
⚠️  Graph database       → Optional Neo4j, in-memory default
⚠️  Shell compatibility  → Test all shells, graceful fallbacks
```

### Mitigation Strategy
```
1. Fix high-risk items FIRST (week 1)
2. Build automation to reduce human error (week 2-3)
3. Parallel agents for faster development (week 3-6)
4. Continuous testing in CI/CD (ongoing)
```

---

## Success Metrics

### Velocity
| Metric | Manual | Automated | Improvement |
|--------|--------|-----------|-------------|
| Effort per command | 4h | 45min | 75% faster |
| Total effort | 188h | 36h | 81% reduction |
| Wall time | 188h | 4h | 98% reduction (parallel) |

### Quality
| Metric | Target | Measure |
|--------|--------|---------|
| Test coverage | 80%+ | `cargo tarpaulin` |
| Build time | <45s | `cargo build --release` |
| Binary size | <20MB | Release binary |
| Startup time | <100ms | Benchmarks |

### Completeness
| Phase | Commands | Date |
|-------|----------|------|
| v2.0.0 | 20/67 (30%) | Week 2 |
| v2.1.0 | 45/67 (67%) | Week 6 |
| v2.2.0 | 67/67 (100%) | Week 12 |

---

## Next Steps

### Immediate (This Week)
1. Fix build failures
2. Fix security vulnerability
3. Implement marketplace stubs
4. Run integration tests
5. Update documentation

**Owner**: Core team
**Effort**: 16 hours (2 developer days)

### Short-Term (Next 2 Weeks)
1. Release v2.0.0 MVP
2. Community beta testing
3. Fix critical bugs
4. Start automation tools

**Owner**: Core team + community
**Effort**: 24 hours

### Medium-Term (Next 6 Weeks)
1. Build automation pipeline
2. Generate 47 command specs
3. Spawn parallel agents
4. Release v2.1.0

**Owner**: Core team
**Effort**: 36 hours

---

## Decision Points

### Should We Automate?
```
✅ YES - ROI is 543% (5.4x return)
✅ YES - 152 hours saved
✅ YES - Reduces human error
✅ YES - Enables parallel development
```

### Should We Use Parallel Agents?
```
✅ YES - 20 hours → 4 hours wall time (98% faster)
✅ YES - Proven pattern (clnrm v1.2.0 validation)
✅ YES - Each agent owns 9-10 commands
✅ YES - No inter-agent dependencies
```

### Should We Release v2.0.0 Now?
```
❌ NO - Build fails (cannot compile)
❌ NO - Security vulnerability (RUSTSEC-2025-0111)
❌ NO - Marketplace commands are stubs
✅ YES - After fixing blockers (16 hours)
```

---

## Resources

### Documentation
- **Full Plan**: `V2_MIGRATION_PHASES.md` (detailed roadmap)
- **Automation**: `V2_AUTOMATION_STRATEGY.md` (automation guide)
- **This File**: `V2_QUICK_REFERENCE.md` (quick reference)

### Code Locations
- Commands: `cli/src/commands/` (36 files)
- Domain: `cli/src/domain/` (39 files)
- Tests: `cli/tests/` (integration tests)
- Specs: `commands/specs/` (YAML specs - to create)

### Tools
- Generator: `tools/gen-command/` (to build)
- Test Gen: `tools/gen-tests/` (to build)
- Doc Gen: `tools/gen-docs/` (to build)

---

## FAQ

**Q: Why 3 phases instead of 1 big release?**
A: Phased rollout reduces risk, gets core features to users faster, allows community feedback.

**Q: Why build automation for 47 commands?**
A: ROI is 543%. Automation saves 152 hours and reduces errors.

**Q: Can we ship v2.0.0 today?**
A: No. Build fails, security issue, marketplace broken. Fix blockers first (16 hours).

**Q: How long until v2.2.0 complete?**
A: 12 weeks (3 months) from now.

**Q: What if automation fails?**
A: Fall back to manual (188 hours). But automation pays for itself after 10 commands.

**Q: Why use parallel agents?**
A: 20 hours of work compressed to 4 hours wall time. Proven in clnrm validation (178KB deliverables).

---

## Visual Timeline

```
Now            Week 2         Week 6           Week 12
 │               │              │                 │
 ├─v2.0.0 MVP───┤              │                 │
 │  (20 cmd)    │              │                 │
 │  Fix block   │              │                 │
 │              │              │                 │
 │              ├─v2.1.0───────┤                 │
 │              │  (45 cmd)    │                 │
 │              │  AI+Graph    │                 │
 │              │              │                 │
 │              │              ├─v2.2.0──────────┤
 │              │              │  (67+ cmd)      │
 │              │              │  Complete       │
 └──────────────┴──────────────┴─────────────────┘
```

---

## Conclusion

**Current State**: 36 commands, broken build, security issue

**Target State**: 67+ commands, production-ready, automated pipeline

**Strategy**: Fix blockers → Build automation → Parallel agents → Complete

**Timeline**: 12 weeks (3 months)

**Effort**: 36 hours (vs 188 manual)

**ROI**: 543% (5.4x return on automation investment)

**Recommendation**: **Proceed with phased approach, build automation first**

**First Action**: Fix v2.0.0 blockers (16 hours, this week)

---

**For Details**: See `V2_MIGRATION_PHASES.md` and `V2_AUTOMATION_STRATEGY.md`
