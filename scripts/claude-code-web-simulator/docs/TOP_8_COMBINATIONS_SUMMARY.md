# Top 8 High-ROI Feature Combinations - Quick Reference

**Updated**: January 29, 2026
**Purpose**: Rapid reference for agents planning implementation

---

## At-a-Glance Summary

| Rank | Combination | ROI | Effort | Value | Status | Blockers |
|------|-------------|-----|--------|-------|--------|----------|
| 1 | Receipt + Skill Tracking | 4.0 | 2 | 8 | Ready | None |
| 2 | Docker + Reproducible Build | 4.0 | 2 | 8 | Ready | None |
| 3 | MCP Cache + Skills | 3.5 | 2 | 7 | Ready | None |
| 4 | Sandbox + Whitelist | 3.5 | 2 | 7 | Ready | Security review |
| 5 | Sandbox + Cache | 3.0 | 2 | 6 | Ready | None |
| 6 | Docker + Pipeline + Receipt | 2.67 | 3 | 8 | Ready | Git integration |
| 7 | MCP Cache + DB | 2.67 | 3 | 8 | Ready | Schema |
| 8 | Orchestration + Hooks | 2.67 | 3 | 8 | Ready | Router testing |

---

## Phase 1: Foundation (Weeks 1-2) - Effort: 4 Total

### Combination 1: Receipt + Skill Invocation + Task Tracking
- **Why**: Highest ROI (4.0), establishes observability foundation
- **What happens**: Every skill execution tracked with cryptographic proof
- **Key integration**: Skill loader → receipt generation → SQLite → audit trail
- **Effort**: 2 (20 person-hours)
- **Files to create/modify**:
  - `modules/skill-loader.sh` - Add execution context capture
  - Database schema - Add task_tracking view
  - Tests - 4-6 task tracking tests

### Combination 2: Docker + Reproducible Build
- **Why**: Highest ROI (4.0), ensures deterministic outputs
- **What happens**: Same input, same Docker image = identical output (always)
- **Key integration**: Dockerfile + pinned versions + CI/CD verification
- **Effort**: 2 (16 person-hours)
- **Files to create/modify**:
  - `Dockerfile` - Add version comments
  - CI/CD workflow - Add reproducibility tests
  - Documentation - Add "reproduce exactly" guide

---

## Phase 2: Performance (Weeks 3-4) - Effort: 4 Total

### Combination 3: MCP Caching + Skill Registry
- **Why**: High ROI (3.5), dramatic speed improvements (<5ms vs <100ms)
- **What happens**: Skill lookup cached alongside MCP tool discovery
- **Key integration**: MCP cache + skill registry + agent task router
- **Effort**: 2 (16 person-hours)
- **Files to create/modify**:
  - `modules/skill-registry.sh` - Add MCP cache layer
  - `examples/` - Cache warm-up example
  - Tests - Cache hit rate validation

### Combination 4: Sandbox + MCP Calls + Domain Whitelist
- **Why**: High ROI (3.5), adds security layer
- **What happens**: MCP tools isolated in sandbox, can only call approved domains
- **Key integration**: Sandbox config + MCP domain validation + security tests
- **Effort**: 2 (16 person-hours)
- **Files to create/modify**:
  - `config/domain-whitelist.json` - New config file
  - `modules/mcp-client.sh` - Add whitelist validation
  - Security tests - Verify isolation

---

## Phase 3: Infrastructure (Weeks 5-6) - Effort: 9 Total

### Combination 5: Sandbox + Persistent Cache
- **Why**: Good ROI (3.0), efficient across restarts
- **Effort**: 2 (16 person-hours)
- **Dependencies**: Sandbox (exists) + Cache (exists)

### Combination 6: Docker + ggen Pipeline + Receipts
- **Why**: Good ROI (2.67), transforms code provenance
- **Effort**: 3 (24 person-hours)
- **Dependencies**: Docker (exists) + Pipeline (exists) + Receipts (exists)

### Combination 7: MCP Performance + Persistent Cache + Database
- **Why**: Good ROI (2.67), enables cross-session optimization
- **Effort**: 3 (24 person-hours)
- **Dependencies**: MCP cache (exists) + SQLite (exists)

### Combination 8: Agent Orchestration + Hook Engine + Task Router
- **Why**: Good ROI (2.67), enables autonomous execution
- **Effort**: 3 (24 person-hours)
- **Dependencies**: Hooks (exists) + Orchestrator (exists)
- **Note**: Highest risk, requires extensive testing

---

## Quick Decision Tree

**Choose Combination Based On:**

| Goal | Best Combo | Why |
|------|-----------|-----|
| Audit compliance | Receipt + Tracking | Complete task provenance |
| Reproducible builds | Docker + Reproducible | Deterministic outputs |
| Fast performance | MCP Cache + Skills | 10-20x speedup |
| Secure execution | Sandbox + Whitelist | Defense-in-depth |
| Cost reduction | MCP + Cache + DB | Minimize server calls |
| Autonomous agents | Orchestration + Hooks | Zero-config routing |

---

## Implementation Checklist

### Before Starting Any Combination

- [ ] Review full analysis in `FEATURE_COMBINATIONS_ANALYSIS.md`
- [ ] Verify dependencies are ready
- [ ] Create detailed task breakdown (10+ todos)
- [ ] Assign 2-3 specialized agents
- [ ] Set up performance benchmarks
- [ ] Plan testing strategy

### During Implementation

- [ ] Run `cargo make check` before each commit
- [ ] Create tests FIRST (Chicago TDD pattern)
- [ ] Measure performance against SLOs
- [ ] Document integration points
- [ ] Create examples for users

### After Implementation

- [ ] All tests passing
- [ ] Performance SLOs met
- [ ] Documentation complete
- [ ] Integration verified
- [ ] Ready for Phase N+1

---

## Performance Targets (SLOs)

| Metric | Target | Current | Gap |
|--------|--------|---------|-----|
| Receipt tracking | <10ms | ~5ms | ✓ |
| Reproducibility | 100% | ~100% | ✓ |
| Cache hit rate | >80% | ~75% | -5% |
| Domain enforcement | 100% | TBD | TBD |
| Audit completeness | 100% | ~95% | -5% |

---

## Risk Assessment

| Combination | Risk | Mitigation |
|-------------|------|-----------|
| Receipt + Tracking | ✅ Low | Minimal new code |
| Docker + Reproducible | ✅ Low | Deterministic pipeline |
| MCP + Skills | ✅ Low | Cache module exists |
| Sandbox + Whitelist | ⚠️ Medium | Security review required |
| Sandbox + Cache | ✅ Low | Persistence exists |
| Docker + Pipeline | ⚠️ Medium | Integration testing |
| MCP + DB | ⚠️ Medium | Schema changes |
| Orchestration | ⚠️ High | Router testing crucial |

---

## Effort Breakdown (Total)

- **Phase 1**: 4 person-days (Foundation)
- **Phase 2**: 4 person-days (Performance)
- **Phase 3**: 9 person-days (Infrastructure)
- **Total**: ~17-18 person-days (~3-4 weeks with 1 FTE)

---

## Expected Outcomes

### After Phase 1
- ✓ Complete task observability
- ✓ Reproducible builds verified
- ✓ Foundation for Phase 2

### After Phase 2
- ✓ 10-20x performance improvements
- ✓ Security baseline established
- ✓ Cost reduction metrics

### After Phase 3
- ✓ Autonomous agent orchestration
- ✓ Complete audit compliance
- ✓ Production-ready MVP

---

## Next Steps

1. **Review** full analysis (this file + `FEATURE_COMBINATIONS_ANALYSIS.md`)
2. **Prioritize** based on business needs
3. **Start Phase 1** with 2-person team
4. **Measure** impact after 2 weeks
5. **Iterate** based on results

---

**Contact**: ggen AI Analysis Team
**Questions**: See `FEATURE_COMBINATIONS_ANALYSIS.md` for detailed Q&A
**Updated**: January 29, 2026
