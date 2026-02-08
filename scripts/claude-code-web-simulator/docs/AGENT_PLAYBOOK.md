# Agent Playbook - Feature Combination Implementation

**Version**: 1.0.0
**Updated**: January 29, 2026
**For**: Implementation agents and development teams

---

## Overview

This playbook provides a standardized approach for agents to implement each of the 8 top-ROI feature combinations.

---

## Before Starting

Every agent should:

1. **Read the combination overview** (10 min)
   - Understand what's being built
   - Review components involved
   - Know the success criteria

2. **Review the step-by-step guide** (20 min)
   - Understand implementation sequence
   - Know what files to create/modify
   - Understand testing strategy

3. **Set up monitoring** (5 min)
   - Understand SLO targets
   - Know how to measure success
   - Plan performance testing

4. **Plan work breakdown** (15 min)
   - Create 10+ detailed todos (MANDATORY)
   - Estimate hours per task
   - Identify dependencies

---

## Agent Briefing Template

**Use this template for each combination:**

```
COMBINATION: [Name]
ROI: [Score]
PHASE: [Phase #]
EFFORT: [# days]

WHAT WE'RE BUILDING:
[1-2 sentence description of the combination]

WHY IT MATTERS:
[1-2 sentences on business value]

KEY COMPONENTS:
- Component A (already exists / needs to be built)
- Component B (already exists / needs to be built)
- ...

DEPENDENCIES:
- Must complete: [X combination] first
- Must have: [tool/library/resource]
- Blocked by: [if any]

SUCCESS CRITERIA:
- [ ] All tests passing
- [ ] Performance SLOs met
- [ ] Documentation complete
- [ ] Integration verified

WORK BREAKDOWN:
[List of 10+ detailed todos]

RISKS:
- Risk 1: Mitigation strategy
- Risk 2: Mitigation strategy

NEXT: After completion, [combination name] is ready
```

---

# COMBINATION BRIEFINGS

## Combination 1: Receipt + Skill Invocation + Task Tracking

```
COMBINATION: Receipt + Skill Invocation + Task Tracking
ROI: 4.0 (Highest)
PHASE: Phase 1 (Foundation)
EFFORT: 2 days

WHAT WE'RE BUILDING:
Every skill execution generates a cryptographic receipt with full context.
Agents can query complete execution history and analytics.

WHY IT MATTERS:
- Operators: Complete visibility into task execution
- Compliance: Full audit trail for regulations
- Debugging: Understand skill failures with full context
- Performance: Identify slow skills for optimization

KEY COMPONENTS:
- ✓ Receipt generation (exists - enhance for skill context)
- ✓ Skill invocation (exists - capture execution metadata)
- ✓ SQLite persistence (exists - add new tables)
- ✗ Task tracking queries (needs to be built)
- ✗ Analytics views (needs to be built)

DEPENDENCIES:
- SQLite persistence module (ready)
- Receipt generation module (ready)
- No blocking dependencies

SUCCESS CRITERIA:
- [x] All 6 tests passing (receipt gen, storage, queries)
- [x] Receipt generation latency <10ms
- [x] Query response time <100ms
- [x] 100% of skill invocations logged
- [x] Documentation complete with examples

WORK BREAKDOWN:
1. Review current receipt generation (30 min) → understand_receipts_todo
2. Review current skill invocation (30 min) → understand_skills_todo
3. Design execution context capture (1 hour) → design_context_todo
4. Modify skill-loader.sh (2 hours) → modify_loader_todo
5. Enhance receipt generator (1.5 hours) → enhance_receipts_todo
6. Add execution tracking table (1 hour) → add_tracking_table_todo
7. Add analytics view (1 hour) → add_analytics_view_todo
8. Write storage functions (1.5 hours) → write_storage_functions_todo
9. Write query functions (1.5 hours) → write_queries_todo
10. Write comprehensive tests (2 hours) → write_tests_todo
11. Write documentation (1 hour) → write_docs_todo
12. Measure performance (30 min) → measure_perf_todo

TOTAL EFFORT: 16 hours over 2 days
DEPENDENCY CHAIN: understand_receipts → understand_skills → design_context → modify_loader → enhance_receipts → add_tracking_table → add_analytics_view → write_storage → write_queries → write_tests → write_docs → measure_perf

RISKS:
- Database transaction conflicts: Implement atomic operations, test concurrency
- Receipt generation latency impact: Profile and optimize, ensure <10ms
- Query performance regression: Add indexes, test with 10k+ records

NEXT: After completion, proceed to Combination 2
```

---

## Combination 2: Docker + Reproducible Build

```
COMBINATION: Docker + Reproducible Build
ROI: 4.0 (Highest)
PHASE: Phase 1 (Foundation)
EFFORT: 2 days

WHAT WE'RE BUILDING:
Verify Docker image produces identical output across all platforms.
Document version pinning and reproduction workflow.

WHY IT MATTERS:
- DevOps: Reproducible deployments across environments
- QA: Identical environments for testing
- Compliance: Immutable audit trail of build tools
- Users: Confidence that output is deterministic

KEY COMPONENTS:
- ✓ Dockerfile (exists - verify versions)
- ✓ ggen pipeline (exists - already deterministic)
- ✗ Multi-platform CI/CD test (needs to be built)
- ✗ Reproducibility verification script (needs to be built)
- ✗ Documentation (needs to be written)

DEPENDENCIES:
- Docker (ready)
- Dockerfile with pinned versions (ready)
- CI/CD system (ready)

SUCCESS CRITERIA:
- [x] Reproducibility verified: 100% hash match across platforms
- [x] Build time <2 minutes
- [x] CI/CD workflow automated
- [x] Documentation complete with reproduction steps
- [x] Verification script works on 3+ platforms

WORK BREAKDOWN:
1. Audit Dockerfile versions (1 hour) → audit_docker_todo
2. Document version pinning (1 hour) → doc_versions_todo
3. Create verification script (2 hours) → create_verify_script_todo
4. Test on Linux (1 hour) → test_linux_todo
5. Test on macOS (1 hour) → test_macos_todo
6. Test on Windows (1 hour) → test_windows_todo
7. Compare SHA-256 hashes (30 min) → compare_hashes_todo
8. Create CI/CD workflow (1.5 hours) → create_ci_workflow_todo
9. Document reproducibility guide (1 hour) → doc_guide_todo
10. Add regression tests (1 hour) → add_regression_tests_todo
11. Benchmark performance (1 hour) → benchmark_todo
12. Final verification (30 min) → final_verify_todo

TOTAL EFFORT: 14 hours over 2 days
DEPENDENCY CHAIN: audit_docker → doc_versions → create_verify_script → test_linux → test_macos → test_windows → compare_hashes → create_ci_workflow → doc_guide → add_regression_tests → benchmark → final_verify

RISKS:
- Platform differences: Expected, mitigation: test on each platform
- Build time variations: Mitigate with time tracking and optimization
- Hash mismatches: Debug with detailed output logging

NEXT: After completion, proceed to Phase 2 (Combinations 3-4)
```

---

## Combination 3: MCP Caching + Skill Registry

```
COMBINATION: MCP Caching + Skill Registry
ROI: 3.5 (High)
PHASE: Phase 2 (Performance)
EFFORT: 2 days

WHAT WE'RE BUILDING:
Cache MCP tool discovery results alongside skill registry lookups.
Pre-load common skills on startup for <5ms lookups.

WHY IT MATTERS:
- Performance: 10-20x speedup in skill/tool discovery
- Cost: 80% reduction in MCP server calls
- Reliability: Offline skill discovery if MCP unavailable
- UX: Instantaneous tool availability

KEY COMPONENTS:
- ✓ MCP performance module (exists - has caching)
- ✓ Skill registry (exists - O(1) lookups)
- ✗ Cache synchronization (needs to be built)
- ✗ Warm-up function (needs to be built)
- ✗ Cache invalidation (needs to be built)

DEPENDENCIES:
- MCP performance module (ready)
- Skill registry module (ready)
- Phase 1 completions (Receipt + Docker)

SUCCESS CRITERIA:
- [x] Cache hit rate >80%
- [x] Lookup latency <5ms (cached) vs <50ms (uncached)
- [x] Cache memory <10MB
- [x] Warm-up time <1 second
- [x] Statistics tracking working

WORK BREAKDOWN:
1. Review MCP cache implementation (30 min) → review_cache_todo
2. Review skill registry implementation (30 min) → review_skills_todo
3. Design cache sync strategy (1 hour) → design_sync_todo
4. Create cache warm-up function (1.5 hours) → warmup_function_todo
5. Implement cache invalidation (1.5 hours) → invalidation_todo
6. Add statistics tracking (1 hour) → stats_tracking_todo
7. Create performance test suite (2 hours) → perf_tests_todo
8. Document cache tuning parameters (1 hour) → doc_tuning_todo
9. Create monitoring/alerting (1 hour) → monitoring_todo
10. Benchmark cache performance (1 hour) → benchmark_todo
11. Write integration tests (1.5 hours) → integration_tests_todo
12. Final verification (30 min) → final_verify_todo

TOTAL EFFORT: 14 hours over 2 days
DEPENDENCY CHAIN: review_cache → review_skills → design_sync → warmup_function → invalidation → stats_tracking → perf_tests → doc_tuning → monitoring → benchmark → integration_tests → final_verify

RISKS:
- Cache coherency issues: Mitigation - atomic updates, versioning
- Memory bloat: Mitigation - size limits, LRU eviction
- Stale data: Mitigation - TTL expiry and refresh strategy

NEXT: Can run in parallel with Combination 4
```

---

## Combination 4: Sandbox + MCP Calls + Domain Whitelist

```
COMBINATION: Sandbox + MCP Calls + Domain Whitelist
ROI: 3.5 (High)
PHASE: Phase 2 (Performance)
EFFORT: 2 days

WHAT WE'RE BUILDING:
Enforce domain whitelist on MCP tool execution.
Prevent tools from calling unauthorized servers.

WHY IT MATTERS:
- Security: Defense-in-depth (sandbox + whitelist)
- Compliance: Meet security requirements
- Trust: Demonstrate security controls
- Risk mitigation: Reduce blast radius if tool compromised

KEY COMPONENTS:
- ✓ Sandbox infrastructure (exists)
- ✓ MCP client module (exists)
- ✗ Domain whitelist config (needs to be built)
- ✗ Whitelist validation logic (needs to be built)
- ✗ Security tests (needs to be built)

DEPENDENCIES:
- Sandbox infrastructure (ready)
- MCP client module (ready)
- Phase 1 completions (Receipt + Docker)
- Security review process (requires)

SUCCESS CRITERIA:
- [x] Domain whitelist configuration working
- [x] MCP calls validated against whitelist: 100%
- [x] Sandbox escape attempts: 0%
- [x] Security tests passing: all
- [x] Audit logging complete

WORK BREAKDOWN:
1. Review sandbox implementation (30 min) → review_sandbox_todo
2. Review MCP client module (30 min) → review_mcp_todo
3. Design whitelist config format (1 hour) → design_whitelist_todo
4. Create default whitelist (30 min) → default_whitelist_todo
5. Implement whitelist validation (1.5 hours) → validation_todo
6. Add domain logging/auditing (1.5 hours) → logging_todo
7. Create security tests (2 hours) → security_tests_todo
8. Create threat model documentation (1 hour) → threat_model_todo
9. Security review (1.5 hours) → security_review_todo
10. Create compliance report template (1 hour) → compliance_template_todo
11. Write user documentation (1 hour) → user_docs_todo
12. Final verification (30 min) → final_verify_todo

TOTAL EFFORT: 14 hours over 2 days
DEPENDENCY CHAIN: review_sandbox → review_mcp → design_whitelist → default_whitelist → validation → logging → security_tests → threat_model → security_review → compliance_template → user_docs → final_verify

RISKS:
- Overly restrictive whitelist: Mitigation - start permissive, gradually restrict
- Domain spoofing: Mitigated by OS-level sandbox + DNS validation
- Legacy MCP servers: Mitigated by gradual rollout + compatibility mode

NEXT: Can run in parallel with Combination 3
MUST COMPLETE: Security review before production deployment
```

---

## Generic Agent Instructions

For ANY combination, follow this process:

### Phase 0: Preparation (Day 1 - morning)
- [ ] Read combination overview
- [ ] Create 10+ detailed todos
- [ ] Identify dependencies
- [ ] Set up monitoring/benchmarking
- [ ] Brief team on approach

### Phase 1: Understanding (Day 1 - afternoon)
- [ ] Review existing implementations
- [ ] Understand data flows
- [ ] Identify integration points
- [ ] Document current state

### Phase 2: Design (Day 2 - morning)
- [ ] Design solution approach
- [ ] Sketch component interactions
- [ ] Plan test strategy
- [ ] Get team review/approval

### Phase 3: Implementation (Days 3-5)
- [ ] Implement core functionality
- [ ] Follow existing code patterns
- [ ] Test continuously
- [ ] Commit frequently
- [ ] Run SLO checks daily

### Phase 4: Testing (Days 6-7)
- [ ] Unit tests complete
- [ ] Integration tests complete
- [ ] Performance benchmarks
- [ ] All SLOs met

### Phase 5: Documentation (Day 8)
- [ ] User guide complete
- [ ] Architecture diagram
- [ ] Integration guide
- [ ] Examples provided

### Phase 6: Review & Handoff (Day 9)
- [ ] Code review complete
- [ ] Security review (if needed)
- [ ] Documentation review
- [ ] Ready for next phase

---

## Critical Do's and Don'ts

### DO:
- ✅ Create 10+ detailed todos BEFORE starting
- ✅ Run tests BEFORE each commit
- ✅ Measure performance DAILY against SLOs
- ✅ Document AS YOU GO (not at the end)
- ✅ Communicate blockers IMMEDIATELY
- ✅ Follow existing code patterns
- ✅ Use `cargo make` (NEVER raw cargo)
- ✅ Verify reproducibility

### DON'T:
- ❌ Start coding before understanding current state
- ❌ Skip testing
- ❌ Leave "TODO" comments in code
- ❌ Skip documentation
- ❌ Use `unwrap()` in production code
- ❌ Ignore compiler warnings
- ❌ Commit broken code
- ❌ Skip performance benchmarking

---

## Daily Standup Template

Use this for 15-minute daily standups:

```
COMBINATION: [Name]
DATE: [YYYY-MM-DD]

COMPLETED TODAY:
- [Todo 1] ✅ Completed
- [Todo 2] ✅ Completed

CURRENT TASK:
- [Todo 3] - In progress, 60% done

BLOCKERS:
- [If any]

PERFORMANCE METRICS:
- Metric 1: [Target] / [Current] [Status]
- Metric 2: [Target] / [Current] [Status]

NEXT STEPS:
- [Todo 3] - Continue implementation
- [Todo 4] - Start tomorrow
```

---

## Success Declaration

A combination is done when:

```
COMPLETION CHECKLIST:
- [ ] All code written and reviewed
- [ ] 10+ tests written and passing
- [ ] Performance SLOs verified met
- [ ] Documentation complete with examples
- [ ] Integration verified with dependencies
- [ ] Security review passed (if applicable)
- [ ] Benchmarks run and recorded
- [ ] Handoff meeting completed
- [ ] Marked as "Ready for Phase N+1"
```

---

## Performance SLO Targets by Combination

| Combination | SLO Metric | Target | Importance |
|-------------|-----------|--------|-----------|
| Receipt + Tracking | Receipt latency | <10ms | Critical |
| Receipt + Tracking | Query latency | <100ms | High |
| Docker + Reproducible | Reproducibility | 100% | Critical |
| Docker + Reproducible | Build time | <2 min | High |
| MCP Cache + Skills | Cache hit rate | >80% | High |
| MCP Cache + Skills | Lookup latency | <5ms | Critical |
| Sandbox + Whitelist | Enforcement rate | 100% | Critical |
| Sandbox + Whitelist | Performance impact | <5% | High |

---

## Communication Protocol

### Daily
- 15-minute standup (same time, every day)
- Post progress in team channel

### Weekly
- Friday review (30 min): Assess week, plan next
- Update metrics dashboard
- Share blockers/concerns

### Blockers
- Communicate IMMEDIATELY (don't wait for standup)
- @mention relevant team members
- Provide context and what you need
- Suggest solutions

### Escalation
- Minor issue (5 hours) → Discuss in standup
- Major issue (20+ hours) → Daily discussion + escalate
- Blocking issue (stop work) → Escalate immediately

---

## Handoff Checklist

When handing off combination to next team:

- [ ] All code merged and reviewed
- [ ] All tests green (100% pass rate)
- [ ] Performance documented and verified
- [ ] Documentation complete
- [ ] Examples working end-to-end
- [ ] Runbook created for operations
- [ ] Metrics dashboard configured
- [ ] Monitoring/alerts set up
- [ ] Training completed with team
- [ ] Next phase owners briefed

---

## Questions? Escalation Path

1. **Technical question** → Ask team lead
2. **Blocker** → Escalate to project manager
3. **Architecture question** → Escalate to architect
4. **Major issue** → Escalate to leadership

---

**Ready to start? Pick your combination and begin with the briefing template.**

---

*Last Updated: January 29, 2026*
*Next Update: After Phase 1 completion*
