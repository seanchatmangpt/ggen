# Phase 2 Project Plan - Quick Reference

**Date**: 2026-01-26
**Status**: READY FOR EXECUTION
**Location**: `/Users/sac/ggen/tai-erlang-autonomics/PHASE_2_PROJECT_PLAN.md` (2,213 lines)

---

## What's Phase 2?

Transform the eval-only pricing engine (Phase 1) into a production-ready system with insurance-backed deployments.

**Timeline**: Weeks 2-5 (4 weeks, 20 working days)
**Budget**: $215,270
**Team**: 4 engineers, 80 hours/week
**Goal**: 1 paying customer pilot + production deployment ready

---

## Week-by-Week Plan

### Week 2: Production Build Scaffolding (40 tasks)
- Create separate `tai_autonomics_prod` OTP application
- Implement 5 core modules (2,500+ LOC)
- Set up prod build system (rebar.config.prod, Containerfile.prod)
- Create GitHub Actions CI/CD pipeline
- All modules tested, type-safe, documented

**Deliverables**:
- `ac_prod_mode.erl` (503 LOC) - Prod equivalent to eval mode
- `ac_insurance_client.erl` (skeletal) - Insurance API client
- `ac_insurance_cert_manager.erl` - Certificate management
- `prod_publisher.erl` - Marketplace publish with guards
- `prod_acquisition.erl` - Customer deployment with guards

**Success**: Zero compilation errors, 80%+ test coverage, production-ready code

---

### Week 3: Insurance Integration (35 tasks)
- Real insurance provider API integration (no mocks)
- Certificate provisioning and management
- Graceful degradation to read-only mode
- Deploy to staging environment
- End-to-end testing with real insurance API

**Deliverables**:
- Real insurance API integrated (100+ API calls tested)
- Certificate lifecycle management operational
- Staging environment stable (99.9% uptime)
- Insurance integration runbook

**Success**: Staging stable, customer notifications working, team confident

---

### Week 4: First Customer Pilot (30 tasks)
- Onboard paying customer with contract (not insurance-backed yet)
- Execute 50+ customer operations
- Generate contractual receipts (eval-only stamp + contract ref)
- Monitor customer health and accuracy
- Record first revenue (ASC 606 compliant)

**Deliverables**:
- Customer #1 onboarded and active
- 50+ operations with 100% receipt coverage
- Accuracy: 99%+ matching customer validation
- NPS: 7+
- MRR: $X recorded in accounting

**Success**: Customer happy, revenue recognized, zero incidents

---

### Week 5: Production Deployment Capabilities (25 tasks)
- Deploy production Cloud Run service (separate from staging)
- Production Firestore with backups
- Comprehensive monitoring and alerting
- On-call procedures and runbooks
- Team trained on production operations

**Deliverables**:
- Production system deployed and validated
- Insurance integration tested in prod
- Deployment procedures automated
- Team trained on production ops
- Ready for Customer #2 (Week 6)

**Success**: Production stable, team confident, ready to scale to 3 customers

---

## 130-Item Completion Checklist

### Organization
- **Section A**: Week 2 Scaffolding (40 items)
- **Section B**: Week 3 Insurance (35 items)
- **Section C**: Week 4 Pilot (30 items)
- **Section D**: Week 5 Production (25 items)

### Tracking
- Each item has owner assignment
- Dependencies clearly marked
- Success criteria defined
- Estimated hours provided

### Status
- Week 2: ⏳ IN PROGRESS (ready to launch)
- Week 3: ⏳ PENDING (after Week 2)
- Week 4: ⏳ PENDING (after Week 3)
- Week 5: ⏳ PENDING (after Week 4)

---

## Key Modules (2,500+ LOC)

| Module | Purpose | LOC | Owner |
|--------|---------|-----|-------|
| `ac_prod_mode.erl` | Startup guardrails with insurance | 503 | Coder-1 |
| `ac_insurance_client.erl` | Insurance API client | 450 | Coder-1 |
| `ac_insurance_cert_manager.erl` | Certificate lifecycle | 661 | Coder-2 |
| `prod_publisher.erl` | Marketplace publish + insurance | 400 | Coder-2 |
| `prod_acquisition.erl` | Customer deployment + insurance | 450 | Coder-2 |
| **Total** | | **2,564** | |

---

## Resource Plan

### Team
- 1 Architect (system design, risk management)
- 2 Coders (module implementation, testing)
- 1 DevOps (build, infrastructure, CI/CD)

### Hours
- Week 2: 90 hours
- Week 3: 85 hours
- Week 4: 75 hours
- Week 5: 70 hours
- **Total**: 320 hours (80 per person/week)

### Budget
- Engineering: $192,000 (1,280 hours @ $150/hr)
- Infrastructure: $2,850 (GCP, monitoring, storage)
- Insurance: $850 (policy, certs, setup)
- Contingency: $19,570 (10%)
- **TOTAL**: $215,270

---

## Risk Register (Top 10)

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Insurance API down | 25% | High | Circuit breaker, cache, fallback |
| Cert expires during deploy | 5% | High | Pre-flight check, 7-day buffer |
| Firestore corruption | 2% | Critical | Daily backups, restore tests |
| Customer demands early prod | 35% | Medium | Contract terms, readiness criteria |
| Team burnout | 40% | Medium | Clear roles, pair programming |
| Rate limiting | 30% | Medium | Request queuing, batching |
| Firestore performance | 15% | Medium | Load testing, indexing |
| Customer complexity | 20% | Medium | Discovery, scope control |
| CI/CD failure | 20% | Medium | Manual fallback, monitoring |
| Security vuln | 15% | High | Code review, pen testing |

---

## Phase 1 → Phase 2 → Phase 3 Dependencies

### What Phase 2 Depends On (Phase 1)
- ✅ Eval-only guardrails (ac_eval_mode.erl, ac_receipt_ledger_mcp.erl)
- ✅ Receipt ledger architecture (Merkle chain, session handling)
- ✅ Docker containerization
- ✅ Firestore integration
- ✅ GitHub Actions skeleton
- ✅ Test infrastructure (eunit, ct)

### What Phase 3 Depends On (Phase 2)
- [ ] Production mode fully operational
- [ ] Insurance API integrated (real certificates)
- [ ] Certificate management system proven
- [ ] Customer onboarding process repeatable
- [ ] Revenue recognition automated (ASC 606)
- [ ] Monitoring and alerting in place
- [ ] Team trained on production ops

### Cross-Link Summary
```
Phase 1: Eval-only guardrails ✅
   ↓ enables
Phase 2: Insurance integration + first customer ⏳
   ↓ enables
Phase 3: 3-customer production scaling (Weeks 6-13)
```

---

## Key Decisions & Gates

### Week 2 Gate
- All 5 modules compile with zero errors/warnings
- 80%+ test coverage on all modules
- Docker image builds and runs locally
- CI/CD pipeline working
- **Decision**: Proceed to Week 3? ✅ or ❌

### Week 3 Gate
- Real insurance API integrated (100+ calls tested)
- Staging environment stable (99.9% uptime)
- Certificate management working end-to-end
- Insurance integration tested without failures
- **Decision**: Proceed to Week 4? ✅ or ❌

### Week 4 Gate
- Customer #1 onboarded and active
- 50+ operations with 100% receipt coverage
- Accuracy 99%+, NPS 7+
- Zero critical incidents
- First revenue recorded (ASC 606)
- **Decision**: Proceed to Week 5 prod deployment? ✅ or ❌

### Week 5 Gate
- Production Cloud Run deployed and validated
- All insurance integration tested in prod
- Load tests passed (p95 <500ms, error rate <0.1%)
- Chaos tests passed (resilient to failures)
- Team trained on production operations
- **Decision**: Open for customer #2? ✅ or ❌

---

## Success Metrics

### Technical
- Compilation: 0 errors, 0 warnings (all 4 weeks)
- Test coverage: 80%+ (Week 2) → 85%+ (Weeks 3-5)
- Type coverage: 100% (all 4 weeks)
- API latency: <500ms p95 (Weeks 2-3) → <400ms (Weeks 4-5)
- Uptime: 99%+ dev (Week 2) → 99.5%+ staging (Week 3) → 99.9%+ prod (Weeks 4-5)
- Insurance integration: Mocked (Week 2) → Real API (Weeks 3-5)

### Business
- Week 2: 0 customers (internal testing)
- Week 3: 0 customers (staging testing)
- Week 4: 1 customer (pilot, eval mode + contract)
- Week 5: 1 customer (pilot, production-capable)
- Revenue: $0 (Weeks 2-3) → $X MRR (Week 4+)

### Operational
- Deployment time: <5 min local, <10 min GCP
- Recovery time: <1 hour from backup
- On-call readiness: Runbooks complete, team trained
- Documentation: All modules documented with examples
- Team morale: No burnout, team wants to continue

---

## Weekly Operating Rhythm

**Every Monday 9:00am** - Planning (90 min)
- Review last week's goals vs actual
- Update customer/revenue/engineering status
- Set top 3 priorities for this week
- Identify blockers and escalations

**Every Day 3:00pm** - Standup (10 min)
- What shipped yesterday
- What working on today
- Any blockers
- Shoutout

**Every Friday 4:00pm** - Review (30 min)
- Celebrate wins
- Review KPI metrics
- Discuss challenges and improvements
- Plan next week

---

## File Location

**Full Plan**: `/Users/sac/ggen/tai-erlang-autonomics/PHASE_2_PROJECT_PLAN.md` (2,213 lines)

**Quick Reference** (this file): `/Users/sac/ggen/tai-erlang-autonomics/PHASE_2_OVERVIEW.md`

---

## Next Steps

1. **Get approvals**: CEO (business), CTO (technical), CFO (budget)
2. **Confirm team**: 4 engineers allocated
3. **Setup workspace**: Git branches, docs, Asana tasks
4. **Contact insurance provider**: Begin API integration
5. **Kickoff Week 2**: Monday 9am planning meeting

---

## Questions?

Refer to:
- **Strategic Context**: PHASE_2_PROJECT_PLAN.md Executive Summary
- **Detailed Tasks**: PHASE_2_PROJECT_PLAN.md Week-by-Week Breakdown
- **Risk Management**: PHASE_2_PROJECT_PLAN.md Risk Register
- **Dependencies**: PHASE_2_PROJECT_PLAN.md Cross-Link Section
- **100-Item Checklist**: PHASE_2_PROJECT_PLAN.md Section Pages

---

**Prepared by**: Strategic Planning Agent
**Date**: 2026-01-26
**Version**: 1.0.0
**Status**: READY FOR EXECUTION
