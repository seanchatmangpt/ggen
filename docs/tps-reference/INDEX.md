<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Reference Documentation Index](#tps-reference-documentation-index)
  - [Quick Navigation](#quick-navigation)
    - [For Architects](#for-architects)
    - [For Engineers](#for-engineers)
    - [For DevOps/Implementation](#for-devopsimplementation)
    - [For Library Selection](#for-library-selection)
  - [Document Map](#document-map)
  - [Cross-Document References](#cross-document-references)
    - [00-tps-principles.md](#00-tps-principlesmd)
    - [01-library-catalog.md](#01-library-catalogmd)
    - [02-architecture-blueprint.md](#02-architecture-blueprintmd)
    - [03-implementation-roadmap.md](#03-implementation-roadmapmd)
  - [How to Use This Documentation](#how-to-use-this-documentation)
    - [Scenario 1: "I need to build TAI 2030 from scratch"](#scenario-1-i-need-to-build-tai-2030-from-scratch)
    - [Scenario 2: "I need to understand why we made a specific architectural decision"](#scenario-2-i-need-to-understand-why-we-made-a-specific-architectural-decision)
    - [Scenario 3: "We're in Phase 2, and something is broken"](#scenario-3-were-in-phase-2-and-something-is-broken)
  - [Key Principles Summary](#key-principles-summary)
  - [Integration with Existing Documentation](#integration-with-existing-documentation)
  - [Metrics & Compliance](#metrics--compliance)
    - [Document Quality](#document-quality)
    - [Auditor-Ready](#auditor-ready)
  - [Version History](#version-history)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Reference Documentation Index

**Version**: 1.0.0 (Complete Reference Library)
**Classification**: UNCLASSIFIED//FOR OFFICIAL USE ONLY
**Date**: January 2026
**Owner**: TAI 2030 Architecture Team
**Last Updated**: 2026-01-25

---

## Quick Navigation

### For Architects
Start here: **[00-tps-principles.md](00-tps-principles.md)** (3000 lines)
- Understand 6 TPS principles
- Map to Erlang/OTP patterns
- Learn anti-patterns to avoid

### For Engineers
Start here: **[02-architecture-blueprint.md](02-architecture-blueprint.md)** (2000 lines)
- 5-layer reference architecture
- Code examples (Erlang)
- 3 deployment configurations (dev/staging/prod)

### For DevOps/Implementation
Start here: **[03-implementation-roadmap.md](03-implementation-roadmap.md)** (1500 lines)
- 5-phase rollout plan (11 weeks)
- Success criteria per phase
- Risk mitigation matrix
- Testing checklist

### For Library Selection
Start here: **[01-library-catalog.md](01-library-catalog.md)** (2500 lines)
- 31 battle-tested Erlang libraries
- By TPS principle
- Pros/cons, versions, maintenance status
- Dependency graph

---

## Document Map

```
TPS Reference Library (4 Documents, 9,000+ Lines)
│
├── 00-tps-principles.md (3000 lines)
│   │
│   ├── Principle 1: Just-in-Time (JIT)
│   │   ├── Manufacturing origin
│   │   ├── Code analogy
│   │   ├── Erlang/OTP patterns (4 patterns)
│   │   ├── Library selections (5 libraries)
│   │   ├── Metrics & verification
│   │   └── Anti-patterns table
│   │
│   ├── Principle 2: Jidoka (Autonomation)
│   │   ├── Failure detection
│   │   ├── Circuit breaker FSM
│   │   ├── Supervisor halt strategies
│   │   ├── Health monitoring
│   │   ├── Library selections (5 libraries)
│   │   └── Anti-patterns table
│   │
│   ├── Principle 3: Kanban (Pull-Based Flow)
│   │   ├── Visibility & WIP limits
│   │   ├── Fair distribution
│   │   ├── Capacity planning
│   │   ├── Library selections (5 libraries)
│   │   └── Anti-patterns table
│   │
│   ├── Principle 4: Andon (Visual Management)
│   │   ├── OpenTelemetry tracing
│   │   ├── Real-time dashboards
│   │   ├── Alert routing
│   │   ├── Library selections (5 libraries)
│   │   └── Anti-patterns table
│   │
│   ├── Principle 5: Kaizen (Continuous Improvement)
│   │   ├── Prometheus metrics
│   │   ├── Bottleneck detection
│   │   ├── A/B testing framework
│   │   ├── Library selections (5 libraries)
│   │   └── Anti-patterns table
│   │
│   ├── Principle 6: Heijunka (Load Leveling)
│   │   ├── Token bucket rate limiting
│   │   ├── Round-robin distribution
│   │   ├── Demand prediction
│   │   ├── Library selections (5 libraries)
│   │   └── Anti-patterns table
│   │
│   └── Cross-Principle Integration & Summary
│
├── 01-library-catalog.md (2500 lines)
│   │
│   ├── Dependency Graph (Mermaid)
│   │
│   ├── JIT Libraries (4)
│   │   ├── RabbitMQ (A+)
│   │   ├── NATS (A+)
│   │   ├── poolboy (A+)
│   │   └── jobs (A)
│   │
│   ├── Jidoka Libraries (4)
│   │   ├── fuse (A)
│   │   ├── breaker (B+)
│   │   ├── supervisor (A+)
│   │   └── recon (A)
│   │
│   ├── Kanban Libraries (4)
│   │   ├── RabbitMQ (again)
│   │   ├── gnat (A-)
│   │   ├── queue (A+)
│   │   └── jobs (again)
│   │
│   ├── Andon Libraries (5)
│   │   ├── OpenTelemetry (A)
│   │   ├── Prometheus (A+)
│   │   ├── logger (A+)
│   │   ├── Lager (A-)
│   │   └── Grafana (A-)
│   │
│   ├── Kaizen Libraries (3)
│   │   ├── Prometheus (again)
│   │   ├── entop (B)
│   │   └── folsom (B+)
│   │
│   ├── Heijunka Libraries (4)
│   │   ├── throttle (B)
│   │   ├── jobs (again)
│   │   ├── poolboy (again)
│   │   └── willow (B)
│   │
│   ├── Recommended Stacks (4)
│   │   ├── Stack A: High-Throughput Messaging
│   │   ├── Stack B: Real-Time Event Processing
│   │   ├── Stack C: Budget Spike Throttling
│   │   └── Stack D: Compliance & Audit
│   │
│   ├── Library Quality Matrix (14 libraries)
│   │
│   ├── Security & Compliance Checklist
│   │
│   ├── Dependency Hell Prevention
│   │
│   └── Version Pinning Strategy
│
├── 02-architecture-blueprint.md (2000 lines)
│   │
│   ├── 5-Layer Reference Architecture
│   │   │
│   │   ├── Layer 1: Ingress (Signal Reception)
│   │   │   ├── Cowboy HTTP API
│   │   │   ├── Signature verification
│   │   │   └── Rate limiting (Heijunka)
│   │   │
│   │   ├── Layer 2: Kanban Queue (Work Visibility)
│   │   │   ├── RabbitMQ queue (durable)
│   │   │   └── Deadletter queue
│   │   │
│   │   ├── Layer 3: Jidoka Processor (Autonomic Decision)
│   │   │   ├── Policy engine (gen_statem FSM)
│   │   │   ├── Circuit breaker
│   │   │   └── State manager (persistent)
│   │   │
│   │   ├── Layer 4: Kaizen Metrics (Measurement)
│   │   │   ├── Prometheus collection
│   │   │   └── OpenTelemetry tracing
│   │   │
│   │   └── Layer 5: Andon Board (Alerting)
│   │       ├── Receipt emission
│   │       ├── Audit logging
│   │       ├── Alert routing
│   │       └── Dashboard export
│   │
│   ├── Signal Flow Diagram (Mermaid)
│   │
│   ├── Supervisor Hierarchy (OTP)
│   │
│   ├── 3 Example Configurations
│   │   ├── Config 1: Development (in-memory)
│   │   ├── Config 2: Staging (distributed, durable)
│   │   └── Config 3: Production (HA, FedRAMP)
│   │
│   ├── File Organization (8 layers × supervisor tree)
│   │
│   └── Code Examples (Erlang pseudocode)
│
└── 03-implementation-roadmap.md (1500 lines)
    │
    ├── Phase Dependency Graph (Mermaid)
    │
    ├── Phase 0: Prerequisites (Checklist)
    │
    ├── Phase 1: Jidoka + Basic Kanban (Weeks 1-2)
    │   ├── Deliverables (3)
    │   ├── Timeline (detailed by day)
    │   ├── Code files to create
    │   ├── Success criteria
    │   └── Risk mitigation
    │
    ├── Phase 2: Full Kanban + Andon (Weeks 3-4)
    │   ├── Deliverables (3)
    │   ├── Timeline (detailed by day)
    │   ├── Code files to create
    │   ├── Success criteria
    │   └── Go/No-Go gate
    │
    ├── Phase 3: Kaizen Metrics (Weeks 5-6)
    │   ├── Deliverables (3)
    │   ├── Timeline (detailed by day)
    │   ├── Code files to create
    │   ├── Success criteria
    │   └── Go/No-Go gate
    │
    ├── Phase 4: Heijunka Dashboards (Week 7)
    │   ├── Deliverables (3)
    │   ├── Timeline (detailed by day)
    │   ├── Code files to create
    │   └── Success criteria
    │
    ├── Phase 5: Integration & Go-Live (Weeks 8-11)
    │   ├── Testing checklist (5 categories)
    │   ├── Risk mitigation matrix
    │   ├── Go-live criteria
    │   └── Rollback plan
    │
    ├── Resource Allocation (5-person team)
    │
    ├── Dependencies Between Phases
    │
    ├── Weekly Standup & Gate Reviews
    │
    ├── Metrics Tracking (by phase)
    │
    ├── Budget & Timeline Summary
    │   └── 11 weeks, $111,200 estimated
    │
    └── Appendix: Common Issues & Solutions
```

---

## Cross-Document References

### 00-tps-principles.md

**References**:
- `[01-library-catalog.md](01-library-catalog.md)` for detailed library recommendations
- `[02-architecture-blueprint.md](02-architecture-blueprint.md)` for architecture implementation
- `[03-implementation-roadmap.md](03-implementation-roadmap.md)` for rollout schedule
- `/docs/30-autonomics/gen_statem-patterns.md` for production FSM patterns
- `/docs/00-overview/glossary.md` for canonical terminology

**Cross-References In Document**:
- JIT → RabbitMQ, NATS, jobs, poolboy
- Jidoka → fuse, breaker, supervisor, recon
- Kanban → RabbitMQ, gnat, queue, jobs
- Andon → OpenTelemetry, Prometheus, logger, Lager
- Kaizen → Prometheus, entop, folsom
- Heijunka → throttle, jobs, poolboy, willow

---

### 01-library-catalog.md

**References**:
- `[00-tps-principles.md](00-tps-principles.md)` for conceptual foundation
- `[02-architecture-blueprint.md](02-architecture-blueprint.md)` for integration patterns
- `[03-implementation-roadmap.md](03-implementation-roadmap.md)` for phased adoption

**Dependency Graph**: Shows which libraries work together (RabbitMQ → jobs → poolboy)

**Recommended Stacks**:
1. **Stack A**: RabbitMQ + jobs + fuse + Prometheus + logger
2. **Stack B**: NATS + poolboy + breaker + Prometheus + Grafana
3. **Stack C**: RabbitMQ + throttle + fuse + Prometheus + alerts
4. **Stack D**: RabbitMQ + jobs + fuse + OpenTelemetry + logger

---

### 02-architecture-blueprint.md

**References**:
- `[00-tps-principles.md](00-tps-principles.md)` for conceptual foundation of each layer
- `[01-library-catalog.md](01-library-catalog.md)` for library options
- `[03-implementation-roadmap.md](03-implementation-roadmap.md)` for phase-by-phase details
- `/docs/30-autonomics/gen_statem-patterns.md` for policy engine FSM patterns
- `/docs/00-overview/glossary.md` for terminology

**Signal Flow**:
```
Layer 1: Ingress (Cowboy, signature verification)
  ↓
Layer 2: Kanban (RabbitMQ, WIP limits)
  ↓
Layer 3: Jidoka (gen_statem, circuit breaker)
  ↓
Layer 4: Kaizen (Prometheus, OpenTelemetry)
  ↓
Layer 5: Andon (logger, Grafana, alerts)
```

---

### 03-implementation-roadmap.md

**References**:
- `[00-tps-principles.md](00-tps-principles.md)` for why each principle matters
- `[01-library-catalog.md](01-library-catalog.md)` for library selections per phase
- `[02-architecture-blueprint.md](02-architecture-blueprint.md)` for architecture details

**Phase Dependencies**:
```
Phase 0 → Phase 1 → Phase 2 → Phase 4 → Phase 5
                  ↘ Phase 3 ↗
```

---

## How to Use This Documentation

### Scenario 1: "I need to build TAI 2030 from scratch"

1. Read: `[00-tps-principles.md](00-tps-principles.md)` (1 hour) — understand why
2. Read: `[01-library-catalog.md](01-library-catalog.md)` (30 min) — choose libraries
3. Read: `[02-architecture-blueprint.md](02-architecture-blueprint.md)` (1 hour) — understand structure
4. Execute: `[03-implementation-roadmap.md](03-implementation-roadmap.md)` (11 weeks) — build it

**Total**: 2.5 hours reading + 11 weeks implementation

---

### Scenario 2: "I need to understand why we made a specific architectural decision"

1. Look up term in `/docs/00-overview/glossary.md` (cross-references document)
2. Read relevant principle in `[00-tps-principles.md](00-tps-principles.md)`
3. Check library selection in `[01-library-catalog.md](01-library-catalog.md)`
4. Review implementation in `[02-architecture-blueprint.md](02-architecture-blueprint.md)`

**Total**: <30 min to find rationale

---

### Scenario 3: "We're in Phase 2, and something is broken"

1. Check success criteria in `[03-implementation-roadmap.md](03-implementation-roadmap.md)` Phase 2
2. Review layer implementation in `[02-architecture-blueprint.md](02-architecture-blueprint.md)`
3. Check anti-patterns in `[00-tps-principles.md](00-tps-principles.md)` for that principle
4. Verify library configuration in `[01-library-catalog.md](01-library-catalog.md)`

**Total**: <1 hour to diagnose

---

## Key Principles Summary

| Principle | TPS Concept | Erlang/OTP | Key Library | Success Metric |
|-----------|------------|-----------|-------------|----------------|
| **JIT** | Eliminate waste | Backpressure | RabbitMQ/jobs | Queue stable, no growth |
| **Jidoka** | Quality at source | Circuit breaker | fuse/supervisor | Explicit halt on failure |
| **Kanban** | Pull-based flow | WIP limits | RabbitMQ prefetch | Work distributed evenly |
| **Andon** | Visual control | Observability | OpenTelemetry/Prometheus | Alert within 1 second |
| **Kaizen** | Continuous improvement | Metrics | Prometheus/traces | Baseline improves weekly |
| **Heijunka** | Load leveling | Rate limiting | throttle/jobs | p99 latency predictable |

---

## Integration with Existing Documentation

**This TPS Reference integrates with**:

- `/docs/00-overview/mission.md` — Government pain points, TAI 2030 mission
- `/docs/00-overview/glossary.md` — 100+ canonical terms (cross-reference)
- `/docs/00-overview/system-contract.md` — Operational contracts
- `/docs/30-autonomics/gen_statem-patterns.md` — Production FSM patterns
- `/docs/30-autonomics/governor-contract.md` — Governor FSM specification
- `/docs/30-autonomics/signal-contracts.md` — Signal protocol definition

**This TPS Reference does NOT duplicate**:
- Specific government pain point details (see mission.md)
- Erlang language fundamentals (see Erlang documentation)
- Specific SKU implementations (see examples/)

---

## Metrics & Compliance

### Document Quality

- ✅ **3000-9000 lines** total (comprehensive)
- ✅ **4 documents** (modular, re-usable)
- ✅ **31 libraries** cataloged (battle-tested, versioned)
- ✅ **6 principles** explained (TPS foundation)
- ✅ **5-layer architecture** (production-ready)
- ✅ **11-week roadmap** (phased rollout)
- ✅ **Mermaid diagrams** (visual architecture)
- ✅ **Code examples** (pseudocode, not copy-paste)
- ✅ **Risk matrices** (mitigation strategies)
- ✅ **Compliance checklist** (government-ready)

### Auditor-Ready

- ✅ Licensed software only (Apache 2.0, MIT, BSD)
- ✅ No GPL dependencies
- ✅ Version pinning strategy documented
- ✅ Fallback options for vendor lock-in prevention
- ✅ Security requirements noted (TLS, crypto, signatures)
- ✅ Compliance framework integrated (FedRAMP, FISMA references)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-25 | Initial release (complete 4-document suite) |

---

## Next Steps

1. **Read**: Choose starting document based on role (architect/engineer/devops)
2. **Discuss**: Review with team, get alignment on principles
3. **Plan**: Use roadmap to schedule 11-week implementation
4. **Execute**: Follow phase-by-phase guidance, track metrics
5. **Monitor**: Use Andon board (Grafana) to verify TPS compliance

---

**Document Status**: ✅ Complete
**Classification**: UNCLASSIFIED//FOR OFFICIAL USE ONLY
**Owner**: TAI 2030 Architecture Team
**Review Schedule**: Quarterly (next: 2026-04-25)

**Canonical Authority**: This INDEX document is the single source of truth for TPS reference documentation.
