<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Wave 3, Task 7: Quick Reference Sheet](#wave-3-task-7-quick-reference-sheet)
  - [Specification Files](#specification-files)
  - [The 11 Adapters (Instant Reference)](#the-11-adapters-instant-reference)
  - [Core Resilience Patterns](#core-resilience-patterns)
    - [1. Watermark Checkpointing (All Adapters)](#1-watermark-checkpointing-all-adapters)
    - [2. Circuit Breaker (All Adapters)](#2-circuit-breaker-all-adapters)
    - [3. Event Sourcing (POS, Ticketing, SAP Financial)](#3-event-sourcing-pos-ticketing-sap-financial)
    - [4. Dual MQTT Brokers (RideControl Only)](#4-dual-mqtt-brokers-ridecontrol-only)
  - [Clean Exit Pattern (All Adapters)](#clean-exit-pattern-all-adapters)
  - [156 Automated Tests (By Category)](#156-automated-tests-by-category)
  - [Implementation Timeline (3 Weeks)](#implementation-timeline-3-weeks)
    - [Week 1: Foundation + 3 Adapters](#week-1-foundation--3-adapters)
    - [Week 2: Remaining 8 Adapters](#week-2-remaining-8-adapters)
    - [Week 3: Validation + Hardening](#week-3-validation--hardening)
  - [Key SLAs & Thresholds](#key-slas--thresholds)
    - [Adapter Latency (p99)](#adapter-latency-p99)
    - [Circuit Breaker Thresholds](#circuit-breaker-thresholds)
    - [Checkpoint Intervals](#checkpoint-intervals)
  - [Operational Commands (Ready for Implementation)](#operational-commands-ready-for-implementation)
    - [Health & Status](#health--status)
    - [Testing](#testing)
    - [Operational](#operational)
    - [Disaster Recovery](#disaster-recovery)
  - [Compliance Coverage](#compliance-coverage)
  - [Common Issues & Quick Fixes](#common-issues--quick-fixes)
    - [Issue: Adapter Stuck in OPEN Circuit Breaker](#issue-adapter-stuck-in-open-circuit-breaker)
    - [Issue: Data Loss Detected (Watermark Gap)](#issue-data-loss-detected-watermark-gap)
    - [Issue: High Latency (p99 > SLA)](#issue-high-latency-p99--sla)
  - [Success Criteria Checklist](#success-criteria-checklist)
    - [Development](#development)
    - [Validation](#validation)
    - [Operations](#operations)
    - [Compliance](#compliance)
  - [Key Contacts](#key-contacts)
  - [Documentation Map](#documentation-map)
  - [Start Here](#start-here)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Wave 3, Task 7: Quick Reference Sheet

**Project**: ggen-disney Park Operations - 11-System Adapter Integration
**Status**: Specification Complete - Ready for Implementation
**Timeline**: 3 weeks (Jan 20-31, 2026)

---

## Specification Files

| File | Size | Lines | Purpose |
|------|------|-------|---------|
| `.specify/adapter-suite-complete.ttl` | 33K | 906 | Primary RDF specification (11 adapters, resilience, clean exit) |
| `.specify/adapter-integration-tests.ttl` | 28K | 554 | Test specification (156 automated tests) |
| `docs/wave-3-task-7-integration-completion-guide.md` | 44K | 1644 | Implementation guide + operations manual |
| `docs/WAVE-3-TASK-7-INDEX.md` | TBD | TBD | Master index (this delivery) |
| `docs/WAVE-3-TASK-7-QUICK-REFERENCE.md` | TBD | TBD | Quick reference (this file) |

---

## The 11 Adapters (Instant Reference)

```
┌─ 1. Workday ────────────→ HR, Staffing (OAuth2/REST, 100 req/min, P1)
├─ 2. SAP ─────────────────→ Finance, Ops (SOAP/XML, 50 req/min, P1)
├─ 3. Slack ───────────────→ Communications (HTTPS/REST, 200 req/min, P2)
├─ 4. ServiceNow ──────────→ Ticketing, ITSM (OAuth2/REST, 100 req/min, P1)
├─ 5. RideControl ────────→ Operations, Safety (MQTT, streaming, P1) 🚨
├─ 6. BadgeScan ───────────→ Access, Presence (API Key, 500 req/min, P1)
├─ 7. Ticketing ───────────→ Revenue, Demand (OAuth2/REST, 200 req/min, P1) 💰
├─ 8. Parking ──────────────→ Capacity, Rates (API Key, 100 req/min, P2)
├─ 9. POS ─────────────────→ Sales, Inventory (OAuth2/REST, 300 req/min, P1) 💰
├─10. Compliance ──────────→ Inspections, Certs (OAuth2/REST, 50 req/min, P1)
└─11. Custom In-House ─────→ Extensible Framework (JWT, 200 req/min, P2)

🚨 = Safety-critical, 💰 = Financial accuracy (event sourcing)
```

---

## Core Resilience Patterns

### 1. Watermark Checkpointing (All Adapters)

```
Problem: Adapter crashes after processing 500 events. Which events reprocess?
Solution: Save watermark every 60 seconds. Resume from saved offset. Zero duplicates.

Process:
  Event 1-500 → Watermark(500) saved to Redis
               ↓ [CRASH]
               ↓ [RESTART]
               ↓ Load watermark = 500
  Event 501+  → Continue from 501 (skip 1-500)

Guarantee: Exactly-once processing, zero data loss
```

### 2. Circuit Breaker (All Adapters)

```
Problem: SAP API fails. We spam it with retries, making it worse.
Solution: Circuit breaker opens after 5 consecutive failures. Serves cached data.

States:
  CLOSED (normal)
    → [5 failures]
      OPEN (serving cache, not contacting SAP)
        → [60s timeout]
          HALF_OPEN (test request)
            → [success]
              CLOSED (back to normal)
```

### 3. Event Sourcing (POS, Ticketing, SAP Financial)

```
Problem: Transaction extracted, system crashes, did we count it?
Solution: Immutable event log. Transaction written FIRST, state updated SECOND.

Pattern:
  Transaction → Write immutable log [SYNC] → Update state [ASYNC] → Reconciliation

Guarantee: 100% financial accuracy, complete audit trail, zero lost transactions
```

### 4. Dual MQTT Brokers (RideControl Only)

```
Problem: RideControl MQTT broker fails. Safety sensor readings stop.
Solution: Automatic failover to secondary broker.

Setup:
  Primary (AZ-1) + Secondary (AZ-2) → Adapter connects to both
  Automatic failover: < 5 seconds, zero event loss
```

---

## Clean Exit Pattern (All Adapters)

```
Problem: Remove Workday adapter for maintenance. Keep 10 others running.
Solution: 4-phase exit procedure. ~6 minutes. Zero side effects.

Phase 1: Prepare (60s)
  ├─ Send NO_ACCEPT to event queue
  ├─ Wait for in-flight requests
  └─ Timeout: hard stop at 60s

Phase 2: Drain Queue (300s)
  ├─ Process all buffered events
  ├─ Write to canonical store
  └─ Update watermark

Phase 3: Close Connections (30s)
  ├─ Graceful shutdown signals
  ├─ Release connection pools
  └─ Timeout: 30s

Phase 4: Release State (10s)
  ├─ Delete watermark
  ├─ Signal other adapters
  └─ Verify others operational

Total: ~400 seconds (6.7 min worst-case)
```

---

## 156 Automated Tests (By Category)

| Category | Count | Timing | Focus |
|----------|-------|--------|-------|
| **Unit** | 44 | 8 min | Adapter-specific: connection, auth, schema |
| **Integration** | 32 | 15 min | Cross-adapter: workflows, shared state |
| **Resilience** | 28 | 20 min | Restart, failover, chaos (critical!) |
| **Data Quality** | 28 | 12 min | Validation, anomaly detection, reconciliation |
| **Operational** | 16 | 8 min | SLA compliance, monitoring, incident response |
| **Compliance** | 8 | 6 min | GDPR, SOX, PCI-DSS, audit trail |
| **Performance** | 4 | 10 min | Throughput, scaling, resource usage |
| **TOTAL** | **156** | **~45 min** | **Full validation** |

---

## Implementation Timeline (3 Weeks)

### Week 1: Foundation + 3 Adapters
```
Day 1-2: Core framework (watermark, circuit breaker, health check)
Day 2-3: Workday, SAP, Slack adapters
Day 4-5: Unit tests (44), resilience tests start

Deliverable: 3 adapters proven, 44 unit tests passing
```

### Week 2: Remaining 8 Adapters
```
Day 6-7: ServiceNow, RideControl, BadgeScan, Ticketing
Day 8-9: Parking, POS, Compliance, Custom In-House
Day 10: Integration tests (32 tests)

Deliverable: All 11 adapters running, 76 tests passing
```

### Week 3: Validation + Hardening
```
Day 11-12: End-to-end testing (all 156 tests)
Day 13: Resilience certification (28 tests)
Day 14: Compliance & operational readiness
Day 15: Documentation + knowledge transfer

Deliverable: Production-ready, all tests passing, operations trained
```

---

## Key SLAs & Thresholds

### Adapter Latency (p99)

| Adapter | SLA | Threshold |
|---------|-----|-----------|
| Workday | 500ms | Alert at 350ms |
| SAP | 1000ms | Alert at 750ms |
| Slack | 200ms | Alert at 150ms |
| ServiceNow | 500ms | Alert at 350ms |
| RideControl | 100ms | Alert at 75ms 🚨 |
| BadgeScan | 200ms | Alert at 150ms |
| Ticketing | 500ms | Alert at 350ms |
| Parking | 400ms | Alert at 300ms |
| POS | 300ms | Alert at 200ms |
| Compliance | 1000ms | Alert at 750ms |

### Circuit Breaker Thresholds

| Adapter | Failure Threshold | Timeout |
|---------|-------------------|---------|
| Workday | 10 failures | 60s |
| SAP | 5 failures | 60s |
| Slack | 20 failures | 30s |
| ServiceNow | 8 failures | 60s |
| RideControl | 15 failures | 30s |
| BadgeScan | 12 failures | 30s |
| Ticketing | 10 failures | 60s |
| Parking | 10 failures | 60s |
| POS | 12 failures | 60s |
| Compliance | 5 failures | 60s |

### Checkpoint Intervals

| Adapter | Interval | Reason |
|---------|----------|--------|
| Workday | 60s | Moderate throughput |
| SAP | 300s | Batch mode, low frequency |
| Slack | 30s | Webhook mode, high responsiveness |
| ServiceNow | 120s | Event-based |
| RideControl | 10s | Streaming, real-time criticality |
| BadgeScan | 30s | Access events, frequent |
| Ticketing | 60s | Revenue-critical, must be reliable |
| Parking | 60s | Capacity tracking |
| POS | 30s | High transaction volume |
| Compliance | 300s | Daily batch |

---

## Operational Commands (Ready for Implementation)

### Health & Status

```bash
# Check all adapters
./ops/health-check-all.sh

# Check specific adapter
adapter_status workday

# Circuit breaker state
./ops/circuit-breaker-status.sh

# Watermark integrity
./ops/watermark-integrity-check.sh
```

### Testing

```bash
# Run all 156 tests
cargo make test-adapter-suite

# Run specific category
cargo test --lib adapters::resilience --release

# Run resilience tests (weekly)
./test/resilience/run_all.sh

# Generate evidence
./test/generate-evidence.sh
```

### Operational

```bash
# Restart adapter (with zero downtime)
./ops/adapter-restart workday

# Emergency: Force clean exit
./ops/adapter-exit-force workday

# Verify other adapters unaffected
./ops/verify-other-adapters-operational

# Daily report
./ops/daily-report.sh

# Monthly audit
./ops/compliance-audit-report.sh
```

### Disaster Recovery

```bash
# Failover to secondary
./dr/initiate-failover workday

# Restore from backup
./dr/restore-from-backup adapter:workday

# Replay events from archive
./dr/replay-events workday 501 549

# Full DR test (quarterly)
./dr/quarterly-test.sh
```

---

## Compliance Coverage

| Requirement | Coverage | Adapter(s) |
|-------------|----------|-----------|
| **SOX** (financial audit trail) | Immutable event log | POS, Ticketing, SAP |
| **GDPR** (data retention <90 days) | Auto-deletion + logging | All adapters |
| **PCI-DSS** (payment card masking) | Mask in transit + logs | POS, Ticketing |
| **HIPAA** (health data) | Encryption, access control | Workday (if applicable) |
| **NFPA 70** (electrical safety) | Real-time monitoring | RideControl |
| **ASTM F24** (ride safety) | Historical records + verification | RideControl, Compliance |
| **ISO 20000** (ITSM) | Complete audit trail | ServiceNow |

---

## Common Issues & Quick Fixes

### Issue: Adapter Stuck in OPEN Circuit Breaker

```bash
# Diagnose
curl https://upstream-api.example.com/health

# If upstream down: wait for recovery
# If upstream up: check credentials
grep -i "auth\|token" /var/log/adapter-workday.log

# Force reset (last resort)
redis-cli SET adapter:workday:circuit_breaker '{"state":"CLOSED"}'
adapter_restart workday
```

### Issue: Data Loss Detected (Watermark Gap)

```bash
# Stop adapter immediately
adapter_stop workday

# Check immutable log
select * from event_log where adapter='workday' and offset between 500 and 550

# Replay missing events
./ops/replay-events-from-archive.sh workday 501 549

# Resume
adapter_start workday
```

### Issue: High Latency (p99 > SLA)

```bash
# Check adapter CPU
docker stats adapter-workday

# Check upstream latency
curl -w "@timing.txt" https://workday-api.example.com

# Check queue depth
redis-cli LLEN adapter:workday:event_queue

# Fix: increase parallelism, reduce batch size, optimize rules
```

---

## Success Criteria Checklist

### Development
- [ ] All 11 adapters implemented
- [ ] All 156 tests passing
- [ ] Code review approval
- [ ] Performance benchmarks established

### Validation
- [ ] Unit tests: 44/44 passing
- [ ] Integration tests: 32/32 passing
- [ ] Resilience tests: 28/28 passing
- [ ] Restart: all 11 adapters zero data loss ✓
- [ ] Clean exit: all 11 adapters independent ✓
- [ ] Failover: <30s with zero loss ✓

### Operations
- [ ] Monitoring configured
- [ ] Alerting validated
- [ ] Runbooks tested
- [ ] On-call team trained
- [ ] Disaster recovery procedures tested

### Compliance
- [ ] Audit trail complete
- [ ] Financial reconciliation verified
- [ ] GDPR compliance confirmed
- [ ] SOX audit trail immutable
- [ ] Security review passed

---

## Key Contacts

| Role | Responsibility | Contact |
|------|-----------------|---------|
| Integration Architecture Lead | Specification, design approval | (Project Leadership) |
| Streaming Systems Architect | RideControl MQTT design | (Project Team) |
| DevOps/SRE Lead | Infrastructure, monitoring | (Operations Team) |
| Security/CISO | Compliance validation | (Security Team) |
| On-Call Engineer | Daily operations | (Ops Roster) |

---

## Documentation Map

```
Primary Specification (RDF/Turtle):
  └─ .specify/adapter-suite-complete.ttl (906 lines)
     ├─ 11 adapter definitions
     ├─ Resilience patterns
     ├─ Clean exit strategies
     └─ Specification closure

Test Specification (RDF/Turtle):
  └─ .specify/adapter-integration-tests.ttl (554 lines)
     ├─ 156 test definitions
     ├─ 7 test categories
     └─ Evidence requirements

Implementation Guide (Markdown):
  └─ docs/wave-3-task-7-integration-completion-guide.md (1644 lines)
     ├─ Chapter 1: Executive Summary
     ├─ Chapter 2: System Architecture
     ├─ Chapter 3: The 11 Adapters
     ├─ Chapter 4: Resilience Patterns
     ├─ Chapter 5: Clean Exit Strategy
     ├─ Chapter 6: Implementation Roadmap
     ├─ Chapter 7: Test Execution Plan
     ├─ Chapter 8: Operational Procedures
     ├─ Chapter 9: Disaster Recovery
     ├─ Chapter 10: Compliance & Audit
     └─ Chapter 11: Troubleshooting Guide

Master Index:
  └─ docs/WAVE-3-TASK-7-INDEX.md
     ├─ Specification overview
     ├─ Implementation roadmap
     └─ Success criteria

Quick Reference:
  └─ docs/WAVE-3-TASK-7-QUICK-REFERENCE.md (This file)
     ├─ Adapter summary
     ├─ Patterns cheatsheet
     ├─ Test matrix
     └─ Commands reference
```

---

## Start Here

1. **First time?** → Read `docs/WAVE-3-TASK-7-INDEX.md` (master overview)
2. **Implementing?** → Follow `docs/wave-3-task-7-integration-completion-guide.md`
3. **Operations?** → Jump to Chapter 8 of implementation guide
4. **Need quick answer?** → Check this quick reference
5. **Deep dive?** → Review `.specify/adapter-suite-complete.ttl` (RDF spec)

---

**Last Updated**: 2026-01-18
**Status**: Specification Complete ✓
**Ready for Implementation**: YES ✓
