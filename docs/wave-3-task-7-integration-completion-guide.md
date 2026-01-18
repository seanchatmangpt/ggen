# Wave 3, Task 7: 11-System Adapter Integration Suite - Complete Implementation Guide

**Version**: 1.0.0
**Date**: 2026-01-18
**Status**: Specification Complete, Ready for Implementation
**Target Delivery**: 2026-01-31

---

## Executive Summary

This document provides the complete implementation blueprint for expanding Disney's adapter integration from 3-5 systems (Wave 2) to a full 11-system suite with proven resilience patterns. The specification ensures:

- **Zero Data Loss**: Watermark-based checkpointing for exactly-once processing
- **Clean Exit Patterns**: Remove any single adapter without affecting remaining 10
- **Read-Only Integration**: Extract-only; never modify source systems
- **Restartable State**: Survive any failure; resume from exact checkpoint
- **Production Hardening**: Circuit breakers, graceful degradation, auto-failover

**Key Metrics**:
- 11 external systems fully integrated
- 156 automated tests (unit, integration, resilience, quality, operational, compliance, performance)
- <4 minute exit procedure per adapter
- <30 second failover time with zero event loss
- 99.9% availability SLA compliance

---

## Table of Contents

1. [System Architecture](#system-architecture)
2. [The 11 Adapter Specifications](#the-11-adapter-specifications)
3. [Resilience Patterns Explained](#resilience-patterns-explained)
4. [Clean Exit Strategy](#clean-exit-strategy)
5. [Implementation Roadmap](#implementation-roadmap)
6. [Test Execution Plan](#test-execution-plan)
7. [Operational Procedures](#operational-procedures)
8. [Disaster Recovery](#disaster-recovery)
9. [Compliance & Audit](#compliance--audit)
10. [Troubleshooting Guide](#troubleshooting-guide)

---

## System Architecture

### High-Level Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      Park Operations Hub                         │
│  (Unified Dashboard, Policy Decisions, Incident Response)        │
└──────────────┬──────────────────────────────────────────────────┘
               │
       ┌───────┴───────┐
       │ Canonical     │
       │ Data Models   │
       │ + Audit Log   │
       └───────┬───────┘
               │
    ┌──────────┴──────────┐
    │   Adapter Suite     │
    │   (11 Systems)      │
    └──────────┬──────────┘
    │
    ├─ 1. Workday (HR, staffing)
    ├─ 2. SAP (Finance, operations)
    ├─ 3. Slack (Communications, audit)
    ├─ 4. ServiceNow (Ticketing, ITSM)
    ├─ 5. Ride Control (Operations, safety)
    ├─ 6. Badge Scan (Access, presence)
    ├─ 7. Ticketing (Revenue, demand)
    ├─ 8. Parking (Capacity, rates)
    ├─ 9. POS (Sales, inventory)
    ├─ 10. Compliance DB (Inspections, certs)
    └─ 11. Custom In-House (Extensible)
```

### Core Architecture Principles

#### 1. Read-Only Integration

All adapters extract data **only**. No writes to source systems.

**Enforcement**:
```rust
// Adapter trait definition
pub trait ExternalSystemAdapter {
    async fn extract_data(&self) -> Result<DataStream>;
    // ❌ No write/update/delete methods
    // ❌ No side-effects on source system
}
```

**Verification**:
- Code review: scan for zero write/update/delete operations
- Credential audit: all API keys are read-only
- Test: verify no modifications occur in sandbox

---

#### 2. Watermark-Based Checkpointing

Enables exact-once processing with zero data loss on restart.

**How it works**:
```
Event Stream: E1(offset=0) → E2(offset=1) → E3(offset=2) → ...
              ↓ Process
              Watermark saved: offset=2
              ↓
              [Adapter crashes]
              ↓
              [Adapter restarts]
              ↓
              Resume from watermark: offset=3 (next event)
              Events E4+ processed without duplicating E1-E3
```

**Implementation**:
- Redis stores watermark per adapter
- Checkpoint interval: 30-300 seconds (adapter-specific)
- Format: JSON with timestamp, offset, hash verification
- Durability: replication across 3+ Redis nodes

**Validation Test**:
```bash
# Test: Verify zero data loss on restart
1. Start adapter; process 500 events
2. Record watermark state (offset=500)
3. Stop adapter abruptly
4. Wait 10 seconds
5. Restart adapter
6. Verify: next event processed is offset=501 (no duplication)
7. Verify: events 1-500 not reprocessed
```

---

#### 3. Circuit Breaker Pattern

Prevents cascade failures when upstream systems fail.

**States**:
```
CLOSED (normal) → (failures > threshold) → OPEN (reject requests)
                                              ↓ (timeout expires)
                                          HALF_OPEN (test requests)
                                              ↓ (test passes)
                                          CLOSED (normal)
```

**Per-Adapter Configuration**:
| Adapter | Threshold | Timeout | Action |
|---------|-----------|---------|--------|
| Workday | 5 failures | 60s | Graceful degrade: return cached |
| SAP | 5 failures | 60s | Graceful degrade: mark as offline |
| RideControl | 15 failures | 30s | Graceful degrade: stream from local |

---

#### 4. Event Sourcing for Financial Accuracy

All financial transactions (POS, Ticketing, SAP) use event sourcing.

**Benefits**:
- Immutable record of every transaction
- Enables complete replay from start of time
- Supports financial reconciliation
- Audit trail proof

**Pattern**:
```
Transaction received → Validated → Stored in event log → Processed → State updated
                        ↓
                    Immutable (WORM)
                        ↓
                    Encrypted archive
```

---

## The 11 Adapter Specifications

### Adapter 1: Workday (HR, Staffing)

**Purpose**: Extract employee records, shift assignments, leave balances, callout status

**Connection**:
- Protocol: HTTPS REST
- Auth: OAuth2 (refreshable tokens)
- Rate limit: 100 req/min
- Timeout: 30s connection, 60s read

**Data Entities**:
- `EmployeeRecord`: ID, name, department, email, hire date
- `ShiftAssignment`: employee ID, shift time, location, role
- `LeaveBalance`: employee ID, accrued, used, pending days
- `CalloutStatus`: employee ID, on-call status, shift coverage

**Normalization Rules**:
- Primary key: `EmployeeID`
- Canonical schema: `CanonicalEmployeeModel`
- Validation: no null PKs, email format, department in master list
- Deduplication: by EmployeeID + timestamp

**Resilience**:
- Checkpoint interval: 60 seconds
- Retry policy: exponential backoff (1s, 2s, 4s, 8s, 16s)
- Max retries: 5
- Circuit breaker threshold: 10 consecutive failures
- Graceful degradation: return cached employee list if unavailable

**Exit Pattern** (detailed in [Clean Exit Strategy](#clean-exit-strategy)):
- Dependencies: none (no other adapters depend on Workday)
- Queue drain timeout: 60s
- Connection close timeout: 30s
- State cleanup timeout: 10s
- Total exit time: < 4 minutes
- Verification: other adapters remain operational

---

### Adapter 2: SAP (Finance, Operations)

**Purpose**: Extract equipment assets, maintenance schedules, financial transactions, operational costs

**Connection**:
- Protocol: SOAP/XML
- Auth: BasicAuth
- Rate limit: 50 req/min
- Timeout: 60s connection, 120s read

**Data Entities**:
- `EquipmentAsset`: ID, name, location, purchase date, cost, status
- `MaintenanceSchedule`: asset ID, scheduled date, work type, estimated hours
- `FinancialTransaction`: ID, amount, date, account code, description
- `OperationalCost`: asset ID, cost type, amount, period

**Normalization Rules**:
- Primary key: `AssetID`
- Canonical schema: `CanonicalAssetModel`
- Change data capture: incremental (delta) loads only
- Financial reconciliation: verify sum of extracted amounts matches ledger

**Resilience**:
- Checkpoint interval: 300 seconds (5 minutes)
- Batch execution: daily at 02:00 AM UTC
- Max retries: 3
- Circuit breaker threshold: 5 failures
- Graceful degradation: mark assets as offline; continue operations with limited visibility

**Data Quality**:
- Completeness: no null required fields
- Consistency: all asset IDs have matching cost records
- Reconciliation: extracted total = source total (±$0.01)

---

### Adapter 3: Slack (Communications, Audit Trail)

**Purpose**: Extract incident alerts, operational messages, escalations

**Connection**:
- Protocol: HTTPS REST + Webhooks
- Auth: Bearer token
- Rate limit: 200 req/min
- Webhook channels: `#incident-ops`, `#park-ops`, `#maintenance`

**Data Entities**:
- `SlackMessage`: ID, timestamp, user, text, channel
- `IncidentAlert`: alert ID, severity, description, resolution
- `Escalation`: original message, escalated to, reason

**Normalization Rules**:
- Primary key: `MessageID`
- Canonical schema: `CanonicalCommunicationModel`
- Thread preservation: maintain parent-child relationships
- User normalization: map Slack user IDs to employee IDs (via Workday)

**Resilience**:
- Checkpoint interval: 30 seconds
- Webhook ingestion: real-time
- Fallback: batch queries every 5 minutes if webhook fails
- Graceful degradation: continue without audit trail

---

### Adapter 4: ServiceNow (Ticketing, ITSM)

**Purpose**: Extract IT tickets, incident status, change requests

**Connection**:
- Protocol: HTTPS REST
- Auth: OAuth2
- Rate limit: 100 req/min
- Timeout: 30s connection, 60s read

**Data Entities**:
- `Incident`: ID, title, description, status, assignee, SLA
- `ServiceRequest`: ID, requestor, service type, status, fulfillment date
- `ChangeRequest`: ID, change type, impact, approval status

**Normalization Rules**:
- Primary key: `TicketID`
- Status mapping: ServiceNow states → canonical enum (OPEN, IN_PROGRESS, RESOLVED, CLOSED)
- Assignee verification: validate against Workday employee master

**Resilience**:
- Checkpoint interval: 120 seconds
- Retry policy: exponential backoff
- Max retries: 5
- Circuit breaker: 8 failures
- Graceful degradation: show as offline in dashboard

**SLA Tracking**:
- Extract SLA times from incidents
- Calculate remaining time at extraction
- Flag imminent breaches (< 2 hours)

---

### Adapter 5: Ride Control Systems (Operations, Safety)

**Purpose**: Extract ride operational status, safety sensor data, downtime events, guest counts

**Connection**:
- Protocol: MQTT (binary streaming)
- Auth: Client certificates
- Publish interval: 1 second (real-time)
- Topics: `park/ride/*/status`, `park/ride/*/safety`, `park/ride/*/downtime`

**Data Entities**:
- `RideStatus`: ride ID, status (running/stopped/maintenance), guests on ride
- `SafetyEvent`: sensor ID, reading, timestamp, anomaly flag
- `DowntimeEvent`: ride ID, duration, cause, impact

**Normalization Rules**:
- Primary key: `RideID`
- Time series aggregation: 5-second windows
- Anomaly detection: statistical outliers flagged
- Duplicate filtering: same ride status every 1 second → aggregated to 5-second

**Resilience**:
- Checkpoint interval: 10 seconds
- High throughput: 100 events/sec expected
- Buffer size: 50,000 events (handles brief network hiccups)
- Dual MQTT brokers: automatic failover
- Retry policy: immediate retry (no exponential backoff)
- Max retries: 10

**Safety Features**:
- Safety events never dropped; always written to immutable log
- Anomalies escalated immediately to operations center
- No buffering delay for safety-critical events

---

### Adapter 6: Badge Scan (Access, Presence)

**Purpose**: Extract badge swipes, access events, attendance records

**Connection**:
- Protocol: HTTPS REST
- Auth: API key
- Rate limit: 500 req/min
- Timeout: 10s connection, 20s read

**Data Entities**:
- `BadgeSwipe`: ID, employee ID, timestamp, location, door
- `AccessEvent`: employee ID, granted/denied, reason
- `AttendanceRecord`: employee ID, arrival time, departure time, date

**Normalization Rules**:
- Primary key: `SwipeID`
- Millisecond precision timestamps (UTC)
- Clock skew detection: flag entries where times are out of order
- Duplicate filtering: same employee, location within 10 seconds = duplicate

**Resilience**:
- Checkpoint interval: 30 seconds
- Local buffering on failure: queue badges on device
- Batch fallback: every 60 seconds
- Max batch size: 10,000 events

---

### Adapter 7: Ticketing (Revenue, Demand)

**Purpose**: Extract ticket sales, demand signals, capacity forecasts, revenue transactions

**Connection**:
- Protocol: HTTPS REST
- Auth: OAuth2
- Rate limit: 200 req/min
- Timeout: 15s connection, 45s read

**Data Entities**:
- `TicketSale`: ID, type, price, date, demand level
- `DemandSignal`: park, capacity percentage, forecast
- `RevenueTransaction`: ID, amount, type, timestamp

**Normalization Rules**:
- Primary key: `TransactionID`
- Currency normalization: convert all to USD
- Price verification: verify against current rate card
- Reconciliation: daily revenue total matches accounting ledger

**Resilience**:
- Checkpoint interval: 60 seconds
- Event sourcing: all transactions immutable
- Financial accuracy: never lose a transaction
- Max retries: 5

**Financial Controls**:
- PCI-DSS compliance: card data masked
- SOX audit trail: all transactions logged
- Daily reconciliation: extracted amount = source amount

---

### Adapter 8: Parking (Capacity, Rates)

**Purpose**: Extract lot capacity, occupancy rates, revenue, vehicle counts

**Connection**:
- Protocol: HTTPS REST
- Auth: API key
- Rate limit: 100 req/min
- Timeout: 20s connection, 40s read

**Data Entities**:
- `LotCapacity`: lot ID, total spaces, reserved, available
- `OccupancyEvent`: lot ID, timestamp, vehicle count, percentage
- `RevenueRecord`: lot ID, amount, transaction type, timestamp

**Normalization Rules**:
- Primary key: `LotID`
- Spatial calculations: available = total - occupied
- Multi-lot support: aggregate across all parking facilities

---

### Adapter 9: POS (Sales, Inventory)

**Purpose**: Extract point-of-sale transactions, inventory levels

**Connection**:
- Protocol: HTTPS REST
- Auth: OAuth2
- Rate limit: 300 req/min
- Timeout: 10s connection, 30s read

**Data Entities**:
- `Transaction`: ID, amount, items, timestamp, location
- `InventoryLevel`: SKU, quantity, location, last updated
- `StockMovement`: SKU, location, delta, reason

**Normalization Rules**:
- Primary key: `TransactionID`
- Currency: normalize to USD
- SKU standardization: canonical product IDs
- Multi-store: aggregate across all locations

**Resilience**:
- Checkpoint interval: 30 seconds
- High throughput: 50,000 transactions/sec expected
- Event sourcing: all transactions immutable
- Financial accuracy: never drop a sale

---

### Adapter 10: Compliance Database (Inspections, Certifications)

**Purpose**: Extract inspection records, safety certifications, audit results

**Connection**:
- Protocol: HTTPS REST
- Auth: OAuth2
- Rate limit: 50 req/min
- Timeout: 30s connection, 90s read

**Data Entities**:
- `InspectionRecord`: ID, asset, inspector, date, findings
- `SafetyCertification`: asset, cert type, expiration, authority
- `AuditResult`: audit ID, findings, recommendation, status

**Normalization Rules**:
- Primary key: `RecordID`
- Historical retention: all records kept permanently
- Certification date validation: ensure dates are in valid range
- Immutable: audit records never modified

**Resilience**:
- Checkpoint interval: 300 seconds (5 minutes)
- Daily batch: 03:00 AM UTC
- Archive: immutable state storage (WORM)

**Compliance**:
- SOX: all records in audit trail
- HIPAA: if employee health data present
- NFPA 70, ASTM F24: ride safety standards

---

### Adapter 11: Custom In-House Systems (Extensible)

**Purpose**: Framework for proprietary Disney systems with dynamic schema support

**Connection**:
- Protocol: HTTPS REST
- Auth: JWT tokens
- Rate limit: 200 req/min
- Custom plugin support: yes

**Data Entities**:
- `GenericMessage`: flexible schema (JSON)
- `CustomEvent`: adapter-specific event types

**Normalization Rules**:
- Schema registry: dynamic schema support
- Flexible transformation: custom rules per system
- Plugin fault isolation: error in one plugin doesn't affect others

**Extensibility**:
- Add new custom system: register in schema registry
- Custom validation rules: define in TTL
- Custom transformations: implement in Rust plugin
- Automatic discovery: plugin loaded without restart

---

## Resilience Patterns Explained

### Pattern 1: Watermark Checkpointing

**Problem**: Adapter crashes after processing 500 events, before writing final state. On restart, which 500 events reprocess?

**Solution**: Save watermark (offset) every 60 seconds. On restart, resume from saved offset.

**Watermark Format**:
```json
{
  "adapter": "Workday",
  "offset": 500,
  "timestamp": "2026-01-18T12:00:00Z",
  "last_event_id": "emp_12345",
  "hash_verification": "sha256:abcd..."
}
```

**Recovery Process**:
```
1. Adapter starts
2. Load watermark from Redis: offset=500
3. Connect to Workday API
4. Query for events after offset=500
5. Process event 501 onwards (skip 1-500)
6. Never reprocess or skip events
```

**Implementation Details**:
- Redis stores watermark (replicated across 3 nodes)
- Checkpoint written synchronously (blocking)
- Hash verification prevents corruption
- Timestamp allows audit trail queries
- Idempotent processing ensures no duplicates even if event reprocesses

---

### Pattern 2: Circuit Breaker with Graceful Degradation

**Problem**: SAP API fails for 10 consecutive requests. We don't want to spam SAP; we want to degrade gracefully.

**Solution**: Circuit breaker tracks failures. After threshold, opens (rejects new requests). Serves stale data instead.

**State Machine**:
```
CLOSED (normal)
  ↓ [5 consecutive failures]
OPEN (rejecting requests, serving cached data)
  ↓ [60-second timeout expires]
HALF_OPEN (testing if healthy)
  ↓ [test request succeeds]
CLOSED (back to normal)
```

**Per-Adapter Configuration**:

| Adapter | Threshold | Timeout | Cache | Fallback |
|---------|-----------|---------|-------|----------|
| Workday | 10 failures | 60s | employee list | stale (< 1 hour) |
| SAP | 5 failures | 60s | asset list | mark offline |
| RideControl | 15 failures | 30s | sensor readings | stream from local buffer |

**Benefits**:
- Prevents overwhelming failing systems
- Provides graceful degradation
- Auto-recovery when system heals
- Reduces noise in logs

---

### Pattern 3: Event Sourcing for Financial Accuracy

**Problem**: POS transaction extracted, but system crashes before it reaches the final state update. Did we count it? Can we reconcile?

**Solution**: Event sourcing. Every transaction immediately written to immutable event log. State updates follow.

**Pattern**:
```
Event: Transaction received
  ↓ [IMMEDIATE] Write to immutable log
  ↓ [ASYNC] Update state
  ↓ [VERIFY] Reconciliation queries immutable log

Result: 100% accuracy; no lost transactions; perfect audit trail
```

**Benefits**:
- No data loss (write happens first)
- Complete audit trail (every transaction logged)
- Replay capability (rebuild state from events)
- Financial reconciliation (audit trail = truth source)

---

### Pattern 4: Redundant Connections for Critical Systems

**Problem**: RideControl MQTT broker fails. Safety sensor readings stop arriving. Park can't verify ride safety.

**Solution**: Dual MQTT brokers (primary + secondary). Automatic failover if primary fails.

**Architecture**:
```
Adapter connects to:
  ├─ Primary MQTT broker (AWS AZ-1)
  └─ Secondary MQTT broker (AWS AZ-2)

If primary fails → automatic failover to secondary (< 5 seconds)
```

**Implementation**:
- MQTT client configured with broker list
- Automatic reconnection on connection loss
- Watermark survives broker switch (stateless failover)
- Both brokers receive all events (synchronized)

---

## Clean Exit Strategy

### The Problem

When integrating 11 systems, we want to:
1. Remove one adapter (e.g., Workday) for maintenance
2. Keep other 10 adapters running continuously
3. Zero data loss during transition
4. Resume Workday adapter without duplicating events

### The Solution: Four-Phase Exit Pattern

Every adapter implements the same four-phase exit:

#### Phase 1: Prepare for Shutdown (60 seconds)

```
adapter.shutdown_signal()
  ↓
Send NO_ACCEPT to event queue
  ↓
Stop accepting new events from upstream
  ↓
Wait for in-flight requests to complete
  ↓
Timeout: 60 seconds (hard stop after 60s)
```

**Verifies**:
- No new events accepted after signal
- In-flight work completes gracefully
- Timeout prevents indefinite hang

#### Phase 2: Drain Queue (300 seconds)

```
While queue not empty:
  ├─ Process buffered event
  ├─ Write to canonical store
  ├─ Update watermark
  └─ Repeat

Timeout: 300 seconds
```

**Verifies**:
- All buffered events processed
- Watermark saved (for recovery)
- Zero events dropped
- Audit trail complete

#### Phase 3: Close Connections (30 seconds)

```
For each connection:
  ├─ Send graceful shutdown signal
  ├─ Wait for ACK (or timeout)
  ├─ Close connection pool
  └─ Release resources

Timeout: 30 seconds
```

**Verifies**:
- Clean connection closure
- No resource leaks
- Upstream system acknowledges closure

#### Phase 4: Release State (10 seconds)

```
Delete watermark checkpoint
Signal other adapters: "Workday exiting, you're independent"
Verify other adapters running normally (health check)
Mark adapter as "offline" in monitoring
```

**Verifies**:
- State cleaned up
- No dangling references
- Other adapters remain healthy

---

### Total Exit Time

| Phase | Duration | Total |
|-------|----------|-------|
| Prepare for shutdown | 60s | 60s |
| Drain queue | 300s | 360s |
| Close connections | 30s | 390s |
| Release state | 10s | 400s |
| **Total** | - | **~400 seconds (6.7 minutes)** |

**In practice**:
- Most exits complete in 3-4 minutes
- 400s is worst-case (heavily buffered queue)
- Can force earlier exit if needed (trade-off: potential data loss)

---

### Clean Exit for Each Adapter

#### Workday Exit

```yaml
dependencies: []  # No adapters depend on Workday
phases:
  1_prepare:
    timeout: 60s
    actions:
      - Stop accepting employee events
      - Wait for in-flight Workday API calls
  2_drain:
    timeout: 300s
    actions:
      - Process buffered employee records
      - Write to employee master
      - Update watermark
  3_close:
    timeout: 30s
    actions:
      - Close OAuth2 session
      - Release connection pools
  4_release:
    timeout: 10s
    actions:
      - Delete watermark
      - Verify other adapters operational

verification:
  - All 10 remaining adapters pass health check
  - Zero employee records lost
  - Audit trail shows clean exit
```

#### ServiceNow Exit

```yaml
dependencies: []  # No adapters depend on ServiceNow
similar_to_workday:
  - Four-phase exit pattern
  - Drain ticket queue
  - Close ITSM integration
  - Verify other adapters continue
```

#### Workday-to-Badge-Scan Dependency Exit

```yaml
# If badge scan depended on Workday (it doesn't, but example):
dependencies:
  - BadgeScan reads employee IDs from Workday

exit_procedure_for_workday:
  1_notify_dependents:
    - Tell BadgeScan: "Workday shutting down"
    - BadgeScan switches to cache
  2_prepare_workday:
    - Standard shutdown
  3_verify_badge_scan:
    - Verify working independently
    - Verify using cached employee list
```

---

### Manual Rollback (If Needed)

If adapter unexpectedly fails during exit:

```bash
# 1. Check adapter health
adapter_status workday

# 2. If failed during shutdown, force restart
adapter_restart workday

# 3. Verify recovery
adapter_validate workday

# 4. Check watermark integrity
redis-cli GET adapter:workday:watermark
```

---

## Implementation Roadmap

### Week 1: Foundation (Days 1-5)

#### Day 1-2: Core Adapter Framework
```
[ ] Define ExternalSystemAdapter trait
    - extract_data() method
    - checkpoint() method
    - health_check() method
    - exit_gracefully() method
[ ] Implement watermark checkpointing
    - Redis connection pool
    - JSON serialization
    - Hash verification
[ ] Implement circuit breaker
    - State machine (CLOSED/OPEN/HALF_OPEN)
    - Failure tracking
    - Timeout mechanism
```

#### Day 2-3: First 3 Adapters (Workday, SAP, Slack)
```
[ ] Workday adapter
    [ ] OAuth2 token management
    [ ] Employee record extraction
    [ ] Schema validation
    [ ] Watermark checkpointing
[ ] SAP adapter
    [ ] SOAP/XML connection
    [ ] Asset extraction
    [ ] Financial reconciliation
    [ ] Batch scheduling (02:00 AM UTC)
[ ] Slack adapter
    [ ] Webhook ingestion
    [ ] Message parsing
    [ ] Channel filtering
    [ ] Thread preservation
```

#### Day 4-5: Testing & Hardening
```
[ ] Unit tests for each adapter
[ ] Integration tests (cross-adapter)
[ ] Resilience tests (restart, failover)
[ ] Data quality validation
```

### Week 2: Remaining Adapters (Days 6-10)

#### Day 6-7: 4 More Adapters (ServiceNow, RideControl, BadgeScan, Ticketing)
```
[ ] ServiceNow adapter (ITSM integration)
[ ] RideControl adapter (MQTT streaming, high throughput)
[ ] BadgeScan adapter (Access events, high frequency)
[ ] Ticketing adapter (Revenue, financial accuracy)
```

#### Day 8-9: Final 3 Adapters (Parking, POS, Compliance)
```
[ ] Parking adapter
[ ] POS adapter (high throughput)
[ ] Compliance adapter (historical records)
```

#### Day 10: Custom In-House Framework
```
[ ] Generic message ingestion
[ ] Schema registry support
[ ] Plugin system
[ ] Extensibility testing
```

### Week 3: Integration & Validation (Days 11-15)

#### Day 11-12: End-to-End Testing
```
[ ] All 11 adapters running simultaneously
[ ] Data flowing to canonical models
[ ] Dashboard displaying unified data
[ ] 156 automated tests passing
```

#### Day 13: Resilience Certification
```
[ ] Restart each adapter: zero data loss (11 tests)
[ ] Clean exit pattern: verified for each (11 tests)
[ ] Failover scenarios: proven (10 tests)
[ ] Circuit breaker: tested (8 tests)
```

#### Day 14: Compliance & Operational Readiness
```
[ ] Audit trail: immutable and complete
[ ] Financial reconciliation: verified
[ ] GDPR compliance: data retention tested
[ ] PCI-DSS: payment card data masked
[ ] Monitoring & alerting: operational
[ ] Runbooks: written and tested
```

#### Day 15: Documentation & Knowledge Transfer
```
[ ] Implementation guide completed
[ ] Operational runbooks written
[ ] Disaster recovery plan documented
[ ] Knowledge transfer to ops team
```

---

## Test Execution Plan

### Test Categories

| Category | Count | Time | When |
|----------|-------|------|------|
| Unit Tests | 44 | 8 min | Day 2+ (all dev sprints) |
| Integration Tests | 32 | 15 min | Day 4, 12, 14 |
| Resilience Tests | 28 | 20 min | Day 5, 13 |
| Data Quality Tests | 28 | 12 min | Day 10, 14 |
| Operational Tests | 16 | 8 min | Day 14, 15 |
| Compliance Tests | 8 | 6 min | Day 14 |
| Performance Tests | 4 | 10 min | Day 13, 15 |
| **Total** | **156** | **~45 min** | **Multiple runs** |

### Test Execution Procedure

```bash
# 1. Unit tests (fast feedback)
cargo test --lib adapters --release
# Expected: 44 tests pass in ~8 minutes

# 2. Integration tests (cross-adapter)
cargo test --test integration adapters --release
# Expected: 32 tests pass in ~15 minutes

# 3. Resilience tests (restart/failover)
./test/resilience/run_all.sh
# Expected: 28 tests pass in ~20 minutes
# Key test: each of 11 adapters restarts without data loss

# 4. Data quality tests
./test/quality/run_all.sh
# Expected: 28 tests pass in ~12 minutes
# Key test: financial reconciliation, referential integrity

# 5. Operational tests
./test/operational/run_all.sh
# Expected: 16 tests pass in ~8 minutes
# Key test: SLA compliance, alerting, runbook

# 6. Compliance tests
./test/compliance/run_all.sh
# Expected: 8 tests pass in ~6 minutes
# Key test: GDPR, SOX, PCI-DSS

# 7. Performance tests
./test/performance/run_all.sh
# Expected: 4 tests pass in ~10 minutes
# Key test: RideControl 100 events/sec, all adapters combined throughput

# 8. Full suite (all tests)
cargo make test-adapter-suite
# Expected: 156 tests, ~45 minutes, all PASS
```

### Evidence Collection

All test runs produce evidence:

```
.specify/wave3-task7-evidence/
├── test-results.json          # JSON: all test outcomes
├── test-execution.log         # Text: detailed logs
├── metrics.csv                # CSV: performance data
├── screenshots/               # PNG: dashboard, metrics
│   ├── dashboard-all-adapters.png
│   ├── adapter-workday-health.png
│   ├── restart-watermark-recovery.png
│   └── failover-scenario.png
├── architecture-diagrams/     # PlantUML/PNG: system diagrams
│   ├── adapter-suite-architecture.png
│   └── data-flow-pipeline.png
└── audit-trail-samples/       # JSON: sample audit records
    ├── workday-extraction-audit.json
    ├── sap-financial-audit.json
    └── pos-transaction-audit.json
```

---

## Operational Procedures

### Daily Operations

#### Morning Briefing (09:00 AM)

```bash
# 1. Health check all adapters
./ops/health-check-all.sh

# Expected output:
# Workday: HEALTHY (2,500 events/hour, p99=150ms)
# SAP: HEALTHY (batch completed successfully)
# Slack: HEALTHY (45 incidents/hour processed)
# ServiceNow: HEALTHY (12 tickets/hour processed)
# RideControl: HEALTHY (98.2 events/sec, streaming)
# BadgeScan: HEALTHY (250 swipes/hour processed)
# Ticketing: HEALTHY (1,200 sales/hour processed)
# Parking: HEALTHY (capacity at 65%)
# POS: HEALTHY (50,000 transactions/hour processed)
# Compliance: HEALTHY (last batch 12h ago)
# Custom: HEALTHY (3 custom systems integrated)

# 2. Check circuit breaker states
./ops/circuit-breaker-status.sh

# Expected: all CLOSED (normal operation)
# Alert if any OPEN (system failing)

# 3. Verify audit trail completeness
./ops/verify-audit-trail.sh

# Expected: all adapters logged events, no gaps
```

#### Monitoring & Alerting

```yaml
# Monitoring thresholds
alerts:
  adapter_latency_p99:
    threshold: 500ms
    action: investigate
  adapter_error_rate:
    threshold: 1%
    action: escalate
  circuit_breaker_open:
    threshold: any OPEN state
    action: page on-call
  checkpoint_age:
    threshold: 10 minutes stale
    action: alert ops
  event_loss_detected:
    threshold: any gap in watermark
    action: critical: page on-call + CTO
```

#### Evening Cleanup (17:00)

```bash
# 1. Rotate logs (keep 30 days)
./ops/rotate-logs.sh

# 2. Archive old audit trail entries (>90 days)
./ops/archive-audit-trail.sh

# 3. Verify Redis checkpoint backups
./ops/verify-redis-backups.sh

# 4. Generate daily report
./ops/daily-report.sh
```

---

### Weekly Maintenance

#### Monday: Resilience Testing

```bash
# Test: Restart each adapter; verify zero data loss

for adapter in workday sap slack servicenow ridecontrol badgescan ticketing parking pos compliance custom
do
  echo "Testing restart: $adapter"

  # Get current watermark
  watermark_before=$(redis-cli GET adapter:$adapter:watermark)

  # Stop adapter
  adapter_stop $adapter

  # Wait 30 seconds
  sleep 30

  # Restart adapter
  adapter_start $adapter

  # Verify watermark (should resume from exact point)
  watermark_after=$(redis-cli GET adapter:$adapter:watermark)

  if [ "$watermark_before" == "$watermark_after" ]; then
    echo "✓ $adapter restart: SUCCESS (watermark preserved)"
  else
    echo "✗ $adapter restart: FAILED (watermark changed)"
  fi
done
```

#### Thursday: Failover Testing

```bash
# Test: Simulate primary adapter failure; secondary takes over

for adapter in critical_adapters  # Workday, SAP, RideControl
do
  # Stop primary instance
  docker stop adapter-$adapter-primary

  # Verify secondary detects failure (10s timeout)
  sleep 10

  # Check secondary is now active
  if [ "$(adapter_get_active_instance $adapter)" == "secondary" ]; then
    echo "✓ Failover $adapter: SUCCESS"
  else
    echo "✗ Failover $adapter: FAILED"
  fi

  # Resume primary
  docker start adapter-$adapter-primary
done
```

---

### Monthly Audit

#### Compliance Audit

```bash
# 1. Verify immutable audit trail
./ops/verify-audit-immutability.sh

# 2. Check financial reconciliation
./ops/financial-reconciliation.sh

# 3. GDPR data retention verification
./ops/verify-data-retention.sh

# 4. Generate compliance report
./ops/compliance-audit-report.sh
```

---

## Disaster Recovery

### Scenario 1: Complete Adapter Suite Failure

**Cause**: Corrupted Redis checkpoint database; all adapters unable to recover state.

**Recovery Procedure**:

```bash
# 1. Assess damage (1 minute)
adapter_status all
redis-cli DBSIZE  # Check Redis health

# 2. Restore Redis from backup (5 minutes)
redis_restore /backups/redis-checkpoint-2026-01-18-11:00.rdb
redis-cli INFO replication  # Verify restore

# 3. Restart all adapters (2 minutes)
for adapter in workday sap slack servicenow ridecontrol badgescan ticketing parking pos compliance custom
do
  adapter_start $adapter
  sleep 10  # Wait for startup
done

# 4. Verify recovery (5 minutes)
./ops/health-check-all.sh
./ops/verify-audit-trail.sh

# 5. Notify stakeholders
notify_team "Adapter suite recovered from Redis failure"
```

**Total RTO** (Recovery Time Objective): ~15 minutes
**Total RPO** (Recovery Point Objective): Latest backup (depends on backup frequency)

---

### Scenario 2: Cascading Failure (One Adapter Brings Down Others)

**Cause**: Workday adapter memory leak causes massive buffering; consumes system resources; other adapters starve.

**Prevention**:
- Memory limits per adapter (via container/process limits)
- Resource isolation: each adapter in separate container
- Monitoring: memory usage alerts at 70%, 85%, 95%

**Recovery**:

```bash
# 1. Detect cascading failure
./ops/cascade-detection.sh
# Output: "Workday using 85% of memory; other adapters at 20%"

# 2. Isolate failing adapter
adapter_isolate workday
# Immediately stops resource consumption

# 3. Restart healthy adapters
for adapter in sap slack servicenow ridecontrol badgescan ticketing parking pos compliance custom
do
  adapter_restart $adapter
done

# 4. Investigate Workday
# Check logs: why memory bloat?
# Hypothesis: checkpoint queue unbounded
tail -100 logs/workday-adapter.log

# 5. Fix and redeploy
# Patch: limit checkpoint queue to 10,000 events
cargo build --release adapters::workday
docker push adapter-workday:latest
adapter_start workday

# 6. Verify
adapter_status all
```

**Total RTO**: ~5 minutes (isolated failure won't cascade)

---

### Scenario 3: Data Loss Detected (Missing Events in Audit Trail)

**Cause**: Watermark checkpoint corrupted; event gap detected (offset jumped from 500 to 550, missing 1-49).

**Detection**:

```bash
# Monitoring detects gap
./ops/watermark-integrity-check.sh

# Output: "⚠ Workday adapter: watermark gap detected (500→550, missing 49 events)"
```

**Recovery**:

```bash
# 1. Immediately alert
notify_team "CRITICAL: Workday event loss detected (49 events)"
escalate_to_cto

# 2. Stop affected adapter
adapter_stop workday

# 3. Query event archive (immutable log)
# Find missing events
select * from event_log
where adapter='workday' and offset >= 501 and offset <= 549

# Result: 49 events found in immutable log

# 4. Replay events from archive
./ops/replay-events-from-archive.sh workday 501 549

# 5. Verify replay
# Check canonical employee store: should now have 549 records
redis-cli DBSIZE adapter:workday:canonical

# 6. Update watermark
redis-cli SET adapter:workday:watermark '{"offset": 549, ...}'

# 7. Resume adapter
adapter_start workday

# 8. Audit & notify
audit_trail.log "Event loss detected and recovered: 49 events replayed from archive"
notify_team "Data recovery complete; all 49 events restored"
```

**Total RTO**: ~10 minutes
**Data Loss**: 0 (recovery from immutable archive)

---

## Compliance & Audit

### Regulatory Frameworks Covered

| Framework | Coverage | Adapter(s) |
|-----------|----------|-----------|
| **SOX** (Financial) | All financial transactions logged, immutable | POS, Ticketing, SAP |
| **GDPR** (Privacy) | Data retention <90 days; deletion logged | All adapters |
| **PCI-DSS** (Payments) | Card data masked; encryption enforced | POS, Ticketing |
| **HIPAA** (Health) | If employee health data present | Workday |
| **NFPA 70, ASTM F24** (Safety) | Ride safety compliance | RideControl, Compliance DB |
| **ISO 20000** (ITSM) | ServiceNow data completeness | ServiceNow |

---

### Audit Trail Structure

Every adapter maintains an immutable audit trail:

```json
{
  "event_id": "evt_12345",
  "adapter": "Workday",
  "timestamp": "2026-01-18T12:00:00Z",
  "source_record_id": "emp_5678",
  "action": "extract",
  "validation_rules_applied": [
    "required_fields_present",
    "email_format_valid",
    "department_in_master"
  ],
  "validation_passed": true,
  "canonical_output": {
    "employee_id": "emp_5678",
    "name": "Jane Doe",
    "department": "Operations"
  },
  "hash_verification": "sha256:abcd...",
  "regulatory_compliance": ["GDPR", "SOX"],
  "retention_until": "2026-04-18"
}
```

**Properties**:
- Immutable: no updates/deletes allowed
- WORM storage: write-once, read-many
- Encrypted: AES-256 at rest
- Timestamped: UTC with millisecond precision
- Hashed: cryptographic proof of integrity
- Retention: tracked and auto-deleted per GDPR

---

## Troubleshooting Guide

### Problem: Adapter Stuck in OPEN (Circuit Breaker)

**Symptoms**:
```
⚠ Workday adapter status: OPEN (circuit breaker open)
⚠ Latency: N/A (rejecting all requests)
⚠ Events processed: 0/hour
```

**Root Causes**:
1. Source system unavailable
2. Network connectivity issue
3. Authentication failure
4. Rate limiting

**Diagnosis**:

```bash
# 1. Check source system connectivity
curl -v https://workday-api.example.com/employees
# HTTP 200? → system up
# HTTP 503? → system down
# HTTP 401? → auth failed

# 2. Check network from adapter
docker exec adapter-workday ping workday-api.example.com
# Response? → network OK
# Timeout? → network issue

# 3. Check authentication
docker logs adapter-workday | grep -i "auth\|token\|401\|403"

# 4. Check rate limiting
docker logs adapter-workday | grep -i "rate\|429\|too many"

# 5. Check circuit breaker state
redis-cli GET adapter:workday:circuit_breaker
# Output: {"state": "OPEN", "failures": 15, "opened_at": "2026-01-18T12:00:00Z"}
```

**Resolution**:

```bash
# If source system down:
# → Wait for source to recover; circuit breaker auto-closes after timeout

# If auth failed:
# → Check credentials
# → Refresh OAuth2 token
# → Redeploy adapter with new credentials

# If rate limiting:
# → Verify rate limit in spec (100 req/min)
# → Backoff and retry
# → May need to request rate limit increase with vendor

# Force circuit breaker reset (last resort):
redis-cli SET adapter:workday:circuit_breaker '{"state": "CLOSED"}'
adapter_restart workday
```

---

### Problem: Data Loss Detected (Event Gap)

**Symptoms**:
```
⚠ Workday adapter: watermark integrity check FAILED
⚠ Watermark jumped from 500 to 550 (missing 49 events)
⚠ Events potentially lost!
```

**Diagnosis**:

```bash
# 1. Check watermark history
redis-cli LRANGE adapter:workday:watermark_history 0 5

# 2. Query immutable event log
select count(*) from event_log
where adapter='workday' and offset >= 500 and offset <= 550

# 3. Check for corruption
redis-cli --scan --pattern adapter:workday:* | xargs redis-cli DUMP

# 4. Check system logs for crashes
docker logs adapter-workday | tail -50
systemctl status adapter-workday
```

**Recovery**:

```bash
# 1. STOP affected adapter (prevent further damage)
adapter_stop workday

# 2. Restore watermark from backup
redis_restore_key adapter:workday:watermark /backups/redis-2026-01-18-11:00.rdb

# 3. Replay missing events from immutable log
./ops/replay-events-from-archive.sh workday 501 549

# 4. Verify recovery
select count(*) from canonical_models.employees
# Should match Workday source count

# 5. Resume adapter
adapter_start workday

# 6. Root cause analysis
# Why was watermark corrupted?
# → Memory issue? Check logs
# → Disk issue? Check storage health
# → Bug in checkpoint code? Review and fix
```

---

### Problem: High Latency (p99 > 500ms)

**Symptoms**:
```
⚠ Workday adapter: p99 latency = 850ms (SLA: 500ms)
⚠ Dashboard slow to load
⚠ Queries timing out
```

**Diagnosis**:

```bash
# 1. Check adapter metrics
prometheus_query 'adapter_latency_p99{adapter="workday"}'
# Shows latency trend

# 2. Check upstream source
curl -w "@timing.txt" https://workday-api.example.com/employees
# Measure Workday API latency

# 3. Check network
ping workday-api.example.com
# High latency? → network issue

# 4. Check adapter resource usage
docker stats adapter-workday
# CPU > 80%? → CPU bound
# Memory > 70%? → Memory pressure

# 5. Check queue depth
redis-cli LLEN adapter:workday:event_queue
# Large queue? → buffering problem
```

**Resolution**:

```bash
# If upstream slow:
# → Check with Workday: scheduled maintenance?
# → Reduce query load
# → Use API caching

# If network slow:
# → Check routing
# → Check ISP connectivity
# → Consider regional failover

# If adapter CPU bound:
# → Increase parallelism
# → Optimize normalization rules
# → Profile code: where is time spent?

# If adapter memory pressure:
# → Reduce batch size
# → Implement streaming instead of batch
# → Add memory limits (prevent growth)

# If queue backed up:
# → Increase throughput (use streaming, parallelism)
# → Reduce batch size
# → Add more adapter workers
```

---

### Problem: Failover Not Triggering (Primary Fails, Secondary Idle)

**Symptoms**:
```
⚠ Primary adapter-workday-1 crashed
⚠ Secondary adapter-workday-2 not taking over
⚠ Events not processed
```

**Diagnosis**:

```bash
# 1. Check health check configuration
docker inspect adapter-workday-1 | grep -A 10 '"HealthCheck"'

# 2. Check health check status
docker inspect adapter-workday-1 | grep '"Status"'
# Should show "unhealthy" after primary fails

# 3. Check secondary status
docker inspect adapter-workday-2
# Should show "running" but check is "healthy"

# 4. Check failover logic
docker logs adapter-workday-2 | grep -i "failover\|leader\|active"

# 5. Check shared state (lease/lock)
redis-cli GET adapter:workday:active_instance
# Should show secondary after primary fails
```

**Resolution**:

```bash
# 1. Verify health check timeout
# Default: 10 seconds
# May be too short; increase if needed

# 2. Manually trigger failover (if auto-failover broken)
redis-cli SET adapter:workday:active_instance secondary
adapter_stop workday-primary
adapter_start workday-secondary

# 3. Resume primary (for future failback)
sleep 30  # Wait for secondary stable
adapter_start workday-primary

# 4. Review failover logic
# Is lease/lock being acquired by secondary?
# Is shared state being updated?
# Are timeouts configured correctly?

# 5. Test failover
./test/failover/test-workday-failover.sh
```

---

## Conclusion

This specification provides everything needed to implement a production-grade 11-system adapter suite with proven resilience, zero data loss, and clean exit patterns.

**Key Deliverables**:
1. ✓ `.specify/adapter-suite-complete.ttl` - Full RDF specification
2. ✓ `.specify/adapter-integration-tests.ttl` - 156 automated tests
3. ✓ This implementation guide

**Next Steps**:
1. Review specification with architecture team
2. Obtain security and compliance sign-off
3. Execute implementation roadmap (Weeks 1-3)
4. Validate all 156 tests passing
5. Deploy to production
6. Monitor for 30 days; adjust as needed

**Success Criteria**:
- All 11 adapters operational
- All 156 tests passing
- Zero data loss proven
- Clean exit pattern validated for all adapters
- 99.9% availability SLA maintained
- Complete audit trail for compliance

**Questions or Issues?**
Contact: Integration Architecture Team
Review Spec: `.specify/adapter-suite-complete.ttl`
Run Tests: `cargo make test-adapter-suite`
