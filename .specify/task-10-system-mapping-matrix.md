# System Mapping Matrix: Park Opening Process Integration Points

**Date**: 2026-01-18  
**Validation Wave**: Task 10  
**Scope**: Workday, SAP, ServiceNow, Slack, Weather API  

---

## System Integration Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    PARK OPENING PROCESS                         │
│  (Task 2: park-opening-process.ttl)                             │
└────┬────────────────────┬────────────────────┬─────────────────┘
     │                    │                    │
     ▼                    ▼                    ▼
  WORKDAY             SAP                  SLACK
  Shift              Equipment         Notifications
  Roster             Orders             Events
     │                │                    │
     ▼                ▼                    ▼
  wo:Shift         wo:Task/Resource     wo:Event
  wo:Event       wo:Incident/Resource
```

---

## 1. WORKDAY → Shift Management

### Data Flow

**Direction**: Read-Only (Source of Truth)  
**Latency**: Synchronous API call, <2s target  
**Frequency**: Once per opening, +5min buffer before start  

**Flow**:
```
06:00 - Execution Starts
  ↓
PreOpeningShift load trigger
  ↓
Workday API: GET /teams/SECURITY-MAIN/roster
Workday API: GET /teams/OPS-MAIN/roster
Workday API: GET /teams/GS-MAIN/roster
  ↓
Response: Staff list with ID, availability, approved leave status
  ↓
Validate: staffing >= required counts
  IF NOT: Use backup staff list OR escalate to OpsManager
  ↓
EventShiftStart → Slack #park-operations
  ↓
PreOpeningShift → StateActive
```

### Mapping Details

| Element | Workday | Work Object | Property | Confidence |
|---------|---------|---|---|---|
| **Shift Identity** | Team ID + Date | `ex:PreOpeningShift` | `wo:hasId` = "SHIFT-PRE-OPENING-001" | 95% |
| **Staff Count** | Team roster size | `ex:PreOpeningShift` | `ex:staffingRequirement[].count` | 95% |
| **Staff List** | Team members + IDs | `ex:PreOpeningShift` → `wo:shiftAssignedTo` → `ex:Person` | `ex:personId` (Workday ID) | 90% |
| **Availability** | Availability status (ON/LEAVE/SHIFT) | `wo:hasStatus` | Pending → Active (only if available) | 85% |
| **Approval Override** | Manager approval | `wo:Approval` (if needed) | New gate if staffing shortfall | 80% |

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|-----------|
| Staff on approved leave not in roster | MEDIUM | Query "availability" field; maintain backup list |
| Roster not available 30 min before start | HIGH | Implement 2-hour cache; alert ops manager |
| API timeout >5s | MEDIUM | Fallback to last known good roster + manual override |
| Staff member leaves company overnight | LOW | Assume small window; handled by backup list |
| Permission denied to query roster | CRITICAL | Pre-authorize service account before Wave 1 |

### Implementation Checklist

- [ ] Workday API credentials stored securely (before Wave 1)
- [ ] Service account has read access to SECURITY-MAIN, OPS-MAIN, GS-MAIN teams
- [ ] Backup staff list maintained in configuration
- [ ] Timeout handling implemented (fail-safe: manual escalation)
- [ ] Integration test: Load roster 100 times, verify <2s avg latency
- [ ] Staging test: Run against production Workday (read-only) before go-live

---

## 2. SAP → Equipment & Maintenance Orders

### Data Flow (Bidirectional)

**Direction**: Read (equipment status) + Write (PM order status update)  
**Latency**: Read <5s, Write <10s target  
**Frequency**: Multiple reads during EquipmentStartup; write on completion  

**Flow**:
```
07:00 - EquipmentStartup task begins
  ↓
SAP Query 1: GET /equipment/23-rides/maint-status
  Response: List of PM order IDs, current status
  ↓
IF all PM orders = "Ready for Testing" OR "Maintenance Complete"
  → Continue to startup
ELSE
  → Incident triggered: "Equipment not ready" (blocks opening)
  ↓
Startup Diagnostics Run (PT90M)
  Each attraction: power-on → self-test → report status
  ↓
07:30 - All 23 systems report green
  ↓
SAP Update: PUT /pm-orders/{order-id}/status
  Body: {"status": "Ready for Operation", "timestamp": now}
  ↓
Response: 200 OK
  ↓
EventEquipmentGreen → Slack #maintenance-ops
  ↓
OperationsApproval gate 2/3: PASS
```

### Mapping Details

| Element | SAP | Work Object | Property | Confidence |
|---------|-----|---|---|---|
| **Equipment ID** | Asset ID (e.g., SAP-20000001) | `ex:MaintenanceEquipment` | `ex:requiredItems[].sapAssetId` | 95% |
| **Availability Check** | PM order status query | `ex:EquipmentStartup` | Pre-flight check (Pending → Active) | 90% |
| **Diagnostic Status** | Self-test report (green/red) | `wo:AcceptanceCriterion` | "all 23 attractions green status" | 85% |
| **Order Update** | Mark PM order complete | `ex:EquipmentStartup` completion → `wo:StateCompleted` | Write to SAP post-task | 85% |

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|-----------|
| SAP API timeout >10s | HIGH | Implement cached fallback (last known status, age <24h) |
| Invalid equipment IDs in spec | MEDIUM | Validate SAP asset IDs against live inventory before go-live |
| PM order stuck in wrong state | MEDIUM | Manual override by Maintenance Director (async remediation) |
| Write permission denied | CRITICAL | Pre-authorize service account (SAP_GGEN_WRITE role) |
| Network latency spike | MEDIUM | Async write with retry queue (5x retry, 30s backoff) |

### Implementation Checklist

- [ ] SAP API endpoint documented (SOAP vs. REST)
- [ ] Service account has read access to ALL equipment assets
- [ ] Service account has write access to PM orders (Gap 6: audit trail required)
- [ ] Equipment asset list (23 ride IDs) validated against live SAP
- [ ] Timeout & retry logic implemented (circuit breaker pattern)
- [ ] Integration test: Query 100 assets, verify <5s response time
- [ ] Staging test: Update PM order status, verify audit trail in SAP

---

## 3. ServiceNow → Incident Management

### Data Flow (Bidirectional)

**Direction**: Write (on incident creation)  
**Latency**: Async, fire-and-forget acceptable  
**Frequency**: Only if incident occurs (should be rare)  

**Flow**:
```
IF EquipmentStartup fails at 07:15
  ↓
IncidentTemplate_EquipmentFailure triggered
  ↓
Incident created in ggen
  ID: INC-EQUIP-FAILURE-001
  Status: StateActive
  Severity: High
  ↓
ServiceNow API: POST /api/v2/incidents
  Body: {
    "title": "Park Opening - Equipment Failure",
    "description": "Attractions 5, 12 failed diagnostic",
    "severity": "P1",
    "assignment_group": "Maintenance",
    "correlate_id": "INC-EQUIP-FAILURE-001"
  }
  ↓
Response: ServiceNow ticket ID (INC001234)
  ↓
Incident.externalSystemRef = "INC001234"
  ↓
Escalate to MaintenanceDirector
  ↓
[Remediation happens outside spec]
  ↓
When repair complete: Incident → StateCompleted
  → Close ServiceNow ticket (optional async)
```

### Mapping Details

| Element | ServiceNow | Work Object | Property | Confidence |
|---------|-----------|---|---|---|
| **Incident Title** | String | `ex:IncidentTemplate_*` | `wo:incidentDescription` | 95% |
| **Severity** | P1-P4 | `wo:incidentSeverity` | "High" → P1, "Medium" → P2-3 | 90% |
| **Assignment** | Assignment group | `wo:incidentAssignedTo` | MaintenanceDirector role | 85% |
| **Remediation** | Linked change/task | `wo:incidentRemediationTask` | Reference external task | 80% |
| **Correlation ID** | External ref | `ex:externalSystemRef` | Link back to ggen incident | 75% |

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|-----------|
| ServiceNow API timeout | MEDIUM | Queue locally; async replay (dedupe by correlate_id) |
| Duplicate tickets (race condition) | MEDIUM | Use correlate_id; ServiceNow dedup on exact match |
| Webhook down (can't close ticket) | LOW | Accept open tickets; close manually next day |
| Escalation routing wrong | MEDIUM | Validate assignment_group exists before wave 1 |

### Implementation Checklist

- [ ] ServiceNow API credentials configured
- [ ] Incident creation endpoint tested (POST /api/v2/incidents)
- [ ] Severity mapping documented (ggen High → P1, Medium → P2, Low → P3)
- [ ] Assignment groups pre-created in ServiceNow
- [ ] Deduplication by correlate_id confirmed
- [ ] Async retry queue implemented (local queue if API down)
- [ ] Integration test: Create 10 incidents, verify in ServiceNow within 5s

---

## 4. Slack → Event Notifications

### Data Flow (Write-Only)

**Direction**: Write (one-way, no read back)  
**Latency**: Async acceptable (fire-and-forget)  
**Frequency**: 4 major events per opening  

**Flow**:
```
06:00 - EventShiftStart
  → Slack API: POST #park-operations
    Message: "Pre-opening shift (26 staff) on site and checked in"

06:50 - EventEquipmentGreen
  → Slack API: POST #maintenance-ops
    Message: "All 23 attractions passed diagnostics (green)"

08:00 - EventWeatherAlert (if condition degrades)
  → Slack API: POST #weather-alerts + PagerDuty ping
    Message: "Wind gust 28mph detected; escalating to OpsController"

08:10 - EventParkOpeningGate
  → Slack API: POST #company-announcements + display systems
    Message: "Park officially opened to guests"
```

### Mapping Details

| Element | Slack | Work Object | Property | Confidence |
|---------|-------|---|---|---|
| **Event Type** | Channel routing | `wo:eventType` | shift_start, milestone, alert, state_change | 95% |
| **Message** | Post content | `wo:eventMessage` | Human-readable summary | 90% |
| **Source System** | Thread origin | `wo:eventSource` | Workday, SAP, Weather API, Operations Center | 85% |
| **Channel** | #park-operations, etc. | `ex:notificationChannel` | Pre-defined set of 4 channels | 80% |
| **Escalation** | PagerDuty mention (on-call) | `ex:escalationThreshold` | **MISSING** - needs definition | 0% |

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|-----------|
| Slack API rate limit hit | LOW | Batch messages or stagger sends |
| Channel doesn't exist | MEDIUM | Pre-create all 4 channels before go-live |
| Message delivery not guaranteed | LOW | Accept; Slack is informational only |
| Wrong channel (routing error) | MEDIUM | Map event type → channel in config; test each |
| PagerDuty escalation missing | MEDIUM | **REQUIRED FOR GAP 5** - add escalation rules now |

### Implementation Checklist

- [ ] Slack bot token with write permissions to all 4 channels
- [ ] Channels exist: #park-operations, #maintenance-ops, #weather-alerts, #company-announcements
- [ ] Event → channel mapping configured
- [ ] Rate limiting handled (queue if needed)
- [ ] **NEW**: Escalation rules added for Critical incidents
- [ ] Integration test: Send 10 messages to each channel, verify all arrive
- [ ] Staging test: Monitor real park opening (day 1 Wave 1), verify notifications

---

## 5. Weather API → Environmental Monitoring

### Data Flow (Read-Only)

**Direction**: Read (external API)  
**Latency**: Async (can be slow)  
**Frequency**: Query once at 08:00, refresh if alert needed  

**Flow**:
```
07:45 - WeatherMonitoring task starts
  ↓
Weather API call: GET /conditions/{lat,lon}/current
  Provider: National Weather Service + private vendor
  ↓
Response: wind_speed, lightning_detected, precipitation, visibility
  ↓
Validate against thresholds:
  wind_speed < 25 mph? ✓
  lightning_detected = false? ✓
  precipitation < 0.1 in/min? ✓
  visibility > 0.5 miles? ✓
  ↓
IF all pass:
  WeatherApproval → StateCompleted
  ↓
IF fail (e.g., wind 28 mph):
  Incident: IncidentTemplate_WeatherEvent triggered
  ↓
  EventWeatherAlert → Slack #weather-alerts + PagerDuty
  ↓
  OpsController must decide:
    Option A: Wait for improvement (up to 2 hours)
    Option B: Delay opening (shift to 10:00 or 11:00)
    Option C: Close park for day (escalate to Park Director)
```

### Mapping Details

| Element | Weather API | Work Object | Property | Confidence |
|---------|------------|---|---|---|
| **Conditions** | Wind, precip, vis, lightning | `wo:taskAcceptanceCriteria` | "wind <25mph, no lightning, vis >0.5mi" | 95% |
| **Query Time** | Timestamp of measurement | `wo:eventTimestamp` | Implicit in API response | 90% |
| **Alert Trigger** | Threshold breach | `ex:IncidentTemplate_WeatherEvent` | Maps to Medium severity incident | 85% |
| **Decision Gate** | Weather delay approval | `ex:WeatherDelayDecision` | **MISSING** - needs explicit gate | 0% |

### Risk Assessment

| Risk | Severity | Mitigation |
|------|----------|-----------|
| API unavailable (vendor outage) | MEDIUM | Use cached data (age <30 min acceptable) + operator override |
| Data latency (report 30 min old) | MEDIUM | Re-query 5 min before gate; use multiple vendors |
| False positive (wind spike) | LOW | Use average over 5-min window, not instantaneous |
| Threshold too tight/loose | MEDIUM | Validate thresholds with ops team before Wave 1 |
| Decision authority unclear | HIGH | **REQUIRED** - define WeatherDelayDecision approval gate |

### Implementation Checklist

- [ ] Weather API endpoint(s) identified (NWS + private vendor)
- [ ] Threshold values documented & validated (wind 25 mph, visibility 0.5 mi, etc.)
- [ ] Caching mechanism implemented (30 min TTL)
- [ ] Multi-vendor data fusion logic (best data or consensus?)
- [ ] **NEW**: WeatherDelayDecision approval gate added to Task 2
- [ ] Integration test: Query API 100 times, verify <10s response time
- [ ] Staging test: Trigger weather alert (simulation), verify escalation

---

## Summary: Confidence by System

| System | Read | Write | Confidence | Rollback | Critical |
|--------|------|-------|---|---|---|
| Workday | ✓ | - | 95% | Yes | YES |
| SAP | ✓ | ✓ | 90% | Yes | YES |
| ServiceNow | - | ✓ | 85% | Yes | Medium |
| Slack | - | ✓ | 80% | No | Low |
| Weather API | ✓ | - | 75% | No | YES |

**Overall Integration Readiness**: 85% ✓

**Critical Path Dependencies**:
1. Workday roster must load successfully (blocks PreOpeningShift)
2. SAP equipment status must be readable (blocks EquipmentStartup)
3. Weather API must be queryable (blocks WeatherApproval)

**Non-Critical (informational)**:
- Slack notifications (nice-to-have)
- ServiceNow incident creation (logged but doesn't block opening)

---

## Implementation Timeline

**Phase 1 (By 2026-01-23)**: HIGH priority gaps
- Add Slack escalation rules
- Add WeatherDelayDecision approval gate
- Pre-authorize service accounts (Workday, SAP)

**Phase 2 (By 2026-02-01)**: MEDIUM priority
- Validate all API endpoints in staging
- Test timeout & retry logic
- Load test each system (100 req/s)

**Phase 3 (2026-02-01 onwards)**: Wave 1 execution & monitoring
- Monitor API latency and error rates
- Collect baseline metrics for Wave 2
- Document any manual overrides used

---

**Report Prepared By**: Integration Architect Task 10  
**Date**: 2026-01-18  
**Next Review**: Week 1 Wave 1 execution (2026-02-08)
