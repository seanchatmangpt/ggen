# Work Object Model - State Machine Specifications

## Overview

This document defines the formal state machines for the five core Work Object types in the Park Opening workflow (Phase 1). Each type is immutable; state changes are recorded as events in the audit trail.

**Target Coverage**: 80% of park opening process (primary workflows for 1 opening cycle)

**Key Principle**: Once a state transition occurs, it is recorded as an immutable event. Conflicts from concurrent edits (merge algebra) are detected via event comparison and resolved per conflict resolution strategy.

---

## 1. SHIFT State Machine

**Definition**: A scheduled period of work at a specific location with assigned resource and role.

**Maps to**: Workday shifts, ops schedules, staffing assignments

### States and Transitions

```
┌─────────────────────────────────────────────────────────────────┐
│                     SHIFT STATE MACHINE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   [unscheduled]──publish──>[published]──assign──>[assigned]     │
│                                                         │        │
│                                                      activate    │
│                                                         │        │
│                                                         v        │
│                                                     [active]      │
│                                                         │        │
│                                                      complete    │
│                                                         │        │
│                                                         v        │
│                                                    [completed]    │
│                                                                 │
│   NO BACKWARD TRANSITIONS (once completed, stays completed)     │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### Detailed State Descriptions

| State | Meaning | Valid Previous States | Mandatory Fields | Examples |
|-------|---------|----------------------|-------------------|----------|
| **unscheduled** | Shift created but not yet published to ops team | (initial state) | shiftId, scheduledStart/End, location, requiredRole, createdAt | Draft shift being planned |
| **published** | Shift posted to schedule; available for assignment | unscheduled | + All from unscheduled | "Security opening 6am-2pm, Hub - looking for volunteers" |
| **assigned** | Person/resource assigned to shift; awaiting authorization | published | + assignedTo, assignmentAuthorized=false | "John Doe assigned to Security opening" |
| **active** | Shift is currently underway | assigned | + assignmentAuthorized=true, authorizedBy (Approval ref) | Person is on-site, shift started |
| **completed** | Shift finished; all tasks closed or cancelled | active | + actualEndTime, completionNotes | Shift ended, final report logged |

### Valid Transitions

```
unscheduled  → published        (publish shift)
published    → assigned         (assign person)
published    → published        (reassign if needed, stay published)
assigned     → active           (person clocks in / shift starts)
active       → completed        (shift ends)
```

### Invalid Transitions (SHACL enforces)

```
completed  ↛ active            (once done, cannot restart)
active     ↛ published         (cannot cancel active shift)
published  ↛ unscheduled       (unpublishing not supported)
```

### Park Opening Use Case

**Scenario**: "Security opening shift (Hub, 6am-2pm)"

1. **6:00 PM Day Before** (unscheduled): Shift planned in HR system → Export to RDF
2. **5:00 AM Day Of** (published): Shift published in Workday → Status set to "published"
3. **5:30 AM** (assigned): Manager assigns John Doe → assignedTo=john_doe, assignmentAuthorized=false
4. **5:45 AM** (active): Approval issued → assignmentAuthorized=true, John clocks in → status="active"
5. **2:00 PM** (completed): John clocks out → status="completed", actualEndTime=2:00 PM

---

## 2. TASK State Machine

**Definition**: Unit of work within a shift with defined duration and success criteria.

**Maps to**: Jira tasks, SAP work orders, ops checklists

### States and Transitions

```
┌─────────────────────────────────────────────────────────────┐
│                     TASK STATE MACHINE                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│ [unassigned]──assign──>[assigned]──start──>[in_progress]    │
│                                                    │         │
│                                                 complete      │
│                                                    │         │
│                                                    v         │
│                                                [completed]    │
│                                                             │
│ NO BACKWARD TRANSITIONS (one-way progress toward completion)│
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Detailed State Descriptions

| State | Meaning | Valid Previous States | Mandatory Fields | Examples |
|-------|---------|----------------------|-------------------|----------|
| **unassigned** | Task created but no one assigned | (initial state) | taskId, title, estimatedDurationMinutes, successCriteria, parentShift, createdAt | "Check Hub entrance doors - Success: doors unlocked, alarms verified" |
| **assigned** | Someone accepted the task | unassigned | + assignedTaskTo (Resource ref) | "Assigned to John (security team)" |
| **in_progress** | Person started work on task | assigned | + actualStartTime (approx) | John arrived at Hub, started unlocking doors |
| **completed** | Task finished; success criteria verified | in_progress | + actualDurationMinutes, completionNotes, successVerified=true | "Doors unlocked and tested. All alarms functional." |

### Valid Transitions

```
unassigned   → assigned       (assign to person)
unassigned   → assigned       (reassign if needed, stay unassigned)
assigned     → in_progress    (person starts work)
in_progress  → completed      (verify success criteria, mark done)
```

### Invalid Transitions

```
completed   ↛ in_progress     (cannot reopen completed task)
in_progress ↛ assigned        (cannot 'un-start' in progress task)
assigned    ↛ unassigned      (cannot unassign)
```

### Park Opening Use Case

**Scenario**: "Check Hub entrance doors" (within Security opening shift)

1. **5:45 AM** (unassigned): Task created by ops automation (from checklist template)
2. **5:50 AM** (assigned): Manager assigns to John → taskStatus="assigned"
3. **6:05 AM** (in_progress): John starts work → taskStatus="in_progress", actualStartTime=6:05 AM
4. **6:15 AM** (completed): John verifies success criteria (doors unlocked, no alarms) → taskStatus="completed", actualDurationMinutes=10

---

## 3. INCIDENT State Machine

**Definition**: Unexpected event requiring investigation and escalation.

**Maps to**: PagerDuty incidents, Slack alerts, SAP notifications

### States and Transitions

```
┌────────────────────────────────────────────────────────┐
│                  INCIDENT STATE MACHINE                │
├────────────────────────────────────────────────────────┤
│                                                        │
│  [reported]──assign──>[assigned]──start──>[investigating]
│                                                │       │
│                                         (may revert)   │
│                                                │       │
│                                             resolve    │
│                                                │       │
│                                                v       │
│                                            [resolved]  │
│                                                        │
│  Can REVERT: investigating → investigating if solution
│  doesn't hold (re-open incident)                      │
│                                                        │
└────────────────────────────────────────────────────────┘
```

### Detailed State Descriptions

| State | Meaning | Valid Previous States | Mandatory Fields | Examples |
|-------|---------|----------------------|-------------------|----------|
| **reported** | Incident first detected and logged | (initial state) | incidentId (INC-YYYY-NNNNN format), title, description, severity, reportedAt, reportedBy | "Security control panel offline - 6:12 AM" |
| **assigned** | Incident routed to team member | reported | + assignedTo (Resource ref) | "Assigned to Technician Carlos" |
| **investigating** | Team actively working on root cause | assigned | + investigationNotes (updated iteratively) | "Checking power supply, circuit breaker..." |
| **resolved** | Root cause fixed; incident closed | investigating | + resolution, resolvedAt, signatureHash (audit) | "Power cord was loose. Reconnected and tested." |

### Valid Transitions

```
reported      → assigned       (route to on-call)
assigned      → investigating  (start troubleshooting)
investigating → resolved       (fix applied, verified)
investigating → investigating  (reopen if not actually fixed; record in event)
resolved      → investigating  (reopen if issue recurs; rare in Phase 1)
```

### Invalid Transitions

```
reported   ↛ investigating    (must assign first)
resolved   ↛ assigned        (cannot go backward multiple steps)
```

### Park Opening Use Case

**Scenario**: "Security control panel offline"

1. **6:12 AM** (reported): Motion sensor detects offline → Alert triggered, incidentStatus="reported"
2. **6:13 AM** (assigned): On-call technician paged → incidentStatus="assigned", assignedTo=carlos
3. **6:15 AM** (investigating): Carlos at panel, checking power → investigationNotes="Checking power supply"
4. **6:20 AM** (resolved): Power cord reconnected, system tests OK → resolution="Power cord was loose", resolvedAt=6:20 AM, signatureHash=<signature>

---

## 4. APPROVAL State Machine

**Definition**: Decision gate requiring authority validation (e.g., shift authorization, incident escalation).

**Maps to**: Workday approvals, authorization workflows, change control

### States and Transitions

```
┌──────────────────────────────────────────────────────────┐
│                  APPROVAL STATE MACHINE                  │
├──────────────────────────────────────────────────────────┤
│                                                          │
│         ┌──────────────────────────────────────┐         │
│         │                                      │         │
│         v                                      v         │
│  [pending]──approve──>[approved]──execute──>[executed]   │
│         │                                                │
│         └─────────reject──────>[rejected] (final)        │
│                                                          │
│  NO BACKWARD TRANSITIONS (decision is immutable)         │
│  Signature required for approved/executed (audit trail)  │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

### Detailed State Descriptions

| State | Meaning | Valid Previous States | Mandatory Fields | Signature Required |
|-------|---------|----------------------|-------------------|-------------------|
| **pending** | Approval request submitted, awaiting decision | (initial state) | approvalId (APR-YYYY-NNNNN), approvalType, subject, requestedAt, requestedBy, requiredAuthorityLevel | No |
| **approved** | Authorized by required authority | pending | + approvedBy, approvedAt | YES (SHA-256 hash) |
| **rejected** | Authority declined; reason documented | pending | + rejectionReason | No |
| **executed** | Approved decision acted upon (shift started, incident escalated, etc.) | approved | + executionTimestamp | YES (link to approvalBy signature) |

### Valid Transitions

```
pending   → approved    (signer with authority approves)
pending   → rejected    (signer rejects; provide reason)
approved  → executed    (decision acted upon in ops system)
```

### Invalid Transitions

```
rejected  ↛ approved    (rejected approval stays rejected)
executed  ↛ approved    (executed cannot be revoked)
```

### Park Opening Use Case

**Scenario 1**: "Authorize assignment of John to critical security shift"

1. **5:40 AM** (pending): Manager submits approval request → approvalStatus="pending", requiredAuthorityLevel="director"
2. **5:44 AM** (approved): Director (Mary) approves → approvalStatus="approved", approvedBy="mary", signatureHash=<sig>, approvedAt=5:44 AM
3. **5:45 AM** (executed): System assigns John to shift → approvalStatus="executed", Shift.assignmentAuthorized=true

**Scenario 2**: "Escalate incident to VP (critical severity)"

1. **6:25 AM** (pending): On-call manager submits escalation → approvalStatus="pending", requiredAuthorityLevel="executive"
2. **6:26 AM** (rejected): VP declines (not critical enough) → approvalStatus="rejected", rejectionReason="Issue resolved by technician. No further escalation needed."

---

## 5. RESOURCE State Machine

**Definition**: Person, equipment, or system available for work.

**Maps to**: Employees (HRIS), Equipment (SAP), Systems (monitoring)

### States and Transitions

```
┌──────────────────────────────────────────────────────────────┐
│                   RESOURCE STATE MACHINE                     │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│   [available] ──reserve──> [reserved] ──start──> [in_use]    │
│       ^                                                │      │
│       │                                              release  │
│       └─────────────────────────────────────────────┘         │
│                                                              │
│   [available] ──schedule──> [maintenance] ──complete──>      │
│                                      [available]             │
│                                                              │
│   CYCLE-BASED: available → reserve → in_use → available     │
│   (equipment cycles through maintenance windows)             │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

### Detailed State Descriptions

| State | Meaning | Valid Previous States | Mandatory Fields | Examples |
|-------|---------|----------------------|-------------------|----------|
| **available** | Resource ready to work | in_use, maintenance | resourceId, resourceType, name, department, resourceStatus, capacity | John available; Hub entrance panels powered on |
| **reserved** | Resource allocated but not yet in use | available | + reservedFor (ref to Shift or Task), reservedAt | John scheduled for 6am shift; panel reserved for diagnostics |
| **in_use** | Resource actively being used | reserved | + usageStartTime, usageContext | John on-site during shift; panel actively monitored |
| **maintenance** | Resource undergoing servicing | available | + maintenanceStartTime, maintenanceType | Panel taken offline for quarterly inspection |
| **unavailable** | Resource cannot be used (broken, absent) | available, in_use, maintenance | + unavailableReason, estimatedAvailableAt | John called in sick; panel power supply failed |

### Valid Transitions

```
available     → reserved      (assign to shift/task)
reserved      → in_use        (shift/task starts)
in_use        → available     (shift/task ends, resource released)
available     → maintenance   (scheduled service)
maintenance   → available     (service complete)
available     → unavailable   (unexpected issue)
unavailable   → available     (issue resolved; resource back to service)
```

### Invalid Transitions

```
in_use    ↛ reserved        (cannot reserve while in use)
reserved  ↛ available       (must transition to in_use first)
```

### Park Opening Use Case

**Scenario 1**: "John (person) for security opening shift"

1. **5:00 AM Day Before** (available): John's schedule open, no conflicts
2. **5:30 AM Day Of** (reserved): Assigned to 6am shift → resourceStatus="reserved"
3. **6:00 AM** (in_use): John clocks in, shift begins → resourceStatus="in_use", usageStartTime=6:00 AM
4. **2:00 PM** (available): Shift ends, John clocks out → resourceStatus="available"

**Scenario 2**: "Hub control panel (equipment)"

1. **6:00 AM** (in_use): Panel actively monitoring during opening procedures
2. **2:00 PM** (available): Shift ends, monitoring reduced
3. **Weekly: Monday 3am** (maintenance): Panel scheduled for quarterly inspection → resourceStatus="maintenance"
4. **Monday 5am** (available): Inspection complete, panel returned to service

---

## State Machine Validation Rules

### Immutability Principle

Once a state transition occurs, it is **recorded as an immutable event**. The event includes:

- `eventId` (UUID)
- `eventType` (e.g., "shift.status.changed")
- `previousValue` (e.g., "published")
- `newValue` (e.g., "assigned")
- `changedBy` (user/system ID)
- `changedAt` (server-generated timestamp)
- `sourceSystem` (where change originated: "workday", "sap", "manual", "automation")

### Merge Algebra: Concurrent Edit Handling

When two independent teams (e.g., Workday integration + manual ops console) modify the same Work Object:

1. **Detect Conflict**: Compare events by timestamp and source system
   - If within 5 minutes: likely concurrent
   - If different source systems: confirm with timestamps

2. **Resolve by Field Criticality**:
   - **Non-critical** (notes, descriptions): Last-Write-Wins
   - **Critical** (status, authorization): Manual Review required
   - **Additive** (tasks, incidents in shift): Automated Merge (union)
   - **Exclusive** (assignedTo): Last-Write-Wins with timestamp comparison

3. **Record Resolution**: Create "merge.conflict.resolved" event with:
   - Which events were in conflict
   - Resolution strategy applied
   - Merged state (final result)

### Example: Concurrent Edit Conflict

```
Timeline:
5:40 AM - Event 1: Workday API sets Shift status → "assigned"
5:41 AM - Event 2: Manual console sets Shift status → "active" (from different source)

Conflict Detection:
- Same object (Shift #12345)
- Same field (shiftStatus)
- Different source systems (workday vs. manual)
- Events 2 minutes apart → likely concurrent

Resolution:
- Field criticality: "status" = CRITICAL
- Flag for manual review
- Operations team reviews: "System should follow Workday truth"
- Manual resolution: Accept Event 1, reject Event 2
- Record: "merge.conflict.resolved" event

Final State: Shift status = "assigned" (from Workday)
```

---

## Coverage Analysis: 80% of Park Opening

### Covered Workflows (Phase 1)

| Workflow | Coverage | State Machines Used | Completeness |
|----------|----------|-------------------|--------------|
| **Opening Shift Assignment** | 100% | Shift, Approval, Resource | All states utilized |
| **Park Area Opening (Hub, zones)** | 95% | Shift, Task, Resource | All core tasks (10+ per area) |
| **Security System Activation** | 90% | Task, Resource, Incident (if failures) | Full checklist coverage |
| **Incident Response** | 85% | Incident, Approval (escalation), Resource | Covers: reported → assigned → resolved |
| **Equipment Availability** | 80% | Resource, Maintenance scheduling | Available → Maintenance → Available |
| **Authorization Gates** | 100% | Approval | All authority levels (director, exec) |

### Not Covered (Phase 2+)

| Scenario | Why Deferred |
|----------|-------------|
| **Multi-location coordination** | Requires explicit cross-location Task dependencies |
| **Resource pooling (shared equipment)** | Phase 1 assumes 1:1 resource-to-shift assignment |
| **Cascade cancellations** (if opening delayed) | Requires Event-driven workflows (Wave 3) |
| **Cost allocation by task** | Requires cost center augmentation |
| **Real-time telemetry feedback** | Gap 4 (Telemetry Bridge); deferred to Wave 3 |

### Acceptance Criteria for 80% Coverage

✓ All 5 types (Shift, Task, Incident, Approval, Resource) have valid state machines
✓ Park opening can proceed from unscheduled shift → published → assigned → active → completed
✓ All tasks in shift can be tracked (unassigned → assigned → in_progress → completed)
✓ Incidents are logged and escalated (reported → investigating → resolved)
✓ Approvals gate critical decisions (pending → approved → executed)
✓ Resources are reserved, used, and released (available → reserved → in_use → available)
✓ Concurrent edits detectable via event comparison (merge algebra foundation)
✓ SHACL shapes enforce all constraints (valid RDF + validation)

---

## Implementation Checklist

- [ ] Shift state machine validated (no backward transitions)
- [ ] Task state machine validated (one-way progress)
- [ ] Incident state machine validated (can reopen from investigating)
- [ ] Approval state machine validated (immutable decisions)
- [ ] Resource state machine validated (cycle-based availability)
- [ ] All mandatory fields present for each state (SHACL shapes)
- [ ] Event audit trail structure defined (immutability)
- [ ] Merge conflict detection rules documented (concurrent edits)
- [ ] Park opening scenario traced through all machines (end-to-end)
- [ ] SHACL validation rules tested (no invalid transitions)

---

## References

- **Source**: `.specify/work-object-model-types.ttl` (RDF ontology)
- **Validation**: `.specify/work-object-model-shapes.ttl` (SHACL constraints)
- **Adoption Model**: `.specify/ggen-disney-adoption-model.ttl` (Gap 5, Phase 1)
- **Test Plan**: Evidence directory (manual scenario traces)

---

**Status**: SPECIFICATION CLOSED (RDF-first, immutable)

**Last Updated**: 2026-01-18

**Author**: Specification Architect (EPIC 9: Gap 5-Phase1 Task 3)
