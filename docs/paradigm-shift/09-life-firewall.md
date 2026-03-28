<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [The Life Firewall: Perimeter-First Design](#the-life-firewall-perimeter-first-design)
  - [The Fundamental Problem](#the-fundamental-problem)
  - [The Firewall Principle](#the-firewall-principle)
  - [Three Ingress Channels (and Only Three)](#three-ingress-channels-and-only-three)
    - [Channel 1: Batch Intake](#channel-1-batch-intake)
    - [Channel 2: Scheduled Interface](#channel-2-scheduled-interface)
    - [Channel 3: Emergency](#channel-3-emergency)
  - [Everything Else: Refused with Receipts](#everything-else-refused-with-receipts)
  - [Packet Discipline: Typed Work Orders](#packet-discipline-typed-work-orders)
    - [Work Order Anatomy](#work-order-anatomy)
    - [Work Order Types](#work-order-types)
  - [The "No Packet ⇒ No Work" Principle](#the-no-packet-%E2%87%92-no-work-principle)
  - [Implementation Patterns in Rust](#implementation-patterns-in-rust)
    - [Core Traits](#core-traits)
    - [Firewall Implementation](#firewall-implementation)
    - [Channel Implementations](#channel-implementations)
    - [Work Order Processing](#work-order-processing)
  - [Protocol Specifications](#protocol-specifications)
    - [Batch Intake Protocol](#batch-intake-protocol)
    - [Scheduled Interface Protocol](#scheduled-interface-protocol)
    - [Emergency Protocol](#emergency-protocol)
    - [Refusal Protocol](#refusal-protocol)
  - [Enforcement Patterns](#enforcement-patterns)
    - [Type-Level Enforcement](#type-level-enforcement)
    - [Runtime Enforcement](#runtime-enforcement)
    - [Audit Trail](#audit-trail)
  - [Real-World Example: Day-to-Day Operations](#real-world-example-day-to-day-operations)
    - [Scenario 1: Morning Planning (Batch Intake)](#scenario-1-morning-planning-batch-intake)
    - [Scenario 2: Interruption Attempt (Refused)](#scenario-2-interruption-attempt-refused)
    - [Scenario 3: Scheduled Review (Scheduled Interface)](#scenario-3-scheduled-review-scheduled-interface)
    - [Scenario 4: Production Outage (Emergency)](#scenario-4-production-outage-emergency)
  - [Integration with ggen](#integration-with-ggen)
  - [Common Questions & Answers](#common-questions--answers)
  - [Metrics and Success Criteria](#metrics-and-success-criteria)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# The Life Firewall: Perimeter-First Design

**Core Principle**: Protect cognitive resources through strict perimeter control with cryptographic accountability.

**Formula**: `Work = μ(Packet)` where work only occurs for validated packets through authorized channels.

---

## The Fundamental Problem

Traditional work management operates in "always-on" mode:

```
┌─────────────────────────────────────────────────────────┐
│              TRADITIONAL WORK INTAKE                     │
│                                                          │
│  Email ─────┐                                           │
│  Slack ─────┤                                           │
│  Teams ─────┤                                           │
│  SMS ───────┼──► YOU ──► Context Switch ──► Work       │
│  Phone ─────┤           (15-30 min recovery)           │
│  Meeting ───┤                                           │
│  Hallway ───┤                                           │
│  "Quick Q"──┘                                           │
│                                                          │
│  Result: 7-12 interruptions/hour                        │
│          Average 23 minutes to regain flow              │
│          Effective work time: ~2 hours/day              │
└─────────────────────────────────────────────────────────┘
```

**The Cost:**
- 47% of work time lost to interruptions (Gloria Mark, UC Irvine)
- Average knowledge worker switches tasks every 3 minutes
- 2.5 hours/day recovering from interruptions
- 40% productivity loss from task switching

**The Root Cause**: No perimeter. Unlimited ingress channels with no packet discipline.

---

## The Firewall Principle

The Life Firewall implements perimeter-first design:

```
┌─────────────────────────────────────────────────────────────────┐
│                    LIFE FIREWALL ARCHITECTURE                   │
│                                                                 │
│  ╔═══════════════════════════════════════════════════════╗     │
│  ║                    PERIMETER                          ║     │
│  ║                                                       ║     │
│  ║   ┌───────────────┐  ┌───────────────┐  ┌─────────┐ ║     │
│  ║   │ Batch Intake  │  │  Scheduled    │  │Emergency│ ║     │
│  ║   │   (Daily)     │  │  Interface    │  │ (Rare)  │ ║     │
│  ║   │   08:00-09:00 │  │ (Checkpoints) │  │ P0 Only │ ║     │
│  ║   └───────┬───────┘  └───────┬───────┘  └────┬────┘ ║     │
│  ║           │                   │                │      ║     │
│  ╚═══════════╪═══════════════════╪════════════════╪══════╝     │
│              │                   │                │            │
│              └───────────┬───────┴────────────────┘            │
│                          │                                     │
│                    ┌─────▼──────┐                              │
│                    │  Work Queue │                             │
│                    │  (Validated │                             │
│                    │   Packets)  │                             │
│                    └─────┬──────┘                              │
│                          │                                     │
│                    ┌─────▼──────┐                              │
│                    │ Processing  │                             │
│                    │   Engine    │                             │
│                    └────────────┘                              │
│                                                                 │
│  ╔═══════════════════════════════════════════════════════╗     │
│  ║              REFUSED (With Receipts)                  ║     │
│  ║                                                       ║     │
│  ║   Email ──X──┐                                       ║     │
│  ║   Slack ──X──┤                                       ║     │
│  ║   SMS ────X──┼──► Receipt Generator                  ║     │
│  ║   Phone ──X──┤     • Why refused                     ║     │
│  ║   Meeting ─X─┤     • When to retry                   ║     │
│  ║   "Quick Q" X┘     • Proper channel                  ║     │
│  ║                    • Audit trail                     ║     │
│  ╚═══════════════════════════════════════════════════════╝     │
└─────────────────────────────────────────────────────────────────┘
```

**Key Insight**: Perimeter control isn't about being unresponsive. It's about channeling communication through validated, documented pathways that preserve cognitive resources.

---

## Three Ingress Channels (and Only Three)

### Channel 1: Batch Intake

**Purpose**: Primary ingress for non-urgent work

**Schedule**: Once per day, dedicated time block
- Example: 08:00-09:00 daily
- No interruptions during processing
- All accumulated requests processed together

**Protocol**:
```rust
pub struct BatchIntake {
    schedule: CronExpression,      // "0 8 * * *" (daily 08:00)
    window_duration: Duration,     // 1 hour
    max_packets_per_batch: usize,  // 20 (prevents overflow)
}

impl Channel for BatchIntake {
    fn is_open(&self, timestamp: DateTime<Utc>) -> bool {
        self.schedule.matches(timestamp) &&
        timestamp.duration_since(self.last_opened)? < self.window_duration
    }

    fn accept(&mut self, request: RawRequest) -> Result<WorkPacket, Refusal> {
        if !self.is_open(Utc::now()) {
            return Err(Refusal::OutsideWindow {
                next_window: self.schedule.next_occurrence(),
                reason: "Batch intake only processes during scheduled window",
            });
        }

        // Validate request has complete packet structure
        let packet = WorkPacket::try_from(request)?;

        // Check batch capacity
        if self.current_batch_size >= self.max_packets_per_batch {
            return Err(Refusal::CapacityExceeded {
                retry_at: self.schedule.next_occurrence(),
            });
        }

        // Log acceptance for audit
        self.audit_log.record(AcceptanceEvent {
            packet_id: packet.id(),
            timestamp: Utc::now(),
            channel: "BatchIntake",
        });

        Ok(packet)
    }
}
```

**Use Cases**:
- Feature requests
- Bug reports (non-critical)
- Documentation updates
- Technical debt items
- Exploratory work
- Research tasks

**Benefits**:
- Context switching confined to one time block
- Batch processing reduces cognitive load
- Clear expectations for requesters (24-hour SLA)
- Enables deep work rest of day

---

### Channel 2: Scheduled Interface

**Purpose**: Synchronous checkpoints for collaboration and review

**Schedule**: Pre-planned, calendar-blocked slots
- Example: Daily standup (09:00-09:15)
- Example: Weekly planning (Monday 14:00-15:00)
- Example: Architecture review (Thursday 10:00-11:00)

**Protocol**:
```rust
pub struct ScheduledInterface {
    calendar: Calendar,
    upcoming_slots: Vec<TimeSlot>,
    allowed_participants: HashSet<UserId>,
}

pub struct TimeSlot {
    start: DateTime<Utc>,
    end: DateTime<Utc>,
    purpose: String,
    max_packets: usize,
}

impl Channel for ScheduledInterface {
    fn is_open(&self, timestamp: DateTime<Utc>) -> bool {
        self.upcoming_slots
            .iter()
            .any(|slot| slot.contains(timestamp))
    }

    fn accept(&mut self, request: RawRequest) -> Result<WorkPacket, Refusal> {
        let now = Utc::now();

        // Check if we're in a scheduled slot
        let slot = self.upcoming_slots
            .iter()
            .find(|s| s.contains(now))
            .ok_or_else(|| Refusal::NoActiveSlot {
                next_slot: self.next_slot(),
            })?;

        // Validate requester is authorized
        if !self.allowed_participants.contains(&request.requester_id) {
            return Err(Refusal::UnauthorizedParticipant {
                slot_purpose: slot.purpose.clone(),
                allowed_participants: self.allowed_participants.clone(),
            });
        }

        // Check slot capacity
        let current_count = self.packets_in_slot(slot.start);
        if current_count >= slot.max_packets {
            return Err(Refusal::SlotFull {
                next_slot: self.next_slot(),
            });
        }

        let packet = WorkPacket::try_from(request)?;

        self.audit_log.record(AcceptanceEvent {
            packet_id: packet.id(),
            timestamp: now,
            channel: "ScheduledInterface",
            slot_purpose: slot.purpose.clone(),
        });

        Ok(packet)
    }
}
```

**Use Cases**:
- Team standups
- Sprint planning
- Code reviews
- Architecture discussions
- Stakeholder updates
- Pair programming sessions

**Benefits**:
- Predictable synchronous time
- Concentrated collaboration
- Clear boundaries (no spillover)
- Mutual respect for time

---

### Channel 3: Emergency

**Purpose**: True emergencies only (production outages, security incidents)

**Criteria**: P0 severity with business-critical impact

**Protocol**:
```rust
pub struct EmergencyChannel {
    severity_threshold: Severity,           // P0 only
    validation_rules: Vec<EmergencyRule>,
    cooldown_period: Duration,              // Prevent abuse
    last_emergency: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    P0, // Production down, data loss, security breach
    P1, // Degraded performance, impacting users
    P2, // Bug, needs fix but not urgent
    P3, // Enhancement, technical debt
}

pub struct EmergencyRule {
    name: String,
    validator: Box<dyn Fn(&RawRequest) -> Result<(), String>>,
}

impl Channel for EmergencyChannel {
    fn is_open(&self, _timestamp: DateTime<Utc>) -> bool {
        // Emergency channel is always "open" but has strict validation
        true
    }

    fn accept(&mut self, request: RawRequest) -> Result<WorkPacket, Refusal> {
        let now = Utc::now();

        // Check cooldown to prevent abuse
        if let Some(last_use) = self.last_emergency {
            let elapsed = now.duration_since(last_use)?;
            if elapsed < self.cooldown_period {
                return Err(Refusal::CooldownActive {
                    remaining: self.cooldown_period - elapsed,
                    reason: "Emergency channel overused - use Batch Intake",
                });
            }
        }

        // Validate severity
        let claimed_severity = request.metadata
            .get("severity")
            .and_then(|s| s.parse::<Severity>().ok())
            .ok_or_else(|| Refusal::MissingSeverity)?;

        if claimed_severity != Severity::P0 {
            return Err(Refusal::InsufficientSeverity {
                claimed: claimed_severity,
                required: Severity::P0,
                reason: "Emergency channel requires P0 severity",
                redirect_to: "BatchIntake",
            });
        }

        // Run emergency validation rules
        for rule in &self.validation_rules {
            (rule.validator)(&request)
                .map_err(|reason| Refusal::EmergencyValidationFailed {
                    rule_name: rule.name.clone(),
                    reason,
                })?;
        }

        // Create emergency work packet
        let mut packet = WorkPacket::try_from(request)?;
        packet.mark_emergency(now);

        // Alert stakeholders
        self.alert_responders(&packet)?;

        // Update last emergency timestamp
        self.last_emergency = Some(now);

        self.audit_log.record(EmergencyAcceptance {
            packet_id: packet.id(),
            timestamp: now,
            severity: claimed_severity,
            validated_by: self.validation_rules
                .iter()
                .map(|r| r.name.clone())
                .collect(),
        });

        Ok(packet)
    }
}

impl EmergencyChannel {
    pub fn with_validation_rules() -> Self {
        Self {
            severity_threshold: Severity::P0,
            cooldown_period: Duration::hours(4),
            last_emergency: None,
            validation_rules: vec![
                EmergencyRule {
                    name: "production_impact".to_string(),
                    validator: Box::new(|req| {
                        req.metadata
                            .get("production_impact")
                            .filter(|v| v == &"true")
                            .ok_or_else(|| "No production impact".to_string())
                            .map(|_| ())
                    }),
                },
                EmergencyRule {
                    name: "revenue_impact".to_string(),
                    validator: Box::new(|req| {
                        req.metadata
                            .get("revenue_impact_usd")
                            .and_then(|v| v.parse::<u64>().ok())
                            .filter(|v| *v > 1000)
                            .ok_or_else(|| "Revenue impact < $1000".to_string())
                            .map(|_| ())
                    }),
                },
                EmergencyRule {
                    name: "incident_commander_assigned".to_string(),
                    validator: Box::new(|req| {
                        req.metadata
                            .get("incident_commander")
                            .ok_or_else(|| "No incident commander assigned".to_string())
                            .map(|_| ())
                    }),
                },
            ],
        }
    }
}
```

**Use Cases**:
- Production system down
- Data loss or corruption
- Security breach
- Customer-impacting outages
- Legal/compliance emergencies

**Benefits**:
- True emergencies get immediate attention
- Abuse prevention via cooldown
- Validation ensures legitimacy
- Audit trail for postmortems

**NOT Use Cases**:
- "Urgent" feature requests (use Batch Intake)
- "Quick questions" (use Scheduled Interface)
- Missed deadlines (poor planning, not emergency)
- Executive requests (use Batch Intake, escalate if needed)

---

## Everything Else: Refused with Receipts

**Principle**: If it doesn't come through one of the three channels, it gets refused with a cryptographic receipt explaining why and how to retry properly.

**Refusal Types**:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Refusal {
    OutsideWindow {
        next_window: DateTime<Utc>,
        reason: String,
    },
    NoActiveSlot {
        next_slot: TimeSlot,
    },
    InsufficientSeverity {
        claimed: Severity,
        required: Severity,
        reason: String,
        redirect_to: String,
    },
    MalformedPacket {
        missing_fields: Vec<String>,
        template_url: String,
    },
    CapacityExceeded {
        retry_at: DateTime<Utc>,
    },
    UnauthorizedChannel {
        attempted_channel: String,
        allowed_channels: Vec<String>,
        documentation: String,
    },
}

impl Refusal {
    pub fn to_receipt(&self) -> RefusalReceipt {
        RefusalReceipt {
            id: Uuid::new_v4(),
            timestamp: Utc::now(),
            refusal: self.clone(),
            signature: self.sign(),
            audit_trail: self.audit_data(),
        }
    }

    fn sign(&self) -> String {
        let serialized = serde_json::to_string(self).unwrap();
        let hash = Sha256::digest(serialized.as_bytes());
        hex::encode(hash)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalReceipt {
    pub id: Uuid,
    pub timestamp: DateTime<Utc>,
    pub refusal: Refusal,
    pub signature: String,
    pub audit_trail: AuditData,
}
```

**Example Refusal Flow**:

```
┌─────────────────────────────────────────────────────────┐
│  Slack Message: "Hey, quick question about auth bug?"   │
└────────────────────┬────────────────────────────────────┘
                     │
                     ▼
          ┌──────────────────────┐
          │  Firewall Detector   │
          │  (Unauthorized Chan) │
          └──────────┬───────────┘
                     │
                     ▼
          ┌──────────────────────┐
          │  Generate Refusal    │
          │  + Receipt           │
          └──────────┬───────────┘
                     │
                     ▼
┌────────────────────────────────────────────────────────┐
│  REFUSAL RECEIPT #a3f8b2c1                             │
│                                                        │
│  Timestamp: 2026-02-09T15:23:45Z                       │
│  Reason: Unauthorized Channel                          │
│                                                        │
│  Your request was received via Slack, which is not     │
│  an authorized ingress channel.                        │
│                                                        │
│  To submit work requests:                              │
│                                                        │
│  1. Batch Intake (Non-urgent)                          │
│     - Opens: Daily 08:00-09:00 UTC                     │
│     - Submit: https://intake.example.com               │
│     - SLA: 24-hour response                            │
│                                                        │
│  2. Scheduled Interface (Collaborative)                │
│     - Next slot: 2026-02-10 09:00-09:15 (Standup)      │
│     - Book: https://calendar.example.com               │
│                                                        │
│  3. Emergency (P0 Only)                                │
│     - Criteria: Production down, revenue impact        │
│     - Trigger: https://emergency.example.com           │
│                                                        │
│  Documentation: https://docs.example.com/firewall      │
│  Audit Trail: https://audit.example.com/a3f8b2c1       │
│                                                        │
│  Signature: a3f8b2c1d4e5f6a7b8c9d0e1f2a3b4c5...       │
└────────────────────────────────────────────────────────┘
```

**Benefits**:
- Clear, non-personal rejection (system enforced)
- Educational (teaches proper channels)
- Accountable (cryptographic signature)
- Trackable (audit trail)
- Prevents "black hole" syndrome (requesters know status)

---

## Packet Discipline: Typed Work Orders

**Core Principle**: No work occurs without a validated packet. Packets encode all information needed for execution, review, and accountability.

### Work Order Anatomy

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkPacket {
    pub id: PacketId,
    pub timestamp: DateTime<Utc>,
    pub requester: UserId,
    pub owner: UserId,

    // Core fields
    pub objective: Objective,
    pub constraints: Constraints,
    pub acceptance_test: AcceptanceTest,
    pub reversibility: Reversibility,
    pub dependencies: Dependencies,

    // Metadata
    pub priority: Priority,
    pub estimated_effort: Duration,
    pub tags: HashSet<String>,

    // Audit
    pub channel: Channel,
    pub signature: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Objective {
    pub summary: String,              // 1-line, ≤80 chars
    pub description: String,          // Detailed, markdown
    pub success_criteria: Vec<String>, // Measurable outcomes
    pub context: String,              // Why this matters
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Constraints {
    pub deadline: Option<DateTime<Utc>>,
    pub max_effort: Option<Duration>,
    pub technologies: Vec<String>,    // Required/forbidden tech
    pub budget: Option<u64>,          // USD cents
    pub quality_gates: Vec<QualityGate>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AcceptanceTest {
    pub test_type: TestType,
    pub steps: Vec<TestStep>,
    pub expected_outcome: String,
    pub verification_method: VerificationMethod,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestType {
    Automated,   // CI/CD runs
    Manual,      // Checklist
    Staged,      // Deploy to staging first
    Canary,      // Gradual rollout
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reversibility {
    pub is_reversible: bool,
    pub rollback_plan: Option<RollbackPlan>,
    pub risk_level: RiskLevel,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RollbackPlan {
    pub steps: Vec<String>,
    pub time_to_rollback: Duration,
    pub data_preservation: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependencies {
    pub blocks: Vec<PacketId>,        // Must complete before this
    pub blocked_by: Vec<PacketId>,    // This blocks these
    pub related: Vec<PacketId>,       // Related but not blocking
}
```

### Work Order Types

**Type 1: Feature Development**
```rust
let feature_packet = WorkPacket {
    id: PacketId::new("FEAT-2024-001"),
    objective: Objective {
        summary: "Add user role-based access control",
        description: r#"
Implement RBAC system with:
- Admin, Editor, Viewer roles
- Permission matrix per resource type
- Audit logging for permission changes
        "#.to_string(),
        success_criteria: vec![
            "Users can be assigned roles".to_string(),
            "Permissions enforce resource access".to_string(),
            "Audit log captures all changes".to_string(),
        ],
        context: "SOC2 compliance requirement for Q1 audit".to_string(),
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::weeks(2)),
        max_effort: Some(Duration::days(5)),
        technologies: vec!["Rust".to_string(), "PostgreSQL".to_string()],
        quality_gates: vec![
            QualityGate::TestCoverage { min: 80 },
            QualityGate::NoCompilerWarnings,
            QualityGate::SecurityScan { severity: "high" },
        ],
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::Automated,
        steps: vec![
            TestStep::new("Create user with Viewer role"),
            TestStep::new("Attempt to edit resource (should fail)"),
            TestStep::new("Upgrade user to Editor role"),
            TestStep::new("Attempt to edit resource (should succeed)"),
            TestStep::new("Verify audit log has 2 entries"),
        ],
        expected_outcome: "All tests pass in CI".to_string(),
        verification_method: VerificationMethod::CI,
    },
    reversibility: Reversibility {
        is_reversible: true,
        rollback_plan: Some(RollbackPlan {
            steps: vec![
                "Run migration rollback script".to_string(),
                "Restart application servers".to_string(),
                "Verify users can still authenticate".to_string(),
            ],
            time_to_rollback: Duration::minutes(5),
            data_preservation: true,
        }),
        risk_level: RiskLevel::Medium,
    },
    dependencies: Dependencies {
        blocks: vec![],
        blocked_by: vec![PacketId::new("FEAT-2024-000")], // Auth system
        related: vec![PacketId::new("DOC-2024-015")],      // User guide
    },
    // ...
};
```

**Type 2: Bug Fix**
```rust
let bugfix_packet = WorkPacket {
    id: PacketId::new("BUG-2024-042"),
    objective: Objective {
        summary: "Fix race condition in session cleanup",
        description: r#"
Sessions are not being properly cleaned up when multiple
requests terminate simultaneously, leading to memory leak.
        "#.to_string(),
        success_criteria: vec![
            "No memory leak after 1000 concurrent session terminations".to_string(),
            "All sessions cleaned up within 5 seconds".to_string(),
        ],
        context: "Causing staging server OOM every 6 hours".to_string(),
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::days(1)),
        max_effort: Some(Duration::hours(4)),
        quality_gates: vec![
            QualityGate::RegressionTests,
            QualityGate::MemoryLeakTest,
        ],
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::Automated,
        steps: vec![
            TestStep::new("Create 1000 sessions"),
            TestStep::new("Terminate all simultaneously"),
            TestStep::new("Wait 5 seconds"),
            TestStep::new("Verify session count = 0"),
            TestStep::new("Verify memory returned to baseline"),
        ],
        expected_outcome: "Test passes 10/10 runs".to_string(),
        verification_method: VerificationMethod::CI,
    },
    reversibility: Reversibility {
        is_reversible: true,
        rollback_plan: Some(RollbackPlan {
            steps: vec!["Revert commit via git".to_string()],
            time_to_rollback: Duration::minutes(2),
            data_preservation: true,
        }),
        risk_level: RiskLevel::Low,
    },
    // ...
};
```

**Type 3: Research/Exploration**
```rust
let research_packet = WorkPacket {
    id: PacketId::new("RES-2024-007"),
    objective: Objective {
        summary: "Evaluate alternative RDF stores for performance",
        description: r#"
Current Oxigraph performance is adequate but evaluating
alternatives for potential 10x improvement at scale.
        "#.to_string(),
        success_criteria: vec![
            "Benchmark 3 alternative stores".to_string(),
            "Document results in comparison matrix".to_string(),
            "Recommendation with migration cost estimate".to_string(),
        ],
        context: "Planning for 100x scale in 2025".to_string(),
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::weeks(1)),
        max_effort: Some(Duration::days(2)),
        technologies: vec![],
        quality_gates: vec![
            QualityGate::DocumentationComplete,
            QualityGate::StakeholderReview,
        ],
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::Manual,
        steps: vec![
            TestStep::new("Benchmark Oxigraph (baseline)"),
            TestStep::new("Benchmark AllegroGraph"),
            TestStep::new("Benchmark Apache Jena"),
            TestStep::new("Benchmark Stardog"),
            TestStep::new("Document results in research/rdf-stores.md"),
            TestStep::new("Present findings in architecture review"),
        ],
        expected_outcome: "Decision: Stick with Oxigraph or migrate to X".to_string(),
        verification_method: VerificationMethod::StakeholderApproval,
    },
    reversibility: Reversibility {
        is_reversible: true,
        rollback_plan: None, // Research is non-destructive
        risk_level: RiskLevel::None,
    },
    // ...
};
```

---

## The "No Packet ⇒ No Work" Principle

**Rule**: Work only occurs for validated packets. No packet = no work.

**Enforcement**:

```rust
pub struct WorkProcessor {
    firewall: Firewall,
    queue: WorkQueue,
    executor: Executor,
}

impl WorkProcessor {
    pub async fn process(&mut self) -> Result<WorkReceipt, WorkError> {
        // 1. Dequeue next packet (validated)
        let packet = self.queue
            .dequeue()
            .ok_or(WorkError::NoPacketsAvailable)?;

        // 2. Double-check packet validity
        packet.validate()?;

        // 3. Verify not already processed
        if self.is_duplicate(&packet.id) {
            return Err(WorkError::DuplicatePacket(packet.id));
        }

        // 4. Execute work
        let result = self.executor.execute(&packet).await?;

        // 5. Validate acceptance criteria
        result.verify_acceptance_test(&packet.acceptance_test)?;

        // 6. Generate receipt
        Ok(WorkReceipt {
            packet_id: packet.id,
            timestamp: Utc::now(),
            result,
            signature: self.sign_receipt(&packet, &result),
        })
    }

    // NO OTHER ENTRY POINTS TO WORK
    // No process_adhoc, no process_urgent, no process_quickie
    // ONLY process() which requires validated packet
}
```

**What This Prevents**:

```rust
// ❌ FORBIDDEN - No packet
fn handle_slack_message(msg: &str) {
    // Just do the work inline because it's "quick"
    fix_bug(msg)?; // NO! Where's the packet?
}

// ❌ FORBIDDEN - Ad-hoc work
fn on_meeting_request(topic: &str) {
    // Someone asked in meeting, just ship it
    implement_feature(topic)?; // NO! Where's the packet?
}

// ✅ CORRECT - Packet required
fn handle_request(req: RawRequest) -> Result<(), Refusal> {
    // 1. Validate channel
    let channel = firewall.identify_channel(&req)?;

    // 2. Create packet (or refuse)
    let packet = channel.accept(req)?;

    // 3. Enqueue for processing
    work_queue.enqueue(packet);

    Ok(())
}
```

**Benefits**:
- All work is documented (audit trail)
- All work has acceptance criteria (verification)
- All work has owner and priority (accountability)
- All work has dependencies (coordination)
- All work is reversible (safety)
- No "black ops" work (transparency)

---

## Implementation Patterns in Rust

### Core Traits

```rust
/// Core trait for ingress channels
pub trait Channel: Send + Sync {
    /// Check if channel is currently accepting requests
    fn is_open(&self, timestamp: DateTime<Utc>) -> bool;

    /// Attempt to accept a raw request, converting to work packet
    /// Returns Refusal if channel closed or request invalid
    fn accept(&mut self, request: RawRequest) -> Result<WorkPacket, Refusal>;

    /// Get next opening time for this channel
    fn next_opening(&self) -> Option<DateTime<Utc>>;

    /// Get channel metadata
    fn metadata(&self) -> ChannelMetadata;
}

/// Core trait for work execution
pub trait Executor: Send + Sync {
    /// Execute a work packet, producing a result
    async fn execute(&self, packet: &WorkPacket) -> Result<WorkResult, ExecutionError>;

    /// Verify result meets acceptance criteria
    fn verify(&self, result: &WorkResult, test: &AcceptanceTest) -> Result<(), VerificationError>;
}

/// Core trait for receipt generation
pub trait ReceiptGenerator: Send + Sync {
    /// Generate cryptographic receipt for completed work
    fn generate(&self, packet: &WorkPacket, result: &WorkResult) -> WorkReceipt;

    /// Generate refusal receipt for rejected request
    fn generate_refusal(&self, request: &RawRequest, refusal: &Refusal) -> RefusalReceipt;
}
```

### Firewall Implementation

```rust
pub struct Firewall {
    channels: HashMap<ChannelId, Box<dyn Channel>>,
    receipt_generator: Arc<dyn ReceiptGenerator>,
    audit_log: AuditLog,
}

impl Firewall {
    pub fn new() -> Self {
        let mut channels: HashMap<ChannelId, Box<dyn Channel>> = HashMap::new();

        // Register three channels
        channels.insert(
            ChannelId::BatchIntake,
            Box::new(BatchIntake::new(CronExpression::parse("0 8 * * *").unwrap())),
        );

        channels.insert(
            ChannelId::Scheduled,
            Box::new(ScheduledInterface::new()),
        );

        channels.insert(
            ChannelId::Emergency,
            Box::new(EmergencyChannel::with_validation_rules()),
        );

        Self {
            channels,
            receipt_generator: Arc::new(DefaultReceiptGenerator::new()),
            audit_log: AuditLog::new(),
        }
    }

    pub fn process_request(&mut self, request: RawRequest) -> Result<WorkPacket, RefusalReceipt> {
        // 1. Identify which channel this request is for
        let channel_id = self.identify_channel(&request)?;

        // 2. Get channel
        let channel = self.channels
            .get_mut(&channel_id)
            .ok_or_else(|| RefusalReceipt::unknown_channel())?;

        // 3. Attempt acceptance
        match channel.accept(request.clone()) {
            Ok(packet) => {
                // Log acceptance
                self.audit_log.record_acceptance(&packet);
                Ok(packet)
            }
            Err(refusal) => {
                // Generate refusal receipt
                let receipt = self.receipt_generator.generate_refusal(&request, &refusal);

                // Log refusal
                self.audit_log.record_refusal(&receipt);

                // Return receipt (NOT error - refusal is expected behavior)
                Err(receipt)
            }
        }
    }

    fn identify_channel(&self, request: &RawRequest) -> Result<ChannelId, RefusalReceipt> {
        // Check explicit channel specification
        if let Some(channel) = request.metadata.get("channel") {
            return channel.parse::<ChannelId>()
                .map_err(|_| RefusalReceipt::invalid_channel(channel));
        }

        // Infer from request properties
        let now = Utc::now();

        // Check if any scheduled slot is active
        if let Some(scheduled) = self.channels.get(&ChannelId::Scheduled) {
            if scheduled.is_open(now) {
                return Ok(ChannelId::Scheduled);
            }
        }

        // Check if batch intake is open
        if let Some(batch) = self.channels.get(&ChannelId::BatchIntake) {
            if batch.is_open(now) {
                return Ok(ChannelId::BatchIntake);
            }
        }

        // Check if emergency criteria met
        if request.metadata.get("severity") == Some(&"P0".to_string()) {
            return Ok(ChannelId::Emergency);
        }

        // Default to batch intake (will be queued for next window)
        Ok(ChannelId::BatchIntake)
    }
}
```

### Channel Implementations

```rust
pub struct BatchIntake {
    schedule: CronExpression,
    window_duration: Duration,
    max_packets_per_batch: usize,
    current_batch: Vec<PacketId>,
    last_opened: Option<DateTime<Utc>>,
    audit_log: AuditLog,
}

impl BatchIntake {
    pub fn new(schedule: CronExpression) -> Self {
        Self {
            schedule,
            window_duration: Duration::hours(1),
            max_packets_per_batch: 20,
            current_batch: Vec::new(),
            last_opened: None,
            audit_log: AuditLog::new(),
        }
    }
}

impl Channel for BatchIntake {
    fn is_open(&self, timestamp: DateTime<Utc>) -> bool {
        // Check if we're in the scheduled window
        if !self.schedule.matches(timestamp) {
            return false;
        }

        // Check if window hasn't expired
        if let Some(opened) = self.last_opened {
            let elapsed = timestamp.signed_duration_since(opened);
            if elapsed > self.window_duration {
                return false;
            }
        }

        true
    }

    fn accept(&mut self, request: RawRequest) -> Result<WorkPacket, Refusal> {
        let now = Utc::now();

        // Verify channel is open
        if !self.is_open(now) {
            return Err(Refusal::OutsideWindow {
                next_window: self.schedule.next_occurrence(),
                reason: "Batch intake only processes during scheduled window (08:00-09:00 daily)".to_string(),
            });
        }

        // Update last_opened if this is first packet in window
        if self.last_opened.is_none() ||
           now.signed_duration_since(self.last_opened.unwrap()) > self.window_duration {
            self.last_opened = Some(now);
            self.current_batch.clear();
        }

        // Check capacity
        if self.current_batch.len() >= self.max_packets_per_batch {
            return Err(Refusal::CapacityExceeded {
                retry_at: self.schedule.next_occurrence(),
            });
        }

        // Validate request can be converted to packet
        let packet = WorkPacket::try_from(request)?;

        // Add to current batch
        self.current_batch.push(packet.id.clone());

        // Log acceptance
        self.audit_log.record(AcceptanceEvent {
            packet_id: packet.id.clone(),
            timestamp: now,
            channel: "BatchIntake".to_string(),
        });

        Ok(packet)
    }

    fn next_opening(&self) -> Option<DateTime<Utc>> {
        Some(self.schedule.next_occurrence())
    }

    fn metadata(&self) -> ChannelMetadata {
        ChannelMetadata {
            id: ChannelId::BatchIntake,
            name: "Batch Intake".to_string(),
            description: "Primary ingress for non-urgent work".to_string(),
            schedule: Some(self.schedule.to_string()),
            max_packets: Some(self.max_packets_per_batch),
        }
    }
}
```

### Work Order Processing

```rust
pub struct WorkQueue {
    packets: VecDeque<WorkPacket>,
    processing: HashMap<PacketId, ProcessingState>,
}

impl WorkQueue {
    pub fn enqueue(&mut self, packet: WorkPacket) {
        // Insert sorted by priority
        let insert_pos = self.packets
            .iter()
            .position(|p| p.priority < packet.priority)
            .unwrap_or(self.packets.len());

        self.packets.insert(insert_pos, packet);
    }

    pub fn dequeue(&mut self) -> Option<WorkPacket> {
        // Get highest priority packet that's not blocked
        for i in 0..self.packets.len() {
            let packet = &self.packets[i];

            // Check if dependencies are satisfied
            let blocked = packet.dependencies.blocked_by
                .iter()
                .any(|dep| self.is_incomplete(dep));

            if !blocked {
                let packet = self.packets.remove(i).unwrap();
                self.processing.insert(packet.id.clone(), ProcessingState::InProgress);
                return Some(packet);
            }
        }

        None
    }

    fn is_incomplete(&self, packet_id: &PacketId) -> bool {
        match self.processing.get(packet_id) {
            Some(ProcessingState::Complete) => false,
            _ => true,
        }
    }
}

pub struct WorkExecutor {
    executor: Arc<dyn Executor>,
    receipt_generator: Arc<dyn ReceiptGenerator>,
}

impl WorkExecutor {
    pub async fn process(&self, packet: WorkPacket) -> Result<WorkReceipt, WorkError> {
        // 1. Execute work
        let result = self.executor.execute(&packet).await?;

        // 2. Verify acceptance criteria
        self.executor.verify(&result, &packet.acceptance_test)?;

        // 3. Generate receipt
        let receipt = self.receipt_generator.generate(&packet, &result);

        Ok(receipt)
    }
}
```

---

## Protocol Specifications

### Batch Intake Protocol

**Endpoint**: `POST /intake/batch`

**Schedule**: Daily 08:00-09:00 UTC (configurable)

**Request Format**:
```json
{
  "objective": {
    "summary": "Add user authentication",
    "description": "Implement OAuth2 flow with Google provider",
    "success_criteria": [
      "Users can sign in with Google",
      "JWT tokens issued after authentication",
      "Refresh token flow works"
    ],
    "context": "Needed for public beta launch"
  },
  "constraints": {
    "deadline": "2026-03-01T00:00:00Z",
    "max_effort": "P5D",
    "technologies": ["Rust", "OAuth2"],
    "quality_gates": [
      {"type": "test_coverage", "min": 80},
      {"type": "security_scan", "severity": "high"}
    ]
  },
  "acceptance_test": {
    "test_type": "automated",
    "steps": [
      "Click 'Sign in with Google'",
      "Complete OAuth flow",
      "Verify JWT token received",
      "Verify user redirected to dashboard"
    ],
    "expected_outcome": "User successfully authenticated",
    "verification_method": "ci"
  },
  "reversibility": {
    "is_reversible": true,
    "rollback_plan": {
      "steps": ["Remove OAuth routes", "Clear OAuth config"],
      "time_to_rollback": "PT5M",
      "data_preservation": true
    },
    "risk_level": "medium"
  },
  "dependencies": {
    "blocks": [],
    "blocked_by": ["FEAT-2024-010"],
    "related": ["DOC-2024-020"]
  },
  "requester": "alice@example.com",
  "owner": "bob@example.com",
  "priority": "high"
}
```

**Response (Success)**:
```json
{
  "status": "accepted",
  "packet_id": "FEAT-2024-042",
  "timestamp": "2026-02-09T08:15:23Z",
  "eta": "2026-02-12T17:00:00Z",
  "queue_position": 3,
  "signature": "a3f8b2c1d4e5f6a7..."
}
```

**Response (Refusal)**:
```json
{
  "status": "refused",
  "refusal": {
    "type": "outside_window",
    "reason": "Batch intake only open 08:00-09:00 UTC",
    "next_window": "2026-02-10T08:00:00Z"
  },
  "receipt_id": "REF-2024-1523",
  "timestamp": "2026-02-09T15:23:45Z",
  "signature": "b4c5d6e7f8a9b0c1..."
}
```

---

### Scheduled Interface Protocol

**Endpoint**: `POST /interface/scheduled`

**Schedule**: Pre-configured calendar slots

**Request Format**:
```json
{
  "slot_id": "standup-2026-02-09-0900",
  "participant_id": "alice@example.com",
  "topic": "Sprint planning",
  "packet": {
    // Full work packet structure
  }
}
```

**Response (Success)**:
```json
{
  "status": "accepted",
  "packet_id": "PLAN-2024-015",
  "slot": {
    "start": "2026-02-09T09:00:00Z",
    "end": "2026-02-09T09:15:00Z",
    "purpose": "Daily Standup"
  },
  "timestamp": "2026-02-09T09:02:34Z"
}
```

**Response (Refusal - No Active Slot)**:
```json
{
  "status": "refused",
  "refusal": {
    "type": "no_active_slot",
    "reason": "No scheduled interface slot active",
    "next_slot": {
      "start": "2026-02-09T14:00:00Z",
      "end": "2026-02-09T15:00:00Z",
      "purpose": "Sprint Planning"
    }
  },
  "receipt_id": "REF-2024-1524",
  "timestamp": "2026-02-09T10:45:12Z"
}
```

---

### Emergency Protocol

**Endpoint**: `POST /emergency`

**Schedule**: Always available (with validation)

**Request Format**:
```json
{
  "severity": "P0",
  "production_impact": true,
  "revenue_impact_usd": 5000,
  "incident_commander": "alice@example.com",
  "description": "Production database unreachable",
  "impact": "All API requests failing with 500 errors",
  "customer_count_affected": 1200,
  "packet": {
    "objective": {
      "summary": "Restore database connectivity",
      "success_criteria": [
        "Database responds to health checks",
        "API success rate > 99%",
        "No error spikes in logs"
      ]
    },
    "acceptance_test": {
      "test_type": "manual",
      "steps": [
        "Verify DB health endpoint returns 200",
        "Run 100 sample API requests",
        "Check error rate in monitoring"
      ]
    }
  }
}
```

**Response (Success)**:
```json
{
  "status": "accepted",
  "packet_id": "EMR-2024-003",
  "incident_id": "INC-2024-0156",
  "responders_alerted": [
    "oncall-eng@example.com",
    "oncall-sre@example.com",
    "cto@example.com"
  ],
  "war_room": "https://zoom.us/emergency/abc123",
  "timestamp": "2026-02-09T23:47:11Z"
}
```

**Response (Refusal - Insufficient Severity)**:
```json
{
  "status": "refused",
  "refusal": {
    "type": "insufficient_severity",
    "claimed": "P1",
    "required": "P0",
    "reason": "Emergency channel requires P0 severity (production down, data loss, security breach)",
    "redirect_to": "batch_intake",
    "guidelines": "https://docs.example.com/severity-levels"
  },
  "receipt_id": "REF-2024-1525",
  "timestamp": "2026-02-09T11:30:22Z"
}
```

---

### Refusal Protocol

**All refusals must include**:

1. **Receipt ID**: Unique identifier for tracking
2. **Timestamp**: When refusal occurred
3. **Refusal Type**: Category (outside_window, insufficient_severity, etc.)
4. **Reason**: Human-readable explanation
5. **Retry Information**: How to submit properly
6. **Signature**: Cryptographic hash for audit trail

**Refusal Storage**:
```bash
.firewall/refusals/
├── 2026-02-09/
│   ├── REF-2024-1523.json
│   ├── REF-2024-1524.json
│   └── REF-2024-1525.json
└── index.json  # Searchable index
```

---

## Enforcement Patterns

### Type-Level Enforcement

Use Rust's type system to make firewall violations impossible:

```rust
// Phantom type states for work packets
pub struct Unvalidated;
pub struct Validated;
pub struct Executed;
pub struct Verified;

pub struct WorkPacket<State = Unvalidated> {
    data: WorkPacketData,
    _state: PhantomData<State>,
}

// Can only be created in Unvalidated state
impl WorkPacket<Unvalidated> {
    pub fn new(data: WorkPacketData) -> Self {
        Self {
            data,
            _state: PhantomData,
        }
    }

    // Transition to Validated state (only via Channel)
    pub fn validate(self, channel: &dyn Channel) -> Result<WorkPacket<Validated>, Refusal> {
        channel.validate_packet(&self.data)?;
        Ok(WorkPacket {
            data: self.data,
            _state: PhantomData,
        })
    }
}

// Can only execute validated packets
impl WorkPacket<Validated> {
    pub async fn execute(self, executor: &dyn Executor) -> Result<WorkPacket<Executed>, ExecutionError> {
        let result = executor.execute(&self.data).await?;
        Ok(WorkPacket {
            data: self.data.with_result(result),
            _state: PhantomData,
        })
    }
}

// Can only verify executed packets
impl WorkPacket<Executed> {
    pub fn verify(self) -> Result<WorkPacket<Verified>, VerificationError> {
        self.data.result.verify_acceptance_test(&self.data.acceptance_test)?;
        Ok(WorkPacket {
            data: self.data,
            _state: PhantomData,
        })
    }
}

// Can only generate receipts for verified packets
impl WorkPacket<Verified> {
    pub fn receipt(self) -> WorkReceipt {
        WorkReceipt::from(self.data)
    }
}

// This makes it IMPOSSIBLE to:
// - Execute unvalidated work (no execute() method on Unvalidated)
// - Skip verification (no receipt() method on Executed)
// - Process work outside channels (only channels can validate)
```

### Runtime Enforcement

```rust
pub struct FirewallGuard {
    allowed_channels: HashSet<ChannelId>,
    audit_log: AuditLog,
}

impl FirewallGuard {
    pub fn check_request(&self, request: &RawRequest) -> Result<(), Violation> {
        // 1. Verify request came from allowed channel
        let channel_id = request.metadata
            .get("channel_id")
            .ok_or(Violation::NoChannel)?
            .parse::<ChannelId>()
            .map_err(|_| Violation::InvalidChannel)?;

        if !self.allowed_channels.contains(&channel_id) {
            self.audit_log.record_violation(Violation::UnauthorizedChannel {
                attempted: channel_id,
                allowed: self.allowed_channels.clone(),
            });
            return Err(Violation::UnauthorizedChannel {
                attempted: channel_id,
                allowed: self.allowed_channels.clone(),
            });
        }

        // 2. Verify request has valid signature
        let signature = request.metadata
            .get("signature")
            .ok_or(Violation::NoSignature)?;

        self.verify_signature(request, signature)?;

        // 3. Verify request is not replayed (check timestamp)
        let timestamp = request.timestamp;
        let age = Utc::now().signed_duration_since(timestamp);
        if age > Duration::minutes(5) {
            return Err(Violation::RequestTooOld { age });
        }

        Ok(())
    }

    fn verify_signature(&self, request: &RawRequest, signature: &str) -> Result<(), Violation> {
        // Implement signature verification
        // Use HMAC-SHA256 or similar
        todo!()
    }
}
```

### Audit Trail

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEvent {
    pub timestamp: DateTime<Utc>,
    pub event_type: AuditEventType,
    pub details: serde_json::Value,
    pub signature: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuditEventType {
    PacketAccepted { packet_id: PacketId, channel: ChannelId },
    PacketRefused { receipt_id: String, reason: String },
    PacketExecuted { packet_id: PacketId, duration: Duration },
    PacketVerified { packet_id: PacketId },
    ViolationDetected { violation: Violation },
}

pub struct AuditLog {
    events: Vec<AuditEvent>,
    storage: Box<dyn AuditStorage>,
}

impl AuditLog {
    pub fn record(&mut self, event_type: AuditEventType, details: serde_json::Value) {
        let event = AuditEvent {
            timestamp: Utc::now(),
            event_type,
            details,
            signature: self.sign_event(&event_type, &details),
        };

        self.events.push(event.clone());
        self.storage.persist(event);
    }

    pub fn query(&self, filter: AuditFilter) -> Vec<AuditEvent> {
        self.events
            .iter()
            .filter(|e| filter.matches(e))
            .cloned()
            .collect()
    }

    fn sign_event(&self, event_type: &AuditEventType, details: &serde_json::Value) -> String {
        let mut hasher = Sha256::new();
        hasher.update(serde_json::to_string(event_type).unwrap().as_bytes());
        hasher.update(serde_json::to_string(details).unwrap().as_bytes());
        hex::encode(hasher.finalize())
    }
}
```

---

## Real-World Example: Day-to-Day Operations

### Scenario 1: Morning Planning (Batch Intake)

**Time**: 08:15 AM

**Action**: Alice submits three work requests during batch intake window

```bash
# Request 1: Feature
curl -X POST https://firewall.example.com/intake/batch \
  -H "Content-Type: application/json" \
  -d @feature-rbac.json

# Response:
{
  "status": "accepted",
  "packet_id": "FEAT-2024-042",
  "eta": "2026-02-12T17:00:00Z",
  "queue_position": 1
}

# Request 2: Bug Fix
curl -X POST https://firewall.example.com/intake/batch \
  -H "Content-Type: application/json" \
  -d @bug-session-leak.json

# Response:
{
  "status": "accepted",
  "packet_id": "BUG-2024-043",
  "eta": "2026-02-10T12:00:00Z",
  "queue_position": 2
}

# Request 3: Research
curl -X POST https://firewall.example.com/intake/batch \
  -H "Content-Type: application/json" \
  -d @research-rdf-stores.json

# Response:
{
  "status": "accepted",
  "packet_id": "RES-2024-008",
  "eta": "2026-02-16T17:00:00Z",
  "queue_position": 7
}
```

**Outcome**: All three requests accepted, queued for processing, with clear ETAs.

---

### Scenario 2: Interruption Attempt (Refused)

**Time**: 10:30 AM (outside batch intake window)

**Action**: Bob sends Slack message: "Hey Alice, quick question about the auth bug?"

**Firewall Detection**:
```rust
// Slack webhook triggers firewall
let request = RawRequest {
    source: "slack",
    requester: "bob@example.com",
    message: "Hey Alice, quick question about the auth bug?",
    timestamp: Utc::now(),
};

let result = firewall.process_request(request);

match result {
    Err(refusal_receipt) => {
        slack_client.send_dm(
            "bob@example.com",
            format_refusal_receipt(&refusal_receipt)
        );
    }
    _ => {}
}
```

**Bob receives**:
```
╔══════════════════════════════════════════════════════════╗
║  FIREWALL REFUSAL RECEIPT                                ║
║                                                          ║
║  Receipt ID: REF-2024-1530                               ║
║  Timestamp: 2026-02-09T10:30:45Z                         ║
║                                                          ║
║  Your message was received via Slack, which is not an    ║
║  authorized ingress channel for work requests.           ║
║                                                          ║
║  To submit work requests:                                ║
║                                                          ║
║  1. Batch Intake (Non-urgent)                            ║
║     Opens: Tomorrow 08:00-09:00 UTC                      ║
║     Submit: https://intake.example.com                   ║
║                                                          ║
║  2. Scheduled Interface (Collaborative)                  ║
║     Next: Today 14:00-15:00 (Sprint Planning)            ║
║     Book: https://calendar.example.com                   ║
║                                                          ║
║  3. Emergency (P0 Only)                                  ║
║     Only for: Production down, data loss                 ║
║     Trigger: https://emergency.example.com               ║
║                                                          ║
║  Documentation: https://docs.example.com/firewall        ║
╚══════════════════════════════════════════════════════════╝
```

**Outcome**: Bob understands proper channels, Alice's focus protected, request can be submitted properly tomorrow.

---

### Scenario 3: Scheduled Review (Scheduled Interface)

**Time**: 14:00 PM (Sprint Planning meeting)

**Action**: Team discusses upcoming work during scheduled slot

```rust
// Meeting is in calendar, slot is active
let slot = ScheduledSlot {
    start: Utc.ymd(2026, 2, 9).and_hms(14, 0, 0),
    end: Utc.ymd(2026, 2, 9).and_hms(15, 0, 0),
    purpose: "Sprint Planning",
    participants: vec![
        "alice@example.com",
        "bob@example.com",
        "carol@example.com",
    ],
};

// During meeting, work requests accepted
for request in meeting_requests {
    let result = scheduled_interface.accept(request);
    // All accepted because we're in authorized slot
}
```

**Packets Created**:
- Sprint goals defined
- 5 user stories converted to work packets
- Dependencies mapped
- All packets queued for processing

**Outcome**: Productive meeting, all work properly documented, no ad-hoc requests outside meeting.

---

### Scenario 4: Production Outage (Emergency)

**Time**: 11:47 PM

**Action**: Database becomes unreachable, triggering alerts

```bash
# Oncall engineer triggers emergency protocol
curl -X POST https://firewall.example.com/emergency \
  -H "Content-Type: application/json" \
  -d '{
    "severity": "P0",
    "production_impact": true,
    "revenue_impact_usd": 8000,
    "incident_commander": "oncall@example.com",
    "description": "Primary database unreachable",
    "impact": "All API requests failing",
    "customer_count_affected": 2400,
    "packet": {
      "objective": {
        "summary": "Restore database connectivity",
        "success_criteria": [
          "Database health check passes",
          "API error rate < 1%"
        ]
      }
    }
  }'

# Response:
{
  "status": "accepted",
  "packet_id": "EMR-2024-005",
  "incident_id": "INC-2024-0178",
  "responders_alerted": [
    "oncall-eng@example.com",
    "oncall-sre@example.com",
    "cto@example.com"
  ],
  "war_room": "https://zoom.us/emergency/xyz789",
  "timestamp": "2026-02-09T23:47:11Z"
}
```

**Outcome**: Emergency properly classified, responders alerted, work documented, postmortem data captured.

---

## Integration with ggen

The Life Firewall integrates naturally with ggen's specification-driven approach:

```turtle
# .specify/firewall.ttl - Firewall configuration as ontology

@prefix fw: <http://example.org/firewall#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

fw:BatchIntakeChannel a fw:Channel ;
    fw:schedule "0 8 * * *" ;  # Daily 08:00
    fw:windowDuration "PT1H" ;
    fw:maxPacketsPerBatch 20 ;
    fw:priority 2 .

fw:ScheduledInterfaceChannel a fw:Channel ;
    fw:calendar fw:TeamCalendar ;
    fw:allowedParticipants (
        fw:Alice
        fw:Bob
        fw:Carol
    ) ;
    fw:priority 1 .

fw:EmergencyChannel a fw:Channel ;
    fw:severityThreshold fw:P0 ;
    fw:cooldownPeriod "PT4H" ;
    fw:validationRules (
        fw:ProductionImpactRule
        fw:RevenueImpactRule
        fw:IncidentCommanderRule
    ) ;
    fw:priority 0 .

# Generate Rust code from ontology
# ggen sync generates:
# - src/channels/batch_intake.rs
# - src/channels/scheduled_interface.rs
# - src/channels/emergency.rs
# - src/firewall.rs
```

**Benefits**:
- Firewall configuration is specification, not code
- Changes to rules regenerate implementation
- Audit trail captures ontology version used
- Deterministic firewall behavior

---

## Common Questions & Answers

**Q: Isn't this too rigid? What if someone really needs help urgently?**

A: The Emergency channel exists for true urgencies. Most "urgent" requests are actually "I want this now" not "this is breaking production." The firewall trains people to distinguish between the two.

**Q: Won't people get frustrated with refusals?**

A: Initially, yes. But:
1. Refusal receipts are educational (teach proper channels)
2. Consistency builds expectations (people learn the system)
3. Protected focus time delivers better results faster
4. Batch processing actually improves response times (no context switching)

**Q: How do I handle executives who expect immediate responses?**

A: Two approaches:
1. Add them to scheduled interface slots (dedicated time)
2. Use batch intake for their requests (24-hour SLA is usually fine)

Most executives respect boundaries when they see productivity increase.

**Q: What if batch intake fills up?**

A: Capacity exceeded refusal includes retry time. Options:
1. Increase max_packets_per_batch
2. Add second batch window (e.g., 16:00-17:00)
3. Prioritize ruthlessly (not everything is worth doing)

**Q: How do I transition my team to this system?**

A:
1. Week 1: Announce firewall, share documentation
2. Week 2: Soft launch (warn before refusing)
3. Week 3: Hard launch (refuse with receipts)
4. Week 4: Review metrics, adjust windows

Give people 2 weeks to adapt.

**Q: What about ongoing conversations (e.g., code reviews)?**

A: Scheduled interface slots can be recurring (e.g., daily 15:00-15:30 for code review). Or use batch intake for async review requests.

**Q: How do I measure success?**

A: Track these metrics:
- Interruptions per day (should decrease)
- Deep work time blocks (should increase)
- Time to completion (should decrease due to less context switching)
- Refusal rate (should decrease as people learn system)
- Emergency channel usage (should be rare, <1/week)

---

## Metrics and Success Criteria

**Track these KPIs**:

```rust
pub struct FirewallMetrics {
    pub period: DateRange,

    // Channel usage
    pub batch_intake_packets: usize,
    pub scheduled_interface_packets: usize,
    pub emergency_packets: usize,

    // Refusals
    pub total_refusals: usize,
    pub refusal_by_reason: HashMap<RefusalReason, usize>,

    // Processing
    pub avg_queue_time: Duration,
    pub avg_execution_time: Duration,
    pub completion_rate: f64, // % of packets completed

    // Quality
    pub acceptance_test_pass_rate: f64,
    pub rollback_rate: f64,

    // Cognitive protection
    pub deep_work_blocks: Vec<Duration>, // Uninterrupted periods
    pub context_switches: usize,
    pub interruption_cost: Duration, // 23 min per interruption
}

impl FirewallMetrics {
    pub fn cognitive_savings(&self) -> Duration {
        // Calculate time saved by preventing interruptions
        let prevented_interruptions = self.total_refusals;
        let cost_per_interruption = Duration::minutes(23);
        Duration::from(prevented_interruptions as i64 * cost_per_interruption.num_minutes())
    }

    pub fn report(&self) -> String {
        format!(r#"
Firewall Metrics ({} to {})
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Channel Usage:
  Batch Intake:        {:4} packets
  Scheduled Interface: {:4} packets
  Emergency:           {:4} packets

Refusals:
  Total:               {:4}
  Outside Window:      {:4}
  Insufficient Severity: {:4}
  Capacity Exceeded:   {:4}

Processing:
  Avg Queue Time:      {}
  Avg Execution Time:  {}
  Completion Rate:     {:.1}%

Quality:
  Acceptance Pass Rate: {:.1}%
  Rollback Rate:       {:.1}%

Cognitive Protection:
  Deep Work Blocks:    {:4} (avg: {})
  Context Switches:    {:4}
  Time Saved:          {} (prevented interruptions)

Channel Health:
  Emergency Usage:     {} (target: <1/week)
  Refusal Trend:       {} (should decrease over time)
"#,
            self.period.start,
            self.period.end,
            self.batch_intake_packets,
            self.scheduled_interface_packets,
            self.emergency_packets,
            self.total_refusals,
            self.refusal_by_reason.get(&RefusalReason::OutsideWindow).unwrap_or(&0),
            self.refusal_by_reason.get(&RefusalReason::InsufficientSeverity).unwrap_or(&0),
            self.refusal_by_reason.get(&RefusalReason::CapacityExceeded).unwrap_or(&0),
            format_duration(self.avg_queue_time),
            format_duration(self.avg_execution_time),
            self.completion_rate * 100.0,
            self.acceptance_test_pass_rate * 100.0,
            self.rollback_rate * 100.0,
            self.deep_work_blocks.len(),
            format_duration(avg_duration(&self.deep_work_blocks)),
            self.context_switches,
            format_duration(self.cognitive_savings()),
            if self.emergency_packets <= 1 { "✅" } else { "⚠️" },
            "📉" // TODO: Calculate trend
        )
    }
}
```

**Success Criteria**:

- Emergency channel usage: <1 per week
- Refusal rate: Decreases 50% after 4 weeks
- Deep work blocks: ≥2 hours continuous, ≥3 per day
- Context switches: <5 per day (down from 40+)
- Completion rate: ≥90%
- Time saved: ≥2 hours per day (vs. traditional)

---

## Next Steps

1. **Implement Core Firewall**
   ```bash
   # Generate from ontology
   ggen sync --spec .specify/firewall.ttl

   # Run tests
   cargo make test
   ```

2. **Configure Channels**
   - Set batch intake schedule (team preference)
   - Add scheduled interface slots (calendar sync)
   - Define emergency validation rules

3. **Roll Out Gradually**
   - Week 1: Announce + document
   - Week 2: Soft launch (warnings)
   - Week 3: Hard launch (refusals)
   - Week 4: Review metrics

4. **Educate Team**
   - Share this documentation
   - Run firewall workshop
   - Create request templates
   - Set clear expectations

5. **Monitor and Adjust**
   - Weekly metrics review
   - Adjust windows based on usage
   - Refine validation rules
   - Celebrate wins (time saved, focus improved)

---

**Remember the core principle:**

> "Work = μ(Packet). No packet, no work. Protect the perimeter, validate the packets, and cognitive resources compound."

The Life Firewall isn't about being unresponsive. It's about being *intentionally* responsive through validated, documented, accountable channels.

---

**Further Reading:**
- [Five-Stage Pipeline](./fundamentals/five-stage-pipeline.md) - How work packets flow through μ₁-μ₅
- [Chicago TDD](../.claude/rules/rust/testing.md) - Testing work packet processing
- [Andon Signals](../.claude/rules/andon/signals.md) - Stop the line when firewall violated
- Research: "The Cost of Interrupted Work" (Gloria Mark, UC Irvine)
- Research: "Deep Work" (Cal Newport)

---

**Version**: 1.0
**Status**: Specification Complete
**Created**: 2026-02-09
**Last Updated**: 2026-02-09
