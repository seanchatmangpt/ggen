<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packet Discipline: Type-Safe Work Orders](#packet-discipline-type-safe-work-orders)
  - [TL;DR](#tldr)
  - [Table of Contents](#table-of-contents)
  - [The Problem: Informal Work Requests](#the-problem-informal-work-requests)
    - [Traditional Request Handling](#traditional-request-handling)
    - [The Packet Discipline Insight](#the-packet-discipline-insight)
  - [The Solution: Work Packets](#the-solution-work-packets)
    - [Core Metaphor: Network Packets](#core-metaphor-network-packets)
    - [The Workflow Analogy](#the-workflow-analogy)
  - [Work Order Structure](#work-order-structure)
    - [Six Required Fields](#six-required-fields)
      - [1. Objective (The "What")](#1-objective-the-what)
      - [2. Constraints (The "Boundaries")](#2-constraints-the-boundaries)
      - [3. Acceptance Test (The "Done")](#3-acceptance-test-the-done)
      - [4. Reversibility Plan (The "Undo")](#4-reversibility-plan-the-undo)
      - [5. Dependencies (The "Prerequisites")](#5-dependencies-the-prerequisites)
      - [6. Owner (The "Who")](#6-owner-the-who)
  - [Type System for Work Orders](#type-system-for-work-orders)
    - [Core Types in Rust](#core-types-in-rust)
  - [Validation and Admission Control](#validation-and-admission-control)
    - [Validation Pipeline](#validation-pipeline)
  - [Refusal Conditions and Receipts](#refusal-conditions-and-receipts)
    - [When to Refuse Work Packets](#when-to-refuse-work-packets)
    - [Refusal Receipt](#refusal-receipt)
    - [Example Refusal](#example-refusal)
  - [Converting Informal to Typed](#converting-informal-to-typed)
    - [Transformation Process](#transformation-process)
    - [Example Transformation](#example-transformation)
      - [Before: Informal Request](#before-informal-request)
      - [Questioning Process](#questioning-process)
      - [After: Typed Work Packet](#after-typed-work-packet)
  - [Well-Formed vs Malformed Examples](#well-formed-vs-malformed-examples)
    - [Example 1: API Performance Optimization](#example-1-api-performance-optimization)
      - [❌ Malformed](#-malformed)
      - [✅ Well-Formed](#-well-formed)
    - [Example 2: Add User Phone Number Field](#example-2-add-user-phone-number-field)
      - [❌ Malformed](#-malformed-1)
      - [✅ Well-Formed](#-well-formed-1)
  - [Implementation Patterns](#implementation-patterns)
    - [Pattern 1: Work Packet Builder](#pattern-1-work-packet-builder)
    - [Pattern 2: Work Packet State Machine](#pattern-2-work-packet-state-machine)
  - [Integration with ggen Workflow](#integration-with-ggen-workflow)
    - [Work Packets as RDF](#work-packets-as-rdf)
    - [Querying Work Packets with SPARQL](#querying-work-packets-with-sparql)
    - [Generation from Work Packets](#generation-from-work-packets)
  - [Conclusion](#conclusion)
    - [The Packet Discipline Mindset](#the-packet-discipline-mindset)
    - [The Type System Analogy](#the-type-system-analogy)
  - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packet Discipline: Type-Safe Work Orders

**Reading Time**: 25-30 minutes | **Difficulty**: Intermediate | **Prerequisites**: Understanding of Rust type systems, basic RDF concepts

---

## TL;DR

**Packet discipline transforms informal requests into typed, validated work orders.**

Traditional development accepts vague requests ("make it faster"). Packet discipline requires structured work packets with explicit objectives, constraints, acceptance tests, and reversibility plans.

Once you adopt packet discipline, you cannot accept malformed work. Like type systems prevent runtime errors, work packets prevent wasted effort.

---

## Table of Contents

1. [The Problem: Informal Work Requests](#the-problem-informal-work-requests)
2. [The Solution: Work Packets](#the-solution-work-packets)
3. [Work Order Structure](#work-order-structure)
4. [Type System for Work Orders](#type-system-for-work-orders)
5. [Validation and Admission Control](#validation-and-admission-control)
6. [Refusal Conditions and Receipts](#refusal-conditions-and-receipts)
7. [Converting Informal to Typed](#converting-informal-to-typed)
8. [Well-Formed vs Malformed Examples](#well-formed-vs-malformed-examples)
9. [Implementation Patterns](#implementation-patterns)
10. [Integration with ggen Workflow](#integration-with-ggen-workflow)

---

## The Problem: Informal Work Requests

### Traditional Request Handling

```
Manager: "Make the API faster"
Developer: "How much faster?"
Manager: "Just faster"
Developer: "By when?"
Manager: "ASAP"
Developer: "What's the success criteria?"
Manager: "Users shouldn't complain"
```

**Result**: Wasted weeks optimizing the wrong thing. No clear definition of done. No way to verify success.

### The Packet Discipline Insight

**Every work request is a function:**

```rust
type WorkRequest = Fn(CurrentState) -> Result<NewState, WorkError>;
```

But most requests lack:
- **Type signature** (what inputs/outputs?)
- **Preconditions** (what must be true before?)
- **Postconditions** (what must be true after?)
- **Error handling** (what if it fails?)
- **Reversibility** (can we undo?)

**Packet discipline makes these explicit.**

---

## The Solution: Work Packets

### Core Metaphor: Network Packets

Network packets contain:
- **Header** (metadata: source, destination, protocol)
- **Payload** (actual data)
- **Checksum** (validation)
- **TTL** (time to live)

**Work packets contain:**
- **Objective** (what to achieve)
- **Constraints** (boundaries and limits)
- **Acceptance Test** (how to verify success)
- **Reversibility Plan** (how to undo if needed)
- **Dependencies** (what must exist first)
- **Owner** (who is accountable)

### The Workflow Analogy

```
NETWORK PACKET:
┌─────────────────────────────────────┐
│ Header: IP=192.168.1.1 → 10.0.0.5   │
│ Protocol: TCP, Port: 443            │
│ Checksum: 0x4AF2                    │
│ Payload: [encrypted data]           │
│ TTL: 64 hops                        │
└─────────────────────────────────────┘
      ↓
   Validated at each router
      ↓
   Delivered or dropped (with reason)


WORK PACKET:
┌─────────────────────────────────────┐
│ Objective: Reduce API latency       │
│ Constraint: p95 < 200ms             │
│ Test: Load test shows p95 < 200ms   │
│ Reversibility: Feature flag toggle  │
│ Dependencies: Metrics pipeline      │
│ Owner: @alice                       │
└─────────────────────────────────────┘
      ↓
   Validated before acceptance
      ↓
   Executed or refused (with reason)
```

---

## Work Order Structure

### Six Required Fields

Every work packet MUST contain:

#### 1. Objective (The "What")

**Definition**: Observable, measurable outcome.

**Format**:
```
[Verb] [Object] [Success Criteria]
```

**Good Examples**:
- ✅ "Reduce API p95 latency to <200ms"
- ✅ "Add phone number field to User model with validation"
- ✅ "Generate TypeScript types from ontology with 100% property coverage"

**Bad Examples**:
- ❌ "Make it faster" (no measurement)
- ❌ "Improve user experience" (not observable)
- ❌ "Fix the bug" (no specific bug ID)

---

#### 2. Constraints (The "Boundaries")

**Definition**: Hard limits that cannot be violated.

**Types**:
- **Time** (deadline, timeout)
- **Resources** (memory, CPU, budget)
- **Dependencies** (no new external dependencies)
- **Compatibility** (backward compatible)
- **Quality** (test coverage ≥80%)

**Example**:
```rust
struct Constraints {
    deadline: Option<DateTime<Utc>>,
    max_duration_hours: u32,
    max_budget_usd: Option<f64>,
    breaking_changes_allowed: bool,
    min_test_coverage_percent: u8,
    max_new_dependencies: u8,
}
```

**Good Examples**:
- ✅ "Complete within 8 hours"
- ✅ "No new external dependencies"
- ✅ "Zero breaking changes to public API"
- ✅ "Memory usage increase <10MB"

**Bad Examples**:
- ❌ "As fast as possible" (unbounded)
- ❌ "Not too expensive" (vague)

---

#### 3. Acceptance Test (The "Done")

**Definition**: Executable verification of objective.

**Format**: Given-When-Then or executable command.

**Good Examples**:
```rust
// Executable acceptance test
#[test]
fn acceptance_reduce_api_latency() {
    // Given: Load test environment configured
    let load_tester = LoadTester::new("https://api.example.com");

    // When: 1000 requests/second for 60 seconds
    let results = load_tester
        .rate(1000)
        .duration(Duration::from_secs(60))
        .run();

    // Then: p95 latency < 200ms
    assert!(results.p95_latency_ms < 200.0);
}
```

```bash
# Executable acceptance test
cargo make bench | grep "p95:" | awk '{print $2}' | assert_less_than 200
```

**Bad Examples**:
- ❌ "Users are happy" (not executable)
- ❌ "Code looks good" (subjective)
- ❌ "No complaints after 1 week" (delayed feedback)

---

#### 4. Reversibility Plan (The "Undo")

**Definition**: How to restore previous state if objective fails or causes issues.

**Types**:
- **Feature flags** (toggle off)
- **Database migrations** (rollback scripts)
- **Deployment** (blue-green, canary rollback)
- **Git** (revert commit)
- **Configuration** (restore previous values)

**Good Examples**:
```rust
struct ReversibilityPlan {
    strategy: ReversibilityStrategy,
    estimated_rollback_time: Duration,
    rollback_command: String,
    verification_test: String,
}

enum ReversibilityStrategy {
    FeatureFlag { flag_name: String },
    GitRevert { commit_sha: String },
    DatabaseMigration { down_migration: String },
    ConfigRestore { backup_path: PathBuf },
    BlueGreenSwitch { previous_version: String },
}
```

**Concrete Examples**:
- ✅ "Disable via feature flag `api_optimization_v2`"
- ✅ "Run `diesel migration revert` to rollback schema"
- ✅ "Git revert commit abc123 and redeploy"

**Bad Examples**:
- ❌ "Rewrite the code again" (not a plan)
- ❌ "Hope it doesn't break" (no plan)
- ❌ "It's irreversible" (requires special approval)

---

#### 5. Dependencies (The "Prerequisites")

**Definition**: What must exist or be true before work can start.

**Types**:
- **Technical** (libraries, services, infrastructure)
- **Knowledge** (documentation, expertise)
- **Permissions** (access, approvals)
- **Blockers** (other work packets must complete first)

**Good Examples**:
```rust
struct Dependencies {
    required_services: Vec<ServiceDependency>,
    required_knowledge: Vec<String>,
    blocked_by_work_packets: Vec<WorkPacketId>,
    required_permissions: Vec<Permission>,
}

impl Dependencies {
    fn are_satisfied(&self) -> Result<(), Vec<DependencyError>> {
        let mut errors = Vec::new();

        // Check services are available
        for service in &self.required_services {
            if !service.is_available() {
                errors.push(DependencyError::ServiceUnavailable(service.name.clone()));
            }
        }

        // Check blockers are resolved
        for blocker_id in &self.blocked_by_work_packets {
            if !blocker_id.is_completed() {
                errors.push(DependencyError::BlockedBy(blocker_id.clone()));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
```

**Concrete Examples**:
- ✅ "Requires metrics pipeline deployed (WP-042)"
- ✅ "Blocked by ontology validation implementation (WP-038)"
- ✅ "Needs production database read access"

**Bad Examples**:
- ❌ "Everything should be ready" (vague)
- ❌ "No dependencies" (rarely true)

---

#### 6. Owner (The "Who")

**Definition**: Single person accountable for completion.

**Not a Team**: Ownership is singular for clarity.

**Good Examples**:
- ✅ `Owner { github: "@alice", email: "alice@example.com" }`
- ✅ `Owner { slack: "@bob", timezone: "America/New_York" }`

**Bad Examples**:
- ❌ "The backend team" (no single accountability)
- ❌ "TBD" (unassigned work)

---

## Type System for Work Orders

### Core Types in Rust

```rust
use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Unique identifier for work packets
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct WorkPacketId(String);

impl WorkPacketId {
    pub fn new(prefix: &str, sequence: u32) -> Self {
        Self(format!("{}-{:04}", prefix, sequence))
    }
}

/// Core work packet structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkPacket {
    pub id: WorkPacketId,
    pub objective: Objective,
    pub constraints: Constraints,
    pub acceptance_test: AcceptanceTest,
    pub reversibility: ReversibilityPlan,
    pub dependencies: Dependencies,
    pub owner: Owner,
    pub status: WorkPacketStatus,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Observable, measurable outcome
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Objective {
    pub description: String,
    pub success_criteria: Vec<SuccessCriterion>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SuccessCriterion {
    pub metric: String,
    pub operator: ComparisonOperator,
    pub threshold: f64,
    pub unit: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ComparisonOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equals,
}

/// Hard limits and boundaries
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Constraints {
    pub deadline: Option<DateTime<Utc>>,
    pub max_duration: Duration,
    pub max_budget_usd: Option<f64>,
    pub breaking_changes_allowed: bool,
    pub min_test_coverage_percent: u8,
    pub max_new_dependencies: u8,
    pub additional: Vec<String>,
}

/// Executable verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AcceptanceTest {
    pub test_type: TestType,
    pub command: String,
    pub expected_output: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestType {
    UnitTest { test_path: String },
    IntegrationTest { test_path: String },
    BenchmarkTest { threshold: f64, metric: String },
    ManualTest { checklist: Vec<String> },
    AutomatedScript { script_path: PathBuf },
}

/// Rollback strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReversibilityPlan {
    pub strategy: ReversibilityStrategy,
    pub estimated_rollback_time: Duration,
    pub rollback_command: String,
    pub verification_test: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReversibilityStrategy {
    FeatureFlag { flag_name: String },
    GitRevert { commit_sha: String },
    DatabaseMigration { down_migration_path: String },
    ConfigRestore { backup_path: PathBuf },
    BlueGreenSwitch { previous_version: String },
    Irreversible { justification: String },
}

/// Prerequisites
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependencies {
    pub required_services: Vec<ServiceDependency>,
    pub blocked_by: Vec<WorkPacketId>,
    pub required_permissions: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceDependency {
    pub name: String,
    pub endpoint: String,
    pub required_version: Option<String>,
}

/// Accountability
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Owner {
    pub github_handle: String,
    pub email: String,
    pub timezone: String,
}

/// State machine for work packets
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum WorkPacketStatus {
    Proposed,
    Validated,
    Accepted,
    InProgress,
    Blocked { reason: String },
    Completed,
    Refused { reason: String },
    Reverted { reason: String },
}
```

---

## Validation and Admission Control

### Validation Pipeline

```rust
use anyhow::{Result, Context, bail};

pub struct WorkPacketValidator;

impl WorkPacketValidator {
    /// Validate work packet before admission
    pub fn validate(packet: &WorkPacket) -> Result<ValidationReport> {
        let mut report = ValidationReport::new(packet.id.clone());

        // Stage 1: Structural validation
        Self::validate_structure(packet, &mut report)?;

        // Stage 2: Semantic validation
        Self::validate_semantics(packet, &mut report)?;

        // Stage 3: Dependency validation
        Self::validate_dependencies(packet, &mut report)?;

        // Stage 4: Constraint validation
        Self::validate_constraints(packet, &mut report)?;

        if report.errors.is_empty() {
            report.status = ValidationStatus::Valid;
            Ok(report)
        } else {
            report.status = ValidationStatus::Invalid;
            Ok(report)
        }
    }

    fn validate_structure(packet: &WorkPacket, report: &mut ValidationReport) -> Result<()> {
        // Objective must have at least one success criterion
        if packet.objective.success_criteria.is_empty() {
            report.errors.push(ValidationError::MissingSuccessCriteria);
        }

        // Acceptance test must be executable
        if packet.acceptance_test.command.is_empty() {
            report.errors.push(ValidationError::NonExecutableAcceptanceTest);
        }

        // Owner must be valid
        if packet.owner.github_handle.is_empty() || packet.owner.email.is_empty() {
            report.errors.push(ValidationError::InvalidOwner);
        }

        Ok(())
    }

    fn validate_semantics(packet: &WorkPacket, report: &mut ValidationReport) -> Result<()> {
        // Objective description must be concrete (check for vague words)
        let vague_words = ["better", "faster", "good", "improve", "optimize"];
        let desc_lower = packet.objective.description.to_lowercase();

        for word in &vague_words {
            if desc_lower.contains(word) && packet.objective.success_criteria.len() < 2 {
                report.warnings.push(ValidationWarning::VagueObjective {
                    word: word.to_string(),
                    suggestion: format!("Add specific success criteria for '{}'", word),
                });
            }
        }

        // Check constraints are reasonable
        if let Some(deadline) = packet.constraints.deadline {
            if deadline < Utc::now() {
                report.errors.push(ValidationError::DeadlineInPast);
            }
        }

        if packet.constraints.max_duration.num_hours() > 40 {
            report.warnings.push(ValidationWarning::LongDuration {
                hours: packet.constraints.max_duration.num_hours(),
            });
        }

        Ok(())
    }

    fn validate_dependencies(packet: &WorkPacket, report: &mut ValidationReport) -> Result<()> {
        // Check for circular dependencies
        for blocker_id in &packet.dependencies.blocked_by {
            if Self::creates_cycle(packet.id.clone(), blocker_id.clone())? {
                report.errors.push(ValidationError::CircularDependency {
                    packet_id: packet.id.clone(),
                    blocker_id: blocker_id.clone(),
                });
            }
        }

        // Verify blockers exist
        for blocker_id in &packet.dependencies.blocked_by {
            if !Self::packet_exists(blocker_id)? {
                report.errors.push(ValidationError::BlockerNotFound {
                    blocker_id: blocker_id.clone(),
                });
            }
        }

        Ok(())
    }

    fn validate_constraints(packet: &WorkPacket, report: &mut ValidationReport) -> Result<()> {
        // Test coverage must be reasonable
        if packet.constraints.min_test_coverage_percent > 100 {
            report.errors.push(ValidationError::InvalidTestCoverage);
        }

        // Warn if no reversibility plan for non-trivial work
        if matches!(packet.reversibility.strategy, ReversibilityStrategy::Irreversible { .. }) {
            if packet.constraints.max_duration.num_hours() > 4 {
                report.warnings.push(ValidationWarning::IrreversibleLongWork);
            }
        }

        Ok(())
    }

    fn creates_cycle(_packet_id: WorkPacketId, _blocker_id: WorkPacketId) -> Result<bool> {
        // Implementation: graph traversal to detect cycles
        // Simplified for example
        Ok(false)
    }

    fn packet_exists(_packet_id: &WorkPacketId) -> Result<bool> {
        // Implementation: query work packet database
        // Simplified for example
        Ok(true)
    }
}

#[derive(Debug, Clone)]
pub struct ValidationReport {
    pub packet_id: WorkPacketId,
    pub status: ValidationStatus,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
    pub validated_at: DateTime<Utc>,
}

impl ValidationReport {
    fn new(packet_id: WorkPacketId) -> Self {
        Self {
            packet_id,
            status: ValidationStatus::Pending,
            errors: Vec::new(),
            warnings: Vec::new(),
            validated_at: Utc::now(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationStatus {
    Pending,
    Valid,
    Invalid,
}

#[derive(Debug, Clone)]
pub enum ValidationError {
    MissingSuccessCriteria,
    NonExecutableAcceptanceTest,
    InvalidOwner,
    DeadlineInPast,
    CircularDependency { packet_id: WorkPacketId, blocker_id: WorkPacketId },
    BlockerNotFound { blocker_id: WorkPacketId },
    InvalidTestCoverage,
}

#[derive(Debug, Clone)]
pub enum ValidationWarning {
    VagueObjective { word: String, suggestion: String },
    LongDuration { hours: i64 },
    IrreversibleLongWork,
}
```

---

## Refusal Conditions and Receipts

### When to Refuse Work Packets

Work packets MUST be refused if:

1. **Structurally Invalid**
   - Missing required fields
   - Non-executable acceptance test
   - Invalid owner information

2. **Semantically Invalid**
   - Vague objectives without measurable criteria
   - Impossible constraints (deadline in past)
   - Circular dependencies

3. **Resource Constraints**
   - Owner at capacity (>2 in-progress packets)
   - Dependencies not satisfied
   - Required permissions not granted

4. **Strategic Misalignment**
   - Contradicts architectural decisions
   - Violates quality standards
   - Duplicates existing work

### Refusal Receipt

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalReceipt {
    pub packet_id: WorkPacketId,
    pub refused_at: DateTime<Utc>,
    pub refused_by: String,
    pub reason: RefusalReason,
    pub suggestions: Vec<String>,
    pub signature: String, // SHA-256 hash for audit trail
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RefusalReason {
    ValidationFailed { errors: Vec<ValidationError> },
    DependenciesNotSatisfied { missing: Vec<String> },
    OwnerAtCapacity { current_load: usize, max_load: usize },
    StrategicMismatch { explanation: String },
    InsufficientInformation { missing_fields: Vec<String> },
}

impl RefusalReceipt {
    pub fn new(
        packet: &WorkPacket,
        reason: RefusalReason,
        refused_by: String,
    ) -> Self {
        let receipt = Self {
            packet_id: packet.id.clone(),
            refused_at: Utc::now(),
            refused_by,
            reason,
            suggestions: Vec::new(),
            signature: String::new(),
        };

        // Generate signature (SHA-256 hash of receipt data)
        let mut receipt_with_sig = receipt.clone();
        receipt_with_sig.signature = Self::generate_signature(&receipt);
        receipt_with_sig
    }

    fn generate_signature(receipt: &RefusalReceipt) -> String {
        use sha2::{Sha256, Digest};

        let mut hasher = Sha256::new();
        hasher.update(receipt.packet_id.0.as_bytes());
        hasher.update(receipt.refused_at.to_rfc3339().as_bytes());
        hasher.update(receipt.refused_by.as_bytes());

        let result = hasher.finalize();
        format!("{:x}", result)
    }

    pub fn add_suggestion(&mut self, suggestion: String) {
        self.suggestions.push(suggestion);
    }
}
```

### Example Refusal

```rust
// Refusing a malformed packet
let packet = WorkPacket {
    id: WorkPacketId::new("WP", 123),
    objective: Objective {
        description: "Make the API better".to_string(),
        success_criteria: vec![], // MISSING
    },
    // ... rest of fields
};

let validation_report = WorkPacketValidator::validate(&packet)?;

if validation_report.status == ValidationStatus::Invalid {
    let mut refusal = RefusalReceipt::new(
        &packet,
        RefusalReason::ValidationFailed {
            errors: validation_report.errors.clone(),
        },
        "@system".to_string(),
    );

    refusal.add_suggestion(
        "Add specific success criteria: e.g., 'p95 latency < 200ms'".to_string()
    );
    refusal.add_suggestion(
        "Replace vague 'better' with measurable objective".to_string()
    );

    // Persist refusal receipt for audit trail
    save_refusal_receipt(&refusal)?;
}
```

---

## Converting Informal to Typed

### Transformation Process

```
INFORMAL REQUEST → ASK QUESTIONS → TYPED WORK PACKET
```

### Example Transformation

#### Before: Informal Request

```
"Hey, can you optimize the database queries?
They're kind of slow and users are complaining."
```

#### Questioning Process

**Agent Response**:
```
I need to convert this to a typed work packet. Let me ask:

1. **Objective Clarity**:
   Q: What specific metric defines "slow"?
   Q: What is the target performance?

2. **Constraints**:
   Q: When does this need to be done?
   Q: Can we change the database schema?
   Q: What's the maximum acceptable complexity?

3. **Acceptance Test**:
   Q: How will we verify it's no longer "slow"?
   Q: What query specifically needs optimization?

4. **Reversibility**:
   Q: If this causes issues, how do we rollback?

5. **Dependencies**:
   Q: Do we have query profiling tools available?
   Q: Can we run load tests in staging?

6. **Owner**:
   Q: Who is accountable for this completion?
```

#### After: Typed Work Packet

```rust
WorkPacket {
    id: WorkPacketId::new("WP", 124),

    objective: Objective {
        description: "Reduce user dashboard query p95 latency to <500ms".to_string(),
        success_criteria: vec![
            SuccessCriterion {
                metric: "dashboard_query_p95_latency_ms".to_string(),
                operator: ComparisonOperator::LessThan,
                threshold: 500.0,
                unit: "milliseconds".to_string(),
            },
            SuccessCriterion {
                metric: "dashboard_query_p99_latency_ms".to_string(),
                operator: ComparisonOperator::LessThan,
                threshold: 1000.0,
                unit: "milliseconds".to_string(),
            },
        ],
    },

    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::days(7)),
        max_duration: Duration::hours(16),
        max_budget_usd: None,
        breaking_changes_allowed: false,
        min_test_coverage_percent: 80,
        max_new_dependencies: 0,
        additional: vec![
            "No database schema changes".to_string(),
            "Must work with PostgreSQL 14+".to_string(),
        ],
    },

    acceptance_test: AcceptanceTest {
        test_type: TestType::BenchmarkTest {
            threshold: 500.0,
            metric: "p95_latency_ms".to_string(),
        },
        command: "cargo bench --bench dashboard_queries".to_string(),
        expected_output: "dashboard_load p95: <500ms".to_string(),
    },

    reversibility: ReversibilityPlan {
        strategy: ReversibilityStrategy::FeatureFlag {
            flag_name: "optimized_dashboard_queries".to_string(),
        },
        estimated_rollback_time: Duration::minutes(5),
        rollback_command: "toggle_feature optimized_dashboard_queries=false".to_string(),
        verification_test: "cargo test test_dashboard_queries".to_string(),
    },

    dependencies: Dependencies {
        required_services: vec![
            ServiceDependency {
                name: "PostgreSQL".to_string(),
                endpoint: "postgres://staging.db".to_string(),
                required_version: Some("14.0".to_string()),
            },
        ],
        blocked_by: vec![
            WorkPacketId::new("WP", 115), // Query profiling tools setup
        ],
        required_permissions: vec![
            "staging_database_read".to_string(),
        ],
    },

    owner: Owner {
        github_handle: "@alice".to_string(),
        email: "alice@example.com".to_string(),
        timezone: "America/New_York".to_string(),
    },

    status: WorkPacketStatus::Proposed,
    created_at: Utc::now(),
    updated_at: Utc::now(),
}
```

---

## Well-Formed vs Malformed Examples

### Example 1: API Performance Optimization

#### ❌ Malformed

```rust
WorkPacket {
    id: WorkPacketId::new("WP", 200),
    objective: Objective {
        description: "Make the API faster".to_string(),
        success_criteria: vec![], // MISSING
    },
    constraints: Constraints {
        deadline: None, // VAGUE
        max_duration: Duration::hours(999), // UNBOUNDED
        breaking_changes_allowed: true, // RISKY
        min_test_coverage_percent: 0, // NO QUALITY GATE
        ..Default::default()
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::ManualTest {
            checklist: vec!["Check if it's faster".to_string()], // NOT EXECUTABLE
        },
        command: "# TODO".to_string(), // NOT DEFINED
        expected_output: "Looks good".to_string(), // SUBJECTIVE
    },
    reversibility: ReversibilityPlan {
        strategy: ReversibilityStrategy::Irreversible {
            justification: "Can't undo performance work".to_string(), // POOR PLANNING
        },
        ..Default::default()
    },
    dependencies: Dependencies::default(), // NO DEPENDENCIES (SUSPICIOUS)
    owner: Owner {
        github_handle: "".to_string(), // MISSING
        email: "".to_string(), // MISSING
        timezone: "".to_string(),
    },
    status: WorkPacketStatus::Proposed,
    created_at: Utc::now(),
    updated_at: Utc::now(),
}
```

**Validation Errors**:
- ❌ No success criteria
- ❌ Vague objective ("faster")
- ❌ Non-executable acceptance test
- ❌ No reversibility plan
- ❌ Missing owner
- ❌ No constraints

**Refusal Reason**: `ValidationFailed` with 6+ errors

---

#### ✅ Well-Formed

```rust
WorkPacket {
    id: WorkPacketId::new("WP", 201),
    objective: Objective {
        description: "Reduce /api/users endpoint p95 latency to <100ms".to_string(),
        success_criteria: vec![
            SuccessCriterion {
                metric: "users_endpoint_p95_latency_ms".to_string(),
                operator: ComparisonOperator::LessThan,
                threshold: 100.0,
                unit: "milliseconds".to_string(),
            },
            SuccessCriterion {
                metric: "users_endpoint_error_rate_percent".to_string(),
                operator: ComparisonOperator::LessThan,
                threshold: 0.1,
                unit: "percent".to_string(),
            },
        ],
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::days(5)),
        max_duration: Duration::hours(12),
        max_budget_usd: Some(500.0),
        breaking_changes_allowed: false,
        min_test_coverage_percent: 85,
        max_new_dependencies: 1,
        additional: vec![
            "Must maintain backward compatibility".to_string(),
            "Cannot add database indices (requires DBA approval)".to_string(),
        ],
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::BenchmarkTest {
            threshold: 100.0,
            metric: "p95_latency_ms".to_string(),
        },
        command: "k6 run load-tests/users-endpoint.js".to_string(),
        expected_output: "http_req_duration{p(95)}<100".to_string(),
    },
    reversibility: ReversibilityPlan {
        strategy: ReversibilityStrategy::FeatureFlag {
            flag_name: "users_api_v2_optimized".to_string(),
        },
        estimated_rollback_time: Duration::minutes(2),
        rollback_command: "launchdarkly toggle users_api_v2_optimized=false".to_string(),
        verification_test: "cargo test integration::test_users_endpoint".to_string(),
    },
    dependencies: Dependencies {
        required_services: vec![
            ServiceDependency {
                name: "Redis Cache".to_string(),
                endpoint: "redis://cache.prod".to_string(),
                required_version: Some("7.0".to_string()),
            },
        ],
        blocked_by: vec![],
        required_permissions: vec![
            "production_read_access".to_string(),
            "launchdarkly_toggle".to_string(),
        ],
    },
    owner: Owner {
        github_handle: "@bob".to_string(),
        email: "bob@example.com".to_string(),
        timezone: "UTC".to_string(),
    },
    status: WorkPacketStatus::Validated,
    created_at: Utc::now(),
    updated_at: Utc::now(),
}
```

**Validation Result**: ✅ Valid

**Why It's Well-Formed**:
- ✅ Specific, measurable objective
- ✅ Executable acceptance test (k6 load test)
- ✅ Clear reversibility plan (feature flag)
- ✅ Realistic constraints
- ✅ Defined dependencies
- ✅ Single owner with accountability

---

### Example 2: Add User Phone Number Field

#### ❌ Malformed

```rust
WorkPacket {
    id: WorkPacketId::new("WP", 300),
    objective: Objective {
        description: "Add phone number to users".to_string(),
        success_criteria: vec![
            SuccessCriterion {
                metric: "has_phone_field".to_string(),
                operator: ComparisonOperator::Equals,
                threshold: 1.0,
                unit: "boolean".to_string(), // WRONG UNIT
            },
        ],
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::hours(2)), // UNREALISTIC
        max_duration: Duration::hours(1), // UNDERESTIMATED
        breaking_changes_allowed: true, // RISKY
        min_test_coverage_percent: 50, // TOO LOW
        ..Default::default()
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::ManualTest {
            checklist: vec!["Field exists in database".to_string()],
        },
        command: "# Manual check".to_string(), // NOT AUTOMATED
        expected_output: "Phone number column exists".to_string(),
    },
    reversibility: ReversibilityPlan {
        strategy: ReversibilityStrategy::DatabaseMigration {
            down_migration_path: "migrations/down.sql".to_string(), // DOESN'T EXIST YET
        },
        estimated_rollback_time: Duration::minutes(30),
        rollback_command: "diesel migration revert".to_string(),
        verification_test: "".to_string(), // MISSING
    },
    dependencies: Dependencies::default(),
    owner: Owner {
        github_handle: "@alice".to_string(),
        email: "alice@example.com".to_string(),
        timezone: "America/New_York".to_string(),
    },
    status: WorkPacketStatus::Proposed,
    created_at: Utc::now(),
    updated_at: Utc::now(),
}
```

**Validation Warnings**:
- ⚠️ Deadline too soon (2 hours for schema change)
- ⚠️ Breaking changes allowed (risky)
- ⚠️ Low test coverage (50%)
- ⚠️ Manual acceptance test (should be automated)

---

#### ✅ Well-Formed

```rust
WorkPacket {
    id: WorkPacketId::new("WP", 301),
    objective: Objective {
        description: "Add optional phone_number field to User model with E.164 validation".to_string(),
        success_criteria: vec![
            SuccessCriterion {
                metric: "phone_number_field_in_schema".to_string(),
                operator: ComparisonOperator::Equals,
                threshold: 1.0,
                unit: "exists".to_string(),
            },
            SuccessCriterion {
                metric: "phone_validation_coverage".to_string(),
                operator: ComparisonOperator::GreaterThanOrEqual,
                threshold: 95.0,
                unit: "percent".to_string(),
            },
        ],
    },
    constraints: Constraints {
        deadline: Some(Utc::now() + Duration::days(3)),
        max_duration: Duration::hours(8),
        max_budget_usd: None,
        breaking_changes_allowed: false,
        min_test_coverage_percent: 80,
        max_new_dependencies: 1, // phonenumber crate
        additional: vec![
            "Field must be optional (nullable)".to_string(),
            "Must support E.164 format validation".to_string(),
            "Existing users default to NULL".to_string(),
        ],
    },
    acceptance_test: AcceptanceTest {
        test_type: TestType::IntegrationTest {
            test_path: "tests/integration/user_phone_number_test.rs".to_string(),
        },
        command: "cargo test test_user_phone_number_field".to_string(),
        expected_output: "test test_user_phone_number_field ... ok".to_string(),
    },
    reversibility: ReversibilityPlan {
        strategy: ReversibilityStrategy::DatabaseMigration {
            down_migration_path: "migrations/2026-02-09-drop-phone-number/down.sql".to_string(),
        },
        estimated_rollback_time: Duration::minutes(10),
        rollback_command: "diesel migration revert".to_string(),
        verification_test: "cargo test --all".to_string(),
    },
    dependencies: Dependencies {
        required_services: vec![
            ServiceDependency {
                name: "PostgreSQL".to_string(),
                endpoint: "postgres://db.staging".to_string(),
                required_version: Some("14.0".to_string()),
            },
        ],
        blocked_by: vec![],
        required_permissions: vec![
            "database_migration_execute".to_string(),
        ],
    },
    owner: Owner {
        github_handle: "@alice".to_string(),
        email: "alice@example.com".to_string(),
        timezone: "America/New_York".to_string(),
    },
    status: WorkPacketStatus::Validated,
    created_at: Utc::now(),
    updated_at: Utc::now(),
}
```

**Validation Result**: ✅ Valid

**Why It's Well-Formed**:
- ✅ Clear objective with validation requirements
- ✅ Automated acceptance test
- ✅ Database migration rollback plan
- ✅ Realistic timeline (3 days)
- ✅ No breaking changes (optional field)
- ✅ Proper test coverage requirement (80%)

---

## Implementation Patterns

### Pattern 1: Work Packet Builder

```rust
pub struct WorkPacketBuilder {
    packet: WorkPacket,
}

impl WorkPacketBuilder {
    pub fn new(id: WorkPacketId) -> Self {
        Self {
            packet: WorkPacket {
                id,
                objective: Objective {
                    description: String::new(),
                    success_criteria: Vec::new(),
                },
                constraints: Constraints::default(),
                acceptance_test: AcceptanceTest::default(),
                reversibility: ReversibilityPlan::default(),
                dependencies: Dependencies::default(),
                owner: Owner::default(),
                status: WorkPacketStatus::Proposed,
                created_at: Utc::now(),
                updated_at: Utc::now(),
            },
        }
    }

    pub fn objective(mut self, description: String) -> Self {
        self.packet.objective.description = description;
        self
    }

    pub fn add_success_criterion(
        mut self,
        metric: String,
        operator: ComparisonOperator,
        threshold: f64,
        unit: String,
    ) -> Self {
        self.packet.objective.success_criteria.push(SuccessCriterion {
            metric,
            operator,
            threshold,
            unit,
        });
        self
    }

    pub fn deadline(mut self, deadline: DateTime<Utc>) -> Self {
        self.packet.constraints.deadline = Some(deadline);
        self
    }

    pub fn acceptance_test(mut self, test: AcceptanceTest) -> Self {
        self.packet.acceptance_test = test;
        self
    }

    pub fn reversibility(mut self, plan: ReversibilityPlan) -> Self {
        self.packet.reversibility = plan;
        self
    }

    pub fn owner(mut self, owner: Owner) -> Self {
        self.packet.owner = owner;
        self
    }

    pub fn build(self) -> Result<WorkPacket> {
        // Validate before returning
        let report = WorkPacketValidator::validate(&self.packet)?;

        if report.status == ValidationStatus::Valid {
            Ok(self.packet)
        } else {
            bail!("Work packet validation failed: {:?}", report.errors);
        }
    }
}
```

**Usage**:
```rust
let packet = WorkPacketBuilder::new(WorkPacketId::new("WP", 400))
    .objective("Reduce API latency to <200ms".to_string())
    .add_success_criterion(
        "api_p95_latency_ms".to_string(),
        ComparisonOperator::LessThan,
        200.0,
        "milliseconds".to_string(),
    )
    .deadline(Utc::now() + Duration::days(7))
    .acceptance_test(AcceptanceTest {
        test_type: TestType::BenchmarkTest {
            threshold: 200.0,
            metric: "p95_latency".to_string(),
        },
        command: "cargo bench".to_string(),
        expected_output: "p95 < 200ms".to_string(),
    })
    .reversibility(ReversibilityPlan {
        strategy: ReversibilityStrategy::FeatureFlag {
            flag_name: "api_optimization_v3".to_string(),
        },
        estimated_rollback_time: Duration::minutes(5),
        rollback_command: "toggle api_optimization_v3=false".to_string(),
        verification_test: "cargo test".to_string(),
    })
    .owner(Owner {
        github_handle: "@charlie".to_string(),
        email: "charlie@example.com".to_string(),
        timezone: "UTC".to_string(),
    })
    .build()?;
```

---

### Pattern 2: Work Packet State Machine

```rust
pub struct WorkPacketStateMachine;

impl WorkPacketStateMachine {
    pub fn transition(
        packet: &mut WorkPacket,
        event: WorkPacketEvent,
    ) -> Result<TransitionReceipt> {
        let old_status = packet.status.clone();

        match (&packet.status, &event) {
            (WorkPacketStatus::Proposed, WorkPacketEvent::Validate) => {
                let report = WorkPacketValidator::validate(packet)?;
                if report.status == ValidationStatus::Valid {
                    packet.status = WorkPacketStatus::Validated;
                } else {
                    packet.status = WorkPacketStatus::Refused {
                        reason: format!("Validation failed: {:?}", report.errors),
                    };
                }
            }

            (WorkPacketStatus::Validated, WorkPacketEvent::Accept) => {
                packet.status = WorkPacketStatus::Accepted;
            }

            (WorkPacketStatus::Accepted, WorkPacketEvent::Start) => {
                packet.status = WorkPacketStatus::InProgress;
            }

            (WorkPacketStatus::InProgress, WorkPacketEvent::Complete) => {
                // Verify acceptance test passed
                if Self::acceptance_test_passes(&packet.acceptance_test)? {
                    packet.status = WorkPacketStatus::Completed;
                } else {
                    bail!("Acceptance test failed, cannot mark complete");
                }
            }

            (WorkPacketStatus::InProgress, WorkPacketEvent::Block { reason }) => {
                packet.status = WorkPacketStatus::Blocked {
                    reason: reason.clone(),
                };
            }

            (WorkPacketStatus::Completed, WorkPacketEvent::Revert { reason }) => {
                packet.status = WorkPacketStatus::Reverted {
                    reason: reason.clone(),
                };
            }

            _ => {
                bail!("Invalid state transition: {:?} -> {:?}", old_status, event);
            }
        }

        packet.updated_at = Utc::now();

        Ok(TransitionReceipt {
            packet_id: packet.id.clone(),
            old_status,
            new_status: packet.status.clone(),
            event,
            transitioned_at: packet.updated_at,
        })
    }

    fn acceptance_test_passes(_test: &AcceptanceTest) -> Result<bool> {
        // Implementation: execute the acceptance test
        // Simplified for example
        Ok(true)
    }
}

#[derive(Debug, Clone)]
pub enum WorkPacketEvent {
    Validate,
    Accept,
    Start,
    Complete,
    Block { reason: String },
    Revert { reason: String },
}

#[derive(Debug, Clone)]
pub struct TransitionReceipt {
    pub packet_id: WorkPacketId,
    pub old_status: WorkPacketStatus,
    pub new_status: WorkPacketStatus,
    pub event: WorkPacketEvent,
    pub transitioned_at: DateTime<Utc>,
}
```

---

## Integration with ggen Workflow

### Work Packets as RDF

```turtle
@prefix wp: <http://ggen.dev/work-packet#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:WP-0124 a wp:WorkPacket ;
    wp:id "WP-0124" ;
    wp:objective [
        a wp:Objective ;
        wp:description "Reduce API p95 latency to <200ms" ;
        wp:successCriteria [
            wp:metric "api_p95_latency_ms" ;
            wp:operator wp:LessThan ;
            wp:threshold 200.0 ;
            wp:unit "milliseconds"
        ]
    ] ;
    wp:constraint [
        wp:deadline "2026-02-16T00:00:00Z"^^xsd:dateTime ;
        wp:maxDuration "PT16H"^^xsd:duration ;
        wp:breakingChangesAllowed false ;
        wp:minTestCoverage 80
    ] ;
    wp:acceptanceTest [
        wp:testType wp:BenchmarkTest ;
        wp:command "cargo bench" ;
        wp:expectedOutput "p95 < 200ms"
    ] ;
    wp:reversibility [
        wp:strategy wp:FeatureFlag ;
        wp:flagName "api_optimization_v2" ;
        wp:rollbackCommand "toggle api_optimization_v2=false" ;
        wp:estimatedRollbackTime "PT5M"^^xsd:duration
    ] ;
    wp:owner [
        wp:githubHandle "@alice" ;
        wp:email "alice@example.com" ;
        wp:timezone "America/New_York"
    ] ;
    wp:status wp:Validated ;
    wp:createdAt "2026-02-09T10:00:00Z"^^xsd:dateTime ;
    wp:updatedAt "2026-02-09T10:30:00Z"^^xsd:dateTime .
```

### Querying Work Packets with SPARQL

```sparql
PREFIX wp: <http://ggen.dev/work-packet#>

# Find all work packets owned by @alice
SELECT ?packet ?objective ?status
WHERE {
    ?packet a wp:WorkPacket ;
            wp:owner [ wp:githubHandle "@alice" ] ;
            wp:objective [ wp:description ?objective ] ;
            wp:status ?status .
}

# Find work packets blocked by dependencies
SELECT ?packet ?blocker
WHERE {
    ?packet a wp:WorkPacket ;
            wp:status wp:Proposed ;
            wp:dependency [ wp:blockedBy ?blocker ] .

    ?blocker wp:status ?blockerStatus .
    FILTER(?blockerStatus != wp:Completed)
}

# Find work packets with reversibility risks
SELECT ?packet ?strategy
WHERE {
    ?packet a wp:WorkPacket ;
            wp:reversibility [ wp:strategy ?strategy ] .
    FILTER(?strategy = wp:Irreversible)
}
```

### Generation from Work Packets

```rust
// Generate code scaffolding from work packet
pub fn generate_from_work_packet(packet: &WorkPacket) -> Result<GeneratedArtifacts> {
    let mut artifacts = GeneratedArtifacts::new();

    // Generate test file from acceptance test
    let test_code = generate_acceptance_test(&packet.acceptance_test)?;
    artifacts.add_file("tests/acceptance.rs".into(), test_code);

    // Generate rollback script from reversibility plan
    let rollback_script = generate_rollback_script(&packet.reversibility)?;
    artifacts.add_file("scripts/rollback.sh".into(), rollback_script);

    // Generate implementation template
    let impl_template = generate_implementation_template(&packet.objective)?;
    artifacts.add_file("src/implementation.rs".into(), impl_template);

    Ok(artifacts)
}
```

---

## Conclusion

### The Packet Discipline Mindset

**Before Packet Discipline**:
- Vague requests accepted
- No clear success criteria
- No rollback plans
- Wasted effort common

**After Packet Discipline**:
- Only well-formed work accepted
- Measurable objectives required
- Reversibility mandatory
- Effort tracked and optimized

### The Type System Analogy

```rust
// Without types (runtime errors)
fn process_data(data: Any) -> Any {
    // Hope for the best
}

// With types (compile-time safety)
fn process_data(data: ValidatedInput) -> Result<Output, Error> {
    // Guarantees or explicit errors
}
```

**Packet discipline is the type system for work itself.**

---

## Further Reading

- [Work Object State Machines](/.specify/WORK-OBJECT-MODEL-STATE-MACHINES.md) - Related state machine patterns
- [Mental Model Shift](/docs/paradigm-shift/fundamentals/mental-model-shift.md) - Understanding paradigm shifts
- [Type-First Design](/.claude/rules/rust/elite-mindset.md) - Rust type system best practices
- [Chicago TDD](/.claude/rules/rust/testing.md) - Testing philosophy alignment

---

**Document Status**: Production-Ready
**Last Updated**: 2026-02-09
**Reading Time**: 28 minutes
**Next**: [Five-Stage Pipeline](/docs/paradigm-shift/fundamentals/five-stage-pipeline.md)
**Feedback**: [Open an issue](https://github.com/seanchatmangpt/ggen/issues) or [discuss](https://github.com/seanchatmangpt/ggen/discussions)
