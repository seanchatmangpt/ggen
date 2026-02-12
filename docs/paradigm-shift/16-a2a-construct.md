# A2A-CONSTRUCT: Manufacturing Protocol for Agent Coordination

**Version**: 1.0.0
**Last Updated**: 2026-02-09
**Status**: Canonical Reference
**Reading Time**: 35-40 minutes | **Difficulty**: Advanced | **Prerequisites**: Understanding of A2A protocol, TPS principles, state machines

---

## Table of Contents

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Executive Summary](#executive-summary)
- [The Fundamental Shift: Chat to Construction](#the-fundamental-shift-chat-to-construction)
  - [Before: Chat Coordination](#before-chat-coordination)
  - [After: A2A-CONSTRUCT](#after-a2a-construct)
  - [The Event Horizon](#the-event-horizon)
- [Core Principles](#core-principles)
  - [Principle 1: Tasks as Kanban Cards](#principle-1-tasks-as-kanban-cards)
    - [Task Structure](#task-structure)
    - [Kanban Board View](#kanban-board-view)
  - [Principle 2: Artifacts Over Prose](#principle-2-artifacts-over-prose)
    - [Before: Prose-Based](#before-prose-based)
    - [After: Artifact-Based](#after-artifact-based)
    - [Artifact Types](#artifact-types)
  - [Principle 3: Events as Andon Signals](#principle-3-events-as-andon-signals)
    - [Event Stream](#event-stream)
    - [Event Categories](#event-categories)
  - [Principle 4: Pull-Only Work Distribution](#principle-4-pull-only-work-distribution)
    - [Push vs Pull](#push-vs-pull)
  - [Principle 5: Terminal State Machines](#principle-5-terminal-state-machines)
    - [State Machine Diagram](#state-machine-diagram)
    - [Terminal State Properties](#terminal-state-properties)
    - [Enforcement](#enforcement)
- [Protocol Specifications](#protocol-specifications)
  - [Task State Machine](#task-state-machine)
  - [Event Stream Protocol](#event-stream-protocol)
  - [Artifact Schema](#artifact-schema)
  - [WIP Limits and Backpressure](#wip-limits-and-backpressure)
- [Post-Human Iteration: Token Flow Through Stations](#post-human-iteration-token-flow-through-stations)
  - [The Vision: Humans Removed from Loop](#the-vision-humans-removed-from-loop)
  - [Token Flow Architecture](#token-flow-architecture)
  - [Station Types](#station-types)
    - [Station 1: RDF Parser](#station-1-rdf-parser)
    - [Station 2: Code Generator](#station-2-code-generator)
    - [Station 3: Validator](#station-3-validator)
    - [Station 4: Test Runner](#station-4-test-runner)
    - [Station 5: Packager](#station-5-packager)
- [a2a-rs: Transport and State Machine Substrate](#a2a-rs-transport-and-state-machine-substrate)
  - [Core Responsibilities](#core-responsibilities)
  - [Integration Points](#integration-points)
  - [Implementation Patterns](#implementation-patterns)
    - [Pattern 1: Task State Machine](#pattern-1-task-state-machine)
    - [Pattern 2: Event Subscription](#pattern-2-event-subscription)
    - [Pattern 3: Artifact Storage](#pattern-3-artifact-storage)
- [TPS Profile: Manufacturing Discipline at Protocol Level](#tps-profile-manufacturing-discipline-at-protocol-level)
  - [Pull-Only (JIT)](#pull-only-jit)
  - [WIP Limits (Kanban)](#wip-limits-kanban)
  - [Terminality (Jidoka)](#terminality-jidoka)
  - [Artifact-First (Heijunka)](#artifact-first-heijunka)
- [Concrete Examples](#concrete-examples)
  - [Example 1: Code Generation Pipeline](#example-1-code-generation-pipeline)
  - [Example 2: Multi-Language Code Generation](#example-2-multi-language-code-generation)
  - [Example 3: Documentation Generation](#example-3-documentation-generation)
- [Anti-Patterns and Common Mistakes](#anti-patterns-and-common-mistakes)
  - [Anti-Pattern 1: Chat-Based Coordination](#anti-pattern-1-chat-based-coordination)
  - [Anti-Pattern 2: Unbounded Queues](#anti-pattern-2-unbounded-queues)
  - [Anti-Pattern 3: Mutable Terminal States](#anti-pattern-3-mutable-terminal-states)
  - [Anti-Pattern 4: Push-Based Work Distribution](#anti-pattern-4-push-based-work-distribution)
  - [Anti-Pattern 5: No Validation Gates](#anti-pattern-5-no-validation-gates)
- [Implementation Guide](#implementation-guide)
  - [Phase 1: Foundation](#phase-1-foundation)
  - [Phase 2: Agent Integration](#phase-2-agent-integration)
  - [Phase 3: Post-Human Scaling](#phase-3-post-human-scaling)
- [Metrics and Validation](#metrics-and-validation)
  - [TPS Metrics](#tps-metrics)
  - [Validation Metrics](#validation-metrics)
  - [Reproducibility Metrics](#reproducibility-metrics)
- [Further Reading](#further-reading)
- [Appendix: Protocol Message Formats](#appendix-protocol-message-formats)
  - [Task Creation (JSON-RPC)](#task-creation-json-rpc)
  - [Task Pull (HTTP GET)](#task-pull-http-get)
  - [Task Completion (HTTP POST)](#task-completion-http-post)
  - [Event Stream (WebSocket)](#event-stream-websocket)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

---

## Executive Summary

**A2A-CONSTRUCT** is a manufacturing-grade protocol for agent-to-agent coordination that eliminates chat-based workflows in favor of deterministic, artifact-driven construction pipelines.

**Core Thesis**: Agent coordination should work like a Toyota production line, not a Slack channel.

**Key Insight**: When agents communicate through artifacts (not prose), workflows become reproducible, verifiable, and ultimately post-human.

**Result**: Token flow through stations replaces human supervision. Work items pull through the system like Kanban cards. Terminal states ensure no work is lost or repeated.

---

## The Fundamental Shift: Chat to Construction

### Before: Chat Coordination

```
Agent 1: "I need code generated"
Agent 2: "Sure, what language?"
Agent 1: "Python and TypeScript"
Agent 2: "OK, generating..."
Agent 2: "Done, but I had to make some assumptions about error handling"
Agent 1: "Can you fix the TypeScript version?"
Agent 2: "Which part?"
Agent 1: "The async handling is wrong"
Agent 2: "Fixed. Here's version 2..."
```

**Problems**:
- ❌ Non-deterministic (conversation depends on order, timing, interpretation)
- ❌ No clear terminal state (when is it "done"?)
- ❌ No audit trail (what changed between v1 and v2?)
- ❌ Cannot replay (conversation is ephemeral)
- ❌ Requires human supervision (someone must read and interpret prose)

---

### After: A2A-CONSTRUCT

```
Task [TASK-001]:
  State: PENDING
  Spec: ontology://user-model.ttl#User
  Targets: [python, typescript]
  Acceptance: [compiles, passes-tests, 80%+ coverage]

Agent 1 (Kanban Board):
  → Pull task TASK-001 (state: PENDING → IN_PROGRESS)
  → Process spec → Generate artifacts
  → Emit artifacts: [user.py, user.ts, user_test.py, user.test.ts]
  → Run validation suite
  → Emit event: TASK-001.VALIDATION_PASSED
  → Update task: IN_PROGRESS → COMPLETED
  → Emit receipt: [spec_hash, artifact_hashes, validation_results]

Agent 2 (Monitor):
  ← Receive event: TASK-001.COMPLETED
  ← Verify receipt: spec_hash matches, all artifacts present
  ← State machine: COMPLETED (terminal state, no further work)
```

**Benefits**:
- ✅ Deterministic (same spec → same artifacts)
- ✅ Terminal state machine (COMPLETED = provably done)
- ✅ Cryptographic audit trail (every artifact hashed)
- ✅ Perfect replay (spec + generator version → artifacts)
- ✅ Post-human (no conversation needed, just pull → process → complete)

---

### The Event Horizon

**You've crossed the event horizon when:**

> "I can remove all humans from the workflow and the system keeps producing artifacts with zero degradation."

**Before**: Human must interpret, negotiate, clarify
**After**: Spec is complete, generator is deterministic, validation is automatic

**Common Reactions**:
- ✅ "This feels like manufacturing software" (correct intuition)
- ✅ "I don't need to supervise agents anymore" (that's the goal)
- ❌ "But what if the spec is wrong?" → [Fix the spec, re-run the generator]
- ❌ "This is too rigid" → [Rigidity is the feature, not a bug]

---

## Core Principles

### Principle 1: Tasks as Kanban Cards

**Manufacturing Original**: Physical cards on a board showing work status

**A2A-CONSTRUCT Translation**: Every task is a typed work object with explicit state

#### Task Structure

```rust
pub struct Task {
    pub id: TaskId,                     // Unique identifier
    pub spec: SpecificationRef,         // What to build (RDF ontology ref)
    pub targets: Vec<Target>,           // What outputs (languages, formats)
    pub acceptance: Vec<Criterion>,     // How to verify success
    pub status: TaskStatus,             // Current state (state machine)
    pub assigned_to: Option<AgentId>,   // Who is working on it
    pub artifacts: Vec<ArtifactRef>,    // What was produced
    pub events: Vec<Event>,             // Audit trail
    pub created_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
}

pub enum TaskStatus {
    Pending,      // Created, awaiting assignment
    Assigned,     // Agent accepted, not started
    InProgress,   // Agent actively processing
    Completed,    // Terminal: success, artifacts emitted
    Failed,       // Terminal: failure, error documented
    Cancelled,    // Terminal: work abandoned
}
```

#### Kanban Board View

```
┌─────────────────────────────────────────────────────────────┐
│                     A2A-CONSTRUCT KANBAN                    │
├─────────────┬─────────────┬─────────────┬──────────────────┤
│   PENDING   │  ASSIGNED   │ IN_PROGRESS │    COMPLETED     │
│   (WIP=0)   │   (WIP=2)   │   (WIP=5)   │   (Terminal)     │
├─────────────┼─────────────┼─────────────┼──────────────────┤
│ TASK-001    │ TASK-004    │ TASK-007    │ TASK-002 ✓       │
│ TASK-003    │ TASK-006    │ TASK-008    │ TASK-005 ✓       │
│ TASK-009    │             │ TASK-010    │ TASK-011 ✓       │
│ TASK-012    │             │ TASK-013    │                  │
│             │             │ TASK-014    │                  │
│ WIP Limit:  │ WIP Limit:  │ WIP Limit:  │ No Limit         │
│ Unlimited   │ 3 (80% full)│ 10 (50%)    │ (Terminal)       │
└─────────────┴─────────────┴─────────────┴──────────────────┘

Pull Rule: Agent can only pull from PENDING when IN_PROGRESS < 10
Backpressure: If IN_PROGRESS = 10, agent waits until slot opens
```

---

### Principle 2: Artifacts Over Prose

**Core Tenet**: Communication is through structured artifacts, not natural language.

#### Before: Prose-Based

```
Agent 1: "I generated the user model in Python"
Agent 2: "Can I see it?"
Agent 1: "Here's the code: [paste 200 lines]"
Agent 2: "The validation is wrong on line 54"
Agent 1: "Fixed. Here's the new version: [paste 200 lines again]"
```

**Problems**: No diff, no hash, no versioning, no validation

---

#### After: Artifact-Based

```json
{
  "task_id": "TASK-001",
  "artifacts": [
    {
      "artifact_id": "ART-001",
      "type": "source_code",
      "language": "python",
      "path": "crates/models/src/user.py",
      "content_hash": "sha256:7f3a9b2c...",
      "size_bytes": 4521,
      "validation": {
        "compiles": true,
        "tests_pass": true,
        "coverage_percent": 87.5,
        "lint_warnings": 0
      }
    },
    {
      "artifact_id": "ART-002",
      "type": "source_code",
      "language": "typescript",
      "path": "packages/models/src/user.ts",
      "content_hash": "sha256:4d2e1a9f...",
      "size_bytes": 3890,
      "validation": {
        "compiles": true,
        "tests_pass": true,
        "coverage_percent": 92.3,
        "lint_warnings": 0
      }
    }
  ],
  "receipt": {
    "spec_hash": "sha256:1a2b3c4d...",
    "generator_version": "ggen-0.2.0",
    "generated_at": "2026-02-09T10:30:00Z",
    "reproducible": true
  }
}
```

**Benefits**:
- ✅ Cryptographic proof (hashes)
- ✅ Validation results attached
- ✅ Reproducible (spec_hash + generator_version → same artifacts)
- ✅ No interpretation needed (structured data)

---

#### Artifact Types

| Type | Description | Validation | Examples |
|------|-------------|-----------|----------|
| **source_code** | Generated code in target language | Compiles, tests pass, coverage | user.py, user.ts, user.rs |
| **test_code** | Generated test suites | Tests pass, coverage measured | user_test.py, user.test.ts |
| **documentation** | Generated docs (Markdown, HTML) | Links valid, renders correctly | API.md, user-guide.html |
| **schema** | Database schemas, API specs | SHACL validation, format checks | schema.sql, openapi.yaml |
| **configuration** | Generated config files | Valid TOML/YAML, required fields | config.toml, k8s.yaml |
| **receipt** | Cryptographic audit trail | Hash verification, signature valid | receipt.json |

---

### Principle 3: Events as Andon Signals

**Manufacturing Original**: Red light/bell when problem detected

**A2A-CONSTRUCT Translation**: All state changes emit events for visibility

#### Event Stream

```
Timeline (Task TASK-001):

2026-02-09T10:00:00Z  EVENT: TASK.CREATED
  → task_id: TASK-001
  → spec: ontology://user-model.ttl#User
  → targets: [python, typescript]

2026-02-09T10:00:05Z  EVENT: TASK.ASSIGNED
  → task_id: TASK-001
  → assigned_to: agent-code-gen-001
  → status: PENDING → ASSIGNED

2026-02-09T10:00:10Z  EVENT: TASK.STARTED
  → task_id: TASK-001
  → status: ASSIGNED → IN_PROGRESS

2026-02-09T10:00:45Z  EVENT: ARTIFACT.GENERATED
  → task_id: TASK-001
  → artifact_id: ART-001
  → type: source_code (python)
  → hash: sha256:7f3a9b2c...

2026-02-09T10:00:46Z  EVENT: ARTIFACT.GENERATED
  → task_id: TASK-001
  → artifact_id: ART-002
  → type: source_code (typescript)
  → hash: sha256:4d2e1a9f...

2026-02-09T10:00:50Z  EVENT: VALIDATION.STARTED
  → task_id: TASK-001
  → validation_suite: [compile, test, coverage, lint]

2026-02-09T10:01:05Z  EVENT: VALIDATION.PASSED
  → task_id: TASK-001
  → all_checks: true
  → coverage: 89.2%

2026-02-09T10:01:06Z  EVENT: TASK.COMPLETED
  → task_id: TASK-001
  → status: IN_PROGRESS → COMPLETED
  → duration_seconds: 56
  → artifacts: [ART-001, ART-002]
```

#### Event Categories

| Category | Purpose | When Emitted | Subscribers |
|----------|---------|--------------|-------------|
| **Task Lifecycle** | State machine transitions | CREATED, ASSIGNED, STARTED, COMPLETED, FAILED | Dashboard, Monitoring, Audit |
| **Artifact Generation** | Work product created | ARTIFACT.GENERATED | Validation agents, Storage |
| **Validation** | Quality gates | VALIDATION.STARTED, PASSED, FAILED | Quality dashboard, Alerts |
| **Error** | Problems detected | ERROR, WARNING | Andon board, On-call alerts |
| **Receipt** | Audit trail | RECEIPT.ISSUED | Compliance, Verification |

---

### Principle 4: Pull-Only Work Distribution

**Manufacturing Original**: Workers pull work when ready (not pushed by manager)

**A2A-CONSTRUCT Translation**: Agents request tasks when capacity available

#### Push vs Pull

**❌ Push (Anti-Pattern)**:
```rust
// Manager pushes work to agents
fn assign_work(task: Task, agents: &Vec<Agent>) {
    // Find least busy agent
    let agent = agents.iter().min_by_key(|a| a.active_tasks.len())?;

    // Push task to agent (what if agent is about to be busy?)
    agent.assign(task);  // Race condition: agent might be full now
}
```

**✅ Pull (A2A-CONSTRUCT)**:
```rust
// Agent pulls work when ready
impl Agent {
    async fn work_loop(&mut self) {
        loop {
            // Check capacity
            if self.active_tasks.len() < self.max_concurrent {
                // Pull next task (only when capacity available)
                match self.kanban.pull_task().await {
                    Some(task) => {
                        self.active_tasks.push(task.id);
                        self.process_task(task).await;
                    }
                    None => {
                        // No work available, wait
                        tokio::time::sleep(Duration::from_secs(1)).await;
                    }
                }
            } else {
                // At capacity, wait for task completion
                tokio::time::sleep(Duration::from_millis(100)).await;
            }
        }
    }
}
```

**Benefits**:
- ✅ No race conditions (agent knows its own capacity)
- ✅ Natural backpressure (agent stops pulling when full)
- ✅ Self-balancing (faster agents pull more work)

---

### Principle 5: Terminal State Machines

**Core Tenet**: Tasks have explicit terminal states (COMPLETED, FAILED, CANCELLED). Once terminal, no further transitions.

#### State Machine Diagram

```
┌────────────────────────────────────────────────────────────┐
│                A2A-CONSTRUCT TASK STATE MACHINE            │
├────────────────────────────────────────────────────────────┤
│                                                            │
│   [PENDING] ──assign──> [ASSIGNED] ──start──> [IN_PROGRESS]
│                                                      │      │
│                                                   complete  │
│                                                      │      │
│                                                      v      │
│                                                [COMPLETED]  │
│                                               (TERMINAL)    │
│                                                             │
│   [IN_PROGRESS] ──error──> [FAILED]                        │
│                          (TERMINAL)                         │
│                                                             │
│   [PENDING/ASSIGNED] ──cancel──> [CANCELLED]               │
│                                 (TERMINAL)                  │
│                                                             │
│   Terminal States: Once reached, no further transitions    │
│   Immutability: Terminal tasks cannot be modified          │
│                                                             │
└────────────────────────────────────────────────────────────┘
```

#### Terminal State Properties

| State | Terminal? | Can Modify? | Can Retry? | Artifacts Present? |
|-------|-----------|-------------|------------|-------------------|
| PENDING | No | Yes | N/A | No |
| ASSIGNED | No | Yes | N/A | No |
| IN_PROGRESS | No | No (readonly) | N/A | Partial |
| **COMPLETED** | **Yes** | **No** | **No** | **Yes** |
| **FAILED** | **Yes** | **No** | **New task** | **No/Partial** |
| **CANCELLED** | **Yes** | **No** | **New task** | **No** |

#### Enforcement

```rust
impl Task {
    pub fn transition(&mut self, new_status: TaskStatus) -> Result<(), TaskError> {
        // Check if current state is terminal
        if self.status.is_terminal() {
            return Err(TaskError::TerminalStateReached {
                task_id: self.id.clone(),
                current_state: self.status.clone(),
                attempted_transition: new_status,
            });
        }

        // Validate transition is legal
        if !self.status.can_transition_to(&new_status) {
            return Err(TaskError::InvalidTransition {
                from: self.status.clone(),
                to: new_status,
            });
        }

        // Record event before transition
        self.events.push(Event::StatusChanged {
            from: self.status.clone(),
            to: new_status.clone(),
            timestamp: Utc::now(),
        });

        // Perform transition
        self.status = new_status;

        Ok(())
    }
}

impl TaskStatus {
    pub fn is_terminal(&self) -> bool {
        matches!(self, TaskStatus::Completed | TaskStatus::Failed | TaskStatus::Cancelled)
    }

    pub fn can_transition_to(&self, target: &TaskStatus) -> bool {
        match (self, target) {
            (TaskStatus::Pending, TaskStatus::Assigned) => true,
            (TaskStatus::Assigned, TaskStatus::InProgress) => true,
            (TaskStatus::InProgress, TaskStatus::Completed) => true,
            (TaskStatus::InProgress, TaskStatus::Failed) => true,
            (TaskStatus::Pending | TaskStatus::Assigned, TaskStatus::Cancelled) => true,
            _ => false,  // All other transitions invalid
        }
    }
}
```

---

## Protocol Specifications

### Task State Machine

**JSON-RPC Method**: `a2a.task.create`

```json
{
  "jsonrpc": "2.0",
  "method": "a2a.task.create",
  "params": {
    "spec": "ontology://user-model.ttl#User",
    "targets": ["python", "typescript", "rust"],
    "acceptance": {
      "compiles": true,
      "tests_pass": true,
      "min_coverage_percent": 80.0,
      "max_lint_warnings": 0
    },
    "priority": "normal",
    "timeout_seconds": 300
  },
  "id": 1
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "task_id": "TASK-001",
    "status": "PENDING",
    "created_at": "2026-02-09T10:00:00Z",
    "kanban_url": "ws://kanban.example.com/tasks/TASK-001"
  },
  "id": 1
}
```

---

### Event Stream Protocol

**WebSocket Subscription**: `ws://kanban.example.com/events`

```json
{
  "action": "subscribe",
  "filters": {
    "task_ids": ["TASK-001"],
    "event_types": ["TASK.*", "ARTIFACT.*", "VALIDATION.*"]
  }
}
```

**Event Stream**:
```json
{
  "event_type": "TASK.COMPLETED",
  "task_id": "TASK-001",
  "timestamp": "2026-02-09T10:01:06Z",
  "data": {
    "duration_seconds": 56,
    "artifacts": [
      {"id": "ART-001", "type": "source_code", "language": "python"},
      {"id": "ART-002", "type": "source_code", "language": "typescript"},
      {"id": "ART-003", "type": "source_code", "language": "rust"}
    ],
    "validation": {
      "all_passed": true,
      "coverage_percent": 89.2
    }
  }
}
```

---

### Artifact Schema

```rust
pub struct Artifact {
    pub id: ArtifactId,
    pub task_id: TaskId,
    pub artifact_type: ArtifactType,
    pub content: ArtifactContent,
    pub content_hash: Hash,
    pub size_bytes: u64,
    pub validation: ValidationResults,
    pub metadata: ArtifactMetadata,
    pub created_at: DateTime<Utc>,
}

pub enum ArtifactType {
    SourceCode { language: Language },
    TestCode { language: Language },
    Documentation { format: DocFormat },
    Schema { schema_type: SchemaType },
    Configuration { config_type: ConfigType },
    Receipt,
}

pub struct ValidationResults {
    pub compiles: bool,
    pub tests_pass: bool,
    pub coverage_percent: f64,
    pub lint_warnings: u32,
    pub validation_errors: Vec<ValidationError>,
}

pub struct ArtifactMetadata {
    pub generator: String,
    pub generator_version: String,
    pub spec_hash: Hash,
    pub reproducible: bool,
}
```

---

### WIP Limits and Backpressure

```rust
pub struct KanbanBoard {
    pub pending: Queue<Task>,           // Unlimited
    pub assigned: Queue<Task>,          // WIP limit: 10
    pub in_progress: Queue<Task>,       // WIP limit: 20
    pub completed: Vec<Task>,           // No limit (terminal)
    pub failed: Vec<Task>,              // No limit (terminal)
}

impl KanbanBoard {
    pub async fn pull_task(&mut self, agent_id: AgentId) -> Option<Task> {
        // Check WIP limit for IN_PROGRESS
        if self.in_progress.len() >= 20 {
            // Backpressure: at capacity
            return None;
        }

        // Check if agent already has assigned task
        if let Some(task) = self.assigned.iter()
            .find(|t| t.assigned_to == Some(agent_id)) {
            // Resume existing assignment
            let mut task = self.assigned.remove(task.id)?;
            task.transition(TaskStatus::InProgress)?;
            self.in_progress.push(task.clone());
            return Some(task);
        }

        // Pull new task from PENDING
        if let Some(mut task) = self.pending.pop() {
            task.assigned_to = Some(agent_id);
            task.transition(TaskStatus::Assigned)?;
            task.transition(TaskStatus::InProgress)?;
            self.in_progress.push(task.clone());
            return Some(task);
        }

        // No work available
        None
    }

    pub fn complete_task(&mut self, task_id: TaskId, artifacts: Vec<Artifact>)
        -> Result<(), TaskError> {
        let mut task = self.in_progress.remove(&task_id)?;

        task.artifacts = artifacts.iter().map(|a| a.id.clone()).collect();
        task.transition(TaskStatus::Completed)?;
        task.completed_at = Some(Utc::now());

        self.completed.push(task);

        Ok(())
    }
}
```

---

## Post-Human Iteration: Token Flow Through Stations

### The Vision: Humans Removed from Loop

**Current State (Human-Supervised)**:
```
Spec → Agent → Artifacts → Human Review → Deploy
              ↑                    │
              └─────Feedback───────┘
```

**Target State (Post-Human)**:
```
Spec → Station 1 (Parse) → Station 2 (Generate) → Station 3 (Validate)
  → Station 4 (Test) → Station 5 (Package) → Deploy

All stations connected by artifact flow (no human checkpoints)
Each station: Pull → Process → Emit → Next station pulls
```

**Enabling Factors**:
1. **Complete Specifications** (RDF ontologies encode all requirements)
2. **Deterministic Generators** (same spec → same artifacts)
3. **Automated Validation** (no subjective "code review")
4. **Terminal States** (clear definition of "done")
5. **Cryptographic Receipts** (audit trail without human signoff)

---

### Token Flow Architecture

```
┌─────────────────────────────────────────────────────────────┐
│              TOKEN FLOW THROUGH STATIONS                    │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  [Spec]                                                     │
│     │                                                       │
│     ▼                                                       │
│  ┌─────────────┐                                            │
│  │ Station 1:  │ ─────> [Artifact: Parsed AST]             │
│  │ RDF Parser  │                                            │
│  └─────────────┘                                            │
│     │                                                       │
│     ▼                                                       │
│  ┌─────────────┐                                            │
│  │ Station 2:  │ ─────> [Artifact: Code (Python)]          │
│  │ Code Gen    │ ─────> [Artifact: Code (TypeScript)]      │
│  └─────────────┘ ─────> [Artifact: Code (Rust)]            │
│     │                                                       │
│     ▼                                                       │
│  ┌─────────────┐                                            │
│  │ Station 3:  │ ─────> [Artifact: Test Results]           │
│  │ Validator   │                                            │
│  └─────────────┘                                            │
│     │                                                       │
│     ▼                                                       │
│  ┌─────────────┐                                            │
│  │ Station 4:  │ ─────> [Artifact: Coverage Report]        │
│  │ Test Runner │                                            │
│  └─────────────┘                                            │
│     │                                                       │
│     ▼                                                       │
│  ┌─────────────┐                                            │
│  │ Station 5:  │ ─────> [Artifact: Receipt]                │
│  │ Packager    │ ─────> [Task: COMPLETED]                  │
│  └─────────────┘                                            │
│                                                             │
│  Flow Properties:                                           │
│  - Each station pulls when ready (no push)                 │
│  - WIP limits prevent overload                             │
│  - Terminal states ensure completion                       │
│  - Artifacts flow like tokens on assembly line             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

### Station Types

#### Station 1: RDF Parser
```rust
impl Station for RdfParser {
    async fn process(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Pull spec artifact
        let spec = self.parse_rdf(input.content)?;

        // Emit parsed AST artifact
        Ok(vec![Artifact {
            artifact_type: ArtifactType::ParsedAst,
            content: serialize_ast(&spec),
            content_hash: hash(&spec),
            validation: ValidationResults::default(),
            ..Default::default()
        }])
    }
}
```

#### Station 2: Code Generator
```rust
impl Station for CodeGenerator {
    async fn process(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Pull parsed AST
        let ast = deserialize_ast(&input.content)?;

        // Generate code in multiple languages
        let mut artifacts = Vec::new();

        for lang in &self.target_languages {
            let code = self.generate(lang, &ast)?;
            artifacts.push(Artifact {
                artifact_type: ArtifactType::SourceCode { language: *lang },
                content: code,
                content_hash: hash(&code),
                validation: ValidationResults::default(),
                ..Default::default()
            });
        }

        Ok(artifacts)
    }
}
```

#### Station 3: Validator
```rust
impl Station for Validator {
    async fn process(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Pull code artifact
        let code = &input.content;
        let language = input.language()?;

        // Run compiler
        let compiles = self.compile(language, code)?;

        // Run linter
        let lint_warnings = self.lint(language, code)?;

        // Emit validation artifact
        Ok(vec![Artifact {
            artifact_type: ArtifactType::ValidationReport,
            content: serialize_validation(&ValidationResults {
                compiles,
                lint_warnings: lint_warnings.len() as u32,
                ..Default::default()
            }),
            content_hash: hash(&ValidationResults { compiles, lint_warnings, .. }),
            ..Default::default()
        }])
    }
}
```

#### Station 4: Test Runner
```rust
impl Station for TestRunner {
    async fn process(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Pull code + test artifacts
        let code = self.find_source_code(&input)?;
        let tests = self.find_test_code(&input)?;

        // Run tests
        let results = self.run_tests(code, tests)?;

        // Calculate coverage
        let coverage = self.calculate_coverage(&results)?;

        // Emit test results artifact
        Ok(vec![Artifact {
            artifact_type: ArtifactType::TestResults,
            content: serialize_test_results(&results),
            validation: ValidationResults {
                tests_pass: results.all_passed(),
                coverage_percent: coverage,
                ..Default::default()
            },
            ..Default::default()
        }])
    }
}
```

#### Station 5: Packager
```rust
impl Station for Packager {
    async fn process(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Pull all artifacts for task
        let artifacts = self.collect_artifacts(&input.task_id)?;

        // Verify all validation passed
        for artifact in &artifacts {
            if !artifact.validation.all_passed() {
                return Err(StationError::ValidationFailed(artifact.id.clone()));
            }
        }

        // Generate receipt
        let receipt = Receipt {
            spec_hash: input.spec_hash,
            artifact_hashes: artifacts.iter().map(|a| a.content_hash).collect(),
            generator_version: env!("CARGO_PKG_VERSION"),
            generated_at: Utc::now(),
            reproducible: true,
        };

        // Emit receipt artifact
        Ok(vec![Artifact {
            artifact_type: ArtifactType::Receipt,
            content: serialize_receipt(&receipt),
            content_hash: hash(&receipt),
            ..Default::default()
        }])
    }
}
```

---

## a2a-rs: Transport and State Machine Substrate

### Core Responsibilities

**a2a-rs** is the foundational library providing:

1. **Transport Layer**: Message passing between agents (HTTP, WebSocket, stdio)
2. **Task State Machine**: Enforces legal transitions, prevents terminal state violations
3. **Event Bus**: Pub/sub for Andon signals
4. **Artifact Storage**: Content-addressable storage (hash → artifact)
5. **Receipt Generation**: Cryptographic audit trail

---

### Integration Points

```
┌─────────────────────────────────────────────────────────────┐
│                     a2a-rs ARCHITECTURE                     │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Application Layer (ggen agents)                            │
│  ─────────────────────────────────────────────────────      │
│    │                                                         │
│    ▼                                                         │
│  ┌─────────────────────────────────────────┐                │
│  │         a2a-rs Core Library             │                │
│  ├─────────────────────────────────────────┤                │
│  │  - Task State Machine                   │                │
│  │  - Event Bus (pub/sub)                  │                │
│  │  - Artifact Storage (CAS)               │                │
│  │  - Receipt Generator                    │                │
│  │  - Validation Framework                 │                │
│  └─────────────────────────────────────────┘                │
│    │                                                         │
│    ▼                                                         │
│  ┌─────────────────────────────────────────┐                │
│  │      Transport Adapters (Ports)         │                │
│  ├─────────────────────────────────────────┤                │
│  │  - HTTP Adapter                         │                │
│  │  - WebSocket Adapter                    │                │
│  │  - Stdio Adapter (for MCP)              │                │
│  └─────────────────────────────────────────┘                │
│    │                                                         │
│    ▼                                                         │
│  Network / IPC                                               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

### Implementation Patterns

#### Pattern 1: Task State Machine

```rust
use a2a_rs::{Task, TaskStatus, TaskError};

// Create task
let mut task = Task::builder()
    .spec("ontology://user-model.ttl#User")
    .targets(vec!["python", "typescript"])
    .acceptance(|validation| {
        validation.compiles
        && validation.tests_pass
        && validation.coverage_percent >= 80.0
    })
    .build()?;

// Enforce state machine
task.transition(TaskStatus::Assigned)?;       // OK
task.transition(TaskStatus::InProgress)?;     // OK
task.transition(TaskStatus::Completed)?;      // OK
task.transition(TaskStatus::InProgress)?;     // ERROR: terminal state reached
```

#### Pattern 2: Event Subscription

```rust
use a2a_rs::{EventBus, Event};

let event_bus = EventBus::new();

// Subscribe to task events
event_bus.subscribe("TASK.*", |event: Event| {
    match event.event_type.as_str() {
        "TASK.COMPLETED" => {
            println!("Task {} completed", event.task_id);
        }
        "TASK.FAILED" => {
            eprintln!("Task {} failed: {:?}", event.task_id, event.data);
        }
        _ => {}
    }
}).await?;

// Emit event (from agent)
event_bus.emit(Event {
    event_type: "TASK.COMPLETED",
    task_id: task.id,
    timestamp: Utc::now(),
    data: serde_json::json!({
        "artifacts": task.artifacts,
    }),
}).await?;
```

#### Pattern 3: Artifact Storage

```rust
use a2a_rs::{ArtifactStore, Artifact};

let store = ArtifactStore::new();

// Store artifact (content-addressable)
let artifact = Artifact {
    content: "generated code...".as_bytes().to_vec(),
    ..Default::default()
};

let artifact_id = store.put(artifact).await?;
// artifact_id = sha256 hash of content

// Retrieve by hash
let retrieved = store.get(&artifact_id).await?;
assert_eq!(artifact.content, retrieved.content);
```

---

## TPS Profile: Manufacturing Discipline at Protocol Level

### Pull-Only (JIT)

**Manufacturing Principle**: Don't produce what isn't needed (Just-In-Time delivery)

**A2A-CONSTRUCT Implementation**:

```rust
// ❌ ANTI-PATTERN: Push-based task assignment
fn assign_tasks_push(tasks: Vec<Task>, agents: Vec<Agent>) {
    for task in tasks {
        let agent = agents.iter().next().unwrap();
        agent.send_task(task);  // Pushed regardless of capacity
    }
}

// ✅ CORRECT: Pull-based task retrieval
impl Agent {
    async fn pull_work(&mut self) {
        loop {
            // Only pull when capacity available
            if self.active_tasks.len() < self.max_concurrent {
                match self.kanban.pull_task(self.id).await {
                    Some(task) => self.process(task).await,
                    None => tokio::time::sleep(Duration::from_secs(1)).await,
                }
            }
        }
    }
}
```

**Verification**:
```rust
// Metric: Queue depth should stay stable
assert!(kanban.in_progress.len() <= kanban.wip_limit);

// Metric: Agents should never be overloaded
for agent in agents {
    assert!(agent.active_tasks.len() <= agent.max_concurrent);
}
```

---

### WIP Limits (Kanban)

**Manufacturing Principle**: Limit work-in-progress to prevent overload

**A2A-CONSTRUCT Implementation**:

```rust
pub struct KanbanBoard {
    pub wip_limits: WipLimits,
    pub queues: HashMap<TaskStatus, Queue<Task>>,
}

pub struct WipLimits {
    pub assigned: usize,      // Max 10
    pub in_progress: usize,   // Max 20
}

impl KanbanBoard {
    pub fn can_pull(&self) -> bool {
        self.queues.get(&TaskStatus::InProgress).unwrap().len()
            < self.wip_limits.in_progress
    }

    pub fn pull_or_wait(&mut self) -> Option<Task> {
        if !self.can_pull() {
            // Backpressure: at WIP limit
            return None;
        }

        self.queues.get_mut(&TaskStatus::Pending)?.pop()
    }
}
```

**Verification**:
```rust
// Enforce WIP limits
#[test]
fn test_wip_limits_enforced() {
    let mut kanban = KanbanBoard::new(WipLimits {
        assigned: 10,
        in_progress: 20
    });

    // Fill to limit
    for i in 0..20 {
        let task = Task::new(format!("TASK-{}", i));
        kanban.queues.get_mut(&TaskStatus::InProgress).unwrap().push(task);
    }

    // Attempt to pull (should fail due to WIP limit)
    assert!(kanban.pull_or_wait().is_none());
}
```

---

### Terminality (Jidoka)

**Manufacturing Principle**: Stop the line when defect detected (don't amplify failures)

**A2A-CONSTRUCT Implementation**:

```rust
impl Task {
    pub fn transition(&mut self, new_status: TaskStatus) -> Result<(), TaskError> {
        // Jidoka: Detect terminal state violation
        if self.status.is_terminal() {
            // Stop the line: emit Andon signal
            emit_andon_signal(AndonSignal::TerminalStateViolation {
                task_id: self.id.clone(),
                current_state: self.status.clone(),
                attempted_transition: new_status,
            });

            // Halt: return error (don't proceed)
            return Err(TaskError::TerminalStateReached {
                task_id: self.id.clone(),
                current_state: self.status.clone(),
            });
        }

        // Allowed transition
        self.status = new_status;
        Ok(())
    }
}

// Andon signal emitted to monitoring dashboard
fn emit_andon_signal(signal: AndonSignal) {
    EVENT_BUS.emit(Event {
        event_type: "ANDON.CRITICAL",
        data: serde_json::to_value(signal).unwrap(),
        severity: Severity::Critical,
        timestamp: Utc::now(),
    });
}
```

**Verification**:
```rust
#[test]
fn test_terminal_state_immutability() {
    let mut task = Task::new("TASK-001");

    task.transition(TaskStatus::Completed).unwrap();

    // Attempt to modify terminal task (should fail)
    let result = task.transition(TaskStatus::InProgress);
    assert!(result.is_err());

    // Verify Andon signal was emitted
    assert!(ANDON_SIGNALS.contains(&task.id));
}
```

---

### Artifact-First (Heijunka)

**Manufacturing Principle**: Level production (even flow, no spikes)

**A2A-CONSTRUCT Implementation**:

```rust
// Artifacts flow at even pace (no spikes)
impl Station {
    async fn process_with_leveling(&self, input: Artifact) -> Result<Vec<Artifact>, StationError> {
        // Rate limiting: max 100 artifacts/sec
        self.rate_limiter.acquire(1).await;

        // Process artifact
        let outputs = self.process(input).await?;

        // Emit artifacts at controlled rate
        for output in outputs {
            self.output_queue.push(output).await;
        }

        Ok(outputs)
    }
}

// Even distribution across stations
pub struct Pipeline {
    pub stations: Vec<Box<dyn Station>>,
}

impl Pipeline {
    pub async fn run(&self, spec: Artifact) {
        let mut current = vec![spec];

        for station in &self.stations {
            let mut next = Vec::new();

            // Process evenly (no station gets overloaded)
            for artifact in current {
                let outputs = station.process_with_leveling(artifact).await?;
                next.extend(outputs);
            }

            current = next;
        }
    }
}
```

**Verification**:
```rust
// Metric: Throughput should be consistent
assert!(pipeline.throughput_variance() < 0.1);  // < 10% variance

// Metric: No station should be bottleneck
for station in pipeline.stations {
    assert!(station.utilization() < 0.8);  // < 80% utilization
}
```

---

## Concrete Examples

### Example 1: Code Generation Pipeline

**Scenario**: Generate user model in 3 languages from RDF ontology

**Workflow**:

```
┌────────────────────────────────────────────────────────────┐
│          CODE GENERATION PIPELINE (A2A-CONSTRUCT)          │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  1. Create Task                                            │
│     ─────────────                                          │
│     POST /a2a/tasks                                        │
│     {                                                      │
│       "spec": "ontology://user-model.ttl#User",           │
│       "targets": ["python", "typescript", "rust"],        │
│       "acceptance": {                                      │
│         "compiles": true,                                  │
│         "tests_pass": true,                                │
│         "coverage_percent": 80.0                           │
│       }                                                    │
│     }                                                      │
│     → Response: {"task_id": "TASK-001", "status": "PENDING"}
│                                                            │
│  2. Agent Pulls Task (Pull-Only)                           │
│     ───────────────────────────                            │
│     GET /a2a/tasks/pull?agent_id=agent-001                 │
│     → Response: {"task": {...}, "status": "ASSIGNED"}      │
│                                                            │
│  3. Agent Processes (Station 1: Parse RDF)                 │
│     ────────────────────────────────────                   │
│     - Parse ontology://user-model.ttl#User                 │
│     - Extract classes, properties, constraints            │
│     - Emit artifact: ART-001 (parsed AST)                  │
│                                                            │
│  4. Agent Processes (Station 2: Generate Code)             │
│     ────────────────────────────────────────               │
│     - Generate user.py (Python)     → ART-002             │
│     - Generate user.ts (TypeScript) → ART-003             │
│     - Generate user.rs (Rust)       → ART-004             │
│                                                            │
│  5. Agent Processes (Station 3: Validate)                  │
│     ───────────────────────────────────                    │
│     - Compile user.py     → OK                            │
│     - Compile user.ts     → OK                            │
│     - Compile user.rs     → OK                            │
│     - Run linter (all)    → 0 warnings                    │
│     - Emit artifact: ART-005 (validation report)          │
│                                                            │
│  6. Agent Processes (Station 4: Test)                      │
│     ───────────────────────────────                        │
│     - Generate tests (all languages)                       │
│     - Run tests:                                           │
│       - user_test.py:  ✓ 12/12 passed (coverage: 92%)    │
│       - user.test.ts:  ✓ 10/10 passed (coverage: 88%)    │
│       - user_test.rs:  ✓ 15/15 passed (coverage: 95%)    │
│     - Emit artifact: ART-006 (test results)               │
│                                                            │
│  7. Agent Completes Task (Terminal State)                  │
│     ──────────────────────────────────                     │
│     POST /a2a/tasks/TASK-001/complete                      │
│     {                                                      │
│       "artifacts": [                                       │
│         "ART-002", "ART-003", "ART-004",                  │
│         "ART-005", "ART-006"                              │
│       ],                                                   │
│       "receipt": {                                         │
│         "spec_hash": "sha256:1a2b3c4d...",               │
│         "artifact_hashes": [...],                         │
│         "generator_version": "ggen-0.2.0"                 │
│       }                                                    │
│     }                                                      │
│     → Response: {"status": "COMPLETED"}                    │
│                                                            │
│  8. Events Emitted (Andon Signals)                         │
│     ────────────────────────────                           │
│     - TASK.CREATED        (10:00:00Z)                     │
│     - TASK.ASSIGNED       (10:00:05Z)                     │
│     - TASK.IN_PROGRESS    (10:00:10Z)                     │
│     - ARTIFACT.GENERATED  (10:00:45Z) × 5                 │
│     - VALIDATION.PASSED   (10:01:00Z)                     │
│     - TESTS.PASSED        (10:01:15Z)                     │
│     - TASK.COMPLETED      (10:01:20Z)                     │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

**Result**:
- ✅ 5 artifacts generated (3 source files, 1 validation report, 1 test report)
- ✅ All validation passed
- ✅ All tests passed
- ✅ Cryptographic receipt issued
- ✅ Task reached terminal state (COMPLETED)
- ✅ No human intervention required

---

### Example 2: Multi-Language Code Generation

**Scenario**: Generate REST API in Python, TypeScript, and Rust from OpenAPI spec

```turtle
# api-spec.ttl
@prefix api: <http://example.org/api#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:UserEndpoint a api:RestEndpoint ;
    api:path "/users/{id}" ;
    api:method api:GET ;
    api:pathParam [
        api:name "id" ;
        api:type xsd:string
    ] ;
    api:response [
        api:status 200 ;
        api:body :User
    ] ;
    api:response [
        api:status 404 ;
        api:body :NotFoundError
    ] .

:User a api:Schema ;
    api:field [ api:name "id" ; api:type xsd:string ; api:required true ] ;
    api:field [ api:name "name" ; api:type xsd:string ; api:required true ] ;
    api:field [ api:name "email" ; api:type xsd:string ; api:required true ] .
```

**Task Creation**:
```json
{
  "spec": "ontology://api-spec.ttl#UserEndpoint",
  "targets": ["python", "typescript", "rust"],
  "acceptance": {
    "compiles": true,
    "tests_pass": true,
    "api_contract_valid": true
  }
}
```

**Generated Artifacts**:

1. **user_api.py** (Python/FastAPI):
```python
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel

app = FastAPI()

class User(BaseModel):
    id: str
    name: str
    email: str

@app.get("/users/{id}", response_model=User)
async def get_user(id: str):
    # Implementation generated from spec
    pass
```

2. **user-api.ts** (TypeScript/Express):
```typescript
import express from 'express';

interface User {
  id: string;
  name: string;
  email: string;
}

const app = express();

app.get('/users/:id', (req, res) => {
  // Implementation generated from spec
});
```

3. **user_api.rs** (Rust/Axum):
```rust
use axum::{Router, extract::Path, Json};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: String,
    name: String,
    email: String,
}

async fn get_user(Path(id): Path<String>) -> Json<User> {
    // Implementation generated from spec
}
```

**Validation Results**:
```json
{
  "task_id": "TASK-002",
  "artifacts": [
    {
      "artifact_id": "ART-007",
      "language": "python",
      "validation": {
        "compiles": true,
        "tests_pass": true,
        "api_contract_matches_spec": true
      }
    },
    {
      "artifact_id": "ART-008",
      "language": "typescript",
      "validation": {
        "compiles": true,
        "tests_pass": true,
        "api_contract_matches_spec": true
      }
    },
    {
      "artifact_id": "ART-009",
      "language": "rust",
      "validation": {
        "compiles": true,
        "tests_pass": true,
        "api_contract_matches_spec": true
      }
    }
  ],
  "status": "COMPLETED"
}
```

---

### Example 3: Documentation Generation

**Scenario**: Generate API documentation in Markdown and HTML from ontology

**Task**:
```json
{
  "spec": "ontology://api-spec.ttl",
  "targets": ["markdown", "html"],
  "acceptance": {
    "all_endpoints_documented": true,
    "links_valid": true,
    "renders_correctly": true
  }
}
```

**Generated Artifacts**:

1. **api-docs.md**:
```markdown
# User API Documentation

## Endpoints

### GET /users/{id}

Retrieve a user by ID.

**Path Parameters**:
- `id` (string, required): User identifier

**Responses**:
- `200 OK`: User found
  ```json
  {
    "id": "string",
    "name": "string",
    "email": "string"
  }
  ```
- `404 Not Found`: User not found
```

2. **api-docs.html**:
```html
<!DOCTYPE html>
<html>
<head>
  <title>User API Documentation</title>
</head>
<body>
  <h1>User API Documentation</h1>
  <h2>Endpoints</h2>
  <h3>GET /users/{id}</h3>
  <p>Retrieve a user by ID.</p>
  <!-- ... -->
</body>
</html>
```

**Validation**:
- ✅ All endpoints from spec documented
- ✅ All links resolve correctly
- ✅ HTML renders without errors
- ✅ Markdown passes linting

---

## Anti-Patterns and Common Mistakes

### Anti-Pattern 1: Chat-Based Coordination

**❌ WRONG**:
```rust
// Agents negotiate via chat
agent1.send_message("Can you generate code for user model?");
agent2.send_message("Sure, what language?");
agent1.send_message("Python and TypeScript");
agent2.send_message("OK, generating...");
```

**✅ CORRECT**:
```rust
// Agents communicate via structured tasks
let task = Task::builder()
    .spec("ontology://user-model.ttl#User")
    .targets(vec!["python", "typescript"])
    .build()?;

kanban.push_task(task).await?;
// Agent pulls when ready (no negotiation)
```

---

### Anti-Pattern 2: Unbounded Queues

**❌ WRONG**:
```rust
pub struct KanbanBoard {
    pub pending: Vec<Task>,  // Unbounded: can grow forever
}
```

**✅ CORRECT**:
```rust
pub struct KanbanBoard {
    pub pending: Queue<Task>,         // Unbounded (input queue)
    pub in_progress: BoundedQueue<Task, 20>,  // WIP limit: 20
}
```

---

### Anti-Pattern 3: Mutable Terminal States

**❌ WRONG**:
```rust
impl Task {
    pub fn reopen(&mut self) {
        // Allow reopening completed tasks
        if self.status == TaskStatus::Completed {
            self.status = TaskStatus::InProgress;  // WRONG!
        }
    }
}
```

**✅ CORRECT**:
```rust
impl Task {
    pub fn transition(&mut self, new_status: TaskStatus) -> Result<(), TaskError> {
        if self.status.is_terminal() {
            return Err(TaskError::TerminalStateReached);
        }
        // Terminal states are immutable
    }
}
```

---

### Anti-Pattern 4: Push-Based Work Distribution

**❌ WRONG**:
```rust
// Manager pushes work to agents
for task in tasks {
    let agent = find_least_busy_agent();
    agent.assign(task);  // Pushed regardless of capacity
}
```

**✅ CORRECT**:
```rust
// Agents pull work when ready
impl Agent {
    async fn work_loop(&mut self) {
        loop {
            if self.has_capacity() {
                if let Some(task) = self.kanban.pull_task(self.id).await {
                    self.process(task).await;
                }
            }
        }
    }
}
```

---

### Anti-Pattern 5: No Validation Gates

**❌ WRONG**:
```rust
// Generate artifacts without validation
let artifacts = self.generate_code(spec)?;
task.complete(artifacts)?;  // No validation!
```

**✅ CORRECT**:
```rust
// Validate before completion
let artifacts = self.generate_code(spec)?;
let validation = self.validate_all(&artifacts)?;

if validation.all_passed() {
    task.complete(artifacts)?;
} else {
    task.fail(validation.errors)?;
}
```

---

## Implementation Guide

### Phase 1: Foundation

**Objective**: Implement core A2A-CONSTRUCT primitives

**Deliverables**:
1. Task state machine (Rust/a2a-rs)
2. Kanban board with WIP limits
3. Event bus for Andon signals
4. Artifact storage (content-addressable)

**Acceptance Criteria**:
- [ ] Task transitions enforce state machine rules
- [ ] Terminal states are immutable
- [ ] WIP limits prevent queue overflow
- [ ] Events emitted for all state changes
- [ ] Artifacts stored by hash

**Timeline**: 2 weeks

---

### Phase 2: Agent Integration

**Objective**: Connect ggen agents to A2A-CONSTRUCT protocol

**Deliverables**:
1. Agent pull loop (work retrieval)
2. Station implementations (parse, generate, validate, test, package)
3. Artifact flow between stations
4. Receipt generation

**Acceptance Criteria**:
- [ ] Agent pulls tasks when capacity available
- [ ] Stations process artifacts sequentially
- [ ] All artifacts validated before task completion
- [ ] Receipts include cryptographic hashes

**Timeline**: 4 weeks

---

### Phase 3: Post-Human Scaling

**Objective**: Remove human supervision, enable token flow

**Deliverables**:
1. Automated spec validation (no manual review)
2. Deterministic generation (same spec → same artifacts)
3. Continuous deployment (completed tasks → production)
4. Monitoring dashboard (Andon board)

**Acceptance Criteria**:
- [ ] 95% of tasks complete without human intervention
- [ ] Reproduction success rate 100%
- [ ] Deployment latency <5 minutes (task complete → production)
- [ ] Andon dashboard shows real-time task status

**Timeline**: 8 weeks

---

## Metrics and Validation

### TPS Metrics

| Metric | Target | Measurement | Purpose |
|--------|--------|-------------|---------|
| **Queue Depth (Pending)** | Stable | avg(pending_count) per hour | Detect demand spikes |
| **WIP (In Progress)** | < WIP Limit | current_in_progress / wip_limit | Prevent overload |
| **Throughput** | 100 tasks/hour | completed_tasks / time | Measure productivity |
| **Cycle Time** | < 5 minutes | completed_at - created_at | Optimize flow |
| **Terminal State Rate** | 95%+ | (completed + failed + cancelled) / total | Verify terminality |

### Validation Metrics

| Metric | Target | Measurement | Purpose |
|--------|--------|-------------|---------|
| **Compile Success** | 100% | artifacts_compiled / artifacts_generated | Code quality |
| **Test Pass Rate** | 100% | tests_passed / tests_run | Functional correctness |
| **Coverage** | ≥80% | lines_covered / lines_total | Test thoroughness |
| **Lint Warnings** | 0 | lint_warnings_count | Code style compliance |

### Reproducibility Metrics

| Metric | Target | Measurement | Purpose |
|--------|--------|-------------|---------|
| **Hash Match Rate** | 100% | hash_matches / replay_attempts | Determinism verification |
| **Replay Success** | 100% | successful_replays / replay_attempts | Generator stability |

---

## Further Reading

- [A2A Protocol Fundamentals](../explanations/fundamentals/a2a-protocol.md) - Core protocol concepts
- [TPS Principles & Erlang/OTP Patterns](../tps-reference/00-tps-principles.md) - Manufacturing principles
- [Work Object Model State Machines](../../.specify/WORK-OBJECT-MODEL-STATE-MACHINES.md) - State machine patterns
- [Regime Split: SELECT/DO vs CONSTRUCT](01-regime-split.md) - Paradigm comparison
- [Packet Discipline](10-packet-discipline.md) - Type-safe work orders

---

## Appendix: Protocol Message Formats

### Task Creation (JSON-RPC)

```json
{
  "jsonrpc": "2.0",
  "method": "a2a.task.create",
  "params": {
    "spec": "ontology://user-model.ttl#User",
    "targets": ["python", "typescript", "rust"],
    "acceptance": {
      "compiles": true,
      "tests_pass": true,
      "min_coverage_percent": 80.0,
      "max_lint_warnings": 0
    },
    "priority": "normal",
    "timeout_seconds": 300
  },
  "id": 1
}
```

### Task Pull (HTTP GET)

```http
GET /a2a/tasks/pull?agent_id=agent-001&max_concurrent=5 HTTP/1.1
Host: kanban.example.com

Response:
{
  "task_id": "TASK-001",
  "status": "ASSIGNED",
  "spec": "ontology://user-model.ttl#User",
  "targets": ["python", "typescript", "rust"],
  "acceptance": { ... },
  "assigned_at": "2026-02-09T10:00:05Z"
}
```

### Task Completion (HTTP POST)

```http
POST /a2a/tasks/TASK-001/complete HTTP/1.1
Host: kanban.example.com
Content-Type: application/json

{
  "artifacts": [
    {
      "artifact_id": "ART-001",
      "type": "source_code",
      "language": "python",
      "content_hash": "sha256:7f3a9b2c..."
    },
    {
      "artifact_id": "ART-002",
      "type": "source_code",
      "language": "typescript",
      "content_hash": "sha256:4d2e1a9f..."
    }
  ],
  "receipt": {
    "spec_hash": "sha256:1a2b3c4d...",
    "generator_version": "ggen-0.2.0",
    "generated_at": "2026-02-09T10:01:20Z"
  }
}
```

### Event Stream (WebSocket)

```json
{
  "event_type": "TASK.COMPLETED",
  "task_id": "TASK-001",
  "timestamp": "2026-02-09T10:01:20Z",
  "data": {
    "duration_seconds": 80,
    "artifacts": ["ART-001", "ART-002"],
    "validation": {
      "all_passed": true,
      "coverage_percent": 89.2
    }
  }
}
```

---

**Document Hash**: `sha256:TBD`
**Generated**: 2026-02-09
**Regime**: CONSTRUCT (this document is specification-driven)
**Last Review**: 2026-02-09
