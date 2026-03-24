# ggen v6.0.0 - System Architecture Guide

## Table of Contents

1. [System Overview](#system-overview)
2. [Design Patterns](#design-patterns)
3. [Architecture Decisions](#architecture-decisions)
4. [Component Interactions](#component-interactions)
5. [Diagrams](#diagrams)

---

## System Overview

### ggen as a Whole

ggen is a **specification-driven code generation framework** that treats software artifacts as projections of knowledge graphs. It transforms RDF ontologies into reproducible, type-safe code through a five-stage pipeline.

**Core Formula:** A = μ(O), where A is artifacts and O is ontologies.

**Stack:** Rust 1.91.1 | Tokio | Oxigraph | Tera | Serde | 30 crates | 87% test coverage

### A2A (Agent-to-Agent) Subsystem

The A2A subsystem enables distributed autonomous agents to collaborate by treating tasks as kanban cards flowing through a state machine.

**Task State Machine:**
```
Created → Running → Blocked ─→ Completed (terminal)
                      ↓
                    Failed (terminal)
```

**Key Components:**
1. **Task State Machine** (`ggen-a2a`) - Kanban task flow with strict transitions
2. **Message Transport** (`ggen-a2a`) - Envelope-based protocol with async support
3. **Tool Registry** (`ggen-a2a-mcp`) - Dynamic tool discovery and execution
4. **Consensus Layer** (`ggen-consensus`) - PBFT for receipt verification
5. **Task Coordination** - Leader election, work queues, consensus

### OSIRIS (Life Domains) Subsystem

OSIRIS implements a **Toyota Production System (TPS)** autonomic architecture for managing life domains with zero cognitive load.

**Core Components:**
- **OSIRIS Core** - Life domain abstraction and lifecycle management
- **OSIRIS Domains** - Domain registry and domain-specific workflows
- **OSIRIS TPS** - Kaizen (continuous improvement), Jidoka (quality gates), Heijunka (load leveling), Andon (signals)
- **OSIRIS Sensors** - Apple/Android sensor integration for real-time metrics
- **OSIRIS Autonomic** - Self-governing life management with constraint satisfaction

**Life Domain Abstraction:**
Domains include: Health, Work, Relationships, Finance, Personal Growth, Recreation
Each domain has: Status tracking, Workflow execution, Performance metrics, Health indicators

### MCP (Model Context Protocol) Integration

MCP provides a **standardized bridge between LLMs and external tools**, enabling agents to discover and invoke capabilities dynamically.

**MCP Stack:**
1. **RMCP Client** (`ggen-ai`) - LLM provider integration (OpenAI, Anthropic, etc.)
2. **MCP Transport** (`ggen-a2a-mcp`) - JSON-RPC 2.0 protocol with bidirectional communication
3. **Tool Adapter Layer** (`ggen-a2a-mcp`) - Convert between A2A skills and MCP tools
4. **Tool Registry** (`ggen-a2a-mcp`) - Dynamic tool discovery and execution
5. **YAWL Bridge** (`ggen-a2a-mcp`) - Map workflow tasks to A2A tasks with event publishing

### Component Interactions

**Typical Agent Workflow:**
```
LLM Agent
   ↓
[MCP Protocol]
   ↓
Tool Registry (ggen-a2a-mcp)
   ↓
A2A Task State Machine (ggen-a2a)
   ↓
Domain Executor (osiris-*)
   ↓
Consensus Verification (ggen-consensus)
   ↓
Receipt Generation (ggen-receipt)
```

---

## Design Patterns

### 1. State Machine Pattern

**Problem:** Tasks need to flow through well-defined states with strict transition rules.

**Solution:** Explicit state machine encoding states and transitions.

**Benefits:**
- Compiler enforces valid states
- Clear transition rules with no invalid combinations
- Deterministic behavior
- No feedback loops or recovery queuing

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a/src/state_machine.rs`

---

### 2. Adapter Pattern

**Problem:** A2A tasks and MCP tools have different interfaces; direct coupling creates brittleness.

**Solution:** Bidirectional adapter layers converting between A2A and MCP representations.

**Benefits:**
- Loose coupling between subsystems
- Easy to add new tool types
- Testing in isolation
- Clear responsibility boundaries

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/adapter.rs`

---

### 3. Message-Oriented Communication

**Problem:** Direct function calls create tight coupling; distributed systems need async, resilient messaging.

**Solution:** Envelope-based message protocol with async/await friendly transport.

**Benefits:**
- Async-friendly, non-blocking operations
- Message ordering guaranteed
- Retry logic at protocol level
- Observable for debugging

**Message Types:** TaskCreated, TaskAssigned, TaskRunning, TaskCompleted, TaskFailed

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a/src/transport.rs`

---

### 4. RDF-Driven Code Generation

**Problem:** Keeping code in sync with specifications is error-prone; changes diverge.

**Solution:** Single source of truth (RDF), generated code, never hand-edited.

**Benefits:**
- Single source of truth
- Reproducible code
- Version control friendly
- Enables code archaeology (trace back to spec)

**Trade-off:** RDF has steeper learning curve but enables powerful reasoning.

---

### 5. Fault Tolerance Patterns

#### Supervisor Tree (Monitoring and Recovery)

**Problem:** Tasks may fail; we need automatic recovery without manual intervention.

**Solution:** Hierarchical supervision with exponential backoff recovery strategies.

**Recovery Strategies:**
- `Restart` (clear state, retry)
- `Escalate` (bubble to parent supervisor)
- `Abandon` (terminal failure, alert)

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/handlers.rs`

---

#### Circuit Breaker

**Problem:** Cascading failures; repeated attempts to failing services consume resources.

**Solution:** Three-state circuit (Closed → Open → Half-Open) for fast failure detection.

**Benefits:**
- Fast failure detection
- Prevents resource exhaustion
- Allows recovery time
- Observable state transitions

**Implementation:** `/Users/sac/ggen/crates/ggen-backpressure/src/`

---

#### Bulkhead Pattern (Isolation)

**Problem:** One failing subsystem brings down the entire system.

**Solution:** Partition resources; isolate failure domains with quotas and limits.

**Mechanisms:**
- Thread pools with fixed sizes
- Memory limits per domain
- Network rate limiting
- Storage quotas

**Implementation:** `/Users/sac/ggen/crates/ggen-backpressure/src/`

---

### 6. Consensus Algorithms

#### PBFT (Practical Byzantine Fault Tolerance)

**Problem:** Distributed systems must agree on truth even if some nodes are faulty or malicious.

**Solution:** Multi-phase consensus tolerating up to f faulty nodes with 3f+1 total replicas.

**Phases:**
1. **PrePrepare**: Primary sends message to all
2. **Prepare**: Replicas acknowledge, collect 2f+1 agreements
3. **Commit**: Replicas commit, must have 2f+1 prepares
4. **ViewChange**: If primary fails, elect new primary

**Example:** 4 replicas (3f+1, f=1) tolerate 1 Byzantine fault

**Benefits:**
- Tolerates Byzantine (malicious) faults
- Cryptographic proof (Ed25519 signatures)
- Deterministic finality (no ambiguity)
- Audit trail in receipt

**Implementation:** `/Users/sac/ggen/crates/ggen-consensus/src/pbft.rs`

---

#### Quorum-Based Voting

**Problem:** Which answers are correct when nodes disagree?

**Solution:** Majority vote (quorum) for consensus.

**Thresholds:**
- Consensus: 2f+1
- Byzantine: f
- Total: 3f+1

---

### 7. Tool Discovery and Composition

**Problem:** LLMs need to know what tools are available and how to use them.

**Solution:** Dynamic tool discovery with capability advertisement via JSON schemas.

**Core Tools Registered:**
- `create_task` - Create new task
- `update_task` - Modify task
- `complete_task` - Mark done
- `fail_task` - Report failure
- `list_tasks` - Query tasks
- `create_domain` - Create life domain
- `update_domain_status` - Change domain state
- `verify_receipt` - Consensus verification

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/registry.rs`

---

## Architecture Decisions

### Why RDF for Specifications

**Rationale:**
- **Expressiveness**: Captures semantic relationships
- **Standardization**: W3C standard, widely adopted
- **Graph structure**: Naturally models complex domains
- **Tooling**: SPARQL for queries, SHACL for validation
- **Archaeology**: Trace code back to specification

---

### Why Tera for Code Generation

**Rationale:**
- **Safety**: Logic-less templates prevent security issues
- **Rust integration**: Designed for Rust ecosystem
- **Inheritance**: Template extension for DRY code
- **Performance**: Fast rendering, small footprint
- **Filters**: Rich set of transformations

---

### Why Chicago TDD for Testing

**Rationale:**
- **Real behavior**: Tests real interactions, not mock contracts
- **Refactoring safe**: Change internals without breaking tests
- **Simplicity**: No mock setup complexity
- **Confidence**: Tests what users experience
- **Performance**: Real objects often faster than mocks

**Guidelines:**
- Use real collaborators (databases, services)
- Avoid mocks except for truly external deps (network)
- State-based assertions (check outcomes)
- AAA pattern (Arrange/Act/Assert)

---

### Why A2A for Agent Coordination

**Rationale:**
- **Task identity**: Each task tracked end-to-end
- **State machine**: Tasks follow clear state transitions
- **Determinism**: Same input always produces same result
- **Observable**: Receipt system provides audit trail
- **Language-agnostic**: Protocol can span systems

---

### Why Consensus for Receipt Verification

**Rationale:**
- **Byzantine tolerance**: Handles malicious or faulty nodes
- **Cryptographic proof**: Ed25519 signatures
- **Audit trail**: Full consensus message history
- **Deterministic**: No ambiguity about truth
- **Regulatory**: Meets compliance requirements

---

## Component Interactions

### 1. Agent Creation Workflow

```
User Request
   ↓
[MCP Tool Call] agent:create_agent
   ↓
Tool Registry (validates schema)
   ↓
A2A Task: "Create Agent"
   ↓
State: Created → Running
   ↓
Executor: Create agent process, register capabilities
   ↓
State: Running → Completed
   ↓
Receipt: Hash + Byzantine consensus
   ↓
Response: Agent ID + MCP tools available
```

**Implementation Path:**
1. `/Users/sac/ggen/crates/ggen-a2a-mcp/src/handlers.rs` - Handler dispatch
2. `/Users/sac/ggen/crates/ggen-a2a/src/state_machine.rs` - State transitions
3. `/Users/sac/ggen/crates/ggen-consensus/src/pbft.rs` - Receipt verification

---

### 2. Message Routing Pipeline

```
Incoming Message
   ↓
[Envelope decode]
   ↓
[Content type detection] (JSON, binary, multipart)
   ↓
[Handler factory] select appropriate handler
   ↓
[Handler processing] execute business logic
   ↓
[Response encoding]
   ↓
[Envelope encode]
   ↓
Outgoing Message
```

**Handler Types:**
- TextContentHandler - JSON, UTF-8
- BinaryContentHandler - Raw bytes, images
- FileContentHandler - File references
- MultipartHandler - Mixed content
- StreamHandler - Continuous data
- BatchProcessor - Multiple messages

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/handlers.rs`

---

### 3. Tool Discovery Process

```
LLM: "What tools are available?"
   ↓
[MCP Tool: tools/list]
   ↓
Tool Registry
   ├─ Query all registered tools
   ├─ Generate JSON schemas
   ├─ Collect descriptions
   └─ Return tool list
   ↓
Response: [ { name, description, inputSchema, ... }, ... ]
   ↓
LLM: "I'll use create_task with these parameters"
   ↓
[MCP Tool: call create_task]
   ↓
Adapter → A2A Task → Executor → Receipt → Response
```

**Implementation:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/registry.rs`

---

### 4. Goal Planning and Execution

```
Goal: "Improve health domain"
   ↓
[Plan Generation] - Decompose into subtasks
   ├─ Task 1: "Measure current health"
   ├─ Task 2: "Identify improvement areas"
   ├─ Task 3: "Create health workflow"
   └─ Task 4: "Execute workflow"
   ↓
[Task Sequencing] - Detect dependencies
   ├─ Task 1 → Task 2 (depends on results)
   ├─ Task 2 → Task 3 (input for planning)
   └─ Task 3 → Task 4 (workflow ready)
   ↓
[Parallel Execution] - Where possible
   ↓
[Consensus] - Verify each task completion
   ├─ PBFT agreement on results
   ├─ Receipt generation
   └─ Audit trail
   ↓
[Goal Completion] - All subtasks done
   ↓
Receipt: Goal completion verified
```

---

### 5. Consensus Coordination

**Scenario:** Verify task completion across 4 replicas (tolerate 1 Byzantine)

```
Primary (Replica 1):
   ├─ [PrePrepare] → "Task X completed with result Y"
   │
Replica 2:
   ├─ [Prepare] → "I saw PrePrepare, agree with result"
   │
Replica 3:
   ├─ [Prepare] → "I saw PrePrepare, agree with result"
   │
Replica 4:
   ├─ [Prepare] → "I disagreed (Byzantine, or slow)"
   │
Primary:
   ├─ Collected 3 Prepare messages (≥ 2f+1 = 3) ✓
   ├─ [Commit] → "Consensus reached"
   │
All replicas:
   ├─ [Commit] → Finalize
   ├─ State: Committed
   └─ Receipt: Ed25519 multi-sig from 3 replicas

Result: Task completion verified with Byzantine tolerance
```

**Implementation:** `/Users/sac/ggen/crates/ggen-consensus/src/pbft.rs`

---

## Diagrams

### System Architecture (All Components)

```
LLM & External Systems (OpenAI, Anthropic, etc.)
   ↓
MCP Protocol Layer (JSON-RPC 2.0, Tool Discovery)
   ↓
Adapter Layer (AgentToToolAdapter, ToolToAgentAdapter)
   ↓
A2A Protocol & Task State Machine
   Created → Running → Blocked → Completed
       ↓
     Failed
   ↓
Domain Execution & Workflow Orchestration
   OSIRIS Core, TPS, Domains, YAWL Bridge
   ↓
Consensus & Receipt Verification
   PBFT, Quorum Voting, Ed25519 Signatures
   ↓
Quality Control & Monitoring
   Jidoka, Andon, Metrics, Backpressure
   ↓
Code Generation & Specification Foundation
   μ₁ Parse → μ₂ Validate → μ₃ Query → μ₄ Transform → μ₅ Codegen
   ↓
Artifact Storage & Configuration
   Generated Code, Configurations, Documentation, Workflows
```

---

### Agent Lifecycle State Machine

```
Uninitialized
   ↓ [Initialize]
Initialized
   ↓ [Register Tools]
Registered
   ↓ [Accept Task]
Running ←─────┐
   ↓   ↑      │
   ├─[Task Blocked]
   ├─[Task Complete]
   └─[Task Fails]
   ↓
Task Completed or Consensus Verified
   ↓ [Shutdown/Cleanup]
Terminated
```

---

### OSIRIS Domain Structure

```
Person (User)
    ├─ Health Domain
    │   ├─ Vitals, Sleep, Exercise, Nutrition, Metrics
    ├─ Work Domain
    │   ├─ Projects, Tasks, Skills, Performance, Career
    ├─ Finance Domain
    │   ├─ Income, Expenses, Investments, Debt, Net Worth
    ├─ Relationships Domain
    │   ├─ Family, Friends, Professional, Community, Communication
    ├─ Personal Growth Domain
    │   ├─ Learning, Creativity, Mindfulness, Spirituality, Development
    └─ Recreation Domain
        ├─ Hobbies, Entertainment, Travel, Physical Activities, Rest
```

Each domain has: Status (Active/Paused/Completed), Workflows, Metrics, Health Indicators

---

## Key Files and Crates

**Core Architecture:**
- `/Users/sac/ggen/crates/ggen-a2a/` - Task state machine and A2A protocol
- `/Users/sac/ggen/crates/ggen-a2a-mcp/` - MCP integration and adapters
- `/Users/sac/ggen/crates/ggen-consensus/` - Byzantine consensus (PBFT)
- `/Users/sac/ggen/crates/osiris-core/` - Life domain framework
- `/Users/sac/ggen/crates/osiris-domains/` - Domain registry and workflows
- `/Users/sac/ggen/crates/ggen-core/` - Code generation pipeline (μ₁-μ₅)

**Operational Excellence:**
- `/Users/sac/ggen/crates/ggen-jidoka/` - Quality gates
- `/Users/sac/ggen/crates/ggen-backpressure/` - Admission control
- `/Users/sac/ggen/crates/ggen-receipt/` - Receipt generation
- `/Users/sac/ggen/crates/ggen-tps-andon/` - Signal system
- `/Users/sac/ggen/crates/ggen-metrics-tps/` - Metrics collection

---

## Conclusion

ggen's architecture combines specification-driven development (RDF), distributed agent coordination (A2A), Byzantine fault tolerance (PBFT), and operational excellence (TPS principles) to create a robust, deterministic, and highly observable system.

