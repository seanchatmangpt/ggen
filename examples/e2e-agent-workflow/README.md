# End-to-End Agent Workflow Integration Example

Wave 5 example demonstrating a complete, integrated system combining:

1. **A2A Agents** - Agent-to-agent task state machine protocol
2. **OSIRIS Domains** - Life domain goal management and balancing  
3. **MCP Tools** - Model Context Protocol tool discovery and execution
4. **Distributed Consensus** - Byzantine fault tolerance across agents

## System Architecture

### Three Key Pillars

**Pillar 1: Fault Tolerance**
- Agent health tracking with degradation and recovery
- Automatic failure detection via heartbeat
- Failed agents isolated and re-routed
- Self-healing agent pool with Byzantine fault tolerance
- Cascading failure prevention

**Pillar 2: Distributed Coordination**
- Multi-agent consensus with supermajority voting
- Byzantine fault tolerance (tolerate f failures in 3f+1 agents)
- Domain priority negotiation
- Goal completion verification
- Consistent state across all agents

**Pillar 3: Autonomous Reasoning**
- Automatic MCP tool discovery
- AI-driven plan generation from goals
- Dependency-aware workflow execution
- Result analysis and verification
- Learning from successes and failures

## Components

### `lifecycle.rs` - Agent Pool Management
- AgentPool: Manages 6 autonomous agents
- AgentHealth: Tracks individual agent metrics
- HealthStatus: Healthy, Degraded, Failed, Recovering
- HealthSummary: System-wide health overview
- Recovery mechanisms for failed agents

### `coordination.rs` - Distributed Consensus
- ConsensusManager: Voting and consensus logic
- VotingStrategy: Majority, Supermajority, Unanimous
- ByzantineFaultTolerance: Safety proofs for 3f+1 agents
- AgentVote: Individual voting records

### `domain_manager.rs` - Life Domain Management
- LifeDomain: 6 domains (Health, Mental, Financial, Social, Career, Learning)
- DomainGoal: Individual goals within domains
- DomainManager: Goal tracking and domain balancing
- DomainStatus: Thriving, Balanced, Needs, Critical

### `tool_integration.rs` - MCP Tool Integration
- ToolSpec: Available tools via MCP
- ToolCall: Tool invocation with arguments
- ToolResult: Execution results and error handling
- ToolManager: Discovery, execution, and tracking

### `workflow.rs` - Workflow Orchestration
- WorkflowStep: Individual workflow steps
- Workflow: Complete workflow with dependencies
- WorkflowExecutor: Async workflow execution
- Dependency resolution and execution ordering

## Running the Example

### Tests
```bash
cargo test --example e2e-agent-workflow
```

### Individual Test Suites
```bash
# End-to-end workflow tests (15 tests)
cargo test --test e2e_workflow_tests

# Integration tests (9 tests)
cargo test --test integration_tests

# Fault tolerance tests (10 tests)
cargo test --test fault_tolerance_tests

# Consensus tests (10 tests)
cargo test --test consensus_tests
```

## Test Coverage

**Total: 44 integration tests**

### E2E Workflow Tests (15)
- Agent pool creation and health
- Agent failure and recovery
- Domain goal management
- Tool discovery and registration
- Workflow creation and execution
- Three-pillar integration
- End-to-end scenarios

### Integration Tests (9)
- Agent-to-workflow integration
- Domain-tool integration
- Agent consensus for priorities
- Complete system workflow
- Failure scenarios with recovery
- Byzantine fault tolerance
- Multi-step workflow dependencies

### Fault Tolerance Tests (10)
- Agent heartbeat and recovery
- Health degradation path (Healthy → Degraded → Failed)
- Failed agent isolation
- Workflow step failure handling
- Recovery through retry
- Consensus resilience with failures
- Byzantine attack resilience
- Self-healing agent pool
- Cascading failure prevention
- Partial failure system continuity

### Consensus Tests (10)
- Majority voting
- Supermajority voting
- Unanimous voting
- Consensus blocking
- Quorum requirements
- Domain prioritization
- Goal completion verification
- Byzantine agreement safety
- Workflow order refinement
- Multi-round consensus

## System Workflow

### 1. Initialization
```rust
let pool = AgentPool::new(6);  // Create 6 agents
```

### 2. Domain Setup
```rust
let mut manager = DomainManager::new();
let goal = DomainGoal::new("Goal".to_string(), LifeDomain::Health, 10);
manager.add_goal(goal);
```

### 3. Tool Discovery
```rust
let tool_mgr = initialize_standard_tools();
let tools = tool_mgr.available_tools();  // Discover MCP tools
```

### 4. Workflow Planning
```rust
let mut workflow = Workflow::new("plan".to_string());
// Add steps based on discovered tools and domain goals
```

### 5. Consensus Decision
```rust
let consensus = ConsensusManager::default_supermajority(6);
let result = consensus.reach_consensus("proposal".to_string(), votes);
```

### 6. Execution with Fault Tolerance
```rust
let success = WorkflowExecutor::execute_workflow(&mut workflow).await;
```

### 7. Result Analysis
```rust
let progress = workflow.progress_percentage();
let summary = pool.health_summary();
```

## Performance Characteristics

- Agent creation: < 1ms
- Health check cycle: < 1s
- Consensus round: < 2s
- Workflow execution: Depends on tool execution time
- System resilience: Tolerates f failures in 3f+1 agents

## Key Invariants

1. **System Consistency**: All agents eventually reach consensus
2. **Fault Tolerance**: System continues despite agent failures
3. **Liveness**: Workflow always makes progress or explicitly fails
4. **Safety**: No invalid state transitions possible
5. **Domain Balance**: Agents maintain balance across all domains

## Architecture Diagram

```
┌─────────────────────────────────────────────────┐
│         End-to-End Agent Workflow               │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────┐                               │
│  │ Agent Pool   │ ← Health tracking & recovery  │
│  │   (6 agents) │                               │
│  └──────────────┘                               │
│        ↓                                         │
│  ┌──────────────────────┐                       │
│  │ Domain Manager       │ ← Goal tracking       │
│  │ (6 life domains)     │   Domain balance     │
│  └──────────────────────┘                       │
│        ↓                                         │
│  ┌──────────────────────┐                       │
│  │ Tool Manager         │ ← MCP discovery      │
│  │ (search, store, etc) │   Tool execution     │
│  └──────────────────────┘                       │
│        ↓                                         │
│  ┌──────────────────────┐                       │
│  │ Workflow             │ ← Plan generation    │
│  │ (dependency DAG)     │   Step execution     │
│  └──────────────────────┘                       │
│        ↓                                         │
│  ┌──────────────────────┐                       │
│  │ Consensus Manager    │ ← Voting             │
│  │ (Byzantine FT)       │   Agreement          │
│  └──────────────────────┘                       │
│                                                 │
└─────────────────────────────────────────────────┘

Pillars Demonstrated:
┌─────────────┬──────────────────┬─────────────────┐
│   Fault     │   Distributed    │   Autonomous    │
│ Tolerance   │  Coordination    │   Reasoning     │
├─────────────┼──────────────────┼─────────────────┤
│ Health      │ Consensus        │ Tool Discovery  │
│ Tracking    │ Voting           │ Plan Generation │
│ Recovery    │ Byzantine FT     │ Execution       │
│ Isolation   │ Agreement        │ Learning        │
└─────────────┴──────────────────┴─────────────────┘
```

## Joe Armstrong AGI Level Demonstration

This example demonstrates "Joe Armstrong AGI level" systems through:

1. **Fault Tolerance**: Like Erlang's supervisor trees, agents monitor each other
2. **Concurrency**: Multiple agents operate independently with coordination
3. **Distribution**: Byzantine consensus ensures agreement despite failures
4. **Reasoning**: Agents discover and plan tool usage autonomously
5. **Learning**: System improves decisions based on outcomes
6. **Resilience**: Cascading failures prevented through careful design

## References

- OSIRIS: Zero cognitive load architecture
- A2A: Agent-to-agent task state machine protocol
- MCP: Model Context Protocol for tool integration
- Byzantine Fault Tolerance: Practical consensus in distributed systems
- Domain-Driven Design: Goal-based life management
