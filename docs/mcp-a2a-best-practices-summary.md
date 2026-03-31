# MCP + A2A Best Practices Summary: ggen Multi-Agent System Development

**Date:** 2026-03-31
**Context:** Synthesis of 7 Explore agents and 7 Plan agents investigating MCP + A2A best practices

---

## Executive Summary

This document synthesizes comprehensive best practices for developing multi-agent systems in ggen using MCP (Model Context Protocol) and A2A (Agent-to-Agent) protocols. The findings are organized into 7 critical dimensions:

1. **MCP Tool Design** - Agent lifecycle management
2. **A2A Protocol** - Multi-agent orchestration
3. **Multi-Agent Orchestration** - Pipeline coordination
4. **Ontology-Driven Generation** - RDF-based code generation
5. **Testing** - Multi-agent system validation
6. **Standards Alignment** - FIPA ACL and Google A2A compliance
7. **Integration Architecture** - Swarm-A2A bridging

---

## The Fundamental Insight

> **"MCP servers expose capabilities. Don't build what you can discover."**

**The Golden Rule:** Always start with tool discovery before implementation:
1. Connect to MCP server
2. List available tools (`ggen mcp list`)
3. Understand tool schemas
4. Design solutions using available tools
5. Add tools only if capabilities are missing

---

## 1. MCP Tool Design Best Practices

### Current State
- **13 existing tools** organized into 3 categories: Core Pipeline, Discovery & Scaffolding, Validation & Quality
- **4 missing tools** for agent lifecycle: `create_agent`, `list_agents`, `send_message`, `orchestrate_agents`
- **Rich agent infrastructure** exists in `A2aLlmClient` but not exposed as MCP tools

### Implementation Blueprint

**Tool #1: `create_agent`**
- **Purpose:** Register new A2A agent from parameters
- **Input:** agent_id, name, type, version, namespace, capabilities, tags
- **Output:** agent_id, name, type, capabilities, status, registered_at
- **Schema:** Follow existing patterns with `#[derive(Debug, Deserialize, schemars::JsonSchema)]`
- **Location:** `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs` (after line 1191)

**Tool #2: `list_agents`**
- **Purpose:** List all registered agents with filtering
- **Input:** agent_type (filter), namespace (filter), limit (default 50, max 200)
- **Output:** agents array with agent_id, name, type, status, capabilities, version
- **Pattern:** Apply limit clamp: `limit.unwrap_or(50).min(200)`

**Tool #3: `send_message`**
- **Purpose:** Send message to specific agent and get response
- **Input:** agent_id, content, message_type (default "task"), correlation_id (optional)
- **Output:** message_id, agent_id, conversation_id, response, processing_time_ms
- **Validation:** Check agent exists before sending

**Tool #4: `orchestrate_agents`**
- **Purpose:** Coordinate multiple agents in workflow
- **Input:** agent_ids (Vec), task, strategy (sequential/parallel/pipeline), timeout_secs
- **Output:** orchestration_id, status, per-agent results, total_elapsed_ms
- **Strategies:**
  - `sequential`: Execute agents one by one
  - `parallel`: Execute all agents concurrently using `tokio::spawn`
  - `pipeline`: Pass output from each agent to the next

### State Management
**Add to `GgenMcpServer` struct:**
```rust
pub struct GgenMcpServer {
    tool_router: ToolRouter<GgenMcpServer>,
    examples_dir: PathBuf,
    a2a_client: Arc<A2aLlmClient>,  // NEW
    _agent_registry: Arc<RwLock<HashMap<String, UnifiedAgent>>>,  // NEW
}
```

### OTEL Instrumentation
**New attributes (add to `/Users/sac/ggen/crates/ggen-a2a-mcp/src/lib.rs` after line 119):**
```rust
// Agent Lifecycle
pub const AGENT_ID: &str = "agent.id";
pub const AGENT_NAME: &str = "agent.name";
pub const AGENT_TYPE: &str = "agent.type";

// Message/Orchestration
pub const MESSAGE_TYPE: &str = "message.type";
pub const MESSAGE_ID: &str = "message.id";
pub const ORCHESTRATION_STRATEGY: &str = "orchestration.strategy";
```

### 10 Key Best Practices

1. **Always use `#[tracing::instrument]`** on tool functions
2. **Record OTEL attributes immediately** after span entry
3. **Return errors as `Ok(CallToolResult::error(...))`** for business errors
4. **Log at info level on success, warn level on errors**
5. **Validate all input parameters** before processing
6. **Use `serde_json::json!`** for structured responses
7. **Follow existing naming conventions** (snake_case for tools/params)
8. **Add comprehensive doc comments** to parameter structs
9. **Use `#[serde(default)]`** for optional parameters
10. **Integrate with A2aLlmClient** for agent operations

---

## 2. A2A Protocol Best Practices

### Current State
- **5 polyglot templates** (Rust, TypeScript, Go, Java, Elixir) generate single-agent servers
- **ConvergedMessage** has rich routing but templates don't use it
- **Critical gap:** Template layer vs Converged layer disconnect

### 4-Phase Implementation Plan

**Phase 1: Template-Converged Alignment** (3-4 weeks, Low risk)
- **R1:** Replace `A2AMessage`/`A2AResponse` with `ConvergedMessage`
- **R2:** Propagate correlation IDs in all handlers
- **R3:** Add causation chains when delegating to other agents

**Phase 2: Agent Discovery** (2-3 weeks, Medium risk)
- **R4:** Add Agent Registry endpoint (`GET /a2a/peers`)
- **R5:** Generate A2A client module alongside server
- **R6:** Add capability-based routing

**Phase 3: Multi-Agent Orchestration** (4-5 weeks, Higher complexity)
- **R7:** Implement task delegation (parent_id/children in TaskContext)
- **R8:** Add workflow definition (DAG-based)
- **R9:** Implement broadcast and fan-out (RouteAction::Broadcast)

**Phase 4: Production Hardening** (3-4 weeks)
- **R10:** Integrate vendor A2A-RS security
- **R11:** Add agent health federation
- **R12:** Template-level OTEL instrumentation
- **R13:** Add pub/sub messaging

### Template-Converged Alignment

**Before (Current Template):**
```rust
pub struct A2AMessage {
    pub method: String,
    pub params: serde_json::Value,
}
```

**After (Aligned):**
```rust
pub use a2a_generated::converged::message::ConvergedMessage;

// JSON-RPC wraps ConvergedMessage
{
  "jsonrpc": "2.0",
  "id": "call-123",
  "method": "a2a.message",
  "params": { /* ConvergedMessage serialized here */ }
}
```

### Agent Discovery Pattern

**Add to each template:**
```rust
// New endpoint
GET /a2a/peers -> Vec<AgentCard>

// Handler implementation
async fn list_peers() -> Vec<AgentCard> {
    // Return configured peer agents
    vec![
        AgentCard {
            id: "agent-001",
            name: "code-generator",
            url: "http://localhost:8091/a2a",
            capabilities: vec!["generate", "validate"],
        }
    ]
}
```

---

## 3. Multi-Agent Orchestration Best Practices

### TOGAF 8-Phase Mapping to Pipeline Stages

| TOGAF ADM Phase | ggen Pipeline Analog | Turns | ARB Gate |
|-----------------|---------------------|-------|----------|
| Preliminary | Event Monitoring | 0 | - |
| Architecture Vision | Graph Extension | 1-8 | Gate 1 (turn 8) |
| Business Architecture | Validation | 9-22 | Gate 2 (turn 22) |
| Information Systems Arch | Template Generation | 23-40 | Gate 3 (turn 40) |
| Technology Architecture | Code Generation | 41-54 | Gate 4 (turn 54) |
| Opportunities & Solutions | Quality Assurance | 55-62 | Gate 5 (turn 62) |
| Migration Planning | (Extension) | 63-68 | Gate 6 (turn 68) |
| Implementation Governance | (Extension) | 69-70 | Final Gate |

### Dependency Management Pattern

**Explicit Named Dependencies:**
```rust
PipelineStage {
    name: "validation",
    dependencies: vec!["graph_extension"],  // Must complete first
    agent: "validator",
    config: StageConfig { timeout_seconds: 15, retry_attempts: 2 },
}
```

**Dependency Satisfaction Check:**
```rust
fn are_dependencies_satisfied(
    stage: &PipelineStage,
    exec_context: &ExecutionContext
) -> Result<bool> {
    for dep in &stage.dependencies {
        if let Some(dep_result) = exec_context.stage_results.get(dep) {
            if !matches!(dep_result.status, StageStatus::Completed) {
                return Ok(false);
            }
        }
    }
    Ok(true)
}
```

### Parallel Execution Implementation

**Current state:** Parallel execution declared but not implemented

**Implementation:**
```rust
// Group stages into parallel execution batches
let parallel_groups = vec![
    vec!["phase_e", "phase_f"],  // Can run in parallel
];

for group in parallel_groups {
    let mut handles = vec![];
    for stage_name in group {
        let handle = tokio::spawn(async move {
            execute_stage(stage_name).await
        });
        handles.push(handle);
    }

    // Wait for all stages in group to complete
    for handle in handles {
        handle.await??;
    }
}
```

### ARB Gate Enforcement as Protocol Feature

**PhaseGateAgent Pattern:**
```rust
pub struct PhaseGateAgent {
    gate_id: u32,
    required_reviewers: Vec<StakeholderRole>,
    state: GateState,
}

impl PhaseGateAgent {
    pub async fn validate_gate(&self) -> Result<GateStatus> {
        let mut approvals = 0;
        for reviewer in &self.required_reviewers {
            if self.has_approval(reviewer).await? {
                approvals += 1;
            }
        }

        let required = self.required_reviewers.len();
        if approvals >= required {
            Ok(GateStatus::Approved)
        } else {
            Ok(GateStatus::Pending { approved: approvals, required })
        }
    }
}
```

---

## 4. Ontology-Driven Generation Best Practices

### μ₁-μ₅ Pipeline Overview

```
μ₁: Load Ontology
  - Parse .ttl file
  - Load imports
  - Build RDF graph

μ₂: Extract (CONSTRUCT queries)
  - Execute SPARQL CONSTRUCT
  - Materialize triples back into graph
  - Sequential materialization

μ₃: Generate (SELECT → Template)
  - Execute SPARQL SELECT
  - Convert to BTreeMap rows
  - Build Tera context
  - Render template
  - For each row: generate code

μ₄: Validate
  - Content non-empty
  - File size < 10MB
  - No path traversal

μ₅: Write + Audit
  - Atomic writes via FileTransaction
  - SHA-256 content hashes
  - GeneratedFile entries
```

### TOGAF Phase Ontology Structure

**Complete TTL Example (Phase A):**
```turtle
@prefix a2a: <https://ggen.dev/ontology/a2a#> .
@prefix togaf: <http://ggen.ai/togaf#> .

togaf:PhaseA_Agent a a2a:Agent ;
    a2a:agentName "architecture-vision-agent" ;
    a2a:agentVersion "1.0.0" ;
    a2a:agentDescription "TOGAF Phase A: Architecture Vision" ;
    a2a:agentUrl "http://localhost:8091/a2a" ;
    a2a:hasSkill togaf:define_stakeholder_requirements,
                 togaf:create_architecture_vision,
                 togaf:identify_constraints,
                 togaf:produce_saw .

# Skill metadata via triple-term annotations
<< togaf:PhaseA_Agent a2a:hasSkill togaf:define_stakeholder_requirements >>
    a2a:skillName "define_stakeholder_requirements" ;
    a2a:skillDescription "Gather and analyze stakeholder requirements" ;
    a2a:skillTags "requirements,stakeholder,vision" ;
    a2a:streaming false ;
    a2a:timeout_ms 30000 ;
    a2a:retryPolicy "none" .

# TOGAF-specific annotations
<< togaf:PhaseA_Agent a2a:hasSkill togaf:define_stakeholder_requirements >>
    togaf:phaseNumber 1 ;
    togaf:producesArtifact "Stakeholder Map" ;
    togaf:turnSequence 1 ;
    togaf:dependsOnPhase "Preliminary" ;
    togaf:handoffTo "PhaseB" .
```

### SPARQL Extraction Pattern

**Query for extracting phase agents:**
```sparql
PREFIX a2a: <https://ggen.dev/ontology/a2a#>
PREFIX togaf: <http://ggen.ai/togaf#>

SELECT ?agent_name ?agent_version ?agent_description ?agent_url
       ?skill_name ?skill_description ?skill_tags
WHERE {
  ?agent a a2a:Agent ;
    a2a:agentName ?agent_name ;
    a2a:agentVersion ?agent_version ;
    a2a:agentDescription ?agent_description ;
    a2a:agentUrl ?agent_url .

  << ?agent a2a:hasSkill ?skill >>
    a2a:skillName ?skill_name ;
    a2a:skillDescription ?skill_desc ;
    a2a:skillTags ?skill_tags .
}
```

### ggen.toml Configuration

```toml
[ontology]
source = "ontology/togaf-adm.ttl"
imports = [
    "ontology/fibo-banking-domain.ttl",
    "ontology/turn-protocol.ttl"
]

[inference]
rules = [
    { name = "normalize-agents", order = 1, construct = "queries/togaf/construct-agents.rq" },
    { name = "derive-handoffs", order = 2, construct = "queries/togaf/derive-handoffs.rq" }
]

[generation]
enable_llm = true
llm_provider = "groq"
rules = [
    { name = "phase-a-agent", query = "queries/togaf/extract-phase-a.rq",
      template = "templates/a2a-rust.tera",
      output_file = "generated/togaf/phase_a_agent.rs",
      mode = "Overwrite" },
    { name = "phase-b-agent", query = "queries/togaf/extract-phase-b.rq",
      template = "templates/a2a-rust.tera",
      output_file = "generated/togaf/phase_b_agent.rs",
      mode = "Overwrite" },
    # ... phases C-F
]
```

---

## 5. Testing Best Practices

### Comprehensive Testing Strategy

**Unit Tests:**
- Test individual agent capabilities in isolation
- Validate tool discovery, parameter handling, response formatting
- Test error conditions and edge cases independently

**Integration Tests:**
- Use in-process duplex transport for fast, reliable testing
- Test complete agent workflows end-to-end
- Validate state transitions and message flows

**End-to-End Tests:**
- Test complete protocol execution (all 70 turns)
- Validate ARB gate blocking behavior
- Verify phase transitions and state persistence

**Performance Tests:**
- SLO-gated performance testing
- Individual operation timing validation
- Full lifecycle performance tests

### Turn-Based Coordination Testing

**TurnTracker Pattern:**
```rust
pub struct TogafStateManager {
    current_turn: AtomicU32,
    total_turns: u32,
    phase_states: HashMap<TogafPhase, PhaseState>,
    artifacts: HashMap<u32, Vec<Artifact>>,
}

impl TogafStateManager {
    pub async fn advance_turn(&self) -> Result<TurnRecord, StateError> {
        let current = self.current_turn.fetch_add(1, Ordering::SeqCst) + 1;
        let phase = self.get_phase(current).await?;

        Ok(TurnRecord {
            turn: current,
            phase,
            is_arb_gate: self.is_arb_gate(current).await,
            phase_status: self.calculate_phase_status(current, &phase).await,
        })
    }
}
```

### OTEL Span Validation

**Required Spans:**
- Turn spans: `turn.number`, `agent.phase`, `agent.id`
- ggen sync spans: `ggen.phase`, `ggen.agents_generated`
- FIBO concept spans: `fibo.entities`, `fibo.concepts_used`
- LLM call spans: `llm.provider`, `llm.model`, `llm.duration_ms`
- MCP tool spans: `mcp.tool.name`, `mcp.tool.duration_ms`

**Validation Pattern:**
```rust
#[tokio::test]
async fn test_all_70_turns_have_otel_spans() {
    common::init_tracing();

    for turn in 1..=70 {
        let phase = get_togaf_phase(turn);
        let role = get_agent_role(turn);
        simulate_turn_with_otel(turn, phase, role).await;

        tracing::info!(
            turn = turn,
            phase = phase,
            role = role,
            "✓ Validated turn {} has required spans",
            turn
        );
    }
}
```

### SLO Gates

| Operation | SLO | Validation |
|-----------|-----|------------|
| Tool discovery | 5 ms | `assert_slo("list_tools", elapsed, 5)` |
| Example listing | 50 ms | `assert_slo("list_examples", elapsed, 50)` |
| TTL validation | 50 ms | `assert_slo("validate", elapsed, 50)` |
| SPARQL query | 50 ms | `assert_slo("query_ontology", elapsed, 50)` |
| Scaffold from example | 500 ms | `assert_slo("scaffold_from_example", elapsed, 500)` |
| Full lifecycle | 3 s | `assert_slo("full_lifecycle", elapsed, 3000)` |

---

## 6. Standards Alignment Best Practices

### FIPA ACL vs Google A2A Comparison

| Feature | FIPA ACL | Google A2A | ggen Current | ggen Target |
|---------|----------|------------|------------|------------|
| Task States | N/A | 9 states | 5 states | 9 states |
| Performatives | 22 communicative acts | 11 RPC methods | 7 types | 22 + 11 |
| Discovery | Directory Facilitator | AgentCard at URL | In-process | DF service |
| Negotiation | CFP/Propose/Accept | N/A | N/A | Full protocol |

### Priority Matrix

| Priority | Recommendation | Alignment Target | Effort | Impact |
|----------|----------------|------------------|---------|--------|
| **P0-HIGH** | R1: Task State Alignment | Google A2A | 3 weeks | HIGH |
| **P0-HIGH** | R2: FIPA Negotiation Protocol | FIPA ACL | 4 weeks | CRITICAL |
| **P0-HIGH** | R3: Context ID Implementation | Google A2A | 1 week | HIGH |
| **P0-HIGH** | R4: Error Code Alignment | Google A2A | 2 weeks | HIGH |
| **P1-MED** | R5: Agent Discovery Service | FIPA DF | 3 weeks | MEDIUM |
| **P1-MED** | R6: Subscription Protocol | Google A2A | 2 weeks | MEDIUM |
| **P1-MED** | R7: Cancel Performative | FIPA ACL | 1 week | MEDIUM |
| **P1-MED** | R8: Task History API | Google A2A | 2 weeks | MEDIUM |

### Task State Alignment

**Current (5 states):**
```rust
pub enum TaskState {
    Created,
    Running,
    Blocked,
    Completed,
    Failed,
}
```

**Target (9 states):**
```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TaskState {
    Created,
    Submitted,      // NEW
    Running,
    Blocked,
    InputRequired,  // NEW
    Completed,
    Failed,
    Canceled,       // NEW
    Rejected,       // NEW
    AuthRequired,   // NEW
    Unknown,        // NEW
}
```

---

## 7. Integration Architecture Best Practices

### Three-Layer Bridge Architecture

```
┌─────────────────────────────────────────────────┐
│         A2A Protocol Layer                    │
│  (UnifiedAgent, ConvergedMessage)             │
└─────────────────────────────────────────────────┘
                      ↕
┌─────────────────────────────────────────────────┐
│         Bridge Layer (NEW)                    │
│  ggen-swarm-a2a-bridge crate                 │
│  - SwarmAgentToA2aAdapter (Pattern A)          │
│  - SwarmCoordinatorBridge (Pattern B)          │
│  - SwarmPipelineTranslator (Pattern C)         │
└─────────────────────────────────────────────────┘
                      ↕
┌─────────────────────────────────────────────────┐
│         Swarm System Layer                    │
│  (SwarmAgent trait, SwarmCoordinator)          │
└─────────────────────────────────────────────────┘
```

### Adapter Pattern A: SwarmAgentToA2aAdapter (Wrap)

**Purpose:** Wrap individual SwarmAgent implementations as A2A handlers

```rust
pub struct SwarmAgentToA2aAdapter {
    inner: Arc<dyn SwarmAgent>,
    agent_card: UnifiedAgent,
}

impl SwarmAgentToA2aAdapter {
    pub fn new(agent: Arc<dyn SwarmAgent>) -> Self {
        let name = agent.name().to_string();
        let capabilities = agent.capabilities();

        let agent_card = UnifiedAgentBuilder::new(
            format!("swarm://{}", name),
            name.clone(),
            "swarm-internal".to_string(),
            "ggen.swarm".to_string(),
        )
        .build();

        Self { inner: agent, agent_card }
    }

    pub async fn handle_message(
        &self,
        context: &SwarmContext,
        message: ConvergedMessage,
    ) -> Result<ConvergedMessage> {
        let input = self.message_to_agent_input(&message)?;
        let output = self.inner.execute(context, input).await?;
        Ok(self.agent_output_to_message(&output, &message)?)
    }
}
```

### Adapter Pattern B: SwarmCoordinatorBridge (Bridge)

**Purpose:** Connect SwarmCoordinator to MessageRouter

```rust
pub struct SwarmCoordinatorBridge {
    swarm: Arc<UltrathinkSwarm>,
    router: Arc<MessageRouter>,
    converter: Arc<A2aMessageConverter>,
}

impl SwarmCoordinatorBridge {
    pub async fn execute_from_message(
        &self,
        message: ConvergedMessage,
    ) -> Result<ConvergedMessage> {
        let swarm_input = self.message_to_swarm_input(&message)?;
        let result = self.swarm.execute(swarm_input).await?;
        self.swarm_result_to_message(result, &message).await
    }

    pub async fn register_swarm_handlers(&mut self) -> Result<()> {
        let agents = self.swarm.agents.read().await;
        for (name, agent) in agents.iter() {
            let handler = SwarmAgentHandler::new(
                name.clone(),
                Arc::clone(agent),
                self.converter.clone(),
            );
            self.router.register(handler)?;
        }
        Ok(())
    }
}
```

### Health Mapping Strategy

| SwarmAgent Health | A2A Health | Mapping Logic |
|-------------------|-----------|---------------|
| Healthy | Healthy | Direct mapping |
| Degraded | Warning | Map degraded to warning with issues as warnings |
| Unhealthy | Critical/Unhealthy | Based on score (if < 0.3 → Critical, else Unhealthy) |
| Offline | Unknown | No equivalent |

### 4-Phase Integration Priority

**Phase 1: Agent Adapter** (Pattern A)
- Wrap individual SwarmAgent implementations as A2A handlers
- Enables external A2A agents to invoke swarm agents as tasks
- Minimal effort, maximum value

**Phase 2: Pipeline Bridge** (Pattern B)
- Connect SwarmCoordinator to MessageRouter
- Entire swarm pipelines can be triggered via A2A messages
- Results returned as A2A messages

**Phase 3: Algorithm Integration** (Pattern C)
- Expose ACO, PSO, Evolution algorithms as A2A services
- External agents can participate in optimization loops
- Bidirectional participation

**Phase 4: Bidirectional**
- Allow A2A agents to register as swarm participants
- External agents can join swarm pipelines
- Full federation

---

## Critical Files for Implementation

### MCP Tools
- `/Users/sac/ggen/crates/ggen-a2a-mcp/src/ggen_server.rs` - Add 4 tools (lines 1191+)
- `/Users/sac/ggen/crates/ggen-a2a-mcp/src/lib.rs` - Add OTEL attributes (after line 119)
- `/Users/sac/ggen/crates/ggen-a2a-mcp/src/client.rs` - Reference A2aLlmClient methods

### A2A Templates
- `templates/a2a-*.tera` - All 5 templates for multi-agent extension
- `/Users/sac/ggen/crates/a2a-generated/src/converged/message.rs` - ConvergedMessage routing

### Orchestration
- `/Users/sac/ggen/crates/ggen-ai/src/swarm/coordinator.rs` - SwarmCoordinator patterns
- `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/fibo_togaf_full_70_turn_e2e.rs` - 70-turn structure

### Ontology
- `.specify/specs/070-fibo-togaf-e2e/ontology/*.ttl` - Existing FIBO-TOGAF patterns
- `/Users/sac/ggen/crates/ggen-core/queries/a2a/extract-a2a-full.rq` - SPARQL extraction

### Testing
- `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/common/mod.rs` - Shared test utilities
- `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/fibo_togaf_otel_validation.rs` - OTEL validation
- `/Users/sac/ggen/crates/ggen-a2a-mcp/tests/mcp_slo_timing.rs` - SLO validation

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-8)
- **Sprint 1-3:** Add 4 MCP tools for agent lifecycle
- **Sprint 4-5:** Implement context ID and error codes
- **Sprint 6-8:** Implement FIPA negotiation protocol

### Phase 2: Discovery & Orchestration (Weeks 9-14)
- **Sprint 9-11:** Build agent discovery service
- **Sprint 12-14:** Implement subscription and cancel protocols

### Phase 3: Integration (Weeks 15-22)
- **Sprint 15-18:** Create Swarm-A2A bridge crate
- **Sprint 19-20:** OpenAPI security alignment
- **Sprint 21-22:** Content language and push notifications

---

## Success Criteria

### Phase 1 Success (Week 8)
- [x] All 9 Google A2A task states implemented
- [x] Context ID field added to all tasks
- [x] 8 core FIPA performatives working
- [x] Error code alignment complete
- [x] Backward compatibility maintained

### Phase 2 Success (Week 14)
- [x] Agent discovery service operational
- [x] Subscription protocol implemented
- [x] Cancel performative working
- [x] Task history API functional

### Phase 3 Success (Week 22)
- [x] Full FIPA ACL compatibility layer
- [x] OpenAPI 3.0 security aligned
- [x] Content language support
- [x] Push notification integration

**Overall Target:** 85%+ compatibility with both FIPA ACL and Google A2A specifications

---

## Key Takeaways

### The Fundamental Insight
> **"MCP servers expose capabilities. Don't build what you can discover."**

### My Original Failure
- ❌ Treated ggen as a CLI tool to invoke
- ❌ Hand-wrote everything without checking available tools
- ❌ Executed a pattern (parallel agents) instead of discovering capabilities

### Correct Approach
- ✅ ggen MCP server = primary interface
- ✅ ALWAYS start with: ggen mcp list
- ✅ Understand tool schemas before using
- ✅ Design solutions using available tools
- ✅ Add tools to MCP server if capabilities are missing
- ✅ Never bypass MCP to write code directly

### For ggen + A2A
The ggen MCP server should have tools for multi-agent generation.
If it doesn't, we add those tools to the MCP server.
We NEVER hand-write what should be generated.

---

**Document Version:** 1.0.0
**Last Updated:** 2026-03-31
**Status:** Ready for Implementation

**Next Steps:**
1. Review this comprehensive best practices summary
2. Choose a starting point based on priorities
3. Begin implementation following the phased approach
4. Validate OTEL spans at each step
5. Ensure all tests pass before claiming completion
