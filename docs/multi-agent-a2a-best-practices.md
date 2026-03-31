# Multi-Agent A2A Best Practices for ggen

**Date:** 2026-03-31
**Context:** Comprehensive analysis of a2a-rs, ggen patterns, and FIBO-TOGAF architecture

---

## Executive Summary

Based on exploration of a2a-rs library, ggen's current A2A infrastructure, and the FIBO-TOGAF test structure, this document establishes best practices for building multi-agent A2A orchestration in ggen.

### Key Findings

1. **a2a-rs has NO built-in agent registry** - provides building blocks, not coordination
2. **ggen has A2aLlmClient with HashMap-based registry** - but no discovery protocol
3. **FIBO-TOGAF defines 6-phase 70-turn protocol** - sophisticated multi-agent collaboration
4. **Best approach**: Hybrid using a2a-generated types + a2a-rs patterns + custom registry

---

## Architecture Overview

### The Hybrid Approach

```
┌─────────────────────────────────────────────────────────────┐
│                    ggen Multi-Agent A2A                    │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Agent Registry (NEW)                                │   │
│  │  - Register agents on startup                       │   │
│  │  - Discover by capability/type                       │   │
│  │  - Health monitoring with heartbeats                 │   │
│  │  - Uses a2a-generated UnifiedAgent types             │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  6 TOGAF Phase Agents (Generated)                    │   │
│  │  - Phase A: Architecture Vision (Turns 1-8)          │   │
│  │  - Phase B: Business Architecture (Turns 9-22)       │   │
│  │  - Phase C: Data Architecture (Turns 23-40)          │   │
│  │  - Phase D: Technology Architecture (Turns 41-54)    │   │
│  │  - Phase E: Opportunities & Solutions (Turns 55-62)  │   │
│  │  - Phase F: Migration Planning (Turns 63-70)         │   │
│  │                                                           │
│  │  Each agent:                                            │
│  │  - Uses a2a-rs HttpServer/WebSocketServer             │   │
│  │  - Implements AsyncMessageHandler trait               │   │
│  │  - Has TOML config + Rust handler                     │   │
│  │  - Tracks state in TogafStateManager                  │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  MCP Tools (ggen-a2a-mcp)                           │   │
│  │  - generate_agents: Generate from ontology            │   │
│  │  - generate_a2a_test: Generate self-play tests        │   │
│  │  - orchestrate_conversation: Multi-turn coordination  │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Best Practice 1: Agent Registry

### Use a2a-generated Types as Core Model

```rust
use a2a_generated::converged::agent::UnifiedAgent;

pub struct AgentRegistry {
    store: Arc<dyn AgentStore>,
    health_monitor: Arc<HealthMonitor>,
}

impl AgentRegistry {
    pub async fn register_agent(&self, agent: UnifiedAgent) -> Result<Registration> {
        // Validate agent capabilities
        // Store in backend
        // Start health monitoring
    }

    pub async fn discover_agents(&self, query: AgentQuery) -> Result<Vec<AgentEntry>> {
        // Search by capability, type, health status
    }
}
```

### Why a2a-generated Over a2a-rs?

| Criterion | a2a-generated | a2a-rs |
|-----------|---------------|---------|
| **Integration** | Already in ggen codebase | External dependency |
| **Type Safety** | Generated from RDF ontology | Manual types |
| **Compatibility** | Works with A2aLlmClient | Requires adapters |
| **Philosophy** | Ontology-driven | Library-driven |

### Storage Abstraction

```rust
#[async_trait]
pub trait AgentStore: Send + Sync {
    async fn register(&self, agent: UnifiedAgent) -> Result<()>;
    async fn get(&self, id: &str) -> Result<Option<UnifiedAgent>>;
    async fn list(&self) -> Result<Vec<UnifiedAgent>>;
    async fn update_health(&self, id: &str, status: HealthStatus) -> Result<()>;
}

// Implementations:
// - MemoryStore (dev/test)
// - SqlxStore (production)
```

### Health Monitoring Protocol

```rust
pub struct HealthMonitor {
    check_interval: Duration,
    timeout: Duration,
    unhealthy_threshold: u32,
}

impl HealthMonitor {
    pub async fn start_monitoring(&self, registry: Arc<AgentRegistry>) {
        let mut interval = tokio::time::interval(self.check_interval);

        while let Ok(agent) = registry.next_agent().await {
            let health = Self::ping_agent(&agent.endpoint).await;
            registry.update_health(agent.id, health).await;
        }
    }
}
```

---

## Best Practice 2: Agent Definition (TOML + Handler)

### Use Declarative TOML Configuration

```toml
# agents/phase-a.toml
[agent]
name = "Architecture Vision Agent"
description = "TOGAF Phase A: Architecture Vision"
version = "1.0.0"

[[skills]]
id = "togaf:PhaseA_Skill"
name = "Architecture Vision"
description = "Gather stakeholder requirements, define vision, establish scope"

[[skills.fibo_concepts]]
concept = "LegalPerson"
mapping = "Stakeholder → FIBO Legal Person"

[[skills.fibo_concepts]]
concept = "Organization"
mapping = "Organization structure → FIBO Organization"

[skills.input]
type = "TogafPhaseA_Input"
required_fields = ["business_requirements", "stakeholders", "constraints"]

[skills.output]
type = "TogafPhaseA_Output"
artifacts = ["StakeholderMap", "ArchitectureVision", "StatementOfWork"]

[skills.state]
phase = "A"
turns = [1, 2, 3, 4, 5, 6, 7, 8]
arb_approval = 8
arb_reviewer = "ChiefArchitect"
```

### Handler Implementation Pattern

```rust
use a2a_rs::port::AsyncMessageHandler;

pub struct TogafPhaseHandler {
    phase: TogafPhase,
    state: Arc<RwLock<TogafState>>,
    artifact_store: Arc<ArtifactStore>,
    fibo_validator: Arc<FiboValidator>,
}

#[async_trait]
impl AsyncMessageHandler for TogafPhaseHandler {
    async fn process_message<'a>(
        &self,
        task_id: &'a str,
        message: &'a Message,
        session_id: Option<&'a str>,
    ) -> Result<Task, A2AError> {
        // 1. Parse message for turn number
        // 2. Check current state
        // 3. Process turn logic
        // 4. Validate FIBO concepts
        // 5. Store artifacts
        // 6. Update state
        // 7. Check if ARB approval needed
        // 8. Return task with result
    }
}
```

### Builder Pattern for Agent Creation

```rust
use a2a_agents::AgentBuilder;

pub async fn create_phase_a_agent() -> Result<A2AAgent> {
    AgentBuilder::from_file("agents/phase-a.toml")?
        .with_handler(PhaseAHandler::new()?)
        .with_storage(SqlxTaskStorage::new("sqlite:phase_a.db").await?)
        .with_http_server("127.0.0.1:3030".to_string())
        .with_websocket_server("127.0.0.1:3031".to_string())
        .build()
        .await
}
```

---

## Best Practice 3: State Management

### Centralized State Manager

```rust
pub struct TogafStateManager {
    phases: Arc<RwLock<HashMap<TogafPhase, PhaseState>>>,
    current_turn: Arc<AtomicUsize>,
    artifacts: Arc<RwLock<ArtifactRegistry>>,
    fibo_cache: Arc<RwLock<FiboValidationCache>>,
}

impl TogafStateManager {
    pub async fn advance_turn(&self) -> Result<TurnRecord> {
        let current = self.current_turn.load(Ordering::SeqCst);
        let phase = self.get_phase_for_turn(current).await?;

        // Check if turn is valid for current phase
        if !phase.valid_turns.contains(&current) {
            return Err(TogafError::InvalidTurn(current, phase.phase));
        }

        // Check ARB approval if needed
        if self.is_arb_gate(current).await? {
            self.await_arb_approval(current).await?;
        }

        // Update state
        self.current_turn.fetch_add(1, Ordering::SeqCst);

        Ok(TurnRecord {
            turn_number: current,
            phase: phase.phase.clone(),
            timestamp: Utc::now(),
        })
    }

    pub async fn store_artifact(&self, artifact: Artifact) -> Result<()> {
        let mut artifacts = self.artifacts.write().await;
        artifacts.insert(artifact.id.clone(), artifact);
        Ok(())
    }
}
```

### Phase-Specific State

```rust
pub struct PhaseState {
    pub phase: TogafPhase,
    pub current_turn: usize,
    pub completed_turns: HashSet<usize>,
    pub artifacts: Vec<Artifact>,
    pub fibo_validations: Vec<FiboValidationResult>,
    pub status: PhaseStatus,
}

pub enum PhaseStatus {
    NotStarted,
    InProgress,
    ArbPending,
    ArbApproved,
    Completed,
}
```

---

## Best Practice 4: Handoff Protocol

### Formal Phase Handoff

```rust
pub struct HandoffProtocol {
    validators: Vec<Box<dyn HandoffValidator>>,
}

#[async_trait]
pub trait HandoffValidator: Send + Sync {
    async fn validate(&self, source: &PhaseState, target: &TogafPhase) -> Result<ValidationResult>;
}

pub struct FiboConsistencyValidator;
pub struct ArtifactCompletenessValidator;
pub struct ArbApprovalValidator;

impl HandoffProtocol {
    pub async fn execute_handoff(
        &self,
        source_phase: &PhaseState,
        target_phase: TogafPhase,
    ) -> Result<HandoffResult> {
        // 1. Run all validators
        for validator in &self.validators {
            let result = validator.validate(source_phase, &target_phase).await?;
            if !result.is_valid() {
                return Err(HandoffError::ValidationFailed(result));
            }
        }

        // 2. Prepare handoff package
        let package = HandoffPackage {
            artifacts: source_phase.artifacts.clone(),
            fibo_mappings: self.extract_fibo_mappings(source_phase).await?,
            state_summary: self.summarize_state(source_phase).await?,
        };

        // 3. Send to target phase
        self.send_handoff_message(target_phase, package).await?;

        // 4. Wait for acceptance
        self.await_acceptance(target_phase).await?;

        Ok(HandoffResult::Accepted)
    }
}
```

### Handoff Message Format

```rust
use a2a_generated::converged::message::ConvergedMessage;

pub fn create_handoff_message(
    source_phase: TogafPhase,
    target_phase: TogafPhase,
    package: HandoffPackage,
) -> ConvergedMessage {
    ConvergedMessage {
        message_id: format!("handoff-{}-{}", source_phase, target_phase),
        source: format!("phase-{}", source_phase),
        target: Some(format!("phase-{}", target_phase)),
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            correlation_id: Some(Uuid::new_v4().to_string()),
            priority: MessagePriority::High,
            ..Default::default()
        },
        payload: ConvergedPayload {
            content: UnifiedContent::Data {
                data: serde_json::to_value(package).unwrap(),
                schema: Some("HandoffPackage".to_string()),
            },
        },
    }
}
```

---

## Best Practice 5: ARB Review Gates

### Automated Stakeholder Approval

```rust
pub struct ArbGate {
    pub turn_number: usize,
    pub required_reviewers: Vec<StakeholderRole>,
    pub approval_criteria: Vec<ApprovalCriterion>,
}

pub enum StakeholderRole {
    ChiefArchitect,
    ExecutiveSponsor,
    ComplianceOfficer,
    BusinessOwner,
}

pub struct ArbApproval {
    pub gate: ArbGate,
    pub approvals: HashMap<StakeholderRole, Approval>,
    pub status: ApprovalStatus,
}

impl ArbApproval {
    pub async fn request_approval(&mut self) -> Result<()> {
        // Send approval requests to all required reviewers
        for reviewer in &self.gate.required_reviewers {
            self.send_approval_request(reviewer).await?;
        }

        // Wait for all approvals
        while !self.is_fully_approved() {
            tokio::time::sleep(Duration::from_secs(5)).await;
            self.check_timeouts().await?;
        }

        Ok(())
    }

    pub fn is_fully_approved(&self) -> bool {
        self.gate.required_reviewers.iter().all(|role| {
            self.approvals.get(role)
                .map(|a| a.status == ApprovalStatus::Approved)
                .unwrap_or(false)
        })
    }
}
```

### ARB Gates by Phase

| Gate | Turn | Reviewers | Criteria |
|------|------|-----------|----------|
| Phase A Complete | 8 | ChiefArchitect | Vision complete, FIBO mapped |
| Phase B Complete | 22 | ChiefArchitect, ComplianceOfficer | Capabilities defined, regulatory mapped |
| Phase C Complete | 40 | ChiefArchitect, ComplianceOfficer | Data entities FIBO-aligned |
| Phase D Complete | 54 | ChiefArchitect | Technology architecture defined |
| Phase E Complete | 62 | ComplianceOfficer | Solutions FIBO-compliant |
| Phase F Complete | 70 | All reviewers | Migration plan complete |

---

## Best Practice 6: MCP Tool Integration

### Tool 1: generate_agents

```rust
#[mcp_tool]
pub async fn generate_agents(
    ontology_path: String,
    output_dir: String,
    framework: AgentFramework,
) -> Result<GenerateAgentsResult> {
    // 1. Load ontology
    let ontology = load_ontology(&ontology_path).await?;

    // 2. Extract agent definitions via SPARQL
    let agents = extract_agents(&ontology).await?;

    // 3. Generate TOML configs
    let toml_files = generate_toml_configs(&agents, &output_dir).await?;

    // 4. Generate Rust handlers
    let rust_files = generate_rust_handlers(&agents, &output_dir).await?;

    // 5. Generate bootstrap code
    let bootstrap = generate_bootstrap(&agents, &output_dir).await?;

    Ok(GenerateAgentsResult {
        toml_configs: toml_files,
        handler_files: rust_files,
        bootstrap_file: bootstrap,
    })
}
```

### Tool 2: orchestrate_conversation

```rust
#[mcp_tool]
pub async fn orchestrate_conversation(
    agents: Vec<String>,
    total_turns: usize,
    arb_gates: Vec<usize>,
    mode: OrchestrationMode,
) -> Result<ConversationResult> {
    let orchestrator = ConversationOrchestrator::new(
        agents,
        total_turns,
        arb_gates,
        mode,
    ).await?;

    orchestrator.run().await
}
```

### Tool 3: generate_a2a_test

```rust
#[mcp_tool]
pub async fn generate_a2a_test(
    agent_definitions: Vec<AgentDefinition>,
    total_turns: usize,
    scenario: TestScenario,
) -> Result<String> {
    // Generate test code using template
    let template = Template::from_file("self-play-test.rs.tera")?;
    let test_code = template.render(context)?;

    Ok(test_code)
}
```

---

## Best Practice 7: A2A Communication Patterns

### Message Routing with Correlation

```rust
pub struct MessageRouter {
    registry: Arc<AgentRegistry>,
    correlation_tracker: Arc<CorrelationTracker>,
}

impl MessageRouter {
    pub async fn route_message(&self, message: ConvergedMessage) -> Result<()> {
        // 1. Record correlation
        self.correlation_tracker.record(&message).await?;

        // 2. Find target agent
        let target = self.registry
            .get_agent(&message.target.as_ref().unwrap())?;

        // 3. Send via a2a-rs HttpClient
        let client = HttpClient::new(target.endpoint)?;
        let task = client.send_task_message(
            &message.message_id,
            &to_a2a_message(&message),
            None,
            None,
        ).await?;

        Ok(())
    }
}
```

### Causation Chain Tracking

```rust
pub struct CausationChain {
    chain: Vec<CausationEvent>,
}

impl CausationChain {
    pub fn record(&mut self, event: CausationEvent) {
        self.chain.push(event);
    }

    pub fn to_otel_spans(&self) -> Vec<Span> {
        // Convert causation chain to OTEL spans
        // with parent-child relationships
    }
}
```

---

## Best Practice 8: FIBO Integration

### Continuous Semantic Validation

```rust
pub struct FiboValidator {
    ontology: Oxigraph,
    shacl_shapes: Vec<Shape>,
}

impl FiboValidator {
    pub async fn validate_artifact(&self, artifact: &Artifact) -> FiboValidationResult {
        // 1. Extract FIBO concepts from artifact
        let concepts = self.extract_fibo_concepts(artifact).await?;

        // 2. Validate against SHACL shapes
        let report = self.shacl_validate(&concepts).await?;

        // 3. Check semantic consistency
        let consistency = self.check_semantic_consistency(&concepts).await?;

        FiboValidationResult {
            is_valid: report.conforms && consistency.is_consistent,
            violations: report.violations,
            warnings: consistency.warnings,
        }
    }
}
```

### FIBO Concept Mapping

```rust
pub struct FiboConceptMapper {
    mappings: HashMap<String, FiboClass>,
}

impl FiboConceptMapper {
    pub fn map_architecture_element(&self, element: &str) -> Option<FiboClass> {
        self.mappings.get(element).cloned()
    }

    pub fn validate_mapping(&self, source: &str, target: &FiboClass) -> bool {
        // Check if mapping is valid according to FIBO ontology
    }
}
```

---

## Implementation Roadmap

### Phase 1: Core Registry (Week 1)
- [ ] Create `ggen-a2a-registry` crate
- [ ] Implement `AgentStore` trait
- [ ] Implement `MemoryStore`
- [ ] Basic CRUD operations
- [ ] Health monitoring infrastructure

### Phase 2: Agent Generation (Week 2)
- [ ] Create TOML templates for 6 TOGAF phases
- [ ] Create handler templates
- [ ] Implement `generate_agents` MCP tool
- [ ] Test agent generation from ontology

### Phase 3: State Management (Week 3)
- [ ] Implement `TogafStateManager`
- [ ] Turn progression logic
- [ ] Artifact storage
- [ ] FIBO validation cache

### Phase 4: Handoff Protocol (Week 4)
- [ ] Implement `HandoffProtocol`
- [ ] Validators (FIBO, completeness, ARB)
- [ ] Handoff message routing
- [ ] Acceptance verification

### Phase 5: ARB Gates (Week 5)
- [ ] Implement `ArbGate` system
- [ ] Stakeholder approval workflow
- [ ] Gate definitions for all phases
- [ ] Timeout and escalation

### Phase 6: MCP Integration (Week 6)
- [ ] Implement `orchestrate_conversation` tool
- [ ] Implement `generate_a2a_test` tool
- [ ] OTEL integration for all operations
- [ ] Documentation and examples

---

## Testing Strategy

### Unit Tests
- Registry CRUD operations
- State progression logic
- Handoff validation
- FIBO concept mapping

### Integration Tests
- Agent registration and discovery
- Multi-phase handoffs
- ARB gate approvals
- OTEL span correlation

### E2E Tests
- Full 70-turn FIBO-TOGAF scenario
- All 6 phases collaboration
- FIBO validation at each turn
- Final code generation

### Property-Based Tests
- State machine transitions
- Message routing properties
- FIBO mapping invariants

---

## Critical Files Reference

### Must Read (Existing)
1. `vendors/a2a-rs/a2a-agents/src/reimbursement_agent/` - Agent patterns
2. `crates/a2a-generated/src/converged/agent.rs` - Core types
3. `crates/ggen-a2a-mcp/src/client.rs` - A2aLlmClient patterns
4. `crates/ggen-ai/src/swarm/orchestration.rs` - Swarm patterns
5. `.specify/specs/070-fibo-togaf-e2e/ontology/` - Ontology definitions

### Must Create (New)
6. `crates/ggen-a2a-registry/src/lib.rs` - Registry API
7. `crates/ggen-a2a-registry/src/store.rs` - Storage trait
8. `crates/ggen-a2a-registry/src/health.rs` - Health monitoring
9. `crates/ggen-core/templates/agents/` - Agent TOML templates
10. `crates/ggen-core/templates/handlers/` - Handler code templates
11. `crates/ggen-core/src/state/togaf_state.rs` - State manager

---

## Summary

### Key Principles

1. **Use a2a-generated types** for core data model (already integrated)
2. **Adopt a2a-rs patterns** for transport and storage (proven patterns)
3. **Declarative TOML configs** for agent definitions (human-editable)
4. **Centralized state management** for 70-turn protocol (consistency)
5. **Formal handoff protocol** between phases (validation)
6. **Automated ARB gates** for governance (compliance)
7. **MCP-first tool integration** for LLM access (ergonomics)
8. **Continuous FIBO validation** for semantic consistency (quality)

### Architecture Benefits

✅ **Type Safe**: Generated from RDF ontology
✅ **Ergonomic**: TOML configs + Builder API
✅ **Observable**: OTEL spans throughout
✅ **Scalable**: Trait-based storage abstraction
✅ **Maintainable**: Clear separation of concerns
✅ **Compliant**: ARB gates + FIBO validation
✅ **Traceable**: Correlation IDs + causation chains

### Next Steps

1. Review and validate this design with ggen maintainers
2. Create `ggen-a2a-registry` crate skeleton
3. Implement Phase 1 (Core Registry)
4. Write tests with TOGAF scenarios
5. Iterate based on feedback

---

**Document Version:** 1.0
**Last Updated:** 2026-03-31
**Status:** Ready for Review
