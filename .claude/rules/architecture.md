---
auto_load: false
category: reference
priority: low
version: 6.0.1
---

# Architecture Reference

Pure reference. No doctrine. No rules. Data only.

## Crate Map by Domain

### Core Pipeline (mu_1 -- mu_5)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-core | Graph-aware code generation engine | Graph, Pipeline, GenContext, Generator, DriftDetector, ThreeWayMerger |
| ggen-cli | CLI via clap-noun-verb | RunResult, cli_match() |
| ggen-domain | Pure business logic, zero CLI deps | ConfigAuditor, SecurityScanner |
| ggen-config | ggen.toml parsing | GgenConfig, ConfigLoader |
| ggen-config-clap | Bridge ggen.toml to clap | LoadConfigFromGgenToml |
| ggen-utils | Shared infra | Error, Result, bail!, ensure! |
| ggen-cli-validation | IO validation | IoValidator, PermissionModel |
| ggen-macros | Proc macros | #[derive(Guard)], #[derive(Bundle)] |

### AI / LLM Layer

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-ai | Multi-provider LLM integration | GenAiClient, LlmClient, ToolRegistry |
| ggen-dspy | DSPy-inspired agent framework | Module, Predictor, ReAct, BaleenBuilder |

### Ontology / RDF

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-ontology-core | RDF/TTL, SPARQL | TripleStore, EntityMapper, validate_ontology() |
| ggen-prompt-mfg | Prompt compilation | PromptCompiler, PromptIR |
| ggen-codegen | Generic codegen framework | Queryable trait, Renderable trait, Rule |
| ggen-canonical | Canonicalization + hash | Canonicalizer trait, compute_hash() |
| ggen-receipt | Cryptographic receipts | Receipt, ReceiptChain, sign(), verify() |
| ggen-yawl | YAWL workflow gen | YawlGenerator |
| ggen-craftplan | RDF to Elixir codegen | CodeGenerator |
| ggen-process-mining | Process mining | ProcessMiner, AlphaPlusPlus, PetriNet |

### TPS (Toyota Production System)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-jidoka | Quality gates | Gate trait, ProductionLine |
| ggen-tps-andon | Andon system | AndonSystem, AlertManager |
| ggen-packet | Work orders | WorkOrder, PriorityRouter |
| ggen-backpressure | Admission control | AdmissionController trait, KanbanBoard |
| ggen-heijunka | Load leveling | LoadLeveler, Scheduler |
| ggen-kaizen | Continuous improvement | PdcaCycle, MetricsCalculator |
| ggen-cli-tps | CLI commands | Commands enum |
| ggen-metrics-tps | Prometheus metrics | MetricsCollector |

### A2A Protocol

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-a2a | Task state machine | Task, TaskStateMachine |
| a2a-generated | A2A types from RDF | Agent, MessageRouter |
| ggen-a2a-mcp | Bridge A2A to MCP | AgentToToolAdapter |
| ggen-a2a-registry | Multi-agent registry | AgentRegistry, AgentStore |
| ggen-transport | Transport + streaming | A2aTransport, SessionManager |
| ggen-integration | Integration layer | SystemCoordinator, Pipeline |

### Consensus / Security

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-consensus | Byzantine consensus | PbftConsensus, VoteCollector |
| ggen-poka-yoke | Compile-time error prevention | ErrorProofing trait, StateMachine |
| ggen-dod | Definition of Done | Kernel, Contract, InvariantChecker |

### RevOps

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-api | REST API (axum) | init_api(), AppState |
| ggen-auth | OAuth2, JWT, RBAC | Rs256JwtManager, PolicyEngine |
| ggen-payments | Stripe | StripeClient, Payment |
| ggen-saas | SaaS tiers | Tier, QuotaManager |
| ggen-marketplace | Marketplace | Package, RdfRegistry |
| ggen-marketplace-tps | GCP Marketplace | MarketplaceClient |

### KNHK Systems Integration

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| knhk-etl | no_std ETL pipeline | Pipeline, SoAArrays |
| knhk-connectors | no_std connectors | Connector trait, CircuitBreaker |
| knhk-lockchain | no_std Merkle receipts | Lockchain |
| knhk-otel | no_std OTEL | Tracer, Span |
| knhk-orchestrator | ETL bridge | Orchestrator |
| knhk-hot | C FFI hot-path | knhk_hot_init() |

### Testing Infrastructure

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| ggen-testing | Chicago TDD harness | TestHarness, StateVerifier |
| ggen-test-audit | Mutation testing | AssertionAnalyzer, MutationAnalyzer |
| ggen-e2e | Cross-platform E2E | TestRunner, GoldenFile |
| tai-testing | Chaos, load, compliance | ChaosExperiment |
| tai-validation | SLO/SLA validation | SloValidator, ShaclValidator |

## Cross-Cutting Patterns

| Pattern | Where Used |
|---------|-----------|
| pub type Result<T> = std::result::Result<T, CrateError> | Every crate |
| Builder pattern (with_* chain) | marketplace, yawl, transport, cli-validation |
| Typestate (Draft/Published) | marketplace, poka-yoke |
| Newtype wrappers | PackageId, WorkOrderId, WIPToken |
| Async traits (#[async_trait]) | a2a, registry, auth, workflow-43 |
| RDF/SPARQL foundation | ontology-core, marketplace, prompt-mfg |
| Pipeline architecture | core, yawl, craftplan, process-mining |
| #![no_std] | knhk-etl, knhk-connectors, knhk-lockchain, knhk-otel |

## Key Trait Index

| Trait | Crate | Purpose |
|-------|-------|---------|
| Queryable | ggen-codegen | SPARQL query execution |
| Renderable | ggen-codegen | Template rendering |
| Canonicalizer | ggen-canonical | Deterministic canonicalization |
| Gate / Signal | ggen-jidoka | Quality gate checks |
| AdmissionController | ggen-backpressure | WIP token acquisition |
| ConsensusProtocol | ggen-consensus | PBFT propose/process/commit |
| Connector | knhk-connectors | Data source connector |
| AgentStore | ggen-a2a-registry | Agent persistence |
| Transport | ggen-transport | Message passing |
| A2aMessageHandler | ggen-transport | Message handling |
| Activity | ggen-workflow-43 | Workflow execution |
| ErrorProofing | ggen-poka-yoke | Compile-time error prevention |
| AsyncRepository | ggen-marketplace | Package CRUD |
| LoadConfigFromGgenToml | ggen-config-clap | Config loading |
| SessionManager | ggen-auth | Redis-backed auth |
