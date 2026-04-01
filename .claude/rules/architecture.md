---
auto_load: false
category: reference
priority: normal
version: 6.0.1
---

# Architecture Reference (LSP-Surveyed)

Full public API surface derived from LSP `documentSymbol` sweep of all workspace crates.

Use LSP for navigation -- this file is orientation, not a substitute for `LSP workspaceSymbol`.

## Crate Map by Domain

### Core Pipeline (mu_1-mu_5)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-core` | Graph-aware code generation engine (49 mods) | `Graph`, `GgenOntology`, `Pipeline`, `PipelineBuilder`, `GenContext`, `Generator`, `DeltaType`, `GraphDelta`, `DriftDetector`, `ThreeWayMerger`, `RegionAwareMerger`, `SnapshotManager`, `TemplateResolver`, `RegistryClient`, `PqcSigner` |
| `ggen-cli` | CLI via clap-noun-verb auto-discovery | `RunResult`, `cli_match()`, `run_for_node()` |
| `ggen-domain` | Pure business logic, zero CLI deps (23+ mods) | `ConfigAuditor`, `SecurityScanner`, re-exports `ggen_utils::error::{Error, Result}` |
| `ggen-config` | ggen.toml parsing and validation | `GgenConfig`, `AiConfig`, `McpConfig`, `A2AConfig`, `ProjectConfig`, `TemplatesConfig`, `ConfigLoader`, `ConfigValidator` |
| `ggen-config-clap` | Bridge ggen.toml to clap apps | `LoadConfigFromGgenToml` trait, `load_ggen_config()`, `expand_env_vars()` |
| `ggen-utils` | Shared infra (13 mods) | `Error`, `Result`, `bail!`, `ensure!`, `SafePath`, `SafeCommand`, `CommandArg`, `CommandName` |
| `ggen-cli-validation` | IO validation and security for CLI ops | `IoValidator`, `NounVerbValidator`, `PermissionModel`, `PathValidation`, `AuditEntry` |
| `ggen-macros` | Proc macros for guards, bundles, compile-time includes | `#[derive(Guard)]`, `#[derive(Bundle)]`, `include_ontology!()`, `include_templates!()` |

### AI / LLM Layer

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-ai` | Multi-provider LLM integration (17 mods) | `GenAiClient`, `LlmClient`, `LlmResponse`, `LlmConfig`, `LlmCache`, `AiConfig`, `ToolRegistry`, `REGISTRY`, `Tool`, `ToolTag`, `SparqlValidator`, re-exports `genai` crate |
| `ggen-dspy` | DSPy-inspired LLM agent framework | `Module`, `Predictor`, `ReAct`, `Retrieve`, `ChainOfThought`, `ProgramOfThought`, `BaleenBuilder`, `MiproOptimizer`, `BootstrapFewShot`, `GgenAiAdapter`, `A2aPredictor` |

### Ontology / RDF

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-ontology-core` | RDF/TTL, SPARQL, entity mapping | `TripleStore`, `EntityMapper`, `SparqlGenerator`, `OntologyMatch`, `ValidationReport`, `validate_ontology()` |
| `ggen-prompt-mfg` | Prompt compilation via SPARQL CONSTRUCT | `PromptCompiler`, `CompiledPrompt`, `PromptIR`, `Section`, `ContentBlock`, `PromptEmitter`, `compute_prompt_hash()` |
| `ggen-codegen` | Generic codegen framework (trait-based) | `Queryable` trait, `Renderable` trait, `Rule<Q,T>`, `GeneratedFile`, `GenerationMode` |
| `ggen-canonical` | Deterministic canonicalization + hash verification | `Canonicalizer` trait, `Canonical<T>`, `JsonCanonicalizer`, `RustCanonicalizer`, `TtlCanonicalizer`, `compute_hash()` |
| `ggen-receipt` | Cryptographic receipts (Ed25519) | `Receipt`, `ReceiptChain`, `generate_keypair()`, `hash_data()`, `sign()`, `verify()` |
| `ggen-yawl` | YAWL workflow gen from industry ontologies | `YawlGenerator`, `YawlXmlGenerator`, `ConstructExecutor`, `OntologyLoader` |
| `ggen-craftplan` | RDF to Elixir codegen (5-stage mu pipeline) | `CodeGenerator`, `generate_from_rdf()`, `with_receipts()` |
| `ggen-process-mining` | Process mining (Alpha++, XES/OCEL, PetriNet) | `ProcessMiner`, `AlphaPlusPlus`, `PetriNet`, `EventLog`, `ConformanceChecker`, `XesParser`, `OcelParser` |

### TPS (Toyota Production System)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-jidoka` | Quality gates and andon signal system | `Gate` trait, `CompilerGate`, `TestGate`, `LintGate`, `SHACLGate`, `ProductionLine`, `SignalMonitor`, `AndonSignal` |
| `ggen-tps-andon` | Production andon: visibility, alerting, diagnostics | `AndonSystem`, `AndonLogger`, `AndonMetrics`, `AndonTracer`, `AndonObserver`, `AlertManager`, `SignalColor` |
| `ggen-packet` | Work order types with routing and validation | `WorkOrder`, `WorkOrderId`, `Constraint`, `AcceptanceTest`, `ReversibilityPolicy`, `PriorityRouter`, `validate_work_order()` |
| `ggen-backpressure` | Admission control (lambda <= mu), WIP tokens, rate limiting | `AdmissionController` trait, `KanbanBoard`, `KanbanConfig`, `RateLimiter`, `WIPToken`, `TokenPool` |
| `ggen-heijunka` | Load leveling and smooth workload distribution | `LoadLeveler`, `WorkBuffer`, `LoadMetrics`, `ThroughputTracker`, `Scheduler`, `SchedulingStrategy` |
| `ggen-kaizen` | Continuous improvement with PDCA cycles | `PdcaCycle`, `PdcaPhase`, `Improvement`, `ImprovementHistory`, `MetricsCalculator`, `TrendAnalysis` |
| `tps-reference` | Self-contained TPS reference (all 6 principles) | `TpsSystem`, `TpsConfig`, `SupervisionTree`, `CircuitBreaker`, `KanbanQueue`, `KaizenMetrics`, `HeijunkaPool` |
| `ggen-cli-tps` | CLI commands for A2A/TPS operations | `Cli` (clap Parser), `Commands` enum (Receipt, Jidoka, Packet, Backpressure, Supplier, A2a, Firewall) |
| `ggen-metrics-tps` | Queueing-theory metrics (lambda, rho, L, W) via Prometheus | `MetricsCollector`, `Counters`, `Histograms`, `Gauges`, `Exporter` |

### A2A Protocol (Agent-to-Agent)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-a2a` | Task state machine protocol | `Task`, `TaskState`, `TaskStateMachine`, `Artifact`, `ArtifactCollection`, `Transport` (in-process), `StateTransition`, `TaskMessage` |
| `a2a-generated` | A2A protocol types generated from RDF ontology | `Agent`, `AgentFactory`, `Message`, `Port`, `Task`, `UnifiedAgent`, `UnifiedAgentBuilder`, `MessageRouter`, `SecurityPolicy` |
| `ggen-a2a-mcp` | Bridge A2A to RMCP (Rusty Model Context Protocol) | `AgentToToolAdapter`, `ToolToAgentAdapter`, `MultipartHandler`, `TextContentHandler`, `MessageRouter`, `TaskMapper`, `YawlBridge`, `otel_attrs` (~50 constants) |
| `ggen-a2a-registry` | Multi-agent orchestration registry | `AgentRegistry`, `AgentEntry`, `AgentStore` trait, `HealthMonitor`, `HealthStatus`, `AgentQuery` |
| `ggen-transport` | MCP/A2A transport with session management + streaming | `A2aTransport`, `A2aMessage`, `SessionManager`, `StreamBuilder`, `MessageStream`, `StreamSender`, `OriginValidator`, `Transport` trait |
| `ggen-integration` | Integration layer connecting all A2A/TPS systems | `SystemCoordinator`, `Pipeline`, `HealthCheck`, `LifecycleManager`, `PipelineResult` |

### Consensus / Security

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-consensus` | Byzantine consensus (PBFT) for receipt verification | `PbftConsensus`, `ConsensusProtocol` trait, `VoteCollector`, `QuorumCalculator`, `SignatureAggregator`, `MultiSignature`, `Phase` |
| `ggen-poka-yoke` | Compile-time error prevention via type-level guarantees | `ErrorProofing` trait, `ValidByConstruction` trait, `StateTransition` trait, `NonEmpty<T>`, `Positive<T>`, `Bounded<MIN,MAX>`, `StateMachine<S>` |
| `ggen-dod` | Definition of Done: autonomous substrate | `Kernel`, `Contract`, `Decision`, `DecisionStore`, `Invariant`, `InvariantChecker`, `Observation`, `Receipt`, `TenantContext`, `MAPEKLoop`, `TimingEnforcer` |

### RevOps (Revenue Operations)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-api` | REST API layer (axum) for monetization | `init_api()`, `AppState`, `ApiConfig`, `ApiError` |
| `ggen-auth` | OAuth2, JWT (HS256+RS256), API keys, RBAC | `Rs256JwtManager`, `OAuthConfig`, `ApiKeyManager`, `PasswordHasher`, `Role`, `PolicyEngine`, `AuthorizationContext`, `Permission` bitflags |
| `ggen-payments` | Stripe integration | `StripeClient`, `Payment`, `Invoice`, `Subscription`, `WebhookEvent` |
| `ggen-saas` | SaaS tier management, quotas | `Tier` (Free/Pro/Enterprise), `TierLimits`, `QuotaManager`, `BillingCycle`, `TierHierarchy` |
| `ggen-marketplace` | Marketplace with RDF store, SPARQL, Ed25519 | `Package`, `PackageId`, `Manifest`, `QualityScore`, `AsyncRepository` trait, `Queryable` trait, `Installable` trait, `RdfRegistry`, `SparqlSearchEngine` |
| `ggen-marketplace-tps` | GCP Marketplace entitlements, CLM proxy | `MarketplaceClient`, `OAuth2Manager`, `Entitlements`, `ClmProxy` |

### KNHK Systems Integration

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `knhk-etl` | `#![no_std]` ETL pipeline (Ingest/Transform/Load/Reflex/Emit) | `Pipeline`, `IngestStage`, `TransformStage`, `LoadStage`, `ReflexStage`, `EmitStage`, `SoAArrays` (align64) |
| `knhk-connectors` | `#![no_std]` connector framework with circuit breakers | `Connector` trait, `ConnectorRegistry`, `CircuitBreaker`, `SourceType`, `Delta`, `Triple` |
| `knhk-lockchain` | `#![no_std]` Merkle-linked receipt storage | `Lockchain`, `LockchainEntry`, `MerkleNode`, `ReceiptHash` |
| `knhk-otel` | `#![no_std]`/std dual-mode OpenTelemetry | `Tracer`, `Span`, `Metric`, `SpanContext`, `OtlpExporter`, `WeaverLiveCheck`, `MetricsHelper` |
| `knhk-orchestrator` | ETL to KGC-4D to Workflow bridge | `Orchestrator`, `EventBus`, `TemporalContext`, `CausalityChain`, `OrchestratorConfig` |
| `knhk-hot` | C FFI hot-path wrapper | `knhk_hot_version()`, `knhk_hot_init()`, `knhk_hot_cleanup()` |

### Testing Infrastructure

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-testing` | Chicago TDD test harness | `TestHarness`, `StateVerifier<T>`, `TempFsFixture`, `InMemoryStoreFixture`, 16 `assert_*` fns, `SnapshotTest`, `PropertyTest<S>` |
| `ggen-test-audit` | Mutation testing, assertion analysis, false positive detection | `AssertionAnalyzer`, `MutationAnalyzer`, `FalsePositiveDetector`, `QualityReport`, `ReportGenerator` |
| `ggen-test-opt` | Test value scoring, Pareto selection | `TestValueScorer`, `ParetoSelector`, `MetadataCollector`, `TestValueScore`, `ScoringWeights` |
| `ggen-e2e` | Cross-platform E2E testing | `TestRunner`, `TestFixture`, `GoldenFile`, `Platform`, `ContainerConfig` |
| `tai-testing` | Production hardening: chaos, load, compliance | `ChaosExperiment`, `LoadTest`, `ComplianceFramework`, `StateInvariant` |
| `tai-validation` | SLO/SLA validation, multi-framework compliance | `SloValidator`, `ComplianceResult` (FISMA, FedRAMP, SOC2, HIPAA, NIST 800-53), `ShaclValidator`, `EvidenceCollector` |

### Domain / Research

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-folk-strategy` | Quantification of 67 folk strategy terms | `FolkTerm` (67 variants), `OpportunityField`, `Trajectory`, `CompetitorDynamics`, `compute_folk_terms()` |
| `ggen-dod` | DfLSS autonomous substrate | `Kernel`, `MAPEKLoop`, `Contract`, `DecisionStore`, `InvariantChecker`, `TimingEnforcer` |
| `ggen-execution` | Common execution framework for 90% semantic convergence | `SemanticConvergenceEngine`, `EnhancedPipelineExecutor`, `TaskOrchestrator`, `RecoveryManager`, `HealthChecker` |
| `ggen-workflow-43` | van der Aalst's 43 workflow patterns | `WorkflowEngine`, `Activity` trait, `WorkflowPattern` trait, `ActivityContext` |
| `ggen-node` | Node.js N-API bindings (napi) | `run()`, `version()`, 21 `#[napi]` fns (marketplace, lifecycle, template, AI) |

## Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| `pub type Result<T> = std::result::Result<T, CrateError>` | Every crate | Each crate has its own error enum via `thiserror` |
| Builder pattern | marketplace, yawl, craftplan, integration, cli-validation, k8s, transport | `with_*()` chain methods for optional config |
| Typestate | ggen-marketplace (`Draft`/`Published`), ggen-poka-yoke (`StateMachine<S>`) | Compile-time state transitions |
| Newtype wrappers | marketplace (`PackageId`, `PackageVersion`, `QualityScore`), packet (`WorkOrderId`), backpressure (`WIPToken`) | Invariant encoding in types |
| Async traits | a2a (`A2aMessageHandler`), registry (`AgentStore`), auth (`SessionManager`, `RateLimiter`, `LockoutManager`), workflow-43 (`Activity`) | `#[async_trait]` with `Result` returns |
| RDF/SPARQL foundation | ontology-core, marketplace, yawl, craftplan, prompt-mfg | All built on `oxigraph` triplestores |
| Pipeline architecture | core (mu_1-mu_5), yawl, craftplan, process-mining, knhk-etl, execution | Multi-stage deterministic transformation |
| Prometheus metrics | tps-andon, metrics-tps | Counters, gauges, histograms with `prometheus` crate |
| `#![no_std]` | knhk-etl, knhk-connectors, knhk-lockchain, knhk-otel | Embedded-capable with `alloc` feature gate |

## Key Trait Index

| Trait | Crate | Purpose |
|-------|-------|---------|
| `Queryable` | ggen-codegen | SPARQL query execution to `Vec<HashMap<String,String>>` |
| `Renderable` | ggen-codegen | Template rendering with bindings to `String` |
| `Canonicalizer` | ggen-canonical | Deterministic canonicalization with hash |
| `Gate` / `Signal` | ggen-jidoka | Quality gate checks (compiler, test, lint, SHACL) |
| `AdmissionController` | ggen-backpressure | WIP token acquisition, utilization tracking |
| `ConsensusProtocol` | ggen-consensus | PBFT propose/process/commit |
| `Connector` | knhk-connectors | Data source connector (Kafka, HTTP, File, Salesforce) |
| `AgentStore` | ggen-a2a-registry | Agent persistence contract |
| `Transport` / `StreamingTransport` | ggen-transport | Message passing with session + streaming |
| `A2aMessageHandler` | ggen-transport | Handle A2A messages and streams |
| `Activity` / `WorkflowPattern` | ggen-workflow-43 | Workflow execution with cancellation |
| `ErrorProofing` / `ValidByConstruction` | ggen-poka-yoke | Compile-time error prevention |
| `AsyncRepository` | ggen-marketplace | Package CRUD operations |
| `LoadConfigFromGgenToml` | ggen-config-clap | Config loading bridge |
| `SessionManager` / `RateLimiter` / `LockoutManager` | ggen-auth | Redis-backed auth infrastructure |
