# ggen v6.0.1 - Rust Code Generation CLI

Specification-driven code generation from RDF ontologies. Formula: A = μ(O) - Code precipitates from RDF via five-stage pipeline (μ₁-μ₅).
Stack: Rust 1.91.1 | Tokio | Oxigraph | Tera | Serde | Clap | Chicago TDD ONLY | 30 crates | 87% test coverage
**Compressed Architecture:** `docs/architecture/COMPRESSED_REFERENCE.md` — verified C4, real sync flow, stub registry, error map. Load this before modifying any code.

## Rules (see .claude/rules/ for details)

---

## Architecture Reference (LSP-Surveyed)

Full public API surface derived from LSP `documentSymbol` sweep of all workspace crates.

### Crate Map by Domain

#### Core Pipeline (μ₁–μ₅)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-core` | Graph-aware code generation engine (49 mods) | `Graph`, `GgenOntology`, `Pipeline`, `PipelineBuilder`, `GenContext`, `Generator`, `DeltaType`, `GraphDelta`, `DriftDetector`, `ThreeWayMerger`, `RegionAwareMerger`, `SnapshotManager`, `TemplateResolver`, `RegistryClient`, `PqcSigner` |
| `ggen-cli` | CLI via clap-noun-verb auto-discovery | `RunResult`, `cli_match()`, `run_for_node()` |
| `ggen-domain` | Pure business logic, zero CLI deps (23+ mods) | `ConfigAuditor`, `SecurityScanner`, re-exports `ggen_utils::error::{Error, Result}` |
| `ggen-config` | ggen.toml parsing and validation | `GgenConfig`, `AiConfig`, `McpConfig`, `A2AConfig`, `ProjectConfig`, `TemplatesConfig`, `ConfigLoader`, `ConfigValidator` |
| `ggen-config-clap` | Bridge ggen.toml → clap apps | `LoadConfigFromGgenToml` trait, `load_ggen_config()`, `expand_env_vars()` |
| `ggen-utils` | Shared infra (13 mods) | `Error`, `Result`, `bail!`, `ensure!`, `SafePath`, `SafeCommand`, `CommandArg`, `CommandName` |
| `ggen-cli-validation` | IO validation and security for CLI ops | `IoValidator`, `NounVerbValidator`, `PermissionModel`, `PathValidation`, `AuditEntry` |
| `ggen-macros` | Proc macros for guards, bundles, compile-time includes | `#[derive(Guard)]`, `#[derive(Bundle)]`, `include_ontology!()`, `include_templates!()` |

#### AI / LLM Layer

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-ai` | Multi-provider LLM integration (17 mods) | `GenAiClient`, `LlmClient`, `LlmResponse`, `LlmConfig`, `LlmCache`, `AiConfig`, `ToolRegistry`, `REGISTRY`, `Tool`, `ToolTag`, `SparqlValidator`, re-exports `genai` crate |
| `ggen-dspy` | DSPy-inspired LLM agent framework | `Module`, `Predictor`, `ReAct`, `Retrieve`, `ChainOfThought`, `ProgramOfThought`, `BaleenBuilder`, `MiproOptimizer`, `BootstrapFewShot`, `GgenAiAdapter`, `A2aPredictor` |

#### Ontology / RDF

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-ontology-core` | RDF/TTL, SPARQL, entity mapping | `TripleStore`, `EntityMapper`, `SparqlGenerator`, `OntologyMatch`, `ValidationReport`, `validate_ontology()` |
| `ggen-prompt-mfg` | Prompt compilation via SPARQL CONSTRUCT | `PromptCompiler`, `CompiledPrompt`, `PromptIR`, `Section`, `ContentBlock`, `PromptEmitter`, `compute_prompt_hash()` |
| `ggen-codegen` | Generic codegen framework (trait-based) | `Queryable` trait, `Renderable` trait, `Rule<Q,T>`, `GeneratedFile`, `GenerationMode` |
| `ggen-canonical` | Deterministic canonicalization + hash verification | `Canonicalizer` trait, `Canonical<T>`, `JsonCanonicalizer`, `RustCanonicalizer`, `TtlCanonicalizer`, `compute_hash()` |
| `ggen-receipt` | Cryptographic receipts (Ed25519) | `Receipt`, `ReceiptChain`, `generate_keypair()`, `hash_data()`, `sign()`, `verify()` |
| `ggen-yawl` | YAWL workflow gen from industry ontologies | `YawlGenerator`, `YawlXmlGenerator`, `ConstructExecutor`, `OntologyLoader` |
| `ggen-craftplan` | RDF → Elixir codegen (5-stage μ pipeline) | `CodeGenerator`, `generate_from_rdf()`, `with_receipts()` |
| `ggen-process-mining` | Process mining (Alpha++, XES/OCEL, PetriNet) | `ProcessMiner`, `AlphaPlusPlus`, `PetriNet`, `EventLog`, `ConformanceChecker`, `XesParser`, `OcelParser` |

#### TPS (Toyota Production System)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-jidoka` | Quality gates and andon signal system | `Gate` trait, `CompilerGate`, `TestGate`, `LintGate`, `SHACLGate`, `ProductionLine`, `SignalMonitor`, `AndonSignal` |
| `ggen-tps-andon` | Production andon: visibility, alerting, diagnostics | `AndonSystem`, `AndonLogger`, `AndonMetrics`, `AndonTracer`, `AndonObserver`, `AlertManager`, `SignalColor` |
| `ggen-packet` | Work order types with routing and validation | `WorkOrder`, `WorkOrderId`, `Constraint`, `AcceptanceTest`, `ReversibilityPolicy`, `PriorityRouter`, `validate_work_order()` |
| `ggen-backpressure` | Admission control (λ ≤ μ), WIP tokens, rate limiting | `AdmissionController` trait, `KanbanBoard`, `KanbanConfig`, `RateLimiter`, `WIPToken`, `TokenPool` |
| `ggen-heijunka` | Load leveling and smooth workload distribution | `LoadLeveler`, `WorkBuffer`, `LoadMetrics`, `ThroughputTracker`, `Scheduler`, `SchedulingStrategy` |
| `ggen-kaizen` | Continuous improvement with PDCA cycles | `PdcaCycle`, `PdcaPhase`, `Improvement`, `ImprovementHistory`, `MetricsCalculator`, `TrendAnalysis` |
| `tps-reference` | Self-contained TPS reference (all 6 principles) | `TpsSystem`, `TpsConfig`, `SupervisionTree`, `CircuitBreaker`, `KanbanQueue`, `KaizenMetrics`, `HeijunkaPool` |
| `ggen-cli-tps` | CLI commands for A2A/TPS operations | `Cli` (clap Parser), `Commands` enum (Receipt, Jidoka, Packet, Backpressure, Supplier, A2a, Firewall) |
| `ggen-metrics-tps` | Queueing-theory metrics (λ, ρ, L, W) via Prometheus | `MetricsCollector`, `Counters`, `Histograms`, `Gauges`, `Exporter` |

#### A2A Protocol (Agent-to-Agent)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-a2a` | Task state machine protocol | `Task`, `TaskState`, `TaskStateMachine`, `Artifact`, `ArtifactCollection`, `Transport` (in-process), `StateTransition`, `TaskMessage` |
| `a2a-generated` | A2A protocol types generated from RDF ontology | `Agent`, `AgentFactory`, `Message`, `Port`, `Task`, `UnifiedAgent`, `UnifiedAgentBuilder`, `MessageRouter`, `SecurityPolicy` |
| `ggen-a2a-mcp` | Bridge A2A ↔ RMCP (Rusty Model Context Protocol) | `AgentToToolAdapter`, `ToolToAgentAdapter`, `MultipartHandler`, `TextContentHandler`, `MessageRouter`, `TaskMapper`, `YawlBridge`, `otel_attrs` (~50 constants) |
| `ggen-a2a-registry` | Multi-agent orchestration registry | `AgentRegistry`, `AgentEntry`, `AgentStore` trait, `HealthMonitor`, `HealthStatus`, `AgentQuery` |
| `ggen-transport` | MCP/A2A transport with session management + streaming | `A2aTransport`, `A2aMessage`, `SessionManager`, `StreamBuilder`, `MessageStream`, `StreamSender`, `OriginValidator`, `Transport` trait |
| `ggen-integration` | Integration layer connecting all A2A/TPS systems | `SystemCoordinator`, `Pipeline`, `HealthCheck`, `LifecycleManager`, `PipelineResult` |

#### Consensus / Security

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-consensus` | Byzantine consensus (PBFT) for receipt verification | `PbftConsensus`, `ConsensusProtocol` trait, `VoteCollector`, `QuorumCalculator`, `SignatureAggregator`, `MultiSignature`, `Phase` |
| `ggen-poka-yoke` | Compile-time error prevention via type-level guarantees | `ErrorProofing` trait, `ValidByConstruction` trait, `StateTransition` trait, `NonEmpty<T>`, `Positive<T>`, `Bounded<MIN,MAX>`, `StateMachine<S>` |
| `ggen-dod` | Definition of Done: autonomous substrate | `Kernel`, `Contract`, `Decision`, `DecisionStore`, `Invariant`, `InvariantChecker`, `Observation`, `Receipt`, `TenantContext`, `MAPEKLoop`, `TimingEnforcer` |

#### RevOps (Revenue Operations)

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-api` | REST API layer (axum) for monetization | `init_api()`, `AppState`, `ApiConfig`, `ApiError` |
| `ggen-auth` | OAuth2, JWT (HS256+RS256), API keys, RBAC | `Rs256JwtManager`, `OAuthConfig`, `ApiKeyManager`, `PasswordHasher`, `Role`, `PolicyEngine`, `AuthorizationContext`, `Permission` bitflags |
| `ggen-payments` | Stripe integration | `StripeClient`, `Payment`, `Invoice`, `Subscription`, `WebhookEvent` |
| `ggen-saas` | SaaS tier management, quotas | `Tier` (Free/Pro/Enterprise), `TierLimits`, `QuotaManager`, `BillingCycle`, `TierHierarchy` |
| `ggen-marketplace` | Marketplace with RDF store, SPARQL, Ed25519 | `Package`, `PackageId`, `Manifest`, `QualityScore`, `AsyncRepository` trait, `Queryable` trait, `Installable` trait, `RdfRegistry`, `SparqlSearchEngine` |
| `ggen-marketplace-tps` | GCP Marketplace entitlements, CLM proxy | `MarketplaceClient`, `OAuth2Manager`, `Entitlements`, `ClmProxy` |

#### KNHK Systems Integration

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `knhk-etl` | `#![no_std]` ETL pipeline (Ingest→Transform→Load→Reflex→Emit) | `Pipeline`, `IngestStage`, `TransformStage`, `LoadStage`, `ReflexStage`, `EmitStage`, `SoAArrays` (align64) |
| `knhk-connectors` | `#![no_std]` connector framework with circuit breakers | `Connector` trait, `ConnectorRegistry`, `CircuitBreaker`, `SourceType`, `Delta`, `Triple` |
| `knhk-lockchain` | `#![no_std]` Merkle-linked receipt storage | `Lockchain`, `LockchainEntry`, `MerkleNode`, `ReceiptHash` |
| `knhk-otel` | `#![no_std]`/std dual-mode OpenTelemetry | `Tracer`, `Span`, `Metric`, `SpanContext`, `OtlpExporter`, `WeaverLiveCheck`, `MetricsHelper` |
| `knhk-orchestrator` | ETL → KGC-4D → Workflow bridge | `Orchestrator`, `EventBus`, `TemporalContext`, `CausalityChain`, `OrchestratorConfig` |
| `knhk-hot` | C FFI hot-path wrapper | `knhk_hot_version()`, `knhk_hot_init()`, `knhk_hot_cleanup()` |

#### Testing Infrastructure

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-testing` | Chicago TDD test harness | `TestHarness`, `StateVerifier<T>`, `TempFsFixture`, `InMemoryStoreFixture`, 16 `assert_*` fns, `SnapshotTest`, `PropertyTest<S>` |
| `ggen-test-audit` | Mutation testing, assertion analysis, false positive detection | `AssertionAnalyzer`, `MutationAnalyzer`, `FalsePositiveDetector`, `QualityReport`, `ReportGenerator` |
| `ggen-test-opt` | Test value scoring, Pareto selection | `TestValueScorer`, `ParetoSelector`, `MetadataCollector`, `TestValueScore`, `ScoringWeights` |
| `ggen-e2e` | Cross-platform E2E testing | `TestRunner`, `TestFixture`, `GoldenFile`, `Platform`, `ContainerConfig` |
| `tai-testing` | Production hardening: chaos, load, compliance | `ChaosExperiment`, `LoadTest`, `ComplianceFramework`, `StateInvariant` |
| `tai-validation` | SLO/SLA validation, multi-framework compliance | `SloValidator`, `ComplianceResult` (FISMA, FedRAMP, SOC2, HIPAA, NIST 800-53), `ShaclValidator`, `EvidenceCollector` |

#### Domain / Research

| Crate | Purpose | Key Types |
|-------|---------|-----------|
| `ggen-folk-strategy` | Quantification of 67 folk strategy terms | `FolkTerm` (67 variants), `OpportunityField`, `Trajectory`, `CompetitorDynamics`, `compute_folk_terms()` |
| `ggen-dod` | DfLSS autonomous substrate | `Kernel`, `MAPEKLoop`, `Contract`, `DecisionStore`, `InvariantChecker`, `TimingEnforcer` |
| `ggen-execution` | Common execution framework for 90% semantic convergence | `SemanticConvergenceEngine`, `EnhancedPipelineExecutor`, `TaskOrchestrator`, `RecoveryManager`, `HealthChecker` |
| `ggen-workflow-43` | van der Aalst's 43 workflow patterns | `WorkflowEngine`, `Activity` trait, `WorkflowPattern` trait, `ActivityContext` |
| `ggen-node` | Node.js N-API bindings (napi) | `run()`, `version()`, 21 `#[napi]` fns (marketplace, lifecycle, template, AI) |

### Cross-Cutting Patterns

| Pattern | Where Used | Details |
|---------|-----------|---------|
| **`pub type Result<T> = std::result::Result<T, CrateError>`** | Every crate | Each crate has its own error enum via `thiserror` |
| **Builder pattern** | marketplace, yawl, craftplan, integration, cli-validation, k8s, transport | `with_*()` chain methods for optional config |
| **Typestate** | ggen-marketplace (`Draft`/`Published`), ggen-poka-yoke (`StateMachine<S>`) | Compile-time state transitions |
| **Newtype wrappers** | marketplace (`PackageId`, `PackageVersion`, `QualityScore`), packet (`WorkOrderId`), backpressure (`WIPToken`) | Invariant encoding in types |
| **Async traits** | a2a (`A2aMessageHandler`), registry (`AgentStore`), auth (`SessionManager`, `RateLimiter`, `LockoutManager`), workflow-43 (`Activity`) | `#[async_trait]` with `Result` returns |
| **RDF/SPARQL foundation** | ontology-core, marketplace, yawl, craftplan, prompt-mfg | All built on `oxigraph` triplestores |
| **Pipeline architecture** | core (μ₁–μ₅), yawl, craftplan, process-mining, knhk-etl, execution | Multi-stage deterministic transformation |
| **Prometheus metrics** | tps-andon, metrics-tps | Counters, gauges, histograms with `prometheus` crate |
| **`#![no_std]`** | knhk-etl, knhk-connectors, knhk-lockchain, knhk-otel | Embedded-capable with `alloc` feature gate |

### Key Trait Index

| Trait | Crate | Purpose |
|-------|-------|---------|
| `Queryable` | ggen-codegen | SPARQL query execution → `Vec<HashMap<String,String>>` |
| `Renderable` | ggen-codegen | Template rendering with bindings → `String` |
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

---

## Evidence-First Principle

**ABSOLUTE RULE:** Never fabricate examples, OTEL traces, MCP schemas, or documentation.

### Forbidden
- Fabricating JSON schemas (read actual code first)
- Making up OTEL traces (run with `RUST_LOG=trace`, capture real output)
- Generic 5 Whys without real failure data (analyze actual logs/errors)
- Template-filled documentation (do real investigation first)
- Claiming features work without OTEL evidence

### Required Workflow
1. **Gather Evidence** — Read code, run tests, capture real OTEL output
2. **Analyze** — 5 Whys on ACTUAL failures with real data
3. **Document** — Write with real examples, actual file paths, real traces
4. **Verify** — Prove claims with captured output/logs

**Rule:** If you didn't read it from the codebase or capture it from execution, don't write it.

---

## Tool Restrictions

### Forbidden Tools
| Tool | Use Instead | Reason |
|------|-------------|--------|
| `mcp__desktop-commander__*` | `Read`, `Write`, `Edit`, `Glob`, `Grep`, `Bash` | Explicitly forbidden by user |

### Approved Tools
- File ops: `Read`, `Write`, `Edit`, `Glob`, `Grep`
- Terminal: `Bash`
- Rust navigation: `LSP` (rust-analyzer-lsp) — **ALWAYS use LSP over Grep for .rs files** (see `.claude/rules/rust/lsp.md`)
- Agents: `Agent` tool (with output verification)

---

## 📋 Agent Coordination Rules

| Rule | Requirement |
|------|-------------|
| **Andon Signals** | 🔴 Compiler errors/test failures = STOP THE LINE. Fix immediately. |
| **Cargo Make Only** | NEVER use direct cargo commands. Always `cargo make [target]`. |
| **Testing** | Chicago TDD ONLY (no mocks, no test doubles). 80%+ coverage. Mutation score ≥60%. All tests pass. |
| **No Unwrap** | Zero `unwrap()/expect()` in production. `Result<T,E>` required. |
| **RDF is Truth** | Edit `.specify/*.ttl` (source). Never edit `.md` (generated). |
| **File Organization** | NEVER save to root. Use `crates/*/src/`, `tests/`, `docs/`, etc. |
| **Batch Operations** | 1 message = ALL related operations. TodoWrite 10+ todos minimum. |
| **Agent Execution** | Use Claude Code Task tool. MCP only coordinates topology. |
| **Type-First** | Encode invariants in types. Compiler as design tool. |
| **Definition of Done** | check + lint + test + slo-check + OTEL traces all pass. No signals. |
| **OTEL Validation** | Verify spans/traces for LLM calls, external services, pipeline stages. |
| **Correctness > Speed** | NEVER sacrifice accuracy for speed. Real evidence > fast output. |
| **Evidence-First** | ALL docs/examples MUST reference actual code, real OTEL output, actual errors. No fabrication. |
| **Agent Verification** | ALWAYS verify agent output before accepting. Read files, check for fabrication. |

## Commands

| Command | Purpose | Timeout |
|---------|---------|---------|
| `cargo make check` | Compilation check | <5s |
| `cargo make test` | Full test suite (unit + integration + property) | <30s |
| `cargo make test-mutation` | Mutation testing (≥60% score) | <5min |
| `cargo make lint` | Clippy + rustfmt | <60s |
| `cargo make pre-commit` | check → lint → test-unit | <2min |
| `cargo make slo-check` | Performance SLOs validation | - |
| `cargo make audit` | Security vulnerabilities scan | - |
| `ggen sync` | Full μ₁-μ₅ pipeline | - |
| `ggen validate <ttl>` | SHACL validation | - |

## 🔍 OpenTelemetry (OTEL) Validation

**CRITICAL:** For any feature involving LLM calls or external services, you MUST verify OTEL spans/traces exist. Tests passing is NOT sufficient.

### Required Spans by Feature

| Feature | Required Spans | Required Attributes | Verification Method |
|---------|---------------|---------------------|-------------------|
| **LLM Integration** | `llm.complete`, `llm.complete_stream` | `llm.model`, `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens` | `RUST_LOG=trace,ggen_ai=trace cargo test` + grep for spans |
| **MCP Tools** | `mcp.tool.call`, `mcp.tool.response` | `mcp.tool.name`, `mcp.tool.duration_ms` | Check logs after MCP server operations |
| **Pipeline Stages** | `pipeline.load`, `pipeline.extract`, `pipeline.generate`, `pipeline.validate`, `pipeline.emit` | `pipeline.stage`, `pipeline.duration_ms` | Run `ggen sync` with tracing enabled |
| **Quality Gates** | `quality_gate.validate`, `quality_gate.pass_fail` | `gate.name`, `gate.result` | Run quality gate validation |

### How to Verify OTEL Spans

```bash
# Enable trace logging for OTEL spans
export RUST_LOG=trace,ggen_ai=trace,ggen_core=trace,genai=trace

# Run tests with OTEL output
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# Verify expected spans exist
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.model.*groq|llm\.model.*gpt" otel_output.txt
grep -E "llm\.prompt_tokens|llm\.completion_tokens|llm\.total_tokens" otel_output.txt

# If spans are missing, the feature is NOT working correctly
# Example failure: No "llm.complete" span = LLM API was never called
```

### OTEL Validation Checklist

Before claiming any LLM/external service feature is complete:

- [ ] Spans exist for the operation (e.g., `llm.complete`)
- [ ] Attributes are populated (e.g., `llm.model=groq::openai/gpt-oss-20b`)
- [ ] Token counts are present (if applicable)
- [ ] Timing information is recorded
- [ ] Error spans appear if operation failed (with `error=true` attribute)
- [ ] Spans show network latency (not mock/synthetic response times)

### What OTEL Spans Prove

| Observation | Conclusion |
|-------------|------------|
| `llm.complete` span exists | Real LLM API call was made |
| `llm.model=groq::openai/gpt-oss-20b` | Correct endpoint used |
| `llm.total_tokens > 0` | LLM returned actual content |
| `llm.prompt_tokens + llm.completion_tokens = llm.total_tokens` | Valid token accounting |
| Span duration ~2-3 seconds | Real network call (not mock) |
| No spans found | Feature not working (tests may be mocked) |

**Rule:** If OTEL spans are missing, the feature is **NOT complete**, even if tests pass.

## 🧪 Testing Policy: Chicago TDD ONLY

**CRITICAL:** This project uses Chicago TDD EXCLUSIVELY. London TDD patterns are NOT acceptable.

### What This Means

**Chicago TDD (REQUIRED):**
- ✅ Real collaborators: actual databases, filesystems, HTTP clients, LLM APIs
- ✅ State-based verification: assert on observable results
- ✅ Empirical observation: tests verify actual system behavior
- ✅ Real execution: tests make real API calls, real I/O, real concurrent operations
- ✅ OTEL trace verification: prove real external calls were made

**London TDD (FORBIDDEN):**
- ❌ Mocks and test doubles (`mockall::mock!`, `#[automock]`, `MockXxx` structs)
- ❌ Behavior verification (`.expect_x().times(1)`, `.with(eq(...))`)
- ❌ Dependency injection for testability (traits as mocks)
- ❌ Test doubles that simulate real behavior
- ❌ Assertions on mock interactions rather than actual state

### How to Verify Tests Are Chicago TDD

1. **No mockall imports:** `grep -r "mockall" tests/` should return nothing (except archives)
2. **No Mock structs:** `grep -r "struct Mock" tests/` should return nothing (except archives)
3. **No behavior verification:** `grep -r "expect_\|times(" tests/` should return nothing (except archives)
4. **OTEL spans present:** `RUST_LOG=trace cargo test` should show real API calls
5. **Real I/O operations:** Tests use `TempDir`, `SqlitePool`, `reqwest::Client`, etc.

### References

- `/Users/sac/ggen/.claude/rules/rust/testing.md` - Detailed Chicago TDD requirements
- `/Users/sac/ggen/.claude/rules/rust/testing-forbidden.md` - Forbidden London TDD patterns
- `/Users/sac/ggen/TEST_CATEGORIZATION_REPORT.md` - Current test categorization (63% Chicago, 37% London)

### Migration Path

Existing London TDD tests should be:
1. **Converted** to Chicago TDD (replace mocks with real collaborators)
2. **Deleted** if they only test mock wiring (not real behavior)
3. **Archived** to `tests-archive/london_tdd_legacy/` with DEPRECATED notice

**See:** `/Users/sac/ggen/docs/LONDON_TDD_MIGRATION_GUIDE.md` (to be created)

## Workflow

```bash
# 1. Create RDF Spec (source of truth)
mkdir -p .specify/specs/NNN-feature && vim .specify/specs/NNN-feature/feature.ttl
ggen validate .specify/specs/NNN-feature/feature.ttl

# 2. Chicago TDD (RED → GREEN → REFACTOR)
vim crates/ggen-core/tests/feature_test.rs  # Write failing test (RED)
cargo make test-unit                        # Verify fails
vim crates/ggen-core/src/feature.rs         # Implement (GREEN)
cargo make test-unit                        # Verify passes
cargo make pre-commit                       # Refactor (maintain GREEN)

# 3. Validation (Definition of Done)
cargo make check && cargo make lint && cargo make test && cargo make slo-check

# 4. Generate from Ontology
ggen sync --audit true  # Full sync with cryptographic receipt

# 5. OTEL Trace Validation (REQUIRED for LLM/external service features)
# Verify OpenTelemetry spans/traces exist for critical operations
RUST_LOG=trace,ggen_ai=trace,ggen_core=trace cargo test -p ggen-cli-lib --test llm_e2e_test 2>&1 | grep -E "(llm\.|otel|span|groq|GROQ_API_KEY|complete|request|response)"
# Expected spans: llm.complete, llm.complete_stream
# Expected attributes: llm.model, llm.prompt_tokens, llm.completion_tokens, llm.total_tokens
# If spans are missing, the feature did not call the actual external service
```

## Phased Agent Workflows

**Pattern:** Explore → Plan → Execute (auto-resume on restart)

```bash
# Phase 1: Discover (5 Explore agents search codebase)
launch 5 explore agents to search for optimization opportunities

# Phase 2: Design (5 Plan agents create strategies)
launch 5 planning agents to design implementation plans

# Phase 3: Implement (20 agents execute work)
launch 20 agents to implement all changes
```

**Auto-Resume:** State saved to `.claude/autonomous/workflow-state.json`. On restart, continues from last incomplete phase.
**See:** `/Users/sac/ggen/.claude/autonomous/workflow-pattern.md` for templates and examples.

### Agent Verification Protocol

After agents complete, ALWAYS:
1. **Read** each output file
2. **Check** — Does it reference real code/files? Or is it generic/template?
3. **Validate** — Are OTEL traces plausible? Cross-reference with actual code
4. **Reject** — If fabricated, delete and redo with grounded prompts

Before launching agents:
- Provide actual context (file paths, real errors, actual OTEL output)
- Include "read code first, use real examples only" in every agent prompt
- Never ask agents to document features without pointing to real implementation

## Support

- **Repository**: https://github.com/seanchatmangpt/ggen
- **Documentation**: /Users/sac/ggen/docs/
- **Detailed Rules**: /Users/sac/ggen/.claude/rules/
- **Research**: /Users/sac/ggen/docs/research/
