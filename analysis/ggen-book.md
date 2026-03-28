# ggen: Ontology-Driven Code Generation

**A Comprehensive Technical Reference**

*Sean Chatman — sean@ggen.ai*
*Version 6.0 — 2026*

---

## Table of Contents

1. [Introduction](#chapter-1-introduction)
2. [The Chatman Equation: A = μ(O)](#chapter-2-the-chatman-equation-a-o)
3. [Architecture Overview](#chapter-3-architecture-overview)
4. [The Five-Stage Pipeline](#chapter-4-the-five-stage-pipeline)
5. [Ontology Layer](#chapter-5-ontology-layer)
6. [Template System](#chapter-6-template-system)
7. [Workflow Engine](#chapter-7-workflow-engine)
8. [AI & DSPy Integration](#chapter-8-ai--dspy-integration)
9. [Cryptographic Proofs](#chapter-9-cryptographic-proofs)
10. [Operations & Observability](#chapter-10-operations--observability)
11. [Marketplace & Registry](#chapter-11-marketplace--registry)
12. [Quality Enforcement](#chapter-12-quality-enforcement)
13. [Configuration & Build System](#chapter-13-configuration--build-system)
14. [Examples & Tutorials](#chapter-14-examples--tutorials)
15. [Integration with ChatmanGPT](#chapter-15-integration-with-chatmangpt)
16. [Crate Reference](#appendix-a-crate-reference)
17. [CLI Reference](#appendix-b-cli-reference)

---

## Chapter 1: Introduction

### What is ggen?

ggen is a language-agnostic, deterministic code generation framework that transforms RDF ontologies into reproducible code through SPARQL queries and Tera templates. It is the reference implementation of the **Chatman Equation**:

```
A = μ(O)

Where:
  A = Artifact     (generated code, configuration, infrastructure)
  O = Ontology     (formal RDF specification — source of truth)
  μ = Transformation (pipeline: Normalize → Extract → Emit → Canonicalize → Receipt)
```

### Why RDF Ontologies?

Traditional code generators work from schemas (JSON, YAML, Protobuf). ggen works from **ontologies** — formal knowledge representations that capture not just structure but *semantics*. An ontology specifies what things *mean*, not just what they look like. This enables:

- **Deterministic generation**: Same ontology always produces identical code (SHA256 verified)
- **Multi-target output**: One ontology generates Rust, Go, Elixir, Python, Terraform, Docker, K8s manifests
- **Inference**: SPARQL CONSTRUCT queries derive new facts from existing ones, enriching the generated output
- **Validation**: Ontology closure guarantees all domain concepts are specified before code generation begins

### At-a-Glance Statistics

| Metric | Value |
|--------|-------|
| Workspace crates | 81 |
| Template files | 456 |
| Ontology files (TTL) | 974 |
| Test files | 1,294 |
| Documentation files (md) | 1,416 |
| Example projects | 143 |
| Marketplace packages | 66 |
| Template categories | 56 |
| CHANGELOG entries | 1,074 lines |
| Workspace Cargo.toml | 749 lines |
| Build system (Makefile.toml) | 1,625 lines |

### Perfect For

- Generating REST APIs, GraphQL servers, and microservices from domain models
- Producing infrastructure-as-code (Terraform, K8s, Docker) from architecture ontologies
- Creating multi-language SDKs from a single specification
- Building workflow engines from YAWL process definitions
- Enforcing compliance (HIPAA, SOC2, GDPR) through ontology constraints
- Automating boilerplate with cryptographic proof of provenance

---

## Chapter 2: The Chatman Equation: A = μ(O)

### Core Axioms

ggen is built on three mathematical properties that guarantee deterministic, reproducible code generation:

**Axiom 1: Idempotence** — `μ ∘ μ = μ`
Running the pipeline twice produces identical output. A closed ontology always generates the same artifact.

**Axiom 2: Content Integrity** — `hash(A) = hash(μ(O))`
The SHA256 hash of the generated artifact equals the hash of the pipeline applied to the ontology. Any change in input produces a different hash.

**Axiom 3: Satisfiability** — `O ⊨ Σ`
The ontology satisfies the specification Σ (all constraints are met). Code generation only proceeds when the ontology is closed — all domain concepts are fully specified.

### The Holographic Principle

ggen's architecture follows the **holographic principle**: the RDF ontology is the "film" (a high-dimensional interference pattern), the pipeline μ is the "laser" (deterministic measurement function), and the generated code is the "hologram" (a 2D projection of the high-dimensional pattern).

Different templates (different laser angles) produce different but equally valid projections of the same ontology. All preserve semantic fidelity (Φ = 1.0) when the ontology is closed.

### Specification Entropy

ggen tracks **specification entropy** `H(O) = log₂(n)` where n is the number of possible instantiations. The target is `H(O) ≤ 20 bits` (~1 million possible configurations). When entropy exceeds this threshold, the specification is too open-ended and generation is blocked until constraints are added.

### Specification Closure

Before any code generation, ggen verifies four closure criteria:

1. **Entropy Bound**: H(O) ≤ 20 bits
2. **Domain Coverage**: 100% of domain concepts have RDF representations
3. **Determinism Proof**: Three consecutive runs produce identical SHA256 hashes
4. **Type Preservation**: All RDF property types are enforced in generated code

---

## Chapter 3: Architecture Overview

### Workspace Structure

```
ggen/
├── crates/                    # 83 Rust crates (workspace members)
│   ├── ggen-core/             # Core pipeline + codegen engine
│   ├── ggen-cli/              # CLI binary (noun-verb pattern)
│   ├── ggen-codegen/          # Code generation orchestrator
│   ├── ggen-ontology-core/    # RDF/TTL loading, SPARQL, Oxigraph
│   ├── ggen-workflow/         # 5-stage workflow engine with NIF bindings
│   ├── ggen-workflow-43/      # van der Aalst 43 workflow patterns
│   ├── ggen-yawl/             # YAWL process definitions
│   ├── ggen-ai/               # AI-native generation (GPT-4, Claude)
│   ├── ggen-dspy/             # DSPy-inspired LLM constructs
│   ├── ggen-receipt/          # Ed25519 cryptographic receipts
│   ├── ggen-consensus/        # Byzantine PBFT consensus
│   ├── ggen-marketplace/      # Package registry + validation
│   ├── ggen-process-mining/   # Process discovery + conformance
│   ├── ggen-config/           # Configuration management
│   ├── ggen-config-clap/      # Clap integration for config
│   ├── ggen-canonical/        # Deterministic formatting + hashing
│   ├── ggen-spec-validator/   # Specification validation
│   ├── ggen-poka-yoke/        # Manufacturing error-proofing
│   ├── ggen-jidoka/           # Automation with human intelligence
│   ├── ggen-kaizen/           # Continuous improvement tracking
│   ├── ggen-heijunka/         # Production leveling
│   ├── ggen-backpressure/     # Flow control
│   ├── ggen-transport/        # Network transport layer
│   ├── ggen-a2a/              # Agent-to-agent protocol
│   ├── ggen-a2a-mcp/          # A2A + MCP bridge
│   ├── ggen-testing/          # Test utilities
│   ├── ggen-test-audit/       # Test result auditing
│   ├── ggen-test-opt/         # Test optimization
│   ├── ggen-utils/            # Shared utilities
│   ├── ggen-macros/           # Procedural macros
│   ├── ggen-execution/        # Runtime execution engine
│   ├── ggen-dod/              # Definition of Done enforcement
│   ├── ggen-firewall/         # Security boundary enforcement
│   ├── ggen-folk-strategy/    # Folk strategy patterns
│   ├── ggen-craftplan/        # Craft planning
│   ├── ggen-domain/           # Domain model abstractions
│   ├── ggen-api/              # API framework
│   ├── ggen-auth/             # Authentication/authorization
│   ├── ggen-saas/             # Multi-tenant SaaS scaffolding
│   ├── ggen-node/             # Node.js bindings
│   ├── ggen-payments/         # Payment processing
│   ├── ggen-supplier/         # Supplier management
│   ├── ggen-prompt-mfg/       # Prompt manufacturing
│   ├── ggen-e2e/              # End-to-end testing
│   ├── ggen-e2e-tps/          # TPS end-to-end tests
│   ├── ggen-integration/      # Integration test framework
│   ├── ggen-cli-tps/          # TPS CLI integration
│   ├── ggen-cli-validation/   # CLI input validation
│   ├── ggen-metrics-tps/      # TPS metrics collection
│   ├── ggen-marketplace-tps/  # TPS marketplace tests
│   ├── ggen-tps-andon/        # Andon alert system
│   ├── ggen-packet/           # Network packet handling
│   ├── ggen-osiris/           # Osiris autonomic computing
│   ├── osiris-*/              # Osiris subsystems (core, autonomic, domains, sensors, tps)
│   ├── tai-*/                 # TAI platform (cache, gateway, gcp, grpc, k8s, lb, obs, resilience, security, state, testing, validation)
│   ├── tps-*/                 # TPS utilities (jidoka, kaizen, reference)
│   ├── knhk-*/                # KNHK systems (connectors, etl, hot, lockchain, orchestrator, otel)
│   └── a2a-generated/         # Generated A2A protocol code
├── templates/                 # 56 template categories, 2,002 files
├── .specify/                  # 92 TTL specification files
├── tests/                     # 285 integration test files
├── examples/                  # 143 example projects
├── docs/                      # 1,416 documentation files
│   ├── 00-overview/           # Mission, glossary, system contract
│   ├── 10-architecture/       # C4 models, security, runtime, evidence plane
│   ├── 30-autonomics/         # Action contracts, signal contracts, invariants, refusal modes
│   ├── 40-operations/         # Incident playbooks, kaizen, disaster recovery, storm discipline
│   ├── diataxis/              # Explanations, how-tos, tutorials, references
│   ├── research/              # AI testing, DSPy evaluation, memory architecture, performance
│   ├── innovation/            # Andon validation, gap analysis, phase implementations
│   └── book/                  # Published HTML book
├── registry/                  # Package registry (index.json with 66 packages)
├── evidence/                  # Evidence plane artifacts
├── ggen.toml                  # Project configuration (274 lines)
├── ggen-paas.toml             # PaaS configuration (348 lines)
├── Makefile.toml              # Build system (1,625 lines, cargo-make)
└── Cargo.toml                 # Workspace manifest (749 lines)
```

### Dependency Flow

```
                    ┌─────────────────┐
                    │  ggen.toml      │  ← Project config
                    │  ontology.ttl   │  ← Source of truth
                    └────────┬────────┘
                             │
                    ┌────────▼────────┐
                    │  ggen-core      │  ← Pipeline orchestration
                    │  ┌───────────┐  │
                    │  │ codegen/  │  │  ← 5-stage transformation
                    │  │ pipeline  │  │
                    │  └───────────┘  │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
    ┌─────────▼──────┐ ┌────▼────────┐ ┌───▼───────────┐
    │ ggen-ontology  │ │ ggen-config │ │ ggen-canonical│
    │    -core       │ │   -clap     │ │               │
    │ (RDF/SPARQL)   │ │ (CLI args)  │ │ (SHA256 hash) │
    └─────────┬──────┘ └────┬────────┘ └───┬───────────┘
              │              │              │
              └──────────────┼──────────────┘
                             │
                    ┌────────▼────────┐
                    │ ggen-codegen    │  ← Template rendering
                    │ (Tera engine)   │
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
    ┌─────────▼──────┐ ┌────▼────────┐ ┌───▼───────────┐
    │ ggen-workflow  │ │ ggen-receipt │ │ ggen-consensus│
    │  + workflow-43 │ │ (Ed25519)    │ │ (PBFT)        │
    └────────────────┘ └─────────────┘ └───────────────┘
```

---

## Chapter 4: The Five-Stage Pipeline

Every ggen generation passes through five deterministic stages, implemented in `ggen-core/src/codegen/pipeline.rs`:

### Stage 1: Normalize (μ₁)

**Input**: Raw RDF ontology files (`.ttl`)
**Output**: Validated, normalized triple store

Actions:
- Load Turtle files into Oxigraph triple store
- Validate RDF syntax and SHACL shapes
- Resolve namespace prefixes
- Compute specification entropy H(O)
- Block if H(O) > 20 bits (specification too open)

### Stage 2: Extract (μ₂)

**Input**: Normalized triple store
**Output**: Enriched graph with inferred facts

Actions:
- Execute SPARQL CONSTRUCT queries (inference rules from manifest)
- Run OWL inference (subclass, property chain, equivalence)
- Materialize derived triples back into the graph
- Track `triples_added` per rule for audit trail

### Stage 3: Emit (μ₃)

**Input**: Enriched graph
**Output**: Rendered code files

Actions:
- Execute SPARQL SELECT queries (generation rules from manifest)
- Bind results to Tera template variables
- Render templates with Jinja2-compatible syntax
- Support conditional blocks (`{% if %}`), loops (`{% for %}`), filters

### Stage 4: Canonicalize (μ₄)

**Input**: Rendered code files
**Output**: Deterministically formatted files with content hashes

Actions:
- Apply language-specific formatting (rustfmt, gofmt, mix format)
- Compute SHA256 hash of each file's content
- Record `content_hash` and `size_bytes` in `GeneratedFile` record
- Ensure idempotency: `format(render(O)) == format(render(O))`

### Stage 5: Receipt (μ₅)

**Input**: All generated files + pipeline metadata
**Output**: Cryptographic receipt chain

Actions:
- Create `Receipt` with operation name, input hashes, output hashes
- Sign with Ed25519 keypair
- Chain to previous receipt via hash link
- Verify chain integrity (no tampering)

### Pipeline State Tracking

The `PipelineState` struct tracks the entire execution:

```rust
pub struct PipelineState {
    pub manifest: GgenManifest,           // Loaded manifest
    pub ontology_graph: Graph,            // Domain ontology
    pub executed_rules: Vec<ExecutedRule>, // Inference + generation
    pub generated_files: Vec<GeneratedFile>, // Output artifacts
    pub validation_results: Vec<ValidationResult>, // Quality gates
    pub started_at: Instant,              // Pipeline start time
}
```

Each `ExecutedRule` records the rule name, type (Inference or Generation), triples added, execution duration, and query hash — providing a complete audit trail.

> **See Also:** [Pattern 11 — SPARQL PROJECTION](../docs/java26-patterns/src/patterns/11-sparql-projection.md) in *A Pattern Language for Java 26 with ggen* shows how SELECT queries become template context in the generation pipeline.

---

## Chapter 5: Ontology Layer

### ggen-ontology-core

The ontology layer (`crates/ggen-ontology-core`) provides the foundational RDF processing:

```rust
// Module organization
pub mod entity_mapper;    // Domain → ontology mapping with confidence scores
pub mod errors;           // Rich error context (OntologyError)
pub mod sparql_generator; // Deterministic SPARQL query building
pub mod triple_store;     // RDF/TTL loading + Oxigraph querying
pub mod validators;       // Syntax + semantic validation
```

**Key capabilities**:
- Load and validate Turtle (TTL) files
- Execute SPARQL SELECT and CONSTRUCT queries via Oxigraph
- Map domain models to ontology classes with confidence scores
- Generate deterministic SPARQL queries from structured inputs

### Triple Store

The triple store wraps Oxigraph for in-memory RDF operations:

```rust
use ggen_ontology_core::triple_store::TripleStore;

let store = TripleStore::new()?;
store.load_turtle("ontology.ttl")?;
let results = store.query_sparql(&query)?;
```

### SPARQL Generator

Deterministic query construction prevents subtle bugs from hand-written SPARQL:

```rust
use ggen_ontology_core::sparql_generator::SparqlGenerator;

let query = SparqlGenerator::find_policies_by_jurisdiction("US");
let results = store.query_sparql(&query)?;
```

### Entity Mapper

Maps domain terms to ontology classes with confidence scoring:

```rust
use ggen_ontology_core::entity_mapper::EntityMapper;

let matches = EntityMapper::match_policy("Privacy Policy")?;
// Returns: [{label: "DataPrivacyPolicy", class: "legal:Policy", score: 0.95}, ...]
```

### Specification Files

The `.specify/` directory contains 92 TTL specification files organized as:

```
.specify/
├── domain.ttl           # Domain entities, properties, relationships
├── constraints.ttl      # Preconditions, invariants, postconditions
├── lifecycle.ttl        # State machines, valid transitions
└── vocabulary.ttl       # Term definitions, aliases
```

Each specification follows a 3-level hierarchy:
- **Level 1**: Abstract types (nouns, verbs, predicates)
- **Level 2**: Concrete properties (fields, methods, enum variants)
- **Level 3**: Lifecycle & constraints (state machines, pre/postconditions)

---

## Chapter 6: Template System

### Template Formats

ggen supports two template formats:

**`.tmpl` files** — Combined SPARQL + Tera templates with frontmatter:
```yaml
---
sparql:
  project_info: |
    PREFIX cli: <http://ggen.dev/schema/cli#>
    SELECT ?name ?version ?description
    WHERE {
      ?project a cli:CliProject ;
               cli:hasName ?name ;
               cli:hasVersion ?version ;
               cli:hasDescription ?description .
    }
---

# {{ sparql_first(results=sparql_results.project_info, column='name') }}
```

**`.tera` files** — Pure Tera templates (Jinja2-compatible):
```tera
{% for class in classes %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} {
    {% for property in class.properties %}
    pub {{ property.name }}: {{ property.rust_type }},
    {% endfor %}
}
{% endfor %}
```

### Template Categories (56 total)

| Category | Templates | Purpose |
|----------|-----------|---------|
| **ai** | ai-client-wrapper, ai-generated, ai-generators, ai-ontology, ai-sparql | AI-powered generation |
| **api** | api-routes, openapi | REST/GraphQL API scaffolding |
| **cli** | cli-command, cli-dispatcher, clap_* (6 templates) | CLI applications with clap |
| **clnrm** | weaver-registry | Weaver OTEL registry generation |
| **cleanroom** | cleanroom/* | Cleanroom methodology templates |
| **docker** | docker-compose, container-spec | Container orchestration |
| **k8s** | k8s-deployment, kubernetes-deployment, istio-virtualservice | Kubernetes manifests |
| **terraform** | terraform-main | Infrastructure-as-code |
| **erlang** | erlang-adapter | Erlang/OTP adapter generation |
| **mcp-server** | mcp-server/* | MCP (Model Context Protocol) servers |
| **llm_construct** | signature.rs.tera | DSPy-style LLM construct generation |
| **ggen** | rust-struct.tera | ggen's own code generation |
| **papers** | papers/* | Academic paper templates |
| **chicago-tdd** | chicago-tdd-suite.tera | Test suite generation |
| **python** | python.tmpl | Python code generation |
| **rust** | rust.tmpl, rust-service-with-placeholders.tmpl | Rust scaffolding |
| **bash** | bash.tmpl | Shell script generation |
| **node-bindings** | node-bindings/* | Node.js FFI bindings |
| **ollama** | ollama/* | Ollama local LLM integration |
| **mcp-board-report** | mcp-board-report/* | Board intelligence reports |

### Key Templates

#### MCP Stdio Server Template

Generates a complete MCP server implementing JSON-RPC 2.0 over stdio:
- Tool registration and invocation
- Resource management (read/write)
- Prompt templates
- ZAI streaming support
- A2A message bridging

#### Chicago TDD Test Suite

Generates Chicago School TDD test suites (behavior verification, no mocks):
```
ARRANGE: Create real objects (no mocks)
ACT: Observable behavior (single logical step)
ASSERT: Verify observable state (not mocks, not side effects)
```

#### LLM Construct Signature

Generates DSPy-inspired LLM constructs with typed input/output fields, constraints (required, min_length, max_length, pattern, semantic_type), and auto-generated tests.

#### Container Specification

Generates complete container specs with health checks, resource limits, security context, and observability configuration.

> **See Also:** [Pattern 12 — TEMPLATE RULE](../docs/java26-patterns/src/patterns/12-template-rule.md) in *A Pattern Language for Java 26 with ggen* describes the three-part rule composition: SPARQL query + Tera template + output path.

---

## Chapter 7: Workflow Engine

### ggen-workflow

The workflow engine (`crates/ggen-workflow`) provides a five-stage transformation pipeline with pattern orchestration and cryptographic receipt generation. It implements the Chatman Equation as an executable pipeline:

```
μ₁ (Normalize) → μ₂ (Extract) → μ₃ (Emit) → μ₄ (Canonicalize) → μ₅ (Receipt)
```

**Module structure**:
```rust
pub mod engine;      // WorkflowEngine, TaskScheduler, FlowRouter
pub mod error;       // WorkflowError, WorkflowResult
pub mod nif;         // Erlang NIF bindings
pub mod parser;      // YAWL spec parser (YawlSpec, Flow, Task, Condition)
pub mod patterns;    // Sequence, Parallel, Choice, Sync
pub mod receipts;    // ReceiptGenerator, ReceiptChain
pub mod state;       // StateMachine, WorkflowState, ExecutionMetrics
```

**Configuration constants**:
```rust
pub struct Constants {
    pub max_workflow_steps: usize,       // Upper bound on step count
    pub max_parallel_tasks: usize,       // Concurrency limit
    pub max_sparql_query_size: usize,    // Query size limit
    pub default_timeout_ms: u64,         // Operation timeout
    pub receipt_hash_length: usize,      // SHA256 output length
}
```

### ggen-workflow-43

Complete implementation of van der Aalst's 43 workflow control-flow patterns:

| Category | Patterns | Description |
|----------|----------|-------------|
| Basic Control Flow | 1-5 | Sequence, Parallel Split, Synchronization, Exclusive Choice, Simple Merge |
| Advanced Branching | 6-12 | Multi-choice, Synchronizing merge, Multi-merge, Discriminator, Arbitrary cycles |
| Multiple Instance | 13-16 | MI without/with synchronization, MI with a priori design time knowledge |
| State-Based | 17-22 | Deferred choice, Interleaved parallel routing, Milestone |
| Cancellation | 23-29 | Cancel activity, Cancel case, Cancel region, Completeness |
| Iteration | 30-43 | Arbitrary cycles, Structured loop, Recursion, Implicit termination |

**Implementation guarantees**:
```rust
#![deny(missing_docs)]
#![deny(unsafe_code)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]
```

### Workflow Patterns

**Sequence**: Linear step execution with sequential dependencies
**Parallel**: Concurrent execution with synchronization barrier
**Choice**: Conditional branching based on evaluation results
**Sync**: Barrier synchronization across multiple concurrent streams

---

## Chapter 8: AI & DSPy Integration

### ggen-ai

AI-native code generation with multi-provider LLM support (GPT-4, Claude, Ollama):

- Intelligent template rendering with semantic understanding
- Conversational workflows for interactive generation
- Semantic validation of generated code against ontology
- Multi-provider support with fallback chains

### ggen-dspy (DSPy-Inspired)

Implements the DSPy methodology for structured LLM interactions:

**Signatures** — Typed input/output specifications:
```rust
use ggen_ai::dspy::{Signature, InputField, OutputField};

let sig = Signature::new("classify_intent", "Classify user intent")
    .with_input(InputField::new("query", "User query text", "String"))
    .with_output(OutputField::new("intent", "Classified intent", "String"))
    .required(true)
    .with_min_length(1);
```

**LLM Constructs** — Generated via `templates/llm_construct/signature.rs.tera`:
- Typed structs wrapping DSPy signatures
- Constraint validation (required, min_length, max_length, pattern, semantic_type)
- Auto-generated test suites verifying construct structure

### AI Template Creation

ggen can generate its own templates using AI:
```bash
ggen template create --ai --description "REST API for user management"
```

The AI generates both the `.ttl` specification and the `.tera` template, ensuring the generated code conforms to the ontology.

---

## Chapter 9: Cryptographic Proofs

### ggen-receipt

Production-ready cryptographic receipt system using Ed25519 digital signatures:

```rust
use ggen_receipt::{Receipt, ReceiptChain, generate_keypair};

let (signing_key, verifying_key) = generate_keypair();

let receipt = Receipt::new(
    "my-operation".to_string(),
    vec!["input-hash-1".to_string()],
    vec!["output-hash-1".to_string()],
    None,
).sign(&signing_key)?;

receipt.verify(&verifying_key)?;
```

**Receipt Chain** — Linked receipts forming an auditable trail:
```rust
let mut chain = ReceiptChain::from_genesis(genesis)?;
let receipt2 = Receipt::new("second-op", ...).chain(&genesis)?.sign(&key)?;
chain.append(receipt2)?;
chain.verify(&verifying_key)?; // Verifies entire chain integrity
```

**Security properties**:
- `#![forbid(unsafe_code)]` — No unsafe code anywhere
- Ed25519 signatures — Quantum-resistant (as of 2026)
- SHA-256 hashing — Content integrity verification
- Chain linking — Tamper-evident audit trail

### ggen-consensus

Byzantine fault-tolerant consensus (PBFT) for multi-agent receipt verification:

```rust
// Version 6.0.0
// 3f+1 safety property: tolerates f Byzantine nodes with 3f+1 total
// Blake3 hashing for fast integrity checks
// Ed25519 signatures for identity verification
```

**Test coverage**:
- 4-node cluster with 1 Byzantine node → consensus on correct value
- 10-node cluster with 3 Byzantine nodes → 7/10 quorum achieved
- Sequential rounds with persistent Byzantine node → recovery after fault correction
- Network partition simulation → no false consensus

---

## Chapter 10: Operations & Observability

### Osiris — Autonomic Computing

The Osiris subsystem (`osiris-*`) implements autonomic computing principles:

| Crate | Purpose |
|-------|---------|
| `osiris-core` | Core autonomic loop (MAPE-K: Monitor, Analyze, Plan, Execute, Knowledge) |
| `osiris-autonomic` | Self-managing system behaviors |
| `osiris-domains` | Domain-specific autonomic policies |
| `osiris-sensors` | System sensors and metric collection |
| `osiris-tps` | TPS integration for osiris |

### TAI — Platform Infrastructure

The TAI (Trustworthy AI Infrastructure) subsystem:

| Crate | Purpose |
|-------|---------|
| `tai-cache` | Distributed caching layer |
| `tai-gateway` | API gateway with rate limiting |
| `tai-gcp` | Google Cloud Platform integration |
| `tai-grpc` | gRPC transport layer |
| `tai-k8s` | Kubernetes operator |
| `tai-loadbalancer` | Load balancing strategies |
| `tai-observability` | OpenTelemetry integration, tracing, metrics |
| `tai-resilience` | Circuit breakers, retries, timeouts |
| `tai-security` | Authentication, authorization, encryption |
| `tai-state` | Distributed state management |
| `tai-testing` | Test utilities for TAI components |
| `tai-validation` | Input/output validation |

### KNHK — Knowledge & Provenance

The KNHK subsystem handles knowledge graphs with full lineage tracking:

| Crate | Purpose |
|-------|---------|
| `knhk-connectors` | External system connectors (SAP, Salesforce, ServiceNow) |
| `knhk-etl` | ETL pipeline orchestration |
| `knhk-hot` | Higher-Order Thinking for knowledge reasoning |
| `knhk-lockchain` | Blockchain-anchored provenance |
| `knhk-orchestrator` | Multi-system orchestration |
| `knhk-otel` | OpenTelemetry semantic conventions for KNHK |

### TPS — Toyota Production System

The TPS subsystem implements lean manufacturing principles:

| Crate | Purpose |
|-------|---------|
| `tps-jidoka` | Automation with human intelligence (stop-and-fix) |
| `tps-kaizen` | Continuous improvement tracking and metrics |
| `tps-reference` | TPS reference implementations and patterns |

---

## Chapter 11: Marketplace & Registry

### Package Registry

ggen includes a built-in package registry at `registry/index.json` (1,741 lines) with 66 packages across 10 categories:

| Category | Count | Examples |
|----------|-------|---------|
| Academic | 6 | academic-bibliography-manager, academic-peer-review-workflow |
| AI | 2 | ai-code-assistant, ai-prompt-optimizer |
| Architecture | 3 | microservice-scaffold, event-sourcing-starter |
| Data | 2 | data-pipeline, etl-framework |
| Enterprise | 3 | compliance-engine, audit-trail |
| Finance | 2 | payment-processor, ledger-system |
| Healthcare | 3 | hipaa-compliance, clinical-workflow, pharma-trial |
| Rust | 3 | advanced-rust-project, rust-axum-api |
| Templates | 4 | rest-api-template, graphql-server-template |
| Uncategorized | 38 | Various domain-specific packages |

### Package Validation

Each package is validated against critical and bonus guards:

```json
{
  "critical_guards_passed": 3,
  "critical_guards_total": 5,
  "bonus_guards_passed": 0,
  "bonus_guards_total": 0,
  "validation_score": "66.7",
  "production_ready": false,
  "last_validated": "2026-03-24T06:27:54.060154+00:00"
}
```

### Publishing

Packages are published to the registry with cryptographic receipts. The publishing guide documents the validation process, guard requirements, and the approval workflow.

---

## Chapter 12: Quality Enforcement

### Poka-Yoke (Error-Proofing)

Manufacturing-grade quality gates that prevent defects before they happen:

- **Template guards**: Validate template inputs against expected types
- **SLO enforcement**: Automatic checks against service level objectives
- **Andon signals**: Visual alerts when quality thresholds are breached
- **FMEA (Failure Mode Effects Analysis)**: Systematic risk assessment

### Jidoka (Automation with Human Intelligence)

The `ggen-jidoka` crate implements stop-and-fix automation:

- Detect anomalies during generation
- Stop the pipeline when quality thresholds are breached
- Escalate to human operators for judgment
- Resume automatically after fix confirmation

### Kaizen (Continuous Improvement)

The `ggen-kaizen` crate tracks improvement metrics over time:

- Weekly metric reviews (test pass rate, generation time, validation score)
- Trend analysis and anomaly detection
- Improvement recommendations based on historical data

### Heijunka (Production Leveling)

The `ggen-heijunka` crate levels generation workloads:

- Batch similar generation tasks
- Prevent burst overload
- Maintain steady throughput

### Chicago TDD

ggen generates Chicago School TDD test suites (behavior verification, no mocks):

```
ARRANGE → ACT → ASSERT
- Real objects, no mocks
- Tests verify observable behavior
- Independent, repeatable, self-checking
```

### Andon System

The `ggen-tps-andon` crate implements manufacturing andon (call-for-help) signals:

- Real-time quality dashboards
- Threshold-based alerting
- Integration with CI/CD pipelines

---

## Chapter 13: Configuration & Build System

### ggen.toml

Project configuration (274 lines):

```toml
[project]
name = "ggen"
version = "5.0.2"
description = "Deterministic, language-agnostic code generation framework"

[ontology]
source = "ontology.ttl"
base_uri = "https://ggen.dev/"
format = "turtle"

[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"
temperature = 0.5
max_tokens = 8000
timeout = 120

[templates]
directory = "templates"
backup_enabled = true
idempotent = true

[sparql]
timeout = 60
max_results = 5000
cache_enabled = true
cache_ttl = 7200

[security]
allowed_domains = ["schema.org", "w3.org", "ggen.dev", "github.com"]
```

### Makefile.toml

Build system (1,625 lines) powered by cargo-make:

- `cargo make build` — Build all workspace crates
- `cargo make test` — Run full test suite (1,294 test files)
- `cargo make lint` — Clippy + rustfmt checks
- `cargo make docs` — Generate documentation
- `cargo make release` — Production build with optimizations
- `cargo make deb` — Build Debian package (cargo-deb)

### ggen-paas.toml

PaaS infrastructure configuration (348 lines) for generating cloud infrastructure from ontology.

### Cargo Workspace

The workspace Cargo.toml (749 lines) defines:
- 83 workspace members
- Shared dependency versions (tokio, serde, tracing, etc.)
- Feature flags per crate
- 285 integration test targets
- Debian packaging metadata

---

## Chapter 14: Examples & Tutorials

### Getting Started (5 Minutes)

```bash
# Install
cargo install ggen

# Check environment
ggen doctor

# Get personalized help
ggen help-me

# List templates
ggen template list

# Generate from ontology
ggen sync -r ontology.ttl -t templates/rust.tmpl -o output/
```

### Example Projects (143 total)

| Category | Examples | Description |
|----------|----------|-------------|
| **Basic** | basic-template-generation | Template fundamentals (15-30 min) |
| **AI** | ai-template-creation, ai-code-generation | AI-powered generation (30-45 min) |
| **Advanced** | advanced-rust-api-8020, advanced-pipeline, advanced-cli-tool | Production patterns |
| **Integration** | advanced-fullstack-integration, advanced-lifecycle-demo | End-to-end workflows |
| **Workflow** | bree-semantic-scheduler | Job orchestration |
| **Infrastructure** | Docker, K8s, Terraform examples | Infrastructure-as-code |
| **Process Mining** | Process discovery and conformance checking | YAWL integration |
| **A2A** | a2a-agent-lifecycle, a2a-tool-use-integration | Agent-to-agent protocols |
| **Cleanroom** | cleanroom/examples/ | <60 second concept-to-deploy |

### Quick Start Tutorials

1. **basic-template-generation/** — Learn template fundamentals (15-30 min)
2. **ai-template-creation/** — AI-powered template creation (30-45 min)
3. **complete-project-generation/** — Generate full projects (45-60 min)
4. **cleanroom/examples/** — Ultra-fast deployment workflows (<60 seconds)

---

## Chapter 15: Integration with ChatmanGPT

### Semconv Synchronization

ggen integrates with ChatmanGPT's OpenTelemetry semantic convention system:

- Weaver registry templates in `templates/clnrm/weaver-registry.tmpl`
- Generates span definitions and attribute schemas from ontology
- Produces typed constants for Elixir, Go, and Rust

### Cross-Project Sync

The `GGEN_SYNC_INTEGRATION_MANIFEST.md` defines the synchronization protocol between ggen and ChatmanGPT's 5 subprojects:

| ChatmanGPT Project | ggen Integration |
|-------------------|-----------------|
| **pm4py-rust** | Process mining engine, YAWL conformance |
| **BusinessOS** | PaaS infrastructure, Go backend generation |
| **Canopy** | Elixir/Phoenix adapter generation |
| **OSA** | Agent orchestration, board intelligence |
| **semconv** | Weaver registry generation |

### Board Intelligence

ggen templates generate the board intelligence pipeline:
- L0→L1→L2→L3 SPARQL inference chain
- Conway's Law vs Little's Law routing
- Encrypted board briefings (X25519+AES-256-GCM)

---

## Chapter 16: Java 26 Code Generation

The `ggen-yawl` crate implements six Java generation rules that produce complete Spring Boot 3 / Jakarta EE applications from YAWL RDF ontologies:

| Rule | Generated Artifact | Source File |
|------|--------------------|-------------|
| JPA Entity Mapping | `@Entity` classes with Jakarta Persistence 3.2 | `crates/ggen-yawl/src/codegen/rules/jpa_entity.rs` |
| Spring Data Repository | `JpaRepository<T, UUID>` interfaces | `crates/ggen-yawl/src/codegen/rules/repositories.rs` |
| DTO Projection | `@Data @Builder` transfer objects | `crates/ggen-yawl/src/codegen/rules/dtos.rs` |
| REST Controller | `@RestController` CRUD endpoints | `crates/ggen-yawl/src/codegen/rules/controllers.rs` |
| Service Layer | `@Service @Transactional` classes | `crates/ggen-yawl/src/codegen/rules/services.rs` |
| Java Enum | Domain enumerations with `getValue()`/`fromValue()` | `crates/ggen-yawl/src/codegen/rules/enums.rs` |

The complete pattern language for these generation rules is documented in [A Pattern Language for Java 26 with ggen](../docs/java26-patterns/src/README.md), which maps each rule to an Alexander-style pattern with copy-paste TTL snippets and executable examples.

### E2E Test Coverage

The YAWL Java generation achieves 95.1% test coverage (77/81 checks passing) across the complete generation pipeline. See `crates/ggen-yawl/tests/E2E_TEST_REPORT.md` for the full report.

## Chapter 17: A2A Protocol Integration

The `ggen-a2a` crate generates Agent-to-Agent (A2A) protocol implementations from ontology definitions in `.specify/specs/014-a2a-integration/`. The generated code includes:

- Task state machine (submitted → working → completed/failed/cancelled)
- Agent card endpoints
- A2A MCP bridge (`ggen-a2a-mcp`)
- Artifact type definitions

The pipeline follows the same μ₁-μ₅ stages as all ggen sync operations, using `a2a-ontology.ttl` as the source of truth.

## Chapter 18: ggen-sync Platform Vectors

The ChatmanGPT integration (Chapter 15) uses five platform vectors, each driven by `ggen sync`:

| Vector | Technology | Ontology |
|--------|-----------|----------|
| **pm4py-rust** | Process mining, Rust | `pm4py-api.ttl` |
| **BusinessOS** | Go microservices, PaaS | `businessos.ttl` |
| **Canopy** | Elixir/Phoenix orchestration | `canopy.ttl` |
| **OSA** | Python agent framework | `osa.ttl` |
| **semconv** | Semantic conventions (Weaver) | `semconv.ttl` |

Each vector is regenerated by running `ggen sync` from the corresponding directory. The Weaver registry integration ensures semantic convention compliance across all vectors.

---

## Appendix A: Crate Reference

### Core Crates

| Crate | Version | Description |
|-------|---------|-------------|
| ggen-core | — | Pipeline orchestration, codegen engine, graph operations |
| ggen-cli | — | CLI binary (noun-verb pattern with clap) |
| ggen-codegen | — | Code generation orchestrator |
| ggen-config | — | Configuration management |
| ggen-utils | — | Shared utilities |

### Domain Crates

| Crate | Version | Description |
|-------|---------|-------------|
| ggen-ontology-core | 0.2.0 | RDF/TTL, SPARQL, Oxigraph triple store |
| ggen-workflow | — | 5-stage workflow engine with NIF bindings |
| ggen-workflow-43 | 0.1.0 | Complete van der Aalst 43 patterns |
| ggen-yawl | — | YAWL process definition support |
| ggen-ai | — | AI-native code generation (multi-provider) |
| ggen-dspy | — | DSPy-inspired LLM constructs |
| ggen-process-mining | — | Process discovery and conformance checking |
| ggen-receipt | 0.2.0 | Ed25519 cryptographic receipts |
| ggen-consensus | 6.0.0 | Byzantine PBFT consensus |

### Infrastructure Crates

| Crate | Version | Description |
|-------|---------|-------------|
| ggen-transport | — | Network transport layer |
| ggen-node | — | Node.js bindings |
| ggen-api | — | API framework |
| ggen-auth | — | Authentication/authorization |
| ggen-saas | — | Multi-tenant SaaS scaffolding |
| ggen-payments | — | Payment processing |
| ggen-supplier | — | Supplier management |

### Quality Crates

| Crate | Version | Description |
|-------|---------|-------------|
| ggen-poka-yoke | — | Manufacturing error-proofing |
| ggen-jidoka | — | Automation with human intelligence |
| ggen-kaizen | — | Continuous improvement tracking |
| ggen-heijunka | — | Production leveling |
| ggen-backpressure | — | Flow control |
| ggen-dod | — | Definition of Done enforcement |
| ggen-firewall | — | Security boundary enforcement |
| ggen-canonical | — | Deterministic formatting and hashing |
| ggen-spec-validator | — | Specification validation |

### Observability Crates

| Crate | Description |
|-------|-------------|
| osiris-core | Core autonomic computing (MAPE-K loop) |
| osiris-autonomic | Self-managing system behaviors |
| osiris-domains | Domain-specific autonomic policies |
| osiris-sensors | System sensors and metric collection |
| osiris-tps | TPS integration for osiris |
| tai-observability | OpenTelemetry integration |
| tai-resilience | Circuit breakers, retries, timeouts |
| tai-security | Authentication, authorization, encryption |
| tai-k8s | Kubernetes operator |
| tai-grpc | gRPC transport layer |
| tai-cache | Distributed caching |
| tai-gateway | API gateway with rate limiting |
| tai-gcp | Google Cloud Platform integration |
| tai-loadbalancer | Load balancing strategies |
| tai-state | Distributed state management |
| tai-testing | Test utilities |
| tai-validation | Input/output validation |
| knhk-connectors | External system connectors |
| knhk-etl | ETL pipeline orchestration |
| knhk-hot | Higher-Order Thinking |
| knhk-lockchain | Blockchain-anchored provenance |
| knhk-orchestrator | Multi-system orchestration |
| knhk-otel | OpenTelemetry semantic conventions |
| tps-jidoka | TPS stop-and-fix automation |
| tps-kaizen | TPS continuous improvement |
| tps-reference | TPS reference patterns |

---

## Appendix B: CLI Reference

### Core Commands

```bash
ggen sync               # Full pipeline: ontology → code
ggen template list      # List available templates
ggen template create    # Create new template
ggen doctor             # Environment health check
ggen help-me            # Personalized help
```

### Generation Commands

```bash
ggen sync -r ontology.ttl -t templates/rust.tmpl -o output/
ggen template generate -t templates/api-routes.tera -r spec.ttl -o src/
ggen template create --ai --description "REST API for user management"
```

### Configuration Commands

```bash
ggen config show        # Display current configuration
ggen config validate    # Validate configuration files
ggen config set key=value  # Set configuration value
```

### Quality Commands

```bash
cargo make test         # Run full test suite
cargo make lint         # Clippy + rustfmt
cargo make docs         # Generate documentation
cargo make release      # Production build
cargo make deb          # Build Debian package
```

---

*Generated: 2026-03-28*
*Source: ggen repository analysis — 81 crates, 456 templates, 974 ontologies, 1,294 tests*
*Author: Sean Chatman <sean@ggen.ai>*
*Repository: https://github.com/seanchatmangpt/ggen*
