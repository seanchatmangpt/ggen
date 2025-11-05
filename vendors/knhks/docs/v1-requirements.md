# KNHKS v1.0 Requirements Document

## 1. Core Foundation (KGC Axioms)

### 1.1 Chatman Equation

- **Law**: A = μ(O) - Action equals projection of Ontology
- **Idempotence**: μ∘μ = μ
- **Typing**: O ⊨ Σ - All inputs must conform to schema
- **Order**: Λ is ≺-total - Deterministic evaluation order
- **Merge**: Π is ⊕-monoid - Associative merge operations
- **Glue**: glue(Cover(O)) = Γ(O) - Local-to-global consistency
- **Shard**: μ(O ⊔ Δ) = μ(O) ⊔ μ(Δ) - Shard composition law
- **Provenance**: hash(A) = hash(μ(O)) - Cryptographic receipts
- **Guard**: μ ⊣ H - Guards block forbidden operations
- **Epoch**: μ ⊂ τ, τ ≤ 8 - Hard time bound
- **Sparsity**: μ → S (80/20) - Dark Matter optimization
- **Minimality**: argmin drift(A) - Minimize state drift
- **Invariants**: preserve(Q) - Maintain invariants

### 1.2 Chatman Constant

- **Definition**: χ ≡ p95(hook eval) ≤ 2 ns = 8 ticks
- **Enforcement**: AOT (Ahead-Of-Time) ingest guard validates IR
- **Violation**: Any hook exceeding 8 ticks routes to cold path

## 2. Hot Path API (C)

### 2.1 Constants

- `KNHKS_TICK_BUDGET = 8`
- `KNHKS_NROWS = 8` (compile-time fixed)
- `KNHKS_ALIGN = 64` bytes

### 2.2 Operations (H_hot)

Must execute in ≤8 ticks p95:

- `KNHKS_OP_ASK_SP` - Subject-predicate existence
- `KNHKS_OP_COUNT_SP_GE` - Count >= k
- `KNHKS_OP_COUNT_SP_LE` - Count <= k
- `KNHKS_OP_COUNT_SP_EQ` - Count == k
- `KNHKS_OP_ASK_SPO` - Triple matching
- `KNHKS_OP_ASK_OP` - Reverse lookup (object-predicate)
- `KNHKS_OP_UNIQUE_SP` - Exactly one value
- `KNHKS_OP_COUNT_OP` - Object count >= k
- `KNHKS_OP_COUNT_OP_LE` - Object count <= k
- `KNHKS_OP_COUNT_OP_EQ` - Object count == k
- `KNHKS_OP_COMPARE_O_EQ` - Object == value
- `KNHKS_OP_COMPARE_O_GT` - Object > value
- `KNHKS_OP_COMPARE_O_LT` - Object < value
- `KNHKS_OP_COMPARE_O_GE` - Object >= value
- `KNHKS_OP_COMPARE_O_LE` - Object <= value
- `KNHKS_OP_CONSTRUCT8` - Fixed-template emit (≤8 triples)

**Excluded from hot path**: SELECT, JOIN, OPTIONAL, GROUP

### 2.3 Data Structures

- `knhks_context_t` - SoA arrays (S[], P[], O[]), 64-byte aligned
- `knhks_pred_run_t` - {pred, off, len} where len ≤ 8
- `knhks_hook_ir_t` - Branchless IR representation
- `knhks_receipt_t` - Timing + provenance receipt

### 2.4 Functions

- `knhks_init_ctx()` - Initialize context
- `knhks_pin_run()` - Set active predicate run (H guards len > 8)
- `knhks_eval_bool()` - Boolean evaluation (≤8 ticks)
- `knhks_eval_construct8()` - Fixed-template emit (≤8 ticks)
- `knhks_eval_batch8()` - Batch up to 8 hooks in Λ order
- `knhks_receipt_merge()` - Merge receipts via ⊕ (associative)
- `knhks_rd_ticks()` - Architecture-specific tick counter
- `knhks_ticks_hz()` - Ticks per second

### 2.5 Constraints

- No heap allocations in μ
- No branches in μ
- Fixed instruction count per operation
- Δ-slices must fit in L1 cache
- Fully unrolled SIMD for NROWS=8

## 3. Warm Path API (Rust)

### 3.1 FFI-Safe Types

- `Run` - {pred: u64, off: u64, len: u64}
- `Ctx` - FFI-safe context wrapper
- `Op` - Operation enum matching C API
- `Ir` - Hook IR structure
- `Receipt` - Timing + provenance receipt

### 3.2 Engine Wrapper

- `Engine::new()` - Validates Σ constraints (64B alignment, NROWS=8)
- `Engine::pin_run()` - Enforces H guard (len ≤ 8)
- `Engine::eval_bool()` - Safe wrapper for knhks_eval_bool
- `Engine::eval_construct8()` - Safe wrapper for CONSTRUCT8
- `Engine::eval_batch8()` - Batch execution with Λ ordering

### 3.3 Responsibilities

- Memory management (SoA allocation)
- Type enforcement (Σ validation)
- Cache warming
- Receipt aggregation (Π ⊕ merge)
- OTEL span creation
- Shard coordination

## 4. Cold Path API (Erlang - High-Level)

### 4.1 Core Functions

- `boot/1` - Initialize Σ and Q
- `connect/1` - Register Dark Matter 80/20 connector
- `cover/1` - Define cover over O (select S ⊂ O, shard runs len ≤ 8)
- `admit/1` - Submit Δ into O (typed, guarded)
- `reflex/1` - Declare hot reflex hook
- `epoch/1` - Plan deterministic epoch (τ ≤ 8, Λ ≺-total)
- `run/1` - Execute μ over O for epoch, return {A, Receipt}
- `route/1` - Install action route (A ports)
- `receipt/1` - Fetch receipt by ID
- `merge/1` - Merge receipts via Π ⊕
- `metrics/0` - OTEL-friendly metrics

### 4.2 Connector Specification

Each connector declares:

- `name` - Connector identifier
- `schema` - Σ IRI for type validation
- `source` - Source specification (Kafka, API, file, etc.)
- `map` - S/P/O/G mapping
- `guard` - Admission guards (max_batch, max_lag_ms, etc.)

### 4.3 Supervision Tree

OTP supervision with:

- `knhks_sigma` - Schema registry
- `knhks_q` - Invariant registry
- `knhks_ingest` - Delta ingestion
- `knhks_unrdf` - SPARQL/SHACL engine (cold path)
- `knhks_lockchain` - Provenance storage
- `knhks_bus` - Event bus
- `knhks_repl` - Replication
- `knhks_otel` - Observability
- `knhks_darkmatter` - 80/20 coverage tracking

## 5. Dark Matter 80/20 Connectors

### 5.1 Connector Framework

- Connector registration API
- Σ-typed validation
- Automatic SoA conversion
- Admission guards (H)
- Metrics and observability

### 5.2 Required Connectors (v1.0)

At minimum, framework + reference implementations for:

- **ERP/Finance**: SAP, NetSuite
- **CRM**: Salesforce, HubSpot
- **HRIS**: Workday
- **Infra/Ops**: AWS, Kubernetes, ServiceNow
- **Data Mesh**: Kafka, Snowflake, Delta Lake

**Note**: v1.0 includes connector framework + at least 2 reference implementations (e.g., Kafka + Salesforce)

### 5.3 Connector Requirements

- Convert structured inputs to RDF/SHACL graphs
- Produce SoA arrays for hot path
- Support streaming Δ ingestion
- Enforce Σ typing
- Emit receipts for all operations

## 6. ETL Pipeline

### 6.1 Pipeline Stages

1. **Ingest**: RDF/Turtle, JSON-LD, streaming triples
2. **Transform**: Typed by Σ, constrained by Q
3. **Load**: SoA-aligned arrays in L1 cache
4. **Reflex**: μ executes in ≤8 ticks per Δ
5. **Emit**: Actions (A) + Receipts → Lockchain + Downstream APIs

### 6.2 Input Ports (O Ports)

- RDF Stores (SPARQL endpoint / file ingest)
- Enterprise APIs (JSON-LD / GraphQL adapters)
- Event Buses (Kafka, NATS, MQTT) → Δ streams
- Sensors & Telemetry (OTLP / protobuf → SoA)

### 6.3 Output Ports (A Ports)

- Webhooks (HTTP POST)
- Kafka events (pub/sub)
- gRPC endpoints
- Lockchain (Git/Merkle tree)
- OTEL exporters (metrics/spans)

## 7. Provenance & Receipts

### 7.1 Receipt Structure

- `ticks` - Execution time (≤8)
- `lanes` - SIMD lanes used
- `span_id` - OTEL-compatible trace ID
- `a_hash` - Fragment toward hash(A) = hash(μ(O))

### 7.2 Receipt Properties

- URDNA2015 canonicalization + SHA-256
- Merkle-linked to Git lockchain
- Commute with pushouts and Γ
- Merge via ⊕ (associative, branchless)
- Equality implies action equality

### 7.3 Lockchain

- Git-based immutable audit log
- SHA3-256 Merkle root verification
- Receipt linking and querying
- Tamper detection

## 8. Ontology-Driven System (O_sys)

### 8.1 Self-Description

System logic defined as RDF triples, not hardcoded:

- Hooks, guards, epochs, runs defined in ontology
- μ evaluates O_sys same as domain O
- Changes are data deltas (Δμ_in_O), not code changes

### 8.2 O_sys Classes

- `knhks:Reflex` - ≤8-tick execution unit
- `knhks:Hook` - Entry point for reflex
- `knhks:Run` - Contiguous predicate window
- `knhks:Epoch` - Time slice (τ)
- `knhks:Guard` - Constraint that blocks execution
- `knhks:Receipt` - Provenance record
- `knhks:Span` - OTEL trace context
- `knhks:Policy` - Rule as triples

### 8.3 O_sys Properties

- `knhks:hasEpoch` - Time window constraint
- `knhks:hasGuard` - Guard controlling execution
- `knhks:emits` - Output artifact
- `knhks:operatesOn` - Input data run
- `knhks:preserves` - Invariant Q
- `knhks:execTime` - Measured latency
- `knhks:hashMatch` - Receipt verification

## 9. Observability

### 9.1 OpenTelemetry Integration

- Automatic span creation for all operations
- Metrics: p50, p95, drift, throughput
- Traces: Full request lifecycle
- Receipts link to OTEL spans

### 9.2 Metrics Required

- Hook execution latency (p50, p95)
- Cache hit rate
- Drift violations (>8 ticks)
- Coverage metrics (80/20 analysis)
- Receipt generation rate
- Connector throughput

## 10. Performance Requirements

### 10.1 Latency Bounds

- **Hot path**: p95 ≤ 2 ns (8 ticks) per hook
- **Warm path**: Coordination overhead minimal
- **Cold path**: Full SPARQL/SHACL (no bound)

### 10.2 Coverage Target

- ≥80% of enterprise queries qualify for hot path
- Dark Matter 80/20: smallest hook set achieving ≥80% coverage

### 10.3 Scalability

- Multi-core parallelism via shard law
- Lock-free concurrent execution
- Deterministic scaling: n cores → n× throughput

## 11. Testing & Validation

### 11.1 Test Requirements

- OTEL span-based validation runner
- Zero-mock E2E tests
- Micro-benchmarks for Chatman Constant verification
- Receipt verification tests
- Shard composition tests
- Glue correctness tests

### 11.2 Evidence Requirements

- Hot-path timing proofs (p95 ≤ 2 ns)
- OTEL spans as truth source
- Lockchain hash verification
- Zero-mock E2E coverage

## 12. Deployment

### 12.1 Target Platforms

- Linux (primary)
- macOS (development)
- Containerized runtime (Docker)

### 12.2 Build Requirements

- C compiler (GCC/Clang) with SIMD support
- Rust toolchain (latest stable)
- Erlang/OTP (v25+)
- CMake or Make build system

### 12.3 Dependencies

- SIMD libraries (ARM NEON / x86 AVX2)
- OpenTelemetry SDK
- RDF parsing libraries (for cold path)
- SPARQL engine (Comunica or equivalent)

## 13. Documentation

### 13.1 Required Documentation

- API reference (C, Rust, Erlang)
- Architecture diagrams (Mermaid)
- Performance benchmarks
- Connector development guide
- ETL pipeline guide
- Receipt verification guide
- O_sys ontology reference

### 13.2 Examples

- Basic ASK query
- COUNT aggregation
- CONSTRUCT8 template emit
- Connector registration
- Epoch execution
- Receipt merging

## 14. Out of Scope (v1.0)

- Full SPARQL SELECT in hot path
- JOIN operations in hot path
- Browser compatibility (server-side only)
- Distributed replication (single-node)
- GUI/UI components
- CLI tools (separate package)

---

**End State**: A = μ(O), μ∘μ = μ, preserve(Q), hash(A) = hash(μ(O)), τ ≤ 8 ticks.

