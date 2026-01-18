# KNHK v1.0 Requirements Document

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
- **Epoch**: μ ⊂ τ, τ ≤ 2ns - Hard time bound
- **Sparsity**: μ → S (80/20) - Dark Matter optimization
- **Minimality**: argmin drift(A) - Minimize state drift
- **Invariants**: preserve(Q) - Maintain invariants

### 1.2 Chatman Constant

- **Definition**: χ ≡ p95(hook eval) ≤ 2 ns
- **Enforcement**: AOT (Ahead-Of-Time) ingest guard validates IR
- **Violation**: Any hook exceeding 2ns routes to cold path
- **Measurement**: External timing by Rust framework (C hot path contains zero timing code)

## 2. Hot Path API (C)

### 2.1 Constants

- `KNHK_TIME_BUDGET_NS = 2.0` (Chatman Constant: 2ns)
- `KNHK_NROWS = 8` (compile-time fixed)
- `KNHK_ALIGN = 64` bytes

### 2.2 Operations (H_hot)

Must execute in ≤2ns p95:

- `KNHK_OP_ASK_SP` - Subject-predicate existence
- `KNHK_OP_COUNT_SP_GE` - Count >= k
- `KNHK_OP_COUNT_SP_LE` - Count <= k
- `KNHK_OP_COUNT_SP_EQ` - Count == k
- `KNHK_OP_ASK_SPO` - Triple matching
- `KNHK_OP_ASK_OP` - Reverse lookup (object-predicate)
- `KNHK_OP_UNIQUE_SP` - Exactly one value
- `KNHK_OP_COUNT_OP` - Object count >= k
- `KNHK_OP_COUNT_OP_LE` - Object count <= k
- `KNHK_OP_COUNT_OP_EQ` - Object count == k
- `KNHK_OP_COMPARE_O_EQ` - Object == value
- `KNHK_OP_COMPARE_O_GT` - Object > value
- `KNHK_OP_COMPARE_O_LT` - Object < value
- `KNHK_OP_COMPARE_O_GE` - Object >= value
- `KNHK_OP_COMPARE_O_LE` - Object <= value
- `KNHK_OP_CONSTRUCT8` - Fixed-template emit (≤8 triples)

**Excluded from hot path**: SELECT, JOIN, OPTIONAL, GROUP

### 2.3 Data Structures

- `knhk_context_t` - SoA arrays (S[], P[], O[]), 64-byte aligned
- `knhk_pred_run_t` - {pred, off, len} where len ≤ 8
- `knhk_hook_ir_t` - Branchless IR representation
- `knhk_receipt_t` - Provenance receipt (no timing, measured externally)

### 2.4 Functions

- `knhk_init_ctx()` - Initialize context
- `knhk_pin_run()` - Set active predicate run (H guards len > 8)
- `knhk_eval_bool()` - Boolean evaluation (≤2ns)
- `knhk_eval_construct8()` - Fixed-template emit (≤2ns)
- `knhk_eval_batch8()` - Batch up to 8 hooks in Λ order
- `knhk_receipt_merge()` - Merge receipts via ⊕ (associative)
- `knhk_generate_span_id()` - Generate OTEL-compatible span ID (no timing dependency)

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
- `Receipt` - Provenance receipt (no timing, measured externally)

### 3.2 Engine Wrapper

- `Engine::new()` - Validates Σ constraints (64B alignment, NROWS=8)
- `Engine::pin_run()` - Enforces H guard (len ≤ 8)
- `Engine::eval_bool()` - Safe wrapper for knhk_eval_bool
- `Engine::eval_construct8()` - Safe wrapper for CONSTRUCT8
- `Engine::eval_batch8()` - Batch execution with Λ ordering

### 3.3 Responsibilities

- Memory management (SoA allocation)
- Type enforcement (Σ validation)
- External timing measurement (cycle counters)
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
- `epoch/1` - Plan deterministic epoch (τ ≤ 2ns, Λ ≺-total)
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

- `knhk_sigma` - Schema registry
- `knhk_q` - Invariant registry
- `knhk_ingest` - Delta ingestion
- `knhk_unrdf` - SPARQL/SHACL engine (cold path)
- `knhk_lockchain` - Provenance storage
- `knhk_bus` - Event bus
- `knhk_repl` - Replication
- `knhk_otel` - Observability
- `knhk_darkmatter` - 80/20 coverage tracking

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
4. **Reflex**: μ executes in ≤2ns per Δ (measured externally)
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

- `lanes` - SIMD lanes used
- `span_id` - OTEL-compatible trace ID (no timing dependency)
- `a_hash` - Fragment toward hash(A) = hash(μ(O))

**Note**: Timing is measured externally by Rust framework, not stored in receipt.

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

- `knhk:Reflex` - ≤8-tick execution unit
- `knhk:Hook` - Entry point for reflex
- `knhk:Run` - Contiguous predicate window
- `knhk:Epoch` - Time slice (τ)
- `knhk:Guard` - Constraint that blocks execution
- `knhk:Receipt` - Provenance record
- `knhk:Span` - OTEL trace context
- `knhk:Policy` - Rule as triples

### 8.3 O_sys Properties

- `knhk:hasEpoch` - Time window constraint
- `knhk:hasGuard` - Guard controlling execution
- `knhk:emits` - Output artifact
- `knhk:operatesOn` - Input data run
- `knhk:preserves` - Invariant Q
- `knhk:execTime` - Measured latency
- `knhk:hashMatch` - Receipt verification

## 9. Observability

### 9.1 OpenTelemetry Integration

- Automatic span creation for all operations
- Metrics: p50, p95, drift, throughput
- Traces: Full request lifecycle
- Receipts link to OTEL spans

### 9.2 Metrics Required

- Hook execution latency (p50, p95)
- Cache hit rate
- Drift violations (>2ns)
- Coverage metrics (80/20 analysis)
- Receipt generation rate
- Connector throughput

## 10. Performance Requirements

### 10.1 Latency Bounds

- **Hot path**: p95 ≤ 2 ns per hook (measured externally by Rust)
- **Warm path**: Coordination overhead minimal
- **Cold path**: Full SPARQL/SHACL (no bound)
- **Note**: C hot path contains zero timing code

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

**End State**: A = μ(O), μ∘μ = μ, preserve(Q), hash(A) = hash(μ(O)), τ ≤ 2ns (measured externally).

