# Architecture

## System Overview

KNHKS (v0.3.0) implements a multi-tier architecture with production-ready infrastructure:

1. **Hot Path Engine** (C) - ≤8 tick query execution
2. **Connector Framework** (Rust) - Enterprise data source integration
3. **ETL Pipeline** (Rust) - Ingest → Transform → Load → Reflex → Emit
4. **Reflexive Control Layer** (Erlang) - Schema, invariants, receipts, routing
5. **Observability** (OTEL) - Metrics, tracing, span generation

Queries route to either hot path (≤8 ticks) or cold path (full SPARQL engine) based on complexity and data characteristics.

## Core Components

### 1. Data Layer (SoA Layout)

Triples are stored in Structure-of-Arrays format:
- `S[]` - Subject array (64-byte aligned)
- `P[]` - Predicate array (64-byte aligned)
- `O[]` - Object array (64-byte aligned)

All arrays are 64-byte aligned for optimal cache line access and SIMD operations.

### 2. Connector Framework (v0.3.0)

**Dark Matter 80/20 Connector Framework**:
- Kafka connector with rdkafka integration
- Salesforce connector with reqwest integration
- HTTP, File, SAP connector support
- Circuit breaker pattern for resilience
- Health checking and metrics
- Guard validation (max_run_len ≤ 8, max_batch_size, max_lag_ms)

### 3. ETL Pipeline (v0.3.0)

**Five-Stage Pipeline**:
- **Ingest**: Connector polling, RDF/Turtle parsing, JSON-LD support
- **Transform**: Schema validation (O ⊨ Σ), IRI hashing (FNV-1a), typed triples
- **Load**: Predicate run grouping, SoA conversion, 64-byte alignment
- **Reflex**: Hot path execution (≤8 ticks), receipt generation, receipt merging (⊕)
- **Emit**: Lockchain writing (Merkle-linked), downstream APIs (webhooks, Kafka, gRPC)

### 4. Query Layer

**Hot Path** (≤8 ticks):
- Simple ASK queries
- COUNT queries (≤8 elements)
- Triple matching (S-P-O)
- Branchless SIMD execution
- Fully unrolled for NROWS=8

**Cold Path**:
- Complex queries (JOINs, OPTIONAL, UNION)
- Multi-predicate queries
- Full SPARQL compliance

### 5. Evaluation Layer

- **Hook IR**: Lightweight query representation
- **Context**: SoA arrays + predicate run metadata
- **SIMD Operations**: ARM NEON / x86 AVX2
- **Receipts**: Timing, provenance, span IDs (OTEL-compatible)

### 6. Reflexive Control Layer (v0.3.0)

**Erlang Supervision Tree**:
- **knhks_sigma**: Schema registry (Σ management)
- **knhks_q**: Invariant registry (Q constraints, preserve(Q))
- **knhks_ingest**: Delta ingestion (O ⊔ Δ)
- **knhks_lockchain**: Receipt storage (Merkle-linked)
- **knhks_hooks**: Hook installation and management
- **knhks_epoch**: Epoch scheduling (Λ ≺-total, τ ≤ 8)
- **knhks_route**: Action routing to downstream systems

## Architecture Diagram

See `architecture.mmd` for visual representation.

## Data Structures

### knhks_context_t
```c
typedef struct {
  uint64_t *S;           // Subject array
  uint64_t *P;           // Predicate array
  uint64_t *O;           // Object array
  size_t triple_count;   // Number of triples
  knhks_pred_run_t run;  // Predicate run metadata
} knhks_context_t;
```

### knhks_hook_ir_t
```c
typedef struct {
  knhks_op_t op;         // Operation type
  uint64_t s, p, o, k;   // Subject, predicate, object, threshold
  uint64_t *select_out;  // SELECT output buffer
  size_t select_capacity;
} knhks_hook_ir_t;
```

## Execution Flow

### Basic Query Flow
1. **RDF Loading**: Parse RDF/Turtle files → SoA arrays
2. **Predicate Run Detection**: Group triples by predicate (len ≤ 8)
3. **Query Compilation**: SPARQL → Hook IR
4. **Path Selection**: Hot path vs cold path routing
5. **Evaluation**: Branchless SIMD execution
6. **Result Return**: Boolean or count result

### Enterprise Pipeline Flow (v0.3.0)
1. **Connect**: Register connectors (Kafka, Salesforce, etc.)
2. **Ingest**: Poll connectors → Raw triples
3. **Transform**: Validate against Σ schema → Typed triples (IRI → u64)
4. **Load**: Group by predicate → SoA arrays (64-byte aligned)
5. **Reflex**: Execute hooks (μ) → Actions (A) + Receipts
6. **Emit**: Write receipts to lockchain → Send actions to downstream APIs
7. **Provenance**: hash(A) = hash(μ(O)) verified via receipts

## Hot Path Requirements

- Predicate run size ≤8 elements (guard constraint enforced)
- Simple operations (ASK, COUNT, triple match)
- Data hot in L1 cache
- Single predicate queries
- Branchless operations (constant-time execution)
- ≤8 ticks (Chatman Constant: 2ns = 8 ticks)

## Cold Path Fallback

Queries that exceed hot path constraints automatically fall back to full SPARQL engine execution.

## Production Infrastructure (v0.3.0)

### Connector Framework
- Real library integrations (rdkafka, reqwest)
- Circuit breaker pattern for resilience
- Health checking and metrics
- Guard validation (max_run_len ≤ 8, max_batch_size, max_lag_ms)

### ETL Pipeline
- Production-ready stages (Ingest, Transform, Load, Reflex, Emit)
- Schema validation (O ⊨ Σ)
- Invariant checking (preserve(Q))
- Receipt generation and merging (⊕)

### Erlang Reflexive Control
- Schema registry (knhks_sigma)
- Invariant management (knhks_q)
- Delta ingestion (knhks_ingest)
- Lockchain (knhks_lockchain) with Merkle linking

### Observability
- OTEL span ID generation (not placeholders)
- Metrics collection
- Tracing support
- Receipt provenance tracking

