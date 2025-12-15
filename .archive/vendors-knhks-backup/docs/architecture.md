# Architecture

## System Overview

KNHK (v0.3.0) implements a multi-tier architecture with production-ready infrastructure:

1. **Hot Path Engine** (C) - ≤8 tick query execution
2. **Connector Framework** (Rust) - Enterprise data source integration
3. **ETL Pipeline** (Rust) - Ingest → Transform → Load → Reflex → Emit
4. **Reflexive Control Layer** (Erlang) - Schema, invariants, receipts, routing
5. **Observability** (OTEL) - Metrics, tracing, span generation

Queries route to either hot path (≤8 ticks) or cold path (full SPARQL engine) based on complexity and data characteristics.

## Modular Code Organization

### Header Structure

The public API is organized into modular headers for maintainability:

```
include/
├── knhk.h              # Main umbrella header (includes all components)
└── knhk/
    ├── types.h          # Type definitions (enums, structs, constants)
    ├── eval.h           # Query evaluation functions (eval_bool, eval_construct8)
    ├── receipts.h       # Receipt operations (receipt_merge)
    └── utils.h          # Utility functions (init_ctx, load_rdf, clock utilities)
```

**Usage**: Include only `knhk.h` - it automatically includes all sub-modules:
```c
#include "knhk.h"  // Includes all API components
```

### Source Structure

SIMD operations are organized into focused modules:

```
src/
├── simd.h               # SIMD umbrella header (includes all SIMD modules)
├── simd/
│   ├── common.h         # Common infrastructure (includes, declarations)
│   ├── existence.h      # ASK operations (exists_8, exists_o_8, spo_exists_8)
│   ├── count.h          # COUNT operations (count_8)
│   ├── compare.h        # Comparison operations (compare_o_8)
│   ├── select.h         # SELECT operations (select_gather_8)
│   ├── validate.h       # Datatype validation (validate_datatype_sp_8)
│   └── construct.h      # CONSTRUCT8 operations (construct8_emit_8)
├── simd.c               # Variable-length SIMD implementations
├── core.c               # Core operations (batch execution)
├── rdf.c                # RDF parsing (Turtle format)
└── clock.c              # Timing utilities and span ID generation
```

### Test Structure

Tests are organized into focused suites:

```
tests/
├── chicago_enterprise_use_cases.c    # Main enterprise test runner
├── chicago_basic_operations.c        # Basic operations (tests 1,2,5)
├── chicago_cardinality.c             # Cardinality tests (tests 3,6,7,9)
├── chicago_object_operations.c       # Object operations (tests 8,10,11,12)
├── chicago_advanced.c                # Advanced operations (tests 4,13-19)
├── chicago_test_helpers.c           # Shared test infrastructure
├── chicago_test_helpers.h            # Test helper declarations
├── chicago_v1_test.c                 # v1.0 test runner
├── chicago_v1_receipts.c             # Receipt tests
├── chicago_v1_operations.c            # Operation tests (CONSTRUCT8, batch)
├── chicago_v1_validation.c           # Validation tests (guards, constants)
├── chicago_integration_v2.c          # Integration test runner
├── chicago_integration_core.c        # Core integration tests
├── chicago_integration_systems.c     # System integration (lockchain, OTEL)
└── chicago_integration_advanced.c    # Advanced integration tests
```

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
- CONSTRUCT8 operations
- Batch execution (≤8 hooks)

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
- **knhk_sigma**: Schema registry (Σ management)
- **knhk_q**: Invariant registry (Q constraints, preserve(Q))
- **knhk_ingest**: Delta ingestion (O ⊔ Δ)
- **knhk_lockchain**: Receipt storage (Merkle-linked)
- **knhk_hooks**: Hook installation and management
- **knhk_epoch**: Epoch scheduling (Λ ≺-total, τ ≤ 8)
- **knhk_route**: Action routing to downstream systems

## Architecture Diagram

See `architecture.mmd` for visual representation.

## Data Structures

### knhk_context_t
```c
typedef struct {
  const uint64_t *S;        // Subject array (KNHK_ALIGN aligned, KNHK_NROWS sized)
  const uint64_t *P;        // Predicate array
  const uint64_t *O;        // Object array
  size_t triple_count;      // Number of loaded triples
  knhk_pred_run_t run;     // Predicate run metadata
} knhk_context_t;
```

### knhk_hook_ir_t
```c
typedef struct {
  knhk_op_t op;            // Operation type
  uint64_t s, p, o, k;      // Subject, predicate, object, threshold
  
  // For CONSTRUCT8 only: preallocated output spans (8 rows max)
  uint64_t *out_S;          // may be NULL for non-CONSTRUCT8
  uint64_t *out_P;
  uint64_t *out_O;
  uint64_t out_mask;        // per-lane bitmask result (returned by μ)
  
  // Legacy SELECT support (cold path only, not in hot v1.0)
  uint64_t *select_out;     // SELECT output buffer
  size_t select_capacity;
} knhk_hook_ir_t;
```

### knhk_receipt_t
```c
typedef struct {
  uint32_t ticks;    // ≤ 8
  uint32_t lanes;    // SIMD width used
  uint64_t span_id;  // OTEL-compatible id
  uint64_t a_hash;   // hash(A) = hash(μ(O)) fragment
} knhk_receipt_t;
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
- Schema registry (knhk_sigma)
- Invariant management (knhk_q)
- Delta ingestion (knhk_ingest)
- Lockchain (knhk_lockchain) with Merkle linking

### Observability
- OTEL span ID generation (not placeholders)
- Metrics collection
- Tracing support
- Receipt provenance tracking

## Modular Design Benefits

- **Maintainability**: Clear separation of concerns
- **Testability**: Focused test suites for each component
- **Performance**: Hot path isolated in inline headers
- **Extensibility**: Easy to add new operations or test suites
- **Code Size**: Reduced compilation unit sizes improve build times

