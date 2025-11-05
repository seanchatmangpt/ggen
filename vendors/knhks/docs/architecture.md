# Architecture

## System Overview

KNKHS implements a two-tier architecture that routes queries to either a hot path (≤8 ticks) or cold path (full SPARQL engine) based on query complexity and data characteristics.

## Core Components

### 1. Data Layer (SoA Layout)

Triples are stored in Structure-of-Arrays format:
- `S[]` - Subject array
- `P[]` - Predicate array  
- `O[]` - Object array

All arrays are 64-byte aligned for optimal cache line access.

### 2. Query Layer

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

### 3. Evaluation Layer

- **Hook IR**: Lightweight query representation
- **Context**: SoA arrays + predicate run metadata
- **SIMD Operations**: ARM NEON / x86 AVX2

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

1. **RDF Loading**: Parse RDF/Turtle files → SoA arrays
2. **Predicate Run Detection**: Group triples by predicate
3. **Query Compilation**: SPARQL → Hook IR
4. **Path Selection**: Hot path vs cold path routing
5. **Evaluation**: Branchless SIMD execution
6. **Result Return**: Boolean or count result

## Hot Path Requirements

- Predicate run size ≤8 elements
- Simple operations (ASK, COUNT, triple match)
- Data hot in L1 cache
- Single predicate queries

## Cold Path Fallback

Queries that exceed hot path constraints automatically fall back to full SPARQL engine execution.

