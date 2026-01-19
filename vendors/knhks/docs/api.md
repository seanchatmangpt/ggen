# API Reference

**Version**: v0.4.0 (Production-Ready)  
**Core Library**: v1.0.0 (API Stable)

**Critical Design**: C hot path contains **zero timing code**. All timing measurements are performed externally by the Rust framework.

## Public API

The KNKHS API is organized into modular headers for better maintainability:

### Header Structure

```
include/
├── knhk.h              # Main umbrella header (includes all components)
└── knhk/
    ├── types.h          # Type definitions (enums, structs, constants)
    ├── eval.h           # Query evaluation functions (eval_bool, eval_construct8)
    ├── receipts.h       # Receipt operations (receipt_merge)
    └── utils.h          # Utility functions (init_ctx, load_rdf, span ID generation)
```

**Usage**: Include only `knhk.h` - it automatically includes all sub-modules:
```c
#include "knhk.h"  // Includes all API components
```

### Types

#### knhk_op_t
Operation types for queries:
```c
typedef enum {
  KNHK_OP_ASK_SP = 1,          // ASK existence check
  KNHK_OP_COUNT_SP_GE = 2,     // COUNT >= k
  KNHK_OP_ASK_SPO = 3,         // Triple matching
  KNHK_OP_SELECT_SP = 4,       // SELECT query (cold path)
  KNHK_OP_COUNT_SP_LE = 5,     // COUNT <= k
  KNHK_OP_COUNT_SP_EQ = 6,     // COUNT == k
  KNHK_OP_ASK_OP = 7,          // ASK(O,P) - reverse lookup
  KNHK_OP_UNIQUE_SP = 8,       // UNIQUE(S,P) - exactly one value
  KNHK_OP_COUNT_OP = 9,        // COUNT(O,P) >= k
  KNHK_OP_COUNT_OP_LE = 10,    // COUNT(O,P) <= k
  KNHK_OP_COUNT_OP_EQ = 11,    // COUNT(O,P) == k
  KNHK_OP_COMPARE_O_EQ = 12,   // O == value (exact match)
  KNHK_OP_COMPARE_O_GT = 13,   // O > value (greater than)
  KNHK_OP_COMPARE_O_LT = 14,   // O < value (less than)
  KNHK_OP_COMPARE_O_GE = 15,   // O >= value (greater or equal)
  KNHK_OP_COMPARE_O_LE = 16,   // O <= value (less or equal)
  KNHK_OP_VALIDATE_DATATYPE_SP = 17,  // Validate datatype: Check if (s, p) has object matching datatype hash
  KNHK_OP_VALIDATE_DATATYPE_SPO = 18, // Validate datatype: Check if (s, p, o) exists and o matches datatype hash
  KNHK_OP_CONSTRUCT8 = 32      // CONSTRUCT8 - fixed-template emit
```

#### knhk_context_t
Context holding SoA arrays and metadata:
```c
typedef struct {
  const uint64_t *S;        // Subject array (KNHK_ALIGN aligned, KNHK_NROWS sized)
  const uint64_t *P;        // Predicate array
  const uint64_t *O;        // Object array
  size_t triple_count;      // Number of loaded triples
  knhk_pred_run_t run;     // Predicate run metadata
} knhk_context_t;
```

#### knhk_hook_ir_t
Query representation (Hook IR):
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
  uint64_t *select_out;     // SELECT output buffer (NULL for ASK/COUNT)
  size_t select_capacity;   // SELECT buffer capacity
} knhk_hook_ir_t;
```

#### knhk_pred_run_t
Predicate run metadata:
```c
typedef struct {
  uint64_t pred;  // Predicate ID
  uint64_t off;   // Offset in arrays
  uint64_t len;   // Length (must be ≤8 for hot path, guard constraint)
} knhk_pred_run_t;
```

#### knhk_receipt_t
Receipt structure for provenance (no timing):
```c
typedef struct {
  uint32_t lanes;    // SIMD width used
  uint64_t span_id;  // OTEL-compatible id (generated via knhk_generate_span_id())
  uint64_t a_hash;   // hash(A) = hash(μ(O)) fragment (provenance)
} knhk_receipt_t;
```

**Note**: Receipts contain provenance only. Timing is measured externally by Rust framework.

## Functions

### Context Management

#### knhk_init_ctx
Initialize context with arrays:
```c
void knhk_init_ctx(knhk_context_t *ctx, const uint64_t *S, const uint64_t *P, const uint64_t *O);
```
- `ctx`: Context to initialize
- `S`, `P`, `O`: Arrays (must be 64-byte aligned, size NROWS)
- Legacy alias: `knhk_init_context` (deprecated)

#### knhk_pin_run
Set the active predicate run:
```c
static inline void knhk_pin_run(knhk_context_t *ctx, knhk_pred_run_t run);
```
- Sets the active predicate run for subsequent queries
- Guard: `run.len` must be ≤8

#### knhk_load_rdf
Load RDF file into context:
```c
int knhk_load_rdf(knhk_context_t *ctx, const char *filename);
```
- Returns: 1 on success, 0 on failure
- Automatically sets predicate run metadata
- Supports Turtle format

### Query Evaluation

#### knhk_eval_bool
Evaluate boolean query (inline, hot path):
```c
static inline int knhk_eval_bool(const knhk_context_t *ctx, const knhk_hook_ir_t *ir, knhk_receipt_t *rcpt);
```
- Returns: 1 if true, 0 if false
- `rcpt`: Optional receipt pointer for provenance (NULL if not needed)
- Inline function for zero-overhead hot path
- Fills `rcpt` with span ID and provenance hash when provided
- All operations ≤2ns (Chatman Constant)
- **Zero timing overhead**: Pure CONSTRUCT logic only
- **Timing validation**: Performed externally by Rust framework
- Supports all boolean operations:
  - `KNHK_OP_ASK_SP`: Subject-predicate existence check
  - `KNHK_OP_ASK_SPO`: Triple matching
  - `KNHK_OP_COUNT_SP_GE`: Count >= k
  - `KNHK_OP_COUNT_SP_LE`: Count <= k
  - `KNHK_OP_COUNT_SP_EQ`: Count == k
  - `KNHK_OP_ASK_OP`: Reverse lookup (object-predicate)
  - `KNHK_OP_UNIQUE_SP`: Uniqueness check (count == 1)
  - `KNHK_OP_COUNT_OP`: Object count >= k
  - `KNHK_OP_COUNT_OP_LE`: Object count <= k
  - `KNHK_OP_COUNT_OP_EQ`: Object count == k
  - `KNHK_OP_COMPARE_O_EQ`: Object == value
  - `KNHK_OP_COMPARE_O_GT`: Object > value
  - `KNHK_OP_COMPARE_O_LT`: Object < value
  - `KNHK_OP_COMPARE_O_GE`: Object >= value
  - `KNHK_OP_COMPARE_O_LE`: Object <= value
  - `KNHK_OP_VALIDATE_DATATYPE_SP`: Validate datatype for (s, p)
  - `KNHK_OP_VALIDATE_DATATYPE_SPO`: Validate datatype for (s, p, o)

#### knhk_eval_construct8
Emit up to 8 triples using a fixed template (CONSTRUCT8):
```c
static inline int knhk_eval_construct8(const knhk_context_t *ctx, knhk_hook_ir_t *ir, knhk_receipt_t *rcpt);
```
- Returns: Number of lanes written
- `ir->out_S`, `ir->out_P`, `ir->out_O`: Preallocated output buffers (must be non-NULL)
- `ir->out_mask`: Set to per-lane bitmask result
- Hot path operation: ≤2ns (pure CONSTRUCT logic)

#### knhk_eval_batch8
Batch execution with deterministic order Λ:
```c
int knhk_eval_batch8(const knhk_context_t *ctx, knhk_hook_ir_t *irs, size_t n, knhk_receipt_t *rcpts);
```
- Returns: Number of hooks executed successfully
- `n`: Number of hooks (must be ≤8)
- `rcpts`: Array of receipts (one per hook)
- Deterministic execution order

#### knhk_eval_select
Evaluate SELECT query:
```c
size_t knhk_eval_select(const knhk_context_t *ctx, const knhk_hook_ir_t *ir);
```
- Returns: Number of results written to `ir->select_out`
- **Performance**: ~1.0-1.4 ns when measured externally
- **Status**: Optimized for hot path
- **Scope**: Limited to max 4 results to fit within 2ns budget
- **Note**: Most enterprise use cases only need 1-2 results
- **Note**: Cold path operation

### Receipt Generation

#### knhk_generate_span_id
Generate OTEL-compatible span ID:
```c
uint64_t knhk_generate_span_id(void);
```
- Returns: 64-bit OTEL-compatible span ID (non-zero)
- Uses counter-based approach with mixing for uniqueness
- Production-ready implementation (no placeholders)
- **No timing dependency**: Uses internal counter, not clock

#### knhk_receipt_merge
Merge receipts via ⊕ operation (associative, branchless):
```c
static inline knhk_receipt_t knhk_receipt_merge(knhk_receipt_t a, knhk_receipt_t b);
```
- Merges two receipts: sum lanes, XOR span_id/a_hash
- Used for batch operations and receipt aggregation
- **No timing in receipts**: Timing measured externally by Rust

### Clock Utilities

**Note**: Clock utilities have been removed from C code. All timing measurements are performed externally by the Rust framework using cycle counters.

For timing measurements, use the Rust framework which:
- Measures timing around C hot path calls
- Validates ≤2ns budget
- Provides performance statistics

## Usage Examples

### Basic ASK Query
```c
#include "knhk.h"

// Allocate aligned arrays
uint64_t ALN S[NROWS], P[NROWS], O[NROWS];
knhk_context_t ctx;
knhk_init_context(&ctx, S, P, O);

// Load RDF data
knhk_load_rdf(&ctx, "data.ttl");

// Create ASK query
knhk_hook_ir_t ir = {
  .op = KNHK_OP_ASK_SP,
  .s = ctx.S[0],
  .p = ctx.run.pred,
  .k = 0,
  .o = 0,
  .select_out = NULL,
  .select_capacity = 0
};

// Execute (without receipt)
int result = knhk_eval_bool(&ctx, &ir, NULL);

// Execute (with receipt for timing/provenance)
knhk_receipt_t rcpt = {0};
int result_with_receipt = knhk_eval_bool(&ctx, &ir, &rcpt);
printf("Result: %d, Lanes: %u, Hash: 0x%llx\n", result_with_receipt, rcpt.lanes, (unsigned long long)rcpt.a_hash);
```

### COUNT Query
```c
knhk_hook_ir_t count_ir = {
  .op = KNHK_OP_COUNT_SP_GE,
  .s = subject_id,
  .p = predicate_id,
  .k = 1,  // Check if count >= 1
  .o = 0,
  .select_out = NULL,
  .select_capacity = 0
};

knhk_receipt_t rcpt = {0};
int has_at_least_one = knhk_eval_bool(&ctx, &count_ir, &rcpt);
```

### Triple Matching
```c
knhk_hook_ir_t spo_ir = {
  .op = KNHK_OP_ASK_SPO,
  .s = subject_id,
  .p = predicate_id,
  .o = object_id,
  .k = 0,
  .select_out = NULL,
  .select_capacity = 0
};

knhk_receipt_t rcpt = {0};
int triple_exists = knhk_eval_bool(&ctx, &spo_ir, &rcpt);
```

## Compilation Constants

- `NROWS`: Maximum rows per predicate run (default: 8)
- Must be ≤8 for hot path optimization
- Enables fully unrolled SIMD when NROWS==8

## Performance Notes

- All hot path functions are inline for zero overhead
- SIMD functions are header-only inline (NROWS==8)
- Context must be initialized before use
- Arrays must be 64-byte aligned
- **Zero timing overhead**: C hot path contains pure CONSTRUCT logic only
- **External timing**: Rust framework measures timing around C calls
- Span ID generation uses counter-based approach (no timing dependency)

## Internal Structure

The API is implemented using modular headers:

- **`include/knhk/types.h`**: All type definitions (enums, structs, constants)
- **`include/knhk/eval.h`**: Query evaluation functions (inline, hot path)
- **`include/knhk/receipts.h`**: Receipt operations (merge, provenance)
- **`include/knhk/utils.h`**: Utility functions (context init, RDF loading, span ID generation)

SIMD operations are organized in `src/simd/`:
- `src/simd/common.h`: Common infrastructure
- `src/simd/existence.h`: ASK operations
- `src/simd/count.h`: COUNT operations
- `src/simd/compare.h`: Comparison operations
- `src/simd/select.h`: SELECT operations
- `src/simd/validate.h`: Datatype validation
- `src/simd/construct.h`: CONSTRUCT8 operations

All SIMD functions are included via `src/simd.h` umbrella header.

