# API Reference

**Version**: v0.3.0 (Production-Ready)  
**Core Library**: v1.0.0 (API Stable)

## Public API

The KNKHS API is organized into modular headers for better maintainability:

### Header Structure

```
include/
├── knhks.h              # Main umbrella header (includes all components)
└── knhks/
    ├── types.h          # Type definitions (enums, structs, constants)
    ├── eval.h           # Query evaluation functions (eval_bool, eval_construct8)
    ├── receipts.h       # Receipt operations (receipt_merge)
    └── utils.h          # Utility functions (init_ctx, load_rdf, clock utilities)
```

**Usage**: Include only `knhks.h` - it automatically includes all sub-modules:
```c
#include "knhks.h"  // Includes all API components
```

### Types

#### knhks_op_t
Operation types for queries:
```c
typedef enum {
  KNHKS_OP_ASK_SP = 1,          // ASK existence check
  KNHKS_OP_COUNT_SP_GE = 2,     // COUNT >= k
  KNHKS_OP_ASK_SPO = 3,         // Triple matching
  KNHKS_OP_SELECT_SP = 4,       // SELECT query (exceeds 8 ticks)
  KNHKS_OP_COUNT_SP_LE = 5,     // COUNT <= k
  KNHKS_OP_COUNT_SP_EQ = 6,     // COUNT == k
  KNHKS_OP_ASK_OP = 7,          // ASK(O,P) - reverse lookup
  KNHKS_OP_UNIQUE_SP = 8,       // UNIQUE(S,P) - exactly one value
  KNHKS_OP_COUNT_OP = 9,        // COUNT(O,P) >= k
  KNHKS_OP_COUNT_OP_LE = 10,    // COUNT(O,P) <= k
  KNHKS_OP_COUNT_OP_EQ = 11,    // COUNT(O,P) == k
  KNHKS_OP_COMPARE_O_EQ = 12,   // O == value (exact match)
  KNHKS_OP_COMPARE_O_GT = 13,   // O > value (greater than)
  KNHKS_OP_COMPARE_O_LT = 14,   // O < value (less than)
  KNHKS_OP_COMPARE_O_GE = 15,   // O >= value (greater or equal)
  KNHKS_OP_COMPARE_O_LE = 16,   // O <= value (less or equal)
  KNHKS_OP_VALIDATE_DATATYPE_SP = 17,  // Validate datatype: Check if (s, p) has object matching datatype hash
  KNHKS_OP_VALIDATE_DATATYPE_SPO = 18, // Validate datatype: Check if (s, p, o) exists and o matches datatype hash
  KNHKS_OP_CONSTRUCT8 = 32      // CONSTRUCT8 - fixed-template emit
```

#### knhks_context_t
Context holding SoA arrays and metadata:
```c
typedef struct {
  const uint64_t *S;        // Subject array (KNHKS_ALIGN aligned, KNHKS_NROWS sized)
  const uint64_t *P;        // Predicate array
  const uint64_t *O;        // Object array
  size_t triple_count;      // Number of loaded triples
  knhks_pred_run_t run;     // Predicate run metadata
} knhks_context_t;
```

#### knhks_hook_ir_t
Query representation (Hook IR):
```c
typedef struct {
  knhks_op_t op;            // Operation type
  uint64_t s, p, o, k;      // Subject, predicate, object, threshold
  
  // For CONSTRUCT8 only: preallocated output spans (8 rows max)
  uint64_t *out_S;          // may be NULL for non-CONSTRUCT8
  uint64_t *out_P;
  uint64_t *out_O;
  uint64_t out_mask;        // per-lane bitmask result (returned by μ)
  
  // Legacy SELECT support (cold path only, not in hot v1.0)
  uint64_t *select_out;     // SELECT output buffer (NULL for ASK/COUNT)
  size_t select_capacity;   // SELECT buffer capacity
} knhks_hook_ir_t;
```

#### knhks_pred_run_t
Predicate run metadata:
```c
typedef struct {
  uint64_t pred;  // Predicate ID
  uint64_t off;   // Offset in arrays
  uint64_t len;   // Length (must be ≤8 for hot path, guard constraint)
} knhks_pred_run_t;
```

#### knhks_receipt_t
Receipt structure for timing and provenance (v0.3.0):
```c
typedef struct {
  uint32_t ticks;    // ≤ 8 (Chatman Constant: 2ns = 8 ticks)
  uint32_t lanes;    // SIMD width used
  uint64_t span_id;  // OTEL-compatible id (generated via knhks_generate_span_id())
  uint64_t a_hash;   // hash(A) = hash(μ(O)) fragment (provenance)
} knhks_receipt_t;
```

**Note**: All receipts are generated with real OTEL-compatible span IDs. No placeholders allowed.

## Functions

### Context Management

#### knhks_init_ctx
Initialize context with arrays:
```c
void knhks_init_ctx(knhks_context_t *ctx, const uint64_t *S, const uint64_t *P, const uint64_t *O);
```
- `ctx`: Context to initialize
- `S`, `P`, `O`: Arrays (must be 64-byte aligned, size NROWS)
- Legacy alias: `knhks_init_context` (deprecated)

#### knhks_pin_run
Set the active predicate run:
```c
static inline void knhks_pin_run(knhks_context_t *ctx, knhks_pred_run_t run);
```
- Sets the active predicate run for subsequent queries
- Guard: `run.len` must be ≤8

#### knhks_load_rdf
Load RDF file into context:
```c
int knhks_load_rdf(knhks_context_t *ctx, const char *filename);
```
- Returns: 1 on success, 0 on failure
- Automatically sets predicate run metadata
- Supports Turtle format

### Query Evaluation

#### knhks_eval_bool
Evaluate boolean query (inline, hot path):
```c
static inline int knhks_eval_bool(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, knhks_receipt_t *rcpt);
```
- Returns: 1 if true, 0 if false
- `rcpt`: Optional receipt pointer for timing/provenance (NULL if not needed)
- Inline function for zero-overhead hot path
- Fills `rcpt` with timing, span ID, and provenance hash when provided
- All operations ≤8 ticks (Chatman Constant: 2ns = 8 ticks)
- Supports all boolean operations:
  - `KNHKS_OP_ASK_SP`: Subject-predicate existence check
  - `KNHKS_OP_ASK_SPO`: Triple matching
  - `KNHKS_OP_COUNT_SP_GE`: Count >= k
  - `KNHKS_OP_COUNT_SP_LE`: Count <= k
  - `KNHKS_OP_COUNT_SP_EQ`: Count == k
  - `KNHKS_OP_ASK_OP`: Reverse lookup (object-predicate)
  - `KNHKS_OP_UNIQUE_SP`: Uniqueness check (count == 1)
  - `KNHKS_OP_COUNT_OP`: Object count >= k
  - `KNHKS_OP_COUNT_OP_LE`: Object count <= k
  - `KNHKS_OP_COUNT_OP_EQ`: Object count == k
  - `KNHKS_OP_COMPARE_O_EQ`: Object == value
  - `KNHKS_OP_COMPARE_O_GT`: Object > value
  - `KNHKS_OP_COMPARE_O_LT`: Object < value
  - `KNHKS_OP_COMPARE_O_GE`: Object >= value
  - `KNHKS_OP_COMPARE_O_LE`: Object <= value
  - `KNHKS_OP_VALIDATE_DATATYPE_SP`: Validate datatype for (s, p)
  - `KNHKS_OP_VALIDATE_DATATYPE_SPO`: Validate datatype for (s, p, o)

#### knhks_eval_construct8
Emit up to 8 triples using a fixed template (CONSTRUCT8):
```c
static inline int knhks_eval_construct8(const knhks_context_t *ctx, knhks_hook_ir_t *ir, knhks_receipt_t *rcpt);
```
- Returns: Number of lanes written
- `ir->out_S`, `ir->out_P`, `ir->out_O`: Preallocated output buffers (must be non-NULL)
- `ir->out_mask`: Set to per-lane bitmask result
- Hot path operation: ≤8 ticks

#### knhks_eval_batch8
Batch execution with deterministic order Λ:
```c
int knhks_eval_batch8(const knhks_context_t *ctx, knhks_hook_ir_t *irs, size_t n, knhks_receipt_t *rcpts);
```
- Returns: Number of hooks executed successfully
- `n`: Number of hooks (must be ≤8)
- `rcpts`: Array of receipts (one per hook)
- Deterministic execution order

#### knhks_eval_select
Evaluate SELECT query:
```c
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);
```
- Returns: Number of results written to `ir->select_out`
- **Performance**: ~8.17 ticks (p50), slight variance
- **Status**: Optimized for hot path
- **Scope**: Limited to max 4 results to fit within 8-tick budget
- **Note**: Most enterprise use cases only need 1-2 results
- **Note**: Cold path operation, exceeds 8-tick budget

### Benchmarking

#### knhks_bench_eval
Benchmark query execution:
```c
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters);
```
- Returns: Nanoseconds per operation
- Warms cache before measurement

### Receipt Generation (v0.3.0)

#### knhks_generate_span_id
Generate OTEL-compatible span ID:
```c
uint64_t knhks_generate_span_id(void);
```
- Returns: 64-bit OTEL-compatible span ID (non-zero)
- Uses FNV-1a hash with ticks and entropy
- Production-ready implementation (no placeholders)

#### knhks_generate_span_id_from_ticks
Generate span ID from existing ticks (faster):
```c
uint64_t knhks_generate_span_id_from_ticks(uint64_t ticks);
```
- Returns: 64-bit OTEL-compatible span ID
- Use this when you already have ticks value to avoid extra clock read
- Optimized for hot path: minimal overhead, branchless

#### knhks_receipt_merge
Merge receipts via ⊕ operation (associative, branchless):
```c
static inline knhks_receipt_t knhks_receipt_merge(knhks_receipt_t a, knhks_receipt_t b);
```
- Merges two receipts: max ticks, sum lanes, XOR span_id/a_hash
- Used for batch operations and receipt aggregation

### Clock Utilities

#### knhks_rd_ticks
Read CPU ticks:
```c
uint64_t knhks_rd_ticks(void);
```
- ARM64: Uses `cntvct_el0`
- x86_64: Uses `rdtsc`

#### knhks_ticks_hz
Get ticks per second:
```c
double knhks_ticks_hz(void);
```
- ARM64: Reads `cntfrq_el0`
- x86_64: Uses `CPU_GHZ` env var or defaults to 4.0 GHz

## Usage Examples

### Basic ASK Query
```c
#include "knhks.h"

// Allocate aligned arrays
uint64_t ALN S[NROWS], P[NROWS], O[NROWS];
knhks_context_t ctx;
knhks_init_context(&ctx, S, P, O);

// Load RDF data
knhks_load_rdf(&ctx, "data.ttl");

// Create ASK query
knhks_hook_ir_t ir = {
  .op = KNHKS_OP_ASK_SP,
  .s = ctx.S[0],
  .p = ctx.run.pred,
  .k = 0,
  .o = 0,
  .select_out = NULL,
  .select_capacity = 0
};

// Execute (without receipt)
int result = knhks_eval_bool(&ctx, &ir, NULL);

// Execute (with receipt for timing/provenance)
knhks_receipt_t rcpt = {0};
int result_with_receipt = knhks_eval_bool(&ctx, &ir, &rcpt);
printf("Result: %d, Ticks: %u, Hash: 0x%llx\n", result_with_receipt, rcpt.ticks, (unsigned long long)rcpt.a_hash);
```

### COUNT Query
```c
knhks_hook_ir_t count_ir = {
  .op = KNHKS_OP_COUNT_SP_GE,
  .s = subject_id,
  .p = predicate_id,
  .k = 1,  // Check if count >= 1
  .o = 0,
  .select_out = NULL,
  .select_capacity = 0
};

knhks_receipt_t rcpt = {0};
int has_at_least_one = knhks_eval_bool(&ctx, &count_ir, &rcpt);
```

### Triple Matching
```c
knhks_hook_ir_t spo_ir = {
  .op = KNHKS_OP_ASK_SPO,
  .s = subject_id,
  .p = predicate_id,
  .o = object_id,
  .k = 0,
  .select_out = NULL,
  .select_capacity = 0
};

knhks_receipt_t rcpt = {0};
int triple_exists = knhks_eval_bool(&ctx, &spo_ir, &rcpt);
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
- Receipt generation adds minimal overhead (~1-2 ticks)
- Span ID generation uses optimized path (no extra clock reads when ticks already available)

## Internal Structure

The API is implemented using modular headers:

- **`include/knhks/types.h`**: All type definitions (enums, structs, constants)
- **`include/knhks/eval.h`**: Query evaluation functions (inline, hot path)
- **`include/knhks/receipts.h`**: Receipt operations (merge, provenance)
- **`include/knhks/utils.h`**: Utility functions (context init, RDF loading, clock)

SIMD operations are organized in `src/simd/`:
- `src/simd/common.h`: Common infrastructure
- `src/simd/existence.h`: ASK operations
- `src/simd/count.h`: COUNT operations
- `src/simd/compare.h`: Comparison operations
- `src/simd/select.h`: SELECT operations
- `src/simd/validate.h`: Datatype validation
- `src/simd/construct.h`: CONSTRUCT8 operations

All SIMD functions are included via `src/simd.h` umbrella header.

