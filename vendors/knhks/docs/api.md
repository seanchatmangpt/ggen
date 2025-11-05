# API Reference

## Public API

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
  uint64_t *S;           // Subject array (must be NROWS aligned)
  uint64_t *P;           // Predicate array
  uint64_t *O;           // Object array
  size_t triple_count;   // Number of loaded triples
  knhks_pred_run_t run;  // Predicate run metadata
} knhks_context_t;
```

#### knhks_hook_ir_t
Query representation (Hook IR):
```c
typedef struct {
  knhks_op_t op;         // Operation type
  uint64_t s, p, o, k;   // Subject, predicate, object, threshold
  uint64_t *select_out;  // SELECT output buffer (NULL for ASK/COUNT)
  size_t select_capacity; // SELECT buffer capacity
} knhks_hook_ir_t;
```

#### knhks_pred_run_t
Predicate run metadata:
```c
typedef struct {
  uint64_t pred;  // Predicate ID
  uint64_t off;   // Offset in arrays
  uint64_t len;   // Length (must be ≤8 for hot path)
} knhks_pred_run_t;
```

## Functions

### Context Management

#### knhks_init_context
Initialize context with arrays:
```c
void knhks_init_context(knhks_context_t *ctx, uint64_t *S, uint64_t *P, uint64_t *O);
```
- `ctx`: Context to initialize
- `S`, `P`, `O`: Arrays (must be 64-byte aligned, size NROWS)

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
static inline int knhks_eval_bool(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);
```
- Returns: 1 if true, 0 if false
- Inline function for zero-overhead hot path
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

#### knhks_eval_select
Evaluate SELECT query:
```c
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);
```
- Returns: Number of results written to `ir->select_out`
- **Performance**: 3.83 ticks (p50), 5.74 ticks (p95)
- **Status**: Optimized for hot path
- **Scope**: Limited to max 4 results to fit within 8-tick budget
- **Note**: Most enterprise use cases only need 1-2 results

### Benchmarking

#### knhks_bench_eval
Benchmark query execution:
```c
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters);
```
- Returns: Nanoseconds per operation
- Warms cache before measurement

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

// Execute
int result = knhks_eval_bool(&ctx, &ir);
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

int has_at_least_one = knhks_eval_bool(&ctx, &count_ir);
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

int triple_exists = knhks_eval_bool(&ctx, &spo_ir);
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

