// knhks.h
// Public API for KNKHS 8-tick knowledge graph query system
// Branchless SIMD operations for sub-2 nanosecond query execution

#ifndef KNHKS_H
#define KNHKS_H

#include <stdint.h>
#include <stddef.h>

// Maximum rows that fit in 8-tick window (default: 8)
#ifndef NROWS
#define NROWS 8u
#endif

// Operation types
typedef enum {
  KNHKS_OP_ASK_SP = 1,
  KNHKS_OP_COUNT_SP_GE = 2,
  KNHKS_OP_ASK_SPO = 3,
  KNHKS_OP_SELECT_SP = 4,
  KNHKS_OP_COUNT_SP_LE = 5,  // COUNT <= k
  KNHKS_OP_COUNT_SP_EQ = 6,  // COUNT == k
  KNHKS_OP_ASK_OP = 7,        // ASK(O,P) - reverse lookup
  KNHKS_OP_UNIQUE_SP = 8,    // UNIQUE(S,P) - exactly one value
  KNHKS_OP_COUNT_OP = 9,      // COUNT(O,P) >= k - count object occurrences
  KNHKS_OP_COUNT_OP_LE = 10,  // COUNT(O,P) <= k
  KNHKS_OP_COUNT_OP_EQ = 11   // COUNT(O,P) == k
} knhks_op_t;

// Predicate run metadata
typedef struct {
  uint64_t pred, off, len;
} knhks_pred_run_t;

// Hook IR (query representation)
typedef struct {
  knhks_op_t op;
  uint64_t s, p, o, k;  // Subject, predicate, object, count threshold
  uint64_t *select_out; // Output buffer for SELECT
  size_t select_capacity;
} knhks_hook_ir_t;

// Context for data arrays (SoA layout)
typedef struct {
  uint64_t *S;  // Subject array
  uint64_t *P;  // Predicate array
  uint64_t *O;  // Object array
  size_t triple_count;
  knhks_pred_run_t run;
} knhks_context_t;

// Initialize context with arrays
void knhks_init_context(knhks_context_t *ctx, uint64_t *S, uint64_t *P, uint64_t *O);

// Load RDF file into context arrays
int knhks_load_rdf(knhks_context_t *ctx, const char *filename);

// Include SIMD headers for inline implementation
#include "simd.h"

// Evaluate boolean query (ASK, COUNT>=k, ASK_SPO)
// Inline for hot path performance - directly implements core logic
static inline int knhks_eval_bool(const knhks_context_t *ctx, const knhks_hook_ir_t *ir)
{
  // cost model ≤2 atoms: (filter by p-run) + (reduce eq S==s)
  if (ir->p != ctx->run.pred)
    return 0;

#if NROWS == 8
  // Use specialized unrolled versions for NROWS=8
  // Switch statement for better branch prediction
  switch (ir->op)
  {
    case KNHKS_OP_ASK_SP:
      return knhks_eq64_exists_8(ctx->S, ctx->run.off, ir->s);

    case KNHKS_OP_ASK_SPO:
      return knhks_eq64_spo_exists_8(ctx->S, ctx->O, ctx->run.off, ir->s, ir->o);

    case KNHKS_OP_COUNT_SP_GE:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
      return cnt >= ir->k;
    }

    case KNHKS_OP_COUNT_SP_LE:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
      return cnt <= ir->k;
    }

    case KNHKS_OP_COUNT_SP_EQ:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
      return cnt == ir->k;
    }

    case KNHKS_OP_ASK_OP:
      return knhks_eq64_exists_o_8(ctx->O, ctx->run.off, ir->o);

    case KNHKS_OP_UNIQUE_SP:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
      return cnt == 1;
    }

    case KNHKS_OP_COUNT_OP:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
      return cnt >= ir->k;
    }

    case KNHKS_OP_COUNT_OP_LE:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
      return cnt <= ir->k;
    }

    case KNHKS_OP_COUNT_OP_EQ:
    {
      uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
      return cnt == ir->k;
    }

    default:
      return 0;
  }
#else
  // General versions for other NROWS
  if (ir->op == KNHKS_OP_ASK_SP)
    return knhks_eq64_exists_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);

  if (ir->op == KNHKS_OP_ASK_SPO)
    return knhks_eq64_spo_exists_run(ctx->S, ctx->O, ctx->run.off, ctx->run.len, ir->s, ir->o);

  if (ir->op == KNHKS_OP_COUNT_SP_GE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    return cnt >= ir->k;
  }

  if (ir->op == KNHKS_OP_COUNT_SP_LE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    return cnt <= ir->k;
  }

  if (ir->op == KNHKS_OP_COUNT_SP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    return cnt == ir->k;
  }

  if (ir->op == KNHKS_OP_ASK_OP)
  {
    return knhks_eq64_exists_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
  }

  if (ir->op == KNHKS_OP_UNIQUE_SP)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    return cnt == 1;
  }

  if (ir->op == KNHKS_OP_COUNT_OP)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    return cnt >= ir->k;  // Count object occurrences >= k
  }

  if (ir->op == KNHKS_OP_COUNT_OP_LE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    return cnt <= ir->k;  // Count object occurrences <= k
  }

  if (ir->op == KNHKS_OP_COUNT_OP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    return cnt == ir->k;  // Count object occurrences == k
  }
#endif

  return 0;
}

// Evaluate SELECT query
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);

// Benchmark evaluation (returns nanoseconds per operation)
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters);

// Clock utilities
uint64_t knhks_rd_ticks(void);
double knhks_ticks_hz(void);

#endif // KNHKS_H

