// knhks.h
// Public API for KNKHS 8-tick knowledge graph query system (v1.0)
// Branchless SIMD operations for sub-2 nanosecond query execution
// KGC: A = μ(O), μ ⊂ τ, τ ≤ 8 ticks

#ifndef KNHKS_H
#define KNHKS_H

#include <stdint.h>
#include <stddef.h>

// v1.0 Constants
#define KNHKS_TICK_BUDGET 8u
#define KNHKS_NROWS       8u        // compile-time fixed
#define KNHKS_ALIGN       64u       // bytes

// Maximum rows that fit in 8-tick window (must be 8 for v1.0)
#ifndef NROWS
#define NROWS KNHKS_NROWS
#endif

#if NROWS != 8
#error "KNHKS_NROWS must be 8 for hot path v1."
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
  KNHKS_OP_COUNT_OP_EQ = 11,  // COUNT(O,P) == k
  KNHKS_OP_COMPARE_O_EQ = 12, // O == value (exact match)
  KNHKS_OP_COMPARE_O_GT = 13, // O > value (greater than)
  KNHKS_OP_COMPARE_O_LT = 14, // O < value (less than)
  KNHKS_OP_COMPARE_O_GE = 15, // O >= value (greater or equal)
  KNHKS_OP_COMPARE_O_LE = 16, // O <= value (less or equal)
  
  // v1.0 addition: hot emit (fixed-template, len ≤ 8)
  KNHKS_OP_CONSTRUCT8 = 32   // CONSTRUCT8 - fixed-template emit
} knhks_op_t;

// Predicate run metadata (len ≤ 8 for hot path)
typedef struct {
  uint64_t pred;  // P id
  uint64_t off;   // SoA offset
  uint64_t len;   // must be ≤ 8 (guarded by H)
} knhks_pred_run_t;

// Timing + provenance receipt (hot)
typedef struct {
  uint32_t ticks;    // ≤ 8
  uint32_t lanes;    // SIMD width used
  uint64_t span_id;  // OTEL-compatible id
  uint64_t a_hash;   // hash(A) = hash(μ(O)) fragment
} knhks_receipt_t;

// Hook IR (query representation) - branchless, constant-time
typedef struct {
  knhks_op_t op;
  uint64_t s, p, o, k;  // ids / thresholds
  
  // For CONSTRUCT8 only: preallocated output spans (8 rows max)
  uint64_t *out_S;      // may be NULL for non-CONSTRUCT8
  uint64_t *out_P;
  uint64_t *out_O;
  uint64_t out_mask;    // per-lane bitmask result (returned by μ)
  
  // Legacy SELECT support (cold path only, not in hot v1.0)
  uint64_t *select_out; // Output buffer for SELECT
  size_t select_capacity;
} knhks_hook_ir_t;

// Context for data arrays (SoA layout, 64-byte aligned)
typedef struct {
  const uint64_t *S;  // Subject array (KNHKS_ALIGN aligned, KNHKS_NROWS sized)
  const uint64_t *P;  // Predicate array
  const uint64_t *O;  // Object array
  size_t triple_count;
  knhks_pred_run_t run;
} knhks_context_t;

// Initialize context with arrays (precondition: arrays are KNHKS_ALIGN aligned)
void knhks_init_ctx(knhks_context_t *ctx, const uint64_t *S, const uint64_t *P, const uint64_t *O);

// Set the active predicate run (len ≤ 8, guarded by H)
static inline void knhks_pin_run(knhks_context_t *ctx, knhks_pred_run_t run)
{
  ctx->run = run;
}

// Legacy aliases for backward compatibility
#define knhks_init_context knhks_init_ctx

// Load RDF file into context arrays
int knhks_load_rdf(knhks_context_t *ctx, const char *filename);

// Clock function declaration (must come before inline functions that use it)
uint64_t knhks_rd_ticks(void);   // arch-specific

// Include SIMD headers for inline implementation
#include "simd.h"

// Combine receipts via ⊕ (associative, branchless)
static inline knhks_receipt_t knhks_receipt_merge(knhks_receipt_t a, knhks_receipt_t b)
{
  knhks_receipt_t merged;
  merged.ticks = (a.ticks > b.ticks) ? a.ticks : b.ticks; // max ticks
  merged.lanes = a.lanes + b.lanes; // sum lanes
  merged.span_id = a.span_id ^ b.span_id; // XOR merge
  merged.a_hash = a.a_hash ^ b.a_hash; // ⊕ merge (XOR)
  return merged;
}

// Evaluate boolean query (ASK, COUNT>=k, ASK_SPO)
// Inline for hot path performance - directly implements core logic
// Fills receipt with timing and provenance information
static inline int knhks_eval_bool(const knhks_context_t *ctx, knhks_hook_ir_t *ir, knhks_receipt_t *rcpt)
{
  uint64_t t0 = knhks_rd_ticks();
  
  // cost model ≤2 atoms: (filter by p-run) + (reduce eq S==s)
  if (ir->p != ctx->run.pred) {
    if (rcpt) {
      rcpt->ticks = (uint32_t)(knhks_rd_ticks() - t0);
      rcpt->lanes = 0;
      rcpt->span_id = 0;
      rcpt->a_hash = 0;
    }
    return 0;
  }

  int result = 0;
  
#if NROWS == 8
  // Use specialized unrolled versions for NROWS=8
  // Direct if-else chain (optimized by compiler) - most common operations first
  // This avoids switch overhead while maintaining good branch prediction
  if (ir->op == KNHKS_OP_ASK_SP)
    result = knhks_eq64_exists_8(ctx->S, ctx->run.off, ir->s);

  else if (ir->op == KNHKS_OP_ASK_SPO)
    result = knhks_eq64_spo_exists_8(ctx->S, ctx->O, ctx->run.off, ir->s, ir->o);

  else if (ir->op == KNHKS_OP_COUNT_SP_GE)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
    result = cnt >= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_SP_LE)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
    result = cnt <= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_SP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
    result = cnt == ir->k;
  }

  else if (ir->op == KNHKS_OP_ASK_OP)
    result = knhks_eq64_exists_o_8(ctx->O, ctx->run.off, ir->o);

  else if (ir->op == KNHKS_OP_UNIQUE_SP)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
    result = cnt == 1;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
    result = cnt >= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP_LE)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
    result = cnt <= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->O, ctx->run.off, ir->o);
    result = cnt == ir->k;
  }

  else if (ir->op == KNHKS_OP_COMPARE_O_EQ)
    result = knhks_compare_o_8(ctx->O, ctx->run.off, ir->o, 0);

  else if (ir->op == KNHKS_OP_COMPARE_O_GT)
    result = knhks_compare_o_8(ctx->O, ctx->run.off, ir->o, 1);

  else if (ir->op == KNHKS_OP_COMPARE_O_LT)
    result = knhks_compare_o_8(ctx->O, ctx->run.off, ir->o, 2);

  else if (ir->op == KNHKS_OP_COMPARE_O_GE)
    result = knhks_compare_o_8(ctx->O, ctx->run.off, ir->o, 3);

  else if (ir->op == KNHKS_OP_COMPARE_O_LE)
    result = knhks_compare_o_8(ctx->O, ctx->run.off, ir->o, 4);
#else
  // General versions for other NROWS (not supported in v1.0, but kept for compatibility)
  if (ir->op == KNHKS_OP_ASK_SP)
    result = knhks_eq64_exists_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);

  else if (ir->op == KNHKS_OP_ASK_SPO)
    result = knhks_eq64_spo_exists_run(ctx->S, ctx->O, ctx->run.off, ctx->run.len, ir->s, ir->o);

  else if (ir->op == KNHKS_OP_COUNT_SP_GE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    result = cnt >= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_SP_LE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    result = cnt <= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_SP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    result = cnt == ir->k;
  }

  else if (ir->op == KNHKS_OP_ASK_OP)
  {
    result = knhks_eq64_exists_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
  }

  else if (ir->op == KNHKS_OP_UNIQUE_SP)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    result = cnt == 1;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    result = cnt >= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP_LE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    result = cnt <= ir->k;
  }

  else if (ir->op == KNHKS_OP_COUNT_OP_EQ)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->O, ctx->run.off, ctx->run.len, ir->o);
    result = cnt == ir->k;
  }

  // Comparison operations (general versions)
  else if (ir->op == KNHKS_OP_COMPARE_O_EQ)
  {
    for (uint64_t i = 0; i < ctx->run.len; i++)
      if (ctx->O[ctx->run.off + i] == ir->o) {
        result = 1;
        break;
      }
  }
  
  else if (ir->op == KNHKS_OP_COMPARE_O_GT)
  {
    for (uint64_t i = 0; i < ctx->run.len; i++)
      if (ctx->O[ctx->run.off + i] > ir->o) {
        result = 1;
        break;
      }
  }
  
  else if (ir->op == KNHKS_OP_COMPARE_O_LT)
  {
    for (uint64_t i = 0; i < ctx->run.len; i++)
      if (ctx->O[ctx->run.off + i] < ir->o) {
        result = 1;
        break;
      }
  }
  
  else if (ir->op == KNHKS_OP_COMPARE_O_GE)
  {
    for (uint64_t i = 0; i < ctx->run.len; i++)
      if (ctx->O[ctx->run.off + i] >= ir->o) {
        result = 1;
        break;
      }
  }
  
  else if (ir->op == KNHKS_OP_COMPARE_O_LE)
  {
    for (uint64_t i = 0; i < ctx->run.len; i++)
      if (ctx->O[ctx->run.off + i] <= ir->o) {
        result = 1;
        break;
      }
  }
#endif

  // Fill receipt
  if (rcpt) {
    uint64_t t1 = knhks_rd_ticks();
    rcpt->ticks = (uint32_t)(t1 - t0);
    rcpt->lanes = KNHKS_NROWS;
    rcpt->span_id = 0; // TODO: Generate from OTEL
    // Simple hash fragment: hash(ir, result, ctx->run)
    rcpt->a_hash = (uint64_t)(ir->s ^ ir->p ^ ir->o ^ ir->k ^ (uint64_t)result ^ ctx->run.pred);
  }

  return result;
}

// Emit up to 8 triples using a fixed template (CONSTRUCT8)
// Returns number of lanes written, fills rcpt
static inline int knhks_eval_construct8(const knhks_context_t *ctx, knhks_hook_ir_t *ir, knhks_receipt_t *rcpt)
{
  if (!ctx || !ir || ir->op != KNHKS_OP_CONSTRUCT8)
    return 0;
  
  if (!ir->out_S || !ir->out_P || !ir->out_O)
    return 0;
  
  if (ir->p != ctx->run.pred)
    return 0;
  
  uint64_t t0 = knhks_rd_ticks();
  
  // For CONSTRUCT8: emit template (S[i], ir->p, ir->o) for matching lanes
  const uint64_t *s_p = ctx->S + ctx->run.off;
  uint64_t mask = 0;
  int written = 0;
  
#if NROWS == 8
  // Fully unrolled: emit all non-zero lanes
  if (s_p[0] != 0) { ir->out_S[written] = s_p[0]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 1ULL; written++; }
  if (s_p[1] != 0) { ir->out_S[written] = s_p[1]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 2ULL; written++; }
  if (s_p[2] != 0) { ir->out_S[written] = s_p[2]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 4ULL; written++; }
  if (s_p[3] != 0) { ir->out_S[written] = s_p[3]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 8ULL; written++; }
  if (s_p[4] != 0) { ir->out_S[written] = s_p[4]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 16ULL; written++; }
  if (s_p[5] != 0) { ir->out_S[written] = s_p[5]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 32ULL; written++; }
  if (s_p[6] != 0) { ir->out_S[written] = s_p[6]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 64ULL; written++; }
  if (s_p[7] != 0 && ctx->run.len > 7) { ir->out_S[written] = s_p[7]; ir->out_P[written] = ir->p; ir->out_O[written] = ir->o; mask |= 128ULL; written++; }
#else
  for (uint64_t i = 0; i < ctx->run.len && i < KNHKS_NROWS; i++) {
    if (s_p[i] != 0) {
      ir->out_S[written] = s_p[i];
      ir->out_P[written] = ir->p;
      ir->out_O[written] = ir->o;
      mask |= (1ULL << i);
      written++;
    }
  }
#endif
  
  ir->out_mask = mask;
  
  // Fill receipt
  if (rcpt) {
    uint64_t t1 = knhks_rd_ticks();
    rcpt->ticks = (uint32_t)(t1 - t0);
    rcpt->lanes = (uint32_t)written;
    rcpt->span_id = 0; // TODO: Generate from OTEL
    rcpt->a_hash = (uint64_t)(ir->s ^ ir->p ^ ir->o ^ ctx->run.pred ^ mask);
  }
  
  return written;
}

// Batch (vector of IRs) with deterministic order Λ; N ≤ 8, no joins
// Returns number of hooks executed successfully
// Implementation in core.c
int knhks_eval_batch8(const knhks_context_t *ctx, knhks_hook_ir_t *irs, size_t n, knhks_receipt_t *rcpts);

// Legacy SELECT query (cold path only, exceeds 8-tick budget)
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);

// Benchmark evaluation (returns nanoseconds per operation)
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters);

// Clock utilities (hot-safe)
uint64_t knhks_rd_ticks(void);   // arch-specific
double knhks_ticks_hz(void);     // arch-specific

#endif // KNHKS_H

