// core.c
// Core evaluation logic (v1.0)

#include "core.h"
#include "simd.h"
#include "clock.h"
#include <string.h>

// Update init_ctx to match v1.0 API (const pointers)
void knhks_init_ctx(knhks_context_t *ctx, const uint64_t *S, const uint64_t *P, const uint64_t *O)
{
  if (!ctx) return;
  ctx->S = S;
  ctx->P = P;
  ctx->O = O;
  ctx->triple_count = 0;
  ctx->run.pred = 0;
  ctx->run.off = 0;
  ctx->run.len = 0;
}

// Legacy alias
void knhks_init_context(knhks_context_t *ctx, uint64_t *S, uint64_t *P, uint64_t *O)
{
  knhks_init_ctx(ctx, (const uint64_t *)S, (const uint64_t *)P, (const uint64_t *)O);
}

// Evaluate boolean query (ASK, COUNT>=k, ASK_SPO)
int knhks_core_eval_bool(const knhks_context_t *ctx, const knhks_hook_ir_t *ir)
{
  // cost model ≤2 atoms: (filter by p-run) + (reduce eq S==s)
  if (ir->p != ctx->run.pred)
    return 0;

#if NROWS == 8
  // Use specialized unrolled versions for NROWS=8
  // For ASK SP queries, use optimized existence check
  if (ir->op == KNHKS_OP_ASK_SP)
    return knhks_eq64_exists_8(ctx->S, ctx->run.off, ir->s);

  // For ASK SPO queries, check both S and O
  if (ir->op == KNHKS_OP_ASK_SPO)
    return knhks_eq64_spo_exists_8(ctx->S, ctx->O, ctx->run.off, ir->s, ir->o);

  // For COUNT queries, use optimized count
  if (ir->op == KNHKS_OP_COUNT_SP_GE)
  {
    uint64_t cnt = knhks_eq64_count_8(ctx->S, ctx->run.off, ir->s);
    return cnt >= ir->k;
  }
#else
  // For ASK SP queries, use optimized existence check
  if (ir->op == KNHKS_OP_ASK_SP)
    return knhks_eq64_exists_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);

  // For ASK SPO queries, check both S and O
  if (ir->op == KNHKS_OP_ASK_SPO)
    return knhks_eq64_spo_exists_run(ctx->S, ctx->O, ctx->run.off, ctx->run.len, ir->s, ir->o);

  // For COUNT queries, use optimized count
  if (ir->op == KNHKS_OP_COUNT_SP_GE)
  {
    uint64_t cnt = knhks_eq64_count_run(ctx->S, ctx->run.off, ctx->run.len, ir->s);
    return cnt >= ir->k;
  }
#endif

  return 0;
}

// CONSTRUCT8: Fixed-template emit (≤8 triples)
// Emits (S[i], P_template, O_template) for matching lanes
// Returns number of lanes written, fills receipt
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
  // Match condition: ASK(S[i], ir->p) - subject exists in run
  const uint64_t *s_p = ctx->S + ctx->run.off;
  uint64_t mask = 0;
  int written = 0;
  
#if NROWS == 8
  // Check which lanes match (S[i] exists)
  // For simplicity, emit all lanes where S[i] != 0 (non-empty)
  // In practice, this would check against a specific predicate match
  for (int i = 0; i < 8 && i < (int)ctx->run.len; i++) {
    if (s_p[i] != 0) { // Match condition
      ir->out_S[written] = s_p[i];
      ir->out_P[written] = ir->p; // Template predicate
      ir->out_O[written] = ir->o; // Template object
      mask |= (1ULL << i);
      written++;
    }
  }
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

// Batch execution with Λ ordering (deterministic, ≤8 hooks)
// CONSTRUCT8 is inline in header, batch calls it
int knhks_eval_batch8(const knhks_context_t *ctx, knhks_hook_ir_t *irs, size_t n, knhks_receipt_t *rcpts)
{
  if (!ctx || !irs || n == 0 || n > KNHKS_NROWS)
    return 0;
  
  if (!rcpts)
    return 0;
  
  int executed = 0;
  
  // Λ ordering: execute hooks in deterministic order (by index)
  // Each hook executes independently, receipts merge via ⊕
  for (size_t i = 0; i < n; i++) {
    knhks_receipt_t rcpt = {0};
    int result = 0;
    
    if (irs[i].op == KNHKS_OP_CONSTRUCT8) {
      result = knhks_eval_construct8(ctx, &irs[i], &rcpt);
    } else {
      result = knhks_eval_bool(ctx, &irs[i], &rcpt);
    }
    
    rcpts[i] = rcpt;
    executed++;
    
    // If a hook fails validation (e.g., run.len > 8), stop
    if (rcpt.ticks > KNHKS_TICK_BUDGET && result == 0) {
      break; // Guard H blocks
    }
  }
  
  return executed;
}

// Evaluate SELECT query and return count of results (legacy, cold path)
size_t knhks_core_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir)
{
  if (ir->p != ctx->run.pred || ir->op != KNHKS_OP_SELECT_SP)
    return 0;

  if (!ir->select_out || ir->select_capacity == 0)
    return 0;

#if NROWS == 8
  // Use optimized unrolled version for NROWS=8
  return knhks_select_gather_8(ctx->S, ctx->O, ctx->run.off, ir->s, ir->select_out, ir->select_capacity);
#else
  return knhks_select_gather(ctx->S, ctx->O, ctx->run.off, ctx->run.len, ir->s, ir->select_out, ir->select_capacity);
#endif
}

