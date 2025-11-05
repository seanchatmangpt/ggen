// core.c
// Core evaluation logic

#include "core.h"
#include "simd.h"

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

// Evaluate SELECT query and return count of results
size_t knhks_core_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir)
{
  if (ir->p != ctx->run.pred || ir->op != KNHKS_OP_SELECT_SP)
    return 0;

  if (!ir->select_out || ir->select_capacity == 0)
    return 0;

  return knhks_select_gather(ctx->S, ctx->O, ctx->run.off, ctx->run.len, ir->s, ir->select_out, ir->select_capacity);
}

