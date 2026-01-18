// knhk/utils.h
// Utility functions: context initialization, RDF loading, clock utilities

#ifndef KNHK_UTILS_H
#define KNHK_UTILS_H

#include "types.h"

// Initialize context with arrays (precondition: arrays are KNHK_ALIGN aligned)
void knhk_init_ctx(knhk_context_t *ctx, const uint64_t *S, const uint64_t *P, const uint64_t *O);

// Set the active predicate run (len ≤ 8, guarded by H)
static inline void knhk_pin_run(knhk_context_t *ctx, knhk_pred_run_t run)
{
  ctx->run = run;
}

// Legacy aliases for backward compatibility
#define knhk_init_context knhk_init_ctx

// Load RDF file into context arrays
int knhk_load_rdf(knhk_context_t *ctx, const char *filename);

// Generate OTEL-compatible span ID (no timing dependency)
uint64_t knhk_generate_span_id(void);

// Batch (vector of IRs) with deterministic order Λ; N ≤ 8, no joins
// Returns number of hooks executed successfully
// Implementation in core.c
int knhk_eval_batch8(const knhk_context_t *ctx, knhk_hook_ir_t *irs, size_t n, knhk_receipt_t *rcpts);

// Legacy SELECT query (cold path only, exceeds 2ns budget)
size_t knhk_eval_select(const knhk_context_t *ctx, const knhk_hook_ir_t *ir);

#endif // KNHK_UTILS_H

