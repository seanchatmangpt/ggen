// knhks/utils.h
// Utility functions: context initialization, RDF loading, clock utilities

#ifndef KNHKS_UTILS_H
#define KNHKS_UTILS_H

#include "types.h"

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

// Clock utilities (hot-safe)
uint64_t knhks_rd_ticks(void);   // arch-specific
double knhks_ticks_hz(void);     // arch-specific
uint64_t knhks_generate_span_id(void); // Generate OTEL-compatible span ID
uint64_t knhks_generate_span_id_from_ticks(uint64_t ticks); // Generate from existing ticks (faster)

// Batch (vector of IRs) with deterministic order Λ; N ≤ 8, no joins
// Returns number of hooks executed successfully
// Implementation in core.c
int knhks_eval_batch8(const knhks_context_t *ctx, knhks_hook_ir_t *irs, size_t n, knhks_receipt_t *rcpts);

// Legacy SELECT query (cold path only, exceeds 8-tick budget)
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);

// Benchmark evaluation (returns nanoseconds per operation)
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters);

#endif // KNHKS_UTILS_H

