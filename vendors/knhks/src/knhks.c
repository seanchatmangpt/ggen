// knhks.c
// Public API implementation

#include "knhks.h"
#include "rdf.h"
#include "core.h"
#include "clock.h"
#include <stdio.h>
#include <string.h>

// Initialize context with arrays (legacy - now in core.c)
// This is kept for backward compatibility, redirects to core.c

// Load RDF file into context arrays
int knhks_load_rdf(knhks_context_t *ctx, const char *filename)
{
  if (!ctx || !ctx->S || !ctx->P || !ctx->O)
    return 0;

  size_t capacity = NROWS;
  size_t count = 0;
  // Cast away const for RDF loading (arrays are written to during load)
  int result = knhks_rdf_load(filename, (uint64_t *)ctx->S, (uint64_t *)ctx->P, (uint64_t *)ctx->O, capacity, &count);
  if (result)
  {
    ctx->triple_count = count;
    // Set run to first predicate found
    if (count > 0)
    {
      ctx->run.pred = ctx->P[0];
      ctx->run.off = 0;
      ctx->run.len = count;
    }
  }
  return result;
}

// Evaluate boolean query is now inline in knhks.h for performance

// Evaluate SELECT query
size_t knhks_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir)
{
  return knhks_core_eval_select(ctx, ir);
}

// Benchmark evaluation (returns nanoseconds per operation)
double knhks_bench_eval(const knhks_context_t *ctx, const knhks_hook_ir_t *ir, int iters)
{
  // warm L1
  volatile int sink = 0;
  for (int i = 0; i < 1024; i++)
    sink ^= knhks_eval_bool(ctx, ir, NULL);
  uint64_t t0 = knhks_rd_ticks();
  for (int i = 0; i < iters; i++)
    sink ^= knhks_eval_bool(ctx, ir, NULL);
  uint64_t t1 = knhks_rd_ticks();
  (void)sink;
  double hz = knhks_ticks_hz();
  double sec = (double)(t1 - t0) / hz;
  return (sec * 1e9) / (double)iters; // ns/op
}

// Clock utilities are implemented in clock.c

