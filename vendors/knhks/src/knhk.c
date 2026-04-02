// knhk.c
// Public API implementation

#include "knhk.h"
#include "rdf.h"
#include "core.h"
#include "clock.h"
#include <stdio.h>
#include <string.h>

// Initialize context with arrays (legacy - now in core.c)
// This is kept for backward compatibility, redirects to core.c

// Load RDF file into context arrays
int knhk_load_rdf(knhk_context_t *ctx, const char *filename)
{
  if (!ctx || !ctx->S || !ctx->P || !ctx->O)
    return 0;

  size_t capacity = NROWS;
  size_t count = 0;
  // Cast away const for RDF loading (arrays are written to during load)
  int result = knhk_rdf_load(filename, (uint64_t *)ctx->S, (uint64_t *)ctx->P, (uint64_t *)ctx->O, capacity, &count);
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

// Evaluate boolean query is now inline in knhk.h for performance

// Evaluate SELECT query
size_t knhk_eval_select(const knhk_context_t *ctx, const knhk_hook_ir_t *ir)
{
  return knhk_core_eval_select(ctx, ir);
}

