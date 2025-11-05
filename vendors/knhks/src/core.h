// core.h
// Core evaluation logic (internal header)

#ifndef KNHKS_CORE_H
#define KNHKS_CORE_H

#include "knhks.h"

// Internal evaluation functions
int knhks_core_eval_bool(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);
size_t knhks_core_eval_select(const knhks_context_t *ctx, const knhks_hook_ir_t *ir);

#endif // KNHKS_CORE_H

