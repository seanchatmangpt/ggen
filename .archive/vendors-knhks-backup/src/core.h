// core.h
// Core evaluation logic (internal header)

#ifndef KNHK_CORE_H
#define KNHK_CORE_H

#include "knhk.h"

// Internal evaluation functions
int knhk_core_eval_bool(const knhk_context_t *ctx, const knhk_hook_ir_t *ir);
size_t knhk_core_eval_select(const knhk_context_t *ctx, const knhk_hook_ir_t *ir);

#endif // KNHK_CORE_H

