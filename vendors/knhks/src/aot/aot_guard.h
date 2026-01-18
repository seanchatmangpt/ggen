// src/aot/aot_guard.h
// Ahead-Of-Time (AOT) Compilation Guard Header

#ifndef KNHK_AOT_GUARD_H
#define KNHK_AOT_GUARD_H

#include "knhk.h"
#include <stdint.h>
#include <stdbool.h>

/// Validate hook IR before execution
/// Returns true if valid, false if invalid (should route to cold path)
bool knhk_aot_validate_ir(knhk_op_t op, uint64_t run_len, uint64_t k);

/// Validate predicate run before pinning
bool knhk_aot_validate_run(knhk_pred_run_t run);

#endif // KNHK_AOT_GUARD_H

