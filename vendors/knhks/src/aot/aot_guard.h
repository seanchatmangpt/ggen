// src/aot/aot_guard.h
// Ahead-Of-Time (AOT) Compilation Guard Header

#ifndef KNHKS_AOT_GUARD_H
#define KNHKS_AOT_GUARD_H

#include "knhks.h"
#include <stdint.h>
#include <stdbool.h>

/// Validate hook IR before execution
/// Returns true if valid, false if invalid (should route to cold path)
bool knhks_aot_validate_ir(knhks_op_t op, uint64_t run_len, uint64_t k);

/// Validate predicate run before pinning
bool knhks_aot_validate_run(knhks_pred_run_t run);

#endif // KNHKS_AOT_GUARD_H

