// src/aot/aot_guard.c
// Ahead-Of-Time (AOT) Compilation Guard
// Validates IR before execution to enforce Chatman Constant (≤8 ticks)

#include "knhks.h"
#include <stdint.h>
#include <stdbool.h>

typedef enum {
    AOT_VALID = 0,
    AOT_EXCEEDS_TICK_BUDGET = 1,
    AOT_INVALID_OPERATION = 2,
    AOT_INVALID_RUN_LENGTH = 3,
} aot_validation_result_t;

/// Validate hook IR before execution
/// Returns true if valid, false if invalid
bool knhks_aot_validate_ir(knhks_op_t op, uint64_t run_len, uint64_t k) {
    // Check run length ≤ 8 (Chatman Constant constraint)
    if (run_len > KNHKS_NROWS) {
        return false;
    }
    
    // Validate operation is in hot path set
    switch (op) {
        case KNHKS_OP_ASK_SP:
        case KNHKS_OP_COUNT_SP_GE:
        case KNHKS_OP_COUNT_SP_LE:
        case KNHKS_OP_COUNT_SP_EQ:
        case KNHKS_OP_ASK_SPO:
        case KNHKS_OP_ASK_OP:
        case KNHKS_OP_UNIQUE_SP:
        case KNHKS_OP_COUNT_OP:
        case KNHKS_OP_COUNT_OP_LE:
        case KNHKS_OP_COUNT_OP_EQ:
        case KNHKS_OP_COMPARE_O_EQ:
        case KNHKS_OP_COMPARE_O_GT:
        case KNHKS_OP_COMPARE_O_LT:
        case KNHKS_OP_COMPARE_O_GE:
        case KNHKS_OP_COMPARE_O_LE:
        case KNHKS_OP_CONSTRUCT8:
            break;
        default:
            return false; // Invalid operation
    }
    
    // Check operation-specific constraints
    switch (op) {
        case KNHKS_OP_UNIQUE_SP:
            // UNIQUE requires run_len ≤ 1
            if (run_len > 1) {
                return false;
            }
            break;
        case KNHKS_OP_COUNT_SP_GE:
        case KNHKS_OP_COUNT_SP_LE:
        case KNHKS_OP_COUNT_SP_EQ:
        case KNHKS_OP_COUNT_OP:
        case KNHKS_OP_COUNT_OP_LE:
        case KNHKS_OP_COUNT_OP_EQ:
            // COUNT operations: k must be ≤ run_len
            if (k > run_len) {
                return false;
            }
            break;
        default:
            // ASK, COMPARE, CONSTRUCT8 operations are always valid if run_len ≤ 8
            break;
    }
    
    return true;
}

/// Validate predicate run before pinning
bool knhks_aot_validate_run(knhks_pred_run_t run) {
    return run.len <= KNHKS_NROWS;
}

