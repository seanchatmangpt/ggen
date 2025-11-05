// src/aot/aot_guard.c
// Ahead-Of-Time (AOT) Compilation Guard
// Validates IR before execution to enforce Chatman Constant (≤8 ticks)

#include "knhk.h"
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
bool knhk_aot_validate_ir(knhk_op_t op, uint64_t run_len, uint64_t k) {
    // Check run length ≤ 8 (Chatman Constant constraint)
    if (run_len > KNHK_NROWS) {
        return false;
    }
    
    // Validate operation is in hot path set
    switch (op) {
        case KNHK_OP_ASK_SP:
        case KNHK_OP_COUNT_SP_GE:
        case KNHK_OP_COUNT_SP_LE:
        case KNHK_OP_COUNT_SP_EQ:
        case KNHK_OP_ASK_SPO:
        case KNHK_OP_ASK_OP:
        case KNHK_OP_UNIQUE_SP:
        case KNHK_OP_COUNT_OP:
        case KNHK_OP_COUNT_OP_LE:
        case KNHK_OP_COUNT_OP_EQ:
        case KNHK_OP_COMPARE_O_EQ:
        case KNHK_OP_COMPARE_O_GT:
        case KNHK_OP_COMPARE_O_LT:
        case KNHK_OP_COMPARE_O_GE:
        case KNHK_OP_COMPARE_O_LE:
        case KNHK_OP_CONSTRUCT8:
            break;
        default:
            return false; // Invalid operation
    }
    
    // Check operation-specific constraints
    switch (op) {
        case KNHK_OP_UNIQUE_SP:
            // UNIQUE requires run_len ≤ 1
            if (run_len > 1) {
                return false;
            }
            break;
        case KNHK_OP_COUNT_SP_GE:
        case KNHK_OP_COUNT_SP_LE:
        case KNHK_OP_COUNT_SP_EQ:
        case KNHK_OP_COUNT_OP:
        case KNHK_OP_COUNT_OP_LE:
        case KNHK_OP_COUNT_OP_EQ:
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
bool knhk_aot_validate_run(knhk_pred_run_t run) {
    return run.len <= KNHK_NROWS;
}

