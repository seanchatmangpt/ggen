// tests/chicago_cli_reflex.c
// Chicago TDD: Reflex Noun Tests
// Tests reflex(#{name, op, run, args, epoch}) - Reflex declaration

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhk.h"
#include "aot/aot_guard.h"

#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

static uint64_t ALN S[NROWS];
static uint64_t ALN P[NROWS];
static uint64_t ALN O[NROWS];
static knhk_context_t ctx;

static void reset_test_data(void) {
    memset(S, 0, sizeof(S));
    memset(P, 0, sizeof(P));
    memset(O, 0, sizeof(O));
    knhk_init_ctx(&ctx, S, P, O);
}

// Test: Reflex declares hook
static int test_reflex_declares_hook(void) {
    printf("[TEST] Reflex - Declare Hook\n");
    reset_test_data();
    
    const char* name = "ask_can_access";
    knhk_op_t op = KNHK_OP_ASK_SP;
    uint64_t pred = 0xC0FFEE;
    uint64_t off = 0;
    uint64_t len = 8;
    
    // Validate operation is in H_hot set
    assert(knhk_aot_validate_ir(op, len, 0) == true);
    
    // Validate run length
    assert(knhk_aot_validate_run((knhk_pred_run_t){.pred = pred, .off = off, .len = len}) == true);
    
    printf("  ✓ Hook declared: %s (op=%d, pred=0x%llx)\n", name, op, (unsigned long long)pred);
    return 1;
}

// Test: Reflex validates operation type
static int test_reflex_validates_operation(void) {
    printf("[TEST] Reflex - Operation Validation\n");
    reset_test_data();
    
    // Valid operations in H_hot set
    assert(knhk_aot_validate_ir(KNHK_OP_ASK_SP, 8, 0) == true);
    assert(knhk_aot_validate_ir(KNHK_OP_COUNT_SP_GE, 8, 5) == true);
    assert(knhk_aot_validate_ir(KNHK_OP_ASK_SPO, 8, 0) == true);
    assert(knhk_aot_validate_ir(KNHK_OP_UNIQUE_SP, 1, 0) == true);
    
    // Invalid operation should fail
    assert(knhk_aot_validate_ir(999, 8, 0) == false);
    
    printf("  ✓ Operation validation passed\n");
    return 1;
}

// Test: Reflex validates run length
static int test_reflex_validates_run_length(void) {
    printf("[TEST] Reflex - Run Length Validation\n");
    reset_test_data();
    
    // Run length must be ≤ 8
    uint64_t valid_len = 8;
    uint64_t invalid_len = 9;
    
    assert(knhk_aot_validate_run((knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = valid_len}) == true);
    assert(knhk_aot_validate_run((knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = invalid_len}) == false);
    
    printf("  ✓ Run length validation passed\n");
    return 1;
}

// Test: Reflex compiles to IR
static int test_reflex_compiles_to_ir(void) {
    printf("[TEST] Reflex - IR Compilation\n");
    reset_test_data();
    
    // Reflex compiles to Hook IR
    S[0] = 0xA11CE;
    P[0] = 0xC0FFEE;
    O[0] = 0xB0B;
    
    knhk_pred_run_t run = { .pred = 0xC0FFEE, .off = 0, .len = 1 };
    knhk_pin_run(&ctx, run);
    
    knhk_hook_ir_t ir = {
        .op = KNHK_OP_ASK_SP,
        .s = 0xA11CE,
        .p = 0xC0FFEE,
        .o = 0,
        .k = 0,
        .out_S = NULL,
        .out_P = NULL,
        .out_O = NULL,
        .out_mask = 0
    };
    
    // Validate IR
    assert(knhk_aot_validate_ir(ir.op, run.len, ir.k) == true);
    
    // Execute IR
    knhk_receipt_t rcpt = {0};
    bool result = knhk_eval_bool(&ctx, &ir, &rcpt);
    assert(result == true);
    assert(rcpt.a_hash != 0);
    
    printf("  ✓ IR compilation successful\n");
    return 1;
}

int main(void) {
    printf("========================================\n");
    printf("Chicago TDD: Reflex Noun Tests\n");
    printf("========================================\n\n");
    
    int passed = 0;
    int total = 0;
    
    total++; if (test_reflex_declares_hook()) passed++;
    total++; if (test_reflex_validates_operation()) passed++;
    total++; if (test_reflex_validates_run_length()) passed++;
    total++; if (test_reflex_compiles_to_ir()) passed++;
    
    printf("\n========================================\n");
    printf("Results: %d/%d tests passed\n", passed, total);
    printf("========================================\n");
    
    return (passed == total) ? 0 : 1;
}

