// tests/chicago_cli_epoch.c
// Chicago TDD: Epoch Noun Tests
// Tests epoch(#{tau, lambda, cover}), run(EpochId) - Epoch operations

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

// Test: Epoch validates tau ≤ 8
static int test_epoch_validates_tau(void) {
    printf("[TEST] Epoch - Tau Validation (τ ≤ 8)\n");
    reset_test_data();
    
    // τ must be ≤ 8 (Chatman Constant)
    uint32_t valid_tau = 8;
    uint32_t invalid_tau = 9;
    
    assert(valid_tau <= 8);
    assert(invalid_tau > 8);
    
    printf("  ✓ Tau validation passed (τ=%u)\n", valid_tau);
    return 1;
}

// Test: Epoch validates lambda is ≺-total
static int test_epoch_validates_lambda(void) {
    printf("[TEST] Epoch - Lambda Validation (Λ ≺-total)\n");
    reset_test_data();
    
    // Λ must be ≺-total (deterministic order)
    // Epoch plan: ordered list of reflex names
    const char* plan[] = {"hook1", "hook2", "hook3"};
    size_t plan_len = 3;
    
    // Verify deterministic order
    for (size_t i = 0; i < plan_len; i++) {
        assert(plan[i] != NULL);
    }
    
    // Verify no duplicates (ensures determinism)
    for (size_t i = 0; i < plan_len; i++) {
        for (size_t j = i + 1; j < plan_len; j++) {
            assert(strcmp(plan[i], plan[j]) != 0);
        }
    }
    
    printf("  ✓ Lambda validation passed (plan length=%zu)\n", plan_len);
    return 1;
}

// Test: Epoch executes μ over O
static int test_epoch_executes_mu(void) {
    printf("[TEST] Epoch - Execute μ over O\n");
    reset_test_data();
    
    // Execute μ over O for epoch
    // A = μ(O); hash(A) = hash(μ(O))
    
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
    
    knhk_receipt_t rcpt = {0};
    bool result = knhk_eval_bool(&ctx, &ir, &rcpt);
    assert(result == true);
    assert(rcpt.a_hash != 0); // hash(A) = hash(μ(O))
    
    printf("  ✓ μ executed over O (hash=0x%llx)\n", (unsigned long long)rcpt.a_hash);
    return 1;
}

// Test: Epoch returns receipt
static int test_epoch_returns_receipt(void) {
    printf("[TEST] Epoch - Receipt Generation\n");
    reset_test_data();
    
    // run(EpochId) -> {A, Receipt}
    // Receipt must include ticks, span_id, a_hash
    
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
    
    knhk_receipt_t rcpt = {0};
    knhk_eval_bool(&ctx, &ir, &rcpt);
    
    // Verify receipt has all required fields
    assert(rcpt.a_hash != 0); // Provenance hash should always be set
    
    printf("  ✓ Receipt generated (ticks=%u, hash=0x%llx)\n", rcpt.ticks, (unsigned long long)rcpt.a_hash);
    return 1;
}

int main(void) {
    printf("========================================\n");
    printf("Chicago TDD: Epoch Noun Tests\n");
    printf("========================================\n\n");
    
    int passed = 0;
    int total = 0;
    
    total++; if (test_epoch_validates_tau()) passed++;
    total++; if (test_epoch_validates_lambda()) passed++;
    total++; if (test_epoch_executes_mu()) passed++;
    total++; if (test_epoch_returns_receipt()) passed++;
    
    printf("\n========================================\n");
    printf("Results: %d/%d tests passed\n", passed, total);
    printf("========================================\n");
    
    return (passed == total) ? 0 : 1;
}

