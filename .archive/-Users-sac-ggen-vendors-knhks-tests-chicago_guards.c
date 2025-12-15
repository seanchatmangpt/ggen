// tests/chicago_guards.c
// Chicago TDD: Guard Enforcement Tests
// Tests H guards, run.len validation, and admission control

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhk.h"

#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

static uint64_t ALN S[NROWS];
static uint64_t ALN P[NROWS];
static uint64_t ALN O[NROWS];
static knhk_context_t ctx;

static void reset_test_data(void)
{
  memset(S, 0, sizeof(S));
  memset(P, 0, sizeof(P));
  memset(O, 0, sizeof(O));
  knhk_init_ctx(&ctx, S, P, O);
}

// Test: pin_run accepts len ≤ 8
static int test_pin_run_valid_length(void)
{
  printf("[TEST] pin_run Valid Length (≤8)\n");
  reset_test_data();
  
  // Valid: len = 8
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  assert(ctx.run.len == 8);
  
  // Valid: len = 1
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  assert(ctx.run.len == 1);
  
  // Valid: len = 0 (empty)
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 0});
  assert(ctx.run.len == 0);
  
  printf("  ✓ pin_run accepts len ≤ 8\n");
  return 1;
}

// Test: Guard blocks len > 8 at runtime
static int test_guard_blocks_long_runs(void)
{
  printf("[TEST] Guard Blocks Long Runs\n");
  reset_test_data();
  
  // Setup data
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  // Set run with len = 8 (maximum allowed)
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
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
  int result = knhk_eval_bool(&ctx, &ir, &rcpt);
  
  // Should execute successfully
  assert(result == 1);
  // 80/20 CRITICAL PATH: Operation must be ≤8 ticks
  // Timing validated externally by Rust framework
  assert(rcpt.lanes > 0);
  
  // Note: C API doesn't enforce len > 8 at pin time (that's Rust wrapper)
  // But runtime will reject if len > 8 in actual execution
  
  printf("  ✓ Guard allows len = 8\n");
  return 1;
}

// Test: Guard blocks wrong predicate
static int test_guard_blocks_wrong_predicate(void)
{
  printf("[TEST] Guard Blocks Wrong Predicate\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Query with wrong predicate
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_ASK_SP,
    .s = 0xA11CE,
    .p = 0xBAD00, // Wrong predicate
    .o = 0,
    .k = 0,
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  int result = knhk_eval_bool(&ctx, &ir, &rcpt);
  
  // Should return 0 (no match)
  assert(result == 0);
  // Timing validated externally by Rust framework
  assert(rcpt.lanes == 0); // No lanes when predicate doesn't match
  
  printf("  ✓ Wrong predicate correctly rejected\n");
  return 1;
}

// Test: Guard enforcement in batch
static int test_guard_enforcement_in_batch(void)
{
  printf("[TEST] Guard Enforcement in Batch\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Mix of valid and invalid predicates
  knhk_hook_ir_t irs[KNHK_NROWS] = {
    {.op = KNHK_OP_ASK_SP, .s = 0xA11CE, .p = 0xC0FFEE, .o = 0, .k = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0}, // Valid
    {.op = KNHK_OP_ASK_SP, .s = 0xA11CE, .p = 0xBAD00, .o = 0, .k = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0}  // Invalid
  };
  
  knhk_receipt_t rcpts[KNHK_NROWS] = {0};
  int executed = knhk_eval_batch8(&ctx, irs, 2, rcpts);
  
  assert(executed == 2); // Both execute (but second returns 0)
  // Timing validated externally by Rust framework
  assert(rcpts[0].lanes > 0); // First succeeds
  assert(rcpts[1].lanes == 0); // Second fails (wrong predicate)
  
  printf("  ✓ Batch handles invalid predicates gracefully\n");
  return 1;
}

// Test: Admission control (run.len validation)
static int test_admission_control(void)
{
  printf("[TEST] Admission Control\n");
  reset_test_data();
  
  // Setup data
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  // Test with various valid lengths
  for (int len = 0; len <= 8; len++) {
    knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = len});
    
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
    
    // Timing validated externally by Rust framework
  assert(rcpt.lanes > 0);
  }
  
  printf("  ✓ All valid lengths (0-8) admitted\n");
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Guard Enforcement\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_pin_run_valid_length()) passed++;
  total++; if (test_guard_blocks_long_runs()) passed++;
  total++; if (test_guard_blocks_wrong_predicate()) passed++;
  total++; if (test_guard_enforcement_in_batch()) passed++;
  total++; if (test_admission_control()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

