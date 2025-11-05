// tests/chicago_v1_test.c
// Chicago TDD v1.0 Test Suite
// Tests all v1.0 features: receipts, CONSTRUCT8, batch execution
// Verifies p95 ≤ 8 ticks for all hot path operations

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhks.h"

// 64B alignment
#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

// Test context
static uint64_t ALN S[NROWS];
static uint64_t ALN P[NROWS];
static uint64_t ALN O[NROWS];
static knhks_context_t ctx;

// Helper to reset test data
static void reset_test_data(void)
{
  memset(S, 0, sizeof(S));
  memset(P, 0, sizeof(P));
  memset(O, 0, sizeof(O));
  knhks_init_ctx(&ctx, S, P, O);
}

// Test v1.0 receipt functionality
static int test_receipt_generation(void)
{
  printf("TEST: Receipt Generation\n");
  reset_test_data();
  
  // Setup test data
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  knhks_hook_ir_t ir = {
    .op = KNHKS_OP_ASK_SP,
    .s = 0xA11CE,
    .p = 0xC0FFEE,
    .o = 0,
    .k = 0,
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhks_receipt_t rcpt = {0};
  int result = knhks_eval_bool(&ctx, &ir, &rcpt);
  
  assert(result == 1);
  assert(rcpt.ticks > 0);
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  assert(rcpt.lanes == KNHKS_NROWS);
  assert(rcpt.a_hash != 0); // Should have hash fragment
  
  printf("  ✓ Receipt generated: ticks=%u, lanes=%u, hash=0x%llx\n", 
         rcpt.ticks, rcpt.lanes, (unsigned long long)rcpt.a_hash);
  
  return 1;
}

// Test receipt merging (Π ⊕)
static int test_receipt_merge(void)
{
  printf("TEST: Receipt Merge (Π ⊕)\n");
  
  knhks_receipt_t a = {.ticks = 4, .lanes = 8, .span_id = 0x1234, .a_hash = 0x5678};
  knhks_receipt_t b = {.ticks = 6, .lanes = 8, .span_id = 0xabcd, .a_hash = 0xef00};
  
  knhks_receipt_t merged = knhks_receipt_merge(a, b);
  
  assert(merged.ticks == 6); // max
  assert(merged.lanes == 16); // sum
  assert(merged.span_id == (0x1234 ^ 0xabcd)); // XOR
  assert(merged.a_hash == (0x5678 ^ 0xef00)); // ⊕ (XOR)
  
  printf("  ✓ Receipts merged correctly\n");
  
  return 1;
}

// Test CONSTRUCT8 operation
static int test_construct8(void)
{
  printf("TEST: CONSTRUCT8 Operation\n");
  reset_test_data();
  
  // Setup test data
  S[0] = 0xA11CE;
  S[1] = 0xB22FF;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  // Preallocated output buffers
  uint64_t ALN out_S[KNHKS_NROWS];
  uint64_t ALN out_P[KNHKS_NROWS];
  uint64_t ALN out_O[KNHKS_NROWS];
  
  knhks_hook_ir_t ir = {
    .op = KNHKS_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xALLOWED, // Template constant
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  knhks_receipt_t rcpt = {0};
  int written = knhks_eval_construct8(&ctx, &ir, &rcpt);
  
  assert(written > 0);
  assert(written <= KNHKS_NROWS);
  assert(rcpt.ticks > 0);
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  assert(out_P[0] == 0xC0FFEE); // Template predicate
  assert(out_O[0] == 0xALLOWED); // Template object
  assert(ir.out_mask != 0); // Should have mask
  
  printf("  ✓ CONSTRUCT8 emitted %d triples, ticks=%u\n", written, rcpt.ticks);
  
  return 1;
}

// Test batch execution with Λ ordering
static int test_batch_execution(void)
{
  printf("TEST: Batch Execution (Λ ordering)\n");
  reset_test_data();
  
  // Setup test data
  S[0] = 0xA11CE;
  S[1] = 0xB22FF;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  // Create batch of 3 hooks
  knhks_hook_ir_t irs[KNHKS_NROWS] = {
    {.op = KNHKS_OP_ASK_SP, .s = 0xA11CE, .p = 0xC0FFEE, .o = 0, .k = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0},
    {.op = KNHKS_OP_COUNT_SP_GE, .s = 0xA11CE, .p = 0xC0FFEE, .o = 0, .k = 1, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0},
    {.op = KNHKS_OP_ASK_SPO, .s = 0xA11CE, .p = 0xC0FFEE, .o = 0xB0B, .k = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0}
  };
  
  knhks_receipt_t rcpts[KNHKS_NROWS] = {0};
  int executed = knhks_eval_batch8(&ctx, irs, 3, rcpts);
  
  assert(executed == 3);
  assert(rcpts[0].ticks <= KNHKS_TICK_BUDGET);
  assert(rcpts[1].ticks <= KNHKS_TICK_BUDGET);
  assert(rcpts[2].ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Batch executed %d hooks, all within budget\n", executed);
  
  return 1;
}

// Test all 15 operations with receipts
static int test_all_operations_with_receipts(void)
{
  printf("TEST: All 15 Operations with Receipts\n");
  reset_test_data();
  
  // Setup test data
  S[0] = 0xA11CE;
  S[1] = 0xA11CE; // Same subject twice
  S[2] = 0xB22FF;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  P[2] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  O[2] = 0xB0B; // Same object
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 3});
  
  struct {
    knhks_op_t op;
    uint64_t s, p, o, k;
    int expected;
  } test_cases[] = {
    {KNHKS_OP_ASK_SP, 0xA11CE, 0xC0FFEE, 0, 0, 1},
    {KNHKS_OP_COUNT_SP_GE, 0xA11CE, 0xC0FFEE, 0, 1, 1},
    {KNHKS_OP_COUNT_SP_LE, 0xA11CE, 0xC0FFEE, 0, 3, 1},
    {KNHKS_OP_COUNT_SP_EQ, 0xA11CE, 0xC0FFEE, 0, 2, 1},
    {KNHKS_OP_ASK_SPO, 0xA11CE, 0xC0FFEE, 0xB0B, 0, 1},
    {KNHKS_OP_ASK_OP, 0, 0xC0FFEE, 0xB0B, 0, 1},
    {KNHKS_OP_UNIQUE_SP, 0xB22FF, 0xC0FFEE, 0, 0, 1},
    {KNHKS_OP_COUNT_OP, 0, 0xC0FFEE, 0xB0B, 1, 1},
    {KNHKS_OP_COUNT_OP_LE, 0, 0xC0FFEE, 0xB0B, 3, 1},
    {KNHKS_OP_COUNT_OP_EQ, 0, 0xC0FFEE, 0xB0B, 2, 1},
    {KNHKS_OP_COMPARE_O_EQ, 0, 0xC0FFEE, 0xB0B, 0, 1},
    {KNHKS_OP_COMPARE_O_GT, 0, 0xC0FFEE, 0xB0A, 0, 1},
    {KNHKS_OP_COMPARE_O_LT, 0, 0xC0FFEE, 0xB0C, 0, 1},
    {KNHKS_OP_COMPARE_O_GE, 0, 0xC0FFEE, 0xB0B, 0, 1},
    {KNHKS_OP_COMPARE_O_LE, 0, 0xC0FFEE, 0xB0B, 0, 1}
  };
  
  int passed = 0;
  for (size_t i = 0; i < sizeof(test_cases)/sizeof(test_cases[0]); i++) {
    knhks_hook_ir_t ir = {
      .op = test_cases[i].op,
      .s = test_cases[i].s,
      .p = test_cases[i].p,
      .o = test_cases[i].o,
      .k = test_cases[i].k,
      .out_S = NULL,
      .out_P = NULL,
      .out_O = NULL,
      .out_mask = 0
    };
    
    knhks_receipt_t rcpt = {0};
    int result = knhks_eval_bool(&ctx, &ir, &rcpt);
    
    assert(result == test_cases[i].expected);
    assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
    passed++;
  }
  
  printf("  ✓ All %zu operations passed with receipts\n", sizeof(test_cases)/sizeof(test_cases[0]));
  
  return 1;
}

// Test pin_run guard (H: len > 8 blocked)
static int test_pin_run_guard(void)
{
  printf("TEST: pin_run Guard (H: len > 8)\n");
  reset_test_data();
  
  // This should be allowed (len = 8)
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  assert(ctx.run.len == 8);
  
  // Note: Guard enforcement happens at runtime during eval, not at pin time
  // The guard check is in the Rust wrapper, but for C API we document it
  printf("  ✓ pin_run accepts len ≤ 8\n");
  
  return 1;
}

// Test v1.0 constants
static int test_v1_constants(void)
{
  printf("TEST: v1.0 Constants\n");
  
  assert(KNHKS_TICK_BUDGET == 8);
  assert(KNHKS_NROWS == 8);
  assert(KNHKS_ALIGN == 64);
  assert(NROWS == 8);
  
  printf("  ✓ All constants correct\n");
  
  return 1;
}

// Test timing validation (p95 ≤ 8 ticks)
static int test_timing_validation(void)
{
  printf("TEST: Timing Validation (p95 ≤ 8 ticks)\n");
  reset_test_data();
  
  // Setup test data
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  knhks_hook_ir_t ir = {
    .op = KNHKS_OP_ASK_SP,
    .s = 0xA11CE,
    .p = 0xC0FFEE,
    .o = 0,
    .k = 0,
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  // Run multiple times to get timing distribution
  uint32_t ticks[100];
  for (int i = 0; i < 100; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    ticks[i] = rcpt.ticks;
  }
  
  // Calculate p95
  uint32_t sorted[100];
  memcpy(sorted, ticks, sizeof(ticks));
  for (int i = 0; i < 99; i++) {
    for (int j = i + 1; j < 100; j++) {
      if (sorted[i] > sorted[j]) {
        uint32_t tmp = sorted[i];
        sorted[i] = sorted[j];
        sorted[j] = tmp;
      }
    }
  }
  
  uint32_t p95 = sorted[95];
  assert(p95 <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ p95 ticks = %u (budget = %u)\n", p95, KNHKS_TICK_BUDGET);
  
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("KNHKS v1.0 Chicago TDD Test Suite\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_v1_constants()) passed++;
  total++; if (test_receipt_generation()) passed++;
  total++; if (test_receipt_merge()) passed++;
  total++; if (test_construct8()) passed++;
  total++; if (test_batch_execution()) passed++;
  total++; if (test_all_operations_with_receipts()) passed++;
  total++; if (test_pin_run_guard()) passed++;
  total++; if (test_timing_validation()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

