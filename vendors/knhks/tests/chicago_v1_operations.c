// chicago_v1_operations.c
// v1.0 Operations tests: CONSTRUCT8, batch execution, all operations

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhks.h"

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
    .o = 0xA110E, // Template constant
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
  // Allow ticks == 0 for very fast operations (sub-tick resolution)
  assert(rcpt.ticks <= 500); // Account for measurement overhead
  assert(out_P[0] == 0xC0FFEE); // Template predicate
  assert(out_O[0] == 0xA110E); // Template object
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
  assert(rcpts[0].ticks <= 500); // Account for measurement overhead
  assert(rcpts[1].ticks <= 500);
  assert(rcpts[2].ticks <= 500);
  
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
    assert(rcpt.ticks <= 500); // Account for measurement overhead
    passed++;
  }
  
  printf("  ✓ All %zu operations passed with receipts\n", sizeof(test_cases)/sizeof(test_cases[0]));
  
  return 1;
}

// Exported test functions
int chicago_test_v1_operations(void)
{
  int passed = 0;
  int total = 3;
  
  if (test_construct8())
    passed++;
  printf("\n");
  
  if (test_batch_execution())
    passed++;
  printf("\n");
  
  if (test_all_operations_with_receipts())
    passed++;
  printf("\n");
  
  printf("v1.0 Operations Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

