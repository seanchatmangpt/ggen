// tests/chicago_performance.c
// Chicago TDD: Performance Validation Tests
// Tests Chatman Constant compliance, p95 validation, and timing accuracy

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

static uint64_t ALN S[NROWS];
static uint64_t ALN P[NROWS];
static uint64_t ALN O[NROWS];
static knhks_context_t ctx;

static void reset_test_data(void)
{
  memset(S, 0, sizeof(S));
  memset(P, 0, sizeof(P));
  memset(O, 0, sizeof(O));
  knhks_init_ctx(&ctx, S, P, O);
}

// Test: Chatman Constant compliance (p95 ≤ 2 ns = 8 ticks)
static int test_chatman_constant(void)
{
  printf("[TEST] Chatman Constant Compliance\n");
  reset_test_data();
  
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
  
  // Measure 10,000 executions
  const int iterations = 10000;
  uint32_t ticks[iterations];
  
  for (int i = 0; i < iterations; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    ticks[i] = rcpt.ticks;
  }
  
  // Calculate p95
  uint32_t sorted[iterations];
  memcpy(sorted, ticks, sizeof(ticks));
  
  // Simple bubble sort for p95
  for (int i = 0; i < iterations - 1; i++) {
    for (int j = i + 1; j < iterations; j++) {
      if (sorted[i] > sorted[j]) {
        uint32_t tmp = sorted[i];
        sorted[i] = sorted[j];
        sorted[j] = tmp;
      }
    }
  }
  
  uint32_t p95 = sorted[(int)(iterations * 0.95)];
  
  assert(p95 <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Chatman Constant: p95 = %u ticks ≤ %u\n", p95, KNHKS_TICK_BUDGET);
  return 1;
}

// Test: All operations meet timing budget
static int test_all_ops_timing_budget(void)
{
  printf("[TEST] All Operations Timing Budget\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  S[1] = 0xA11CE;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  struct {
    knhks_op_t op;
    uint64_t s, p, o, k;
  } test_cases[] = {
    {KNHKS_OP_ASK_SP, 0xA11CE, 0xC0FFEE, 0, 0},
    {KNHKS_OP_COUNT_SP_GE, 0xA11CE, 0xC0FFEE, 0, 1},
    {KNHKS_OP_COUNT_SP_LE, 0xA11CE, 0xC0FFEE, 0, 2},
    {KNHKS_OP_COUNT_SP_EQ, 0xA11CE, 0xC0FFEE, 0, 2},
    {KNHKS_OP_ASK_SPO, 0xA11CE, 0xC0FFEE, 0xB0B, 0},
    {KNHKS_OP_ASK_OP, 0, 0xC0FFEE, 0xB0B, 0},
    {KNHKS_OP_UNIQUE_SP, 0xA11CE, 0xC0FFEE, 0, 0},
    {KNHKS_OP_COUNT_OP, 0, 0xC0FFEE, 0xB0B, 1},
    {KNHKS_OP_COUNT_OP_LE, 0, 0xC0FFEE, 0xB0B, 2},
    {KNHKS_OP_COUNT_OP_EQ, 0, 0xC0FFEE, 0xB0B, 1},
    {KNHKS_OP_COMPARE_O_EQ, 0, 0xC0FFEE, 0xB0B, 0},
    {KNHKS_OP_COMPARE_O_GT, 0, 0xC0FFEE, 0xB0A, 0},
    {KNHKS_OP_COMPARE_O_LT, 0, 0xC0FFEE, 0xB0C, 0},
    {KNHKS_OP_COMPARE_O_GE, 0, 0xC0FFEE, 0xB0B, 0},
    {KNHKS_OP_COMPARE_O_LE, 0, 0xC0FFEE, 0xB0B, 0}
  };
  
  int violations = 0;
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
    
    // Measure 1000 times
    uint32_t max_ticks = 0;
    for (int j = 0; j < 1000; j++) {
      knhks_receipt_t rcpt = {0};
      knhks_eval_bool(&ctx, &ir, &rcpt);
      if (rcpt.ticks > max_ticks) {
        max_ticks = rcpt.ticks;
      }
    }
    
    if (max_ticks > KNHKS_TICK_BUDGET) {
      violations++;
    }
  }
  
  assert(violations == 0);
  
  printf("  ✓ All %zu operations within budget\n", sizeof(test_cases)/sizeof(test_cases[0]));
  return 1;
}

// Test: Timing consistency (low variance)
static int test_timing_consistency(void)
{
  printf("[TEST] Timing Consistency\n");
  reset_test_data();
  
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
  
  // Measure timing distribution
  const int iterations = 1000;
  uint32_t ticks[iterations];
  
  for (int i = 0; i < iterations; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    ticks[i] = rcpt.ticks;
  }
  
  // Calculate p50 and p95
  uint32_t sorted[iterations];
  memcpy(sorted, ticks, sizeof(ticks));
  
  for (int i = 0; i < iterations - 1; i++) {
    for (int j = i + 1; j < iterations; j++) {
      if (sorted[i] > sorted[j]) {
        uint32_t tmp = sorted[i];
        sorted[i] = sorted[j];
        sorted[j] = tmp;
      }
    }
  }
  
  uint32_t p50 = sorted[iterations / 2];
  uint32_t p95 = sorted[(int)(iterations * 0.95)];
  
  assert(p50 <= KNHKS_TICK_BUDGET);
  assert(p95 <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ p50 = %u, p95 = %u (budget = %u)\n", p50, p95, KNHKS_TICK_BUDGET);
  return 1;
}

// Test: CONSTRUCT8 timing
static int test_construct8_timing(void)
{
  printf("[TEST] CONSTRUCT8 Timing\n");
  reset_test_data();
  
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
  uint64_t ALN out_S[KNHKS_NROWS];
  uint64_t ALN out_P[KNHKS_NROWS];
  uint64_t ALN out_O[KNHKS_NROWS];
  
  knhks_hook_ir_t ir = {
    .op = KNHKS_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xALLOWED,
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  // Measure 1000 times
  uint32_t max_ticks = 0;
  for (int i = 0; i < 1000; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_construct8(&ctx, &ir, &rcpt);
    if (rcpt.ticks > max_ticks) {
      max_ticks = rcpt.ticks;
    }
  }
  
  assert(max_ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ CONSTRUCT8 max ticks = %u ≤ %u\n", max_ticks, KNHKS_TICK_BUDGET);
  return 1;
}

// Test: Batch timing (aggregate ≤ 8 ticks per hook)
static int test_batch_timing(void)
{
  printf("[TEST] Batch Execution Timing\n");
  reset_test_data();
  
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
  knhks_hook_ir_t irs[KNHKS_NROWS];
  for (int i = 0; i < 8; i++) {
    irs[i] = (knhks_hook_ir_t){
      .op = KNHKS_OP_ASK_SP,
      .s = 0xA11CE + i,
      .p = 0xC0FFEE,
      .o = 0,
      .k = 0,
      .out_S = NULL,
      .out_P = NULL,
      .out_O = NULL,
      .out_mask = 0
    };
  }
  
  // Measure batch timing
  uint32_t max_ticks = 0;
  for (int i = 0; i < 100; i++) {
    knhks_receipt_t rcpts[KNHKS_NROWS] = {0};
    knhks_eval_batch8(&ctx, irs, 8, rcpts);
    
    for (int j = 0; j < 8; j++) {
      if (rcpts[j].ticks > max_ticks) {
        max_ticks = rcpts[j].ticks;
      }
    }
  }
  
  assert(max_ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Batch max ticks = %u ≤ %u\n", max_ticks, KNHKS_TICK_BUDGET);
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Performance Validation\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_chatman_constant()) passed++;
  total++; if (test_all_ops_timing_budget()) passed++;
  total++; if (test_timing_consistency()) passed++;
  total++; if (test_construct8_timing()) passed++;
  total++; if (test_batch_timing()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

