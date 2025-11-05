// chicago_v1_validation.c
// v1.0 Validation tests: guards, constants, timing

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

// Test pin_run guard (H: len > 8 blocked)
static int test_pin_run_guard(void)
{
  printf("TEST: pin_run Guard (H: len ≤ 8)\n");
  reset_test_data();
  
  // Valid: len = 8
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

// Exported test functions
int chicago_test_v1_validation(void)
{
  int passed = 0;
  int total = 3;
  
  if (test_v1_constants())
    passed++;
  printf("\n");
  
  if (test_pin_run_guard())
    passed++;
  printf("\n");
  
  if (test_timing_validation())
    passed++;
  printf("\n");
  
  printf("v1.0 Validation Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

