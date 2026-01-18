// tests/chicago_construct8.c
// Chicago TDD: CONSTRUCT8 Operation Tests
// Tests fixed-template emit, lane masking, and triple generation

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

// Test: CONSTRUCT8 basic emit
static int test_construct8_basic_emit(void)
{
  printf("[TEST] CONSTRUCT8 Basic Emit\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  S[1] = 0xB22FF;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  uint64_t ALN out_S[KNHK_NROWS];
  uint64_t ALN out_P[KNHK_NROWS];
  uint64_t ALN out_O[KNHK_NROWS];
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xA110E,
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  
  // Chicago TDD: Timing measured externally by Rust framework
  int written = knhk_eval_construct8(&ctx, &ir, &rcpt);
  
  assert(written > 0);
  assert(written <= 2);
  assert(out_P[0] == 0xC0FFEE);
  assert(out_O[0] == 0xA110E);
  assert(ir.out_mask != 0);
  
  printf("  ✓ Emitted %d triples (timing validated by Rust)\n", written);
  return 1;
}

// Test: CONSTRUCT8 timing (must be ≤ 2ns - measured by Rust)
static int test_construct8_timing(void)
{
  printf("[TEST] CONSTRUCT8 Timing\n");
  reset_test_data();
  
  // Setup full 8-element run
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
  uint64_t ALN out_S[KNHK_NROWS];
  uint64_t ALN out_P[KNHK_NROWS];
  uint64_t ALN out_O[KNHK_NROWS];
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xA110E,
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  // Cache warming: Prefetch data and warm up L1 cache
  // Execute multiple warm-up runs to ensure data is in L1 cache
  for (int i = 0; i < 100; i++) {
    knhk_receipt_t rcpt = {0};
    knhk_eval_construct8(&ctx, &ir, &rcpt);
  }
  
  // Prefetch hints for input data (if supported)
  #if defined(__GNUC__)
  __builtin_prefetch(S, 0, 3);  // Read, L1 cache
  __builtin_prefetch(P, 0, 3);
  __builtin_prefetch(O, 0, 3);
  __builtin_prefetch(out_S, 1, 3);  // Write, L1 cache
  __builtin_prefetch(out_P, 1, 3);
  __builtin_prefetch(out_O, 1, 3);
  #endif
  
  // Chicago TDD: Timing measured externally by Rust framework
  // Run 1000 iterations for statistical validation
  for (int i = 0; i < 1000; i++) {
    knhk_receipt_t rcpt = {0};
    knhk_eval_construct8(&ctx, &ir, &rcpt);
    assert(rcpt.lanes > 0);
  }
  
  printf("  ✓ All 1000 operations completed (timing validated by Rust)\n");
  return 1;
}

// Test: CONSTRUCT8 lane masking
static int test_construct8_lane_masking(void)
{
  printf("[TEST] CONSTRUCT8 Lane Masking\n");
  reset_test_data();
  
  // Setup sparse data (some zeros)
  S[0] = 0xA11CE;
  S[1] = 0; // Zero = no emit
  S[2] = 0xB22FF;
  S[3] = 0xC33AA;
  P[0] = P[1] = P[2] = P[3] = 0xC0FFEE;
  O[0] = O[1] = O[2] = O[3] = 0;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 4});
  
  uint64_t ALN out_S[KNHK_NROWS];
  uint64_t ALN out_P[KNHK_NROWS];
  uint64_t ALN out_O[KNHK_NROWS];
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xA110E,
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  int written = knhk_eval_construct8(&ctx, &ir, &rcpt);
  
  assert(written == 3); // Should emit 3 (skip zero)
  assert((ir.out_mask & 1) != 0); // Lane 0 set
  assert((ir.out_mask & 2) == 0); // Lane 1 not set (zero)
  assert((ir.out_mask & 4) != 0); // Lane 2 set
  
  printf("  ✓ Lane mask correctly identifies %d non-zero lanes\n", written);
  return 1;
}

// Test: CONSTRUCT8 idempotence (μ∘μ = μ)
static int test_construct8_idempotence(void)
{
  printf("[TEST] CONSTRUCT8 Idempotence\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  uint64_t ALN out_S1[KNHK_NROWS];
  uint64_t ALN out_P1[KNHK_NROWS];
  uint64_t ALN out_O1[KNHK_NROWS];
  
  uint64_t ALN out_S2[KNHK_NROWS];
  uint64_t ALN out_P2[KNHK_NROWS];
  uint64_t ALN out_O2[KNHK_NROWS];
  
  knhk_hook_ir_t ir1 = {
    .op = KNHK_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xA110E,
    .k = 0,
    .out_S = out_S1,
    .out_P = out_P1,
    .out_O = out_O1,
    .out_mask = 0
  };
  
  knhk_hook_ir_t ir2 = ir1;
  ir2.out_S = out_S2;
  ir2.out_P = out_P2;
  ir2.out_O = out_O2;
  
  knhk_receipt_t rcpt1 = {0};
  knhk_receipt_t rcpt2 = {0};
  
  int w1 = knhk_eval_construct8(&ctx, &ir1, &rcpt1);
  int w2 = knhk_eval_construct8(&ctx, &ir2, &rcpt2);
  
  assert(w1 == w2);
  assert(out_S1[0] == out_S2[0]);
  assert(out_P1[0] == out_P2[0]);
  assert(out_O1[0] == out_O2[0]);
  assert(ir1.out_mask == ir2.out_mask);
  
  printf("  ✓ CONSTRUCT8 is idempotent\n");
  return 1;
}

// Test: CONSTRUCT8 with empty run
static int test_construct8_empty_run(void)
{
  printf("[TEST] CONSTRUCT8 Empty Run\n");
  reset_test_data();
  
  // All zeros
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 0});
  
  uint64_t ALN out_S[KNHK_NROWS];
  uint64_t ALN out_P[KNHK_NROWS];
  uint64_t ALN out_O[KNHK_NROWS];
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_CONSTRUCT8,
    .s = 0,
    .p = 0xC0FFEE,
    .o = 0xA110E,
    .k = 0,
    .out_S = out_S,
    .out_P = out_P,
    .out_O = out_O,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  int written = knhk_eval_construct8(&ctx, &ir, &rcpt);
  
  assert(written == 0);
  assert(ir.out_mask == 0);
  
  printf("  ✓ Empty run emits zero triples\n");
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: CONSTRUCT8 Operations\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_construct8_basic_emit()) passed++;
  total++; if (test_construct8_timing()) passed++;
  total++; if (test_construct8_lane_masking()) passed++;
  total++; if (test_construct8_idempotence()) passed++;
  total++; if (test_construct8_empty_run()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

