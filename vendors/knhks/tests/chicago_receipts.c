// tests/chicago_receipts.c
// Chicago TDD: Receipt Generation and Provenance Tests
// Tests cryptographic receipts, hash equality, and receipt merging

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

// Test: Receipt generation for all operations
static int test_receipt_generation_all_ops(void)
{
  printf("[TEST] Receipt Generation for All Operations\n");
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
    
    assert(rcpt.ticks > 0);
    assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
    assert(rcpt.lanes > 0);
    assert(rcpt.a_hash != 0); // Hash fragment should be non-zero
    passed++;
  }
  
  printf("  ✓ All %zu operations generated valid receipts\n", sizeof(test_cases)/sizeof(test_cases[0]));
  return 1;
}

// Test: Receipt determinism (same inputs → same receipt)
static int test_receipt_determinism(void)
{
  printf("[TEST] Receipt Determinism\n");
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
  
  knhks_receipt_t rcpt1 = {0};
  knhks_receipt_t rcpt2 = {0};
  
  knhks_eval_bool(&ctx, &ir, &rcpt1);
  knhks_eval_bool(&ctx, &ir, &rcpt2);
  
  // Receipts should be identical for same inputs
  assert(rcpt1.ticks == rcpt2.ticks);
  assert(rcpt1.lanes == rcpt2.lanes);
  assert(rcpt1.a_hash == rcpt2.a_hash);
  
  printf("  ✓ Receipts are deterministic\n");
  return 1;
}

// Test: Receipt merge (Π ⊕) associativity
static int test_receipt_merge_associativity(void)
{
  printf("[TEST] Receipt Merge Associativity\n");
  
  knhks_receipt_t a = {.ticks = 4, .lanes = 8, .span_id = 0x1234, .a_hash = 0x5678};
  knhks_receipt_t b = {.ticks = 6, .lanes = 8, .span_id = 0xabcd, .a_hash = 0xef00};
  knhks_receipt_t c = {.ticks = 5, .lanes = 8, .span_id = 0x9999, .a_hash = 0xaaaa};
  
  // Test associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)
  knhks_receipt_t ab_then_c = knhks_receipt_merge(knhks_receipt_merge(a, b), c);
  knhks_receipt_t bc_then_a = knhks_receipt_merge(a, knhks_receipt_merge(b, c));
  
  assert(ab_then_c.ticks == bc_then_a.ticks); // max(max(4,6),5) = max(4,max(6,5)) = 6
  assert(ab_then_c.lanes == bc_then_a.lanes); // (8+8)+8 = 8+(8+8) = 24
  assert(ab_then_c.span_id == bc_then_a.span_id); // XOR is associative
  assert(ab_then_c.a_hash == bc_then_a.a_hash); // ⊕ is associative
  
  printf("  ✓ Receipt merge is associative\n");
  return 1;
}

// Test: Receipt timing accuracy (ticks ≤ 8)
static int test_receipt_timing_accuracy(void)
{
  printf("[TEST] Receipt Timing Accuracy\n");
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
  
  // Run 1000 times and verify all receipts within budget
  int violations = 0;
  for (int i = 0; i < 1000; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    if (rcpt.ticks > KNHKS_TICK_BUDGET) {
      violations++;
    }
  }
  
  assert(violations == 0); // No violations allowed
  
  printf("  ✓ All 1000 receipts within budget\n");
  return 1;
}

// Test: Hash equality property (hash(A) = hash(μ(O)))
static int test_hash_equality_property(void)
{
  printf("[TEST] Hash Equality Property\n");
  reset_test_data();
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  knhks_hook_ir_t ir1 = {
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
  
  knhks_hook_ir_t ir2 = ir1; // Same IR
  
  knhks_receipt_t rcpt1 = {0};
  knhks_receipt_t rcpt2 = {0};
  
  int result1 = knhks_eval_bool(&ctx, &ir1, &rcpt1);
  int result2 = knhks_eval_bool(&ctx, &ir2, &rcpt2);
  
  assert(result1 == result2); // Same result
  assert(rcpt1.a_hash == rcpt2.a_hash); // Same hash
  
  printf("  ✓ Hash equality property holds\n");
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Receipts & Provenance\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_receipt_generation_all_ops()) passed++;
  total++; if (test_receipt_determinism()) passed++;
  total++; if (test_receipt_merge_associativity()) passed++;
  total++; if (test_receipt_timing_accuracy()) passed++;
  total++; if (test_hash_equality_property()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

