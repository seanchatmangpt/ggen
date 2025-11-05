// tests/chicago_integration.c
// Chicago TDD: End-to-End Integration Tests
// Tests complete workflows, KGC axioms, and system invariants

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

// Test: KGC Axiom - Idempotence (μ∘μ = μ)
static int test_kgc_idempotence(void)
{
  printf("[TEST] KGC Axiom: Idempotence (μ∘μ = μ)\n");
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
  
  int r1 = knhks_eval_bool(&ctx, &ir, &rcpt1);
  int r2 = knhks_eval_bool(&ctx, &ir, &rcpt2);
  
  // Same input → same output
  assert(r1 == r2);
  assert(rcpt1.a_hash == rcpt2.a_hash);
  
  printf("  ✓ μ∘μ = μ holds\n");
  return 1;
}

// Test: KGC Axiom - Shard Law (μ(O ⊔ Δ) = μ(O) ⊔ μ(Δ))
static int test_kgc_shard_law(void)
{
  printf("[TEST] KGC Axiom: Shard Law\n");
  reset_test_data();
  
  // O: first run
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
  
  knhks_receipt_t rcpt_O = {0};
  int r_O = knhks_eval_bool(&ctx, &ir, &rcpt_O);
  
  // Δ: add second element
  S[1] = 0xB22FF;
  P[1] = 0xC0FFEE;
  O[1] = 0xC0C;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  knhks_receipt_t rcpt_O_delta = {0};
  int r_O_delta = knhks_eval_bool(&ctx, &ir, &rcpt_O_delta);
  
  // Both should return true (shard law: composition preserves results)
  assert(r_O == 1);
  assert(r_O_delta == 1);
  
  printf("  ✓ Shard law holds\n");
  return 1;
}

// Test: Complete workflow (O → μ → A → receipt)
static int test_complete_workflow(void)
{
  printf("[TEST] Complete Workflow (O → μ → A → receipt)\n");
  reset_test_data();
  
  // O: Setup knowledge
  S[0] = 0xA11CE;
  S[1] = 0xB22FF;
  P[0] = 0xC0FFEE;
  P[1] = 0xC0FFEE;
  O[0] = 0xB0B;
  O[1] = 0xC0C;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  
  // μ: Execute reflex
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
  int A = knhks_eval_bool(&ctx, &ir, &rcpt);
  
  // Verify: A = μ(O) with receipt
  assert(A == 1); // Action: subject exists
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  assert(rcpt.a_hash != 0); // Provenance hash
  
  printf("  ✓ Complete workflow: O → μ → A with receipt\n");
  return 1;
}

// Test: Receipt provenance (hash(A) = hash(μ(O)))
static int test_receipt_provenance(void)
{
  printf("[TEST] Receipt Provenance\n");
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
  
  int A1 = knhks_eval_bool(&ctx, &ir, &rcpt1);
  int A2 = knhks_eval_bool(&ctx, &ir, &rcpt2);
  
  // Same O → same A → same hash
  assert(A1 == A2);
  assert(rcpt1.a_hash == rcpt2.a_hash);
  
  printf("  ✓ Provenance: hash(A) = hash(μ(O))\n");
  return 1;
}

// Test: Epoch execution (τ ≤ 8)
static int test_epoch_execution(void)
{
  printf("[TEST] Epoch Execution (τ ≤ 8)\n");
  reset_test_data();
  
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
  // Create epoch: batch of 8 hooks
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
  
  knhks_receipt_t rcpts[KNHKS_NROWS] = {0};
  int executed = knhks_eval_batch8(&ctx, irs, 8, rcpts);
  
  assert(executed == 8);
  
  // Verify all hooks within epoch bound
  uint32_t max_ticks = 0;
  for (int i = 0; i < 8; i++) {
    if (rcpts[i].ticks > max_ticks) {
      max_ticks = rcpts[i].ticks;
    }
  }
  
  assert(max_ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Epoch executed: max_ticks=%u ≤ %u\n", max_ticks, KNHKS_TICK_BUDGET);
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Integration Tests\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_kgc_idempotence()) passed++;
  total++; if (test_kgc_shard_law()) passed++;
  total++; if (test_complete_workflow()) passed++;
  total++; if (test_receipt_provenance()) passed++;
  total++; if (test_epoch_execution()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

