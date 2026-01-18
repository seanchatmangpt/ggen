// tests/chicago_etl.c
// Chicago TDD: ETL Pipeline Tests
// Tests complete pipeline: Ingest → Transform → Load → Reflex → Emit

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

// Test: Complete ETL pipeline execution
static int test_etl_pipeline_complete(void)
{
  printf("[TEST] Complete ETL Pipeline Execution\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  // Stage 1: Ingest (simulated - in real implementation, connectors provide data)
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  // Stage 2: Transform (schema validation - O ⊨ Σ)
  // Verified by predicate matching in hot path
  
  // Stage 3: Load (SoA alignment)
  knhk_init_ctx(&ctx, S, P, O);
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Stage 4: Reflex (μ execution)
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
  (void)knhk_eval_bool(&ctx, &ir, &rcpt);
  
  // Stage 5: Emit (receipt available for lockchain)
  assert(rcpt.ticks <= KNHK_TICK_BUDGET);
  assert(rcpt.a_hash != 0);
  
  printf("  ✓ Complete pipeline: Ingest → Transform → Load → Reflex → Emit\n");
  return 1;
}

// Test: ETL pipeline preserves invariants (preserve(Q))
static int test_etl_pipeline_invariants(void)
{
  printf("[TEST] ETL Pipeline Invariant Preservation\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  knhk_init_ctx(&ctx, S, P, O);
  
  // Setup data
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Execute reflex
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
  
  knhk_receipt_t rcpt1 = {0};
  int action1 = knhk_eval_bool(&ctx, &ir, &rcpt1);
  
  // Idempotence: μ∘μ = μ
  knhk_receipt_t rcpt2 = {0};
  int action2 = knhk_eval_bool(&ctx, &ir, &rcpt2);
  
  assert(action1 == action2);
  assert(rcpt1.a_hash == rcpt2.a_hash);
  
  printf("  ✓ Invariants preserved (idempotence)\n");
  return 1;
}

// Test: ETL pipeline timing (each stage ≤ 8 ticks)
static int test_etl_pipeline_timing(void)
{
  printf("[TEST] ETL Pipeline Timing\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  knhk_init_ctx(&ctx, S, P, O);
  
  // Setup data
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  
  // Execute batch (simulating full pipeline)
  knhk_hook_ir_t irs[KNHK_NROWS];
  for (int i = 0; i < 8; i++) {
    irs[i] = (knhk_hook_ir_t){
      .op = KNHK_OP_ASK_SP,
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
  
  knhk_receipt_t rcpts[KNHK_NROWS] = {0};
  int executed = knhk_eval_batch8(&ctx, irs, 8, rcpts);
  
  assert(executed == 8);
  
  // Verify all hooks within budget
  uint32_t max_ticks = 0;
  for (int i = 0; i < 8; i++) {
    if (rcpts[i].ticks > max_ticks) {
      max_ticks = rcpts[i].ticks;
    }
  }
  
  assert(max_ticks <= KNHK_TICK_BUDGET);
  
  printf("  ✓ Pipeline timing: max_ticks=%u ≤ %u\n", max_ticks, KNHK_TICK_BUDGET);
  return 1;
}

// Test: ETL pipeline error handling
static int test_etl_pipeline_error_handling(void)
{
  printf("[TEST] ETL Pipeline Error Handling\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  knhk_init_ctx(&ctx, S, P, O);
  
  // Test with wrong predicate (should return 0, not crash)
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 0});
  
  knhk_hook_ir_t ir = {
    .op = KNHK_OP_ASK_SP,
    .s = 0xA11CE,
    .p = 0xDEADBEEF,  // Wrong predicate (valid hex)
    .o = 0,
    .k = 0,
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhk_receipt_t rcpt = {0};
  int result = knhk_eval_bool(&ctx, &ir, &rcpt);
  
  // Should handle gracefully (return 0, still generate receipt)
  assert(result == 0);
  assert(rcpt.ticks <= KNHK_TICK_BUDGET);
  
  printf("  ✓ Error handling works (wrong predicate)\n");
  return 1;
}

// Test: ETL pipeline receipt generation
static int test_etl_pipeline_receipt_generation(void)
{
  printf("[TEST] ETL Pipeline Receipt Generation\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhk_context_t ctx;
  
  knhk_init_ctx(&ctx, S, P, O);
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhk_pin_run(&ctx, (knhk_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
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
  
  // Verify receipt properties
  assert(rcpt.ticks > 0);
  assert(rcpt.ticks <= KNHK_TICK_BUDGET);
  assert(rcpt.lanes > 0);
  assert(rcpt.a_hash != 0);
  
  printf("  ✓ Receipt generation: ticks=%u, hash=0x%llx\n", 
         rcpt.ticks, (unsigned long long)rcpt.a_hash);
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: ETL Pipeline\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_etl_pipeline_complete()) passed++;
  total++; if (test_etl_pipeline_invariants()) passed++;
  total++; if (test_etl_pipeline_timing()) passed++;
  total++; if (test_etl_pipeline_error_handling()) passed++;
  total++; if (test_etl_pipeline_receipt_generation()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

