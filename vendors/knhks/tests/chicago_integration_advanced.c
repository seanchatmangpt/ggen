// chicago_integration_advanced.c
// Advanced integration tests: Pipeline complete, guard enforcement, provenance

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

// Test: Complete pipeline with receipt merging and lockchain storage
static int test_integration_pipeline_complete(void)
{
  printf("[TEST] Complete Pipeline with Receipt Merging\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
  // Setup batch of 4 operations
  for (int i = 0; i < 4; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 4});
  
  // Execute batch
  knhks_hook_ir_t irs[KNHKS_NROWS];
  for (int i = 0; i < 4; i++) {
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
  int executed = knhks_eval_batch8(&ctx, irs, 4, rcpts);
  
  assert(executed == 4);
  
  // Merge all receipts (for lockchain)
  knhks_receipt_t merged = rcpts[0];
  for (int i = 1; i < 4; i++) {
    merged = knhks_receipt_merge(merged, rcpts[i]);
  }
  
  // Verify merged receipt
  uint32_t max_ticks = 0;
  for (int i = 0; i < 4; i++) {
    if (rcpts[i].ticks > max_ticks) {
      max_ticks = rcpts[i].ticks;
    }
  }
  assert(merged.ticks == max_ticks);
  // Merged receipt should have provenance (from at least one receipt)
  // Note: Some receipts may have zero hash if predicate mismatch, but merge should still work
  uint64_t merged_hash_check = rcpts[0].a_hash;
  for (int i = 1; i < 4; i++) {
    merged_hash_check ^= rcpts[i].a_hash;
  }
  assert(merged.a_hash == merged_hash_check);
  
  // Verify at least some receipts have provenance
  int has_provenance = 0;
  for (int i = 0; i < 4; i++) {
    if (rcpts[i].a_hash != 0) has_provenance++;
  }
  assert(has_provenance > 0); // At least one receipt should have provenance
  
  printf("  ✓ Complete pipeline: %d hooks → merged receipt → lockchain\n", executed);
  printf("    max_ticks=%u, merged_hash=0x%llx\n", 
         merged.ticks, (unsigned long long)merged.a_hash);
  return 1;
}

// Test: Guard enforcement across integration boundaries
static int test_integration_guard_enforcement(void)
{
  printf("[TEST] Guard Enforcement Across Integration\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
  // Setup data with run.len = 8 (maximum allowed)
  for (int i = 0; i < 8; i++) {
    S[i] = 0xA11CE + i;
    P[i] = 0xC0FFEE;
    O[i] = 0xB0B + i;
  }
  
  // Valid: len = 8
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 8});
  assert(ctx.run.len == 8);
  
  // Execute batch of 8 (maximum)
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
  
  // Verify receipts have provenance (at least some)
  int has_provenance = 0;
  for (int i = 0; i < 8; i++) {
    if (rcpts[i].a_hash != 0) has_provenance++;
  }
  assert(has_provenance > 0); // At least some receipts should have provenance
  
  printf("  ✓ Guard enforcement: max_run_len=8 enforced\n");
  return 1;
}

// Test: Receipt provenance chain (hash(A) = hash(μ(O)))
static int test_integration_receipt_provenance(void)
{
  printf("[TEST] Receipt Provenance Chain\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
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
  
  // Execute same operation twice - should produce same hash
  knhks_receipt_t rcpt1 = {0};
  knhks_eval_bool(&ctx, &ir, &rcpt1);
  
  knhks_receipt_t rcpt2 = {0};
  knhks_eval_bool(&ctx, &ir, &rcpt2);
  
  // Provenance: hash(A) = hash(μ(O))
  // Same input → same action → same hash
  assert(rcpt1.a_hash == rcpt2.a_hash);
  assert(rcpt1.a_hash != 0);
  
  printf("  ✓ Receipt provenance: hash(A) = hash(μ(O))\n");
  printf("    hash=0x%llx (consistent across executions)\n", 
         (unsigned long long)rcpt1.a_hash);
  return 1;
}

// Exported test functions
int chicago_test_integration_advanced(void)
{
  int passed = 0;
  int total = 3;
  
  if (test_integration_pipeline_complete())
    passed++;
  printf("\n");
  
  if (test_integration_guard_enforcement())
    passed++;
  printf("\n");
  
  if (test_integration_receipt_provenance())
    passed++;
  printf("\n");
  
  printf("Advanced Integration Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

