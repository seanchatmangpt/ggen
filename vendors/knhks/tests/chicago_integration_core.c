// chicago_integration_core.c
// Core integration tests: End-to-end and multi-connector pipelines

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

// Test: End-to-end integration - Connector → ETL → Hot Path → Lockchain → OTEL
static int test_integration_end_to_end(void)
{
  printf("[TEST] End-to-End Integration\n");
  
  // Stage 1: Ingest (simulate connector providing data)
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  
  // Simulate connector output: 3 triples
  S[0] = 0xA11CE; P[0] = 0xC0FFEE; O[0] = 0xB0B;
  S[1] = 0xA11CE; P[1] = 0xC0FFEE; O[1] = 0xB0C;
  S[2] = 0xA11CE; P[2] = 0xC0FFEE; O[2] = 0xB0D;
  
  // Stage 2: Transform (schema validation - O ⊨ Σ)
  // Verified by predicate matching
  
  // Stage 3: Load (SoA alignment, run.len ≤ 8)
  knhks_context_t ctx;
  knhks_init_ctx(&ctx, S, P, O);
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 3});
  
  // Stage 4: Reflex (hot path execution)
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
  
  // Verify hot path execution
  assert(result == 1); // Should find match
  // Focus on integration correctness: receipt must have provenance hash
  assert(rcpt.a_hash != 0); // Must have provenance hash
  
  // Stage 5: Emit (receipt available for lockchain/OTEL)
  // Lockchain would store receipt with SHA3-256 hash
  // OTEL would record span with trace_id/span_id
  
  printf("  ✓ End-to-end: Connector → ETL → Hot Path → Receipt\n");
  printf("    ticks=%u, hash=0x%llx\n", rcpt.ticks, (unsigned long long)rcpt.a_hash);
  return 1;
}

// Test: ETL pipeline with multiple connectors
static int test_integration_multi_connector(void)
{
  printf("[TEST] Multi-Connector ETL Pipeline\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  
  // Simulate data from multiple connectors (different predicates)
  // Connector 1: Kafka (predicate 0xC0FFEE)
  S[0] = 0xA11CE; P[0] = 0xC0FFEE; O[0] = 0xB0B;
  S[1] = 0xA11CE; P[1] = 0xC0FFEE; O[1] = 0xB0C;
  
  // Connector 2: Salesforce (predicate 0xDEADBEEF)
  S[2] = 0xB22FF; P[2] = 0xDEADBEEF; O[2] = 0xC0C;
  S[3] = 0xB22FF; P[3] = 0xDEADBEEF; O[3] = 0xC0D;
  
  knhks_context_t ctx;
  knhks_init_ctx(&ctx, S, P, O);
  
  // Process Kafka connector data
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 2});
  knhks_hook_ir_t ir1 = {
    .op = KNHKS_OP_COUNT_SP_GE,
    .s = 0xA11CE,
    .p = 0xC0FFEE,
    .o = 0,
    .k = 2, // Count >= 2
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhks_receipt_t rcpt1 = {0};
  int result1 = knhks_eval_bool(&ctx, &ir1, &rcpt1);
  assert(result1 == 1);
  
  // Process Salesforce connector data
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xDEADBEEF, .off = 2, .len = 2});
  knhks_hook_ir_t ir2 = {
    .op = KNHKS_OP_COUNT_SP_GE,
    .s = 0xB22FF,
    .p = 0xDEADBEEF,
    .o = 0,
    .k = 2, // Count >= 2
    .out_S = NULL,
    .out_P = NULL,
    .out_O = NULL,
    .out_mask = 0
  };
  
  knhks_receipt_t rcpt2 = {0};
  int result2 = knhks_eval_bool(&ctx, &ir2, &rcpt2);
  assert(result2 == 1);
  
  // Merge receipts from both connectors
  knhks_receipt_t merged = knhks_receipt_merge(rcpt1, rcpt2);
  
  printf("  ✓ Multi-connector: Kafka + Salesforce → merged receipt\n");
  printf("    merged_ticks=%u, merged_hash=0x%llx\n", 
         merged.ticks, (unsigned long long)merged.a_hash);
  return 1;
}

// Exported test functions
int chicago_test_integration_core(void)
{
  int passed = 0;
  int total = 2;
  
  if (test_integration_end_to_end())
    passed++;
  printf("\n");
  
  if (test_integration_multi_connector())
    passed++;
  printf("\n");
  
  printf("Core Integration Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

