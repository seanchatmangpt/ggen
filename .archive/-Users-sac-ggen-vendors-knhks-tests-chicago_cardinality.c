// chicago_cardinality.c
// Cardinality constraint tests: COUNT operations and uniqueness validation

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "chicago_test_helpers.h"

// Test Case 3: Cardinality Constraints (15% runtime)
static int test_cardinality_constraints(void)
{
  printf("[TEST] Test 3: Cardinality Constraints\n");

  knhk_init_ctx(&ctx, S, P, O);

  if (!knhk_load_rdf(&ctx, "tests/data/enterprise_cardinality.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load cardinality data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_user = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhk_hook_ir_t count_ir = {.op = KNHK_OP_COUNT_SP_GE, .s = test_user, .p = test_predicate, .k = 1, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhk_eval_bool(&ctx, &count_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&count_ir, 200000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
  }
  else
  {
    printf("  Result: FAIL (performance exceeded)\n");
  }

  return perf_pass;
}

// Test Case 6: MaxCount Validation (COUNT <= k)
static int test_maxcount_validation(void)
{
  printf("[TEST] Test 6: MaxCount Validation (COUNT <= k)\n");

  knhk_init_ctx(&ctx, S, P, O);

  if (!knhk_load_rdf(&ctx, "tests/data/enterprise_maxcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load maxcount data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhk_hook_ir_t count_ir = {.op = KNHK_OP_COUNT_SP_LE, .s = test_subject, .p = test_predicate, .k = 3, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhk_eval_bool(&ctx, &count_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&count_ir, 200000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
  }
  else
  {
    printf("  Result: FAIL (performance exceeded)\n");
  }

  return perf_pass;
}

// Test Case 7: Exact Count Validation (COUNT == k)
static int test_exactcount_validation(void)
{
  printf("[TEST] Test 7: Exact Count Validation (COUNT == k)\n");

  knhk_init_ctx(&ctx, S, P, O);

  if (!knhk_load_rdf(&ctx, "tests/data/enterprise_exactcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load exactcount data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhk_hook_ir_t count_ir = {.op = KNHK_OP_COUNT_SP_EQ, .s = test_subject, .p = test_predicate, .k = 2, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhk_eval_bool(&ctx, &count_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&count_ir, 200000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
  }
  else
  {
    printf("  Result: FAIL (performance exceeded)\n");
  }

  return perf_pass;
}

// Test Case 9: Uniqueness Validation (COUNT == 1)
static int test_uniqueness_validation(void)
{
  printf("[TEST] Test 9: Uniqueness Validation (COUNT == 1)\n");

  knhk_init_ctx(&ctx, S, P, O);

  if (!knhk_load_rdf(&ctx, "tests/data/enterprise_uniqueness.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load uniqueness data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhk_hook_ir_t unique_ir = {.op = KNHK_OP_UNIQUE_SP, .s = test_subject, .p = test_predicate, .k = 0, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhk_eval_bool(&ctx, &unique_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&unique_ir, 200000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
  }
  else
  {
    printf("  Result: FAIL (performance exceeded)\n");
  }

  return perf_pass;
}

// Exported test functions
int chicago_test_cardinality(void)
{
  int passed = 0;
  int total = 4;

  if (test_cardinality_constraints())
    passed++;
  printf("\n");

  if (test_maxcount_validation())
    passed++;
  printf("\n");

  if (test_exactcount_validation())
    passed++;
  printf("\n");

  if (test_uniqueness_validation())
    passed++;
  printf("\n");

  printf("Cardinality Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

