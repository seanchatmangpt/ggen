// chicago_object_operations.c
// Object-based operation tests: Reverse lookup and object counting

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "chicago_test_helpers.h"

// Test Case 8: Reverse Lookup (ASK O,P)
static int test_reverse_lookup(void)
{
  printf("[TEST] Test 8: Reverse Lookup (ASK O,P)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_reverse.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load reverse lookup data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_object = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t ask_op_ir = {.op = KNHKS_OP_ASK_OP, .s = 0, .p = test_predicate, .k = 0, .o = test_object, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &ask_op_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&ask_op_ir, 200000);

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

// Test Case 10: Object Count (COUNT O,P)
static int test_object_count(void)
{
  printf("[TEST] Test 10: Object Count (COUNT O,P)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_object = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t count_ir = {.op = KNHKS_OP_COUNT_OP, .s = 0, .p = test_predicate, .k = 1, .o = test_object, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &count_ir, NULL);
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

// Test Case 11: Object Count MaxCount (COUNT O,P <= k)
static int test_object_count_maxcount(void)
{
  printf("[TEST] Test 11: Object Count MaxCount (COUNT O,P <= k)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_object = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t count_ir = {.op = KNHKS_OP_COUNT_OP_LE, .s = 0, .p = test_predicate, .k = 5, .o = test_object, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &count_ir, NULL);
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

// Test Case 12: Object Count Exact (COUNT O,P == k)
static int test_object_count_exact(void)
{
  printf("[TEST] Test 12: Object Count Exact (COUNT O,P == k)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_object = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t count_ir = {.op = KNHKS_OP_COUNT_OP_EQ, .s = 0, .p = test_predicate, .k = 3, .o = test_object, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &count_ir, NULL);
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

// Exported test functions
int chicago_test_object_operations(void)
{
  int passed = 0;
  int total = 4;

  if (test_reverse_lookup())
    passed++;
  printf("\n");

  if (test_object_count())
    passed++;
  printf("\n");

  if (test_object_count_maxcount())
    passed++;
  printf("\n");

  if (test_object_count_exact())
    passed++;
  printf("\n");

  printf("Object Operations Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

