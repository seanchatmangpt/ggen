// chicago_advanced.c
// Advanced operation tests: Type checking, SELECT, COMPARE, VALIDATE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "chicago_test_helpers.h"

// Test Case 4: Type Checking (10% runtime)
static int test_type_checking(void)
{
  printf("[TEST] Test 4: Type Checking\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_types.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load types data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_resource = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t ask_ir = {.op = KNHKS_OP_ASK_SP, .s = test_resource, .p = test_predicate, .k = 0, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &ask_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&ask_ir, 400000);

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

// Test Case 13: SELECT_SP Operation
static int test_select_sp(void)
{
  printf("[TEST] Test 13: SELECT_SP Operation\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_lookups.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load lookups data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_entity = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;
  static uint64_t ALN out_buffer[8];

  knhks_hook_ir_t select_ir = {.op = KNHKS_OP_SELECT_SP, .s = test_entity, .p = test_predicate, .k = 0, .o = 0, .select_out = out_buffer, .select_capacity = 8, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  size_t count = knhks_eval_select(&ctx, &select_ir);
  assert(count > 0);

  perf_stats_t stats = measure_p50_p95_select(&select_ir, 400000);

  printf("  Triples=%zu, Results=%zu\n", ctx.triple_count, count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 14: Comparison Operations (EQ)
static int test_compare_eq(void)
{
  printf("[TEST] Test 14: Comparison Operations (O == value)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_value = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t compare_ir = {.op = KNHKS_OP_COMPARE_O_EQ, .s = 0, .p = test_predicate, .k = 0, .o = test_value, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &compare_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&compare_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 15: Comparison Operations (GT)
static int test_compare_gt(void)
{
  printf("[TEST] Test 15: Comparison Operations (O > value)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_value = ctx.O[0] - 1;
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t compare_ir = {.op = KNHKS_OP_COMPARE_O_GT, .s = 0, .p = test_predicate, .k = 0, .o = test_value, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &compare_ir, NULL);

  perf_stats_t stats = measure_p50_p95(&compare_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 16: Comparison Operations (LT)
static int test_compare_lt(void)
{
  printf("[TEST] Test 16: Comparison Operations (O < value)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_value = ctx.O[0] + 1;
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t compare_ir = {.op = KNHKS_OP_COMPARE_O_LT, .s = 0, .p = test_predicate, .k = 0, .o = test_value, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &compare_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&compare_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 17: Comparison Operations (GE)
static int test_compare_ge(void)
{
  printf("[TEST] Test 17: Comparison Operations (O >= value)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_value = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t compare_ir = {.op = KNHKS_OP_COMPARE_O_GE, .s = 0, .p = test_predicate, .k = 0, .o = test_value, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &compare_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&compare_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 18: Comparison Operations (LE)
static int test_compare_le(void)
{
  printf("[TEST] Test 18: Comparison Operations (O <= value)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_objectcount.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load object count data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_value = ctx.O[0] + 1;
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t compare_ir = {.op = KNHKS_OP_COMPARE_O_LE, .s = 0, .p = test_predicate, .k = 0, .o = test_value, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &compare_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&compare_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Test Case 19: Datatype Validation (SP)
static int test_datatype_validation_sp(void)
{
  printf("[TEST] Test 19: Datatype Validation (SP)\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_datatype.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load datatype validation data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_object = ctx.O[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t validate_ir = {.op = KNHKS_OP_VALIDATE_DATATYPE_SP, .s = test_subject, .p = test_predicate, .k = 0, .o = test_object, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &validate_ir, NULL);
  assert(result == 1);

  perf_stats_t stats = measure_p50_p95(&validate_ir, 400000);

  printf("  Triples=%zu\n", ctx.triple_count);
  printf("  p50: %.2f ticks (%.3f ns)\n", stats.p50_ticks, stats.p50);
  printf("  p95: %.2f ticks (%.3f ns)\n", stats.p95_ticks, stats.p95);

  int perf_pass = assert_performance_guard(stats, 8.0, 8.0);
  if (perf_pass)
  {
    printf("  Result: PASS (≤8 ticks)\n");
    return 1;
  }
  else
  {
    printf("  Result: FAIL (exceeds 8 ticks)\n");
    return 0;
  }
}

// Exported test functions
int chicago_test_advanced(void)
{
  int passed = 0;
  int total = 8;

  if (test_type_checking())
    passed++;
  printf("\n");

  if (test_select_sp())
    passed++;
  printf("\n");

  if (test_compare_eq())
    passed++;
  printf("\n");

  if (test_compare_gt())
    passed++;
  printf("\n");

  if (test_compare_lt())
    passed++;
  printf("\n");

  if (test_compare_ge())
    passed++;
  printf("\n");

  if (test_compare_le())
    passed++;
  printf("\n");

  if (test_datatype_validation_sp())
    passed++;
  printf("\n");

  printf("Advanced Tests: %d/%d tests passed\n", passed, total);
  return passed == total;
}

