// chicago_basic_operations.c
// Basic operation tests: Authorization, Property Existence, Simple Lookups

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "chicago_test_helpers.h"

// Test Case 1: Authorization Checks (30% runtime)
static int test_authorization_checks(void)
{
  printf("[TEST] Test 1: Authorization Checks\n");

  // Initialize context
  knhks_init_ctx(&ctx, S, P, O);

  // Load authorization data
  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_authorization.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load authorization data\n");
    return 0;
  }

  // Assert invariant: predicate run size ≤8
  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  // Find a user and permission to test
  uint64_t test_user = ctx.S[0]; // First user
  uint64_t test_predicate = ctx.run.pred;

  // Create ASK query: Does user have permission?
  knhks_hook_ir_t ask_ir = {.op = KNHKS_OP_ASK_SP, .s = test_user, .p = test_predicate, .k = 0, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  // Test correctness: User should have permission
  int result = knhks_eval_bool(&ctx, &ask_ir, NULL);
  assert(result == 1);

  // Measure performance
  const int iterations = 400000;
  perf_stats_t stats = measure_p50_p95(&ask_ir, iterations);

  printf("  Triples=%zu, Predicate=0x%llx\n", ctx.triple_count, (unsigned long long)test_predicate);
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

// Test Case 2: Property Existence Validation (20% runtime)
static int test_property_existence(void)
{
  printf("[TEST] Test 2: Property Existence Validation\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_property.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load property data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t ask_ir = {.op = KNHKS_OP_ASK_SP, .s = test_subject, .p = test_predicate, .k = 0, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &ask_ir, NULL);
  assert(result == 1);

  const int iterations = 400000;
  perf_stats_t stats = measure_p50_p95(&ask_ir, iterations);

  printf("  Triples=%zu, Predicate=0x%llx\n", ctx.triple_count, (unsigned long long)test_predicate);
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

// Test Case 5: Simple Lookups (5% runtime)
static int test_simple_lookups(void)
{
  printf("[TEST] Test 5: Simple Lookups\n");

  knhks_init_ctx(&ctx, S, P, O);

  if (!knhks_load_rdf(&ctx, "tests/data/enterprise_lookup.ttl"))
  {
    fprintf(stderr, "  FAIL: Failed to load lookup data\n");
    return 0;
  }

  assert(ctx.triple_count <= NROWS);
  assert(ctx.run.len <= NROWS);

  uint64_t test_subject = ctx.S[0];
  uint64_t test_predicate = ctx.run.pred;

  knhks_hook_ir_t ask_ir = {.op = KNHKS_OP_ASK_SP, .s = test_subject, .p = test_predicate, .k = 0, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};

  int result = knhks_eval_bool(&ctx, &ask_ir, NULL);
  assert(result == 1);

  const int iterations = 400000;
  perf_stats_t stats = measure_p50_p95(&ask_ir, iterations);

  printf("  Triples=%zu, Predicate=0x%llx\n", ctx.triple_count, (unsigned long long)test_predicate);
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
int chicago_test_basic_operations(void)
{
  int passed = 0;
  int total = 3;

  if (test_authorization_checks())
    passed++;
  printf("\n");

  if (test_property_existence())
    passed++;
  printf("\n");

  if (test_simple_lookups())
    passed++;
  printf("\n");

  printf("Basic Operations: %d/%d tests passed\n", passed, total);
  return passed == total;
}

