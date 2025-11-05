// chicago_test_helpers.h
// Shared test infrastructure for Chicago TDD test suite
// Common utilities, direct wrappers, and performance measurement

#ifndef CHICAGO_TEST_HELPERS_H
#define CHICAGO_TEST_HELPERS_H

#include <stdint.h>
#include <stddef.h>
#include "knhks.h"
#include "rdf.h"

// Alignment macro for 64-byte aligned arrays
#define ALN __attribute__((aligned(64)))

// Test context (shared across all test files)
extern uint64_t ALN S[NROWS];
extern uint64_t ALN P[NROWS];
extern uint64_t ALN O[NROWS];
extern knhks_context_t ctx;

// Performance measurement structure
typedef struct
{
  double p50;
  double p95;
  double p50_ticks;
  double p95_ticks;
} perf_stats_t;

// Direct SIMD operation callers (bypass routing overhead)
// These directly call the SIMD functions to measure pure operation cost
static inline int direct_ask_sp(uint64_t s)
{
  return knhks_eq64_exists_8(ctx.S, ctx.run.off, s);
}

static inline int direct_ask_spo(uint64_t s, uint64_t o)
{
  return knhks_eq64_spo_exists_8(ctx.S, ctx.O, ctx.run.off, s, o);
}

static inline int direct_count_sp_ge(uint64_t s, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.S, ctx.run.off, s);
  return cnt >= k;
}

static inline int direct_count_sp_le(uint64_t s, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.S, ctx.run.off, s);
  return cnt <= k;
}

static inline int direct_count_sp_eq(uint64_t s, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.S, ctx.run.off, s);
  return cnt == k;
}

static inline int direct_ask_op(uint64_t o)
{
  return knhks_eq64_exists_o_8(ctx.O, ctx.run.off, o);
}

static inline int direct_unique_sp(uint64_t s)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.S, ctx.run.off, s);
  return cnt == 1;
}

static inline int direct_count_op(uint64_t o, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.O, ctx.run.off, o);
  return cnt >= k;
}

static inline int direct_count_op_le(uint64_t o, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.O, ctx.run.off, o);
  return cnt <= k;
}

static inline int direct_count_op_eq(uint64_t o, uint64_t k)
{
  uint64_t cnt = knhks_eq64_count_8(ctx.O, ctx.run.off, o);
  return cnt == k;
}

static inline size_t direct_select_sp(uint64_t s, uint64_t *out, size_t capacity)
{
  return knhks_select_gather_8(ctx.S, ctx.O, ctx.run.off, s, out, capacity);
}

static inline int direct_compare_o_eq(uint64_t o)
{
  return knhks_compare_o_8(ctx.O, ctx.run.off, o, 0); // 0 = EQ
}

static inline int direct_compare_o_gt(uint64_t o)
{
  return knhks_compare_o_8(ctx.O, ctx.run.off, o, 1); // 1 = GT
}

static inline int direct_compare_o_lt(uint64_t o)
{
  return knhks_compare_o_8(ctx.O, ctx.run.off, o, 2); // 2 = LT
}

static inline int direct_compare_o_ge(uint64_t o)
{
  return knhks_compare_o_8(ctx.O, ctx.run.off, o, 3); // 3 = GE
}

static inline int direct_compare_o_le(uint64_t o)
{
  return knhks_compare_o_8(ctx.O, ctx.run.off, o, 4); // 4 = LE
}

static inline int direct_validate_datatype_sp(uint64_t s, uint64_t datatype_hash)
{
  return knhks_validate_datatype_sp_8(ctx.S, ctx.O, ctx.run.off, s, datatype_hash);
}

// Performance measurement functions
perf_stats_t measure_p50_p95_select(knhks_hook_ir_t *ir, int iterations);
perf_stats_t measure_p50_p95(knhks_hook_ir_t *ir, int iterations);
int assert_performance_guard(perf_stats_t stats, double max_p50_ticks, double max_p95_ticks);
uint64_t get_count(uint64_t s, uint64_t p);

#endif // CHICAGO_TEST_HELPERS_H

