// chicago_test_helpers.c
// Shared test infrastructure implementation

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chicago_test_helpers.h"

// Alignment macro for 64-byte aligned arrays
#define ALN __attribute__((aligned(64)))

// Test context (shared across all test files)
uint64_t ALN S[NROWS];
uint64_t ALN P[NROWS];
uint64_t ALN O[NROWS];
knhks_context_t ctx;

// Measure p50/p95 percentiles for SELECT operations
perf_stats_t measure_p50_p95_select(knhks_hook_ir_t *ir, int iterations)
{
  const int batch_size = 1000;
  const int num_batches = iterations / batch_size;
  const int discard_batches = 4;
  const int measure_batches = num_batches - discard_batches;

  if (measure_batches < 2)
  {
    perf_stats_t fail = {0, 0, 999, 999};
    return fail;
  }

  uint64_t *batch_times = malloc(measure_batches * sizeof(uint64_t));
  assert(batch_times != NULL);

  double hz = knhks_ticks_hz();

  // Warm cache
  volatile uint64_t dummy = 0;
  static uint64_t ALN out_buffer[8];
  for (int pass = 0; pass < 8; pass++)
  {
    for (unsigned int i = 0; i < NROWS; i++)
    {
      dummy ^= ctx.S[i];
      dummy ^= ctx.P[i];
      dummy ^= ctx.O[i];
    }
  }
  for (int i = 0; i < 8192; i++)
  {
    dummy ^= direct_select_sp(ir->s, out_buffer, 8);
  }
  (void)dummy;

  // Measure loop overhead
  volatile size_t sink = 0;
  for (int w = 0; w < 10; w++)
  {
    for (int i = 0; i < batch_size; i++)
      sink ^= 0;
  }

  uint64_t overhead_t0 = knhks_rd_ticks();
  for (int i = 0; i < batch_size; i++)
    sink ^= 0;
  uint64_t overhead_t1 = knhks_rd_ticks();
  uint64_t loop_overhead = overhead_t1 - overhead_t0;
  (void)sink;

  // Measure batches
  int batch_idx = 0;
  for (int b = 0; b < num_batches; b++)
  {
    uint64_t t0 = knhks_rd_ticks();
    volatile size_t sink2 = 0;
    for (int i = 0; i < batch_size; i++)
    {
      sink2 ^= direct_select_sp(ir->s, out_buffer, 8);
    }
    uint64_t t1 = knhks_rd_ticks();
    (void)sink2;

    if (b >= discard_batches)
    {
      batch_times[batch_idx] = (t1 > t0 + loop_overhead) ? (t1 - t0 - loop_overhead) : 0;
      batch_idx++;
    }
  }

  assert(batch_idx == measure_batches);

  // Sort and calculate percentiles
  for (int i = 0; i < measure_batches - 1; i++)
  {
    for (int j = i + 1; j < measure_batches; j++)
    {
      if (batch_times[i] > batch_times[j])
      {
        uint64_t tmp = batch_times[i];
        batch_times[i] = batch_times[j];
        batch_times[j] = tmp;
      }
    }
  }

  int p50_idx = measure_batches / 2;
  int p95_idx = (int)(measure_batches * 0.95);

  perf_stats_t stats;
  double p50_batch_ns = ((double)batch_times[p50_idx] / hz) * 1e9;
  double p95_batch_ns = ((double)batch_times[p95_idx] / hz) * 1e9;

  stats.p50 = p50_batch_ns / batch_size;
  stats.p95 = p95_batch_ns / batch_size;

  const double tick_ns = 0.25;
  stats.p50_ticks = stats.p50 / tick_ns;
  stats.p95_ticks = stats.p95 / tick_ns;

  free(batch_times);
  return stats;
}

// Measure p50/p95 percentiles - zero overhead measurement
perf_stats_t measure_p50_p95(knhks_hook_ir_t *ir, int iterations)
{
  const int batch_size = 1000;
  const int num_batches = iterations / batch_size;
  const int discard_batches = 4;
  const int measure_batches = num_batches - discard_batches;

  if (measure_batches < 2)
  {
    fprintf(stderr, "  ERROR: Not enough batches after discarding cold path\n");
    perf_stats_t fail = {0, 0, 999, 999};
    return fail;
  }

  uint64_t *batch_times = malloc(measure_batches * sizeof(uint64_t));
  assert(batch_times != NULL);

  double hz = knhks_ticks_hz();

  // Warm cache
  volatile uint64_t dummy = 0;
  for (int pass = 0; pass < 8; pass++)
  {
    for (unsigned int i = 0; i < NROWS; i++)
    {
      dummy ^= ctx.S[i];
      dummy ^= ctx.P[i];
      dummy ^= ctx.O[i];
    }
  }
  // Warmup the specific operation path
  for (int i = 0; i < 8192; i++)
  {
    switch (ir->op)
    {
    case KNHKS_OP_ASK_SP:
      dummy ^= direct_ask_sp(ir->s);
      break;
    case KNHKS_OP_ASK_SPO:
      dummy ^= direct_ask_spo(ir->s, ir->o);
      break;
    case KNHKS_OP_COUNT_SP_GE:
      dummy ^= direct_count_sp_ge(ir->s, ir->k);
      break;
    case KNHKS_OP_COUNT_SP_LE:
      dummy ^= direct_count_sp_le(ir->s, ir->k);
      break;
    case KNHKS_OP_COUNT_SP_EQ:
      dummy ^= direct_count_sp_eq(ir->s, ir->k);
      break;
    case KNHKS_OP_ASK_OP:
      dummy ^= direct_ask_op(ir->o);
      break;
    case KNHKS_OP_UNIQUE_SP:
      dummy ^= direct_unique_sp(ir->s);
      break;
    case KNHKS_OP_COUNT_OP:
      dummy ^= direct_count_op(ir->o, ir->k);
      break;
    case KNHKS_OP_COUNT_OP_LE:
      dummy ^= direct_count_op_le(ir->o, ir->k);
      break;
    case KNHKS_OP_COUNT_OP_EQ:
      dummy ^= direct_count_op_eq(ir->o, ir->k);
      break;
    case KNHKS_OP_COMPARE_O_EQ:
      dummy ^= direct_compare_o_eq(ir->o);
      break;
    case KNHKS_OP_COMPARE_O_GT:
      dummy ^= direct_compare_o_gt(ir->o);
      break;
    case KNHKS_OP_COMPARE_O_LT:
      dummy ^= direct_compare_o_lt(ir->o);
      break;
    case KNHKS_OP_COMPARE_O_GE:
      dummy ^= direct_compare_o_ge(ir->o);
      break;
    case KNHKS_OP_COMPARE_O_LE:
      dummy ^= direct_compare_o_le(ir->o);
      break;
    case KNHKS_OP_VALIDATE_DATATYPE_SP:
      dummy ^= direct_validate_datatype_sp(ir->s, ir->o);
      break;
    default:
      dummy ^= 0;
      break;
    }
  }
  (void)dummy;

  // Measure loop overhead
  volatile int sink = 0;
  for (int w = 0; w < 10; w++)
  {
    for (int i = 0; i < batch_size; i++)
      sink ^= 0;
  }

  uint64_t overhead_t0 = knhks_rd_ticks();
  for (int i = 0; i < batch_size; i++)
  {
    sink ^= 0;
  }
  uint64_t overhead_t1 = knhks_rd_ticks();
  uint64_t loop_overhead = overhead_t1 - overhead_t0;
  (void)sink;

  // Measure batches
  int batch_idx = 0;
  for (int b = 0; b < num_batches; b++)
  {
    uint64_t t0 = knhks_rd_ticks();
    volatile int sink2 = 0;
    for (int i = 0; i < batch_size; i++)
    {
      switch (ir->op)
      {
      case KNHKS_OP_ASK_SP:
        sink2 ^= direct_ask_sp(ir->s);
        break;
      case KNHKS_OP_ASK_SPO:
        sink2 ^= direct_ask_spo(ir->s, ir->o);
        break;
      case KNHKS_OP_COUNT_SP_GE:
        sink2 ^= direct_count_sp_ge(ir->s, ir->k);
        break;
      case KNHKS_OP_COUNT_SP_LE:
        sink2 ^= direct_count_sp_le(ir->s, ir->k);
        break;
      case KNHKS_OP_COUNT_SP_EQ:
        sink2 ^= direct_count_sp_eq(ir->s, ir->k);
        break;
      case KNHKS_OP_ASK_OP:
        sink2 ^= direct_ask_op(ir->o);
        break;
      case KNHKS_OP_UNIQUE_SP:
        sink2 ^= direct_unique_sp(ir->s);
        break;
      case KNHKS_OP_COUNT_OP:
        sink2 ^= direct_count_op(ir->o, ir->k);
        break;
      case KNHKS_OP_COUNT_OP_LE:
        sink2 ^= direct_count_op_le(ir->o, ir->k);
        break;
      case KNHKS_OP_COUNT_OP_EQ:
        sink2 ^= direct_count_op_eq(ir->o, ir->k);
        break;
      case KNHKS_OP_COMPARE_O_EQ:
        sink2 ^= direct_compare_o_eq(ir->o);
        break;
      case KNHKS_OP_COMPARE_O_GT:
        sink2 ^= direct_compare_o_gt(ir->o);
        break;
      case KNHKS_OP_COMPARE_O_LT:
        sink2 ^= direct_compare_o_lt(ir->o);
        break;
      case KNHKS_OP_COMPARE_O_GE:
        sink2 ^= direct_compare_o_ge(ir->o);
        break;
      case KNHKS_OP_COMPARE_O_LE:
        sink2 ^= direct_compare_o_le(ir->o);
        break;
      case KNHKS_OP_VALIDATE_DATATYPE_SP:
        sink2 ^= direct_validate_datatype_sp(ir->s, ir->o);
        break;
      default:
        sink2 ^= 0;
        break;
      }
    }
    uint64_t t1 = knhks_rd_ticks();
    (void)sink2;

    if (b >= discard_batches)
    {
      batch_times[batch_idx] = (t1 > t0 + loop_overhead) ? (t1 - t0 - loop_overhead) : 0;
      batch_idx++;
    }
  }

  assert(batch_idx == measure_batches);

  // Sort batches for percentile calculation
  for (int i = 0; i < measure_batches - 1; i++)
  {
    for (int j = i + 1; j < measure_batches; j++)
    {
      if (batch_times[i] > batch_times[j])
      {
        uint64_t tmp = batch_times[i];
        batch_times[i] = batch_times[j];
        batch_times[j] = tmp;
      }
    }
  }

  int p50_idx = measure_batches / 2;
  int p95_idx = (int)(measure_batches * 0.95);

  perf_stats_t stats;
  double p50_batch_ns = ((double)batch_times[p50_idx] / hz) * 1e9;
  double p95_batch_ns = ((double)batch_times[p95_idx] / hz) * 1e9;

  stats.p50 = p50_batch_ns / batch_size;
  stats.p95 = p95_batch_ns / batch_size;

  const double tick_ns = 0.25;
  stats.p50_ticks = stats.p50 / tick_ns;
  stats.p95_ticks = stats.p95 / tick_ns;

  free(batch_times);
  return stats;
}

// Assert performance guard
int assert_performance_guard(perf_stats_t stats, double max_p50_ticks, double max_p95_ticks)
{
  int pass = 1;
  if (stats.p50_ticks > max_p50_ticks)
  {
    fprintf(stderr, "  FAIL: p50 exceeds threshold (%.2f > %.2f ticks)\n", stats.p50_ticks, max_p50_ticks);
    pass = 0;
  }
  if (stats.p95_ticks > max_p95_ticks)
  {
    fprintf(stderr, "  FAIL: p95 exceeds threshold (%.2f > %.2f ticks)\n", stats.p95_ticks, max_p95_ticks);
    pass = 0;
  }
  return pass;
}

// Helper to get count
uint64_t get_count(uint64_t s, uint64_t p)
{
  knhks_hook_ir_t ir = {.op = KNHKS_OP_COUNT_SP_GE, .s = s, .p = p, .k = 1, .o = 0, .select_out = NULL, .select_capacity = 0, .out_S = NULL, .out_P = NULL, .out_O = NULL, .out_mask = 0};
  return knhks_eval_bool(&ctx, &ir, NULL) ? 1 : 0;
}

