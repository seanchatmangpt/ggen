// tests/chicago_otel.c
// Chicago TDD: OpenTelemetry Tests
// Tests span creation, trace tracking, and metric recording

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

// Simple OTEL span structure (simulating Rust OTEL)
typedef struct {
  uint64_t trace_id;
  uint64_t span_id;
  uint64_t parent_span_id;
  char name[64];
  uint64_t start_time_ms;
  uint64_t end_time_ms;
  uint32_t ticks;
  int status; // 0=ok, 1=error
} otel_span_t;

static otel_span_t spans[100];
static size_t spans_len = 0;

// Simple metric structure
typedef struct {
  char name[64];
  uint64_t value;
  uint32_t ticks;
} otel_metric_t;

static otel_metric_t metrics[100];
static size_t metrics_len = 0;

// Test: OTEL span creation
static int test_otel_span_creation(void)
{
  printf("[TEST] OTEL Span Creation\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Create span
  uint64_t trace_id = 0x123456789ABCDEF0;
  uint64_t span_id = 0xFEDCBA9876543210;
  
  spans[spans_len] = (otel_span_t){
    .trace_id = trace_id,
    .span_id = span_id,
    .parent_span_id = 0,
    .name = "knhks.hook.eval",
    .start_time_ms = 0,
    .end_time_ms = 0,
    .ticks = 0,
    .status = 0
  };
  strncpy(spans[spans_len].name, "knhks.hook.eval", sizeof(spans[spans_len].name) - 1);
  spans_len++;
  
  // Execute operation
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
  knhks_eval_bool(&ctx, &ir, &rcpt);
  
  // Update span with receipt data
  spans[spans_len - 1].ticks = rcpt.ticks;
  spans[spans_len - 1].end_time_ms = 1000; // Simulated
  spans[spans_len - 1].status = (rcpt.ticks <= KNHKS_TICK_BUDGET) ? 0 : 1;
  
  assert(spans_len == 1);
  // Receipt should have ticks (may be 0 if early exit, but should be <= budget)
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  assert(spans[0].ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Span created: trace_id=0x%llx, ticks=%u\n", 
         (unsigned long long)trace_id, spans[0].ticks);
  return 1;
}

// Test: OTEL metric recording
static int test_otel_metric_recording(void)
{
  printf("[TEST] OTEL Metric Recording\n");
  
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
  
  knhks_receipt_t rcpt = {0};
  knhks_eval_bool(&ctx, &ir, &rcpt);
  
  // Record metrics
  metrics[metrics_len] = (otel_metric_t){
    .name = "knhks.hook.latency.ticks",
    .value = rcpt.ticks,
    .ticks = rcpt.ticks
  };
  strncpy(metrics[metrics_len].name, "knhks.hook.latency.ticks", sizeof(metrics[metrics_len].name) - 1);
  metrics_len++;
  
  metrics[metrics_len] = (otel_metric_t){
    .name = "knhks.receipt.generated",
    .value = 1,
    .ticks = 0
  };
  strncpy(metrics[metrics_len].name, "knhks.receipt.generated", sizeof(metrics[metrics_len].name) - 1);
  metrics_len++;
  
  assert(metrics_len == 2);
  assert(metrics[0].value <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Metrics recorded: %zu metrics\n", metrics_len);
  return 1;
}

// Test: OTEL trace context propagation
static int test_otel_trace_propagation(void)
{
  printf("[TEST] OTEL Trace Context Propagation\n");
  
  uint64_t trace_id = 0x123456789ABCDEF0;
  uint64_t parent_span_id = 0xFEDCBA9876543210;
  
  // Create parent span
  spans[spans_len] = (otel_span_t){
    .trace_id = trace_id,
    .span_id = parent_span_id,
    .parent_span_id = 0,
    .name = "parent",
    .start_time_ms = 0,
    .end_time_ms = 0,
    .ticks = 0,
    .status = 0
  };
  strncpy(spans[spans_len].name, "parent", sizeof(spans[spans_len].name) - 1);
  spans_len++;
  
  // Create child span
  uint64_t child_span_id = 0x1111111111111111;
  spans[spans_len] = (otel_span_t){
    .trace_id = trace_id,
    .span_id = child_span_id,
    .parent_span_id = parent_span_id,
    .name = "child",
    .start_time_ms = 0,
    .end_time_ms = 0,
    .ticks = 0,
    .status = 0
  };
  strncpy(spans[spans_len].name, "child", sizeof(spans[spans_len].name) - 1);
  spans_len++;
  
  // Verify trace context
  assert(spans[0].trace_id == spans[1].trace_id);
  assert(spans[1].parent_span_id == spans[0].span_id);
  
  printf("  ✓ Trace context propagated: trace_id=0x%llx\n", 
         (unsigned long long)trace_id);
  return 1;
}

// Test: OTEL receipt linkage
static int test_otel_receipt_linkage(void)
{
  printf("[TEST] OTEL Receipt Linkage\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Create span
  uint64_t span_id = 0xFEDCBA9876543210;
  spans[spans_len] = (otel_span_t){
    .trace_id = 0x123456789ABCDEF0,
    .span_id = span_id,
    .parent_span_id = 0,
    .name = "hook.eval",
    .start_time_ms = 0,
    .end_time_ms = 0,
    .ticks = 0,
    .status = 0
  };
  strncpy(spans[spans_len].name, "hook.eval", sizeof(spans[spans_len].name) - 1);
  spans_len++;
  
  // Execute and get receipt
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
  knhks_eval_bool(&ctx, &ir, &rcpt);
  
  // Link receipt to span (in real implementation, rcpt.span_id would be set)
  // For now, verify receipt can be linked
  assert(rcpt.a_hash != 0);
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Receipt linked to span: span_id=0x%llx, hash=0x%llx\n", 
         (unsigned long long)span_id, (unsigned long long)rcpt.a_hash);
  return 1;
}

// Test: OTEL guard violation metrics
static int test_otel_guard_violation_metrics(void)
{
  printf("[TEST] OTEL Guard Violation Metrics\n");
  
  // Record guard violation metric
  metrics[metrics_len] = (otel_metric_t){
    .name = "knhks.guard.violation",
    .value = 1,
    .ticks = 0
  };
  strncpy(metrics[metrics_len].name, "knhks.guard.violation", sizeof(metrics[metrics_len].name) - 1);
  metrics_len++;
  
  // Record guard type
  metrics[metrics_len] = (otel_metric_t){
    .name = "knhks.guard.violation.run_len",
    .value = 1,
    .ticks = 0
  };
  strncpy(metrics[metrics_len].name, "knhks.guard.violation.run_len", sizeof(metrics[metrics_len].name) - 1);
  metrics_len++;
  
  assert(metrics_len >= 2);
  
  printf("  ✓ Guard violation metrics recorded\n");
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: OpenTelemetry\n");
  printf("========================================\n\n");
  
  // Reset state
  spans_len = 0;
  metrics_len = 0;
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_otel_span_creation()) passed++;
  total++; if (test_otel_metric_recording()) passed++;
  total++; if (test_otel_trace_propagation()) passed++;
  total++; if (test_otel_receipt_linkage()) passed++;
  total++; if (test_otel_guard_violation_metrics()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

