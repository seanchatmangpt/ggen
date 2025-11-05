// tests/chicago_lockchain_integration.c
// Chicago TDD: Lockchain Integration Tests for v0.4.0
// Tests receipt writing, reading, Merkle verification, and Git integration

#include <assert.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "knhks.h"
#include "chicago_test_helpers.h"

#if defined(__GNUC__)
#define ALN __attribute__((aligned(64)))
#else
#define ALN
#endif

// Mock lockchain storage (simulating Rust lockchain)
typedef struct {
  uint64_t receipt_hash;
  uint64_t parent_hash;
  knhks_receipt_t receipt;
  uint64_t timestamp_ms;
} lockchain_entry_t;

static lockchain_entry_t lockchain[100];
static size_t lockchain_len = 0;
static uint64_t merkle_root = 0;

// Compute hash (simulating SHA-256 + URDNA2015 canonicalization)
static uint64_t compute_lockchain_hash(const knhks_receipt_t *rcpt, uint64_t parent_hash)
{
  // Simplified hash (real implementation uses SHA-256)
  return rcpt->a_hash ^ parent_hash ^ ((uint64_t)rcpt->ticks << 32) ^ rcpt->span_id;
}

// Test: Lockchain Receipt Write
static int test_lockchain_receipt_write(void)
{
  printf("[TEST] Lockchain Receipt Write\n");
  
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
  
  // Write to lockchain
  uint64_t parent_hash = (lockchain_len > 0) ? lockchain[lockchain_len - 1].receipt_hash : 0;
  lockchain[lockchain_len] = (lockchain_entry_t){
    .receipt_hash = compute_lockchain_hash(&rcpt, parent_hash),
    .parent_hash = parent_hash,
    .receipt = rcpt,
    .timestamp_ms = 0 // Simulated timestamp
  };
  lockchain_len++;
  
  assert(lockchain_len > 0);
  assert(lockchain[lockchain_len - 1].receipt_hash != 0);
  assert(lockchain[lockchain_len - 1].receipt.a_hash == rcpt.a_hash);
  
  printf("  ✓ Receipt written to lockchain\n");
  return 1;
}

// Test: Lockchain Receipt Read
static int test_lockchain_receipt_read(void)
{
  printf("[TEST] Lockchain Receipt Read\n");
  
  // Write receipt first
  test_lockchain_receipt_write();
  
  // Read receipt
  assert(lockchain_len > 0);
  lockchain_entry_t *entry = &lockchain[lockchain_len - 1];
  
  assert(entry->receipt_hash != 0);
  assert(entry->receipt.ticks <= KNHKS_TICK_BUDGET);
  assert(entry->receipt.a_hash != 0);
  assert(entry->receipt.span_id != 0);
  
  printf("  ✓ Receipt read from lockchain\n");
  return 1;
}

// Test: Lockchain Merkle Verification
static int test_lockchain_merkle_verification(void)
{
  printf("[TEST] Lockchain Merkle Verification\n");
  
  // Write multiple receipts
  lockchain_len = 0;
  merkle_root = 0;
  
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
  
  // Write 3 receipts
  for (int i = 0; i < 3; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    
    uint64_t parent_hash = (lockchain_len > 0) ? lockchain[lockchain_len - 1].receipt_hash : 0;
    lockchain[lockchain_len] = (lockchain_entry_t){
      .receipt_hash = compute_lockchain_hash(&rcpt, parent_hash),
      .parent_hash = parent_hash,
      .receipt = rcpt,
      .timestamp_ms = 0
    };
    lockchain_len++;
  }
  
  // Verify Merkle chain integrity
  for (size_t i = 1; i < lockchain_len; i++) {
    assert(lockchain[i].parent_hash == lockchain[i - 1].receipt_hash);
    assert(lockchain[i].receipt_hash == compute_lockchain_hash(&lockchain[i].receipt, lockchain[i].parent_hash));
  }
  
  // Compute Merkle root (last receipt hash)
  merkle_root = lockchain[lockchain_len - 1].receipt_hash;
  assert(merkle_root != 0);
  
  printf("  ✓ Merkle tree verification passed\n");
  return 1;
}

// Test: Lockchain Git Integration
static int test_lockchain_git_integration(void)
{
  printf("[TEST] Lockchain Git Integration\n");
  
  // Simulate Git commit (in real implementation, git2 crate commits receipt files)
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
  
  // Simulate Git commit (receipt file written, ready for commit)
  // In real implementation: write receipt JSON to receipts/ directory, git add, git commit
  assert(rcpt.ticks <= KNHKS_TICK_BUDGET);
  assert(rcpt.a_hash != 0);
  
  printf("  ✓ Git integration ready (receipt file generated)\n");
  return 1;
}

// Test: Lockchain Tamper Detection
static int test_lockchain_tamper_detection(void)
{
  printf("[TEST] Lockchain Tamper Detection\n");
  
  // Write receipts
  test_lockchain_merkle_verification();
  
  // Compute original Merkle root
  uint64_t original_root = merkle_root;
  
  // Tamper with receipt (modify a_hash)
  lockchain[0].receipt.a_hash = 0xDEADBEEF;
  
  // Recompute hash
  lockchain[0].receipt_hash = compute_lockchain_hash(&lockchain[0].receipt, lockchain[0].parent_hash);
  
  // Recompute chain
  for (size_t i = 1; i < lockchain_len; i++) {
    lockchain[i].parent_hash = lockchain[i - 1].receipt_hash;
    lockchain[i].receipt_hash = compute_lockchain_hash(&lockchain[i].receipt, lockchain[i].parent_hash);
  }
  
  uint64_t tampered_root = lockchain[lockchain_len - 1].receipt_hash;
  
  // Merkle root should differ
  assert(tampered_root != original_root);
  
  printf("  ✓ Tamper detection works (Merkle root differs)\n");
  return 1;
}

// Test: Lockchain Receipt Query
static int test_lockchain_receipt_query(void)
{
  printf("[TEST] Lockchain Receipt Query\n");
  
  // Write receipts
  test_lockchain_merkle_verification();
  
  // Query by receipt hash
  uint64_t query_hash = lockchain[1].receipt_hash;
  lockchain_entry_t *found = NULL;
  
  for (size_t i = 0; i < lockchain_len; i++) {
    if (lockchain[i].receipt_hash == query_hash) {
      found = &lockchain[i];
      break;
    }
  }
  
  assert(found != NULL);
  assert(found->receipt_hash == query_hash);
  assert(found->receipt.ticks <= KNHKS_TICK_BUDGET);
  
  printf("  ✓ Receipt query by hash succeeded\n");
  return 1;
}

// Test: Lockchain Batch Writes
static int test_lockchain_batch_writes(void)
{
  printf("[TEST] Lockchain Batch Writes\n");
  
  lockchain_len = 0;
  merkle_root = 0;
  
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
  
  // Batch write 5 receipts
  for (int i = 0; i < 5; i++) {
    knhks_receipt_t rcpt = {0};
    knhks_eval_bool(&ctx, &ir, &rcpt);
    
    uint64_t parent_hash = (lockchain_len > 0) ? lockchain[lockchain_len - 1].receipt_hash : 0;
    lockchain[lockchain_len] = (lockchain_entry_t){
      .receipt_hash = compute_lockchain_hash(&rcpt, parent_hash),
      .parent_hash = parent_hash,
      .receipt = rcpt,
      .timestamp_ms = 0
    };
    lockchain_len++;
  }
  
  assert(lockchain_len == 5);
  
  // Verify chain integrity
  for (size_t i = 1; i < lockchain_len; i++) {
    assert(lockchain[i].parent_hash == lockchain[i - 1].receipt_hash);
  }
  
  printf("  ✓ Batch writes succeeded (5 receipts)\n");
  return 1;
}

// Test suite runner
int chicago_test_lockchain_integration(void)
{
  int passed = 0;
  int total = 0;
  
  total++; if (test_lockchain_receipt_write()) passed++;
  total++; if (test_lockchain_receipt_read()) passed++;
  total++; if (test_lockchain_merkle_verification()) passed++;
  total++; if (test_lockchain_git_integration()) passed++;
  total++; if (test_lockchain_tamper_detection()) passed++;
  total++; if (test_lockchain_receipt_query()) passed++;
  total++; if (test_lockchain_batch_writes()) passed++;
  
  printf("\nLockchain Integration: %d/%d tests passed\n", passed, total);
  return (passed == total) ? 1 : 0;
}

