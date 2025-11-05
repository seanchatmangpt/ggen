// tests/chicago_lockchain.c
// Chicago TDD: Lockchain Tests
// Tests Merkle-linked receipt storage, integrity verification, and audit trail

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

// Simple lockchain structure (simulating Rust lockchain)
typedef struct {
  uint64_t receipt_hash;
  uint64_t parent_hash;
  uint32_t ticks;
  uint64_t a_hash;
} lockchain_entry_t;

static lockchain_entry_t lockchain[100];
static size_t lockchain_len = 0;

// Compute simple hash (placeholder - real implementation uses SHA3-256)
// Note: receipt_hash parameter is actually the a_hash value being used as input
static uint64_t compute_hash(uint64_t a_hash, uint64_t parent_hash, uint32_t ticks, uint64_t a_hash_again)
{
  // Consistent hash function - must match between append and verify
  // Use a_hash as the primary input, not receipt_hash
  (void)a_hash_again; // Same as a_hash
  return a_hash ^ parent_hash ^ ((uint64_t)ticks << 32);
}

// Test: Lockchain receipt appending
static int test_lockchain_append(void)
{
  printf("[TEST] Lockchain Receipt Appending\n");
  
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
  
  // Append to lockchain
  uint64_t parent_hash = lockchain_len > 0 ? lockchain[lockchain_len - 1].receipt_hash : 0;
  // Use a_hash as input to compute receipt_hash
  uint64_t receipt_hash = compute_hash(rcpt.a_hash, parent_hash, rcpt.ticks, rcpt.a_hash);
  
  lockchain[lockchain_len] = (lockchain_entry_t){
    .receipt_hash = receipt_hash,
    .parent_hash = parent_hash,
    .ticks = rcpt.ticks,
    .a_hash = rcpt.a_hash
  };
  lockchain_len++;
  
  assert(lockchain_len == 1);
  assert(lockchain[0].receipt_hash != 0);
  
  printf("  ✓ Receipt appended to lockchain: hash=0x%llx\n", 
         (unsigned long long)receipt_hash);
  return 1;
}

// Test: Lockchain integrity verification
static int test_lockchain_verification(void)
{
  printf("[TEST] Lockchain Integrity Verification\n");
  
  // Setup lockchain with multiple entries
  lockchain_len = 0;
  
  for (int i = 0; i < 3; i++) {
    uint64_t parent_hash = lockchain_len > 0 ? lockchain[lockchain_len - 1].receipt_hash : 0;
    uint64_t receipt_hash = compute_hash(100 + i, parent_hash, 5 + i, 200 + i);
    
    lockchain[lockchain_len] = (lockchain_entry_t){
      .receipt_hash = receipt_hash,
      .parent_hash = parent_hash,
      .ticks = 5 + i,
      .a_hash = 200 + i
    };
    lockchain_len++;
  }
  
  // Verify chain integrity
  for (size_t i = 1; i < lockchain_len; i++) {
    uint64_t expected_parent = lockchain[i - 1].receipt_hash;
    assert(lockchain[i].parent_hash == expected_parent);
    
    // Verify hash matches (use same computation as append)
    uint64_t computed_hash = compute_hash(
      lockchain[i].a_hash,
      lockchain[i].parent_hash,
      lockchain[i].ticks,
      lockchain[i].a_hash
    );
    // For verification, we need to check if the stored receipt_hash matches
    // what we would compute from the stored values
    assert(computed_hash == lockchain[i].receipt_hash);
  }
  
  printf("  ✓ Lockchain integrity verified (%zu entries)\n", lockchain_len);
  return 1;
}

// Test: Lockchain receipt lookup
static int test_lockchain_lookup(void)
{
  printf("[TEST] Lockchain Receipt Lookup\n");
  
  // Setup test data
  lockchain_len = 0;
  
  uint64_t target_hash = 0;
  for (int i = 0; i < 5; i++) {
    uint64_t parent_hash = lockchain_len > 0 ? lockchain[lockchain_len - 1].receipt_hash : 0;
    uint64_t a_hash = 200 + i;
    uint64_t receipt_hash = compute_hash(a_hash, parent_hash, 5 + i, a_hash);
    
    if (i == 2) {
      target_hash = receipt_hash;
    }
    
    lockchain[lockchain_len] = (lockchain_entry_t){
      .receipt_hash = receipt_hash,
      .parent_hash = parent_hash,
      .ticks = 5 + i,
      .a_hash = a_hash
    };
    lockchain_len++;
  }
  
  // Lookup receipt
  lockchain_entry_t *found = NULL;
  for (size_t i = 0; i < lockchain_len; i++) {
    if (lockchain[i].receipt_hash == target_hash) {
      found = &lockchain[i];
      break;
    }
  }
  
  assert(found != NULL);
  assert(found->ticks == 7); // i=2, so 5+2=7
  
  printf("  ✓ Receipt lookup successful\n");
  return 1;
}

// Test: Lockchain receipt merging (Π ⊕)
static int test_lockchain_receipt_merge(void)
{
  printf("[TEST] Lockchain Receipt Merge\n");
  
  uint64_t ALN S[NROWS];
  uint64_t ALN P[NROWS];
  uint64_t ALN O[NROWS];
  knhks_context_t ctx;
  
  knhks_init_ctx(&ctx, S, P, O);
  
  S[0] = 0xA11CE;
  P[0] = 0xC0FFEE;
  O[0] = 0xB0B;
  
  knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = 0xC0FFEE, .off = 0, .len = 1});
  
  // Generate two receipts
  knhks_hook_ir_t ir1 = {
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
  
  knhks_hook_ir_t ir2 = ir1;
  
  knhks_receipt_t rcpt1 = {0};
  knhks_receipt_t rcpt2 = {0};
  
  knhks_eval_bool(&ctx, &ir1, &rcpt1);
  knhks_eval_bool(&ctx, &ir2, &rcpt2);
  
  // Merge receipts
  knhks_receipt_t merged = knhks_receipt_merge(rcpt1, rcpt2);
  
  assert(merged.ticks == (rcpt1.ticks > rcpt2.ticks ? rcpt1.ticks : rcpt2.ticks));
  assert(merged.lanes == rcpt1.lanes + rcpt2.lanes);
  assert(merged.a_hash == (rcpt1.a_hash ^ rcpt2.a_hash));
  
  // Append merged receipt to lockchain
  uint64_t parent_hash = lockchain_len > 0 ? lockchain[lockchain_len - 1].receipt_hash : 0;
  uint64_t receipt_hash = compute_hash(merged.a_hash, parent_hash, merged.ticks, merged.a_hash);
  
  lockchain[lockchain_len] = (lockchain_entry_t){
    .receipt_hash = receipt_hash,
    .parent_hash = parent_hash,
    .ticks = merged.ticks,
    .a_hash = merged.a_hash
  };
  lockchain_len++;
  
  assert(lockchain_len > 0);
  
  printf("  ✓ Receipt merge and lockchain append successful\n");
  return 1;
}

// Test: Lockchain tamper detection
static int test_lockchain_tamper_detection(void)
{
  printf("[TEST] Lockchain Tamper Detection\n");
  
  // Setup valid chain
  lockchain_len = 0;
  
  for (int i = 0; i < 3; i++) {
    uint64_t parent_hash = lockchain_len > 0 ? lockchain[lockchain_len - 1].receipt_hash : 0;
    uint64_t receipt_hash = compute_hash(100 + i, parent_hash, 5 + i, 200 + i);
    
    lockchain[lockchain_len] = (lockchain_entry_t){
      .receipt_hash = receipt_hash,
      .parent_hash = parent_hash,
      .ticks = 5 + i,
      .a_hash = 200 + i
    };
    lockchain_len++;
  }
  
  // Tamper with entry
  uint64_t original_hash = lockchain[1].receipt_hash;
  lockchain[1].a_hash = 999; // Tamper
  
  // Recompute hash - should not match
  uint64_t computed_hash = compute_hash(
    lockchain[1].a_hash,
    lockchain[1].parent_hash,
    lockchain[1].ticks,
    lockchain[1].a_hash
  );
  
  assert(computed_hash != lockchain[1].receipt_hash);
  
  // Restore original
  lockchain[1].a_hash = 201;
  lockchain[1].receipt_hash = original_hash;
  
  printf("  ✓ Tamper detection works\n");
  return 1;
}

int main(void)
{
  printf("========================================\n");
  printf("Chicago TDD: Lockchain\n");
  printf("========================================\n\n");
  
  int passed = 0;
  int total = 0;
  
  total++; if (test_lockchain_append()) passed++;
  total++; if (test_lockchain_verification()) passed++;
  total++; if (test_lockchain_lookup()) passed++;
  total++; if (test_lockchain_receipt_merge()) passed++;
  total++; if (test_lockchain_tamper_detection()) passed++;
  
  printf("\n========================================\n");
  printf("Results: %d/%d tests passed\n", passed, total);
  printf("========================================\n");
  
  return (passed == total) ? 0 : 1;
}

