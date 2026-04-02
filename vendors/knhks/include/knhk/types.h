// knhk/types.h
// Type definitions for KNKHS: constants, enums, structs

#ifndef KNHK_TYPES_H
#define KNHK_TYPES_H

#include <stdint.h>
#include <stddef.h>

// v1.0 Constants
#define KNHK_TIME_BUDGET_NS 2.0  // 2 nanoseconds budget (Chatman Constant)
#define KNHK_NROWS       8u        // compile-time fixed
#define KNHK_ALIGN       64u       // bytes

// Maximum rows that fit in 2ns window (must be 8 for v1.0)
#ifndef NROWS
#define NROWS KNHK_NROWS
#endif

#if NROWS != 8
#error "KNHK_NROWS must be 8 for hot path v1."
#endif

// Operation types
typedef enum {
  KNHK_OP_ASK_SP = 1,
  KNHK_OP_COUNT_SP_GE = 2,
  KNHK_OP_ASK_SPO = 3,
  KNHK_OP_SELECT_SP = 4,
  KNHK_OP_COUNT_SP_LE = 5,  // COUNT <= k
  KNHK_OP_COUNT_SP_EQ = 6,  // COUNT == k
  KNHK_OP_ASK_OP = 7,        // ASK(O,P) - reverse lookup
  KNHK_OP_UNIQUE_SP = 8,    // UNIQUE(S,P) - exactly one value
  KNHK_OP_COUNT_OP = 9,      // COUNT(O,P) >= k - count object occurrences
  KNHK_OP_COUNT_OP_LE = 10,  // COUNT(O,P) <= k
  KNHK_OP_COUNT_OP_EQ = 11,  // COUNT(O,P) == k
  KNHK_OP_COMPARE_O_EQ = 12, // O == value (exact match)
  KNHK_OP_COMPARE_O_GT = 13, // O > value (greater than)
  KNHK_OP_COMPARE_O_LT = 14, // O < value (less than)
  KNHK_OP_COMPARE_O_GE = 15, // O >= value (greater or equal)
  KNHK_OP_COMPARE_O_LE = 16, // O <= value (less or equal)
  KNHK_OP_VALIDATE_DATATYPE_SP = 17, // Validate datatype: Check if (s, p) has object matching datatype hash
  KNHK_OP_VALIDATE_DATATYPE_SPO = 18, // Validate datatype: Check if (s, p, o) exists and o matches datatype hash
  
  // v1.0 addition: hot emit (fixed-template, len ≤ 8)
  KNHK_OP_CONSTRUCT8 = 32   // CONSTRUCT8 - fixed-template emit
} knhk_op_t;

// Predicate run metadata (len ≤ 8 for hot path)
typedef struct {
  uint64_t pred;  // P id
  uint64_t off;   // SoA offset
  uint64_t len;   // must be ≤ 8 (guarded by H)
} knhk_pred_run_t;

// Provenance receipt (hot path)
typedef struct {
  uint32_t lanes;    // SIMD width used
  uint64_t span_id;  // OTEL-compatible id
  uint64_t a_hash;   // hash(A) = hash(μ(O)) fragment
} knhk_receipt_t;

// Hook IR (query representation) - branchless, constant-time
typedef struct {
  knhk_op_t op;
  uint64_t s, p, o, k;  // ids / thresholds
  
  // For CONSTRUCT8 only: preallocated output spans (8 rows max)
  uint64_t *out_S;      // may be NULL for non-CONSTRUCT8
  uint64_t *out_P;
  uint64_t *out_O;
  uint64_t out_mask;    // per-lane bitmask result (returned by μ)
  
  // Legacy SELECT support (cold path only, not in hot v1.0)
  uint64_t *select_out; // Output buffer for SELECT
  size_t select_capacity;
} knhk_hook_ir_t;

// Context for data arrays (SoA layout, 64-byte aligned)
typedef struct {
  const uint64_t *S;  // Subject array (KNHK_ALIGN aligned, KNHK_NROWS sized)
  const uint64_t *P;  // Predicate array
  const uint64_t *O;  // Object array
  size_t triple_count;
  knhk_pred_run_t run;
} knhk_context_t;

#endif // KNHK_TYPES_H

