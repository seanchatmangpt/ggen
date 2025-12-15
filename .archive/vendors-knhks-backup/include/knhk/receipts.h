// knhk/receipts.h
// Receipt operations: merging and provenance tracking

#ifndef KNHK_RECEIPTS_H
#define KNHK_RECEIPTS_H

#include "types.h"

// Combine receipts via ⊕ (associative, branchless)
static inline knhk_receipt_t knhk_receipt_merge(knhk_receipt_t a, knhk_receipt_t b)
{
  knhk_receipt_t merged;
  merged.lanes = a.lanes + b.lanes; // sum lanes
  merged.span_id = a.span_id ^ b.span_id; // XOR merge
  merged.a_hash = a.a_hash ^ b.a_hash; // ⊕ merge (XOR)
  return merged;
}

#endif // KNHK_RECEIPTS_H

