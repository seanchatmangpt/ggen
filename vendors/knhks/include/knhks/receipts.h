// knhks/receipts.h
// Receipt operations: merging and provenance tracking

#ifndef KNHKS_RECEIPTS_H
#define KNHKS_RECEIPTS_H

#include "types.h"

// Combine receipts via ⊕ (associative, branchless)
static inline knhks_receipt_t knhks_receipt_merge(knhks_receipt_t a, knhks_receipt_t b)
{
  knhks_receipt_t merged;
  merged.ticks = (a.ticks > b.ticks) ? a.ticks : b.ticks; // max ticks
  merged.lanes = a.lanes + b.lanes; // sum lanes
  merged.span_id = a.span_id ^ b.span_id; // XOR merge
  merged.a_hash = a.a_hash ^ b.a_hash; // ⊕ merge (XOR)
  return merged;
}

#endif // KNHKS_RECEIPTS_H

