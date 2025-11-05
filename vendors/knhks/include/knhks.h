// knhks.h
// Public API for KNKHS 8-tick knowledge graph query system (v1.0)
// Branchless SIMD operations for sub-2 nanosecond query execution
// KGC: A = μ(O), μ ⊂ τ, τ ≤ 8 ticks
// Umbrella header - includes all API components

#ifndef KNHKS_H
#define KNHKS_H

// Include all API components
#include "knhks/types.h"      // Constants, enums, structs
#include "knhks/utils.h"      // Context initialization, RDF loading, clock utilities
#include "knhks/receipts.h"   // Receipt operations
#include "knhks/eval.h"       // Query evaluation functions

#include "aot/aot_guard.h"

#endif // KNHKS_H
