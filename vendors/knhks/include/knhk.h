// knhk.h
// Public API for KNHK 2ns knowledge graph query system (v1.0)
// Branchless SIMD operations for sub-2 nanosecond query execution
// KGC: A = μ(O), μ ⊂ τ, τ ≤ 2ns
// Umbrella header - includes all API components

#ifndef KNHK_H
#define KNHK_H

// Include all API components
#include "knhk/types.h"      // Constants, enums, structs
#include "knhk/utils.h"      // Context initialization, RDF loading, clock utilities
#include "knhk/receipts.h"   // Receipt operations
#include "knhk/eval.h"       // Query evaluation functions

#include "aot/aot_guard.h"

#endif // KNHK_H
