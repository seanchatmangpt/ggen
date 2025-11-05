// simd.h
// SIMD-optimized operations (internal header)
// Umbrella header - includes all SIMD operation category headers

#ifndef KNHK_SIMD_H
#define KNHK_SIMD_H

// Common infrastructure and non-inline function declarations
#include "simd/common.h"

// Include all SIMD operation category headers
#include "simd/existence.h"   // ASK operations (exists_8, exists_o_8, spo_exists_8)
#include "simd/count.h"       // COUNT operations (count_8)
#include "simd/compare.h"     // Comparison operations (compare_o_8)
#include "simd/select.h"     // SELECT operations (select_gather_8)
#include "simd/validate.h"   // Datatype validation (validate_datatype_sp_8)
#include "simd/construct.h"   // CONSTRUCT8 operations (construct8_emit_8)

// Declaration for variable-length SELECT gather (implemented in simd.c)
size_t knhk_select_gather(const uint64_t *S_base, const uint64_t *O_base,
                            uint64_t off, uint64_t len, uint64_t s_key,
                            uint64_t *out, size_t out_capacity);

#endif // KNHK_SIMD_H
