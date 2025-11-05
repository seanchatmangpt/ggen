// simd/common.h
// Common SIMD infrastructure and includes

#ifndef KNHK_SIMD_COMMON_H
#define KNHK_SIMD_COMMON_H

#include <stdint.h>
#include <stddef.h>
#include <limits.h>

// Ensure NROWS is defined
#ifndef NROWS
#define NROWS 8u
#endif

#if defined(__aarch64__)
#include <arm_neon.h>
#elif defined(__x86_64__)
#include <immintrin.h>
#endif

// Non-inline function declarations (implemented in simd.c)
int knhk_eq64_exists_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key);
uint64_t knhk_eq64_count_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key);
int knhk_eq64_spo_exists_run(const uint64_t *S_base, const uint64_t *O_base,
                               uint64_t off, uint64_t len, uint64_t s_key, uint64_t o_key);

#endif // KNHK_SIMD_COMMON_H

