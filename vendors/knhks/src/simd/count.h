// simd/count.h
// Count operations: COUNT_SP

#ifndef KNHK_SIMD_COUNT_H
#define KNHK_SIMD_COUNT_H

#include "common.h"

#if NROWS == 8
static inline uint64_t knhk_eq64_count_8(const uint64_t *base, uint64_t off, uint64_t key)
{
#if defined(__aarch64__)
  const uint64_t *p = base + off;
  uint64x2_t K = vdupq_n_u64(key);
  const uint64x2_t ONE = vdupq_n_u64(1);
  uint64x2_t acc = vdupq_n_u64(0);
  // Process first 4 elements
  uint64x2_t a0 = vld1q_u64(p + 0);
  uint64x2_t a1 = vld1q_u64(p + 2);
  uint64x2_t m0 = vceqq_u64(a0, K);
  uint64x2_t m1 = vceqq_u64(a1, K);
  uint64x2_t c0 = vandq_u64(m0, ONE);
  uint64x2_t c1 = vandq_u64(m1, ONE);
  acc = vaddq_u64(acc, vaddq_u64(c0, c1));
  // Process remaining 4 elements
  uint64x2_t a2 = vld1q_u64(p + 4);
  uint64x2_t a3 = vld1q_u64(p + 6);
  uint64x2_t m2 = vceqq_u64(a2, K);
  uint64x2_t m3 = vceqq_u64(a3, K);
  uint64x2_t c2 = vandq_u64(m2, ONE);
  uint64x2_t c3 = vandq_u64(m3, ONE);
  acc = vaddq_u64(acc, vaddq_u64(c2, c3));
  uint64_t t[2];
  vst1q_u64(t, acc);
  return t[0] + t[1];
#elif defined(__x86_64__)
  const uint64_t *p = base + off;
  __m256i K = _mm256_set1_epi64x((long long)key);
  __m256i acc = _mm256_setzero_si256();
  const __m256i ONE = _mm256_set1_epi64x(1);
  // Process first 4 elements
  __m256i a0 = _mm256_loadu_si256((const __m256i *)(p + 0));
  __m256i m0 = _mm256_cmpeq_epi64(a0, K);
  __m256i c0 = _mm256_and_si256(m0, ONE);
  acc = _mm256_add_epi64(acc, c0);
  // Process remaining 4 elements
  __m256i a1 = _mm256_loadu_si256((const __m256i *)(p + 4));
  __m256i m1 = _mm256_cmpeq_epi64(a1, K);
  __m256i c1 = _mm256_and_si256(m1, ONE);
  acc = _mm256_add_epi64(acc, c1);
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, acc);
  return t[0] + t[1] + t[2] + t[3];
#else
  const uint64_t *p = base + off;
  uint64_t cnt = 0;
  cnt += (p[0] == key);
  cnt += (p[1] == key);
  cnt += (p[2] == key);
  cnt += (p[3] == key);
  cnt += (p[4] == key);
  cnt += (p[5] == key);
  cnt += (p[6] == key);
  cnt += (p[7] == key);
  return cnt;
#endif
}

#endif // NROWS == 8

#endif // KNHK_SIMD_COUNT_H
