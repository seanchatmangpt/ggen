// simd/compare.h
// Comparison operations: COMPARE_O_EQ/GT/LT/GE/LE

#ifndef KNHK_SIMD_COMPARE_H
#define KNHK_SIMD_COMPARE_H

#include "common.h"

#if NROWS == 8
static inline int knhk_compare_o_8(const uint64_t *O_base, uint64_t off, uint64_t threshold, int op_type)
{
  const uint64_t *o_p = O_base + off;
  uint64_t result = 0;
  
#if defined(__aarch64__)
  uint64x2_t K = vdupq_n_u64(threshold);
  uint64x2_t o0 = vld1q_u64(o_p + 0);
  uint64x2_t o1 = vld1q_u64(o_p + 2);
  uint64x2_t o2 = vld1q_u64(o_p + 4);
  uint64x2_t o3 = vld1q_u64(o_p + 6);
  
  uint64x2_t m0, m1, m2, m3;
  switch (op_type) {
    case 0: // EQ
      m0 = vceqq_u64(o0, K);
      m1 = vceqq_u64(o1, K);
      m2 = vceqq_u64(o2, K);
      m3 = vceqq_u64(o3, K);
      break;
    case 1: // GT (unsigned comparison)
      m0 = vcgtq_u64(o0, K);
      m1 = vcgtq_u64(o1, K);
      m2 = vcgtq_u64(o2, K);
      m3 = vcgtq_u64(o3, K);
      break;
    case 2: // LT
      m0 = vcltq_u64(o0, K);
      m1 = vcltq_u64(o1, K);
      m2 = vcltq_u64(o2, K);
      m3 = vcltq_u64(o3, K);
      break;
    case 3: // GE
      m0 = vcgeq_u64(o0, K);
      m1 = vcgeq_u64(o1, K);
      m2 = vcgeq_u64(o2, K);
      m3 = vcgeq_u64(o3, K);
      break;
    case 4: // LE
      m0 = vcleq_u64(o0, K);
      m1 = vcleq_u64(o1, K);
      m2 = vcleq_u64(o2, K);
      m3 = vcleq_u64(o3, K);
      break;
    default:
      return 0;
  }
  
  uint64_t t[2];
  vst1q_u64(t, m0);
  result |= (t[0] | t[1]);
  vst1q_u64(t, m1);
  result |= (t[0] | t[1]);
  vst1q_u64(t, m2);
  result |= (t[0] | t[1]);
  vst1q_u64(t, m3);
  result |= (t[0] | t[1]);
#elif defined(__x86_64__)
  __m256i K = _mm256_set1_epi64x((long long)threshold);
  __m256i o0 = _mm256_loadu_si256((const __m256i *)(o_p + 0));
  __m256i o1 = _mm256_loadu_si256((const __m256i *)(o_p + 4));
  
  __m256i m0, m1;
  switch (op_type) {
    case 0: // EQ
      m0 = _mm256_cmpeq_epi64(o0, K);
      m1 = _mm256_cmpeq_epi64(o1, K);
      break;
    case 1: // GT (unsigned comparison)
      m0 = _mm256_cmpgt_epi64(o0, _mm256_set1_epi64x((long long)threshold));
      m1 = _mm256_cmpgt_epi64(o1, _mm256_set1_epi64x((long long)threshold));
      break;
    case 2: // LT
      m0 = _mm256_cmpgt_epi64(_mm256_set1_epi64x((long long)threshold), o0);
      m1 = _mm256_cmpgt_epi64(_mm256_set1_epi64x((long long)threshold), o1);
      break;
    case 3: // GE
      m0 = _mm256_or_si256(_mm256_cmpeq_epi64(o0, K), _mm256_cmpgt_epi64(o0, _mm256_set1_epi64x((long long)threshold)));
      m1 = _mm256_or_si256(_mm256_cmpeq_epi64(o1, K), _mm256_cmpgt_epi64(o1, _mm256_set1_epi64x((long long)threshold)));
      break;
    case 4: // LE
      m0 = _mm256_or_si256(_mm256_cmpeq_epi64(o0, K), _mm256_cmpgt_epi64(_mm256_set1_epi64x((long long)threshold), o0));
      m1 = _mm256_or_si256(_mm256_cmpeq_epi64(o1, K), _mm256_cmpgt_epi64(_mm256_set1_epi64x((long long)threshold), o1));
      break;
    default:
      return 0;
  }
  
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, m0);
  result |= (t[0] | t[1] | t[2] | t[3]);
  _mm256_storeu_si256((__m256i *)t, m1);
  result |= (t[0] | t[1] | t[2] | t[3]);
#else
  // Fallback: scalar comparison
  switch (op_type) {
    case 0: // EQ
      result = (o_p[0] == threshold) | (o_p[1] == threshold) | (o_p[2] == threshold) | (o_p[3] == threshold) |
               (o_p[4] == threshold) | (o_p[5] == threshold) | (o_p[6] == threshold) | (o_p[7] == threshold);
      break;
    case 1: // GT
      result = (o_p[0] > threshold) | (o_p[1] > threshold) | (o_p[2] > threshold) | (o_p[3] > threshold) |
               (o_p[4] > threshold) | (o_p[5] > threshold) | (o_p[6] > threshold) | (o_p[7] > threshold);
      break;
    case 2: // LT
      result = (o_p[0] < threshold) | (o_p[1] < threshold) | (o_p[2] < threshold) | (o_p[3] < threshold) |
               (o_p[4] < threshold) | (o_p[5] < threshold) | (o_p[6] < threshold) | (o_p[7] < threshold);
      break;
    case 3: // GE
      result = (o_p[0] >= threshold) | (o_p[1] >= threshold) | (o_p[2] >= threshold) | (o_p[3] >= threshold) |
               (o_p[4] >= threshold) | (o_p[5] >= threshold) | (o_p[6] >= threshold) | (o_p[7] >= threshold);
      break;
    case 4: // LE
      result = (o_p[0] <= threshold) | (o_p[1] <= threshold) | (o_p[2] <= threshold) | (o_p[3] <= threshold) |
               (o_p[4] <= threshold) | (o_p[5] <= threshold) | (o_p[6] <= threshold) | (o_p[7] <= threshold);
      break;
  }
#endif
  
  return result != 0;
}

#endif // NROWS == 8

#endif // KNHK_SIMD_COMPARE_H
