// simd/validate.h
// Validation operations: VALIDATE_DATATYPE_SP

#ifndef KNHK_SIMD_VALIDATE_H
#define KNHK_SIMD_VALIDATE_H

#include "common.h"

#if NROWS == 8
static inline int knhk_validate_datatype_sp_8(const uint64_t *S_base, const uint64_t *O_base,
                                                uint64_t off, uint64_t s_key, uint64_t datatype_hash)
{
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  
#if defined(__aarch64__)
  uint64x2_t Ks = vdupq_n_u64(s_key);
  uint64x2_t Kdt = vdupq_n_u64(datatype_hash);
  
  // Check first 4 elements: S matches AND O matches datatype
  uint64x2_t s0 = vld1q_u64(s_p + 0);
  uint64x2_t o0 = vld1q_u64(o_p + 0);
  uint64x2_t ms0 = vceqq_u64(s0, Ks);
  uint64x2_t mo0 = vceqq_u64(o0, Kdt);
  uint64x2_t combined0 = vandq_u64(ms0, mo0);
  
  // Check remaining 4 elements
  uint64x2_t s1 = vld1q_u64(s_p + 2);
  uint64x2_t o1 = vld1q_u64(o_p + 2);
  uint64x2_t ms1 = vceqq_u64(s1, Ks);
  uint64x2_t mo1 = vceqq_u64(o1, Kdt);
  uint64x2_t combined1 = vandq_u64(ms1, mo1);
  
  uint64x2_t s2 = vld1q_u64(s_p + 4);
  uint64x2_t o2 = vld1q_u64(o_p + 4);
  uint64x2_t ms2 = vceqq_u64(s2, Ks);
  uint64x2_t mo2 = vceqq_u64(o2, Kdt);
  uint64x2_t combined2 = vandq_u64(ms2, mo2);
  
  uint64x2_t s3 = vld1q_u64(s_p + 6);
  uint64x2_t o3 = vld1q_u64(o_p + 6);
  uint64x2_t ms3 = vceqq_u64(s3, Ks);
  uint64x2_t mo3 = vceqq_u64(o3, Kdt);
  uint64x2_t combined3 = vandq_u64(ms3, mo3);
  
  uint64_t t[2];
  uint64_t result = 0;
  vst1q_u64(t, combined0);
  result |= (t[0] | t[1]);
  vst1q_u64(t, combined1);
  result |= (t[0] | t[1]);
  vst1q_u64(t, combined2);
  result |= (t[0] | t[1]);
  vst1q_u64(t, combined3);
  result |= (t[0] | t[1]);
  
  return result != 0;
#elif defined(__x86_64__)
  __m256i Ks = _mm256_set1_epi64x((long long)s_key);
  __m256i Kdt = _mm256_set1_epi64x((long long)datatype_hash);
  
  // Check first 4 elements
  __m256i s0 = _mm256_loadu_si256((const __m256i *)(s_p + 0));
  __m256i o0 = _mm256_loadu_si256((const __m256i *)(o_p + 0));
  __m256i ms0 = _mm256_cmpeq_epi64(s0, Ks);
  __m256i mo0 = _mm256_cmpeq_epi64(o0, Kdt);
  __m256i combined0 = _mm256_and_si256(ms0, mo0);
  
  // Check remaining 4 elements
  __m256i s1 = _mm256_loadu_si256((const __m256i *)(s_p + 4));
  __m256i o1 = _mm256_loadu_si256((const __m256i *)(o_p + 4));
  __m256i ms1 = _mm256_cmpeq_epi64(s1, Ks);
  __m256i mo1 = _mm256_cmpeq_epi64(o1, Kdt);
  __m256i combined1 = _mm256_and_si256(ms1, mo1);
  
  uint64_t t[4];
  uint64_t result = 0;
  _mm256_storeu_si256((__m256i *)t, combined0);
  result |= (t[0] | t[1] | t[2] | t[3]);
  _mm256_storeu_si256((__m256i *)t, combined1);
  result |= (t[0] | t[1] | t[2] | t[3]);
  
  return result != 0;
#else
  // Fallback: scalar check
  uint64_t result = 0;
  for (int i = 0; i < 8; i++) {
    if (s_p[i] == s_key && o_p[i] == datatype_hash) {
      result = 1;
      break;
    }
  }
  return result != 0;
#endif
}

#endif // NROWS == 8

#endif // KNHK_SIMD_VALIDATE_H
