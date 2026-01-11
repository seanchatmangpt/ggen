// simd/existence.h
// Existence checking operations: ASK_SP, ASK_OP, ASK_SPO

#ifndef KNHK_SIMD_EXISTENCE_H
#define KNHK_SIMD_EXISTENCE_H

#include "common.h"

#if NROWS == 8
static inline int knhk_eq64_exists_8(const uint64_t *base, uint64_t off, uint64_t key)
{
#if defined(__aarch64__)
  const uint64_t *p = base + off;
  uint64x2_t K = vdupq_n_u64(key);
  // Load first 4 elements
  uint64x2_t a0 = vld1q_u64(p + 0);
  uint64x2_t a1 = vld1q_u64(p + 2);
  uint64x2_t m0 = vceqq_u64(a0, K);
  uint64x2_t m1 = vceqq_u64(a1, K);
  uint64_t t[2];
  vst1q_u64(t, m0);
  uint64_t has_match = t[0] | t[1];
  vst1q_u64(t, m1);
  has_match |= (t[0] | t[1]);
  // Load remaining 4 elements
  uint64x2_t a2 = vld1q_u64(p + 4);
  uint64x2_t a3 = vld1q_u64(p + 6);
  uint64x2_t m2 = vceqq_u64(a2, K);
  uint64x2_t m3 = vceqq_u64(a3, K);
  vst1q_u64(t, m2);
  has_match |= (t[0] | t[1]);
  vst1q_u64(t, m3);
  has_match |= (t[0] | t[1]);
  return has_match != 0;
#elif defined(__x86_64__)
  const uint64_t *p = base + off;
  __m256i K = _mm256_set1_epi64x((long long)key);
  // Load first 4 elements
  __m256i a0 = _mm256_loadu_si256((const __m256i *)(p + 0));
  __m256i m0 = _mm256_cmpeq_epi64(a0, K);
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, m0);
  uint64_t has_match = t[0] | t[1] | t[2] | t[3];
  // Load remaining 4 elements
  __m256i a1 = _mm256_loadu_si256((const __m256i *)(p + 4));
  __m256i m1 = _mm256_cmpeq_epi64(a1, K);
  _mm256_storeu_si256((__m256i *)t, m1);
  has_match |= (t[0] | t[1] | t[2] | t[3]);
  return has_match != 0;
#else
  const uint64_t *p = base + off;
  uint64_t has_match = 0;
  has_match |= (p[0] == key ? UINT64_MAX : 0);
  has_match |= (p[1] == key ? UINT64_MAX : 0);
  has_match |= (p[2] == key ? UINT64_MAX : 0);
  has_match |= (p[3] == key ? UINT64_MAX : 0);
  has_match |= (p[4] == key ? UINT64_MAX : 0);
  has_match |= (p[5] == key ? UINT64_MAX : 0);
  has_match |= (p[6] == key ? UINT64_MAX : 0);
  has_match |= (p[7] == key ? UINT64_MAX : 0);
  return has_match != 0;
#endif
}

// Ultra-fast ASK(O,P) for exactly 8 elements - fully unrolled (reverse lookup)
static inline int knhk_eq64_exists_o_8(const uint64_t *base, uint64_t off, uint64_t key)
{
  // Same as knhk_eq64_exists_8 but semantically for O array
  return knhk_eq64_exists_8(base, off, key);
}

// Ultra-fast ASK(S,P,O) for exactly 8 elements - fully unrolled
static inline int knhk_eq64_spo_exists_8(const uint64_t *S_base, const uint64_t *O_base,
                             uint64_t off, uint64_t s_key, uint64_t o_key)
{
#if defined(__aarch64__)
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  uint64x2_t Ks = vdupq_n_u64(s_key);
  uint64x2_t Ko = vdupq_n_u64(o_key);
  uint64_t has_match = 0;
  // Process first 4 elements
  uint64x2_t s0 = vld1q_u64(s_p + 0);
  uint64x2_t o0 = vld1q_u64(o_p + 0);
  uint64x2_t ms0 = vceqq_u64(s0, Ks);
  uint64x2_t mo0 = vceqq_u64(o0, Ko);
  uint64x2_t combined0 = vandq_u64(ms0, mo0);
  uint64_t t[2];
  vst1q_u64(t, combined0);
  has_match |= (t[0] | t[1]);
  // Process remaining 4 elements
  uint64x2_t s1 = vld1q_u64(s_p + 2);
  uint64x2_t o1 = vld1q_u64(o_p + 2);
  uint64x2_t ms1 = vceqq_u64(s1, Ks);
  uint64x2_t mo1 = vceqq_u64(o1, Ko);
  uint64x2_t combined1 = vandq_u64(ms1, mo1);
  vst1q_u64(t, combined1);
  has_match |= (t[0] | t[1]);
  uint64x2_t s2 = vld1q_u64(s_p + 4);
  uint64x2_t o2 = vld1q_u64(o_p + 4);
  uint64x2_t ms2 = vceqq_u64(s2, Ks);
  uint64x2_t mo2 = vceqq_u64(o2, Ko);
  uint64x2_t combined2 = vandq_u64(ms2, mo2);
  vst1q_u64(t, combined2);
  has_match |= (t[0] | t[1]);
  uint64x2_t s3 = vld1q_u64(s_p + 6);
  uint64x2_t o3 = vld1q_u64(o_p + 6);
  uint64x2_t ms3 = vceqq_u64(s3, Ks);
  uint64x2_t mo3 = vceqq_u64(o3, Ko);
  uint64x2_t combined3 = vandq_u64(ms3, mo3);
  vst1q_u64(t, combined3);
  has_match |= (t[0] | t[1]);
  return has_match != 0;
#elif defined(__x86_64__)
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  __m256i Ks = _mm256_set1_epi64x((long long)s_key);
  __m256i Ko = _mm256_set1_epi64x((long long)o_key);
  uint64_t has_match = 0;
  // Process first 4 elements
  __m256i s0 = _mm256_loadu_si256((const __m256i *)(s_p + 0));
  __m256i o0 = _mm256_loadu_si256((const __m256i *)(o_p + 0));
  __m256i ms0 = _mm256_cmpeq_epi64(s0, Ks);
  __m256i mo0 = _mm256_cmpeq_epi64(o0, Ko);
  __m256i combined0 = _mm256_and_si256(ms0, mo0);
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, combined0);
  has_match |= (t[0] | t[1] | t[2] | t[3]);
  // Process remaining 4 elements
  __m256i s1 = _mm256_loadu_si256((const __m256i *)(s_p + 4));
  __m256i o1 = _mm256_loadu_si256((const __m256i *)(o_p + 4));
  __m256i ms1 = _mm256_cmpeq_epi64(s1, Ks);
  __m256i mo1 = _mm256_cmpeq_epi64(o1, Ko);
  __m256i combined1 = _mm256_and_si256(ms1, mo1);
  _mm256_storeu_si256((__m256i *)t, combined1);
  has_match |= (t[0] | t[1] | t[2] | t[3]);
  return has_match != 0;
#else
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  uint64_t has_match = 0;
  has_match |= ((s_p[0] == s_key) && (o_p[0] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[1] == s_key) && (o_p[1] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[2] == s_key) && (o_p[2] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[3] == s_key) && (o_p[3] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[4] == s_key) && (o_p[4] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[5] == s_key) && (o_p[5] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[6] == s_key) && (o_p[6] == o_key) ? UINT64_MAX : 0);
  has_match |= ((s_p[7] == s_key) && (o_p[7] == o_key) ? UINT64_MAX : 0);
  return has_match != 0;
#endif
}

#endif // NROWS == 8

#endif // KNHK_SIMD_EXISTENCE_H
