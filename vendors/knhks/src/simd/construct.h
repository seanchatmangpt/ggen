// simd/construct.h
// CONSTRUCT operations: CONSTRUCT8

#ifndef KNHKS_SIMD_CONSTRUCT_H
#define KNHKS_SIMD_CONSTRUCT_H

#include "common.h"

#if NROWS == 8
static inline size_t knhks_construct8_emit_8(const uint64_t *S_base, uint64_t off, uint64_t len,
                                               uint64_t p_const, uint64_t o_const,
                                               uint64_t *out_S, uint64_t *out_P, uint64_t *out_O,
                                               uint64_t *out_mask)
{
  const uint64_t *s_p = S_base + off;
  
#if defined(__aarch64__)
  // Load all 8 subjects
  uint64x2_t s0 = vld1q_u64(s_p + 0);
  uint64x2_t s1 = vld1q_u64(s_p + 2);
  uint64x2_t s2 = vld1q_u64(s_p + 4);
  uint64x2_t s3 = vld1q_u64(s_p + 6);
  
  // Compare with zero (non-zero mask)
  uint64x2_t zero = vdupq_n_u64(0);
  uint64x2_t m0 = vceqq_u64(s0, zero);
  uint64x2_t m1 = vceqq_u64(s1, zero);
  uint64x2_t m2 = vceqq_u64(s2, zero);
  uint64x2_t m3 = vceqq_u64(s3, zero);
  
  // Invert masks (non-zero = 1, zero = 0)
  m0 = vceqq_u64(m0, zero); // non-zero lanes become 1
  m1 = vceqq_u64(m1, zero);
  m2 = vceqq_u64(m2, zero);
  m3 = vceqq_u64(m3, zero);
  
  // Extract masks to scalars
  uint64_t mask0 = vgetq_lane_u64(m0, 0);
  uint64_t mask1 = vgetq_lane_u64(m0, 1);
  uint64_t mask2 = vgetq_lane_u64(m1, 0);
  uint64_t mask3 = vgetq_lane_u64(m1, 1);
  uint64_t mask4 = vgetq_lane_u64(m2, 0);
  uint64_t mask5 = vgetq_lane_u64(m2, 1);
  uint64_t mask6 = vgetq_lane_u64(m3, 0);
  uint64_t mask7 = (len > 7) ? vgetq_lane_u64(m3, 1) : 0;
  
  // Extract subjects
  uint64_t v0 = vgetq_lane_u64(s0, 0);
  uint64_t v1 = vgetq_lane_u64(s0, 1);
  uint64_t v2 = vgetq_lane_u64(s1, 0);
  uint64_t v3 = vgetq_lane_u64(s1, 1);
  uint64_t v4 = vgetq_lane_u64(s2, 0);
  uint64_t v5 = vgetq_lane_u64(s2, 1);
  uint64_t v6 = vgetq_lane_u64(s3, 0);
  uint64_t v7 = vgetq_lane_u64(s3, 1);
  
  // Branchless write: pack non-zero values sequentially
  size_t idx = 0;
  
  // Write up to 8 results (branchless conditional writes)
  out_S[idx] = (mask0) ? v0 : out_S[idx];
  out_P[idx] = (mask0) ? p_const : out_P[idx];
  out_O[idx] = (mask0) ? o_const : out_O[idx];
  idx += (mask0 != 0);
  
  out_S[idx] = (mask1) ? v1 : out_S[idx];
  out_P[idx] = (mask1) ? p_const : out_P[idx];
  out_O[idx] = (mask1) ? o_const : out_O[idx];
  idx += (mask1 != 0);
  
  out_S[idx] = (mask2) ? v2 : out_S[idx];
  out_P[idx] = (mask2) ? p_const : out_P[idx];
  out_O[idx] = (mask2) ? o_const : out_O[idx];
  idx += (mask2 != 0);
  
  out_S[idx] = (mask3) ? v3 : out_S[idx];
  out_P[idx] = (mask3) ? p_const : out_P[idx];
  out_O[idx] = (mask3) ? o_const : out_O[idx];
  idx += (mask3 != 0);
  
  out_S[idx] = (mask4) ? v4 : out_S[idx];
  out_P[idx] = (mask4) ? p_const : out_P[idx];
  out_O[idx] = (mask4) ? o_const : out_O[idx];
  idx += (mask4 != 0);
  
  out_S[idx] = (mask5) ? v5 : out_S[idx];
  out_P[idx] = (mask5) ? p_const : out_P[idx];
  out_O[idx] = (mask5) ? o_const : out_O[idx];
  idx += (mask5 != 0);
  
  out_S[idx] = (mask6) ? v6 : out_S[idx];
  out_P[idx] = (mask6) ? p_const : out_P[idx];
  out_O[idx] = (mask6) ? o_const : out_O[idx];
  idx += (mask6 != 0);
  
  out_S[idx] = (mask7) ? v7 : out_S[idx];
  out_P[idx] = (mask7) ? p_const : out_P[idx];
  out_O[idx] = (mask7) ? o_const : out_O[idx];
  idx += (mask7 != 0);
  
  *out_mask = (mask0 ? 1ULL : 0) | (mask1 ? 2ULL : 0) | (mask2 ? 4ULL : 0) | (mask3 ? 8ULL : 0) |
              (mask4 ? 16ULL : 0) | (mask5 ? 32ULL : 0) | (mask6 ? 64ULL : 0) | (mask7 ? 128ULL : 0);
  
  return idx;
#elif defined(__x86_64__)
  // Load all 8 subjects
  __m256i s0 = _mm256_loadu_si256((const __m256i *)(s_p + 0));
  __m256i s1 = _mm256_loadu_si256((const __m256i *)(s_p + 4));
  
  // Compare with zero
  __m256i zero = _mm256_setzero_si256();
  __m256i m0 = _mm256_cmpeq_epi64(s0, zero);
  __m256i m1 = _mm256_cmpeq_epi64(s1, zero);
  
  // Invert masks (non-zero = 1)
  m0 = _mm256_cmpeq_epi64(m0, zero);
  m1 = _mm256_cmpeq_epi64(m1, zero);
  
  // Extract masks and subjects
  uint64_t mask0 = _mm256_extract_epi64(m0, 0) ? 1 : 0;
  uint64_t mask1 = _mm256_extract_epi64(m0, 1) ? 1 : 0;
  uint64_t mask2 = _mm256_extract_epi64(m0, 2) ? 1 : 0;
  uint64_t mask3 = _mm256_extract_epi64(m0, 3) ? 1 : 0;
  uint64_t mask4 = _mm256_extract_epi64(m1, 0) ? 1 : 0;
  uint64_t mask5 = _mm256_extract_epi64(m1, 1) ? 1 : 0;
  uint64_t mask6 = _mm256_extract_epi64(m1, 2) ? 1 : 0;
  uint64_t mask7 = (len > 7) ? (_mm256_extract_epi64(m1, 3) ? 1 : 0) : 0;
  
  uint64_t v0 = _mm256_extract_epi64(s0, 0);
  uint64_t v1 = _mm256_extract_epi64(s0, 1);
  uint64_t v2 = _mm256_extract_epi64(s0, 2);
  uint64_t v3 = _mm256_extract_epi64(s0, 3);
  uint64_t v4 = _mm256_extract_epi64(s1, 0);
  uint64_t v5 = _mm256_extract_epi64(s1, 1);
  uint64_t v6 = _mm256_extract_epi64(s1, 2);
  uint64_t v7 = _mm256_extract_epi64(s1, 3);
  
  // Branchless write
  size_t idx = 0;
  
  out_S[idx] = (mask0) ? v0 : out_S[idx];
  out_P[idx] = (mask0) ? p_const : out_P[idx];
  out_O[idx] = (mask0) ? o_const : out_O[idx];
  idx += mask0;
  
  out_S[idx] = (mask1) ? v1 : out_S[idx];
  out_P[idx] = (mask1) ? p_const : out_P[idx];
  out_O[idx] = (mask1) ? o_const : out_O[idx];
  idx += mask1;
  
  out_S[idx] = (mask2) ? v2 : out_S[idx];
  out_P[idx] = (mask2) ? p_const : out_P[idx];
  out_O[idx] = (mask2) ? o_const : out_O[idx];
  idx += mask2;
  
  out_S[idx] = (mask3) ? v3 : out_S[idx];
  out_P[idx] = (mask3) ? p_const : out_P[idx];
  out_O[idx] = (mask3) ? o_const : out_O[idx];
  idx += mask3;
  
  out_S[idx] = (mask4) ? v4 : out_S[idx];
  out_P[idx] = (mask4) ? p_const : out_P[idx];
  out_O[idx] = (mask4) ? o_const : out_O[idx];
  idx += mask4;
  
  out_S[idx] = (mask5) ? v5 : out_S[idx];
  out_P[idx] = (mask5) ? p_const : out_P[idx];
  out_O[idx] = (mask5) ? o_const : out_O[idx];
  idx += mask5;
  
  out_S[idx] = (mask6) ? v6 : out_S[idx];
  out_P[idx] = (mask6) ? p_const : out_P[idx];
  out_O[idx] = (mask6) ? o_const : out_O[idx];
  idx += mask6;
  
  out_S[idx] = (mask7) ? v7 : out_S[idx];
  out_P[idx] = (mask7) ? p_const : out_P[idx];
  out_O[idx] = (mask7) ? o_const : out_O[idx];
  idx += mask7;
  
  *out_mask = (mask0 ? 1ULL : 0) | (mask1 ? 2ULL : 0) | (mask2 ? 4ULL : 0) | (mask3 ? 8ULL : 0) |
              (mask4 ? 16ULL : 0) | (mask5 ? 32ULL : 0) | (mask6 ? 64ULL : 0) | (mask7 ? 128ULL : 0);
  
  return idx;
#else
  // Scalar fallback
  size_t idx = 0;
  uint64_t mask = 0;
  for (size_t i = 0; i < len && i < 8; i++) {
    if (s_p[i] != 0) {
      out_S[idx] = s_p[i];
      out_P[idx] = p_const;
      out_O[idx] = o_const;
      mask |= (1ULL << i);
      idx++;
    }
  }
  *out_mask = mask;
  return idx;
#endif
}
#endif // NROWS == 8

#endif // KNHKS_SIMD_CONSTRUCT_H
