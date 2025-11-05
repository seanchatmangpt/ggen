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
  // Load all 8 subjects (SIMD load)
  uint64x2_t s0 = vld1q_u64(s_p + 0);
  uint64x2_t s1 = vld1q_u64(s_p + 2);
  uint64x2_t s2 = vld1q_u64(s_p + 4);
  uint64x2_t s3 = vld1q_u64(s_p + 6);
  
  // Generate masks: non-zero subjects become UINT64_MAX, zero becomes 0
  uint64x2_t zero = vdupq_n_u64(0);
  uint64x2_t m0 = vceqq_u64(s0, zero);  // 0 if equal, UINT64_MAX if not
  uint64x2_t m1 = vceqq_u64(s1, zero);
  uint64x2_t m2 = vceqq_u64(s2, zero);
  uint64x2_t m3 = vceqq_u64(s3, zero);
  
  // Invert masks: non-zero subjects should be UINT64_MAX
  m0 = vceqq_u64(m0, zero);  // non-zero lanes become UINT64_MAX
  m1 = vceqq_u64(m1, zero);
  m2 = vceqq_u64(m2, zero);
  m3 = vceqq_u64(m3, zero);
  
  // Apply length mask: zero out positions beyond len (simple branchless mask)
  // This is done after stores, so we just need to mask the bitmap
  // No need for SIMD comparison - simple bitwise mask is faster
  
  // Blend: select subjects where mask is set, zero otherwise (SIMD blend)
  uint64x2_t out_s0 = vbslq_u64(m0, s0, zero);
  uint64x2_t out_s1 = vbslq_u64(m1, s1, zero);
  uint64x2_t out_s2 = vbslq_u64(m2, s2, zero);
  uint64x2_t out_s3 = vbslq_u64(m3, s3, zero);
  
  // Broadcast constants for P and O arrays
  uint64x2_t p_vec = vdupq_n_u64(p_const);
  uint64x2_t o_vec = vdupq_n_u64(o_const);
  
  uint64x2_t out_p0 = vbslq_u64(m0, p_vec, zero);
  uint64x2_t out_p1 = vbslq_u64(m1, p_vec, zero);
  uint64x2_t out_p2 = vbslq_u64(m2, p_vec, zero);
  uint64x2_t out_p3 = vbslq_u64(m3, p_vec, zero);
  
  uint64x2_t out_o0 = vbslq_u64(m0, o_vec, zero);
  uint64x2_t out_o1 = vbslq_u64(m1, o_vec, zero);
  uint64x2_t out_o2 = vbslq_u64(m2, o_vec, zero);
  uint64x2_t out_o3 = vbslq_u64(m3, o_vec, zero);
  
  // Store all 8 positions at once (SIMD store)
  vst1q_u64(out_S + 0, out_s0);
  vst1q_u64(out_S + 2, out_s1);
  vst1q_u64(out_S + 4, out_s2);
  vst1q_u64(out_S + 6, out_s3);
  
  vst1q_u64(out_P + 0, out_p0);
  vst1q_u64(out_P + 2, out_p1);
  vst1q_u64(out_P + 4, out_p2);
  vst1q_u64(out_P + 6, out_p3);
  
  vst1q_u64(out_O + 0, out_o0);
  vst1q_u64(out_O + 2, out_o1);
  vst1q_u64(out_O + 4, out_o2);
  vst1q_u64(out_O + 6, out_o3);
  
  // Optimize mask extraction: build bitmap directly without intermediate array
  // Extract mask bits and build bitmap in one pass (minimal operations)
  uint64_t len_mask_bits = (len >= 8) ? 0xFFULL : ((1ULL << len) - 1);
  uint64_t mask = 0;
  
  // Extract and build bitmap directly (8 extracts, but unavoidable)
  mask |= ((vgetq_lane_u64(m0, 0) != 0) ? 1ULL : 0);
  mask |= ((vgetq_lane_u64(m0, 1) != 0) ? 2ULL : 0);
  mask |= ((vgetq_lane_u64(m1, 0) != 0) ? 4ULL : 0);
  mask |= ((vgetq_lane_u64(m1, 1) != 0) ? 8ULL : 0);
  mask |= ((vgetq_lane_u64(m2, 0) != 0) ? 16ULL : 0);
  mask |= ((vgetq_lane_u64(m2, 1) != 0) ? 32ULL : 0);
  mask |= ((vgetq_lane_u64(m3, 0) != 0) ? 64ULL : 0);
  mask |= ((vgetq_lane_u64(m3, 1) != 0) ? 128ULL : 0);
  
  mask &= len_mask_bits;  // Apply len mask
  
  // Count using popcount (single instruction)
  size_t count = (size_t)__builtin_popcountll(mask);
  
  *out_mask = mask;
  return count;
#elif defined(__x86_64__)
  // Load all 8 subjects (SIMD load)
  __m256i s0 = _mm256_loadu_si256((const __m256i *)(s_p + 0));
  __m256i s1 = _mm256_loadu_si256((const __m256i *)(s_p + 4));
  
  // Generate masks: non-zero subjects become UINT64_MAX, zero becomes 0
  __m256i zero = _mm256_setzero_si256();
  __m256i m0 = _mm256_cmpeq_epi64(s0, zero);  // 0 if equal, UINT64_MAX if not
  __m256i m1 = _mm256_cmpeq_epi64(s1, zero);
  
  // Invert masks: non-zero subjects should be UINT64_MAX
  m0 = _mm256_cmpeq_epi64(m0, zero);  // non-zero lanes become UINT64_MAX
  m1 = _mm256_cmpeq_epi64(m1, zero);
  
  // Apply length mask: zero out positions beyond len (simple branchless mask)
  // This is done after stores, so we just need to mask the bitmap
  // No need for SIMD comparison - simple bitwise mask is faster
  
  // Blend: select subjects where mask is set, zero otherwise (SIMD blend)
  __m256i out_s0 = _mm256_blendv_epi8(zero, s0, m0);
  __m256i out_s1 = _mm256_blendv_epi8(zero, s1, m1);
  
  // Broadcast constants for P and O arrays
  __m256i p_vec = _mm256_set1_epi64x((long long)p_const);
  __m256i o_vec = _mm256_set1_epi64x((long long)o_const);
  
  __m256i out_p0 = _mm256_blendv_epi8(zero, p_vec, m0);
  __m256i out_p1 = _mm256_blendv_epi8(zero, p_vec, m1);
  
  __m256i out_o0 = _mm256_blendv_epi8(zero, o_vec, m0);
  __m256i out_o1 = _mm256_blendv_epi8(zero, o_vec, m1);
  
  // Store all 8 positions at once (SIMD store)
  _mm256_storeu_si256((__m256i *)(out_S + 0), out_s0);
  _mm256_storeu_si256((__m256i *)(out_S + 4), out_s1);
  
  _mm256_storeu_si256((__m256i *)(out_P + 0), out_p0);
  _mm256_storeu_si256((__m256i *)(out_P + 4), out_p1);
  
  _mm256_storeu_si256((__m256i *)(out_O + 0), out_o0);
  _mm256_storeu_si256((__m256i *)(out_O + 4), out_o1);
  
  // Optimize mask extraction: build bitmap directly without intermediate array
  // Extract mask bits and build bitmap in one pass (minimal operations)
  uint64_t len_mask_bits = (len >= 8) ? 0xFFULL : ((1ULL << len) - 1);
  uint64_t mask = 0;
  
  // Extract and build bitmap directly (8 extracts, but unavoidable)
  mask |= ((_mm256_extract_epi64(m0, 0) != 0) ? 1ULL : 0);
  mask |= ((_mm256_extract_epi64(m0, 1) != 0) ? 2ULL : 0);
  mask |= ((_mm256_extract_epi64(m0, 2) != 0) ? 4ULL : 0);
  mask |= ((_mm256_extract_epi64(m0, 3) != 0) ? 8ULL : 0);
  mask |= ((_mm256_extract_epi64(m1, 0) != 0) ? 16ULL : 0);
  mask |= ((_mm256_extract_epi64(m1, 1) != 0) ? 32ULL : 0);
  mask |= ((_mm256_extract_epi64(m1, 2) != 0) ? 64ULL : 0);
  mask |= ((_mm256_extract_epi64(m1, 3) != 0) ? 128ULL : 0);
  
  mask &= len_mask_bits;  // Apply len mask
  
  // Count using popcount (single instruction)
  size_t count = (size_t)__builtin_popcountll(mask);
  
  *out_mask = mask;
  return count;
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
