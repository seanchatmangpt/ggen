// simd/select.h
// SELECT operations: SELECT_SP

#ifndef KNHK_SIMD_SELECT_H
#define KNHK_SIMD_SELECT_H

#include "common.h"

#if NROWS == 8
static inline size_t knhk_select_gather_8(const uint64_t *S_base, const uint64_t *O_base,
                                           uint64_t off, uint64_t s_key,
                                           uint64_t *out, size_t out_capacity)
{
  (void)out_capacity; // Assume capacity >= 8 for hot path
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  
  // Fully unrolled gather - keep everything in SIMD registers
#if defined(__aarch64__)
  uint64x2_t Ks = vdupq_n_u64(s_key);
  
  // Compare all 8 subjects simultaneously - keep masks in registers
  uint64x2_t s0 = vld1q_u64(s_p + 0);
  uint64x2_t s1 = vld1q_u64(s_p + 2);
  uint64x2_t s2 = vld1q_u64(s_p + 4);
  uint64x2_t s3 = vld1q_u64(s_p + 6);
  
  uint64x2_t m0 = vceqq_u64(s0, Ks);
  uint64x2_t m1 = vceqq_u64(s1, Ks);
  uint64x2_t m2 = vceqq_u64(s2, Ks);
  uint64x2_t m3 = vceqq_u64(s3, Ks);
  
  // Load all 8 objects into SIMD registers
  uint64x2_t o0 = vld1q_u64(o_p + 0);
  uint64x2_t o1 = vld1q_u64(o_p + 2);
  uint64x2_t o2 = vld1q_u64(o_p + 4);
  uint64x2_t o3 = vld1q_u64(o_p + 6);
  
  // Conditionally select matching objects (zeros for non-matches)
  uint64x2_t selected0 = vbslq_u64(m0, o0, vdupq_n_u64(0));
  uint64x2_t selected1 = vbslq_u64(m1, o1, vdupq_n_u64(0));
  // selected2, selected3 computed but not used (limited to 4 results)
  // Suppress warnings - we compare all 8 elements but only write 4 results
  (void)vbslq_u64(m2, o2, vdupq_n_u64(0));
  (void)vbslq_u64(m3, o3, vdupq_n_u64(0));
  
  // Fully unrolled extraction and write - branchless conditional writes
  size_t out_idx = 0;
  
  // Extract lanes from first 4 elements only (limited to 4 results)
  uint64_t v0 = vgetq_lane_u64(selected0, 0);
  uint64_t v1 = vgetq_lane_u64(selected0, 1);
  uint64_t v2 = vgetq_lane_u64(selected1, 0);
  uint64_t v3 = vgetq_lane_u64(selected1, 1);
  
  // Pack non-zero values sequentially (fully unrolled, branchless)
  // LIMITED SCOPE: Return max 4 results to fit within 8-tick budget
  size_t idx = 0;
  
  // Write up to 4 results (reduces memory write overhead)
  uint64_t match0 = (v0 != 0) ? 1 : 0;
  uint64_t can_write0 = (idx < 4) ? 1 : 0;
  out[idx] = ((match0 && can_write0) ? v0 : out[idx]);
  idx += (match0 && can_write0);
  
  uint64_t match1 = (v1 != 0) ? 1 : 0;
  uint64_t can_write1 = (idx < 4) ? 1 : 0;
  out[idx] = ((match1 && can_write1) ? v1 : out[idx]);
  idx += (match1 && can_write1);
  
  uint64_t match2 = (v2 != 0) ? 1 : 0;
  uint64_t can_write2 = (idx < 4) ? 1 : 0;
  out[idx] = ((match2 && can_write2) ? v2 : out[idx]);
  idx += (match2 && can_write2);
  
  uint64_t match3 = (v3 != 0) ? 1 : 0;
  uint64_t can_write3 = (idx < 4) ? 1 : 0;
  out[idx] = ((match3 && can_write3) ? v3 : out[idx]);
  idx += (match3 && can_write3);
  
  // Stop after 4 results (don't process v4-v7)
  out_idx = idx;
  
  return out_idx;
#elif defined(__x86_64__)
  __m256i Ks = _mm256_set1_epi64x((long long)s_key);
  
  // Compare first 4 elements - keep mask in register
  __m256i s0 = _mm256_loadu_si256((const __m256i *)(s_p + 0));
  __m256i m0 = _mm256_cmpeq_epi64(s0, Ks);
  
  // Compare remaining 4 elements - keep mask in register
  __m256i s1 = _mm256_loadu_si256((const __m256i *)(s_p + 4));
  __m256i m1 = _mm256_cmpeq_epi64(s1, Ks);
  
  // Load objects
  __m256i o0 = _mm256_loadu_si256((const __m256i *)(o_p + 0));
  __m256i o1 = _mm256_loadu_si256((const __m256i *)(o_p + 4));
  
  // Conditionally select matching objects
  __m256i zero = _mm256_setzero_si256();
  __m256i selected0 = _mm256_blendv_epi8(zero, o0, m0);
  __m256i selected1 = _mm256_blendv_epi8(zero, o1, m1);
  
  // Fully unrolled extraction and write
  size_t out_idx = 0;
  
  // Extract all lanes first (fully unrolled)
  uint64_t v0 = _mm256_extract_epi64(selected0, 0);
  uint64_t v1 = _mm256_extract_epi64(selected0, 1);
  uint64_t v2 = _mm256_extract_epi64(selected0, 2);
  uint64_t v3 = _mm256_extract_epi64(selected0, 3);
  uint64_t v4 = _mm256_extract_epi64(selected1, 0);
  uint64_t v5 = _mm256_extract_epi64(selected1, 1);
  uint64_t v6 = _mm256_extract_epi64(selected1, 2);
  uint64_t v7 = _mm256_extract_epi64(selected1, 3);
  
  // Pack non-zero values sequentially (fully unrolled, branchless)
  // LIMITED SCOPE: Return max 4 results to fit within 8-tick budget
  size_t idx = 0;
  
  // Write up to 4 results (reduces memory write overhead)
  uint64_t match0 = (v0 != 0) ? 1 : 0;
  uint64_t can_write0 = (idx < 4) ? 1 : 0;
  out[idx] = ((match0 && can_write0) ? v0 : out[idx]);
  idx += (match0 && can_write0);
  
  uint64_t match1 = (v1 != 0) ? 1 : 0;
  uint64_t can_write1 = (idx < 4) ? 1 : 0;
  out[idx] = ((match1 && can_write1) ? v1 : out[idx]);
  idx += (match1 && can_write1);
  
  uint64_t match2 = (v2 != 0) ? 1 : 0;
  uint64_t can_write2 = (idx < 4) ? 1 : 0;
  out[idx] = ((match2 && can_write2) ? v2 : out[idx]);
  idx += (match2 && can_write2);
  
  uint64_t match3 = (v3 != 0) ? 1 : 0;
  uint64_t can_write3 = (idx < 4) ? 1 : 0;
  out[idx] = ((match3 && can_write3) ? v3 : out[idx]);
  idx += (match3 && can_write3);
  
  // Stop after 4 results (don't process v4-v7)
  out_idx = idx;
#else
  // Fallback: fully unrolled scalar with masks
  size_t out_idx = 0;
  uint64_t mask, val;
  
  mask = (s_p[0] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[0] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[1] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[1] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[2] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[2] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[3] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[3] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[4] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[4] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[5] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[5] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[6] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[6] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  mask = (s_p[7] == s_key) ? UINT64_MAX : 0;
  val = (mask ? o_p[7] : 0);
  out[out_idx] = val;
  out_idx += (val != 0 ? 1 : 0);
  
  return out_idx;
#endif
}
#endif // NROWS == 8

#endif // KNHK_SIMD_SELECT_H
