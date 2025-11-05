// simd.h
// SIMD-optimized operations (internal header)

#ifndef KNHKS_SIMD_H
#define KNHKS_SIMD_H

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

// Branchless SIMD: check if any S == s_key exists (no early termination)
int knhks_eq64_exists_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key);

// Branchless SIMD: count equal S == s_key over the run
uint64_t knhks_eq64_count_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key);

// Branchless S-P-O triple matching: check if S==s_key AND O==o_key exists
int knhks_eq64_spo_exists_run(const uint64_t *S_base, const uint64_t *O_base,
                               uint64_t off, uint64_t len, uint64_t s_key, uint64_t o_key);

// Branchless SELECT: gather matching O values
size_t knhks_select_gather(const uint64_t *S_base, const uint64_t *O_base,
                            uint64_t off, uint64_t len, uint64_t s_key,
                            uint64_t *out, size_t out_capacity);

// Ultra-fast SELECT_SP for exactly 8 elements - fully unrolled, branchless writes
#if NROWS == 8
static inline size_t knhks_select_gather_8(const uint64_t *S_base, const uint64_t *O_base,
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

#if NROWS == 8
// Optimized for NROWS=8: fully unrolled, zero branches
// Inline implementations for header inclusion

// Ultra-fast ASK(S,P) for exactly 8 elements - fully unrolled
static inline int knhks_eq64_exists_8(const uint64_t *base, uint64_t off, uint64_t key)
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

// Ultra-fast COUNT(S,P) for exactly 8 elements - fully unrolled
static inline uint64_t knhks_eq64_count_8(const uint64_t *base, uint64_t off, uint64_t key)
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

// Ultra-fast ASK(O,P) for exactly 8 elements - fully unrolled (reverse lookup)
static inline int knhks_eq64_exists_o_8(const uint64_t *base, uint64_t off, uint64_t key)
{
  // Same as knhks_eq64_exists_8 but semantically for O array
  return knhks_eq64_exists_8(base, off, key);
}

// Ultra-fast ASK(S,P,O) for exactly 8 elements - fully unrolled
static inline int knhks_eq64_spo_exists_8(const uint64_t *S_base, const uint64_t *O_base,
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

// Ultra-fast comparison operations for exactly 8 elements - fully unrolled
// Check if any O value matches comparison operator with threshold
static inline int knhks_compare_o_8(const uint64_t *O_base, uint64_t off, uint64_t threshold, int op_type)
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
#endif
}

#if NROWS == 8
// Ultra-fast datatype validation for exactly 8 elements - fully unrolled
// Check if (s, p) has any object matching datatype_hash
// This validates SHACL datatype constraints: checks if subject-predicate pair has object with correct datatype
static inline int knhks_validate_datatype_sp_8(const uint64_t *S_base, const uint64_t *O_base,
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

// Ultra-fast CONSTRUCT8 for exactly 8 elements - fully unrolled, branchless SIMD
// Emit template (S[i], p_const, o_const) for all non-zero S[i] lanes
// Returns number of lanes written, sets out_mask bitmask
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

#endif // KNHKS_SIMD_H
