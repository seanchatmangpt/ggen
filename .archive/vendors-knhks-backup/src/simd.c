// simd.c
// SIMD-optimized operations for hot path execution

#include "simd.h"
#include <limits.h>

// Ensure NROWS is defined (default to 8 if not set)
#ifndef NROWS
#define NROWS 8u
#endif

#if defined(__aarch64__)
#include <arm_neon.h>
#elif defined(__x86_64__)
#include <immintrin.h>
#endif

// Branchless SIMD: count equal S == s_key over the run
uint64_t knhk_eq64_count_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key)
{
#if defined(__aarch64__)
  const uint64_t *p = base + off;
  const uint64x2_t K = vdupq_n_u64(key);
  uint64x2_t acc = vdupq_n_u64(0);
  uint64_t i = 0, n = len & ~3ULL;
  for (; i < n; i += 4)
  {
    uint64x2_t a0 = vld1q_u64(p + i + 0);
    uint64x2_t a1 = vld1q_u64(p + i + 2);
    uint64x2_t m0 = vceqq_u64(a0, K);
    uint64x2_t m1 = vceqq_u64(a1, K);
    // mask lanes -> {0,1} then accumulate
    const uint64x2_t ONE = vdupq_n_u64(1);
    uint64x2_t c0 = vandq_u64(m0, ONE);
    uint64x2_t c1 = vandq_u64(m1, ONE);
    acc = vaddq_u64(acc, vaddq_u64(c0, c1));
  }
  uint64_t t[2];
  vst1q_u64(t, acc);
  uint64_t cnt = t[0] + t[1];
  for (; i < len; ++i)
    cnt += (p[i] == key); // short tail
  return cnt;
#elif defined(__x86_64__)
  const uint64_t *p = base + off;
  const __m256i K = _mm256_set1_epi64x((long long)key);
  __m256i acc = _mm256_setzero_si256();
  const __m256i ONE = _mm256_set1_epi64x(1);
  uint64_t i = 0, n = len & ~3ULL;
  for (; i < n; i += 4)
  {
    __m256i a = _mm256_loadu_si256((const __m256i *)(p + i));
    __m256i m = _mm256_cmpeq_epi64(a, K);
    __m256i c = _mm256_and_si256(m, ONE);
    acc = _mm256_add_epi64(acc, c);
  }
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, acc);
  uint64_t cnt = t[0] + t[1] + t[2] + t[3];
  for (; i < len; ++i)
    cnt += (p[i] == key);
  return cnt;
#else
  uint64_t cnt = 0;
  for (uint64_t i = 0; i < len; i++)
    cnt += (base[off + i] == key);
  return cnt;
#endif
}

// Branchless SIMD: check if any S == s_key exists (no early termination)
int knhk_eq64_exists_run(const uint64_t *base, uint64_t off, uint64_t len, uint64_t key)
{
#if defined(__aarch64__)
  const uint64_t *p = base + off;
  const uint64x2_t K = vdupq_n_u64(key);
  uint64x2_t acc = vdupq_n_u64(0);
  uint64_t i = 0, n = len & ~3ULL;
  // Branchless: accumulate matches, check result after loop
  for (; i < n; i += 4)
  {
    uint64x2_t a0 = vld1q_u64(p + i + 0);
    uint64x2_t a1 = vld1q_u64(p + i + 2);
    uint64x2_t m0 = vceqq_u64(a0, K);
    uint64x2_t m1 = vceqq_u64(a1, K);
    // Or-reduce: any non-zero lane means match exists
    acc = vorrq_u64(acc, vorrq_u64(m0, m1));
  }
  // Check accumulated result (branchless reduction)
  uint64_t t[2];
  vst1q_u64(t, acc);
  uint64_t has_match = t[0] | t[1];
  // Handle tail branchlessly
  for (; i < len; ++i)
    has_match |= (p[i] == key);
  return has_match != 0;
#elif defined(__x86_64__)
  const uint64_t *p = base + off;
  const __m256i K = _mm256_set1_epi64x((long long)key);
  __m256i acc = _mm256_setzero_si256();
  uint64_t i = 0, n = len & ~3ULL;
  // Branchless: accumulate matches
  for (; i < n; i += 4)
  {
    __m256i a = _mm256_loadu_si256((const __m256i *)(p + i));
    __m256i m = _mm256_cmpeq_epi64(a, K);
    acc = _mm256_or_si256(acc, m);
  }
  // Check accumulated result
  uint64_t t[4];
  _mm256_storeu_si256((__m256i *)t, acc);
  uint64_t has_match = t[0] | t[1] | t[2] | t[3];
  // Handle tail branchlessly
  for (; i < len; ++i)
    has_match |= (p[i] == key);
  return has_match != 0;
#else
  uint64_t has_match = 0;
  for (uint64_t i = 0; i < len; i++)
    has_match |= (base[off + i] == key ? UINT64_MAX : 0);
  return has_match != 0;
#endif
}

// NROWS==8 optimized functions are now inline in simd.h
// No need to define them here - they're header-only inline functions

// Branchless S-P-O triple matching: check if S==s_key AND O==o_key exists
int knhk_eq64_spo_exists_run(const uint64_t *S_base, const uint64_t *O_base,
                              uint64_t off, uint64_t len, uint64_t s_key, uint64_t o_key)
{
#if defined(__aarch64__)
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  if (len == 0)
    return 0;
  if (len == 1)
    return ((s_p[0] == s_key) && (o_p[0] == o_key)) ? 1 : 0;
  if (len == 2)
  {
    uint64x2_t s0 = vld1q_u64(s_p + 0);
    uint64x2_t o0 = vld1q_u64(o_p + 0);
    uint64x2_t Ks = vdupq_n_u64(s_key);
    uint64x2_t Ko = vdupq_n_u64(o_key);
    uint64x2_t ms = vceqq_u64(s0, Ks);
    uint64x2_t mo = vceqq_u64(o0, Ko);
    uint64x2_t combined = vandq_u64(ms, mo);
    uint64_t t[2];
    vst1q_u64(t, combined);
    return (t[0] | t[1]) != 0;
  }
  // For len >= 3, process in chunks
  uint64_t has_match = 0;
  uint64_t i = 0;
  uint64_t n = len & ~1ULL; // Process pairs
  for (; i < n; i += 2)
  {
    uint64x2_t s0 = vld1q_u64(s_p + i);
    uint64x2_t o0 = vld1q_u64(o_p + i);
    uint64x2_t Ks = vdupq_n_u64(s_key);
    uint64x2_t Ko = vdupq_n_u64(o_key);
    uint64x2_t ms = vceqq_u64(s0, Ks);
    uint64x2_t mo = vceqq_u64(o0, Ko);
    uint64x2_t combined = vandq_u64(ms, mo);
    uint64_t t[2];
    vst1q_u64(t, combined);
    has_match |= (t[0] | t[1]);
  }
  // Handle tail
  for (; i < len; ++i)
  {
    has_match |= ((s_p[i] == s_key) && (o_p[i] == o_key) ? UINT64_MAX : 0);
  }
  return has_match != 0;
#elif defined(__x86_64__)
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  if (len == 0)
    return 0;
  if (len == 1)
    return ((s_p[0] == s_key) && (o_p[0] == o_key)) ? 1 : 0;
  // Process 4 at a time
  uint64_t has_match = 0;
  uint64_t i = 0;
  uint64_t n = len & ~3ULL;
  for (; i < n; i += 4)
  {
    __m256i s = _mm256_loadu_si256((const __m256i *)(s_p + i));
    __m256i o = _mm256_loadu_si256((const __m256i *)(o_p + i));
    __m256i Ks = _mm256_set1_epi64x((long long)s_key);
    __m256i Ko = _mm256_set1_epi64x((long long)o_key);
    __m256i ms = _mm256_cmpeq_epi64(s, Ks);
    __m256i mo = _mm256_cmpeq_epi64(o, Ko);
    __m256i combined = _mm256_and_si256(ms, mo);
    uint64_t t[4];
    _mm256_storeu_si256((__m256i *)t, combined);
    has_match |= (t[0] | t[1] | t[2] | t[3]);
  }
  // Handle tail
  for (; i < len; ++i)
  {
    has_match |= ((s_p[i] == s_key) && (o_p[i] == o_key) ? UINT64_MAX : 0);
  }
  return has_match != 0;
#else
  uint64_t has_match = 0;
  for (uint64_t i = 0; i < len; i++)
    has_match |= ((S_base[off + i] == s_key) && (O_base[off + i] == o_key) ? UINT64_MAX : 0);
  return has_match != 0;
#endif
}

// Branchless SELECT: gather matching O values (optimized for variable NROWS)
// For NROWS=8, use knhk_select_gather_8 instead (fully unrolled inline version)
size_t knhk_select_gather(const uint64_t *S_base, const uint64_t *O_base,
                           uint64_t off, uint64_t len, uint64_t s_key,
                           uint64_t *out, size_t out_capacity)
{
  size_t out_idx = 0;
#if defined(__aarch64__) || defined(__x86_64__)
  const uint64_t *s_p = S_base + off;
  const uint64_t *o_p = O_base + off;
  // For variable NROWS, process sequentially with branchless conditional writes
  // Use mask-based writes to avoid branches
  for (uint64_t i = 0; i < len && out_idx < out_capacity; ++i)
  {
    // Branchless comparison: match is 1 if equal, 0 otherwise
    uint64_t match = (s_p[i] == s_key) ? 1 : 0;
    // Only write if match AND have capacity (branchless)
    uint64_t write_mask = match & (out_idx < out_capacity ? 1 : 0);
    // Conditional write using mask (branchless)
    out[out_idx] = (write_mask ? o_p[i] : out[out_idx]);
    // Increment only if match (branchless)
    out_idx += match;
  }
#else
  for (uint64_t i = 0; i < len && out_idx < out_capacity; ++i)
  {
    if (S_base[off + i] == s_key)
    {
      out[out_idx++] = O_base[off + i];
    }
  }
#endif
  return out_idx;
}

