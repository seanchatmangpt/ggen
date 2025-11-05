# CONSTRUCT8 Optimization Analysis: Concurrency & Inference Opportunities

## Current Performance State

- **Current**: ~42 ticks (exceeds 8-tick budget)
- **Target**: ≤8 ticks (2ns)
- **Status**: Needs optimization

## Current Optimizations Applied

1. ✅ SIMD vectorization (ARM64: 4x uint64x2_t, x86_64: 2x __m256i)
2. ✅ Branchless operations (mask-based selection)
3. ✅ Alignment hints (`__builtin_assume_aligned`)
4. ✅ Prefetch hints (`__builtin_prefetch`)
5. ✅ ILP comments (overlap stores + mask extraction)
6. ✅ Constants computed once (zero, all_ones, p_vec, o_vec)

## Concurrency Opportunities

### 1. Instruction-Level Parallelism (ILP) Improvements

**Current Issues:**
- ARM: 12 SIMD stores are sequential (store stalls)
- Mask extraction happens after stores (should overlap)
- Broadcast operations could be parallelized with loads

**Optimizations:**

#### A. Overlap Loads with Mask Generation
```c
// Current: Sequential
s0 = vld1q_u64(s_p + 0);
s1 = vld1q_u64(s_p + 2);
m0 = veorq_u64(vceqq_u64(s0, zero), all_ones);

// Optimized: Pipeline overlap
s0 = vld1q_u64(s_p + 0);
s1 = vld1q_u64(s_p + 2);  // Load while computing m0
m0 = veorq_u64(vceqq_u64(s0, zero), all_ones);
s2 = vld1q_u64(s_p + 4);  // Load while computing m1
m1 = veorq_u64(vceqq_u64(s1, zero), all_ones);
```

#### B. Parallel Store Operations
```c
// Current: 12 sequential stores
// ARM has multiple execution units - can issue stores in parallel

// Option 1: Interleave stores (reduce store buffer pressure)
vst1q_u64(out_S + 0, out_s0);
vst1q_u64(out_P + 0, out_p0);  // Different cache line
vst1q_u64(out_S + 2, out_s1);
vst1q_u64(out_P + 2, out_p1);

// Option 2: Use non-temporal stores for write-only data
vst1q_u64(out_P + 0, out_p0);  // All same value (p_const)
// Could use streaming stores if data won't be read soon
```

#### C. Extract Mask During Blend Operations
```c
// Current: Extract mask after all blends
// Optimized: Extract mask bits while computing blends

// ARM: Extract MSBs during blend computation
uint64_t mask_bit_0 = (vgetq_lane_u64(m0, 0) >> 63) & 1ULL;
out_s0 = vbslq_u64(m0, s0, zero);  // Blend happens in parallel
mask_bit_1 = (vgetq_lane_u64(m0, 1) >> 63) & 1ULL;
// ... continue overlap
```

### 2. SIMD Width Optimization

**Current:**
- ARM: Using 128-bit vectors (2x uint64 per vector)
- x86: Using 256-bit vectors (4x uint64 per vector)

**Opportunities:**
- ARM: Could use SVE (Scalable Vector Extension) if available for wider vectors
- x86: Already using AVX2 (256-bit), could consider AVX-512 if available

**Trade-off:** Wider vectors may not help if we're only processing 8 elements

### 3. Register Pressure Reduction

**Current ARM register usage:**
- 4x s0-s3 (8 registers)
- 4x m0-m3 (4 registers)
- 2x p_vec, o_vec (2 registers)
- 12x out_* (12 registers)
- Total: ~26 registers

**ARM64 has 32 SIMD registers (128-bit)**, so we have headroom, but:
- Reducing register pressure allows better instruction scheduling
- Could reuse registers for mask extraction

### 4. Memory Access Optimization

**Current:**
- 4 loads (ARM) or 2 loads (x86)
- 12 stores (ARM) or 6 stores (x86)

**Optimizations:**

#### A. Non-Temporal Stores
```c
// If output arrays won't be read immediately, use streaming stores
// ARM: vst1q_u64 (regular) vs vstnt1q_u64 (non-temporal)
// x86: _mm256_store_si256 vs _mm256_stream_si256
```

#### B. Cache Line Alignment
- Ensure stores don't cross cache lines unnecessarily
- Current: 64-byte alignment should help, but verify store addresses

#### C. Write Combining
- ARM: Stores to same cache line can be combined
- x86: Write combining buffers can merge stores

## Inference Opportunities

### 1. Compile-Time Constant Folding

**If `len` is known at compile time:**
```c
// Current: Runtime computation
const uint64_t len_mask_bits = ((1ULL << len) - 1) & 0xFFULL;

// Optimized: Compile-time specialization
#if len == 8
  // Skip len_mask application (all bits set)
  mask &= 0xFFULL;  // Compiler can optimize to mask = mask
#elif len == 4
  mask &= 0x0FULL;  // Compile-time constant
#endif
```

**Implementation:** Template specialization or macro-based generation

### 2. Known Pattern Inference

**If all subjects are known non-zero:**
```c
// Current: Always computes masks
m0 = veorq_u64(vceqq_u64(s0, zero), all_ones);

// Optimized: Skip mask computation if pattern known
#ifdef KNOWN_ALL_NONZERO
  // Set all masks to all_ones directly
  m0 = all_ones;
  m1 = all_ones;
  m2 = all_ones;
  m3 = all_ones;
#else
  // Normal mask computation
#endif
```

**How to infer:**
- Track mask patterns from previous operations
- Cache mask results for same input pattern
- Use hint flags from caller

### 3. Constant Propagation

**If `p_const` and `o_const` are compile-time constants:**
```c
// Current: Runtime broadcast
const uint64x2_t p_vec = vdupq_n_u64(p_const);

// Optimized: Compile-time broadcast
#if defined(P_CONST_VALUE)
  const uint64x2_t p_vec = vdupq_n_u64(P_CONST_VALUE);  // Compiler can optimize
#endif
```

### 4. Mask Reuse Inference

**If mask was computed in previous operation:**
```c
// Current: Always computes mask from scratch
// Opportunity: Reuse mask from previous CONSTRUCT8 if:
// - Same input array (S_base)
// - Same offset (off)
// - Input hasn't changed

// Add mask cache:
static uint64_t cached_mask = 0;
static uint64_t cached_S_hash = 0;
uint64_t current_S_hash = hash(S_base + off, len);

if (cached_S_hash == current_S_hash) {
  // Reuse cached mask
  mask = cached_mask;
} else {
  // Compute mask and cache
  mask = compute_mask(...);
  cached_mask = mask;
  cached_S_hash = current_S_hash;
}
```

**Trade-off:** Cache lookup overhead vs mask computation cost

### 5. Zero-Detection Inference

**If we know pattern ahead of time:**
```c
// Current: Compares all subjects with zero
// Opportunity: If caller provides hint about zero positions

// Add hint parameter:
size_t knhk_construct8_emit_8(..., uint8_t zero_hint);

// If zero_hint is provided:
if (zero_hint == 0xFF) {
  // All non-zero - skip comparison
  mask = 0xFF;
} else if (zero_hint == 0x00) {
  // All zero - skip everything
  return 0;
} else {
  // Use hint to optimize mask computation
  // Only check positions where hint is uncertain
}
```

## Pipeline Parallelism Opportunities

### Current Pipeline Stages:
1. Load subjects (4 loads ARM, 2 loads x86)
2. Generate masks (4 comparisons ARM, 2 comparisons x86)
3. Broadcast constants (2 broadcasts)
4. Blend operations (12 blends ARM, 6 blends x86)
5. Store results (12 stores ARM, 6 stores x86)
6. Extract mask (8 lane extracts ARM, 2 movemask x86)
7. Apply len mask (1 bitwise AND)
8. Popcount (1 instruction)

### Optimized Pipeline Overlap:

```
Stage 1: Load s0, s1
Stage 2: Load s2, s3 | Compute m0 from s0
Stage 3: Load s4... | Compute m1 from s1 | Broadcast p_vec, o_vec
Stage 4: Compute m2 | Blend out_s0, out_p0, out_o0 | Store out_s0
Stage 5: Compute m3 | Blend out_s1, out_p1, out_o1 | Store out_p0, out_s1
Stage 6: Extract mask bits | Blend out_s2... | Store out_o0, out_p1
...
```

## Specific Optimizations to Implement

### Priority 1: Critical Path (Biggest Impact)

1. **Overlap mask extraction with stores**
   - Extract mask bits while stores are in flight
   - Use independent execution units

2. **Reduce store count**
   - ARM: 12 stores → could potentially combine
   - x86: 6 stores → already optimal width

3. **Eliminate redundant operations**
   - Broadcast p_vec, o_vec only once (already done)
   - Avoid recomputing constants

### Priority 2: Inference (Easy Wins)

1. **Compile-time `len` specialization**
   - If `len == 8` always, skip len_mask
   - Generate specialized code paths

2. **Known pattern hints**
   - Add optional hint parameter for zero positions
   - Skip comparisons where not needed

3. **Constant propagation**
   - Use `constexpr` or template parameters for known constants

### Priority 3: Advanced (Long-term)

1. **Mask caching**
   - Cache masks for repeated patterns
   - Requires careful invalidation logic

2. **SVE/AVX-512 support**
   - Wider vectors if hardware supports
   - May not help for fixed 8 elements

3. **Non-temporal stores**
   - If output is write-only
   - Requires caller knowledge

## Recommended Implementation Order

1. **Overlap mask extraction with stores** (immediate impact)
2. **Compile-time `len` specialization** (if len is often constant)
3. **Pipeline overlap improvements** (better instruction scheduling)
4. **Known pattern hints** (if caller can provide)
5. **Register pressure reduction** (if profiling shows issues)

## Measurement Strategy

- Profile with `perf` or `Instruments` to identify bottlenecks
- Use CPU performance counters to measure:
  - Store buffer stalls
  - Cache misses
  - Instruction retirement rates
  - Execution unit utilization

## Conclusion

**Key Opportunities:**
1. **ILP**: Overlap mask extraction with stores (biggest win)
2. **Inference**: Compile-time specialization for known `len`
3. **Pipeline**: Better instruction scheduling
4. **Memory**: Optimize store patterns

**Constraints:**
- ≤8 ticks budget is very tight
- Multi-threading overhead would exceed budget
- GPU would have too much overhead
- Focus on single-core optimization

**Next Steps:**
1. Profile current implementation to identify exact bottlenecks
2. Implement mask extraction overlap
3. Add compile-time specialization
4. Measure and iterate

