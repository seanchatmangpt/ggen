# Rust AOT/Compile-Time Optimizations for CONSTRUCT8 Hot Path

## Overview
All optimizations that Rust can prepare on the warm/cold path to eliminate runtime overhead in the C hot path. Organized by impact and implementation complexity.

## 1. Code Generation & Specialization (Zero Runtime Cost)

### 1.1. Length-Specialized Function Generation
**Current Runtime Cost:** `len_mask_bits = ((1ULL << len) - 1) & 0xFFULL` (~0.25 ticks)

**Rust AOT Action:**
- Generate specialized C functions for each `len` value: `{1, 2, 3, 4, 5, 6, 7, 8}`
- Functions: `knhk_construct8_emit_8_len1()`, `knhk_construct8_emit_8_len8()`, etc.
- Each function has `len_mask_bits` as compile-time constant (0 ticks)
- For `len == 8`: Skip `len_mask` application entirely (compiler optimizes to no-op)

**Savings:** ~0.25 ticks per call

**Implementation:**
```rust
// rust/knhk-aot/src/codegen.rs
pub fn generate_len_specialized_functions() -> Vec<String> {
    (1..=8).map(|len| {
        let len_mask = (1u64 << len) - 1;
        format!(
            "static inline size_t knhk_construct8_emit_8_len{}(...) {{
                // len_mask_bits = {}ULL (compile-time constant)
                const uint64_t len_mask_bits = {}ULL;
                // ... rest of function with len_mask_bits precomputed
            }}",
            len, len_mask, len_mask
        )
    }).collect()
}
```

### 1.2. Constant-Specialized Function Generation
**Current Runtime Cost:** `p_vec = vdupq_n_u64(p_const)` (~0.5 ticks per broadcast)

**Rust AOT Action:**
- Analyze common `p_const`/`o_const` values at compile time
- Generate specialized functions: `knhk_construct8_emit_8_p_X_o_Y()`
- Compiler can optimize broadcasts to immediate constants
- For common constants (0, 1, small values), compiler may eliminate broadcasts

**Savings:** ~0.5-1 ticks per call (if constants are common)

**Implementation:**
```rust
// Track common p_const/o_const pairs
pub fn generate_constant_specialized_functions(
    common_pairs: &[(u64, u64)]
) -> Vec<String> {
    common_pairs.iter().map(|(p, o)| {
        format!(
            "static inline size_t knhk_construct8_emit_8_p_{}_o_{}(...) {{
                // p_vec = vdupq_n_u64({}) - compiler optimizes to constant
                const uint64x2_t p_vec = vdupq_n_u64({});
                const uint64x2_t o_vec = vdupq_n_u64({});
                // ... rest of function
            }}",
            p, o, p, p, o
        )
    }).collect()
}
```

### 1.3. Pattern-Specialized Function Generation
**Current Runtime Cost:** 4 mask comparisons (~4 ticks)

**Rust AOT Action:**
- Analyze input patterns at compile time:
  - `ALL_NONZERO`: All subjects are non-zero → skip mask generation
  - `ALL_ZERO`: All subjects are zero → early return
  - `SPARSE`: Mixed pattern → use optimized sparse path
- Generate specialized functions for each pattern

**Savings:** ~4 ticks for ALL_NONZERO pattern

**Implementation:**
```rust
pub enum PatternType {
    AllNonZero,  // Skip mask generation entirely
    AllZero,      // Early return (0 ticks)
    Sparse,       // Optimized sparse computation
    Dense,        // Standard computation
}

pub fn generate_pattern_specialized_functions() -> Vec<String> {
    vec![
        // All non-zero: skip mask generation
        "static inline size_t knhk_construct8_emit_8_all_nonzero(...) {
            // m0 = m1 = m2 = m3 = all_ones (compile-time constant)
            const uint64x2_t m0 = all_ones;
            // ... skip mask computation
        }".to_string(),
        // All zero: early return
        "static inline size_t knhk_construct8_emit_8_all_zero(...) {
            *out_mask = 0;
            return 0;
        }".to_string(),
    ]
}
```

## 2. Precomputed Constants & Hints (Near-Zero Runtime Cost)

### 2.1. Precomputed Length Mask Bits
**Current Runtime Cost:** `len_mask_bits = ((1ULL << len) - 1) & 0xFFULL` (~0.25 ticks)

**Rust AOT Action:**
- Precompute `len_mask_bits` for known `len` values
- Store in IR structure as hint field
- Hot path uses precomputed value directly

**Savings:** ~0.25 ticks

**Implementation:**
```rust
// rust/knhk-hot/src/ffi.rs
#[repr(C)]
pub struct Ir {
    // ... existing fields
    pub len_mask_hint: u64,  // Precomputed: ((1 << len) - 1) & 0xFF
}

impl Ir {
    pub fn new_construct8(len: u64, p: u64, o: u64) -> Self {
        let len_mask = if len == 8 { 0xFF } else { (1 << len) - 1 };
        Self {
            // ... 
            len_mask_hint: len_mask,
        }
    }
}
```

### 2.2. Zero-Position Hint Bitmask
**Current Runtime Cost:** 4 mask comparisons (~4 ticks)

**Rust AOT Action:**
- Analyze input subjects at warm-path time
- Precompute zero-position bitmask: `bit i = 1 if S[i] == 0, else 0`
- Store in IR structure as hint
- Hot path uses hint to skip unnecessary comparisons or use optimized paths

**Savings:** ~2-4 ticks (depending on pattern)

**Implementation:**
```rust
#[repr(C)]
pub struct Ir {
    // ... existing fields
    pub zero_hint: u8,  // Bitmask: bit i = 1 if subject[i] == 0
}

// Warm path analysis
pub fn analyze_zero_pattern(subjects: &[u64; 8]) -> u8 {
    let mut hint = 0u8;
    for (i, &s) in subjects.iter().enumerate() {
        if s == 0 {
            hint |= 1 << i;
        }
    }
    hint
}
```

### 2.3. Precomputed Broadcast Values
**Current Runtime Cost:** 2 broadcasts (~1 tick)

**Rust AOT Action:**
- Precompute broadcast vectors for common `p_const`/`o_const` values
- Store in lookup table or as immediate constants
- Hot path uses precomputed vectors

**Savings:** ~0.5-1 ticks (if values are common)

**Implementation:**
```rust
// Precomputed broadcast vectors for common constants
pub const COMMON_BROADCASTS: &[(u64, [u64; 2])] = &[
    (0, [0, 0]),
    (1, [1, 1]),
    // ... other common values
];

pub fn get_broadcast_hint(constant: u64) -> Option<[u64; 2]> {
    COMMON_BROADCASTS.iter()
        .find(|(c, _)| *c == constant)
        .map(|(_, vec)| *vec)
}
```

## 3. Function Pointer Dispatch (Low Runtime Cost)

### 3.1. Optimal Function Selection
**Current Runtime Cost:** Generic function with runtime checks

**Rust AOT Action:**
- Analyze operation parameters at warm-path time
- Select optimal specialized function variant
- Store function pointer in IR structure
- Hot path calls specialized function directly (no dispatch overhead)

**Savings:** ~1-2 ticks (eliminates runtime checks)

**Implementation:**
```rust
// rust/knhk-aot/src/dispatch.rs
pub type Construct8Fn = unsafe extern "C" fn(
    *const u64, u64, u64, u64, u64,
    *mut u64, *mut u64, *mut u64,
    *mut u64
) -> usize;

pub fn select_optimal_function(
    len: u64,
    p_const: u64,
    o_const: u64,
    zero_hint: u8
) -> Construct8Fn {
    match (len, zero_hint) {
        (8, 0xFF) => knhk_construct8_emit_8_all_zero,  // All zero
        (8, 0x00) => knhk_construct8_emit_8_all_nonzero,  // All non-zero
        (8, _) => knhk_construct8_emit_8_len8,  // len=8 specialization
        (len, _) => knhk_construct8_emit_8_lenN,  // Other len values
    }
}

#[repr(C)]
pub struct Ir {
    // ... existing fields
    pub construct8_fn: Option<Construct8Fn>,  // AOT-selected function pointer
}
```

## 4. Memory Layout & Alignment (Zero Runtime Cost)

### 4.1. Guaranteed 64-Byte Alignment
**Current Runtime Cost:** Alignment hints (`__builtin_assume_aligned`) - 0 ticks (compiler hint)

**Rust AOT Action:**
- Ensure all arrays allocated with 64-byte alignment
- Use `#[repr(align(64))]` or aligned allocators
- Verify alignment at allocation time (warm path)
- No runtime alignment checks needed

**Savings:** 0 ticks (eliminates need for runtime checks)

**Implementation:**
```rust
// rust/knhk-hot/src/ffi.rs
#[repr(align(64))]
pub struct AlignedArray<T>(pub T);

// Ensure alignment at allocation
pub fn allocate_aligned_arrays() -> (
    Box<AlignedArray<[u64; 8]>>,
    Box<AlignedArray<[u64; 8]>>,
    Box<AlignedArray<[u64; 8]>>
) {
    (
        Box::new(AlignedArray([0; 8])),
        Box::new(AlignedArray([0; 8])),
        Box::new(AlignedArray([0; 8])),
    )
}
```

### 4.2. Pre-allocated Output Buffers
**Current Runtime Cost:** Buffer allocation check (if not pre-allocated)

**Rust AOT Action:**
- Pre-allocate all output buffers at warm-path time
- Ensure buffers are 64-byte aligned and sized correctly
- Store buffer pointers in IR structure
- Hot path uses pre-allocated buffers (no allocation overhead)

**Savings:** ~0.5 ticks (eliminates allocation checks)

**Implementation:**
```rust
pub struct PreallocatedBuffers {
    pub out_s: Box<AlignedArray<[u64; 8]>>,
    pub out_p: Box<AlignedArray<[u64; 8]>>,
    pub out_o: Box<AlignedArray<[u64; 8]>>,
}

impl PreallocatedBuffers {
    pub fn new() -> Self {
        Self {
            out_s: Box::new(AlignedArray([0; 8])),
            out_p: Box::new(AlignedArray([0; 8])),
            out_o: Box::new(AlignedArray([0; 8])),
        }
    }
}
```

## 5. Input Analysis & Validation (Zero Runtime Cost)

### 5.1. Precomputed Input Validation
**Current Runtime Cost:** Validation checks in hot path (~0.5 ticks)

**Rust AOT Action:**
- Validate all inputs at warm-path time
- Ensure `len <= 8`, buffers non-NULL, alignment correct
- Set validation flags in IR structure
- Hot path skips validation (trusts AOT validation)

**Savings:** ~0.5 ticks

**Implementation:**
```rust
#[repr(C)]
pub struct Ir {
    // ... existing fields
    pub validated: bool,  // AOT validation flag
}

// Warm path validation
pub fn validate_and_prepare_ir(ir: &mut Ir) -> Result<(), ValidationError> {
    // Validate all constraints
    if ir.len > 8 { return Err(ValidationError::InvalidLength); }
    if ir.out_s.is_null() { return Err(ValidationError::NullBuffer); }
    // ... other validations
    
    ir.validated = true;
    Ok(())
}
```

### 5.2. Precomputed Offset Validation
**Current Runtime Cost:** Offset bounds checking

**Rust AOT Action:**
- Validate `off + len <= array_size` at warm-path time
- Ensure no out-of-bounds access
- Store validation result in IR
- Hot path trusts AOT validation

**Savings:** ~0.25 ticks

## 6. Pattern Caching (Medium Runtime Cost, High Benefit)

### 6.1. Mask Pattern Cache
**Current Runtime Cost:** Full mask computation (~4 ticks)

**Rust AOT Action:**
- Cache mask patterns for repeated input patterns
- Use hash of input subjects as cache key
- Store cached mask in IR structure
- Hot path reuses cached mask if available

**Trade-off:** Cache lookup (~0.5 ticks) vs mask computation (~4 ticks)

**Implementation:**
```rust
use std::collections::HashMap;

pub struct MaskCache {
    cache: HashMap<u64, u64>,  // hash(subjects) -> mask
}

impl MaskCache {
    pub fn get_or_compute(&mut self, subjects: &[u64; 8]) -> u64 {
        let key = self.hash_subjects(subjects);
        *self.cache.entry(key)
            .or_insert_with(|| self.compute_mask(subjects))
    }
    
    fn hash_subjects(&self, subjects: &[u64; 8]) -> u64 {
        // Fast hash of subjects
        subjects.iter().fold(0, |acc, &s| acc ^ s)
    }
    
    fn compute_mask(&self, subjects: &[u64; 8]) -> u64 {
        // Compute mask (warm path only)
        subjects.iter().enumerate()
            .fold(0, |acc, (i, &s)| acc | ((s != 0) as u64) << i)
    }
}
```

## 7. Compile-Time Code Generation (Build-Time)

### 7.1. Macro-Based Code Generation
**Current Runtime Cost:** Generic code paths

**Rust AOT Action:**
- Use Rust macros to generate C code at build time
- Generate specialized functions for all combinations:
  - 8 len values × N pattern types × M constant pairs
- Compile generated C code into library
- Hot path uses specialized functions

**Savings:** Varies by specialization (0-4 ticks per call)

**Implementation:**
```rust
// rust/knhk-aot/build.rs
use std::fs::File;
use std::io::Write;

fn main() {
    let mut generated = File::create("src/generated/construct8_specialized.c").unwrap();
    
    // Generate all combinations
    for len in 1..=8 {
        for pattern in &[PatternType::AllNonZero, PatternType::Sparse, PatternType::Dense] {
            writeln!(generated, "{}", generate_function(len, pattern)).unwrap();
        }
    }
}
```

### 7.2. Template Specialization
**Current Runtime Cost:** Generic template instantiation

**Rust AOT Action:**
- Use Rust to generate C template instantiations
- Generate specialized code for common cases
- Compile specialized versions into library
- Hot path uses appropriate specialization

**Savings:** ~1-2 ticks (eliminates template overhead)

## 8. Summary: Total Potential Savings

| Optimization | Current Cost | Rust AOT Cost | Savings | Implementation Complexity |
|--------------|--------------|---------------|---------|---------------------------|
| Length specialization | 0.25 ticks | 0 ticks | 0.25 ticks | Low |
| Constant specialization | 1 tick | 0 ticks | 1 tick | Medium |
| Pattern specialization (all non-zero) | 4 ticks | 0 ticks | 4 ticks | Low |
| Zero-position hint | 4 ticks | 0 ticks | 2-4 ticks | Medium |
| Function pointer dispatch | 1-2 ticks | 0 ticks | 1-2 ticks | Medium |
| Precomputed len_mask | 0.25 ticks | 0 ticks | 0.25 ticks | Low |
| Alignment guarantees | 0 ticks | 0 ticks | 0 ticks | Low |
| Pre-allocated buffers | 0.5 ticks | 0 ticks | 0.5 ticks | Low |
| Input validation | 0.5 ticks | 0 ticks | 0.5 ticks | Low |
| Mask pattern cache | 4 ticks | 0.5 ticks | 3.5 ticks | Medium |

**Total Potential Savings:** ~10-15 ticks (bringing 42 ticks down to ~27-32 ticks)

**Best Case Scenario (All Optimizations):**
- Length specialization: -0.25 ticks
- Pattern specialization (all non-zero): -4 ticks
- Zero-position hint: -2 ticks
- Function dispatch: -1 tick
- Precomputed constants: -0.5 ticks
- **Total: ~-7.75 ticks**

**Remaining Cost:** ~34 ticks (still exceeds 8-tick budget, but significantly improved)

## 9. Implementation Priority

### Priority 1: High Impact, Low Complexity
1. ✅ Precomputed `len_mask_bits` (0.25 ticks saved)
2. ✅ Pre-allocated buffers (0.5 ticks saved)
3. ✅ Length-specialized functions (0.25 ticks saved)
4. ✅ Pattern specialization for all-nonzero (4 ticks saved)

### Priority 2: Medium Impact, Medium Complexity
5. ✅ Zero-position hint (2-4 ticks saved)
6. ✅ Function pointer dispatch (1-2 ticks saved)
7. ✅ Constant specialization (1 tick saved)

### Priority 3: Advanced (Long-term)
8. Mask pattern cache (3.5 ticks saved, but requires cache management)
9. Build-time code generation (varies, but high maintenance cost)

## 10. Files to Create/Modify

**Rust Warm/Cold Path:**
- `rust/knhk-aot/src/codegen.rs` - Code generation for specialized functions
- `rust/knhk-aot/src/pattern_analyzer.rs` - Pattern detection and analysis
- `rust/knhk-aot/src/dispatch.rs` - Function pointer dispatch logic
- `rust/knhk-aot/src/hints.rs` - Precomputed hint generation
- `rust/knhk-hot/src/ffi.rs` - Add AOT hint fields to IR structure

**C Hot Path:**
- `src/simd/construct.h` - Add specialized function variants
- `include/knhk/types.h` - Add AOT hint fields to IR structure
- `include/knhk/eval.h` - Add function pointer dispatch

**Build System:**
- `rust/knhk-aot/build.rs` - Build-time code generation

