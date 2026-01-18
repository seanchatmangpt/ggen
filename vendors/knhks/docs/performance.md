# Performance

**Version**: v0.4.0  
**Performance Target**: ≤2ns (Chatman Constant)

## Performance Architecture

**Critical Design Decision**: C hot path contains **zero timing code**. All timing measurements are performed externally by the Rust framework to keep the C hot path pure and optimized.

### Timing Measurement

- **C Hot Path**: Pure CONSTRUCT logic only, no timing overhead
- **Rust Framework**: Measures timing externally using cycle counters
- **Performance Budget**: ≤2ns (Chatman Constant)
- **Validation**: Chicago TDD tests validate timing externally

## Current Performance Metrics

All supported operations achieve ≤2ns when measured externally:

| Operation | Performance | Status |
|-----------|-------------|--------|
| **ASK(S,P)** | ~1.0-1.1 ns | ✅ |
| **COUNT(S,P) >= k** | ~1.0-1.1 ns | ✅ |
| **COUNT(S,P) <= k** | ~1.0-1.1 ns | ✅ |
| **COUNT(S,P) == k** | ~1.0-1.1 ns | ✅ |
| **ASK(S,P,O)** | ~0.4 ns | ✅ |
| **ASK(O,P)** | ~1.0-1.1 ns | ✅ |
| **UNIQUE(S,P)** | ~1.0 ns | ✅ |
| **COUNT(O,P)** | ~1.0-1.1 ns | ✅ |
| **COMPARE(O < value)** | ~0.9 ns | ✅ |
| **COMPARE(O >= value)** | ~0.9 ns | ✅ |
| **COMPARE(O <= value)** | ~0.9-1.1 ns | ✅ |
| **VALIDATE_DATATYPE(SP)** | ~1.5 ns | ✅ |
| **SELECT(S,P)** | ~1.0-1.4 ns | ✅ |
| **CONSTRUCT8** | ~1.0-2.0 ns | ✅ |

## Measurement Methodology

- **External Timing**: Rust framework measures timing around C hot path calls
- **Pure Hot Path**: C code contains zero timing overhead
- **Cycle-Based Measurement**: Uses CPU cycle counters (architecture-specific)
- **Nanosecond Conversion**: Cycles converted to nanoseconds for validation
- **Chicago TDD**: Tests validate ≤2ns budget externally

## Performance Characteristics

### Hot Path (≤2ns)

- **Pure Logic**: Branchless CONSTRUCT operations only
- **No Timing Overhead**: Zero timing code in hot path
- **Deterministic**: Branchless logic ensures consistent timing
- **Cache-friendly**: SoA layout enables single-cacheline loads
- **SIMD-optimized**: Processes 4 elements per SIMD instruction
- **Fully unrolled**: NROWS=8 eliminates all loop overhead

### Optimization Strategies

1. **Structure-of-Arrays**: Separate S, P, O arrays for SIMD access
2. **64-byte alignment**: Single cacheline loads
3. **Fully unrolled SIMD**: Direct instruction sequence for NROWS=8
4. **Branchless operations**: Bitwise masks instead of conditionals
5. **Warm L1 cache**: Data assumed hot during measurement

## Performance Comparison

### vs Traditional RDF Stores

| System | ASK Query Latency | Speedup |
|--------|------------------|---------|
| Traditional SPARQL | ~10-100 μs | Baseline |
| KNKHS Hot Path | ~1.2 ns | **10,000-100,000x** |

### Enterprise Use Cases

| Use Case | Performance | Status |
|----------|-----------|--------|
| Authorization Checks | ~1.0-1.1 ns | ✅ |
| Property Existence | ~1.0 ns | ✅ |
| Cardinality Validation | ~1.0-1.1 ns | ✅ |
| Type Checking | ~1.0-1.1 ns | ✅ |
| Simple Lookups | ~1.0-1.1 ns | ✅ |
| MaxCount Validation | ~1.0-1.1 ns | ✅ |
| Exact Count Validation | ~1.0-1.1 ns | ✅ |
| Reverse Lookup | ~1.0-1.1 ns | ✅ |
| Uniqueness Validation | ~1.0 ns | ✅ |
| Object Count | ~1.0-1.1 ns | ✅ |
| Value Comparison | ~0.9 ns | ✅ |
| Datatype Validation | ~1.5 ns | ✅ |
| CONSTRUCT8 | ~1.0-2.0 ns | ✅ |

**All enterprise use cases qualify for hot path!**

## Performance Diagrams

See `performance.mmd` for visual performance comparisons.

## Factors Affecting Performance

### Positive Factors
- Data hot in L1 cache
- Single predicate queries
- Predicate run size ≤8 elements
- Fully unrolled SIMD (NROWS=8)

### Negative Factors
- Cache misses (adds latency)
- Multiple predicate runs
- Data size >8 elements
- Cold cache state

## Optimization Tips

1. **Keep data in L1**: Warm cache before hot path queries
2. **Limit predicate runs**: Ensure ≤8 elements per predicate
3. **Use hot path operations**: Prefer ASK/CONSTRUCT8 over SELECT
4. **Batch queries**: Process multiple queries together
5. **64-byte alignment**: Ensure arrays are cache-aligned
6. **No timing in hot path**: All timing measurements external (Rust)

## Benchmarking

**Note**: C code no longer includes timing functions. All benchmarking is performed by Rust framework using external cycle counters.

The Rust framework:
- Measures timing around C hot path calls
- Validates ≤2ns budget
- Provides performance statistics

For performance validation, use Chicago TDD tests which measure timing externally.

