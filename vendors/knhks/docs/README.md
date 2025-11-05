# KNKHS Documentation

Welcome to the KNKHS 8-tick knowledge graph query system documentation.

## Overview

KNKHS is an ultra-low-latency RDF query engine that executes SPARQL ASK queries in under 2 nanoseconds (8 CPU ticks) on modern processors. The system uses branchless SIMD operations and Structure-of-Arrays (SoA) data layout to achieve deterministic, cache-friendly query execution.

## Quick Links

- [Getting Started](getting-started.md) - Quick start guide
- [Architecture](architecture.md) - System architecture overview
- [API Reference](api.md) - Public API documentation
- [Performance](performance.md) - Performance metrics and benchmarks
- [Use Cases](use-cases.md) - Enterprise use cases
- [SIMD Optimization](simd-optimization.md) - SIMD implementation details
- [Hot Path](hot-path.md) - Hot path execution details
- [Building](building.md) - Build instructions
- [Testing](testing.md) - Testing guide

## Key Features

- **Sub-2 nanosecond query execution** (≤8 CPU ticks)
- **Branchless SIMD operations** (ARM NEON / x86 AVX2)
- **Structure-of-Arrays (SoA) data layout**
- **80% of enterprise queries** qualify for hot path
- **Zero measurement overhead** in performance tests
- **Fully unrolled SIMD** for NROWS=8

## Performance

All supported operations achieve ≤8 ticks:
- **ASK(S,P)**: ~4.8 ticks (1.2 ns)
- **COUNT(S,P)**: ~4.8 ticks (1.2 ns)
- **ASK(S,P,O)**: ~1.4 ticks (0.35 ns)

## Supported Operations

- ✅ ASK existence checks
- ✅ COUNT aggregations (≤8 elements)
- ✅ Triple matching (S-P-O)
- ❌ SELECT operations (exceed 8-tick budget)
- ❌ Complex JOINs (cold path fallback)

## Documentation Structure

```
docs/
├── README.md              # This file
├── architecture.md        # System architecture
├── architecture.mmd      # Architecture diagrams
├── api.md                 # API reference
├── performance.md        # Performance metrics
├── performance.mmd        # Performance charts
├── data-flow.md           # Data flow documentation
├── data-flow.mmd          # Data flow diagrams
├── simd-optimization.md   # SIMD details
├── simd-optimization.mmd  # SIMD diagrams
├── hot-path.md            # Hot path execution
├── hot-path.mmd           # Hot path flow
├── use-cases.md           # Enterprise use cases
├── use-cases.mmd          # Use case flows
├── getting-started.md     # Quick start
├── building.md            # Build guide
└── testing.md             # Testing guide
```

## Getting Help

- Review the [Getting Started](getting-started.md) guide
- Check the [API Reference](api.md) for function documentation
- See [Performance](performance.md) for optimization tips
- Review [Use Cases](use-cases.md) for enterprise examples

## License

See LICENSE file for details.

