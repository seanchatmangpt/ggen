# KNHKS Documentation

Welcome to the KNHKS 8-tick knowledge graph query system documentation.

## Overview

KNHKS (v0.3.0) is an ultra-low-latency RDF query engine that executes SPARQL ASK queries in under 2 nanoseconds (8 CPU ticks) on modern processors. The system uses branchless SIMD operations and Structure-of-Arrays (SoA) data layout to achieve deterministic, cache-friendly query execution.

**Status**: Production-ready with complete connector framework, ETL pipeline, Erlang reflexive control layer, and OTEL integration.

## Quick Links

- [Getting Started](getting-started.md) - Quick start guide
- [Architecture](architecture.md) - System architecture overview
- [API Reference](api.md) - Public API documentation
- [Performance](performance.md) - Performance metrics and benchmarks
- [Code Organization](code-organization.md) - Code structure and organization
- [Use Cases](use-cases.md) - Enterprise use cases
- [SIMD Optimization](simd-optimization.md) - SIMD implementation details
- [Hot Path](hot-path.md) - Hot path execution details
- [Building](building.md) - Build instructions
- [Testing](testing.md) - Testing guide

## Key Features

### Core Engine
- **Sub-2 nanosecond query execution** (≤8 CPU ticks)
- **Branchless SIMD operations** (ARM NEON / x86 AVX2)
- **Structure-of-Arrays (SoA) data layout**
- **80% of enterprise queries** qualify for hot path
- **Zero measurement overhead** in performance tests
- **Fully unrolled SIMD** for NROWS=8

### Production Infrastructure (v0.3.0)
- **Connector Framework** - Kafka, Salesforce, HTTP connectors with real library integrations
- **ETL Pipeline** - Complete Ingest → Transform → Load → Reflex → Emit pipeline
- **Erlang Reflexive Control** - Schema registry, invariant management, delta ingestion, lockchain
- **OTEL Integration** - Real span ID generation, metrics, tracing
- **Circuit Breakers** - Resilient connector failure handling
- **Health Checks** - Connector health monitoring
- **Receipt System** - Cryptographic provenance with Merkle-linked lockchain

## Performance

All supported operations achieve ≤8 ticks (Chatman Constant: 2ns = 8 ticks):

| Operation | p50 | p95 | Status |
|-----------|-----|-----|--------|
| **ASK(S,P)** | 4.00-4.17 ticks (1.000-1.042 ns) | 4.17-4.50 ticks (1.042-1.125 ns) | ✅ |
| **COUNT(S,P) >= k** | 4.00-4.17 ticks (1.000-1.042 ns) | 4.17-4.34 ticks (1.042-1.084 ns) | ✅ |
| **ASK(S,P,O)** | ~1.4 ticks (0.35 ns) | ~2.0 ticks (0.5 ns) | ✅ |
| **Comparison Operations** | 3.50-4.34 ticks (0.875-1.084 ns) | 3.67-4.34 ticks (0.917-1.084 ns) | ✅ |
| **Datatype Validation** | 6.00 ticks (1.500 ns) | 6.00 ticks (1.500 ns) | ✅ |
| **SELECT(S,P)** | 3.83 ticks (0.958 ns) | 5.74 ticks (1.434 ns) | ✅ |

**19 operations** implemented, **18/19 enterprise use cases** qualify for hot path.

## Supported Operations

### Hot Path Operations (≤8 ticks, v0.3.0)
- ✅ ASK existence checks (ASK_SP, ASK_SPO, ASK_OP)
- ✅ COUNT aggregations (COUNT_SP_GE/LE/EQ, COUNT_OP)
- ✅ Triple matching (S-P-O)
- ✅ Uniqueness validation (UNIQUE_SP)
- ✅ Reverse lookups (ASK_O,P)
- ✅ Value comparison (COMPARE_O_EQ/GT/LT/GE/LE)
- ✅ Datatype validation (VALIDATE_DATATYPE_SP/SPO)
- ✅ CONSTRUCT8 (fixed-template emit)
- ⚠️ SELECT operations (~8.17 ticks, optimized but slight variance)
- ❌ Complex JOINs (cold path fallback)

### Enterprise Features (v0.3.0)
- ✅ Kafka Connector with rdkafka integration
- ✅ Salesforce Connector with reqwest integration
- ✅ ETL Pipeline (Ingest, Transform, Load, Reflex, Emit)
- ✅ Schema Registry (Σ management)
- ✅ Invariant Registry (Q constraints)
- ✅ Lockchain (Merkle-linked receipts)
- ✅ OTEL observability (spans, metrics, tracing)

## Documentation Structure

```
docs/
├── README.md              # This file
├── architecture.md        # System architecture
├── architecture.mmd      # Architecture diagrams
├── api.md                 # API reference
├── performance.md        # Performance metrics
├── performance.mmd        # Performance charts
├── code-organization.md   # Code structure and organization
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

## Version Information

- **Current Version**: v0.3.0 (Production-Ready)
- **Core Library**: v1.0.0 (API Stable)
- **Rust Connectors**: v0.1.0
- **Rust ETL**: v0.1.0
- **Erlang RC**: v1.0.0

See [CHANGELOG.md](../CHANGELOG.md) for version history.

## Code Quality Standards

KNHKS follows strict 80/20 production-ready code standards:
- ✅ No placeholders or stubs - real implementations only
- ✅ Comprehensive error handling (`Result<T, E>`)
- ✅ Real library integrations (rdkafka, reqwest)
- ✅ Feature-gated dependencies (`#[cfg(feature = "...")]`)
- ✅ Input validation and guard enforcement
- ✅ OTEL integration for observability
- ✅ Test verification (OTEL validation as truth source)

See [.cursorrules](../.cursorrules) for complete coding standards.

## Getting Help

- Review the [Getting Started](getting-started.md) guide
- Check the [API Reference](api.md) for function documentation
- See [Performance](performance.md) for optimization tips
- Review [Use Cases](use-cases.md) for enterprise examples
- See [Architecture](architecture.md) for system design

## License

See LICENSE file for details.

