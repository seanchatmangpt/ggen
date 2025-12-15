# KNHK - Knowledge Hook System

**Version**: 0.4.0  
**Status**: Production Ready  
**Architecture**: ≤2ns Hot Path Knowledge Graph Query System

## Overview

KNHK (Knowledge Hook System) is a high-performance knowledge graph query system designed for enterprise-scale RDF data processing. The system achieves **≤2ns performance** (Chatman Constant) on critical path operations through SIMD-optimized C hot path with pure CONSTRUCT logic (zero timing overhead), safe Rust warm path for timing and orchestration, and Erlang cold path architecture.

## Quick Start

### Build

```bash
# Build C library
make lib

# Build CLI
cd rust/knhk-cli
cargo build --release

# Run tests
make test
```

### CLI Usage

```bash
# Initialize system
knhk boot init schema.ttl invariants.sparql

# Register connector
knhk connect register kafka-prod urn:knhk:schema:default kafka://localhost:9092/triples

# Define cover
knhk cover define "SELECT ?s ?p ?o WHERE { ?s ?p ?o }" "max_run_len 8"

# Admit delta
knhk admit delta delta.json

# Declare reflex
knhk reflex declare check-count ASK_SP 0xC0FFEE 0 8

# Create epoch
knhk epoch create epoch1 8 "reflex1,reflex2"

# Run pipeline
knhk pipeline run --connectors kafka-prod
```

## Architecture

### Three-Tier Architecture

1. **Hot Path (C)** - ≤2ns operations using SIMD (pure CONSTRUCT logic, no timing)
   - Structure-of-Arrays (SoA) layout
   - 64-byte alignment for SIMD
   - Branchless operations
   - Zero timing overhead
   - 19 query operations (ASK, COUNT, COMPARE, SELECT, CONSTRUCT8)
   - **Timing measured externally by Rust**

2. **Warm Path (Rust)** - Safe abstractions over hot path + timing
   - ETL Pipeline (Ingest → Transform → Load → Reflex → Emit)
   - Connector framework (Kafka, Salesforce)
   - Lockchain integration (Merkle-linked receipts)
   - OTEL observability
   - **External timing measurement** (cycle counters)

3. **Cold Path (Erlang)** - Complex queries and validation
   - SPARQL query execution
   - SHACL validation
   - Schema registry (knhk_sigma)
   - Invariant registry (knhk_q)

### Key Components

- **ETL Pipeline**: Ingest → Transform → Load → Reflex → Emit
- **Connectors**: Kafka, Salesforce (with circuit breaker pattern)
- **Lockchain**: Merkle-linked provenance storage (URDNA2015 + SHA-256)
- **OTEL Integration**: Spans, metrics, traces
- **CLI Tool**: 13 command modules, 20+ commands

## Features

### Core Features (80% Value)

✅ **Hot Path Operations** - 19 operations achieving ≤2ns  
✅ **ETL Pipeline** - Complete pipeline with guard enforcement  
✅ **Connector Framework** - Kafka, Salesforce with circuit breakers  
✅ **Lockchain** - Merkle-linked receipts with URDNA2015 + SHA-256  
✅ **CLI Tool** - Production-ready command-line interface  
✅ **OTEL Integration** - Observability and metrics  
✅ **Guard Constraints** - max_run_len ≤ 8, τ ≤ 2ns enforced  
✅ **Zero Timing Overhead** - C hot path contains pure CONSTRUCT logic only

### Performance

- **Hot Path**: ≤2ns (Chatman Constant) - pure CONSTRUCT logic only
- **Zero Timing Overhead**: C code contains no timing measurements
- **External Timing**: Rust framework measures performance externally
- **SoA Layout**: 64-byte alignment for SIMD operations
- **Branchless**: Constant-time execution on hot path

## Documentation

### 📚 Full Documentation Book

**Online**: [Read the full documentation book](https://seanchatmangpt.github.io/ggen/knhk/)  
**Local**: Build and serve locally with mdbook:

```bash
# Build book
make docs

# Serve locally (http://localhost:3000)
make docs-serve
```

### Essential Documentation (80% Value)

- **[CLI Guide](rust/knhk-cli/README.md)** - CLI usage and commands
- **[Architecture](docs/architecture.md)** - System architecture overview
- **[API Reference](docs/api.md)** - API documentation
- **[Release Notes](RELEASE_NOTES_v0.4.0.md)** - v0.4.0 release details

### Additional Documentation

- **[Implementation Guide](rust/knhk-cli/IMPLEMENTATION.md)** - CLI implementation details
- **[Definition of Done](VERSION_0.4.0_DEFINITION_OF_DONE.md)** - Release criteria
- **[Integration Guide](docs/integration.md)** - Integration examples
- **[Deployment Guide](docs/deployment.md)** - Deployment instructions

## Testing

```bash
# Run all tests
make test

# Run CLI tests
make test-cli-all

# Run integration tests
make test-gaps-v1
```

**Test Coverage**:
- 11 CLI noun tests (Chicago TDD)
- 12 integration/E2E tests
- Performance validation tests
- Guard violation tests

## Code Quality

✅ **Zero TODOs** in production code  
✅ **Zero unwrap()** calls in production paths  
✅ **Proper error handling** throughout  
✅ **Guard constraints** enforced at runtime  
✅ **Feature-gated** optional dependencies  

## 80/20 Focus Areas

### Critical Path (80% Value)
- Basic triple pattern matching (ASK, SELECT on single predicate)
- Simple property constraints (minCount, maxCount, unique)
- Datatype validation (basic type checks)
- Existence checks (ASK_SP, ASK_SPO)
- Count aggregations (COUNT_SP_GE, COUNT_SP_EQ)

### Deferred (20% Edge Cases)
- Complex JOINs across multiple predicates
- OPTIONAL patterns
- Transitive property paths
- Full OWL inference
- Complex SPARQL queries (multi-predicate, nested)

## Project Structure

```
vendors/knhk/
├── src/              # C hot path implementation
├── include/          # C headers
├── rust/             # Rust warm path crates
│   ├── knhk-cli/    # CLI tool
│   ├── knhk-etl/    # ETL pipeline
│   ├── knhk-connectors/  # Connector framework
│   ├── knhk-lockchain/   # Provenance lockchain
│   └── knhk-otel/   # OTEL integration
├── erlang/           # Erlang cold path
├── tests/            # Test suite
├── docs/             # Documentation
└── Makefile          # Build system
```

## Dependencies

### C
- Standard C library (no external dependencies)

### Rust
- `clap-noun-verb` - CLI framework
- `rdkafka` - Kafka integration (optional)
- `reqwest` - HTTP client (optional)
- `sha2` - SHA-256 hashing
- `serde_json` - JSON serialization

### Erlang
- Standard OTP libraries

## Contributing

Follow these principles:
- **80/20 Focus**: Prioritize critical path implementations
- **No Placeholders**: Real implementations only
- **Proper Error Handling**: Result<T, E> for all fallible operations
- **Guard Constraints**: Enforce max_run_len ≤ 8, τ ≤ 8
- **Test Verification**: All code must be tested

## License

[License information]

## Release Status

**Current Version**: v0.4.0  
**Release Date**: December 2024  
**Status**: Production Ready

See [RELEASE_NOTES_v0.4.0.md](RELEASE_NOTES_v0.4.0.md) for full release details.

---

**80/20 Philosophy**: Focus on the critical 20% that delivers 80% of value.
