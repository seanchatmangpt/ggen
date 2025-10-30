# Ggen-Marketplace Architecture Documentation

This directory contains comprehensive architecture documentation for the ggen-marketplace library.

## Documents

1. **[ARCHITECTURE.md](./ARCHITECTURE.md)** - Complete system architecture overview
   - Design principles
   - Architecture diagrams (Mermaid)
   - Module structure
   - Core components
   - Data flow diagrams
   - Type system design
   - Error handling strategy
   - Performance considerations
   - Feature flags
   - Plugin system
   - Testing strategy
   - Security and scalability

2. **[TRAITS.md](./TRAITS.md)** - Core trait definitions and documentation
   - Registry trait (package metadata)
   - PackageStore trait (binary storage)
   - SearchEngine trait (package discovery)
   - CryptoVerifier trait (security)
   - MetricsCollector trait (observability)
   - Replication trait (distributed consensus)
   - Usage examples for each trait
   - Custom implementation guide

3. **[INTEGRATION.md](./INTEGRATION.md)** - Integration guide for projects
   - Basic integration steps
   - Ggen CLI integration (complete example)
   - Clnrm integration (validators and cleanroom)
   - Custom backend implementations
   - Testing integration
   - Performance optimization tips
   - Security best practices
   - Troubleshooting guide

4. **[PERFORMANCE.md](./PERFORMANCE.md)** - Performance optimization guide
   - Performance goals and metrics
   - Async/await optimization
   - Memory management (zero-copy, pooling)
   - Database performance (connection pooling, query optimization)
   - Storage optimization (streaming, compression)
   - Search performance (indexing, caching)
   - Network optimization (HTTP/2, batching)
   - Benchmarking with Criterion
   - Profiling (CPU and memory)
   - Horizontal and vertical scaling

## Quick Navigation

- **Getting Started**: See [../README.md](../README.md) for basic usage
- **Examples**: See [../../examples/](../../examples/) for working code
- **Implementation**: See [../../src/](../../src/) for source code

## Architecture At A Glance

```
MarketplaceClient (Public API)
    ↓
Core Traits (Registry, PackageStore, SearchEngine, CryptoVerifier, MetricsCollector)
    ↓
Implementations (Local, Distributed, File, S3, Tantivy, Ed25519)
    ↓
Storage Backends (SQLite, PostgreSQL, S3, IPFS, Redis)
```

## Key Design Decisions

1. **Trait-Based**: Extensible, testable, composable
2. **Async-First**: High-performance concurrency with Tokio
3. **Zero-Copy**: Efficient memory management with `Bytes` and `Cow`
4. **Type-Safe**: Leverage Rust's type system for correctness
5. **Production-Ready**: Comprehensive error handling and observability

## Contributing

When updating architecture documentation:
1. Keep diagrams synchronized with code
2. Update examples to reflect API changes
3. Document ADRs (Architecture Decision Records) for major changes
4. Run `cargo doc` to verify doc tests
5. Update this index when adding new documents
