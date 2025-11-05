# KNHKS v0.4.0 Release Notes

**Release Date**: December 2024  
**Status**: Production Ready

## Overview

KNHKS v0.4.0 is a **production integration and testing release** focused on completing the CLI tool, implementing end-to-end integration, and adding real network integrations. This release represents the culmination of the 80/20 critical path development, delivering production-ready functionality.

## Key Features

### 🎯 Complete CLI Tool
- **13 command modules** with 20+ CLI commands
- **Noun-verb interface** based on CONVO.txt API
- **Proper error handling** with Result types
- **JSON-based persistence** for configuration and state
- **Guard validation** enforced at runtime

### 🔗 End-to-End Integration
- **Full pipeline integration**: Connector → ETL → Lockchain
- **Real lockchain integration** in ETL emit stage
- **Merkle-linked receipts** with URDNA2015 + SHA-256
- **Git-based storage** for receipt files

### 🌐 Network Integrations
- **HTTP client** (reqwest) with retry logic
- **Kafka producer** (rdkafka) with delivery confirmation
- **gRPC client** (HTTP gateway fallback)
- **OTEL exporters** (OTLP JSON serialization)

### ✅ Testing & Quality
- **23 tests total**: 11 CLI tests + 12 integration tests
- **Zero TODOs** in production code
- **Zero unwrap()** calls in production paths
- **Guard constraints** enforced (max_run_len ≤ 8, τ ≤ 8)

## CLI Commands

### Boot
- `knhks boot init <sigma> <q>` - Initialize Σ and Q registries

### Connect
- `knhks connect register <name> <schema> <source>` - Register connector
- `knhks connect list` - List all connectors

### Cover
- `knhks cover define <select> <shard>` - Define cover over O
- `knhks cover list` - List all covers

### Admit
- `knhks admit delta <file>` - Admit delta into O

### Reflex
- `knhks reflex declare <name> <op> <pred> <off> <len>` - Declare reflex
- `knhks reflex list` - List all reflexes

### Epoch
- `knhks epoch create <id> <tau> <lambda>` - Create epoch
- `knhks epoch run <id>` - Execute epoch
- `knhks epoch list` - List epochs

### Route
- `knhks route install <name> <kind> <target>` - Install route
- `knhks route list` - List routes

### Receipt
- `knhks receipt get <id>` - Get receipt by ID
- `knhks receipt merge <ids>` - Merge receipts
- `knhks receipt list` - List receipts

### Pipeline
- `knhks pipeline run [--connectors] [--schema]` - Execute ETL pipeline
- `knhks pipeline status` - Show pipeline status

### Metrics
- `knhks metrics get` - Get OTEL metrics

### Coverage
- `knhks coverage get` - Get 80/20 coverage metrics

## Installation

```bash
# Build CLI
cd rust/knhks-cli
cargo build --release

# Install
cargo install --path .
```

## Verification

All tests pass:
```bash
make test-cli-all
make test
```

## Breaking Changes

None - this is the first production release with CLI.

## Migration Guide

No migration needed - fresh installation.

## Performance

- **Hot path operations**: ≤8 ticks (Chatman Constant)
- **Receipt generation**: Separated from hot path timing
- **Network operations**: Retry logic with exponential backoff

## Documentation

- CLI README: `rust/knhks-cli/README.md`
- Implementation guide: `rust/knhks-cli/IMPLEMENTATION.md`
- Definition of Done: `VERSION_0.4.0_DEFINITION_OF_DONE.md`

## Contributors

- Core team implementation following 80/20 principles
- Production-ready code standards enforced

## Next Steps

- v0.5.0: Advanced features (multi-shard, replication)
- v1.0.0: Full production release

---

**Release Manager**: Automated Release  
**Sign-Off**: Ready for Production
