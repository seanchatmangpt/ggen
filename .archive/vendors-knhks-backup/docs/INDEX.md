# KNHK Documentation Index

**80/20 Principle**: This index prioritizes the 20% of documentation that provides 80% of value.

## Essential Documentation (Must Read)

### Getting Started
1. **[README.md](../README.md)** - Project overview and quick start
2. **[Quick Start](QUICK_START.md)** - 5-minute setup guide
3. **[CLI Guide](cli.md)** - Complete CLI command reference

### Core Concepts
4. **[Architecture](architecture.md)** - System architecture overview
5. **[API Reference](api.md)** - API documentation
6. **[Integration Guide](integration.md)** - Integration examples
7. **[Deployment Guide](deployment.md)** - Deployment instructions

### Release Information
8. **[Release Notes](../RELEASE_NOTES_v0.4.0.md)** - v0.4.0 release details
9. **[Changelog](../CHANGELOG.md)** - Complete version history
10. **[Definition of Done](../VERSION_0.4.0_DEFINITION_OF_DONE.md)** - Release criteria

## Detailed Documentation

### Implementation
- **[CLI Implementation](../rust/knhk-cli/IMPLEMENTATION.md)** - CLI implementation details
- **[Code Organization](code-organization.md)** - Code structure
- **[Data Flow](data-flow.md)** - Data flow diagrams

### Performance & Optimization
- **[Performance](performance.md)** - Performance characteristics
- **[80/20 Analysis](../docs/archived/analysis/80_20_ANALYSIS.md)** - Development strategy

### Integration
- **[Weaver Integration](weaver-integration.md)** - Weaver.ai integration
- **[Integration Guide](integration.md)** - Integration examples

## Archived Documentation

Historical and version-specific documentation has been archived:
- **Version Docs**: `docs/archived/versions/`
- **Analysis Docs**: `docs/archived/analysis/`
- **Status Docs**: `docs/archived/status/`

## Quick Reference

### CLI Commands
```bash
knhk boot init <sigma> <q>
knhk connect register <name> <schema> <source>
knhk cover define <select> <shard>
knhk reflex declare <name> <op> <pred> <off> <len>
knhk epoch create <id> <tau> <lambda>
knhk pipeline run [--connectors] [--schema]
```

### Key Concepts
- **Hot Path**: ≤2ns operations (C, SIMD, pure CONSTRUCT logic)
- **Warm Path**: Safe abstractions (Rust, handles timing)
- **Cold Path**: Complex queries (Erlang)
- **Lockchain**: Merkle-linked receipts
- **Guard Constraints**: max_run_len ≤ 8, τ ≤ 2ns
- **Timing**: Measured externally by Rust framework only

## Documentation Structure

```
docs/
├── INDEX.md              # This file
├── QUICK_START.md        # Quick start guide
├── cli.md                # CLI documentation
├── architecture.md       # Architecture overview
├── api.md                # API reference
├── integration.md        # Integration guide
├── deployment.md         # Deployment guide
├── performance.md        # Performance docs
├── code-organization.md  # Code structure
├── data-flow.md          # Data flow
├── weaver-integration.md # Weaver integration
└── archived/             # Historical docs
    ├── versions/         # Version-specific docs
    ├── analysis/          # Analysis docs
    └── status/           # Status reports
```

## Contribution Guidelines

When adding documentation:
1. **80/20 Focus**: Prioritize essential information
2. **No Redundancy**: Check existing docs first
3. **Clear Structure**: Follow existing patterns
4. **Update Index**: Add new docs to this index

---

**Last Updated**: v0.4.0  
**Maintained By**: Core Team
