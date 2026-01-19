# KNHK v0.5.0 Product Requirements Document (PRD)

**Version**: 0.5.0  
**Status**: Planning  
**Target Release**: Q1 2025  
**Document Owner**: Product Team  
**Last Updated**: December 2024

## Executive Summary

KNHK v0.5.0 focuses on **production hardening, performance optimization, and advanced features** to address known limitations from v0.4.0 and expand the system's capabilities for enterprise deployment. This release prioritizes moving CONSTRUCT8 to warm path, completing configuration management, and enhancing observability.

## Background

### v0.4.0 Achievements
- ✅ CLI tool complete (25/25 commands)
- ✅ Network integrations (HTTP, Kafka, gRPC, OTEL)
- ✅ ETL pipeline operational
- ✅ Guard validation enforced
- ✅ Production-ready for hot path queries

### v0.4.0 Limitations Addressed
- ⚠️ CONSTRUCT8 exceeds 8-tick budget (41-83 ticks)
- ⚠️ Configuration management incomplete
- ⚠️ CLI documentation pending
- ⚠️ Examples directory missing

## Goals & Objectives

### Primary Goals
1. **Production Hardening** - Address CONSTRUCT8 tick budget violation
2. **Configuration Management** - Complete TOML-based configuration system
3. **Documentation** - Complete CLI documentation and examples
4. **Performance** - Optimize warm path operations

### Success Criteria
- CONSTRUCT8 moved to warm path OR optimized to ≤8 ticks
- Configuration management fully implemented
- CLI documentation complete with examples
- 100% test coverage on critical paths
- Production deployment guide complete

## Target Users

### Primary Users
- **Developers** - CLI users, integration developers
- **DevOps** - Deployment and configuration management
- **Architects** - System design and integration patterns

### Use Cases
1. **Production Deployment** - Enterprise deployment with full configuration
2. **Warm Path Operations** - CONSTRUCT8 and other emit operations
3. **Integration** - Third-party system integration with examples
4. **Monitoring** - Enhanced observability and metrics

## Features & Requirements

### Phase 1: CONSTRUCT8 Warm Path Migration (CRITICAL)

#### Requirement 1.1: Move CONSTRUCT8 to Warm Path
**Priority**: P0 (Critical)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Move CONSTRUCT8 operations from hot path (8-tick budget) to warm path (≤500ms budget) since CONSTRUCT8 performs emit work (SIMD loads, blending, stores) which inherently exceeds 8 ticks.

**Acceptance Criteria**:
- [ ] CONSTRUCT8 operations execute in warm path (<500ms)
- [ ] Hot path operations remain ≤8 ticks (no regressions)
- [ ] Clear separation between hot path (query) and warm path (emit)
- [ ] Tests updated to reflect warm path timing
- [ ] Documentation updated with warm path vs hot path distinction

**Technical Approach**:
- Create `knhk_warm_path` module for warm path operations
- Implement CONSTRUCT8 in warm path with ≤500ms budget
- Update CLI and API to route CONSTRUCT8 to warm path
- Add warm path metrics and observability

**Success Metrics**:
- CONSTRUCT8 operations complete in <500ms (p95)
- Hot path operations remain ≤8 ticks (100%)
- Zero hot path regressions

---

### Phase 2: Configuration Management (HIGH PRIORITY)

#### Requirement 2.1: TOML Configuration System
**Priority**: P1 (High)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Implement comprehensive configuration management system using TOML format with support for all CLI commands, connectors, epochs, hooks, and routes.

**Acceptance Criteria**:
- [ ] `~/.knhk/config.toml` (or `%APPDATA%/knhk/config.toml` on Windows) support
- [ ] Environment variable override support
- [ ] Configuration validation and error reporting
- [ ] CLI commands respect configuration file
- [ ] Default configuration with sensible defaults
- [ ] Configuration schema documentation

**Configuration Structure**:
```toml
[knhk]
version = "0.5.0"
context = "default"

[connectors]
[knhk.connectors.kafka-prod]
type = "kafka"
bootstrap_servers = ["localhost:9092"]
topic = "triples"
schema = "urn:knhk:schema:enterprise"
max_run_len = 8
max_batch_size = 1000

[epochs]
[knhk.epochs.default]
tau = 8
ordering = "deterministic"

[hooks]
max_count = 100

[routes]
[knhk.routes.webhook-1]
kind = "webhook"
target = "https://api.example.com/webhook"
encode = "json-ld"
```

**Technical Approach**:
- Create `knhk-config` crate with TOML parsing
- Implement configuration loading hierarchy (env > file > defaults)
- Add configuration validation
- Update CLI commands to use configuration

**Success Metrics**:
- Configuration file loads successfully (100%)
- Environment variables override file (100%)
- Configuration validation errors are clear and actionable

---

#### Requirement 2.2: Environment Variable Support
**Priority**: P1 (High)  
**Effort**: Low  
**Dependencies**: 2.1

**Description**:
Support environment variable overrides for all configuration options with `KNHK_` prefix.

**Acceptance Criteria**:
- [ ] `KNHK_CONTEXT` - Set default context
- [ ] `KNHK_CONNECTOR_*` - Connector configuration
- [ ] `KNHK_EPOCH_*` - Epoch configuration
- [ ] Environment variables override config file
- [ ] Documentation for all environment variables

**Examples**:
```bash
export KNHK_CONTEXT=production
export KNHK_CONNECTOR_KAFKA_BOOTSTRAP_SERVERS=localhost:9092
export KNHK_EPOCH_DEFAULT_TAU=8
```

---

### Phase 3: Documentation & Examples (MEDIUM PRIORITY)

#### Requirement 3.1: CLI Documentation
**Priority**: P2 (Medium)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Create comprehensive CLI documentation with examples for all commands.

**Acceptance Criteria**:
- [ ] `docs/cli.md` with all commands documented
- [ ] Examples for each command
- [ ] Configuration guide
- [ ] Troubleshooting guide
- [ ] Command reference table

**Content Structure**:
- Overview
- Installation
- Configuration
- Command Reference (all 25 commands)
- Examples
- Troubleshooting

---

#### Requirement 3.2: Examples Directory
**Priority**: P2 (Medium)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Create `examples/` directory with working examples for common use cases.

**Acceptance Criteria**:
- [ ] `examples/basic-hook/` - Basic hook execution
- [ ] `examples/kafka-connector/` - Kafka connector setup
- [ ] `examples/etl-pipeline/` - Full ETL pipeline
- [ ] `examples/receipt-verification/` - Receipt verification
- [ ] `examples/cli-usage/` - CLI usage examples
- [ ] All examples are runnable and documented

**Example Structure**:
```
examples/
├── README.md
├── basic-hook/
│   ├── README.md
│   ├── hook.ttl
│   └── run.sh
├── kafka-connector/
│   ├── README.md
│   ├── config.toml
│   └── setup.sh
└── etl-pipeline/
    ├── README.md
    ├── pipeline-config.toml
    └── run.sh
```

---

### Phase 4: Performance & Observability (MEDIUM PRIORITY)

#### Requirement 4.1: Warm Path Performance Optimization
**Priority**: P2 (Medium)  
**Effort**: High  
**Dependencies**: 1.1

**Description**:
Optimize warm path operations (CONSTRUCT8, complex queries) to achieve <500ms latency (p95).

**Acceptance Criteria**:
- [ ] CONSTRUCT8 operations complete in <500ms (p95)
- [ ] Warm path metrics tracked (latency, throughput)
- [ ] Performance benchmarks documented
- [ ] Optimization opportunities identified and implemented

**Technical Approach**:
- Profile warm path operations
- Optimize SIMD operations where possible
- Implement connection pooling for network operations
- Add caching for frequently accessed data

---

#### Requirement 4.2: Enhanced Observability
**Priority**: P2 (Medium)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Enhance OTEL integration with more metrics, traces, and logs.

**Acceptance Criteria**:
- [ ] Warm path metrics (latency, throughput)
- [ ] Configuration metrics (loaded configs, validation errors)
- [ ] Enhanced traces for warm path operations
- [ ] Structured logging with OTEL correlation
- [ ] Metrics dashboard configuration

**Metrics to Add**:
- `knhk.warm_path.operations.count` - Warm path operation count
- `knhk.warm_path.operations.latency` - Warm path latency histogram
- `knhk.config.loads` - Configuration load count
- `knhk.config.errors` - Configuration error count

---

### Phase 5: Enhanced RDF Parsing (LOW PRIORITY)

#### Requirement 5.1: Complete RDF/Turtle Parser
**Priority**: P3 (Low)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Enhance RDF/Turtle parser to support full Turtle syntax, prefixes, blank nodes, and base URIs.

**Acceptance Criteria**:
- [ ] Full Turtle syntax support
- [ ] Prefix resolution
- [ ] Blank node handling
- [ ] Base URI resolution
- [ ] Error reporting with line numbers
- [ ] Performance: <100ms for typical files

**Technical Approach**:
- Use or enhance existing RDF parser (raptor2 or Rust crate)
- Add comprehensive error handling
- Implement prefix and base URI resolution
- Add performance optimizations

---

#### Requirement 5.2: JSON-LD Parser
**Priority**: P3 (Low)  
**Effort**: Medium  
**Dependencies**: None

**Description**:
Implement JSON-LD parser with expansion, context resolution, and frame support.

**Acceptance Criteria**:
- [ ] JSON-LD expansion
- [ ] Context resolution
- [ ] Frame support
- [ ] Error handling
- [ ] Performance: <200ms for typical documents

---

## Technical Architecture

### Warm Path Architecture

```
┌─────────────────┐
│   Hot Path      │  ≤8 ticks (query operations)
│   (v0.4.0)      │
└─────────────────┘
         │
         │ CONSTRUCT8
         ▼
┌─────────────────┐
│   Warm Path     │  ≤500ms (emit operations)
│   (v0.5.0)      │
└─────────────────┘
         │
         │ Network/Storage
         ▼
┌─────────────────┐
│   Cold Path     │  >500ms (complex operations)
│   (Future)      │
└─────────────────┘
```

### Configuration System Architecture

```
Environment Variables (highest priority)
         │
         ▼
Config File (~/.knhk/config.toml)
         │
         ▼
Default Configuration (lowest priority)
         │
         ▼
Runtime Configuration
```

## Dependencies

### External Dependencies
- `toml` crate - TOML parsing
- `serde` - Serialization/deserialization
- `opentelemetry` - Enhanced observability
- `json-ld` crate (optional) - JSON-LD parsing

### Internal Dependencies
- `knhk-hot` - Hot path operations
- `knhk-etl` - ETL pipeline
- `knhk-cli` - CLI tool
- `knhk-otel` - Observability

## Risks & Mitigations

### Risk 1: CONSTRUCT8 Warm Path Migration Complexity
**Risk**: Moving CONSTRUCT8 to warm path may require significant refactoring  
**Mitigation**: 
- Create clear abstraction layer between hot and warm paths
- Implement warm path module incrementally
- Maintain backward compatibility during migration

### Risk 2: Configuration System Complexity
**Risk**: Configuration system may become too complex  
**Mitigation**:
- Start with simple TOML parsing
- Use clear configuration schema
- Provide good defaults
- Comprehensive documentation

### Risk 3: Performance Degradation
**Risk**: Changes may introduce performance regressions  
**Mitigation**:
- Comprehensive performance testing
- Benchmark before and after changes
- Continuous performance monitoring

## Success Metrics

### Phase 1: CONSTRUCT8 Warm Path
- ✅ CONSTRUCT8 operations <500ms (p95)
- ✅ Hot path operations ≤8 ticks (100%)
- ✅ Zero hot path regressions

### Phase 2: Configuration Management
- ✅ Configuration file loads successfully (100%)
- ✅ Environment variables override file (100%)
- ✅ Configuration validation errors are clear

### Phase 3: Documentation
- ✅ CLI documentation complete
- ✅ Examples directory with 5+ examples
- ✅ All examples are runnable

### Phase 4: Performance
- ✅ Warm path operations <500ms (p95)
- ✅ Enhanced metrics and observability
- ✅ Performance benchmarks documented

## Timeline & Milestones

### Milestone 1: CONSTRUCT8 Warm Path (Weeks 1-2)
- Week 1: Design warm path architecture
- Week 2: Implement warm path module and migrate CONSTRUCT8

### Milestone 2: Configuration Management (Weeks 3-4)
- Week 3: Implement TOML configuration system
- Week 4: Add environment variable support and validation

### Milestone 3: Documentation & Examples (Weeks 5-6)
- Week 5: Complete CLI documentation
- Week 6: Create examples directory

### Milestone 4: Performance & Observability (Weeks 7-8)
- Week 7: Optimize warm path performance
- Week 8: Enhance observability and metrics

### Milestone 5: Testing & Release (Weeks 9-10)
- Week 9: Comprehensive testing
- Week 10: Release preparation and documentation

**Total Estimated Duration**: 10 weeks

## Out of Scope (Future Releases)

### v0.6.0+
- Multi-predicate queries
- Complex JOIN operations
- Full SPARQL compliance
- GPU batch evaluator
- Distributed lockchain
- Multi-shard support

## Appendix

### A. Configuration Schema

See `docs/configuration.md` for complete configuration schema.

### B. Warm Path API

See `docs/warm-path.md` for warm path API documentation.

### C. Examples

See `examples/README.md` for example usage.

### D. Performance Benchmarks

See `docs/performance.md` for performance benchmarks.

---

**Document Status**: Draft  
**Next Review**: After v0.4.0 release  
**Approval Required**: Product Owner, Engineering Lead

