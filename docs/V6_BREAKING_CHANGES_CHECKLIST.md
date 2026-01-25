<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [v6.0.0 Breaking Changes Implementation Checklist](#v600-breaking-changes-implementation-checklist)
  - [Quick Reference](#quick-reference)
  - [🔴 CRITICAL PRIORITY (Must fix for v6.0.0)](#-critical-priority-must-fix-for-v600)
    - [1. SPARQL Injection Prevention](#1-sparql-injection-prevention)
    - [2. SafePath Type Enforcement](#2-safepath-type-enforcement)
    - [3. Rate Limiting Implementation](#3-rate-limiting-implementation)
    - [4. Resource Limits](#4-resource-limits)
    - [5. Timeout Enforcement](#5-timeout-enforcement)
  - [🟡 HIGH PRIORITY (Should fix for v6.0.0)](#-high-priority-should-fix-for-v600)
    - [6. OpenTelemetry Observability](#6-opentelemetry-observability)
    - [7. Rich Error Types](#7-rich-error-types)
  - [🟢 MEDIUM PRIORITY (Can defer to v6.1.0)](#-medium-priority-can-defer-to-v610)
    - [8. Circuit Breakers & Graceful Degradation](#8-circuit-breakers--graceful-degradation)
  - [Implementation Timeline](#implementation-timeline)
    - [Week 1 (Jan 27-31)](#week-1-jan-27-31)
    - [Week 2 (Feb 3-7)](#week-2-feb-3-7)
    - [Week 3 (Feb 10-14)](#week-3-feb-10-14)
    - [Week 4 (Feb 17-21)](#week-4-feb-17-21)
    - [Week 5 (Feb 24-28)](#week-5-feb-24-28)
  - [Testing Strategy](#testing-strategy)
    - [Unit Tests](#unit-tests)
    - [Integration Tests](#integration-tests)
    - [Security Tests](#security-tests)
    - [Performance Tests](#performance-tests)
  - [Documentation Requirements](#documentation-requirements)
  - [Dependencies to Add](#dependencies-to-add)
  - [Breaking API Changes Summary](#breaking-api-changes-summary)
  - [Risk Assessment](#risk-assessment)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# v6.0.0 Breaking Changes Implementation Checklist

**Created**: 2026-01-24
**Target Release**: v6.0.0
**Status**: Planning Phase

## Quick Reference

This checklist tracks implementation of the 8 critical breaking changes identified in the production readiness analysis.

---

## 🔴 CRITICAL PRIORITY (Must fix for v6.0.0)

### 1. SPARQL Injection Prevention

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-core/src/rdf/query.rs` (line 193)
- [ ] `crates/ggen-core/src/graph/update.rs` (lines 115, 127, 139)
- [ ] `crates/ggen-ontology-core/src/triple_store.rs`
- [ ] `crates/ggen-core/src/graph/construct.rs`

**New Files to Create**:
- [ ] `crates/ggen-core/src/rdf/query_builder.rs`
- [ ] `crates/ggen-core/src/rdf/sparql_safe.rs`

**Tests Required**:
- [ ] Test injection prevention with malicious inputs
- [ ] Test parameterized query construction
- [ ] Test IRI validation
- [ ] Migration guide validation

**API Changes**:
```rust
// OLD (REMOVE)
fn query(&self, query: &str) -> Result<String>

// NEW (ADD)
fn execute(&self, query: SparqlQuery) -> Result<String>
```

**Estimated Effort**: 2-3 days

---

### 2. SafePath Type Enforcement

**Status**: ❌ Not Started

**Files to Modify** (High Priority):
- [ ] `crates/ggen-core/src/graph/core.rs`
- [ ] `crates/ggen-core/src/templates/generator.rs`
- [ ] `crates/ggen-core/src/v6/pipeline.rs`
- [ ] `crates/ggen-core/src/manifest/parser.rs`
- [ ] `crates/ggen-cli/src/cmds/sync.rs`

**Files to Create**:
- [ ] `crates/ggen-core/src/security/safe_path.rs`

**Tests Required**:
- [ ] Test path traversal prevention (`../../../etc/passwd`)
- [ ] Test null byte injection
- [ ] Test symlink attack prevention
- [ ] Test base directory escaping

**API Changes**:
```rust
// OLD (REMOVE)
fn load_turtle(&self, path: &Path) -> Result<()>

// NEW (ADD)
fn load_turtle(&self, path: &SafePath) -> Result<()>
```

**Estimated Effort**: 3-4 days (widespread changes)

---

### 3. Rate Limiting Implementation

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-api/src/middleware/rate_limit.rs` (currently TODOs)

**New Dependencies**:
- [ ] Add `governor = "0.6"` to Cargo.toml
- [ ] Add rate limiting config to ggen.toml schema

**Tests Required**:
- [ ] Test per-IP rate limiting
- [ ] Test per-endpoint rate limiting
- [ ] Test global rate limiting
- [ ] Test 429 responses

**Configuration Schema**:
```toml
[security.rate_limiting]
enabled = true
requests_per_ip_per_minute = 60
global_requests_per_second = 1000
```

**Estimated Effort**: 1-2 days

---

### 4. Resource Limits

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-core/src/graph/core.rs`
- [ ] `crates/ggen-ontology-core/src/triple_store.rs`
- [ ] `crates/ggen-core/src/v6/pipeline.rs`
- [ ] `crates/ggen-config/src/schema.rs`

**New Files to Create**:
- [ ] `crates/ggen-core/src/resources/limits.rs`
- [ ] `crates/ggen-core/src/resources/tracking.rs`

**Limits to Enforce**:
- [ ] Max RDF triples: 1,000,000
- [ ] Max file size: 100MB
- [ ] Max SPARQL query time: 5s
- [ ] Max memory usage: 500MB
- [ ] Max output files: 10,000

**Tests Required**:
- [ ] Test file size limit rejection
- [ ] Test triple count limit
- [ ] Test query timeout
- [ ] Test memory limit (integration test)

**Estimated Effort**: 2-3 days

---

### 5. Timeout Enforcement

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-core/src/v6/pipeline.rs`
- [ ] `crates/ggen-core/src/v6/pass.rs`
- [ ] `crates/ggen-core/src/v6/passes/normalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/extraction.rs`
- [ ] `crates/ggen-core/src/v6/passes/emission.rs`
- [ ] `crates/ggen-core/src/v6/passes/canonicalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/receipt_gen.rs`

**New Dependencies**:
- [ ] Ensure tokio timeout utilities used
- [ ] Add tokio-util for CancellationToken

**Configuration**:
```toml
[pipeline.timeouts]
per_pass_ms = 10000
total_pipeline_ms = 60000
```

**Tests Required**:
- [ ] Test per-pass timeout
- [ ] Test total pipeline timeout
- [ ] Test cancellation token
- [ ] Test timeout error messages

**Estimated Effort**: 2 days

---

## 🟡 HIGH PRIORITY (Should fix for v6.0.0)

### 6. OpenTelemetry Observability

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-core/src/v6/passes/normalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/extraction.rs`
- [ ] `crates/ggen-core/src/v6/passes/emission.rs`
- [ ] `crates/ggen-core/src/v6/passes/canonicalization.rs`
- [ ] `crates/ggen-core/src/v6/passes/receipt_gen.rs`

**New Dependencies**:
- [ ] `opentelemetry = "0.21"`
- [ ] `opentelemetry-otlp = "0.14"`
- [ ] `tracing-opentelemetry = "0.22"`

**Metrics to Add**:
- [ ] Pass execution count
- [ ] Pass duration (histogram)
- [ ] SPARQL query count
- [ ] RDF triple count
- [ ] Error count by type

**Estimated Effort**: 2-3 days

---

### 7. Rich Error Types

**Status**: ❌ Not Started

**Files to Modify**:
- [ ] `crates/ggen-utils/src/error.rs`
- [ ] All error creation sites (widespread)

**New Error Types**:
- [ ] `Error::SparqlQuery` with location + context
- [ ] `Error::RdfParse` with file/line/column
- [ ] `Error::Template` with template name + line

**Features to Add**:
- [ ] Source code snippets in errors
- [ ] Backtrace capture
- [ ] User-friendly suggestions

**Estimated Effort**: 3-4 days (widespread changes)

---

## 🟢 MEDIUM PRIORITY (Can defer to v6.1.0)

### 8. Circuit Breakers & Graceful Degradation

**Status**: ❌ Not Started

**New Files to Create**:
- [ ] `crates/ggen-core/src/resilience/circuit_breaker.rs`
- [ ] `crates/ggen-core/src/resilience/retry.rs`
- [ ] `crates/ggen-core/src/resilience/fallback.rs`

**Features**:
- [ ] Circuit breaker for external services
- [ ] Exponential backoff retry
- [ ] Fallback strategies

**Estimated Effort**: 2-3 days

---

## Implementation Timeline

### Week 1 (Jan 27-31)
- [ ] #1: SPARQL injection prevention
- [ ] #2: SafePath enforcement (start)

### Week 2 (Feb 3-7)
- [ ] #2: SafePath enforcement (complete)
- [ ] #3: Rate limiting
- [ ] #4: Resource limits

### Week 3 (Feb 10-14)
- [ ] #5: Timeout enforcement
- [ ] #6: OpenTelemetry (start)

### Week 4 (Feb 17-21)
- [ ] #6: OpenTelemetry (complete)
- [ ] #7: Rich error types
- [ ] Integration testing

### Week 5 (Feb 24-28)
- [ ] Documentation
- [ ] Migration guide
- [ ] Release prep

**Target v6.0.0 Release**: March 1, 2026

---

## Testing Strategy

### Unit Tests
- [ ] All new security features have unit tests
- [ ] All resource limits have unit tests
- [ ] All timeout logic has unit tests

### Integration Tests
- [ ] End-to-end security tests
- [ ] Load testing with resource limits
- [ ] Timeout behavior under load

### Security Tests
- [ ] SPARQL injection test suite
- [ ] Path traversal test suite
- [ ] DoS protection tests
- [ ] Fuzzing tests for input validation

### Performance Tests
- [ ] Benchmark overhead of SafePath
- [ ] Benchmark overhead of rate limiting
- [ ] Benchmark overhead of tracing

---

## Documentation Requirements

- [ ] Migration guide from v5 to v6
- [ ] Security best practices guide
- [ ] Resource tuning guide
- [ ] Observability setup guide
- [ ] Error handling guide
- [ ] API documentation updates

---

## Dependencies to Add

```toml
[dependencies]
# Security
governor = "0.6"           # Rate limiting

# Observability
opentelemetry = "0.21"
opentelemetry-otlp = "0.14"
tracing-opentelemetry = "0.22"

# Resilience
tokio-util = "0.7"         # CancellationToken
```

---

## Breaking API Changes Summary

| Old API | New API | Impact |
|---------|---------|--------|
| `Graph::load_turtle(&Path)` | `Graph::load_turtle(&SafePath)` | All file loading |
| `store.query(String)` | `store.execute(SparqlQuery)` | All SPARQL usage |
| `Graph::new()` | `Graph::with_limits(GraphLimits)` | Graph construction |
| No rate limiting | Required in config | API deployments |
| No timeouts | Required timeouts | Long operations |

---

## Risk Assessment

| Change | Risk Level | Mitigation |
|--------|-----------|------------|
| SPARQL builders | HIGH | Comprehensive migration guide + helper scripts |
| SafePath | HIGH | Gradual rollout with deprecation warnings |
| Rate limiting | LOW | Configurable, can disable |
| Resource limits | MEDIUM | Sensible defaults + override options |
| Timeouts | MEDIUM | Configurable + clear error messages |

---

**Next Steps**:
1. Review this checklist with team
2. Set up project board to track progress
3. Begin implementation starting with #1 (SPARQL injection)
4. Update CLAUDE.md with v6 security requirements
