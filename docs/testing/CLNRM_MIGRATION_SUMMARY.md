# CLNRM Migration Strategy - Executive Summary

**Date**: 2025-10-17
**Author**: CLNRM Migration Architect
**Status**: Ready for Implementation

---

## Overview

This document summarizes the comprehensive strategy for migrating ggen's Rust integration tests to declarative `.clnrm.toml` files with OpenTelemetry (OTEL) validation.

**Full Document**: [CLNRM_MIGRATION_STRATEGY.md](./CLNRM_MIGRATION_STRATEGY.md)

---

## Key Benefits

### 1. Fake-Green Detection (7 Layers)

Current Rust tests can pass without actually executing:
```rust
#[test]
fn test_search() {
    // ⚠️ Could pass even if search never runs!
    assert!(true);
}
```

CLNRM forces proof via OTEL spans:
```toml
[[expect.span]]
name = "ggen.marketplace.search"  # Must exist or test fails
attrs.all = { "query" = "rust", "result_count.gte" = 1 }
```

### 2. Reduced Complexity

- **Before**: 50-200 lines of Rust per test
- **After**: 30-80 lines of TOML per test
- **Savings**: 40-60% reduction in code

### 3. Self-Documenting

```toml
[meta]
description = "Verify marketplace search executes and returns results"

[[scenario]]
name = "search_rust_packages"
run = "ggen market search rust"

# Test proves it ran by checking for this span
[[expect.span]]
name = "ggen.marketplace.search"
```

Non-developers can understand test intent!

### 4. Production-Grade Observability

- **Spans** - Trace execution paths
- **Attributes** - Validate inputs/outputs
- **Events** - Detect critical operations
- **Hierarchy** - Ensure correct call graphs
- **Temporal** - Validate execution order

---

## Migration Scope

### Tests to Migrate (135 total)

| Category | Count | Priority | Phase |
|----------|-------|----------|-------|
| Integration tests | 42 | 🔴 HIGH | Week 4-5 |
| Security tests | 65 | 🟠 MED | Week 6 |
| Property tests | 28 | 🟡 LOW | Week 7 |
| **Unit tests** | **85** | **🟢 SKIP** | **Keep in Rust** |

**Rationale**:
- Integration tests provide most E2E value
- Security tests need hermeticity guarantees
- Property tests benefit from templating
- Unit tests stay in Rust (faster, simpler)

---

## 7-Layer OTEL Validation

### Layer 1: Span Existence
```toml
[[expect.span]]
name = "ggen.marketplace.search"  # Must exist
```

### Layer 2: Attribute Validation
```toml
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust", "result_count.gte" = 1 }
```

### Layer 3: Hierarchy Validation
```toml
[[expect.span]]
name = "ggen.registry.query"
parent = "ggen.marketplace.search"
```

### Layer 4: Event Emission
```toml
[[expect.span]]
name = "ggen.registry.query"
events.all = ["http.request", "http.response"]
```

### Layer 5: Status Validation
```toml
[expect.status]
all = "OK"  # All spans must succeed
```

### Layer 6: Temporal Ordering
```toml
[expect.temporal]
sequence = ["ggen.lifecycle.init", "ggen.lifecycle.build", "ggen.lifecycle.test"]
```

### Layer 7: Hermeticity
```toml
[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = { "service.name" = "ggen" }
```

---

## Implementation Timeline

```
Week 1: Analysis & Planning
├─ Test inventory and categorization
├─ OTEL span taxonomy design
└─ Migration priorities and templates

Week 2-3: Instrumentation
├─ Add OTEL dependencies
├─ Instrument core functions
└─ Validate span emission

Week 4-5: Convert Integration Tests (42 tests)
├─ Marketplace tests (15)
├─ Lifecycle tests (12)
└─ Registry API tests (15)

Week 6: Convert Security Tests (65 tests)
├─ Signature verification
├─ Input validation
└─ Injection prevention

Week 7: Convert Property Tests (28 tests)
├─ Search properties
├─ Version properties
└─ Serialization properties

Week 8: Validation & Cleanup
├─ Parallel execution validation
├─ CI/CD pipeline updates
└─ Documentation and cleanup
```

---

## Example Conversion

### Before (Rust - 75 lines)

```rust
#[tokio::test]
async fn test_get_popular_keywords() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();
    let keyword_sets = vec![
        vec!["rust", "cli"],
        vec!["rust", "web"],
        vec!["rust", "cli", "tool"],
        vec!["python", "web"],
        vec!["rust"],
    ];

    for (i, keywords) in keyword_sets.iter().enumerate() {
        // ... 50+ lines of setup code ...
    }

    let client = RegistryClient::with_base_url(base_url)?;
    let keywords = client.get_popular_keywords().await?;

    assert!(keywords.len() >= 3);
    assert_eq!(keywords[0].0, "rust");
    assert_eq!(keywords[0].1, 4);

    Ok(())
}
```

### After (CLNRM TOML - 45 lines)

```toml
[meta]
name = "registry_popular_keywords_test"
description = "Verify get_popular_keywords aggregates keyword frequencies correctly"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "setup_test_registry"
run = "cat > /tmp/test_registry.json <<'EOF' ... EOF"

[[scenario]]
name = "get_popular_keywords"
run = "cargo run -- registry keywords --file /tmp/test_registry.json"
depends_on = ["setup_test_registry"]

# OTEL Validation: Must generate these spans
[[expect.span]]
name = "ggen.registry.get_popular_keywords"
attrs.all = {
    "total_packages" = 5,
    "unique_keywords.gte" = 3,
    "keyword_count_rust" = 4
}

[[expect.span]]
name = "ggen.registry.aggregate_keywords"
parent = "ggen.registry.get_popular_keywords"

[expect.status]
all = "OK"

[expect.hermeticity]
no_external_services = true
```

**Benefits**:
- ✅ 60% less code (45 vs 75 lines)
- ✅ 7-layer OTEL validation
- ✅ Self-documenting
- ✅ Reproducible (container isolation)

---

## Quick Start

### 1. Install CLNRM

```bash
cargo install clnrm
```

### 2. Start OTEL Collector

```bash
docker run -d --name otel-collector \
  -p 4317:4317 -p 4318:4318 \
  otel/opentelemetry-collector-contrib:latest
```

### 3. Add OTEL to Ggen

```toml
# Cargo.toml
[dependencies]
tracing = "0.1"
tracing-opentelemetry = "0.21"
opentelemetry = "0.21"
opentelemetry-otlp = "0.14"
```

### 4. Instrument Function

```rust
use tracing::instrument;

#[instrument(name = "ggen.marketplace.search", skip(self))]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    // Span automatically created and emitted
    let results = self.registry.search(query).await?;
    Ok(results)
}
```

### 5. Create Test

```toml
# tests/clnrm/marketplace_search.clnrm.toml
[meta]
name = "marketplace_search_test"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "search_rust"
run = "cargo run -- market search rust"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust" }
```

### 6. Run Test

```bash
clnrm run tests/clnrm/marketplace_search.clnrm.toml
```

---

## Success Criteria

| Metric | Target | Status |
|--------|--------|--------|
| Integration tests converted | ≥42 | 🟡 In Progress |
| Security tests converted | ≥65 | 🟡 In Progress |
| Property tests converted | ≥28 | 🟡 In Progress |
| OTEL span coverage | 100% | 🟡 In Progress |
| Fake-green detection | 0 false positives | ⬜ Pending |
| Test execution time | <5 minutes | ⬜ Pending |
| CI/CD integration | Green builds | ⬜ Pending |

---

## Templates Library

### Basic CLI Command Test

```toml
[meta]
name = "{{ test_name }}"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "{{ scenario_name }}"
run = "{{ command }}"

[[expect.span]]
name = "{{ expected_span }}"
attrs.all = {{ expected_attributes }}
```

### Multi-Phase Test

```toml
{% for phase in phases %}
[[scenario]]
name = "{{ phase.name }}"
run = "{{ phase.command }}"
depends_on = {{ phase.depends_on }}

[[expect.span]]
name = "{{ phase.expected_span }}"
{% endfor %}

[expect.temporal]
sequence = [{% for phase in phases %}"{{ phase.expected_span }}"{% endfor %}]
```

### Security Test

```toml
# Valid case
[[scenario]]
name = "{{ test_name }}_valid"
run = "{{ valid_command }}"

[[expect.span]]
status = "OK"

# Attack case
[[scenario]]
name = "{{ test_name }}_attack"
run = "{{ attack_command }}"
expect_exit_code = 1

[[expect.span]]
status = "ERROR"
events.all = ["security_violation"]
```

---

## Key Decisions

### ADR-001: Migrate Integration Tests Only

**Decision**: Keep unit tests in Rust, migrate integration/security/property tests to CLNRM.

**Rationale**:
- Unit tests are fast and simple in Rust
- Integration tests need OTEL validation
- Security tests need hermeticity
- Property tests benefit from templating

### ADR-002: Use Generic Container Plugin

**Decision**: Use Docker containers for all tests.

**Rationale**:
- Simplest integration
- Already used in CI/CD
- Reproducible environments

### ADR-003: Implement All 7 OTEL Layers

**Decision**: Use all 7 validation layers for critical tests.

**Rationale**:
- Comprehensive fake-green detection
- Production-grade observability
- Better debugging

---

## Resources

### Documentation

- **Full Strategy**: [CLNRM_MIGRATION_STRATEGY.md](./CLNRM_MIGRATION_STRATEGY.md)
- **CLNRM GitHub**: https://github.com/your-org/clnrm
- **OpenTelemetry Spec**: https://opentelemetry.io/docs/specs/otel/

### Internal Docs

- `docs/testing/clnrm-integration-analysis.md` - CLNRM analysis
- `docs/testing/otel_span_taxonomy.rs` - Span naming conventions
- `ggen-core/tests/README.md` - Test suite overview

### Tools

```bash
# Install clnrm
cargo install clnrm

# Install OTEL collector
docker pull otel/opentelemetry-collector-contrib

# Run tests
clnrm run tests/clnrm/*.clnrm.toml

# Debug spans
clnrm debug --test tests/clnrm/marketplace_search.clnrm.toml --show-spans
```

---

## Next Steps

1. ✅ Review migration strategy
2. ⬜ Approve architecture decisions
3. ⬜ Begin Phase 1: Analysis & Planning
4. ⬜ Set up OTEL infrastructure
5. ⬜ Start instrumentation

---

## Contact

**CLNRM Migration Architect**
**Date**: 2025-10-17
**Status**: Ready for Implementation

For questions or clarifications, see the full [CLNRM_MIGRATION_STRATEGY.md](./CLNRM_MIGRATION_STRATEGY.md) document.
