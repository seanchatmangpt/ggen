# CLNRM Quick Reference Guide

**Audience**: Developers converting Rust tests to CLNRM TOML
**Last Updated**: 2025-10-17

---

## Table of Contents

1. [Basic Structure](#basic-structure)
2. [Common Patterns](#common-patterns)
3. [OTEL Assertions](#otel-assertions)
4. [Templates](#templates)
5. [Troubleshooting](#troubleshooting)
6. [CLI Commands](#cli-commands)

---

## Basic Structure

### Minimal Test

```toml
[meta]
name = "my_test"
version = "1.0.0"
description = "What this test proves"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "test_scenario"
service = "ggen"
run = "cargo run -- market search rust"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust" }

[expect.status]
all = "OK"
```

---

## Common Patterns

### Pattern 1: Simple CLI Command

```toml
[[scenario]]
name = "search_rust"
run = "ggen market search rust"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust", "result_count.gte" = 1 }
```

### Pattern 2: Multi-Step Test

```toml
# Step 1: Setup
[[scenario]]
name = "setup"
run = "ggen lifecycle init --name test-project"

[[expect.span]]
name = "ggen.lifecycle.init"
attrs.all = { "project_name" = "test-project" }

# Step 2: Build (depends on setup)
[[scenario]]
name = "build"
run = "ggen lifecycle build"
depends_on = ["setup"]

[[expect.span]]
name = "ggen.lifecycle.build"
parent = "ggen.lifecycle.init"
```

### Pattern 3: Error Test

```toml
[[scenario]]
name = "invalid_input"
run = "ggen market resolve nonexistent-package"
expect_exit_code = 1  # Expect failure

[[expect.span]]
name = "ggen.package.resolve"
status = "ERROR"
attrs.all = { "error.type" = "PackageNotFoundError" }
```

### Pattern 4: Performance Test

```toml
[[scenario]]
name = "fast_search"
run = "ggen market search rust"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = {
    "search_duration_ms.lte" = 5000  # Must complete in <5s
}
```

### Pattern 5: Security Test

```toml
[[scenario]]
name = "tampered_signature"
run = "ggen crypto verify --signature ${TAMPERED_SIG}"
expect_exit_code = 1

[[expect.span]]
name = "ggen.crypto.verify"
status = "ERROR"
attrs.all = { "tampering_detected" = true }
events.all = ["security_violation"]

[expect.hermeticity]
no_external_services = true
```

---

## OTEL Assertions

### Span Existence

```toml
[[expect.span]]
name = "ggen.marketplace.search"  # Span must exist
```

### Exact Attribute Match

```toml
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust", "result_count" = 5 }
```

### Attribute Comparisons

```toml
[[expect.span]]
attrs.all = {
    "result_count.gte" = 1,      # >=
    "result_count.lte" = 100,    # <=
    "search_duration_ms.lt" = 5000  # <
}
```

### Partial Attribute Match

```toml
[[expect.span]]
attrs.any = {
    "cache_hit" = true,
    "cache_miss" = true
}  # At least one must match
```

### String Contains

```toml
[[expect.span]]
attrs.all = {
    "error.message.contains" = "not found"
}
```

### Span Hierarchy

```toml
[[expect.span]]
name = "ggen.registry.query"
parent = "ggen.marketplace.search"  # Must be child of this span
child = "ggen.http.request"         # Must have this child
```

### Event Validation

```toml
[[expect.span]]
events.all = ["http.request", "http.response"]  # All must exist
events.any = ["cache.hit", "cache.miss"]        # At least one
```

### Status Validation

```toml
# Individual span status
[[expect.span]]
name = "ggen.marketplace.search"
status = "OK"  # or "ERROR"

# All spans status
[expect.status]
all = "OK"  # All spans must be OK
any = "ERROR"  # At least one span must be ERROR
```

### Temporal Ordering

```toml
[expect.temporal]
sequence = [
    "ggen.lifecycle.init",
    "ggen.lifecycle.build",
    "ggen.lifecycle.test"
]

# Alternative: explicit before/after
[[expect.temporal]]
before = "ggen.package.download"
after = "ggen.package.extract"
```

### Call Graph Validation

```toml
[expect.graph]
must_include = [
    ["ggen.cli.execute", "ggen.marketplace.search"],
    ["ggen.marketplace.search", "ggen.registry.query"]
]
acyclic = true  # No cycles allowed
```

### Hermeticity

```toml
[expect.hermeticity]
no_external_services = true  # No network calls
resource_attrs.must_match = {
    "service.name" = "ggen",
    "service.version" = "1.0.0"
}

# Or allow specific hosts
no_external_services = false
allowed_hosts = ["registry.ggen.io", "github.com"]
```

### Custom Assertions (Python)

```toml
[[expect.span]]
attrs.custom = """
assert attrs.result_count <= attrs.total_packages, \
       f"Results {attrs.result_count} exceed total {attrs.total_packages}"
assert attrs.search_duration_ms < 10000, \
       "Search too slow"
"""
```

---

## Templates

### Template with Variables

```toml
# Define variables
{% set queries = ["rust", "web", "cli"] %}

# Generate scenarios
{% for query in queries %}
[[scenario]]
name = "search_{{ query }}"
run = "ggen market search {{ query }}"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "{{ query }}" }
{% endfor %}
```

### Template with Matrix

```toml
{% set matrix = [
    {"query": "rust", "min_results": 10},
    {"query": "web", "min_results": 5},
    {"query": "cli", "min_results": 3}
] %}

{% for test in matrix %}
[[scenario]]
name = "search_{{ test.query }}"
run = "ggen market search {{ test.query }}"

[[expect.span]]
attrs.all = {
    "query" = "{{ test.query }}",
    "result_count.gte" = {{ test.min_results }}
}
{% endfor %}
```

### Conditional Logic

```toml
{% if environment == "production" %}
[expect.hermeticity]
no_external_services = true
{% else %}
[expect.hermeticity]
allowed_hosts = ["localhost", "127.0.0.1"]
{% endif %}
```

---

## Troubleshooting

### Span Not Found

**Error**:
```
Expected span 'ggen.marketplace.search' not found
```

**Fix**:
```rust
// Add instrumentation to Rust code
#[instrument(name = "ggen.marketplace.search", skip(self))]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    // ...
}
```

### Attribute Mismatch

**Error**:
```
Expected attribute 'result_count' = 5, got 3
```

**Fix**:
```toml
# Use >= instead of exact match
attrs.all = { "result_count.gte" = 1 }
```

### Temporal Violation

**Error**:
```
Span 'B' occurred before 'A' (expected A → B)
```

**Fix**:
```toml
# Add explicit dependency
[[scenario]]
name = "scenario_b"
depends_on = ["scenario_a"]
```

### Container Timeout

**Error**:
```
Container timeout after 300s
```

**Fix**:
```toml
[service.ggen]
timeout_seconds = 600  # Increase to 10 minutes
```

### Hermeticity Violation

**Error**:
```
Unexpected external call to 'api.example.com'
```

**Fix**:
```toml
[expect.hermeticity]
no_external_services = false
allowed_hosts = ["api.example.com"]
```

---

## CLI Commands

### Run Single Test

```bash
clnrm run tests/clnrm/marketplace_search.clnrm.toml
```

### Run All Tests

```bash
clnrm run tests/clnrm/*.clnrm.toml
```

### Run with JUnit XML Output

```bash
clnrm run tests/clnrm/*.clnrm.toml --junit-xml results.xml
```

### Debug Span Traces

```bash
clnrm debug --test tests/clnrm/marketplace_search.clnrm.toml --show-spans
```

### Validate Test Configuration

```bash
clnrm validate --test tests/clnrm/marketplace_search.clnrm.toml
```

### Check for Fake-Green Tests

```bash
clnrm analyze --fake-green-check tests/clnrm/
```

### Generate Timeline Visualization

```bash
clnrm timeline --test tests/clnrm/lifecycle_complete.clnrm.toml
```

### Render Tera Template

```bash
clnrm template render \
  --template templates/basic_cli_command.clnrm.toml \
  --vars test_name="my_test" command="ggen --version" \
  --output tests/clnrm/my_test.clnrm.toml
```

### Watch Mode (Auto-rerun on changes)

```bash
clnrm watch tests/clnrm/*.clnrm.toml
```

---

## Span Taxonomy Reference

### CLI Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.cli.execute` | Top-level command execution | - |
| `ggen.cli.parse` | Argument parsing | `ggen.cli.execute` |

### Marketplace Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.marketplace.search` | Package search | `ggen.cli.execute` |
| `ggen.marketplace.resolve` | Version resolution | `ggen.cli.execute` |
| `ggen.marketplace.install` | Package installation | `ggen.cli.execute` |

### Registry Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.registry.init` | Initialize registry | `ggen.cli.execute` |
| `ggen.registry.query` | Query registry | `ggen.marketplace.search` |
| `ggen.registry.fetch` | Fetch index | `ggen.registry.query` |

### Package Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.package.download` | Download package | `ggen.marketplace.install` |
| `ggen.package.extract` | Extract package | `ggen.marketplace.install` |
| `ggen.package.verify` | Verify integrity | `ggen.package.extract` |

### Lifecycle Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.lifecycle.init` | Initialize project | `ggen.cli.execute` |
| `ggen.lifecycle.build` | Build project | `ggen.cli.execute` |
| `ggen.lifecycle.test` | Test project | `ggen.cli.execute` |
| `ggen.lifecycle.deploy` | Deploy project | `ggen.cli.execute` |

### Crypto Spans

| Span Name | Purpose | Parent |
|-----------|---------|--------|
| `ggen.crypto.sign` | Sign data | `ggen.cli.execute` |
| `ggen.crypto.verify` | Verify signature | `ggen.cli.execute` |
| `ggen.crypto.hash` | Hash data | `ggen.crypto.sign` |

---

## Environment Variables

```bash
# OTEL configuration
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
export OTEL_SERVICE_NAME=ggen
export OTEL_SERVICE_VERSION=1.0.0

# Rust logging
export RUST_LOG=info,ggen=trace

# clnrm configuration
export CLNRM_TIMEOUT=600  # Default timeout in seconds
export CLNRM_PARALLEL=4   # Number of parallel tests
```

---

## Best Practices

### ✅ DO

1. **Use descriptive test names**
   ```toml
   [meta]
   name = "marketplace_search_returns_rust_packages"
   description = "Verify search for 'rust' returns at least 1 package"
   ```

2. **Always include OTEL validation**
   ```toml
   [[expect.span]]
   name = "ggen.marketplace.search"
   attrs.all = { "query" = "rust", "result_count.gte" = 1 }
   ```

3. **Use `.gte`/`.lte` for flexible assertions**
   ```toml
   attrs.all = { "result_count.gte" = 1 }  # Instead of exact count
   ```

4. **Add hermeticity checks**
   ```toml
   [expect.hermeticity]
   no_external_services = true
   ```

5. **Document test purpose**
   ```toml
   [meta]
   description = "Proves marketplace search actually executes by validating OTEL spans"
   ```

### ❌ DON'T

1. **Don't use exact counts when unnecessary**
   ```toml
   # ❌ BAD
   attrs.all = { "result_count" = 5 }  # Brittle

   # ✅ GOOD
   attrs.all = { "result_count.gte" = 1 }  # Flexible
   ```

2. **Don't skip span validation**
   ```toml
   # ❌ BAD - Could be fake-green
   [[scenario]]
   run = "ggen market search rust"
   # No [[expect.span]] assertions!

   # ✅ GOOD
   [[scenario]]
   run = "ggen market search rust"

   [[expect.span]]
   name = "ggen.marketplace.search"
   ```

3. **Don't use long timeouts unnecessarily**
   ```toml
   # ❌ BAD
   timeout_seconds = 3600  # 1 hour is excessive

   # ✅ GOOD
   timeout_seconds = 300  # 5 minutes is reasonable
   ```

4. **Don't ignore hermeticity**
   ```toml
   # ❌ BAD - Missing hermeticity check
   [service.ggen]
   plugin = "generic_container"

   # ✅ GOOD
   [service.ggen]
   plugin = "generic_container"

   [expect.hermeticity]
   no_external_services = true
   ```

5. **Don't hardcode absolute paths**
   ```toml
   # ❌ BAD
   volumes = ["/Users/alice/ggen:/app"]

   # ✅ GOOD
   volumes = ["./:/app"]
   ```

---

## Conversion Checklist

When converting a Rust test to CLNRM TOML:

- [ ] Identify test purpose and scope
- [ ] Map test operations to CLI commands
- [ ] Define expected OTEL spans
- [ ] Add span attribute assertions
- [ ] Validate span hierarchy
- [ ] Check temporal ordering
- [ ] Add hermeticity checks
- [ ] Test error cases (if applicable)
- [ ] Add descriptive metadata
- [ ] Run both Rust and CLNRM versions
- [ ] Compare results
- [ ] Document any differences

---

## Examples Directory

See `tests/clnrm/examples/` for complete examples:

- `marketplace_search.clnrm.toml` - Basic search test
- `lifecycle_complete_flow.clnrm.toml` - Multi-phase test
- `crypto_signature_verification.clnrm.toml` - Security test
- `search_property_test.clnrm.toml` - Property test with Tera
- `error_handling.clnrm.toml` - Error case validation

---

## Resources

- **Full Strategy**: [CLNRM_MIGRATION_STRATEGY.md](./CLNRM_MIGRATION_STRATEGY.md)
- **Summary**: [CLNRM_MIGRATION_SUMMARY.md](./CLNRM_MIGRATION_SUMMARY.md)
- **CLNRM Docs**: https://docs.clnrm.io
- **OpenTelemetry**: https://opentelemetry.io/docs/

---

**Last Updated**: 2025-10-17
**Maintainer**: CLNRM Migration Team
