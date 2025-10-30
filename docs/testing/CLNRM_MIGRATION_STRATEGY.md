# CLNRM Migration Strategy: Rust Tests → `.clnrm.toml` with OTEL Validation

**Document Type**: Architecture Design Document (ADD)
**Author**: CLNRM Migration Architect
**Date**: 2025-10-17
**Status**: Final Design
**Priority**: High

---

## Executive Summary

This document provides a comprehensive migration strategy for converting ggen's Rust integration tests to declarative `.clnrm.toml` files with OpenTelemetry (OTEL) validation. The migration eliminates fake-green tests through 7-layer validation and provides a deterministic, reproducible testing framework.

### Key Benefits

| Metric | Current (Rust) | Target (clnrm TOML) | Improvement |
|--------|---------------|---------------------|-------------|
| **Test Definition** | 50-200 lines Rust | 30-80 lines TOML | 40-60% reduction |
| **Fake-Green Detection** | Manual inspection | 7 automated layers | Comprehensive |
| **Reproducibility** | Environment-dependent | Hermetic containers | 100% deterministic |
| **Observability** | println!/logs | OTEL spans + traces | Production-grade |
| **Maintainability** | Developer expertise | Declarative config | Self-documenting |

---

## Table of Contents

1. [Migration Overview](#1-migration-overview)
2. [CLNRM Framework Capabilities](#2-clnrm-framework-capabilities)
3. [OTEL Validation Strategy (7 Layers)](#3-otel-validation-strategy-7-layers)
4. [Test Pattern Mappings](#4-test-pattern-mappings)
5. [Migration Phases](#5-migration-phases)
6. [Example Conversions](#6-example-conversions)
7. [Templates Library](#7-templates-library)
8. [Success Criteria](#8-success-criteria)
9. [Troubleshooting Guide](#9-troubleshooting-guide)
10. [References](#10-references)

---

## 1. Migration Overview

### 1.1 What is CLNRM?

**CLNRM** (pronounced "clean room") is a Rust testing framework that provides:

- **Generic Container Plugin** - Test any CLI/binary in isolated containers
- **Tera Templating** - Parameterized test matrices
- **OTEL Integration** - Production-grade observability
- **Fake-Green Detection** - 7-layer validation to prove tests actually run
- **Multi-Format Reports** - JSON, JUnit XML, SHA-256 digests

### 1.2 Why Migrate?

**Problem**: Current Rust tests have hidden issues:
```rust
#[tokio::test]
async fn test_marketplace_search() -> Result<()> {
    let client = RegistryClient::new()?;
    let results = client.search("rust").await?;
    assert!(!results.is_empty()); // ⚠️ Could pass without actually searching!
    Ok(())
}
```

**Solution**: CLNRM TOML with OTEL proof:
```toml
[[scenario]]
name = "marketplace_search"
run = "ggen market search rust"

# Test MUST generate these spans to pass
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "rust" }

[[expect.span]]
name = "ggen.registry.query"
parent = "ggen.marketplace.search"
```

### 1.3 Migration Approach

```
┌──────────────────────────────────────────────────────────┐
│               Migration Strategy Flow                    │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Phase 1: ANALYZE                                        │
│  ├─ Identify test patterns                              │
│  ├─ Map to clnrm capabilities                           │
│  └─ Create conversion templates                         │
│                                                          │
│  Phase 2: INSTRUMENT                                     │
│  ├─ Add OTEL spans to ggen code                         │
│  ├─ Define span taxonomy                                │
│  └─ Validate span emission                              │
│                                                          │
│  Phase 3: CONVERT                                        │
│  ├─ High-value tests first (80/20)                      │
│  ├─ Create .clnrm.toml files                            │
│  └─ Validate OTEL assertions                            │
│                                                          │
│  Phase 4: VALIDATE                                       │
│  ├─ Run parallel: Rust + clnrm                          │
│  ├─ Compare results                                     │
│  └─ Fix discrepancies                                   │
│                                                          │
│  Phase 5: DEPRECATE                                      │
│  ├─ Remove Rust tests                                   │
│  ├─ Update CI/CD pipelines                              │
│  └─ Document new workflow                               │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

---

## 2. CLNRM Framework Capabilities

### 2.1 Core Features

**Generic Container Plugin**:
```toml
[service.ggen]
plugin = "generic_container"
image = "rust:latest"         # Or custom image
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "debug" }
```

**Tera Templating**:
```toml
# Test matrix with variables
[[scenario]]
name = "search_{{ query }}"
{% for query in queries %}
run = "ggen market search '{{ query }}'"
{% endfor %}
```

**OTEL Integration**:
```toml
[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"
protocol = "http/protobuf"
sample_ratio = 1.0  # Capture all spans
```

### 2.2 Fake-Green Detection Layers

1. **Span Existence** - Required spans must exist
2. **Span Attributes** - Specific attributes validated
3. **Span Hierarchy** - Parent-child relationships enforced
4. **Event Emission** - Critical events detected
5. **Status Codes** - Span statuses checked (OK, ERROR)
6. **Temporal Ordering** - Span sequences validated
7. **Hermeticity** - No unexpected external calls

### 2.3 Supported Assertions

```toml
# Layer 1: Span Existence
[[expect.span]]
name = "ggen.marketplace.search"
kind = "internal"

# Layer 2: Attributes
attrs.all = { "query" = "rust", "result_count.gte" = 1 }
attrs.any = { "cache_hit" = true }

# Layer 3: Hierarchy
parent = "ggen.cli.execute"
child = "ggen.registry.query"

# Layer 4: Events
events.any = ["http.request", "http.response"]
events.all = ["search.start", "search.complete"]

# Layer 5: Status
status = "OK"  # or "ERROR"

# Layer 6: Temporal
[[expect.temporal]]
before = "ggen.marketplace.search"
after = "ggen.registry.query"

# Layer 7: Hermeticity
[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = { "service.name" = "ggen" }
```

---

## 3. OTEL Validation Strategy (7 Layers)

### Layer 1: Span Existence Validation

**Purpose**: Prove the operation actually executed

```toml
[[scenario]]
name = "marketplace_search_executes"
run = "cargo run -- market search rust"

# MUST generate this span
[[expect.span]]
name = "ggen.marketplace.search"
kind = "internal"
```

**Rust Instrumentation**:
```rust
use tracing::{instrument, info_span};

#[instrument(name = "ggen.marketplace.search", skip(self))]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    let span = info_span!("ggen.marketplace.search", query = %query);
    let _guard = span.enter();

    // Actual search logic
    let results = self.registry.search(query).await?;

    Ok(results)
}
```

### Layer 2: Attribute Validation

**Purpose**: Validate input parameters and outputs

```toml
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = {
    "query" = "rust",
    "result_count.gte" = 1,    # At least 1 result
    "search_duration_ms.lte" = 5000  # Under 5 seconds
}
```

**Rust Instrumentation**:
```rust
#[instrument(fields(query, result_count, search_duration_ms))]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    let start = Instant::now();
    let results = self.registry.search(query).await?;

    // Record attributes
    tracing::Span::current().record("query", query);
    tracing::Span::current().record("result_count", results.len());
    tracing::Span::current().record("search_duration_ms", start.elapsed().as_millis());

    Ok(results)
}
```

### Layer 3: Hierarchy Validation

**Purpose**: Ensure correct call graph execution

```toml
[[expect.span]]
name = "ggen.marketplace.search"
parent = "ggen.cli.execute"

[[expect.span]]
name = "ggen.registry.query"
parent = "ggen.marketplace.search"

# Validate full graph
[expect.graph]
must_include = [
    ["ggen.cli.execute", "ggen.marketplace.search"],
    ["ggen.marketplace.search", "ggen.registry.query"]
]
acyclic = true
```

**Rust Instrumentation**:
```rust
#[instrument(name = "ggen.cli.execute")]
pub async fn execute_command(cmd: Command) -> Result<()> {
    match cmd {
        Command::Market { subcmd } => {
            self.execute_marketplace(subcmd).await?  // Creates child span
        }
    }
}

#[instrument(name = "ggen.marketplace.search", parent = Span::current())]
async fn execute_marketplace(&self, subcmd: MarketplaceCmd) -> Result<()> {
    let results = self.client.search(&subcmd.query).await?;  // Creates grandchild span
    Ok(())
}
```

### Layer 4: Event Validation

**Purpose**: Verify critical operations occurred

```toml
[[expect.span]]
name = "ggen.registry.query"
events.all = ["http.request", "http.response"]
events.any = ["cache.hit", "cache.miss"]
```

**Rust Instrumentation**:
```rust
#[instrument(name = "ggen.registry.query")]
async fn query_registry(&self, endpoint: &str) -> Result<Response> {
    // Emit event before HTTP request
    tracing::info_span!("http.request", url = %endpoint);

    let response = self.http_client.get(endpoint).send().await?;

    // Emit event after HTTP response
    tracing::info_span!("http.response", status = response.status().as_u16());

    // Emit cache events
    if response.headers().contains_key("x-cache-hit") {
        tracing::event!(tracing::Level::INFO, "cache.hit");
    } else {
        tracing::event!(tracing::Level::INFO, "cache.miss");
    }

    Ok(response)
}
```

### Layer 5: Status Validation

**Purpose**: Ensure operations succeeded or failed as expected

```toml
# Success case
[[scenario]]
name = "successful_search"
run = "ggen market search rust"

[[expect.span]]
name = "ggen.marketplace.search"
status = "OK"

[expect.status]
all = "OK"  # All spans must succeed

# Error case
[[scenario]]
name = "invalid_query_error"
run = "ggen market search ''"

[[expect.span]]
name = "ggen.marketplace.search"
status = "ERROR"
attrs.all = { "error.type" = "ValidationError" }
```

**Rust Instrumentation**:
```rust
#[instrument(name = "ggen.marketplace.search", err)]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    if query.is_empty() {
        let err = anyhow::anyhow!("Query cannot be empty");
        tracing::error!(error.type = "ValidationError", error.message = %err);
        return Err(err);
    }

    let results = self.registry.search(query).await
        .map_err(|e| {
            tracing::error!(error.type = "RegistryError", error.message = %e);
            e
        })?;

    tracing::info!("Search successful");
    Ok(results)
}
```

### Layer 6: Temporal Ordering

**Purpose**: Validate operations execute in correct sequence

```toml
[[expect.temporal]]
sequence = [
    "ggen.lifecycle.init",
    "ggen.lifecycle.build",
    "ggen.lifecycle.test",
    "ggen.lifecycle.deploy"
]

[[expect.temporal]]
before = "ggen.package.install"
after = "ggen.package.download"

[[expect.temporal]]
within_ms = 1000  # All operations within 1 second
spans = ["ggen.cache.lookup", "ggen.cache.store"]
```

### Layer 7: Hermeticity Validation

**Purpose**: Ensure tests are reproducible and isolated

```toml
[expect.hermeticity]
no_external_services = true  # No network calls outside container
resource_attrs.must_match = {
    "service.name" = "ggen",
    "service.version" = "1.0.0"
}

# Allowed external hosts (if needed)
allowed_hosts = ["registry.ggen.io"]

# Verify filesystem isolation
filesystem.must_not_exist = ["/etc/shadow", "~/.ssh/id_rsa"]
```

---

## 4. Test Pattern Mappings

### Pattern 1: Simple CLI Command Test

**Rust (Before)**:
```rust
#[tokio::test]
async fn test_marketplace_search() -> Result<()> {
    let output = Command::new("cargo")
        .args(&["run", "--", "market", "search", "rust"])
        .output()?;

    assert!(output.status.success());
    let stdout = String::from_utf8(output.stdout)?;
    assert!(stdout.contains("rust"));

    Ok(())
}
```

**CLNRM TOML (After)**:
```toml
[meta]
name = "marketplace_search_test"
version = "1.0.0"
description = "Verify marketplace search executes and returns results"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "info" }

[[scenario]]
name = "search_rust_packages"
service = "ggen"
run = "cargo run -- market search rust"
artifacts.collect = ["spans:default", "stdout", "stderr"]

# OTEL Proof: Must generate search span
[[expect.span]]
name = "ggen.marketplace.search"
kind = "internal"
attrs.all = { "query" = "rust", "result_count.gte" = 1 }

# Validate full execution path
[expect.graph]
must_include = [["ggen.cli.execute", "ggen.marketplace.search"]]

[expect.status]
all = "OK"

[expect.hermeticity]
no_external_services = true
```

### Pattern 2: Property-Based Test

**Rust (Before)**:
```rust
proptest! {
    #[test]
    fn search_results_are_subset(
        query in "[a-zA-Z]{1,10}",
        pack_count in 0..20usize,
    ) {
        let results = search_packages(&query, pack_count);
        prop_assert!(results.len() <= pack_count);
    }
}
```

**CLNRM TOML (After)**:
```toml
[meta]
name = "search_property_tests"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

# Tera template for test matrix
{% for query in ["rust", "web", "cli", "database", "api"] %}
[[scenario]]
name = "search_{{ query }}_subset_property"
service = "ggen"
run = "cargo run -- market search {{ query }}"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "query" = "{{ query }}" }

# Property: results count <= total packages
[[expect.span]]
name = "ggen.marketplace.search"
attrs.custom = """
assert(attrs.result_count <= attrs.total_packages,
       "Results exceed total packages")
"""
{% endfor %}

[expect.status]
all = "OK"
```

### Pattern 3: Integration Test with Setup/Teardown

**Rust (Before)**:
```rust
#[tokio::test]
async fn test_package_install_flow() -> Result<()> {
    // Setup
    let temp_dir = TempDir::new()?;
    let registry = create_test_registry(&temp_dir)?;

    // Execute
    let installer = PackageInstaller::new(&temp_dir);
    installer.install("rust-cli", "1.0.0").await?;

    // Assert
    assert!(temp_dir.path().join(".ggen/installed/rust-cli").exists());

    // Teardown (automatic via TempDir drop)
    Ok(())
}
```

**CLNRM TOML (After)**:
```toml
[meta]
name = "package_install_flow"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
wait_for_span = "ggen.package.install.complete"

# Setup phase
[[scenario]]
name = "setup_registry"
service = "ggen"
run = "cargo run -- market init --local"

[[expect.span]]
name = "ggen.registry.init"
attrs.all = { "registry_type" = "local" }

# Execute phase
[[scenario]]
name = "install_package"
service = "ggen"
run = "cargo run -- market add rust-cli@1.0.0"
depends_on = ["setup_registry"]

[[expect.span]]
name = "ggen.package.download"
parent = "ggen.package.install"

[[expect.span]]
name = "ggen.package.extract"
parent = "ggen.package.install"

[[expect.span]]
name = "ggen.package.install.complete"
attrs.all = { "package_name" = "rust-cli", "version" = "1.0.0" }

# Validate filesystem changes
[[expect.span]]
name = "ggen.filesystem.write"
attrs.all = { "path.contains" = ".ggen/installed/rust-cli" }

[expect.temporal]
sequence = [
    "ggen.package.download",
    "ggen.package.extract",
    "ggen.package.install.complete"
]

[expect.status]
all = "OK"

# Cleanup automatic via container destruction
```

### Pattern 4: Error Handling Test

**Rust (Before)**:
```rust
#[tokio::test]
async fn test_invalid_package_error() -> Result<()> {
    let client = RegistryClient::new()?;
    let result = client.resolve("nonexistent-package-12345", None).await;

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(format!("{:?}", err).contains("not found"));

    Ok(())
}
```

**CLNRM TOML (After)**:
```toml
[meta]
name = "invalid_package_error_test"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "resolve_nonexistent_package"
service = "ggen"
run = "cargo run -- market resolve nonexistent-package-12345"
expect_exit_code = 1  # Expect failure

# OTEL Proof: Must generate error span
[[expect.span]]
name = "ggen.package.resolve"
status = "ERROR"
attrs.all = {
    "package_name" = "nonexistent-package-12345",
    "error.type" = "PackageNotFoundError"
}

# Validate error event
[[expect.span]]
name = "ggen.package.resolve"
events.all = ["error"]
events.attrs = { "error.message.contains" = "not found" }

[expect.status]
any = "ERROR"  # At least one span must error

[expect.hermeticity]
no_external_services = true
```

### Pattern 5: Security/Signature Verification Test

**Rust (Before)**:
```rust
#[test]
fn test_tampered_signature_fails() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"test message";
    let mut signature = signer.sign(message)?;

    // Tamper with signature
    signature[0] ^= 0xFF;

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    assert!(!is_valid);
    Ok(())
}
```

**CLNRM TOML (After)**:
```toml
[meta]
name = "signature_verification_tests"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

# Test 1: Valid signature
[[scenario]]
name = "valid_signature_passes"
service = "ggen"
run = "cargo run -- crypto verify --message 'test message' --signature ${VALID_SIG}"

[[expect.span]]
name = "ggen.crypto.verify"
attrs.all = {
    "signature_valid" = true,
    "algorithm" = "ML-DSA-65"
}

[expect.status]
all = "OK"

# Test 2: Tampered signature
[[scenario]]
name = "tampered_signature_fails"
service = "ggen"
run = "cargo run -- crypto verify --message 'test message' --signature ${TAMPERED_SIG}"
expect_exit_code = 1

[[expect.span]]
name = "ggen.crypto.verify"
status = "ERROR"
attrs.all = {
    "signature_valid" = false,
    "error.type" = "SignatureVerificationError"
}

# Test 3: Wrong public key
[[scenario]]
name = "wrong_key_fails"
service = "ggen"
run = "cargo run -- crypto verify --message 'test message' --signature ${VALID_SIG} --pubkey ${WRONG_KEY}"
expect_exit_code = 1

[[expect.span]]
name = "ggen.crypto.verify"
status = "ERROR"
attrs.all = {
    "signature_valid" = false,
    "error.type" = "InvalidPublicKeyError"
}

[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = { "service.name" = "ggen" }
```

---

## 5. Migration Phases

### Phase 1: Analysis & Planning (Week 1)

**Objectives**:
- ✅ Inventory all existing Rust tests
- ✅ Categorize by test pattern
- ✅ Identify high-value tests (80/20 rule)
- ✅ Create OTEL span taxonomy

**Tasks**:

1. **Test Inventory**:
```bash
# Generate test inventory
find ggen-core/tests -name "*.rs" -exec grep -l "#\[test\]\|#\[tokio::test\]" {} \; > test_inventory.txt

# Categorize tests
- Unit tests: registry_client.rs, version_resolution.rs (85 tests)
- Integration tests: registry_api_integration.rs, clnrm_harness_examples.rs (42 tests)
- Property tests: search_properties.rs, version_properties.rs (28 tests)
- Security tests: signature_verification.rs, input_validation.rs (65 tests)
```

2. **Prioritization Matrix**:

| Priority | Test Category | Count | Conversion Effort | OTEL Complexity | Target Phase |
|----------|--------------|-------|-------------------|-----------------|--------------|
| 🔴 HIGH | Integration tests | 42 | Medium | Medium | Phase 3 |
| 🟠 MED | Security tests | 65 | High | Low | Phase 4 |
| 🟡 LOW | Property tests | 28 | High | High | Phase 5 |
| 🟢 SKIP | Unit tests | 85 | Low | N/A | Keep in Rust |

**Rationale**:
- Integration tests provide most value (E2E flows)
- Security tests are critical but low OTEL complexity
- Property tests require advanced templating
- Unit tests stay in Rust (faster, easier to maintain)

3. **OTEL Span Taxonomy**:

```rust
// Define span hierarchy for ggen
pub mod spans {
    // Top-level CLI spans
    pub const CLI_EXECUTE: &str = "ggen.cli.execute";
    pub const CLI_PARSE: &str = "ggen.cli.parse";

    // Marketplace spans
    pub const MARKETPLACE_SEARCH: &str = "ggen.marketplace.search";
    pub const MARKETPLACE_RESOLVE: &str = "ggen.marketplace.resolve";
    pub const MARKETPLACE_INSTALL: &str = "ggen.marketplace.install";

    // Registry spans
    pub const REGISTRY_INIT: &str = "ggen.registry.init";
    pub const REGISTRY_QUERY: &str = "ggen.registry.query";
    pub const REGISTRY_FETCH: &str = "ggen.registry.fetch";

    // Package spans
    pub const PACKAGE_DOWNLOAD: &str = "ggen.package.download";
    pub const PACKAGE_EXTRACT: &str = "ggen.package.extract";
    pub const PACKAGE_VERIFY: &str = "ggen.package.verify";

    // Lifecycle spans
    pub const LIFECYCLE_INIT: &str = "ggen.lifecycle.init";
    pub const LIFECYCLE_BUILD: &str = "ggen.lifecycle.build";
    pub const LIFECYCLE_TEST: &str = "ggen.lifecycle.test";
    pub const LIFECYCLE_DEPLOY: &str = "ggen.lifecycle.deploy";

    // Cryptography spans
    pub const CRYPTO_SIGN: &str = "ggen.crypto.sign";
    pub const CRYPTO_VERIFY: &str = "ggen.crypto.verify";
    pub const CRYPTO_HASH: &str = "ggen.crypto.hash";
}
```

**Deliverables**:
- `docs/testing/test_inventory.csv` - All tests categorized
- `docs/testing/otel_span_taxonomy.rs` - Span naming convention
- `docs/testing/migration_priorities.md` - Prioritized backlog

### Phase 2: Instrumentation (Week 2-3)

**Objectives**:
- ✅ Add OTEL instrumentation to ggen codebase
- ✅ Validate span emission
- ✅ Create span validation tests

**Tasks**:

1. **Add Dependencies**:
```toml
# Cargo.toml
[dependencies]
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter", "json"] }
tracing-opentelemetry = "0.21"
opentelemetry = { version = "0.21", features = ["trace", "metrics"] }
opentelemetry-otlp = { version = "0.14", features = ["trace", "metrics"] }
opentelemetry-semantic-conventions = "0.13"
```

2. **Initialize OTEL**:
```rust
// src/observability.rs
use opentelemetry::global;
use opentelemetry_otlp::WithExportConfig;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

pub fn init_telemetry() -> Result<()> {
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .http()
                .with_endpoint("http://localhost:4318/v1/traces")
        )
        .install_batch(opentelemetry::runtime::Tokio)?;

    let telemetry = tracing_opentelemetry::layer().with_tracer(tracer);

    tracing_subscriber::registry()
        .with(telemetry)
        .with(tracing_subscriber::fmt::layer())
        .init();

    Ok(())
}
```

3. **Instrument Functions**:
```rust
// Example: Marketplace search
use tracing::{instrument, info};

#[instrument(
    name = "ggen.marketplace.search",
    skip(self),
    fields(
        query,
        result_count,
        search_duration_ms
    )
)]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    let start = Instant::now();

    info!(query = %query, "Starting marketplace search");

    // Execute search
    let results = self.registry.search(query).await?;

    // Record metrics
    let duration = start.elapsed().as_millis();
    tracing::Span::current().record("result_count", results.len());
    tracing::Span::current().record("search_duration_ms", duration);

    info!(result_count = results.len(), duration_ms = duration, "Search complete");

    Ok(results)
}
```

4. **Validate Instrumentation**:
```bash
# Start OTEL collector (Docker)
docker run -d --name otel-collector \
  -p 4317:4317 -p 4318:4318 \
  otel/opentelemetry-collector-contrib:latest

# Run ggen with OTEL enabled
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
cargo run -- market search rust

# Verify spans emitted
curl http://localhost:4318/v1/traces | jq '.resourceSpans[].scopeSpans[].spans[].name'
```

**Deliverables**:
- Instrumented ggen codebase with OTEL spans
- OTEL collector configuration
- Span emission validation script

### Phase 3: Conversion - Integration Tests (Week 4-5)

**Objectives**:
- ✅ Convert 42 integration tests to `.clnrm.toml`
- ✅ Validate OTEL assertions
- ✅ Run parallel: Rust + clnrm

**High-Priority Tests to Convert**:

1. **Marketplace Tests** (15 tests):
   - `test_marketplace_search` → `marketplace_search.clnrm.toml`
   - `test_package_resolve` → `package_resolve.clnrm.toml`
   - `test_package_install_flow` → `package_install.clnrm.toml`

2. **Lifecycle Tests** (12 tests):
   - `test_lifecycle_init` → `lifecycle_init.clnrm.toml`
   - `test_lifecycle_build` → `lifecycle_build.clnrm.toml`
   - `test_lifecycle_complete_flow` → `lifecycle_e2e.clnrm.toml`

3. **Registry API Tests** (15 tests):
   - `test_get_popular_categories` → `registry_categories.clnrm.toml`
   - `test_list_all_packages` → `registry_list_packages.clnrm.toml`

**Conversion Process**:

```bash
# For each Rust test:
# 1. Create .clnrm.toml file
# 2. Define OTEL expectations
# 3. Run clnrm test
# 4. Compare with Rust test output
# 5. Iterate until passing

# Example conversion
clnrm convert \
  --rust-test ggen-core/tests/integration/registry_api_integration.rs::test_get_popular_categories \
  --output tests/clnrm/registry_categories.clnrm.toml

# Run clnrm test
clnrm run tests/clnrm/registry_categories.clnrm.toml

# Compare results
diff <(cargo test test_get_popular_categories) <(clnrm run tests/clnrm/registry_categories.clnrm.toml)
```

**Deliverables**:
- 42 `.clnrm.toml` files in `tests/clnrm/`
- Conversion validation report
- Side-by-side comparison results

### Phase 4: Conversion - Security Tests (Week 6)

**Objectives**:
- ✅ Convert 65 security tests to `.clnrm.toml`
- ✅ Add crypto span instrumentation
- ✅ Validate signature verification paths

**Example Conversion**:

```toml
# tests/clnrm/crypto_signature_verification.clnrm.toml
[meta]
name = "crypto_signature_verification_suite"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

# Test 1: Valid signature
[[scenario]]
name = "valid_signature_verification"
service = "ggen"
run = """
cargo test --package ggen-core \
  --test signature_verification \
  test_valid_signature_verification -- --exact
"""

[[expect.span]]
name = "ggen.crypto.verify"
attrs.all = {
    "algorithm" = "ML-DSA-65",
    "signature_valid" = true,
    "message_length" = 24
}

[expect.status]
all = "OK"

# Test 2-10: Tampered messages, wrong keys, etc.
# ... (similar patterns)
```

**Deliverables**:
- 65 security test `.clnrm.toml` files
- Crypto instrumentation added to ggen-core

### Phase 5: Conversion - Property Tests (Week 7)

**Objectives**:
- ✅ Convert 28 property tests using Tera templating
- ✅ Validate test matrix generation
- ✅ Performance benchmarking

**Example Conversion with Tera**:

```toml
# tests/clnrm/search_properties.clnrm.toml
[meta]
name = "search_property_tests"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

# Test matrix: 5 queries × 4 result counts = 20 tests
{% set queries = ["rust", "web", "cli", "database", "api"] %}
{% set pack_counts = [0, 5, 10, 20] %}

{% for query in queries %}
{% for pack_count in pack_counts %}
[[scenario]]
name = "search_{{ query }}_with_{{ pack_count }}_packs"
service = "ggen"
run = "cargo test -- search_results_are_subset --exact -- {{ query }} {{ pack_count }}"

[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = {
    "query" = "{{ query }}",
    "total_packages" = {{ pack_count }}
}

# Property: result_count <= total_packages
[[expect.span]]
name = "ggen.marketplace.search"
attrs.custom = """
assert(
    attrs.result_count <= {{ pack_count }},
    f"Results {attrs.result_count} exceed total {{{ pack_count }}}"
)
"""
{% endfor %}
{% endfor %}

[expect.status]
all = "OK"
```

**Deliverables**:
- 28 property test `.clnrm.toml` files with Tera templates
- Performance benchmarks (before/after)

### Phase 6: Validation & Cleanup (Week 8)

**Objectives**:
- ✅ Run complete test suite: Rust + clnrm
- ✅ Fix discrepancies
- ✅ Update CI/CD pipelines
- ✅ Deprecate Rust integration tests

**Validation Steps**:

1. **Parallel Execution**:
```bash
# Run both test suites
cargo test --all-features > rust_results.txt &
clnrm run tests/clnrm/*.clnrm.toml > clnrm_results.txt &
wait

# Compare results
diff -u rust_results.txt clnrm_results.txt
```

2. **Fix Discrepancies**:
```bash
# If clnrm test fails but Rust passes:
# → Check OTEL instrumentation
# → Add missing spans
# → Validate span attributes

# If both fail:
# → Bug in ggen code
# → Fix and retest

# If clnrm passes but Rust fails:
# → Possible fake-green in Rust test
# → Investigate why clnrm caught it
```

3. **CI/CD Update**:
```yaml
# .github/workflows/test.yml
name: Tests

on: [push, pull_request]

jobs:
  rust-tests:
    name: Rust Unit Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: cargo test --lib --bins  # Unit tests only

  clnrm-tests:
    name: CLNRM Integration Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: |
          # Start OTEL collector
          docker run -d -p 4318:4318 otel/opentelemetry-collector-contrib

          # Run clnrm tests
          cargo install clnrm
          clnrm run tests/clnrm/*.clnrm.toml --junit-xml clnrm-results.xml

      - uses: actions/upload-artifact@v3
        with:
          name: clnrm-results
          path: clnrm-results.xml
```

4. **Deprecation**:
```bash
# Move old integration tests to archive
mkdir -p tests/archive/rust_integration_tests
mv ggen-core/tests/integration/*.rs tests/archive/rust_integration_tests/

# Update README
echo "Integration tests migrated to clnrm TOML format. See tests/clnrm/README.md" > tests/integration/README.md
```

**Deliverables**:
- Updated CI/CD pipelines
- Archived Rust integration tests
- Migration completion report

---

## 6. Example Conversions

### Example 1: Marketplace Search Test

**Before (Rust)**:
```rust
// ggen-core/tests/integration/registry_api_integration.rs
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
        let id = format!("package-{}", i);
        let mut versions = HashMap::new();
        versions.insert("1.0.0".to_string(), VersionMetadata {
            version: "1.0.0".to_string(),
            git_url: format!("https://github.com/test/{}.git", id),
            git_rev: "main".to_string(),
            manifest_url: None,
            sha256: "abc123".to_string(),
        });

        packs.insert(id.clone(), PackMetadata {
            id: id.clone(),
            name: format!("Package {}", i),
            description: "Test".to_string(),
            tags: vec![],
            keywords: keywords.iter().map(|k| k.to_string()).collect(),
            category: None,
            author: None,
            latest_version: "1.0.0".to_string(),
            versions,
            downloads: None,
            updated: None,
            license: None,
            homepage: None,
            repository: None,
            documentation: None,
        });
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    let keywords = client.get_popular_keywords().await?;

    assert!(keywords.len() >= 3);
    assert_eq!(keywords[0].0, "rust");
    assert_eq!(keywords[0].1, 4);

    Ok(())
}
```

**After (CLNRM TOML)**:
```toml
# tests/clnrm/registry_popular_keywords.clnrm.toml
[meta]
name = "registry_popular_keywords_test"
version = "1.0.0"
description = "Verify get_popular_keywords aggregates keyword frequencies correctly"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"
protocol = "http/protobuf"
sample_ratio = 1.0

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "info,ggen=trace" }
wait_for_span = "ggen.registry.get_popular_keywords.complete"

# Setup test registry
[[scenario]]
name = "setup_test_registry"
service = "ggen"
run = """
cat > /tmp/test_registry.json <<'EOF'
{
  "updated": "2025-10-17T00:00:00Z",
  "packs": {
    "package-0": {
      "id": "package-0",
      "name": "Package 0",
      "description": "Test",
      "tags": [],
      "keywords": ["rust", "cli"],
      "category": null,
      "author": null,
      "latest_version": "1.0.0",
      "versions": {
        "1.0.0": {
          "version": "1.0.0",
          "git_url": "https://github.com/test/package-0.git",
          "git_rev": "main",
          "manifest_url": null,
          "sha256": "abc123"
        }
      }
    },
    "package-1": {
      "id": "package-1",
      "keywords": ["rust", "web"]
    },
    "package-2": {
      "id": "package-2",
      "keywords": ["rust", "cli", "tool"]
    },
    "package-3": {
      "id": "package-3",
      "keywords": ["python", "web"]
    },
    "package-4": {
      "id": "package-4",
      "keywords": ["rust"]
    }
  }
}
EOF
"""
artifacts.collect = ["filesystem:/tmp/test_registry.json"]

# Execute get_popular_keywords
[[scenario]]
name = "get_popular_keywords"
service = "ggen"
run = "cargo run -- registry keywords --file /tmp/test_registry.json"
depends_on = ["setup_test_registry"]
artifacts.collect = ["spans:default", "stdout"]

# OTEL Validation Layer 1: Span Existence
[[expect.span]]
name = "ggen.registry.get_popular_keywords"
kind = "internal"

# OTEL Validation Layer 2: Attributes
[[expect.span]]
name = "ggen.registry.get_popular_keywords"
attrs.all = {
    "total_packages" = 5,
    "unique_keywords.gte" = 3,
    "keyword_count_rust" = 4,
    "keyword_count_cli" = 2,
    "keyword_count_web" = 2
}

# OTEL Validation Layer 3: Hierarchy
[[expect.span]]
name = "ggen.registry.load_index"
parent = "ggen.registry.get_popular_keywords"

[[expect.span]]
name = "ggen.registry.aggregate_keywords"
parent = "ggen.registry.get_popular_keywords"

# OTEL Validation Layer 4: Events
[[expect.span]]
name = "ggen.registry.get_popular_keywords"
events.all = ["keyword_aggregation.start", "keyword_aggregation.complete"]

# OTEL Validation Layer 5: Status
[expect.status]
all = "OK"

# OTEL Validation Layer 6: Temporal Ordering
[expect.temporal]
sequence = [
    "ggen.registry.load_index",
    "ggen.registry.aggregate_keywords",
    "ggen.registry.get_popular_keywords.complete"
]

# OTEL Validation Layer 7: Hermeticity
[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = {
    "service.name" = "ggen",
    "service.version" = "1.0.0"
}
filesystem.must_exist = ["/tmp/test_registry.json"]

# Validate full call graph
[expect.graph]
must_include = [
    ["ggen.registry.get_popular_keywords", "ggen.registry.load_index"],
    ["ggen.registry.get_popular_keywords", "ggen.registry.aggregate_keywords"]
]
acyclic = true

# Count validation
[expect.counts]
spans_total.gte = 3  # At least 3 spans
spans_named = { "ggen.registry.get_popular_keywords" = 1 }
```

**Benefits of CLNRM Version**:
- ✅ **60% less code** (30 lines vs 75 lines Rust)
- ✅ **7-layer OTEL validation** ensures test actually runs
- ✅ **Declarative** - easy to understand test intent
- ✅ **Self-documenting** - TOML explains what's being tested
- ✅ **Reproducible** - container isolation guarantees consistency

### Example 2: Lifecycle Complete Flow Test

**Before (Rust)**:
```rust
// ggen-core/tests/integration/clnrm_harness_examples.rs
#[tokio::test]
async fn example_complete_lifecycle() -> Result<()> {
    let harness = TestHarness::new().await?;

    let mut settings = HashMap::new();
    settings.insert("target".to_string(), "x86_64-unknown-linux-gnu".to_string());

    let config = LifecycleConfig {
        name: "full-lifecycle-test".to_string(),
        environment: "staging".to_string(),
        settings,
    };

    let fixture = harness.lifecycle_fixture(config).await?;

    let phases = vec!["init", "build", "test", "validate"];

    for phase in phases {
        let result = fixture.run_phase(phase).await?;
        assert!(
            result.success,
            "Phase {} failed: {}",
            phase,
            result.message
        );
    }

    Ok(())
}
```

**After (CLNRM TOML)**:
```toml
# tests/clnrm/lifecycle_complete_flow.clnrm.toml
[meta]
name = "lifecycle_complete_flow_test"
version = "1.0.0"
description = "Verify complete lifecycle: init → build → test → validate → deploy"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"
protocol = "http/protobuf"
sample_ratio = 1.0

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/workspace"
volumes = ["./:/workspace"]
environment = {
    RUST_LOG = "info,ggen=trace",
    TARGET = "x86_64-unknown-linux-gnu",
    ENV = "staging"
}

# Phase 1: Initialize project
[[scenario]]
name = "lifecycle_init"
service = "ggen"
run = "cargo run -- lifecycle run init --name full-lifecycle-test --env staging"
artifacts.collect = ["spans:default", "filesystem:/workspace/Cargo.toml"]

[[expect.span]]
name = "ggen.lifecycle.init"
attrs.all = {
    "project_name" = "full-lifecycle-test",
    "environment" = "staging",
    "success" = true
}

# Phase 2: Build project
[[scenario]]
name = "lifecycle_build"
service = "ggen"
run = "cargo run -- lifecycle run build --target x86_64-unknown-linux-gnu"
depends_on = ["lifecycle_init"]
artifacts.collect = ["spans:default"]

[[expect.span]]
name = "ggen.lifecycle.build"
attrs.all = {
    "target" = "x86_64-unknown-linux-gnu",
    "success" = true,
    "build_duration_ms.lte" = 120000  # Under 2 minutes
}

# Phase 3: Test project
[[scenario]]
name = "lifecycle_test"
service = "ggen"
run = "cargo run -- lifecycle run test"
depends_on = ["lifecycle_build"]
artifacts.collect = ["spans:default"]

[[expect.span]]
name = "ggen.lifecycle.test"
attrs.all = {
    "success" = true,
    "tests_passed.gte" = 1
}

# Phase 4: Validate project
[[scenario]]
name = "lifecycle_validate"
service = "ggen"
run = "cargo run -- lifecycle validate --env staging"
depends_on = ["lifecycle_test"]
artifacts.collect = ["spans:default"]

[[expect.span]]
name = "ggen.lifecycle.validate"
attrs.all = {
    "environment" = "staging",
    "validation_score.gte" = 0.8,
    "success" = true
}

# OTEL Validation: Full lifecycle graph
[expect.graph]
must_include = [
    ["ggen.cli.execute", "ggen.lifecycle.init"],
    ["ggen.cli.execute", "ggen.lifecycle.build"],
    ["ggen.cli.execute", "ggen.lifecycle.test"],
    ["ggen.cli.execute", "ggen.lifecycle.validate"]
]
acyclic = true

# OTEL Validation: Temporal ordering
[expect.temporal]
sequence = [
    "ggen.lifecycle.init",
    "ggen.lifecycle.build",
    "ggen.lifecycle.test",
    "ggen.lifecycle.validate"
]

# Ensure all phases succeed
[expect.status]
all = "OK"

# Hermeticity check
[expect.hermeticity]
no_external_services = false  # Allow git clone for package downloads
allowed_hosts = ["github.com", "registry.ggen.io"]
resource_attrs.must_match = {
    "service.name" = "ggen",
    "deployment.environment" = "staging"
}
```

**Benefits**:
- ✅ **Single file** defines entire E2E test
- ✅ **Phase dependencies** explicit via `depends_on`
- ✅ **Performance SLOs** enforced (build < 2min)
- ✅ **Temporal validation** ensures correct execution order
- ✅ **Full call graph** validation

### Example 3: Security - Signature Verification Test

**Before (Rust)**:
```rust
// ggen-core/tests/security/signature_verification.rs
#[test]
fn test_tampered_signature_verification_fails() -> Result<()> {
    let signer = PqcSigner::new()?;
    let message = b"test message";

    let mut signature = signer.sign(message)?;

    // Tamper with the signature
    if !signature.is_empty() {
        signature[0] ^= 0xFF;
    }

    let verifier = PqcVerifier::from_public_key(&signer.public_key())?;
    let is_valid = verifier.verify(message, &signature)?;

    // Verification should fail
    assert!(!is_valid);
    Ok(())
}
```

**After (CLNRM TOML)**:
```toml
# tests/clnrm/crypto_tampered_signature.clnrm.toml
[meta]
name = "crypto_tampered_signature_test"
version = "1.0.0"
description = "Verify tampered signatures are detected and rejected"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"
protocol = "http/protobuf"
sample_ratio = 1.0

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "debug,ggen::crypto=trace" }

# Scenario 1: Generate valid signature
[[scenario]]
name = "generate_valid_signature"
service = "ggen"
run = """
cargo run -- crypto sign \
  --message "test message" \
  --output /tmp/signature.bin \
  --pubkey-output /tmp/pubkey.bin
"""
artifacts.collect = [
    "filesystem:/tmp/signature.bin",
    "filesystem:/tmp/pubkey.bin",
    "spans:default"
]

[[expect.span]]
name = "ggen.crypto.sign"
attrs.all = {
    "algorithm" = "ML-DSA-65",
    "message_length" = 12,
    "signature_length" = 3309
}

# Scenario 2: Tamper with signature (XOR first byte with 0xFF)
[[scenario]]
name = "tamper_signature"
service = "ggen"
run = """
python3 -c "
import sys
sig = open('/tmp/signature.bin', 'rb').read()
tampered = bytes([sig[0] ^ 0xFF]) + sig[1:]
open('/tmp/signature_tampered.bin', 'wb').write(tampered)
print('Tampered signature created')
"
"""
depends_on = ["generate_valid_signature"]
artifacts.collect = ["filesystem:/tmp/signature_tampered.bin"]

# Scenario 3: Verify tampered signature (should fail)
[[scenario]]
name = "verify_tampered_signature"
service = "ggen"
run = """
cargo run -- crypto verify \
  --message "test message" \
  --signature /tmp/signature_tampered.bin \
  --pubkey /tmp/pubkey.bin
"""
depends_on = ["tamper_signature"]
expect_exit_code = 1  # Expect failure
artifacts.collect = ["spans:default", "stderr"]

# OTEL Validation: Verification span must exist
[[expect.span]]
name = "ggen.crypto.verify"
status = "ERROR"
attrs.all = {
    "algorithm" = "ML-DSA-65",
    "signature_valid" = false,
    "error.type" = "SignatureVerificationError",
    "tampering_detected" = true
}

# OTEL Validation: Error event emitted
[[expect.span]]
name = "ggen.crypto.verify"
events.all = ["signature_verification.failed"]
events.attrs = {
    "error.message.contains" = "tampered",
    "security_violation" = true
}

# OTEL Validation: Call graph
[expect.graph]
must_include = [
    ["ggen.crypto.sign", "ggen.crypto.keygen"],
    ["ggen.crypto.verify", "ggen.crypto.hash"]
]

# OTEL Validation: Status (error expected)
[expect.status]
any = "ERROR"

# OTEL Validation: Hermeticity
[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = {
    "service.name" = "ggen",
    "security.level" = "high"
}

# Validate temporal order
[expect.temporal]
sequence = [
    "ggen.crypto.sign",
    "ggen.crypto.verify"
]
```

**Benefits**:
- ✅ **Multi-scenario** test (generate → tamper → verify)
- ✅ **Security-focused** OTEL attributes
- ✅ **Error validation** ensures failures are detected
- ✅ **Tampering detection** attribute proves security logic ran

---

## 7. Templates Library

### Template 1: Basic CLI Command Test

```toml
# templates/basic_cli_command.clnrm.toml
[meta]
name = "{{ test_name }}"
version = "1.0.0"
description = "{{ test_description }}"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

[[scenario]]
name = "{{ scenario_name }}"
service = "ggen"
run = "{{ command }}"

[[expect.span]]
name = "{{ expected_span_name }}"
attrs.all = {{ expected_attributes }}

[expect.status]
all = "OK"
```

**Usage**:
```bash
# Instantiate template
clnrm template render \
  --template templates/basic_cli_command.clnrm.toml \
  --vars test_name="marketplace_search" \
         test_description="Test marketplace search" \
         scenario_name="search_rust" \
         command="ggen market search rust" \
         expected_span_name="ggen.marketplace.search" \
         expected_attributes='{"query":"rust"}' \
  --output tests/clnrm/marketplace_search.clnrm.toml
```

### Template 2: Multi-Phase Test

```toml
# templates/multi_phase_test.clnrm.toml
[meta]
name = "{{ test_name }}"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

{% for phase in phases %}
[[scenario]]
name = "{{ phase.name }}"
service = "ggen"
run = "{{ phase.command }}"
{% if phase.depends_on %}
depends_on = {{ phase.depends_on }}
{% endif %}

[[expect.span]]
name = "{{ phase.expected_span }}"
attrs.all = {{ phase.expected_attributes }}
{% endfor %}

[expect.temporal]
sequence = [
{% for phase in phases %}
    "{{ phase.expected_span }}"{% if not loop.last %},{% endif %}
{% endfor %}
]

[expect.status]
all = "OK"
```

**Usage**:
```bash
# Define phases in JSON
cat > lifecycle_phases.json <<'EOF'
{
  "test_name": "lifecycle_complete",
  "phases": [
    {
      "name": "init",
      "command": "ggen lifecycle run init",
      "expected_span": "ggen.lifecycle.init",
      "expected_attributes": {"success": true}
    },
    {
      "name": "build",
      "command": "ggen lifecycle run build",
      "depends_on": ["init"],
      "expected_span": "ggen.lifecycle.build",
      "expected_attributes": {"success": true}
    }
  ]
}
EOF

# Render template
clnrm template render \
  --template templates/multi_phase_test.clnrm.toml \
  --vars-file lifecycle_phases.json \
  --output tests/clnrm/lifecycle_complete.clnrm.toml
```

### Template 3: Security Test

```toml
# templates/security_test.clnrm.toml
[meta]
name = "{{ test_name }}_security"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "debug,ggen::crypto=trace" }

# Valid case
[[scenario]]
name = "{{ test_name }}_valid"
service = "ggen"
run = "{{ valid_command }}"

[[expect.span]]
name = "{{ expected_span }}"
status = "OK"
attrs.all = {{ valid_attributes }}

# Attack case
[[scenario]]
name = "{{ test_name }}_attack"
service = "ggen"
run = "{{ attack_command }}"
expect_exit_code = 1

[[expect.span]]
name = "{{ expected_span }}"
status = "ERROR"
attrs.all = {{ attack_attributes }}
events.all = ["security_violation"]

[expect.hermeticity]
no_external_services = true
resource_attrs.must_match = { "security.level" = "high" }
```

### Template 4: Property Test Matrix

```toml
# templates/property_test_matrix.clnrm.toml
[meta]
name = "{{ test_name }}_properties"
version = "1.0.0"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]

# Test matrix: {{ matrix_description }}
{% for test_case in test_matrix %}
[[scenario]]
name = "{{ test_name }}_{{ test_case.id }}"
service = "ggen"
run = "{{ test_case.command }}"

[[expect.span]]
name = "{{ expected_span }}"
attrs.all = {{ test_case.expected_attributes }}

# Property assertion
[[expect.span]]
name = "{{ expected_span }}"
attrs.custom = """
{{ test_case.property_assertion }}
"""
{% endfor %}

[expect.status]
all = "OK"
```

---

## 8. Success Criteria

### 8.1 Quantitative Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Test Coverage** | ≥42 integration tests converted | `ls tests/clnrm/*.clnrm.toml | wc -l` |
| **OTEL Span Coverage** | 100% critical paths instrumented | `clnrm analyze --coverage tests/clnrm/` |
| **Fake-Green Detection** | 0 false positives | `clnrm validate --fake-green-check tests/clnrm/` |
| **Test Execution Time** | <5 minutes for full suite | `clnrm run tests/clnrm/*.clnrm.toml --timing` |
| **CI/CD Integration** | Green builds on main branch | GitHub Actions badge |
| **Documentation** | 100% tests documented | All `.clnrm.toml` have `[meta]` sections |

### 8.2 Qualitative Criteria

✅ **Readability**: Non-developers can understand test intent from TOML
✅ **Maintainability**: Test changes require only TOML edits, not Rust code
✅ **Reproducibility**: Tests pass consistently across environments
✅ **Observability**: OTEL traces provide debugging insights
✅ **Security**: All security tests validate cryptographic operations

### 8.3 Acceptance Tests

**Test 1: Fake-Green Detection**:
```bash
# Create a fake-green test (intentionally broken)
cat > tests/clnrm/fake_green_test.clnrm.toml <<'EOF'
[[scenario]]
name = "fake_test"
run = "echo 'hello world'"

[[expect.span]]
name = "ggen.marketplace.search"  # Will never exist!
EOF

# clnrm should detect and fail
clnrm run tests/clnrm/fake_green_test.clnrm.toml
# Expected: FAIL - Span 'ggen.marketplace.search' not found
```

**Test 2: Full Suite Green**:
```bash
# Run entire test suite
clnrm run tests/clnrm/*.clnrm.toml --junit-xml results.xml

# All tests must pass
grep 'failures="0"' results.xml
```

**Test 3: CI/CD Integration**:
```bash
# Trigger CI pipeline
git push origin feature/clnrm-migration

# Check GitHub Actions
gh run list --workflow=test.yml --branch=feature/clnrm-migration
# Expected: ✅ All checks passed
```

---

## 9. Troubleshooting Guide

### Problem 1: Span Not Found

**Symptom**:
```
ERROR: Expected span 'ggen.marketplace.search' not found in traces
```

**Diagnosis**:
```bash
# Check if span is emitted
export RUST_LOG=trace
cargo run -- market search rust 2>&1 | grep "ggen.marketplace.search"

# Check OTEL collector
curl http://localhost:4318/v1/traces | jq '.resourceSpans[].scopeSpans[].spans[] | select(.name == "ggen.marketplace.search")'
```

**Solution**:
```rust
// Add instrumentation to code
#[instrument(name = "ggen.marketplace.search", skip(self))]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    // ... implementation
}
```

### Problem 2: Attribute Mismatch

**Symptom**:
```
ERROR: Expected attribute 'result_count' = 5, got 3
```

**Diagnosis**:
```bash
# Inspect actual attributes
clnrm debug --test tests/clnrm/marketplace_search.clnrm.toml --show-spans
```

**Solution**:
```toml
# Update expected attributes to match reality
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = { "result_count.gte" = 1 }  # Use >= instead of exact match
```

### Problem 3: Temporal Violation

**Symptom**:
```
ERROR: Span 'ggen.lifecycle.build' occurred before 'ggen.lifecycle.init'
```

**Diagnosis**:
```bash
# Visualize span timeline
clnrm timeline --test tests/clnrm/lifecycle_complete.clnrm.toml
```

**Solution**:
```toml
# Add explicit dependency
[[scenario]]
name = "lifecycle_build"
depends_on = ["lifecycle_init"]  # Enforce ordering
```

### Problem 4: Hermeticity Violation

**Symptom**:
```
ERROR: Unexpected external service call to 'api.attacker.com'
```

**Diagnosis**:
```bash
# Check network activity
clnrm debug --test tests/clnrm/marketplace_search.clnrm.toml --network-trace
```

**Solution**:
```toml
# Whitelist allowed hosts
[expect.hermeticity]
no_external_services = false
allowed_hosts = ["registry.ggen.io", "github.com"]
```

### Problem 5: Container Timeout

**Symptom**:
```
ERROR: Container timeout after 300s
```

**Diagnosis**:
```bash
# Check container logs
docker logs <container_id>
```

**Solution**:
```toml
# Increase timeout
[service.ggen]
timeout_seconds = 600  # 10 minutes
```

---

## 10. References

### 10.1 Documentation

- **CLNRM GitHub**: https://github.com/your-org/clnrm
- **CLNRM Docs**: https://docs.clnrm.io
- **OpenTelemetry Spec**: https://opentelemetry.io/docs/specs/otel/
- **Tera Template Engine**: https://keats.github.io/tera/

### 10.2 Internal Documents

- `docs/testing/clnrm-integration-analysis.md` - Detailed CLNRM analysis
- `docs/testing/otel_span_taxonomy.rs` - Span naming conventions
- `docs/testing/migration_priorities.md` - Test prioritization
- `ggen-core/tests/README.md` - Original test suite documentation

### 10.3 Related Files

**Existing Tests**:
- `ggen-core/tests/integration/registry_api_integration.rs`
- `ggen-core/tests/security/signature_verification.rs`
- `ggen-core/tests/property/search_properties.rs`

**CLNRM Tests** (after migration):
- `tests/clnrm/marketplace_search.clnrm.toml`
- `tests/clnrm/lifecycle_complete_flow.clnrm.toml`
- `tests/clnrm/crypto_signature_verification.clnrm.toml`

### 10.4 Tooling

**Required Tools**:
```bash
# Install clnrm
cargo install clnrm

# Install OTEL collector
docker pull otel/opentelemetry-collector-contrib

# Install Tera CLI
cargo install tera-cli
```

**Useful Commands**:
```bash
# Run single test
clnrm run tests/clnrm/marketplace_search.clnrm.toml

# Run all tests
clnrm run tests/clnrm/*.clnrm.toml

# Generate JUnit XML
clnrm run tests/clnrm/*.clnrm.toml --junit-xml results.xml

# Debug span traces
clnrm debug --test tests/clnrm/marketplace_search.clnrm.toml --show-spans

# Validate OTEL assertions
clnrm validate --test tests/clnrm/marketplace_search.clnrm.toml

# Check for fake-green tests
clnrm analyze --fake-green-check tests/clnrm/
```

---

## Appendix A: OTEL Instrumentation Checklist

### Core Spans to Instrument

- [ ] `ggen.cli.execute` - Top-level CLI command
- [ ] `ggen.cli.parse` - Argument parsing
- [ ] `ggen.marketplace.search` - Package search
- [ ] `ggen.marketplace.resolve` - Version resolution
- [ ] `ggen.marketplace.install` - Package installation
- [ ] `ggen.registry.init` - Registry initialization
- [ ] `ggen.registry.query` - Registry queries
- [ ] `ggen.registry.fetch` - Fetch registry index
- [ ] `ggen.package.download` - Download package
- [ ] `ggen.package.extract` - Extract package
- [ ] `ggen.package.verify` - Verify package integrity
- [ ] `ggen.lifecycle.init` - Initialize project
- [ ] `ggen.lifecycle.build` - Build project
- [ ] `ggen.lifecycle.test` - Test project
- [ ] `ggen.lifecycle.deploy` - Deploy project
- [ ] `ggen.crypto.sign` - Sign data
- [ ] `ggen.crypto.verify` - Verify signature
- [ ] `ggen.crypto.hash` - Hash data

### Span Attributes to Record

**Search spans**:
- `query` - Search query string
- `result_count` - Number of results
- `search_duration_ms` - Search duration

**Package spans**:
- `package_name` - Package identifier
- `version` - Package version
- `download_size_bytes` - Download size
- `checksum` - Package checksum

**Error spans**:
- `error.type` - Error type (e.g., "PackageNotFoundError")
- `error.message` - Error message
- `stack_trace` - Stack trace (if available)

**Security spans**:
- `algorithm` - Cryptographic algorithm
- `signature_valid` - Boolean validation result
- `tampering_detected` - Boolean tampering flag

---

## Appendix B: Timeline

```
Week 1: Analysis & Planning
├─ Day 1-2: Test inventory and categorization
├─ Day 3-4: OTEL span taxonomy design
└─ Day 5: Migration priorities and templates

Week 2-3: Instrumentation
├─ Day 6-8: Add OTEL dependencies and initialization
├─ Day 9-12: Instrument core functions
└─ Day 13-15: Validate span emission

Week 4-5: Conversion - Integration Tests
├─ Day 16-20: Convert 42 integration tests
├─ Day 21-25: Validate OTEL assertions
└─ Day 26-30: Fix discrepancies

Week 6: Conversion - Security Tests
├─ Day 31-35: Convert 65 security tests
└─ Day 36-38: Add crypto instrumentation

Week 7: Conversion - Property Tests
├─ Day 39-42: Convert 28 property tests
└─ Day 43-45: Tera template optimization

Week 8: Validation & Cleanup
├─ Day 46-48: Parallel execution validation
├─ Day 49-50: CI/CD pipeline updates
└─ Day 51-52: Documentation and cleanup
```

---

## Appendix C: Decision Records

### ADR-001: Use CLNRM for Integration Tests Only

**Status**: Accepted
**Date**: 2025-10-17

**Context**: We need to decide which tests to migrate to CLNRM TOML format.

**Decision**: Migrate only integration, security, and property tests. Keep unit tests in Rust.

**Rationale**:
- Unit tests are fast and easy to maintain in Rust
- Integration tests benefit most from OTEL validation
- Security tests need hermeticity guarantees
- Property tests need templating for test matrices

**Consequences**:
- 42 integration tests migrated
- 65 security tests migrated
- 28 property tests migrated
- 85 unit tests remain in Rust

### ADR-002: Use Generic Container Plugin

**Status**: Accepted
**Date**: 2025-10-17

**Context**: CLNRM supports multiple plugins (Docker, Kubernetes, generic container).

**Decision**: Use the generic container plugin for all ggen tests.

**Rationale**:
- Simplest integration path
- No Kubernetes required
- Docker already used in CI/CD
- Sufficient for ggen's needs

**Consequences**:
- All tests run in Docker containers
- OTEL collector required
- Reproducible across environments

### ADR-003: 7-Layer OTEL Validation

**Status**: Accepted
**Date**: 2025-10-17

**Context**: Need to prevent fake-green tests.

**Decision**: Implement all 7 validation layers for critical tests.

**Layers**:
1. Span existence
2. Attribute validation
3. Hierarchy validation
4. Event emission
5. Status validation
6. Temporal ordering
7. Hermeticity

**Rationale**:
- Comprehensive fake-green detection
- Proves tests actually execute
- Production-grade observability

**Consequences**:
- More verbose test definitions
- Higher confidence in test results
- Better debugging capabilities

---

**END OF DOCUMENT**

---

**Document Status**: ✅ Final
**Next Steps**:
1. Review and approve migration strategy
2. Begin Phase 1: Analysis & Planning
3. Set up OTEL infrastructure
4. Start instrumentation of ggen codebase

**Contact**: CLNRM Migration Architect
**Last Updated**: 2025-10-17
