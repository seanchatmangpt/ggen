# CLNRM Implementation Guide - Step-by-Step

**Audience**: Developers implementing CLNRM migration
**Last Updated**: 2025-10-17
**Difficulty**: Intermediate

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Step 1: Setup OTEL Infrastructure](#step-1-setup-otel-infrastructure)
3. [Step 2: Add OTEL to Ggen](#step-2-add-otel-to-ggen)
4. [Step 3: Instrument Functions](#step-3-instrument-functions)
5. [Step 4: Write First Test](#step-4-write-first-test)
6. [Step 5: Run and Validate](#step-5-run-and-validate)
7. [Step 6: Convert Existing Test](#step-6-convert-existing-test)
8. [Step 7: CI/CD Integration](#step-7-cicd-integration)

---

## Prerequisites

### Install Required Tools

```bash
# Install clnrm
cargo install clnrm

# Install Rust (if not already)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install Docker (for OTEL collector)
# macOS
brew install docker

# Linux
curl -fsSL https://get.docker.com -o get-docker.sh
sh get-docker.sh

# Verify installations
clnrm --version
cargo --version
docker --version
```

### Clone Ggen Repository

```bash
git clone https://github.com/your-org/ggen.git
cd ggen
```

---

## Step 1: Setup OTEL Infrastructure

### 1.1 Start OTEL Collector

```bash
# Create OTEL collector configuration
cat > otel-collector-config.yaml <<'EOF'
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318
      grpc:
        endpoint: 0.0.0.0:4317

processors:
  batch:
    timeout: 1s
    send_batch_size: 1024

exporters:
  logging:
    loglevel: debug
  file:
    path: ./traces.json

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [batch]
      exporters: [logging, file]
EOF

# Start OTEL collector
docker run -d --name otel-collector \
  -p 4317:4317 -p 4318:4318 \
  -v $(pwd)/otel-collector-config.yaml:/etc/otel-collector-config.yaml \
  otel/opentelemetry-collector-contrib:latest \
  --config=/etc/otel-collector-config.yaml

# Verify collector is running
docker ps | grep otel-collector
curl http://localhost:4318/v1/traces  # Should return 405 (method not allowed)
```

### 1.2 Test OTEL Collector

```bash
# Send test span
curl -X POST http://localhost:4318/v1/traces \
  -H "Content-Type: application/json" \
  -d '{
    "resourceSpans": [{
      "scopeSpans": [{
        "spans": [{
          "traceId": "5b8aa5a2d2c872e8321cf37308d69df2",
          "spanId": "051581bf3cb55c13",
          "name": "test.span",
          "kind": 1,
          "startTimeUnixNano": "1544712660000000000",
          "endTimeUnixNano": "1544712661000000000"
        }]
      }]
    }]
  }'

# Check logs
docker logs otel-collector | grep "test.span"
```

---

## Step 2: Add OTEL to Ggen

### 2.1 Update Cargo.toml

```toml
# ggen-core/Cargo.toml

[dependencies]
# Existing dependencies...
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["env-filter", "json"] }
tracing-opentelemetry = "0.21"
opentelemetry = { version = "0.21", features = ["trace", "metrics"] }
opentelemetry-otlp = { version = "0.14", features = ["trace", "metrics"] }
opentelemetry-semantic-conventions = "0.13"
```

### 2.2 Create Observability Module

```rust
// ggen-core/src/observability.rs

use opentelemetry::global;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::runtime::Tokio;
use opentelemetry_sdk::trace::{self, TracerProvider};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use anyhow::Result;

/// Initialize OpenTelemetry tracing
pub fn init_telemetry() -> Result<()> {
    // Create OTLP exporter
    let exporter = opentelemetry_otlp::new_exporter()
        .http()
        .with_endpoint("http://localhost:4318/v1/traces");

    // Create tracer provider
    let tracer_provider = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(exporter)
        .with_trace_config(
            trace::config()
                .with_resource(opentelemetry_sdk::Resource::new(vec![
                    opentelemetry::KeyValue::new("service.name", "ggen"),
                    opentelemetry::KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
                ]))
        )
        .install_batch(Tokio)
        .map_err(|e| anyhow::anyhow!("Failed to install tracer: {}", e))?;

    // Set global tracer provider
    global::set_tracer_provider(tracer_provider);

    // Create tracing layer
    let telemetry = tracing_opentelemetry::layer()
        .with_tracer(global::tracer("ggen"));

    // Initialize subscriber
    tracing_subscriber::registry()
        .with(telemetry)
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    Ok(())
}

/// Shutdown telemetry (call before exit)
pub fn shutdown_telemetry() {
    global::shutdown_tracer_provider();
}
```

### 2.3 Initialize OTEL in Main

```rust
// ggen-cli/src/main.rs

mod observability;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize telemetry
    observability::init_telemetry()?;

    // Existing CLI logic
    let result = run_cli().await;

    // Shutdown telemetry
    observability::shutdown_telemetry();

    result
}
```

### 2.4 Build and Test

```bash
# Build ggen with OTEL
cargo build --release

# Test OTEL is working
export RUST_LOG=info,ggen=trace
./target/release/ggen --version

# Check OTEL collector logs for spans
docker logs otel-collector | grep "ggen"
```

---

## Step 3: Instrument Functions

### 3.1 Instrument Marketplace Search

```rust
// ggen-core/src/marketplace/search.rs

use tracing::{instrument, info, error};
use std::time::Instant;

#[instrument(
    name = "ggen.marketplace.search",
    skip(self),
    fields(
        query = %query,
        result_count,
        search_duration_ms
    ),
    err
)]
pub async fn search(&self, query: &str) -> Result<Vec<SearchResult>> {
    let start = Instant::now();

    info!(query = %query, "Starting marketplace search");

    // Input validation
    if query.is_empty() {
        error!("Empty search query");
        return Err(anyhow::anyhow!("Query cannot be empty"));
    }

    // Execute search
    let results = self.registry
        .search(query)
        .await
        .map_err(|e| {
            error!(error = %e, "Registry search failed");
            e
        })?;

    // Record metrics
    let duration = start.elapsed();
    tracing::Span::current().record("result_count", results.len());
    tracing::Span::current().record("search_duration_ms", duration.as_millis());

    info!(
        result_count = results.len(),
        duration_ms = duration.as_millis(),
        "Search completed successfully"
    );

    Ok(results)
}
```

### 3.2 Instrument Registry Query

```rust
// ggen-core/src/registry/client.rs

use tracing::{instrument, info_span, event, Level};

#[instrument(
    name = "ggen.registry.query",
    skip(self),
    fields(
        endpoint = %endpoint,
        http_status,
        response_time_ms
    )
)]
async fn query_registry(&self, endpoint: &str) -> Result<Response> {
    let start = Instant::now();

    // HTTP request event
    event!(Level::INFO, "http.request", url = %endpoint);

    let response = self.http_client
        .get(endpoint)
        .send()
        .await?;

    // HTTP response event
    event!(Level::INFO, "http.response", status = response.status().as_u16());

    // Record attributes
    tracing::Span::current().record("http_status", response.status().as_u16());
    tracing::Span::current().record("response_time_ms", start.elapsed().as_millis());

    // Cache event
    if response.headers().contains_key("x-cache-hit") {
        event!(Level::INFO, "cache.hit");
    } else {
        event!(Level::INFO, "cache.miss");
    }

    Ok(response)
}
```

### 3.3 Instrument Lifecycle Operations

```rust
// ggen-core/src/lifecycle/mod.rs

#[instrument(
    name = "ggen.lifecycle.init",
    skip(path),
    fields(
        project_name = %project_name,
        environment = %environment,
        success
    )
)]
pub async fn init(path: &Path, project_name: &str, environment: &str) -> Result<()> {
    info!(project_name = %project_name, "Initializing project");

    // Create project structure
    create_project_structure(path, project_name).await?;

    // Record success
    tracing::Span::current().record("success", true);

    info!("Project initialized successfully");
    Ok(())
}

#[instrument(
    name = "ggen.lifecycle.build",
    skip(path),
    fields(
        target,
        build_duration_ms,
        success
    )
)]
pub async fn build(path: &Path, target: &str) -> Result<()> {
    let start = Instant::now();

    info!(target = %target, "Building project");

    // Execute cargo build
    let output = Command::new("cargo")
        .args(&["build", "--release", "--target", target])
        .current_dir(path)
        .output()?;

    // Record metrics
    let duration = start.elapsed();
    tracing::Span::current().record("target", target);
    tracing::Span::current().record("build_duration_ms", duration.as_millis());
    tracing::Span::current().record("success", output.status.success());

    if !output.status.success() {
        error!("Build failed: {}", String::from_utf8_lossy(&output.stderr));
        return Err(anyhow::anyhow!("Build failed"));
    }

    info!(duration_ms = duration.as_millis(), "Build completed successfully");
    Ok(())
}
```

### 3.4 Test Instrumentation

```bash
# Run ggen command
export RUST_LOG=trace
cargo run -- market search rust

# Check OTEL collector logs
docker logs otel-collector | grep "ggen.marketplace.search"

# Should see:
# - Span name: ggen.marketplace.search
# - Attributes: query=rust, result_count=N, search_duration_ms=N
```

---

## Step 4: Write First Test

### 4.1 Create Test File

```bash
mkdir -p tests/clnrm
```

### 4.2 Write Marketplace Search Test

```toml
# tests/clnrm/marketplace_search.clnrm.toml

[meta]
name = "marketplace_search_test"
version = "1.0.0"
description = "Verify marketplace search executes and returns results"
author = "Your Name <your.email@example.com>"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"
protocol = "http/protobuf"
sample_ratio = 1.0  # Capture all spans

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = {
    RUST_LOG = "info,ggen=trace",
    OTEL_EXPORTER_OTLP_ENDPOINT = "http://host.docker.internal:4318"
}

[[scenario]]
name = "search_rust_packages"
service = "ggen"
run = "cargo run --release -- market search rust"
artifacts.collect = ["spans:default", "stdout", "stderr"]

# OTEL Validation Layer 1: Span Existence
[[expect.span]]
name = "ggen.marketplace.search"
kind = "internal"

# OTEL Validation Layer 2: Attributes
[[expect.span]]
name = "ggen.marketplace.search"
attrs.all = {
    "query" = "rust",
    "result_count.gte" = 1,
    "search_duration_ms.lte" = 5000
}

# OTEL Validation Layer 3: Hierarchy
[[expect.span]]
name = "ggen.registry.query"
parent = "ggen.marketplace.search"

# OTEL Validation Layer 4: Events
[[expect.span]]
name = "ggen.registry.query"
events.any = ["http.request", "http.response"]

# OTEL Validation Layer 5: Status
[expect.status]
all = "OK"

# OTEL Validation Layer 6: Temporal
[expect.temporal]
sequence = ["ggen.marketplace.search", "ggen.registry.query"]

# OTEL Validation Layer 7: Hermeticity
[expect.hermeticity]
no_external_services = false  # Allow registry access
allowed_hosts = ["registry.ggen.io", "github.com"]
resource_attrs.must_match = {
    "service.name" = "ggen"
}
```

---

## Step 5: Run and Validate

### 5.1 Run Test

```bash
# Run clnrm test
clnrm run tests/clnrm/marketplace_search.clnrm.toml

# Expected output:
# ✅ marketplace_search_test: PASSED
#    ✅ Scenario: search_rust_packages
#    ✅ Span validation: ggen.marketplace.search (7 checks passed)
#    ✅ Span validation: ggen.registry.query (2 checks passed)
#    ✅ Status validation: all OK
#    ✅ Temporal validation: sequence correct
#    ✅ Hermeticity validation: passed
```

### 5.2 Debug Failures

```bash
# Show detailed span information
clnrm debug \
  --test tests/clnrm/marketplace_search.clnrm.toml \
  --show-spans

# Show timeline
clnrm timeline \
  --test tests/clnrm/marketplace_search.clnrm.toml

# Show collected artifacts
clnrm artifacts \
  --test tests/clnrm/marketplace_search.clnrm.toml \
  --show-all
```

### 5.3 View Traces in OTEL Collector

```bash
# Check traces.json
cat traces.json | jq '.resourceSpans[].scopeSpans[].spans[] | {
  name: .name,
  attributes: .attributes,
  status: .status
}'

# Example output:
# {
#   "name": "ggen.marketplace.search",
#   "attributes": [
#     {"key": "query", "value": {"stringValue": "rust"}},
#     {"key": "result_count", "value": {"intValue": "15"}},
#     {"key": "search_duration_ms", "value": {"intValue": "42"}}
#   ],
#   "status": {"code": "STATUS_CODE_OK"}
# }
```

---

## Step 6: Convert Existing Test

### 6.1 Identify Rust Test to Convert

```rust
// ggen-core/tests/integration/registry_api_integration.rs

#[tokio::test]
async fn test_list_all_packages() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let index_path = temp_dir.path().join("index.json");

    let mut packs = HashMap::new();

    for i in 0..20 {
        let id = format!("package-{}", i);
        // ... create test packages ...
    }

    let index = ggen_core::registry::RegistryIndex {
        updated: chrono::Utc::now(),
        packs,
    };

    fs::write(&index_path, serde_json::to_string_pretty(&index)?)?;

    let base_url = Url::from_file_path(temp_dir.path())
        .map_err(|_| anyhow::anyhow!("Failed to create file URL"))?;
    let client = RegistryClient::with_base_url(base_url)?;

    let packages = client.list_packages().await?;

    assert_eq!(packages.len(), 20);

    Ok(())
}
```

### 6.2 Create CLNRM TOML Version

```toml
# tests/clnrm/registry_list_packages.clnrm.toml

[meta]
name = "registry_list_packages_test"
version = "1.0.0"
description = "Verify registry lists all packages correctly"

[otel]
exporter = "otlp"
endpoint = "http://localhost:4318"

[service.ggen]
plugin = "generic_container"
image = "rust:latest"
workdir = "/app"
volumes = ["./:/app"]
environment = { RUST_LOG = "info,ggen=trace" }

# Setup: Create test registry
[[scenario]]
name = "setup_test_registry"
service = "ggen"
run = """
cat > /tmp/test_registry.json <<'EOF'
{
  "updated": "2025-10-17T00:00:00Z",
  "packs": {
    "package-0": {"id": "package-0", "name": "Package 0", ...},
    "package-1": {"id": "package-1", "name": "Package 1", ...},
    ...
    "package-19": {"id": "package-19", "name": "Package 19", ...}
  }
}
EOF
"""

# Execute: List packages
[[scenario]]
name = "list_packages"
service = "ggen"
run = "cargo run -- registry list --file /tmp/test_registry.json"
depends_on = ["setup_test_registry"]

# OTEL Validation
[[expect.span]]
name = "ggen.registry.list_packages"
attrs.all = {
    "total_packages" = 20,
    "packages_returned" = 20
}

[[expect.span]]
name = "ggen.registry.load_index"
parent = "ggen.registry.list_packages"

[expect.status]
all = "OK"

[expect.hermeticity]
no_external_services = true
```

### 6.3 Run Both Versions

```bash
# Run original Rust test
cargo test test_list_all_packages

# Run new CLNRM test
clnrm run tests/clnrm/registry_list_packages.clnrm.toml

# Compare results
echo "Both should pass!"
```

---

## Step 7: CI/CD Integration

### 7.1 Update GitHub Actions Workflow

```yaml
# .github/workflows/test.yml

name: Tests

on:
  push:
    branches: [ main, master ]
  pull_request:
    branches: [ main, master ]

jobs:
  unit-tests:
    name: Rust Unit Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          profile: minimal
          toolchain: stable
          override: true

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Run unit tests
        run: cargo test --lib --bins

  clnrm-tests:
    name: CLNRM Integration Tests
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Install clnrm
        run: cargo install clnrm

      - name: Start OTEL collector
        run: |
          docker run -d --name otel-collector \
            -p 4317:4317 -p 4318:4318 \
            otel/opentelemetry-collector-contrib:latest

      - name: Build ggen
        run: cargo build --release

      - name: Run clnrm tests
        run: |
          clnrm run tests/clnrm/*.clnrm.toml \
            --junit-xml clnrm-results.xml \
            --parallel 4

      - name: Upload test results
        uses: actions/upload-artifact@v3
        if: always()
        with:
          name: clnrm-results
          path: clnrm-results.xml

      - name: Publish test results
        uses: EnricoMi/publish-unit-test-result-action@v2
        if: always()
        with:
          files: clnrm-results.xml
```

### 7.2 Add Coverage Reporting

```yaml
# Add to .github/workflows/test.yml

  coverage:
    name: Test Coverage
    runs-on: ubuntu-latest
    needs: [unit-tests, clnrm-tests]
    steps:
      - uses: actions/checkout@v4

      - name: Install tarpaulin
        run: cargo install cargo-tarpaulin

      - name: Generate coverage
        run: |
          cargo tarpaulin \
            --out Xml \
            --output-dir coverage \
            --all-features

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/cobertura.xml
```

### 7.3 Test CI/CD Pipeline

```bash
# Commit changes
git add .
git commit -m "feat: Add CLNRM integration tests with OTEL validation"

# Push to branch
git push origin feature/clnrm-migration

# Check GitHub Actions
gh run list --workflow=test.yml

# View run details
gh run view --web
```

---

## Verification Checklist

After completing all steps:

- [ ] OTEL collector running and receiving traces
- [ ] Ggen instrumented with tracing spans
- [ ] First CLNRM test written and passing
- [ ] Existing Rust test converted to CLNRM
- [ ] Both versions produce same results
- [ ] CI/CD pipeline updated and green
- [ ] Test coverage maintained or improved
- [ ] Documentation updated

---

## Troubleshooting

### Problem: Spans not appearing in OTEL collector

**Solution**:
```bash
# Check environment variables
echo $OTEL_EXPORTER_OTLP_ENDPOINT

# Verify collector is running
docker ps | grep otel-collector

# Check collector logs
docker logs otel-collector

# Test with curl
curl -X POST http://localhost:4318/v1/traces \
  -H "Content-Type: application/json" \
  -d '{"resourceSpans": [...]}'
```

### Problem: Container can't reach OTEL collector

**Solution**:
```toml
# Use host.docker.internal on macOS/Windows
environment = {
    OTEL_EXPORTER_OTLP_ENDPOINT = "http://host.docker.internal:4318"
}

# Or use host network mode
[service.ggen]
network_mode = "host"
```

### Problem: Test timeout

**Solution**:
```toml
[service.ggen]
timeout_seconds = 600  # Increase timeout

# Or increase clnrm global timeout
[meta]
timeout_seconds = 600
```

---

## Next Steps

1. ✅ Complete this implementation guide
2. ⬜ Instrument remaining ggen functions
3. ⬜ Convert high-priority integration tests
4. ⬜ Set up continuous OTEL monitoring
5. ⬜ Document OTEL span taxonomy
6. ⬜ Create test templates
7. ⬜ Train team on CLNRM usage

---

## Resources

- **Full Strategy**: [CLNRM_MIGRATION_STRATEGY.md](./CLNRM_MIGRATION_STRATEGY.md)
- **Quick Reference**: [CLNRM_QUICK_REFERENCE.md](./CLNRM_QUICK_REFERENCE.md)
- **OTEL Docs**: https://opentelemetry.io/docs/
- **Tracing Docs**: https://docs.rs/tracing/

---

**Last Updated**: 2025-10-17
**Status**: Implementation Ready
