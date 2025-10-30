# OpenTelemetry Instrumentation Guide

## Overview

Ggen is instrumented with OpenTelemetry (OTEL) to generate distributed traces for all operations. This enables validation of behavior using cleanroom (`clnrm`) tests.

## Architecture

### Trace Spans

Ggen generates the following span types:

#### Marketplace Operations

1. **`ggen.market.search`**
   - **Attributes:**
     - `query`: Search query string
     - `result_count`: Number of results returned
   - **Events:**
     - `searching marketplace`
     - `search completed`

2. **`ggen.market.advanced_search`**
   - **Attributes:**
     - `query`: Search query string
     - `category`: Optional category filter
     - `result_count`: Number of results returned
   - **Events:**
     - `advanced search`
     - `advanced search completed`

3. **`ggen.market.resolve`**
   - **Attributes:**
     - `pack_id`: Package identifier
     - `version`: Requested version (optional)
     - `resolved_version`: Actual resolved version
   - **Events:**
     - `resolving package`
     - `package resolved`

4. **`ggen.registry.fetch_index`**
   - **Attributes:**
     - `url`: Registry URL
   - **Parent of:** All market operations

#### Lifecycle Operations

1. **`ggen.lifecycle.phase`**
   - **Attributes:**
     - `phase`: Phase name (init, setup, build, test, deploy)
     - `duration_ms`: Execution duration in milliseconds
     - `status`: success or error
   - **Events:**
     - `lifecycle phase starting`
     - `lifecycle phase completed` (or `lifecycle phase failed`)

2. **`ggen.lifecycle.pipeline`**
   - **Attributes:**
     - `phases`: Array of phase names
     - `phase_count`: Number of phases
   - **Events:**
     - `starting lifecycle pipeline`

## Usage

### CLI Flags

Enable OpenTelemetry tracing with CLI flags:

```bash
# Basic usage with default endpoint
ggen --enable-otel market search "rust web"

# Custom OTLP endpoint
ggen --enable-otel --otel-endpoint http://localhost:4318 lifecycle run init

# Custom sample ratio (0.0 to 1.0)
ggen --enable-otel --otel-sample-ratio 0.1 market search "database"

# Environment variable
export OTEL_EXPORTER_OTLP_ENDPOINT=http://collector:4318
ggen --enable-otel lifecycle run test
```

### Environment Variables

- `OTEL_EXPORTER_OTLP_ENDPOINT`: OTLP collector endpoint (default: `http://localhost:4318`)

### Programmatic Usage

```rust
use ggen_core::telemetry::{init_telemetry, shutdown_telemetry, TelemetryConfig};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize telemetry
    let config = TelemetryConfig {
        endpoint: "http://localhost:4318".to_string(),
        service_name: "my-ggen-app".to_string(),
        sample_ratio: 1.0,
        console_output: true,
    };
    init_telemetry(config)?;

    // Your code here - spans will be automatically generated

    // Shutdown and flush
    shutdown_telemetry();
    Ok(())
}
```

## OTLP Collector Setup

### Using Jaeger (Recommended for Development)

```bash
# Run Jaeger all-in-one with OTLP receiver
docker run -d --name jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest

# View traces at http://localhost:16686
```

### Using OpenTelemetry Collector

```yaml
# otel-collector-config.yaml
receivers:
  otlp:
    protocols:
      http:
        endpoint: 0.0.0.0:4318
      grpc:
        endpoint: 0.0.0.0:4317

processors:
  batch:

exporters:
  logging:
    loglevel: debug
  jaeger:
    endpoint: jaeger:14250
    tls:
      insecure: true

service:
  pipelines:
    traces:
      receivers: [otlp]
      processors: [batch]
      exporters: [logging, jaeger]
```

```bash
docker run -d --name otel-collector \
  -p 4317:4317 \
  -p 4318:4318 \
  -v $(pwd)/otel-collector-config.yaml:/etc/otel-collector-config.yaml \
  otel/opentelemetry-collector:latest \
  --config=/etc/otel-collector-config.yaml
```

## Validation with Cleanroom (clnrm)

### Test Structure

```rust
use clnrm::prelude::*;

#[tokio::test]
async fn test_ggen_marketplace_search_tracing() {
    // Start OTLP collector container
    let collector = Container::new("jaegertracing/all-in-one")
        .with_env("COLLECTOR_OTLP_ENABLED", "true")
        .with_port(4318)
        .with_port(16686)
        .start()
        .await
        .expect("Failed to start collector");

    let endpoint = format!("http://localhost:{}", collector.port(4318));

    // Run ggen with OTEL enabled
    let output = Command::new("ggen")
        .args(&["--enable-otel", "--otel-endpoint", &endpoint, "market", "search", "rust"])
        .output()
        .await
        .expect("Failed to execute ggen");

    assert!(output.status.success());

    // Query Jaeger API for traces
    let traces = query_jaeger_traces(&endpoint, "ggen.market.search")
        .await
        .expect("Failed to query traces");

    // Validate span attributes
    assert_eq!(traces[0].span_name, "ggen.market.search");
    assert!(traces[0].attributes.contains_key("query"));
    assert_eq!(traces[0].attributes["query"], "rust");
    assert!(traces[0].attributes.contains_key("result_count"));

    // Validate span hierarchy
    assert!(traces[0].has_child("ggen.registry.fetch_index"));
}
```

## Span Attributes Reference

### Common Attributes

All spans include:
- `service.name`: `ggen` or `ggen-cli`
- `service.version`: Ggen version from `Cargo.toml`

### Operation-Specific Attributes

| Span Name | Attributes | Events |
|-----------|------------|--------|
| `ggen.market.search` | `query`, `result_count` | `searching marketplace`, `search completed` |
| `ggen.market.advanced_search` | `query`, `category`, `result_count` | `advanced search`, `advanced search completed` |
| `ggen.market.resolve` | `pack_id`, `version`, `resolved_version` | `resolving package`, `package resolved` |
| `ggen.registry.fetch_index` | `url` | - |
| `ggen.lifecycle.phase` | `phase`, `duration_ms`, `status` | `lifecycle phase starting`, `lifecycle phase completed` |
| `ggen.lifecycle.pipeline` | `phases`, `phase_count` | `starting lifecycle pipeline` |

## Best Practices

1. **Always enable OTEL in CI/CD**: Use `--enable-otel` flag in production pipelines
2. **Use appropriate sample ratios**: Set `--otel-sample-ratio` to reduce overhead in high-traffic scenarios
3. **Set service names**: Use unique service names for different deployments
4. **Monitor span duration**: Track `duration_ms` attributes for performance analysis
5. **Validate span hierarchies**: Ensure child spans are properly nested under parents

## Troubleshooting

### No Spans Appearing

1. Check OTLP collector is running:
   ```bash
   curl http://localhost:4318/v1/traces
   ```

2. Verify endpoint configuration:
   ```bash
   ggen --enable-otel --otel-endpoint http://localhost:4318 market search test
   ```

3. Check logs for errors:
   ```bash
   RUST_LOG=debug ggen --enable-otel market search test
   ```

### Spans Missing Attributes

- Ensure you're using the latest ggen version
- Check that operations complete successfully (errors may prevent attribute recording)
- Verify OTEL initialization succeeded

### Performance Impact

- Use `--otel-sample-ratio 0.1` to sample 10% of requests
- Disable console output in production: set `console_output: false` in config
- Use batch exporters (already configured by default)

## References

- [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/)
- [OTLP Protocol](https://opentelemetry.io/docs/specs/otel/protocol/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [Cleanroom Testing Framework](https://github.com/seanchatmangpt/cleanroom)
