# Weaver Live-Check Integration

This document describes the integration of [OpenTelemetry Weaver](https://github.com/open-telemetry/weaver) live-check functionality into the KNHK OTEL crate.

## Overview

Weaver live-check validates telemetry data against semantic conventions, ensuring compliance with OpenTelemetry standards and custom schemas. This integration allows KNHK to:

1. **Validate telemetry in real-time** - Check spans and metrics against semantic conventions
2. **CI/CD integration** - Fail builds if telemetry violates conventions
3. **Developer feedback** - Get immediate feedback during development/debugging

## Architecture

```
┌─────────────┐         ┌──────────────┐         ┌──────────────┐
│   KNHK     │ OTLP    │    Weaver    │         │   Registry   │
│   Tracer    │────────>│  live-check  │<────────│  (Semantic   │
│             │         │   (gRPC)     │         │ Conventions) │
└─────────────┘         └──────────────┘         └──────────────┘
                              │
                              │ JSON/ANSI
                              ▼
                        ┌──────────────┐
                        │   Reports    │
                        │  (Output)    │
                        └──────────────┘
```

## Usage

### Basic Example

```rust
use knhk_otel::{Tracer, SpanStatus, WeaverLiveCheck, MetricsHelper};

// 1. Start Weaver live-check server
let weaver = WeaverLiveCheck::new()
    .with_registry("./schemas/my-registry".to_string())
    .with_otlp_port(4317)
    .with_format("json".to_string());

let mut weaver_process = weaver.start()?;

// 2. Configure tracer to export to Weaver
let mut tracer = Tracer::with_otlp_exporter(
    format!("http://{}", weaver.otlp_endpoint())
);

// 3. Generate and export telemetry
let span_ctx = tracer.start_span("knhk.operation.execute".to_string(), None);
tracer.end_span(span_ctx, SpanStatus::Ok);
tracer.export()?;

// 4. Stop Weaver
weaver.stop()?;
weaver_process.wait()?;
```

### CI/CD Integration

```bash
#!/bin/bash
# Run Weaver live-check in CI/CD

# Start Weaver in background
weaver registry live-check \
    --registry ./schemas/registry \
    --format json \
    --output ./weaver-reports \
    --inactivity-timeout 300 \
    & LIVE_CHECK_PID=$!

# Wait for Weaver to start
sleep 2

# Run tests that generate telemetry
cargo test --features std

# Stop Weaver
kill -HUP $LIVE_CHECK_PID
wait $LIVE_CHECK_PID

# Check exit code (non-zero = violations)
if [ $? -ne 0 ]; then
    echo "Telemetry validation failed!"
    cat ./weaver-reports/*.json
    exit 1
fi
```

## API Reference

### `WeaverLiveCheck`

Builder pattern for configuring and running Weaver live-check.

#### Methods

- `new()` - Create a new instance with default settings
- `with_registry(path: String)` - Set semantic convention registry path
- `with_otlp_address(address: String)` - Set OTLP gRPC address (default: 127.0.0.1)
- `with_otlp_port(port: u16)` - Set OTLP gRPC port (default: 4317)
- `with_admin_port(port: u16)` - Set HTTP admin port (default: 8080)
- `with_inactivity_timeout(timeout: u64)` - Set inactivity timeout in seconds (default: 60)
- `with_format(format: String)` - Set output format: "json" or "ansi" (default: "json")
- `with_output(path: String)` - Set output directory for reports
- `start()` - Start Weaver process, returns `std::process::Child`
- `stop()` - Stop Weaver via HTTP admin endpoint
- `otlp_endpoint()` - Get the OTLP endpoint address:port string

### `Tracer::export_to_weaver(endpoint: &str)`

Convenience method to export telemetry directly to a Weaver endpoint.

## Weaver Exit Codes

- `0` - No violations found, telemetry is compliant
- Non-zero - Violations found, telemetry does not conform to registry

## Report Format

Weaver produces JSON reports with the following structure:

```json
{
  "live_check_result": {
    "all_advice": [
      {
        "advice_level": "violation",
        "advice_type": "missing_attribute",
        "message": "Attribute `operation` is required but missing",
        "advice_context": {"attribute_name": "operation"},
        "signal_type": "span",
        "signal_name": "knhk.operation.execute"
      }
    ],
    "highest_advice_level": "violation"
  },
  "advice_level_counts": {
    "violation": 1,
    "improvement": 0,
    "information": 0
  },
  "total_entities": 1,
  "registry_coverage": 0.5
}
```

## Advice Levels

- **violation** - Critical issue, telemetry does not conform to registry
- **improvement** - Suggestion for better compliance
- **information** - Informational note about telemetry

## Requirements

1. **Weaver CLI** - Must be installed and available in PATH
   ```bash
   # Install from releases: https://github.com/open-telemetry/weaver/releases
   # Or build from source:
   git clone https://github.com/open-telemetry/weaver.git
   cd weaver
   cargo build --release
   ```

2. **Semantic Convention Registry** (optional) - Custom schema for validation
   - Default: Uses OpenTelemetry Semantic Conventions
   - Custom: Provide path to registry YAML files

## Integration Points

### With CI/CD

Add Weaver validation to your CI pipeline:

```yaml
# .github/workflows/telemetry-validation.yml
- name: Validate Telemetry
  run: |
    weaver registry live-check --format json --output ./reports &
    cargo test
    kill -HUP $!
    wait $!
```

### With Development

Run Weaver during development for real-time feedback:

```bash
# Terminal 1: Start Weaver
weaver registry live-check

# Terminal 2: Run your application
cargo run
```

## Best Practices

1. **Use custom registries** - Define your own semantic conventions for KNHK-specific telemetry
2. **Fail fast in CI** - Set exit code check to fail builds on violations
3. **Use JSON reports** - Parse reports programmatically for custom validation logic
4. **Monitor coverage** - Track `registry_coverage` metric to ensure comprehensive instrumentation

## See Also

- [Weaver Documentation](https://github.com/open-telemetry/weaver)
- [OpenTelemetry Semantic Conventions](https://opentelemetry.io/docs/specs/semconv/)
- [OTEL Rust SDK](https://docs.rs/opentelemetry/)

