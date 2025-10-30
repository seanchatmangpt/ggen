# OpenTelemetry Validation Guide

## Overview

This guide explains how to use OpenTelemetry instrumentation to validate that all README capabilities work correctly end-to-end.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                 README Capabilities                      │
│  (Quickstart, Doctor, AI, Lifecycle, Marketplace, etc.) │
└───────────────────────┬─────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────┐
│           OpenTelemetry Instrumentation                  │
│  • Spans for each operation                             │
│  • Metrics for performance                              │
│  • Attributes for context                               │
└───────────────────────┬─────────────────────────────────┘
                        │
                        ▼
┌─────────────────────────────────────────────────────────┐
│              Trace Validation Framework                  │
│  • Collect traces                                       │
│  • Assert expected spans                                │
│  • Validate performance SLOs                            │
│  • Generate proof reports                               │
└─────────────────────────────────────────────────────────┘
```

## Quick Start

### 1. Run Validation Tests

```bash
# Run all OpenTelemetry validation tests
cargo test --test otel_validation_tests

# Run with trace output
RUST_LOG=info cargo test --test otel_validation_tests -- --nocapture

# Run with OTLP collector (optional)
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
cargo test --test otel_validation_tests
```

### 2. Run Automated Validation Script

```bash
# Complete validation with report generation
./scripts/validate-readme.sh
```

This will:
- ✅ Run all capability validation tests
- ✅ Collect OpenTelemetry traces
- ✅ Validate performance SLOs
- ✅ Generate markdown report
- ✅ Store results in `docs/validation/`

## Validated Capabilities

### ✅ Quickstart (2-Minute Setup)
- **Span:** `ggen.quickstart`
- **Assertion:** Completes in <120s
- **Test:** `test_readme_quickstart_capability`

### ✅ Doctor Command
- **Span:** `ggen.doctor`
- **Assertion:** Checks all prerequisites
- **Test:** `test_readme_doctor_capability`

### ✅ Lifecycle Commands
- **Span:** `ggen.lifecycle.list`
- **Assertion:** Lists all phases
- **Test:** `test_readme_lifecycle_capability`

### ✅ Marketplace Search
- **Span:** `ggen.marketplace.search`
- **Assertion:** Returns results in <5s
- **Test:** `test_readme_marketplace_capability`

### ✅ AI-Powered Generation
- **Span:** `ggen.ai.generate`
- **Assertion:** Generates valid template
- **Test:** `test_readme_ai_generation`

### ✅ GitHub Integration
- **Span:** `ggen.github.pages`
- **Assertion:** API calls succeed
- **Test:** `test_readme_github_capability`

### ✅ Generation Performance
- **Span:** `ggen.generate`
- **Assertion:** <3s per generation (SLO)
- **Test:** `test_readme_generation_performance_slo`

### ✅ Deterministic Output
- **Span:** `ggen.generate.deterministic`
- **Assertion:** Byte-identical outputs
- **Test:** `test_readme_deterministic_output`

## Framework Components

### 1. Trace Collector (`tests/otel_validation/mod.rs`)

```rust
use ggen_core::telemetry::{init_telemetry, TelemetryConfig};

let ctx = ValidationContext::new();
ctx.init()?;

// Run capability tests...

ctx.collector.assert_span_exists("ggen.quickstart")?;
ctx.collector.assert_span_success("ggen.quickstart")?;
ctx.collector.assert_duration_under("ggen.quickstart", 120_000.0)?;

ctx.shutdown();
```

### 2. Capability Validators (`tests/otel_validation/capabilities.rs`)

Each README capability has a dedicated validator:

```rust
#[instrument(name = "validate.quickstart")]
pub async fn validate_quickstart(ctx: &ValidationContext) -> Result<ValidationResult> {
    // Run command
    let output = Command::new("cargo")
        .args(&["run", "--", "quickstart", "demo", "--dry-run"])
        .output()?;

    // Validate traces
    ctx.collector.assert_span_exists("ggen.quickstart")?;
    ctx.collector.assert_duration_under("ggen.quickstart", 120_000.0)?;

    Ok(ValidationResult::success(...))
}
```

### 3. Trace Assertions (`tests/otel_validation/collectors.rs`)

Fluent API for trace validation:

```rust
let asserter = TraceAsserter::new(&collector);

asserter
    .span_exists("ggen.generate")?
    .span_succeeded("ggen.generate")?
    .span_duration_under("ggen.generate", 3000.0)?
    .span_has_attribute("ggen.generate", "template", "rust-module")?
    .metric_in_range("memory_usage_mb", 0.0, 100.0)?;
```

### 4. Report Generation (`tests/otel_validation/validators.rs`)

```rust
let report = generate_validation_report(&ctx).await?;

// Print to console
report.print_summary();

// Generate markdown
let markdown = report.to_markdown();
std::fs::write("docs/validation/RESULTS.md", markdown)?;
```

## Performance SLO Validation

All performance SLOs from the README are validated with traces:

| SLO | Expected | Validated Span | Test |
|-----|----------|----------------|------|
| Generation Time | <3s | `ggen.generate` | ✅ |
| Memory Usage | <100MB | Metrics | ✅ |
| Quickstart Time | <2min | `ggen.quickstart` | ✅ |
| RDF Processing | <5s | `ggen.graph.parse` | ✅ |
| Marketplace Search | <5s | `ggen.marketplace.search` | ✅ |

## Integration with CI/CD

Add to your CI pipeline:

```yaml
# .github/workflows/validation.yml
- name: Run OpenTelemetry Validation
  run: |
    cargo test --test otel_validation_tests
    ./scripts/validate-readme.sh

- name: Upload Validation Report
  uses: actions/upload-artifact@v3
  with:
    name: validation-report
    path: docs/validation/VALIDATION_RESULTS.md
```

## Custom Validations

Add new capability validators:

```rust
// tests/otel_validation/capabilities.rs

#[instrument(name = "validate.my_capability")]
pub async fn validate_my_capability(ctx: &ValidationContext) -> Result<ValidationResult> {
    let start = Instant::now();
    let mut errors = Vec::new();

    // Run your command
    let output = Command::new("ggen")
        .args(&["my-command", "--args"])
        .output()?;

    // Validate traces
    if let Err(e) = ctx.collector.assert_span_exists("ggen.my_command") {
        errors.push(e.to_string());
    }

    // Validate performance
    if let Err(e) = ctx.collector.assert_duration_under("ggen.my_command", 1000.0) {
        errors.push(e.to_string());
    }

    let duration = start.elapsed().as_secs_f64() * 1000.0;

    if errors.is_empty() {
        Ok(ValidationResult::success("My Capability".to_string(), duration, 1, 0))
    } else {
        Ok(ValidationResult::failure("My Capability".to_string(), duration, errors))
    }
}
```

Then add to validators:

```rust
// tests/otel_validation/validators.rs

pub async fn validate_all_capabilities(ctx: &ValidationContext) -> Result<Vec<ValidationResult>> {
    let mut results = Vec::new();

    // ... existing validations ...
    results.push(validate_my_capability(ctx).await?);

    Ok(results)
}
```

## Viewing Traces

### With OTLP Collector (Jaeger)

```bash
# Start Jaeger all-in-one
docker run -d --name jaeger \
  -p 16686:16686 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest

# Run tests with OTLP export
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
cargo test --test otel_validation_tests

# View traces at http://localhost:16686
```

### In-Memory (Test Mode)

Tests run in-memory by default for CI/CD:

```rust
let ctx = ValidationContext::new();
// Traces collected in ctx.collector
let spans = ctx.collector.get_spans();
```

## Troubleshooting

### No Traces Collected

**Problem:** Tests run but no spans found.

**Solution:** Ensure instrumentation is enabled:

```rust
use tracing::instrument;

#[instrument(name = "ggen.my_command")]
pub fn my_command() {
    // Your code
}
```

### OTLP Connection Failed

**Problem:** `Failed to install OTLP tracer`

**Solution:** Either:
1. Start OTLP collector (Jaeger/Prometheus)
2. Remove `OTEL_EXPORTER_OTLP_ENDPOINT` (uses in-memory)

### Span Not Found

**Problem:** `Expected span 'X' not found`

**Solution:**
1. Check span name matches exactly
2. Verify command actually ran
3. Check RUST_LOG level includes span

## Best Practices

1. **Instrument All Critical Paths**
   ```rust
   #[instrument(name = "ggen.capability")]
   pub fn capability() { ... }
   ```

2. **Add Context Attributes**
   ```rust
   tracing::info!(template = "rust-module", "generating code");
   ```

3. **Validate Performance**
   ```rust
   ctx.collector.assert_duration_under("span", max_ms)?;
   ```

4. **Test Success AND Failure**
   ```rust
   ctx.collector.assert_span_success("span")?; // For success
   // For expected errors, check error span
   ```

5. **Keep Tests Fast**
   - Use `--dry-run` where possible
   - Mock external services
   - Run in parallel where safe

## Next Steps

1. Add more capability validators
2. Integrate with performance monitoring
3. Add regression testing
4. Create dashboard for trends
5. Automate validation reports

## References

- [OpenTelemetry Tracing](https://opentelemetry.io/docs/instrumentation/rust/getting-started/)
- [ggen Telemetry Module](../../ggen-core/src/telemetry.rs)
- [Validation Tests](../../tests/otel_validation_tests.rs)
- [README Capabilities](../../README.md)
