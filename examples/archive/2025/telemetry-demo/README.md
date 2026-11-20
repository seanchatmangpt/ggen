# OpenTelemetry Instrumentation Demo

This example demonstrates ggen's OpenTelemetry instrumentation capabilities.

## Prerequisites

1. **Docker** - For running Jaeger
2. **Rust** - For building ggen
3. **curl** - For verifying the setup

## Quick Start

### 1. Start Jaeger (OTLP Collector)

```bash
docker run -d --name jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4317:4317 \
  -p 4318:4318 \
  jaegertracing/all-in-one:latest
```

Verify Jaeger is running:
```bash
curl http://localhost:16686/
```

### 2. Build ggen with OTEL Support

```bash
cd /path/to/ggen
cargo build --release
```

### 3. Run ggen with OpenTelemetry

```bash
# Marketplace search with tracing
./target/release/ggen --enable-otel market search "rust web"

# Lifecycle operations with tracing
./target/release/ggen --enable-otel lifecycle run init

# Custom OTLP endpoint
./target/release/ggen --enable-otel \
  --otel-endpoint http://localhost:4318 \
  market search "database"

# Production mode with sampling (10% of requests)
./target/release/ggen --enable-otel \
  --otel-sample-ratio 0.1 \
  lifecycle run deploy
```

### 4. View Traces in Jaeger UI

Open your browser to: **http://localhost:16686**

1. Select **Service**: `ggen-cli`
2. Click **Find Traces**
3. Explore the trace details

## Expected Spans

### Marketplace Search Operation

```
Service: ggen-cli
├── Span: ggen.market.search
│   ├── Attributes:
│   │   ├── query: "rust web"
│   │   └── result_count: 15
│   ├── Events:
│   │   ├── searching marketplace
│   │   └── search completed
│   └── Child Span: ggen.registry.fetch_index
│       └── Attributes:
│           └── url: "https://registry.ggen.dev/index.json"
```

### Lifecycle Pipeline Operation

```
Service: ggen-cli
├── Span: ggen.lifecycle.pipeline
│   ├── Attributes:
│   │   ├── phases: ["init", "setup", "build", "test", "deploy"]
│   │   └── phase_count: 5
│   ├── Events:
│   │   └── starting lifecycle pipeline
│   └── Child Spans:
│       ├── ggen.lifecycle.phase (init)
│       │   ├── Attributes:
│       │   │   ├── phase: "init"
│       │   │   ├── duration_ms: 1234
│       │   │   └── status: "success"
│       │   └── Events:
│       │       ├── lifecycle phase starting
│       │       └── lifecycle phase completed
│       ├── ggen.lifecycle.phase (setup)
│       ├── ggen.lifecycle.phase (build)
│       ├── ggen.lifecycle.phase (test)
│       └── ggen.lifecycle.phase (deploy)
```

## Advanced Usage

### Environment Variables

```bash
# Set OTLP endpoint via environment
export OTEL_EXPORTER_OTLP_ENDPOINT=http://collector.example.com:4318

# Run without explicit --otel-endpoint flag
ggen --enable-otel market search "rust"
```

### Custom Sampling

```bash
# Sample 100% of requests (default)
ggen --enable-otel --otel-sample-ratio 1.0 market search "rust"

# Sample 10% of requests (production)
ggen --enable-otel --otel-sample-ratio 0.1 market search "rust"

# Sample 1% of requests (high traffic)
ggen --enable-otel --otel-sample-ratio 0.01 market search "rust"
```

### CI/CD Integration

```yaml
# .github/workflows/ci.yml
jobs:
  test:
    runs-on: ubuntu-latest
    services:
      jaeger:
        image: jaegertracing/all-in-one:latest
        env:
          COLLECTOR_OTLP_ENABLED: true
        ports:
          - 4318:4318
          - 16686:16686

    steps:
      - uses: actions/checkout@v3

      - name: Build ggen
        run: cargo build --release

      - name: Run with OTEL
        run: |
          ./target/release/ggen --enable-otel \
            --otel-endpoint http://localhost:4318 \
            market search "rust"

      - name: Query traces
        run: |
          curl http://localhost:16686/api/traces?service=ggen-cli
```

## Troubleshooting

### No Traces Appearing

1. **Check Jaeger is running:**
   ```bash
   docker ps | grep jaeger
   curl http://localhost:4318/v1/traces
   ```

2. **Verify ggen OTEL is enabled:**
   ```bash
   ggen --enable-otel --help
   ```

3. **Check logs for errors:**
   ```bash
   RUST_LOG=debug ggen --enable-otel market search "test"
   ```

### Connection Refused

```bash
# Ensure Jaeger is listening on 4318
netstat -an | grep 4318

# Try explicit endpoint
ggen --enable-otel --otel-endpoint http://localhost:4318 market search "test"
```

### Spans Missing Attributes

- Ensure operations complete successfully
- Check ggen version: `ggen --version`
- Verify trace sampling is not too low

## Performance Benchmarking

Compare performance with and without OTEL:

```bash
# Without OTEL
time ggen market search "rust"

# With OTEL (100% sampling)
time ggen --enable-otel --otel-sample-ratio 1.0 market search "rust"

# With OTEL (10% sampling)
time ggen --enable-otel --otel-sample-ratio 0.1 market search "rust"
```

Expected overhead:
- **100% sampling**: 1-2% overhead
- **10% sampling**: <0.5% overhead
- **1% sampling**: <0.1% overhead

## Cleanup

```bash
# Stop and remove Jaeger
docker stop jaeger
docker rm jaeger

# Clean up Docker images (optional)
docker rmi jaegertracing/all-in-one:latest
```

## Resources

- [OpenTelemetry Docs](https://opentelemetry.io/docs/)
- [Jaeger Documentation](https://www.jaegertracing.io/docs/)
- [ggen OTEL Guide](../../docs/telemetry/OTEL_INSTRUMENTATION.md)
- [Cleanroom Testing](https://github.com/seanchatmangpt/cleanroom)

## Next Steps

1. **Integrate with production**: Use dedicated OTLP collector
2. **Set up alerting**: Monitor span error rates
3. **Analyze performance**: Use trace data to optimize bottlenecks
4. **Validate with clnrm**: Run cleanroom tests against trace data
