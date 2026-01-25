# TPS Reference System - Production-Ready Implementation

**A complete, observable, production-ready implementation of Toyota Production System (TPS) principles in Rust.**

## Quick Start (60 seconds)

```bash
# Start the complete stack (all services)
make up

# Check health
make health

# Send a signal (in another terminal)
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"demo"}}'

# View metrics
curl http://localhost:8080/metrics | jq .

# Open dashboards
make dashboard    # Grafana
make traces       # Jaeger
```

## Services

| Service | Port | Purpose | URL |
|---------|------|---------|-----|
| **TPS System** | 8080 | Signal processing API | http://localhost:8080 |
| **Metrics** | 9090 | Prometheus metrics | http://localhost:9090 |
| **Prometheus** | 9091 | Metrics database | http://localhost:9091 |
| **Grafana** | 3000 | Dashboards | http://localhost:3000 (admin/admin) |
| **Jaeger** | 16686 | Distributed tracing | http://localhost:16686 |
| **NATS** | 4222 | Message queue | nats://localhost:4222 |
| **RabbitMQ** | 5672 | Fallback queue | amqp://guest:guest@localhost:5672/ |
| **Loki** | 3100 | Log aggregation | http://localhost:3100 |

## The 6 TPS Principles

### 1. **Jidoka** (Autonomation)
Automatic circuit breaker detects service overload and isolates faults:
- Open circuit when failures exceed threshold
- Prevent cascade failures
- Graceful recovery

### 2. **Kanban** (Pull-Based Work)
Work pulled only when capacity available:
- Queue-based signal processing
- NATS for messaging
- RabbitMQ fallback

### 3. **Andon** (Visual Signals)
Real-time status indicators for problem visibility:
- RED: Critical (circuit open)
- YELLOW: Warning (high error rate)
- GREEN: Healthy

### 4. **Kaizen** (Continuous Improvement)
Comprehensive metrics for ongoing improvement:
- Success/error tracking
- Performance percentiles (p95, p99)
- Error rate trends

### 5. **Heijunka** (Level Loading)
Workload balanced across worker pool:
- Automatic load distribution
- Prevents worker overload
- Efficient capacity utilization

### 6. **Tracing** (Observability)
End-to-end request visibility:
- Trace IDs for correlation
- Jaeger integration
- Full request lifecycle

## Commands

### Stack Management
```bash
make up          # Start all services
make down        # Stop all services
make restart     # Restart everything
make clean       # Remove volumes and containers
make logs        # Tail all logs
```

### Testing & Development
```bash
make test        # Run all integration tests
make build       # Build Docker image
make run-local   # Run locally (no Docker)
```

### Load Testing
```bash
make load        # Normal load (100 req/s)
make spike       # Traffic spike (1000 req/s)
make stress      # Sustained stress (500 req/s)
make chaos       # Kill components (resilience test)
```

### Observation
```bash
make health      # Check service health
make status      # Show system status
make metrics     # Get latest metrics
make dashboard   # Open Grafana
make traces      # Open Jaeger
```

## API Examples

### Send a Signal
```bash
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{
    "signal_type": "execute",
    "priority": 2,
    "payload": {"data": "example"},
    "timeout_ms": 30000
  }'
```

### Check Health
```bash
curl http://localhost:8080/health | jq .
```

### Get Metrics
```bash
curl http://localhost:8080/metrics | jq .
```

### Get System Info
```bash
curl http://localhost:8080/info | jq .
```

## Configuration

### Environment Variables

See `.env.example` for all available options:

```bash
cp .env.example .env
# Edit .env with your values
export $(cat .env | xargs)
make up
```

### Key Configuration Parameters

```bash
# Worker configuration
TPS_NUM_WORKERS=4              # Number of worker threads

# Circuit breaker
TPS_CB_THRESHOLD=5             # Failures before circuit opens
TPS_CB_TIMEOUT=30              # Seconds before recovery attempt

# Queue configuration
TPS_KANBAN_BUFFER=1000         # Max queue depth

# Observability
RUST_LOG=debug                 # Log level
JAEGER_ENDPOINT=...            # Tracing endpoint
```

## Performance Profiles

### Development
```bash
TPS_NUM_WORKERS=2
TPS_CB_THRESHOLD=3
TPS_KANBAN_BUFFER=100
```

### Staging
```bash
TPS_NUM_WORKERS=4
TPS_CB_THRESHOLD=5
TPS_KANBAN_BUFFER=500
```

### Production
```bash
TPS_NUM_WORKERS=8
TPS_CB_THRESHOLD=10
TPS_KANBAN_BUFFER=2000
```

## Troubleshooting

### Services won't start
```bash
# Check Docker
docker-compose ps

# View logs
make logs

# Rebuild
docker-compose build --no-cache
make up
```

### Circuit breaker open
```bash
# Check health
curl http://localhost:8080/health

# Wait for reset (default 30s)
sleep 35

# Or restart
docker-compose restart tps-system
```

### Queue growing unbounded
```bash
# Check queue depth
curl http://localhost:8080/status | jq '.health.queue_depth'

# Increase workers
export TPS_NUM_WORKERS=8
docker-compose restart tps-system
```

### Port already in use
```bash
# Find process using port
lsof -i :8080

# Use different port
export HTTP_PORT=8081
make up
```

## Architecture

```
┌──────────────────────────────────┐
│   HTTP API (Port 8080)           │
│   /signal  /health  /metrics     │
└────────────┬─────────────────────┘
             │
  ┌──────────┼──────────┬───────────┬────────┐
  │          │          │           │        │
  ▼          ▼          ▼           ▼        ▼
Jidoka    Kanban     Andon      Kaizen   Heijunka
Circuit   Queue      Signals    Metrics  Loading
Breaker   (NATS)     (Logs)     (Prom)   (Pool)
  │          │          │           │        │
  └──────────┼──────────┴───────────┴────────┘
             │
    ┌────────┴────────┐
    │                 │
    ▼                 ▼
Prometheus        Jaeger
Metrics DB        Tracing
    │                 │
    └─────┬───────────┘
          │
    ┌─────▼──────┐
    │  Grafana   │
    │ Dashboards │
    └────────────┘
```

## Load Test Results

### Normal Load (100 req/s)
- Success rate: > 99%
- Avg latency: 40-60ms
- P99 latency: 100-150ms
- Circuit: Closed

### Spike (1000 req/s)
- Queue depth: Increases initially
- Recovery: Within 30-60 seconds
- Errors: < 5%
- Circuit: Holds or briefly opens

### Stress (500 req/s, 30s)
- Throughput: 400-450 req/s
- Latency: Increases gradually
- Error rate: < 1%
- Memory: Stable

## Integration

### Use in Your Project

1. Add to Cargo.toml:
```toml
[dependencies]
tps-reference = { path = "../path/to/crates/tps-reference" }
```

2. In your code:
```rust
use tps_reference::{TpsConfig, TpsSystem};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = TpsConfig::from_env()?;
    let system = TpsSystem::new(config).await?;
    system.start().await?;

    // Use system...
    let result = system.process_signal("validate", json!({})).await?;

    system.shutdown().await?;
    Ok(())
}
```

## Documentation

See `/docs/tps-reference/99-reference-implementation.md` for:
- Complete architecture
- API reference
- Configuration guide
- Load testing procedures
- Troubleshooting guide
- Advanced customization

## Testing

Run all integration tests:
```bash
make test

# Or specific test
cargo test --test integration_test test_scenario_normal_operation
```

Tests cover:
1. Normal operation (signal → result)
2. Circuit breaker behavior
3. Queue management
4. Load balancing
5. End-to-end tracing
6. Metrics collection
7. Andon signals
8. Concurrent processing
9. Health status
10. Recovery behavior

## Performance Characteristics

- **Throughput**: 400-1000 req/s (depending on hardware)
- **Latency**: p99 < 200ms under normal load
- **Queue drain time**: < 60 seconds after overload
- **Circuit recovery**: 30 seconds by default
- **Memory**: < 200MB steady state
- **CPU**: 2-4 cores typical usage

## Contributing

1. Fork the repository
2. Create feature branch
3. Add tests
4. Ensure `cargo test` passes
5. Submit pull request

## License

MIT - See LICENSE file

## References

- [ggen Repository](https://github.com/seanchatmangpt/ggen)
- [TPS Reference Docs](/docs/tps-reference/99-reference-implementation.md)
- [Rust Documentation](https://doc.rust-lang.org/)
- [Toyota Production System](https://en.wikipedia.org/wiki/Toyota_production_system)

---

**Questions?** Check the [troubleshooting guide](docs/tps-reference/99-reference-implementation.md#troubleshooting) or open an issue.
