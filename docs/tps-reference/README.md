# TPS Reference Documentation

## Overview

This directory contains comprehensive documentation for the **Toyota Production System (TPS) Reference Implementation** - a production-ready system demonstrating all 6 TPS principles integrated into a single observable, resilient system.

## Quick Navigation

### Getting Started
- **[99-reference-implementation.md](99-reference-implementation.md)** - Complete guide (2000+ lines)
  - Prerequisites and installation
  - Quick start (60 seconds)
  - Configuration guide
  - API reference
  - Load testing procedures
  - Troubleshooting

### Key Sections

#### Architecture
- System components overview
- Data flow diagrams
- Supervision tree design
- Service integration

#### Running the System
- Docker Compose stack setup
- Health checks
- Graceful shutdown
- Service dependencies

#### The 6 TPS Principles

1. **Jidoka** (Autonomation)
   - Circuit breaker implementation
   - Fault isolation strategy
   - Recovery mechanisms

2. **Kanban** (Pull-Based Work)
   - Queue-based signal processing
   - NATS integration
   - RabbitMQ fallback

3. **Andon** (Visual Signals)
   - Signal recording
   - Status indicators
   - Alert system

4. **Kaizen** (Continuous Improvement)
   - Metrics collection
   - Performance analysis
   - Percentile calculation

5. **Heijunka** (Level Loading)
   - Worker pool distribution
   - Load balancing
   - Rebalancing logic

6. **Tracing** (Observability)
   - Request correlation
   - Trace ID propagation
   - Jaeger integration

#### Operations & Monitoring
- `make` commands reference
- Health checks
- Metrics collection
- Log aggregation
- Distributed tracing
- Dashboard access

#### Load Testing
- Normal load (100 req/s)
- Traffic spikes (1000 req/s)
- Sustained stress (500 req/s)
- Chaos testing
- Results interpretation

#### Integration
- Using in your projects
- Custom signal types
- External systems
- Cloud deployment
- High availability

#### Troubleshooting
- Common issues
- Debug procedures
- Performance optimization
- Log analysis
- Container troubleshooting

## Project Structure

```
examples/tps-reference-system/
├── README.md                      # Quick start and overview
├── Makefile                       # Operations (make up, make test, etc.)
├── Dockerfile                     # Container build
├── docker-compose.yml             # Complete stack definition
├── prometheus.yml                 # Metrics configuration
├── promtail-config.yml            # Log collector
├── loki-config.yml                # Log storage
├── .env.example                   # Configuration template
├── .gitignore                     # Version control exclusions
├── src/
│   └── main.rs                    # Reference system implementation (800 lines)
├── tests/
│   └── integration_test.rs        # 10 integration test scenarios (600 lines)
└── Cargo.toml                     # Rust dependencies

crates/tps-reference/
├── Cargo.toml                     # Core library manifest
└── src/
    └── lib.rs                     # TPS core implementation (1000 lines)
        ├── Jidoka module          # Circuit breaker
        ├── Kanban module          # Queue management
        ├── Andon module           # Signal system
        ├── Kaizen module          # Metrics
        ├── Heijunka module        # Load balancing
        ├── Config module          # Configuration
        ├── WorkSignal struct      # Signal definition
        ├── ProcessingResult struct# Result type
        ├── HealthStatus struct    # Status reporting
        ├── TpsSystem struct       # Main orchestrator
        └── SupervisionTree        # Component supervision
```

## Quick Reference: Make Commands

| Command | Purpose |
|---------|---------|
| `make up` | Start all services |
| `make down` | Stop all services |
| `make test` | Run integration tests |
| `make load` | Run load test (100 req/s) |
| `make spike` | Run spike test (1000 req/s) |
| `make health` | Check service health |
| `make metrics` | View metrics |
| `make dashboard` | Open Grafana |
| `make traces` | Open Jaeger |
| `make logs` | Tail all logs |

## Quick Reference: API Endpoints

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/signal` | POST | Process work signal |
| `/signal/:id/status` | GET | Get signal status |
| `/health` | GET | System health check |
| `/metrics` | GET | Performance metrics |
| `/info` | GET | System information |
| `/status` | GET | Comprehensive status |
| `/` | GET | Documentation page |

## Technology Stack

- **Language**: Rust 1.70+
- **Async Runtime**: Tokio
- **Web Framework**: Axum
- **Message Queue**: NATS + RabbitMQ
- **Metrics**: Prometheus
- **Visualization**: Grafana
- **Tracing**: Jaeger
- **Logging**: Loki + Promtail
- **Container**: Docker + Docker Compose

## Performance Targets

- **Throughput**: 400-1000 requests/second
- **P99 Latency**: < 200ms under normal load
- **Queue Recovery**: < 60 seconds after overload
- **Circuit Recovery**: 30 seconds (configurable)
- **Memory Usage**: < 200MB steady state

## Getting Help

1. **Quick Issues**: See [Troubleshooting](99-reference-implementation.md#troubleshooting)
2. **Configuration**: See [Configuration Guide](99-reference-implementation.md#configuration)
3. **Load Testing**: See [Load Testing](99-reference-implementation.md#load-testing)
4. **API**: See [API Reference](99-reference-implementation.md#api-reference)
5. **Integration**: See [Customization](99-reference-implementation.md#customization)

## Common Tasks

### Start the system
```bash
cd examples/tps-reference-system
make up
```

### Send a test signal
```bash
curl -X POST http://localhost:8080/signal \
  -H "Content-Type: application/json" \
  -d '{"signal_type":"execute","payload":{"task":"test"}}'
```

### Run load test
```bash
make spike    # 1000 req/s for 10 seconds
```

### View dashboard
```bash
make dashboard    # Opens Grafana at http://localhost:3000
```

### Check logs
```bash
make logs-system    # Show TPS system logs
```

### Stop everything
```bash
make down
```

## Documentation Files

- **99-reference-implementation.md** (2000+ lines)
  - Complete comprehensive guide
  - All sections from prerequisites to advanced topics
  - Step-by-step procedures
  - Troubleshooting flowcharts
  - Configuration examples

## Key Concepts

### Supervision Tree
Each TPS component is supervised with:
- Failure detection
- Automatic recovery
- State management
- Health reporting

### Signal Processing Pipeline
Every signal flows through all 6 principles:
1. Jidoka → Check circuit breaker
2. Kanban → Enqueue signal
3. Worker → Process work
4. Andon → Record status
5. Kaizen → Collect metrics
6. Heijunka → Update load
7. Tracing → Complete trace

### Observable System
Complete visibility at all layers:
- Metrics: Prometheus + Grafana
- Traces: Jaeger
- Logs: Loki
- Health: HTTP endpoints

## Next Steps

1. **Start here**: [99-reference-implementation.md](99-reference-implementation.md#quick-start)
2. **Run the system**: `make up`
3. **Send signals**: Use the API examples
4. **View dashboards**: Open Grafana and Jaeger
5. **Run load tests**: `make spike`
6. **Customize**: Modify signal types and metrics

## Support

- **Code**: [GitHub Repository](https://github.com/seanchatmangpt/ggen)
- **Documentation**: See files in this directory
- **Issues**: Check troubleshooting section first
- **Contributing**: See CONTRIBUTING.md (in main repo)

---

**Status**: Production-Ready (v1.0.0)
**Last Updated**: January 2026
**Maintainer**: TPS Reference Team
**License**: MIT

