# TAI Load Balancer

Production-grade load balancing and service discovery for gRPC microservices with advanced resilience patterns.

## Features

### Core Capabilities

- **Dynamic Service Discovery**: Register/deregister endpoints with automatic updates
- **Multiple Load Balancing Strategies**:
  - Round-robin: Uniform distribution
  - Least-connections: Route to endpoint with fewest active connections
  - Consistent hash: Session affinity with consistent hashing
  - Weighted: Support for canary deployments
  - Random: Probabilistic distribution

- **Health-Aware Routing**:
  - Configurable periodic health checks
  - Automatic unhealthy instance exclusion
  - Grace period for new instances
  - Configurable failure/success thresholds

- **Connection Pooling**:
  - Configurable min/max connections per endpoint
  - Connection acquire timeout
  - Idle and lifetime management
  - Backpressure handling

- **Session Affinity**:
  - Source IP based routing
  - User ID based routing
  - Cookie based routing
  - Custom key based routing
  - Configurable TTL and cache size

- **Automatic Rebalancing**:
  - Detect topology changes
  - Gradual connection draining
  - Immediate or manual rebalancing strategies

- **Backup/Failover**:
  - Primary and backup endpoint configuration
  - Automatic failover on primary failure
  - Configurable max failover attempts
  - Failover statistics and logging

- **Observable Metrics**:
  - Request success/failure rates
  - Latency percentiles (p50, p95, p99)
  - Per-endpoint statistics
  - Health check metrics

## Quick Start

### Basic Usage

```rust
use tai_loadbalancer::{
    Endpoint, IntegratedLoadBalancer, LoadBalancerConfig,
    LoadBalancingStrategy, ServiceRegistry,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create service registry
    let registry = Arc::new(ServiceRegistry::new());

    // Register endpoints
    registry
        .register(
            "api-service".to_string(),
            Endpoint::new("127.0.0.1:5001".parse()?, None),
        )
        .await?;

    registry
        .register(
            "api-service".to_string(),
            Endpoint::new("127.0.0.1:5002".parse()?, None),
        )
        .await?;

    // Create load balancer
    let lb = IntegratedLoadBalancer::new(
        registry,
        LoadBalancingStrategy::RoundRobin,
        LoadBalancerConfig::default(),
    )
    .await?;

    // Start background processes (health checks, rebalancing)
    lb.start().await?;

    // Get next endpoint
    let endpoint = lb.next_endpoint("api-service").await?;
    println!("Selected endpoint: {}", endpoint.address);

    // Cleanup
    lb.stop().await?;

    Ok(())
}
```

### Session Affinity

```rust
use tai_loadbalancer::{AffinityStrategy, AffinityConfig};
use std::time::Duration;

let affinity_config = AffinityConfig {
    strategy: AffinityStrategy::CustomKey,
    ttl: Duration::from_secs(3600),
    max_entries: 100_000,
};

// Get endpoint with session affinity
let endpoint = lb.next_endpoint_with_affinity("service", "user-123").await?;
```

### Failover Configuration

```rust
use tai_loadbalancer::{FailoverConfig, FailoverManager};

let config = FailoverConfig {
    enabled: true,
    max_attempts: 3,
    log_failovers: true,
};

let manager = FailoverManager::new(config);

// Register failover group
let primary = Endpoint::new("127.0.0.1:5001".parse()?, None);
let backups = vec![
    Endpoint::new("127.0.0.1:5002".parse()?, None),
    Endpoint::new("127.0.0.1:5003".parse()?, None),
];

manager.register("critical-service", primary, backups).await?;

// Trigger failover on error
if let Err(_) = some_request().await {
    manager.failover("critical-service", &endpoint).await?;
}
```

## Architecture

```
┌─────────────────┐
│     Client      │
└────────┬────────┘
         │
    ┌────▼─────────────────────┐
    │   Load Balancer          │
    │  (Routing Decision)       │
    └────┬──────────────────────┘
         │
    ┌────┴────────────────────────────┐
    │                                  │
    ▼                                  ▼
┌──────────────┐            ┌──────────────┐
│Service       │            │Service       │
│Registry      │            │Registry      │
│              │            │              │
│- Endpoints   │            │- Endpoints   │
│- Metadata    │            │- Metadata    │
└──┬───────────┘            └──┬───────────┘
   │                           │
   ▼                           ▼
┌──────────────────────────────────────┐
│  Health Check Manager                │
│  - Periodic checks                   │
│  - Status tracking                   │
│  - Metrics collection                │
└──────────────────────────────────────┘
   │
   ▼
┌──────────────────────────────────────┐
│  Connection Pool                     │
│  - Per-endpoint pools                │
│  - Backpressure handling             │
│  - Connection lifecycle mgmt         │
└──────────────────────────────────────┘
   │
   ▼
┌──────────────────────────────────────┐
│  Healthy Backend Instances           │
│  - API Servers                       │
│  - Database Replicas                 │
│  - Cache Nodes                       │
└──────────────────────────────────────┘
```

## Components

### ServiceRegistry

Manages dynamic endpoint registration and deregistration.

```rust
// Register single endpoint
registry.register("service", endpoint).await?;

// Batch register
registry.register_batch("service", vec![...]).await?;

// Deregister
registry.deregister("service", &endpoint).await?;

// Find by metadata
let endpoints = registry.find_matching("service", Some("us-west-2"), None).await?;
```

### LoadBalancer

Routes requests using configurable strategies.

```rust
// Simple routing
let endpoint = lb.next_endpoint("service").await?;

// Hash-based routing
let endpoint = lb.next_endpoint_with_hash("service", "user-123").await?;
```

### HealthCheckManager

Monitors endpoint health with configurable checks.

```rust
// Start health checking
manager.start().await?;

// Check if endpoint is healthy
let healthy = manager.is_healthy("service", &endpoint).await?;

// Get health status
let status = manager.get_health_status("service", &endpoint).await?;
```

### ConnectionPool

Manages connections to endpoints with backpressure.

```rust
// Acquire connection
let conn = pool.acquire(&endpoint).await?;

// Connection is automatically released when dropped
drop(conn);
```

### AffinityManager

Handles session affinity and sticky routing.

```rust
// Store affinity
manager.set_affinity("service", "session-key", endpoint).await;

// Retrieve affinity
if let Some(endpoint) = manager.get_affinity("service", "session-key").await {
    // Route to same endpoint
}
```

### RebalanceManager

Handles topology changes and automatic rebalancing.

```rust
// Start watching for changes
manager.start().await?;

// Manually trigger rebalance
let event = manager.rebalance_service("service").await?;
```

### FailoverManager

Manages primary and backup endpoints.

```rust
// Register failover group
manager.register("service", primary, backups).await?;

// Trigger failover
let next = manager.failover("service", &current).await?;

// Reset failover
manager.reset_failover("service", &primary).await?;
```

## Testing

Run comprehensive tests:

```bash
cargo test --package tai-loadbalancer

# Integration tests
cargo test --package tai-loadbalancer --test loadbalancer_integration_tests

# Chaos scenario tests
cargo test --package tai-loadbalancer --test chaos_scenario_tests
```

## Configuration

See [CONFIGURATION.md](./CONFIGURATION.md) for detailed configuration options and tuning guide.

Key configuration areas:

- **Load Balancing Strategy**: Choose appropriate strategy for your workload
- **Health Checking**: Configure frequency, thresholds, and timeouts
- **Connection Pooling**: Tune pool sizes for your throughput requirements
- **Session Affinity**: Configure for stateful services
- **Rebalancing**: Choose immediate or gradual rebalancing

## Performance

SLO Targets:

- **Endpoint selection**: < 1μs (constant time)
- **Health check overhead**: < 5% of total latency
- **Connection pool operation**: < 100μs
- **Affinity lookup**: < 10μs (with LRU cache)

## Observability

### Metrics

```rust
let metrics = lb.metrics();
println!("Requests: {}", metrics.total_requests);
println!("Success: {}%",
    100.0 * metrics.successful_requests / metrics.total_requests);
```

### Logging

Debug logging available via `tracing` crate:

```bash
RUST_LOG=tai_loadbalancer=debug cargo run
```

### Integration with Monitoring

- Prometheus compatible metrics
- OpenTelemetry support
- Custom metric exporters

## Integration with TAI Ecosystem

### With Circuit Breaker

```rust
use tai_resilience::CircuitBreaker;

let cb = CircuitBreaker::new(config)?;
let endpoint = lb.next_endpoint("service").await?;

// Circuit breaker protects against cascade failures
cb.execute(|| async {
    // Make request to endpoint
    Ok(())
}).await?;
```

### With Service Mesh (Istio)

Coordinates with service mesh for traffic management:

- Works alongside Istio's load balancing
- Respects virtual service rules
- Integrates with destination rules

## Limitations

- Does not replace service mesh load balancing
- Health checks are gRPC agnostic (implement custom checks for specific protocols)
- Affinity is local to this instance (distribute load across multiple instances with consistent strategy)

## Contributing

See main project CONTRIBUTING.md

## License

MIT

## Related Crates

- `tai-resilience`: Circuit breaker and traffic management
- `tai-grpc`: gRPC server and client
- `tai-observability`: Observability and tracing
- `tai-k8s`: Kubernetes integration

## Version

0.1.0 - Initial production release
