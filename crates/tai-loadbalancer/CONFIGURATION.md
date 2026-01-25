# TAI Load Balancer: Configuration & Tuning Guide

## Overview

The TAI Load Balancer provides production-grade load balancing and service discovery for gRPC microservices with advanced features including health checking, connection pooling, session affinity, and automatic failover.

## Configuration

### Basic Setup

```rust
use tai_loadbalancer::{
    LoadBalancerConfig, LoadBalancingStrategy, ServiceRegistry,
    IntegratedLoadBalancer, ConnectionPoolConfig, HealthCheckConfig
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create service registry
    let registry = Arc::new(ServiceRegistry::new());

    // Configure load balancer
    let config = LoadBalancerConfig {
        pool_config: ConnectionPoolConfig::default(),
        health_check_config: HealthCheckConfig::default(),
        ..Default::default()
    };

    // Create integrated load balancer
    let lb = IntegratedLoadBalancer::new(
        registry,
        LoadBalancingStrategy::RoundRobin,
        config,
    ).await?;

    // Start background processes
    lb.start().await?;

    Ok(())
}
```

### LoadBalancingStrategy Options

#### 1. RoundRobin (Default)

Distributes requests evenly across all healthy endpoints in a circular pattern.

**Use Case**: Uniform workload distribution, stateless services

```rust
let strategy = LoadBalancingStrategy::RoundRobin;
```

**Configuration**: No additional configuration needed

**Performance**: O(1) per request, minimal overhead

#### 2. LeastConnections

Routes requests to the endpoint with the fewest active connections.

**Use Case**: Heterogeneous backend capabilities, long-lived connections

```rust
let strategy = LoadBalancingStrategy::LeastConnections;
```

**Configuration**: Requires connection tracking enabled

**Performance**: O(n) per request where n = number of endpoints

#### 3. ConsistentHash

Uses consistent hashing to map requests to endpoints based on a key (user ID, session ID, etc.).

**Use Case**: Session affinity, data locality, cache-friendly routing

```rust
let endpoint = lb.next_endpoint_with_hash("service", "user-123").await?;
```

**Configuration**: Hash key must be provided per request

**Performance**: O(log n) per request with ring hash

#### 4. Weighted

Routes requests based on endpoint weights (useful for canary deployments).

**Use Case**: Gradual rollouts, blue-green deployments

```rust
let endpoint = Endpoint::with_weight(
    "127.0.0.1:5001".parse()?,
    None,
    100 // weight
);
```

**Configuration**: Set weight on each endpoint

**Performance**: O(n) per request with random selection

#### 5. Random

Randomly selects endpoints.

**Use Case**: Simple load distribution, debugging

```rust
let strategy = LoadBalancingStrategy::Random;
```

**Configuration**: No additional configuration needed

**Performance**: O(1) per request

### ConnectionPoolConfig

```rust
#[derive(Clone)]
pub struct ConnectionPoolConfig {
    /// Minimum connections per endpoint (default: 2)
    pub min_connections: usize,

    /// Maximum connections per endpoint (default: 32)
    pub max_connections: usize,

    /// Timeout for acquiring connection (default: 5s)
    pub acquire_timeout: Duration,

    /// Idle connection timeout (default: 300s)
    pub idle_timeout: Duration,

    /// Max connection lifetime (default: 600s)
    pub max_lifetime: Duration,
}
```

**Tuning Guidelines**:

- **Small services (< 100 rps)**: `min=2, max=16`
- **Medium services (100-1000 rps)**: `min=5, max=32`
- **Large services (> 1000 rps)**: `min=10, max=128`
- **Long-lived connections**: Increase `idle_timeout` to 600s+
- **Short-lived requests**: Decrease `idle_timeout` to 30-60s

### HealthCheckConfig

```rust
#[derive(Clone)]
pub struct HealthCheckConfig {
    /// Check interval (default: 10s)
    pub check_interval: Duration,

    /// Check timeout (default: 5s)
    pub check_timeout: Duration,

    /// Failures before unhealthy (default: 3)
    pub failure_threshold: usize,

    /// Successes before healthy (default: 2)
    pub success_threshold: usize,

    /// Grace period before first check (default: 5s)
    pub grace_period: Duration,
}
```

**Tuning Guidelines**:

- **Development**: `interval=5s, failure_threshold=2, success_threshold=1`
- **Production (stable)**: `interval=30s, failure_threshold=3, success_threshold=2`
- **Production (unstable)**: `interval=5s, failure_threshold=1, success_threshold=3`
- **High-availability**: `interval=1s, failure_threshold=2, success_threshold=2` (faster failover)

### AffinityConfig

```rust
#[derive(Clone)]
pub struct AffinityConfig {
    /// Affinity strategy (default: None)
    pub strategy: AffinityStrategy,

    /// Affinity TTL (default: 3600s)
    pub ttl: Duration,

    /// Max cached entries (default: 100,000)
    pub max_entries: usize,
}
```

**Strategies**:

- `None`: No affinity (each request is load balanced)
- `SourceIP`: Route based on client IP
- `UserId`: Route based on user ID
- `Cookie`: Route based on session cookie
- `CustomKey`: Route based on custom identifier

**Tuning Guidelines**:

- **Stateless services**: Use `None`
- **Session-sensitive**: Use `CustomKey` with user ID
- **Short sessions (< 1 hour)**: `ttl=1800s`
- **Long sessions (> 1 hour)**: `ttl=3600s+`
- **High concurrency (> 1M users)**: `max_entries=1000000`

### FailoverConfig

```rust
#[derive(Clone)]
pub struct FailoverConfig {
    /// Enable failover (default: true)
    pub enabled: bool,

    /// Max failover attempts (default: 3)
    pub max_attempts: usize,

    /// Log failover events (default: true)
    pub log_failovers: bool,
}
```

**Tuning Guidelines**:

- **Critical services**: `max_attempts=5`
- **Standard services**: `max_attempts=3`
- **Best-effort services**: `max_attempts=1`

### RebalanceConfig

```rust
#[derive(Clone)]
pub struct RebalanceConfig {
    /// Rebalancing strategy (default: Gradual)
    pub strategy: RebalanceStrategy,

    /// Check interval (default: 30s)
    pub check_interval: Duration,

    /// Gradual rebalance duration (default: 300s)
    pub gradual_duration: Duration,

    /// Max drain percentage per iteration (default: 10%)
    pub max_drain_percentage: f32,
}
```

**Strategies**:

- `Immediate`: Rebalance immediately (may cause storms)
- `Gradual`: Drain connections over time (recommended for production)
- `Manual`: Require manual rebalancing

**Tuning Guidelines**:

- **Development**: Use `Immediate`
- **Production**: Use `Gradual` with `max_drain_percentage=0.1` (10%)
- **Zero-downtime**: Use `Gradual` with `max_drain_percentage=0.05` (5%) and longer `gradual_duration`

## Observability

### Metrics

The load balancer provides comprehensive metrics:

```rust
let metrics = lb.metrics();
println!("Total requests: {}", metrics.total_requests);
println!("Success rate: {:.2}%",
    100.0 * metrics.successful_requests as f64 / metrics.total_requests as f64);
println!("Failed requests: {}", metrics.failed_requests);
println!("Timeouts: {}", metrics.timeouts);
```

### Key Metrics to Monitor

1. **Request Metrics**:
   - `total_requests`: Total requests processed
   - `successful_requests`: Successfully completed
   - `failed_requests`: Failed requests
   - `timeouts`: Requests that timed out

2. **Latency Metrics**:
   - `average_latency_ms`: Mean latency
   - `p50_latency_ms`: Median latency
   - `p95_latency_ms`: 95th percentile latency
   - `p99_latency_ms`: 99th percentile latency

3. **Health Metrics**:
   - `healthy_endpoints`: Number of healthy endpoints
   - `unhealthy_endpoints`: Number of unhealthy endpoints
   - `health_check_failures`: Failed health checks

4. **Pool Metrics**:
   - `active_connections`: Current active connections
   - `total_connections`: Total connections created
   - `pool_exhausted_events`: Times pool was exhausted

## Monitoring & Alerting

### SLO Targets

```rust
// Request success rate
assert!(success_rate > 0.99); // 99% SLO

// Latency SLOs
assert!(p50_latency < 50.0);  // Median < 50ms
assert!(p95_latency < 200.0); // P95 < 200ms
assert!(p99_latency < 500.0); // P99 < 500ms

// Availability SLO
assert!(healthy_endpoints > 0); // At least one healthy endpoint
```

### Alert Conditions

1. **Critical Alerts**:
   - All endpoints unhealthy
   - Request failure rate > 5%
   - P99 latency > 1s
   - Connection pool exhausted

2. **Warning Alerts**:
   - > 50% endpoints unhealthy
   - Request failure rate > 1%
   - P95 latency > 500ms
   - Connection pool > 90% utilized

3. **Info Alerts**:
   - Endpoint added/removed
   - Failover triggered
   - Rebalancing started

## Performance Tuning

### For High Throughput

```rust
let config = LoadBalancerConfig {
    pool_config: ConnectionPoolConfig {
        min_connections: 20,
        max_connections: 256,
        acquire_timeout: Duration::from_secs(1),
        ..Default::default()
    },
    health_check_config: HealthCheckConfig {
        check_interval: Duration::from_secs(30),
        failure_threshold: 5,
        ..Default::default()
    },
    ..Default::default()
};
```

### For Low Latency

```rust
let config = LoadBalancerConfig {
    pool_config: ConnectionPoolConfig {
        min_connections: 10,
        max_connections: 64,
        acquire_timeout: Duration::from_millis(500),
        ..Default::default()
    },
    health_check_config: HealthCheckConfig {
        check_interval: Duration::from_secs(5),
        failure_threshold: 2,
        ..Default::default()
    },
    ..Default::default()
};
```

### For Stability

```rust
let config = LoadBalancerConfig {
    pool_config: ConnectionPoolConfig {
        min_connections: 5,
        max_connections: 32,
        idle_timeout: Duration::from_secs(600),
        ..Default::default()
    },
    health_check_config: HealthCheckConfig {
        check_interval: Duration::from_secs(30),
        failure_threshold: 3,
        success_threshold: 3,
        ..Default::default()
    },
    ..Default::default()
};
```

## Best Practices

1. **Always use health checks** in production
2. **Configure appropriate timeouts** for your service latency
3. **Monitor metrics** continuously
4. **Test failover scenarios** regularly
5. **Use gradual rebalancing** in production
6. **Set affinity** only when necessary (session affinity has overhead)
7. **Keep pool sizes reasonable** (excessive connections hurt performance)
8. **Review metrics weekly** and adjust configuration

## Troubleshooting

### Issue: Connection Pool Exhausted

**Symptoms**: `Error::PoolExhausted`

**Solutions**:
1. Increase `max_connections`
2. Decrease request processing time
3. Reduce `idle_timeout` to close stale connections faster
4. Add more backend instances

### Issue: High Latency

**Symptoms**: P95/P99 latency increasing

**Solutions**:
1. Reduce health check interval
2. Increase pool size
3. Check backend health
4. Review error rates
5. Consider least-connections strategy

### Issue: Frequent Failovers

**Symptoms**: Failover logs, service instability

**Solutions**:
1. Increase `failure_threshold` in health checks
2. Increase `grace_period`
3. Check backend logs for issues
4. Consider load distribution problem

### Issue: Affinity Not Working

**Symptoms**: Requests going to different endpoints

**Solutions**:
1. Verify affinity strategy is enabled
2. Check affinity key is consistent
3. Verify affinity TTL is sufficient
4. Monitor affinity cache size

## Migration from v0.1 to v0.2

No breaking changes in v0.2. New features:
- Enhanced metrics collection
- Improved health checking algorithm
- Support for weighted load balancing

## Advanced Topics

### Circuit Breaker Integration

```rust
use tai_resilience::CircuitBreaker;

let cb = CircuitBreaker::new(config)?;
let endpoint = lb.next_endpoint("service").await?;

cb.execute(|| async {
    // Make request to endpoint
    Ok(())
}).await?;
```

### Custom Health Checks

Implement custom health check logic by extending `HealthCheckManager`:

```rust
async fn custom_health_check(endpoint: &Endpoint) -> Result<()> {
    // Custom logic: TCP connect, gRPC health check, HTTP endpoint, etc.
    Ok(())
}
```

### Observability Integration

The load balancer exports metrics compatible with Prometheus:

```rust
// Get metrics for Prometheus scraping
let metrics = lb.metrics();
// Publish to Prometheus push gateway or expose on /metrics endpoint
```

See the main crate documentation for integration examples.
