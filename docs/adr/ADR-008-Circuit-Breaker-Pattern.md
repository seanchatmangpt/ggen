<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-008: Circuit Breaker Pattern for gRPC Services](#adr-008-circuit-breaker-pattern-for-grpc-services)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [Circuit Breaker Benefits](#circuit-breaker-benefits)
  - [Implementation](#implementation)
    - [Istio DestinationRule (Infrastructure Layer)](#istio-destinationrule-infrastructure-layer)
    - [Application-Level Circuit Breaker (Rust)](#application-level-circuit-breaker-rust)
    - [Health Check Integration](#health-check-integration)
    - [Metrics and Monitoring](#metrics-and-monitoring)
    - [Alerting](#alerting)
  - [Integration with Canary Deployments](#integration-with-canary-deployments)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Configuration by Service](#configuration-by-service)
  - [Testing Circuit Breaker](#testing-circuit-breaker)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-008: Circuit Breaker Pattern for gRPC Services

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Preventing cascading failures in service-to-service calls
**Deciders:** Reliability Engineering Team

## Problem Statement

When a downstream service degrades or becomes unavailable:
- Callers keep retrying, wasting resources
- Requests queue up, exhausting connection pools
- Timeouts cascade through the system
- System takes longer to recover

Need: Detect failures fast, fail fast, recover gracefully.

## Decision

**Implement circuit breaker with three states:**
1. **Closed:** Normal operation, requests pass through
2. **Open:** Service unhealthy, fail fast without calling
3. **Half-Open:** Testing if service recovered, gradual traffic increase

## Rationale

### Circuit Breaker Benefits
1. **Fail Fast:**
   - Stop wasting resources on doomed requests
   - Return error immediately when circuit is open
   - Frees up resources for healthy services

2. **Self-Healing:**
   - Automatic retry when service recovers
   - Gradual traffic increase (half-open state)
   - No manual intervention needed

3. **Cascading Failure Prevention:**
   - Stops error propagation up the stack
   - Other services keep working normally
   - System degrades gracefully

## Implementation

### Istio DestinationRule (Infrastructure Layer)

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: governor-circuit-breaker
  namespace: tai-system
spec:
  host: governor.tai-system.svc.cluster.local
  trafficPolicy:
    # Connection pool limits
    connectionPool:
      tcp:
        maxConnections: 100          # Max TCP connections
      http:
        http1MaxPendingRequests: 50  # Max pending requests
        http2MaxRequests: 100        # Max concurrent requests
        maxRequestsPerConnection: 2  # Max per connection

    # Outlier detection (circuit breaker)
    outlierDetection:
      consecutive5xxErrors: 5        # Open after 5 errors
      interval: 30s                  # Check interval
      baseEjectionTime: 30s          # Min time circuit is open
      maxEjectionPercent: 50         # Max 50% pods ejected
      minRequestVolume: 100          # Need 100 requests to evaluate
      splitExternalLocalOriginErrors: true
```

### Application-Level Circuit Breaker (Rust)

Using `tower` crate for typed circuit breaker:

```rust
use tower::circuit_breaker::{CircuitBreaker, Predicate};
use tower::service_builder::ServiceBuilder;
use tonic::transport::Channel;

pub struct CircuitBreakerConfig {
    pub failure_threshold: u32,      // Open after N failures
    pub success_threshold: u32,      // Close after N successes
    pub timeout_duration: Duration,  // Half-open timeout
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            success_threshold: 2,
            timeout_duration: Duration::from_secs(60),
        }
    }
}

pub async fn create_governor_client(
    address: &str,
    config: CircuitBreakerConfig,
) -> Result<CircuitBreakerClient> {
    let channel = Channel::from_shared(address.to_string())?
        .connect()
        .await?;

    // Wrap with circuit breaker
    let svc = ServiceBuilder::new()
        .rate_limit(100, Duration::from_secs(1))  // Rate limiting
        .circuit_breaker(CircuitBreakerPredicate::new(config))
        .timeout(Duration::from_secs(10))         // RPC timeout
        .trace_make_service()
        .service(channel);

    Ok(CircuitBreakerClient::new(svc))
}

// Custom predicate to determine circuit breaker state
pub struct CircuitBreakerPredicate {
    failure_count: Arc<AtomicU32>,
    success_count: Arc<AtomicU32>,
    state: Arc<Mutex<CircuitState>>,
    last_failure_time: Arc<Mutex<Instant>>,
    config: CircuitBreakerConfig,
}

#[derive(Debug, Clone, Copy)]
pub enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

impl CircuitBreakerPredicate {
    pub fn new(config: CircuitBreakerConfig) -> Self {
        Self {
            failure_count: Arc::new(AtomicU32::new(0)),
            success_count: Arc::new(AtomicU32::new(0)),
            state: Arc::new(Mutex::new(CircuitState::Closed)),
            last_failure_time: Arc::new(Mutex::new(Instant::now())),
            config,
        }
    }

    pub async fn check_state(&self) -> CircuitState {
        let mut state = self.state.lock().await;

        match *state {
            CircuitState::Closed => {
                if self.failure_count.load(Ordering::Relaxed) >= self.config.failure_threshold {
                    *state = CircuitState::Open;
                    println!("Circuit breaker opened: too many failures");
                    CircuitState::Open
                } else {
                    CircuitState::Closed
                }
            }
            CircuitState::Open => {
                let last_failure = self.last_failure_time.lock().await;
                if last_failure.elapsed() >= self.config.timeout_duration {
                    *state = CircuitState::HalfOpen;
                    self.success_count.store(0, Ordering::Relaxed);
                    println!("Circuit breaker half-open: testing recovery");
                    CircuitState::HalfOpen
                } else {
                    CircuitState::Open
                }
            }
            CircuitState::HalfOpen => {
                if self.success_count.load(Ordering::Relaxed) >= self.config.success_threshold {
                    *state = CircuitState::Closed;
                    self.failure_count.store(0, Ordering::Relaxed);
                    println!("Circuit breaker closed: service recovered");
                    CircuitState::Closed
                } else if self.failure_count.load(Ordering::Relaxed) > 0 {
                    *state = CircuitState::Open;
                    println!("Circuit breaker reopened: recovery failed");
                    CircuitState::Open
                } else {
                    CircuitState::HalfOpen
                }
            }
        }
    }

    pub fn record_success(&self) {
        self.success_count.fetch_add(1, Ordering::Relaxed);
    }

    pub fn record_failure(&self) {
        self.failure_count.fetch_add(1, Ordering::Relaxed);
        self.last_failure_time.store(Instant::now(), Ordering::Relaxed);
    }
}

// Service wrapper with circuit breaker logic
pub struct GovernorClientWithBreaker {
    inner: GovernorClient<Channel>,
    breaker: Arc<CircuitBreakerPredicate>,
}

impl GovernorClientWithBreaker {
    pub async fn propose_policy(
        &mut self,
        policy: Policy,
    ) -> Result<Receipt> {
        // Check circuit state
        let state = self.breaker.check_state().await;

        match state {
            CircuitState::Open => {
                // Fail fast without calling service
                Err(Status::unavailable(
                    "Service unavailable (circuit breaker open)"
                ))
            }
            CircuitState::Closed | CircuitState::HalfOpen => {
                // Try calling service
                match self.inner.propose_policy(tonic::Request::new(policy)).await {
                    Ok(response) => {
                        self.breaker.record_success();
                        Ok(response.into_inner())
                    }
                    Err(e) => {
                        self.breaker.record_failure();
                        Err(e)
                    }
                }
            }
        }
    }
}
```

### Health Check Integration

```rust
// Health check to inform circuit breaker
pub async fn health_check_loop(
    governor_client: GovernorClient<Channel>,
    breaker: Arc<CircuitBreakerPredicate>,
) {
    let mut interval = tokio::time::interval(Duration::from_secs(10));

    loop {
        interval.tick().await;

        let request = HealthCheckRequest {
            service_name: "governor".to_string(),
        };

        match governor_client
            .clone()
            .health_check(tonic::Request::new(request))
            .timeout(Duration::from_secs(5))
            .await
        {
            Ok(response) => {
                if response.get_ref().status == "healthy" {
                    breaker.record_success();
                } else {
                    breaker.record_failure();
                }
            }
            Err(_) => {
                breaker.record_failure();
            }
        }
    }
}
```

### Metrics and Monitoring

```prometheus
# Circuit breaker state
tai_circuit_breaker_state{service="governor"} 0  # 0=Closed, 1=Open, 2=HalfOpen

# Failure count
tai_circuit_breaker_failures_total{service="governor"} 15

# Fast fails (circuit was open)
tai_circuit_breaker_fast_fails_total{service="governor"} 42

# State transitions
tai_circuit_breaker_state_changes_total{service="governor",from="closed",to="open"} 3
tai_circuit_breaker_state_changes_total{service="governor",from="open",to="half_open"} 3
```

### Alerting

```yaml
- name: CircuitBreakerOpen
  expr: tai_circuit_breaker_state{service="governor"} == 1
  for: 5m
  severity: warning
  annotations:
    summary: "Governor circuit breaker is open"

- name: HighFailureRate
  expr: |
    rate(tai_circuit_breaker_failures_total[5m]) > 0.1
  for: 1m
  severity: critical
  annotations:
    summary: "High failure rate detected"
```

## Integration with Canary Deployments

Circuit breaker works with canary:
1. Canary version deployed alongside stable
2. If canary error rate high, circuit opens for canary subset
3. Traffic automatically fails over to stable
4. Canary rolled back if circuit stays open > 5 minutes

## Consequences

### Positive
- Prevents cascading failures
- Fails fast (reduced resource waste)
- Automatic recovery
- Graceful degradation
- Improved system resilience

### Negative
- Client must handle "unavailable" errors
- Configuration tuning needed per service
- False positives possible (network issues)
- Adds complexity to debugging
- Performance overhead minimal

## Configuration by Service

| Service | Failure Threshold | Timeout | Half-Open Success |
|---------|------------------|---------|-------------------|
| Governor | 5 errors | 30s | 2 successes |
| Coordinator | 10 errors | 60s | 3 successes |
| Scheduler | 5 errors | 30s | 2 successes |

## Testing Circuit Breaker

```rust
#[tokio::test]
async fn test_circuit_breaker_opens_on_failures() {
    let config = CircuitBreakerConfig {
        failure_threshold: 3,
        success_threshold: 2,
        timeout_duration: Duration::from_millis(100),
    };

    let breaker = Arc::new(CircuitBreakerPredicate::new(config));

    // Record 3 failures
    breaker.record_failure();
    breaker.record_failure();
    breaker.record_failure();

    // Circuit should open
    assert_eq!(breaker.check_state().await, CircuitState::Open);

    // Wait for timeout
    tokio::time::sleep(Duration::from_millis(200)).await;

    // Should be half-open
    assert_eq!(breaker.check_state().await, CircuitState::HalfOpen);

    // Record successes
    breaker.record_success();
    breaker.record_success();

    // Should close
    assert_eq!(breaker.check_state().await, CircuitState::Closed);
}
```

## References
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)
- [Tower Rust Library](https://docs.rs/tower/)
- [Istio Circuit Breaker](https://istio.io/latest/docs/concepts/traffic-management/#circuit-breakers)
- [Resilience4j](https://resilience4j.readme.io/) (reference implementation)
