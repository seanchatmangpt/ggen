# Service Mesh & Advanced Resilience Patterns (TAI v0.1.0)

**Author**: TAI Resilience Specialist
**Version**: 0.1.0
**Release Date**: January 2026
**Status**: Production-Ready

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Service Mesh Fundamentals](#service-mesh-fundamentals)
3. [Advanced Circuit Breaker](#advanced-circuit-breaker)
4. [Traffic Management Patterns](#traffic-management-patterns)
5. [Outlier Detection](#outlier-detection)
6. [Istio Integration](#istio-integration)
7. [Security Patterns](#security-patterns)
8. [Observability & Metrics](#observability--metrics)
9. [Deployment Strategies](#deployment-strategies)
10. [Troubleshooting Guide](#troubleshooting-guide)

---

## Architecture Overview

The TAI resilience system provides a comprehensive solution for managing microservices at scale, with three layers of protection:

```
┌─────────────────────────────────────────────────────┐
│         Application Layer (Your Services)           │
├─────────────────────────────────────────────────────┤
│  Circuit Breaker Layer (tai_resilience)             │
│  ├─ Monitors request failures                       │
│  ├─ Opens circuit on threshold violations           │
│  ├─ Supports Half-Open for recovery testing         │
│  └─ Enables slow-start for safe recovery            │
├─────────────────────────────────────────────────────┤
│  Outlier Detection Layer (tai_resilience)           │
│  ├─ Monitors instance health metrics                │
│  ├─ Detects unhealthy instances                     │
│  ├─ Automatically ejects bad instances              │
│  └─ Periodic re-injection testing                   │
├─────────────────────────────────────────────────────┤
│  Traffic Management Layer (tai_resilience + Istio)  │
│  ├─ Canary deployments (gradual rollout)            │
│  ├─ Blue-green deployments (instant switchover)     │
│  ├─ A/B testing (traffic splitting)                 │
│  ├─ Traffic mirroring (shadow traffic)              │
│  └─ Load balancing (algorithms & routing)           │
├─────────────────────────────────────────────────────┤
│  Service Mesh Data Plane (Istio/Envoy)              │
│  ├─ VirtualServices (traffic routing)               │
│  ├─ DestinationRules (load balancing)               │
│  ├─ Gateways (ingress management)                   │
│  └─ PeerAuthentication (mTLS enforcement)           │
├─────────────────────────────────────────────────────┤
│  Kubernetes Control Plane                           │
│  ├─ Pod scheduling & orchestration                  │
│  ├─ Service discovery                               │
│  └─ Network policies                                │
└─────────────────────────────────────────────────────┘
```

## Service Mesh Fundamentals

### What is a Service Mesh?

A service mesh is a dedicated infrastructure layer that handles service-to-service communication in microservices architectures. It provides:

- **Transparent Proxying**: Network communication is intercepted without application code changes
- **Traffic Management**: Sophisticated routing, load balancing, and traffic splitting
- **Security**: Mutual TLS (mTLS) encryption, authentication, and authorization
- **Observability**: Metrics, distributed tracing, and logging
- **Resilience**: Circuit breaking, retries, timeouts, and outlier detection

### Istio Architecture

Istio implements the service mesh using two planes:

#### Data Plane (Envoy Proxies)
- Sidecar proxies deployed alongside each service
- Intercepts all network traffic
- Enforces policies and routing rules
- Collects metrics and traces
- Zero application code changes

#### Control Plane
- Istiod: Central control component
- Manages configuration and policy distribution
- Provides service discovery
- Issues certificates for mTLS
- Collects and aggregates metrics

## Advanced Circuit Breaker

### State Machine

The TAI circuit breaker implements a sophisticated state machine:

```
                    Failure Threshold Reached
                           ↓
Closed ─────────────────→ Open
  ↑                        │
  │                        │ Timeout Duration Elapsed
  │                        ↓
  └─────────── Half-Open ──→ SlowStart ─→ (Wait Duration)
     Success Threshold Met          ↓
                                 Closed
```

### States Explained

1. **Closed**: Normal operation
   - Requests pass through
   - Monitors for failures
   - Transitions to Open when failure threshold reached

2. **Open**: Circuit is broken
   - All requests rejected immediately
   - No load on downstream service
   - Waits for timeout duration before testing recovery

3. **Half-Open**: Testing recovery
   - Limited requests allowed (configurable)
   - Tests if service is recovering
   - Returns to Open on failure, to SlowStart/Closed on success

4. **SlowStart**: Gradual recovery
   - Service is recovering but not fully healthy
   - Reduces concurrent requests
   - Gradually increases traffic over time
   - Transitions to Closed after slow-start duration

### Configuration Example

```rust
use tai_resilience::circuit_breaker_v2::{CircuitBreaker, CircuitBreakerConfig};
use std::time::Duration;

let config = CircuitBreakerConfig {
    name: "payment-service".to_string(),
    failure_threshold: 5,              // Open after 5 failures
    success_threshold: 2,              // Close after 2 successes in HalfOpen
    timeout_duration: Duration::from_secs(60),  // Wait 60s before HalfOpen
    half_open_max_requests: 3,         // Allow 3 requests in HalfOpen
    slow_start_duration: Duration::from_secs(120), // 2min slow-start
    max_concurrent_requests: Some(100),
    request_timeout: Duration::from_secs(30),
    slow_response_threshold: Duration::from_secs(5),
    enable_slow_start: true,
    ..Default::default()
};

let mut cb = CircuitBreaker::new(config)?;

// Execute requests through circuit breaker
let result = cb.execute(|| async {
    // Your service call here
    Ok::<String, String>("Success".to_string())
}).await;
```

### Failure Type Categorization

The circuit breaker tracks different failure types:

- **Timeout**: Request exceeded configured timeout
- **Error**: Service returned error (4xx, 5xx)
- **SlowResponse**: Response time exceeded threshold

Per-failure-type thresholds can be configured:

```rust
let mut config = CircuitBreakerConfig::default();
config.failure_thresholds.insert(FailureType::Timeout, 3);
config.failure_thresholds.insert(FailureType::Error, 5);
config.failure_thresholds.insert(FailureType::SlowResponse, 10);
```

### Health Check Probes

During Half-Open state, periodic health checks verify service recovery:

```rust
let mut cb = CircuitBreaker::with_health_checker(
    config,
    Arc::new(CustomHealthChecker::new(service_url))
)?;

// Perform health check
cb.health_check().await?;
```

### Metrics and Observability

All state transitions and failures are tracked:

```rust
let metrics = cb.get_metrics().await;
println!("State: {:?}", metrics.state);
println!("Success rate: {:.1}%",
    (metrics.successful_requests as f32 / metrics.total_requests as f32) * 100.0);
println!("Avg response time: {:.1}ms", metrics.avg_response_time_ms);
```

## Traffic Management Patterns

### Canary Deployment

Gradually roll out new versions by progressively increasing traffic.

**Use Case**: Deploy new features/versions with minimal blast radius

```rust
use tai_resilience::traffic_management::CanaryDeployment;
use std::time::Duration;

let mut canary = CanaryDeployment::new(
    "my-service",
    "1.0.0",  // Current version
    "1.1.0",  // New version
);

// Configure stages: 10% → 50% → 100%
canary = canary.with_stages(vec![10, 50, 100]);

// Run canary with 5 minutes per stage
canary.start(vec![10, 50, 100], Duration::from_secs(300)).await?;

// Check progress
let progress = canary.get_progress(); // Returns 0-100
```

**Workflow**:
1. Deploy new version alongside old version
2. Route 10% of traffic to new version
3. Monitor metrics (error rate, latency, business metrics)
4. If healthy, advance to 50%
5. Continue advancing until 100% reaches new version
6. Remove old version

**Automatic Rollback**: If error rate exceeds threshold during any stage, automatically rollback to previous version.

### Blue-Green Deployment

Instant switchover between two complete environments.

**Use Case**: Risky deployments requiring instant rollback capability

```rust
use tai_resilience::traffic_management::BlueGreenDeployment;

let mut bg = BlueGreenDeployment::new(
    "api-gateway",
    "2.0.0",  // Blue (current)
    "2.1.0",  // Green (new)
);

// Deploy green environment and verify health
bg.deploy_and_switch().await?;

// If issues detected, instant rollback
if detected_problem {
    bg.rollback().await?;
}
```

**Advantages**:
- Instant rollback (seconds vs minutes)
- No partial deployments
- Simple rollback procedure

**Disadvantages**:
- Requires 2x resources (2 environments)
- Database migrations harder (need downtime or dual-write)

### A/B Testing

Split traffic between variants and measure outcomes.

**Use Case**: Test new algorithms, UI changes, or features

```rust
use tai_resilience::traffic_management::ABTestSplit;

let mut ab_test = ABTestSplit::new(
    "checkout",
    "algorithm-v1",  // Variant A
    "algorithm-v2",  // Variant B
);

// Start with 60% A, 40% B
ab_test.start(60)?;

// Route requests deterministically
let variant = ab_test.route_request(request_id);

// Record metrics
ab_test.record_metric_a("conversion_rate", 0.045);
ab_test.record_metric_b("conversion_rate", 0.052);

// Determine winner (higher metrics = better)
if let Some(winner) = ab_test.get_winner() {
    println!("Winner: {}", winner);
}
```

### Traffic Mirroring

Send shadow traffic to new version without affecting real responses.

**Use Case**: Test new implementation against real production traffic safely

```rust
use tai_resilience::traffic_management::TrafficMirror;

let mut mirror = TrafficMirror::new(
    "frontend",
    "production",  // Primary (responds to user)
    "staging",     // Mirror (shadow traffic)
);

mirror.enable();
mirror.set_mirror_percentage(10)?;  // Mirror 10% of traffic

// Check if this request should be mirrored
if mirror.should_mirror(request_id) {
    // Send copy of request to staging
    // User sees response from production only
}
```

**Benefits**:
- Test with real traffic without user impact
- Compare behaviors side-by-side
- Validate new implementation safely

## Outlier Detection

Automatically detect and eject unhealthy instances.

### Configuration

```rust
use tai_resilience::outlier_detection::{OutlierDetection, OutlierDetectionConfig};
use std::time::Duration;

let config = OutlierDetectionConfig {
    service_name: "payment-api".to_string(),
    min_request_volume: 100,              // Require 100+ requests
    consecutive_errors_threshold: 5,      // Eject after 5 consecutive
    error_rate_threshold: 0.05,           // Eject if error rate > 5%
    slow_response_threshold_ms: 5000.0,   // 5s is slow
    slow_response_percentage_threshold: 30.0, // Eject if >30% slow
    base_ejection_time: Duration::from_secs(30),
    reinjection_interval: Duration::from_secs(60),
    ..Default::default()
};

let mut od = OutlierDetection::new(config);
```

### Recording Metrics

```rust
// Record successful request
od.record_request("instance-1", true, 100.0);  // 100ms response

// Record failed request
od.record_request("instance-2", false, 500.0); // Failed, 500ms

// Run analysis to detect outliers
od.analyze().await?;

// Get results
let ejected = od.get_ejected_instances();
let healthy = od.get_healthy_instances();
```

### Automatic Re-injection

Ejected instances are periodically probed for recovery:

```rust
// During each analysis cycle:
// 1. Check if ejection time has passed
// 2. If yes, re-inject instance (allow requests again)
// 3. Monitor if it stays healthy
// 4. If healthy, keep it injected
// 5. If unhealthy again, re-eject with longer duration
```

### Respecting SLOs

Outlier detection respects maximum ejection percentage:

```rust
let config = OutlierDetectionConfig {
    max_ejection_percentage: 50,  // Never eject >50% of instances
    ..Default::default()
};

// Even if 8 of 10 instances are unhealthy,
// only 5 will be ejected to maintain capacity
```

## Istio Integration

### VirtualService: Traffic Routing

Controls how traffic is routed to service versions:

```rust
use tai_resilience::service_mesh::{
    VirtualService, HttpRoute, RouteDestination, RouteMatch,
};

let vs = VirtualService {
    name: "my-service".to_string(),
    namespace: "default".to_string(),
    hosts: vec!["my-service".to_string()],
    gateways: vec!["my-gateway".to_string()],
    http_routes: vec![
        HttpRoute {
            name: "canary".to_string(),
            match_conditions: vec![],
            route: vec![
                RouteDestination {
                    host: "my-service-v1".to_string(),
                    subset: Some("v1".to_string()),
                    weight: 90,  // 90% traffic
                    ..Default::default()
                },
                RouteDestination {
                    host: "my-service-v2".to_string(),
                    subset: Some("v2".to_string()),
                    weight: 10,  // 10% traffic (canary)
                    ..Default::default()
                },
            ],
            timeout: "30s".to_string(),
            retries: Some(RetryPolicy {
                max_retries: 3,
                ..Default::default()
            }),
        },
    ],
    tcp_routes: vec![],
};
```

### DestinationRule: Load Balancing

Configures how traffic is distributed across instances:

```rust
use tai_resilience::service_mesh::{DestinationRule, TrafficPolicy};

let dr = DestinationRule {
    name: "my-service".to_string(),
    host: "my-service.default.svc.cluster.local".to_string(),
    namespace: "default".to_string(),
    traffic_policy: TrafficPolicy {
        load_balancer: LoadBalancingAlgorithm::RoundRobin,
        connection_pool: ConnectionPool {
            tcp_max_connections: 100,
            http_max_pending_requests: 100,
            http_max_requests: 1000,
            ..Default::default()
        },
        outlier_detection: OutlierDetectionConfig {
            consecutive_errors: 5,
            interval: "10s".to_string(),
            base_ejection_time: "30s".to_string(),
            ..Default::default()
        },
    },
    subsets: vec![
        Subset {
            name: "v1".to_string(),
            labels: vec![("version".to_string(), "v1".to_string())].into_iter().collect(),
        },
        Subset {
            name: "v2".to_string(),
            labels: vec![("version".to_string(), "v2".to_string())].into_iter().collect(),
        },
    ],
};
```

### Gateway: Ingress Management

Manages inbound traffic from outside the mesh:

```rust
use tai_resilience::service_mesh::Gateway;

let gateway = Gateway {
    name: "my-gateway".to_string(),
    namespace: "default".to_string(),
    selector: vec![("istio".to_string(), "ingressgateway".to_string())]
        .into_iter()
        .collect(),
    servers: vec![
        Server {
            port: PortConfig {
                number: 80,
                protocol: "TCP".to_string(),
                name: "http".to_string(),
            },
            protocol: "HTTP".to_string(),
            hosts: vec!["example.com".to_string()],
            tls: None,
        },
        Server {
            port: PortConfig {
                number: 443,
                protocol: "TCP".to_string(),
                name: "https".to_string(),
            },
            protocol: "HTTPS".to_string(),
            hosts: vec!["example.com".to_string()],
            tls: Some(TlsConfig {
                mode: "SIMPLE".to_string(),
                certificate_chain: Some("/etc/istio/ingressgateway-certs/tls.crt".to_string()),
                private_key: Some("/etc/istio/ingressgateway-certs/tls.key".to_string()),
                ..Default::default()
            }),
        },
    ],
};
```

### PeerAuthentication: mTLS

Enforces mutual TLS between services:

```rust
use tai_resilience::service_mesh::PeerAuthentication;

let peer_auth = PeerAuthentication {
    name: "default".to_string(),
    namespace: "default".to_string(),
    mtls_mode: "STRICT".to_string(),  // Require mTLS for all services
    selector: None,  // Applies to all services in namespace
};
```

### AuthorizationPolicy: Access Control

Controls which services can communicate:

```rust
use tai_resilience::service_mesh::AuthorizationPolicy;

let auth_policy = AuthorizationPolicy {
    name: "allow-frontend-to-api".to_string(),
    namespace: "default".to_string(),
    action: "ALLOW".to_string(),
    rules: vec![
        AuthRule {
            from: Some(vec![
                Source {
                    principals: Some(vec!["cluster.local/ns/default/sa/frontend".to_string()]),
                    ..Default::default()
                },
            ]),
            to: Some(vec![
                Operation {
                    hosts: Some(vec!["api-gateway".to_string()]),
                    ..Default::default()
                },
            ]),
            when: None,
        },
    ],
    selector: None,
};
```

## Security Patterns

### Mutual TLS (mTLS)

All service-to-service communication is encrypted:

```bash
# Enable strict mTLS in namespace
kubectl apply -f - <<EOF
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
  namespace: default
spec:
  mtls:
    mode: STRICT
EOF
```

### Authorization Policies

Fine-grained access control:

```bash
# Only allow frontend to call api-gateway
kubectl apply -f - <<EOF
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: allow-frontend
  namespace: default
spec:
  rules:
  - from:
    - source:
        principals: ["cluster.local/ns/default/sa/frontend"]
    to:
    - operation:
        hosts: ["api-gateway"]
EOF
```

## Observability & Metrics

### Metrics Collected

Circuit Breaker:
- State transitions (Closed → Open → HalfOpen → SlowStart)
- Request counts (total, successful, failed, rejected)
- Failure types and counts
- Response time statistics

Outlier Detection:
- Instance metrics (error rate, slow %, response time)
- Ejection events
- Re-injection events
- System-wide error rate

Traffic Management:
- Traffic split percentages
- Canary stage progress
- Blue-green environment status
- A/B test variant metrics

### Exporting Metrics

```rust
// Circuit breaker metrics
let metrics = cb.get_metrics().await;
metrics_registry.register_gauge("circuit_breaker_state", metrics.state);
metrics_registry.register_counter("circuit_breaker_failures", metrics.failed_requests);

// Outlier detection metrics
let od_metrics = od.get_metrics().await;
metrics_registry.register_gauge("outlier_detection_ejected", od_metrics.ejected_instances.len());
```

## Deployment Strategies

### Canary Deployment Checklist

1. **Pre-deployment**:
   - Run integration tests on new version
   - Verify backward compatibility
   - Plan rollback procedure

2. **Initial Stage (10%)**:
   - Monitor for immediate failures
   - Check error rates, latency
   - Watch for unexpected behaviors

3. **Intermediate Stage (50%)**:
   - Run performance tests
   - Check business metrics (conversion, revenue)
   - Monitor resource utilization

4. **Final Stage (100%)**:
   - Gradual ramp-up of remaining traffic
   - Final verification
   - Remove old version

5. **Post-deployment**:
   - Monitor for hours/days
   - Keep rollback ready
   - Gradually reduce monitoring

### Blue-Green Deployment Checklist

1. **Setup**:
   - Prepare green environment
   - Run smoke tests
   - Verify connectivity

2. **Switch**:
   - Update router/load balancer
   - Switch to green
   - Monitor closely (first hour critical)

3. **Verification**:
   - Run end-to-end tests
   - Check database consistency
   - Verify business flows

4. **Cleanup**:
   - Keep blue as instant fallback for 24-48h
   - Monitor metrics
   - After confidence built, remove blue

## Troubleshooting Guide

### Problem: Circuit Breaker Not Opening

**Symptoms**: Circuit breaker stays in Closed state despite failures

**Root Causes**:
1. Failure threshold not reached: `failure_threshold` set too high
2. Counting wrong failures: Error type mismatch (Timeout vs Error vs SlowResponse)
3. Request succeeds intermittently: Some requests succeed, resetting count

**Solution**:
```rust
// Lower threshold for testing
config.failure_threshold = 2;  // Open after just 2 failures

// Check error type thresholds
config.failure_thresholds.insert(FailureType::Timeout, 2);

// Review logs for actual failures
```

### Problem: Canary Deployment Rolls Back Unexpectedly

**Symptoms**: Canary rolls back during early stages

**Root Causes**:
1. Error rate threshold too sensitive: `error_rate_threshold` too low
2. Transient failures counted: Network hiccups or temporary overload
3. Slow responses counted as errors: `slow_response_threshold` too low

**Solution**:
```rust
let mut canary = CanaryDeployment::new("service", "1.0.0", "1.1.0");
canary = canary.with_error_threshold(0.1);  // Allow 10% errors

// Increase duration for each stage
canary.start(vec![10, 50, 100], Duration::from_secs(600)).await?;
```

### Problem: Traffic Not Splitting Correctly

**Symptoms**: Canary getting 100% traffic instead of 10%

**Root Causes**:
1. VirtualService not applied: Istio not managing traffic
2. DestinationRule not created: Load balancer not configured
3. Service labels incorrect: Subset labels don't match pods

**Solution**:
```bash
# Verify VirtualService applied
kubectl get vs my-service -o yaml

# Check DestinationRule
kubectl get dr my-service -o yaml

# Verify pod labels
kubectl get pods --show-labels

# Debug Envoy config
kubectl exec -it <pod> -c istio-proxy -- \
  curl localhost:15000/config_dump | grep route_config
```

### Problem: Outlier Detection Ejects All Instances

**Symptoms**: All instances ejected, service down

**Root Causes**:
1. `max_ejection_percentage` not set: Ejects more than intended
2. `min_request_volume` too low: Ejects on minimal failures
3. Health check misconfigured: False positives

**Solution**:
```rust
let config = OutlierDetectionConfig {
    max_ejection_percentage: 25,  // Never eject >25%
    min_request_volume: 1000,      // Require more data
    consecutive_errors_threshold: 10,  // Higher threshold
    ..Default::default()
};
```

### Problem: High Memory Usage in Circuit Breaker

**Symptoms**: Memory grows over time

**Root Causes**:
1. Unbounded metrics accumulation: No cleanup of old data
2. Too many concurrent instances: One circuit breaker per service

**Solution**:
```rust
// Reset metrics periodically
#[tokio::task]
async fn cleanup_task(mut cb: Arc<Mutex<CircuitBreaker>>) {
    loop {
        tokio::time::sleep(Duration::from_secs(3600)).await;
        let mut cb = cb.lock().await;
        // Reset would require new implementation
    }
}
```

### Problem: mTLS Certificate Errors

**Symptoms**: "PERMISSION_DENIED: peer certificate verification failed"

**Root Causes**:
1. Certificates not issued: Istiod not running or cert issue
2. PeerAuthentication mode STRICT too strict: Pods without mTLS sidecar
3. Certificate revoked: Old certificate expired

**Solution**:
```bash
# Check certificate
kubectl get cert -A

# Check Istiod logs
kubectl logs -n istio-system -l app=istiod

# Force PeerAuthentication to PERMISSIVE
kubectl apply -f - <<EOF
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
spec:
  mtls:
    mode: PERMISSIVE  # Allow both mTLS and plaintext
EOF
```

---

## Best Practices

1. **Start Conservative**: Use higher thresholds initially, lower gradually
2. **Monitor Everything**: Metrics are your visibility window
3. **Test Rollback**: Practice rollback procedures regularly
4. **Gradual Changes**: Small incremental changes are safer
5. **Load Testing**: Test under realistic load before production
6. **Capacity Planning**: Ensure sufficient resources for resilience
7. **Documentation**: Document your deployment procedures
8. **Alerts**: Set up alerts for circuit breaker state changes
9. **On-call**: Have procedures for handling failures
10. **Regular Reviews**: Review and adjust thresholds monthly

---

**For additional information, see**:
- [Circuit Breaker Pattern](https://martinfowler.com/bliki/CircuitBreaker.html)
- [Istio Documentation](https://istio.io/latest/docs/)
- [Kubernetes Service Mesh](https://kubernetes.io/blog/2020/12/introducing-istio-operator/)
- [Blue-Green Deployment](https://martinfowler.com/bliki/BlueGreenDeployment.html)
- [Canary Deployments](https://martinfowler.com/bliki/CanaryRelease.html)
