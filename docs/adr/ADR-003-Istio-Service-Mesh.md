# ADR-003: Istio Service Mesh Integration

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Implementing service-to-service communication policies and observability
**Deciders:** Platform Architecture Team

## Problem Statement

TAI microservices need:
- Service-to-service authentication (mTLS)
- Traffic management (load balancing, circuit breaking, retries)
- Observability (distributed tracing, metrics)
- Security policies (encryption, authorization)
- Canary deployments

Manual configuration in application code is error-prone and couples business logic to infrastructure.

## Decision

**Deploy Istio 1.18+ as service mesh with:**
- mTLS by default for all service-to-service traffic
- VirtualServices for traffic routing
- DestinationRules for load balancing policies
- PeerAuthentication for authentication policies
- AuthorizationPolicy for fine-grained access control

## Rationale

### Service Mesh Benefits

1. **Security (mTLS):**
   - Automatic certificate generation and rotation
   - Per-connection encryption without application changes
   - Certificate rotation: 24h default, <1s disruption
   - FIPS 140-2 compliance option

2. **Traffic Management:**
   - Circuit breakers prevent cascading failures
   - Automatic retries with exponential backoff
   - Load balancing algorithms (round-robin, least request)
   - Connection pooling and timeout configuration
   - Canary deployments with weight splitting

3. **Observability:**
   - Automatic distributed tracing (Jaeger integration)
   - Service metrics: request rate, latency, errors
   - Request/response size metrics
   - Protocol detection (HTTP, gRPC, TCP)

4. **Policy Enforcement:**
   - Authentication without code changes
   - Authorization policies based on principals, namespaces, IP blocks
   - Egress control for compliance
   - Rate limiting and quotas

## Architecture

### Istio Components
```
Ingress Gateway (external traffic)
    ↓
Virtual Service (routing policy)
    ↓
Destination Rule (load balancing)
    ↓
Service Instance (Envoy sidecar)
    ↓
Pod (application container)
```

### Deployment Topology
```yaml
# Kubernetes namespaces
- default (system services)
- istio-system (Istiod, ingress/egress gateways)
- tai-system (TAI services with sidecar injection)
```

## Configuration Examples

### Mutual TLS Enforcement
```yaml
# PeerAuthentication: Enforce mTLS for namespace
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
  namespace: tai-system
spec:
  mtls:
    mode: STRICT  # Only mTLS traffic allowed
```

### Circuit Breaker Pattern
```yaml
# DestinationRule: Circuit breaking config
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: governor-circuit-breaker
  namespace: tai-system
spec:
  host: governor.tai-system.svc.cluster.local
  trafficPolicy:
    connectionPool:
      tcp:
        maxConnections: 100
      http:
        http1MaxPendingRequests: 50
        http2MaxRequests: 100
        maxRequestsPerConnection: 2
    outlierDetection:
      consecutive5xxErrors: 5
      interval: 30s
      baseEjectionTime: 30s
      maxEjectionPercent: 50
```

### Canary Deployment
```yaml
# VirtualService: Route 10% to canary, 90% to stable
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: coordinator-canary
  namespace: tai-system
spec:
  hosts:
  - coordinator
  http:
  - match:
    - headers:
        user-agent:
          exact: "canary-tester"
    route:
    - destination:
        host: coordinator
        subset: canary
      weight: 100
  - route:
    - destination:
        host: coordinator
        subset: stable
      weight: 90
    - destination:
        host: coordinator
        subset: canary
      weight: 10
---
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: coordinator
  namespace: tai-system
spec:
  host: coordinator.tai-system.svc.cluster.local
  trafficPolicy:
    connectionPool:
      tcp:
        maxConnections: 100
      http:
        http1MaxPendingRequests: 100
  subsets:
  - name: stable
    labels:
      version: v1
  - name: canary
    labels:
      version: v2
```

### Authorization Policy
```yaml
# AuthorizationPolicy: Coordinator can only be called by Governor and Scheduler
apiVersion: security.istio.io/v1beta1
kind: AuthorizationPolicy
metadata:
  name: coordinator-authz
  namespace: tai-system
spec:
  selector:
    matchLabels:
      app: coordinator
  rules:
  - from:
    - source:
        principals:
        - cluster.local/ns/tai-system/sa/governor
        - cluster.local/ns/tai-system/sa/scheduler
    to:
    - operation:
        methods: ["GET", "POST"]
        paths:
        - /tai.Governor/*
        - /tai.Scheduler/*
```

## Monitoring and Observability

### Prometheus Metrics
- `istio_requests_total`: Total requests by source/dest/method
- `istio_request_duration_milliseconds_bucket`: Request latency
- `istio_request_bytes`: Request size
- `envoy_cluster_upstream_cx`: Connection pool stats

### Distributed Tracing (Jaeger)
```yaml
apiVersion: telemetry.istio.io/v1alpha1
kind: Telemetry
metadata:
  name: tai-tracing
  namespace: tai-system
spec:
  tracing:
  - providers:
    - name: jaeger
    randomSamplingPercentage: 100  # Sample all traces in dev
```

### Service Graph Visualization
- Kiali dashboard shows real-time service topology
- Request flow visualization
- Error rate heatmap
- Performance metrics per service pair

## Implementation Checklist

1. **Istio Installation:**
   - `istioctl install --set profile=production`
   - Configure ingress gateway for external traffic
   - Enable sidecar injection in tai-system namespace

2. **Service Configuration:**
   - Apply PeerAuthentication for mTLS
   - Define VirtualServices for routing policies
   - Configure DestinationRules for traffic policies
   - Implement AuthorizationPolicies

3. **Observability Setup:**
   - Deploy Prometheus for metrics collection
   - Deploy Jaeger for distributed tracing
   - Deploy Kiali for service visualization
   - Configure alerts for error rate > 5%

4. **Testing:**
   - Verify mTLS handshakes with `istioctl authn tls-check`
   - Test circuit breaker with load spike
   - Validate canary weight shifting works
   - Confirm distributed tracing captures calls

## Consequences

### Positive
- Transparent mTLS without code changes
- Advanced traffic management capabilities
- Rich observability and debugging
- Policy enforcement at infrastructure layer
- Proven in production at scale

### Negative
- Operational complexity (learning curve)
- Resource overhead (Envoy sidecars ~50MB memory each)
- Debugging complexity (traffic goes through proxy)
- Version compatibility with Kubernetes
- Istio control plane management burden

## Performance Impact

- **Latency:** +5-10ms per hop (Envoy proxy overhead)
- **Throughput:** -5-10% (encryption, proxy processing)
- **Memory:** +50-100MB per pod (sidecar)
- **CPU:** +100-200m per pod during load

Mitigation: Use connection pooling and HTTP/2.

## Migration Path

1. **Phase 1:** Deploy Istio in monitoring mode (no enforcement)
2. **Phase 2:** Enable mTLS in PERMISSIVE mode (allows both)
3. **Phase 3:** Switch to STRICT mTLS (enforcement)
4. **Phase 4:** Implement advanced policies (circuit breaker, canary)

## References
- [Istio Documentation](https://istio.io/latest/docs/)
- [Istio Security](https://istio.io/latest/docs/concepts/security/)
- [Istio Traffic Management](https://istio.io/latest/docs/concepts/traffic-management/)
- [Kiali Visualization](https://kiali.io/)
- Helm chart: helm/tai-chart/istio-values.yaml
