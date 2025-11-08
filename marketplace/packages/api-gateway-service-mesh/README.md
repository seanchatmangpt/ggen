# API Gateway & Service Mesh

Enterprise-grade API gateway with service mesh integration (Istio, Linkerd, Consul Connect).

## Features

### Core Capabilities
- **API Gateway**: High-performance routing, load balancing, rate limiting
- **Service Mesh Integration**: Istio, Linkerd, Consul Connect support
- **Security**: mTLS, JWT authentication, OAuth2, API keys
- **Resilience**: Circuit breakers, retries, timeouts, bulkheads
- **Observability**: Distributed tracing (Jaeger, Zipkin), metrics, logging
- **Traffic Management**: Canary deployments, blue-green, traffic splitting

### Service Mesh Features
- **Mutual TLS (mTLS)**: STRICT, PERMISSIVE, DISABLE modes
- **Certificate Management**: Automatic rotation, CA integration
- **Service Discovery**: Kubernetes, Consul, Eureka
- **Health Checks**: Liveness, readiness, startup probes
- **Load Balancing**: Round-robin, weighted, least connections
- **Sidecar Proxies**: Envoy integration

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    API Gateway                          │
│  ┌─────────────┐  ┌──────────────┐  ┌───────────────┐ │
│  │ Rate Limiter│  │Circuit Breaker│  │Retry Policy   │ │
│  └─────────────┘  └──────────────┘  └───────────────┘ │
└───────────────────────┬─────────────────────────────────┘
                        │
        ┌───────────────┼───────────────┐
        │               │               │
   ┌────▼────┐    ┌────▼────┐    ┌────▼────┐
   │Service A│    │Service B│    │Service C│
   │ +Sidecar│    │ +Sidecar│    │ +Sidecar│
   └─────────┘    └─────────┘    └─────────┘
        │               │               │
        └───────────────┼───────────────┘
                        │
                ┌───────▼────────┐
                │ Control Plane  │
                │ (Istio/Linkerd)│
                └────────────────┘
```

## Quick Start

### Installation

```bash
# Add to ggen project
ggen install api-gateway-service-mesh

# Or use with Cargo
cargo add api-gateway-service-mesh
```

### Basic Usage (Rust)

```rust
use api_gateway_service_mesh::{GatewayState, ServiceMeshConfig, RouteConfig};

#[tokio::main]
async fn main() {
    // Configure service mesh
    let mesh_config = ServiceMeshConfig {
        mesh_type: "istio".to_string(),
        mtls_mode: "STRICT".to_string(),
        sidecar_port: 15001,
        control_plane_endpoint: "istiod.istio-system:15012".to_string(),
    };

    let gateway = GatewayState::new(mesh_config);

    // Add route with circuit breaker
    gateway.add_route(RouteConfig {
        path: "/api/v1/users".to_string(),
        method: "GET".to_string(),
        backend_service: "users-service:8080".to_string(),
        rate_limit_rps: Some(100),
        circuit_breaker: Some(CircuitBreakerConfig {
            failure_threshold: 5,
            timeout_seconds: 30,
            half_open_requests: 3,
        }),
        retry_policy: Some(RetryConfig {
            max_attempts: 3,
            backoff_multiplier: 2.0,
            initial_delay_ms: 100,
        }),
        timeout_seconds: 5,
    }).await;

    // Create router
    let app = create_router(Arc::new(gateway));

    // Start server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
```

### TypeScript Usage

```typescript
import { ApiGateway, ServiceMeshConfig } from 'api-gateway-service-mesh';

const meshConfig: ServiceMeshConfig = {
  meshType: 'istio',
  mtlsMode: 'STRICT',
  sidecarPort: 15001,
  controlPlaneEndpoint: 'istiod.istio-system:15012'
};

const gateway = new ApiGateway(meshConfig);

gateway.addRoute({
  path: '/api/v1/orders',
  method: 'POST',
  backendService: 'orders-service:8080',
  rateLimitRps: 50,
  circuitBreaker: {
    failureThreshold: 5,
    timeoutSeconds: 30,
    halfOpenRequests: 3
  }
});

gateway.listen(8080);
```

### Python Usage

```python
from api_gateway_service_mesh import ApiGateway, ServiceMeshConfig, RouteConfig

mesh_config = ServiceMeshConfig(
    mesh_type="istio",
    mtls_mode="STRICT",
    sidecar_port=15001,
    control_plane_endpoint="istiod.istio-system:15012"
)

gateway = ApiGateway(mesh_config)

gateway.add_route(RouteConfig(
    path="/api/v1/products",
    method="GET",
    backend_service="products-service:8080",
    rate_limit_rps=200,
    timeout_seconds=5
))

gateway.run(host="0.0.0.0", port=8080)
```

## Configuration

### Istio Integration

```yaml
# VirtualService for API Gateway
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: api-gateway
spec:
  hosts:
  - api.example.com
  gateways:
  - api-gateway
  http:
  - match:
    - uri:
        prefix: /api/v1/
    route:
    - destination:
        host: api-gateway
        port:
          number: 8080
    retries:
      attempts: 3
      perTryTimeout: 2s
    timeout: 10s
```

### Circuit Breaker Configuration

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: users-service
spec:
  host: users-service
  trafficPolicy:
    connectionPool:
      http:
        http1MaxPendingRequests: 100
        maxRequestsPerConnection: 2
    outlierDetection:
      consecutiveErrors: 5
      interval: 30s
      baseEjectionTime: 30s
      maxEjectionPercent: 50
```

### mTLS Policy

```yaml
apiVersion: security.istio.io/v1beta1
kind: PeerAuthentication
metadata:
  name: default
spec:
  mtls:
    mode: STRICT
```

## RDF Ontology

The package includes a comprehensive RDF ontology (350+ lines) defining:

- API Gateway components (routes, load balancers, rate limiters)
- Service mesh concepts (sidecars, control plane, data plane)
- Security (mTLS, certificates, authentication)
- Resilience patterns (circuit breakers, retries, timeouts)
- Observability (distributed tracing, metrics)
- Traffic management (canary deployments, traffic splitting)

### SPARQL Queries

Query all routes with rate limiting:
```sparql
PREFIX agw: <https://ggen.ai/ontology/api-gateway#>

SELECT ?route ?path ?ratelimit
WHERE {
  ?route a agw:Route ;
         agw:routePath ?path ;
         agw:hasRateLimiter ?rl .
  ?rl agw:requestsPerSecond ?ratelimit .
}
```

Find services requiring mTLS:
```sparql
PREFIX agw: <https://ggen.ai/ontology/api-gateway#>

SELECT ?service ?mtlsMode
WHERE {
  ?service agw:hasMutualTLS ?mtls .
  ?mtls agw:mtlsMode ?mtlsMode .
  FILTER(?mtlsMode = "STRICT")
}
```

## Testing

### Run Chicago TDD Tests

```bash
# Rust tests
cargo test

# TypeScript tests
npm test

# Python tests
pytest
```

### Test Coverage

- **Unit Tests**: Route registration, circuit breakers, rate limiting
- **Integration Tests**: mTLS, distributed tracing, health checks
- **Performance Tests**: Concurrent request handling, throughput
- **Security Tests**: JWT validation, API key authentication
- **Resilience Tests**: Retry logic, circuit breaker states

## Performance

### Benchmarks

- **Throughput**: 50,000+ requests/second
- **Latency**: p50 < 5ms, p99 < 50ms
- **Circuit Breaker Overhead**: < 1ms
- **Rate Limiter Overhead**: < 0.5ms

### Optimization Tips

1. **Connection Pooling**: Reuse connections to backends
2. **HTTP/2**: Enable HTTP/2 for multiplexing
3. **Compression**: Use gzip/brotli for responses
4. **Caching**: Cache route lookups and health checks
5. **Async I/O**: Use Tokio for non-blocking I/O

## Production Deployment

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-gateway
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: gateway
        image: api-gateway:latest
        ports:
        - containerPort: 8080
        env:
        - name: MESH_TYPE
          value: "istio"
        - name: MTLS_MODE
          value: "STRICT"
        resources:
          requests:
            cpu: "500m"
            memory: "512Mi"
          limits:
            cpu: "2000m"
            memory: "2Gi"
```

### High Availability

- **Multiple Replicas**: Deploy 3+ gateway instances
- **Load Balancing**: Use Kubernetes Service or external LB
- **Health Checks**: Configure liveness and readiness probes
- **Auto-Scaling**: HPA based on CPU/memory/RPS

## Security Best Practices

1. **mTLS**: Always use STRICT mode in production
2. **Certificate Rotation**: Automate with cert-manager
3. **JWT Validation**: Verify signature, expiration, claims
4. **API Keys**: Use with scopes and rate limits
5. **OAuth2**: Implement PKCE for public clients
6. **Audit Logging**: Log all authentication/authorization events

## Troubleshooting

### Circuit Breaker Always Open

- Check failure threshold is appropriate
- Verify backend health checks
- Review timeout configuration
- Check network connectivity

### mTLS Handshake Failures

- Verify certificate validity and expiration
- Check CA certificate chain
- Ensure sidecar proxy is running
- Review Istio/Linkerd logs

### Rate Limiting Too Aggressive

- Increase RPS limit
- Adjust burst capacity
- Check for bursty traffic patterns
- Consider per-client rate limiting

## License

MIT

## Support

- **Documentation**: https://ggen.ai/docs/api-gateway
- **Issues**: https://github.com/ggen/marketplace/issues
- **Slack**: #api-gateway channel
