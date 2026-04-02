# Monitoring and Observability

## Overview

Comprehensive observability for distributed microservices using OpenTelemetry, Jaeger, Prometheus, and structured logging.

## Distributed Tracing with Jaeger

### Access Jaeger UI

```bash
# Docker Compose
open http://localhost:16686

# Kubernetes
kubectl port-forward service/jaeger-query 16686:16686
open http://localhost:16686
```

### Trace Analysis

**Finding Traces:**
1. Select service: `api-gateway`, `user-service`, `product-service`, `order-service`
2. Set time range: Last 1 hour
3. Filter by operation: `GET /users`, `POST /orders`
4. Click "Find Traces"

**Analyzing a Trace:**
```
Trace ID: 0af7651916cd43dd8448eb211c80319c
Total Duration: 245ms

├── api-gateway (200ms)
│   ├── http.method: POST
│   ├── http.route: /orders
│   ├── http.status_code: 201
│   │
│   ├── user-service (50ms)
│   │   ├── operation: get-user
│   │   ├── db.query: SELECT * FROM users WHERE id = $1
│   │   └── db.duration: 20ms
│   │
│   └── product-service (80ms)
│       ├── operation: get-product
│       ├── db.query: db.products.findOne({id: "prod-123"})
│       └── db.duration: 35ms
```

**Key Metrics:**
- **P50 Latency**: Median request duration
- **P95 Latency**: 95th percentile (slow requests)
- **P99 Latency**: 99th percentile (very slow requests)
- **Error Rate**: Percentage of failed requests
- **Throughput**: Requests per second

### Trace Context Propagation

OpenTelemetry automatically propagates trace context:

```http
GET /users/123 HTTP/1.1
Host: user-service:8001
traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
tracestate: vendor=data
```

**Traceparent Format:**
- Version: `00`
- Trace ID: `0af7651916cd43dd8448eb211c80319c` (128-bit)
- Parent Span ID: `b7ad6b7169203331` (64-bit)
- Trace Flags: `01` (sampled)

### Custom Spans

**Rust (Axum):**
```rust
use opentelemetry::trace::{Tracer, get_active_span};

async fn process_order(order: Order) {
    let tracer = global::tracer("order-service");
    let span = tracer.start("process-payment");

    span.set_attribute("order.id", order.id.to_string());
    span.set_attribute("order.amount", order.total);

    // Process payment
    let result = charge_payment(&order).await;

    if result.is_err() {
        span.set_attribute("error", true);
    }

    span.end();
}
```

**TypeScript (Express):**
```typescript
import { trace } from '@opentelemetry/api';

app.post('/users', async (req, res) => {
  const tracer = trace.getTracer('user-service');
  const span = tracer.startSpan('create-user');

  span.setAttribute('user.email', req.body.email);

  try {
    const user = await createUser(req.body);
    res.status(201).json(user);
  } catch (error) {
    span.setAttribute('error', true);
    span.setAttribute('error.message', error.message);
    res.status(500).json({ error: 'Failed to create user' });
  } finally {
    span.end();
  }
});
```

**Python (FastAPI):**
```python
from opentelemetry import trace

@app.post('/products')
async def create_product(product: Product):
    tracer = trace.get_tracer(__name__)
    with tracer.start_as_current_span('create-product') as span:
        span.set_attribute('product.id', product.id)
        span.set_attribute('product.price', product.price)

        # Save to database
        products[product.id] = product
        return product
```

## Metrics with Prometheus

### Metrics Endpoints

All services expose Prometheus metrics at `/metrics`:

```bash
# API Gateway
curl http://localhost:8000/metrics

# User Service
curl http://localhost:8001/metrics

# Product Service
curl http://localhost:8002/metrics

# Order Service
curl http://localhost:8003/metrics
```

### Standard Metrics

**HTTP Request Metrics:**
```prometheus
# Request count
http_requests_total{service="api-gateway",method="GET",path="/users",status="200"} 1234

# Request duration histogram
http_request_duration_seconds_bucket{service="user-service",le="0.1"} 890
http_request_duration_seconds_bucket{service="user-service",le="0.5"} 980
http_request_duration_seconds_bucket{service="user-service",le="1.0"} 995

# Request size
http_request_size_bytes_sum{service="product-service"} 1048576
http_request_size_bytes_count{service="product-service"} 500
```

**Circuit Breaker Metrics:**
```prometheus
# Circuit breaker state (0=closed, 1=open, 2=half-open)
circuit_breaker_state{service="api-gateway",target="user-service"} 0

# Failure count
circuit_breaker_failures_total{service="api-gateway",target="user-service"} 3

# Success count after recovery
circuit_breaker_successes_total{service="api-gateway",target="user-service"} 150
```

**Database Metrics:**
```prometheus
# Connection pool usage
db_pool_connections_active{service="user-service",database="postgres"} 8
db_pool_connections_idle{service="user-service",database="postgres"} 2
db_pool_connections_max{service="user-service",database="postgres"} 10

# Query duration
db_query_duration_seconds{service="order-service",query="insert_order"} 0.045
```

### Prometheus Configuration

**prometheus.yml:**
```yaml
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'api-gateway'
    static_configs:
      - targets: ['api-gateway:8000']
    metrics_path: '/metrics'

  - job_name: 'user-service'
    static_configs:
      - targets: ['user-service:8001']

  - job_name: 'product-service'
    static_configs:
      - targets: ['product-service:8002']

  - job_name: 'order-service'
    static_configs:
      - targets: ['order-service:8003']

  - job_name: 'postgres'
    static_configs:
      - targets: ['postgres-exporter:9187']

  - job_name: 'mongodb'
    static_configs:
      - targets: ['mongodb-exporter:9216']
```

### PromQL Queries

**Request Rate:**
```promql
# Requests per second
rate(http_requests_total[5m])

# By service
sum(rate(http_requests_total[5m])) by (service)

# By status code
sum(rate(http_requests_total{status=~"5.."}[5m])) by (service)
```

**Error Rate:**
```promql
# Percentage of errors
sum(rate(http_requests_total{status=~"5.."}[5m]))
/
sum(rate(http_requests_total[5m])) * 100

# Errors by service
sum(rate(http_requests_total{status=~"5.."}[5m])) by (service)
```

**Latency:**
```promql
# P50 latency
histogram_quantile(0.5,
  rate(http_request_duration_seconds_bucket[5m])
)

# P95 latency
histogram_quantile(0.95,
  rate(http_request_duration_seconds_bucket[5m])
)

# P99 latency
histogram_quantile(0.99,
  rate(http_request_duration_seconds_bucket[5m])
)
```

**Throughput:**
```promql
# Total throughput
sum(rate(http_requests_total[1m]))

# Per service
sum(rate(http_requests_total[1m])) by (service)
```

## Health Checks

### Liveness Probes

Checks if service is running:

```bash
# API Gateway
curl http://localhost:8000/health/live
# Response: "alive"

# User Service
curl http://localhost:8001/health/live
# Response: "alive"
```

**Implementation (Rust):**
```rust
async fn health_live() -> &'static str {
    "alive"
}
```

### Readiness Probes

Checks if service can handle traffic:

```bash
# API Gateway
curl http://localhost:8000/health/ready
# Response: "ready" or 503 if dependencies unavailable
```

**Implementation (TypeScript):**
```typescript
app.get('/health/ready', async (req, res) => {
  try {
    // Check database connection
    await db.query('SELECT 1');
    res.send('ready');
  } catch (error) {
    res.status(503).send('not ready');
  }
});
```

**Kubernetes Configuration:**
```yaml
livenessProbe:
  httpGet:
    path: /health/live
    port: 8000
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /health/ready
    port: 8000
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 2
```

## Structured Logging

### Log Format

All services use JSON structured logging:

```json
{
  "timestamp": "2024-01-15T10:30:45.123Z",
  "level": "info",
  "service": "user-service",
  "trace_id": "0af7651916cd43dd8448eb211c80319c",
  "span_id": "b7ad6b7169203331",
  "message": "User created successfully",
  "user_id": "user-123",
  "email": "user@example.com",
  "duration_ms": 45
}
```

### Log Levels

- **DEBUG**: Detailed information for debugging
- **INFO**: General informational messages
- **WARN**: Warning messages for non-critical issues
- **ERROR**: Error messages for failures
- **FATAL**: Critical errors causing service shutdown

### Logging Best Practices

**Do:**
- Include trace_id and span_id for correlation
- Log structured data (JSON)
- Include duration for operations
- Log user actions (with privacy considerations)

**Don't:**
- Log sensitive data (passwords, tokens)
- Log at DEBUG in production
- Include stack traces in INFO logs
- Log every request (use sampling)

### Log Aggregation

**Using ELK Stack:**

```yaml
# Filebeat configuration
filebeat.inputs:
  - type: container
    paths:
      - '/var/lib/docker/containers/*/*.log'
    processors:
      - add_kubernetes_metadata:
          in_cluster: true

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
  index: "microservices-%{+yyyy.MM.dd}"
```

**Kibana Queries:**
```
# All errors in last hour
level:error AND timestamp:[now-1h TO now]

# Specific trace
trace_id:"0af7651916cd43dd8448eb211c80319c"

# Slow requests
duration_ms:>1000 AND service:user-service
```

## Alerting Rules

### Prometheus Alerting

**alerts.yml:**
```yaml
groups:
  - name: microservices
    interval: 30s
    rules:
      # High error rate
      - alert: HighErrorRate
        expr: |
          sum(rate(http_requests_total{status=~"5.."}[5m])) by (service)
          /
          sum(rate(http_requests_total[5m])) by (service)
          > 0.05
        for: 5m
        labels:
          severity: critical
        annotations:
          summary: "High error rate on {{ $labels.service }}"
          description: "Error rate is {{ $value | humanizePercentage }}"

      # High latency
      - alert: HighLatency
        expr: |
          histogram_quantile(0.95,
            rate(http_request_duration_seconds_bucket[5m])
          ) > 1.0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "High latency detected"
          description: "P95 latency is {{ $value }}s"

      # Circuit breaker open
      - alert: CircuitBreakerOpen
        expr: circuit_breaker_state == 1
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Circuit breaker open for {{ $labels.target }}"

      # Service down
      - alert: ServiceDown
        expr: up{job=~".*-service"} == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Service {{ $labels.job }} is down"

      # High memory usage
      - alert: HighMemoryUsage
        expr: |
          container_memory_usage_bytes{container!=""}
          /
          container_spec_memory_limit_bytes{container!=""}
          > 0.9
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "High memory usage on {{ $labels.container }}"
```

### Notification Channels

**Alertmanager Configuration:**
```yaml
route:
  group_by: ['alertname', 'service']
  group_wait: 10s
  group_interval: 10s
  repeat_interval: 12h
  receiver: 'team-notifications'

receivers:
  - name: 'team-notifications'
    slack_configs:
      - api_url: 'https://hooks.slack.com/services/XXX'
        channel: '#alerts'
        title: '{{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'

    pagerduty_configs:
      - service_key: 'YOUR_PAGERDUTY_KEY'
        severity: '{{ .CommonLabels.severity }}'

    email_configs:
      - to: 'team@example.com'
        from: 'alerts@example.com'
```

## Dashboards

### Grafana Dashboard

**Import Dashboard:**
1. Access Grafana: `http://localhost:3000`
2. Import → Upload JSON
3. Use provided `grafana-dashboard.json`

**Key Panels:**
- Request rate by service
- Error rate by service
- P95/P99 latency
- Circuit breaker states
- Database connection pools
- Memory and CPU usage

**PromQL Examples:**
```promql
# Request rate
sum(rate(http_requests_total[1m])) by (service)

# Error percentage
sum(rate(http_requests_total{status=~"5.."}[5m])) by (service)
/
sum(rate(http_requests_total[5m])) by (service) * 100
```

## Performance Analysis

### Identifying Bottlenecks

1. **Check Jaeger for slow spans**
   - Find traces with high duration
   - Identify which service/operation is slow
   - Examine database queries

2. **Review Prometheus metrics**
   - Check P95/P99 latencies
   - Identify services with high error rates
   - Monitor resource usage

3. **Analyze logs**
   - Search for slow operations
   - Check for errors and warnings
   - Correlate with trace IDs

### Optimization Strategies

**Database:**
- Add indexes for frequent queries
- Implement connection pooling
- Use read replicas for read-heavy workloads
- Cache frequently accessed data

**Application:**
- Enable HTTP/2 and multiplexing
- Implement response caching
- Optimize circuit breaker thresholds
- Tune thread pool sizes

**Infrastructure:**
- Scale horizontally (add replicas)
- Use faster instance types
- Optimize network latency
- Enable CDN for static assets

## Monitoring Checklist

- [ ] All services reporting to Jaeger
- [ ] Prometheus scraping all `/metrics` endpoints
- [ ] Health checks responding correctly
- [ ] Alerts configured and tested
- [ ] Dashboards created in Grafana
- [ ] Log aggregation working
- [ ] Trace sampling configured (1.0 = 100%)
- [ ] Circuit breakers tested
- [ ] SLO/SLA targets defined

## SLO/SLA Targets

**Service Level Objectives:**
- Availability: 99.9% (43.2 minutes downtime/month)
- Latency: P95 < 500ms, P99 < 1000ms
- Error Rate: < 1% of requests
- Throughput: 10,000 req/s per service

**Service Level Agreements:**
- Customer-facing API: 99.95% availability
- Response time: 95% of requests < 200ms
- Support response: < 1 hour for critical issues
