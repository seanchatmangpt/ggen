# ggen Metrics Schema Reference

Metric definitions, types, and cardinality for all monitored components.

---

## Supervisor Metrics (:9001)

### `supervisor_restart_count`
- **Type:** Counter
- **Description:** Total number of process restarts
- **Unit:** count
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  rate(supervisor_restart_count[1m])
  ```
- **Expected Values:** 0-1000+ cumulative
- **Alert Threshold:** `rate(...) > 5` per minute
- **Implementation:**
  ```rust
  lazy_static! {
      static ref SUPERVISOR_RESTARTS: Counter = Counter::new(
          "supervisor_restart_count",
          "Total process restarts"
      ).unwrap();
  }

  pub fn record_restart() {
      SUPERVISOR_RESTARTS.inc();
  }
  ```

### `supervisor_uptime_seconds`
- **Type:** Gauge
- **Description:** Process uptime since last restart
- **Unit:** seconds
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  supervisor_uptime_seconds
  ```
- **Expected Values:** 0 (just restarted) to 86400+ (days running)

### `supervisor_memory_bytes`
- **Type:** Gauge
- **Description:** Current process memory usage
- **Unit:** bytes
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  supervisor_memory_bytes / 1024 / 1024  # MB
  ```
- **Expected Values:** 50MB - 1000MB

### `supervisor_cpu_percent`
- **Type:** Gauge
- **Description:** Current CPU usage percentage
- **Unit:** percent (0-100)
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  supervisor_cpu_percent
  ```
- **Expected Values:** 0-100

---

## Circuit Breaker Metrics (:9002)

### `circuit_breaker_state`
- **Type:** Gauge
- **Description:** Current state of circuit breaker
- **Unit:** state code (0=CLOSED, 1=OPEN, 2=HALF_OPEN, 3=SLOW_START)
- **Labels:** `instance`, `job`, `service`, `component`
- **Example Query:**
  ```promql
  circuit_breaker_state{service="auth-service"}
  ```
- **Expected Values:** 0 (most of time), occasional 1,2,3
- **Alert Threshold:** `== 1` for 30 seconds
- **Implementation:**
  ```rust
  #[derive(Clone, Copy)]
  enum CircuitBreakerState {
      Closed = 0,
      Open = 1,
      HalfOpen = 2,
      SlowStart = 3,
  }

  fn update_state_metric(service: &str, state: CircuitBreakerState) {
      CIRCUIT_STATE
          .with_label_values(&[service])
          .set(state as i64);
  }
  ```

### `circuit_breaker_failure_count`
- **Type:** Counter
- **Description:** Total failures that triggered circuit breaker
- **Unit:** count
- **Labels:** `instance`, `job`, `service`, `failure_type` (timeout|error|slow_response)
- **Example Query:**
  ```promql
  rate(circuit_breaker_failure_count{failure_type="timeout"}[5m])
  ```
- **Expected Values:** 0 (healthy) to 1000+ (failures)

### `circuit_breaker_open_timestamp`
- **Type:** Gauge
- **Description:** Unix timestamp when circuit opened
- **Unit:** seconds since epoch
- **Labels:** `instance`, `job`, `service`
- **Example Query:**
  ```promql
  time() - circuit_breaker_open_timestamp  # Time open in seconds
  ```
- **Expected Values:** 0 (closed) or recent timestamp

### `circuit_breaker_success_count`
- **Type:** Counter
- **Description:** Total successful requests since last open
- **Unit:** count
- **Labels:** `instance`, `job`, `service`
- **Example Query:**
  ```promql
  rate(circuit_breaker_success_count[5m])
  ```
- **Expected Values:** 0-1000+

---

## Consensus Metrics (:9003)

### `consensus_node_healthy`
- **Type:** Gauge
- **Description:** Health status of a consensus node
- **Unit:** binary (0=unhealthy, 1=healthy)
- **Labels:** `instance`, `job`, `node_id`, `cluster`
- **Example Query:**
  ```promql
  consensus_node_healthy{cluster="primary"}
  ```
- **Expected Values:** 1 (all nodes healthy) or 0 (node down)
- **Alert Threshold:** `(count healthy / count total) < 0.6666` for 2 min
- **Implementation:**
  ```rust
  fn update_node_health(node_id: &str, cluster: &str, healthy: bool) {
      NODE_HEALTH
          .with_label_values(&[node_id, cluster])
          .set(if healthy { 1 } else { 0 });
  }
  ```

### `consensus_quorum_health`
- **Type:** Gauge
- **Description:** Consensus quorum health (0-100%)
- **Unit:** percent
- **Labels:** `instance`, `job`, `cluster`
- **Example Query:**
  ```promql
  consensus_quorum_health{cluster="primary"}
  ```
- **Expected Values:** 100 (all healthy), 50 (half down), 0 (all down)

### `consensus_term`
- **Type:** Counter
- **Description:** Current consensus term number
- **Unit:** count
- **Labels:** `instance`, `job`, `cluster`
- **Example Query:**
  ```promql
  consensus_term{cluster="primary"}
  ```
- **Expected Values:** Increasing over time (e.g., 1, 2, 3...)

### `consensus_leader_election_duration_seconds`
- **Type:** Gauge
- **Description:** Time taken for last leader election
- **Unit:** seconds
- **Labels:** `instance`, `job`, `cluster`
- **Example Query:**
  ```promql
  consensus_leader_election_duration_seconds
  ```
- **Expected Values:** 0.5 - 30 seconds

### `consensus_committed_index`
- **Type:** Gauge
- **Description:** Highest log index known to be committed
- **Unit:** count
- **Labels:** `instance`, `job`, `cluster`, `node_id`
- **Example Query:**
  ```promql
  consensus_committed_index{cluster="primary"}
  ```
- **Expected Values:** Increasing monotonically

---

## Event Store Metrics (:9004)

### `event_store_operation_duration_seconds`
- **Type:** Histogram
- **Description:** Latency of event store operations
- **Unit:** seconds
- **Labels:** `instance`, `job`, `operation` (read|write|query), `status` (success|error)
- **Buckets:** 0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0
- **Example Query:**
  ```promql
  # p99 latency
  histogram_quantile(0.99, rate(event_store_operation_duration_seconds_bucket[5m]))

  # p95 latency
  histogram_quantile(0.95, rate(event_store_operation_duration_seconds_bucket[5m]))

  # p50 (median) latency
  histogram_quantile(0.50, rate(event_store_operation_duration_seconds_bucket[5m]))

  # Average latency
  rate(event_store_operation_duration_seconds_sum[5m]) /
  rate(event_store_operation_duration_seconds_count[5m])
  ```
- **Expected Values:** p99 < 1.0s (SLO)
- **Alert Threshold:** `histogram_quantile(0.99, ...) > 1.0` for 5 min
- **Implementation:**
  ```rust
  lazy_static! {
      static ref OPERATION_DURATION: HistogramVec = HistogramVec::new(
          HistogramOpts::new(
              "event_store_operation_duration_seconds",
              "Operation latency"
          ).buckets(vec![0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]),
          &["operation", "status"]
      ).unwrap();
  }

  pub fn time_operation<T, F: FnOnce() -> T>(
      operation: &str,
      f: F
  ) -> T {
      let start = Instant::now();
      let result = f();
      let elapsed = start.elapsed().as_secs_f64();

      OPERATION_DURATION
          .with_label_values(&[operation, "success"])
          .observe(elapsed);

      result
  }
  ```

### `event_store_operations_total`
- **Type:** Counter
- **Description:** Total number of operations
- **Unit:** count
- **Labels:** `instance`, `job`, `operation`, `status`
- **Example Query:**
  ```promql
  rate(event_store_operations_total{operation="write"}[5m])
  ```
- **Expected Values:** 0-1000+

### `event_store_errors_total`
- **Type:** Counter
- **Description:** Total operation errors
- **Unit:** count
- **Labels:** `instance`, `job`, `operation`, `error_type`
- **Example Query:**
  ```promql
  rate(event_store_errors_total[5m])
  ```
- **Expected Values:** 0 (healthy) or increasing

### `event_store_queue_depth`
- **Type:** Gauge
- **Description:** Number of pending operations in queue
- **Unit:** count
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  event_store_queue_depth
  ```
- **Expected Values:** 0 (no backlog) to 100+ (under load)

### `event_store_connection_pool_size`
- **Type:** Gauge
- **Description:** Available connections in pool
- **Unit:** count
- **Labels:** `instance`, `job`
- **Example Query:**
  ```promql
  event_store_connection_pool_size
  ```
- **Expected Values:** 10-100 depending on configuration

---

## Health Dashboard Metrics (:8080)

### `http_requests_total`
- **Type:** Counter
- **Description:** Total HTTP requests
- **Unit:** count
- **Labels:** `instance`, `job`, `method`, `path`, `status`
- **Example Query:**
  ```promql
  rate(http_requests_total{status="500"}[5m])
  ```

### `http_request_duration_seconds`
- **Type:** Histogram
- **Description:** HTTP request latency
- **Unit:** seconds
- **Labels:** `instance`, `job`, `method`, `path`
- **Example Query:**
  ```promql
  histogram_quantile(0.99, rate(http_request_duration_seconds_bucket[5m]))
  ```

### `grpc_requests_total`
- **Type:** Counter
- **Description:** Total gRPC requests
- **Unit:** count
- **Labels:** `instance`, `job`, `service`, `method`, `code`

### `grpc_request_duration_seconds`
- **Type:** Histogram
- **Description:** gRPC request latency
- **Unit:** seconds
- **Labels:** `instance`, `job`, `service`, `method`

---

## Cardinality Estimates

### Low Cardinality (Safe)
- `supervisor_restart_count` - 1-5 instances
- `circuit_breaker_state` - 5-20 services
- `consensus_node_healthy` - 3-7 nodes per cluster

### Medium Cardinality (Monitor)
- `event_store_operation_duration_seconds` - 10-50 operation combos
- `http_requests_total` - 50-500 path/method combinations

### High Cardinality (Risk)
- Any metric with user IDs, request IDs, or timestamps as labels
- Avoid: `label_values = high_cardinality_field`

**Cardinality Query:**
```promql
# Top 20 metrics by series count
topk(20, count by (__name__) (count({})))
```

---

## Alerting Best Practices

### Rate of Change
```promql
# Good: Uses rate() to measure per-second change
rate(supervisor_restart_count[1m]) > 5

# Bad: Raw counter (doesn't account for time)
supervisor_restart_count > 1000
```

### Offset for Comparison
```promql
# Spike detection: compare current to 1 hour ago
rate(event_store_errors_total[5m]) > 10 * rate(event_store_errors_total[5m] offset 1h)
```

### Absence
```promql
# Alert if metric hasn't appeared in 5 minutes (scrape failure)
absent(event_store_operation_duration_seconds)
```

---

## Metric Export Format

Prometheus expects text format (no JSON):
```
# HELP metric_name Help text
# TYPE metric_name metric_type
metric_name{label1="value1"} value timestamp
```

Example:
```
# HELP supervisor_restart_count Total process restarts
# TYPE supervisor_restart_count counter
supervisor_restart_count 42
event_store_operation_duration_seconds_bucket{operation="write",le="0.1"} 1234
event_store_operation_duration_seconds_bucket{operation="write",le="1.0"} 5678
event_store_operation_duration_seconds_sum{operation="write"} 3456.78
event_store_operation_duration_seconds_count{operation="write"} 5678
```

---

## Recommended Instrumentation Libraries

### Rust (ggen project)
- `prometheus` crate for metrics export
- `prometheus_static_check!` macro for compile-time validation
- `lazy_static!` for singleton metric instances

### Patterns
```rust
// Counter - strictly increasing
lazy_static! {
    static ref REQUESTS: Counter = Counter::new(...).unwrap();
}
REQUESTS.inc();

// Gauge - can go up/down
lazy_static! {
    static ref MEMORY: Gauge = Gauge::new(...).unwrap();
}
MEMORY.set(memory_usage as f64);

// Histogram - measure distributions
lazy_static! {
    static ref LATENCY: Histogram = Histogram::new(...).unwrap();
}
LATENCY.observe(elapsed.as_secs_f64());

// GaugeVec / CounterVec / HistogramVec - with labels
lazy_static! {
    static ref REQUESTS_VEC: CounterVec = CounterVec::new(...).unwrap();
}
REQUESTS_VEC
    .with_label_values(&["POST", "/api/v1/events"])
    .inc();
```

---

## Validation Checklist

Before shipping metrics to production:

- [ ] All metrics have descriptive HELP text
- [ ] Metric types are correct (Counter/Gauge/Histogram)
- [ ] Units are documented (seconds, bytes, count, percent)
- [ ] Labels have bounded cardinality (< 10k series per metric)
- [ ] Histogram buckets are appropriate for SLO thresholds
- [ ] Rate functions used for counters in alerts
- [ ] Recording rules exist for expensive queries
- [ ] Alert thresholds validated with test data
- [ ] Grafana panels refresh frequently enough (10-30s)
- [ ] Documentation covers interpretation and troubleshooting
