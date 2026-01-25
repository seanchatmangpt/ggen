# TAI Pattern Reference Implementation

This directory contains two complete, production-like reference implementations of the **TAI Pattern** (Signal-Policy-Action) demonstrating all **Toyota Production System** principles in action.

## What is TAI?

**TAI** = **Signal** → **Policy** → **Action**

A deterministic pattern for processing requests where:
- **Signal**: Incoming request/event (e.g., payment, deployment)
- **Policy**: Business rules/constraints (fraud detection, safety checks)
- **Action**: Execute operation (charge card, deploy service)

Enhanced with TPS principles:
- **Jidoka**: Autonomation (circuit breaker, auto-rollback)
- **Kanban**: Pull-based queuing (async processing, sequential ops)
- **Heijunka**: Load leveling (gradual rollout, rate limiting)
- **Kaizen**: Continuous improvement (metrics, trend analysis)
- **Andon**: Visual signals (alerts on SLO violations)
- **Tracing**: End-to-end observability

## Quick Start (5 Minutes)

### 1. Start the Full Stack

```bash
cd examples/tai-reference

# Build and start all services
docker-compose up -d

# Wait for services to be healthy
docker-compose ps
```

Services will be available at:
- Payment System: http://localhost:3001
- Deployment System: http://localhost:3002
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3000 (admin/admin)
- Jaeger: http://localhost:16686
- NATS: nats://localhost:4222
- Vault: http://localhost:8200

### 2. Test Payment System

```bash
# Process a payment
curl -X POST http://localhost:3001/payment \
  -H "Content-Type: application/json" \
  -d '{
    "amount": 99.99,
    "customer_id": "cust_123",
    "currency": "USD",
    "merchant_id": "merc_456"
  }'

# Expected response:
# {
#   "transaction_id": "txn_abc123...",
#   "status": "SUCCESS",
#   "amount": 99.99,
#   "currency": "USD",
#   "processing_time_ms": 145,
#   "fraud_score": 0.25,
#   "gateway_response": "txn_stripe_..."
# }

# Get metrics
curl http://localhost:3001/metrics
```

### 3. Test Deployment System

```bash
# Trigger a deployment
curl -X POST http://localhost:3002/deploy \
  -H "Content-Type: application/json" \
  -d '{
    "service": "api-service",
    "version": "v2.0.0",
    "target_region": "us-west-2",
    "rollout_strategy": "canary"
  }'

# Expected response:
# {
#   "deployment_id": "dpl_xyz789...",
#   "service": "api-service",
#   "version": "v2.0.0",
#   "status": "SUCCESS",
#   "duration_secs": 45,
#   "stages_completed": ["validation", "canary", "staging", "prod"]
# }

# Get metrics
curl http://localhost:3002/metrics
```

### 4. View Dashboards

- **Grafana** (http://localhost:3000):
  - Payment system metrics dashboard
  - Deployment system metrics dashboard
  - Circuit breaker status
  - Fraud detection rates

- **Jaeger** (http://localhost:16686):
  - Trace each payment request
  - Trace each deployment through all stages
  - View service dependencies
  - Identify bottlenecks

## Architecture Overview

### Payment System

```
Client HTTP Request
    ↓
PaymentService::process_payment()
    ├─ Signal: PaymentSignal { amount, customer_id, currency, merchant_id }
    │
    ├─ Policy: FraudPolicyEvaluator
    │   ├─ Blacklist check
    │   ├─ Velocity check (max 5/min per customer)
    │   ├─ Amount anomaly (flag > $5000)
    │   └─ Geolocation anomaly (multiple currencies)
    │
    ├─ Jidoka: PaymentGatewayCircuitBreaker
    │   ├─ Closed: Process normally
    │   ├─ Open: Fail fast (gateway unavailable)
    │   └─ HalfOpen: Attempt recovery
    │
    ├─ Action: Call payment gateway
    │   └─ Simulate Stripe API call with 5% failure rate
    │
    ├─ Kanban: Async Pub/Sub
    │   └─ Publish successful payments to NATS for async processing
    │
    ├─ Heijunka: Rate limiting
    │   └─ Smooth payment flow (no burst spikes)
    │
    ├─ Kaizen: Metrics collection
    │   ├─ Transaction success rate
    │   ├─ Fraud detection rate
    │   ├─ Average latency
    │   └─ Circuit breaker trip count
    │
    ├─ Andon: Alert thresholds
    │   ├─ Fraud rate > 2% → ALERT
    │   ├─ Latency > 5s → ALERT
    │   └─ Circuit breaker open → ALERT
    │
    └─ Tracing: OpenTelemetry
        └─ Trace ID correlates entire transaction path
```

### Deployment System

```
Client HTTP Request
    ↓
DeploymentOrchestrator::orchestrate_deployment()
    ├─ Signal: DeploymentRequest { service, version, region, strategy }
    │
    ├─ Policy: DeploymentPolicyValidator
    │   ├─ Image exists?
    │   ├─ Cluster has capacity?
    │   └─ Rollback plan exists?
    │
    ├─ Heijunka: Multi-stage rollout
    │   ├─ Canary: 5% traffic (5 min)
    │   ├─ Staging: 25% traffic (10 min)
    │   └─ Production: 100% traffic (full rollout)
    │
    ├─ Action: Deploy to infrastructure
    │   ├─ Apply Terraform config
    │   ├─ Deploy to Kubernetes
    │   └─ Update DNS/load balancer
    │
    ├─ Jidoka: Automatic rollback
    │   ├─ Health check failure → Automatic rollback
    │   ├─ Error rate > 5% → Automatic rollback
    │   └─ P99 latency > SLO → Automatic rollback
    │
    ├─ Kanban: Sequential regional deployment
    │   ├─ Deploy to US-WEST first
    │   ├─ Wait for success
    │   └─ Then deploy to US-EAST
    │
    ├─ Kaizen: Deployment metrics
    │   ├─ Deployment success rate
    │   ├─ Average deployment duration
    │   ├─ Rollback frequency
    │   └─ Stage duration tracking
    │
    ├─ Andon: Deployment alerts
    │   ├─ Deployment failure → ALERT
    │   ├─ Health check failure → ALERT
    │   └─ Automatic rollback triggered → ALERT
    │
    └─ Tracing: OpenTelemetry
        └─ Trace ID correlates entire deployment path
```

## Pattern Details

### 1. Signal - The Starting Point

**Payment System**:
```json
{
  "amount": 99.99,
  "customer_id": "cust_123",
  "currency": "USD",
  "merchant_id": "merc_456"
}
```

**Deployment System**:
```json
{
  "service": "api-service",
  "version": "v2.0.0",
  "target_region": "us-west-2",
  "rollout_strategy": "canary"
}
```

### 2. Policy - Business Rules

**Payment System Policy**:
- Reject if customer on blacklist (fraud_score += 100)
- Reject if > 5 transactions/minute (fraud_score += 50)
- Flag if amount > $5000 (fraud_score += 30)
- Flag if geolocation anomaly (fraud_score += 20)
- REJECT if fraud_score ≥ 50

**Deployment System Policy**:
- Image must exist in registry
- Cluster must have capacity
- Valid rollback plan must exist
- Health metrics must pass (error_rate < 5%, latency_p99 < 1s)

### 3. Action - Execution

**Payment System**:
1. Call payment gateway (with circuit breaker protection)
2. Record receipt in audit log
3. Publish to Kanban queue for async processing

**Deployment System**:
1. Execute Canary rollout (5% traffic)
2. Monitor health metrics
3. If healthy, proceed to Staging (25%)
4. If healthy, proceed to Production (100%)
5. If unhealthy at any stage, automatic rollback

### 4. Jidoka - Autonomation (Automatic Fault Isolation)

**Payment System**:
- Circuit breaker on payment gateway
- Fails fast when gateway unavailable
- Prevents cascading failures
- Attempts recovery after timeout

**Deployment System**:
- Automatic rollback on health failure
- Triggers if error_rate > 5%
- Triggers if p99_latency > SLO
- Prevents bad deployments from reaching production

### 5. Kanban - Pull-Based Queuing

**Payment System**:
- Successful payments published to NATS
- Async workers process confirmations
- Decouples real-time request from background jobs
- Prevents overload (bounded queue)

**Deployment System**:
- Sequential regional deployment queue
- One deployment per region at a time
- Prevents cascading regional failures
- Reduces blast radius of mistakes

### 6. Heijunka - Load Leveling

**Payment System**:
- Rate limiting per customer (5/min)
- Smooth transaction flow
- Prevents burst spikes
- Protects against DDoS

**Deployment System**:
- Gradual rollout stages:
  - Canary: 5% traffic (5 min) - Catch bugs early
  - Staging: 25% traffic (10 min) - Increase exposure
  - Production: 100% traffic - Full rollout
- Smooth, controlled deployment
- Early failure detection with minimal impact

### 7. Kaizen - Continuous Improvement Metrics

**Payment System**:
```
{
  "total_transactions": 10000,
  "successful_transactions": 9800,
  "fraudulent_transactions": 150,
  "success_rate_percent": 98.0,
  "fraud_rate_percent": 1.5,
  "avg_latency_ms": 245,
  "circuit_breaker_trips": 3
}
```

**Deployment System**:
```
{
  "total_deployments": 250,
  "successful_deployments": 247,
  "rollback_count": 2,
  "success_rate_percent": 98.8,
  "avg_duration_secs": 45
}
```

### 8. Andon - Visual Signals/Alerts

**Payment System Alerts**:
- 🔴 Fraud rate > 2% → CRITICAL
- 🔴 Circuit breaker OPEN → CRITICAL
- 🟡 Latency > 5s → HIGH
- 🟢 Normal operation

**Deployment System Alerts**:
- 🔴 Deployment failed → CRITICAL
- 🔴 Automatic rollback triggered → CRITICAL
- 🟡 Health check marginal → HIGH
- 🟢 Successful deployment

### 9. Tracing - End-to-End Observability

**Payment Transaction Trace**:
```
trace_id: trace_abc123
  ├─ signal_created (timestamp, payment details)
  ├─ policy_evaluated (fraud_score, checks_performed)
  ├─ circuit_breaker_checked (state, failure_count)
  ├─ gateway_called (latency_ms, response_code)
  ├─ kanban_published (queue_depth)
  ├─ metrics_recorded (success, fraud_score, latency)
  └─ response_sent (final_status)
```

**Deployment Trace**:
```
deployment_id: dpl_xyz789
  ├─ signal_received (service, version, region)
  ├─ policy_validated (image_check, capacity_check)
  ├─ canary_stage (duration, error_rate, health)
  ├─ staging_stage (duration, error_rate, health)
  ├─ prod_stage (duration, error_rate, health)
  ├─ metrics_recorded (success_rate, duration)
  └─ deployment_complete (status)
```

## Running Integration Tests

```bash
# Run all tests
cargo test --test integration_tests -- --nocapture

# Run specific test
cargo test --test integration_tests payment_signal_happy_path -- --nocapture

# Run with logging
RUST_LOG=debug cargo test --test integration_tests -- --nocapture
```

### Test Coverage

The integration test suite includes:

**Payment System Tests** (12 tests):
- ✅ Happy path payment processing
- ✅ Fraud detection (blacklist, velocity, amount)
- ✅ Circuit breaker (closed, open, half-open)
- ✅ Andon alerts (fraud rate, circuit breaker)
- ✅ Kanban queue publishing
- ✅ Kaizen metrics tracking

**Deployment System Tests** (8 tests):
- ✅ Happy path deployment
- ✅ Policy validation (image, capacity, rollback)
- ✅ Heijunka stages (canary, staging, prod)
- ✅ Jidoka rollback (health, latency)
- ✅ Kanban sequential deployment
- ✅ Kaizen metrics tracking

**Chaos & Resilience Tests** (8 tests):
- ✅ Payment gateway failure (circuit breaker protection)
- ✅ Deployment health failure (automatic rollback)
- ✅ Sustained fraud attack (Andon alerting)
- ✅ Partial regional failure (graceful degradation)
- ✅ DDoS protection (velocity limits)

**Compliance Tests** (4 tests):
- ✅ Payment audit trail
- ✅ Deployment audit trail
- ✅ Trace correlation (payment)
- ✅ Trace correlation (deployment)

## Observability

### Prometheus Metrics

Access at http://localhost:9090

Exposed metrics:
- `payment_gateway_circuit_breaker_state` (open/closed/half-open)
- `payment_transaction_total` (counter)
- `payment_fraud_detected_total` (counter)
- `payment_processing_duration_seconds` (histogram)
- `deployment_success_total` (counter)
- `deployment_rollback_total` (counter)
- `deployment_duration_seconds` (histogram)

### Grafana Dashboards

Access at http://localhost:3000 (admin/admin)

Dashboards:
- **Payment System Dashboard**: Transaction rates, fraud detection, circuit breaker
- **Deployment System Dashboard**: Deployment success, rollback rates, stage duration
- **System Health**: CPU, memory, request latency

### Jaeger Tracing

Access at http://localhost:16686

View traces for:
- Individual payment transactions
- Complete deployment orchestration
- Service dependencies
- Performance analysis

## Customization

### Modify Payment Policy

Edit `payment-system/src/main.rs`:
```rust
// Change fraud detection thresholds
pub async fn evaluate_fraud_policy(&self, signal: &PaymentSignal) -> FraudPolicyResult {
    // Modify check thresholds here
    if signal.amount > 10000.0 {  // Change from 5000
        fraud_score += 30.0;
    }
}
```

### Modify Deployment Strategy

Edit `deployment-system/src/main.rs`:
```rust
// Change rollout strategy
pub async fn execute_rollout_stages(&self, ...) -> Result<Vec<StageResult>> {
    // Modify stages or durations
    let stages = vec![
        RolloutStage::Canary,    // 5% traffic
        RolloutStage::Staging,   // 25% traffic
        RolloutStage::Prod,      // 100% traffic
    ];
}
```

### Adjust SLO Thresholds

Edit configuration:
```rust
// Payment system
pub struct AndonThresholds {
    pub fraud_rate_percent: f64,      // Change from 2.0
    pub latency_threshold_ms: u64,    // Change from 5000
    pub error_rate_percent: f64,      // Change from 5.0
}

// Deployment system
pub struct DeploymentPolicy {
    pub max_error_rate_percent: f64,  // Change from 5.0
    pub max_latency_p99_ms: u64,      // Change from 1000
}
```

## Load Testing

```bash
# Generate sustained payment traffic
for i in {1..1000}; do
  curl -X POST http://localhost:3001/payment \
    -H "Content-Type: application/json" \
    -d "{\"amount\": $((RANDOM % 1000 + 1)).99, \"customer_id\": \"cust_$((RANDOM % 100))\", \"currency\": \"USD\", \"merchant_id\": \"merc_456\"}" \
    &
  sleep 0.1
done
wait

# Monitor metrics
curl http://localhost:3001/metrics | jq .
```

## Troubleshooting

### Services won't start

```bash
# Check logs
docker-compose logs -f

# Rebuild images
docker-compose up --build -d

# Check health
docker-compose ps
```

### Can't reach services

```bash
# Verify network
docker network ls | grep tai-reference

# Check container IPs
docker-compose ps

# Test connectivity
docker exec -it tai-nats nc -z redis 6379
```

### Metrics not appearing

```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# Check Jaeger
curl http://localhost:14268/api/traces

# View logs
docker-compose logs prometheus
docker-compose logs jaeger
```

## Production Deployment

For production deployment, consider:

1. **Scaling**:
   - Multiple payment service instances (load balanced)
   - Multiple deployment orchestrator instances
   - Dedicated NATS cluster
   - Redis cluster (instead of single instance)

2. **Reliability**:
   - Persistent state store (instead of in-memory)
   - Database backups for audit logs
   - High-availability Vault for secrets
   - Multi-region deployment

3. **Security**:
   - TLS for all service-to-service communication
   - API authentication (OAuth2)
   - Rate limiting per API key
   - DDoS protection

4. **Observability**:
   - ELK stack for centralized logging
   - Datadog/New Relic for APM
   - On-call alerting (PagerDuty)
   - SLO dashboards

## References

- **TAI Pattern**: `docs/tai-reference/README.md`
- **TPS Principles**: Toyota Production System documentation
- **Circuit Breaker Pattern**: Release It! by Michael T. Nygard
- **Canary Deployments**: Google Cloud documentation
- **OpenTelemetry**: https://opentelemetry.io

## License

MIT
