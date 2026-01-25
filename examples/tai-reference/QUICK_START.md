# TAI Reference Implementation - Quick Start Guide

Get the complete TAI pattern reference system running in 5 minutes!

## Prerequisites

- Docker and Docker Compose (or Podman)
- Rust 1.91+ (optional, for building from source)
- curl or Postman (for testing)

## Start in 5 Minutes

### 1. Start All Services (2 minutes)

```bash
cd examples/tai-reference

# Start the full stack
docker-compose up -d

# Wait for services to be healthy
sleep 10

# Verify services are running
docker-compose ps
```

All services will start:
- NATS (message queue)
- Redis (cache & state)
- Prometheus (metrics)
- Grafana (dashboards)
- Jaeger (tracing)
- Vault (secrets)
- Payment System (port 3001)
- Deployment System (port 3002)

### 2. Test Payment System (2 minutes)

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
#   "processing_time_ms": 145,
#   "fraud_score": 0.25
# }

# View metrics
curl http://localhost:3001/metrics | jq .
```

### 3. Test Deployment System (1 minute)

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
#   "status": "SUCCESS",
#   "duration_secs": 45,
#   "stages_completed": ["validation", "canary", "staging", "prod"]
# }

# View metrics
curl http://localhost:3002/metrics | jq .
```

## Using the Makefile

The Makefile provides convenient shortcuts:

```bash
# View help
make help

# Build images
make build

# Start services
make up

# View logs
make logs
make logs-payment
make logs-deployment

# Run tests
make test
make test-payment
make test-deployment

# Load testing
make stress-payment
make stress-deployment

# View service status
make health
make status

# View dashboards
make view-grafana    # http://localhost:3000 (admin/admin)
make view-prometheus # http://localhost:9090
make view-traces     # http://localhost:16686

# Stop all services
make down
```

## Observability Dashboards

### Grafana (http://localhost:3000)
- Username: `admin`
- Password: `admin`

Dashboards available:
- Payment System Metrics
- Deployment System Metrics
- System Health

### Prometheus (http://localhost:9090)
Query metrics directly:
- `payment_transaction_total` - Total payment transactions
- `payment_fraud_detected_total` - Fraudulent transactions
- `deployment_success_total` - Successful deployments
- `deployment_duration_seconds` - Deployment duration

### Jaeger (http://localhost:16686)
View distributed traces:
- Search by service (payment-system, deployment-system)
- View individual request traces
- Analyze request path and latency

## Common Test Scenarios

### Scenario 1: Successful Payment

```bash
curl -X POST http://localhost:3001/payment \
  -H "Content-Type: application/json" \
  -d '{
    "amount": 50.00,
    "customer_id": "cust_good",
    "currency": "USD",
    "merchant_id": "merc_456"
  }'
```

Expected: `"status": "SUCCESS"`

### Scenario 2: Fraud Detection (Blacklisted Customer)

```bash
curl -X POST http://localhost:3001/payment \
  -H "Content-Type: application/json" \
  -d '{
    "amount": 50.00,
    "customer_id": "cust_blacklist",
    "currency": "USD",
    "merchant_id": "merc_456"
  }'
```

Expected: `"status": "FRAUD_BLOCKED"`, `"fraud_score": 1.0`

### Scenario 3: Fraud Detection (High Amount)

```bash
curl -X POST http://localhost:3001/payment \
  -H "Content-Type: application/json" \
  -d '{
    "amount": 10000.00,
    "customer_id": "cust_123",
    "currency": "USD",
    "merchant_id": "merc_456"
  }'
```

Expected: `"status": "SUCCESS"` but high `"fraud_score"` (0.3-0.4)

### Scenario 4: Velocity Limit (Rapid Transactions)

```bash
# Send 10 rapid payments from same customer
for i in {1..10}; do
  curl -X POST http://localhost:3001/payment \
    -H "Content-Type: application/json" \
    -d '{"amount": 10.00, "customer_id": "cust_rapid", "currency": "USD", "merchant_id": "merc_456"}' &
done
wait
```

Expected: Some will succeed, some will fail due to velocity limit

### Scenario 5: Successful Deployment

```bash
curl -X POST http://localhost:3002/deploy \
  -H "Content-Type: application/json" \
  -d '{
    "service": "api-service",
    "version": "v2.0.0",
    "target_region": "us-west-2",
    "rollout_strategy": "canary"
  }'
```

Expected: `"status": "SUCCESS"`, progresses through canary → staging → prod

### Scenario 6: Deployment with Health Check Failure

```bash
# In a real system, a simulated health failure would trigger rollback
# For now, the example shows gradual rollout stages

curl -X POST http://localhost:3002/deploy \
  -H "Content-Type: application/json" \
  -d '{
    "service": "bad-service",
    "version": "v1.0.0",
    "target_region": "us-east-1",
    "rollout_strategy": "canary"
  }'
```

Expected: May see `"status": "ROLLED_BACK"` if health checks fail

## Load Testing

### Light Load Test

```bash
make stress-payment    # 100 concurrent payments
make stress-deployment # 50 concurrent deployments
```

### Monitor During Load Test

```bash
# In another terminal, watch metrics
watch -n 1 'curl -s http://localhost:3001/metrics | jq .'
watch -n 1 'curl -s http://localhost:3002/metrics | jq .'
```

## Running Integration Tests

```bash
# All tests
make test

# Specific test category
make test-payment
make test-deployment

# With verbose output
cargo test --test integration_tests -- --nocapture

# Single test
cargo test --test integration_tests test_payment_signal_happy_path -- --nocapture
```

## Troubleshooting

### Services won't start

```bash
# Check logs
docker-compose logs

# Rebuild images
make rebuild

# Ensure ports are available
netstat -an | grep 3001  # Payment system
netstat -an | grep 3002  # Deployment system
```

### Can't connect to services

```bash
# Verify services are running
docker-compose ps

# Check network
docker network ls | grep tai

# Test connectivity
curl http://localhost:3001/metrics
curl http://localhost:3002/metrics
```

### Metrics not appearing

```bash
# Check Prometheus targets
curl http://localhost:9090/api/v1/targets

# View scraped metrics
curl http://localhost:9090/api/v1/query?query=up
```

### Traces not appearing in Jaeger

```bash
# Verify Jaeger is receiving traces
curl http://localhost:14268/api/traces

# Check service configuration for Jaeger endpoint
docker-compose logs jaeger | grep "listening"
```

## Next Steps

1. **Read the docs**: See `README.md` for detailed architecture
2. **Study the code**: Look at `payment-system/src/main.rs` and `deployment-system/src/main.rs`
3. **Run tests**: `make test` to see integration test scenarios
4. **Load test**: `make stress-payment` to see system under load
5. **Customize**: Modify policies, thresholds, and logic

## Architecture Summary

### Payment System
Signal → Policy (fraud detection) → Action (charge card) → Kanban (async confirmation) → Kaizen (metrics)

### Deployment System
Signal → Policy (safety checks) → Heijunka (gradual rollout) → Jidoka (auto-rollback) → Kaizen (metrics)

Both demonstrate:
- **Jidoka**: Circuit breakers, automatic rollback
- **Kanban**: Queue-based async processing, sequential operations
- **Heijunka**: Rate limiting, gradual rollout
- **Kaizen**: Metrics collection and tracking
- **Andon**: Threshold-based alerting
- **Tracing**: End-to-end observability

## API Reference

### Payment System

**Endpoint**: `POST /payment`

Request:
```json
{
  "amount": 99.99,
  "customer_id": "cust_123",
  "currency": "USD",
  "merchant_id": "merc_456"
}
```

Response:
```json
{
  "transaction_id": "txn_abc123...",
  "status": "SUCCESS|FRAUD_BLOCKED|FAILED",
  "amount": 99.99,
  "currency": "USD",
  "timestamp": "2026-01-25T10:30:00Z",
  "processing_time_ms": 145,
  "fraud_score": 0.25,
  "gateway_response": "txn_stripe_..."
}
```

**Metrics**: `GET /metrics`

### Deployment System

**Endpoint**: `POST /deploy`

Request:
```json
{
  "service": "api-service",
  "version": "v2.0.0",
  "target_region": "us-west-2",
  "rollout_strategy": "canary"
}
```

Response:
```json
{
  "deployment_id": "dpl_xyz789...",
  "service": "api-service",
  "version": "v2.0.0",
  "status": "SUCCESS|ROLLED_BACK|FAILED",
  "duration_secs": 45,
  "stages_completed": ["validation", "canary", "staging", "prod"]
}
```

**Metrics**: `GET /metrics`

## Getting Help

See full documentation:
- `README.md` - Architecture and detailed explanation
- `docs/tai-reference/COMPLETE_GUIDE.md` - 2000+ line comprehensive guide
- Integration tests in `tests/integration_tests.rs` - 20+ example scenarios

Good luck! 🚀
