# TAI Autonomic Pricing Engine

Production-grade value calculation and billing system for outcome-based pricing models on Erlang/OTP.

**Core Formula**: `V = f(metrics)` → `P = α × V` with cryptographic proof

## Features

### Value Calculation
- **Deterministic**: Same inputs always produce same value (SHA-256 proof)
- **Configurable**: Weighted sum or multiplicative aggregation
- **Bounded**: Min/max price enforcement (no unbounded growth)
- **Auditable**: Complete merkle chain for every calculation

### Cryptographic Proof
- **SHA-256 Hashing**: Deterministic receipt generation
- **HMAC-SHA256**: Tamper-proof signatures
- **Merkle Chain**: Link receipts for sequence verification
- **Constant-Time Comparison**: Timing attack prevention

### Billing Integration
- **Stripe Ready**: Create customers, invoices, process payments
- **Webhook Support**: Handle payment status updates
- **Multi-Tenant**: 100% customer isolation
- **Error Resilient**: Result types throughout (no unwrap/panic)

### Observability
- **Prometheus Metrics**: Grafana-compatible dashboards
- **OpenTelemetry Tracing**: Distributed request tracing
- **Structured JSON Logs**: Cloud Logging integration
- **Real-Time Alerting**: Anomaly detection and escalation

### Security
- **Input Validation**: All metrics checked for bounds
- **Anomaly Detection**: Spike detection (>500% increases)
- **Customer Isolation**: No cross-customer data leakage
- **Signature Verification**: All receipts verified
- **Audit Trails**: Immutable logs for compliance

## Quick Start

### Prerequisites
```bash
# Erlang/OTP 26+ (check: erl --version)
# Rebar3 (check: rebar3 --version)
# Docker (optional, for containerization)
```

### Build
```bash
cd /Users/sac/ggen/tai-erlang-autonomics/pricing-engine
rebar3 compile
```

### Test
```bash
# Unit tests (20+ tests)
rebar3 eunit

# Integration tests (7 scenarios)
rebar3 ct

# Full test suite
rebar3 do eunit, ct

# Code quality checks
rebar3 dialyzer
rebar3 xref
```

### Run Locally
```bash
# Start Erlang shell with pricing engine
rebar3 shell

# In Erlang shell:
> pricing_engine:start_link(#{}).
{ok, <0.123.0>}

> pricing_engine:calculate_value(
    <<"customer_001">>,
    [{<<"throughput">>, 100.0}, {<<"latency">>, 50.0}],
    #{alpha_coefficient => 100.0, min_price => 0.0, max_price => 10000.0},
    #{aggregation_method => weighted_sum}
  ).
{ok, #value_record{
    customer_id = <<"customer_001">>,
    calculated_value = 75.3,
    billed_price = 7530.0,
    receipt_hash = <<"a1b2c3d4...">>,
    status = completed
}}

> pricing_engine:get_value_history(<<"customer_001">>, 10).
{ok, [#value_record{...}]}
```

## Architecture

### Core Components

```
┌─────────────────────────────────────┐
│   Dashboard API (value_dashboard_api.erl)
│   - GET /api/value/current
│   - GET /api/value/history
│   - GET /api/receipt/{id}
│   - POST /api/receipt/{id}/verify
│   - GET /api/audit/export
└──────────────┬──────────────────────┘
               │
    ┌──────────┼──────────┬──────────┐
    │          │          │          │
┌───▼───┐ ┌────▼────┐ ┌──▼────┐ ┌─▼────┐
│Pricing│ │ Receipt │ │Stripe │ │Security
│Engine │ │Generator│ │Billing│ │Module
│       │ │(Crypto) │ │       │ │
└───┬───┘ └────┬────┘ └──┬────┘ └─┬────┘
    │          │         │        │
    └──────────┼─────────┼────────┘
               │         │
        ┌──────▼─┬───────▼──────┐
        │  Value │  Monitoring  │
        │  Ledger│  & Alerting  │
        └────────┴──────────────┘
```

### Modules

| Module | Purpose | Status |
|--------|---------|--------|
| **pricing_engine.erl** | Core value calculation (gen_statem) | ✅ Complete |
| **receipt_generator.erl** | Cryptographic proofs (HMAC, Merkle) | ✅ Complete |
| **stripe_billing.erl** | Stripe integration (payments, invoices) | ✅ Complete |
| **value_dashboard_api.erl** | REST API endpoints (customer portal) | ✅ Complete |
| **pricing_security.erl** | Anti-fraud validation, input sanitization | ✅ Complete |
| **pricing_monitoring.erl** | Prometheus, OpenTelemetry, alerting | ✅ Complete |
| **pricing_engine_tests.erl** | 20+ unit tests (Chicago TDD) | ✅ Complete |
| **integration_tests.erl** | 7 end-to-end scenarios | ✅ Complete |

## File Organization

```
pricing-engine/
├── src/
│   ├── pricing_engine.erl           (Core engine, gen_statem)
│   ├── pricing_engine.hrl           (Record definitions)
│   ├── receipt_generator.erl        (Cryptographic proofs)
│   ├── stripe_billing.erl           (Stripe integration)
│   ├── value_dashboard_api.erl      (REST endpoints)
│   ├── pricing_security.erl         (Anti-fraud, validation)
│   └── pricing_monitoring.erl       (Observability)
├── test/
│   ├── pricing_engine_tests.erl     (Unit tests)
│   └── integration_tests.erl        (Integration scenarios)
├── docs/
│   ├── API_REFERENCE.md             (Complete API docs)
│   ├── IMPLEMENTATION_GUIDE.md      (Architecture, patterns)
│   └── AUDIT_TRAIL_FORMAT.md        (Compliance format)
├── config/
│   ├── pricing.config               (Runtime config)
│   └── sys.config                   (System config)
├── priv/
│   └── value_ontology.ttl           (RDF specification)
└── README.md                        (This file)
```

## API Examples

### Calculate Value
```bash
curl -X POST https://pricing-api.autonomic.services/api/value/calculate \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "customer_id": "cust_abc123",
    "metrics": {
      "throughput": 150.5,
      "latency": 45.2,
      "availability": 99.95
    },
    "aggregation_method": "weighted_sum",
    "pricing_config": {
      "alpha_coefficient": 100.0,
      "min_price": 10.0,
      "max_price": 5000.0
    }
  }'
```

### Get Current Value
```bash
curl https://pricing-api.autonomic.services/api/value/current?customer_id=cust_abc123 \
  -H "Authorization: Bearer YOUR_TOKEN"
```

### Verify Receipt
```bash
curl -X POST https://pricing-api.autonomic.services/api/receipt/receipt_xyz789/verify \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "customer_id": "cust_abc123",
    "hmac_key": "base64_encoded_key"
  }'
```

See [API_REFERENCE.md](docs/API_REFERENCE.md) for complete endpoint documentation.

## Value Calculation Formula

### Weighted Sum Aggregation (Default)

```
1. Normalize metrics using sigmoid:
   normalized_i = 1 / (1 + exp(-metric_i / scale))

2. Apply weights:
   weighted_i = w_i × normalized_i

3. Aggregate:
   V = Σ(weighted_i)

4. Calculate price:
   P = min(max(α × V + min_price, min_price), max_price)
```

### Example Calculation
```
Metrics:
  throughput = 150.0 (ops/sec)
  availability = 99.95 (%)
  latency = 45.2 (ms)

Weights: [0.5, 0.3, 0.2]
Alpha coefficient: 100.0
Min price: $10.0
Max price: $5000.0

Normalized:
  throughput = sigmoid(1.5) ≈ 0.881
  availability = sigmoid(0.9995) ≈ 0.732
  latency = sigmoid(0.452) ≈ 0.612

Aggregation:
  V = 0.5×0.881 + 0.3×0.732 + 0.2×0.612
  V ≈ 0.782

Price:
  P = min(max(100×0.782 + 10, 10), 5000)
  P = $88.20
```

## Test Coverage

### Unit Tests (20 tests)
- ✅ Basic value calculation
- ✅ Zero metrics
- ✅ Maximum metrics
- ✅ Invalid negative metrics (rejected)
- ✅ Invalid oversized metrics (rejected)
- ✅ Price calculation with bounds
- ✅ Receipt hash deterministic
- ✅ Merkle chain verification
- ✅ Value history pagination
- ✅ Customer statistics
- ✅ Concurrent access
- ✅ Customer isolation

### Integration Tests (7 scenarios)
- ✅ Complete customer lifecycle
- ✅ Multi-tenant isolation (10 customers)
- ✅ Billing cycle workflow (daily to monthly)
- ✅ Anomaly detection (spike handling)
- ✅ Concurrent multi-customer load (200+ concurrent ops)
- ✅ Receipt verification chain
- ✅ Error handling and recovery

**Coverage Target**: >85% line coverage, 100% function coverage

Run tests:
```bash
rebar3 do eunit, ct
rebar3 cover
```

## Security Features

### Input Validation
- [x] Metrics bounded [0, 1,000,000,000]
- [x] Customer IDs sanitized (alphanumeric + dash/underscore)
- [x] Price formula constraints (min ≤ actual ≤ max)
- [x] Injection attack prevention

### Cryptographic Security
- [x] SHA-256 hashing (deterministic receipts)
- [x] HMAC-SHA256 signatures (tamper proof)
- [x] Constant-time comparison (timing attack resistant)
- [x] Merkle chain linking (sequence verification)
- [x] Ed25519 audit signatures (non-repudiation)

### Fraud Detection
- [x] Spike detection (>500% increases flagged)
- [x] Anomaly detection (zero value inconsistency)
- [x] Customer isolation (no cross-customer data)
- [x] Signature mismatch detection
- [x] Replay attack prevention

### Audit & Compliance
- [x] Immutable audit trail (append-only log)
- [x] Digital signatures on all events
- [x] Retention policies enforced
- [x] Customer isolation verified
- [x] Data export for regulatory compliance

See [SECURITY.md](docs/SECURITY.md) for detailed security guide.

## Monitoring & Alerts

### Prometheus Metrics

```prometheus
# Latency histogram
pricing_calculation_latency_ms (buckets: 10, 50, 100, 500, 1000, 5000)
pricing_receipt_generation_ms (buckets: 1, 5, 10, 50, 100)

# Counter metrics
pricing_calculations_total
pricing_receipts_generated_total
pricing_invoices_created_total
pricing_payments_succeeded_total
pricing_payments_failed_total
pricing_refunds_issued_total
pricing_revenue_usd
pricing_anomalies_detected_total
pricing_errors_total

# Gauge metrics
pricing_current_value
```

### Alert Rules

```prometheus
# High calculation latency
alert: HighCalculationLatency
  expr: histogram_quantile(0.95, pricing_calculation_latency_ms) > 1000
  for: 5m
  action: PagerDuty

# Low payment success rate
alert: LowPaymentSuccessRate
  expr: pricing_payments_succeeded_total / (pricing_payments_succeeded_total + pricing_payments_failed_total) < 0.95
  for: 15m
  action: Slack notification

# Critical anomalies
alert: CriticalAnomalies
  expr: pricing_critical_anomalies > 5
  for: 1m
  action: PagerDuty + Jira incident
```

## Performance SLOs

| Operation | Target SLO | Status |
|-----------|-----------|--------|
| Value calculation | ≤100ms | ✅ Achievable |
| Receipt generation | ≤10ms | ✅ Achievable |
| Price calculation | ≤5ms | ✅ Achievable |
| Merkle verification | ≤50ms | ✅ Achievable |
| API response time (p95) | ≤500ms | ✅ Target |
| Payment processing | ≤2s | ✅ Target |

## Deployment

### Local Development
```bash
rebar3 shell
pricing_engine:start_link(#{}).
```

### Docker
```bash
docker build -t pricing-engine:latest .
docker run -p 8080:8080 -e STRIPE_API_KEY=$STRIPE_KEY pricing-engine:latest
```

### Production (GCP Cloud Run)
```bash
gcloud run deploy pricing-engine \
  --image gcr.io/project/pricing-engine:latest \
  --memory 512Mi \
  --cpu 1 \
  --set-env-vars STRIPE_API_KEY=$STRIPE_KEY
```

## Configuration

### Environment Variables
```bash
# Stripe integration
STRIPE_API_KEY=sk_live_...
STRIPE_WEBHOOK_SECRET=whsec_...

# Database
FIRESTORE_PROJECT=my-gcp-project
FIRESTORE_DATABASE=pricing-ledger

# Observability
OTEL_ENABLED=true
OTEL_EXPORTER_OTLP_ENDPOINT=https://otel-collector.example.com
PROMETHEUS_ENABLED=true

# Logging
LOG_LEVEL=info
JSON_LOGS=true
```

### Runtime Config (config/pricing.config)
```erlang
{pricing_engine, [
    {stripe_enabled, true},
    {firestore_enabled, true},
    {monitoring_enabled, true},
    {security_level, strict},
    {audit_enabled, true}
]}.
```

## Production Checklist

- [x] All tests passing (unit + integration)
- [x] Code quality checks (dialyzer, xref)
- [x] Security review complete
- [x] Monitoring configured (Prometheus, alerts)
- [x] Logging structured (JSON, centralized)
- [x] Error handling comprehensive (Result types)
- [x] Documentation complete (API, implementation)
- [x] Audit trail configured
- [x] Customer isolation verified
- [x] Rate limiting enforced
- [x] Backup/recovery tested
- [x] Load testing passed

## Troubleshooting

### High Latency
```bash
# Check Prometheus metrics
curl http://localhost:9090/api/v1/query?query=pricing_calculation_latency_ms

# Profile with fprof
rebar3 shell
> fprof:start().
> % ... run operations ...
> fprof:stop().
> fprof:profile().
```

### Payment Failures
```bash
# Check Stripe logs
stripe logs

# Verify webhook configuration
stripe listen --forward-to localhost:8080/webhooks/stripe

# Test payment
curl -X POST http://localhost:8080/test/charge -d '{"amount": 1000}'
```

### Merkle Chain Issues
```bash
# Verify chain integrity
> pricing_security:verify_merkle_chain(CustomerId, History, Ledger).
{ok, ok}

# Rebuild if corrupted
> pricing_engine:rebuild_customer_chain(CustomerId).
{ok, rebuilt}
```

## Support

- **Documentation**: [docs/](docs/)
- **API Reference**: [docs/API_REFERENCE.md](docs/API_REFERENCE.md)
- **Implementation Guide**: [docs/IMPLEMENTATION_GUIDE.md](docs/IMPLEMENTATION_GUIDE.md)
- **Issues**: GitHub Issues
- **Email**: pricing-support@autonomic.services

## License

Apache-2.0

---

## Compliance & Legal

This pricing engine is designed for:
- ✅ GDPR compliance (data privacy, retention policies)
- ✅ SOC 2 Type II requirements (audit trails, security controls)
- ✅ HIPAA compatibility (encryption, access logs)
- ✅ PCI DSS compliance (Stripe handling payments)
- ✅ FTC guidelines (clear pricing disclosure)

**Important**: Always consult with legal counsel regarding pricing practices in your jurisdiction.

---

## Contributing

We welcome contributions! Please:
1. Write tests for new features
2. Maintain test coverage >85%
3. Follow Erlang style guide
4. Use Result types (no panic)
5. Document all public APIs
6. Create PR with description

---

**Last Updated**: 2026-01-25
**Maintainer**: TAI Engineering Team
