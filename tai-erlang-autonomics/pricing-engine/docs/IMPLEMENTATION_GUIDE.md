# Pricing Engine Implementation Guide

## Architecture Overview

The TAI Autonomic Pricing Engine is built on Erlang/OTP with the following architecture:

```
┌─────────────────────────────────────────────────┐
│         Customer Dashboard / Portal             │
│            (REST API Layer)                      │
└──────────────────┬──────────────────────────────┘
                   │
    ┌──────────────┼──────────────┐
    │              │              │
┌────▼──────┐ ┌────▼──────┐ ┌────▼──────┐
│  Pricing   │ │  Receipt   │ │  Stripe   │
│  Engine    │ │ Generator  │ │  Billing  │
│(gen_statem)│ │(Crypto)    │ │(Integration)
└────┬──────┘ └────┬──────┘ └────┬──────┘
     │             │             │
     └─────────────┼─────────────┘
                   │
        ┌──────────▼──────────┐
        │   Value Ledger      │
        │   (Immutable Store)  │
        └─────────────────────┘
```

---

## Core Components

### 1. Pricing Engine (pricing_engine.erl)

**Purpose**: Deterministic value calculation with cryptographic proofs

**Key Functions**:
- `calculate_value/4` - Calculate V from metrics
- `aggregate_metrics/2` - Combine metrics using weighted sum or multiplicative
- `calculate_price/3` - Apply formula P = α × V
- `hash_value_record/7` - Generate SHA-256 hash for merkle chain

**Design Pattern**: gen_statem (state machine)

**State Flow**:
```
idle → calculating → idle (success)
        ↓
        idle (error)
```

**Concurrency Model**:
- Handles concurrent requests via gen_statem
- Customer isolation enforced (each customer's data separate)
- No mutable shared state (all via message passing)

---

### 2. Receipt Generator (receipt_generator.erl)

**Purpose**: Cryptographic proof of value realization

**Key Features**:
- **HMAC-SHA256 Signatures**: Authenticity verification
- **Merkle Chain**: Link receipts for tamper detection
- **Constant-Time Comparison**: Prevent timing attacks
- **UUID Generation**: Unique receipt identifiers

**Security Properties**:
- Signature verification protects against forgery
- Merkle chain proves sequence integrity
- Hash chain is immutable (previous hash included)

**Receipt Structure**:
```erlang
-record(receipt, {
    receipt_id,           % UUID
    customer_id,          % Customer
    calculated_value,     % V
    billed_price,         % P
    value_hash,           % SHA-256(record)
    signature,            % HMAC-SHA256
    verified,             % Boolean
    payment_status        % pending | paid | failed
}).
```

---

### 3. Stripe Billing Integration (stripe_billing.erl)

**Purpose**: Connect to Stripe for payment processing

**Key Functions**:
- `create_customer/3` - Register customer in Stripe
- `add_payment_method/3` - Store credit card
- `create_invoice/3` - Issue invoice for billing cycle
- `charge_customer/3` - Process payment
- `handle_payment_webhook/2` - React to payment events
- `refund_charge/3` - Issue refund

**Webhook Events**:
- `payment_intent.succeeded` - Payment completed
- `payment_intent.payment_failed` - Payment failed
- `invoice.payment_succeeded` - Invoice paid
- `customer.deleted` - Customer account deleted

**Error Handling**:
- All functions return `{ok, Result}` or `{error, Reason}`
- No panic/unwrap (pure Erlang error tuples)
- Retry logic for transient failures

---

### 4. Value Dashboard API (value_dashboard_api.erl)

**Purpose**: Customer-facing REST endpoints

**Endpoints**:
1. `GET /api/value/current` - Current value and price
2. `GET /api/value/history` - Historical data with pagination
3. `GET /api/receipt/{id}` - Receipt details and verification
4. `GET /api/billing/cycles` - Billing cycle summaries
5. `POST /api/receipt/{id}/verify` - Verify receipt authenticity
6. `GET /api/audit/export` - Compliance audit trail
7. `GET /api/analytics` - Trends and projections

**Authentication**: Bearer token validation on all endpoints

**Rate Limiting**: 1000 requests/minute per customer

---

### 5. Pricing Security (pricing_security.erl)

**Purpose**: Anti-fraud and validation layer

**Security Controls**:
- **Input Validation**: Metrics must pass bounds checks
- **Anomaly Detection**: Spike detection (>500% increases)
- **Customer Isolation**: No cross-customer data leakage
- **Signature Verification**: HMAC-SHA256 validation
- **Injection Prevention**: Sanitize customer IDs and metric names
- **Replay Prevention**: Check for duplicate receipts

**Anomaly Detection Rules**:
1. Value spike (current > 5x previous)
2. Zero value with positive metrics
3. Metrics inconsistent (don't sum to value)
4. Signature mismatch

---

### 6. Pricing Monitoring (pricing_monitoring.erl)

**Purpose**: Observability and alerting

**Metrics Collected**:
- Calculation latency (histogram)
- Receipt generation latency
- Total calculations, receipts, invoices
- Payment success/failure rates
- Revenue collected
- Anomalies detected
- Critical errors

**Integration Points**:
- **Prometheus**: `/metrics` endpoint for Grafana dashboards
- **OpenTelemetry**: Distributed tracing spans
- **Structured Logging**: JSON logs to Cloud Logging
- **Alerting**: Critical anomalies → PagerDuty/Slack

---

## Value Calculation Formula

### Basic Weighted Sum Aggregation

```
Normalized_i = sigmoid(metric_i / scale)
V = Σ(w_i × Normalized_i)  where Σ(w_i) = 1
P = min(max(α × V + min_price, min_price), max_price)
```

### Example
```
Input Metrics:
  throughput = 150.0 (ops/sec)
  availability = 99.95 (%)
  latency = 45.2 (ms)

Normalized:
  throughput_norm = sigmoid(150.0 / 100) ≈ 0.881
  availability_norm = sigmoid(99.95 / 100) ≈ 0.732
  latency_norm = sigmoid(45.2 / 100) ≈ 0.612

Weights: [0.5, 0.3, 0.2]

V = 0.5 × 0.881 + 0.3 × 0.732 + 0.2 × 0.612
V = 0.4405 + 0.2196 + 0.1224
V ≈ 0.782

Pricing:
α = 100.0 (coefficient)
min_price = 10.0
max_price = 5000.0

P = min(max(100 × 0.782 + 10, 10), 5000)
P = min(max(88.2, 10), 5000)
P = 88.2 USD
```

---

## Merkle Chain Implementation

### Hash Chain Structure

```
Record 1 (first)
├── previous_hash = undefined
├── value_hash = SHA-256(customer_id | value | price | metrics | "")
└── next_record → Record 2

Record 2
├── previous_hash = SHA-256(Record 1)
├── value_hash = SHA-256(customer_id | value | price | metrics | previous_hash)
└── next_record → Record 3

Record 3
├── previous_hash = SHA-256(Record 2)
├── value_hash = SHA-256(customer_id | value | price | metrics | previous_hash)
└── (end of chain)
```

### Verification Algorithm

```erlang
verify_merkle_chain(Record, History, Ledger) ->
    case Record#value_record.previous_hash of
        undefined -> ok;  %% First record
        PrevHash ->
            case find_record_by_hash(PrevHash, History) of
                {ok, _} -> ok;  %% Chain intact
                error -> {error, chain_broken}
            end
    end.
```

---

## Billing Cycle Workflow

### Monthly Cycle Example

```
Day 1 (2026-01-01 00:00:00)
├─ Receive metrics for customer
├─ Calculate value V₁
├─ Calculate price P₁
└─ Store receipt R₁

Day 2 (2026-01-02 00:00:00)
├─ Receive metrics for customer
├─ Calculate value V₂ (linked to R₁)
├─ Calculate price P₂
└─ Store receipt R₂

...

Day 31 (2026-01-31 23:59:59)
├─ Final calculation of month
└─ Store receipt R₃₁

Start of Month End (2026-02-01 00:00:00)
├─ Aggregate cycle: total_value = V₁ + V₂ + ... + V₃₁
├─ Create invoice for total_price
├─ Send to Stripe
└─ Begin collection

Billing Complete
├─ Payment received
├─ Mark all receipts as paid
├─ Store payment_date and stripe_charge_id
└─ Begin next month
```

---

## Error Handling Strategy

### Result Type Pattern

All functions return `{ok, Value}` or `{error, Reason}`:

```erlang
%% Good
{ok, ValueRecord}

%% Error cases
{error, validation_failed}
{error, {aggregation_failed, unsupported_method}}
{error, {hash_generation_failed, invalid_hmac_key}}
{error, customer_isolation_violation}
```

### NO unwrap/expect/panic pattern

```erlang
%% WRONG - NEVER DO THIS
Value = maps:get(key, Map),  % Will crash if key missing

%% RIGHT - Always handle errors
case maps:find(key, Map) of
    {ok, Value} -> handle_value(Value);
    error -> {error, missing_key}
end.
```

---

## Testing Strategy

### Unit Tests (pricing_engine_tests.erl)

**Coverage Areas**:
1. **Value Calculation** (6 tests)
   - Basic calculation
   - Zero metrics
   - Max metrics
   - Invalid negative metrics
   - Invalid oversized metrics
   - Invalid format

2. **Price Formula** (3 tests)
   - Basic P = α × V
   - With minimum price floor
   - With maximum price ceiling

3. **Hashing** (2 tests)
   - Deterministic (same inputs = same hash)
   - Different values produce different hashes

4. **History/Retrieval** (3 tests)
   - Empty history
   - Multiple records
   - Respects limit parameter

5. **Statistics** (2 tests)
   - Total value accumulation
   - Receipt count

6. **Concurrency** (2 tests)
   - Concurrent calculations
   - Customer isolation

### Integration Tests (integration_tests.erl)

**Scenarios**:
1. Complete customer lifecycle (register → calculate → verify → bill)
2. Multi-tenant isolation (10 customers, separate data)
3. Billing cycle workflow (7 daily metrics → monthly invoice)
4. Anomaly detection (normal → spike → normal)
5. Concurrent multi-customer load (20 customers, 10 ops each)
6. Receipt chain verification (sequence integrity)
7. Error handling and recovery

---

## Production Deployment

### Prerequisites
```bash
# Erlang/OTP 26+
erl --version

# Rebar3
rebar3 --version

# Dependencies installed
rebar3 get-deps
```

### Build and Test
```bash
# Compile
rebar3 compile

# Unit tests
rebar3 eunit

# Integration tests
rebar3 ct

# Full suite
rebar3 do eunit, ct

# Code quality
rebar3 dialyzer
rebar3 xref
```

### Configuration
```erlang
{pricing_engine, [
    {environment, production},
    {db_backend, firestore},
    {stripe_api_key, "${STRIPE_API_KEY}"},
    {otel_enabled, true},
    {metrics_enabled, true},
    {log_level, info}
]}.
```

### Monitoring Setup

**Prometheus Metrics**:
```bash
# Scrape configuration (prometheus.yml)
- job_name: 'pricing-engine'
  static_configs:
    - targets: ['localhost:8080']
  metrics_path: '/metrics'
```

**Grafana Dashboard**:
- Calculation latency (histogram)
- Receipt generation latency
- Payment success rate
- Revenue collected
- Critical anomalies

**Alerting Rules**:
```prometheus
alert: HighCalculationLatency
  expr: pricing_calculation_latency_ms > 1000
  for: 5m

alert: LowPaymentSuccessRate
  expr: pricing_payments_succeeded_total / (pricing_payments_succeeded_total + pricing_payments_failed_total) < 0.95
  for: 15m

alert: CriticalAnomalies
  expr: pricing_critical_anomalies > 5
  for: 1m
```

---

## Security Checklist

- [x] All metrics validated (0 to 1,000,000,000)
- [x] Customer IDs sanitized (alphanumeric + dash/underscore)
- [x] HMAC-SHA256 signatures verified
- [x] Merkle chain integrity checked
- [x] No customer data leakage
- [x] Spike detection alerts on >500% increases
- [x] All calculations are deterministic
- [x] Result types used throughout (no panic)
- [x] Audit trail immutable and signed
- [x] Rate limiting enforced
- [x] Input validation on all endpoints
- [x] Error messages safe for clients (no leaking internals)

---

## Troubleshooting

### Issue: Calculation Latency High

**Symptoms**: `pricing_calculation_latency_ms` histogram shows values > 1s

**Causes**:
1. Heavy concurrent load
2. Metrics validation taking time
3. Hash generation bottleneck

**Solutions**:
1. Check system resources
2. Profile with `fprof`
3. Consider connection pooling for Stripe

### Issue: Merkle Chain Broken

**Symptoms**: Receipt verification fails with "chain broken" error

**Causes**:
1. Data corruption in ledger
2. Incomplete record write
3. Concurrent writes to same customer

**Solutions**:
1. Restore from backup
2. Rebuild chain from transaction log
3. Use distributed ledger (Firestore) for consistency

### Issue: Payment Success Rate Dropping

**Symptoms**: `pricing_payments_failed_total` increasing

**Causes**:
1. Stripe API rate limits
2. Network connectivity issues
3. Invalid payment methods

**Solutions**:
1. Check Stripe status page
2. Implement exponential backoff
3. Retry with jitter

---

## Performance Optimization

### Calculation SLO

Target: First calculation ≤ 100ms, incremental ≤ 50ms

**Optimization Techniques**:
1. Memoize normalized metric values
2. Cache aggregation weights
3. Batch HMAC operations
4. Pre-allocate binary buffers

### Hashing SLO

Target: Receipt generation ≤ 10ms

**Optimization Techniques**:
1. Use crypto:hash instead of erlang crypto
2. Avoid list concatenation (use iolist)
3. Pre-compute canonical form

### Memory Usage

Target: <500MB per 10k customers

**Optimization Techniques**:
1. Use ETS for customer stats (not maps)
2. Implement ledger rotation
3. Archive old receipts to Firestore

---

## References

- **Erlang/OTP Documentation**: https://erlang.org/doc/
- **Crypto Module**: https://erlang.org/doc/man/crypto.html
- **Gen_Statem**: https://erlang.org/doc/man/gen_statem.html
- **Rebar3**: https://rebar3.org/docs/
