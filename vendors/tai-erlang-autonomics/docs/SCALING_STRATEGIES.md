# Scaling Strategies

Comprehensive guide to scaling TAI Erlang Autonomics for increasing load and performance requirements.

---

## Scaling Dimensions

```
┌─────────────────────────────────────────────────┐
│     TAI Erlang Autonomics Scaling Matrix       │
├─────────────────────────────────────────────────┤
│ HORIZONTAL:                                     │
│   - Cloud Run instances (0-100+)                │
│   - Auto-scaling based on load                  │
│   - Region distribution                         │
│                                                 │
│ VERTICAL:                                       │
│   - CPU per instance (0.25 - 4 vCPU)           │
│   - Memory per instance (512MB - 8GB)          │
│   - Action pool size (1 - 100)                 │
│                                                 │
│ STORAGE:                                        │
│   - Receipt buffer size                         │
│   - Firestore capacity                          │
│   - BigQuery retention                          │
│                                                 │
│ DEPENDENCY:                                     │
│   - Pub/Sub subscription rate                   │
│   - Firestore write throughput                  │
│   - Network capacity                            │
└─────────────────────────────────────────────────┘
```

---

## Horizontal Scaling (Cloud Run)

### Auto-Scaling Configuration

**For Moderate Load** (10-100 req/s):
```bash
gcloud run deploy tai-autonomics \
  --min-instances=1 \
  --max-instances=10 \
  --memory=2Gi \
  --cpu=2 \
  --concurrency=100
```

**For High Load** (100-1000 req/s):
```bash
gcloud run deploy tai-autonomics \
  --min-instances=2 \
  --max-instances=50 \
  --memory=2Gi \
  --cpu=2 \
  --concurrency=500
```

**For Extreme Load** (1000+ req/s):
```bash
gcloud run deploy tai-autonomics \
  --min-instances=5 \
  --max-instances=200 \
  --memory=4Gi \
  --cpu=4 \
  --concurrency=1000
```

### Scaling Triggers

```
Request Volume:
  < 10 req/s      → min=0, max=5
  10-100 req/s    → min=1, max=10
  100-1000 req/s  → min=2, max=50
  > 1000 req/s    → min=5, max=200

CPU Usage:
  > 70%           → scale up by 50%
  < 30%           → scale down gradually

Memory Usage:
  > 80%           → increase instance size
  < 50%           → decrease if possible

Latency:
  P99 > 1s        → increase instances or CPU
  P99 < 100ms     → consider scaling down
```

### Predictive Scaling

For known traffic patterns:

```bash
# Monday-Friday peaks: 8am-6pm
gcloud scheduler jobs create app-engine scale-peak \
  --schedule="0 8 * * 1-5" \
  --http-method=POST \
  --uri="https://cloud.googleapis.com/..." \
  --message='{"min": 5, "max": 100}'

# Nights/weekends low traffic
gcloud scheduler jobs create app-engine scale-low \
  --schedule="0 20 * * 1-5" \
  --http-method=POST \
  --uri="https://cloud.googleapis.com/..." \
  --message='{"min": 1, "max": 10}'
```

---

## Vertical Scaling (Instance Sizing)

### Performance Tiers

**Tier 1: Small** (Development/Testing)
```
CPU:    0.25 vCPU
Memory: 512 MB
Cost:   ~$2/month
Max:    10 req/s
Type:   t2.micro equivalent
```

**Tier 2: Medium** (Production - Standard)
```
CPU:    2 vCPU
Memory: 2 GB
Cost:   ~$20/month per instance
Max:    100 req/s
Type:   Standard instance
```

**Tier 3: Large** (Production - High Performance)
```
CPU:    4 vCPU
Memory: 4 GB
Cost:   ~$50/month per instance
Max:    500 req/s
Type:   High-performance instance
```

**Tier 4: Extra Large** (Enterprise)
```
CPU:    4 vCPU
Memory: 8 GB
Cost:   ~$100/month per instance
Max:    1000+ req/s
Type:   Maximum throughput
```

### Scaling Decision Matrix

| Metric | Action | Target |
|--------|--------|--------|
| P99 Latency > 500ms | Increase CPU or scale out | < 500ms |
| Memory > 80% | Increase instance memory or reduce buffer | < 70% |
| CPU > 70% sustained | Scale horizontally first, then vertically | < 50% |
| Error rate > 0.1% | Investigate, may need vertical or scaling | < 0.01% |
| Throughput < 10 req/s/instance | Scale vertically or consolidate | > 50 req/s/instance |

### Cost vs Performance

```
Configuration: 1x Medium (2CPU, 2GB)
- Cost: $20/month
- Max: 100 req/s
- Cost per req/s: $0.20/month

Configuration: 10x Small (0.25CPU, 512MB)
- Cost: $20/month
- Max: 100 req/s (10x10)
- Cost per req/s: $0.20/month
- Benefit: Better resilience, auto-scaling

Configuration: 2x Large (4CPU, 4GB)
- Cost: $100/month
- Max: 1000 req/s
- Cost per req/s: $0.10/month
- Benefit: Lower per-request cost
```

---

## Internal Scaling (Action Executor)

### Action Pool Configuration

**For Low Concurrency** (0-10 actions/s):
```bash
export ACTION_POOL_SIZE=5
export ACTION_QUEUE_SIZE=100
export ACTION_TIMEOUT_MS=60000
```

**For Medium Concurrency** (10-100 actions/s):
```bash
export ACTION_POOL_SIZE=20
export ACTION_QUEUE_SIZE=1000
export ACTION_TIMEOUT_MS=60000
```

**For High Concurrency** (100+ actions/s):
```bash
export ACTION_POOL_SIZE=50
export ACTION_QUEUE_SIZE=5000
export ACTION_TIMEOUT_MS=30000
```

### Tuning Action Processing

```erlang
%% Worker pool configuration
{action_executor, #{
  pool_size => 50,              % Number of workers
  queue_size => 5000,            % Max queued actions
  worker_timeout_ms => 60000,    % Worker timeout
  max_retries => 3,              % Retry on failure
  retry_backoff_ms => 1000,      % Wait between retries
  graceful_shutdown_ms => 30000  % Shutdown timeout
}}
```

### Queue Monitoring

```bash
# Monitor action queue depth
gcloud monitoring timeseries list \
  --filter='metric.type="custom.googleapis.com/tai_action_queue_depth"'

# Alert on queue overflow
queue_depth > 80% of max
  → Increase ACTION_POOL_SIZE or ACTION_QUEUE_SIZE
  → Scale horizontally

queue_depth < 20% of max
  → Consider reducing pool size to save memory
```

---

## Receipt Ledger Scaling

### Buffer Configuration for Scale

**For Low Volume** (< 100 receipts/s):
```bash
export RECEIPT_BUFFER_SIZE=1000
export RECEIPT_FLUSH_INTERVAL_MS=1000
```

**For Medium Volume** (100-1000 receipts/s):
```bash
export RECEIPT_BUFFER_SIZE=10000
export RECEIPT_FLUSH_INTERVAL_MS=500
```

**For High Volume** (1000+ receipts/s):
```bash
export RECEIPT_BUFFER_SIZE=50000
export RECEIPT_FLUSH_INTERVAL_MS=100
```

### Backend Selection

**ETS (In-Memory)**:
- Max: 10,000 receipts/s per instance
- Latency: < 1ms
- Retention: 24 hours
- Use: Development, testing, low-retention needs

**Firestore**:
- Max: 20,000 writes/s per database
- Latency: 10-50ms
- Retention: 7 days (configurable)
- Use: Production, multi-region, durable

**BigQuery**:
- Max: Unlimited (streaming inserts)
- Latency: 1-2 seconds (streaming)
- Retention: 1+ years
- Use: Analytics, compliance, historical data

### Switching Backends at Scale

```erlang
%% Dynamic backend selection based on load
get_ledger_backend() ->
  ReceiptRate = get_receipt_rate(),
  case ReceiptRate of
    R when R < 1000  → ets;        % ETS fast for low volume
    R when R < 20000 → firestore;  % Firestore for medium
    _                → bigquery    % BigQuery for high volume
  end.
```

---

## Dependency Scaling

### Pub/Sub Subscription

**For Moderate Load**:
```bash
gcloud pubsub subscriptions update erlang-autonomics-signals-sub \
  --push-endpoint=https://SERVICE_URL/pubsub \
  --push-auth-service-account=tai-autonomics-sa@PROJECT_ID.iam.gserviceaccount.com \
  --message-retention-duration=7d \
  --dead-letter-topic=erlang-autonomics-dlq
```

**For High Load**:
```bash
# Increase subscription concurrency
# (managed automatically by Pub/Sub)

# Monitor subscription metrics
gcloud monitoring timeseries list \
  --filter='metric.type="pubsub.googleapis.com/subscription/ack_latency"'

# If lagging: increase push endpoint concurrency
# By scaling Cloud Run max-instances
```

### Firestore Scaling

**Capacity Planning**:
```
Estimated load:
- 100 writes/s
- 1000 reads/s
- 30-day retention

Firestore capacity:
- Writes: ~20,000/s per database
- Reads: Unlimited
- Storage: 1 GB included, $0.18/GB extra

Monthly costs:
- Writes: 100 * 60 * 60 * 24 * 30 = 259.2M writes ≈ $1
- Reads: Minimal
- Storage: 30 days * 100/s * 1KB = 259 GB ≈ $46
- Total: ~$50/month
```

**Scaling Actions**:

```bash
# 1. Create regional database for lower latency
gcloud firestore databases create \
  --region=us-central1 \
  --type=firestore-native

# 2. Monitor throughput
gcloud monitoring timeseries list \
  --filter='metric.type="firestore.googleapis.com/operation_latency"'

# 3. If hitting limits: request quota increase
gcloud compute project-info describe --project=PROJECT_ID \
  --format='value(quotas[name="FIRESTORE_API_OPERATIONS_PER_SECOND_QUOTA"])'

# 4. Consider sharding for write-heavy
# Split receipts across multiple collections by tenant hash
```

---

## Multi-Region Scaling

### Active-Active Deployment

For global low-latency:

```bash
# Deploy to multiple regions
for region in us-central1 europe-west1 asia-southeast1; do
  gcloud run deploy tai-autonomics \
    --region=$region \
    --image=IMAGE_URI \
    --min-instances=2 \
    --max-instances=50
done

# Setup Cloud Load Balancer for geo-routing
gcloud compute backend-services create tai-autonomics \
  --global \
  --load-balancing-scheme=EXTERNAL \
  --protocol=HTTPS

# Add regional backends
for region in us-central1 europe-west1 asia-southeast1; do
  gcloud compute backend-services add-backends tai-autonomics \
    --global \
    --instance-group=tai-autonomics-$region-ig \
    --instance-group-region=$region \
    --balancing-mode=RATE \
    --max-rate-per-instance=1000
done
```

### Multi-Region Firestore

```bash
# Create multi-region database
gcloud firestore databases create \
  --location=nam5 \
  --type=firestore-native

# Regional distribution automatic
# Data replicated to: us-central1, us-east1, us-west1
```

---

## Load Testing

### Load Testing Setup

```bash
# Using Apache Bench
ab -n 10000 -c 100 https://SERVICE_URL/health

# Using wrk
wrk -t12 -c400 -d30s https://SERVICE_URL/pubsub

# Using locust (for complex scenarios)
pip install locust
locust -f locustfile.py --host=https://SERVICE_URL
```

### Expected Performance Baseline

```
Single instance (2CPU, 2GB):
- Requests/second: 100-500
- P95 latency: 50-100ms
- P99 latency: 100-500ms
- Error rate: < 0.01%

10 instances (20CPU, 20GB total):
- Requests/second: 1000-5000
- P95 latency: 50-100ms
- P99 latency: 100-500ms
- Error rate: < 0.01%
```

### Load Scaling Test Procedure

```bash
# 1. Baseline (10 req/s for 5 min)
./load_test.sh --rate=10 --duration=300

# 2. Ramp up (50 req/s for 5 min)
./load_test.sh --rate=50 --duration=300

# 3. Peak (100 req/s for 10 min)
./load_test.sh --rate=100 --duration=600

# 4. Sustained (100 req/s for 30 min)
./load_test.sh --rate=100 --duration=1800

# 5. Cool down (10 req/s for 5 min)
./load_test.sh --rate=10 --duration=300
```

---

## Cost Optimization

### Savings Opportunities

**Committed Use Discounts**:
```bash
# 25% off Cloud Run with 1-year commitment
# 30% off Cloud Run with 3-year commitment

# Calculate savings:
# Standard: 2x instances * $20/month * 12 months = $480
# 1-year CUD: $480 * 0.75 = $360 (saves $120)
```

**Reserved Capacity**:
```bash
# Reserve Firestore capacity for predictable load
# Reduces per-operation cost

# Reserve Pub/Sub capacity
# Reduces per-MB cost
```

**Consolidation**:
```bash
# Combine multiple small instances
# 5 small instances → 1 large instance
# Same cost, better efficiency

# Use smaller instances during off-peak
# Automatic with Cloud Run auto-scaling
```

### Cost Calculator

```
Monthly Cost Estimation:

Cloud Run:
  - 2 instances avg, 2 CPU, 2 GB
  - $0.00002400 per vCPU-second
  - $0.00000050 per GB-second
  - Instance: 2 * 0.00002400 * 2,592,000 = $124.42
  - Memory:   2 * 0.00000050 * 2,592,000 = $2.59
  - Total: ~$25/month per instance

Firestore:
  - 100 writes/s
  - Storage ~250GB
  - Writes: (100 * 86,400 * 30) / 1M * $0.06 = $15.55
  - Storage: 250 * $0.18 = $45
  - Total: ~$60/month

Pub/Sub:
  - 100 messages/s, 1KB each
  - (100 * 86,400 * 30) MB / 1GB * $0.04 = $103.68
  - Total: ~$104/month

TOTAL: ~$190/month (1 instance)
```

---

## Scaling Decision Tree

```
START: Receive alert or metric
│
├─ Is error rate high (> 1%)?
│  ├─ YES → Investigate errors
│  │        1. Check error logs
│  │        2. Fix or rollback
│  │
│  └─ NO → Continue
│
├─ Is latency high (P99 > 1s)?
│  ├─ YES → Scaling needed
│  │        1. Check bottleneck
│  │        2. Scale appropriately
│  │
│  └─ NO → Continue
│
├─ Is throughput low (< 10 req/s/instance)?
│  ├─ YES → Increase instance size
│  │        1. Increase CPU/memory
│  │        2. Monitor improvements
│  │
│  └─ NO → Continue
│
├─ Is cost high?
│  ├─ YES → Optimize
│  │        1. Use commitments
│  │        2. Consolidate instances
│  │        3. Reduce inefficiency
│  │
│  └─ NO → Continue
│
└─ No action needed
```

---

## Monitoring Scaling

### Key Metrics to Track

```promql
# Requests per second by region
sum(rate(tai_http_requests_total[1m])) by (region)

# Instance count
cloud_run_instance_count

# Average latency trending
avg(rate(tai_http_request_duration_seconds_sum[5m])) /
avg(rate(tai_http_requests_total[5m]))

# Resource utilization
cloud_run_memory_utilization
cloud_run_cpu_utilization

# Cost per request
monthly_cost / total_requests
```

### Scaling Dashboard

Create dashboard with:
- Request rate trend
- Latency percentiles (P50, P95, P99)
- Instance count
- Memory/CPU usage
- Error rate
- Cost tracking

---

## References

- Configuration: CONFIG.md
- Monitoring: MONITORING.md
- Runbook: RUNBOOK.md
- Cloud Run Documentation: https://cloud.google.com/run/docs/quickstarts/build-and-deploy
