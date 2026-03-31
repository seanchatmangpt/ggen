# Week 10-13 Operations Scaling & Infrastructure Readiness
**Status:** Active Implementation Plan
**Target:** Scale operations to support 3 customers in production
**Timeframe:** Weeks 10-13 (28 days)
**Success Metric:** Zero downtime, 99.5% availability, <500ms p95 latency

---

## Executive Summary

As TAI Autonomics moves from proof-of-concept (1 customer) to scaling (3 customers), infrastructure must support 10x growth trajectory while maintaining reliability. This document outlines the scaling strategy across 6 operational domains:

1. **Infrastructure Capacity** - Assess current limits, identify bottlenecks, plan scaling
2. **Auto-Scaling Configuration** - Cloud Run, Firestore, Load Balancer auto-scaling
3. **Backup & Disaster Recovery** - Daily backups, tested restores, RTO/RPO targets
4. **Monitoring & Observability** - Expanded dashboards, alerts, SLO tracking
5. **Deployment Procedures** - Reusable runbooks for customer onboarding
6. **Performance SLOs** - Targets and tracking for production excellence

---

## Part 1: Infrastructure Capacity Assessment

### Current State (Week 9, 1 Customer)

**Cloud Run Configuration**
```
CPU:                    1 vCPU per container
Memory:                 512 MB
Concurrent requests:    80
Max instances:          10
Estimated peak QPS:     50 (Customer #1 pilot)
```

**Firestore Configuration**
```
Read capacity:          10,000 reads/day (Customer #1)
Write capacity:         5,000 writes/day (Customer #1)
Indexes:                12 (basic)
Storage:                2 GB
Document count:         ~50K
```

**Load Balancer**
```
Capacity:               100 Gbps (global)
SSL/TLS termination:    Yes
Geographic distribution: 4 regions
Health checks:          Every 10s
```

### Bottleneck Analysis at 3 Customers

**Scenario: Customer #2 + #3 Go-Live Simultaneously**

| Component | Current | With 3 Cust | Bottleneck? | Risk Level |
|-----------|---------|------------|-----------|-----------|
| Cloud Run QPS | 50 | 150+ | Yes | HIGH |
| Cloud Run memory | 512 MB | 1-2 GB | Possible | MEDIUM |
| Firestore reads | 10K/day | 50K/day | Yes | HIGH |
| Firestore writes | 5K/day | 25K/day | Yes | HIGH |
| Database indexing | 12 | 40+ | Yes | MEDIUM |
| TLS cert handling | 1 cert | 3 SNI domains | No | LOW |
| Network egress | <5 GB/month | 20+ GB/month | Possible | MEDIUM |

**Critical Bottlenecks Identified:**
1. **Firestore reads/writes** - 5x growth with 3 customers
2. **Cloud Run CPU/Memory** - Concurrent request handling
3. **Database indexes** - Complex query patterns from multi-tenant
4. **API latency** - p95 could exceed 500ms without optimization

### Scaling Targets (Week 13 & Beyond)

**Phase 1: 3 Customers (Week 13)**
```
Cloud Run:
  - CPU per container:          2 vCPU (from 1)
  - Memory per container:       1 GB (from 512 MB)
  - Max instances:              25 (from 10)
  - Target QPS:                 150

Firestore:
  - Daily reads:                50K (from 10K)
  - Daily writes:               25K (from 5K)
  - Regional capacity:          Enable US-central, US-east
  - Indexes:                    40+
```

**Phase 2: 10 Customers (Month 5)**
```
Cloud Run:
  - Distributed across 3 regions
  - Auto-scaling: 5-100 instances
  - Target QPS:                 500+

Firestore:
  - Multi-region replication
  - Daily reads:                200K+
  - Indexes:                    100+
```

### Cost Implications

**Current (1 customer):**
```
Cloud Run:             ~$50/month
Firestore:             ~$30/month (free tier mostly)
Load Balancer:         ~$16/month
Storage:               ~$5/month
Total:                 ~$100/month
```

**Scaled (3 customers):**
```
Cloud Run:             ~$200/month (2-4x usage)
Firestore:             ~$150/month (approaching paid tier)
Load Balancer:         ~$16/month (fixed)
Storage:               ~$20/month
Total:                 ~$386/month
```

**Scaled (10 customers):**
```
Cloud Run:             ~$800/month
Firestore:             ~$600/month
Load Balancer:         ~$50/month (multi-region)
Storage:               ~$50/month
CDN (Cloud Armor):     ~$20/month
Monitoring:            ~$50/month
Total:                 ~$1,570/month
```

---

## Part 2: Auto-Scaling Configuration

### Cloud Run Auto-Scaling

**Current Configuration (Manual)**
```yaml
# cloudrun-autoscale.yaml
apiVersion: serving.knative.dev/v1
kind: Service
metadata:
  name: tai-api
  namespace: production
spec:
  template:
    metadata:
      annotations:
        autoscaling.knative.dev/maxScale: "10"
        autoscaling.knative.dev/minScale: "1"
        autoscaling.knative.dev/targetUtilization: "70"
    spec:
      containers:
      - image: gcr.io/tai-autonomics/api:latest
        resources:
          limits:
            cpu: "1"
            memory: "512Mi"
          requests:
            cpu: "500m"
            memory: "256Mi"
```

**Week 10-13 Target Configuration**
```yaml
apiVersion: serving.knative.dev/v1
kind: Service
metadata:
  name: tai-api
spec:
  template:
    metadata:
      annotations:
        autoscaling.knative.dev/maxScale: "50"              # ← Increase from 10
        autoscaling.knative.dev/minScale: "2"               # ← Keep 2 warm
        autoscaling.knative.dev/targetUtilization: "75"     # ← 75% target
        autoscaling.knative.dev/targetConcurrency: "30"     # ← 30 requests per instance
    spec:
      containers:
      - image: gcr.io/tai-autonomics/api:latest
        resources:
          limits:
            cpu: "2"                                         # ← Increase from 1
            memory: "1Gi"                                    # ← Increase from 512Mi
          requests:
            cpu: "1"
            memory: "512Mi"
```

**Configuration Steps (Week 10 - Day 1):**

1. Update Cloud Run service configuration
```bash
gcloud run services update tai-api \
  --max-instances=50 \
  --min-instances=2 \
  --memory=1Gi \
  --cpu=2 \
  --region=us-central1
```

2. Enable Cloud Run autoscaling metrics
```bash
gcloud beta run services update tai-api \
  --region=us-central1 \
  --set-env-vars=AUTOSCALE_TARGET=75,MIN_INSTANCES=2,MAX_INSTANCES=50
```

3. Verify configuration
```bash
gcloud run services describe tai-api --region=us-central1
```

### Firestore Auto-Scaling

**Current State: On-Demand Billing (No manual scaling)**
```
Billing mode: On-demand (good for variable load)
Default capacity: 50 write ops/sec, 500 read ops/sec
Burst capacity: 2x for 1 hour
```

**Optimization for 3 Customers (Week 10-11):**

1. **Create database-level capacity reservations**
```bash
# Reserve capacity for predictable workloads
gcloud firestore databases capacity-reservations create \
  --project=tai-autonomics \
  --read-ops-per-second=500 \
  --write-ops-per-second=100
```

2. **Add index optimization**
```
Current indexes: 12
Target indexes: 40+
Optimization focus:
  - Compound indexes for multi-customer queries
  - Sort fields for inventory sorting (warehouse, sku, location)
  - Filter + sort combinations
```

3. **Query optimization**
```
Before (slow):
  - Fetch all documents, filter in application
  - N+1 queries for related data

After (fast):
  - Push filters to Firestore
  - Batch queries with batch read API
  - Use collection group queries for cross-warehouse searches
```

4. **Hot collection management**
```
High-traffic collections:
  - inventory_items (100K+ docs)
  - transactions (50K+)
  - user_sessions (1K+ active)

Strategy:
  - Shard inventory_items: inventory_items_001 to 010
  - Use time-series partitioning for transactions
  - Cache user_sessions in Cloud Memorystore (Redis)
```

### Load Balancer Auto-Scaling

**Configuration (Already Optimal)**
```
Load Balancer: Cloud Load Balancing (regional)
Health checks: Every 10 seconds
Failover: Automatic to healthy backends
Geographic routing: Not needed for US-only (Week 10-13)
```

**Week 13+ Enhancement (5+ customers):**
- Upgrade to Global HTTP(S) Load Balancer
- Enable Cloud Armor (DDoS protection)
- Add CDN caching for static assets
- Geographic routing by region

---

## Part 3: Backup & Disaster Recovery

### Backup Strategy

**Firestore Automated Backups**

1. **Enable scheduled exports (Week 10 - Day 1)**
```bash
# Daily backup at 2am UTC
gcloud firestore databases backup-schedules create \
  --database=default \
  --retention-days=30 \
  --recurrence=DAILY \
  --backup-time=02:00:00
```

2. **Export destination: Cloud Storage**
```
Bucket: gs://tai-autonomics-backups
Structure:
  /firestore/daily/2026-01-26/
  /firestore/daily/2026-01-27/
  ...
Retention: 30 days
Versioning: Enabled
```

3. **Backup verification script** (run weekly)
```bash
#!/bin/bash
# Verify most recent backup is <24 hours old
LATEST_BACKUP=$(gsutil ls -h gs://tai-autonomics-backups/firestore/daily/ | tail -1)
BACKUP_DATE=$(date -f "%Y-%m-%d" -d "$(echo $LATEST_BACKUP | grep -o '20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]')")
CURRENT_DATE=$(date +%Y-%m-%d)

if [[ "$BACKUP_DATE" == "$CURRENT_DATE" ]] || [[ "$BACKUP_DATE" == "$(date -d 'yesterday' +%Y-%m-%d)" ]]; then
  echo "✓ Backup current"
  exit 0
else
  echo "✗ Backup STALE - escalate to CTO"
  exit 1
fi
```

**Cloud Run Application Backups**

1. **Docker image versioning** (automatic via Cloud Build)
   - Tag: `gcr.io/tai-autonomics/api:v1.2.3` (production releases)
   - Tag: `gcr.io/tai-autonomics/api:latest` (current)
   - Keep: 20 previous versions (for rollback)

2. **Configuration backups** (weekly)
```bash
# Backup Cloud Run environment variables, secrets
gsutil cp -r gs://tai-autonomics-config/ gs://tai-autonomics-backups/config-$(date +%Y%m%d)/
```

3. **Source code backups** (automatic via GitHub)
   - Primary: github.com/seanchatmangpt/tai-autonomics (with branch protection)
   - Mirror: gs://tai-autonomics-backups/github-mirror/ (weekly)

### Restore Procedures

**Test Restore Schedule (Critical - Non-negotiable)**

Every 2 weeks (starting Week 10):
1. **Restore to staging Firestore instance**
   ```bash
   gcloud firestore databases restore create \
     --backup=projects/tai-autonomics/locations/us-central1/backups/LATEST \
     --database=staging
   ```

2. **Verify data integrity**
   - Count documents (staging vs production)
   - Spot-check sample documents
   - Verify integrity constraints

3. **Test restore performance**
   - Measure restore time (SLO: <10 minutes)
   - Verify all data accessible within 5 minutes

4. **Document findings**
   - Restore success/failure
   - Any data anomalies
   - Recommendations for next restore

### RTO/RPO Targets

| Scenario | RTO | RPO | Procedure |
|----------|-----|-----|-----------|
| Single document corruption | 1 hour | 24 hours | Point-in-time restore, verify, merge |
| Database-wide issue | 4 hours | 24 hours | Restore from latest backup |
| Complete data loss | 8 hours | 24 hours | Full restore, extensive validation |
| Regional outage | 2 hours | 1 hour | Failover to secondary region (future) |

---

## Part 4: Monitoring & Observability Expansion

### Current Monitoring (Week 9)

**Cloud Monitoring Dashboards:** 1 (basic)
- API response times
- Cloud Run CPU/memory
- Firestore read/write errors

**Alerting:** 5 alerts
- CPU > 80%
- Error rate > 1%
- API latency p95 > 1000ms
- Quota exceeded (Firestore)
- Cloud Run quota exceeded

### Expanded Monitoring (Week 10-13)

**New Dashboards (Create 4 total)**

1. **Customer Health Dashboard**
```
Per-customer metrics:
  - Request rate (requests/minute)
  - Error rate (%)
  - P50/P95/P99 latency
  - Inventory items processed
  - API quota utilization
  - Success rate (%)

Rollup by customer:
  - Daily summary
  - Weekly trends
  - Month-over-month growth
```

2. **Infrastructure Dashboard**
```
Cloud Run:
  - Instance count (current, min, max)
  - CPU utilization (avg, p95)
  - Memory utilization
  - Concurrent request rate
  - Cold start latency

Firestore:
  - Read/write operations (actual vs quota)
  - Document count by collection
  - Query latency (p50, p95, p99)
  - Index usage
```

3. **Financial Impact Dashboard**
```
Costs:
  - Daily costs (Cloud Run, Firestore, etc.)
  - Cost per customer
  - Cost trend (7-day, 30-day)
  - Projected monthly costs

Revenue vs Costs:
  - Revenue per customer (ACV/12)
  - Gross margin per customer
  - Unit economics trend
```

4. **Business KPI Dashboard**
```
Sales:
  - Pipeline total ($)
  - Win rate (%)
  - Average deal size
  - Sales cycle length

Customer Success:
  - NPS score (by customer)
  - Implementation % complete
  - Expansion revenue (%)
  - Churn risk customers
```

**New Alerts (Add 8 total)**

```
Critical (page CTO immediately):
1. Error rate > 5%
2. API latency p95 > 2 seconds
3. Firestore quota exceeded
4. Cloud Run max instances reached
5. Certificate expiring < 7 days

Warning (email CTO + team):
6. Firestore quota > 80%
7. Error rate > 1%
8. Daily costs > threshold (TBD)

Note (dashboard only):
9. New customer added
10. API version deprecated
```

**Monitoring Configuration (Terraform)**

```hcl
# monitoring.tf - Create via infrastructure-as-code
resource "google_monitoring_dashboard" "customer_health" {
  dashboard_json = file("${path.module}/dashboards/customer-health.json")
}

resource "google_monitoring_alert_policy" "error_rate" {
  display_name       = "High Error Rate"
  notification_channels = [google_monitoring_notification_channel.pagerduty.id]

  conditions {
    display_name = "Error rate > 5%"

    condition_threshold {
      filter = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.labels.response_code_class=\"5xx\""
      comparison = "COMPARISON_GT"
      threshold_value = 0.05
    }
  }
}
```

### Observability (Tracing & Logging)

**Cloud Trace Integration (Week 10)**

1. **Enable distributed tracing**
```
Add to all API endpoints:
- Request ID generation
- Trace propagation (OpenTelemetry)
- Span timing for critical paths
- Customer ID labeling
```

2. **Trace destinations**
- Cloud Trace (real-time viewing)
- Cloud Logging (audit trail)
- Big Query (analysis)

**Cloud Logging Enhancement (Week 10)**

```
Structured logging:
- All logs as JSON
- Standard fields: timestamp, severity, request_id, customer_id, user_id
- Sample rate: 100% for errors, 5% for info (cost optimization)

Log aggregation:
- Group by customer
- Group by endpoint
- Group by error type
- Retention: 30 days

Alert triggers:
- 10+ errors in 5 minutes → Page CTO
- New error type detected → Log notification
```

---

## Part 5: Customer Deployment Procedures & Runbooks

### Standard Onboarding Timeline

**Phase 1: Pre-Implementation (Days 1-5)**
```
Monday   | CSM kickoff meeting, technical architecture review
Tuesday  | Customer admin setup (users, API keys, environment)
Wednesday| Training session (2 hours)
Thursday | Pilot environment setup, sample data load
Friday   | Go/No-go review
```

**Phase 2: Pilot (Days 6-20, 2 weeks)**
```
Week 1 | Load real data, establish baselines
Week 2 | Run full inventory cycle, validate accuracy
```

**Phase 3: Production (Days 21-30)**
```
Week 3 | Cutover: sync with legacy system, go live
Week 4 | Monitoring, optimization, success celebration
```

### Deployment Runbook for Customer #2

**Pre-Deployment Checklist (Week 10, Day 3)**

- [ ] Contract signed
- [ ] Customer admin created in Auth0
- [ ] Customer Firestore collection created
- [ ] API keys generated and securely shared
- [ ] Load Balancer SNI domain configured (customer2.tai.app)
- [ ] Monitoring alerts configured for customer #2
- [ ] Backup schedule created for customer #2 data
- [ ] CSM training materials prepared
- [ ] Technical runbooks customized

**Deployment Steps (Week 10, Day 4)**

```bash
#!/bin/bash
# deploy-customer-2.sh
# Execution time: ~30 minutes
# Owner: CTO

set -e

CUSTOMER_ID="customer-002"
CUSTOMER_NAME="Customer #2"
ENVIRONMENT="production"

echo "=== $CUSTOMER_NAME Deployment Start ==="

# Step 1: Create customer namespace in Firestore
echo "1. Creating Firestore namespace..."
gcloud firestore databases create \
  --database=customer-${CUSTOMER_ID} \
  --location=us-central1 \
  --type=firestore-native

# Step 2: Configure API access
echo "2. Configuring API keys..."
API_KEY=$(gcloud services api-keys create \
  --api-target=tai-api.json \
  --display-name="$CUSTOMER_NAME API Key")
echo "API Key: $API_KEY" >> deployment-secrets.txt

# Step 3: Load customer configuration
echo "3. Loading configuration..."
gcloud firestore documents create \
  projects/tai-autonomics/databases/customer-${CUSTOMER_ID}/documents/config \
  --data="{name:'$CUSTOMER_NAME',created:$(date +%s),status:'active'}"

# Step 4: Configure monitoring
echo "4. Setting up monitoring..."
gcloud monitoring alert-policies create \
  --display-name="$CUSTOMER_NAME - High Error Rate" \
  --notification-channels=projects/tai-autonomics/notificationChannels/CHANNEL_ID

# Step 5: Warm up Cloud Run
echo "5. Warming up API..."
for i in {1..5}; do
  curl -X GET https://api.tai.app/health \
    -H "X-Customer-ID: $CUSTOMER_ID" \
    -H "Authorization: Bearer $API_KEY"
done

# Step 6: Smoke test
echo "6. Running smoke test..."
bash tests/smoke-test.sh $CUSTOMER_ID

echo "✓ $CUSTOMER_NAME Deployment Complete"
echo "  API Endpoint: https://api.tai.app"
echo "  Customer ID: $CUSTOMER_ID"
echo "  Status: READY"
```

**Post-Deployment Verification (30 minutes)**

- [ ] API responds to requests (health check)
- [ ] Firestore data accessible
- [ ] Monitoring alerts firing correctly
- [ ] Load Balancer routing traffic correctly
- [ ] SSL/TLS certificate valid
- [ ] Customer #1 still responding normally (no regression)

### Deployment Runbook for Customer #3 (Week 11)

Same procedure as Customer #2 (reuse runbook with different CUSTOMER_ID).

---

## Part 6: Performance SLOs & Targets

### Target SLOs (Week 13)

| Metric | Target | Alert Level | Owner |
|--------|--------|------------|-------|
| API Availability | 99.5% | < 99% | CTO |
| API Latency p95 | 500ms | > 1s | CTO |
| API Latency p99 | 1s | > 2s | CTO |
| Error Rate | < 0.5% | > 1% | CTO |
| Firestore Read Latency | < 100ms | > 200ms | CTO |
| Firestore Write Latency | < 500ms | > 1s | CTO |
| Data Backup Current | < 24h old | > 48h | CTO |
| Restore Time | < 10min | > 20min | CTO |
| Certificate Expiration | > 30 days | < 14 days | CTO |

### Tracking & Reporting

**Weekly SLO Report (Friday, 4pm)**

```
Week 10:
  API Availability:    99.7% ✓
  API Latency p95:     385ms ✓
  Error Rate:          0.2% ✓
  Firestore:           All green ✓
  Backups:             Current ✓
  Status:              All systems healthy
```

**Remediation Plan** (if SLO breached)

1. **Page CTO immediately** (availability, latency)
2. **Investigate root cause** (within 30 minutes)
3. **Implement fix** (target: 1 hour resolution)
4. **Document incident** (post-mortem within 24 hours)
5. **Prevent recurrence** (action items + timeline)

---

## Implementation Timeline (Week 10-13)

### Week 10: Foundation
- [ ] Mon: Auto-scaling configuration (Cloud Run, Firestore)
- [ ] Tue: Expanded monitoring dashboards created
- [ ] Wed: Backup automation verified (weekly restore test)
- [ ] Thu: Customer #2 deployment runbook finalized + tested
- [ ] Fri: Weekly review - all systems ready for Customer #2

### Week 11: Customer #2 Live
- [ ] Mon: Customer #2 deployment (use runbook)
- [ ] Tue: Customer #3 deployment runbook finalized
- [ ] Wed: Firestore index optimization (40+ indexes)
- [ ] Thu: Load testing (simulate 3 customers)
- [ ] Fri: Weekly review - Customer #2 stable, Customer #3 ready

### Week 12: Customer #3 Live
- [ ] Mon: Customer #3 deployment (use runbook)
- [ ] Tue: Performance optimization (query tuning, caching)
- [ ] Wed: Disaster recovery test (full restore simulation)
- [ ] Thu: Security audit (data encryption, access controls)
- [ ] Fri: Weekly review - All systems stable

### Week 13: Optimization & Hardening
- [ ] Mon: Cost analysis and optimization
- [ ] Tue: SLO validation against targets
- [ ] Wed: Documentation updates
- [ ] Thu: Team training on runbooks
- [ ] Fri: Celebration + planning for next scale phase

---

## Success Criteria (Week 13 End State)

```
INFRASTRUCTURE SCALING:
  ✓ Cloud Run: 50 max instances configured
  ✓ Firestore: 40+ indexes, multi-region capable
  ✓ Load Balancer: Handling 3 customers at peak
  ✓ Cost: <$400/month for 3-customer load

AUTO-SCALING:
  ✓ Cloud Run: Auto-scales 2-50 instances
  ✓ Firestore: On-demand capacity sufficient
  ✓ No manual intervention needed during peaks

BACKUP & RECOVERY:
  ✓ Daily automated backups running
  ✓ Weekly restore tests passing
  ✓ RTO/RPO targets met
  ✓ All team members trained on restore

MONITORING:
  ✓ 4 operational dashboards live
  ✓ 8 alerts configured and working
  ✓ SLO tracking dashboard updated weekly
  ✓ Distributed tracing enabled

DEPLOYMENTS:
  ✓ Customer #2 live in production
  ✓ Customer #3 live in production
  ✓ Zero deployment incidents
  ✓ Runbook process validated

PERFORMANCE:
  ✓ API latency p95: < 500ms
  ✓ Error rate: < 0.5%
  ✓ Availability: 99.5%+
  ✓ All SLOs met or exceeded
```

---

## Risk Mitigation & Contingency

### Risk 1: Firestore Quota Exceeded (HIGH)
**Probability:** Medium (with 3 customers)
**Impact:** Total service outage
**Mitigation:**
- Monitor quota daily
- Reserve capacity in advance
- Implement query optimization
- Fallback: Scale to Cloud Datastore

### Risk 2: Cloud Run Performance Degradation (MEDIUM)
**Probability:** Low (with auto-scaling)
**Impact:** Slow customer experience
**Mitigation:**
- Load test weekly
- Increase max instances preemptively
- API rate limiting per customer
- Fallback: Enable CDN caching

### Risk 3: Backup Failure (MEDIUM)
**Probability:** Low (automated)
**Impact:** Data loss risk
**Mitigation:**
- Weekly restore tests (non-negotiable)
- 3-backup retention (daily, weekly, monthly)
- Automated verification script
- Fallback: Manual export to Cloud Storage

### Risk 4: Certificate Expiration (LOW)
**Probability:** Very low (auto-renewing)
**Impact:** HTTPS errors, customer outages
**Mitigation:**
- Automated renewal (90+ days buffer)
- Alert at 30 days
- Secondary certificate ready
- Fallback: Temporary HTTP access (last resort)

---

## Appendix A: Configuration Files

### firestore-indexes.yaml
```yaml
# Compound indexes for multi-customer queries
indexes:
  - name: InventoryByCustomerAndLocation
    collection: inventory_items
    query_scope: COLLECTION
    fields:
      - name: customer_id
        order: ASCENDING
      - name: location
        order: ASCENDING
      - name: created_at
        order: DESCENDING

  - name: TransactionsByCustomerAndDate
    collection: transactions
    query_scope: COLLECTION
    fields:
      - name: customer_id
        order: ASCENDING
      - name: transaction_date
        order: DESCENDING
      - name: status
        order: ASCENDING

  # ... 38 more indexes
```

### alerting-policy.tf
```hcl
resource "google_monitoring_alert_policy" "critical_alerts" {
  display_name = "Critical Alerts - TAI Production"

  conditions {
    display_name = "Error Rate Spike"
    condition_threshold {
      filter = "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.labels.response_code_class=\"5xx\""
      comparison = "COMPARISON_GT"
      threshold_value = 0.05
      duration = "300s"  # 5 minutes
    }
  }

  notification_channels = [
    google_monitoring_notification_channel.pagerduty.id,
    google_monitoring_notification_channel.email_cto.id
  ]
}
```

---

## Appendix B: SLO Calculation Formula

```
API Availability = (Total Requests - Failed Requests) / Total Requests
  Example: (1,000,000 - 5,000) / 1,000,000 = 99.5%

Latency p95 = 95th percentile of response times
  Example: 95% of requests complete within 500ms

Error Rate = Failed Requests / Total Requests
  Example: 5,000 / 1,000,000 = 0.5%
```

---

**Document Status:** Ready for Implementation
**Last Updated:** January 26, 2026
**Owner:** CTO
**Review Cycle:** Weekly (every Friday)
