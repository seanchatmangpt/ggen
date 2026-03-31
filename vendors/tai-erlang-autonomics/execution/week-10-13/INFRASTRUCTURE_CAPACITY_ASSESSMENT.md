# Infrastructure Capacity Assessment: Week 10-13 Analysis

**Status:** Technical Assessment Ready for Review
**Purpose:** Identify bottlenecks at 3 customers, plan scaling strategy
**Owner:** CTO
**Timeline:** Review Week 10, implement Week 10-13

---

## Part 1: Current Infrastructure (Week 9 Baseline)

### Cloud Run Configuration

**Current Setup**
```
Service: tai-api (production)
Image: gcr.io/tai-autonomics/api:latest
Region: us-central1
CPU per container: 1 vCPU
Memory per container: 512 MB
Min instances: 1 (no warm instances)
Max instances: 10
Timeout: 60 seconds
Concurrent requests: 80 per instance
Est. throughput: 800 requests/second (theoretical max with 10 instances)
```

**Customer #1 Load Profile (Week 9)**
```
Peak QPS: 40-50 (morning inventory check)
Average QPS: 15
Min instances needed: 1
Current instances active: 1-2
CPU utilization: 20-30%
Memory utilization: 25-35%
Error rate: 0.1%
P95 latency: 120ms
P99 latency: 250ms
```

**Assessment: ADEQUATE FOR 1 CUSTOMER**
- Plenty of headroom for single customer
- No scaling needed currently
- Can handle 5x load before hitting max instances

---

### Firestore Database Configuration

**Current Setup**
```
Database: default (single region)
Billing mode: On-demand
Regions: us-central1
Collections: 6 active
Documents: ~50,000 total
Storage: ~2 GB
Read latency: 5-20ms (local)
Write latency: 20-50ms (local)
```

**Firestore Collections & Document Counts**

| Collection | Doc Count | Avg Size | Total Size |
|-----------|----------|----------|-----------|
| users | 5 | 2 KB | 10 KB |
| warehouses | 2 | 1 KB | 2 KB |
| inventory_items | 45,000 | 2 KB | 90 MB |
| transactions | 3,000 | 1.5 KB | 4.5 MB |
| user_sessions | 20 | 500 B | 10 KB |
| audit_log | 1,000 | 1 KB | 1 MB |
| **TOTAL** | **49,027** | — | **~95 MB** |

**Firestore Quota Usage (Daily)**

| Operation | Daily Volume | Daily Quota | % Used |
|-----------|-------------|------------|--------|
| Document reads | 8,000 | 50,000,000 | 0.016% |
| Document writes | 4,000 | 50,000,000 | 0.008% |
| Delete ops | 100 | 50,000,000 | 0.0002% |
| Queries | 500 | Unlimited | — |
| Stored data | 95 MB | 1 GB (free) | 9.5% |

**Assessment: ADEQUATE FOR 1 CUSTOMER**
- On-demand billing is perfect for variable load
- Storage well within free tier (1 GB)
- Document reads/writes negligible vs. quota
- Indexes: 12 (basic, no compound indexes needed yet)

---

### Load Balancer Configuration

**Current Setup**
```
Type: Regional HTTP(S) Load Balancer
Regions served: us-central1
Capacity: 100 Gbps (shared across all users)
SSL/TLS: Active with auto-renewal
Health check: Every 10 seconds
Unhealthy threshold: 3 consecutive failures
Healthy threshold: 2 consecutive successes
Timeout: 30 seconds
Session affinity: None (stateless APIs)
```

**Traffic Pattern (Customer #1)**
```
Peak traffic: 50 QPS
Peak bandwidth: ~5 Mbps (assuming 100 KB avg response)
Daily data transfer: ~400 GB (50 requests/sec × 86,400 sec × 100 KB)
Load Balancer cost impact: ~$16/month (fixed, regardless of traffic)
```

**Assessment: ADEQUATE FOR 1 CUSTOMER**
- Regional LB is correct choice for US-only customers
- Bandwidth well within limits
- No DDoS concerns yet

---

### Network & Egress

**Egress Costs (Data leaving GCP)**
```
Current: Customer #1 only
  Download data: ~400 GB/month
  GCP egress cost: 400 GB × $0.12/GB = $48/month

Projected (3 customers):
  Download data: ~1,200 GB/month
  GCP egress cost: 1,200 × $0.12 = $144/month

Scaling (10+ customers):
  Download data: ~4,000 GB/month
  GCP egress cost: 4,000 × $0.12 = $480/month

Optimization: Enable Cloud CDN (cache static content)
  → Reduces egress 30-50% (saves $150-200/month at scale)
```

---

## Part 2: Bottleneck Analysis (3 Customers Scenario)

### Scaling Load: Customer #2 & #3 Go-Live

**Assumption: All 3 customers active simultaneously**
```
Customer #1: 40-50 QPS (morning peak)
Customer #2: 30-40 QPS (morning peak, new customer)
Customer #3: 25-35 QPS (morning peak, new customer)
TOTAL: 95-125 QPS peak
```

### Bottleneck 1: Cloud Run Capacity (HIGH RISK)

**Current Setup**
```
Max instances: 10
CPU per instance: 1 vCPU
Memory per instance: 512 MB
Concurrent requests per instance: 80
Est. max throughput: 10 instances × 80 req/inst = 800 QPS
Actual (with latency): ~200 QPS at acceptable latency
```

**3-Customer Load Analysis**
```
Peak QPS: 125 requests/second
Instances needed (100 req/instance): 125/100 = 1.25 → min 2 instances
BUT: With latency target (p95 < 500ms), need more headroom

Estimated instances needed for p95 < 500ms:
  - Per instance: ~50 concurrent requests (not 80)
  - For 125 QPS: 125/50 = 2.5 → min 3 instances

Current max: 10 instances
Available headroom: 10 - 3 = 7 instances (70% spare capacity) ✓

VERDICT: NOT A BOTTLENECK if max instances increased to 50
```

**Recommendation:**
```
Week 10 Action:
  1. Increase max instances: 10 → 50
  2. Decrease min instances: 1 → 2 (keep 2 warm for fast startup)
  3. Update target concurrency: 80 → 30 (be more aggressive on scaling)

Result:
  - Auto-scales from 2-50 instances based on load
  - Safely handles 3 customers + 5x growth buffer
  - Cost: ~$200/month (vs $100 for 1 customer)
```

### Bottleneck 2: Firestore Read/Write Capacity (HIGH RISK)

**3-Customer Load Projection**
```
Customer #1: 8,000 reads/day + 4,000 writes/day
Customer #2: 7,000 reads/day + 3,500 writes/day (similar size)
Customer #3: 5,000 reads/day + 2,500 writes/day (slightly smaller)
TOTAL: 20,000 reads/day + 10,000 writes/day
Daily avg: 20,000 / 86,400 sec = 0.23 reads/sec, 0.11 writes/sec
```

**On-Demand Quota**
```
Firestore on-demand mode:
  - Burst write capacity: 50 writes/sec (sustainable)
  - Burst read capacity: 500 reads/sec (sustainable)

3-Customer load: 0.11 writes/sec + 0.23 reads/sec = WELL WITHIN QUOTA ✓

Peak bursts (unlikely):
  - If 3 customers all do inventory sync simultaneously: 500+ reads/sec
  - On-demand handles this without issue ✓

Cost impact:
  - Cost per operation: $0.000001 per write, $0.000001 per read
  - 10,000 writes/day = $0.01/day = $0.30/month (negligible)
  - Total 20,000 ops/day = $0.60/month
  - Firestore baseline: $6/month → projected $20/month (manageable)

VERDICT: NOT A BOTTLENECK with on-demand billing
```

**Recommendation:**
```
Week 10 Action:
  1. Keep on-demand billing (most cost-effective for variable load)
  2. Monitor quota usage (alert at 50% utilization)
  3. No optimization needed for 3 customers

Future (10+ customers): Consider provisioned capacity
  - Cost: $5/month per 100 write ops/sec
  - Benefit: Predictable costs, slightly better latency
```

### Bottleneck 3: Firestore Indexing (MEDIUM RISK)

**Current Indexes: 12**

These indexes are basic (single-field). Multi-customer queries require compound indexes.

**New Indexes Needed (20-30 total)**

```
Current Indexes (12):
  - inventory_items (single field): sku, location, created_at
  - transactions (single field): type, status, created_at
  - users (single field): email, role, warehouse_id
  [9 more auto-created indexes]

New Compound Indexes (20-25):
  - By customer + location: inventory_items (customer_id, location)
  - By customer + date: transactions (customer_id, created_at DESC)
  - By warehouse + sku: inventory_items (warehouse_id, sku)
  - By status + date: transactions (status, created_at DESC)
  - By customer + location + sku: inventory_items (customer_id, location, sku)
  [15-20 more as needed]
```

**Implementation Cost**
```
Index creation time: 10-30 minutes per index (Firestore auto-builds)
Impact on reads: None (indexes built in background)
Impact on writes: Minimal (5-10% slower during indexing)
Cost: Free (indexes included in on-demand pricing)
```

**Recommendation:**
```
Week 10 Action:
  1. Analyze query patterns (CTO reviews code)
  2. Create 5-10 most-used compound indexes
  3. Monitor slow queries (queries >500ms)
  4. Create additional indexes as needed (based on monitoring)

Weekly Action:
  - Monitor slow queries
  - Create new indexes proactively (before they cause performance issues)

Target: 40-50 total indexes by Week 13
```

### Bottleneck 4: API Latency (MEDIUM RISK)

**Current Latency (Customer #1)**
```
P50: 50ms (median)
P95: 120ms (95th percentile)
P99: 250ms (99th percentile)
Max: 500ms (99.9th percentile)
Target: P95 < 500ms ✓ ACHIEVED
```

**Projected Latency (3 Customers)**

If no optimization:
```
Same query complexity, but 3x database size
- Database latency increase: 5-10% (indexes help)
- Network latency: Unchanged
- Application processing: Unchanged

Projected P95: 120ms × 1.1 = 132ms (still good) ✓
```

If optimization applied:
```
Apply: Caching (Redis), Query optimization, Lazy loading
- Database latency reduction: 30-50%
- Network latency reduction: 10% (CDN)
- Application processing: 10-20% faster (caching)

Projected P95: 120ms × 0.7 = 84ms (even better) ✓
```

**Recommendation:**
```
Week 10-11 Action:
  1. Enable Cloud Memorystore (Redis) for session caching
     - Cost: $25/month (0.5 GB instance)
     - Benefit: 100-200ms latency reduction for frequently accessed data

  2. Query optimization
     - Review slow queries (>200ms)
     - Add indexes or simplify queries

  3. Implement lazy loading (for dashboard data)
     - Load dashboard "above fold" data (100ms target)
     - Load detail data asynchronously (user doesn't wait)

Target: P95 latency < 300ms by Week 13 (even with 3 customers)
```

### Bottleneck 5: Cold Starts (LOW-MEDIUM RISK)

**Current Cold Start Latency**
```
Time to first response: 3-5 seconds (first request after idle)
Cause: Docker container startup, dependency initialization
Frequency: Rare (min instances = 1, usually warm)
Impact: Occasional slow requests during off-hours

With 3 customers: More requests → cold starts even rarer ✓
```

**Recommendation:**
```
Week 10 Action:
  1. Set min instances = 2 (always keep 2 warm)
     - Cost: ~$50/month extra
     - Benefit: Eliminates cold starts
     - Worth it for production reliability

  2. Implement warm-up calls (optional)
     - CloudScheduler hits API every 2 minutes
     - Keeps containers warm
     - Cost: Negligible (<$1/month)
```

---

## Part 3: Scaling Targets & Implementation

### Phase 1: 3 Customers (Week 13 Target)

**Cloud Run**
```
Min instances: 2 (vs. 1)
Max instances: 50 (vs. 10)
CPU per instance: 2 vCPU (vs. 1)
Memory per instance: 1 GB (vs. 512 MB)
Target concurrency: 30 requests (vs. 80, be more aggressive)
Result: 50 instances × 30 req/inst = 1,500 QPS capacity ✓
Cost: ~$200/month (vs. $100) → 2x cost, 3x capacity
```

**Firestore**
```
Billing mode: On-demand (unchanged)
Daily reads: 20,000 (vs. 8,000)
Daily writes: 10,000 (vs. 4,000)
Total indexes: 40-50 (vs. 12)
Cost: ~$20/month (vs. ~$6) → modest increase
```

**Load Balancer**
```
Type: Regional (unchanged)
Health checks: Every 10 seconds (unchanged)
SSL/TLS: Active (unchanged)
Cost: ~$16/month (fixed, unchanged)
```

**Network Egress**
```
Monthly data: ~1,200 GB (vs. 400 GB)
Cost: ~$144/month (vs. $48) → 3x increase
Optimization: Not needed yet for 3 customers
```

**Total Infrastructure Cost (Week 13)**
```
Cloud Run: $200/month
Firestore: $20/month
Load Balancer: $16/month
Network egress: $144/month
Monitoring: $25/month
Redis cache: $25/month (optional)
Misc: $10/month
TOTAL: ~$440/month (vs. $100 for 1 customer)
```

### Phase 2: 10 Customers (Month 5 Target)

**Cloud Run**
```
Regions: 3 (us-central1, us-east1, us-west1)
Instances per region: 10-20 average, 50-100 peak
Total: 150+ instances at peak
Cost: ~$800/month
```

**Firestore**
```
Multi-region replication: Enabled (for resilience)
Daily operations: 200K+ reads/writes
Indexes: 100+
Cost: ~$200-300/month
```

**Load Balancer**
```
Type: Global HTTP(S) Load Balancer
Cost: ~$50/month (vs. $16)
```

**Total Infrastructure Cost (10 Customers)**
```
Cloud Run: $800/month
Firestore: $300/month
Load Balancer: $50/month
Network egress: $500/month
Monitoring: $50/month
Redis cache: $50/month
CDN/Cloud Armor: $20/month
Misc: $30/month
TOTAL: ~$1,800/month
Cost per customer: ~$180/month (vs. $200 for Customer #1 alone)
→ Economies of scale kick in ✓
```

---

## Part 4: Performance Optimization (Week 10-11)

### Quick Wins (No Code Changes, 2-4 Hours)

1. **Firestore Indexes**
   - Time: 2 hours (analysis + creation)
   - Benefit: 20-30% latency improvement on complex queries
   - Cost: Free

2. **Cloud Run Configuration**
   - Time: 1 hour (update via gcloud)
   - Benefit: Faster scaling, warm instances
   - Cost: +$50/month (but essential for production)

3. **Load Balancer Tuning**
   - Time: 1 hour (update health check settings)
   - Benefit: Faster failover, more stable
   - Cost: None

### Medium-Term Optimizations (Code Changes, 1-2 Weeks)

1. **Query Optimization**
   - Time: 8-16 hours (CTO + engineers)
   - Approach: Review slow queries, simplify, add pagination
   - Benefit: 30-50% latency reduction
   - Cost: None

2. **Redis Caching**
   - Time: 4-8 hours (implement caching layer)
   - Approach: Cache user sessions, customer config, dashboard data
   - Benefit: 100-200ms latency reduction
   - Cost: $25/month (Redis instance)

3. **Pagination & Lazy Loading**
   - Time: 4-8 hours (implement in UI + API)
   - Approach: Load first 50 items, load more on scroll
   - Benefit: Perceived performance improvement, 50% bandwidth reduction
   - Cost: None

### Long-Term Optimizations (Architecture, 4+ Weeks)

1. **Sharding (if needed at 50+ customers)**
   - Approach: Partition inventory_items by customer_id
   - Benefit: Query performance maintained at scale
   - Cost: Engineering effort, increased complexity

2. **Multi-Region Deployment**
   - Approach: Deploy API to 3+ regions, route by geography
   - Benefit: Reduced latency for global customers
   - Cost: $500/month infrastructure, engineering effort

3. **GraphQL API**
   - Approach: Replace REST with GraphQL (if client needs flexible queries)
   - Benefit: Reduce over-fetching, more efficient data transfer
   - Cost: Engineering effort, client changes

---

## Part 5: Disaster Recovery & Resilience

### Current State (Week 9)

**Single Point of Failures**
- [ ] Single region (us-central1) → Regional outage = complete downtime
- [ ] Single database (Firestore) → Data loss risk
- [ ] Single API instance (usually 1-2 warm) → No redundancy
- [ ] Single load balancer → No failover

**Risk Level: HIGH**
```
RPO (Recovery Point Objective): 24 hours (daily backups)
RTO (Recovery Time Objective): 8 hours (manual restore)
This is NOT acceptable for production!
```

### Target State (Week 13)

**Redundancy Improvements**

1. **Multi-Instance**
   - Min instances: 2 (always running)
   - Load Balancer: Automatically routes to healthy instance
   - Result: 1 instance can fail, service continues ✓

2. **Automated Backups**
   - Frequency: Daily (automated)
   - Retention: 30 days
   - RPO: 24 hours ✓
   - RTO: <10 minutes (automated restore)

3. **Monitoring & Alerting**
   - Alerts on high error rate, latency spike
   - Page CTO immediately on critical issues
   - Result: Issues detected in <5 minutes

4. **Graceful Degradation**
   - Circuit breakers on external APIs
   - Cached data fallback (if Firestore slow)
   - Read-only mode (if writes fail)
   - Result: Service continues even with issues ✓

**Target Metrics (Week 13)**
```
Availability: 99.5% (vs. current ~95%)
RPO: 24 hours (vs. undefined)
RTO: 10 minutes (vs. 8 hours)
MTTR: <30 minutes (mean time to repair)
```

---

## Part 6: Cost Analysis & Optimization

### Cost Breakdown (Week 13)

| Component | Cost | Variable | Notes |
|-----------|------|----------|-------|
| Cloud Run | $200 | Yes | 2-50 instances, $0.0000247/sec per vCPU |
| Firestore | $20 | Yes | On-demand, ~$0.06 per 100K ops |
| Load Balancer | $16 | No | Fixed regional cost |
| Network egress | $144 | Yes | 1,200 GB/month @ $0.12/GB |
| Redis cache | $25 | No | 0.5 GB instance |
| Monitoring | $25 | No | Cloud Monitoring logs + dashboards |
| **TOTAL** | **$430** | — | **Per month** |

### Cost per Customer (Week 13)

```
Customer #1: $143/month (40% allocation)
Customer #2: $143/month (40% allocation)
Customer #3: $144/month (20% allocation)
Average: $143/month per customer

Cost as % of ACV:
  - Customer #1: $143 × 12 = $1,716 / $50,000 ACV = 3.4% ✓
  - Customer #2: $143 × 12 = $1,716 / $40,000 ACV = 4.3% ✓
  - Customer #3: $144 × 12 = $1,728 / $35,000 ACV = 4.9% ✓

Gross margin (if COGS = 10%):
  - Revenue: $125,000 / 12 = $10,417/month
  - COGS (infrastructure): $430/month
  - Gross profit: $10,417 - $430 = $9,987/month
  - Gross margin: 96% ✓ EXCELLENT
```

### Cost Optimization Opportunities

**Quick Wins (Week 10-11)**
1. **Reduce egress cost** (enable Cloud CDN)
   - Benefit: 30% reduction = $43/month savings
   - Cost: $10/month CDN fee
   - Net savings: $33/month
   - Effort: 2 hours

2. **Optimize queries** (faster = fewer compute seconds)
   - Benefit: 20% Cloud Run cost reduction = $40/month savings
   - Effort: 16 hours (CTO + engineers)
   - ROI: Very high (ongoing savings)

3. **Use committed use discounts** (CUD) for predictable workloads
   - If: 3-4 dedicated instances always running
   - Benefit: 25% discount on Cloud Run = $50/month savings
   - Cost: Commit to 1-year contract, less flexibility
   - Effort: 1 hour setup

**Potential Total Savings (with optimization): $100+/month (23% reduction)**

---

## Part 7: Implementation Checklist (Week 10-13)

### Week 10: Foundation

- [ ] **Monday (Day 1)**
  - [ ] CTO reviews this assessment with team (1 hour)
  - [ ] Priorities confirmed: indexing, auto-scaling, monitoring
  - [ ] Assign work items

- [ ] **Tuesday-Wednesday**
  - [ ] Cloud Run scaling config updated (max instances 10→50, min 1→2)
  - [ ] Verify update: `gcloud run services describe tai-api`
  - [ ] Monitor auto-scaling behavior (no issues expected)

- [ ] **Wednesday-Thursday**
  - [ ] Firestore indexes created (5-10 compound indexes)
  - [ ] Slow query analysis (identify any >500ms queries)
  - [ ] Add indexes for slow queries

- [ ] **Friday**
  - [ ] Performance baseline established (latency, error rate)
  - [ ] Weekly review: Ready for Customer #2?
  - [ ] Document findings

### Week 11: Customer #2 Deployment

- [ ] **During Customer #2 onboarding**
  - [ ] Monitor Cloud Run scaling (should scale 1→3 instances)
  - [ ] Monitor Firestore latency (should remain < 100ms)
  - [ ] Monitor error rates (should remain < 0.5%)
  - [ ] Verify backup automation is working

### Week 12: Optimization

- [ ] Query optimization (if needed, >500ms queries found)
- [ ] Redis caching implementation (if performance needs boost)
- [ ] Load testing (simulate 3 customers, verify SLOs)
- [ ] Disaster recovery test (full restore simulation)

### Week 13: Validation & Documentation

- [ ] **Capacity validation**
  - [ ] All 3 customers live: Measure peak latency, error rate
  - [ ] Compare to targets: P95 < 500ms? Error rate < 0.5%?
  - [ ] All SLOs met? ✓

- [ ] **Documentation**
  - [ ] Update WEEK_10_13_OPERATIONS_SCALING.md with actual results
  - [ ] Document any issues encountered and resolutions
  - [ ] Create runbook for future scaling (copy template, update numbers)

- [ ] **Team training**
  - [ ] CTO trains team on monitoring dashboards
  - [ ] Team trained on incident response
  - [ ] Runbooks reviewed and understood

---

## Success Criteria (Week 13 End State)

```
INFRASTRUCTURE SCALING:
  ✓ Cloud Run: 50 max instances configured (vs. 10)
  ✓ Firestore: 40-50 indexes (vs. 12)
  ✓ All 3 customers operating at peak simultaneously
  ✓ No auto-scaling issues or timeout errors

PERFORMANCE:
  ✓ API latency P95: < 500ms (with 3 customers)
  ✓ Error rate: < 0.5%
  ✓ Availability: 99.5%+
  ✓ Cold starts: Eliminated (min 2 instances)

COST EFFICIENCY:
  ✓ Infrastructure cost: ~$430/month (3.4-4.9% of ACV)
  ✓ No cost overruns (within budget)
  ✓ Cost per customer: ~$143/month (scalable)

RELIABILITY:
  ✓ Backups: Automated, tested weekly
  ✓ Monitoring: 4 dashboards, 8 alerts
  ✓ SLOs: Tracked and reported weekly
  ✓ Incident response: Tested and documented

DOCUMENTATION:
  ✓ Capacity assessment completed
  ✓ Optimization recommendations implemented
  ✓ Runbooks created and tested
  ✓ Team trained on new infrastructure
```

---

**Document Status:** Ready for CTO Review
**Last Updated:** January 26, 2026
**Owner:** CTO
**Next Review:** Week 11 (monitor actual vs. projected)
