# Technical Architecture: Multi-Region Deployment

## 1. System Architecture Overview

### 1.1 High-Level System Design

```
┌──────────────────────────────────────────────────────────────────────────┐
│                          Global User Request                             │
└──────────────────────────┬───────────────────────────────────────────────┘
                           │
                    ┌──────▼──────┐
                    │Cloud Armor  │ DDoS Protection, WAF Rules
                    └──────┬──────┘
                           │
                    ┌──────▼────────────┐
                    │Global Load        │ Health-based routing
                    │Balancer           │ Latency-based affinity
                    └──────┬────────────┘
                           │
          ┌────────────────┼────────────────┐
          │                │                │
    ┌─────▼────────┐  ┌────▼──────┐  ┌─────▼─────────┐
    │us-central1   │  │ us-east1  │  │europe-west1   │
    │(Primary)     │  │(Secondary)│  │(Secondary)    │
    │              │  │           │  │               │
    │┌────────────┐│  │┌────────┐ │  │┌──────────────┐│
    ││Cloud Run  ││  ││Cloud   │ │  ││Cloud Run     ││
    ││Services   ││  ││Run     │ │  ││Services      ││
    ││            ││  ││Services││  ││               ││
    │└─────┬──────┘│  │└───┬────┘ │  │└───┬──────────┘│
    │      │       │  │    │      │  │    │          │
    │┌─────▼──────┐│  │┌────▼────┐│  │┌────▼─────────┐│
    ││Firestore   ││  ││Firestore││  ││Firestore     ││
    ││(Read/Write)││  ││(Read)   ││  ││(Read)        ││
    ││Multi-      ││  ││Replica  ││  ││Replica       ││
    ││region      ││◄─┤◄─────────┤├──┤               ││
    │└────────────┘│  │└────────┘ │  │└───────────────┘│
    │              │  │           │  │                │
    │┌────────────┐│  │┌────────┐ │  │┌──────────────┐│
    ││Redis      ││  ││Redis   │ │  ││Redis         ││
    ││Sentinel   ││  ││Replica │ │  ││Replica       ││
    ││(Primary)  ││◄─┤│(sync)  ││◄──┤(sync)         ││
    │└────────────┘│  │└────────┘ │  │└──────────────┘│
    │              │  │           │  │                │
    │┌────────────┐│  │┌────────┐ │  │┌──────────────┐│
    ││Cloud KMS   ││  ││Cloud   │ │  ││Cloud KMS    ││
    ││Key Ring    ││  ││KMS     │ │  ││Key Ring     ││
    ││(Replicated)││  ││Key Ring││  ││(Replicated)  ││
    │└────────────┘│  │└────────┘ │  │└──────────────┘│
    │              │  │           │  │                │
    │┌────────────┐│  │┌────────┐ │  │┌──────────────┐│
    ││Backup      ││  ││Backup  │ │  ││Backup       ││
    ││Storage     ││  ││Storage │ │  ││Storage      ││
    ││(GCS)       ││  ││(GCS)   │ │  ││(GCS)        ││
    │└────────────┘│  │└────────┘ │  │└──────────────┘│
    └──────────────┘  └───────────┘  └────────────────┘
```

### 1.2 Replication Architecture

```
PRIMARY (us-central1)          SECONDARY (us-east1)       SECONDARY (europe-west1)
┌────────────────────┐         ┌──────────────────┐       ┌──────────────────┐
│ Firestore          │         │ Firestore        │       │ Firestore        │
│ Database           │─────────│ Read Replica     │───────│ Read Replica     │
│ (Authoritative)    │         │ (Sync)           │       │ (Sync)           │
└────────────────────┘         └──────────────────┘       └──────────────────┘
          │                             │                        │
          └─────────────────────────────┴────────────────────────┘
                    Event Stream (Pub/Sub)
                    < 5s replication lag

┌────────────────────┐         ┌──────────────────┐       ┌──────────────────┐
│ Redis Sentinel     │         │ Redis Sentinel   │       │ Redis Sentinel   │
│ + Cluster          │─────────│ + Replica        │───────│ + Replica        │
│ (Primary Cache)    │         │ (Read-only)      │       │ (Read-only)      │
└────────────────────┘         └──────────────────┘       └──────────────────┘
          │                             │                        │
          └─────────────────────────────┴────────────────────────┘
            Cache Invalidation (Pub/Sub + Direct Sync)
                  < 1s cache sync
```

## 2. Firestore Multi-Region Strategy

### 2.1 Firestore Configuration

**Setup**:
- **Primary Database**: us-central1 (transactional authority)
- **Read Replicas**: us-east1, europe-west1 (eventual consistency)
- **Consistency Model**: Eventual consistency with causal ordering
- **Replication Lag Target**: < 5 seconds (99th percentile)

**Collections Replicated**:
```
marketplace_products/          # Catalog data (replicate all)
marketplace_skus/              # SKU inventory (replicate all)
marketplace_users/             # User profiles (replicate all)
marketplace_orders/            # Orders (replicate all, with conflict resolution)
marketplace_payments/          # Payment history (replicate all)
marketplace_subscriptions/     # Subscription state (replicate all)
marketplace_analytics/         # Analytics events (replicate all)
```

### 2.2 Consistency & Conflict Resolution

**Conflict Resolution Strategy**:

```
Conflict Scenario:
┌──────────────────┐         ┌──────────────────┐
│ us-central1      │         │ us-east1         │
│                  │         │                  │
│ Order #123       │         │ Order #123       │
│ status: pending  │         │ status: pending  │
│ ts: 1000         │         │ ts: 1000         │
│ vector: [1,0,0]  │         │ vector: [1,0,0]  │
└──────────────────┘         └──────────────────┘

LOCAL WRITE (us-central1):
  status: approved
  ts: 1005
  vector: [2,0,0]

CONCURRENT WRITE (us-east1):
  status: rejected
  ts: 1004
  vector: [1,1,0]

RESOLUTION (Vector Clock):
  [2,0,0] > [1,1,0] → us-central1 wins (causal ordering)
  Final state: status=approved, vector=[2,0,0], ts=1005
```

**Conflict Resolution Implementation**:
1. **Vector Clocks**: Track causal ordering of writes
2. **Timestamps**: Last-Write-Wins (LWW) for tie-breaking
3. **Application Logic**: Custom reconciliation for business logic conflicts

### 2.3 Replication Pipeline

```
Write in us-central1
    ↓
Firestore Multi-region Sync
    ↓
Pub/Sub: firestore-replication topic
    ↓
┌─────────────────────────────────────┐
│ Replication Worker (Cloud Run)       │
│ - Apply document changes             │
│ - Handle conflicts (vector clocks)   │
│ - Update replicas (batched)          │
│ - Report replication lag metrics     │
└─────────────────────────────────────┘
    ↓
Update us-east1 and europe-west1 replicas
    ↓
Monitor replication lag
    ↓
Alert if lag > 10 seconds
```

**Code Example: Replication Worker**

```go
// Replication worker listens to Pub/Sub and applies changes
type ReplicationWorker struct {
    primaryDB    *firestore.Client  // us-central1
    replicaDBs   map[string]*firestore.Client  // us-east1, europe-west1
    conflictMgr  *ConflictResolver
    metricsDB    *firestore.Client  // Track lag
}

func (w *ReplicationWorker) ApplyChange(ctx context.Context, msg *pubsub.Message) error {
    var change ChangeEvent
    json.Unmarshal(msg.Data, &change)

    // Resolve conflicts using vector clocks
    resolved := w.conflictMgr.Resolve(change)

    // Apply to all replicas (batched)
    batch := w.primaryDB.Batch()
    for region, replica := range w.replicaDBs {
        batch.Set(replica.Doc(change.DocPath), resolved)
    }

    // Record replication timestamp
    w.recordLag(ctx, change.Region, time.Now())

    return batch.Commit(ctx)
}
```

## 3. Redis Cross-Region Failover

### 3.1 Redis Architecture

**Primary** (us-central1):
- Redis Sentinel cluster (3 nodes)
- Main Redis cluster (6 nodes + 2 replicas)
- Handles all read/write operations
- Backups to GCS daily

**Replicas** (us-east1, europe-west1):
- Redis Sentinel (1 node each, read-only replicas)
- Cross-region replication via Sentinel
- Automatic failover to primary if degradation detected
- Async write operations accepted during primary failure

### 3.2 Failover Mechanism

```
Health Check Loop (every 30s):

┌─────────────────────────┐
│ Monitor Primary Redis   │
│ us-central1             │
│ - Connection test       │
│ - Latency check (<50ms) │
│ - Memory check (>80%)   │
│ - Replication lag check │
└────────────┬────────────┘
             │
             ├─ HEALTHY ──────► Continue normal operation
             │
             ├─ UNHEALTHY (1) ─► Mark degraded, retry
             │
             ├─ UNHEALTHY (3x) ┐
             └─────────────────┤
                               │
                      ┌────────▼──────────┐
                      │ Failover Trigger  │
                      │ - Promote replica │
                      │ - Route traffic   │
                      │ - Alert ops team  │
                      └──────────────────┘
```

### 3.3 Failover Procedure

1. **Health Check Failure**: 3 consecutive failed checks (90 seconds)
2. **Automatic Promotion**:
   - Promote us-east1 replica to primary
   - Update DNS/Load Balancer
   - Route new writes to us-east1
3. **Graceful Degradation**:
   - Clients buffer writes locally
   - Retry with exponential backoff
   - Sync when primary recovers
4. **Recovery**:
   - Restore us-central1 as replica
   - Resync data (conflict resolution)
   - Monitor replication lag
5. **Promotion Back** (manual):
   - Run `scripts/promote-region.sh` when us-central1 is healthy
   - Verify replication lag < 1 second
   - Execute promotion with zero-downtime migration

## 4. Traffic Routing & Load Balancing

### 4.1 Global Load Balancer Configuration

```
┌─────────────────────────────────────┐
│ Cloud Armor (WAF)                   │
│ - DDoS protection                   │
│ - GeoIP blocking                    │
│ - Rate limiting (1000 req/s)        │
└────────────┬────────────────────────┘
             │
     ┌───────▼────────┐
     │ Cloud Load      │
     │ Balancer        │
     │ (Global)        │
     └───────┬────────┘
             │
    ┌────────┴──────────┐
    │                   │
┌───▼────────┐   ┌──────▼───────┐
│ Health     │   │ Health       │
│ Check      │   │ Check        │
│ us-central1│   │ us-east1     │
└───┬────────┘   └──────┬───────┘
    │                   │
    │ HEALTHY?          │ HEALTHY?
    │ YES               │ YES
    │                   │
    ├──────────────────┼──────────────────┐
    │                  │                  │
    ▼                  ▼                  ▼
┌──────────────────────────────────────────────┐
│ Routing Rules (Priority Order):              │
│ 1. Geographic affinity (user location)       │
│ 2. Latency-based (closest region)           │
│ 3. Capacity-based (available resources)      │
│ 4. Cost-aware (leverage cheaper regions)     │
└──────────────────────────────────────────────┘
```

### 4.2 Istio Service Mesh Traffic Management

**VirtualService Configuration**:
```yaml
apiVersion: networking.istio.io/v1beta1
kind: VirtualService
metadata:
  name: ggen-marketplace-api
spec:
  hosts:
  - api.ggen-marketplace.example.com
  http:
  # Route based on geography
  - match:
    - withoutHeaders:
        user-region:
          regex: "us-east.*|us-central.*"
    route:
    - destination:
        host: ggen-marketplace.us-central1.svc.cluster.local
        port:
          number: 443
      weight: 100
    timeout: 5s
    retries:
      attempts: 3
      perTryTimeout: 1s

  # Route based on latency
  - route:
    - destination:
        host: ggen-marketplace.us-central1.svc.cluster.local
      weight: 50
    - destination:
        host: ggen-marketplace.us-east1.svc.cluster.local
      weight: 35
    - destination:
        host: ggen-marketplace.europe-west1.svc.cluster.local
      weight: 15
```

### 4.3 Circuit Breaker Configuration

```yaml
apiVersion: networking.istio.io/v1beta1
kind: DestinationRule
metadata:
  name: ggen-marketplace-api
spec:
  host: ggen-marketplace.default.svc.cluster.local
  trafficPolicy:
    connectionPool:
      tcp:
        maxConnections: 100
      http:
        http1MaxPendingRequests: 100
        maxRequestsPerConnection: 2
    outlierDetection:
      consecutive5xxErrors: 5
      interval: 30s
      baseEjectionTime: 30s
      maxEjectionPercent: 100
      minRequestVolume: 5
```

## 5. KEDA Autoscaling Configuration

### 5.1 CPU-Based Autoscaling

```yaml
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: ggen-marketplace-cpu
spec:
  scaleTargetRef:
    name: ggen-marketplace-api
  minReplicaCount: 2
  maxReplicaCount: 20
  triggers:
  - type: cpu
    metricType: Utilization
    metadata:
      value: "70"  # Scale up if CPU > 70%
```

### 5.2 Request-Rate Based Autoscaling

```yaml
apiVersion: keda.sh/v1alpha1
kind: ScaledObject
metadata:
  name: ggen-marketplace-requests
spec:
  scaleTargetRef:
    name: ggen-marketplace-api
  minReplicaCount: 2
  maxReplicaCount: 20
  triggers:
  - type: prometheus
    metadata:
      serverAddress: https://monitoring.googleapis.com
      metricName: request_rate
      threshold: "1000"  # Scale up if > 1000 req/s
      query: |
        rate(http_requests_total[1m])
```

## 6. Health Check & Failover Automation

### 6.1 Health Check Configuration

**Endpoint Health Checks** (every 30 seconds):
```yaml
Check Type              | Target                    | Failure Threshold
Service Readiness       | /health endpoint          | 2 consecutive failures
Data Consistency        | Replication lag           | > 10 seconds
Cache Connectivity      | Redis Sentinel ping       | 3 consecutive timeouts
Database Connectivity   | Firestore write test      | 2 consecutive failures
Certificate Expiry      | TLS cert in < 30 days     | Immediate alert
```

### 6.2 Automatic Failover Logic

```
FAILOVER_STATE_MACHINE:

1. HEALTHY
   - All health checks pass
   - Replication lag < 5s
   - Traffic routed normally

2. DEGRADED (1 failed check)
   - Alert to #incidents channel
   - Begin retry attempts
   - Maintain current routing

3. UNHEALTHY (2+ failed checks)
   - Page on-call engineer
   - Prepare failover
   - Increase monitoring frequency (10s)

4. FAILURE (3+ consecutive checks fail)
   - Execute automatic failover
   - Page incident commander
   - Trigger runbook alerts

5. RECOVERY
   - Restore failed region
   - Resync data
   - Verify replication lag < 5s
   - Promote back to primary (manual)
```

## 7. Monitoring & Observability

### 7.1 Key Metrics

```
Metric                          | Target    | Warning Threshold | Critical Threshold
Replication Lag                 | < 5s      | > 10s             | > 30s
Health Check Success Rate       | > 99.9%   | < 98%             | < 95%
Cross-region Latency (p95)      | < 200ms   | > 300ms           | > 500ms
Cache Hit Rate                  | > 80%     | < 70%             | < 50%
Error Rate (4xx, 5xx)           | < 0.1%    | > 0.5%            | > 1%
Firestore Reads (cost)          | < 1M/day  | > 1.5M/day        | > 2M/day
Data Consistency Violations     | 0         | > 10              | > 100
Backup Success Rate             | 100%      | < 99%             | < 95%
```

### 7.2 Alerting Policies

All configured in GCP Cloud Monitoring:

```
Alert Name                      | Severity | Escalation
Replication Lag > 10s           | WARNING  | #incidents (Slack)
Replication Lag > 30s           | CRITICAL | Page on-call DBA
Health Check Failures (2+ regions)| CRITICAL| Page incident commander
Failover Triggered              | WARNING  | Email ops team
Backup Failure                  | CRITICAL | Page on-call DBA
Certificate Expiring in 30d     | WARNING  | #incidents (Slack)
Unusual Egress Traffic          | WARNING  | #security (Slack)
KMS Key Rotation Failed         | CRITICAL | Page on-call security
```

---

**Last Updated**: 2026-01-25
**Maintainers**: Platform Engineering Team
**Review Cadence**: Monthly
