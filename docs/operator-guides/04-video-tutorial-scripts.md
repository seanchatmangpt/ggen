# TPS Video Tutorial Scripts

**Version**: 1.0
**Format**: Scripts for 5 video tutorials (~8 minutes each)
**Audience**: New operators, visual learners
**Goal**: Step-by-step walkthrough of common operational tasks

---

## Video 1: "Deploy TPS System to Production" (8 minutes)

### Intro (0:00-0:30)

**Host on screen**: Standing next to monitor showing empty terminal

"Hi, I'm [Name], and I'm going to show you how to deploy a complete TPS (Toyota Production System) to production in less than 10 minutes. We'll be deploying to GKE with full observability: Prometheus for metrics, Grafana for dashboards, Jaeger for tracing, and Loki for logs. Let's get started."

**Cut to**: Full screen terminal

### Part 1: Prerequisites (0:30-1:30)

**Narration**: "First, let's verify we have everything we need."

**On screen**:
```bash
# Show 1: kubectl version
$ kubectl version --short
Client Version: v1.25.0
Server Version: v1.25.5
✓ kubectl is working

# Show 2: helm version
$ helm version --short
v3.10.0
✓ helm is installed

# Show 3: GKE cluster info
$ kubectl cluster-info
Kubernetes master is running at https://1.2.3.4
CoreDNS is running at https://1.2.3.4/api/v1/namespaces/kube-system/services/coredns
✓ GKE cluster is ready

# Show 4: Node count
$ kubectl get nodes
NAME          STATUS   ROLES    AGE
node-1        Ready    <none>   14d
node-2        Ready    <none>   14d
node-3        Ready    <none>   14d
✓ We have 3 nodes for our TPS system
```

**Narration**: "You can see we have kubectl, helm, and a 3-node GKE cluster ready. All the prerequisites are in place."

### Part 2: Create Namespace (1:30-2:30)

**Narration**: "Step 1: Create an isolated namespace for TPS. This keeps our system separate from other workloads."

**On screen**: Type commands in real-time with pauses
```bash
$ kubectl create namespace tps-system
namespace/tps-system created

$ kubectl label namespace tps-system tier=system
namespace/tps-system labeled

$ kubectl get namespace tps-system
NAME         STATUS   AGE
tps-system   Active   5s
✓ Namespace created and labeled
```

**Narration**: "The namespace is created and labeled 'tier: system' so we can identify it as infrastructure. Next, we need to create persistent storage."

### Part 3: Create Storage (2:30-3:45)

**Narration**: "Step 2: Create persistent volumes for Prometheus (20GB), Grafana (5GB), and Loki (10GB). This data survives pod restarts."

**On screen**: Show file creation and apply
```bash
$ cat > /tmp/pv.yaml << 'EOF'
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: prometheus-storage
  namespace: tps-system
spec:
  accessModes:
    - ReadWriteOnce
  resources:
    requests:
      storage: 20Gi
# ... (other PVCs for grafana, loki)
EOF

$ kubectl apply -f /tmp/pv.yaml
persistentvolumeclaim/prometheus-storage created
persistentvolumeclaim/grafana-storage created
persistentvolumeclaim/loki-storage created

# Wait for PVCs to bind (takes ~15 seconds)
$ kubectl wait --for=condition=Bound pvc/prometheus-storage \
  -n tps-system --timeout=60s
condition met

$ kubectl get pvc -n tps-system
NAME                  STATUS   VOLUME                 CAPACITY
prometheus-storage    Bound    pvc-abc123...          20Gi
grafana-storage       Bound    pvc-def456...          5Gi
loki-storage          Bound    pvc-ghi789...          10Gi
✓ All storage is ready
```

**Narration**: "The storage is now provisioned. Prometheus will store metrics here, Grafana will store dashboards and configuration, and Loki will store logs."

### Part 4: Deploy TPS System (3:45-6:00)

**Narration**: "Step 3: Deploy the TPS system using Helm. We're setting 2 replicas for high availability."

**On screen**: Deploy with visual feedback
```bash
$ helm repo add tps https://charts.tps-reference.io
"tps" has been added to your repositories.

$ helm repo update
Update Complete.

$ helm install tps-release tps/tps-reference \
  --namespace tps-system \
  --set replicas=2 \
  --set prometheus.storage.size=20Gi \
  --set grafana.storage.size=5Gi \
  --set loki.storage.size=10Gi \
  --wait --timeout=5m

NAME: tps-release
LAST DEPLOYED: Thu Jan 25 14:00:00 2026
NAMESPACE: tps-system
STATUS: deployed
REVISION: 1
✓ Helm deployment started

# Monitor rollout (show real-time pod creation)
$ kubectl rollout status deployment/tps-system \
  -n tps-system --timeout=3m

Waiting for deployment "tps-system" rollout to finish:
0 of 2 replicas updated. Waiting 15 seconds.
1 of 2 replicas updated. Waiting 15 seconds.
1 of 2 replicas updated. 1 of 2 up to date. Waiting 15 seconds.
2 of 2 replicas updated. 2 of 2 up to date. Waiting 15 seconds.
deployment "tps-system" successfully rolled out

$ kubectl get pods -n tps-system
NAME                        READY   STATUS    RESTARTS
tps-system-5d8f9...         1/1     Running   0
tps-system-7b2c3...         1/1     Running   0
prometheus-0                1/1     Running   0
grafana-8f9e2...            1/1     Running   0
loki-0                      1/1     Running   0
jaeger-0                    1/1     Running   0
✓ All 6 services are running
```

**Narration**: "Great! The TPS system is now deployed. We can see 2 TPS system replicas, Prometheus for metrics, Grafana for visualization, Loki for logs, and Jaeger for distributed tracing. All running successfully."

### Part 5: Verify Health (6:00-7:00)

**Narration**: "Step 4: Verify the system is healthy by checking the API."

**On screen**: Health check
```bash
# Port-forward to API
$ kubectl port-forward -n tps-system svc/tps-api 8080:8080 &
Forwarding from 127.0.0.1:8080 -> 8080

# Test health endpoint
$ curl -s http://localhost:8080/health | jq .
{
  "status": "healthy",
  "components": {
    "circuit_breaker": "healthy",
    "queue": "healthy",
    "downstream": "healthy",
    "metrics": "healthy",
    "tracing": "healthy"
  },
  "timestamp": "2026-01-25T14:05:30Z"
}
✓ System is healthy
```

**Narration**: "Perfect! All components are healthy. The circuit breaker is ready, the queue is initialized, downstream connections are working, metrics collection is active, and tracing is enabled."

### Part 6: Access Dashboards (7:00-7:45)

**Narration**: "Step 5: Let's access the dashboards to see our system in action."

**On screen**: Open browser windows
```bash
# Grafana
$ kubectl port-forward -n tps-system svc/grafana 3000:3000 &

# Jaeger
$ kubectl port-forward -n tps-system svc/jaeger 16686:16686 &

# Open browser (simulate with screenshots)
# Grafana login page appears
# Default: admin / admin

# Dashboard loads
# Shows TPS Overview with all metrics at baseline
```

**Narration**: "We've set up three port forwards: Grafana on 3000 for dashboards, Jaeger on 16686 for traces, and the API on 8080. Let me open Grafana."

**Show browser screenshots**:
- Grafana login
- TPS Overview dashboard
- Queue depth gauge showing 0
- Circuit breaker showing "Closed"
- Processing rate showing baseline

**Narration**: "The TPS Overview dashboard is now showing. You can see the queue is empty, the circuit breaker is closed, and all metrics are at baseline. The system is ready to receive signals."

### Outro (7:45-8:00)

**Narration**: "And that's it! Your TPS system is now running in production. You have full observability with metrics, dashboards, traces, and logs. In the next video, we'll send test signals and show you how to interpret what's happening in the system. Thanks for watching!"

**On screen**: Summary slide
- ✓ Namespace created
- ✓ Storage provisioned
- ✓ System deployed (2 replicas)
- ✓ Health verified
- ✓ Dashboards accessible

---

## Video 2: "Read and Interpret Dashboards" (8 minutes)

### Intro (0:00-0:30)

**Host**: Standing with Grafana dashboard visible behind them

"In this video, I'll teach you how to read and interpret the TPS dashboards. Dashboards tell a story about your system's health. By the end, you'll know exactly what to look for and what each metric means."

### Part 1: Tour the Dashboards (0:30-1:30)

**Narration**: "There are 7 dashboards in the TPS system. Let me show you each one."

**On screen**: Click through Grafana dashboards
1. TPS Overview - High-level system health
2. Jidoka - Circuit breaker and fault isolation
3. Kanban - Queue depth and backpressure
4. Andon - Alerts and problem visibility
5. Kaizen - Performance analysis and percentiles
6. Heijunka - Worker load balancing
7. Tracing - Distributed tracing metrics

**Narration**: "The main dashboard is TPS Overview. This is where you'll spend most of your time. It shows the system's pulse: queue depth, processing latency, error rate, and circuit breaker state. Let's dive deeper."

### Part 2: TPS Overview Dashboard (1:30-4:00)

**Narration**: "The TPS Overview has 5 key sections. Let me walk you through each."

**Show dashboard and point to each section**:

**Section 1: Circuit Breaker Status (1:30-2:00)**
- Show gauge showing "Closed" in green
- **Narration**: "The circuit breaker gauge shows 0 when closed (accepting work) and 1 when open (rejecting work). If this is red (value 1), it means the downstream service is broken. You'll see rejections happening."
- Show status message: "Circuit is Closed. Downstream is healthy."

**Section 2: Queue Depth (2:00-2:30)**
- Show time-series graph with queue rising and falling
- **Narration**: "This shows how many signals are waiting in the queue. Green line means the queue is at a healthy level (40-60% full). If it spikes to the red zone (near the max of 100), you have too much incoming load and workers can't keep up."
- Highlight: "Sweet spot is here [40-60% area]. Too low (empty) means workers are idle waste. Too high (full) means latency is increasing."

**Section 3: Processing Latency (2:30-3:15)**
- Show 3 lines: P50, P90, P99 percentiles
- **Narration**: "Three percentile lines show the latency distribution. P50 (median) should be under 50ms. P90 should be under 100ms. P99 (worst-case) should be under 200ms. If P99 spikes, it means some signals are getting stuck in the queue. That's usually a sign that workers need scaling."
- Point to spike: "Here P99 jumped to 300ms. This happened because we were sending 20 signals per second but workers could only handle 5. Queue filled up, causing latency to climb."

**Section 4: Error Rate (3:15-3:45)**
- Show gauge and sparkline
- **Narration**: "Error rate should be as close to zero as possible. Normal systems have 0-1% errors (transient timeouts). If you see > 5%, something is broken. Could be bad input data, downstream service failing, or circuit breaker rejecting."
- Show annotation: "If error rate spikes, cross-reference with Circuit Breaker status. If circuit is open, errors are rejections (expected). If circuit is closed, errors are real failures (investigate)."

**Section 5: Throughput (3:45-4:00)**
- Show signals/second gauge
- **Narration**: "This shows how many signals per second the system is processing. With 2 workers, you can expect 5-15 signals per second depending on task complexity. If this drops to 0, workers have stopped. If it stays consistently at your incoming rate, you're at capacity and might need more workers."

### Part 3: Jidoka Dashboard (4:00-5:00)

**Narration**: "The Jidoka dashboard focuses on circuit breaker behavior and fault isolation."

**Show dashboard sections**:
- Circuit breaker state over time
- Health check status
- Rejection count
- Recovery time

**Narration**: "Watch the circuit breaker state. When it flips from 0 to 1, the circuit opened (downstream broke). When it flips back to 0, the circuit closed and recovered. Recovery should take 30 seconds. If it takes longer, there might be a lingering issue."

**Show example trace**: Circuit opens → System rejects for 30 seconds → Circuit closes automatically

**Narration**: "This automatic recovery is Jidoka in action. No manual intervention needed. The system detects the problem and isolates it automatically."

### Part 4: Kanban Dashboard (5:00-6:00)

**Narration**: "The Kanban dashboard shows queue dynamics."

**Show sections**:
- Queue max size (100)
- Current queue depth
- Enqueue rate (signals/sec entering queue)
- Dequeue rate (signals/sec leaving queue)
- Worker utilization

**Narration**: "When enqueue rate > dequeue rate, queue grows. When dequeue rate > enqueue rate, queue shrinks. In steady state, they should be equal. The queue depth oscillates around 40-60% of max."

**Point to graph**: "Here we enqueued 20 signals but only dequeued 5 per second. Queue grew to 95% capacity. Workers were at 100% utilization."

**Then show recovery**: "Then we stopped sending signals. Queue drained in about 60 seconds. Workers dropped to 10% utilization."

### Part 5: Summary (6:00-8:00)

**Narration**: "Here's what to watch for in the dashboards:"

**Display checklist on screen**:
1. Circuit breaker is Closed (green)
2. Queue depth is 40-60% full (yellow zone)
3. P99 latency < 200ms (or your SLA)
4. Error rate < 5%
5. Processing throughput is stable
6. Worker utilization 50-80% (not too idle, not overloaded)

**Narration**: "If any metric is outside these ranges, something needs attention. The color coding helps: green means healthy, yellow means watch it, red means action needed."

**End with practice prompt**: "In the next video, we'll show you what to do when something goes wrong. We'll inject failures and show you how to fix them."

---

## Video 3: "Incident Response: Queue Growing" (8 minutes)

### Scenario Setup (0:00-0:30)

**Host**: Standing with terminal visible

"Scenario: It's Monday 9 AM, and you just got an alert: 'Queue Depth Growing'. Queue is at 85% capacity and climbing. You have 5 minutes to prevent cascading failure. Let's run through this incident."

### Diagnosis (0:30-3:00)

**Narration**: "Step 1: Understand the problem. Is this backpressure (too much incoming load) or a processing bottleneck (workers are slow)?"

**Show terminal**:
```bash
# Check current status
$ curl -s http://localhost:8080/status | jq '.kanban'
{
  "queue_max_size": 100,
  "queue_current_depth": 85,      # 85% FULL!
  "workers": 2,                    # Only 2 workers
  "processing_rate": 5.2,          # 5 signals/sec
  "incoming_rate": 18.3            # But 18 incoming/sec!
}

# The problem is clear:
# Incoming (18/sec) > Processing (5/sec)
# With only 2 workers, we can't keep up with incoming load
```

**Narration**: "The diagnosis is clear. We're receiving 18 signals per second, but only processing 5. With 2 workers, we're at 100% utilization and still behind. The queue is filling because of backpressure: incoming load exceeds processing capacity."

**Show Grafana screenshot**: Queue rising, workers at 100% CPU

### Decision (3:00-4:00)

**Narration**: "Step 2: Decide on immediate action. Options are: (a) reduce incoming load, (b) scale workers, or (c) optimize task processing. Let's scale workers immediately since that's fastest."

**Show decision tree on screen**:
```
Too Much Load?
├─ Yes, reduce incoming → Contact load generator team (slow)
├─ Yes, add workers → Scale immediately (fast)
└─ No, optimize tasks → Profile and refactor (slowest)

We choose: Add workers (fastest)
```

### Action (4:00-7:00)

**Narration**: "Step 3: Execute the fix. Scale from 2 to 8 workers."

**Show terminal with live output**:
```bash
$ kubectl scale deployment/tps-system \
  -n tps-system --replicas=8

deployment.apps/tps-system scaled

# Watch rollout
$ kubectl rollout status deployment/tps-system \
  -n tps-system --timeout=5m

Waiting for deployment "tps-system" rollout to finish:
0 of 8 replicas updated
2 of 8 replicas updated
4 of 8 replicas updated
6 of 8 replicas updated
8 of 8 replicas updated
deployment "tps-system" successfully rolled out
```

**Narration**: "The rollout took about 90 seconds. Now we have 8 workers instead of 2. Let's watch the queue depth recover."

**Show time-lapse of Grafana dashboard**:
- T=0s: Queue at 85%
- T=30s: Queue still at 85% (new pods starting)
- T=60s: Queue at 75% (new pods ready, processing ramping up)
- T=120s: Queue at 60% (stable, new workers handling load)
- T=180s: Queue at 50% (optimal range)

**Show terminal output over time**:
```bash
# Watch queue recover
$ while true; do \
    curl -s http://localhost:8080/status | \
    jq '.kanban | {depth, incoming_rate, processing_rate}'; \
    sleep 10; \
  done

# T=0s:
{
  "depth": 85,
  "incoming_rate": 18.3,
  "processing_rate": 5.2
}

# T=60s:
{
  "depth": 75,
  "incoming_rate": 18.3,
  "processing_rate": 12.8
}

# T=120s:
{
  "depth": 60,
  "incoming_rate": 18.3,
  "processing_rate": 18.5    # Now processing > incoming!
}

# T=180s:
{
  "depth": 50,
  "incoming_rate": 18.3,
  "processing_rate": 18.1    # Queue stabilizing
}
```

**Narration**: "As new workers come online, processing rate increases. Initially, new workers are still starting up, so depth stays high. By T=60 seconds, new workers are ready. By T=120 seconds, we're processing faster than incoming (18.5/sec vs 18.3/sec). Queue shrinks back to healthy 50%."

### Verification (7:00-7:30)

**Narration**: "Step 4: Verify the alert clears."

**Show Grafana alert status**:
- Before: "Queue Depth Growing" - FIRING (red)
- After: "Queue Depth Growing" - NOT FIRING (green)

**Show metric values**:
- P99 Latency: 45ms (was 280ms)
- Error Rate: 0.2% (was 8%)
- Queue Depth: 50% (was 85%)

**Narration**: "All metrics are back to healthy. The incident is resolved. Total time: about 3 minutes from alert to resolution."

### Reflection (7:30-8:00)

**Host on screen**:

"So what did we learn? When the queue is growing, first diagnose whether it's backpressure (incoming > processing) or a processing bottleneck (workers are slow). In this case, it was backpressure, so scaling workers fixed it immediately. In the next video, we'll cover a different scenario: circuit breaker open."

---

## Video 4: "Incident Response: Circuit Breaker Open" (8 minutes)

### Scenario Setup (0:00-0:30)

**Host**:

"Scenario: Your downstream service (an external API you depend on) suddenly stops responding. Your circuit breaker opens, rejecting all requests. Signals get backed up, SLA breaches. Let's walk through the diagnosis and fix."

### Diagnosis (0:30-3:00)

**Show alert notification**: "Circuit Breaker Open"

**Terminal**:
```bash
$ curl -s http://localhost:8080/status | jq '.jidoka'
{
  "circuit_breaker": "Open",
  "last_error": "connection timeout",
  "failures_count": 47,
  "last_failure_time": "2026-01-25T14:05:23Z"
}

# Check error logs
$ kubectl logs -l app=jidoka -n tps-system --tail=20
[ERROR] Circuit breaker opened: Downstream service timeout
[ERROR] Failed to connect to downstream-api:8080
[ERROR] Retrying in 30 seconds...
```

**Narration**: "The circuit breaker opened because the downstream service is timing out. We're seeing consistent connection failures. The circuit will try to recover in 30 seconds, but we don't have time to wait. Let's investigate the downstream service."

### Investigation (3:00-5:00)

**Narration**: "Is the downstream service actually down, or just slow? Let's check."

**Terminal**:
```bash
# Test downstream directly
$ curl -v http://downstream-api:8080/health --connect-timeout 5
* Trying 1.2.3.4:8080...
* Connection timed out
* Failed to connect to port 8080: Temporary failure in name resolution

# The downstream service is unreachable!

# Check if it's running in the cluster
$ kubectl get pods -l app=downstream-service
NAME                               READY   STATUS
downstream-service-5d8f9...        0/1     CrashLoopBackOff
downstream-service-7b2c3...        0/1     CrashLoopBackOff

# Both pods are crash looping!
$ kubectl logs deployment/downstream-service --tail=20
[PANIC] Database connection failed: Connection refused
[PANIC] thread 'main' panicked at 'could not connect to database'

# Aha! Downstream can't connect to its database
```

**Narration**: "The downstream service is running but crashing because it can't connect to its database. The pods keep restarting. Let's fix the database connection."

### Resolution (5:00-7:00)

**Narration**: "The root cause is a database connection issue. Options: (a) restart the downstream service, (b) fix the database, (c) bypass downstream temporarily. Let's restart the downstream service first."

**Terminal**:
```bash
# Restart downstream service
$ kubectl rollout restart deployment/downstream-service \
  -n production

deployment.apps/downstream-service restarted

# Monitor rollout
$ kubectl rollout status deployment/downstream-service \
  -n production --timeout=5m

Waiting for deployment "downstream-service" rollout to finish:
0 of 2 replicas updated
2 of 2 replicas updated
2 of 2 up to date
deployment "downstream-service" successfully rolled out

# Verify it's responding
$ curl -s http://downstream-api:8080/health | jq .
{
  "status": "healthy",
  "database": "connected"
}

# Excellent! Downstream is recovering.
```

**Narration**: "The restart succeeded. Now let's watch the circuit breaker recover."

**Show time-lapse Grafana**:
- T=0s: Circuit Open (red), rejections happening
- T=30s: Circuit attempts recovery
- T=35s: Circuit closes (green), accepting work again
- T=120s: Queue drains, latency returns to normal

**Terminal**:
```bash
# Watch circuit breaker recover
$ while true; do \
    curl -s http://localhost:8080/status | \
    jq '.jidoka.circuit_breaker'; \
    sleep 5; \
  done

# T=0s:  "Open"
# T=30s: "Open"    (waiting for recovery period)
# T=35s: "Closed"  (recovered!)

# Verify rejections stopped
$ curl -s http://localhost:8080/metrics | grep jidoka_rejections
# Counter should stabilize (not increasing)
```

### Verification and Remediation (7:00-7:45)

**Narration**: "Circuit is closed. Requests are being accepted again. But let's make sure we prevent this from happening again."

**Show action items**:
1. Add health check to downstream database
2. Add alert for downstream service errors
3. Add timeout tuning to circuit breaker
4. Document recovery procedure

**Terminal**:
```bash
# Add alert for future occurrences
$ kubectl patch alertmanager alerts-config \
  -n monitoring --type merge -p \
  '{"spec":{"groups":[{
    "name":"downstream",
    "rules":[{
      "alert":"DownstreamServiceDown",
      "expr":"tps_circuit_breaker_state == 1",
      "for":"2m",
      "annotations":{"summary":"Downstream service is down"}
    }]
  }]}}'

alert patched

# Document incident in runbook
cat >> /docs/runbook-incident-response.md << 'EOF'
## Circuit Breaker Open (Downstream Down) - Jan 25, 2026

**Root Cause**: Downstream service database connection failed
**Detection**: Circuit breaker opened automatically
**Time to Diagnosis**: 2 minutes
**Time to Resolution**: 3 minutes (restart downstream)
**Prevention**: Added health check monitoring for downstream database

**Lessons Learned**:
- Circuit breaker provided excellent isolation (prevented cascading failure)
- Automatic recovery after 30 seconds would have resolved itself
- Manual investigation found root cause faster
EOF
```

### Conclusion (7:45-8:00)

**Host**:

"This incident took 3 minutes from circuit opening to full resolution. The key lessons: (1) circuit breaker automatically isolated the problem, preventing cascading failure, (2) we diagnosed the root cause (database connection) quickly using logs, (3) restarting the service fixed the issue. In a real scenario, you'd also check if the database itself needs attention. Next video: latency spikes."

---

## Video 5: "Performance Tuning: Find and Fix Bottlenecks" (8 minutes)

### Intro (0:00-0:30)

**Host**:

"P99 latency used to be 50ms. Now it's consistently 150ms. Users are complaining about slowness. Where is the time being spent? Let's use tracing to find the bottleneck."

### Data Collection (0:30-3:00)

**Narration**: "Step 1: Capture slow traces. In Jaeger, we'll look at the slowest signal."

**Show Jaeger UI**:
- Service: tps-system
- Operation: ProcessSignal
- Sort by: Duration (descending)
- Min duration: 100ms (filter to slow traces)
- Results show 200+ slow traces (big problem!)

**Click on slowest trace** (153ms):
```
ProcessSignal (153ms)
├── ReceiveSignal (2ms)
├── Kanban.Enqueue (3ms)
├── Worker.Dequeue + Wait (52ms) ← Wait is high!
├── ProcessWork (70ms) ← Process time is high!
│   ├── ValidateSignal (1ms)
│   ├── ExecuteTask (65ms) ← Task is slow!
│   └── RecordResult (1ms)
├── Jidoka.Check (2ms)
├── Kaizen.RecordMetric (1ms)
├── Heijunka.UpdateLoad (1ms)
└── ReturnResponse (1ms)
```

**Narration**: "The breakdown shows: 52ms in queue wait (workers are busy) + 70ms in ProcessWork (task is slow). Two potential bottlenecks. Let me look at the ProcessWork details."

**Expand ProcessWork span**:
```
ProcessWork (70ms)
├── ValidateSignal (1ms)
├── ExecuteTask (65ms)
│   ├── FetchFromCache (2ms)
│   │   └── CacheMiss
│   ├── FetchFromDatabase (60ms) ← HERE!
│   └── ParseResult (1ms)
└── RecordResult (1ms)
```

**Narration**: "Found it! FetchFromDatabase is taking 60ms out of 70ms. Database query is the bottleneck. Let's look at the database metrics to confirm."

### Analysis (3:00-5:00)

**Narration**: "Step 2: Confirm the bottleneck. Let's check database query logs."

**Show Prometheus**:
```bash
# Query: avg(pg_query_time_seconds) by (query)
# Top slow queries:

SELECT sku_info FROM products WHERE id = $1
  - Avg: 58ms
  - p99: 120ms
  - Calls: 1200/min

SELECT customer_preferences FROM customers WHERE id = $1
  - Avg: 5ms
  - p99: 15ms
  - Calls: 900/min
```

**Narration**: "The 'SELECT sku_info FROM products WHERE id' query is taking 58ms on average. That's the problem. Two options: (1) add an index, or (2) implement caching. Caching would be faster."

**Show query plan** (EXPLAIN output):
```
Seq Scan on products
  Filter: id = $1
  Rows: 1
  -> Cost: 58ms
```

**Narration**: "It's doing a sequential scan (slow). It should do an index lookup (fast). Let's add an index."

### Fix (5:00-7:00)

**Narration**: "Step 3: Implement the fix. Add index to products table."

**Show terminal**:
```bash
# Login to database
$ kubectl exec -it postgres-pod -n production -- \
  psql -U admin -d tpsdb -c \
  "CREATE INDEX idx_products_id ON products(id);"

CREATE INDEX

# Verify index was created
$ kubectl exec -it postgres-pod -n production -- \
  psql -U admin -d tpsdb -c \
  "SELECT * FROM pg_indexes WHERE tablename='products';"

 schemaname | tablename |    indexname     | tablespace
 public     | products  | idx_products_id  | (null)
```

**Narration**: "Index created. Now the query should use index lookup instead of sequential scan. Let's test the latency immediately."

**Show before/after Jaeger traces**:

**Before**: ExecuteTask (70ms → FetchFromDatabase 60ms)
**After**: ExecuteTask (8ms → FetchFromDatabase 3ms)

**Narration**: "Wow! ExecuteTask dropped from 70ms to 8ms. That's a 9x improvement just by adding one index. Now let's see the system-wide latency impact."

**Show Grafana time-series** (before → after index):
- P99 latency: 150ms → 45ms
- Processing rate: 8 sig/sec → 25 sig/sec
- Queue depth: 60% → 20%

**Narration**: "System-wide latency dropped from 150ms to 45ms. Throughput tripled from 8 to 25 signals per second. Queue depth went from 60% to 20% (workers have spare capacity now)."

### Reflection (7:00-8:00)

**Host on screen**:

"Here's the process we followed: (1) identified slowness through user reports, (2) captured slow traces in Jaeger, (3) analyzed trace breakdown to find hot span (FetchFromDatabase), (4) confirmed hypothesis with database metrics, (5) implemented fix (index), (6) verified improvement in traces and dashboards.

One index. Nine-fold latency improvement. This is what performance optimization really looks like: data-driven investigation, hypothesis testing, measured improvement.

In the next video, we'll cover configuration tuning. Thanks for watching!"

---

## Scripts Summary

| Video | Topic | Duration | Audience |
|-------|-------|----------|----------|
| 1 | Deploy to Production | 8 min | New operators |
| 2 | Read Dashboards | 8 min | New operators |
| 3 | Queue Growing Incident | 8 min | On-call engineers |
| 4 | Circuit Breaker Open | 8 min | On-call engineers |
| 5 | Performance Tuning | 8 min | DevOps engineers |

---

**Version**: 1.0
**Last Updated**: January 2026
**Total Video Runtime**: 40 minutes (all 5 videos)
**Suggested Playlist Order**: 1 → 2 → 3 → 4 → 5
