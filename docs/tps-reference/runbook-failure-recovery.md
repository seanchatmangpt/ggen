# Failure Recovery Runbook for TPS Systems

**Version**: 1.0
**Last Updated**: January 2026
**Audience**: On-call engineers, ops team
**Recovery Time Targets**: < 5 minutes for most failures

---

## Four Types of Failures

### Type 1: Jidoka Failure (Worker Unhealthy)

**Definition**: Individual worker or worker pool becomes unhealthy, can't process items

**Symptoms**:
- Error rate for specific worker: 100%
- Worker queue growing while others drain
- Worker logs show errors (crashes, exceptions)
- Worker health check failing

---

#### Detection

**Automated**:
- Health check alert: "Worker X failing health checks"
- Queue imbalance alert: "Worker X queue 50% full while others empty"
- Error rate alert: "Worker X error rate 100%"

**Manual**:
- Look at worker pool list: Which worker is red (unhealthy)?
- Check worker logs: See error messages
- Check worker metrics: Is that worker processing items?

---

#### Recovery Procedure

**Step 1: Remove Unhealthy Worker** (1 minute)
```bash
# Kubernetes:
kubectl delete pod api-worker-xyz

# Docker:
docker kill api-worker-xyz

# Manual:
systemctl stop api-worker-xyz
```

**Step 2: Verify Removal** (1 minute)
- Worker should stop accepting requests
- Requests should be sent to other workers
- Queue for that worker should drain (no new items added)

**Step 3: Add Replacement Worker** (1-2 minutes)
```bash
# Auto-scaling should kick in automatically
# If not, manually add:

# Kubernetes:
kubectl scale deployment api-worker --replicas=10

# Docker:
docker run -d [image] api-worker

# Manual:
systemctl start api-worker-xyz
```

**Step 4: Verify Recovery** (2 minutes)
- New worker should be healthy
- Health check passing?
- Processing items?
- No errors in logs?
- Original queue load rebalanced to remaining workers?

---

#### Prevention

**For Next Time**:
- Check worker logs: Why did it fail? (OOM? Bug? Network?)
- Fix the root cause
- Add monitoring for this metric
- Test failure scenario in staging

---

### Type 2: Kanban Failure (Queue Backend Down)

**Definition**: Queue service (NATS, Kafka, RabbitMQ) is down, can't store/retrieve work

**Symptoms**:
- All workers unable to connect to queue
- All error messages mention queue connection
- Queue metrics: 0 depth (queue not accessible)
- Incoming requests: Being rejected (can't queue them)

---

#### Detection

**Automated**:
- Queue connection alert: "Cannot connect to queue"
- Queue availability alert: "Queue unavailable for X seconds"
- Worker error rate spike: "Error rate 100%, all workers failing"

**Manual**:
- Can you connect to queue from your machine?
- ```bash
  # Test NATS connectivity
  telnet nats-server 4222

  # Test Kafka connectivity
  nc -zv kafka-broker 9092

  # Test RabbitMQ connectivity
  amqp-connection-test rabbitmq-host
  ```

---

#### Recovery Procedure

**Step 1: Assess Queue Health** (1-2 minutes)
```bash
# Check if queue service is running
systemctl status nats-server
ps aux | grep kafka
docker ps | grep rabbitmq

# Check queue service logs
journalctl -u nats-server -n 50
docker logs nats-server

# Check if queue storage is healthy
# - Disk not full?
# - Permissions ok?
# - No corruption?
```

**Step 2: Recover Queue Service** (1-3 minutes)

**Option A: Restart Queue Service**
```bash
# If just hung, restart usually fixes it
systemctl restart nats-server
# or
docker restart nats-server

# Wait for service to be ready (usually 30 seconds)
sleep 30
```

**Option B: Restore from Backup**
```bash
# If queue data corrupted
1. Stop queue service
2. Restore queue data from backup
   - Find latest backup
   - Copy to queue directory
3. Restart queue service
4. Verify queue is readable
```

**Step 3: Verify Queue Recovery** (1-2 minutes)
```bash
# Can workers connect to queue?
kubectl logs -l app=api-worker --tail=10 | grep "connected to queue"

# Is queue responding?
# Check queue metrics:
- Queue depth should be visible
- Workers should start pulling items
- Error rate should drop to 0%
```

**Step 4: Replay Items (if needed)** (5-10 minutes)
```bash
# If queue was not persisted and items were lost
1. Identify lost items (check logs, timestamps)
2. Replay from source (API requests, event log)
3. Verify all items are being processed
```

---

#### Prevention

**For Next Time**:
- Add queue persistence (don't run in-memory only)
- Add queue monitoring (disk space, message rate)
- Add queue redundancy (cluster of queue brokers)
- Regular queue backup (daily)
- Queue recovery drill (monthly test of recovery procedure)

---

### Type 3: Andon Failure (Can't See System)

**Definition**: Monitoring/logging/tracing is down, can't see what's happening

**Symptoms**:
- Dashboard is blank
- Metrics are missing
- Traces are not appearing
- Logs are not being written
- Alerts not firing (even for real problems)

---

#### Detection

**Obvious Indicators**:
- Grafana dashboard: Blank or "no data"
- Prometheus: Can't scrape metrics
- Jaeger: No new traces appearing
- Kibana: No logs appearing

---

#### Recovery Procedure

**Step 1: Identify Which Monitoring is Down** (1 minute)
```bash
# Check each monitoring component
1. Prometheus:
   - kubectl get pod prometheus-0 (is it running?)
   - Check prometheus logs

2. Grafana:
   - Can you access web UI?
   - Check Grafana logs

3. Jaeger:
   - Can you access Jaeger web UI?
   - Check collector logs

4. Logging (ELK/Splunk/etc):
   - Can you search logs?
   - Check logging backend
```

**Step 2: Fix Monitoring** (2-5 minutes)

**If Prometheus down**:
```bash
# Restart Prometheus
kubectl restart statefulset prometheus
# or
docker restart prometheus

# Wait for it to start scraping (30 seconds)
# Check: Metrics should start appearing
```

**If Grafana down**:
```bash
# Restart Grafana
kubectl restart deployment grafana
# or
docker restart grafana

# Wait for it to be ready (1-2 minutes)
# Check: Can you access web UI?
```

**If Jaeger down**:
```bash
# Restart Jaeger
kubectl restart deployment jaeger
# or
docker restart jaeger

# Wait for it to start receiving traces
# Check: New traces appearing?
```

**If Logging down**:
```bash
# Restart logging stack
kubectl restart deployment elasticsearch fluent-bit kibana
# or
docker restart elasticsearch fluent-bit kibana

# Wait for full startup (3-5 minutes)
# Check: Can you search logs?
```

**Step 3: Verify Monitoring Works** (2 minutes)
```bash
# Send test signal
1. Generate a test request to system
2. Check if trace appears in Jaeger (30 second latency)
3. Check if metrics appear in Prometheus (10-30 second latency)
4. Check if logs appear (1-2 minute latency)
```

---

#### Prevention

**For Next Time**:
- Run monitoring redundantly (2+ instances)
- Use managed monitoring service (they handle reliability)
- Monitor the monitoring (alert if Prometheus down)
- Backup dashboards + alerts (don't lose configuration)
- Regular drill: Turn off monitoring, practice recovery

---

### Type 4: Heijunka Failure (Can't Scale)

**Definition**: Auto-scaling is broken, can't add workers when load increases

**Symptoms**:
- Load increases but worker count stays same
- Workers getting overloaded (100% utilization)
- Queue filling up
- Latency spiking
- Auto-scaler logs show errors

---

#### Detection

**Automated**:
- Alert: "Auto-scaler not scaling despite high load"
- Alert: "Worker count hasn't changed in 10 minutes despite 100% utilization"

**Manual**:
- Check worker count: `kubectl get deployment api-worker`
- Check auto-scaler status: `kubectl describe hpa api-worker`
- Check worker utilization: > 85% and not scaling? Problem!

---

#### Recovery Procedure

**Step 1: Diagnose Auto-Scaler** (1-2 minutes)
```bash
# Check auto-scaler status
kubectl describe hpa api-worker
# Look for: "Error scaling up", "Failed to compute desired replicas"

# Check metrics available to auto-scaler
kubectl get metrics nodes
kubectl get metrics pods

# Check resource requests/limits
kubectl describe deployment api-worker
# Look for: "resources.requests.cpu", "resources.limits.memory"
```

**Step 2: Manual Scaling (immediate, while diagnosing)** (1 minute)
```bash
# Manually add workers while investigating auto-scaler
kubectl scale deployment api-worker --replicas=30
# If 20 workers were there, scale to 30 (50% more)

# Verify new workers are healthy
kubectl get pods -l app=api-worker --watch
```

**Step 3: Fix Auto-Scaler** (2-5 minutes)

**Option A: Restart Auto-Scaler**
```bash
# Auto-scaler might just be stuck
kubectl rollout restart deployment metrics-server
kubectl rollout restart deployment horizontal-pod-autoscaler

# Wait 1 minute for it to be ready
# Check: Is it scaling now?
```

**Option B: Fix Metrics**
```bash
# Auto-scaler needs metrics to work
# If metrics missing, can't scale

# Check if metrics exist:
kubectl get hpa api-worker
# Look for: Average CPU, Average Memory

# If metrics missing, fix metrics collection:
kubectl restart deployment metrics-server
```

**Option C: Adjust Scaling Thresholds**
```bash
# If auto-scaler is broken or not scaling aggressively enough
# Adjust scale-up trigger

kubectl edit hpa api-worker
# Change:
# - targetCPUUtilizationPercentage: 70 (too high)
# + targetCPUUtilizationPercentage: 50 (more aggressive)

# Apply change
# Auto-scaler should immediately scale up
```

**Step 4: Verify Scaling Works** (2 minutes)
```bash
# Generate load to test auto-scaling
# Workers should automatically scale up

# Check: Worker count increasing? Y/N
# Check: Workers healthy? Y/N
# Check: Utilization dropping back down? Y/N
```

---

#### Prevention

**For Next Time**:
- Monitor auto-scaler health (alert if not scaling)
- Set scaling limits (min/max replicas to prevent runaway scaling)
- Test auto-scaling regularly (monthly drill)
- Use multiple scaling policies (CPU, memory, custom metrics)
- Have manual scaling as backup (when auto-scaling fails)

---

## Recovery Checklists

### Jidoka Failure (Worker Down)

- [ ] Identify unhealthy worker
- [ ] Remove unhealthy worker (kill pod/process)
- [ ] Verify removal (queue stops growing for that worker)
- [ ] Add replacement worker (auto-scaling or manual)
- [ ] Verify replacement healthy (health checks passing)
- [ ] Check logs: Why did original worker fail?
- [ ] Fix root cause
- [ ] Test fix in staging

**Recovery Time**: 3-5 minutes
**Data Loss**: No (items reprocessed)
**Severity**: Medium (affects only that worker's items)

---

### Kanban Failure (Queue Down)

- [ ] Verify queue service is down (try to connect)
- [ ] Check queue logs (why is it down?)
- [ ] Restart queue service (if just hung)
- [ ] OR restore from backup (if corrupted)
- [ ] Verify queue is responsive (metrics flowing)
- [ ] Check for lost items (are all items there?)
- [ ] Replay lost items if needed
- [ ] Investigate root cause

**Recovery Time**: 2-10 minutes (depending on recovery method)
**Data Loss**: Possible (if no persistence)
**Severity**: Critical (entire system blocked)

---

### Andon Failure (Monitoring Down)

- [ ] Identify which monitoring component is down
- [ ] Restart component (Prometheus, Grafana, Jaeger, etc.)
- [ ] Verify monitoring is working (send test signal)
- [ ] Check recent alerts: What did you miss?
- [ ] Investigate root cause

**Recovery Time**: 2-5 minutes
**Data Loss**: No (monitoring data might be lost during outage)
**Severity**: High (can't see problems, but system still running)

---

### Heijunka Failure (Can't Scale)

- [ ] Identify auto-scaler is not scaling (check metrics)
- [ ] Manually scale workers (immediate relief)
- [ ] Diagnose auto-scaler (restart if hung, fix if broken)
- [ ] Verify auto-scaler works again (test scaling)
- [ ] Investigate root cause

**Recovery Time**: 3-5 minutes
**Data Loss**: No
**Severity**: High (overload if sustained)

---

## Multi-Failure Scenario

**What if multiple things fail at once?**

Example: Queue down + monitoring down + auto-scaler down

**Recovery Priority**:
1. **Restore visibility** (monitoring/logging first)
   - Even if you can't fix everything, you need to see what's happening
2. **Stabilize system** (auto-scale or manual scaling)
   - Prevent cascading failure
3. **Fix critical component** (queue, if down)
   - System might be stuck without queue
4. **Investigate all components** (after system is stable)
   - Why did multiple things fail? Related cause?

**Timeline**:
```
T=0: Multiple alerts, chaos
T=1: Restart monitoring (1-2 minutes)
T=3: Restart auto-scaler (1 minute)
T=4: Manually scale workers (1 minute)
T=5: Restart queue if needed (1-2 minutes)
T=7: System stable, investigate root cause
```

---

## Post-Failure Checklist

After recovery is complete:
- [ ] System is stable (all metrics green)
- [ ] No ongoing alerts
- [ ] Data integrity verified (no data lost)
- [ ] Users can access system again
- [ ] Document what happened
- [ ] Schedule post-incident review (24 hours later)

---

**End of Failure Recovery Runbook**
