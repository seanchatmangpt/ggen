# Operations Runbook

Operational procedures and troubleshooting guide for TAI Erlang Autonomics.

---

## Quick Reference

| Operation | Command | Time |
|-----------|---------|------|
| Start service | `_build/default/rel/tai_autonomics/bin/tai_autonomics start` | 2-5s |
| Stop service | `_build/default/rel/tai_autonomics/bin/tai_autonomics stop` | 2-5s |
| Health check | `curl http://localhost:8080/health` | <100ms |
| View logs | `tail -f _build/default/rel/tai_autonomics/log/erlang.log.1` | instant |
| Restart | `make build && _build/default/rel/tai_autonomics/bin/tai_autonomics restart` | 10-15s |
| Deploy | `make gcp-deploy` | 3-5min |

---

## Startup Procedures

### Local Development

```bash
# 1. Build release
rebar3 compile
rebar3 release

# 2. Set environment
export PORT=8080
export RECEIPT_LEDGER_BACKEND=ets
export LOG_LEVEL=debug

# 3. Start service
_build/default/rel/tai_autonomics/bin/tai_autonomics start

# 4. Verify startup
curl http://localhost:8080/health
# Expected: {"status": "ok"}
```

### Docker Compose (Local)

```bash
# 1. Build images
docker-compose build

# 2. Start services (includes Pub/Sub emulator, Firestore emulator)
docker-compose up -d

# 3. Wait for startup (30s)
sleep 30

# 4. Verify health
curl http://localhost:8080/health
docker-compose logs tai-autonomics
```

### GCP Cloud Run

```bash
# 1. Deploy via Terraform
export GCP_PROJECT_ID=my-project
export GCP_REGION=us-central1
make terraform-apply

# 2. Get service URL
SERVICE_URL=$(cd terraform && terraform output -raw cloud_run_service_url)
echo $SERVICE_URL

# 3. Wait for readiness (1-2 min)
sleep 60

# 4. Verify health
curl $SERVICE_URL/health
```

### Startup Validation Checklist

After startup, verify:

- [ ] HTTP server responding: `curl http://localhost:8080/health` returns 200
- [ ] Supervisors running: `erlang:whereis(governance_sup) =/= undefined`
- [ ] Firestore connected: Log shows "Firestore connection established"
- [ ] Pub/Sub subscription ready: Log shows "Pub/Sub subscription active"
- [ ] Metrics exported: `/metrics` endpoint responds
- [ ] No ERROR logs: `grep ERROR logs/` returns empty

---

## Health Checks

### Manual Health Check

```bash
curl -v http://localhost:8080/health
```

Expected response:
```json
{
  "status": "ok",
  "timestamp": 1704067200,
  "dependencies": {
    "supervisors": "ok",
    "firestore": "ok",
    "pubsub": "ok"
  }
}
```

### Cloud Run Health Checks

Cloud Run automatically configures:

```
Startup Probe:
  - Timeout: 240 seconds
  - Check: GET /health

Liveness Probe:
  - Timeout: 10 seconds
  - Period: 60 seconds
  - Check: GET /health

Readiness Probe:
  - Timeout: 10 seconds
  - Period: 10 seconds
  - Check: GET /health
```

### Kubernetes Health Checks

```yaml
livenessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3

readinessProbe:
  httpGet:
    path: /health
    port: 8080
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3
```

---

## Common Troubleshooting

### Issue: Health Check Fails (503)

**Symptoms**:
```
curl http://localhost:8080/health
{"status": "unavailable", "reason": "supervisor governance_sup not running"}
```

**Diagnosis**:
```erlang
% SSH/Console into service
rebar3 shell

% Check supervisors
erlang:whereis(governance_sup).
erlang:whereis(receipt_ledger_sup).

% Check all processes
supervisor:which_children(governance_sup).

% View recent errors
observer:start().
```

**Solutions**:

1. Check logs for errors:
```bash
tail -100f logs/erlang.log.1 | grep -i error
```

2. Verify configuration:
```bash
cat config/sys.config | grep -A 20 tai_autonomics
```

3. Restart supervisors:
```erlang
supervisor:terminate_child(tai_autonomics_sup, governance_sup),
supervisor:restart_child(tai_autonomics_sup, governance_sup).
```

4. Full restart:
```bash
_build/default/rel/tai_autonomics/bin/tai_autonomics stop
sleep 2
_build/default/rel/tai_autonomics/bin/tai_autonomics start
```

### Issue: Receipts Not Being Written

**Symptoms**:
- Requests succeed but receipts don't appear in Firestore
- Logs show "Receipt emitted" but Firestore collection is empty

**Diagnosis**:

```bash
# Check Firestore configuration
env | grep -i firestore

# Verify GCP credentials
gcloud auth list
gcloud config get-value project

# Check Firestore collection
gcloud firestore documents list --collection-id=receipts --limit=5
```

**Solutions**:

1. Enable Firestore writes:
```bash
export FIRESTORE_ENABLED=true
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

2. Check GCP credentials:
```bash
gcloud auth application-default login
export GOOGLE_APPLICATION_CREDENTIALS=~/.config/gcloud/application_default_credentials.json
```

3. Verify IAM permissions:
```bash
gcloud projects get-iam-policy PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:tai-autonomics-sa@*"
```

4. Check network connectivity:
```bash
# From Cloud Run container
curl https://firestore.googleapis.com/v1/projects/PROJECT_ID/databases
```

### Issue: High Latency (>1s per request)

**Symptoms**:
- Requests taking longer than expected
- Slow response times even during low traffic

**Diagnosis**:

```bash
# Check request latencies
curl -w "Time: %{time_total}s\n" http://localhost:8080/pubsub

# Monitor system resources
top -l 1 | head -20
df -h
```

**Solutions**:

1. Increase action pool size:
```bash
export ACTION_POOL_SIZE=20
export ACTION_QUEUE_SIZE=5000
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

2. Reduce flush interval (for lower latency):
```bash
export RECEIPT_FLUSH_INTERVAL_MS=100
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

3. Scale horizontally (Cloud Run):
```bash
gcloud run deploy tai-autonomics \
  --min-instances=2 \
  --max-instances=100
```

4. Monitor Firestore latency:
```bash
gcloud logging read "resource.type=cloud_firestore" \
  --limit=10 --format=json | jq '.[] | select(.severity=="ERROR")'
```

### Issue: Out of Memory

**Symptoms**:
- OOM killer invoked
- Cloud Run crashing with exit code 137
- Memory usage growing over time

**Diagnosis**:

```erlang
% Check memory usage
erlang:memory().

% Check process memory
erlang:statistics(memory).

% Find large processes
lists:sort(fun({_,A},{_,B}) -> A > B end,
  [(Pid, erlang:process_info(Pid, memory)) || Pid <- erlang:processes()]).
```

**Solutions**:

1. Reduce receipt buffer size:
```bash
export RECEIPT_BUFFER_SIZE=1000
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

2. Increase flush frequency:
```bash
export RECEIPT_FLUSH_INTERVAL_MS=100
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

3. Use Firestore backend instead of ETS:
```bash
export RECEIPT_LEDGER_BACKEND=firestore
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

4. Increase Cloud Run memory:
```bash
gcloud run deploy tai-autonomics --memory=4Gi
```

5. Enable garbage collection:
```bash
export ERL_MAX_ETS_TABLES=1000
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

### Issue: Pub/Sub Messages Not Processing

**Symptoms**:
- Pub/Sub subscription has messages but none are being processed
- No logs indicating message receipt

**Diagnosis**:

```bash
# Check subscription status
gcloud pubsub subscriptions describe erlang-autonomics-signals-sub

# Check message age
gcloud pubsub subscriptions pull erlang-autonomics-signals-sub --limit=1

# Check dead letter queue
gcloud pubsub topics list | grep dlq
gcloud pubsub subscriptions list | grep dlq
```

**Solutions**:

1. Verify subscription configuration:
```bash
export PUBSUB_SUBSCRIPTION=erlang-autonomics-signals-sub
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

2. Check authentication:
```bash
gcloud auth application-default login
```

3. Restart subscription handler:
```erlang
gcp_pubsub:stop_subscription(),
gcp_pubsub:start_subscription().
```

4. Process dead letter queue:
```bash
gcloud pubsub subscriptions pull erlang-autonomics-dlq --auto-ack --limit=100
```

### Issue: Hash Chain Verification Fails

**Symptoms**:
- Logs show "Hash verification failed"
- Receipt chain broken error

**Diagnosis**:

```bash
# Get recent receipts
gcloud firestore documents list --collection-id=receipts --limit=10

# Verify hash manually
curl http://localhost:8080/receipts/receipt-id
```

**Solutions**:

1. Disable hash verification temporarily:
```bash
export RECEIPT_VERIFY_HASHES=false
_build/default/rel/tai_autonomics/bin/tai_autonomics restart
```

2. Check JSON encoding:
```erlang
% Verify receipt encoding is consistent
jiffy:encode(Receipt#{hash => undefined, chain_hash => undefined}).
```

3. Rebuild receipt chain from Firestore:
```erlang
tai_receipts:rebuild_chain().
```

---

## Monitoring and Observability

### Viewing Logs

```bash
# Real-time logs
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1

# Filter by level
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1 | grep ERROR

# Search logs
grep "tenant-123" _build/default/rel/tai_autonomics/log/erlang.log.1
```

### Cloud Logging (GCP)

```bash
# View all logs
gcloud logging read "resource.type=cloud_run_revision" \
  --limit=50 --format=json

# Filter by severity
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" \
  --limit=50

# View metrics
gcloud monitoring timeseries list --filter='metric.type="custom.googleapis.com/tai_autonomics/*"'
```

### Prometheus Metrics

```bash
# Export metrics
curl http://localhost:8080/metrics

# Key metrics
curl http://localhost:8080/metrics | grep -E "tai_http_requests|tai_governor|tai_receipt"
```

### Distributed Tracing

```bash
# View traces in GCP Cloud Trace
gcloud trace list

# Export trace
gcloud trace export --limit=10 --format=json
```

---

## Graceful Shutdown

### Local

```bash
# Send SIGTERM
_build/default/rel/tai_autonomics/bin/tai_autonomics stop

# Monitor shutdown
tail -f _build/default/rel/tai_autonomics/log/erlang.log.1
# Look for "Shutdown complete" message
```

### Cloud Run (Automatic)

When redeploying or scaling down, Cloud Run:
1. Sends SIGTERM to all instances
2. Waits up to 30 seconds for graceful shutdown
3. Force kills if still running

### Graceful Shutdown Steps

The application will:

1. Stop accepting new HTTP requests
2. Complete in-flight Pub/Sub messages
3. Flush receipt buffer to storage
4. Close Firestore connection
5. Shutdown supervisor hierarchy
6. Exit cleanly

---

## Capacity Planning

### Resource Requests

| Environment | CPU | Memory | Disk |
|------------|-----|--------|------|
| Development | 0.5 CPU | 512 MB | 10 GB |
| Staging | 2 CPU | 2 GB | 50 GB |
| Production | 4 CPU | 4 GB | 100 GB |

### Scalability

**Vertical Scaling** (per instance):
- CPU: 0.25 - 4 vCPU
- Memory: 512 MB - 8 GB

**Horizontal Scaling** (Cloud Run):
- Min instances: 0 (development) to 2 (production)
- Max instances: 10 (staging) to 100+ (production)

### Performance Targets

- Latency: P95 < 100ms, P99 < 500ms
- Throughput: 100+ requests/second per instance
- Availability: 99.99% uptime SLA

---

## Backup and Recovery

### Receipt Ledger Backup

**ETS (In-Memory)**:
```erlang
% Save to file
ets:tab2file(receipt_ledger, "receipt_ledger.ets").

% Restore from file
ets:file2tab("receipt_ledger.ets").
```

**Firestore (GCP)**:
```bash
# Automated backups (configured in Terraform)
gcloud firestore backups list

# Manual export to BigQuery
bq mk --dataset backup_dataset
gcloud firestore export gs://my-bucket/backup --database my-db
```

### Disaster Recovery

**Recovery Procedure**:

1. Restore Firestore backup:
```bash
gcloud firestore restore BACKUP_ID
```

2. Restart application:
```bash
gcloud run deploy tai-autonomics \
  --image REGISTRY/tai-autonomics:latest
```

3. Verify receipts:
```bash
curl https://SERVICE_URL/health
gcloud firestore documents list --collection-id=receipts --limit=5
```

---

## Performance Optimization

### High Throughput Tuning

For 1000+ requests/second:

```bash
# Increase concurrency
export MAX_CONCURRENT_REQUESTS=5000
export ACTION_POOL_SIZE=50
export ACTION_QUEUE_SIZE=10000

# Optimize flush
export RECEIPT_FLUSH_INTERVAL_MS=100

# Scale horizontally
gcloud run deploy tai-autonomics --max-instances=100

# Increase Cloud Run CPU
gcloud run deploy tai-autonomics --cpu=4
```

### Low Latency Tuning

For P95 < 50ms:

```bash
# Pre-warm instances
gcloud run deploy tai-autonomics --min-instances=5

# Use ETS backend (faster than Firestore)
export RECEIPT_LEDGER_BACKEND=ets

# Reduce tracing overhead
export TRACING_SAMPLE_RATE=0.01

# Optimize timeout
export RECEIPT_FLUSH_INTERVAL_MS=10
```

---

## Maintenance Windows

### Planned Maintenance

```bash
# 1. Announce maintenance window (15 min before)
echo "Maintenance window: 2024-01-15 02:00-02:30 UTC"

# 2. Blue-green deployment
gcloud run deploy tai-autonomics-blue --image NEW_IMAGE
gcloud run services update-traffic tai-autonomics --to-revisions BLUE=100

# 3. Verify new version
curl https://SERVICE_URL/health

# 4. Monitor error rates (5 min)
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR"

# 5. Route back to old version if needed
gcloud run services update-traffic tai-autonomics --to-revisions GREEN=100

# 6. Announce completion
echo "Maintenance complete"
```

### Zero-Downtime Deployment

Cloud Run supports zero-downtime deployment:

1. New revision is deployed alongside old
2. Traffic gradually shifted to new revision
3. Old revision kept running for rollback
4. Health checks verify new revision ready

---

## On-Call Procedures

### Escalation Path

1. **Automated Alerts** (5 min response)
   - Health check failures
   - Error rate > 1%
   - Latency P99 > 1s

2. **On-Call Engineer** (15 min response)
   - Check dashboards
   - Review logs
   - Execute runbook

3. **Incident Commander** (30 min response)
   - Coordinate resolution
   - Communicate status
   - Declare incident

### Quick Diagnostic Commands

```bash
# Status check
curl https://SERVICE_URL/health

# Recent errors
gcloud logging read "severity=ERROR" --limit=20

# Performance metrics
gcloud monitoring timeseries list \
  --filter='metric.type="run.googleapis.com/request_latencies"' \
  --limit=10

# Check service status
gcloud run services describe tai-autonomics
```

---

## Contact and Escalation

| Issue | Contact | Response |
|-------|---------|----------|
| Deployment | DevOps Team | 15 min |
| Performance | SRE Team | 30 min |
| Data/Security | Security Team | 1 hour |
| Production Incident | Incident Commander | Immediate |

---

## References

- Configuration: CONFIG.md
- API Endpoints: ENDPOINTS.md
- Receipt Schema: RECEIPTS.md
- GCP Deployment: GCP_DEPLOYMENT.md
- Cloud Run Documentation: https://cloud.google.com/run/docs
- Erlang Logger: https://erlang.org/doc/apps/kernel/logger.html
