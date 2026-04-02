# TAI Erlang Autonomics - Operational Guide

**Status**: Phase 1 (Eval-only)
**Last Updated**: 2026-01-26
**Audience**: DevOps engineers, SREs, operations team

---

## 1. Deployment Checklist

### Pre-Deployment Verification

Before deploying to production, verify:

#### Infrastructure
- [ ] GCP project created and billing enabled
- [ ] Cloud Run enabled in GCP project
- [ ] Firestore database provisioned (native mode)
- [ ] Pub/Sub topic and subscription created
- [ ] Service account with proper IAM roles
- [ ] VPC and networking configured
- [ ] SSL/TLS certificates provisioned

#### Application
- [ ] All tests passing: `rebar3 ct`
- [ ] Code formatted: `rebar3 format`
- [ ] No compiler warnings: `rebar3 compile`
- [ ] Docker image builds cleanly
- [ ] Smoke test passes locally: `./tools/smoke.sh`
- [ ] Environment variables documented
- [ ] Configuration reviewed and approved

#### Monitoring
- [ ] Cloud Monitoring dashboards created
- [ ] Alert policies configured
- [ ] Log aggregation enabled
- [ ] Trace collection enabled
- [ ] Error budget calculated

### Deployment Steps

1. **Build & Test** (15 min)
   ```bash
   # Compile and run tests
   rebar3 compile
   rebar3 ct

   # Build Docker image
   docker build -f container/Containerfile \
     -t gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION} .

   # Push to registry
   docker push gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION}
   ```

2. **Canary Deploy** (30 min)
   ```bash
   # Deploy to canary Cloud Run service (1 instance)
   gcloud run deploy tai-autonomics-canary \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION} \
     --region=${REGION} \
     --no-allow-unauthenticated

   # Run smoke tests
   ./tools/smoke.sh canary

   # Monitor for 15 minutes
   # Check: error rate < 0.1%, latency p99 < 500ms
   ```

3. **Production Deploy** (15 min)
   ```bash
   # Deploy to production Cloud Run service (3+ instances)
   gcloud run deploy tai-autonomics \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION} \
     --region=${REGION} \
     --no-allow-unauthenticated \
     --min-instances=3

   # Gradual traffic shift (optional)
   # Shift 10% → 25% → 50% → 100% over 1 hour
   ```

4. **Post-Deployment** (10 min)
   ```bash
   # Verify deployment
   gcloud run services describe tai-autonomics --region=${REGION}

   # Check logs
   gcloud logging read \
     "resource.type=cloud_run_revision AND resource.labels.service_name=tai-autonomics" \
     --limit 100 \
     --format=json | grep error

   # Verify metrics
   gcloud monitoring metrics-descriptors list --filter="tai_autonomics"
   ```

---

## 2. Health Checks & Monitoring

### Health Check Endpoint

The `/health` endpoint is used by Cloud Run for liveness and readiness probes.

```bash
# Manual health check
curl http://localhost:8080/health

# Expected response (200 OK)
{
  "status": "ok",
  "node": "tai@container-id",
  "uptime_ms": 45000,
  "timestamp": "2026-01-26T14:30:45.123Z",
  "checks": {
    "http_server": "ok",
    "pubsub_ready": "ok"
  }
}
```

### Health Check Configuration (GCP)

```bash
gcloud run services update tai-autonomics \
  --region=${REGION} \
  --http2 \
  --http-health-check /health \
  --startup-cpu-throttling \
  --cpu=2 \
  --memory=2Gi \
  --timeout=300 \
  --max-instances=100
```

### Key Metrics to Monitor

#### Request Metrics
- **Request count**: `tai_http_requests_total`
- **Request latency**: `tai_http_request_duration_seconds`
- **Error rate**: Count of 4xx/5xx responses

#### Governor Metrics
- **Governor state**: `tai_governor_state` (gauge)
- **State transitions**: `tai_governor_transitions_total`
- **Actions pending**: `tai_governor_actions_pending`

#### Receipt Metrics
- **Receipts emitted**: `tai_receipts_total`
- **Receipt latency**: `tai_receipt_emission_seconds`
- **Firestore writes**: `tai_firestore_writes_total`

#### System Metrics
- **Memory usage**: `process_resident_memory_bytes`
- **CPU usage**: `process_cpu_seconds_total`
- **Erlang processes**: `erlang_processes`

### Setting Up Cloud Monitoring

```bash
# Create custom dashboard
cat > /tmp/dashboard.json << 'EOF'
{
  "displayName": "TAI Autonomics Dashboard",
  "dashboardFilters": [],
  "gridLayout": {
    "widgets": [
      {
        "title": "Request Rate",
        "xyChart": {
          "dataSets": [{
            "timeSeriesQuery": {
              "timeSeriesFilter": {
                "filter": "metric.type=\"tai_http_requests_total\"",
                "aggregation": {"perSeriesAligner": "ALIGN_RATE"}
              }
            }
          }]
        }
      },
      {
        "title": "Error Rate",
        "xyChart": {
          "dataSets": [{
            "timeSeriesQuery": {
              "timeSeriesFilter": {
                "filter": "metric.type=\"tai_http_errors_total\"",
                "aggregation": {"perSeriesAligner": "ALIGN_RATE"}
              }
            }
          }]
        }
      }
    ]
  }
}
EOF

# Deploy dashboard
gcloud monitoring dashboards create --config-from-file=/tmp/dashboard.json
```

---

## 3. Alerting Strategy

### Phase 1: Manual Alerting

During Phase 1 (eval-only), alerting is manual:

1. **Monitor logs daily**
   ```bash
   gcloud logging read \
     "resource.type=cloud_run_revision AND resource.labels.service_name=tai-autonomics" \
     --limit 100 \
     --format="table(timestamp, severity, jsonPayload.message)"
   ```

2. **Check metrics hourly**
   ```bash
   # Query recent errors
   gcloud monitoring time-series list \
     --filter='metric.type="tai_http_errors_total"' \
     --format=json | jq '.[] | .points[] | .value'
   ```

3. **Review receipt ledger weekly**
   ```bash
   # Count receipts by type
   gcloud firestore documents list --collection-id=receipts \
     --limit=1000 --format="table(document_id)"
   ```

### Phase 2: Automated Alerting (TBD)

Planned for Phase 2:
- [ ] PagerDuty integration
- [ ] Slack notifications
- [ ] Auto-escalation on repeated failures
- [ ] Incident auto-creation

### Alert Thresholds (Recommended)

| Metric | Threshold | Action |
|--------|-----------|--------|
| Error rate | > 1% for 5 min | Page on-call |
| Latency p99 | > 1000ms for 10 min | Investigate |
| Memory usage | > 80% for 5 min | Review growth |
| Governor crash | > 2 per hour | Immediate investigation |

---

## 4. Troubleshooting Guide

### Server Won't Start

**Symptoms**: Service fails to start or crashes immediately

**Diagnosis**:
```bash
# Check logs
gcloud logging read \
  "resource.type=cloud_run_revision AND resource.labels.service_name=tai-autonomics" \
  --limit 50 | grep -i error

# Check most recent revision
gcloud run revisions list --service=tai-autonomics --region=${REGION}
```

**Common Causes & Solutions**:

| Cause | Symptom | Solution |
|-------|---------|----------|
| Port already bound | "eaddrinuse" | Check if multiple instances running |
| Bad config | "badmatch" or "badarg" | Verify config/sys.config syntax |
| Missing deps | "undefined function" | Check rebar.lock, rebuild container |
| Memory too low | "killed" with no error | Increase memory: `--memory=2Gi` |

**Resolution Steps**:

1. Check configuration
   ```bash
   gcloud run services describe tai-autonomics --format='value(spec.template.spec)'
   ```

2. View complete logs
   ```bash
   gcloud logging read \
     "resource.type=cloud_run_revision AND resource.labels.service_name=tai-autonomics" \
     --limit 200 --format=json | jq '.[] | .jsonPayload'
   ```

3. Rollback to previous version
   ```bash
   gcloud run deploy tai-autonomics \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${LAST_WORKING_VERSION} \
     --region=${REGION}
   ```

### Endpoints Not Responding

**Symptoms**: Health check fails, requests timeout

**Diagnosis**:
```bash
# Check service status
gcloud run services describe tai-autonomics --region=${REGION}

# Check recent errors
curl -v http://localhost:8080/health

# Check logs for errors
gcloud logging read \
  "resource.type=cloud_run_revision AND severity=ERROR" \
  --limit 50
```

**Common Causes & Solutions**:

| Cause | Symptom | Solution |
|-------|---------|----------|
| Handler crash | 500 error with stack trace | Check error log, deploy fix |
| Governor stuck | Timeout on gov endpoint | Check governor state, restart |
| External service down | Timeout on dependent calls | Verify Firestore/Pub/Sub |
| Rate limiting | 429 response | Reduce request rate or increase quota |

**Resolution Steps**:

1. Restart the service
   ```bash
   # Gradual restart (no downtime with 3+ instances)
   for revision in $(gcloud run revisions list --service=tai-autonomics \
      --filter="status.conditions[0].status=True" --format="value(name)"); do
     gcloud run revisions update-traffic tai-autonomics --to-revisions=$revision=0
     sleep 30  # Allow graceful drain
     gcloud run revisions update-traffic tai-autonomics --to-revisions=$revision=100
   done
   ```

2. Check external dependencies
   ```bash
   # Test Firestore connectivity
   gcloud firestore databases describe --region=${REGION}

   # Test Pub/Sub connectivity
   gcloud pubsub topics describe erlang-autonomics-events
   ```

3. Review recent changes
   ```bash
   # Get deployment history
   gcloud run revisions list --service=tai-autonomics --region=${REGION} \
     --limit 10 --format="table(name,status.conditions[0].lastTransitionTime)"
   ```

### Tools Not Registering

**Symptoms**: MCP tools not available in client

**Diagnosis**:
```bash
# Check if MCP server started
gcloud logging read \
  "resource.type=cloud_run_revision AND jsonPayload.message=~'MCP'" \
  --limit 10

# Verify tool registration
curl http://localhost:8080/mcp/tools | jq '.tools'
```

**Solutions**:

1. Check tool module is registered
   ```erlang
   % In taiea_mcp_registry.erl
   get_tools() ->
       [
           my_tool,  % Check it's listed here
           other_tool
       ].
   ```

2. Verify tool spec is valid
   ```bash
   rebar3 compile
   rebar3 ct --suite=my_tool_SUITE
   ```

3. Restart MCP server
   ```bash
   gcloud run deploy tai-autonomics --no-traffic \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION}
   ```

### Performance Degradation

**Symptoms**: Increasing latency, timeouts, memory growth

**Diagnosis**:
```bash
# Check memory trend
gcloud monitoring read \
  --filter='metric.type="process_resident_memory_bytes"' \
  --format=json | jq '.[].points[] | {time: .interval.endTime, value: .value}'

# Check request latency
gcloud monitoring read \
  --filter='metric.type="tai_http_request_duration_seconds"' \
  --format=json
```

**Solutions**:

1. **Memory Leak**
   - Check for unbounded collections
   - Review recent code changes
   - Restart service to reset memory
   ```bash
   gcloud run deploy tai-autonomics \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${VERSION} \
     --region=${REGION} \
     --no-traffic
   ```

2. **CPU Throttling**
   - Increase CPU allocation
   ```bash
   gcloud run services update tai-autonomics \
     --cpu=4 \
     --region=${REGION}
   ```

3. **Database Bottleneck**
   - Check Firestore read/write quotas
   - Review receipt write patterns
   - Consider batching writes

---

## 5. Scaling Considerations

### Phase 1: Single Instance

Phase 1 is configured for single-instance deployment:

```bash
gcloud run deploy tai-autonomics \
  --region=${REGION} \
  --cpu=2 \
  --memory=2Gi \
  --min-instances=1 \
  --max-instances=1
```

**Limitations**:
- No automatic failover
- Manual restart required if instance crashes
- Limited concurrency (CPU/memory bounded)

**Capacity**:
- ~100 requests/second
- ~10 concurrent governors
- ~1000 actions in flight

### Phase 2: Multi-Instance (Planned)

Phase 2 will support:
- [ ] 3+ concurrent instances
- [ ] Shared receipt ledger (Firestore)
- [ ] Distributed governor state (Redis or similar)
- [ ] Load balancing with Cloud Load Balancer

---

## 6. Log Aggregation

### Cloud Logging

All logs are automatically aggregated in GCP Cloud Logging.

```bash
# View all logs
gcloud logging read \
  "resource.type=cloud_run_revision AND \
   resource.labels.service_name=tai-autonomics" \
  --limit 100

# Filter by severity
gcloud logging read \
  "resource.type=cloud_run_revision AND \
   resource.labels.service_name=tai-autonomics AND \
   severity>=ERROR" \
  --limit 50

# Search for specific pattern
gcloud logging read \
  "resource.type=cloud_run_revision AND \
   jsonPayload.message=~'governor.*crash'" \
  --limit 20

# Export to BigQuery (for analysis)
gcloud logging sinks create tai-autonomics-bigquery \
  bigquery.googleapis.com/projects/${PROJECT_ID}/datasets/tai_logs \
  --log-filter="resource.type=cloud_run_revision AND \
    resource.labels.service_name=tai-autonomics"
```

### Log Levels

Configure in `config/sys.config`:

```erlang
{kernel, [
    {logger, [
        {default, {info}},  % info, warning, error, critical
        {handlers, [
            {logger_std_h, primary, #{
                formatter => {logger_formatter, #{
                    template => [time, " [", level, "] ", msg, "\n"]
                }}
            }}
        ]}
    ]}
]}.
```

**Recommended Levels**:
- **Development**: `debug`
- **Staging**: `info`
- **Production**: `warning` (info for troubleshooting)

---

## 7. Backup & Recovery

### Phase 1: No Automated Backup

Phase 1 is eval-only, no backup strategy yet.

### Firestore Data Preservation

```bash
# Manual export for disaster recovery
gcloud firestore export gs://${BACKUP_BUCKET}/receipts-backup-$(date +%Y%m%d)

# List exports
gsutil ls gs://${BACKUP_BUCKET}/

# Restore from backup (if needed)
gcloud firestore import gs://${BACKUP_BUCKET}/receipts-backup-20240125
```

### Configuration Backup

```bash
# Backup current configuration
gcloud run services describe tai-autonomics \
  --region=${REGION} \
  --format=json > tai-autonomics-config-backup.json

# Restore configuration
gcloud run deploy tai-autonomics \
  --region=${REGION} \
  $(cat tai-autonomics-config-backup.json | \
    jq -r '.spec | keys[] as $k | "--\($k | gsub("_";"-"))=\(.[$k])"')
```

---

## 8. Incident Response

### Minor Issues (Non-blocking)

1. Check logs for errors
2. Review recent deployments
3. Plan fix for next release

### Major Issues (Production Down)

1. **Immediate**: Rollback to last known good version
   ```bash
   gcloud run deploy tai-autonomics \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:${LAST_WORKING_VERSION}
   ```

2. **Triage**: Identify root cause
   ```bash
   gcloud logging read \
     "resource.type=cloud_run_revision AND severity=ERROR" \
     --limit 200 --format=json
   ```

3. **Communicate**: Update status page, notify stakeholders

4. **Fix**: Deploy fix or keep rollback in place

5. **Postmortem**: Analyze failure, document prevention

---

## 9. Compliance & Audit

### Receipt Ledger Audit

All actions are recorded in the receipt ledger (Firestore).

```bash
# Query receipts by tenant
gcloud firestore documents list --collection-id=receipts \
  --filter="tenant_id == 'acme-corp'" \
  --format="table(document_id,data)"

# Verify receipt chain integrity
# (Tool: taiea_receipts:verify_receipt_chain/1)
gcloud firestore documents list --collection-id=receipts \
  --order-by="timestamp" \
  --limit=1000 \
  --format=json | jq '.[] | {receipt_id: .data.receipt_id, hash: .data.receipt_hash}'
```

### Compliance Checks

Run before production deployment:

```bash
# Verify no hardcoded secrets
grep -r "password\|secret\|token" apps/ --include="*.erl" | grep -v ".erl~"

# Verify all public functions have specs
rebar3 dialyzer

# Check for unsafe operations
grep -r "unwrap\|expect\|panic" apps/ --include="*.erl"
```

---

## 10. Runbooks

### Runbook: Deploy New Version

**Time**: 30 minutes
**Risk**: Low (with proper testing)

1. Build and test locally
   ```bash
   rebar3 ct
   docker build -f container/Containerfile -t tai-autonomics:test .
   docker run -e PORT=8080 tai-autonomics:test
   ```

2. Push to registry
   ```bash
   docker tag tai-autonomics:test gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1
   docker push gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1
   ```

3. Deploy to canary
   ```bash
   gcloud run deploy tai-autonomics-canary \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1
   ```

4. Monitor for 15 minutes
   - Check error rate < 0.1%
   - Check latency p99 < 500ms

5. Deploy to production
   ```bash
   gcloud run deploy tai-autonomics \
     --image=gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1
   ```

6. Verify
   ```bash
   curl https://tai-autonomics.example.com/health
   ```

### Runbook: Rollback Version

**Time**: 5 minutes
**Risk**: None (restores known-good state)

```bash
# Get previous working version
gcloud run revisions list --service=tai-autonomics --region=${REGION} \
  --limit 10 --format="table(name,status.conditions[0].lastTransitionTime)"

# Rollback (shift 100% traffic to previous revision)
gcloud run services update-traffic tai-autonomics \
  --to-revisions=<PREVIOUS_REVISION_NAME>=100 \
  --region=${REGION}

# Verify
curl https://tai-autonomics.example.com/health
```

### Runbook: Scale Instances

**Time**: 5 minutes
**Risk**: Low (only in Phase 2+)

```bash
# Increase max instances
gcloud run services update tai-autonomics \
  --max-instances=10 \
  --region=${REGION}

# Monitor resource usage
gcloud monitoring read \
  --filter='metric.type="run.googleapis.com/request_count"'
```

---

## 11. Maintenance Windows

### Phase 1 Maintenance

Phase 1 has no scheduled maintenance windows. Ad-hoc maintenance only.

### Phase 2 Maintenance (Planned)

Planned for Phase 2:
- Weekly: Monitor review and log cleanup
- Monthly: Firestore data export and archival
- Quarterly: Security updates and patches

---

## 12. Appendix: Configuration Reference

### Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `PORT` | `8080` | HTTP server port |
| `NODE_NAME` | `tai@localhost` | Erlang node name |
| `FIRESTORE_PROJECT_ID` | Auto-detected | GCP project ID |
| `PUBSUB_TOPIC` | `erlang-autonomics-events` | Pub/Sub topic |
| `LOG_LEVEL` | `info` | Logging verbosity |

### GCP Permissions

Service account requires:
- `firestore.databases.get`
- `firestore.documents.create`
- `firestore.documents.list`
- `pubsub.topics.get`
- `pubsub.subscriptions.consume`
- `monitoring.timeSeries.create` (for metrics)

---

**Questions?** Contact the operations team or review ARCHITECTURE.md for system design details.
