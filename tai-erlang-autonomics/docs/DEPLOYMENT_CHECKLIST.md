# GCP Deployment Checklist

Production deployment validation checklist for TAI Erlang Autonomics on Google Cloud Run.

---

## Pre-Deployment (48 Hours Before)

### Infrastructure Validation

- [ ] GCP project created and billing enabled
- [ ] Required APIs enabled (Cloud Run, Pub/Sub, Firestore, etc.)
- [ ] Service account created with minimal required permissions
- [ ] Cloud Run quotas checked (instances, CPU, memory)
- [ ] Firestore database created and configured
- [ ] Pub/Sub topic and subscription created
- [ ] Artifact Registry repository created

```bash
# Verify APIs are enabled
gcloud services list --enabled --project=PROJECT_ID | grep -E "run|pubsub|firestore|artifactregistry"

# Check quotas
gcloud compute project-info describe --project=PROJECT_ID --format="value(quotas[name].usage)"
```

### Code and Configuration Review

- [ ] Code review completed (LGTM from 2 reviewers)
- [ ] All tests passing locally: `make build && make test`
- [ ] Container builds successfully: `make docker-build`
- [ ] Configuration files validated: `make terraform-plan`
- [ ] Environment variables set correctly
- [ ] Secrets stored in Secret Manager (not in code)
- [ ] Changelog updated with deployment notes

```bash
# Run pre-deployment checks
make gcp-ready
```

### Documentation Review

- [ ] Runbook reviewed and accessible
- [ ] Alert thresholds documented
- [ ] Known issues documented
- [ ] Rollback procedure documented
- [ ] Team notified of deployment window

---

## 24 Hours Before Deployment

### Final Testing

- [ ] Integration tests passing: `make test`
- [ ] Load test completed (if applicable)
- [ ] GCP integration tests passing: `make gcp-test`
- [ ] Disaster recovery tested (backup/restore)
- [ ] Monitoring dashboards created
- [ ] Alert policies configured

```bash
# Run comprehensive tests
make test
make gcp-test

# Verify Terraform plan
cd terraform && terraform plan
```

### Team Preparation

- [ ] On-call engineer assigned
- [ ] Rollback plan reviewed
- [ ] Communication channels established (Slack, PagerDuty)
- [ ] Status page updated (if applicable)
- [ ] Customers notified (if needed)

---

## 1 Hour Before Deployment

### Final Pre-Deployment Checks

- [ ] All team members ready
- [ ] No active incidents
- [ ] Monitoring dashboards open
- [ ] Communication channels ready
- [ ] Deployment procedure reviewed
- [ ] Quick reference accessible

```bash
# Final verification
gcloud run services list --project=PROJECT_ID
gcloud pubsub subscriptions list --project=PROJECT_ID
gcloud firestore databases list --project=PROJECT_ID
```

---

## Deployment Procedure

### Phase 1: Build and Push Container

- [ ] Pull latest code: `git pull origin main`
- [ ] Verify version: `cat Cargo.toml | grep version`
- [ ] Build Erlang release: `make build`
- [ ] Build Docker image: `make docker-build`
- [ ] Push to Artifact Registry: `make docker-push`

```bash
export GCP_PROJECT_ID=my-project
export GCP_REGION=us-central1
make docker-build
make docker-push
```

**Time Estimate**: 3-5 minutes

### Phase 2: Deploy Infrastructure

- [ ] Validate Terraform: `cd terraform && terraform plan`
- [ ] Apply Terraform: `cd terraform && terraform apply`
- [ ] Wait for deployment (2-5 min)
- [ ] Verify Cloud Run service created
- [ ] Verify Pub/Sub subscription active

```bash
make terraform-plan
make terraform-apply
sleep 120
```

**Time Estimate**: 2-5 minutes

### Phase 3: Verify Deployment

- [ ] Get service URL: `cd terraform && terraform output cloud_run_service_url`
- [ ] Health check: `curl $SERVICE_URL/health`
- [ ] Verify status code 200
- [ ] Check logs: `gcloud run logs read --limit=20`
- [ ] Verify no ERROR logs

```bash
SERVICE_URL=$(cd terraform && terraform output -raw cloud_run_service_url)
curl $SERVICE_URL/health
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" --limit=10
```

**Time Estimate**: 2-3 minutes

### Phase 4: Integration Testing

- [ ] Run integration tests: `make gcp-test`
- [ ] All tests passing
- [ ] Monitor error rate for 5 min (should be < 0.1%)
- [ ] Monitor latency for 5 min (should be < 500ms)
- [ ] Monitor health check success rate (should be 100%)

```bash
SERVICE_URL=$(cd terraform && terraform output -raw cloud_run_service_url)
GCP_PROJECT_ID=my-project CLOUD_RUN_SERVICE_URL=$SERVICE_URL make gcp-test
```

**Time Estimate**: 5-10 minutes

### Phase 5: Canary Deployment (Production)

For production, use gradual rollout:

- [ ] Set canary traffic to 10%: `gcloud run services update-traffic tai-autonomics --to-revisions NEW=10`
- [ ] Monitor metrics for 5 minutes
- [ ] If error rate < 0.1%, increase to 50%
- [ ] Monitor metrics for 5 minutes
- [ ] If error rate < 0.1%, move to 100%

```bash
gcloud run services update-traffic tai-autonomics --to-revisions BLUE=10,GREEN=90
sleep 300
# Monitor dashboards
gcloud run services update-traffic tai-autonomics --to-revisions BLUE=50,GREEN=50
sleep 300
# Monitor dashboards
gcloud run services update-traffic tai-autonomics --to-revisions BLUE=100
```

**Time Estimate**: 15-20 minutes

---

## Post-Deployment (1 Hour)

### Immediate Verification

- [ ] Service URL responding to requests
- [ ] Health check returning 200
- [ ] No ERROR logs in Cloud Logging
- [ ] Error rate < 0.1%
- [ ] Latency P99 < 500ms
- [ ] Receipts being written to Firestore

```bash
# Check service metrics
gcloud monitoring timeseries list \
  --filter='metric.type="run.googleapis.com/request_latencies"' \
  --limit=10

# Check receipts
gcloud firestore documents list --collection-id=receipts --limit=5
```

### Dashboard Monitoring

- [ ] Service Health dashboard green
- [ ] Receipt Processing dashboard healthy
- [ ] Governor State transitions normal
- [ ] Resource Utilization within expected bounds
- [ ] No alert triggers

### Team Communication

- [ ] Post deployment message to team
- [ ] Document any issues found
- [ ] Note any unexpected metrics
- [ ] Schedule post-deployment review (24 hours)

---

## Post-Deployment (24 Hours)

### Extended Monitoring

- [ ] No incidents reported
- [ ] Error rate consistently < 0.1%
- [ ] Latency consistently within SLO
- [ ] Receipt processing working correctly
- [ ] Pub/Sub messages processing normally
- [ ] Firestore database performing well

### Performance Validation

- [ ] Average latency: < 100ms
- [ ] P95 latency: < 200ms
- [ ] P99 latency: < 500ms
- [ ] Throughput: > 100 req/s per instance
- [ ] Memory usage: < 80% of limit
- [ ] CPU usage: < 80% of limit

### Health Check

```bash
# Review metrics over past 24 hours
gcloud monitoring timeseries list \
  --filter='metric.type="run.googleapis.com/request_count"' \
  --format=json | jq '.[] | select(.points[0].value.int64_value)'

# Check error logs
gcloud logging read "resource.type=cloud_run_revision AND severity=ERROR" \
  --limit=50 \
  --filter="timestamp>=$(date -u -d '24 hours ago' +%Y-%m-%dT%H:%M:%S)Z"

# Verify Firestore
gcloud firestore documents list --collection-id=receipts --limit=100
```

### Team Retrospective

- [ ] Deployment review meeting scheduled
- [ ] Issues documented and assigned
- [ ] Improvements identified
- [ ] Process improvements implemented
- [ ] Knowledge shared with team

---

## Rollback Procedure (If Needed)

### Quick Rollback (< 2 minutes)

If critical issues detected immediately:

```bash
# Get previous revision
PREV_REVISION=$(gcloud run revisions list --service=tai-autonomics --format='value(name)' --limit=2 | tail -1)

# Rollback traffic to previous revision
gcloud run services update-traffic tai-autonomics --to-revisions $PREV_REVISION=100

# Verify rollback
curl https://SERVICE_URL/health
```

### Terraform Rollback (< 5 minutes)

If infrastructure issues:

```bash
# Get previous state backup
git checkout HEAD~1 -- terraform/

# Reapply previous configuration
make terraform-apply

# Verify rollback
gcloud run services describe tai-autonomics
```

### Database Rollback (< 30 minutes)

If data issues:

```bash
# List backups
gcloud firestore backups list

# Restore from backup
gcloud firestore restore BACKUP_ID

# Verify restore
gcloud firestore documents list --collection-id=receipts --limit=5
```

### Communication

- [ ] Immediately notify team of rollback
- [ ] Post status update to communication channel
- [ ] Document reason for rollback
- [ ] Plan remediation steps
- [ ] Schedule post-mortem

---

## Success Criteria

Deployment is considered successful if:

1. **Service Health**
   - ✓ Health check returns 200 for all checks
   - ✓ Service processing requests
   - ✓ No critical errors in logs

2. **Performance**
   - ✓ Error rate < 0.1%
   - ✓ Latency P99 < 500ms
   - ✓ Response times stable

3. **Data Processing**
   - ✓ Receipts emitted for all requests
   - ✓ Receipts stored in Firestore
   - ✓ Hash chain valid
   - ✓ No data loss

4. **Monitoring**
   - ✓ All metrics reporting
   - ✓ Dashboards functional
   - ✓ Alerts configured
   - ✓ Traces visible

5. **Operations**
   - ✓ Logs aggregating to Cloud Logging
   - ✓ Team aware of deployment
   - ✓ Runbooks accessible
   - ✓ Escalation path clear

---

## Deployment Statistics

### Expected Timings

| Phase | Duration | Notes |
|-------|----------|-------|
| Build & Push | 3-5 min | Includes Docker build |
| Deploy Infrastructure | 2-5 min | Terraform apply |
| Initial Verification | 2-3 min | Health checks |
| Integration Testing | 5-10 min | Full test suite |
| Canary Rollout | 15-20 min | Production only |
| **Total** | **30-45 min** | Full deployment cycle |

### Rollback Times

| Type | Duration | Notes |
|------|----------|-------|
| Traffic Rollback | < 2 min | Switch to previous revision |
| Terraform Rollback | < 5 min | Revert infrastructure |
| Database Rollback | < 30 min | Restore from backup |

---

## Issue Resolution Guide

### High Error Rate

**Threshold**: > 1% for 5 min

**Actions**:
1. Check recent logs: `gcloud logging read ... --limit=50`
2. Identify error pattern
3. If code issue: rollback deployment
4. If config issue: update and re-deploy
5. If data issue: investigate and fix

### High Latency

**Threshold**: P99 > 1s for 5 min

**Actions**:
1. Check resource utilization: `gcloud monitoring timeseries list`
2. Increase max instances: `gcloud run deploy --max-instances=100`
3. Check Firestore latency
4. Monitor for 5 min
5. If still high: investigate bottlenecks

### Memory Issues

**Threshold**: > 90% for 2 min

**Actions**:
1. Check memory metrics
2. Increase Cloud Run memory: `gcloud run deploy --memory=4Gi`
3. Reduce buffer sizes
4. Monitor GC logs
5. If OOM kills occur: increase or optimize code

### Receipts Not Being Written

**Symptoms**:
- Requests succeed but no receipts in Firestore

**Actions**:
1. Verify Firestore enabled: `gcloud firestore databases describe`
2. Check IAM permissions on service account
3. Verify network connectivity
4. Check Firestore quotas
5. Restart service if needed

---

## Contact and Escalation

| Role | Contact | Available |
|------|---------|-----------|
| Deployment Lead | Sean | 24/7 |
| On-Call SRE | [team-oncall] | 24/7 |
| Incident Commander | [ic-oncall] | 24/7 |
| GCP Support | support@google.com | 24/7 |

---

## Post-Deployment Checklist

### 1 Day After

- [ ] All metrics normal for past 24 hours
- [ ] No incidents or alerts
- [ ] Customer feedback positive
- [ ] Deployment review completed

### 1 Week After

- [ ] Extended monitoring shows stability
- [ ] No performance degradation
- [ ] Cost within expected range
- [ ] Team confident in changes

### 1 Month After

- [ ] Feature working as expected
- [ ] Metrics baseline established
- [ ] No technical debt introduced
- [ ] Documentation up to date

---

## Deployment Timeline Template

```
[DATE] [TIME] - Pre-deployment checks starting
[DATE] [TIME] - Building container
[DATE] [TIME] - Pushing to Artifact Registry
[DATE] [TIME] - Deploying infrastructure
[DATE] [TIME] - Running integration tests
[DATE] [TIME] - Starting canary rollout
[DATE] [TIME] - Increasing to 50% traffic
[DATE] [TIME] - Moving to 100% traffic
[DATE] [TIME] - Deployment complete
[DATE] [TIME+1h] - Extended monitoring complete
[DATE] [TIME+24h] - Post-deployment review
```

---

## References

- Deployment Guide: GCP_DEPLOYMENT.md
- Runbook: RUNBOOK.md
- Configuration: CONFIG.md
- Monitoring: MONITORING.md
- Cloud Run Documentation: https://cloud.google.com/run/docs
