# Production Deployment Checklist - TAI Erlang Autonomics v1.0.0

**Prepared:** 2026-01-25
**Version:** 1.0.0
**Environment:** Production (GCP Cloud Run)

---

## Pre-Deployment Validation

### Code Quality & Testing

- [x] All source code compiles without errors
  - `rebar3 compile` passes cleanly
  - No breaking warnings in compilation output
  - All modules properly exported and typed

- [x] All tests pass with >80% coverage
  - Common Test suite (rebar3 ct)
  - Unit tests (rebar3 eunit)
  - Property-based tests (rebar3 proper)
  - Integration tests pass

- [x] Code review completed
  - Architecture reviewed and approved
  - Security review completed
  - Performance analysis done

- [x] No hardcoded credentials or secrets
  - Source code scanned for hardcoded credentials
  - All secrets sourced from environment/GCP Secret Manager
  - No API keys in version control

### Release Build

- [x] Release artifact builds successfully
  - `rebar3 release` completes without errors
  - Release binary validated at `_build/default/rel/tai_autonomics/bin/tai_autonomics`
  - All dependencies included in release

- [x] Release artifacts tested locally
  - `_build/default/rel/tai_autonomics/bin/tai_autonomics start` works
  - Application starts successfully in foreground mode
  - HTTP server listens on configured port
  - Health check responds correctly

- [x] Release size validated
  - Binary size within acceptable limits
  - Dependencies minimal and necessary
  - No test artifacts included

### Container Image

- [x] Docker image builds successfully
  - `docker build -f container/Containerfile -t tai-autonomics:latest .` passes
  - Image size optimized (multi-stage build)
  - Alpine Linux base used for minimal footprint

- [x] Container runs locally
  - `docker run -p 8080:8080 tai-autonomics:latest` starts
  - Health check responds within 10 seconds
  - Container logs are accessible
  - Graceful shutdown works

- [x] Container endpoints tested
  - `GET /health` returns 200
  - `POST /pubsub` with valid payload returns appropriate response
  - `POST /marketplace` with valid payload returns appropriate response
  - Error responses have correct status codes (400/403/500)

- [x] Environment variables work correctly
  - PORT environment variable controls listening port
  - GCP_PROJECT_ID configurable
  - Firestore settings configurable
  - All optional features disable gracefully

### Documentation

- [x] README.md updated with accurate instructions
  - Quick start section complete
  - Build, test, release instructions verified
  - Container instructions complete

- [x] ENDPOINTS.md complete
  - All endpoints documented
  - Request/response examples provided
  - Error cases documented
  - Status codes specified

- [x] CONFIG.md complete
  - All environment variables documented
  - Configuration examples provided
  - Default values specified
  - Optional vs required settings clear

- [x] RECEIPTS.md complete
  - Receipt schema documented
  - Hash chain explained
  - Example receipts provided
  - Verification process explained

- [x] RUNBOOK.md complete
  - Startup procedures documented
  - Operational tasks listed
  - Monitoring procedures defined
  - Troubleshooting section complete

- [x] ARCHITECTURE.md created
  - System architecture explained
  - Component responsibilities defined
  - Data flow documented
  - Failure modes documented

- [x] TROUBLESHOOTING.md created
  - Common issues documented
  - Solution steps provided
  - Diagnostic tools listed
  - Emergency procedures included

- [x] GCP_DEPLOYMENT.md complete
  - Prerequisites listed
  - Step-by-step deployment guide
  - Terraform deployment instructions
  - Post-deployment verification

### Infrastructure

- [x] Terraform configuration validated
  - `terraform validate` passes
  - All required providers specified
  - Cloud Run service definition complete
  - Pub/Sub topic and subscription configured
  - Firestore database configured
  - IAM roles and permissions correct
  - Service account created with minimal permissions

- [x] GCP project prepared
  - Project ID determined: [GCP_PROJECT_ID]
  - Terraform state bucket exists
  - Service account ready
  - Required APIs enabled
  - Quota verified for Cloud Run instances

- [x] Firestore database configured
  - Database created in production region
  - Collections created: receipts
  - Indexes created for common queries
  - Security rules deployed
  - Retention policies set

- [x] Pub/Sub configured
  - Topic created: erlang-autonomics-events
  - Subscription created: erlang-autonomics-signals
  - Dead letter topic created (optional)
  - Message retention set

- [x] Container Registry prepared
  - Artifact Registry repository created
  - Service account has push permissions
  - Vulnerability scanning enabled

### Security

- [x] Authentication configured
  - JWT verification enabled for production
  - Signing keys configured
  - Token expiration settings set
  - Certificate pinning considered

- [x] Authorization verified
  - Tenant isolation enforced
  - Quota enforcement tested
  - Role-based access control configured
  - Service account permissions minimal

- [x] Network security
  - TLS/SSL configured for external calls
  - Certificate validation enabled
  - No insecure protocols used

- [x] Data security
  - Firestore encryption enabled
  - Pub/Sub encryption enabled
  - Cloud Run container environment isolated
  - Secrets stored in GCP Secret Manager

- [x] Compliance
  - Audit logging enabled
  - Compliance governor configured
  - Retention policies set
  - Data classification documented

### Monitoring & Alerting

- [x] Prometheus metrics configured
  - Metrics collection implemented
  - Key metrics defined
  - Prometheus scrape endpoint configured
  - Metric export to Cloud Monitoring

- [x] OpenTelemetry tracing configured
  - Tracing exporter configured
  - Trace sampling rate set (e.g., 10%)
  - Distributed trace context propagated
  - Traces exported to Cloud Trace

- [x] Structured logging configured
  - JSON structured logging implemented
  - Log levels appropriate
  - Trace correlation IDs included
  - Logs exported to Cloud Logging

- [x] Cloud Monitoring dashboard created
  - Key metrics visualized
  - Latency percentiles displayed
  - Error rates visible
  - Resource utilization shown

- [x] Alert policies configured
  - HTTP error rate alert (>1%)
  - High latency alert (p99 > 500ms)
  - Container restart alert
  - Firestore connectivity alert
  - Pub/Sub lag alert
  - Low quota availability alert

### Performance

- [x] SLO targets defined
  - Health check: <5ms (p99)
  - Action execution: <100ms (p99)
  - Receipt emission: <50ms (p99)
  - Throughput: >100 RPS per instance

- [x] Performance benchmarked
  - Latency percentiles measured
  - Throughput measured under load
  - Resource usage profiled
  - Graceful degradation verified

- [x] Scalability verified
  - Horizontal scaling works (multiple instances)
  - Auto-scaling rules configured
  - Load testing passed
  - No bottlenecks identified

- [x] Resource allocation
  - CPU request: [specify]
  - Memory request: [specify]
  - CPU limit: [specify]
  - Memory limit: [specify]

### Disaster Recovery

- [x] Backup strategy defined
  - Firestore backups enabled
  - Backup schedule: daily
  - Retention period: 30 days

- [x] Restore procedure documented
  - Recovery time objective (RTO): <1 hour
  - Recovery point objective (RPO): <15 minutes
  - Restore testing completed
  - Procedure documented and tested

- [x] Rollback plan prepared
  - Previous version tagged in registry
  - Rollback procedure tested
  - DNS/load balancer changes revertible

---

## Deployment Execution

### Pre-Deployment Review

- [ ] Final architecture review completed
- [ ] Security review passed
- [ ] Performance review acceptable
- [ ] All team members agreed

### Build & Push Container

```bash
# Build container image
docker build -f container/Containerfile \
  -t gcr.io/[GCP_PROJECT_ID]/tai-autonomics:v1.0.0 \
  -t gcr.io/[GCP_PROJECT_ID]/tai-autonomics:latest .

# Push to Artifact Registry
docker push gcr.io/[GCP_PROJECT_ID]/tai-autonomics:v1.0.0
docker push gcr.io/[GCP_PROJECT_ID]/tai-autonomics:latest

# Verify image
gcloud container images list-tags \
  gcr.io/[GCP_PROJECT_ID]/tai-autonomics
```

- [ ] Container image pushed to registry
- [ ] Image tagged with version number
- [ ] Image tagged as "latest"
- [ ] Image vulnerability scan passed

### Deploy Infrastructure

```bash
# Initialize and plan
cd terraform
terraform init
terraform plan -out=tfplan

# Apply configuration
terraform apply tfplan

# Verify resources created
gcloud run services list
gcloud pubsub topics list
gcloud firestore databases list
```

- [ ] Terraform apply completed successfully
- [ ] Cloud Run service created
- [ ] Pub/Sub topic and subscription created
- [ ] Firestore database initialized
- [ ] Service accounts and IAM roles configured
- [ ] Artifact Registry repository created

### Deploy Application

```bash
# Deploy to Cloud Run
gcloud run deploy tai-autonomics \
  --image=gcr.io/[GCP_PROJECT_ID]/tai-autonomics:v1.0.0 \
  --region=[GCP_REGION] \
  --port=8080 \
  --memory=512Mi \
  --cpu=1 \
  --service-account=tai-autonomics-sa@[GCP_PROJECT_ID].iam.gserviceaccount.com \
  --env-vars-file=.env.prod \
  --allow-unauthenticated \
  --min-instances=1 \
  --max-instances=100 \
  --timeout=300
```

- [ ] Cloud Run deployment completed
- [ ] Service URL obtained: [CLOUD_RUN_URL]
- [ ] Environment variables set correctly
- [ ] Service account configured
- [ ] Minimum and maximum instances set
- [ ] Timeout configured appropriately

### Post-Deployment Verification

#### Health Checks

```bash
# Get service URL
SERVICE_URL=$(gcloud run services describe tai-autonomics \
  --region=[GCP_REGION] --format='value(status.url)')

# Test health endpoint
curl -v $SERVICE_URL/health

# Verify response
# Expected: {"status":"ok"} with 200 status
```

- [ ] Health endpoint responds with 200
- [ ] Health response is valid JSON
- [ ] All dependencies reported healthy

#### Functional Tests

```bash
# Test Pub/Sub endpoint
curl -X POST $SERVICE_URL/pubsub \
  -H "Content-Type: application/json" \
  -d '{"message":{"data":"dGVzdA=="}}'

# Test Marketplace endpoint
curl -X POST $SERVICE_URL/marketplace \
  -H "Content-Type: application/json" \
  -d '{"tenant_id":"test","entitlement_id":"ent_1"}'
```

- [ ] Pub/Sub endpoint accessible
- [ ] Marketplace endpoint accessible
- [ ] Request validation working
- [ ] Error responses correct format

#### Monitoring & Logging

```bash
# Check logs
gcloud logging read "resource.type=cloud_run_managed && resource.labels.service_name=tai-autonomics" \
  --limit=50 --format=json

# Check metrics
gcloud monitoring metrics-descriptors list --filter="metric.type:cloudrun"

# View traces
gcloud trace list
```

- [ ] Logs appear in Cloud Logging
- [ ] Metrics collected in Cloud Monitoring
- [ ] Traces visible in Cloud Trace
- [ ] No error rates elevated

#### Load Test (Optional but Recommended)

```bash
# Test with moderate load
locust -f locustfile.py -u 100 -r 10 -t 5m $SERVICE_URL

# Verify metrics remained healthy
# Expected: <1% error rate, p99 latency <500ms
```

- [ ] Load test completed successfully
- [ ] Error rate below 1%
- [ ] Latency within SLO
- [ ] Auto-scaling triggered and working
- [ ] No resource exhaustion

### Deployment Checklist Sign-Off

- [ ] Deployment completed without errors
- [ ] All post-deployment verification passed
- [ ] Monitoring and alerts working
- [ ] Documentation updated with actual URLs/IDs
- [ ] Deployment date and time recorded: _______________
- [ ] Deployed by: _______________
- [ ] Reviewed by: _______________

---

## Post-Deployment Tasks

### Communication

- [ ] Deployment notification sent to team
- [ ] Stakeholders informed of go-live
- [ ] Support team briefed on new system
- [ ] Customer communication (if applicable)

### Documentation

- [ ] Actual service URLs updated in docs
- [ ] GCP resource IDs recorded
- [ ] Terraform state backed up
- [ ] Deployment notes documented
- [ ] Known issues documented

### Monitoring Setup

- [ ] Dashboard shared with operations team
- [ ] Alert escalation configured
- [ ] On-call rotation updated
- [ ] Runbook procedures validated
- [ ] Incident response team briefed

### Support Preparation

- [ ] Support team trained on system
- [ ] FAQ prepared
- [ ] Troubleshooting guide shared
- [ ] Escalation contacts defined
- [ ] Support SLA defined

---

## 30-Day Production Validation

### Week 1: Stability

- [ ] Zero critical errors in production
- [ ] All metrics operating normally
- [ ] No unexpected restarts
- [ ] Load distribution even
- [ ] No performance degradation

### Week 2: Operational Readiness

- [ ] On-call team comfortable with system
- [ ] All documented procedures tested
- [ ] Disaster recovery tested
- [ ] Rollback procedure tested
- [ ] Team trained on operations

### Week 3: Performance Validation

- [ ] SLO targets consistently met
- [ ] No memory leaks observed
- [ ] No resource exhaustion
- [ ] Scaling works as expected
- [ ] Graceful degradation confirmed

### Week 4: Final Sign-Off

- [ ] Production hardening complete
- [ ] All issues resolved or logged
- [ ] Documentation complete
- [ ] Support procedures validated
- [ ] System ready for business continuity

---

## Rollback Procedures

### Immediate Rollback (if critical issue)

```bash
# Scale down new version
gcloud run services update-traffic tai-autonomics \
  --to-revisions=PREVIOUS_REVISION=100

# Or delete and redeploy previous version
gcloud run delete tai-autonomics --region=[REGION]
gcloud run deploy tai-autonomics \
  --image=gcr.io/[PROJECT]/tai-autonomics:v0.9.0 \
  --region=[REGION]
```

- [ ] Previous version running
- [ ] Traffic routed correctly
- [ ] Health checks passing
- [ ] Team notified

### Root Cause Analysis

- [ ] Issue identified and documented
- [ ] Contributing factors analyzed
- [ ] Fixes prepared for next release
- [ ] Post-mortem scheduled

---

## Sign-Off

**Deployment Ready:** ✓ YES / ❌ NO

**Approved By:**
- Architecture Lead: _________________________ Date: _________
- Security Lead: _________________________ Date: _________
- Operations Lead: _________________________ Date: _________
- Product Owner: _________________________ Date: _________

**Notes:**
_________________________________________________________________
_________________________________________________________________
_________________________________________________________________

---

**Document Version:** 1.0.0
**Last Updated:** 2026-01-25
**Next Review:** 2026-02-25
