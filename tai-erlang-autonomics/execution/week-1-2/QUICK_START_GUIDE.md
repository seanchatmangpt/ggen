# Week 1-2 Deployment - Quick Start Guide

**Timeline**: 10 business days (Week 1-2)
**Complexity**: Advanced (GCP infrastructure, containerization, monitoring)
**Team Size**: 2-3 engineers (1 DevOps, 1 Backend, 1 QA/Testing)
**Estimated Hours**: 60-80 hours total

## Before You Start

### Prerequisites
```bash
# 1. Install required tools
brew install gcloud terraform docker

# 2. Authenticate with GCP
gcloud auth login
gcloud config set project tai-autonomics-prod

# 3. Verify Docker is running
docker ps

# 4. Clone repository
git clone https://github.com/your-org/tai-autonomics.git
cd tai-autonomics
```

### Estimated Costs
- **Week 1-2**: ~$300 (dev/test workload)
- **Production Run**: ~$3,200/month (estimated from baseline)
- **Recommendation**: Set budget alert at $500

## Fast Track Deployment (6-8 hours)

### Phase 1: Setup GCP (30 min)
```bash
#!/bin/bash
export PROJECT_ID="tai-autonomics-prod"
export REGION="us-central1"

# Create project
gcloud projects create $PROJECT_ID
gcloud config set project $PROJECT_ID

# Enable billing (need billing account ID)
gcloud billing projects link $PROJECT_ID --billing-account=BILLING_ACCOUNT_ID

# Enable all APIs in one command
for api in run pubsub firestore cloudbuild artifactregistry \
           secretmanager monitoring logging cloudtrace iam compute dns; do
  gcloud services enable ${api}.googleapis.com
done

echo "✅ GCP Foundation ready"
```

### Phase 2: Docker & Artifact Registry (2 hours)
```bash
# Build and push image
cd /Users/sac/ggen/tai-erlang-autonomics

# Configure Docker auth
gcloud auth configure-docker us-central1-docker.pkg.dev

# Create Artifact Registry (if not exists)
gcloud artifacts repositories create tai-autonomics \
  --repository-format=docker \
  --location=us-central1

# Build and push (15-20 minutes)
docker build -t tai-autonomics:v1.0.0 -f container/Containerfile .

docker tag tai-autonomics:v1.0.0 \
  us-central1-docker.pkg.dev/tai-autonomics-prod/tai-autonomics/tai-autonomics:v1.0.0

docker push us-central1-docker.pkg.dev/tai-autonomics-prod/tai-autonomics/tai-autonomics:v1.0.0

echo "✅ Docker image ready"
```

### Phase 3: Terraform Deploy (1 hour)
```bash
cd terraform

# Setup Terraform backend
gsutil mb -l us-central1 gs://tai-autonomics-terraform-state
gsutil versioning set on gs://tai-autonomics-terraform-state

# Initialize & deploy
terraform init -backend-config="bucket=tai-autonomics-terraform-state"
terraform validate

# Create production.tfvars (see template below)
cat > production.tfvars << 'EOF'
project_id            = "tai-autonomics-prod"
region                = "us-central1"
environment           = "production"
image_tag             = "v1.0.0"
min_instances         = 1
max_instances         = 10
execution_environment = "gen2"
firestore_enabled     = true
tracing_enabled       = true
enable_public_access  = true
EOF

# Deploy
terraform plan -var-file=production.tfvars -out=tfplan
terraform apply tfplan

echo "✅ Infrastructure deployed"
```

### Phase 4: Initialize Database & Deploy Services (1 hour)
```bash
# Initialize Firestore schema
chmod +x ../execution/week-1-2/firestore-schema-init.sh
../execution/week-1-2/firestore-schema-init.sh tai-autonomics-prod us-central1

# Get Cloud Run image URL
IMAGE="us-central1-docker.pkg.dev/tai-autonomics-prod/tai-autonomics/tai-autonomics:v1.0.0"
SA="tai-autonomics-sa@tai-autonomics-prod.iam.gserviceaccount.com"

# Deploy Cloud Run
gcloud run deploy tai-autonomics \
  --image=$IMAGE \
  --region=us-central1 \
  --memory=2Gi \
  --cpu=2 \
  --service-account=$SA \
  --allow-unauthenticated

# Verify deployment
gcloud run services describe tai-autonomics --region=us-central1
SERVICE_URL=$(gcloud run services describe tai-autonomics --region=us-central1 --format='value(status.url)')
curl $SERVICE_URL/health

echo "✅ Services deployed"
```

### Phase 5: Monitoring & Testing (1 hour)
```bash
# Setup monitoring
chmod +x ../execution/week-1-2/monitoring-alerts-setup.sh
../execution/week-1-2/monitoring-alerts-setup.sh tai-autonomics-prod us-central1

# Run smoke tests
chmod +x ../execution/week-1-2/smoke-test.sh
../execution/week-1-2/smoke-test.sh https://tai-autonomics-[id].run.app

# Quick performance baseline (5 minutes)
# Using Apache Bench (simple load test)
ab -n 1000 -c 100 https://tai-autonomics-[id].run.app/health

echo "✅ Monitoring & testing complete"
```

## Critical Decisions

### 1. Domain & SSL
**Option A (Recommended)**: Use Google-managed SSL + Cloud DNS
- Automatic renewal
- Free (included in Load Balancer)
- No manual intervention

**Option B**: Import existing certificate
- Requires manual renewal tracking
- Manual rotation every 90 days

### 2. Database Location
**Option A (Recommended)**: us-central1 (multi-region)
- Better performance for US users
- Automatic failover

**Option B**: us-east1
- Latency from GCP to your office
- Consider co-location

### 3. Autoscaling
**Option A (Recommended)**: Min=1, Max=10
- Always warm (no cold starts)
- Reasonable cost
- Good for production

**Option B**: Min=0, Max=20
- Save costs during idle times
- Cold starts (2-5 seconds)
- Risk of overscaling during spikes

## Troubleshooting Quick Reference

### Issue: Docker Build Fails
```bash
# Increase memory
docker build --memory=4g -f container/Containerfile .

# Check Erlang version
grep "FROM erlang" container/Containerfile

# Build with verbose output
docker build --progress=plain ...
```

### Issue: Terraform Apply Fails
```bash
# Validate configuration
terraform validate

# Check syntax
terraform fmt

# View state
terraform state list

# Refresh state
terraform refresh
```

### Issue: Cloud Run Service Not Starting
```bash
# Check logs
gcloud run services logs read tai-autonomics --limit=50

# Verify image exists
gcloud artifacts docker images list --repository=tai-autonomics

# Check IAM permissions
gcloud iam service-accounts get-iam-policy \
  tai-autonomics-sa@tai-autonomics-prod.iam.gserviceaccount.com

# Test image locally
docker run --rm -p 8080:8080 \
  us-central1-docker.pkg.dev/tai-autonomics-prod/tai-autonomics/tai-autonomics:v1.0.0
```

### Issue: Health Check Failing
```bash
# Verify endpoint
curl -v https://[URL]/health

# Check response code
curl -o /dev/null -s -w "%{http_code}" https://[URL]/health

# Verify service configuration
gcloud run services describe tai-autonomics --region=us-central1

# Check firestore connectivity
gcloud firestore databases describe
```

## Day-by-Day Execution Plan

### Week 1

**Monday (6 hours)**
- [ ] GCP project setup (1 hr)
- [ ] Enable APIs (0.5 hr)
- [ ] Create service account & IAM (1 hr)
- [ ] Create Artifact Registry (0.5 hr)
- [ ] Setup Terraform backend (1 hr)
- [ ] Review Dockerfile (1 hr)

**Tuesday-Wednesday (10 hours)**
- [ ] Build Docker image locally (3 hr)
- [ ] Test image locally (1 hr)
- [ ] Push to Artifact Registry (1 hr)
- [ ] Setup Terraform configuration (3 hr)
- [ ] Terraform validate & plan (2 hr)

**Thursday (6 hours)**
- [ ] Terraform apply (1 hr)
- [ ] Firestore initialization (1 hr)
- [ ] Verify Firestore schema (1 hr)
- [ ] Review Terraform outputs (1 hr)
- [ ] Documentation review (1 hr)
- [ ] Backup cleanup (0.5 hr)

**Friday (4 hours)**
- [ ] Deploy Cloud Run service (1 hr)
- [ ] Verify service health (1 hr)
- [ ] Initial monitoring setup (1 hr)
- [ ] Weekly checklist completion (1 hr)

### Week 2

**Monday (4 hours)**
- [ ] Setup Cloud Load Balancer (1.5 hr)
- [ ] Configure DNS (1 hr)
- [ ] Verify SSL/TLS (1.5 hr)

**Tuesday (4 hours)**
- [ ] Complete monitoring setup (2 hr)
- [ ] Create dashboards (2 hr)

**Wednesday (4 hours)**
- [ ] Load testing setup (1 hr)
- [ ] Execute load tests (2 hr)
- [ ] Review results (1 hr)

**Thursday (4 hours)**
- [ ] Backup testing (1.5 hr)
- [ ] Disaster recovery drill (1.5 hr)
- [ ] Runbook finalization (1 hr)

**Friday (4 hours)**
- [ ] Smoke test execution (1 hr)
- [ ] Final checklist review (1 hr)
- [ ] Customer demo preparation (2 hr)

## Cost Estimation Worksheet

```
PROJECT_ID="tai-autonomics-prod"
REGION="us-central1"

Cloud Run Costs:
- 730 hours/month * 4 instances * 2 CPU = ???
- 730 hours/month * 4 instances * 2GB RAM = ???
- 1M requests/month * $0.40 per 1M = ???
Subtotal: $______ / month

Firestore Costs:
- 50M reads/month = $30
- 5M writes/month = $9
- 10GB storage = $1.80
Subtotal: $______ / month

Load Balancer:
- Forwarding rules: $0.30/hour = $219/month
- Data processing: $______ / month
Subtotal: $______ / month

Monitoring & Logging:
- Log ingestion: 1TB/month * $0.50/GB = $500
- Cloud Trace: $100/month
Subtotal: $600 / month

TOTAL ESTIMATED: $________ / month (~$________ / year)
```

## Success Metrics

At the end of Week 2, verify:

✅ **Availability**: 99%+ uptime
✅ **Performance**: p99 latency < 1 second
✅ **Reliability**: < 0.5% error rate
✅ **Monitoring**: Dashboards showing 24+ hours of metrics
✅ **Documentation**: All runbooks complete
✅ **Testing**: Load tests completed, baseline established
✅ **Backups**: Restore procedure tested
✅ **Team**: Ready to support Week 3 demo

## Week 3 Preparation

By Friday of Week 2, complete:

1. **Demo Environment**
   - [ ] Demo data loaded
   - [ ] Demo endpoints tested
   - [ ] Demo script prepared

2. **Team Readiness**
   - [ ] Team trained on deployment
   - [ ] On-call rotation established
   - [ ] Support procedures documented

3. **Customer Communication**
   - [ ] Demo schedule confirmed
   - [ ] Success criteria defined
   - [ ] Escalation contacts shared

4. **Contingency Planning**
   - [ ] Rollback procedures tested
   - [ ] Backup restoration practiced
   - [ ] Emergency contacts established

## Additional Resources

### Documentation Files
- `WEEK_1_2_PRODUCTION_DEPLOYMENT.md` - Comprehensive guide
- `DEPLOYMENT_RUNBOOK.md` - Step-by-step procedures
- `IMPLEMENTATION_CHECKLIST.md` - Detailed checklist
- `terraform-production.tfvars` - Configuration template
- `monitoring-alerts-setup.sh` - Monitoring script
- `firestore-schema-init.sh` - Database setup script

### GCP Documentation
- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Firestore Documentation](https://firebase.google.com/docs/firestore)
- [Cloud Load Balancer](https://cloud.google.com/load-balancing/docs)
- [Cloud Monitoring](https://cloud.google.com/monitoring/docs)
- [Cloud Logging](https://cloud.google.com/logging/docs)

### Tools
- [gcloud CLI Reference](https://cloud.google.com/sdk/gcloud)
- [Terraform GCP Provider](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Docker Documentation](https://docs.docker.com/)

## Support & Escalation

**Questions?** Check the `DEPLOYMENT_RUNBOOK.md` first.

**Issues?** Follow the troubleshooting section above.

**Blockers?** Contact the infrastructure lead immediately.

---

**Ready to deploy?** Start with "GCP Setup" above.
**Last Updated**: 2026-01-26
**Next Checkpoint**: End of Week 1 (Thursday)
