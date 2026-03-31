# TAI Erlang Autonomics - Deployment Runbook

## Quick Deployment Guide (Week 1-2)

This runbook provides step-by-step instructions for deploying the TAI Erlang Autonomics system to GCP Cloud Run.

**Estimated Duration**: 6-8 hours (includes build time)

## Prerequisites

### Required Tools
```bash
# Install gcloud CLI
curl https://sdk.cloud.google.com | bash

# Install kubectl (for Kubernetes operations if needed)
gcloud components install kubectl

# Install terraform (>= 1.0)
terraform version

# Install Docker (for local testing)
docker --version
```

### GCP Setup
```bash
# 1. Create GCP project
PROJECT_ID="tai-autonomics-prod"
gcloud projects create $PROJECT_ID
gcloud config set project $PROJECT_ID

# 2. Enable billing
gcloud billing projects link $PROJECT_ID \
  --billing-account=BILLING_ACCOUNT_ID

# 3. Authenticate
gcloud auth login
gcloud auth application-default login
```

## Step-by-Step Deployment

### Phase 1: GCP Foundation Setup (30 minutes)

#### 1.1 Enable Required APIs
```bash
PROJECT_ID="tai-autonomics-prod"
gcloud config set project $PROJECT_ID

# Enable all required APIs
gcloud services enable \
  run.googleapis.com \
  pubsub.googleapis.com \
  firestore.googleapis.com \
  cloudbuild.googleapis.com \
  artifactregistry.googleapis.com \
  secretmanager.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com \
  cloudtrace.googleapis.com \
  iam.googleapis.com \
  compute.googleapis.com \
  dns.googleapis.com

echo "✅ All APIs enabled"
```

#### 1.2 Create Service Account
```bash
# Create service account
gcloud iam service-accounts create tai-autonomics-sa \
  --display-name="TAI Autonomics Service Account"

# Set service account email
SA_EMAIL="tai-autonomics-sa@${PROJECT_ID}.iam.gserviceaccount.com"

# Grant IAM roles
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/pubsub.subscriber"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/datastore.user"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/logging.logWriter"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/monitoring.metricWriter"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/cloudtrace.agent"

gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member="serviceAccount:${SA_EMAIL}" \
  --role="roles/artifactregistry.reader"

echo "✅ Service account created with roles: $SA_EMAIL"
```

#### 1.3 Create Artifact Registry Repository
```bash
REGION="us-central1"
REGISTRY_NAME="tai-autonomics"

gcloud artifacts repositories create $REGISTRY_NAME \
  --repository-format=docker \
  --location=$REGION \
  --description="Container registry for TAI Erlang Autonomics"

# Configure Docker authentication
gcloud auth configure-docker ${REGION}-docker.pkg.dev

echo "✅ Artifact Registry repository created"
```

#### 1.4 Create GCS Bucket for Terraform State
```bash
STATE_BUCKET="tai-autonomics-terraform-state"

gsutil mb -p $PROJECT_ID -l $REGION gs://${STATE_BUCKET}

# Enable versioning for safety
gsutil versioning set on gs://${STATE_BUCKET}

echo "✅ GCS bucket for Terraform state created: gs://${STATE_BUCKET}"
```

### Phase 2: Build and Push Docker Image (45 minutes)

#### 2.1 Build Docker Image Locally (for testing)
```bash
# From repository root
cd /Users/sac/ggen/tai-erlang-autonomics

# Build image (takes 15-20 minutes for Erlang compilation)
docker build \
  -t tai-autonomics:latest \
  -f container/Containerfile \
  .

echo "✅ Docker image built successfully"

# Test locally (optional)
docker run -p 8080:8080 tai-autonomics:latest &
sleep 5
curl http://localhost:8080/health
```

#### 2.2 Tag and Push to Artifact Registry
```bash
PROJECT_ID="tai-autonomics-prod"
REGION="us-central1"
REGISTRY="tai-autonomics"
IMAGE_TAG="v1.0.0"

# Tag image for Artifact Registry
docker tag tai-autonomics:latest \
  ${REGION}-docker.pkg.dev/${PROJECT_ID}/${REGISTRY}/tai-autonomics:${IMAGE_TAG}

docker tag tai-autonomics:latest \
  ${REGION}-docker.pkg.dev/${PROJECT_ID}/${REGISTRY}/tai-autonomics:latest

# Push to Artifact Registry (takes 5-10 minutes)
docker push ${REGION}-docker.pkg.dev/${PROJECT_ID}/${REGISTRY}/tai-autonomics:${IMAGE_TAG}
docker push ${REGION}-docker.pkg.dev/${PROJECT_ID}/${REGISTRY}/tai-autonomics:latest

echo "✅ Docker image pushed to Artifact Registry"
```

#### 2.3 Verify Image in Registry
```bash
gcloud artifacts docker images list \
  --repository=tai-autonomics \
  --location=$REGION

# Get full image name for deployment
FULL_IMAGE="${REGION}-docker.pkg.dev/${PROJECT_ID}/tai-autonomics/tai-autonomics:${IMAGE_TAG}"
echo "Full image name: $FULL_IMAGE"
```

### Phase 3: Deploy Infrastructure with Terraform (30 minutes)

#### 3.1 Prepare Terraform Configuration
```bash
cd /Users/sac/ggen/tai-erlang-autonomics/terraform

# Initialize Terraform (first time only)
terraform init \
  -backend-config="bucket=tai-autonomics-terraform-state" \
  -backend-config="prefix=terraform/state"

# Validate configuration
terraform validate
```

#### 3.2 Create terraform.tfvars
```bash
cat > production.tfvars << EOF
project_id            = "$PROJECT_ID"
region                = "us-central1"
environment           = "production"
image_tag             = "v1.0.0"
container_concurrency = 80
cpu_limit             = "2"
memory_limit          = "2Gi"
min_instances         = 1
max_instances         = 10
timeout_seconds       = 300
execution_environment = "gen2"
firestore_location    = "us-central"
firestore_enabled     = true
receipt_ledger_backend = "firestore"
tracing_enabled       = true
enable_public_access  = true
EOF

echo "✅ terraform.tfvars created"
```

#### 3.3 Plan Terraform Deployment
```bash
# Plan infrastructure changes
terraform plan \
  -var-file=production.tfvars \
  -out=tfplan

# Review plan carefully before applying
echo ""
echo "📋 Review the plan above. Press Enter to continue with apply..."
read
```

#### 3.4 Apply Terraform Configuration
```bash
# Apply infrastructure (takes 5-10 minutes)
terraform apply tfplan

# Save outputs
terraform output -json > deployment-outputs.json

# Display critical outputs
echo ""
echo "📍 Deployment Outputs:"
terraform output -json | jq .

echo "✅ Infrastructure deployed successfully"
```

### Phase 4: Initialize Firestore Database (10 minutes)

#### 4.1 Run Firestore Schema Initialization
```bash
# Make script executable
chmod +x /Users/sac/ggen/tai-erlang-autonomics/execution/week-1-2/firestore-schema-init.sh

# Run schema initialization
/Users/sac/ggen/tai-erlang-autonomics/execution/week-1-2/firestore-schema-init.sh \
  $PROJECT_ID \
  "us-central1"

echo "✅ Firestore schema initialized"
```

#### 4.2 Verify Firestore Collections
```bash
# List collections
gcloud firestore collections list

# List documents in users collection
gcloud firestore documents list --collection=users

echo "✅ Firestore verified"
```

### Phase 5: Deploy Cloud Run Services (20 minutes)

#### 5.1 Deploy Pricing Engine Service
```bash
PROJECT_ID="tai-autonomics-prod"
REGION="us-central1"
IMAGE="${REGION}-docker.pkg.dev/${PROJECT_ID}/tai-autonomics/tai-autonomics:v1.0.0"
SA_EMAIL="tai-autonomics-sa@${PROJECT_ID}.iam.gserviceaccount.com"

gcloud run deploy tai-autonomics \
  --image=$IMAGE \
  --region=$REGION \
  --platform=managed \
  --memory=2Gi \
  --cpu=2 \
  --timeout=300 \
  --service-account=$SA_EMAIL \
  --set-env-vars=\
PORT=8080,\
GCP_PROJECT_ID=$PROJECT_ID,\
GCP_REGION=$REGION,\
FIRESTORE_ENABLED=true,\
TRACING_ENABLED=true,\
RECEIPT_LEDGER_BACKEND=firestore \
  --allow-unauthenticated \
  --min-instances=1 \
  --max-instances=10

echo "✅ Cloud Run service deployed"

# Get service URL
gcloud run services describe tai-autonomics \
  --region=$REGION \
  --format='value(status.url)'
```

#### 5.2 Test Health Check Endpoint
```bash
SERVICE_URL=$(gcloud run services describe tai-autonomics \
  --region=us-central1 \
  --format='value(status.url)')

echo "Testing health endpoint: $SERVICE_URL/health"

# Test health endpoint
curl -v "$SERVICE_URL/health"

echo "✅ Service health check passed"
```

### Phase 6: Setup Cloud Load Balancer (30 minutes)

#### 6.1 Create Domain DNS Entries (if not using default domain)
```bash
# Option 1: Use Cloud DNS
DOMAIN="api.pricing.example.com"

# Create DNS zone
gcloud dns managed-zones create tai-autonomics-zone \
  --dns-name="$DOMAIN" \
  --description="DNS zone for TAI Autonomics"

# Get nameservers
gcloud dns managed-zones describe tai-autonomics-zone \
  --format='value(nameServers)'

echo "⚠️  Update domain registrar with above nameservers"
```

#### 6.2 Deploy Load Balancer with Terraform
```bash
# Use the terraform-loadbalancer.tf configuration
terraform apply \
  -var-file=production.tfvars \
  -var="domain=api.pricing.example.com"

# Get Load Balancer IP
terraform output load_balancer_ip

echo "✅ Load Balancer deployed"
```

#### 6.3 Verify SSL Certificate
```bash
# Wait for certificate provisioning (5-10 minutes)
gcloud compute ssl-certificates describe tai-autonomics-cert \
  --format='value(managedStatus)'

# Check certificate details
openssl s_client -connect api.pricing.example.com:443

echo "✅ SSL/TLS certificate verified"
```

### Phase 7: Configure Monitoring and Alerting (20 minutes)

#### 7.1 Create Monitoring Dashboards
```bash
chmod +x /Users/sac/ggen/tai-erlang-autonomics/execution/week-1-2/monitoring-alerts-setup.sh

/Users/sac/ggen/tai-erlang-autonomics/execution/week-1-2/monitoring-alerts-setup.sh \
  $PROJECT_ID \
  "us-central1" \
  "tai-autonomics"

echo "✅ Monitoring dashboards created"
```

#### 7.2 Access Cloud Monitoring Console
```bash
echo ""
echo "🔗 Open Cloud Monitoring Dashboard:"
echo "https://console.cloud.google.com/monitoring/dashboards?project=$PROJECT_ID"
echo ""
echo "📊 View Logs:"
echo "https://console.cloud.google.com/logs/query?project=$PROJECT_ID"
echo ""
echo "🚨 View Alerts:"
echo "https://console.cloud.google.com/monitoring/alerting?project=$PROJECT_ID"
```

### Phase 8: Deployment Verification (15 minutes)

#### 8.1 Health Checks
```bash
# Verify Cloud Run service
gcloud run services list --region=us-central1

# Check service status
gcloud run services describe tai-autonomics --region=us-central1

# Verify logs
gcloud run services logs read tai-autonomics --region=us-central1 --limit=20

echo "✅ Cloud Run services verified"
```

#### 8.2 Database Connectivity Test
```bash
# Test Firestore access
gcloud firestore documents list --collection=users --limit=5

echo "✅ Firestore connectivity verified"
```

#### 8.3 Load Balancer Connectivity
```bash
# Test HTTPS endpoint
curl -v https://api.pricing.example.com/health

echo "✅ Load Balancer connectivity verified"
```

#### 8.4 Monitoring Verification
```bash
# Wait 5-10 minutes for metrics to appear
echo "⏳ Waiting for metrics to appear in Cloud Monitoring..."
sleep 300

# Check if metrics are flowing
gcloud monitoring metrics-descriptors list | grep run.googleapis.com

echo "✅ Monitoring metrics verified"
```

## Troubleshooting

### Docker Build Issues
```bash
# Check Docker daemon
docker version

# If build fails, try with increased memory
docker build --memory=4g ...

# View build logs
docker build --progress=plain ...
```

### Terraform Errors
```bash
# Validate configuration
terraform validate

# Check for syntax errors
terraform fmt -recursive

# Check state
terraform state list

# Refresh state (be careful!)
terraform refresh
```

### Cloud Run Deployment Issues
```bash
# Check service status
gcloud run services describe tai-autonomics --region=us-central1

# View detailed logs
gcloud logging read "resource.service=tai-autonomics" --limit=50 --format=json

# Check service account permissions
gcloud iam service-accounts get-iam-policy \
  tai-autonomics-sa@$PROJECT_ID.iam.gserviceaccount.com

# Restart service
gcloud run services update-traffic tai-autonomics --to-revisions=LATEST
```

### Firestore Issues
```bash
# Test Firestore connectivity
gcloud firestore databases describe

# Check collection access
gcloud firestore documents list --collection=users

# View Firestore indexes
gcloud firestore indexes list
```

## Post-Deployment Checklist

- [ ] All Cloud APIs enabled
- [ ] Service account created with proper roles
- [ ] Docker image pushed to Artifact Registry
- [ ] Terraform infrastructure deployed
- [ ] Firestore database initialized
- [ ] Cloud Run service deployed and healthy
- [ ] Load Balancer configured with SSL/TLS
- [ ] DNS entries configured
- [ ] Monitoring dashboards created
- [ ] Alert policies configured
- [ ] Health check endpoints responding
- [ ] Metrics appearing in Cloud Monitoring
- [ ] No errors in Cloud Logging (5+ minutes)
- [ ] SSL certificate valid and auto-renewing

## Rollback Procedures

### Rollback Cloud Run Service
```bash
# List previous revisions
gcloud run revisions list --service=tai-autonomics --region=us-central1

# Route traffic back to previous revision
gcloud run services update-traffic tai-autonomics \
  --to-revisions=tai-autonomics-[OLD-REVISION]=100 \
  --region=us-central1
```

### Rollback Infrastructure
```bash
# Restore previous Terraform state
terraform state show

# If needed, destroy and re-apply
terraform destroy -var-file=production.tfvars
terraform apply -var-file=production.tfvars
```

### Rollback Firestore Data
```bash
# Restore from backup
gcloud firestore backups restore [BACKUP_ID]

# Verify restoration
gcloud firestore documents list --collection=users
```

## Cost Monitoring

```bash
# Estimate monthly costs
gcloud compute instances list

# View billing
gcloud billing budgets list

# Export cost data
gcloud billing budgets export gs://cost-analysis-bucket/costs.csv
```

## Week 3 Readiness Sign-Off

Before Week 3 customer demo, verify:

- [ ] System uptime > 99% in Week 1-2
- [ ] All alert thresholds properly tuned
- [ ] Backup procedures tested and documented
- [ ] Team trained on deployment procedures
- [ ] Documentation complete and reviewed
- [ ] Performance baselines established
- [ ] Disaster recovery plan documented

---

**Deployment Date**: 2026-01-26
**Next Review**: Post-Week-2 validation
**Approver**: Infrastructure Lead
