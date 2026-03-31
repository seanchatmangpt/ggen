# TAIEA GCP Cloud Run Deployment Guide

Complete guide for deploying TAIEA (TAI Autonomic Erlang Application) to Google Cloud Platform Cloud Run.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Environment Setup](#environment-setup)
3. [Building Docker Image](#building-docker-image)
4. [Deploying to Cloud Run](#deploying-to-cloud-run)
5. [Verifying Deployment](#verifying-deployment)
6. [Monitoring & Logs](#monitoring--logs)
7. [Rollback Procedures](#rollback-procedures)
8. [Troubleshooting](#troubleshooting)
9. [Cost Estimation](#cost-estimation)

## Prerequisites

### Required Tools

1. **gcloud CLI** - Google Cloud SDK
   ```bash
   # Install from https://cloud.google.com/sdk/docs/install
   gcloud --version
   gcloud auth login
   gcloud auth application-default login
   ```

2. **Docker** - Container runtime
   ```bash
   # Install from https://docs.docker.com/get-docker/
   docker --version
   ```

3. **Erlang/OTP** (optional) - For local testing
   ```bash
   erl -version
   ```

### GCP Project Requirements

1. GCP Project created with billing enabled
2. The following APIs enabled:
   - Cloud Run API
   - Container Registry API (or Artifact Registry API)
   - Cloud Pub/Sub API
   - Cloud Firestore API
   - Cloud Logging API
   - Cloud Monitoring API
   - Service Usage API

3. Service account with appropriate permissions:
   ```bash
   # Create service account
   gcloud iam service-accounts create taiea-sa \
     --description="TAIEA service account"

   # Grant required roles
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member=serviceAccount:taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com \
     --role=roles/run.developer

   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member=serviceAccount:taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com \
     --role=roles/storage.admin

   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member=serviceAccount:taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com \
     --role=roles/pubsub.editor

   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member=serviceAccount:taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com \
     --role=roles/datastore.user
   ```

## Environment Setup

### 1. Clone Configuration Files

```bash
# Copy environment template
cp .env.example .env

# Edit with your GCP project details
vim .env
```

### 2. Set Environment Variables

```bash
# Source the environment file
source .env

# Verify settings
echo "Project ID: $PROJECT_ID"
echo "Region: $REGION"
echo "Image Tag: $IMAGE_TAG"

# Optional: Set in your shell profile for persistence
echo "export PROJECT_ID=$PROJECT_ID" >> ~/.bashrc
echo "export REGION=$REGION" >> ~/.bashrc
echo "export IMAGE_TAG=$IMAGE_TAG" >> ~/.bashrc
```

### 3. Verify gcloud Configuration

```bash
# Set default project
gcloud config set project $PROJECT_ID

# Verify authentication
gcloud auth list

# Check enabled APIs
gcloud services list --enabled | grep -E "run|container|pubsub|firestore"
```

## Building Docker Image

### 1. Build and Push Image

The `gcp-build-image.sh` script automates the build and push process:

```bash
# Make script executable
chmod +x tools/gcp-build-image.sh

# Build and push with default settings
./tools/gcp-build-image.sh

# Or with custom tag
IMAGE_TAG=v1.0.0 ./tools/gcp-build-image.sh

# Dry run to preview what would happen
./tools/gcp-build-image.sh --dry-run

# Verbose output for debugging
./tools/gcp-build-image.sh --verbose
```

### 2. Manual Build (if needed)

```bash
# Build locally
docker build -f tools/Dockerfile -t taiea:${IMAGE_TAG} .

# Push to Container Registry
docker tag taiea:${IMAGE_TAG} gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG}
docker push gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG}

# Push as latest
docker tag taiea:${IMAGE_TAG} gcr.io/${PROJECT_ID}/taiea:latest
docker push gcr.io/${PROJECT_ID}/taiea:latest
```

### 3. Verify Image

```bash
# List images in Container Registry
gcloud container images list --project=$PROJECT_ID

# Describe specific image
gcloud container images describe gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG}

# Get image digest
gcloud container images describe gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG} \
  --format='value(image_summary.digest)'
```

## Deploying to Cloud Run

### 1. Simulate Deployment (Recommended First Step)

```bash
# Make script executable
chmod +x tools/gcp-deploy.sh

# Run simulation
./tools/gcp-deploy.sh

# Dry run mode
./tools/gcp-deploy.sh --dry-run --verbose
```

### 2. Deploy to Cloud Run

```bash
# Deploy using the simulation script (no actual deployment)
# The script shows what would be deployed

# For actual deployment, use gcloud command:
gcloud run deploy taiea \
  --image=gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG} \
  --platform=managed \
  --region=${REGION} \
  --memory=512Mi \
  --cpu=2 \
  --timeout=300s \
  --max-instances=100 \
  --min-instances=0 \
  --set-env-vars="TAIEA_ENV=production,GCP_PROJECT_ID=${PROJECT_ID},GCP_REGION=${REGION}" \
  --no-allow-unauthenticated \
  --service-account=taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com
```

### 3. Configure Pub/Sub Integration

```bash
# Create topic
gcloud pubsub topics create taiea-signals --project=${PROJECT_ID}

# Create subscription with push endpoint
gcloud pubsub subscriptions create taiea-subscription \
  --topic=taiea-signals \
  --push-endpoint=https://taiea-xxx.a.run.app/signals \
  --push-auth-service-account=taiea-sa@${PROJECT_ID}.iam.gserviceaccount.com \
  --project=${PROJECT_ID}
```

### 4. Configure Firestore

```bash
# Create Firestore database
gcloud firestore databases create \
  --project=${PROJECT_ID} \
  --location=${REGION} \
  --type=firestore-native

# Create collections (in Firestore console or via CLI)
# - receipts: Receipt ledger
# - signals: Autonomic signals
# - entitlements: Entitlement cache
```

## Verifying Deployment

### 1. Run Verification Script

```bash
# Make script executable
chmod +x tools/gcp-verify-deployment.sh

# Run full verification
./tools/gcp-verify-deployment.sh

# Verbose output
./tools/gcp-verify-deployment.sh --verbose

# Custom service name
SERVICE_NAME=taiea-v2 ./tools/gcp-verify-deployment.sh
```

### 2. Manual Verification

```bash
# Get service URL
SERVICE_URL=$(gcloud run services describe taiea \
  --region=${REGION} \
  --format='value(status.url)' \
  --project=${PROJECT_ID})

echo "Service URL: $SERVICE_URL"

# Check health endpoint
curl -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  $SERVICE_URL/health | jq .

# Check metrics endpoint
curl -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  $SERVICE_URL/metrics | head -20

# Check service status
gcloud run services describe taiea \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

### 3. Load Testing

```bash
# Simple load test with curl
for i in {1..10}; do
  curl -s $SERVICE_URL/health > /dev/null
  echo "Request $i completed"
done

# Using Apache Bench (if installed)
ab -n 100 -c 10 -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  $SERVICE_URL/health

# Using wrk (if installed)
wrk -t4 -c10 -d30s \
  -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  $SERVICE_URL/health
```

## Monitoring & Logs

### 1. Cloud Logs

```bash
# View recent logs
gcloud logging read \
  "resource.type=cloud_run_revision AND resource.labels.service_name=taiea" \
  --limit=50 \
  --format=json \
  --project=${PROJECT_ID} | jq .

# Follow logs in real-time
gcloud alpha logging tail \
  "resource.type=cloud_run_revision AND resource.labels.service_name=taiea" \
  --project=${PROJECT_ID}

# Filter by severity
gcloud logging read \
  'resource.type=cloud_run_revision AND severity>=ERROR' \
  --limit=50 \
  --project=${PROJECT_ID}
```

### 2. Cloud Monitoring Dashboards

```bash
# Create monitoring dashboard (manual step in Cloud Console)
# https://console.cloud.google.com/monitoring/dashboards

# Key metrics to monitor:
# - cloud.run/request_latencies: Request latency distribution
# - cloud.run/request_count: Number of requests
# - cloud.run/request_count_filtered: Errors and specific status codes
# - compute.googleapis.com/instance/cpu/utilization: CPU usage
# - compute.googleapis.com/instance/memory/usage: Memory usage
```

### 3. Set Up Alerting

```bash
# Create alert policy (via Cloud Console or gcloud)
gcloud alpha monitoring policies create \
  --notification-channels=CHANNEL_ID \
  --display-name="TAIEA Error Rate Alert" \
  --condition-display-name="Error rate > 5%" \
  --condition-threshold-value=0.05 \
  --condition-threshold-duration=300s
```

## Rollback Procedures

### 1. Quick Rollback to Previous Revision

```bash
# List recent revisions
gcloud run revisions list \
  --service=taiea \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Get a specific revision's image
gcloud run revisions describe REVISION_NAME \
  --service=taiea \
  --region=${REGION} \
  --project=${PROJECT_ID} \
  --format='value(spec.template.spec.containers[0].image)'

# Deploy previous image
gcloud run deploy taiea \
  --image=gcr.io/${PROJECT_ID}/taiea:PREVIOUS_TAG \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

### 2. Instant Rollback Using Traffic Splitting

```bash
# Route traffic to specific revision
gcloud run services update-traffic taiea \
  --to-revisions=PREVIOUS_REVISION_NAME=100 \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Revert to all revisions
gcloud run services update-traffic taiea \
  --to-revisions=LATEST=100 \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

### 3. Gradual Rollback

```bash
# Canary deployment: 90/10 traffic split
gcloud run services update-traffic taiea \
  --to-revisions=STABLE_REVISION=90,NEW_REVISION=10 \
  --region=${REGION} \
  --project=${PROJECT_ID}

# Monitor errors for 5 minutes, then:

# Full rollback if issues found
gcloud run services update-traffic taiea \
  --to-revisions=STABLE_REVISION=100 \
  --region=${REGION} \
  --project=${PROJECT_ID}
```

## Troubleshooting

### Common Issues

#### 1. Docker Build Fails

```bash
# Check Dockerfile syntax
docker build -f tools/Dockerfile --dry-run .

# Build with verbose output
DOCKER_BUILDKIT=0 docker build -f tools/Dockerfile -t taiea:latest . -v

# Check available disk space
df -h

# Check Docker daemon
docker ps
docker system df
```

#### 2. Push to Registry Fails

```bash
# Check authentication
gcloud auth list
gcloud auth configure-docker gcr.io

# Check project ID
echo $PROJECT_ID
gcloud config get-value project

# Verify image exists locally
docker images | grep taiea
```

#### 3. Cloud Run Deployment Fails

```bash
# Check service account has required roles
gcloud projects get-iam-policy ${PROJECT_ID} \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:taiea-sa@"

# Check service quotas
gcloud compute project-info describe --project=${PROJECT_ID} \
  --format='value(quotas[name=CPUS].usage,quotas[name=CPUS].limit)'

# Check if service name is available
gcloud run services list --project=${PROJECT_ID} | grep taiea
```

#### 4. Health Check Failures

```bash
# Check recent logs
gcloud logging read \
  'resource.type=cloud_run_revision AND severity>=WARNING' \
  --limit=20 \
  --project=${PROJECT_ID}

# Check if health endpoint is implemented
curl -v $SERVICE_URL/health

# Check environment variables are set
gcloud run services describe taiea \
  --region=${REGION} \
  --project=${PROJECT_ID} \
  --format='value(spec.template.spec.containers[0].env)'
```

### Debug Mode Deployment

```bash
# Deploy with extended timeout and lower resource limits for debugging
gcloud run deploy taiea \
  --image=gcr.io/${PROJECT_ID}/taiea:${IMAGE_TAG} \
  --region=${REGION} \
  --timeout=3600s \
  --memory=1024Mi \
  --cpu=2 \
  --set-env-vars="TAIEA_ENV=debug,LOG_LEVEL=debug" \
  --project=${PROJECT_ID}

# Follow logs during startup
gcloud alpha logging tail \
  'resource.type=cloud_run_revision' \
  --project=${PROJECT_ID}
```

## Cost Estimation

### Cloud Run Pricing

- **Requests**: $0.40 per 1M requests (always free tier first)
- **Compute Time**: $0.00001667 per vCPU-second (2 CPU @ 512MB)
- **Memory**: $0.0000083 per GB-second (0.5 GB)

### Estimated Monthly Costs

For 1,000 requests/day (30K/month):

```
Requests: 30,000 × $0.40/1M = $0.012
Compute (2 CPU, avg 500ms): 30,000 × 0.5s × 0.00001667 × 2 = $0.50
Memory (0.5 GB): 30,000 × 0.5s × 0.0000083 × 0.5 = $0.062
Pub/Sub (if used): ~$0.05 per million messages = variable
Firestore (if used): ~$0.06 per 100K reads = variable

Estimated Base: $10-30/month
With Pub/Sub/Firestore: $20-100/month
```

### Cost Optimization Tips

1. **Resource Allocation**
   - Start with 256MB memory, 1 CPU
   - Scale up based on actual usage
   - Use `--min-instances=0` for variable workloads

2. **Revisions**
   - Delete old revisions to save storage
   - Automatic cleanup after 30 days

3. **Monitoring**
   - Use Cloud Monitoring for cost analysis
   - Set up budget alerts

## Additional Resources

- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Cloud Run Quotas & Limits](https://cloud.google.com/run/quotas)
- [Container Registry Documentation](https://cloud.google.com/container-registry/docs)
- [Cloud Pub/Sub Documentation](https://cloud.google.com/pubsub/docs)
- [Cloud Firestore Documentation](https://cloud.google.com/firestore/docs)
- [Cloud Logging Documentation](https://cloud.google.com/logging/docs)
- [Cloud Monitoring Documentation](https://cloud.google.com/monitoring/docs)

## Support & Issues

For issues or questions:

1. Check [Cloud Run Troubleshooting Guide](https://cloud.google.com/run/docs/troubleshooting)
2. Review [Cloud Run Known Issues](https://cloud.google.com/run/docs/issues)
3. Check [GCP Status Dashboard](https://status.cloud.google.com/)
4. Contact GCP Support

## Version History

- **v1.0.0** - Initial deployment guide (2026-01-26)
  - Docker build automation
  - Cloud Run deployment
  - Verification procedures
  - Monitoring setup
  - Rollback procedures
  - Troubleshooting guide
