# GCP Deployment Guide

This guide covers deploying TAI Erlang Autonomics to Google Cloud Platform (GCP) using Cloud Run, Pub/Sub, and Firestore.

## Prerequisites

### Required Tools

- **Erlang/OTP 26+** - Runtime environment
- **Rebar3 3.20+** - Build tool
- **Docker** - Containerization
- **Terraform 1.0+** - Infrastructure as Code
- **gcloud CLI** - GCP command-line tool

### GCP Setup

1. **Create a GCP Project**
   ```bash
   gcloud projects create your-project-id --name="TAI Autonomics"
   gcloud config set project your-project-id
   ```

2. **Enable Billing**
   ```bash
   gcloud beta billing projects link your-project-id --billing-account=BILLING_ACCOUNT_ID
   ```

3. **Authenticate**
   ```bash
   gcloud auth login
   gcloud auth application-default login
   ```

## Quick Start

### 1. Verify GCP Readiness

```bash
./scripts/gcp-ready.sh
```

This script checks:
- Required tools are installed
- GCP authentication is configured
- Erlang code compiles
- Docker image builds
- Terraform configuration is valid
- Required GCP APIs are enabled

### 2. Build and Test Locally

```bash
# Build Erlang release
make build

# Run tests
make test

# Build Docker image
make docker-build

# Run locally with Docker Compose
docker-compose up -d
```

**Note**: The Firestore emulator uses port 8081 to avoid conflicts with the main service (port 8080).

### 3. Configure Terraform

```bash
# Copy example variables
cp terraform/terraform.tfvars.example terraform/terraform.tfvars

# Edit terraform/terraform.tfvars with your values
# Required: project_id, region, environment
```

### 4. Deploy to GCP

```bash
# Initialize Terraform
make terraform-init

# Review deployment plan
make terraform-plan

# Deploy infrastructure
make gcp-deploy
```

This will:
1. Build Docker image locally
2. Push container image to Artifact Registry
3. Create Cloud Run service
4. Set up Pub/Sub topic and subscription
5. Configure Firestore database
6. Set up IAM roles and service accounts
7. Configure monitoring and alerting

### 5. Verify Deployment

```bash
# Get service URL
cd terraform && terraform output cloud_run_service_url

# Test health endpoint
curl $(cd terraform && terraform output -raw cloud_run_service_url)/health

# Run integration tests
make gcp-test
```

## Architecture

### Components

- **Cloud Run**: Serverless container hosting
- **Pub/Sub**: Event messaging for autonomic signals
- **Firestore**: Receipt ledger storage
- **Artifact Registry**: Container image storage
- **Cloud Monitoring**: Metrics and alerting
- **IAM**: Service account and permissions

### Networking

- Cloud Run service is accessible via HTTPS
- Pub/Sub uses push subscriptions
- Firestore uses native mode with regional location

## Configuration

### Environment Variables

The application reads configuration from environment variables:

- `PORT`: HTTP server port (default: 8080)
- `GCP_PROJECT_ID`: GCP project ID
- `GCP_REGION`: GCP region
- `PUBSUB_SUBSCRIPTION`: Pub/Sub subscription name
- `RECEIPT_LEDGER_BACKEND`: Backend type (ets, gcs, bigquery)
- `TRACING_ENABLED`: Enable OpenTelemetry tracing

### Terraform Variables

Key variables in `terraform/terraform.tfvars`:

- `project_id`: GCP project ID (required)
- `region`: GCP region (default: us-central1)
- `environment`: Environment name (dev, staging, prod)
- `image_tag`: Container image tag (default: latest)
- `min_instances`: Minimum Cloud Run instances (default: 0)
- `max_instances`: Maximum Cloud Run instances (default: 10)
- `cpu_limit`: CPU limit per container (default: 2)
- `memory_limit`: Memory limit per container (default: 2Gi)

## Monitoring

### Health Checks

The service exposes a `/health` endpoint that:
- Returns 200 OK with `{"status": "ok"}` when all dependencies are healthy
- Returns 503 Service Unavailable with `{"status": "unavailable"}` when dependencies are unavailable
- Checks supervisor processes (governance_sup, receipt_ledger_sup)
- Verifies Firestore connectivity (if enabled)
- Used by Cloud Run for liveness and readiness probes

### Metrics

Prometheus metrics are exported at `/metrics`:
- HTTP request counts and latencies
- Pub/Sub message processing metrics
- Governor state transitions
- Receipt ledger operations

### Logging

Structured JSON logs are sent to Cloud Logging:
- Request logs with trace IDs
- Error logs with stack traces
- Receipt logs for audit trail

### Alerting

Alert policies are configured for:
- Health check failures
- High error rates
- Pub/Sub message processing failures
- Firestore connectivity issues

## Security

### Service Account

The Cloud Run service uses a dedicated service account with minimal permissions:
- `roles/pubsub.subscriber` - Read from Pub/Sub
- `roles/datastore.user` - Access Firestore
- `roles/logging.logWriter` - Write logs
- `roles/monitoring.metricWriter` - Write metrics
- `roles/cloudtrace.agent` - Write traces

### Access Control

- Public access: Disabled by default
- Authenticated access: Enabled by default
- IAM bindings: Managed via Terraform

### Secrets

Sensitive configuration should be stored in Secret Manager:
```bash
# Create secret
gcloud secrets create api-key --data-file=api-key.txt

# Grant access to service account
gcloud secrets add-iam-policy-binding api-key \
  --member="serviceAccount:tai-autonomics-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/secretmanager.secretAccessor"
```

## Troubleshooting

### Container Build Failures

```bash
# Check Docker build logs
docker build -f container/Containerfile -t tai-autonomics:test . --no-cache

# Verify Erlang release builds locally
rebar3 release
```

### Terraform Errors

```bash
# Validate Terraform configuration
cd terraform && terraform validate

# Check for API enablement
gcloud services list --enabled --project=PROJECT_ID
```

### Cloud Run Deployment Issues

```bash
# Check service logs
gcloud run services logs read tai-autonomics --region=REGION --limit=50

# Check service status
gcloud run services describe tai-autonomics --region=REGION
```

### Integration Test Failures

```bash
# Verify service URL
cd terraform && terraform output cloud_run_service_url

# Test health endpoint manually
curl https://SERVICE_URL/health

# Check service account permissions
gcloud projects get-iam-policy PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:tai-autonomics-sa@*"
```

## CI/CD

GitHub Actions workflow (`/.github/workflows/gcp-deploy.yml`) automates:
- Building and testing
- Container image building and pushing
- Terraform validation and deployment
- Integration testing

### Manual Deployment

```bash
# Build and push image
make docker-build
docker tag tai-autonomics:latest \
  REGION-docker.pkg.dev/PROJECT_ID/ARTIFACT_REGISTRY/tai-autonomics:latest
docker push REGION-docker.pkg.dev/PROJECT_ID/ARTIFACT_REGISTRY/tai-autonomics:latest

# Deploy with Terraform
make terraform-apply
```

## Cost Estimation

Estimated monthly costs (us-central1):

- **Cloud Run**: $0-50 (pay per request, 2 vCPU, 2GB RAM)
- **Pub/Sub**: $0-10 (1M messages/month)
- **Firestore**: $0-25 (reads/writes, storage)
- **Artifact Registry**: $0-5 (storage)
- **Cloud Monitoring**: $0-10 (metrics, logs)

**Total**: ~$0-100/month for low-medium traffic

## Next Steps

- [ ] Set up CI/CD pipeline
- [ ] Configure custom domain
- [ ] Set up backup and disaster recovery
- [ ] Implement blue-green deployments
- [ ] Add performance testing
- [ ] Set up cost monitoring and alerts

## References

- [Cloud Run Documentation](https://cloud.google.com/run/docs)
- [Pub/Sub Documentation](https://cloud.google.com/pubsub/docs)
- [Firestore Documentation](https://cloud.google.com/firestore/docs)
- [Terraform GCP Provider](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
