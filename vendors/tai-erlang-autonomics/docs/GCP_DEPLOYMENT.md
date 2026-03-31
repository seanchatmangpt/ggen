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

### 0. Simulate Deployment (Recommended First Step)

To preview what the deployment would do without making actual GCP API calls:

```bash
# View what would be deployed
./tools/gcp-deploy.sh --dry-run --verbose

# Run full simulation (shows receipt)
./tools/gcp-deploy.sh
```

The simulator validates configuration, builds the image URL, and shows all steps that would be executed in Phase 2. Use this to:
- Test deployment configuration without GCP authentication
- CI/CD pipelines for validation
- Documentation and training
- Phase 1 proof-of-concept

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

## Cloud Run Deployment Simulator

The `tools/gcp-deploy.sh` script provides a simulation of the Cloud Run deployment without making actual GCP API calls. This is useful for:

- **Phase 1**: Testing deployment configuration before GCP authentication is available
- **CI/CD**: Validating deployment parameters in automated pipelines
- **Documentation**: Generating deployment receipts for documentation and training
- **Development**: Testing the deployment flow locally

### Running the Simulator

```bash
# Basic simulation (shows receipt)
./tools/gcp-deploy.sh

# Dry run with verbose output
./tools/gcp-deploy.sh --dry-run --verbose

# Custom configuration
GCP_PROJECT=my-project GCP_REGION=europe-west1 ./tools/gcp-deploy.sh
```

### Simulator Features

1. **Configuration Validation**
   - Validates GCP Project ID format
   - Checks Cloud Run region availability
   - Validates memory and CPU specifications
   - Validates timeout configuration

2. **Dockerfile Validation**
   - Checks for Dockerfile existence
   - Displays Dockerfile preview
   - Simulates multi-stage build process

3. **Deployment Simulation**
   - Shows container image building steps
   - Simulates registry push
   - Shows Cloud Run deployment commands
   - Simulates Pub/Sub setup
   - Simulates Firestore configuration
   - Simulates IAM role assignment

4. **Deployment Receipt**
   - Timestamped receipt with all configuration
   - Service URL and endpoints
   - Resource allocation details
   - Cost estimation
   - Next steps for Phase 2 (actual deployment)

### Simulator Environment Variables

```bash
# Customize simulator behavior
GCP_PROJECT=taiea-phase1       # GCP Project ID
GCP_REGION=us-central1         # GCP Region
DOCKER_REGISTRY=gcr.io         # Container registry
IMAGE_TAG=1.0.0                # Image tag
TAIEA_ENV=prod                 # Environment
MEMORY_LIMIT=512Mi             # Memory per instance
CPU_LIMIT=2                    # CPU per instance
TIMEOUT_SECONDS=300            # Request timeout

# Example: Customize deployment parameters
GCP_PROJECT=my-project \
  GCP_REGION=europe-west1 \
  MEMORY_LIMIT=1024Mi \
  CPU_LIMIT=4 \
  ./tools/gcp-deploy.sh --dry-run
```

### Dockerfile

The `tools/Dockerfile` provides a production-ready multi-stage build:

1. **Builder Stage**
   - Compiles Erlang release using rebar3
   - Installs build dependencies
   - Includes Erlang 26

2. **Runtime Stage**
   - Minimal debian:bookworm-slim base
   - Only runtime dependencies installed
   - Non-root user for security
   - Health checks configured
   - Total size: ~200-300MB

### Phase 2: Actual Deployment

When you're ready to deploy to GCP (Phase 2):

1. Authenticate with GCP:
   ```bash
   gcloud auth application-default login
   gcloud config set project taiea-phase1
   ```

2. Enable required APIs:
   ```bash
   gcloud services enable \
     cloudrun.googleapis.com \
     artifactregistry.googleapis.com \
     pubsub.googleapis.com \
     firestore.googleapis.com
   ```

3. Run the actual deployment:
   ```bash
   ./tools/gcp-deploy.sh
   ```

4. Verify service is running:
   ```bash
   curl -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
     https://taiea-xxx.a.run.app/health
   ```

## Next Steps

**Phase 1 (Current)**:
- [x] Create deployment simulator (`tools/gcp-deploy.sh`)
- [x] Create production Dockerfile (`tools/Dockerfile`)
- [x] Document deployment process
- [ ] Run simulator to validate configuration
- [ ] Collect deployment feedback

**Phase 2 (Future)**:
- [ ] Authenticate with GCP
- [ ] Set up GCP project
- [ ] Enable required APIs
- [ ] Run actual Cloud Run deployment
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
- [Docker Multi-Stage Builds](https://docs.docker.com/build/building/multi-stage/)
