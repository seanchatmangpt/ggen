# TAIEA Terraform Infrastructure Guide

## Overview

This directory contains Terraform configurations for deploying TAI Autonomics Engine (TAIEA) to Google Cloud Platform. The infrastructure includes:

- **Cloud Run**: Serverless container deployment for the TAIEA Erlang application
- **Firestore**: NoSQL database for receipt ledger and operational state
- **IAM**: Service accounts and role bindings for secure access
- **Monitoring**: Cloud Monitoring dashboards and alerting policies
- **Artifact Registry**: Container image repository

## Prerequisites

Before deploying, ensure you have:

1. **GCP Account & Project**
   ```bash
   # Create a new project or use existing
   gcloud projects create taiea-prod --name "TAI Autonomics"
   export PROJECT_ID="taiea-prod"
   gcloud config set project $PROJECT_ID
   ```

2. **Terraform Installed**
   ```bash
   # Install Terraform (requires v1.0+)
   brew install terraform
   terraform version
   ```

3. **GCP CLI Installed**
   ```bash
   # Install gcloud CLI
   brew install google-cloud-sdk
   gcloud auth login
   gcloud config set project $PROJECT_ID
   ```

4. **Service Account with Admin Permissions**
   ```bash
   # Create service account for Terraform
   gcloud iam service-accounts create terraform-admin \
     --display-name="Terraform Admin"

   # Grant admin permissions
   gcloud projects add-iam-policy-binding $PROJECT_ID \
     --member=serviceAccount:terraform-admin@$PROJECT_ID.iam.gserviceaccount.com \
     --role=roles/editor

   # Create and download key
   gcloud iam service-accounts keys create terraform-key.json \
     --iam-account=terraform-admin@$PROJECT_ID.iam.gserviceaccount.com
   ```

5. **Terraform State Bucket**
   ```bash
   # Create GCS bucket for Terraform state
   gsutil mb gs://taiea-terraform-state-$PROJECT_ID

   # Enable versioning for safety
   gsutil versioning set on gs://taiea-terraform-state-$PROJECT_ID
   ```

## File Organization

```
gcp/
├── main.tf              # Provider setup, API enablement, container registry
├── cloud-run.tf         # Cloud Run service configuration
├── firestore.tf         # Firestore database and backup policies
├── iam.tf               # Service accounts and IAM bindings
├── monitoring.tf        # Dashboards, alerting, log sinks
├── outputs.tf           # Output values for reference
├── variables.tf         # Variable definitions and validation
├── terraform.tfvars     # Configuration values (create from example)
└── TERRAFORM_GUIDE.md   # This file
```

## Quick Start (5 Minutes)

### 1. Create Configuration File

```bash
cd gcp/

# Copy the example configuration
cp terraform.tfvars.example terraform.tfvars

# Edit with your values
vim terraform.tfvars
```

Example `terraform.tfvars`:
```hcl
project_id              = "taiea-prod"
region                  = "us-central1"
firestore_location      = "us-central"
environment             = "prod"
image_tag               = "latest"

# Cloud Run settings
cpu_limit               = "2"
memory_limit            = "2Gi"
min_instances           = 0
max_instances           = 100
container_concurrency   = 80
timeout_seconds         = 300

# Features
enable_monitoring_dashboard = true
enable_log_sink            = true
enable_alerting            = true
enable_pitr               = false
enable_backups            = true
```

### 2. Initialize Terraform

```bash
# Initialize Terraform (downloads providers, validates config)
terraform init

# If using GCS backend, authenticate first
export GOOGLE_APPLICATION_CREDENTIALS="path/to/terraform-key.json"
terraform init
```

Expected output:
```
Initializing the backend...
Successfully configured the backend "gcs"!
Initializing provider plugins...
- Installed hashicorp/google v5.x.x
- Installed hashicorp/google-beta v5.x.x
- Installed hashicorp/random v3.x.x
- Installed hashicorp/local v2.x.x

Terraform has been successfully initialized!
```

### 3. Validate Configuration

```bash
# Check for syntax errors and validate against schema
terraform validate

# Format check (ensures consistent style)
terraform fmt -check

# Auto-format if needed
terraform fmt -recursive
```

### 4. Plan Deployment

```bash
# Preview what Terraform will create
terraform plan -out=tfplan

# Save plan for review
terraform plan -out=/tmp/taiea.tfplan

# Show detailed plan
terraform show /tmp/taiea.tfplan
```

Expected output shows resource creation (≈20 resources):
- 1x Cloud Run service
- 2x Firestore databases
- 1x Service account
- 5+ IAM bindings
- 1x Artifact Registry
- 4x Firestore indexes
- 2+ Monitoring dashboards
- 3+ Alert policies
- 1x BigQuery dataset

### 5. Apply Configuration

```bash
# Deploy infrastructure (requires confirmation)
terraform apply tfplan

# Or apply with automatic approval (caution!)
terraform apply -auto-approve

# Wait for completion (typically 3-5 minutes)
```

After successful apply, you'll see:

```
Apply complete! Resources added: 20, changed: 0, destroyed: 0.

Outputs:
cloud_run_service_url = "https://taiea-xxxxx-uc.a.run.app"
firestore_receipts_database_name = "taiea-receipts"
service_account_email = "taiea-sa@taiea-prod.iam.gserviceaccount.com"
gcp_project_id = "taiea-prod"
```

## Deployment Workflows

### Development Environment

```bash
terraform workspace create dev
terraform workspace select dev

# Use different tfvars
terraform plan -var-file=dev.tfvars
terraform apply -var-file=dev.tfvars
```

### Staging Environment

```bash
terraform workspace create staging
terraform workspace select staging

terraform plan -var-file=staging.tfvars
terraform apply -var-file=staging.tfvars
```

### Production Environment (with Approvals)

```bash
terraform workspace create prod
terraform workspace select prod

# Always review plan first
terraform plan -var-file=prod.tfvars -out=tfplan

# Manual review by ops team
cat tfplan  # Review changes

# Apply after approval
terraform apply tfplan
```

## Updating Cloud Run Image

When deploying a new container image version:

```bash
# Build and push new image
docker build -t taiea:v1.2.3 .
docker push us-central1-docker.pkg.dev/taiea-prod/taiea-registry/taiea:v1.2.3

# Update image tag in Terraform
terraform apply -var="image_tag=v1.2.3"

# Or update terraform.tfvars
echo 'image_tag = "v1.2.3"' >> terraform.tfvars
terraform apply
```

## Verification Steps

### 1. Verify Cloud Run Service

```bash
# Check service status
gcloud run services describe taiea --region=us-central1

# Get service URL
terraform output cloud_run_service_url

# Test health endpoint
curl https://$(terraform output -raw cloud_run_service_url)/health/ready
```

### 2. Verify Firestore Database

```bash
# List databases
gcloud firestore databases list

# Check database size
gcloud firestore databases describe taiea-receipts

# View Firestore console
echo "https://console.cloud.google.com/firestore/databases"
```

### 3. Verify IAM Bindings

```bash
# Check service account permissions
gcloud projects get-iam-policy $PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:taiea-sa@*"

# List service account keys
gcloud iam service-accounts keys list \
  --iam-account=taiea-sa@$PROJECT_ID.iam.gserviceaccount.com
```

### 4. Verify Monitoring Setup

```bash
# Check alert policies created
gcloud alpha monitoring policies list

# View dashboards
echo "https://console.cloud.google.com/monitoring/dashboards"

# Check log sink
gcloud logging sinks list
```

### 5. Test Cloud Run Invocation

```bash
# Call Cloud Run service with auth
curl -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  https://$(terraform output -raw cloud_run_service_url)/api/receipts

# Call with public access (if enabled)
curl https://$(terraform output -raw cloud_run_service_url)/api/receipts
```

## Common Operations

### Scale Cloud Run

```bash
# Update max instances
terraform apply -var="max_instances=200"

# Increase CPU/memory
terraform apply -var="cpu_limit=4" -var="memory_limit=4Gi"

# Adjust concurrency
terraform apply -var="container_concurrency=120"
```

### Enable Features

```bash
# Enable Point-in-Time Recovery
terraform apply -var="enable_pitr=true"

# Enable public access
terraform apply -var="enable_public_access=true"

# Enable Pub/Sub integration
terraform apply -var="enable_pubsub=true"
```

### Manage Alerting

```bash
# Set notification channels (requires setup first)
# Create channels in Cloud Console, then get IDs
CHANNEL_ID=$(gcloud alpha monitoring channels list --format='value(name)' | head -1)

terraform apply -var="alert_notification_channels=[$CHANNEL_ID]"
```

### Destroy Infrastructure

```bash
# WARNING: This deletes all resources!

# Preview what will be deleted
terraform plan -destroy

# Destroy resources
terraform destroy

# Destroy specific resources
terraform destroy -target=google_cloud_run_service.taiea
```

## Troubleshooting

### API Not Enabled Error

```
Error: Error enabling service: googleapi: Error 403:
The caller does not have permission to enable services
```

**Solution**: Ensure your service account has Editor or Service Usage Admin role.

### Firestore Database Already Exists

```
Error: Error creating Firestore database: googleapi: Error 409:
A Firestore database already exists for the project
```

**Solution**: Either delete the existing database or import it into Terraform state:

```bash
# Import existing database
terraform import google_firestore_database.receipts projects/$PROJECT_ID/databases/taiea-receipts
```

### Cloud Run Service Name Already Taken

```
Error: Error creating service: googleapi: Error 409:
The Cloud Run service already exists
```

**Solution**: Import existing service:

```bash
terraform import google_cloud_run_service.taiea us-central1/taiea
```

### Permission Denied on GCS State Bucket

```
Error: error reading gs://bucket/path: googleapi: Error 403:
Access Denied
```

**Solution**: Ensure your credentials have GCS read/write permissions:

```bash
gcloud projects add-iam-policy-binding $PROJECT_ID \
  --member=serviceAccount:terraform-admin@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/storage.objectAdmin
```

### State Lock Timeout

If Terraform state is locked from a crashed run:

```bash
# Unlock state (caution: only if you're sure no one else is applying)
terraform force-unlock LOCK_ID

# Or manually clear lock
gcloud storage objects delete gs://bucket/path/.terraform.lock.hcl
```

## Cost Management

### Estimate Costs

```bash
# Use Terraform cost estimation
terraform plan | grep -E "^[+~-]"

# Use GCP cost calculator
echo "https://cloud.google.com/products/calculator"
```

### Cost-Saving Tips

1. **Reduce min_instances to 0** (serverless scaling)
   ```hcl
   min_instances = 0
   ```

2. **Use smaller CPU/memory in dev**
   ```hcl
   cpu_limit   = "1"
   memory_limit = "512Mi"
   ```

3. **Disable unnecessary features**
   ```hcl
   enable_pitr               = false
   enable_backups            = false
   enable_monitoring_dashboard = false
   ```

4. **Use reserved instances for prod** (requires GCP Console setup)

## Security Best Practices

### 1. Terraform State Security

```bash
# Enable versioning on state bucket
gsutil versioning set on gs://taiea-terraform-state

# Enable uniform bucket-level access
gsutil uniformbucketlevelaccess set on gs://taiea-terraform-state

# Restrict state access to admins only
gcloud storage buckets add-iam-policy-binding gs://taiea-terraform-state \
  --member=group:terraform-admins@company.com \
  --role=roles/storage.objectAdmin
```

### 2. Service Account Security

```bash
# Limit service account permissions (principle of least privilege)
# Don't grant Editor role; use specific roles instead

# For Cloud Run service:
# - roles/datastore.user (Firestore)
# - roles/logging.logWriter
# - roles/monitoring.metricWriter
# - roles/cloudtrace.agent

# Audit service account usage
gcloud logging read "protoPayload.authenticationInfo.principalEmail=taiea-sa@*" \
  --limit=50 --format=json
```

### 3. Access Control

```bash
# Restrict Cloud Run to authenticated users only
terraform apply -var="enable_public_access=false"

# Or restrict to specific service accounts
terraform apply -var="allowed_invoker_service_accounts=[\"api@mycompany.iam.gserviceaccount.com\"]"
```

### 4. Secrets Management

If using secrets:

```bash
# Create secrets in Secret Manager
gcloud secrets create taiea-db-password \
  --data-file=- <<< "your-secret-password"

# Grant service account access
gcloud secrets add-iam-policy-binding taiea-db-password \
  --member=serviceAccount:taiea-sa@$PROJECT_ID.iam.gserviceaccount.com \
  --role=roles/secretmanager.secretAccessor

# Enable in Terraform
terraform apply -var="enable_secrets=true"
```

## Monitoring & Alerting

### View Logs

```bash
# Stream Cloud Run logs
gcloud logging read "resource.type=cloud_run_revision AND resource.labels.service_name=taiea" \
  --limit=50 --format=json

# View in Cloud Console
echo "https://console.cloud.google.com/logs/query"
```

### Monitor Performance

```bash
# View Cloud Monitoring dashboard
terraform output monitoring_dashboard_url

# Or open in browser
open "https://console.cloud.google.com/monitoring/dashboards"
```

### Check Firestore Usage

```bash
# View Firestore database stats
gcloud firestore databases describe taiea-receipts --format="table(name, type, location_id, create_time)"

# Check backup status
gcloud firestore backups list
```

## Advanced Topics

### Using Workspaces for Multi-Environment

```bash
# Create workspace for each environment
terraform workspace new dev
terraform workspace new staging
terraform workspace new prod

# Use different variables per workspace
terraform workspace select dev
terraform apply -var-file=dev.tfvars

terraform workspace select prod
terraform apply -var-file=prod.tfvars
```

### Remote Runs with Terraform Cloud

```bash
# Authenticate with Terraform Cloud
terraform login

# Configure backend
terraform init # Choose Terraform Cloud backend

# Run remotely
terraform plan # Runs on Terraform Cloud servers
```

### Import Existing Infrastructure

```bash
# If infrastructure already exists, import it
terraform import google_cloud_run_service.taiea us-central1/taiea
terraform import google_firestore_database.receipts projects/$PROJECT_ID/databases/taiea-receipts
terraform import google_service_account.taiea projects/$PROJECT_ID/serviceAccounts/taiea-sa@*
```

## Support & Documentation

- **GCP Terraform Provider**: https://registry.terraform.io/providers/hashicorp/google/latest/docs
- **Cloud Run Docs**: https://cloud.google.com/run/docs
- **Firestore Docs**: https://cloud.google.com/firestore/docs
- **Terraform Docs**: https://www.terraform.io/docs

## Receipt Summary

```
TERRAFORM CONFIGURATION SUMMARY
================================

Files Created:
✓ gcp/main.tf           - Provider setup, API enablement
✓ gcp/cloud-run.tf      - Cloud Run deployment
✓ gcp/firestore.tf      - Firestore databases & backups
✓ gcp/iam.tf            - Service accounts & IAM bindings
✓ gcp/monitoring.tf     - Dashboards, alerts, log sinks
✓ gcp/variables.tf      - Configuration variables
✓ gcp/outputs.tf        - Output values
✓ gcp/TERRAFORM_GUIDE.md - This guide

Resources to Deploy:
- 1x Cloud Run Service
- 2x Firestore Databases
- 1x Service Account
- 8x IAM Bindings
- 1x Artifact Registry Repository
- 4x Firestore Composite Indexes
- 2x Firestore Backup Schedules
- 1x BigQuery Dataset (for logs)
- 1x Monitoring Dashboard
- 3x Alert Policies
- 1x Log Sink

Next Steps:
1. Run: cd gcp/
2. Copy terraform.tfvars.example to terraform.tfvars
3. Edit terraform.tfvars with your project details
4. Run: terraform init
5. Run: terraform plan
6. Run: terraform apply
```
