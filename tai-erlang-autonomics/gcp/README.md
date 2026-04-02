# TAIEA Terraform Infrastructure

Terraform configuration for TAI Autonomics Engine (TAIEA) on Google Cloud Platform.

## Quick Start

```bash
# 1. Navigate to directory
cd gcp/

# 2. Create configuration
cp terraform.tfvars.example terraform.tfvars
vim terraform.tfvars

# 3. Initialize
terraform init

# 4. Plan
terraform plan -out=tfplan

# 5. Apply
terraform apply tfplan
```

## Files Overview

- **main.tf** - Provider setup, API enablement, container registry
- **cloud-run.tf** - Cloud Run serverless service
- **firestore.tf** - Firestore NoSQL databases
- **iam.tf** - Service accounts and IAM roles
- **monitoring.tf** - Cloud Monitoring dashboards and alerts
- **variables.tf** - Configuration variables
- **outputs.tf** - Output values

## Documentation

- **TERRAFORM_GUIDE.md** - Complete deployment guide (820 lines)
- **DEPLOYMENT_CHECKLIST.md** - Step-by-step verification checklist
- **RECEIPT.md** - Deployment receipt with resource summary

## What Gets Deployed

- 1x Cloud Run service (taiea)
- 2x Firestore databases (receipts & operations)
- 1x Service account with IAM bindings
- 1x Artifact Registry for container images
- 1x Cloud Monitoring dashboard (7 widgets)
- 3x Alert policies
- 1x BigQuery dataset for logs
- Automated weekly backups

## Key Commands

```bash
# Initialize Terraform
terraform init

# Validate configuration
terraform validate

# Plan deployment
terraform plan -out=tfplan

# Apply configuration
terraform apply tfplan

# View outputs
terraform output

# Destroy resources (WARNING)
terraform destroy
```

## Configuration

Edit `terraform.tfvars`:

```hcl
project_id         = "your-gcp-project"
region             = "us-central1"
environment        = "dev"  # or staging, prod
image_tag          = "latest"
```

## Next Steps

See **DEPLOYMENT_CHECKLIST.md** for:
1. Pre-deployment setup
2. Initialization steps
3. Validation procedures
4. Post-deployment verification

## Support

For detailed troubleshooting, see:
- TERRAFORM_GUIDE.md (Troubleshooting section)
- DEPLOYMENT_CHECKLIST.md (Common Issues)
- GCP Console for service logs

## Status

✅ Configuration ready for deployment
✅ All resources defined
✅ Security baseline met
✅ Documentation complete

Next: Run `terraform init` to begin deployment.
