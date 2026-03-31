# TAIEA Terraform Deployment Checklist

**Version**: 1.0
**Status**: Ready for Deployment
**Last Updated**: 2026-01-26

---

## Pre-Deployment Checklist

### GCP Project Setup

- [ ] GCP project created and billing enabled
- [ ] Project ID noted: `_________________________`
- [ ] GCP CLI installed (`gcloud --version`)
- [ ] GCP authentication configured (`gcloud auth login`)
- [ ] Correct project selected (`gcloud config set project PROJECT_ID`)

### Terraform Setup

- [ ] Terraform v1.0+ installed (`terraform version`)
- [ ] Terraform working directory: `/Users/sac/ggen/tai-erlang-autonomics/gcp/`
- [ ] All `.tf` files present (7 files: main, cloud-run, firestore, iam, monitoring, outputs, variables)
- [ ] All documentation files present (TERRAFORM_GUIDE.md, RECEIPT.md, this file)

### Service Account & Credentials

- [ ] Service account created for Terraform
- [ ] Service account ID: `_________________________`
- [ ] Service account email: `_________________________`
- [ ] Editor role granted to service account
- [ ] Service account key downloaded (JSON file)
- [ ] Key file stored securely and not committed to git

### Terraform State Setup

- [ ] GCS bucket created for state (`gs://taiea-terraform-state`)
- [ ] Bucket versioning enabled
- [ ] Bucket uniform access enabled
- [ ] Bucket permissions restricted (only admins can access)
- [ ] State backend configured in `main.tf`

### Configuration Preparation

- [ ] `terraform.tfvars` created (copied from example)
- [ ] `project_id` set in tfvars: `_________________________`
- [ ] `region` set in tfvars: `_________________________`
- [ ] `environment` set in tfvars (dev/staging/prod): `_________________________`
- [ ] `image_tag` reviewed and set: `_________________________`
- [ ] All required variables filled in
- [ ] terraform.tfvars added to .gitignore

---

## Initialization Phase

### Step 1: Initialize Terraform

```bash
cd /Users/sac/ggen/tai-erlang-autonomics/gcp/

# Set credentials
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/terraform-key.json"

# Initialize
terraform init
```

**Expected Output**:
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

**Checklist**:
- [ ] No errors in terraform init output
- [ ] Backend successfully configured
- [ ] All providers installed
- [ ] `.terraform` directory created

### Step 2: Validate Configuration

```bash
# Syntax validation
terraform validate

# Format check
terraform fmt -check

# Auto-format if needed
terraform fmt -recursive
```

**Expected Output**:
```
Success! The configuration is valid.
```

**Checklist**:
- [ ] terraform validate passes
- [ ] No syntax errors
- [ ] No formatting issues

---

## Planning Phase

### Step 3: Generate Terraform Plan

```bash
# Create detailed plan
terraform plan -out=tfplan

# For detailed review
terraform show tfplan | less
```

**Expected Output Summary**:
```
Plan: 20 to add, 0 to change, 0 to destroy.
```

**Checklist**:
- [ ] Plan generated successfully
- [ ] No errors in plan output
- [ ] Resource count reasonable (20-30 resources)
- [ ] Expected resources visible:
  - [ ] google_cloud_run_service
  - [ ] google_firestore_database (2x)
  - [ ] google_service_account
  - [ ] google_project_service (15x APIs)
  - [ ] google_monitoring_alert_policy
  - [ ] google_logging_project_sink

### Step 4: Review Plan Details

```bash
# Show formatted plan
terraform show tfplan

# Search for specific resources
terraform show tfplan | grep "resource.type"
```

**Key Resources to Verify**:
- [ ] Cloud Run service name is "taiea"
- [ ] Firestore database location matches `var.firestore_location`
- [ ] Service account email is "taiea-sa@..."
- [ ] All API services being enabled
- [ ] Alert policies being created (high error rate, high latency)

### Step 5: Estimate Costs

```bash
# Review resource specifications
terraform show tfplan | grep -E "(cpu_limit|memory_limit|min_instances|max_instances)"

# Use GCP calculator for accurate estimates
# https://cloud.google.com/products/calculator
```

**Checklist**:
- [ ] Resource sizes appropriate for environment
- [ ] Estimated monthly cost acceptable
- [ ] Dev: ~$2-8/month, Prod: ~$200-500/month

---

## Deployment Phase

### Step 6: Deploy Infrastructure

```bash
# Apply Terraform configuration
terraform apply tfplan

# Monitor progress (should take 3-5 minutes)
```

**Expected Progress**:
```
Applying ...
google_project_service.required_apis["run.googleapis.com"]: Creating...
google_project_service.required_apis["firestore.googleapis.com"]: Creating...
google_artifact_registry_repository.taiea_registry: Creating...
...
Apply complete! Resources added: 20, changed: 0, destroyed: 0.
```

**Checklist**:
- [ ] No errors during apply
- [ ] All resources created successfully
- [ ] Apply completed in < 10 minutes
- [ ] Output shows deployed resources

### Step 7: Verify Deployment

```bash
# Display outputs
terraform output

# Or individual outputs
terraform output cloud_run_service_url
terraform output firestore_receipts_database_name
terraform output service_account_email
```

**Expected Outputs**:
- `cloud_run_service_url`: `https://taiea-xxxxx-uc.a.run.app`
- `firestore_receipts_database_name`: `taiea-receipts`
- `service_account_email`: `taiea-sa@project.iam.gserviceaccount.com`
- `gcp_project_id`: Your project ID

**Checklist**:
- [ ] All outputs displayed
- [ ] URLs are properly formatted
- [ ] No null values in outputs

---

## Post-Deployment Validation

### Cloud Run Verification

```bash
# Get service details
gcloud run services describe taiea --region=us-central1

# Check service is running
gcloud run services list
```

**Checklist**:
- [ ] Service name is "taiea"
- [ ] Status shows "Service is running"
- [ ] Service URL is accessible
- [ ] Region matches configuration

### Firestore Verification

```bash
# List databases
gcloud firestore databases list

# Describe specific database
gcloud firestore databases describe taiea-receipts

# View collections (will be empty initially)
```

**Checklist**:
- [ ] `taiea-receipts` database exists
- [ ] `taiea-operations` database exists
- [ ] Both in correct location
- [ ] Mode is "Native"

### Service Account Verification

```bash
# Check service account exists
gcloud iam service-accounts list | grep taiea-sa

# View IAM roles
gcloud projects get-iam-policy $PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:taiea-sa@*"
```

**Checklist**:
- [ ] Service account exists
- [ ] Service account email correct
- [ ] Firestore roles granted
- [ ] Logging roles granted
- [ ] Monitoring roles granted

### Health Endpoint Test

```bash
# Get Cloud Run URL
CLOUD_RUN_URL=$(terraform output -raw cloud_run_service_url)

# Test health endpoint (may return 404 until service configured)
curl -v $CLOUD_RUN_URL/health/ready
```

**Expected**:
- Status 200 if health checks implemented
- Status 404 if not yet implemented (expected initially)

**Checklist**:
- [ ] Endpoint responds (any status code)
- [ ] No timeout or connection errors
- [ ] HTTPS connection successful

---

## Configuration Verification

### Cloud Monitoring Dashboard

```bash
# Get dashboard URL
terraform output monitoring_dashboard_url

# Or navigate to:
# https://console.cloud.google.com/monitoring/dashboards
```

**Checklist**:
- [ ] Dashboard exists
- [ ] Dashboard has 7+ widgets
- [ ] Widgets showing data (or pending data)

### Alerting Policies

```bash
# List alert policies
gcloud alpha monitoring policies list --format=json | jq '.[] | .displayName'
```

**Expected Alert Policies**:
- [ ] "TAIEA High Error Rate"
- [ ] "TAIEA High Latency"
- [ ] "TAIEA Instance Crash"

**Checklist**:
- [ ] All alert policies created
- [ ] Policies in "enabled" state
- [ ] Notification channels configured (if provided)

### Log Sink

```bash
# List log sinks
gcloud logging sinks list

# Check sink destination
gcloud logging sinks describe taiea-cloud-run-logs-sink
```

**Checklist**:
- [ ] Sink "taiea-cloud-run-logs-sink" exists
- [ ] Destination is BigQuery dataset
- [ ] BigQuery dataset "taiea_logs" exists

---

## Configuration Adjustment Phase

### Update for Production

If deploying to production, run:

```bash
# Update configuration
terraform apply -var="environment=prod" \
                 -var="min_instances=2" \
                 -var="max_instances=200" \
                 -var="enable_pitr=true" \
                 -var="enable_alerting=true"
```

**Checklist**:
- [ ] Min instances increased for availability
- [ ] Max instances appropriate for expected load
- [ ] PITR enabled for data recovery
- [ ] Alerting enabled with notification channels

### Add Notification Channels

```bash
# Create email notification channel
gcloud alpha monitoring channels create \
  --display-name="TAIEA Ops Email" \
  --type=email \
  --channel-labels=email_address=ops@company.com

# Get channel ID
CHANNEL_ID=$(gcloud alpha monitoring channels list \
  --format='value(name)' --filter='displayName=TAIEA*' | head -1)

# Update Terraform
terraform apply -var="alert_notification_channels=[$CHANNEL_ID]"
```

**Checklist**:
- [ ] Notification channel created
- [ ] Channel ID obtained
- [ ] Alerts configured with channel

---

## Documentation & Handoff

### State Management

```bash
# Save state reference
terraform state list > state-resources.txt

# Backup state locally (for reference only)
cp terraform.tfstate terraform.tfstate.backup

# Verify state is in GCS
gsutil ls gs://taiea-terraform-state/gcp/
```

**Checklist**:
- [ ] State saved in GCS
- [ ] State backup created locally
- [ ] State access restricted

### Documentation Review

```bash
# Review guide
cat TERRAFORM_GUIDE.md

# Review receipt
cat RECEIPT.md

# Review checklist (this file)
cat DEPLOYMENT_CHECKLIST.md
```

**Checklist**:
- [ ] TERRAFORM_GUIDE.md reviewed
- [ ] RECEIPT.md reviewed
- [ ] Deployment checklist completed
- [ ] All procedures documented

### Handoff to Next Agent

Provide to Agent 8 (Deployment):
- [ ] Terraform state files (in GCS)
- [ ] Configuration values (terraform.tfvars)
- [ ] Service account credentials
- [ ] Deployment checklist (completed)
- [ ] Cloud Run service URL
- [ ] Firestore database names
- [ ] Alert policy IDs

---

## Rollback Procedures

### If Deployment Fails

```bash
# 1. Check for errors
terraform show

# 2. Review most recent operation
gcloud logging read "resource.type=gce_operation" --limit=10

# 3. Fix configuration
vim terraform.tfvars  # Update variables

# 4. Re-apply
terraform apply

# 5. If still failing, rollback
terraform destroy  # WARNING: Deletes resources
```

**Checklist**:
- [ ] Error identified
- [ ] Root cause addressed
- [ ] Configuration corrected
- [ ] Deployment retried

### If Resources Need Deletion

```bash
# Preview deletion
terraform plan -destroy

# Destroy specific resource
terraform destroy -target=google_cloud_run_service.taiea

# Destroy all resources
terraform destroy  # WARNING: Deletes everything
```

**WARNING**: This action is irreversible!

**Checklist**:
- [ ] Backup created
- [ ] Data exported (if needed)
- [ ] Confirmation obtained
- [ ] Destruction command run

---

## Success Criteria

### All Checks Completed

- [ ] Terraform init successful
- [ ] Terraform validate passed
- [ ] Terraform plan reviewed
- [ ] Terraform apply completed
- [ ] All 20 resources created
- [ ] Cloud Run service running
- [ ] Firestore databases accessible
- [ ] IAM roles properly assigned
- [ ] Health endpoints responding
- [ ] Monitoring dashboard populated
- [ ] Alert policies created
- [ ] Log sink configured

### Infrastructure Ready for Next Phase

- [ ] Cloud Run service URL: `_________________________`
- [ ] Firestore database ID: `_________________________`
- [ ] Service account email: `_________________________`
- [ ] All documentation reviewed
- [ ] Credentials secured
- [ ] State backed up

### Deployment Status: ✅ COMPLETE

Once all items checked:

1. Commit completed checklist to git
2. Notify Agent 8 (Deployment) that infrastructure ready
3. Provide outputs and credentials
4. Begin CI/CD pipeline setup (Agent 9)

---

## Support & Escalation

### Common Issues

| Issue | Solution | Link |
|-------|----------|------|
| API not enabled | Grant Service Usage Admin role | Troubleshooting Guide |
| Permission denied | Increase service account permissions | IAM Documentation |
| Resource quota exceeded | Request quota increase in GCP Console | GCP Quotas |
| Service account key error | Create new key with correct permissions | Service Account Setup |

### Getting Help

- **Terraform Issues**: See TERRAFORM_GUIDE.md "Troubleshooting"
- **GCP Issues**: Use `gcloud logs read` for detailed error logs
- **Permission Issues**: Check service account IAM roles
- **Cost Issues**: Review deployment in GCP Pricing Calculator

---

**Checklist Version**: 1.0
**Last Updated**: 2026-01-26
**Next Review**: After first production deployment

---

**Sign-Off** (after completion):

Agent/User: ___________________________
Date Completed: ___________________________
Environment: ___________________________
Project ID: ___________________________
