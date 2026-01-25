<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Acceptance Testing & Deployment Verification Guide](#acceptance-testing--deployment-verification-guide)
  - [📋 Overview](#-overview)
  - [⏱️ Pre-Deployment Checklist (10 Items)](#-pre-deployment-checklist-10-items)
  - [🚀 Deployment Stage 1: Infrastructure (Terraform)](#-deployment-stage-1-infrastructure-terraform)
  - [✅ Acceptance Test 1: Infrastructure Verification](#-acceptance-test-1-infrastructure-verification)
    - [1.1 Verify All Terraform Resources Created](#11-verify-all-terraform-resources-created)
    - [1.2 Verify No Drift](#12-verify-no-drift)
    - [1.3 Verify Resource Properties](#13-verify-resource-properties)
    - [✅ Acceptance Criteria](#-acceptance-criteria)
  - [✅ Acceptance Test 2: Cloud Build Pipeline](#-acceptance-test-2-cloud-build-pipeline)
    - [2.1 Verify Trigger Created](#21-verify-trigger-created)
    - [2.2 Verify Artifact Registry](#22-verify-artifact-registry)
    - [2.3 Manual Build Trigger](#23-manual-build-trigger)
    - [2.4 Verify Image Pushed to Registry](#24-verify-image-pushed-to-registry)
    - [✅ Acceptance Criteria](#-acceptance-criteria-1)
  - [✅ Acceptance Test 3: Cloud Scheduler & Cloud Run Job](#-acceptance-test-3-cloud-scheduler--cloud-run-job)
    - [3.1 Verify Cloud Run Job Created](#31-verify-cloud-run-job-created)
    - [3.2 Verify Cloud Scheduler](#32-verify-cloud-scheduler)
    - [3.3 Manual Job Execution](#33-manual-job-execution)
    - [✅ Acceptance Criteria](#-acceptance-criteria-2)
  - [✅ Acceptance Test 4: Security & IAM](#-acceptance-test-4-security--iam)
    - [4.1 Service Accounts Verification](#41-service-accounts-verification)
    - [4.2 IAM Roles Verification](#42-iam-roles-verification)
    - [4.3 Secret Manager Verification](#43-secret-manager-verification)
    - [✅ Acceptance Criteria](#-acceptance-criteria-3)
  - [✅ Acceptance Test 5: Firestore & Data](#-acceptance-test-5-firestore--data)
    - [5.1 Firestore Database Verification](#51-firestore-database-verification)
    - [5.2 Collections Verification](#52-collections-verification)
    - [5.3 Sample Document Verification](#53-sample-document-verification)
    - [✅ Acceptance Criteria](#-acceptance-criteria-4)
  - [✅ Acceptance Test 6: Smoke Test — Controller Execution](#-acceptance-test-6-smoke-test--controller-execution)
    - [6.1 Trigger Controller Manually](#61-trigger-controller-manually)
    - [6.2 Verify Controller Output](#62-verify-controller-output)
    - [6.3 Verify Receipt Created](#63-verify-receipt-created)
    - [✅ Acceptance Criteria](#-acceptance-criteria-5)
  - [✅ Acceptance Test 7: Health Checks](#-acceptance-test-7-health-checks)
    - [7.1 Cloud Run Job Health](#71-cloud-run-job-health)
    - [7.2 Firestore Health](#72-firestore-health)
    - [7.3 Cloud Scheduler Health](#73-cloud-scheduler-health)
    - [✅ Acceptance Criteria](#-acceptance-criteria-6)
  - [✅ Acceptance Test 8: SKU Deployment Verification](#-acceptance-test-8-sku-deployment-verification)
    - [8.1 Verify Firestore Collections (Per SKU)](#81-verify-firestore-collections-per-sku)
    - [8.2 Verify Pub/Sub Topics (Per SKU)](#82-verify-pubsub-topics-per-sku)
    - [8.3 Verify Entitlement Collection](#83-verify-entitlement-collection)
    - [✅ Acceptance Criteria](#-acceptance-criteria-7)
  - [✅ Acceptance Test 9: Entitlement & Marketplace Integration](#-acceptance-test-9-entitlement--marketplace-integration)
    - [9.1 Simulate Marketplace Webhook](#91-simulate-marketplace-webhook)
    - [9.2 Verify Entitlement FSM Created](#92-verify-entitlement-fsm-created)
    - [✅ Acceptance Criteria](#-acceptance-criteria-8)
  - [✅ Acceptance Test 10: Autonomic Signals & Jidoka](#-acceptance-test-10-autonomic-signals--jidoka)
    - [10.1 Send Test Signal (Jidoka Halt)](#101-send-test-signal-jidoka-halt)
    - [10.2 Verify Signal Stored](#102-verify-signal-stored)
    - [10.3 Verify Governor FSM State](#103-verify-governor-fsm-state)
    - [✅ Acceptance Criteria](#-acceptance-criteria-9)
  - [✅ Acceptance Test 11: Cloud Logging & Audit Trail](#-acceptance-test-11-cloud-logging--audit-trail)
    - [11.1 Verify Logs Exported](#111-verify-logs-exported)
    - [11.2 Query Logs in BigQuery](#112-query-logs-in-bigquery)
    - [11.3 Verify Audit Trail](#113-verify-audit-trail)
    - [✅ Acceptance Criteria](#-acceptance-criteria-10)
  - [✅ Acceptance Test 12: Rollback Scenario](#-acceptance-test-12-rollback-scenario)
    - [12.1 Simulate Failure & Trigger Rollback](#121-simulate-failure--trigger-rollback)
    - [12.2 Verify Automatic Rollback](#122-verify-automatic-rollback)
    - [✅ Acceptance Criteria](#-acceptance-criteria-11)
  - [📊 Post-Deployment Checklist (10 Items)](#-post-deployment-checklist-10-items)
  - [🚨 Failure Scenarios & Recovery](#-failure-scenarios--recovery)
    - [Scenario 1: Controller Job Failing](#scenario-1-controller-job-failing)
    - [Scenario 2: Cloud Build Failing](#scenario-2-cloud-build-failing)
    - [Scenario 3: Firestore Quota Exceeded](#scenario-3-firestore-quota-exceeded)
  - [📊 Acceptance Test Report Template](#-acceptance-test-report-template)
  - [📚 Receipt Contract](#-receipt-contract)
  - [✅ Definition of Done](#-definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Acceptance Testing & Deployment Verification Guide

**Version**: v6.0.0 (Production-Ready)
**Last Updated**: January 2026
**Audience**: QA engineers, platform engineers, operations team
**Purpose**: Verify deployment succeeded and system is ready for production traffic

---

## 📋 Overview

This guide provides step-by-step acceptance tests to verify the ggen Erlang autonomic system deployed successfully. Tests verify:

1. **Infrastructure** — All Terraform modules deployed
2. **Cloud Build** — CI/CD pipeline working
3. **Controller** — Cloud Run Job executing successfully
4. **Security** — IAM roles, service accounts configured
5. **Data** — Firestore receipts, entitlements, signals
6. **Autonomics** — Jidoka enforcement, governor FSM, degraded mode
7. **Integration** — Marketplace webhooks → system response

---

## ⏱️ Pre-Deployment Checklist (10 Items)

Complete **BEFORE** running Terraform:

- [ ] **GCP Project**: Project ID confirmed, billing enabled
  ```bash
  gcloud projects describe ggen-autonomics-prod
  gcloud billing projects link ggen-autonomics-prod --billing-account=XXXXXX
  ```

- [ ] **Terraform Backend**: GCS bucket created with versioning
  ```bash
  gsutil ls gs://ggen-autonomics-tf-state-prod/
  gsutil versioning get gs://ggen-autonomics-tf-state-prod/
  ```

- [ ] **Service Accounts**: Terraform service account created with required roles
  ```bash
  gcloud iam service-accounts describe terraform@ggen-autonomics-prod.iam.gserviceaccount.com
  ```

- [ ] **API Enablement**: Required APIs enabled
  ```bash
  gcloud services enable compute.googleapis.com \
    cloudrun.googleapis.com \
    firestore.googleapis.com \
    pubsub.googleapis.com \
    cloudbuild.googleapis.com \
    cloudscheduler.googleapis.com
  ```

- [ ] **Secrets**: GitHub token created in Secret Manager
  ```bash
  gcloud secrets list | grep github-token
  ```

- [ ] **Repository Access**: GitHub repo accessible with credentials
  ```bash
  git clone https://github.com/seanchatmangpt/ggen.git /tmp/test
  ```

- [ ] **VPC/Network**: Network policy allows Cloud Run ↔ Firestore traffic
  ```bash
  gcloud compute networks describe ggen-autonomics-vpc
  ```

- [ ] **IAM Permissions**: Deploying user has required roles (roles/editor OR specific roles)
  ```bash
  gcloud projects get-iam-policy ggen-autonomics-prod \
    --flatten="bindings[].members" \
    --filter="bindings.members:user:me@example.com"
  ```

- [ ] **Budget**: Sufficient budget for infrastructure (~$30/month estimated)
  ```bash
  gcloud billing budgets list --billing-account=XXXXXX
  ```

- [ ] **DNS/TLS**: If using custom domain, DNS records and TLS cert configured
  ```bash
  dig my-domain.com
  ```

---

## 🚀 Deployment Stage 1: Infrastructure (Terraform)

```bash
# Initialize Terraform
cd infra/
terraform init \
  -backend-config="bucket=ggen-autonomics-tf-state-prod" \
  -backend-config="prefix=terraform/state"

# Validate configuration
terraform validate

# Preview changes (ALWAYS review before apply)
terraform plan -var-file=prod.tfvars -out=plan.tfplan

# Apply infrastructure
terraform apply plan.tfplan

# Expected duration: ~28 minutes (see terraform-reference.md)
```

---

## ✅ Acceptance Test 1: Infrastructure Verification

### 1.1 Verify All Terraform Resources Created

```bash
# Check Terraform state
terraform state list

# Expected output (all 10 module types):
# ✓ google_storage_bucket.terraform_state
# ✓ google_compute_network.vpc
# ✓ google_firestore_database.catalog
# ✓ google_cloud_run_v2_job.controller
# ✓ google_pubsub_topic.sku_topics["sku-001"]
# ✓ google_cloud_scheduler_job.controller_trigger
# ✓ google_service_account.controller
# ✓ google_cloud_build_trigger.github
# ✓ google_monitoring_alert_policy.* (multiple)
```

### 1.2 Verify No Drift

```bash
# Show current state (should match actual GCP resources)
terraform plan -var-file=prod.tfvars

# Expected output: "No changes. Infrastructure is up-to-date."
```

### 1.3 Verify Resource Properties

```bash
# Verify VPC created with correct CIDR
terraform state show google_compute_subnetwork.main | grep ip_cidr_range
# Expected: 10.0.0.0/20

# Verify Cloud Run Job created
terraform state show google_cloud_run_v2_job.controller | grep name
# Expected: autonomics-catalog-controller

# Verify Firestore database created
terraform state show google_firestore_database.catalog | grep location_id
# Expected: us-central1 (or configured region)
```

### ✅ Acceptance Criteria
- [ ] `terraform state list` shows 50+ resources
- [ ] `terraform plan` shows "No changes"
- [ ] All module outputs accessible
- [ ] No warnings in Terraform output

---

## ✅ Acceptance Test 2: Cloud Build Pipeline

### 2.1 Verify Trigger Created

```bash
# List Cloud Build triggers
gcloud builds triggers list

# Expected: 1 trigger named "ggen-autonomics-controller"
gcloud builds triggers describe ggen-autonomics-controller

# Verify GitHub integration
gcloud builds triggers describe ggen-autonomics-controller \
  --format=json | jq '.github'

# Expected:
# {
#   "owner": "seanchatmangpt",
#   "name": "ggen",
#   "push": { "branch": "^main$" }
# }
```

### 2.2 Verify Artifact Registry

```bash
# Check repository exists
gcloud artifacts repositories describe controller \
  --location=us-central1 \
  --repository-format=docker

# Verify service account has push permissions
gcloud projects get-iam-policy ggen-autonomics-prod \
  --flatten="bindings[].members" \
  --filter="bindings.role:roles/artifactregistry.admin" \
  --format=table
```

### 2.3 Manual Build Trigger

```bash
# Trigger build manually to verify pipeline works
gcloud builds submit \
  --config=cloudbuild.yaml \
  --substitutions=_ENV=prod,SHORT_SHA=$(git rev-parse --short HEAD) \
  .

# Monitor build progress
gcloud builds log <BUILD_ID> --stream

# Expected: Build completes in <3 minutes with status SUCCESS
gcloud builds describe <BUILD_ID> \
  --format=json | jq '.status'
# Expected: SUCCESS
```

### 2.4 Verify Image Pushed to Registry

```bash
# List images in registry
gcloud artifacts docker images list \
  us-central1-docker.pkg.dev/ggen-autonomics-prod/controller

# Expected: autonomics-catalog-controller with multiple tags
# autonomics-catalog-controller:abc123d
# autonomics-catalog-controller:latest
```

### ✅ Acceptance Criteria
- [ ] Cloud Build trigger created and configured
- [ ] Artifact Registry repository exists
- [ ] Manual build completes successfully (<3 min)
- [ ] Docker image pushed to registry
- [ ] Image has both versioned and latest tags

---

## ✅ Acceptance Test 3: Cloud Scheduler & Cloud Run Job

### 3.1 Verify Cloud Run Job Created

```bash
# Describe Cloud Run Job
gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json

# Verify critical properties
gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json | jq '{
    name: .metadata.name,
    status: .status.conditions[-1].status,
    image: .spec.template.spec.containers[0].image,
    memory: .spec.template.spec.containers[0].resources.limits.memory,
    timeout: .spec.taskTtlSeconds
  }'

# Expected:
# {
#   "name": "autonomics-catalog-controller",
#   "status": "True",
#   "image": "us-central1-docker.pkg.dev/.../controller:...",
#   "memory": "2Gi",
#   "timeout": 3600
# }
```

### 3.2 Verify Cloud Scheduler

```bash
# Describe scheduler job
gcloud scheduler jobs describe autonomics-catalog-controller-trigger \
  --location=us-central1 \
  --format=json

# Verify schedule (every 15 minutes)
gcloud scheduler jobs describe autonomics-catalog-controller-trigger \
  --location=us-central1 \
  --format=json | jq '.schedule'
# Expected: "*/15 * * * *"

# Verify job is enabled
gcloud scheduler jobs describe autonomics-catalog-controller-trigger \
  --location=us-central1 \
  --format=json | jq '.state'
# Expected: "ENABLED"
```

### 3.3 Manual Job Execution

```bash
# Execute job manually
gcloud run jobs execute autonomics-catalog-controller \
  --region=us-central1 \
  --wait

# Monitor execution
gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json | jq '.status.latestCreatedExecution'

# Get execution ID
EXEC_ID=$(gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json | jq -r '.status.latestCreatedExecution.name | split("/")[-1]')

# View logs
gcloud run jobs logs read autonomics-catalog-controller \
  --region=us-central1 \
  --execution=$EXEC_ID
```

### ✅ Acceptance Criteria
- [ ] Cloud Run Job exists and is healthy
- [ ] Cloud Scheduler trigger configured and enabled
- [ ] Manual execution completes successfully
- [ ] Job logs visible in Cloud Logging
- [ ] Receipt written to Firestore

---

## ✅ Acceptance Test 4: Security & IAM

### 4.1 Service Accounts Verification

```bash
# List service accounts
gcloud iam service-accounts list

# Expected:
# - controller@ggen-autonomics-prod.iam.gserviceaccount.com
# - cloudbuild@ggen-autonomics-prod.iam.gserviceaccount.com
# - terraform@ggen-autonomics-prod.iam.gserviceaccount.com
```

### 4.2 IAM Roles Verification

```bash
# Check controller service account roles
gcloud projects get-iam-policy ggen-autonomics-prod \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:controller@*" \
  --format=table

# Expected roles:
# ✓ roles/datastore.user (Firestore access)
# ✓ roles/pubsub.publisher (Pub/Sub publish)
# ✓ roles/storage.objectViewer (Read Terraform state)
# ✓ roles/cloudlogging.logWriter (Write logs)
# ✓ roles/cloudbuild.builds.editor (Trigger Cloud Build)
```

### 4.3 Secret Manager Verification

```bash
# List secrets
gcloud secrets list

# Expected: github-token (for private repos)
# Get secret value (verify it's set)
gcloud secrets versions access latest --secret=github-token
# Expected: (secret string, masked in output)

# Verify service account has access
gcloud secrets get-iam-policy github-token \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:controller@*"
# Expected: roles/secretmanager.secretAccessor
```

### ✅ Acceptance Criteria
- [ ] All service accounts created
- [ ] Controller service account has correct IAM roles
- [ ] Cloud Build service account has artifactregistry.admin role
- [ ] Secrets stored in Secret Manager (not in code)
- [ ] Service accounts have least privilege (no editor/owner)

---

## ✅ Acceptance Test 5: Firestore & Data

### 5.1 Firestore Database Verification

```bash
# Describe database
gcloud firestore databases describe \
  --database='(default)'

# Verify location
gcloud firestore databases describe \
  --database='(default)' \
  --format=json | jq '.locationConfig.name'
# Expected: projects/ggen-autonomics-prod/locations/us-central1

# Verify type
gcloud firestore databases describe \
  --database='(default)' \
  --format=json | jq '.type'
# Expected: FIRESTORE_NATIVE
```

### 5.2 Collections Verification

```bash
# List collections
gcloud firestore databases list-collections --database='(default)'

# Expected collections:
# ✓ catalog (SKU metadata)
# ✓ catalog_runs (execution receipts)
# ✓ entitlements (marketplace entitlements)
# ✓ signals (jidoka signals)
```

### 5.3 Sample Document Verification

```bash
# List documents in catalog_runs
gcloud firestore documents list --collection-id=catalog_runs --database='(default)' --limit=5

# Get first receipt
RECEIPT_ID=$(gcloud firestore documents list --collection-id=catalog_runs --database='(default)' --limit=1 --format=json | jq -r '.[0].name | split("/")[-1]')

# View receipt contents
gcloud firestore documents get catalog_runs/$RECEIPT_ID --database='(default)' --format=json

# Expected fields in receipt:
# {
#   "runId": "20250118-1530",
#   "timestamp": "2025-01-18T15:30:00Z",
#   "status": "success",
#   "terraformChanges": { ... },
#   "cloudBuild": { "triggered": true, "buildId": "..." }
# }
```

### ✅ Acceptance Criteria
- [ ] Firestore database created (NATIVE type)
- [ ] All 4 expected collections exist
- [ ] At least one receipt in catalog_runs
- [ ] Receipt has required fields (runId, timestamp, status, etc.)
- [ ] Backups enabled (if configured)

---

## ✅ Acceptance Test 6: Smoke Test — Controller Execution

### 6.1 Trigger Controller Manually

```bash
# Execute controller
gcloud run jobs execute autonomics-catalog-controller \
  --region=us-central1 \
  --wait

# Monitor execution
gcloud run jobs logs read autonomics-catalog-controller \
  --region=us-central1 \
  --limit=100
```

### 6.2 Verify Controller Output

Expected logs (JSON structured):

```json
{
  "timestamp": "2025-01-18T15:30:00Z",
  "severity": "INFO",
  "jsonPayload": {
    "status": "success",
    "action": "reconcile",
    "catalog_repo": "https://github.com/seanchatmangpt/ggen-catalog",
    "catalog_branch": "main",
    "terraform_changes": {
      "plan": {
        "resources_to_add": 0,
        "resources_to_change": 0,
        "resources_to_destroy": 0
      }
    },
    "cloud_build_triggered": false,
    "duration_seconds": 45
  }
}
```

### 6.3 Verify Receipt Created

```bash
# Get latest receipt
gcloud firestore documents list \
  --collection-id=catalog_runs \
  --database='(default)' \
  --order-by=timestamp \
  --limit=1 \
  --format=json

# Expected:
# [
#   {
#     "name": "projects/.../databases/..../documents/catalog_runs/20250118-1530",
#     "fields": {
#       "runId": { "stringValue": "20250118-1530" },
#       "status": { "stringValue": "success" },
#       "timestamp": { "timestampValue": "2025-01-18T15:30:00Z" }
#     }
#   }
# ]
```

### ✅ Acceptance Criteria
- [ ] Controller executes successfully (status: success)
- [ ] Receipt written to Firestore
- [ ] Logs visible in Cloud Logging
- [ ] Execution time <2 minutes (SLO)
- [ ] No errors in logs

---

## ✅ Acceptance Test 7: Health Checks

### 7.1 Cloud Run Job Health

```bash
# Check job status
gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json | jq '.status.conditions'

# Expected: All conditions should have status "True"
# [
#   {
#     "type": "Ready",
#     "status": "True",
#     "message": "Job is ready"
#   }
# ]
```

### 7.2 Firestore Health

```bash
# Simple write/read test
gcloud firestore documents update catalog_runs/health-check \
  --update="timestamp=$(date -u +%Y-%m-%dT%H:%M:%SZ),test=true"

gcloud firestore documents get catalog_runs/health-check

# Expected: Document exists and is readable
```

### 7.3 Cloud Scheduler Health

```bash
# Check last execution
gcloud scheduler jobs describe autonomics-catalog-controller-trigger \
  --location=us-central1 \
  --format=json | jq '.lastExecutionTime'

# Expected: Recent timestamp (within last 15 min)
```

### ✅ Acceptance Criteria
- [ ] Cloud Run Job shows "Ready: True"
- [ ] Firestore read/write works
- [ ] Cloud Scheduler shows recent execution
- [ ] No error messages in status

---

## ✅ Acceptance Test 8: SKU Deployment Verification

### 8.1 Verify Firestore Collections (Per SKU)

```bash
# For each SKU in tfvars (e.g., sku-001-marketplace, sku-002-enterprise)
SKU="sku-001-marketplace"

# Check Firestore document created
gcloud firestore documents get catalog/$SKU --database='(default)' --format=json

# Expected:
# {
#   "fields": {
#     "display_name": { "stringValue": "Marketplace SKU 001" },
#     "entitlement_ttl": { "integerValue": "86400" }
#   }
# }
```

### 8.2 Verify Pub/Sub Topics (Per SKU)

```bash
# For each SKU, verify Pub/Sub topic created
gcloud pubsub topics describe $SKU-events

# Get subscription
gcloud pubsub subscriptions describe $SKU-events-sub

# Expected: Topic and subscription exist
```

### 8.3 Verify Entitlement Collection

```bash
# Check entitlements collection exists
gcloud firestore documents list --collection-id=entitlements --database='(default)' --limit=1

# Expected: Collection exists (may be empty initially)
```

### ✅ Acceptance Criteria
- [ ] One Firestore document per SKU in catalog collection
- [ ] One Pub/Sub topic per SKU
- [ ] One Pub/Sub subscription per SKU
- [ ] Entitlements collection created

---

## ✅ Acceptance Test 9: Entitlement & Marketplace Integration

### 9.1 Simulate Marketplace Webhook

```bash
# Create test entitlement payload
cat > /tmp/webhook.json <<'EOF'
{
  "eventId": "test-event-123",
  "eventType": "ENTITLEMENT_CREATION_REQUESTED",
  "account": {
    "name": "projects/my-project/accounts/test-account",
    "displayName": "Test Account"
  },
  "entitlementPlan": "sku-001-marketplace"
}
EOF

# Send to Pub/Sub topic (simulate webhook)
gcloud pubsub topics publish marketplace-webhooks \
  --message="$(cat /tmp/webhook.json)"
```

### 9.2 Verify Entitlement FSM Created

```bash
# Check entitlements collection for new document
gcloud firestore documents list --collection-id=entitlements \
  --database='(default)' \
  --limit=1

# Expected:
# {
#   "accountId": "test-account",
#   "skuId": "sku-001-marketplace",
#   "state": "PROVISIONING_IN_PROGRESS",
#   "createdAt": "2025-01-18T15:30:00Z",
#   "expiresAt": "2025-01-19T15:30:00Z"
# }
```

### ✅ Acceptance Criteria
- [ ] Webhook published to Pub/Sub
- [ ] Entitlement document created in Firestore
- [ ] FSM state is PROVISIONING_IN_PROGRESS
- [ ] Expiration timestamp set correctly

---

## ✅ Acceptance Test 10: Autonomic Signals & Jidoka

### 10.1 Send Test Signal (Jidoka Halt)

```bash
# Publish test signal to jidoka halt topic
gcloud pubsub topics publish autonomic-signals \
  --message='{"signal_type": "JIDOKA_HALT", "reason": "test_signal", "component": "catalog-controller"}'

# Verify signal received in logs
gcloud logging read 'resource.type="cloud_run_job" AND textPayload:~"JIDOKA_HALT"' \
  --limit=1
```

### 10.2 Verify Signal Stored

```bash
# Check signals collection
gcloud firestore documents list --collection-id=signals \
  --database='(default)' \
  --limit=5

# Expected:
# {
#   "signalType": "JIDOKA_HALT",
#   "reason": "test_signal",
#   "component": "catalog-controller",
#   "timestamp": "2025-01-18T15:30:00Z"
# }
```

### 10.3 Verify Governor FSM State

```bash
# After jidoka halt, system should enter degraded mode
# Check for governor state document
gcloud firestore documents get catalog/governor-state --database='(default)' --format=json

# Expected:
# {
#   "state": "DEGRADED",
#   "haltReason": "JIDOKA_HALT",
#   "lastSignal": "2025-01-18T15:30:00Z"
# }
```

### ✅ Acceptance Criteria
- [ ] Signal published to autonomic-signals topic
- [ ] Signal stored in signals collection
- [ ] Governor FSM transitions to DEGRADED on halt
- [ ] System prevents further changes in degraded mode

---

## ✅ Acceptance Test 11: Cloud Logging & Audit Trail

### 11.1 Verify Logs Exported

```bash
# Check Cloud Logging sink
gcloud logging sinks list | grep catalog

# Expected: Sink named something like "catalog-logs-sink"

# Verify BigQuery dataset created
bq ls -d | grep catalog_logs

# Expected: Dataset "catalog_logs" exists
```

### 11.2 Query Logs in BigQuery

```bash
# Export logs to BigQuery and query
bq query --use_legacy_sql=false <<'EOF'
SELECT
  timestamp,
  jsonPayload.status,
  jsonPayload.duration_seconds
FROM
  `ggen-autonomics-prod.catalog_logs.events_*`
WHERE
  timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 DAY)
ORDER BY
  timestamp DESC
LIMIT 10;
EOF
```

### 11.3 Verify Audit Trail

```bash
# Get receipt with audit trail
gcloud firestore documents get catalog_runs/$(date +%Y%m%d-$(date +%H%M)) \
  --database='(default)' \
  --format=json | jq '.fields.auditTrail'

# Expected:
# {
#   "inputHash": { "stringValue": "sha256:abc123..." },
#   "outputHashes": {
#     "mapValue": {
#       "fields": {
#         "terraform.tfstate": { "stringValue": "sha256:def456..." }
#       }
#     }
#   }
# }
```

### ✅ Acceptance Criteria
- [ ] Cloud Logging sink configured
- [ ] BigQuery dataset created
- [ ] Logs exported to BigQuery
- [ ] Audit trail hashes present in receipts
- [ ] Logs queryable via BigQuery

---

## ✅ Acceptance Test 12: Rollback Scenario

### 12.1 Simulate Failure & Trigger Rollback

```bash
# For testing ONLY - force Terraform to a bad state
# (Don't do this in production!)

# Manually delete a critical resource (e.g., Firestore document)
gcloud firestore documents delete catalog/sku-001-marketplace

# Run controller to verify it detects drift
gcloud run jobs execute autonomics-catalog-controller \
  --region=us-central1 \
  --wait

# Check for drift alert
gcloud logging read 'resource.type="cloud_run_job" AND textPayload:~"drift"' --limit=1
```

### 12.2 Verify Automatic Rollback

```bash
# If Cloud Build fails, previous image should still be serving
# Verify controller is using previous known-good version

gcloud run jobs describe autonomics-catalog-controller \
  --region=us-central1 \
  --format=json | jq '.spec.template.spec.containers[0].image'

# If latest deployment failed:
# Image should be: previous-known-good-tag
# NOT: broken-image-tag
```

### ✅ Acceptance Criteria
- [ ] Drift detection triggered on resource change
- [ ] Alert published to Cloud Logging
- [ ] Rollback to previous version on failure (automatic)
- [ ] System continues operating (zero downtime)

---

## 📊 Post-Deployment Checklist (10 Items)

Complete **AFTER** all acceptance tests pass:

- [ ] **Logs Archived**: CloudLogging logs exported to GCS/BigQuery
  ```bash
  gcloud logging sinks create archive-sink \
    gs://ggen-logs-archive/ \
    --log-filter='resource.type="cloud_run_job"'
  ```

- [ ] **Disaster Recovery Tested**: Backup/restore of Firestore data
  ```bash
  gcloud firestore export gs://ggen-backups/export-$(date +%s)
  ```

- [ ] **Budget Alerts Configured**: Receive alerts if spending exceeds threshold
  ```bash
  gcloud billing budgets create --billing-account=XXXXXX \
    --display-name=ggen-autonomics-prod \
    --budget-amount=1000
  ```

- [ ] **Ops Runbooks Written**: Team can handle common incidents
  - [ ] Controller job failure
  - [ ] Firestore quota exceeded
  - [ ] Git repository access lost
  - [ ] Terraform state lock stuck

- [ ] **Team Training Completed**: DevOps team trained on:
  - [ ] Deployment procedure
  - [ ] Health check monitoring
  - [ ] Rollback procedure
  - [ ] Incident response

- [ ] **Monitoring Dashboard Created**: Track key metrics
  ```bash
  gcloud monitoring dashboards create --config-from-file=dashboard.json
  ```

- [ ] **On-Call Rotation Established**: Escalation path for incidents
  - [ ] Primary on-call engineer assigned
  - [ ] Secondary on-call assigned
  - [ ] Slack/PagerDuty integration configured

- [ ] **Change Log Updated**: Document deployment date/version
  ```bash
  echo "Production deployment: $(date) - v6.0.0" >> CHANGELOG.md
  git add CHANGELOG.md && git commit -m "docs: record deployment"
  ```

- [ ] **Stakeholder Notification**: Inform product/marketing of availability
  - [ ] Status page updated
  - [ ] Slack announcement sent
  - [ ] Internal wiki updated

- [ ] **Compliance Evidence Collected**: See ato-evidence-pack.md
  - [ ] Infrastructure security checklist completed
  - [ ] IAM role assignments verified
  - [ ] Encryption enabled (at-rest, in-transit)
  - [ ] Audit logs enabled and archived
  - [ ] Data retention policies configured

---

## 🚨 Failure Scenarios & Recovery

### Scenario 1: Controller Job Failing

**Symptom**: `gcloud run jobs describe` shows failed execution

**Diagnosis**:
```bash
# Check logs
gcloud run jobs logs read autonomics-catalog-controller --limit=50

# Check Cloud Scheduler trigger
gcloud scheduler jobs logs list autonomics-catalog-controller-trigger
```

**Recovery**:
```bash
# 1. Fix root cause (check logs for error message)
# 2. Update environment variables if needed
gcloud run jobs update autonomics-catalog-controller \
  --set-env-vars=LOG_LEVEL=DEBUG

# 3. Re-execute manually
gcloud run jobs execute autonomics-catalog-controller --region=us-central1 --wait

# 4. Verify success
gcloud firestore documents list --collection-id=catalog_runs --limit=1
```

### Scenario 2: Cloud Build Failing

**Symptom**: Latest build shows FAILURE

**Diagnosis**:
```bash
# Get failed build ID
BUILD_ID=$(gcloud builds list --limit=1 --format=json | jq -r '.[0].id')

# View logs
gcloud builds log $BUILD_ID
```

**Recovery**:
```bash
# 1. Fix issue in Dockerfile or cloudbuild.yaml
# 2. Re-trigger build
git commit --allow-empty -m "chore: rebuild controller"
git push origin main

# 3. Or manually trigger
gcloud builds submit --config=cloudbuild.yaml .

# 4. Monitor build
gcloud builds log <NEW_BUILD_ID> --stream
```

### Scenario 3: Firestore Quota Exceeded

**Symptom**: Logs show quota exceeded errors

**Diagnosis**:
```bash
# Check quota usage
gcloud compute project-info describe --project=ggen-autonomics-prod | grep quota
```

**Recovery**:
```bash
# 1. Reduce collection size (delete old documents)
gcloud firestore documents delete catalog_runs/<OLD_ID>

# 2. Request quota increase (via Console)
# 3. Implement TTL-based document deletion
```

---

## 📊 Acceptance Test Report Template

```markdown
# Acceptance Test Report - ggen Autonomic System
Date: 2025-01-18
Tested By: DevOps Team
Environment: Production

## Test Results

| Test | Status | Duration | Notes |
|------|--------|----------|-------|
| 1. Infrastructure Verification | ✓ PASS | 2s | All 50+ resources present |
| 2. Cloud Build Pipeline | ✓ PASS | 3m 15s | Manual build successful |
| 3. Cloud Scheduler & Run Job | ✓ PASS | 1m 30s | Execution completed |
| 4. Security & IAM | ✓ PASS | 45s | All roles verified |
| 5. Firestore & Data | ✓ PASS | 1m | Receipt created |
| 6. Smoke Test | ✓ PASS | 2m | Controller healthy |
| 7. Health Checks | ✓ PASS | 30s | All services green |
| 8. SKU Deployment | ✓ PASS | 1m | 2 SKUs verified |
| 9. Marketplace Integration | ✓ PASS | 2m | Webhook tested |
| 10. Autonomic Signals | ✓ PASS | 1m 15s | Jidoka working |
| 11. Logging & Audit | ✓ PASS | 1m | BigQuery accessible |
| 12. Rollback Scenario | ✓ PASS | 3m | Automatic rollback works |

**Total Duration**: 24 minutes
**Overall Result**: ✓ ALL TESTS PASSED - READY FOR PRODUCTION

## Approvals

- [ ] QA Lead: _________________ Date: _______
- [ ] DevOps Lead: _________________ Date: _______
- [ ] Product Manager: _________________ Date: _______
```

---

## 📚 Receipt Contract

Every acceptance test should produce a receipt:

```json
{
  "test_suite": "ggen-autonomic-system",
  "test_run_id": "acceptance-20250118-1530",
  "timestamp": "2025-01-18T15:30:00Z",
  "environment": "production",
  "tests_total": 12,
  "tests_passed": 12,
  "tests_failed": 0,
  "duration_minutes": 24,
  "infrastructure": {
    "terraform_resources": 51,
    "no_drift": true
  },
  "cloud_build": {
    "pipeline_success": true,
    "build_time_seconds": 195
  },
  "controller": {
    "cloud_run_job_healthy": true,
    "scheduler_enabled": true,
    "execution_successful": true
  },
  "security": {
    "service_accounts_count": 3,
    "iam_roles_verified": true,
    "secrets_configured": true
  },
  "data": {
    "firestore_collections": 4,
    "receipts_stored": true,
    "backups_enabled": true
  },
  "approvals": {
    "qa_lead": "john@example.com",
    "devops_lead": "jane@example.com",
    "product_manager": "alice@example.com"
  }
}
```

---

## ✅ Definition of Done

Acceptance testing is complete when:

- [ ] All 12 acceptance tests pass
- [ ] No critical warnings or errors
- [ ] 10-item post-deployment checklist completed
- [ ] Ops runbooks written and reviewed
- [ ] Team trained on operations
- [ ] Monitoring dashboard functional
- [ ] On-call rotation established
- [ ] Compliance evidence collected (see ato-evidence-pack.md)
- [ ] Stakeholders notified
- [ ] Receipt signed by QA + DevOps leads
- [ ] Documentation updated with deployment details
- [ ] Go/no-go decision made for production traffic

---

**Related Documentation**:
- [terraform-reference.md](terraform-reference.md) — Infrastructure provisioning
- [cloudbuild-reference.md](cloudbuild-reference.md) — CI/CD pipeline
- [controller-reference.md](controller-reference.md) — Controller CLI and configuration
- [ato-evidence-pack.md](ato-evidence-pack.md) — Compliance and security evidence
- [glossary.md](glossary.md) — Terms and definitions
