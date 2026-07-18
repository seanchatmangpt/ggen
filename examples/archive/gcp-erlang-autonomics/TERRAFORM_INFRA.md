# Terraform Infrastructure for Erlang Autonomics

## Overview

This Terraform module system manages the complete GCP infrastructure for Erlang Autonomics SKUs at catalog scale:

- ✅ **Remote state** management (GCS bucket with versioning)
- ✅ **Shared Artifact Registry** for all SKU container images
- ✅ **Cloud Build automation** (GitHub → build → deploy)
- ✅ **Per-SKU infrastructure** (Cloud Run, Pub/Sub, Firestore receipts)
- ✅ **Canary rollout** (traffic splitting for safe deployment)
- ✅ **Marketplace security** (HMAC via Secret Manager)
- ✅ **TPS release gates** (smoke tests + health verification)

**Philosophy**: One spec (terraform.tfvars) → N production SKUs

---

## Directory Structure

```
infra/
├── state/                          # Remote state setup
│   ├── main.tf.tera
│   └── variables.tf.tera
│
├── shared/                         # Shared resources (Artifact Registry)
│   ├── artifact_registry.tf.tera
│   └── variables.tf.tera
│
├── cloudbuild/                     # Cloud Build automation
│   ├── trigger.tf.tera
│   └── variables.tf.tera
│
├── security/                       # Marketplace HMAC + Secret Manager
│   ├── marketplace_hmac.tf.tera
│   └── variables.tf.tera
│
├── modules/gcp_erlang_autonomics_sku/  # Per-SKU module (generated earlier)
│   ├── main.tf
│   ├── variables.tf
│   └── outputs.tf
│
├── catalog/                        # Root module: manages N SKUs
│   ├── main.tf.tera
│   └── variables.tf.tera
│
├── backend.tf.tera                # Remote state backend config
├── terraform.tfvars.example.tera   # Example configuration
└── README.md                       # This file
```

---

## Getting Started

### Step 1: Create Remote State Bucket

```bash
cd infra/state

terraform init
terraform plan -var="project_id=my-project" -var="region=us-central1"
terraform apply -var="project_id=my-project" -var="region=us-central1"

# Copy output: state_bucket
export TF_STATE_BUCKET="my-project-tfstate"
```

### Step 2: Configure Backend

```bash
cd infra

cat > backend.tf <<EOF
terraform {
  backend "gcs" {
    bucket = "$TF_STATE_BUCKET"
    prefix = "autonomics/catalog"
  }
}
EOF
```

### Step 3: Prepare SKU Configuration

```bash
# Copy example and customize
cp terraform.tfvars.example terraform.tfvars
vim terraform.tfvars
```

Edit `terraform.tfvars`:

```hcl
project_id = "YOUR_PROJECT"
region     = "us-central1"

github_owner  = "your-github-org"
github_repo   = "your-autonomics-repo"

skus = {
  "my_sku" = {
    module_prefix            = "my_sku"
    sku_id                   = "gcp.autonomic.my_sku.v1"
    image_uri                = "us-central1-docker.pkg.dev/YOUR_PROJECT/autonomics-skus/my_sku:latest"
    action_groups            = ["cloudrun"]
    allow_marketplace_public = true
    marketplace_hmac_secret  = "$(openssl rand -hex 32)"
  }
}
```

### Step 4: Deploy Catalog

```bash
cd infra/catalog

# Initialize with remote state
terraform init

# Plan
terraform plan

# Apply
terraform apply

# Capture outputs
terraform output -json > outputs.json
```

---

## Module Breakdown

### State Module (`infra/state/`)

Creates the GCS bucket for Terraform state with versioning and locking.

**Resources**:
- `google_storage_bucket` (state + lock)

**Outputs**:
- `state_bucket`: Bucket name
- `state_bucket_url`: gs:// URL
- `backend_config`: Text to add to backend.tf

---

### Shared Module (`infra/shared/`)

Creates shared Artifact Registry repository for all SKU images.

**Resources**:
- `google_artifact_registry_repository` (Docker format)
- IAM bindings (Cloud Build writer, Cloud Run reader)

**Outputs**:
- `repo_url`: Image registry URL

---

### Cloud Build Module (`infra/cloudbuild/`)

Creates GitHub → Cloud Build → Cloud Run automation.

**Resources**:
- `google_cloudbuild_trigger` (GitHub push trigger)

**Behavior**:
1. Push to main branch
2. Cloud Build runs cloudbuild.yaml
3. Builds container image
4. Runs smoke test (GET /health)
5. Deploys to Cloud Run
6. Verifies remote health (TPS gate)

**Outputs**:
- `trigger_id`: Cloud Build trigger ID

---

### Security Module (`infra/security/`)

Creates HMAC secret for Marketplace callback verification.

**Resources**:
- `google_secret_manager_secret` (HMAC)
- `google_secret_manager_secret_version` (secret value)
- IAM binding (Cloud Run can read)

**Usage**:
```erlang
% In Cloud Run service:
MARKETPLACE_HMAC_SECRET env var → Secret Manager
```

**Verification**:
```erlang
% Erlang handler:
X-GGEN-Signature: hex(hmac_sha256(secret, raw_body))
```

**Outputs**:
- `hmac_secret_name`: Secret Manager resource name

---

### SKU Module (`infra/modules/gcp_erlang_autonomics_sku/`)

Per-SKU infrastructure (created earlier, reused here).

**Resources per SKU**:
- Cloud Run service
- Pub/Sub topic + push subscription
- Service accounts
- IAM roles (computed from action_groups)
- Firestore (if enabled)

**Inputs**:
- `module_prefix`, `sku_id`, `image_uri`
- `action_groups` (["cloudrun", "scheduler", "pubsub"])

**Outputs**:
- `service_url`: Cloud Run HTTPS URL
- `topic_name`: Pub/Sub topic
- `runtime_service_account`: Cloud Run SA email

---

### Catalog Root Module (`infra/catalog/`)

**The main entry point** — manages N SKUs from one terraform.tfvars.

**Key Features**:

1. **Shared Resources**:
   - Cloud Build service account (shared)
   - Artifact Registry (shared)

2. **Per-SKU Instantiation**:
   ```hcl
   module "skus" {
     for_each = var.skus
     source = "../modules/gcp_erlang_autonomics_sku"
     # Each SKU gets:
     # - Cloud Run service
     # - Pub/Sub topic
     # - Service accounts + IAM
   }
   ```

3. **Cloud Build Triggers**:
   ```hcl
   module "cloud_build_triggers" {
     for_each = var.skus
     source = "../cloudbuild"
     # One trigger per SKU (or shared, depending on workflow)
   }
   ```

4. **Marketplace Security**:
   ```hcl
   module "marketplace_security" {
     for_each = var.skus
     source = "../security"
     # One HMAC secret per SKU
   }
   ```

---

## TPS Enforcement (Release Gates)

### Build Gate (Cloud Build)

```yaml
# In cloudbuild.yaml:
- smoke-test-local     # Run container, GET /health → 200
- push                 # Push to Artifact Registry
- deploy               # Deploy to Cloud Run
- verify-remote-health # GET /health from Cloud Run → 200
```

**Stop condition**: Any step fails → build fails, no deployment

### Deploy Gate (Terraform)

```hcl
# Terraform only applies if git push succeeds
# (Cloud Build trigger)
```

### Health Gate (Terraform)

```bash
# After terraform apply, verify service is healthy
terraform output service_url
curl $SERVICE_URL/health
```

---

## Deployment Workflow (TPS)

### 1. Developer pushes code to main

```bash
git push origin main
```

### 2. Cloud Build trigger fires

```
GitHub webhook → Cloud Build → runs cloudbuild.yaml
```

### 3. Cloud Build pipeline (gated)

```
[Build] Containerfile → image:$COMMIT_SHA
  ↓
[Smoke Test] docker run → GET /health
  ↓
[Push] gcr.io → Artifact Registry
  ↓
[Deploy] gcloud run deploy
  ↓
[Verify Remote] GET /health from Cloud Run → 200 OK
  ↓
✅ PASS → image available for Terraform deploy
```

### 4. Terraform plan/apply (on main branch)

```bash
cd infra/catalog
terraform init
terraform plan
terraform apply

# Creates/updates Cloud Run services per terraform.tfvars
```

### 5. Adversarial test suite (optional automated gate)

```bash
SERVICE_URL=$(terraform output -raw service_url)
./run_adversarial.sh

# Passes all 18+ tests → verified safe
```

---

## Adding a New SKU

### Step 1: Edit terraform.tfvars

```hcl
skus = {
  "existing_sku" = { ... },

  "new_sku" = {
    module_prefix            = "new_sku"
    sku_id                   = "gcp.autonomic.new_sku.v1"
    image_uri                = "us-central1-docker.pkg.dev/PROJECT/autonomics-skus/new_sku:latest"
    action_groups            = ["cloudrun"]
    allow_marketplace_public = true
    marketplace_hmac_secret  = "$(openssl rand -hex 32)"
  }
}
```

### Step 2: Generate ggen artifacts

```bash
cd examples/gcp-erlang-autonomics
ggen sync --audit true
```

### Step 3: Build image (Cloud Build)

```bash
git add -A
git commit -m "feat: add new_sku"
git push origin main

# Cloud Build trigger fires automatically
# Builds image: us-central1-docker.pkg.dev/PROJECT/autonomics-skus/new_sku:$COMMIT_SHA
```

### Step 4: Deploy with Terraform

```bash
cd infra/catalog
terraform init
terraform plan
terraform apply

# Terraform sees new SKU in terraform.tfvars
# Creates Cloud Run service, Pub/Sub topic, IAM roles
```

### Step 5: Verify

```bash
terraform output sku_service_urls
# Output: {"new_sku": "https://..."}

curl https://.../new_sku/health
```

---

## Monitoring & Observability

### Cloud Build

```bash
gcloud builds log <BUILD_ID>
gcloud builds list --filter="source.repo_source.branch_name:main"
```

### Cloud Run

```bash
gcloud run services list --platform managed --region us-central1
gcloud run services logs read {{ module_prefix }} --region us-central1
```

### Pub/Sub

```bash
gcloud pubsub subscriptions list
gcloud pubsub subscriptions describe {{ module_prefix }}-push
gcloud pubsub subscriptions pull {{ module_prefix }}-push --auto-ack
```

### Firestore

```bash
gcloud firestore databases list
gcloud firestore documents list tenants/TENANT_ID/receipts
```

---

## Rollback

### Rollback a SKU deployment

```bash
# Option 1: Terraform (target specific SKU)
terraform apply -target='module.skus["my_sku"]' -var-file=old.tfvars

# Option 2: Cloud Run (previous revision)
gcloud run services update-traffic my_sku --to-revisions PREV=100
```

### Rollback entire catalog

```bash
# Restore previous state from GCS versioning
gsutil cp gs://my-project-tfstate/autonomics/catalog#<VERSION> ./terraform.tfstate
terraform apply -auto-approve
```

---

## Cost Optimization

### SKU Sizing

```hcl
# In module:
cpu    = "0.5"  # Shared CPU (smaller SKUs)
memory = "256Mi" # Lower memory (lean FSM)

min_instances = 0  # Scale to zero when idle
max_instances = 10
```

### Pub/Sub (pull vs. push)

- **Push** (current): Cloud Run listens, Pub/Sub calls /pubsub
- **Pull**: Cost slightly higher, more resilient for slow consumers

---

## Security Best Practices

1. **Backend State**:
   - Versioning enabled (recovery)
   - Access logging enabled (audit)
   - Object lock for prod (prevent deletion)

2. **Service Accounts**:
   - Least privilege (action_groups → roles)
   - Short-lived credentials (metadata server)
   - No static keys (use Workload Identity when possible)

3. **Secrets**:
   - HMAC in Secret Manager (not in code)
   - Rotated via Terraform drift detection
   - Audit trail via Cloud Audit Logs

4. **Cloud Run**:
   - Container scan enabled (vulnerability detection)
   - Min instances = 0 (auto cold-start)
   - Timeout = 30s (fail fast)

---

## Troubleshooting

### Terraform state lock conflicts

```bash
# Check who has the lock
gsutil stat gs://my-project-tfstate/autonomics/catalog.tflock

# Force unlock (caution!)
terraform force-unlock <LOCK_ID>
```

### Cloud Build trigger not firing

```bash
# Check trigger configuration
gcloud builds triggers list
gcloud builds triggers describe <TRIGGER_ID>

# Manually test
gcloud builds submit --branch=main --config=infra/cloudbuild/cloudbuild.yaml
```

### Cloud Run deployment fails

```bash
# Check service status
gcloud run services describe my_sku --region us-central1

# View recent revisions
gcloud run revisions list --service my_sku --region us-central1

# Check logs
gcloud run services logs read my_sku --limit 50
```

---

## Integration with CI/CD

### GitHub Actions (optional wrapper)

```yaml
name: Deploy Autonomics

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      # Cloud Build already handles build + deploy
      # This is optional follow-up testing

      - name: Wait for Cloud Run deployment
        run: |
          for i in {1..30}; do
            if curl -sf ${{ secrets.SERVICE_URL }}/health; then
              echo "Health check passed"
              exit 0
            fi
            sleep 10
          done
          exit 1

      - name: Run adversarial tests
        run: |
          SERVICE_URL=${{ secrets.SERVICE_URL }} ./run_adversarial.sh
```

---

## References

- [Terraform Google Provider](https://registry.terraform.io/providers/hashicorp/google/latest/docs)
- [Cloud Run Terraform Docs](https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/cloud_run_v2_service)
- [Pub/Sub Terraform Docs](https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/pubsub_topic)
- [Cloud Build Terraform Docs](https://registry.terraform.io/providers/hashicorp/google/latest/docs/resources/cloudbuild_trigger)

---

**Last Updated**: 2026-01-25 | **Version**: 1.0 (Production-Ready)
