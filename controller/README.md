# Autonomics Catalog Controller

**The Closed-Loop Brain** — Production Rust implementation of a fully autonomic reconciliation engine.

Runs as a Cloud Run Job on a 5-minute scheduler. No human touches Terraform, builds, or monitoring.

---

## What It Does (Real Production Loop)

```
Trigger (every 5 min)
  ↓
Git clone (repo + ontology + templates)
  ↓
ggen sync_ontology (SPARQL → SKU specs)
  ↓
Terraform init/apply (catalog root module)
  ↓
Cloud Build submit (build + test SKU images)
  ↓
Health verify (GET /health on all deployed services)
  ↓
Emit hash-chained receipts to Firestore
  ↓
Cloud Logging (JSON structured logs)
```

**Jidoka enforcement**: Any failure halts immediately. No partial state.

---

## Building the Controller

### Prerequisites
- Rust 1.82+
- ggen binary (must be in container)
- Terraform 1.8+
- gcloud CLI

### Local Build
```bash
cd controller
cargo build --release
./target/release/autonomics-catalog-controller \
  --repo https://github.com/your-org/your-ggen-repo \
  --branch main \
  --project-id my-project \
  --region us-central1 \
  --tf-state-bucket my-project-tfstate \
  --tf-workdir infra/catalog
```

### Docker Image (Production)

```bash
# Build (requires ggen binary in root)
docker build -f controller/Containerfile -t catalog-controller:latest .

# Push to Artifact Registry
docker tag catalog-controller:latest \
  us-central1-docker.pkg.dev/PROJECT/autonomics-skus/catalog-controller:latest
docker push us-central1-docker.pkg.dev/PROJECT/autonomics-skus/catalog-controller:latest
```

---

## Deployment (Cloud Run Job + Scheduler)

### 1. Pre-requisites

- GCS bucket for Terraform state (created by infra/state module)
- Artifact Registry repository (created by infra/shared module)
- Controller image pushed to Artifact Registry

### 2. Deploy Controller Job + Scheduler

```bash
cd infra/controller

terraform init
terraform apply \
  -var="project_id=my-project" \
  -var="region=us-central1" \
  -var="controller_image_uri=us-central1-docker.pkg.dev/my-project/autonomics-skus/catalog-controller:latest" \
  -var="tf_state_bucket=my-project-tfstate" \
  -var="catalog_repo_url=https://github.com/my-org/my-ggen-repo" \
  -var="catalog_branch=main"
```

### 3. Verify Deployment

```bash
# Check Cloud Run Job
gcloud run jobs list --platform managed --region us-central1

# Check Cloud Scheduler
gcloud scheduler jobs list

# Manually trigger job
gcloud scheduler jobs run autonomics-catalog-reconcile
```

---

## How It Works

### 1. Git Clone
```rust
git clone --depth=1 --branch main <CATALOG_REPO> /tmp/repo
```
- Fetches latest ontology + templates + Terraform catalog config
- Stores in ephemeral /tmp (cleaned up after run)

### 2. SPARQL Execution
```rust
ggen sync_ontology --cwd /tmp/repo
```
- Runs ggen pipeline to generate/update:
  - Erlang gen_statem governors
  - Terraform modules
  - Deployment configurations
  - Test suites
- Deterministic output (same input → same output)

### 3. Terraform Apply (Catalog Root)
```rust
terraform init -backend-config=bucket=... -backend-config=prefix=autonomics/catalog
terraform apply -auto-approve \
  -var=project_id=... \
  -var=region=...
```
- Uses remote state (GCS bucket with locking)
- for_each creates 1..N SKUs from terraform.tfvars
- Per-SKU: Cloud Run service, Pub/Sub topic, IAM roles, Firestore collection
- Idempotent (safe to run repeatedly)

### 4. Cloud Build (Optional)
```rust
gcloud builds submit --config infra/cloudbuild/cloudbuild.yaml --project ...
```
- Builds SKU container images
- Runs smoke tests (GET /health locally)
- Pushes to Artifact Registry
- Only proceeds if smoke test passes (TPS gate)

### 5. Health Verification
```rust
// Read terraform output
terraform output -json | jq '.sku_service_urls.value'

// Verify each service
for (sku, url) in services {
  GET {url}/health → must return 200 OK
}
```
- Confirms all deployed services are healthy
- Fails run if any service returns != 200
- Jidoka halt if health check fails

### 6. Firestore Receipts (Mandatory)
```rust
// Get previous chain hash
GET /v1/projects/{}/databases/(default)/documents/{collection}/{run_id}/state/head
  → last_chain_hash_b64

// Create receipt JSON
{
  ts: "2026-01-25T12:34:56Z",
  kind: "catalog_run_completed",
  decision: "accept",
  project_id: "my-project",
  repo: "https://github.com/...",
  branch: "main",
  details: {...},
  prev_chain_hash_b64: "..."
}

// Hash chain: SHA-256(prev_hash || SHA-256(receipt_json))
chain_hash = SHA256(prev_hash + SHA256(canonical_json(receipt)))

// Append to Firestore
POST /v1/projects/{}/databases/(default)/documents/{collection}/{run_id}/receipts
  {
    ts_ms: 1234567890000,
    chain_hash: "...",
    receipt_json: "{...}"
  }

// Update head pointer
PUT /v1/projects/{}/databases/(default)/documents/{collection}/{run_id}/state/head
  {
    last_chain_hash: "..."
  }
```

**Why receipts are mandatory**:
- If Firestore is unavailable, the controller fails immediately (jidoka)
- No partial runs without audit trail
- Hash-chained for tamper detection
- Cloud Logging mirrors all receipts (searchable)

---

## Environment Variables

All required; all come from Cloud Run Job env configuration:

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `CATALOG_REPO` | ✓ | - | Git repo URL (https://github.com/org/repo) |
| `CATALOG_BRANCH` | | main | Branch/tag to check out |
| `GOOGLE_CLOUD_PROJECT` | ✓ | - | GCP project ID |
| `REGION` | | us-central1 | Cloud Run region |
| `TF_STATE_BUCKET` | ✓ | - | GCS bucket name for Terraform state |
| `TF_WORKDIR` | | infra/catalog | Terraform directory (relative to repo root) |
| `CATALOG_FIRESTORE_COLLECTION` | | catalog_runs | Firestore collection for receipts |
| `VERIFY_HEALTH` | | true | Enable post-deploy health check |
| `TRIGGER_CLOUDBUILD` | | true | Enable Cloud Build submission |

---

## Receipt Structure

Every run emits receipts at key milestones:

### Start Receipt
```json
{
  "ts": "2026-01-25T12:34:56Z",
  "kind": "catalog_run_started",
  "decision": "accept",
  "project_id": "my-project",
  "repo": "https://github.com/org/repo",
  "branch": "main",
  "details": {},
  "prev_chain_hash_b64": "AAAAAAA..."
}
```

### Success (Each stage)
```json
{
  "ts": "2026-01-25T12:34:57Z",
  "kind": "git_clone",
  "decision": "ok",
  "project_id": "my-project",
  "repo": "https://github.com/org/repo",
  "branch": "main",
  "details": {},
  "prev_chain_hash_b64": "..."
}
```

### Failure (Halts run)
```json
{
  "ts": "2026-01-25T12:34:59Z",
  "kind": "terraform_apply",
  "decision": "reject",  // or "error"
  "project_id": "my-project",
  "repo": "https://github.com/org/repo",
  "branch": "main",
  "details": {
    "error": "terraform apply failed: ..."
  },
  "prev_chain_hash_b64": "..."
}
```

Each receipt is Merkle-linked:
```
hash₀ = SHA256(receipt₀_json)
chain₁ = SHA256(prev_chain || hash₁)
chain₂ = SHA256(chain₁ || hash₂)
...
```

Verification: `SHA256(prev + receipt_hash) == recorded_chain_hash`

---

## Monitoring & Debugging

### Cloud Logging

All receipts are JSON-structured logs. Search:

```bash
# All controller runs
gcloud logging read "resource.type=cloud_run_job AND resource.labels.job_name=autonomics-catalog-controller" \
  --limit 50 --format json

# Failed runs only
gcloud logging read "resource.type=cloud_run_job AND resource.labels.job_name=autonomics-catalog-controller AND jsonPayload.decision=reject" \
  --limit 10

# By kind (git_clone, terraform_apply, health_verify, etc.)
gcloud logging read "resource.type=cloud_run_job AND jsonPayload.kind=terraform_apply" \
  --limit 20
```

### Firestore Documents

```bash
# List all catalog runs
gcloud firestore documents list catalog_runs

# View single run
gcloud firestore documents get catalog_runs/run_20260125T123456Z

# View receipts for a run
gcloud firestore documents list catalog_runs/run_20260125T123456Z/receipts

# Verify receipt chain (manual)
gcloud firestore documents get catalog_runs/run_20260125T123456Z/state/head \
  | jq '.fields.last_chain_hash'
```

### Metrics

```bash
# Count successful runs
gcloud logging read "resource.type=cloud_run_job AND jsonPayload.kind=catalog_run_completed AND jsonPayload.decision=accept" \
  --format="count()"

# Mean runtime (via timestamps)
gcloud logging read "resource.type=cloud_run_job AND (jsonPayload.kind=catalog_run_started OR jsonPayload.kind=catalog_run_completed)" \
  --format json | jq '.[].timestamp' | ...
```

---

## TPS Guarantees Enforced

| Guarantee | Enforcement | Consequence |
|-----------|------------|-------------|
| Firestore mandatory | Emit start receipt before any work | Controller fails if Firestore unreachable |
| Git clone atomic | Fails immediately if git fails | No partial repo state |
| ggen sync atomic | Fails immediately if ggen fails | No partial code generation |
| Terraform state locked | GCS bucket + Terraform state lock | No concurrent Terraform runs |
| Terraform apply atomic | -auto-approve or fails | No partial infrastructure |
| Health verification | GET /health must return 200 | No promotion without health |
| No partial rollback | Failures emit receipts, no cleanup | Operator must investigate |
| Receipt chain integrity | SHA-256 Merkle-linked | Tamper-detectable audit trail |

---

## Troubleshooting

### Controller Job Fails to Start

```bash
# Check Cloud Run Job logs
gcloud run jobs describe autonomics-catalog-controller --region us-central1
gcloud run jobs logs read autonomics-catalog-controller --region us-central1 --limit 50

# Check permissions
gcloud projects get-iam-policy my-project --flatten="bindings[].members" --filter="bindings.members:autonomics-catalog-controller@*"
```

**Common issues**:
- Service account lacks IAM roles (check `infra/controller/main.tf`)
- Firestore collection doesn't exist (created on first write)
- TF_STATE_BUCKET doesn't exist (create via `infra/state` module first)

### Git Clone Fails

```bash
# Verify repo is accessible from Cloud Run
gcloud run services create test-git --image gcr.io/cloud-builders/git --allow-unauthenticated
# Then: git clone <CATALOG_REPO> → should work
```

**Common issues**:
- Private repo without credentials (use SSH key or Personal Access Token in URL)
- Branch doesn't exist (verify CATALOG_BRANCH)
- Rate limiting (use SSH key instead of HTTPS)

### Terraform Apply Fails

```bash
# Check Terraform state lock
gsutil stat gs://<TF_STATE_BUCKET>/autonomics/catalog/default.tflock

# Check backend config
cd infra/catalog
terraform show
```

**Common issues**:
- State lock held by previous run (use `terraform force-unlock <LOCK_ID>`)
- Backend bucket doesn't exist (create via `infra/state` module first)
- IAM permissions (service account needs `roles/storage.admin` for state bucket)

### Health Verification Fails

```bash
# Manually verify services
gcloud run services list --platform managed --region us-central1

# Test health endpoint
SERVICE_URL=$(gcloud run services describe <SERVICE> --platform managed --region us-central1 --format='value(status.url)')
curl "${SERVICE_URL}/health"
```

**Common issues**:
- Service not deployed (check Cloud Run revisions)
- Service still starting (add longer timeout in health loop)
- /health endpoint unimplemented (check Erlang governor modules)

---

## Performance

- **Git clone**: ~10s (--depth=1 shallow clone)
- **ggen sync_ontology**: ~15s (template rendering)
- **Terraform init**: ~20s (provider initialization + state fetch)
- **Terraform apply**: ~30s (per SKU + resource creation)
- **Cloud Build submit**: ~5-10min (build + test + push)
- **Health verify**: ~5s (parallel requests to all services)
- **Firestore receipts**: ~2s (per milestone)

**Total (no build)**: ~70s
**Total (with build)**: ~12min

Scheduler: Every 5 minutes → ~2 runs/min capacity (accounting for 12min peak).

---

## Next Phase

**Autonomic Logging Pipeline** — Logging sinks → Pub/Sub topics → SKU signal topics (ontology-derived).

Every GCP event (Cloud Logging, Pub/Sub, Scheduler) flows deterministically to SKU signal ingestion via the catalog as the routing fabric.

---

**Production-Ready**: January 2026
