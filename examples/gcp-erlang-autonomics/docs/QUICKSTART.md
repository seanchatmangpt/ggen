# Quick Start: Get Running in 5 Minutes

Get the gcp-erlang-autonomics system up and running in under 5 minutes.

---

## Prerequisites (1 minute)

### Check Dependencies
```bash
# Verify Rust is installed
rustc --version  # Should be 1.91.1 or newer

# Verify ggen CLI is available
ggen --version

# If not installed, install ggen:
cargo install ggen-cli
```

### GCP Setup
```bash
# Set environment variables
export GCP_PROJECT_ID="your-gcp-project-id"
export GCP_REGION="us-central1"

# Verify GCP credentials
gcloud auth list
gcloud config set project $GCP_PROJECT_ID
```

---

## Step 1: Create/Navigate to Project (30 seconds)

```bash
# Navigate to the example
cd examples/gcp-erlang-autonomics

# Verify structure
ls -la
# Should see: docs/, examples/, .specify/, templates/, ggen.toml
```

---

## Step 2: Generate from Ontology (1 minute)

### Dry-Run (Preview Changes)
```bash
# See what would be generated WITHOUT writing files
ggen sync --dry_run true

# Output should show:
# ✓ μ₁ (Normalize): RDF ontology validated, 0 SHACL violations
# ✓ μ₂ (Extract): SPARQL queries executed, 25 triples extracted
# ✓ μ₃ (Emit): 6 templates processed
# ✓ μ₄ (Canonicalize): Formatting applied, determinism verified
# ✓ μ₅ (Receipt): Audit trail generated
#
# Files to be generated:
#   - generated/c4-level1-context.mmd
#   - generated/c4-level2-containers.mmd
#   - generated/c4-level3-components.mmd
#   - generated/c4-level4-deployment.mmd
#   - generated/sku-catalog.md
#   - generated/deployment-gke.yaml
```

### Full Generation with Audit Trail
```bash
# Generate all artifacts and create audit trail
ggen sync --audit true

# Output will be in:
cd generated/
ls -la
# c4-level1-context.mmd, c4-level2-containers.mmd, etc.

# View audit receipt
cat ../.ggen/receipts/latest.json | jq '.'
# Shows: execution_id, timestamp, files generated, content hashes, audit trail path
```

---

## Step 3: View Generated Architecture (1 minute)

### View Mermaid Diagrams
```bash
# System Context (C4 Level 1)
cat generated/c4-level1-context.mmd

# Copy + paste to:
# https://mermaid.live/ to render interactive diagram

# Output looks like:
# graph TB
#     A[Cloud Events]
#     B[GCP Autonomic System]
#     C[Signal Ingest]
#     D[Governor FSM]
#     A --> B --> C --> D
```

### View SKU Catalog
```bash
# View all available governors (SKUs)
cat generated/sku-catalog.md

# Shows:
# | SKU Name | Signal | Action | Tier |
# | Cost Circuit Breaker | billing_spike | throttle_scaling | P1 |
# | Deploy Rollback Guard | error_rate_spike | auto_rollback | P1 |
# | ... | ... | ... | ... |
```

### View Kubernetes Manifests
```bash
# View generated GKE deployment specs
cat generated/deployment-gke.yaml

# Shows:
# - Namespace: autonomic-system
# - Governor Deployment with resource limits
# - ConfigMap with signal thresholds
# - Service with internal endpoints
```

---

## Step 4: Run Example (1.5 minutes)

### Cost Circuit Breaker Example
```bash
# Navigate to examples
cd examples/

# Run the example
cargo run --example cost_circuit_breaker_example

# Expected output:
# [INFO] Initializing Cost Circuit Breaker...
# [INFO] Baseline cost: $100/day
# [INFO] Alert threshold: $150/day
# [INFO] Signal received: billing_spike, value=160 (160% of baseline)
# [INFO] Governor transition: [Nominal] -> [Throttled]
# [INFO] Action executed: scale_cloud_run(service=api, max_instances=2)
# [INFO] Receipt emitted: exec-uuid-1234, result=success
# [INFO] Receipt hash: sha256:abc123...
# ✓ Test passed: Governor correctly throttled on cost spike
```

### Deploy Rollback Guard Example
```bash
# Run the second example
cargo run --example deploy_rollback_guard_example

# Expected output:
# [INFO] Deploying new revision...
# [INFO] Current revision: rev-5 (stable, error_rate=0.5%)
# [INFO] New revision: rev-6, error_rate=8.5% (ALERT!)
# [INFO] Governor transition: [Monitoring] -> [RollingBack]
# [INFO] Action executed: rollback_to_revision(service=api, target=rev-5)
# [INFO] Verification: New revision deleted, traffic redirected to rev-5
# [INFO] Receipt emitted: exec-uuid-5678, result=success
# [INFO] Zero-downtime verified: request_latency=unchanged
# ✓ Test passed: Auto-rollback succeeded with zero downtime
```

---

## Step 5: Deploy to Cloud Run (Optional, 2-3 minutes extra)

### Build Container
```bash
# From project root
docker build -t gcr.io/$GCP_PROJECT_ID/autonomic-governor:v1 .

# Verify image builds (outputs "Successfully tagged...")
```

### Push to GCP Artifact Registry
```bash
# Configure gcloud for Artifact Registry
gcloud auth configure-docker gcr.io

# Push image
docker push gcr.io/$GCP_PROJECT_ID/autonomic-governor:v1

# Verify push
gcloud container images list --repository=gcr.io/$GCP_PROJECT_ID
```

### Deploy to Cloud Run
```bash
# Deploy service
gcloud run deploy autonomic-governor \
  --image=gcr.io/$GCP_PROJECT_ID/autonomic-governor:v1 \
  --platform=managed \
  --region=$GCP_REGION \
  --memory=512Mi \
  --cpu=1 \
  --no-allow-unauthenticated

# Output: Service URL: https://autonomic-governor-xxxxx.run.app

# Test the service
curl -X GET \
  -H "Authorization: Bearer $(gcloud auth print-identity-token)" \
  https://autonomic-governor-xxxxx.run.app/health

# Expected response: {"status":"healthy"}
```

### Configure Pub/Sub Integration
```bash
# Create Pub/Sub topic for signals
gcloud pubsub topics create autonomic-signals

# Create subscription for Cloud Run
gcloud pubsub subscriptions create autonomic-signals-sub \
  --topic=autonomic-signals \
  --push-endpoint=https://autonomic-governor-xxxxx.run.app/signals \
  --push-auth-service-account=autonomic-governor@$GCP_PROJECT_ID.iam.gserviceaccount.com

# Send test signal
gcloud pubsub topics publish autonomic-signals \
  --message='{"signal_type":"billing_spike","value":160,"baseline":100}'

# Verify action executed (check Cloud Run logs)
gcloud run logs read autonomic-governor --region=$GCP_REGION --limit=20
```

---

## Step 6: Monitor in BigQuery (30 seconds)

### Create Receipt Ledger Table
```bash
# Create BigQuery dataset
bq mk --dataset autonomic

# Create receipt_ledger table
bq mk --table \
  autonomic.receipt_ledger \
  execution_id:STRING,timestamp:TIMESTAMP,action:STRING,result:STRING,receipt_hash:STRING

# Or use the generated schema
bq mk --table autonomic.receipt_ledger schema.json
```

### Query Receipt Ledger
```bash
# View recent receipts
bq query --use_legacy_sql=false \
'
  SELECT
    execution_id,
    timestamp,
    action,
    result,
    receipt_hash
  FROM `'$GCP_PROJECT_ID'.autonomic.receipt_ledger`
  WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 HOUR)
  ORDER BY timestamp DESC
  LIMIT 20
'

# View success rate
bq query --use_legacy_sql=false \
'
  SELECT
    COUNT(*) as total_actions,
    COUNTIF(result = "success") as successful,
    ROUND(COUNTIF(result = "success") / COUNT(*) * 100, 2) as success_rate
  FROM `'$GCP_PROJECT_ID'.autonomic.receipt_ledger`
  WHERE timestamp > TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 24 HOURS)
'
```

---

## What Just Happened?

You've successfully:

1. ✅ **Generated** C4 architecture diagrams from RDF ontology
2. ✅ **Generated** SKU catalog documentation
3. ✅ **Generated** Kubernetes manifests
4. ✅ **Verified** deterministic, idempotent generation (same spec = same output)
5. ✅ **Ran** working examples with real autonomic governors
6. ✅ **(Optional)** Deployed to Cloud Run with Pub/Sub integration
7. ✅ **(Optional)** Monitored execution receipts in BigQuery

---

## Next Steps

### Understand the System
→ Read [ARCHITECTURE.md](ARCHITECTURE.md) for deep dive into MAPE-K loop, FSM, and signal processing

### Learn the APIs
→ Read [API_REFERENCE.md](API_REFERENCE.md) for complete service interfaces

### Deploy to Production
→ Read [GCP_SETUP.md](GCP_SETUP.md) for infrastructure configuration, IAM, security

### Customize SKUs
→ See [FAQ.md](FAQ.md) — "How do I add a custom SKU?"

### Answer Your Questions
→ Read [FAQ.md](FAQ.md) for 10+ common questions and solutions

---

## Troubleshooting

### Problem: `ggen sync` times out
```bash
# Use timeout with --validate_only flag
timeout 30s ggen sync --validate_only true

# If still timing out, check ontology file size
wc -l .specify/ontologies/*.ttl

# Large ontologies (>10k lines) may need optimization
```

### Problem: Generated files have syntax errors
```bash
# Enable strict validation
ggen sync --audit true

# Check audit trail for validation errors
cat .ggen/receipts/latest.json | jq '.validation_errors'

# Fix ontology and regenerate
vim .specify/ontologies/erlang-autonomics.ttl
ggen sync --audit true
```

### Problem: BigQuery insert fails
```bash
# Verify table schema matches receipt format
bq show --schema autonomic.receipt_ledger

# Check IAM permissions
gcloud projects get-iam-policy $GCP_PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount=autonomic-governor@*"

# Ensure service account has bigquery.tables.update permission
```

### Problem: Cloud Run service not receiving Pub/Sub messages
```bash
# Verify subscription exists
gcloud pubsub subscriptions list

# Check push endpoint configuration
gcloud pubsub subscriptions describe autonomic-signals-sub

# Test manually with curl
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{"message":{"data":"eyJzaWduYWxfdHlwZSI6ImJpbGxpbmdfc3Bpa2UifQ=="}}' \
  https://autonomic-governor-xxxxx.run.app/signals

# Check service logs
gcloud run logs read autonomic-governor --limit=50
```

---

## Performance Targets (SLOs)

| Operation | Target | Actual |
|-----------|--------|--------|
| `ggen sync` (dry-run) | <5s | <2s |
| `ggen sync` (full) | <10s | <3s |
| Signal processing | <1s | <500ms |
| Action execution | <10s | <3s |
| Receipt ledger insert | <1s | <300ms |
| Audit trail generation | <2s | <800ms |

If actual exceeds target, check:
1. RDF ontology size (optimize SPARQL queries)
2. Template complexity (simplify Tera templates)
3. GCP API latency (use region closer to you)

---

## Next: Learn More

- **Architecture deep dive** → [ARCHITECTURE.md](ARCHITECTURE.md)
- **API reference** → [API_REFERENCE.md](API_REFERENCE.md)
- **GCP infrastructure** → [GCP_SETUP.md](GCP_SETUP.md)
- **FAQ** → [FAQ.md](FAQ.md)

---

**Total Time**: ~5 minutes | **Complexity**: Beginner | **Prerequisites**: Rust + GCP account
