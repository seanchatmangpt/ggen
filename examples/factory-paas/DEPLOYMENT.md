# FactoryPaaS GCP Deployment Guide

**Platform**: Google Cloud Platform (GCP)
**Automation**: Terraform + Cloud Build
**Version**: 1.0.0
**Last Updated**: 2026-01-24

---

## 📋 Prerequisites

### Required Tools

| Tool | Version | Installation |
|------|---------|--------------|
| **ggen** | ≥6.0.0 | `cargo install ggen-cli` or [brew install](../../README.md#quick-start-5-minutes) |
| **Terraform** | ≥1.5.0 | [terraform.io/downloads](https://terraform.io/downloads) |
| **gcloud CLI** | ≥450.0.0 | [cloud.google.com/sdk/install](https://cloud.google.com/sdk/install) |
| **Rust toolchain** | ≥1.91.1 | [rustup.rs](https://rustup.rs) |

### GCP Requirements

- **GCP Account** with billing enabled
- **GCP Project** (create via [console.cloud.google.com](https://console.cloud.google.com))
- **Billing Account** linked to project
- **Quotas**: Sufficient for e2-medium VM + Cloud SQL (check [quotas page](https://console.cloud.google.com/iam-admin/quotas))

### Permissions Required

Your GCP user or service account needs:

```yaml
roles:
  - roles/compute.admin          # Compute Engine VMs
  - roles/cloudsql.admin         # Cloud SQL databases
  - roles/storage.admin          # Cloud Storage buckets
  - roles/iam.serviceAccountAdmin # Service account creation
  - roles/resourcemanager.projectIamAdmin # IAM bindings
```

---

## 🚀 Quick Deployment (10 Minutes)

### Step 1: Generate Infrastructure Code

```bash
cd examples/rust-attribution-context

# Generate all code from ontology (including Terraform)
ggen sync
```

**Output**:
```
🟢 Ontology validation: PASSED (6 files, 428 triples)
🟢 SPARQL extraction: PASSED (15 queries, 847 bindings)
🟢 Code generation: PASSED (24 files, 12,847 lines)
✓ Generated: world/infra/main.tf (GCP infrastructure)
✓ Generated: world/infra/variables.tf (Terraform variables)
✓ Generated: world/infra/outputs.tf (Terraform outputs)
```

### Step 2: Configure GCP Project

```bash
# Set your GCP project ID
export GCP_PROJECT_ID="factorypaas-prod-123"
export GCP_REGION="us-central1"
export GCP_ZONE="us-central1-a"

# Authenticate with GCP
gcloud auth login
gcloud auth application-default login

# Set default project
gcloud config set project $GCP_PROJECT_ID
```

### Step 3: Enable Required APIs

```bash
# Enable GCP services
gcloud services enable compute.googleapis.com \
  sql-component.googleapis.com \
  sqladmin.googleapis.com \
  storage-api.googleapis.com \
  storage-component.googleapis.com \
  cloudresourcemanager.googleapis.com \
  iam.googleapis.com \
  monitoring.googleapis.com \
  logging.googleapis.com
```

### Step 4: Configure Terraform Backend (Optional but Recommended)

```bash
# Create GCS bucket for Terraform state
gsutil mb -p $GCP_PROJECT_ID -l $GCP_REGION gs://$GCP_PROJECT_ID-terraform-state

# Enable versioning (rollback protection)
gsutil versioning set on gs://$GCP_PROJECT_ID-terraform-state

# Create backend configuration
cat > world/infra/backend.tf <<EOF
terraform {
  backend "gcs" {
    bucket = "$GCP_PROJECT_ID-terraform-state"
    prefix = "factorypaas/state"
  }
}
EOF
```

### Step 5: Deploy Infrastructure

```bash
# Deploy using automation script
./world/run/up
```

**What happens**:
1. Terraform initializes (`terraform init`)
2. Terraform plans deployment (`terraform plan`)
3. User confirms changes (interactive prompt)
4. Terraform applies (`terraform apply`)
5. Deployment verification (health checks)

**Expected output**:
```
Terraform will perform the following actions:

  # google_compute_instance.attribution_api will be created
  + resource "google_compute_instance" "attribution_api" {
      + name         = "factorypaas-api-vm"
      + machine_type = "e2-medium"
      + zone         = "us-central1-a"
      ...
    }

  # google_sql_database_instance.postgres will be created
  + resource "google_sql_database_instance" "postgres" {
      + name             = "factorypaas-postgres"
      + database_version = "POSTGRES_15"
      ...
    }

  # google_storage_bucket.receipts will be created
  + resource "google_storage_bucket" "receipts" {
      + name     = "factorypaas-receipts-prod"
      + location = "US-CENTRAL1"
      ...
    }

Plan: 12 to add, 0 to change, 0 to destroy.

Do you want to perform these actions? (yes/no): yes

Apply complete! Resources: 12 added, 0 changed, 0 destroyed.

Outputs:

api_url = "https://api.factorypaas.example.com"
database_ip = "10.128.0.3"
receipts_bucket = "factorypaas-receipts-prod"
```

### Step 6: Verify Deployment

```bash
# Run verification script
./world/run/verify
```

**Expected output**:
```
✓ API health: https://api.factorypaas.example.com/health (200 OK)
✓ Metrics endpoint: /metrics (Prometheus format, 47 metrics)
✓ Receipts ledger: 0 receipts (empty on first deploy)
✓ Database: PostgreSQL 15 (migrations applied: 5)
✓ Load balancer: HTTPS with CDN enabled
✓ Monitoring: Dashboard created (12 panels)
```

---

## 🏗️ Infrastructure Components

### Deployed Resources

| Resource | Type | Purpose | Cost/Month (Estimate) |
|----------|------|---------|------------------------|
| **Compute Engine VM** | e2-medium (2 vCPU, 4 GB RAM) | Attribution API server | ~$25 |
| **Cloud SQL PostgreSQL** | db-f1-micro (1 vCPU, 3.75 GB RAM) | Event store + read models | ~$15 |
| **Cloud Storage Bucket** | Standard (US-CENTRAL1) | Receipts ledger | ~$0.50/GB |
| **Cloud Load Balancer** | HTTPS with SSL | API frontend + CDN | ~$18 |
| **Cloud Monitoring** | Dashboards + alerting | Observability | Free tier |
| **Cloud DNS** | Managed zone | Custom domain | ~$0.40 |

**Total Estimated Cost**: ~$60/month (plus traffic/storage costs)

### Network Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Internet (HTTPS traffic)                                    │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ Cloud Load Balancer (HTTPS + CDN)                          │
│ - SSL termination                                           │
│ - Global anycast IP                                         │
│ - HTTP/2, QUIC enabled                                      │
└─────────────────────┬───────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────────────────┐
│ VPC Network (10.128.0.0/20)                                 │
│                                                              │
│  ┌───────────────────────────────────────┐                 │
│  │ Compute Instance (Attribution API)    │                 │
│  │ - Rust binary (Axum + Tokio)          │                 │
│  │ - Internal IP: 10.128.0.2             │                 │
│  │ - Health check: /health               │                 │
│  └────────────┬──────────────────────────┘                 │
│               │                                              │
│               ├───────────> Cloud SQL (PostgreSQL)          │
│               │             - Internal IP: 10.128.0.3       │
│               │             - Database: attribution_db      │
│               │             - Connection pooling: max 20    │
│               │                                              │
│               └───────────> Cloud Storage                   │
│                             - Bucket: receipts-prod         │
│                             - Append-only, versioned        │
│                             - Lifecycle: 7-year retention   │
└─────────────────────────────────────────────────────────────┘
```

### Security Configuration

```
┌─────────────────────────────────────────────────────────────┐
│ Firewall Rules                                              │
├─────────────────────────────────────────────────────────────┤
│ ALLOW tcp:443 from 0.0.0.0/0 (Load Balancer)               │
│ ALLOW tcp:22  from 35.235.240.0/20 (IAP for SSH)           │
│ DENY  all     from 0.0.0.0/0 (default deny)                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ IAM Service Accounts                                        │
├─────────────────────────────────────────────────────────────┤
│ api-server@project.iam.gserviceaccount.com                  │
│ - roles/cloudsql.client (Cloud SQL access)                 │
│ - roles/storage.objectCreator (receipts write)             │
│ - roles/logging.logWriter (Cloud Logging)                  │
│                                                              │
│ postgres-backup@project.iam.gserviceaccount.com             │
│ - roles/storage.objectAdmin (backup bucket)                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Cloud SQL Security                                          │
├─────────────────────────────────────────────────────────────┤
│ - Private IP only (no public internet access)              │
│ - SSL/TLS required for connections                         │
│ - Automatic backups (daily at 03:00 UTC)                   │
│ - Point-in-time recovery (7 days)                          │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔧 Configuration

### Environment Variables

Create `.env` file in `world/`:

```bash
# GCP Configuration
GCP_PROJECT_ID="factorypaas-prod-123"
GCP_REGION="us-central1"
GCP_ZONE="us-central1-a"

# Database Configuration
DATABASE_URL="postgresql://factorypaas:CHANGE_ME@10.128.0.3:5432/attribution_db"
DATABASE_POOL_SIZE="20"

# API Configuration
API_PORT="8080"
API_HOST="0.0.0.0"
API_WORKERS="4"

# Receipts Configuration
RECEIPTS_BUCKET="factorypaas-receipts-prod"
RECEIPTS_SIGNING_KEY="CHANGE_ME_64_CHAR_HEX_STRING"

# Monitoring Configuration
OTEL_EXPORTER_OTLP_ENDPOINT="https://monitoring.googleapis.com"
RUST_LOG="info,world=debug"
```

### Terraform Variables

Edit `world/infra/terraform.tfvars`:

```hcl
# Project Configuration
project_id = "factorypaas-prod-123"
region     = "us-central1"
zone       = "us-central1-a"

# Compute Configuration
vm_machine_type = "e2-medium"  # 2 vCPU, 4 GB RAM
vm_disk_size_gb = 20

# Database Configuration
db_tier              = "db-f1-micro"  # 1 vCPU, 3.75 GB RAM
db_disk_size_gb      = 10
db_backup_enabled    = true
db_backup_start_time = "03:00"

# Storage Configuration
receipts_bucket_location = "US-CENTRAL1"
receipts_lifecycle_days  = 2555  # 7 years

# Networking
enable_cdn         = true
ssl_policy         = "MODERN"  # TLS 1.2+
enable_iap         = true      # Identity-Aware Proxy for SSH
```

---

## 🔄 CI/CD Integration

### Google Cloud Build

Create `cloudbuild.yaml` in project root:

```yaml
steps:
  # Step 1: Generate code from ontology
  - name: 'gcr.io/cloud-builders/gcloud'
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        cargo install ggen-cli
        ggen sync
    id: 'generate'

  # Step 2: Build Rust binary
  - name: 'rust:1.91.1'
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        cd world
        cargo build --release
    id: 'build'
    waitFor: ['generate']

  # Step 3: Run tests
  - name: 'rust:1.91.1'
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        cd world
        cargo test --release
    id: 'test'
    waitFor: ['build']

  # Step 4: Deploy infrastructure
  - name: 'hashicorp/terraform:1.5'
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        cd world/infra
        terraform init
        terraform apply -auto-approve
    id: 'deploy'
    waitFor: ['test']

  # Step 5: Verify deployment
  - name: 'gcr.io/cloud-builders/gcloud'
    entrypoint: 'bash'
    args:
      - '-c'
      - |
        ./world/run/verify
    id: 'verify'
    waitFor: ['deploy']

timeout: '30m'

options:
  machineType: 'E2_HIGHCPU_8'
  logging: CLOUD_LOGGING_ONLY
```

### Trigger on Push

```bash
# Create Cloud Build trigger
gcloud builds triggers create github \
  --repo-name="factorypaas" \
  --repo-owner="your-org" \
  --branch-pattern="^main$" \
  --build-config="cloudbuild.yaml"
```

---

## 📊 Monitoring & Alerting

### Cloud Monitoring Dashboard

Automatically created by Terraform:

```
Dashboard: FactoryPaaS Production
Panels:
  - API Request Rate (requests/sec)
  - API Latency (p50, p95, p99)
  - Click Tracking Throughput (clicks/sec)
  - Attribution Computation Time (ms)
  - Database Connections (active/idle)
  - Receipt Write Rate (receipts/sec)
  - Error Rate (errors/sec)
  - CPU Utilization (%)
  - Memory Usage (GB)
  - Disk I/O (MB/sec)
```

### Alerting Policies

```yaml
# API Latency Alert
- name: "API Latency High"
  condition: "p99_latency > 500ms for 5 minutes"
  notification: "pagerduty,email"

# Error Rate Alert
- name: "Error Rate High"
  condition: "error_rate > 1% for 5 minutes"
  notification: "pagerduty,email"

# Database Connections Alert
- name: "Database Connections Exhausted"
  condition: "active_connections > 18 (90% of pool)"
  notification: "email"

# Receipt Write Failures
- name: "Receipt Write Failures"
  condition: "receipt_write_errors > 0 for 1 minute"
  notification: "pagerduty,email,sms"  # Critical
```

---

## 🔒 Security Hardening

### Enable VPC Service Controls

```bash
# Create perimeter
gcloud access-context-manager perimeters create factorypaas-perimeter \
  --title="FactoryPaaS Production" \
  --resources="projects/$GCP_PROJECT_ID" \
  --restricted-services="storage.googleapis.com,sqladmin.googleapis.com"
```

### Enable Binary Authorization

```bash
# Require signed container images
gcloud container binauthz policy import policy.yaml
```

### Enable Cloud Armor (DDoS Protection)

```hcl
# Add to world/infra/main.tf
resource "google_compute_security_policy" "ddos_protection" {
  name = "factorypaas-ddos-protection"

  rule {
    action   = "rate_based_ban"
    priority = "1000"
    match {
      versioned_expr = "SRC_IPS_V1"
      config {
        src_ip_ranges = ["*"]
      }
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"
      enforce_on_key = "IP"
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
    }
  }
}
```

---

## 🧪 Deployment Testing

### Pre-Deployment Checks

```bash
# Validate Terraform configuration
cd world/infra && terraform validate

# Plan deployment (dry-run)
terraform plan -out=tfplan

# Cost estimation
terraform show -json tfplan | jq '.resource_changes[].change.actions'
```

### Post-Deployment Verification

```bash
# Health checks
./world/run/verify

# Load test (Gatling)
cd world/tests/load && ./run_load_test.sh

# Receipt integrity test
cargo make verify-receipts --manifest-path world/Cargo.toml
```

---

## 🔄 Updating Deployment

### To Update Infrastructure

1. **Edit ontology**: `vim ontology/infra.ttl`
2. **Regenerate**: `ggen sync`
3. **Plan changes**: `cd world/infra && terraform plan`
4. **Apply changes**: `terraform apply`

### To Update Application Code

1. **Edit ontology**: `vim ontology/entities.ttl` (or commands, events, etc.)
2. **Regenerate**: `ggen sync`
3. **Build**: `cd world && cargo build --release`
4. **Deploy**: `./world/run/up` (redeploys VM with new binary)

---

## 🗑️ Teardown

### Delete All Resources

```bash
# Destroy infrastructure (interactive confirmation)
./world/run/down
```

**Or manually**:

```bash
cd world/infra
terraform destroy
```

**Warning**: This deletes:
- Compute instances (VM destroyed)
- Cloud SQL databases (data loss)
- Storage buckets (receipts deleted, unless lifecycle retention applies)
- Load balancers (traffic interrupted)

**Backup first**:

```bash
# Backup receipts
gsutil -m cp -r gs://factorypaas-receipts-prod ./backups/receipts

# Backup database
gcloud sql export sql factorypaas-postgres \
  gs://factorypaas-backups/postgres-$(date +%Y%m%d).sql \
  --database=attribution_db
```

---

## 📞 Support

**Deployment Issues**: support@factorypaas.example.com
**Infrastructure Questions**: https://docs.factorypaas.example.com/deployment

---

**Version**: 1.0.0
**Last Updated**: 2026-01-24
**Tested on**: GCP (us-central1), Terraform 1.5.7, ggen 6.0.0
