# Terraform Reference - ggen Erlang Autonomic System

**Version**: v6.0.0 (Production-Ready)
**Last Updated**: January 2026
**Audience**: Platform engineers, DevOps, infrastructure architects
**Maintenance**: Infrastructure team

---

## 📋 Overview

The ggen Erlang autonomic system uses Terraform to provision GCP infrastructure for:
- **Catalog system**: Multi-SKU marketplace infrastructure with terraform state
- **Controller**: Cloud Run job for autonomic reconciliation
- **Cloud Build**: CI/CD pipeline for controller deployment
- **Security**: VPC, IAM, service accounts, secret management
- **Monitoring**: Cloud Logging, Cloud Monitoring, budget alerts

This document provides complete reference for all Terraform modules, variables, outputs, and deployment patterns.

---

## 🏗️ Module Index

```
10 Production Modules
├── 1. infra/state/               ← GCS backend for Terraform state
├── 2. infra/shared/              ← Shared resources (VPC, networks)
├── 3. infra/catalog/             ← Marketplace catalog infrastructure
├── 4. infra/controller/          ← Cloud Run job + Cloud Scheduler
├── 5. infra/cloudbuild/          ← CI/CD pipeline configuration
├── 6. infra/security/            ← IAM, service accounts, roles
├── 7. infra/firestore/           ← Firestore database + collections
├── 8. infra/pubsub/              ← Pub/Sub topics + subscriptions
├── 9. infra/monitoring/          ← Cloud Logging, budgets, alerts
└── 10. infra/governance/         ← Policy constraints, governance rules
```

---

## Module 1: infra/state/ — Terraform State Backend

### Purpose
Persistent storage for Terraform state using Google Cloud Storage. Enables state locking via Firestore for concurrent apply prevention.

### Use Case
- Shared state across team
- Environment isolation (dev/stage/prod = separate Terraform workspaces)
- State locking to prevent concurrent applies

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
  # Required - no default
}

variable "region" {
  description = "GCP region for state bucket"
  type        = string
  default     = "us-central1"
}

variable "bucket_name" {
  description = "GCS bucket name for Terraform state (must be globally unique)"
  type        = string
  # Required - no default
}

variable "versioning_enabled" {
  description = "Enable GCS versioning for state history"
  type        = bool
  default     = true
}

variable "log_bucket" {
  description = "GCS bucket for access logs (optional)"
  type        = string
  default     = ""
}

variable "enable_lock" {
  description = "Enable Firestore state locking"
  type        = bool
  default     = true
}
```

### Output Values

```hcl
output "state_bucket_name" {
  description = "Name of GCS bucket holding Terraform state"
  value       = google_storage_bucket.terraform_state.name
}

output "state_bucket_url" {
  description = "gs:// URL for Terraform state bucket"
  value       = "gs://${google_storage_bucket.terraform_state.name}"
}

output "lock_table_name" {
  description = "Firestore collection name for state locking"
  value       = google_firestore_document.lock_table.name
}
```

### Required IAM Roles
- `roles/storage.admin` — Full access to state bucket
- `roles/firestore.admin` — State locking via Firestore

### Deployment Time
~2 minutes (GCS bucket creation + Firestore setup)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "region": "us-central1",
  "bucket_name": "ggen-autonomics-tf-state-prod-20260118",
  "versioning_enabled": true,
  "log_bucket": "ggen-autonomics-tf-logs-prod",
  "enable_lock": true
}
```

### Key Commands
```bash
# Initialize Terraform backend
terraform init \
  -backend-config="bucket=ggen-autonomics-tf-state-prod-20260118" \
  -backend-config="prefix=terraform/state"

# Verify state backend
terraform state list

# Inspect state (never manually edit!)
terraform state show

# Force unlock (DANGEROUS - only if process crashed)
terraform force-unlock <LOCK_ID>
```

---

## Module 2: infra/shared/ — Shared VPC & Networks

### Purpose
Foundational networking: VPC, subnets, Cloud NAT for outbound connectivity, firewall rules.

### Use Case
- Multi-service communication within VPC
- Private Cloud Run job access to resources
- Isolated network per environment

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Primary GCP region"
  type        = string
  default     = "us-central1"
}

variable "network_name" {
  description = "VPC network name"
  type        = string
  default     = "ggen-autonomics-vpc"
}

variable "subnet_cidr" {
  description = "Subnet CIDR range (e.g., 10.0.0.0/20)"
  type        = string
  default     = "10.0.0.0/20"
}

variable "enable_nat" {
  description = "Enable Cloud NAT for outbound connectivity"
  type        = bool
  default     = true
}

variable "nat_ips_count" {
  description = "Number of Cloud NAT static IPs"
  type        = number
  default     = 2
}

variable "firewall_rules" {
  description = "Custom firewall rules (protocol, ports)"
  type = list(object({
    name      = string
    protocol  = string
    ports     = list(string)
    source_ranges = list(string)
  }))
  default = []
}
```

### Output Values

```hcl
output "vpc_name" {
  description = "VPC network name"
  value       = google_compute_network.vpc.name
}

output "vpc_self_link" {
  description = "VPC self_link for Connector reference"
  value       = google_compute_network.vpc.self_link
}

output "subnet_name" {
  description = "Subnet name in primary region"
  value       = google_compute_subnetwork.main.name
}

output "nat_ips" {
  description = "Cloud NAT static IP addresses"
  value       = google_compute_address.nat_ips[*].address
}

output "firewall_rules" {
  description = "Firewall rule names"
  value       = { for rule in google_compute_firewall.custom_rules : rule.name => rule.id }
}
```

### Required IAM Roles
- `roles/compute.networkAdmin` — VPC, subnets, firewall management
- `roles/compute.securityAdmin` — Service accounts, firewall policies

### Deployment Time
~3 minutes (VPC + subnets + Cloud NAT)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "region": "us-central1",
  "network_name": "ggen-autonomics-vpc",
  "subnet_cidr": "10.0.0.0/20",
  "enable_nat": true,
  "nat_ips_count": 2,
  "firewall_rules": [
    {
      "name": "allow-pubsub-internal",
      "protocol": "tcp",
      "ports": ["443"],
      "source_ranges": ["10.0.0.0/20"]
    }
  ]
}
```

---

## Module 3: infra/catalog/ — Marketplace Catalog Infrastructure

### Purpose
Provisioning Firestore collections, Pub/Sub topics, and data structures for multi-SKU catalog management.

### Use Case
- Store marketplace SKU metadata
- Publish SKU changes to Cloud Pub/Sub
- Enable horizontal scaling via for_each (multi-SKU)

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "firestore_location" {
  description = "Firestore database location (e.g., us-central1)"
  type        = string
  default     = "us-central1"
}

variable "skus" {
  description = "Map of SKU configurations (each gets Firestore + Pub/Sub)"
  type = map(object({
    display_name    = string
    description     = string
    entitlement_ttl = number    # seconds
  }))
  default = {}
  # Example:
  # {
  #   "sku-001-marketplace" = {
  #     display_name    = "Marketplace SKU 001"
  #     description     = "Cloud Marketplace integration"
  #     entitlement_ttl = 86400
  #   }
  # }
}

variable "pubsub_retention" {
  description = "Message retention period (days)"
  type        = number
  default     = 7
}

variable "catalog_collection" {
  description = "Root Firestore collection for catalog"
  type        = string
  default     = "catalog"
}

variable "enable_backup" {
  description = "Enable automated Firestore backups"
  type        = bool
  default     = true
}
```

### Output Values

```hcl
output "firestore_database_name" {
  description = "Firestore database name"
  value       = google_firestore_database.catalog.name
}

output "catalog_collections" {
  description = "Map of created Firestore collections"
  value = {
    for sku, config in var.skus :
    sku => google_firestore_document.catalog[sku].path
  }
}

output "pubsub_topics" {
  description = "Map of Pub/Sub topic names (per SKU)"
  value = {
    for sku in keys(var.skus) :
    sku => google_pubsub_topic.sku_topics[sku].name
  }
}

output "pubsub_subscriptions" {
  description = "Map of Pub/Sub subscription names"
  value = {
    for sku in keys(var.skus) :
    sku => google_pubsub_subscription.sku_subscriptions[sku].name
  }
}
```

### Required IAM Roles
- `roles/datastore.admin` — Firestore collection + document management
- `roles/pubsub.admin` — Topic + subscription creation

### Deployment Time
~5 minutes (Firestore database + N×Pub/Sub topics, where N = number of SKUs)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "firestore_location": "us-central1",
  "skus": {
    "sku-001-marketplace": {
      "display_name": "Marketplace SKU 001",
      "description": "Cloud Marketplace integration",
      "entitlement_ttl": 86400
    },
    "sku-002-enterprise": {
      "display_name": "Enterprise SKU 002",
      "description": "Enterprise features",
      "entitlement_ttl": 2592000
    }
  },
  "pubsub_retention": 7,
  "catalog_collection": "catalog",
  "enable_backup": true
}
```

### for_each Pattern Example

```hcl
# Define SKUs once, provision infrastructure N times
resource "google_firestore_document" "catalog" {
  for_each = var.skus

  project       = var.project_id
  database      = google_firestore_database.catalog.name
  collection    = var.catalog_collection
  document      = each.key

  fields = {
    display_name = {
      string_value = each.value.display_name
    }
    entitlement_ttl = {
      integer_value = each.value.entitlement_ttl
    }
    created_at = {
      timestamp_value = timestamp()
    }
  }
}

# Create Pub/Sub topic per SKU
resource "google_pubsub_topic" "sku_topics" {
  for_each = var.skus

  name                       = "${each.key}-events"
  message_retention_duration = "${var.pubsub_retention * 24 * 3600}s"
}
```

---

## Module 4: infra/controller/ — Cloud Run Job + Cloud Scheduler

### Purpose
Provision Cloud Run job (controller binary) and Cloud Scheduler trigger (autonomic reconciliation).

### Use Case
- Deploy autonomics-catalog-controller image to Cloud Run Job
- Schedule periodic reconciliation via Cloud Scheduler
- Health checks + retries on failure

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Region for Cloud Run deployment"
  type        = string
  default     = "us-central1"
}

variable "controller_image" {
  description = "Container image URI (e.g., gcr.io/.../controller:v1.0)"
  type        = string
  # Required - no default
}

variable "controller_memory" {
  description = "Memory allocation (e.g., 512Mi, 1Gi, 2Gi)"
  type        = string
  default     = "1Gi"
}

variable "controller_cpu" {
  description = "CPU allocation (1, 2, 4)"
  type        = string
  default     = "2"
}

variable "controller_timeout" {
  description = "Execution timeout (seconds)"
  type        = number
  default     = 3600  # 1 hour
}

variable "tf_state_bucket" {
  description = "GCS bucket containing Terraform state"
  type        = string
}

variable "tf_workdir" {
  description = "Terraform working directory in repo"
  type        = string
  default     = "infra/catalog"
}

variable "catalog_repo" {
  description = "Git repository URL (https://github.com/...)"
  type        = string
}

variable "catalog_branch" {
  description = "Git branch for catalog spec"
  type        = string
  default     = "main"
}

variable "firestore_collection" {
  description = "Firestore collection for receipts"
  type        = string
  default     = "catalog_runs"
}

variable "schedule_frequency" {
  description = "Cloud Scheduler cron expression (UTC)"
  type        = string
  default     = "*/15 * * * *"  # Every 15 minutes
}

variable "service_account_email" {
  description = "Service account email for Cloud Run job"
  type        = string
}

variable "env_vars" {
  description = "Environment variables for controller"
  type        = map(string)
  default     = {}
}

variable "secrets" {
  description = "Secret Manager references"
  type        = map(string)
  default     = {}
}
```

### Output Values

```hcl
output "job_name" {
  description = "Cloud Run job name"
  value       = google_cloud_run_v2_job.controller.name
}

output "job_location" {
  description = "Cloud Run job location"
  value       = google_cloud_run_v2_job.controller.location
}

output "job_uid" {
  description = "Cloud Run job unique ID"
  value       = google_cloud_run_v2_job.controller.uid
}

output "scheduler_job_name" {
  description = "Cloud Scheduler job name"
  value       = google_cloud_scheduler_job.controller_trigger.name
}

output "scheduler_frequency" {
  description = "Cloud Scheduler cron expression"
  value       = google_cloud_scheduler_job.controller_trigger.schedule
}
```

### Required IAM Roles
- `roles/run.admin` — Cloud Run job management
- `roles/cloudscheduler.admin` — Cloud Scheduler job creation
- `roles/iam.serviceAccountUser` — Impersonate service account

### Deployment Time
~2 minutes (Cloud Run job + Cloud Scheduler setup)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "region": "us-central1",
  "controller_image": "us-central1-docker.pkg.dev/ggen-autonomics-prod/controller/autonomics-catalog-controller:v1.0.0",
  "controller_memory": "2Gi",
  "controller_cpu": "4",
  "controller_timeout": 3600,
  "tf_state_bucket": "ggen-autonomics-tf-state-prod-20260118",
  "tf_workdir": "infra/catalog",
  "catalog_repo": "https://github.com/seanchatmangpt/ggen-catalog.git",
  "catalog_branch": "main",
  "firestore_collection": "catalog_runs",
  "schedule_frequency": "*/15 * * * *",
  "service_account_email": "controller@ggen-autonomics-prod.iam.gserviceaccount.com",
  "env_vars": {
    "LOG_LEVEL": "INFO",
    "VERIFY_HEALTH": "true"
  },
  "secrets": {
    "GITHUB_TOKEN": "projects/ggen-autonomics-prod/secrets/github-token/versions/latest"
  }
}
```

---

## Module 5: infra/cloudbuild/ — CI/CD Pipeline

### Purpose
Cloud Build configuration for building, testing, and deploying controller image.

### Use Case
- Build controller Docker image on every push
- Run smoke tests
- Push to Artifact Registry
- Deploy to Cloud Run Job

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "region" {
  description = "Default region for resources"
  type        = string
  default     = "us-central1"
}

variable "github_owner" {
  description = "GitHub repository owner"
  type        = string
  # Required - no default
}

variable "github_repo" {
  description = "GitHub repository name"
  type        = string
  # Required - no default
}

variable "github_branch" {
  description = "GitHub branch to trigger on"
  type        = string
  default     = "main"
}

variable "image_repo" {
  description = "Artifact Registry repository (e.g., controller)"
  type        = string
  default     = "controller"
}

variable "image_name" {
  description = "Docker image name"
  type        = string
  default     = "autonomics-catalog-controller"
}

variable "cloudbuild_service_account" {
  description = "Service account for Cloud Build"
  type        = string
}

variable "docker_file_path" {
  description = "Path to Dockerfile within repo"
  type        = string
  default     = "controller/Dockerfile"
}

variable "enable_security_scan" {
  description = "Enable vulnerability scanning for pushed images"
  type        = bool
  default     = true
}

variable "notification_channel" {
  description = "Cloud Monitoring notification channel ID for build alerts"
  type        = string
  default     = ""
}
```

### Output Values

```hcl
output "trigger_name" {
  description = "Cloud Build trigger name"
  value       = google_cloudbuild_trigger.github.name
}

output "trigger_id" {
  description = "Cloud Build trigger ID"
  value       = google_cloudbuild_trigger.github.id
}

output "trigger_filename" {
  description = "Cloud Build config file path in repo"
  value       = google_cloudbuild_trigger.github.filename
}

output "image_repo_name" {
  description = "Artifact Registry repository name"
  value       = google_artifact_registry_repository.controller_images.repository_id
}

output "image_repo_url" {
  description = "Artifact Registry repository URL"
  value       = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.controller_images.repository_id}"
}
```

### Required IAM Roles
- `roles/cloudbuild.admin` — Cloud Build trigger + job management
- `roles/artifactregistry.admin` — Image push to Artifact Registry
- `roles/run.admin` — Deploy to Cloud Run Job (via Cloud Build)

### Deployment Time
~2 minutes (trigger + repository setup)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "region": "us-central1",
  "github_owner": "seanchatmangpt",
  "github_repo": "ggen",
  "github_branch": "main",
  "image_repo": "controller",
  "image_name": "autonomics-catalog-controller",
  "cloudbuild_service_account": "cloudbuild@ggen-autonomics-prod.iam.gserviceaccount.com",
  "docker_file_path": "controller/Dockerfile",
  "enable_security_scan": true,
  "notification_channel": "projects/ggen-autonomics-prod/notificationChannels/1234567890"
}
```

---

## Module 6: infra/security/ — IAM, Service Accounts, Secrets

### Purpose
Identity and access management: service accounts, IAM roles, Secret Manager.

### Use Case
- Manage service account for controller, Cloud Build, Terraform
- Assign minimal required roles (least privilege)
- Store secrets (GitHub token, API keys) securely

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "service_accounts" {
  description = "Map of service accounts to create"
  type = map(object({
    display_name = string
    description  = string
  }))
  default = {
    "controller" = {
      display_name = "Catalog Controller"
      description  = "Service account for autonomic controller"
    }
    "cloudbuild" = {
      display_name = "Cloud Build"
      description  = "Service account for CI/CD pipeline"
    }
  }
}

variable "iam_bindings" {
  description = "IAM role bindings per service account"
  type = map(list(string))
  default = {
    "controller" = [
      "roles/datastore.user",          # Firestore access
      "roles/pubsub.publisher",        # Pub/Sub publish
      "roles/storage.objectViewer",    # Read Terraform state
      "roles/cloudlogging.logWriter"   # Write logs
    ]
    "cloudbuild" = [
      "roles/artifactregistry.admin",  # Image push
      "roles/run.admin",               # Deploy to Cloud Run
      "roles/storage.objectViewer"     # Read Terraform state
    ]
  }
}

variable "secrets" {
  description = "Map of secrets to create in Secret Manager"
  type = map(object({
    data        = string
    description = string
  }))
  default = {}
  # Example:
  # {
  #   "github-token" = {
  #     data        = "ghp_xxxxxxxxxxxxxxxxxxxx"
  #     description = "GitHub personal access token"
  #   }
  # }
}

variable "secret_accessors" {
  description = "IAM bindings for secret accessor role"
  type = map(list(string))
  default = {}
  # Example:
  # {
  #   "github-token" = ["controller"]
  # }
}
```

### Output Values

```hcl
output "service_accounts" {
  description = "Map of created service accounts (email)"
  value = {
    for name, sa in google_service_account.accounts :
    name => sa.email
  }
}

output "secrets" {
  description = "Map of created secrets (names only, no values!)"
  value = {
    for name, secret in google_secret_manager_secret.secrets :
    name => secret.id
  }
  sensitive = true  # Don't print secret IDs in logs
}

output "iam_bindings" {
  description = "Summary of IAM role assignments"
  value = {
    for name, roles in var.iam_bindings :
    name => roles
  }
}
```

### Required IAM Roles
- `roles/iam.securityAdmin` — Service account creation + IAM bindings
- `roles/secretmanager.admin` — Secret creation + access management

### Deployment Time
~3 minutes (service accounts + IAM + secrets)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "service_accounts": {
    "controller": {
      "display_name": "Catalog Controller",
      "description": "Service account for autonomic controller"
    },
    "cloudbuild": {
      "display_name": "Cloud Build",
      "description": "Service account for CI/CD pipeline"
    }
  },
  "iam_bindings": {
    "controller": [
      "roles/datastore.user",
      "roles/pubsub.publisher",
      "roles/storage.objectViewer",
      "roles/cloudlogging.logWriter"
    ],
    "cloudbuild": [
      "roles/artifactregistry.admin",
      "roles/run.admin",
      "roles/storage.objectViewer"
    ]
  },
  "secrets": {
    "github-token": {
      "data": "ghp_xxxxxxxxxxxxxxxxxxxx",
      "description": "GitHub personal access token"
    }
  },
  "secret_accessors": {
    "github-token": ["controller"]
  }
}
```

---

## Module 7: infra/firestore/ — Firestore Database + Collections

### Purpose
Create Firestore database and root collections for receipt tracking, entitlements, signals.

### Use Case
- Store catalog run receipts
- Track entitlement FSM state
- Log autonomic signals (jidoka halts, degraded mode)

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "database_id" {
  description = "Firestore database ID"
  type        = string
  default     = "(default)"
}

variable "location" {
  description = "Firestore database location"
  type        = string
  default     = "us-central1"
}

variable "type" {
  description = "Database type (FIRESTORE_NATIVE or DATASTORE)"
  type        = string
  default     = "FIRESTORE_NATIVE"

  validation {
    condition     = contains(["FIRESTORE_NATIVE", "DATASTORE"], var.type)
    error_message = "Type must be FIRESTORE_NATIVE or DATASTORE."
  }
}

variable "collections" {
  description = "Collections to create with index config"
  type = map(object({
    description = string
    indexes = list(object({
      fields = list(string)
      order  = string  # "ASCENDING" or "DESCENDING"
    }))
  }))
  default = {
    "catalog_runs" = {
      description = "Controller execution receipts"
      indexes = [
        {
          fields = ["status", "timestamp"]
          order  = "DESCENDING"
        }
      ]
    }
    "entitlements" = {
      description = "Marketplace entitlement state"
      indexes = [
        {
          fields = ["sku_id", "account_id", "expires_at"]
          order  = "DESCENDING"
        }
      ]
    }
    "signals" = {
      description = "Autonomic jidoka signals and halts"
      indexes = [
        {
          fields = ["signal_type", "timestamp"]
          order  = "DESCENDING"
        }
      ]
    }
  }
}

variable "enable_backup" {
  description = "Enable automated daily backups"
  type        = bool
  default     = true
}
```

### Output Values

```hcl
output "database_name" {
  description = "Firestore database name"
  value       = google_firestore_database.main.name
}

output "database_uid" {
  description = "Firestore database unique ID"
  value       = google_firestore_database.main.uid
}

output "collections" {
  description = "Created collection names"
  value       = keys(var.collections)
}
```

### Required IAM Roles
- `roles/datastore.admin` — Database + collection + index management

### Deployment Time
~5 minutes (database creation + index building)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "database_id": "(default)",
  "location": "us-central1",
  "type": "FIRESTORE_NATIVE",
  "collections": {
    "catalog_runs": {
      "description": "Controller execution receipts",
      "indexes": [
        {
          "fields": ["status", "timestamp"],
          "order": "DESCENDING"
        }
      ]
    },
    "entitlements": {
      "description": "Marketplace entitlement state",
      "indexes": [
        {
          "fields": ["sku_id", "account_id", "expires_at"],
          "order": "DESCENDING"
        }
      ]
    },
    "signals": {
      "description": "Autonomic jidoka signals and halts",
      "indexes": [
        {
          "fields": ["signal_type", "timestamp"],
          "order": "DESCENDING"
        }
      ]
    }
  },
  "enable_backup": true
}
```

---

## Module 8: infra/pubsub/ — Pub/Sub Topics + Subscriptions

### Purpose
Message queue infrastructure for event-driven communication (webhooks, signals, entitlements).

### Use Case
- Marketplace webhook → Pub/Sub topic (subscriber: controller)
- SKU change → Pub/Sub topic (broadcast to interested systems)
- Signal distribution (jidoka halt broadcast to all components)

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "topics" {
  description = "Map of Pub/Sub topics"
  type = map(object({
    description           = string
    message_retention_days = number
    kms_key_name          = optional(string)
  }))
  default = {
    "marketplace-webhooks" = {
      description           = "Marketplace entitlement webhooks"
      message_retention_days = 7
      kms_key_name          = null
    }
    "autonomic-signals" = {
      description           = "Jidoka halts and degraded mode signals"
      message_retention_days = 30
      kms_key_name          = null
    }
  }
}

variable "subscriptions" {
  description = "Map of Pub/Sub subscriptions"
  type = map(object({
    topic                = string
    ack_deadline_seconds = number
    push_endpoint        = optional(string)
    service_account      = optional(string)
  }))
  default = {
    "marketplace-webhooks-controller-sub" = {
      topic                = "marketplace-webhooks"
      ack_deadline_seconds = 60
      push_endpoint        = null
      service_account      = null
    }
  }
}
```

### Output Values

```hcl
output "topics" {
  description = "Map of Pub/Sub topic names"
  value = {
    for name, topic in google_pubsub_topic.topics :
    name => topic.name
  }
}

output "subscriptions" {
  description = "Map of Pub/Sub subscription names"
  value = {
    for name, sub in google_pubsub_subscription.subscriptions :
    name => sub.name
  }
}
```

### Required IAM Roles
- `roles/pubsub.admin` — Topic + subscription management

### Deployment Time
~2 minutes (topic + subscription creation)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "topics": {
    "marketplace-webhooks": {
      "description": "Marketplace entitlement webhooks",
      "message_retention_days": 7,
      "kms_key_name": null
    },
    "autonomic-signals": {
      "description": "Jidoka halts and degraded mode signals",
      "message_retention_days": 30,
      "kms_key_name": null
    }
  },
  "subscriptions": {
    "marketplace-webhooks-controller-sub": {
      "topic": "marketplace-webhooks",
      "ack_deadline_seconds": 60,
      "push_endpoint": null,
      "service_account": null
    }
  }
}
```

---

## Module 9: infra/monitoring/ — Cloud Logging, Budgets, Alerts

### Purpose
Observability infrastructure: logs export, budget enforcement, alert policies.

### Use Case
- Centralize logs from all systems (controller, Cloud Build)
- Budget alerts if spending exceeds threshold
- Alert on anomalies (jidoka halt, test failures, deployment rollbacks)

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "log_sink_name" {
  description = "Cloud Logging sink name"
  type        = string
  default     = "catalog-logs-sink"
}

variable "log_destination" {
  description = "BigQuery dataset for log storage"
  type        = string
  default     = "catalog_logs"
}

variable "budget_amount" {
  description = "Monthly budget in USD"
  type        = number
  default     = 1000
}

variable "budget_alert_thresholds" {
  description = "Alert thresholds as percentage of budget"
  type        = list(number)
  default     = [50, 90, 100]
}

variable "notification_emails" {
  description = "Email addresses for budget alerts"
  type        = list(string)
  default     = []
}

variable "alert_policies" {
  description = "Custom alert policies (metric + threshold)"
  type = list(object({
    name         = string
    metric       = string
    threshold    = number
    comparison   = string  # COMPARISON_GT, COMPARISON_LT, etc.
    duration     = string  # "300s", "600s", etc.
  }))
  default = []
}
```

### Output Values

```hcl
output "log_sink_name" {
  description = "Cloud Logging sink name"
  value       = google_logging_project_sink.catalog_sink.name
}

output "log_dataset" {
  description = "BigQuery dataset for logs"
  value       = google_bigquery_dataset.logs.dataset_id
}

output "budget_id" {
  description = "Budget ID for tracking"
  value       = google_billing_budget.catalog.name
}

output "alert_policies" {
  description = "Created alert policy IDs"
  value = [
    for policy in google_monitoring_alert_policy.policies :
    policy.name
  ]
}
```

### Required IAM Roles
- `roles/logging.admin` — Log sink + export configuration
- `roles/monitoring.admin` — Alert policy creation
- `roles/bigquery.admin` — BigQuery dataset (log destination)

### Deployment Time
~3 minutes (sink + dataset + alerts)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "log_sink_name": "catalog-logs-sink",
  "log_destination": "catalog_logs",
  "budget_amount": 1000,
  "budget_alert_thresholds": [50, 90, 100],
  "notification_emails": [
    "devops@example.com",
    "alerts@example.com"
  ],
  "alert_policies": [
    {
      "name": "controller-failure-rate",
      "metric": "cloud.run/request_count",
      "threshold": 10,
      "comparison": "COMPARISON_GT",
      "duration": "300s"
    }
  ]
}
```

---

## Module 10: infra/governance/ — Policy Constraints, Governance Rules

### Purpose
Enforce organizational policies: resource restrictions, naming conventions, access controls.

### Use Case
- Prevent resources in unauthorized regions
- Enforce encryption at rest/transit
- Block public IP assignment
- Require compliance tags

### Input Variables

```hcl
variable "project_id" {
  description = "GCP project ID"
  type        = string
}

variable "allowed_regions" {
  description = "Allowed regions for resource deployment"
  type        = list(string)
  default     = ["us-central1"]
}

variable "allowed_services" {
  description = "Allowed GCP services (restrict to essential only)"
  type        = list(string)
  default     = [
    "compute.googleapis.com",
    "cloudrun.googleapis.com",
    "firestore.googleapis.com",
    "pubsub.googleapis.com"
  ]
}

variable "require_encryption" {
  description = "Enforce encryption at rest"
  type        = bool
  default     = true
}

variable "require_labels" {
  description = "Required resource labels"
  type        = list(string)
  default     = ["environment", "team", "cost-center"]
}

variable "block_public_access" {
  description = "Block public IP assignments"
  type        = bool
  default     = true
}
```

### Output Values

```hcl
output "policies_enabled" {
  description = "Enabled policy constraints"
  value = {
    require_encryption = var.require_encryption
    block_public_access = var.block_public_access
    allowed_regions = var.allowed_regions
  }
}
```

### Required IAM Roles
- `roles/orgpolicy.policyAdmin` — Organization policy management
- `roles/resourcemanager.projectIamAdmin` — Project-level policy enforcement

### Deployment Time
~1 minute (policy constraint application)

### Example tfvars.json
```json
{
  "project_id": "ggen-autonomics-prod",
  "allowed_regions": ["us-central1"],
  "allowed_services": [
    "compute.googleapis.com",
    "cloudrun.googleapis.com",
    "firestore.googleapis.com",
    "pubsub.googleapis.com"
  ],
  "require_encryption": true,
  "require_labels": ["environment", "team", "cost-center"],
  "block_public_access": true
}
```

---

## 🔄 Common Patterns

### Multi-SKU Deployment (for_each)

Create identical infrastructure for multiple SKUs using `for_each`:

```hcl
# Define SKUs once
variable "skus" {
  type = map(object({
    display_name    = string
    entitlement_ttl = number
  }))
}

# Provision N copies (one per SKU)
resource "google_firestore_document" "sku_catalogs" {
  for_each = var.skus

  collection = "catalog"
  document   = each.key
  fields = {
    display_name = {
      string_value = each.value.display_name
    }
  }
}

# Reference in outputs
output "catalogs" {
  value = {
    for sku, doc in google_firestore_document.sku_catalogs :
    sku => doc.path
  }
}
```

### Data Source Pattern (Query Existing Resources)

Reference existing resources instead of creating them:

```hcl
# Query existing VPC (don't create)
data "google_compute_network" "existing_vpc" {
  name = "my-existing-vpc"
}

# Use in Cloud Run
resource "google_cloud_run_v2_job" "controller" {
  template {
    spec {
      containers {
        env {
          name  = "VPC_NAME"
          value = data.google_compute_network.existing_vpc.name
        }
      }
    }
  }
}
```

### Locals Pattern (Computed Values)

Compute values once, use throughout:

```hcl
locals {
  image_uri = "${var.region}-docker.pkg.dev/${var.project_id}/${var.image_repo}/${var.image_name}:${var.image_tag}"

  common_labels = {
    environment = var.environment
    team        = "platform-eng"
    managed_by  = "terraform"
  }

  iam_sa_email = google_service_account.controller.email
}

# Use in multiple resources
resource "google_cloud_run_v2_job" "controller" {
  containers {
    image = local.image_uri
  }
}

resource "google_service_account_iam_binding" "controller" {
  service_account_id = local.iam_sa_email
  role               = "roles/run.invoker"
}
```

---

## 🔍 Drift Detection

Detect when infrastructure has drifed from Terraform state:

```bash
# Plan shows what would change
terraform plan -out=plan.tfplan

# If output shows changes, infrastructure has drifted
# Review changes before applying
terraform apply plan.tfplan

# Verify final state
terraform state list
terraform state show <resource>
```

### Common Drift Causes
1. Manual changes via Cloud Console
2. Service-created resources (e.g., auto-scaling)
3. API-applied configuration
4. Concurrent terraform apply (prevented by state locking)

### Prevention Strategies
1. **State locking** — Use GCS bucket locking (enabled in Module 1)
2. **Code ownership** — Only modify infrastructure via Terraform
3. **Regular drift checks** — `terraform plan` in CI/CD pipeline
4. **Taint on manual changes** — Mark resources as changed, force replacement

---

## 🔐 State Locking

Prevent concurrent Terraform applies using Firestore-based locking:

### How It Works
1. Before `terraform apply`, acquire lock in Firestore
2. If lock exists, wait or fail (configurable)
3. After `terraform apply` completes, release lock
4. If process crashes, lock expires after timeout

### Commands
```bash
# Initialize with state locking
terraform init \
  -backend-config="bucket=ggen-tf-state" \
  -backend-config="lock_table=terraform_locks"

# View active locks
gcloud firestore documents list --collection-id=terraform_locks

# Force unlock (DANGEROUS - only if process crashed)
terraform force-unlock <LOCK_ID>

# Verify no locks before manual operations
gcloud firestore documents list --collection-id=terraform_locks
```

---

## 🔑 Secrets Handling

**NEVER put secrets in .tf files or tfvars.json**

### Correct Pattern: Secret Manager

```hcl
# Define reference to Secret Manager
variable "github_token_secret" {
  description = "Secret Manager resource ID (NOT the token itself!)"
  type        = string
  # Example: "projects/my-project/secrets/github-token/versions/latest"
}

# Pass to resource
resource "google_cloud_run_v2_job" "controller" {
  template {
    spec {
      containers {
        env {
          name = "GITHUB_TOKEN"
          value_source {
            secret_key_ref {
              secret  = var.github_token_secret
              version = "latest"
            }
          }
        }
      }
    }
  }
}
```

### Initialize Secret Manually
```bash
# Create secret (one-time, before Terraform)
gcloud secrets create github-token --replication-policy="automatic"

# Add secret value (securely via stdin)
echo -n "ghp_xxxxxxxxxxxxxxxxxxxx" | gcloud secrets versions add github-token --data-file=-

# Grant service account access
gcloud secrets add-iam-policy-binding github-token \
  --member=serviceAccount:controller@my-project.iam.gserviceaccount.com \
  --role=roles/secretmanager.secretAccessor
```

### Never in tfvars.json
```json
// WRONG - Secrets in tfvars
{
  "github_token": "ghp_xxxxxxxxxxxxxxxxxxxx"  // 🚫 NEVER!
}

// CORRECT - Reference to Secret Manager
{
  "github_token_secret": "projects/my-project/secrets/github-token/versions/latest"
}
```

---

## 🌍 Workspace Strategy

Use Terraform workspaces for dev/stage/prod isolation:

```bash
# Create workspaces
terraform workspace new dev
terraform workspace new stage
terraform workspace new prod

# Switch workspace
terraform workspace select prod

# List all workspaces
terraform workspace list

# Apply for specific workspace
terraform apply -var-file=prod.tfvars
```

### tfvars per Workspace
```
.
├── dev.tfvars       # dev environment
├── stage.tfvars     # staging environment
└── prod.tfvars      # production environment
```

### State Isolation
```bash
# Each workspace has separate state
# dev uses: terraform/dev/terraform.tfstate
# prod uses: terraform/prod/terraform.tfstate

terraform state list -state-path=.terraform/workspaces/prod/terraform.tfstate
```

---

## 🗑️ Destroy Strategy

What happens when you run `terraform destroy`:

```bash
terraform destroy -var-file=prod.tfvars
```

### Destroyed Resources
```
google_cloud_run_v2_job.controller      ✓ Deleted
google_cloud_scheduler_job              ✓ Deleted
google_firestore_document               ✓ Deleted
google_pubsub_topic                     ✓ Deleted
google_pubsub_subscription              ✓ Deleted
google_service_account                  ✓ Deleted
google_storage_bucket (Terraform state) ✓ PRESERVED (protect-input-true)
```

### Preserved Resources
- **Terraform state bucket** (protected from accidental deletion)
- **Firestore backups** (retention policy)
- **Service account** (careful if used by other systems)

### Safe Destroy
```bash
# Review what will be destroyed
terraform plan -destroy -var-file=prod.tfvars

# Backup Firestore before destroy
gcloud firestore export gs://my-backup-bucket/export-$(date +%s)

# Destroy only non-critical resources
terraform destroy -target=google_pubsub_subscription.example
```

---

## ⚠️ Troubleshooting

### Error: Permission Denied

```
Error: Error waiting for Creating Firestore Database: Error code 7, message:
  Permission 'datastore.databases.create' denied
```

**Solution**: Grant required role
```bash
gcloud projects add-iam-policy-binding my-project \
  --member=user:me@example.com \
  --role=roles/datastore.admin
```

### Error: State Lock Held

```
Error: Error acquiring the state lock

Lock Info:
  ID:        abc123
  Path:      terraform/state
  Operation: OperationTypeApply
  Who:       user@example.com
  Version:   1.0.0
  Created:   2025-01-18 15:30:00 UTC
```

**Solution**: Check if another apply is running; if so, wait. Otherwise, force-unlock:
```bash
# Check who holds lock
gcloud firestore documents describe terraform_locks/abc123

# Wait for timeout (default: 5 minutes), OR force-unlock
terraform force-unlock abc123
```

### Error: Backend Not Found

```
Error: Failed to load backend: Failed to get existing workspaces:
  failed to read "terraform-workspace" directory in the bucket
```

**Solution**: Re-initialize backend with correct bucket
```bash
terraform init \
  -backend-config="bucket=ggen-tf-state-prod" \
  -backend-config="prefix=terraform/state" \
  -reconfigure
```

### Error: Variable Type Mismatch

```
Error: Incorrect attribute value type
  on main.tf line 42, in resource "google_firestore_document" "catalog":
  fields = {...}

  Argument must be a map of string, got: list of string
```

**Solution**: Check tfvars.json types match variable definitions
```hcl
# In .tf file
variable "fields" {
  type = map(string)  # Expects map, not list
}
```

```json
// In tfvars.json - CORRECT
{
  "fields": {
    "name": "value"
  }
}

// WRONG - list instead of map
{
  "fields": ["name", "value"]
}
```

---

## 📊 Deployment Time Summary

| Module | Time | Resources |
|--------|------|-----------|
| infra/state | 2 min | GCS bucket, Firestore |
| infra/shared | 3 min | VPC, subnets, Cloud NAT |
| infra/catalog | 5 min | Firestore, Pub/Sub (N×SKU) |
| infra/controller | 2 min | Cloud Run job, Cloud Scheduler |
| infra/cloudbuild | 2 min | Build trigger, Artifact Registry |
| infra/security | 3 min | Service accounts, IAM, secrets |
| infra/firestore | 5 min | Database, collections, indexes |
| infra/pubsub | 2 min | Topics, subscriptions |
| infra/monitoring | 3 min | Logging sink, budgets, alerts |
| infra/governance | 1 min | Policy constraints |
| **TOTAL** | **~28 min** | 50+ resources |

---

## 📚 Receipt Contract

Every `terraform apply` generates a receipt:

```json
{
  "execution_id": "tf-20260118-1530",
  "timestamp": "2025-01-18T15:30:00Z",
  "workspace": "prod",
  "command": "terraform apply",
  "resources_created": 15,
  "resources_modified": 2,
  "resources_destroyed": 0,
  "state_hash": "sha256:abc123...",
  "duration_seconds": 145,
  "status": "success"
}
```

Verify receipt:
```bash
# View latest state
terraform state list

# Show resource details
terraform state show google_cloud_run_v2_job.controller

# Verify state integrity
terraform validate
```

---

## ✅ Definition of Done

- [ ] All modules deployed successfully (28 min SLO)
- [ ] No drift detected: `terraform plan` shows no changes
- [ ] All resources have correct IAM roles
- [ ] Secrets stored in Secret Manager (not in tfvars.json)
- [ ] State locking enabled and tested
- [ ] Firestore backups enabled
- [ ] Cloud Logging sink created
- [ ] Budget alerts configured
- [ ] Service accounts created with minimal roles
- [ ] Terraform state backed up to GCS
- [ ] All 10 modules working together in integration test
- [ ] Documented all custom variable overrides
- [ ] Post-deployment checklist completed (see acceptance-testing.md)

---

**Related Documentation**:
- [cloudbuild-reference.md](cloudbuild-reference.md) — CI/CD pipeline
- [controller-reference.md](controller-reference.md) — Controller CLI
- [acceptance-testing.md](acceptance-testing.md) — Deployment verification
- [ato-evidence-pack.md](ato-evidence-pack.md) — Compliance evidence
- [glossary.md](glossary.md) — Terms and definitions
