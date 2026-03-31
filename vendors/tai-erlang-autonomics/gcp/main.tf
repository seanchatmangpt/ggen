# Main Terraform Configuration for TAIEA GCP Infrastructure
# Purpose: Provider setup, API enablement, and core infrastructure

terraform {
  required_version = ">= 1.0"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.1"
    }
    local = {
      source  = "hashicorp/local"
      version = "~> 2.4"
    }
  }

  # Cloud Storage backend for Terraform state
  # IMPORTANT: Create the bucket before initializing Terraform
  # Run: gsutil mb gs://taiea-terraform-state-${PROJECT_ID}
  backend "gcs" {
    bucket = "taiea-terraform-state"
    prefix = "gcp/state"
  }
}

# GCP Provider Configuration
provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
}

# ==============================
# Enable Required GCP APIs
# ==============================

# Define all required APIs in a single map for organization
variable "required_apis" {
  description = "List of GCP APIs required for TAIEA infrastructure"
  type        = set(string)
  default = [
    # Core Cloud Run
    "run.googleapis.com",
    # Firestore
    "firestore.googleapis.com",
    "appengine.googleapis.com",
    # Artifact Registry (for container images)
    "artifactregistry.googleapis.com",
    # Logging & Monitoring
    "logging.googleapis.com",
    "monitoring.googleapis.com",
    "cloudtrace.googleapis.com",
    # Additional services
    "cloudbuild.googleapis.com",
    "secretmanager.googleapis.com",
    "iam.googleapis.com",
    "serviceusage.googleapis.com",
    "bigquery.googleapis.com",
    "storage-api.googleapis.com"
  ]
}

# Enable APIs
resource "google_project_service" "required_apis" {
  for_each = var.required_apis

  project = var.project_id
  service = each.value

  disable_dependent_services = false
  disable_on_destroy         = false
}

# ==============================
# Container Registry Setup
# ==============================

# Artifact Registry for TAIEA container images
resource "google_artifact_registry_repository" "taiea_registry" {
  location      = var.region
  repository_id = "taiea-registry"
  description   = "Container registry for TAIEA application images"
  format        = "DOCKER"

  labels = {
    environment = var.environment
    service     = "taiea"
    managed-by  = "terraform"
  }

  depends_on = [
    google_project_service.required_apis["artifactregistry.googleapis.com"]
  ]
}

# ==============================
# Data Sources for Outputs
# ==============================

# Get current GCP account for reference
data "google_client_config" "current" {}

# ==============================
# Locals for Common Values
# ==============================

locals {
  # Service labels for consistent tagging
  common_labels = {
    environment = var.environment
    service     = "taiea"
    managed-by  = "terraform"
    created-at  = timestamp()
  }

  # Image URI for Cloud Run
  image_uri = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.taiea_registry.repository_id}/taiea:${var.image_tag}"
}

# ==============================
# Outputs for Terraform Use
# ==============================

output "artifact_registry_repository" {
  description = "Full name of the Artifact Registry repository"
  value       = google_artifact_registry_repository.taiea_registry.repository_full_path
}

output "artifact_registry_docker_endpoint" {
  description = "Docker endpoint for Artifact Registry"
  value       = "${var.region}-docker.pkg.dev"
}

output "gcp_account_email" {
  description = "Email of the authenticated GCP account"
  value       = data.google_client_config.current.client_email
}
