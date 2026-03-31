# Terraform configuration for TAI Erlang Autonomics on GCP
# Provider: Google Cloud Platform
# Target: Cloud Run, Pub/Sub, Firestore, IAM

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
  }

  backend "gcs" {
    bucket = "tai-autonomics-terraform-state"
    prefix = "terraform/state"
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

provider "google-beta" {
  project = var.project_id
  region  = var.region
}

# Enable required APIs
resource "google_project_service" "required_apis" {
  for_each = toset([
    "run.googleapis.com",
    "pubsub.googleapis.com",
    "firestore.googleapis.com",
    "cloudbuild.googleapis.com",
    "artifactregistry.googleapis.com",
    "secretmanager.googleapis.com",
    "monitoring.googleapis.com",
    "logging.googleapis.com",
    "cloudtrace.googleapis.com",
    "iam.googleapis.com"
  ])

  project = var.project_id
  service = each.value

  disable_dependent_services = false
  disable_on_destroy         = false
}

# Service account for Cloud Run
resource "google_service_account" "tai_autonomics" {
  account_id   = "tai-autonomics-sa"
  display_name = "TAI Erlang Autonomics Service Account"
  description  = "Service account for TAI Erlang Autonomics Cloud Run service"
}

# IAM roles for service account
resource "google_project_iam_member" "tai_autonomics_pubsub" {
  project = var.project_id
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_firestore" {
  project = var.project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_logging" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_monitoring" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

resource "google_project_iam_member" "tai_autonomics_trace" {
  project = var.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.tai_autonomics.email}"
}

# Artifact Registry for container images
resource "google_artifact_registry_repository" "tai_autonomics" {
  location      = var.region
  repository_id = "tai-autonomics"
  description   = "Container registry for TAI Erlang Autonomics"
  format        = "DOCKER"
}

# Pub/Sub topic for signals
resource "google_pubsub_topic" "signals" {
  name                       = "erlang-autonomics-signals"
  message_retention_duration = "604800s" # 7 days
  labels = {
    environment = var.environment
    service     = "tai-autonomics"
  }
}

# Pub/Sub subscription
resource "google_pubsub_subscription" "signals" {
  name  = "erlang-autonomics-signals-sub"
  topic = google_pubsub_topic.signals.name

  ack_deadline_seconds = 60
  retain_acked_messages = false

  expiration_policy {
    ttl = "604800s" # 7 days
  }

  retry_policy {
    minimum_backoff = "10s"
    maximum_backoff = "600s"
  }

  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.dead_letter.id
    max_delivery_attempts = 5
  }

  labels = {
    environment = var.environment
    service     = "tai-autonomics"
  }
}

# Dead letter topic for failed messages
resource "google_pubsub_topic" "dead_letter" {
  name = "erlang-autonomics-signals-dlq"
  labels = {
    environment = var.environment
    service     = "tai-autonomics"
  }
}

# Firestore database (Native mode)
resource "google_firestore_database" "tai_autonomics" {
  project     = var.project_id
  name        = "(default)"
  location_id = var.firestore_location
  type        = "FIRESTORE_NATIVE"
  concurrency_mode = "OPTIMISTIC"
  app_engine_integration_mode = "DISABLED"
}

# Cloud Run service
resource "google_cloud_run_service" "tai_autonomics" {
  name     = "tai-autonomics"
  location = var.region

  template {
    spec {
      service_account_name = google_service_account.tai_autonomics.email
      container_concurrency = var.container_concurrency

      containers {
        image = "${var.region}-docker.pkg.dev/${var.project_id}/${google_artifact_registry_repository.tai_autonomics.repository_id}/tai-autonomics:${var.image_tag}"

        ports {
          container_port = 8080
        }

        env {
          name  = "PORT"
          value = "8080"
        }

        env {
          name  = "GCP_PROJECT_ID"
          value = var.project_id
        }

        env {
          name  = "GCP_REGION"
          value = var.region
        }

        env {
          name  = "PUBSUB_SUBSCRIPTION"
          value = google_pubsub_subscription.signals.name
        }

        env {
          name  = "RECEIPT_LEDGER_BACKEND"
          value = var.receipt_ledger_backend
        }

        env {
          name  = "METRICS_COLLECTION_INTERVAL_MS"
          value = "10000"
        }

        env {
          name  = "TRACING_ENABLED"
          value = var.tracing_enabled ? "true" : "false"
        }

        env {
          name  = "GCP_ZONE"
          value = var.gcp_zone
        }

        env {
          name  = "FIRESTORE_ENABLED"
          value = var.firestore_enabled ? "true" : "false"
        }

        resources {
          limits = {
            cpu    = var.cpu_limit
            memory = var.memory_limit
          }
        }

        startup_probe {
          http_get {
            path = "/health"
          }
          initial_delay_seconds = 5
          timeout_seconds       = 3
          period_seconds        = 5
          failure_threshold     = 3
        }

        liveness_probe {
          http_get {
            path = "/health"
          }
          initial_delay_seconds = 10
          timeout_seconds       = 3
          period_seconds        = 10
          failure_threshold     = 3
        }
      }

      timeout_seconds = var.timeout_seconds
    }

    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale" = tostring(var.min_instances)
        "autoscaling.knative.dev/maxScale" = tostring(var.max_instances)
        "run.googleapis.com/execution-environment" = var.execution_environment
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }

  depends_on = [
    google_project_service.required_apis
  ]
}

# IAM policy for Cloud Run service (public access)
resource "google_cloud_run_service_iam_member" "public_access" {
  count    = var.enable_public_access ? 1 : 0
  service  = google_cloud_run_service.tai_autonomics.name
  location = google_cloud_run_service.tai_autonomics.location
  role     = "roles/run.invoker"
  member   = "allUsers"
}

# IAM policy for Cloud Run service (authenticated access)
resource "google_cloud_run_service_iam_member" "authenticated_access" {
  count    = var.enable_authenticated_access ? 1 : 0
  service  = google_cloud_run_service.tai_autonomics.name
  location = google_cloud_run_service.tai_autonomics.location
  role     = "roles/run.invoker"
  member   = "allAuthenticatedUsers"
}

# Monitoring alert policy for health check failures
resource "google_monitoring_alert_policy" "health_check_failure" {
  display_name = "TAI Autonomics Health Check Failure"
  combiner     = "OR"

  conditions {
    display_name = "Health check endpoint failing"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\""
      duration        = "60s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.1

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.alert_notification_channels

  depends_on = [
    google_project_service.required_apis["monitoring.googleapis.com"]
  ]
}
