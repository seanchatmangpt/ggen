# Cloud Run Deployment Configuration
# Service: TAI Autonomics Receipt Engine
# Purpose: Serverless containerized Erlang application for receipt processing and ledger management

resource "google_cloud_run_service" "taiea" {
  name     = "taiea"
  location = var.region

  template {
    spec {
      service_account_name = google_service_account.taiea.email
      container_concurrency = var.container_concurrency
      timeout_seconds       = var.timeout_seconds

      containers {
        # Image reference: GCR or Artifact Registry
        image = "${var.region}-docker.pkg.dev/${var.project_id}/taiea-registry/taiea:${var.image_tag}"

        ports {
          container_port = 8080
          name           = "http1"
        }

        # Environment Variables
        env {
          name  = "PORT"
          value = "8080"
        }

        env {
          name  = "TAIEA_ENV"
          value = var.environment
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
          name  = "FIRESTORE_DATABASE"
          value = google_firestore_database.receipts.name
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
          name  = "PUBSUB_TOPIC"
          value = var.pubsub_topic_name
        }

        env {
          name  = "LOG_LEVEL"
          value = var.log_level
        }

        # Resource allocation
        resources {
          limits = {
            cpu    = var.cpu_limit
            memory = var.memory_limit
          }
        }

        # Startup probe: ensure service is ready
        startup_probe {
          http_get {
            path = "/health/startup"
          }
          initial_delay_seconds = 5
          timeout_seconds       = 3
          period_seconds        = 5
          failure_threshold     = 3
        }

        # Liveness probe: ensure service remains healthy
        liveness_probe {
          http_get {
            path = "/health/live"
          }
          initial_delay_seconds = 10
          timeout_seconds       = 3
          period_seconds        = 10
          failure_threshold     = 3
        }

        # Readiness probe: ensure service can handle traffic
        readiness_probe {
          http_get {
            path = "/health/ready"
          }
          initial_delay_seconds = 5
          timeout_seconds       = 3
          period_seconds        = 5
          failure_threshold     = 2
        }
      }
    }

    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale"           = tostring(var.min_instances)
        "autoscaling.knative.dev/maxScale"           = tostring(var.max_instances)
        "run.googleapis.com/execution-environment"   = var.execution_environment
        "run.googleapis.com/cpu-throttling-disabled" = "false"
        "run.googleapis.com/client-name"             = "cloud-console"
      }
      labels = {
        environment = var.environment
        service     = "taiea"
        managed-by  = "terraform"
      }
    }
  }

  # Traffic configuration
  traffic {
    percent         = 100
    latest_revision = true
  }

  # Labels for organization
  labels = {
    environment = var.environment
    service     = "taiea"
    managed-by  = "terraform"
  }

  depends_on = [
    google_project_service.required_apis["run.googleapis.com"],
    google_service_account.taiea,
    google_firestore_database.receipts
  ]
}

# Cloud Run Service Update (for more granular control)
resource "google_cloud_run_service_update" "taiea_update" {
  service  = google_cloud_run_service.taiea.name
  location = google_cloud_run_service.taiea.location

  autogenerate_revision_name = true
  metadata {
    annotations = {
      "run.googleapis.com/client-name" = "terraform-provider"
    }
  }
}

# Output the Cloud Run service URL
output "cloud_run_url" {
  description = "URL of the Cloud Run service"
  value       = google_cloud_run_service.taiea.status[0].url
}

output "cloud_run_service_name" {
  description = "Name of the Cloud Run service"
  value       = google_cloud_run_service.taiea.name
}

output "cloud_run_revision" {
  description = "Latest revision of Cloud Run service"
  value       = google_cloud_run_service.taiea.status[0].latest_revision
}
