terraform {
  required_providers {
    google = { source = "hashicorp/google", version = ">= 5.30.0" }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

resource "google_project_service" "apis" {
  for_each = toset([
    "run.googleapis.com",
    "cloudscheduler.googleapis.com",
    "firestore.googleapis.com",
    "iam.googleapis.com",
    "cloudbuild.googleapis.com",
    "artifactregistry.googleapis.com",
    "logging.googleapis.com"
  ])
  project            = var.project_id
  service            = each.value
  disable_on_destroy = false
}

resource "google_service_account" "controller" {
  project      = var.project_id
  account_id   = "autonomics-catalog-controller"
  display_name = "Autonomics Catalog Controller"
}

# Controller needs power (v1). Tighten later.
resource "google_project_iam_member" "controller_roles" {
  for_each = toset([
    "roles/run.admin",
    "roles/iam.securityAdmin",
    "roles/pubsub.admin",
    "roles/cloudscheduler.admin",
    "roles/artifactregistry.admin",
    "roles/datastore.user",
    "roles/secretmanager.admin",
    "roles/storage.admin",
    "roles/cloudbuild.builds.editor",
    "roles/logging.logWriter"
  ])
  project = var.project_id
  role    = each.value
  member  = "serviceAccount:${google_service_account.controller.email}"
}

resource "google_cloud_run_v2_job" "catalog_controller" {
  project  = var.project_id
  location = var.region
  name     = "autonomics-catalog-controller"

  template {
    template {
      service_account = google_service_account.controller.email
      timeout         = "3600s"
      max_retries      = 1

      containers {
        image = var.controller_image_uri

        env { name = "GOOGLE_CLOUD_PROJECT" value = var.project_id }
        env { name = "REGION" value = var.region }
        env { name = "TF_STATE_BUCKET" value = var.tf_state_bucket }

        env { name = "CATALOG_REPO" value = var.catalog_repo_url }
        env { name = "CATALOG_BRANCH" value = var.catalog_branch }

        env { name = "TF_WORKDIR" value = var.tf_workdir }
        env { name = "CATALOG_FIRESTORE_COLLECTION" value = "catalog_runs" }

        env { name = "VERIFY_HEALTH" value = "true" }
        env { name = "TRIGGER_CLOUDBUILD" value = "true" }
      }
    }
  }

  depends_on = [google_project_service.apis]
}

resource "google_cloud_scheduler_job" "reconcile" {
  project   = var.project_id
  region    = var.region
  name      = "autonomics-catalog-reconcile"
  schedule  = "*/5 * * * *"
  time_zone = "Etc/UTC"

  http_target {
    uri         = "https://run.googleapis.com/apis/run.googleapis.com/v1/namespaces/${var.project_id}/jobs/${google_cloud_run_v2_job.catalog_controller.name}:run"
    http_method = "POST"

    oidc_token {
      service_account_email = google_service_account.controller.email
      audience              = "https://run.googleapis.com/"
    }
  }
}

output "controller_job_name" {
  value = google_cloud_run_v2_job.catalog_controller.name
}

output "scheduler_job_name" {
  value = google_cloud_scheduler_job.reconcile.name
}

output "controller_service_account" {
  value = google_service_account.controller.email
}
