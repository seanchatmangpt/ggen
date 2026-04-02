# GCP Secret Manager Configuration
# Stores sensitive credentials and configuration values
# These secrets are used by CI/CD pipelines and cloud services

terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
  }
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# Enable Secret Manager API
resource "google_project_service" "secretmanager" {
  project = var.project_id
  service = "secretmanager.googleapis.com"

  disable_dependent_services = false
  disable_on_destroy         = false
}

# GitHub Personal Access Token (for CI/CD workflows)
resource "google_secret_manager_secret" "github_token" {
  secret_id = "github-token"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "ci-cd"
    category    = "vcs"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# GCP Service Account Key (JSON format, base64 encoded)
resource "google_secret_manager_secret" "gcp_service_account_key" {
  secret_id = "gcp-service-account-key"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "ci-cd"
    category    = "iam"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# Firestore Database URL
resource "google_secret_manager_secret" "firestore_url" {
  secret_id = "firestore-url"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "database"
    category    = "database"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# Docker Registry Token
resource "google_secret_manager_secret" "docker_registry_token" {
  secret_id = "docker-registry-token"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "registry"
    category    = "authentication"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# Slack Webhook URL (for notifications)
resource "google_secret_manager_secret" "slack_webhook_url" {
  secret_id = "slack-webhook-url"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "notifications"
    category    = "integration"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# Erlang Cookie (for Erlang node distribution)
resource "google_secret_manager_secret" "erlang_cookie" {
  secret_id = "erlang-cookie"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "tai-autonomics"
    category    = "application"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# Database Connection String (Firestore)
resource "google_secret_manager_secret" "database_connection_string" {
  secret_id = "database-connection-string"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "database"
    category    = "database"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# API Key for internal services
resource "google_secret_manager_secret" "internal_api_key" {
  secret_id = "internal-api-key"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "api"
    category    = "authentication"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# TLS Certificate (for HTTPS)
resource "google_secret_manager_secret" "tls_certificate" {
  secret_id = "tls-certificate"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "tls"
    category    = "security"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# TLS Private Key (for HTTPS)
resource "google_secret_manager_secret" "tls_private_key" {
  secret_id = "tls-private-key"
  project   = var.project_id

  labels = {
    environment = var.environment
    service     = "tls"
    category    = "security"
  }

  replication {
    automatic = true
  }

  depends_on = [google_project_service.secretmanager]
}

# IAM binding to allow Cloud Run service to access secrets
resource "google_secret_manager_secret_iam_member" "tai_autonomics_secret_accessor" {
  for_each = toset([
    "github-token",
    "gcp-service-account-key",
    "firestore-url",
    "docker-registry-token",
    "slack-webhook-url",
    "erlang-cookie",
    "database-connection-string",
    "internal-api-key",
    "tls-certificate",
    "tls-private-key"
  ])

  secret_id = each.value
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:tai-autonomics-sa@${var.project_id}.iam.gserviceaccount.com"
  project   = var.project_id

  depends_on = [
    google_secret_manager_secret.github_token,
    google_secret_manager_secret.gcp_service_account_key,
    google_secret_manager_secret.firestore_url,
    google_secret_manager_secret.docker_registry_token,
    google_secret_manager_secret.slack_webhook_url,
    google_secret_manager_secret.erlang_cookie,
    google_secret_manager_secret.database_connection_string,
    google_secret_manager_secret.internal_api_key,
    google_secret_manager_secret.tls_certificate,
    google_secret_manager_secret.tls_private_key
  ]
}
