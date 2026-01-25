###############################################################################
# Multi-Region Deployment: Root Module
# Orchestrates all regions: us-central1 (primary), us-east1, europe-west1
###############################################################################

terraform {
  required_version = ">= 1.0"
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = ">= 5.30.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = ">= 5.30.0"
    }
  }

  backend "gcs" {
    # Configure via init: terraform init -backend-config="bucket=..."
  }
}

provider "google" {
  project = var.project_id
}

provider "google-beta" {
  project = var.project_id
}

###############################################################################
# Firestore Multi-Region Database
###############################################################################

resource "google_firestore_database" "primary" {
  provider    = google-beta
  project     = var.project_id
  name        = "default"
  location_id = "us-central1"
  type        = "FIRESTORE_NATIVE"

  # Enable multi-region writes for active-active topology
  multi_region_config {
    regions = [
      "us-central1",
      "us-east1",
      "europe-west1"
    ]
  }

  # Enable point-in-time recovery (7 days retention)
  point_in_time_recovery_enablement = "POINT_IN_TIME_RECOVERY_ENABLED"

  # Enforce encryption at rest
  cmek_config {
    kms_key_name = google_kms_crypto_key.firestore.id
  }

  delete_protection_enabled = false
  depends_on = [
    google_kms_crypto_key_iam_member.firestore_encrypt
  ]
}

###############################################################################
# Firestore Composite Indexes
###############################################################################

resource "google_firestore_index" "orders_by_customer" {
  provider   = google-beta
  project    = var.project_id
  database   = google_firestore_database.primary.name
  collection = "marketplace_orders"

  fields {
    field_path = "customerId"
    order      = "ASCENDING"
  }

  fields {
    field_path = "createdAt"
    order      = "DESCENDING"
  }

  query_scope = "COLLECTION"
}

resource "google_firestore_index" "products_by_category" {
  provider   = google-beta
  project    = var.project_id
  database   = google_firestore_database.primary.name
  collection = "marketplace_products"

  fields {
    field_path = "categoryId"
    order      = "ASCENDING"
  }

  fields {
    field_path = "updatedAt"
    order      = "DESCENDING"
  }

  query_scope = "COLLECTION"
}

###############################################################################
# Cloud KMS Keys for Encryption
###############################################################################

resource "google_kms_key_ring" "firestore" {
  for_each = toset([var.primary_region, "us-east1", "europe-west1"])

  project  = var.project_id
  name     = "ggen-firestore-keyring-${each.value}"
  location = each.value
}

resource "google_kms_crypto_key" "firestore" {
  for_each = google_kms_key_ring.firestore

  name            = "ggen-firestore-key-${each.value.location}"
  key_ring        = each.value.id
  rotation_period = "7776000s"  # 90 days
  version_template {
    algorithm = "GOOGLE_SYMMETRIC_ENCRYPTION"
  }
}

resource "google_kms_crypto_key_iam_member" "firestore_encrypt" {
  crypto_key_id = google_kms_crypto_key.firestore["us-central1"].id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.project.number}@gcp-sa-cloud-firestore.iam.gserviceaccount.com"
}

###############################################################################
# Global Load Balancer
###############################################################################

resource "google_compute_backend_service" "marketplace" {
  project       = var.project_id
  name          = "ggen-marketplace-backend"
  load_balancing_scheme = "EXTERNAL"
  protocol      = "HTTPS"
  health_checks = [google_compute_health_check.api.id]

  # CDN configuration
  enable_cdn           = var.enable_cdn
  cache_mode           = "CACHE_ALL_STATIC"
  default_ttl          = var.cdn_default_ttl
  max_ttl              = var.cdn_max_ttl
  negative_caching     = true

  negative_caching_policy {
    code = 404
    ttl  = 120
  }

  negative_caching_policy {
    code = 410
    ttl  = 120
  }

  session_affinity = "CLIENT_IP"
  affinity_cookie_ttl_sec = 3600

  backend {
    group           = google_compute_instance_group_manager.primary["us-central1"].instance_group
    balancing_mode  = "RATE"
    max_rate_per_instance = 1000
    capacity_scaler = 1.0
  }

  backend {
    group           = google_compute_instance_group_manager.primary["us-east1"].instance_group
    balancing_mode  = "RATE"
    max_rate_per_instance = 1000
    capacity_scaler = 0.8  # Lower capacity on secondary
  }

  backend {
    group           = google_compute_instance_group_manager.primary["europe-west1"].instance_group
    balancing_mode  = "RATE"
    max_rate_per_instance = 1000
    capacity_scaler = 0.6  # Lower capacity on tertiary
  }
}

###############################################################################
# Health Checks
###############################################################################

resource "google_compute_health_check" "api" {
  project = var.project_id
  name    = "ggen-api-healthcheck"

  https_health_check {
    port         = 443
    request_path = "/health"
    proxy_header = "NONE"
  }

  check_interval_sec  = 30
  timeout_sec         = 5
  healthy_threshold   = 2
  unhealthy_threshold = 3
}

###############################################################################
# Global Load Balancer Frontend
###############################################################################

resource "google_compute_url_map" "marketplace" {
  project         = var.project_id
  name            = "ggen-marketplace-url-map"
  default_service = google_compute_backend_service.marketplace.id

  host_rule {
    hosts        = ["api.ggen-marketplace.example.com"]
    path_matcher = "api-routes"
  }

  path_matcher {
    name            = "api-routes"
    default_service = google_compute_backend_service.marketplace.id

    path_rule {
      paths   = ["/health", "/healthz"]
      service = google_compute_backend_service.marketplace.id
    }

    path_rule {
      paths   = ["/v1/*"]
      service = google_compute_backend_service.marketplace.id
    }
  }
}

resource "google_compute_ssl_certificate" "marketplace" {
  project = var.project_id
  name    = "ggen-marketplace-cert"

  certificate = file(var.ssl_certificate_path)
  private_key = file(var.ssl_private_key_path)
}

resource "google_compute_target_https_proxy" "marketplace" {
  project      = var.project_id
  name         = "ggen-marketplace-https-proxy"
  url_map      = google_compute_url_map.marketplace.id
  ssl_certificates = [google_compute_ssl_certificate.marketplace.id]

  ssl_policy = google_compute_ssl_policy.modern.id
}

resource "google_compute_ssl_policy" "modern" {
  project = var.project_id
  name    = "ggen-marketplace-ssl-policy"
  profile = "MODERN"
  min_tls_version = "TLS_1_2"
}

resource "google_compute_global_forwarding_rule" "https" {
  project       = var.project_id
  name          = "ggen-marketplace-https"
  target        = google_compute_target_https_proxy.marketplace.id
  ip_protocol   = "TCP"
  port_range    = "443"
  address       = google_compute_global_address.marketplace.id
  load_balancing_scheme = "EXTERNAL"
}

resource "google_compute_global_address" "marketplace" {
  project       = var.project_id
  name          = "ggen-marketplace-ip"
  address_type  = "EXTERNAL"
}

###############################################################################
# Instance Groups & Auto Scaling
###############################################################################

resource "google_compute_instance_template" "marketplace" {
  project      = var.project_id
  name         = "ggen-marketplace-template"
  machine_type = var.machine_type

  disk {
    source_image = data.google_compute_image.cos.self_link
    auto_delete  = true
    boot         = true
    disk_size_gb = 20
    disk_type    = "pd-standard"
  }

  network_interface {
    network    = "default"
    access_config {}
  }

  service_account {
    email  = google_service_account.api.email
    scopes = ["cloud-platform"]
  }

  metadata_startup_script = base64encode(file("${path.module}/../scripts/startup.sh"))

  tags = ["ggen-api", "http-server", "https-server"]
}

resource "google_compute_instance_group_manager" "primary" {
  for_each = toset([var.primary_region, "us-east1", "europe-west1"])

  project            = var.project_id
  name               = "ggen-marketplace-ig-${each.value}"
  base_instance_name = "ggen-api"
  zone               = data.google_compute_zones.available[each.value].names[0]

  version {
    instance_template = google_compute_instance_template.marketplace.id
    name              = "primary"
  }

  target_size = 3
}

resource "google_compute_autoscaler" "marketplace" {
  for_each = google_compute_instance_group_manager.primary

  project  = var.project_id
  name     = "ggen-marketplace-autoscaler-${each.key}"
  zone     = each.value.zone
  target   = each.value.id

  autoscaling_policy {
    min_replicas    = 2
    max_replicas    = 10
    cooldown_period = 60

    cpu_utilization {
      target = 0.7
    }
  }
}

###############################################################################
# Redis Memorystore Instances
###############################################################################

resource "google_redis_instance" "cache" {
  for_each = toset([var.primary_region, "us-east1", "europe-west1"])

  project        = var.project_id
  name           = "ggen-redis-${each.value}"
  region         = each.value
  tier           = var.redis_tier
  memory_size_gb = var.redis_size

  redis_version       = "7.2"
  display_name        = "ggen Marketplace Cache - ${each.value}"
  reserved_ip_range   = google_compute_global_address.redis_reserved[each.value].id

  auth_enabled        = true
  authoritative_zone  = each.value

  replica_configuration {
    availability_zone = data.google_compute_zones.available[each.value].names[1]
  }

  maintenance_policy {
    weekly_maintenance_window {
      day = "SUNDAY"
      start_time {
        hours   = 2
        minutes = 0
      }
    }
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "google_compute_global_address" "redis_reserved" {
  for_each = toset([var.primary_region, "us-east1", "europe-west1"])

  project       = var.project_id
  name          = "ggen-redis-reserved-${each.value}"
  purpose       = "VPC_PEERING"
  address_type  = "INTERNAL"
  prefix_length = 16
  network       = "default"
}

###############################################################################
# Service Accounts & IAM
###############################################################################

resource "google_service_account" "api" {
  project      = var.project_id
  account_id   = "ggen-api-sa"
  display_name = "ggen Marketplace API Service Account"
}

resource "google_project_iam_member" "api_firestore" {
  project = var.project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.api.email}"
}

resource "google_project_iam_member" "api_redis" {
  project = var.project_id
  role    = "roles/redis.editor"
  member  = "serviceAccount:${google_service_account.api.email}"
}

resource "google_project_iam_member" "api_logs" {
  project = var.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.api.email}"
}

resource "google_project_iam_member" "api_metrics" {
  project = var.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.api.email}"
}

###############################################################################
# Monitoring & Logging
###############################################################################

resource "google_monitoring_alert_policy" "replication_lag" {
  project      = var.project_id
  display_name = "Firestore Replication Lag Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Replication lag > 10s"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/firestore/replication_lag\" AND resource.label.region!=\"us-central1\""
      duration        = "60s"
      comparison      = "COMPARISON_GT"
      threshold_value = 10000

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MAX"
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.slack.id]
  documentation {
    content   = "Firestore replication from primary region is lagging > 10 seconds. Check replication pipeline health."
    mime_type = "text/markdown"
  }
}

resource "google_monitoring_alert_policy" "health_check_failure" {
  project      = var.project_id
  display_name = "Health Check Failures Alert"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Backend unhealthy"

    condition_threshold {
      filter          = "metric.type=\"compute.googleapis.com/https/backend_request_count\" AND metric.status!=\"ok\""
      duration        = "120s"
      comparison      = "COMPARISON_GT"
      threshold_value = 100

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = [google_monitoring_notification_channel.slack.id]
}

resource "google_monitoring_notification_channel" "slack" {
  project      = var.project_id
  display_name = "Slack - #incidents"
  type         = "slack"
  labels = {
    channel_name = "#incidents"
  }
  enabled = true
}

###############################################################################
# Data Sources
###############################################################################

data "google_project" "project" {
  project_id = var.project_id
}

data "google_compute_image" "cos" {
  project = "cos-cloud"
  family  = "cos-stable"
}

data "google_compute_zones" "available" {
  for_each = toset([var.primary_region, "us-east1", "europe-west1"])

  project = var.project_id
  region  = each.value
}

###############################################################################
# Outputs
###############################################################################

output "load_balancer_ip" {
  value       = google_compute_global_address.marketplace.address
  description = "Global Load Balancer IP address"
}

output "load_balancer_dns" {
  value       = "api.ggen-marketplace.example.com"
  description = "Load Balancer DNS name"
}

output "firestore_database" {
  value       = google_firestore_database.primary.name
  description = "Firestore database name"
}

output "redis_primary_host" {
  value       = google_redis_instance.cache[var.primary_region].host
  description = "Primary Redis host"
}

output "redis_instances" {
  value = {
    for region, instance in google_redis_instance.cache :
    region => {
      host = instance.host
      port = instance.port
    }
  }
  description = "Redis instances by region"
}

output "api_service_account" {
  value       = google_service_account.api.email
  description = "API service account email"
}
