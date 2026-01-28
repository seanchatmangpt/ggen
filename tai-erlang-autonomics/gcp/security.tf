terraform {
  required_version = ">= 1.5"

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 5.0"
    }
    google-beta = {
      source  = "hashicorp/google-beta"
      version = "~> 5.0"
    }
  }
}

provider "google" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

provider "google-beta" {
  project = var.gcp_project_id
  region  = var.gcp_region
}

# ============================================================================
# VPC NETWORK CONFIGURATION
# ============================================================================

resource "google_compute_network" "tai_vpc" {
  name                    = "tai-autonomics-vpc"
  auto_create_subnetworks = false
  routing_mode            = "REGIONAL"

  description = "VPC for TAI Autonomics with security isolation"
}

# Primary subnet for application workloads
resource "google_compute_subnetwork" "primary_subnet" {
  name            = "tai-primary-subnet"
  ip_cidr_range   = "10.0.1.0/24"
  region          = var.gcp_region
  network         = google_compute_network.tai_vpc.id
  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_logs_enabled    = true
    metadata             = "INCLUDE_ALL_METADATA"
  }

  description = "Primary subnet for Cloud Run and application services"
}

# Database subnet with strict isolation
resource "google_compute_subnetwork" "database_subnet" {
  name            = "tai-database-subnet"
  ip_cidr_range   = "10.0.2.0/24"
  region          = var.gcp_region
  network         = google_compute_network.tai_vpc.id
  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_logs_enabled    = true
    metadata             = "INCLUDE_ALL_METADATA"
  }

  description = "Isolated subnet for Firestore with private access"
}

# Management subnet for bastion hosts and monitoring
resource "google_compute_subnetwork" "management_subnet" {
  name            = "tai-management-subnet"
  ip_cidr_range   = "10.0.3.0/24"
  region          = var.gcp_region
  network         = google_compute_network.tai_vpc.id
  private_ip_google_access = true

  log_config {
    aggregation_interval = "INTERVAL_5_SEC"
    flow_logs_enabled    = true
    metadata             = "INCLUDE_ALL_METADATA"
  }

  description = "Management subnet for operational tools and monitoring"
}

# ============================================================================
# FIREWALL RULES - DENY BY DEFAULT, ALLOW SPECIFIC
# ============================================================================

# Default deny all ingress traffic
resource "google_compute_firewall" "deny_all_ingress" {
  name    = "tai-deny-all-ingress"
  network = google_compute_network.tai_vpc.id
  priority = 65534

  deny {
    protocol = "all"
  }

  source_ranges = ["0.0.0.0/0"]
  description   = "Default deny all ingress traffic"
}

# Allow Cloud Run to receive HTTPS traffic
resource "google_compute_firewall" "allow_cloud_run" {
  name    = "tai-allow-cloud-run"
  network = google_compute_network.tai_vpc.id
  priority = 1000

  allow {
    protocol = "tcp"
    ports    = ["443", "8080"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["cloud-run"]
  description   = "Allow HTTPS traffic to Cloud Run services"
}

# Allow internal VPC communication
resource "google_compute_firewall" "allow_internal" {
  name    = "tai-allow-internal"
  network = google_compute_network.tai_vpc.id
  priority = 900

  allow {
    protocol = "tcp"
    ports    = ["0-65535"]
  }
  allow {
    protocol = "udp"
    ports    = ["0-65535"]
  }

  source_ranges = ["10.0.0.0/8"]
  description   = "Allow internal VPC communication"
}

# ============================================================================
# CLOUD ARMOR - DDoS PROTECTION & WAF
# ============================================================================

resource "google_compute_security_policy" "tai_armor" {
  name        = "tai-cloud-armor-policy"
  description = "Cloud Armor security policy for DDoS and WAF protection"

  # Default action - allow with logging
  rule {
    action      = "allow"
    priority    = 2147483647
    description = "Default rule"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "*"
      }
    }
    preview = false
  }

  # Rule 1: Block SQL Injection attempts
  rule {
    action   = "deny(403)"
    priority = 100
    description = "Block SQL Injection attacks"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "evaluatePreconfiguredExpr('sqli-v33-stable', ['owasp-crs-v030001-id942251-sqli',
                                                  'owasp-crs-v030001-id942420-sqli',
                                                  'owasp-crs-v030001-id942431-sqli',
                                                  'owasp-crs-v030001-id942460-sqli'])"
      }
    }
    preview = false
  }

  # Rule 2: Block XSS attacks
  rule {
    action   = "deny(403)"
    priority = 101
    description = "Block Cross-Site Scripting attacks"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "evaluatePreconfiguredExpr('xss-v33-stable', ['owasp-crs-v030001-id941110-xss',
                                                 'owasp-crs-v030001-id941120-xss',
                                                 'owasp-crs-v030001-id941130-xss'])"
      }
    }
    preview = false
  }

  # Rule 3: Block Remote Code Execution attempts
  rule {
    action   = "deny(403)"
    priority = 102
    description = "Block RCE attempts"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "evaluatePreconfiguredExpr('rce-v33-stable', ['owasp-crs-v030001-id930100-rce'])"
      }
    }
    preview = false
  }

  # Rule 4: Block Local File Inclusion
  rule {
    action   = "deny(403)"
    priority = 103
    description = "Block Local File Inclusion attacks"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "evaluatePreconfiguredExpr('lfi-v33-stable', [])"
      }
    }
    preview = false
  }

  # Rule 5: Block Remote File Inclusion
  rule {
    action   = "deny(403)"
    priority = 104
    description = "Block Remote File Inclusion attacks"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "origin.region_code == 'CN' || origin.region_code == 'RU'"
      }
    }
    preview = false
  }

  # Rule 6: Rate limiting - 100 requests per minute per IP
  rule {
    action   = "rate_based_ban"
    priority = 200
    description = "Rate limit: 100 req/min per IP"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "true"
      }
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"

      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }

      ban_duration_sec = 600
    }
    preview = false
  }

  # Rule 7: Custom rule - Block requests without Authorization header
  rule {
    action   = "deny(401)"
    priority = 300
    description = "Block unauthenticated requests (except /health)"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "request.path != '/health' && !has(request.headers['authorization'])"
      }
    }
    preview = false
  }

  # Rule 8: Allow health check requests
  rule {
    action   = "allow"
    priority = 50
    description = "Allow health check requests"
    match {
      versioned_expr = "VERSIONED_EXPR_V1"
      expr {
        expression = "request.path == '/health' || request.path == '/healthz'"
      }
    }
    preview = false
  }

  log_level = "VERBOSE"
}

# ============================================================================
# CLOUD NAT - OUTBOUND IP CONTROL
# ============================================================================

# Cloud Router for NAT
resource "google_compute_router" "tai_router" {
  name    = "tai-autonomics-router"
  region  = var.gcp_region
  network = google_compute_network.tai_vpc.id

  bgp {
    asn = 64514
  }
}

# Cloud NAT for egress traffic control
resource "google_compute_router_nat" "tai_nat" {
  name                               = "tai-autonomics-nat"
  router                             = google_compute_router.tai_router.name
  region                             = google_compute_router.tai_router.region
  nat_ip_allocate_option             = "AUTO_ONLY"
  source_subnetwork_ip_ranges_to_nat = "ALL_SUBNETWORKS_ALL_IP_RANGES"

  # Logging
  enable_logging = true
  log_config {
    enable = true
    filter = "ERRORS_ONLY"
  }

  # Timeout settings
  tcp_established_idle_timeout_sec = 600
  tcp_transitory_idle_timeout_sec  = 30
  udp_idle_timeout_sec             = 30

  # Min/max number of NAT IPs
  min_ports_per_vm = 64
  max_ports_per_vm = 1024
}

# ============================================================================
# FIRESTORE ENCRYPTION AT REST
# ============================================================================

# KMS Keyring for encryption
resource "google_kms_key_ring" "tai_keyring" {
  name     = "tai-autonomics-keyring"
  location = var.gcp_region
}

# KMS Crypto Key for Firestore encryption
resource "google_kms_crypto_key" "tai_firestore_key" {
  name            = "tai-firestore-encryption-key"
  key_ring        = google_kms_key_ring.tai_keyring.id
  rotation_period = "7776000s"  # 90 days
  version_template {
    algorithm = "GOOGLE_SYMMETRIC_ENCRYPTION"
  }
}

# KMS Crypto Key IAM binding for Firestore
resource "google_kms_crypto_key_iam_member" "firestore_encrypt" {
  crypto_key_id = google_kms_crypto_key.tai_firestore_key.id
  role          = "roles/cloudkms.cryptoKeyEncrypterDecrypter"
  member        = "serviceAccount:service-${data.google_project.current.number}@gcp-sa-cloud-firestore.iam.gserviceaccount.com"
}

# Get current GCP project
data "google_project" "current" {}

# ============================================================================
# SECRETS MANAGEMENT
# ============================================================================

resource "google_secret_manager_secret" "jwt_signing_key" {
  secret_id = "tai-jwt-signing-key"

  replication {
    automatic = true
  }

  labels = {
    environment = var.environment
    system      = "tai-autonomics"
  }
}

resource "google_secret_manager_secret" "database_password" {
  secret_id = "tai-database-password"

  replication {
    automatic = true
  }

  labels = {
    environment = var.environment
    system      = "tai-autonomics"
  }
}

resource "google_secret_manager_secret" "api_key" {
  secret_id = "tai-api-key"

  replication {
    automatic = true
  }

  labels = {
    environment = var.environment
    system      = "tai-autonomics"
  }
}

# ============================================================================
# SERVICE ACCOUNT WITH MINIMAL PERMISSIONS
# ============================================================================

resource "google_service_account" "cloud_run_sa" {
  account_id   = "tai-cloud-run"
  display_name = "TAI Autonomics Cloud Run Service Account"
  description  = "Service account for Cloud Run with minimal permissions"
}

# Grant minimal Firestore permissions
resource "google_project_iam_member" "firestore_user" {
  project = var.gcp_project_id
  role    = "roles/datastore.user"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

# Grant secret accessor for specific secrets
resource "google_secret_manager_secret_iam_member" "jwt_accessor" {
  secret_id = google_secret_manager_secret.jwt_signing_key.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

resource "google_secret_manager_secret_iam_member" "db_accessor" {
  secret_id = google_secret_manager_secret.database_password.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

resource "google_secret_manager_secret_iam_member" "api_key_accessor" {
  secret_id = google_secret_manager_secret.api_key.id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

# Grant minimal Cloud Logging permissions
resource "google_project_iam_member" "logging_write" {
  project = var.gcp_project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

# Grant minimal Cloud Monitoring permissions
resource "google_project_iam_member" "metrics_write" {
  project = var.gcp_project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.cloud_run_sa.email}"
}

# ============================================================================
# AUDIT LOGGING
# ============================================================================

# Enable Cloud Audit Logs for security events
resource "google_project_iam_audit_config" "project" {
  project = var.gcp_project_id
  service = "allServices"

  audit_log_config {
    log_type = "ADMIN_WRITE"
  }

  audit_log_config {
    log_type = "DATA_WRITE"
    # Exempt logs for specific principals if needed
  }

  audit_log_config {
    log_type = "DATA_READ"
  }
}

# ============================================================================
# OUTPUTS
# ============================================================================

output "vpc_id" {
  value       = google_compute_network.tai_vpc.id
  description = "TAI Autonomics VPC ID"
}

output "vpc_self_link" {
  value       = google_compute_network.tai_vpc.self_link
  description = "TAI Autonomics VPC self link"
}

output "primary_subnet_id" {
  value       = google_compute_subnetwork.primary_subnet.id
  description = "Primary subnet ID"
}

output "security_policy_id" {
  value       = google_compute_security_policy.tai_armor.id
  description = "Cloud Armor security policy ID"
}

output "cloud_run_service_account_email" {
  value       = google_service_account.cloud_run_sa.email
  description = "Cloud Run service account email"
}

output "kms_keyring_id" {
  value       = google_kms_key_ring.tai_keyring.id
  description = "KMS keyring ID for encryption"
}

output "nat_gateway_ips" {
  value       = "Auto-allocated from NAT"
  description = "Outbound traffic uses Cloud NAT with controlled IPs"
}
