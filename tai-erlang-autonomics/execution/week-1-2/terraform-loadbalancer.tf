# Cloud Load Balancer Configuration for TAI Erlang Autonomics
# Frontend (HTTPS) → Backend Services (Cloud Run)

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
  }
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP Region"
  type        = string
  default     = "us-central1"
}

variable "domain" {
  description = "Custom domain for load balancer"
  type        = string
}

variable "environment" {
  description = "Environment (dev, staging, prod)"
  type        = string
  default     = "prod"
}

provider "google" {
  project = var.project_id
  region  = var.region
}

# ============================================================================
# Cloud Armor Security Policy
# ============================================================================

resource "google_compute_security_policy" "tai_autonomics" {
  name    = "tai-autonomics-security-policy"
  project = var.project_id

  # Default action: allow
  rules {
    action   = "allow"
    priority = "65535"
    match {
      versioned_expr = "FIREWALL_RULES"
      description    = "Default rule"
    }
  }

  # Rate limiting rule: 100 requests per minute per IP
  rules {
    action      = "rate_based_ban"
    priority    = "1000"
    description = "Rate limit: 100 requests/minute per IP"
    match {
      versioned_expr = "FIREWALL_RULES"
    }
    rate_limit_options {
      conform_action = "allow"
      exceed_action  = "deny(429)"
      enforce_on_key = "IP"
      ban_duration_sec = 600  # 10 minute ban
      rate_limit_threshold {
        count        = 100
        interval_sec = 60
      }
    }
  }

  # Optional: Block known bad IPs or geographic regions
  # Add custom rules for DDoS/attack patterns as needed
}

# ============================================================================
# Cloud Run Network Endpoint Groups (NEGs)
# ============================================================================

resource "google_compute_network_endpoint_group" "tai_autonomics" {
  name                  = "tai-autonomics-neg"
  project               = var.project_id
  network_endpoint_type = "SERVERLESS"
  region                = var.region

  cloud_run {
    service = "tai-autonomics"
  }

  depends_on = [
    google_cloud_run_service.tai_autonomics
  ]
}

# ============================================================================
# Health Checks
# ============================================================================

resource "google_compute_health_check" "tai_autonomics" {
  name        = "tai-autonomics-health-check"
  description = "Health check for TAI Autonomics services"
  project     = var.project_id

  # Use HTTP health check (Cloud Run returns 200 on /health)
  http_health_check {
    port              = 8080
    request_path      = "/health"
    check_interval_sec = 30
    timeout_sec       = 5
    healthy_threshold = 2
    unhealthy_threshold = 3
  }

  # Keep alive interval
  timeout_sec         = 5
  check_interval_sec  = 30
  healthy_threshold   = 2
  unhealthy_threshold = 3
}

# ============================================================================
# Backend Service
# ============================================================================

resource "google_compute_backend_service" "tai_autonomics" {
  name                = "tai-autonomics-backend-service"
  project             = var.project_id
  health_checks       = [google_compute_health_check.tai_autonomics.id]
  protocol            = "HTTP2"
  timeout_sec         = 30
  session_affinity    = "NONE"
  load_balancing_scheme = "EXTERNAL"

  # Configure backend
  backend {
    group = google_compute_network_endpoint_group.tai_autonomics.id
  }

  # Connection draining
  connection_draining_timeout_sec = 30

  # Log configuration
  log_config {
    enable      = true
    sample_rate = 1.0
  }

  # Circuit breaker configuration
  circuit_breakers {
    max_connections             = 1000
    max_pending_requests        = 1000
    max_requests                = 1000
    max_requests_per_connection = 100
  }

  # Session affinity
  session_affinity = "NONE"

  depends_on = [
    google_compute_network_endpoint_group.tai_autonomics,
    google_compute_health_check.tai_autonomics
  ]
}

# ============================================================================
# SSL/TLS Certificate (Google-managed)
# ============================================================================

resource "google_compute_managed_ssl_certificate" "tai_autonomics" {
  name    = "tai-autonomics-cert"
  project = var.project_id

  managed {
    domains = [var.domain]
  }

  lifecycle {
    create_before_destroy = true
  }
}

# ============================================================================
# SSL Policy (modern TLS 1.2+)
# ============================================================================

resource "google_compute_ssl_policy" "tai_autonomics" {
  name            = "tai-autonomics-ssl-policy"
  project         = var.project_id
  profile         = "MODERN"
  min_tls_version = "TLS_1_2"

  # Strong cipher suites only
}

# ============================================================================
# URL Map (routing rules)
# ============================================================================

resource "google_compute_url_map" "tai_autonomics" {
  name            = "tai-autonomics-url-map"
  project         = var.project_id
  default_service = google_compute_backend_service.tai_autonomics.id

  # Routing rules for different paths
  host_rule {
    hosts        = [var.domain]
    path_matcher = "default"
  }

  path_matcher {
    name            = "default"
    default_service = google_compute_backend_service.tai_autonomics.id

    # You can add custom path routing here
    # Example: /api/pricing → pricing-backend
    #         /app → onboarding-app
  }
}

# ============================================================================
# HTTPS Forwarding Rule
# ============================================================================

resource "google_compute_target_https_proxy" "tai_autonomics" {
  name             = "tai-autonomics-https-proxy"
  project          = var.project_id
  url_map          = google_compute_url_map.tai_autonomics.id
  ssl_certificates = [google_compute_managed_ssl_certificate.tai_autonomics.id]
  ssl_policy       = google_compute_ssl_policy.tai_autonomics.id
}

resource "google_compute_global_forwarding_rule" "tai_autonomics_https" {
  name       = "tai-autonomics-https-rule"
  project    = var.project_id
  ip_version = "IPV4"
  load_balancing_scheme = "EXTERNAL"
  port_range = "443"
  target     = google_compute_target_https_proxy.tai_autonomics.id
}

# ============================================================================
# HTTP → HTTPS Redirect
# ============================================================================

resource "google_compute_url_map" "http_redirect" {
  name    = "tai-autonomics-http-redirect"
  project = var.project_id

  default_url_redirect {
    redirect_response_code = "MOVED_PERMANENTLY_DEFAULT"
    https_redirect         = true
    strip_query            = false
  }
}

resource "google_compute_target_http_proxy" "http_redirect" {
  name    = "tai-autonomics-http-proxy"
  project = var.project_id
  url_map = google_compute_url_map.http_redirect.id
}

resource "google_compute_global_forwarding_rule" "http_redirect" {
  name       = "tai-autonomics-http-rule"
  project    = var.project_id
  port_range = "80"
  target     = google_compute_target_http_proxy.http_redirect.id
  load_balancing_scheme = "EXTERNAL"
}

# ============================================================================
# Cloud Run Service (referenced from main.tf)
# ============================================================================
# Note: This is imported from main.tf - ensure it's configured correctly

data "google_cloud_run_service" "tai_autonomics" {
  name     = "tai-autonomics"
  location = var.region
  project  = var.project_id
}

# ============================================================================
# Outputs
# ============================================================================

output "load_balancer_ip" {
  description = "Public IP address of the load balancer"
  value       = google_compute_global_forwarding_rule.tai_autonomics_https.ip_address
}

output "load_balancer_url" {
  description = "Load balancer URL"
  value       = "https://${var.domain}"
}

output "backend_service_id" {
  description = "Backend service ID"
  value       = google_compute_backend_service.tai_autonomics.id
}

output "security_policy_id" {
  description = "Cloud Armor security policy ID"
  value       = google_compute_security_policy.tai_autonomics.id
}

output "ssl_certificate_id" {
  description = "SSL certificate ID"
  value       = google_compute_managed_ssl_certificate.tai_autonomics.id
}
