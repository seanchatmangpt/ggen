variable "gcp_project_id" {
  description = "GCP Project ID"
  type        = string
  sensitive   = false
}

variable "gcp_region" {
  description = "GCP region for resources"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "prod"

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}

variable "enable_deletion_protection" {
  description = "Enable deletion protection for critical resources"
  type        = bool
  default     = true
}

variable "log_retention_days" {
  description = "Number of days to retain logs"
  type        = number
  default     = 90

  validation {
    condition     = var.log_retention_days > 0
    error_message = "Log retention must be > 0 days."
  }
}

variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP region for deployment"
  type        = string
  default     = "us-central1"
}

variable "image_tag" {
  description = "Container image tag (pinned; never use latest in production)"
  type        = string
  default     = "v1.0.0"
}

variable "container_concurrency" {
  description = "Maximum number of concurrent requests per container instance"
  type        = number
  default     = 80
}

variable "cpu_limit" {
  description = "CPU limit for Cloud Run container"
  type        = string
  default     = "2"
}

variable "memory_limit" {
  description = "Memory limit for Cloud Run container"
  type        = string
  default     = "2Gi"
}

variable "min_instances" {
  description = "Minimum number of Cloud Run instances"
  type        = number
  default     = 0
}

variable "max_instances" {
  description = "Maximum number of Cloud Run instances"
  type        = number
  default     = 10
}

variable "timeout_seconds" {
  description = "Request timeout in seconds"
  type        = number
  default     = 300
}

variable "execution_environment" {
  description = "Cloud Run execution environment (gen1 or gen2)"
  type        = string
  default     = "gen2"
}

variable "receipt_ledger_backend" {
  description = "Receipt ledger backend (ets, firestore, gcs, bigquery)"
  type        = string
  default     = "firestore"
}

variable "tracing_enabled" {
  description = "Enable OpenTelemetry tracing"
  type        = bool
  default     = true
}

variable "firestore_enabled" {
  description = "Enable Firestore database resources"
  type        = bool
  default     = true
}

variable "firestore_location" {
  description = "Firestore database location"
  type        = string
  default     = "us-central"
}

variable "enable_pitr" {
  description = "Enable Point-In-Time Recovery for Firestore"
  type        = bool
  default     = true
}

variable "enable_backups" {
  description = "Enable scheduled Firestore backups"
  type        = bool
  default     = true
}

variable "enable_gcs_backups" {
  description = "Enable GCS backup IAM permissions"
  type        = bool
  default     = true
}

variable "enable_public_access" {
  description = "Enable public unauthenticated access to Cloud Run service"
  type        = bool
  default     = false
}

variable "enable_authenticated_access" {
  description = "Enable allAuthenticatedUsers access to Cloud Run service"
  type        = bool
  default     = false
}

variable "allowed_invoker_service_accounts" {
  description = "List of service account emails allowed to invoke Cloud Run"
  type        = list(string)
  default     = []
}

variable "enable_pubsub" {
  description = "Enable Pub/Sub IAM permissions"
  type        = bool
  default     = true
}

variable "pubsub_topic_name" {
  description = "Pub/Sub topic name for events"
  type        = string
  default     = "taiea-events"
}

variable "enable_secrets" {
  description = "Enable Secret Manager IAM permissions"
  type        = bool
  default     = true
}

variable "log_level" {
  description = "Application log level"
  type        = string
  default     = "info"
}

variable "enable_log_sink" {
  description = "Enable BigQuery log sink"
  type        = bool
  default     = false
}

variable "bigquery_location" {
  description = "BigQuery dataset location for log sink"
  type        = string
  default     = "US"
}

variable "enable_monitoring_dashboard" {
  description = "Enable Cloud Monitoring dashboard"
  type        = bool
  default     = true
}

variable "enable_alerting" {
  description = "Enable Cloud Monitoring alert policies"
  type        = bool
  default     = true
}

variable "alert_notification_channels" {
  description = "List of notification channel IDs for alerts"
  type        = list(string)
  default     = []
}
