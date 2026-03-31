variable "project_id" {
  description = "GCP Project ID"
  type        = string
}

variable "region" {
  description = "GCP Region for deployment"
  type        = string
  default     = "us-central1"
}

variable "environment" {
  description = "Environment name (dev, staging, prod)"
  type        = string
  default     = "dev"
}

variable "image_tag" {
  description = "Container image tag"
  type        = string
  default     = "latest"
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
  description = "Receipt ledger backend (ets, gcs, bigquery)"
  type        = string
  default     = "ets"
}

variable "tracing_enabled" {
  description = "Enable OpenTelemetry tracing"
  type        = bool
  default     = true
}

variable "firestore_location" {
  description = "Firestore database location"
  type        = string
  default     = "us-central"
}

variable "enable_public_access" {
  description = "Enable public access to Cloud Run service"
  type        = bool
  default     = false
}

variable "enable_authenticated_access" {
  description = "Enable authenticated access to Cloud Run service"
  type        = bool
  default     = true
}

variable "alert_notification_channels" {
  description = "List of notification channel IDs for alerts"
  type        = list(string)
  default     = []
}

variable "gcp_zone" {
  description = "GCP Zone (optional, for GCE instances)"
  type        = string
  default     = ""
}

variable "firestore_enabled" {
  description = "Enable Firestore connectivity"
  type        = bool
  default     = true
}
