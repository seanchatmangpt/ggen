# IAM Configuration Variables
# Purpose: Control authentication and authorization behavior

variable "enable_workload_identity" {
  description = "Enable Workload Identity for GKE integration"
  type        = bool
  default     = false
}

variable "kubernetes_namespace" {
  description = "Kubernetes namespace for Workload Identity binding"
  type        = string
  default     = "default"
}

variable "create_admin_service_account_key" {
  description = "Create an admin service account key for CI/CD pipelines"
  type        = bool
  default     = false
}

variable "create_api_client_key" {
  description = "Create an API client service account key for development"
  type        = bool
  default     = false
}

variable "enable_bigquery_export" {
  description = "Enable BigQuery export for billing events"
  type        = bool
  default     = false
}

variable "tenant_isolation_enabled" {
  description = "Enable tenant isolation via Firestore partition"
  type        = bool
  default     = true
}

variable "custom_roles_prefix" {
  description = "Prefix for custom role IDs (must be lowercase)"
  type        = string
  default     = "taiea"
}
