variable "project_id" {
  type        = string
  description = "GCP Project ID"
}

variable "primary_region" {
  type        = string
  default     = "us-central1"
  description = "Primary GCP region"
}

variable "secondary_regions" {
  type        = list(string)
  default     = ["us-east1", "europe-west1"]
  description = "Secondary regions for failover"
}

###############################################################################
# Firestore Configuration
###############################################################################

variable "firestore_retention_days" {
  type        = number
  default     = 7
  description = "Point-in-time recovery retention days (max 35)"
}

variable "firestore_collections" {
  type = list(string)
  default = [
    "marketplace_products",
    "marketplace_orders",
    "marketplace_users",
    "marketplace_payments",
    "marketplace_subscriptions",
    "marketplace_analytics"
  ]
  description = "Firestore collections to replicate"
}

###############################################################################
# Compute Configuration
###############################################################################

variable "machine_type" {
  type        = string
  default     = "n2-standard-2"
  description = "GCE machine type for API servers"
}

variable "min_replicas" {
  type        = number
  default     = 2
  description = "Minimum replicas per region"
}

variable "max_replicas" {
  type        = number
  default     = 10
  description = "Maximum replicas per region"
}

variable "target_cpu_utilization" {
  type        = number
  default     = 0.7
  description = "Target CPU utilization for autoscaling (0.0-1.0)"
}

###############################################################################
# Load Balancer Configuration
###############################################################################

variable "enable_cdn" {
  type        = bool
  default     = true
  description = "Enable Cloud CDN on load balancer"
}

variable "cdn_default_ttl" {
  type        = number
  default     = 3600
  description = "CDN default time-to-live (seconds)"
}

variable "cdn_max_ttl" {
  type        = number
  default     = 86400
  description = "CDN maximum time-to-live (seconds)"
}

variable "health_check_interval_sec" {
  type        = number
  default     = 30
  description = "Health check interval (seconds)"
}

variable "health_check_timeout_sec" {
  type        = number
  default     = 5
  description = "Health check timeout (seconds)"
}

###############################################################################
# Redis Configuration
###############################################################################

variable "redis_tier" {
  type        = string
  default     = "standard"
  description = "Redis tier (standard or premium)"

  validation {
    condition     = contains(["standard", "premium"], var.redis_tier)
    error_message = "Redis tier must be 'standard' or 'premium'."
  }
}

variable "redis_size" {
  type        = number
  default     = 5
  description = "Redis instance size in GB"

  validation {
    condition     = var.redis_size >= 1 && var.redis_size <= 300
    error_message = "Redis size must be between 1 and 300 GB."
  }
}

variable "redis_version" {
  type        = string
  default     = "7.2"
  description = "Redis version"
}

###############################################################################
# Security Configuration
###############################################################################

variable "ssl_certificate_path" {
  type        = string
  description = "Path to SSL certificate file"
  sensitive   = true
}

variable "ssl_private_key_path" {
  type        = string
  description = "Path to SSL private key file"
  sensitive   = true
}

variable "kms_rotation_period_days" {
  type        = number
  default     = 90
  description = "KMS key rotation period in days"
}

###############################################################################
# Monitoring Configuration
###############################################################################

variable "alert_email" {
  type        = string
  default     = "incident-commander@ggen-marketplace.example.com"
  description = "Email for critical alerts"
}

variable "slack_webhook_url" {
  type        = string
  sensitive   = true
  description = "Slack webhook URL for alerts"
  default     = ""
}

###############################################################################
# Network Configuration
###############################################################################

variable "create_custom_network" {
  type        = bool
  default     = false
  description = "Create custom VPC (false = use default)"
}

variable "vpc_network_name" {
  type        = string
  default     = "default"
  description = "VPC network name"
}

###############################################################################
# Cost Optimization
###############################################################################

variable "enable_cud_commitment" {
  type        = bool
  default     = true
  description = "Purchase Commitment Discount Units"
}

variable "enable_reserved_capacity" {
  type        = bool
  default     = false
  description = "Reserve compute capacity (more expensive)"
}

variable "backup_retention_days" {
  type        = number
  default     = 30
  description = "Backup retention period in days"
}

###############################################################################
# Environment Tags
###############################################################################

variable "environment" {
  type        = string
  default     = "production"
  description = "Environment (dev, staging, production)"

  validation {
    condition     = contains(["dev", "staging", "production"], var.environment)
    error_message = "Environment must be 'dev', 'staging', or 'production'."
  }
}

variable "application_name" {
  type        = string
  default     = "ggen-marketplace"
  description = "Application name for tagging"
}

variable "labels" {
  type        = map(string)
  default     = {}
  description = "Additional labels for all resources"
}
