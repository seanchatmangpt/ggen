# ============================================================================
# Observability & Alerting Variables
# ============================================================================

variable "alert_email" {
  description = "Email address for alert notifications"
  type        = string
  default     = ""
  sensitive   = true
}

variable "slack_webhook_url" {
  description = "Slack webhook URL for alerts"
  type        = string
  default     = ""
  sensitive   = true
}

variable "slack_channel" {
  description = "Slack channel name for alerts"
  type        = string
  default     = "#tai-autonomics-alerts"
}

variable "pagerduty_service_key" {
  description = "PagerDuty service key for incident creation"
  type        = string
  default     = ""
  sensitive   = true
}

variable "enable_trace_sampling" {
  description = "Enable distributed tracing with sampling"
  type        = bool
  default     = true
}

variable "trace_sampling_rate" {
  description = "Fraction of requests to trace (0.0-1.0)"
  type        = number
  default     = 0.1
}

variable "log_retention_days" {
  description = "How many days to retain logs in BigQuery"
  type        = number
  default     = 90
}

variable "trace_retention_days" {
  description = "How many days to retain traces in BigQuery"
  type        = number
  default     = 30
}

variable "metrics_collection_interval_ms" {
  description = "Interval in milliseconds for metrics collection"
  type        = number
  default     = 10000
}

variable "enable_custom_metrics" {
  description = "Enable export of custom Erlang metrics to Cloud Monitoring"
  type        = bool
  default     = true
}
