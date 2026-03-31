###############################################################################
# Cloud Alerting Policies & Notification Channels
# Comprehensive SLO monitoring, escalation, and remediation
###############################################################################

# ============================================================================
# Notification Channels (Can be configured via tfvars)
# ============================================================================

# Email notification channel (requires var.alert_email)
resource "google_monitoring_notification_channel" "email" {
  count           = var.alert_email != "" ? 1 : 0
  display_name    = "TAI Autonomics Email Alerts"
  type            = "email"
  enabled         = true
  labels = {
    email_address = var.alert_email
  }
}

# Slack notification channel (requires var.slack_webhook_url)
resource "google_monitoring_notification_channel" "slack" {
  count        = var.slack_webhook_url != "" ? 1 : 0
  display_name = "TAI Autonomics Slack"
  type         = "slack"
  enabled      = true
  labels = {
    channel_name = var.slack_channel
  }
  sensitive_labels {
    auth_token = var.slack_webhook_url
  }
}

# PagerDuty notification channel (requires var.pagerduty_key)
resource "google_monitoring_notification_channel" "pagerduty" {
  count        = var.pagerduty_service_key != "" ? 1 : 0
  display_name = "TAI Autonomics PagerDuty"
  type         = "pagerduty"
  enabled      = true
  labels = {
    service_key = var.pagerduty_service_key
  }
}

# ============================================================================
# SLO Definitions
# ============================================================================

# SLO: 99.5% Uptime (max 3.6 hours downtime per month)
resource "google_monitoring_slo" "uptime_slo" {
  service_level_indicator {
    request_based {
      good_filter   = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.response_code_class!=\"5xx\""
      total_filter  = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\""
    }
  }

  goal                = 0.995
  display_name        = "TAI Autonomics - 99.5% Uptime SLO"
  service_managed_sli = false

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# SLO: 99% Success Rate (requests that don't error)
resource "google_monitoring_slo" "success_rate_slo" {
  service_level_indicator {
    request_based {
      good_filter   = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.response_code_class=\"2xx\""
      total_filter  = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\""
    }
  }

  goal                = 0.99
  display_name        = "TAI Autonomics - 99% Success Rate SLO"
  service_managed_sli = false

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# SLO: P99 Latency < 500ms
resource "google_monitoring_slo" "latency_slo" {
  service_level_indicator {
    request_based {
      good_filter   = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.response_time < 500"
      total_filter  = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\""
    }
  }

  goal                = 0.95
  display_name        = "TAI Autonomics - P99 Latency < 500ms SLO"
  service_managed_sli = false

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Error Budget Alerts
# ============================================================================

# Alert: Using more than 50% of monthly error budget
resource "google_monitoring_alert_policy" "error_budget_50_percent" {
  display_name = "TAI Autonomics - Error Budget 50% Consumed"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error budget 50% consumed"

    condition_threshold {
      filter          = "select_slo_burn_rate(\"${google_monitoring_slo.uptime_slo.name}\", [\"24h\"])"
      duration        = "600s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.5

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = concat(
    var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : [],
    var.slack_webhook_url != "" ? [google_monitoring_notification_channel.slack[0].id] : []
  )

  documentation {
    content   = "Error budget is being consumed at 50% rate. Review SLO metrics and increase stability."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: Using 100% of error budget (burning through budget quickly)
resource "google_monitoring_alert_policy" "error_budget_100_percent" {
  display_name = "TAI Autonomics - Error Budget Fully Consumed"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error budget 100% consumed"

    condition_threshold {
      filter          = "select_slo_burn_rate(\"${google_monitoring_slo.uptime_slo.name}\", [\"1h\"])"
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 1.0

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = concat(
    var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : [],
    var.slack_webhook_url != "" ? [google_monitoring_notification_channel.slack[0].id] : [],
    var.pagerduty_service_key != "" ? [google_monitoring_notification_channel.pagerduty[0].id] : []
  )

  documentation {
    content   = <<-EOT
ERROR BUDGET EXHAUSTED: Service is failing SLO targets.
Immediate action required:
1. Check Cloud Logs for error patterns
2. Scale up instances if high load
3. Review recent deployments for regressions
4. Enable automated rollback if available
    EOT
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Incident Detection & Auto-Remediation Alerts
# ============================================================================

# Alert: Multiple consecutive failed health checks
resource "google_monitoring_alert_policy" "cascading_failures" {
  display_name = "TAI Autonomics - Cascading Failures Detected"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "3+ consecutive request failures"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.response_code_class=\"5xx\""
      duration        = "60s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 50  # % of requests

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }

      trigger {
        count   = 3
        percent = 0
      }
    }
  }

  notification_channels = concat(
    var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : [],
    var.slack_webhook_url != "" ? [google_monitoring_notification_channel.slack[0].id] : [],
    var.pagerduty_service_key != "" ? [google_monitoring_notification_channel.pagerduty[0].id] : []
  )

  documentation {
    content   = "Cascading failures detected. Service may be experiencing outage."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: OOM (Out of Memory) Conditions
resource "google_monitoring_alert_policy" "out_of_memory" {
  display_name = "TAI Autonomics - Out of Memory Condition"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Instance killed due to OOM"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND protoPayload.resourceName=~\".*\" AND protoPayload.methodName=\"google.cloud.run.v1.Executions.CancelExecution\" AND severity=\"ERROR\""
      duration        = "60s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = concat(
    var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : [],
    var.slack_webhook_url != "" ? [google_monitoring_notification_channel.slack[0].id] : [],
    var.pagerduty_service_key != "" ? [google_monitoring_notification_channel.pagerduty[0].id] : []
  )

  documentation {
    content   = "Instance killed due to Out of Memory. Increase memory_limit or investigate memory leaks."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: Slow Startups
resource "google_monitoring_alert_policy" "slow_startup" {
  display_name = "TAI Autonomics - Slow Service Startup"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Startup time > 30 seconds"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/request_count\""
      duration        = "300s"
      comparison      = "COMPARISON_LESS_THAN"
      threshold_value = 1

      aggregations {
        alignment_period   = "300s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : []

  documentation {
    content   = "Service is taking > 30 seconds to start. Check container initialization."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Governor-Specific Alerts
# ============================================================================

# Alert: Governor stuck in error state
resource "google_monitoring_alert_policy" "governor_error_state" {
  display_name = "TAI Autonomics - Governor In Error State"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Governor in error state > 5 minutes"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/tai_autonomics/governor_state_transitions\" AND metric.to_state=\"error\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }

      trigger {
        count   = 5
        percent = 0
      }
    }
  }

  notification_channels = var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : []

  documentation {
    content   = "Governor entered error state. Review logs for error details."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: High refusal rate
resource "google_monitoring_alert_policy" "high_refusal_rate" {
  display_name = "TAI Autonomics - High Refusal Rate"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Refusal rate > 5%"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/tai_autonomics/receipts_total\" AND metric.receipt_type=\"refusal\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.05

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }

      trigger {
        count   = 2
        percent = 0
      }
    }
  }

  notification_channels = var.alert_email != "" ? [google_monitoring_notification_channel.email[0].id] : []

  documentation {
    content   = "Refusal rate exceeds 5%. Check governor policies and system capacity."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Outputs
# ============================================================================

output "uptime_slo_id" {
  description = "ID of the uptime SLO"
  value       = google_monitoring_slo.uptime_slo.name
}

output "success_rate_slo_id" {
  description = "ID of the success rate SLO"
  value       = google_monitoring_slo.success_rate_slo.name
}

output "latency_slo_id" {
  description = "ID of the latency SLO"
  value       = google_monitoring_slo.latency_slo.name
}

output "alert_policies" {
  description = "Map of alert policies"
  value = {
    high_error_rate        = try(google_monitoring_alert_policy.high_error_rate.display_name, "")
    high_latency           = try(google_monitoring_alert_policy.high_latency.display_name, "")
    high_cpu               = try(google_monitoring_alert_policy.high_cpu.display_name, "")
    high_memory            = try(google_monitoring_alert_policy.high_memory.display_name, "")
    service_errors         = try(google_monitoring_alert_policy.service_errors.display_name, "")
    error_budget_50        = try(google_monitoring_alert_policy.error_budget_50_percent.display_name, "")
    error_budget_100       = try(google_monitoring_alert_policy.error_budget_100_percent.display_name, "")
    cascading_failures     = try(google_monitoring_alert_policy.cascading_failures.display_name, "")
    out_of_memory          = try(google_monitoring_alert_policy.out_of_memory.display_name, "")
    governor_error         = try(google_monitoring_alert_policy.governor_error_state.display_name, "")
    high_refusal_rate      = try(google_monitoring_alert_policy.high_refusal_rate.display_name, "")
  }
}
