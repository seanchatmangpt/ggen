# Cloud Monitoring Configuration
# Purpose: Dashboards, alerting policies, log sinks, and observability

# ==============================
# Log Sink to BigQuery
# ==============================

# BigQuery dataset for logs
resource "google_bigquery_dataset" "logs" {
  count      = var.enable_log_sink ? 1 : 0
  dataset_id = "taiea_logs"
  project    = var.project_id
  location   = var.bigquery_location

  description = "TAIEA service logs for analysis and auditing"

  labels = {
    environment = var.environment
    service     = "taiea"
    managed-by  = "terraform"
  }

  depends_on = [
    google_project_service.required_apis["bigquery.googleapis.com"]
  ]
}

# Log sink for Cloud Run logs to BigQuery
resource "google_logging_project_sink" "cloud_run_logs" {
  count       = var.enable_log_sink ? 1 : 0
  name        = "taiea-cloud-run-logs-sink"
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.logs[0].dataset_id}"

  # Filter logs from Cloud Run service
  filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\""

  unique_writer_identity = true

  depends_on = [
    google_bigquery_dataset.logs,
    google_cloud_run_service.taiea
  ]
}

# Grant BigQuery write permission to the sink's service account
resource "google_bigquery_dataset_iam_member" "sink_dataset_editor" {
  count      = var.enable_log_sink ? 1 : 0
  dataset_id = google_bigquery_dataset.logs[0].dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.cloud_run_logs[0].writer_identity

  depends_on = [
    google_logging_project_sink.cloud_run_logs
  ]
}

# ==============================
# Monitoring Dashboard
# ==============================

resource "google_monitoring_dashboard" "taiea_main" {
  count          = var.enable_monitoring_dashboard ? 1 : 0
  dashboard_json = jsonencode({
    displayName = "TAIEA Main Dashboard"

    gridLayout = {
      widgets = [
        # Cloud Run Request Count
        {
          title = "Cloud Run Request Count"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_count\""
                  }
                }
              }
            ]
          }
        },
        # Cloud Run Request Latencies
        {
          title = "Cloud Run Request Latencies (P50, P95, P99)"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_latencies\""
                  }
                }
              }
            ]
          }
        },
        # Cloud Run Error Count
        {
          title = "Cloud Run Error Count"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.response_code_class=\"5xx\""
                  }
                }
              }
            ]
          }
        },
        # Cloud Run Execution Times
        {
          title = "Cloud Run Execution Times"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_latencies\""
                  }
                }
              }
            ]
          }
        },
        # Firestore Document Reads
        {
          title = "Firestore Document Reads"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"api\" AND service.name=\"firestore.googleapis.com\" AND metric.type=\"serviceruntime.googleapis.com/api/consumer/quota_used_count\" AND metric.quota_metric=\"firebasedatabase_googleapis_com-document_read_ops\""
                  }
                }
              }
            ]
          }
        },
        # Firestore Document Writes
        {
          title = "Firestore Document Writes"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"api\" AND service.name=\"firestore.googleapis.com\" AND metric.type=\"serviceruntime.googleapis.com/api/consumer/quota_used_count\" AND metric.quota_metric=\"firebasedatabase_googleapis_com-document_write_ops\""
                  }
                }
              }
            ]
          }
        },
        # Firestore Network Egress
        {
          title = "Firestore Network Egress"
          xyChart = {
            dataSets = [
              {
                timeSeriesQuery = {
                  timeSeriesFilter = {
                    filter = "resource.type=\"api\" AND service.name=\"firestore.googleapis.com\" AND metric.type=\"serviceruntime.googleapis.com/api/consumer/quota_used_count\" AND metric.quota_metric=\"firebasedatabase_googleapis_com-network_egress\""
                  }
                }
              }
            ]
          }
        }
      ]
    }
  })

  depends_on = [
    google_cloud_run_service.taiea,
    google_firestore_database.receipts
  ]
}

# ==============================
# Alerting Policies
# ==============================

# Alert Policy: High Error Rate
resource "google_monitoring_alert_policy" "high_error_rate" {
  count        = var.enable_alerting ? 1 : 0
  display_name = "TAIEA High Error Rate"
  combiner     = "OR"

  conditions {
    display_name = "Error rate > 5%"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.05

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }
    }
  }

  notification_channels = var.alert_notification_channels

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Alert Policy: High Latency
resource "google_monitoring_alert_policy" "high_latency" {
  count        = var.enable_alerting ? 1 : 0
  display_name = "TAIEA High Latency"
  combiner     = "OR"

  conditions {
    display_name = "P95 latency > 2s"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_latencies\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 2000 # milliseconds

      aggregations {
        alignment_period     = "60s"
        per_series_aligner   = "ALIGN_PERCENTILE_95"
        cross_series_reducer = "REDUCE_NONE"
      }
    }
  }

  notification_channels = var.alert_notification_channels

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Alert Policy: Cloud Run Instance Crash
resource "google_monitoring_alert_policy" "instance_crash" {
  count        = var.enable_alerting ? 1 : 0
  display_name = "TAIEA Instance Crash"
  combiner     = "OR"

  conditions {
    display_name = "Instance crash detected"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.taiea.name}\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.response_code=\"500\""
      duration        = "60s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 10 # More than 10 errors in 60s

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_COUNT"
      }
    }
  }

  notification_channels = var.alert_notification_channels

  depends_on = [
    google_cloud_run_service.taiea
  ]
}

# Alert Policy: Firestore Storage Growth
resource "google_monitoring_alert_policy" "firestore_storage_growth" {
  count        = var.enable_alerting ? 1 : 0
  display_name = "TAIEA Firestore Storage Growth"
  combiner     = "OR"

  conditions {
    display_name = "Storage > 90GB"

    condition_threshold {
      filter          = "resource.type=\"firestore.googleapis.com/Database\" AND metric.type=\"firestore.googleapis.com/firestore/document_count\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 90 * 1024 * 1024 * 1024 # 90GB in bytes

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_SUM"
      }
    }
  }

  notification_channels = var.alert_notification_channels

  depends_on = [
    google_firestore_database.receipts
  ]
}

# ==============================
# Outputs
# ==============================

output "bigquery_logs_dataset" {
  description = "BigQuery dataset for logs"
  value       = var.enable_log_sink ? google_bigquery_dataset.logs[0].dataset_id : null
}

output "monitoring_dashboard_url" {
  description = "URL of the main monitoring dashboard"
  value       = var.enable_monitoring_dashboard ? "https://console.cloud.google.com/monitoring/dashboards/custom/${google_monitoring_dashboard.taiea_main[0].id}" : null
}
