###############################################################################
# Cloud Monitoring & Observability Configuration
# Dashboards, log sinks, traces, and custom metrics for TAI Erlang Autonomics
###############################################################################

# ============================================================================
# Custom Metrics for Cloud Monitoring
# ============================================================================

# Custom metric descriptor for request rate
resource "google_monitoring_metric_descriptor" "request_rate" {
  display_name = "tai-autonomics/request_rate"
  metric_kind  = "GAUGE"
  value_type   = "DOUBLE"
  type         = "custom.googleapis.com/tai_autonomics/request_rate_per_second"

  labels {
    key         = "endpoint"
    value_type  = "STRING"
    description = "HTTP endpoint path"
  }

  labels {
    key         = "method"
    value_type  = "STRING"
    description = "HTTP method (GET, POST, etc.)"
  }

  labels {
    key         = "status_code"
    value_type  = "INT64"
    description = "HTTP response status code"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Custom metric descriptor for error rate
resource "google_monitoring_metric_descriptor" "error_rate" {
  display_name = "tai-autonomics/error_rate"
  metric_kind  = "GAUGE"
  value_type   = "DOUBLE"
  type         = "custom.googleapis.com/tai_autonomics/error_rate_percent"

  labels {
    key         = "error_type"
    value_type  = "STRING"
    description = "Type of error (request_failed, timeout, etc.)"
  }

  labels {
    key         = "governor"
    value_type  = "STRING"
    description = "Governor module name"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Custom metric descriptor for latency percentiles
resource "google_monitoring_metric_descriptor" "latency_p99" {
  display_name = "tai-autonomics/latency_p99"
  metric_kind  = "GAUGE"
  value_type   = "DISTRIBUTION"
  type         = "custom.googleapis.com/tai_autonomics/latency_ms_p99"

  labels {
    key         = "endpoint"
    value_type  = "STRING"
    description = "HTTP endpoint"
  }

  labels {
    key         = "operation"
    value_type  = "STRING"
    description = "Operation name"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Custom metric for memory usage
resource "google_monitoring_metric_descriptor" "memory_usage" {
  display_name = "tai-autonomics/memory_usage"
  metric_kind  = "GAUGE"
  value_type   = "INT64"
  type         = "custom.googleapis.com/tai_autonomics/memory_bytes"

  labels {
    key         = "node"
    value_type  = "STRING"
    description = "Erlang node name"
  }

  labels {
    key         = "type"
    value_type  = "STRING"
    description = "Memory type (heap, total, etc.)"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Custom metric for governor state transitions
resource "google_monitoring_metric_descriptor" "governor_transitions" {
  display_name = "tai-autonomics/governor_transitions"
  metric_kind  = "CUMULATIVE"
  value_type   = "INT64"
  type         = "custom.googleapis.com/tai_autonomics/governor_state_transitions"

  labels {
    key         = "governor"
    value_type  = "STRING"
    description = "Governor module name"
  }

  labels {
    key         = "from_state"
    value_type  = "STRING"
    description = "Previous state"
  }

  labels {
    key         = "to_state"
    value_type  = "STRING"
    description = "New state"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Custom metric for receipt generation
resource "google_monitoring_metric_descriptor" "receipts_generated" {
  display_name = "tai-autonomics/receipts_generated"
  metric_kind  = "CUMULATIVE"
  value_type   = "INT64"
  type         = "custom.googleapis.com/tai_autonomics/receipts_total"

  labels {
    key         = "receipt_type"
    value_type  = "STRING"
    description = "Receipt type (acceptance, refusal)"
  }

  labels {
    key         = "reason"
    value_type  = "STRING"
    description = "Refusal reason if applicable"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Monitoring Dashboard
# ============================================================================

resource "google_monitoring_dashboard" "tai_autonomics" {
  dashboard_json = jsonencode({
    displayName = "TAI Erlang Autonomics - Operations Dashboard"
    mosaicLayout = {
      columns = 12
      tiles = [
        # Title tile
        {
          width  = 12
          height = 1
          widget = {
            title = "TAI Erlang Autonomics - Real-time Operations"
            text = {
              content = "**Cloud Run Service Health & Performance Metrics**"
            }
          }
        },

        # Row 1: Key Health Metrics
        {
          xPos   = 0
          yPos   = 1
          width  = 3
          height = 3
          widget = {
            title = "Error Rate (%)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/error_rate_percent\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_MEAN"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              timeshiftDuration = "0s"
              yAxis = {
                label = "Error Rate %"
                scale = "LINEAR"
              }
            }
          }
        },

        {
          xPos   = 3
          yPos   = 1
          width  = 3
          height = 3
          widget = {
            title = "P99 Latency (ms)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/latency_ms_p99\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_PERCENTILE_99"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "Latency (ms)"
                scale = "LINEAR"
              }
            }
          }
        },

        {
          xPos   = 6
          yPos   = 1
          width  = 3
          height = 3
          widget = {
            title = "Request Rate (req/s)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/request_rate_per_second\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "Requests/sec"
                scale = "LINEAR"
              }
            }
          }
        },

        {
          xPos   = 9
          yPos   = 1
          width  = 3
          height = 3
          widget = {
            title = "Memory Usage (GB)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/memory_bytes\" resource.type=\"cloud_run_revision\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_MEAN"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
              yAxis = {
                label = "Memory (GB)"
                scale = "LINEAR"
              }
            }
          }
        },

        # Row 2: Governor State & Actions
        {
          xPos   = 0
          yPos   = 4
          width  = 6
          height = 3
          widget = {
            title = "Governor State Transitions"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/governor_state_transitions\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                        groupByFields = [
                          "metric.governor",
                          "metric.to_state"
                        ]
                      }
                    }
                  }
                  plotType = "STACKED_AREA"
                }
              ]
            }
          }
        },

        {
          xPos   = 6
          yPos   = 4
          width  = 6
          height = 3
          widget = {
            title = "Receipts Generated (Total)"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "metric.type=\"custom.googleapis.com/tai_autonomics/receipts_total\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                        crossSeriesReducer = "REDUCE_SUM"
                        groupByFields = [
                          "metric.receipt_type"
                        ]
                      }
                    }
                  }
                  plotType = "STACKED_AREA"
                }
              ]
            }
          }
        },

        # Row 3: Cloud Run Metrics
        {
          xPos   = 0
          yPos   = 7
          width  = 4
          height = 3
          widget = {
            title = "Cloud Run Request Count"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/request_count\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_RATE"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
            }
          }
        },

        {
          xPos   = 4
          yPos   = 7
          width  = 4
          height = 3
          widget = {
            title = "Cloud Run Request Latencies"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/request_latencies\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_PERCENTILE_95"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
            }
          }
        },

        {
          xPos   = 8
          yPos   = 7
          width  = 4
          height = 3
          widget = {
            title = "Cloud Run Instances"
            xyChart = {
              dataSets = [
                {
                  timeSeriesQuery = {
                    timeSeriesFilter = {
                      filter = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/instances\""
                      aggregation = {
                        alignmentPeriod  = "60s"
                        perSeriesAligner = "ALIGN_MEAN"
                      }
                    }
                  }
                  plotType = "LINE"
                }
              ]
            }
          }
        }
      ]
    }
  })
}

# ============================================================================
# Log Sinks to BigQuery
# ============================================================================

# BigQuery dataset for logs
resource "google_bigquery_dataset" "tai_logs" {
  dataset_id           = "tai_autonomics_logs"
  friendly_name        = "TAI Autonomics Logs"
  description          = "Cloud Logging sink for TAI Erlang Autonomics"
  location             = var.region
  default_table_expiration_ms = 7776000000  # 90 days

  labels = {
    environment = var.environment
    service     = "tai-autonomics"
  }

  depends_on = [google_project_service.required_apis["bigquery.googleapis.com"]]
}

# BigQuery table for logs
resource "google_bigquery_table" "tai_logs_table" {
  dataset_id = google_bigquery_dataset.tai_logs.dataset_id
  table_id   = "application_logs"

  schema = jsonencode([
    {
      name        = "timestamp"
      type        = "TIMESTAMP"
      mode        = "NULLABLE"
      description = "Log entry timestamp"
    },
    {
      name        = "severity"
      type        = "STRING"
      mode        = "NULLABLE"
      description = "Log severity (DEBUG, INFO, WARNING, ERROR, CRITICAL)"
    },
    {
      name        = "message"
      type        = "STRING"
      mode        = "NULLABLE"
      description = "Log message"
    },
    {
      name        = "labels"
      type        = "RECORD"
      mode        = "REPEATED"
      description = "Log labels"
      fields = [
        {
          name = "key"
          type = "STRING"
          mode = "NULLABLE"
        },
        {
          name = "value"
          type = "STRING"
          mode = "NULLABLE"
        }
      ]
    },
    {
      name        = "json_payload"
      type        = "JSON"
      mode        = "NULLABLE"
      description = "JSON structured log data"
    },
    {
      name        = "resource"
      type        = "RECORD"
      mode        = "NULLABLE"
      description = "Resource that generated the log"
      fields = [
        {
          name = "type"
          type = "STRING"
          mode = "NULLABLE"
        },
        {
          name = "labels"
          type = "RECORD"
          mode = "REPEATED"
          fields = [
            {
              name = "key"
              type = "STRING"
              mode = "NULLABLE"
            },
            {
              name = "value"
              type = "STRING"
              mode = "NULLABLE"
            }
          ]
        }
      ]
    },
    {
      name        = "trace_id"
      type        = "STRING"
      mode        = "NULLABLE"
      description = "Distributed trace ID"
    },
    {
      name        = "span_id"
      type        = "STRING"
      mode        = "NULLABLE"
      description = "OpenTelemetry span ID"
    }
  ])

  time_partitioning {
    type          = "DAY"
    field         = "timestamp"
    expiration_ms = 7776000000  # 90 days
  }

  clustering = ["severity", "resource.type"]
}

# Log sink to BigQuery
resource "google_logging_project_sink" "tai_logs_sink" {
  name        = "tai-autonomics-bigquery-sink"
  destination = "bigquery.googleapis.com/projects/${var.project_id}/datasets/${google_bigquery_dataset.tai_logs.dataset_id}"

  filter = <<-EOT
resource.type="cloud_run_revision"
AND resource.labels.service_name="${google_cloud_run_service.tai_autonomics.name}"
AND (
  severity="ERROR"
  OR severity="WARNING"
  OR severity="CRITICAL"
  OR jsonPayload.event_type="governor_transition"
  OR jsonPayload.event_type="receipt_generated"
  OR jsonPayload.event_type="refusal"
)
  EOT

  unique_writer_identity = true

  depends_on = [google_project_service.required_apis["logging.googleapis.com"]]
}

# IAM binding for log sink to write to BigQuery
resource "google_bigquery_dataset_iam_member" "log_sink_writer" {
  dataset_id = google_bigquery_dataset.tai_logs.dataset_id
  role       = "roles/bigquery.dataEditor"
  member     = google_logging_project_sink.tai_logs_sink.writer_identity
}

# ============================================================================
# Cloud Trace Configuration
# ============================================================================

# BigQuery dataset for trace data
resource "google_bigquery_dataset" "tai_traces" {
  dataset_id    = "tai_autonomics_traces"
  friendly_name = "TAI Autonomics Traces"
  description   = "Cloud Trace data exported to BigQuery"
  location      = var.region

  labels = {
    environment = var.environment
    service     = "tai-autonomics"
  }

  depends_on = [google_project_service.required_apis["bigquery.googleapis.com"]]
}

# Table for span data
resource "google_bigquery_table" "trace_spans" {
  dataset_id = google_bigquery_dataset.tai_traces.dataset_id
  table_id   = "spans"

  schema = jsonencode([
    {
      name = "trace_id"
      type = "STRING"
      mode = "NULLABLE"
    },
    {
      name = "span_id"
      type = "STRING"
      mode = "NULLABLE"
    },
    {
      name = "parent_span_id"
      type = "STRING"
      mode = "NULLABLE"
    },
    {
      name = "operation_name"
      type = "STRING"
      mode = "NULLABLE"
    },
    {
      name = "start_time"
      type = "TIMESTAMP"
      mode = "NULLABLE"
    },
    {
      name = "end_time"
      type = "TIMESTAMP"
      mode = "NULLABLE"
    },
    {
      name = "duration_ms"
      type = "FLOAT64"
      mode = "NULLABLE"
    },
    {
      name = "status"
      type = "STRING"
      mode = "NULLABLE"
    },
    {
      name = "attributes"
      type = "JSON"
      mode = "NULLABLE"
    }
  ])

  time_partitioning {
    type          = "DAY"
    field         = "start_time"
    expiration_ms = 2592000000  # 30 days
  }

  clustering = ["operation_name", "status"]
}

# ============================================================================
# Alert Policies
# ============================================================================

# Alert: Error Rate > 1%
resource "google_monitoring_alert_policy" "high_error_rate" {
  display_name = "TAI Autonomics - Error Rate > 1%"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Error rate exceeds 1%"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/tai_autonomics/error_rate_percent\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 1.0

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "Error rate has exceeded 1%. Check logs in Cloud Logging and trace errors in Cloud Trace."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: P99 Latency > 500ms
resource "google_monitoring_alert_policy" "high_latency" {
  display_name = "TAI Autonomics - P99 Latency > 500ms"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "P99 latency exceeds 500ms"

    condition_threshold {
      filter          = "metric.type=\"custom.googleapis.com/tai_autonomics/latency_ms_p99\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 500.0

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_PERCENTILE_99"
      }

      trigger {
        count   = 2
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "P99 latency has exceeded 500ms. Review performance metrics and scale up if needed."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: CPU > 70%
resource "google_monitoring_alert_policy" "high_cpu" {
  display_name = "TAI Autonomics - CPU Utilization > 70%"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "CPU exceeds 70%"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/cpu_utilizations\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.7

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count   = 2
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "CPU utilization has exceeded 70%. Consider scaling up max_instances or optimizing code."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: Memory > 80%
resource "google_monitoring_alert_policy" "high_memory" {
  display_name = "TAI Autonomics - Memory Utilization > 80%"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Memory exceeds 80%"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/memory_utilizations\""
      duration        = "300s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 0.8

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_MEAN"
      }

      trigger {
        count   = 2
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "Memory utilization has exceeded 80%. Check for memory leaks or increase memory_limit."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: Service Unavailable (502/503)
resource "google_monitoring_alert_policy" "service_errors" {
  display_name = "TAI Autonomics - Service Unavailable Errors"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "5xx errors detected"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.response_code_class=\"5xx\""
      duration        = "60s"
      comparison      = "COMPARISON_GREATER_THAN"
      threshold_value = 5

      aggregations {
        alignment_period   = "60s"
        per_series_aligner = "ALIGN_RATE"
      }

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "Service is returning 5xx errors. Check Cloud Logs and restart service if needed."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# Alert: No requests received for 5 minutes
resource "google_monitoring_alert_policy" "no_traffic" {
  display_name = "TAI Autonomics - No Traffic Received"
  combiner     = "OR"
  enabled      = true

  conditions {
    display_name = "Zero traffic for 5 minutes"

    condition_threshold {
      filter          = "resource.type=\"cloud_run_revision\" AND resource.labels.service_name=\"${google_cloud_run_service.tai_autonomics.name}\" AND metric.type=\"run.googleapis.com/request_count\""
      duration        = "300s"
      comparison      = "COMPARISON_LESS_THAN_OR_EQUAL"
      threshold_value = 0

      aggregations {
        alignment_period  = "300s"
        per_series_aligner = "ALIGN_RATE"
      }

      trigger {
        count   = 1
        percent = 0
      }
    }
  }

  notification_channels = var.alert_notification_channels

  documentation {
    content   = "No traffic received for 5 minutes. Check if service is accessible or if upstream traffic is down."
    mime_type = "text/markdown"
  }

  depends_on = [google_project_service.required_apis["monitoring.googleapis.com"]]
}

# ============================================================================
# Outputs
# ============================================================================

output "monitoring_dashboard_url" {
  description = "URL to the Cloud Monitoring dashboard"
  value       = "https://console.cloud.google.com/monitoring/dashboards/custom/${google_monitoring_dashboard.tai_autonomics.id}?project=${var.project_id}"
}

output "logs_bigquery_dataset" {
  description = "BigQuery dataset for logs"
  value       = google_bigquery_dataset.tai_logs.dataset_id
}

output "logs_bigquery_table" {
  description = "BigQuery table for logs"
  value       = google_bigquery_table.tai_logs_table.table_id
}

output "traces_bigquery_dataset" {
  description = "BigQuery dataset for traces"
  value       = google_bigquery_dataset.tai_traces.dataset_id
}

output "traces_bigquery_table" {
  description = "BigQuery table for traces"
  value       = google_bigquery_table.trace_spans.table_id
}
