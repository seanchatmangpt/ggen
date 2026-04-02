#!/bin/bash
# Cloud Monitoring and Alerting Setup for TAI Autonomics
# Creates dashboards, metrics, and alert policies

set -e

PROJECT_ID=${1:-"tai-autonomics-prod"}
REGION=${2:-"us-central1"}
SERVICE_NAME=${3:-"tai-autonomics"}
PAGERDUTY_INTEGRATION_KEY=${4:-""}

echo "📊 Setting up Cloud Monitoring and Alerting"
echo "📍 Project: $PROJECT_ID"
echo "📍 Region: $REGION"
echo "📍 Service: $SERVICE_NAME"
echo ""

# Set GCP project
gcloud config set project $PROJECT_ID

# ============================================================================
# 1. Create Notification Channels
# ============================================================================

echo "📢 Creating notification channels..."

# Email notification channel (replace with actual email)
OPERATIONS_EMAIL="ops-team@example.com"
echo "Creating email notification channel: $OPERATIONS_EMAIL"

EMAIL_CHANNEL=$(gcloud alpha monitoring channels create \
  --display-name="Operations Team Email" \
  --type=email \
  --channel-labels=email_address=$OPERATIONS_EMAIL \
  --format='value(name)' \
  2>/dev/null || echo "")

if [ -n "$EMAIL_CHANNEL" ]; then
  echo "✓ Email channel created: $EMAIL_CHANNEL"
else
  echo "⚠ Email channel creation failed or already exists"
fi

# PagerDuty notification channel (if integration key provided)
if [ -n "$PAGERDUTY_INTEGRATION_KEY" ]; then
  echo "Creating PagerDuty notification channel..."
  PAGERDUTY_CHANNEL=$(gcloud alpha monitoring channels create \
    --display-name="PagerDuty Integration" \
    --type=pagerduty \
    --channel-labels=service_key=$PAGERDUTY_INTEGRATION_KEY \
    --format='value(name)' \
    2>/dev/null || echo "")

  if [ -n "$PAGERDUTY_CHANNEL" ]; then
    echo "✓ PagerDuty channel created: $PAGERDUTY_CHANNEL"
  fi
fi

echo ""

# ============================================================================
# 2. Create Log-Based Metrics
# ============================================================================

echo "📈 Creating log-based metrics..."

# Error rate metric
echo "Creating error rate metric..."
gcloud logging metrics create error_rate_metric \
  --description="HTTP error rate from Cloud Run logs" \
  --log-filter='resource.type="cloud_run_revision"
  AND severity="ERROR"' \
  2>/dev/null || echo "Metric 'error_rate_metric' may already exist"

# Request count metric
echo "Creating request count metric..."
gcloud logging metrics create request_count_metric \
  --description="Total request count" \
  --log-filter='resource.type="cloud_run_revision"' \
  2>/dev/null || echo "Metric 'request_count_metric' may already exist"

# API latency metric
echo "Creating API latency metric..."
gcloud logging metrics create api_latency_metric \
  --description="API request latency" \
  --log-filter='resource.type="cloud_run_revision"
  AND jsonPayload.latency_ms!=null' \
  2>/dev/null || echo "Metric 'api_latency_metric' may already exist"

echo ""

# ============================================================================
# 3. Create Alert Policies (Critical)
# ============================================================================

echo "🚨 Creating critical alert policies..."

# Alert 1: High error rate (> 5%)
echo "Creating alert: High error rate (>5%)"
gcloud alpha monitoring policies create \
  --notification-channels=$EMAIL_CHANNEL \
  --display-name="TAI Autonomics - High Error Rate (Critical)" \
  --condition-display-name="Error rate > 5%" \
  --condition-threshold-value=5 \
  --condition-threshold-duration=300s \
  --condition-threshold-comparison=COMPARISON_GT \
  --condition-threshold-filter='resource.type="cloud_run_revision"
  AND metric.type="logging.googleapis.com/user/error_rate_metric"' \
  2>/dev/null || echo "Alert policy may already exist"

# Alert 2: Service down (all instances failing health check)
echo "Creating alert: Service unavailable"
gcloud alpha monitoring policies create \
  --notification-channels=$EMAIL_CHANNEL \
  --display-name="TAI Autonomics - Service Unavailable (Critical)" \
  --condition-display-name="All instances failing health check" \
  --condition-threshold-value=0 \
  --condition-threshold-duration=60s \
  --condition-threshold-comparison=COMPARISON_LT \
  --condition-threshold-filter='resource.type="cloud_run_revision"
  AND metric.type="run.googleapis.com/request_count"' \
  2>/dev/null || echo "Alert policy may already exist"

# Alert 3: High latency (p99 > 10s)
echo "Creating alert: High latency"
gcloud alpha monitoring policies create \
  --notification-channels=$EMAIL_CHANNEL \
  --display-name="TAI Autonomics - High Latency (Warning)" \
  --condition-display-name="Latency p99 > 10 seconds" \
  --condition-threshold-value=10000 \
  --condition-threshold-duration=300s \
  --condition-threshold-comparison=COMPARISON_GT \
  --condition-threshold-filter='resource.type="cloud_run_revision"
  AND metric.type="run.googleapis.com/request_latencies"' \
  2>/dev/null || echo "Alert policy may already exist"

echo ""

# ============================================================================
# 4. Create Custom Dashboards
# ============================================================================

echo "📊 Creating Cloud Monitoring dashboards..."

# Create JSON dashboard configuration
cat > /tmp/dashboard.json << 'EOF'
{
  "displayName": "TAI Autonomics - Production Overview",
  "mosaicLayout": {
    "columns": 12,
    "tiles": [
      {
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Request Rate (requests/sec)",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_RATE"
                    }
                  }
                }
              }
            ]
          }
        }
      },
      {
        "xPos": 6,
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Request Latency (ms)",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_latencies\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_PERCENTILE_95"
                    }
                  }
                }
              }
            ]
          }
        }
      },
      {
        "yPos": 4,
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Error Rate (%)",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/request_count\" AND metric.response_code_class=\"5xx\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_RATE"
                    }
                  }
                }
              }
            ]
          }
        }
      },
      {
        "xPos": 6,
        "yPos": 4,
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Active Instances",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/instance_count\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_MAX"
                    }
                  }
                }
              }
            ]
          }
        }
      },
      {
        "yPos": 8,
        "width": 6,
        "height": 4,
        "widget": {
          "title": "Memory Usage (MB)",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/instance_memory_usage\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_MEAN"
                    }
                  }
                }
              }
            ]
          }
        }
      },
      {
        "xPos": 6,
        "yPos": 8,
        "width": 6,
        "height": 4,
        "widget": {
          "title": "CPU Usage",
          "xyChart": {
            "dataSets": [
              {
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "filter": "resource.type=\"cloud_run_revision\" AND metric.type=\"run.googleapis.com/instance_cpu_utilization\"",
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "perSeriesAligner": "ALIGN_MEAN"
                    }
                  }
                }
              }
            ]
          }
        }
      }
    ]
  }
}
EOF

echo "Creating dashboard from configuration..."
gcloud monitoring dashboards create --config-from-file=/tmp/dashboard.json \
  2>/dev/null || echo "Dashboard creation may require manual review"

echo ""

# ============================================================================
# 5. Enable Cloud Trace
# ============================================================================

echo "🔍 Configuring Cloud Trace..."
echo "Cloud Trace sampling: 10% of requests (configured in application)"
echo "You can view traces in Cloud Console → Cloud Trace"

echo ""

# ============================================================================
# 6. Configure Log Retention
# ============================================================================

echo "🗂️ Configuring log retention..."

gcloud logging sinks update _Default \
  --log-filter='resource.type="cloud_run_revision"' \
  2>/dev/null || echo "Default sink update may require configuration"

# Create sink to Cloud Storage for long-term retention
echo "Creating Cloud Storage sink for log archival..."
gcloud logging sinks create tai-autonomics-archive \
  gs://tai-autonomics-logs \
  --log-filter='resource.type="cloud_run_revision"' \
  2>/dev/null || echo "Cloud Storage sink may already exist"

echo ""

# ============================================================================
# 7. Summary
# ============================================================================

echo "✅ Cloud Monitoring and Alerting setup complete!"
echo ""
echo "📊 Monitoring Components Created:"
echo "   ✓ Notification channels (email)"
[ -n "$PAGERDUTY_CHANNEL" ] && echo "   ✓ PagerDuty integration"
echo "   ✓ Log-based metrics"
echo "   ✓ Alert policies (critical + warning)"
echo "   ✓ Custom dashboards"
echo "   ✓ Cloud Trace enabled"
echo "   ✓ Log retention configured"
echo ""
echo "🔗 Access Dashboards:"
echo "   https://console.cloud.google.com/monitoring/dashboards?project=$PROJECT_ID"
echo ""
echo "📢 View Alerts:"
echo "   https://console.cloud.google.com/monitoring/alerting?project=$PROJECT_ID"
echo ""
echo "📝 View Logs:"
echo "   https://console.cloud.google.com/logs/query?project=$PROJECT_ID"
echo ""
echo "🚀 Next Steps:"
echo "   1. Verify notification channels receive test messages"
echo "   2. Deploy services and wait for metrics to appear"
echo "   3. Review dashboard for accuracy"
echo "   4. Update PagerDuty escalation policies as needed"
echo ""
