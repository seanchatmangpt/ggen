//! Dashboard API for external integrations

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::super::error::{GovernanceError, Result};
use super::metrics::{Dashboard, MetricsSnapshot};

/// Metrics export format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsExport {
    pub format: ExportFormat,
    pub data: MetricsSnapshot,
    pub exported_at: DateTime<Utc>,
}

/// Export format options
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ExportFormat {
    Json,
    Prometheus,
    Csv,
}

/// Dashboard API for external integrations
pub struct DashboardApi {
    dashboard: Dashboard,
}

impl DashboardApi {
    /// Create a new dashboard API
    pub fn new(dashboard: Dashboard) -> Self {
        Self { dashboard }
    }

    /// Export metrics in specified format
    pub async fn export_metrics(&self, format: ExportFormat) -> Result<String> {
        let snapshot = self.dashboard.get_metrics_snapshot().await?;

        match format {
            ExportFormat::Json => serde_json::to_string_pretty(&snapshot)
                .map_err(|e| GovernanceError::SerializationError(e.to_string())),
            ExportFormat::Prometheus => Ok(self.to_prometheus_format(&snapshot)),
            ExportFormat::Csv => Ok(self.to_csv_format(&snapshot)),
        }
    }

    /// Convert metrics to Prometheus format
    fn to_prometheus_format(&self, metrics: &MetricsSnapshot) -> String {
        format!(
            "# HELP decisions_processed Total decisions processed\n\
             # TYPE decisions_processed counter\n\
             decisions_processed {}\n\
             # HELP decisions_approved Total decisions approved\n\
             # TYPE decisions_approved counter\n\
             decisions_approved {}\n\
             # HELP decisions_rejected Total decisions rejected\n\
             # TYPE decisions_rejected counter\n\
             decisions_rejected {}\n\
             # HELP policy_violations Total policy violations\n\
             # TYPE policy_violations counter\n\
             policy_violations {}\n\
             # HELP approval_rate Current approval rate\n\
             # TYPE approval_rate gauge\n\
             approval_rate {}\n",
            metrics.decisions_processed,
            metrics.decisions_approved,
            metrics.decisions_rejected,
            metrics.policy_violations,
            metrics.approval_rate,
        )
    }

    /// Convert metrics to CSV format
    fn to_csv_format(&self, metrics: &MetricsSnapshot) -> String {
        format!(
            "metric,value\n\
             decisions_processed,{}\n\
             decisions_approved,{}\n\
             decisions_rejected,{}\n\
             decisions_pending,{}\n\
             policy_violations,{}\n\
             safety_violations,{}\n\
             approval_rate,{}\n\
             rejection_rate,{}\n",
            metrics.decisions_processed,
            metrics.decisions_approved,
            metrics.decisions_rejected,
            metrics.decisions_pending,
            metrics.policy_violations,
            metrics.safety_violations,
            metrics.approval_rate,
            metrics.rejection_rate,
        )
    }

    /// Get health status
    pub async fn get_health(&self) -> Result<String> {
        let health = self.dashboard.get_health_status().await?;
        serde_json::to_string_pretty(&health)
            .map_err(|e| GovernanceError::SerializationError(e.to_string()))
    }

    /// Get metrics snapshot
    pub async fn get_metrics(&self) -> Result<String> {
        let metrics = self.dashboard.get_metrics_snapshot().await?;
        serde_json::to_string_pretty(&metrics)
            .map_err(|e| GovernanceError::SerializationError(e.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::metrics::DashboardConfig;

    #[tokio::test]
    async fn test_json_export() {
        let dashboard = Dashboard::new(DashboardConfig::default());
        let api = DashboardApi::new(dashboard);

        let json = api.export_metrics(ExportFormat::Json).await.unwrap();
        assert!(json.contains("decisions_processed"));
    }

    #[tokio::test]
    async fn test_prometheus_export() {
        let dashboard = Dashboard::new(DashboardConfig::default());
        let api = DashboardApi::new(dashboard);

        let prometheus = api.export_metrics(ExportFormat::Prometheus).await.unwrap();
        assert!(prometheus.contains("# HELP"));
        assert!(prometheus.contains("# TYPE"));
    }

    #[tokio::test]
    async fn test_csv_export() {
        let dashboard = Dashboard::new(DashboardConfig::default());
        let api = DashboardApi::new(dashboard);

        let csv = api.export_metrics(ExportFormat::Csv).await.unwrap();
        assert!(csv.contains("metric,value"));
        assert!(csv.contains("decisions_processed"));
    }
}
