//! Performance Metrics and Monitoring

use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

use crate::OSIRISError, Result;

/// Performance metrics collector
#[derive(Debug, Clone)]
pub struct PerformanceMetrics {
    metrics: Arc<RwLock<HashMap<String, MetricValue>>>,
    history: Vec<MetricSnapshot>,
    max_history_size: usize,
}

#[derive(Debug, Clone)]
pub struct MetricValue {
    pub value: f64,
    pub unit: String,
    pub timestamp: Instant,
    pub tags: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct MetricSnapshot {
    pub timestamp: Instant,
    pub metrics: HashMap<String, MetricValue>,
}

#[derive(Debug, Clone)]
pub struct SLOCheck {
    pub response_time: u64, // ms
    pub throughput: u64,
    pub error_rate: f64,
    pub timestamp: Instant,
}

/// Monitoring service
pub struct MonitoringService {
    metrics: PerformanceMetrics,
    slo_checks: Vec<SLOCheck>,
    alert_thresholds: HashMap<String, f64>,
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self {
            metrics: Arc::new(RwLock::new(HashMap::new())),
            history: Vec::new(),
            max_history_size: 1000,
        }
    }
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_history_size(mut self, size: usize) -> Self {
        self.max_history_size = size;
        self
    }

    /// Record a metric value
    pub async fn record(&self, name: &str, value: f64, unit: &str, tags: HashMap<String, String>) {
        let metric = MetricValue {
            value,
            unit: unit.to_string(),
            timestamp: Instant::now(),
            tags,
        };

        let mut metrics = self.metrics.write().await;
        metrics.insert(name.to_string(), metric);

        // Keep only recent metrics
        if metrics.len() > self.max_history_size {
            if let Some(oldest_key) = metrics.keys().next().cloned() {
                metrics.remove(&oldest_key);
            } else {
                warn!("Metrics collection empty during cleanup attempt");
            }
        }

        debug!("Recorded metric {}: {} {}", name, value, unit);
    }

    /// Get metric value
    pub async fn get_metric(&self, name: &str) -> Option<MetricValue> {
        let metrics = self.metrics.read().await;
        metrics.get(name).cloned()
    }

    /// Get all metrics
    pub async fn get_all_metrics(&self) -> HashMap<String, MetricValue> {
        let metrics = self.metrics.read().await;
        metrics.clone()
    }

    /// Calculate average for a metric
    pub async fn calculate_average(&self, name: &str) -> Option<f64> {
        let metrics = self.metrics.read().await;

        let values: Vec<_> = metrics
            .values()
            .filter(|m| m.timestamp.elapsed() < Duration::from_secs(300)) // Last 5 minutes
            .map(|m| m.value)
            .collect();

        if values.is_empty() {
            return None;
        }

        Some(values.iter().sum::<f64>() / values.len() as f64)
    }

    /// Calculate metrics percentiles
    pub async fn calculate_percentiles(&self, name: &str, percentiles: &[f64]) -> HashMap<f64, f64> {
        let metrics = self.metrics.read().await;

        let values: Vec<_> = metrics
            .values()
            .filter(|m| m.timestamp.elapsed() < Duration::from_secs(300))
            .map(|m| m.value)
            .collect();

        if values.is_empty() {
            return HashMap::new();
        }

        let mut sorted = values;
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let mut result = HashMap::new();
        for &p in percentiles {
            let index = ((sorted.len() - 1) as f64 * p / 100.0) as usize;
            result.insert(p, sorted[index]);
        }

        result
    }

    /// Take a snapshot of current metrics
    pub async fn snapshot(&self) -> MetricSnapshot {
        let metrics = self.metrics.read().await;
        let timestamp = Instant::now();

        let snapshot = MetricSnapshot {
            timestamp,
            metrics: metrics.clone(),
        };

        self.history.push(snapshot.clone());

        // Keep history size limited
        if self.history.len() > self.max_history_size {
            self.history.remove(0);
        }

        snapshot
    }
}

impl MonitoringService {
    pub fn new() -> Self {
        Self {
            metrics: PerformanceMetrics::new(),
            slo_checks: Vec::new(),
            alert_thresholds: Self::default_alert_thresholds(),
        }
    }

    pub fn with_thresholds(mut self, thresholds: HashMap<String, f64>) -> Self {
        self.alert_thresholds = thresholds;
        self
    }

    /// Start the monitoring service
    pub async fn start(&self) {
        info!("Monitoring service started");

        // Start metrics collection task
        let metrics_service = self.metrics.clone();
        tokio::spawn(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(60)).await;

                // Take periodic snapshot
                metrics_service.snapshot().await;

                info!("Periodic metrics snapshot taken");
            }
        });
    }

    /// Record a performance metric
    pub async fn record_metric(
        &self,
        name: &str,
        value: f64,
        unit: &str,
        tags: HashMap<String, String>,
    ) {
        self.metrics.record(name, value, unit, tags).await;
    }

    /// Check SLO compliance
    pub async fn check_slos(&self) -> Result<SLOCheck> {
        let response_time = self.metrics.calculate_average("response_time").await
            .unwrap_or(0.0);
        let throughput = self.metrics.calculate_average("throughput").await
            .unwrap_or(0.0) as u64;
        let error_rate = self.metrics.calculate_average("error_rate").await
            .unwrap_or(0.0);

        let slo_check = SLOCheck {
            response_time: response_time as u64,
            throughput,
            error_rate,
            timestamp: Instant::now(),
        };

        // Check against thresholds
        if let Some(threshold) = self.alert_thresholds.get("response_time") {
            if response_time > *threshold {
                warn!("SLO violated: response time {}ms > {}ms", response_time, threshold);
            }
        }

        if let Some(threshold) = self.alert_thresholds.get("error_rate") {
            if error_rate > *threshold {
                warn!("SLO violated: error rate {} > {}", error_rate, threshold);
            }
        }

        if let Some(threshold) = self.alert_thresholds.get("throughput") {
            if throughput as f64 < *threshold {
                warn!("SLO violated: throughput {} < {}", throughput, threshold);
            }
        }

        self.slo_checks.push(slo_check.clone());

        Ok(slo_check)
    }

    /// Get current metrics
    pub async fn get_metrics(&self) -> Value {
        let metrics = self.metrics.get_all_metrics().await;
        let slo_check = self.check_slos().await.unwrap_or_default();

        json!({
            "timestamp": chrono::Utc::now().to_rfc3339(),
            "slo_check": {
                "response_time_ms": slo_check.response_time,
                "throughput": slo_check.throughput,
                "error_rate": slo_check.error_rate,
            },
            "metrics": metrics.iter().map(|(name, metric)| {
                json!({
                    "name": name,
                    "value": metric.value,
                    "unit": metric.unit,
                    "timestamp": metric.timestamp.elapsed().as_secs(),
                    "tags": metric.tags,
                })
            }).collect::<Vec<_>>(),
            "alert_thresholds": self.alert_thresholds,
        })
    }

    /// Get performance summary
    pub async fn get_performance_summary(&self) -> Value {
        let avg_response_time = self.metrics.calculate_average("response_time").await;
        let avg_throughput = self.metrics.calculate_average("throughput").await;
        let avg_error_rate = self.metrics.calculate_average("error_rate").await;

        json!({
            "summary": {
                "avg_response_time_ms": avg_response_time.unwrap_or(0.0),
                "avg_throughput": avg_throughput.unwrap_or(0.0),
                "avg_error_rate": avg_error_rate.unwrap_or(0.0),
                "timestamp": chrono::Utc::now().to_rfc3339(),
            },
            "status": if (avg_error_rate.unwrap_or(0.0) < 0.01 && avg_response_time.unwrap_or(0.0) < 100.0) {
                "healthy"
            } else if (avg_error_rate.unwrap_or(0.0) < 0.05 && avg_response_time.unwrap_or(0.0) < 200.0) {
                "warning"
            } else {
                "critical"
            },
        })
    }

    /// Default alert thresholds
    fn default_alert_thresholds() -> HashMap<String, f64> {
        let mut thresholds = HashMap::new();
        thresholds.insert("response_time".to_string(), 100.0);
        thresholds.insert("error_rate".to_string(), 0.01);
        thresholds.insert("throughput".to_string(), 10.0);
        thresholds
    }
}

impl Default for MonitoringService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_metrics_recording() {
        let metrics = PerformanceMetrics::new();

        let tags = HashMap::new();
        metrics.record("test_metric", 10.5, "ms", tags.clone()).await;

        let value = metrics.get_metric("test_metric").await;
        assert!(value.is_some());
        assert_eq!(value.unwrap().value, 10.5);
    }

    #[tokio::test]
    async fn test_average_calculation() {
        let metrics = PerformanceMetrics::new();

        let tags = HashMap::new();
        metrics.record("test_metric", 10.0, "ms", tags.clone()).await;
        metrics.record("test_metric", 20.0, "ms", tags.clone()).await;
        metrics.record("test_metric", 30.0, "ms", tags.clone()).await;

        let avg = metrics.calculate_average("test_metric").await;
        assert!(avg.is_some());
        assert!((avg.unwrap() - 20.0).abs() < 0.001);
    }

    #[tokio::test]
    async fn test_monitoring_service() {
        let service = MonitoringService::new();

        let tags = HashMap::new();
        service.record_metric("test_metric", 10.0, "ms", tags).await;

        let metrics = service.get_metrics().await;
        assert!(metrics.is_object());
        assert!(metrics["metrics"].is_array());
    }
}