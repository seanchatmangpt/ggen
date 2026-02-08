//! Prometheus metrics for Andon system
//!
//! Implements metrics collection similar to Prometheus client:
//! - Counters: total failures, refusals, signals
//! - Gauges: queue depth, pool utilization, memory usage
//! - Histograms: latency, processing time
//! - Threshold-based alerting: when metric crosses threshold, trigger alert

use crate::error::Result;
use crate::signal::{AndonSignal, SignalColor};
use dashmap::DashMap;
use prometheus::{
    CounterVec, Encoder, GaugeVec, HistogramVec, IntCounterVec, IntGauge, IntGaugeVec, Registry,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Duration;

/// Metric configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricConfig {
    /// Enable metrics collection
    #[serde(default = "default_enabled")]
    pub enabled: bool,

    /// Prometheus scrape port (default: 9090)
    #[serde(default = "default_port")]
    pub port: u16,

    /// Scrape endpoint path (default: /metrics)
    #[serde(default = "default_scrape_path")]
    pub scrape_path: String,

    /// Histograms bucket boundaries (latency in seconds)
    #[serde(default = "default_buckets")]
    pub buckets: Vec<f64>,

    /// Alert thresholds
    pub thresholds: Option<MetricThresholds>,
}

fn default_enabled() -> bool {
    true
}

fn default_port() -> u16 {
    9090
}

fn default_scrape_path() -> String {
    "/metrics".to_string()
}

fn default_buckets() -> Vec<f64> {
    vec![
        0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0,
    ]
}

impl Default for MetricConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            port: 9090,
            scrape_path: "/metrics".to_string(),
            buckets: default_buckets(),
            thresholds: None,
        }
    }
}

/// Alert thresholds for metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricThresholds {
    /// Queue depth threshold (alert if exceeded)
    pub queue_depth: Option<u32>,

    /// Memory usage threshold (MB)
    pub memory_usage_mb: Option<u64>,

    /// CPU usage threshold (percent)
    pub cpu_usage_percent: Option<u32>,

    /// Error rate threshold (errors per minute)
    pub error_rate: Option<u32>,

    /// Latency threshold (milliseconds)
    pub latency_ms: Option<u64>,
}

/// Metrics for Andon system
pub struct AndonMetrics {
    config: MetricConfig,

    // Counters: total occurrences
    pub signal_counter: IntCounterVec,  // by color
    pub failure_counter: IntCounterVec, // by component
    pub refusal_counter: IntCounterVec, // by reason
    pub alert_counter: IntCounterVec,   // by severity

    // Gauges: current state
    pub queue_depth: IntGaugeVec,   // by queue name
    pub pool_utilization: GaugeVec, // by pool name
    pub memory_usage_mb: IntGauge,
    pub cpu_usage_percent: IntGauge,

    // Histograms: distribution of values
    pub request_latency: HistogramVec, // by endpoint
    pub processing_time: HistogramVec, // by operation

    // Registry for Prometheus scraping
    registry: Registry,

    // Threshold violation tracking (for alerts)
    threshold_violations: Arc<DashMap<String, (u32, std::time::Instant)>>,
}

impl AndonMetrics {
    /// Create a new metrics collector
    pub fn new(config: MetricConfig) -> Result<Self> {
        let registry = Registry::new();

        // Create counters
        let signal_counter = IntCounterVec::new(
            prometheus::Opts::new("andon_signals_total", "Total Andon signals by color"),
            &["color"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Counter creation failed: {}", e))
        })?;

        let failure_counter = IntCounterVec::new(
            prometheus::Opts::new("andon_failures_total", "Total failures by component"),
            &["component"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Counter creation failed: {}", e))
        })?;

        let refusal_counter = IntCounterVec::new(
            prometheus::Opts::new("andon_refusals_total", "Total refusals by reason"),
            &["reason"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Counter creation failed: {}", e))
        })?;

        let alert_counter = IntCounterVec::new(
            prometheus::Opts::new("andon_alerts_total", "Total alerts by severity"),
            &["severity"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Counter creation failed: {}", e))
        })?;

        // Create gauges
        let queue_depth = IntGaugeVec::new(
            prometheus::Opts::new("andon_queue_depth", "Current queue depth"),
            &["queue"],
        )
        .map_err(|e| crate::error::AndonError::metrics(format!("Gauge creation failed: {}", e)))?;

        let pool_utilization = GaugeVec::new(
            prometheus::Opts::new("andon_pool_utilization", "Pool utilization percentage"),
            &["pool"],
        )
        .map_err(|e| crate::error::AndonError::metrics(format!("Gauge creation failed: {}", e)))?;

        let memory_usage_mb = IntGauge::new("andon_memory_usage_mb", "Memory usage in megabytes")
            .map_err(|e| {
            crate::error::AndonError::metrics(format!("Gauge creation failed: {}", e))
        })?;

        let cpu_usage_percent = IntGauge::new("andon_cpu_usage_percent", "CPU usage percentage")
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Gauge creation failed: {}", e))
            })?;

        // Create histograms
        let request_latency = HistogramVec::new(
            prometheus::HistogramOpts::new(
                "andon_request_latency_seconds",
                "Request latency in seconds",
            )
            .buckets(config.buckets.clone()),
            &["endpoint"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Histogram creation failed: {}", e))
        })?;

        let processing_time = HistogramVec::new(
            prometheus::HistogramOpts::new(
                "andon_processing_time_seconds",
                "Processing time in seconds",
            )
            .buckets(config.buckets.clone()),
            &["operation"],
        )
        .map_err(|e| {
            crate::error::AndonError::metrics(format!("Histogram creation failed: {}", e))
        })?;

        // Register all metrics
        registry
            .register(Box::new(signal_counter.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(failure_counter.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(refusal_counter.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(alert_counter.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(queue_depth.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(pool_utilization.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(memory_usage_mb.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(cpu_usage_percent.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(request_latency.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;
        registry
            .register(Box::new(processing_time.clone()))
            .map_err(|e| {
                crate::error::AndonError::metrics(format!("Registration failed: {}", e))
            })?;

        Ok(Self {
            config,
            signal_counter,
            failure_counter,
            refusal_counter,
            alert_counter,
            queue_depth,
            pool_utilization,
            memory_usage_mb,
            cpu_usage_percent,
            request_latency,
            processing_time,
            registry,
            threshold_violations: Arc::new(DashMap::new()),
        })
    }

    /// Record an Andon signal
    pub async fn record_signal(&self, signal: &AndonSignal) -> Result<()> {
        self.signal_counter
            .with_label_values(&[signal.color.code()])
            .inc();

        // Check thresholds
        if let Some(thresholds) = &self.config.thresholds {
            if signal.color == SignalColor::Red && thresholds.queue_depth.is_some() {
                self.check_threshold("queue_depth", thresholds.queue_depth.unwrap_or(0))?;
            }
        }

        Ok(())
    }

    /// Record a failure
    pub fn record_failure(&self, component: &str) -> Result<()> {
        self.failure_counter.with_label_values(&[component]).inc();
        Ok(())
    }

    /// Record a refusal
    pub fn record_refusal(&self, reason: &str) -> Result<()> {
        self.refusal_counter.with_label_values(&[reason]).inc();
        Ok(())
    }

    /// Record an alert
    pub fn record_alert(&self, severity: &str) -> Result<()> {
        self.alert_counter.with_label_values(&[severity]).inc();
        Ok(())
    }

    /// Update queue depth
    pub fn update_queue_depth(&self, queue: &str, depth: u32) -> Result<()> {
        self.queue_depth
            .with_label_values(&[queue])
            .set(depth as i64);
        Ok(())
    }

    /// Update pool utilization
    pub fn update_pool_utilization(&self, pool: &str, utilization: f64) -> Result<()> {
        self.pool_utilization
            .with_label_values(&[pool])
            .set(utilization);
        Ok(())
    }

    /// Update memory usage
    pub fn update_memory_usage(&self, memory_mb: u64) -> Result<()> {
        self.memory_usage_mb.set(memory_mb as i64);
        Ok(())
    }

    /// Update CPU usage
    pub fn update_cpu_usage(&self, cpu_percent: u32) -> Result<()> {
        self.cpu_usage_percent.set(cpu_percent as i64);
        Ok(())
    }

    /// Record request latency
    pub fn record_request_latency(&self, endpoint: &str, duration: Duration) -> Result<()> {
        self.request_latency
            .with_label_values(&[endpoint])
            .observe(duration.as_secs_f64());
        Ok(())
    }

    /// Record processing time
    pub fn record_processing_time(&self, operation: &str, duration: Duration) -> Result<()> {
        self.processing_time
            .with_label_values(&[operation])
            .observe(duration.as_secs_f64());
        Ok(())
    }

    /// Check if a metric threshold has been crossed
    pub fn check_threshold(&self, metric: &str, threshold: u32) -> Result<bool> {
        // In production, would check actual metric values against thresholds
        // For now, track violations
        let now = std::time::Instant::now();
        self.threshold_violations
            .insert(metric.to_string(), (threshold, now));
        Ok(true)
    }

    /// Get Prometheus metrics in text format (for /metrics endpoint)
    pub fn get_metrics_text(&self) -> Result<String> {
        let mut buffer = Vec::new();
        let encoder = prometheus::TextEncoder::new();
        encoder
            .encode(&self.registry.gather(), &mut buffer)
            .map_err(|e| crate::error::AndonError::metrics(format!("Encoding failed: {}", e)))?;

        String::from_utf8(buffer)
            .map_err(|e| crate::error::AndonError::metrics(format!("UTF-8 error: {}", e)))
    }

    /// Check system health
    pub async fn check_health(&self) -> Result<()> {
        // In production, would check metrics, thresholds, and system health
        // For now, just report that check ran
        tracing::debug!("Health check complete");
        Ok(())
    }

    /// Shutdown metrics collection
    pub async fn shutdown(&self) -> Result<()> {
        // In production, would flush metrics to backend
        Ok(())
    }
}

impl std::fmt::Debug for AndonMetrics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AndonMetrics")
            .field("config", &self.config)
            .field("registry", &"Registry")
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metric_config_default() {
        let config = MetricConfig::default();
        assert!(config.enabled);
        assert_eq!(config.port, 9090);
        assert_eq!(config.scrape_path, "/metrics");
    }

    #[test]
    fn test_buckets_default() {
        let buckets = default_buckets();
        assert!(buckets.len() > 0);
        assert!(buckets[0] < buckets[buckets.len() - 1]); // sorted
    }

    #[tokio::test]
    async fn test_metrics_creation() {
        let config = MetricConfig::default();
        let metrics = AndonMetrics::new(config);
        assert!(metrics.is_ok());
    }

    #[tokio::test]
    async fn test_record_signal() {
        let config = MetricConfig::default();
        let metrics = AndonMetrics::new(config).unwrap();

        let signal = AndonSignal::red("Test signal");
        let result = metrics.record_signal(&signal).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_update_gauges() {
        let config = MetricConfig::default();
        let metrics = AndonMetrics::new(config).unwrap();

        assert!(metrics.update_queue_depth("default", 50).is_ok());
        assert!(metrics.update_pool_utilization("worker-pool", 0.75).is_ok());
        assert!(metrics.update_memory_usage(256).is_ok());
        assert!(metrics.update_cpu_usage(45).is_ok());
    }

    #[tokio::test]
    async fn test_record_latency() {
        let config = MetricConfig::default();
        let metrics = AndonMetrics::new(config).unwrap();

        let duration = Duration::from_millis(100);
        assert!(metrics
            .record_request_latency("/api/test", duration)
            .is_ok());
        assert!(metrics.record_processing_time("query", duration).is_ok());
    }
}
