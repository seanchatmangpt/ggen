//! Kaizen metrics collection with Prometheus + OpenTelemetry integration
//!
//! Implements TPS metrics:
//! - **Jidoka**: Circuit breaker autonomic quality metrics
//! - **Kanban**: Flow metrics (queue depth, throughput, latency)
//! - **Andon**: Visibility metrics (alerts, logs, MTTD)
//! - **Heijunka**: Level scheduling and load balancing metrics

use crate::Result;
use dashmap::DashMap;
use prometheus::{Counter, Gauge, Histogram, IntCounter, IntGauge, Registry};
use std::sync::Arc;

/// Jidoka metrics (autonomic circuit breaker quality)
#[derive(Clone, Debug)]
pub struct JidokaMetrics {
    /// Circuit open percentage
    pub circuit_open_percent: Arc<Gauge>,
    /// Failure rate per minute
    pub failure_rate: Arc<Gauge>,
    /// Mean recovery time (seconds)
    pub recovery_time_secs: Arc<Gauge>,
    /// Total circuit opens
    pub total_opens: Arc<IntCounter>,
    /// Total failures detected
    pub total_failures: Arc<IntCounter>,
    /// Circuits by status
    pub circuit_status: Arc<IntGauge>,
}

impl JidokaMetrics {
    /// Create new Jidoka metrics (must register with Prometheus registry)
    pub fn new(registry: &Registry) -> Result<Self> {
        let circuit_open_percent = Arc::new(
            Gauge::new("jidoka_circuit_open_percent", "Percentage of circuits open")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(circuit_open_percent.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let failure_rate = Arc::new(
            Gauge::new("jidoka_failure_rate_per_min", "Failures detected per minute")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(failure_rate.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let recovery_time_secs = Arc::new(
            Gauge::new("jidoka_recovery_time_secs", "Mean recovery time in seconds")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(recovery_time_secs.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let total_opens = Arc::new(
            IntCounter::new("jidoka_circuit_opens_total", "Total circuit open events")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(total_opens.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let total_failures = Arc::new(
            IntCounter::new("jidoka_failures_total", "Total failures detected")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(total_failures.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let circuit_status = Arc::new(
            IntGauge::new("jidoka_circuit_status", "Circuit status (1=open, 0=closed)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(circuit_status.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        Ok(Self {
            circuit_open_percent,
            failure_rate,
            recovery_time_secs,
            total_opens,
            total_failures,
            circuit_status,
        })
    }
}

/// Kanban metrics (flow, queue, throughput)
#[derive(Clone, Debug)]
pub struct KanbanMetrics {
    /// Queue depth (tasks waiting)
    pub queue_depth: Arc<Gauge>,
    /// Latency p99 (milliseconds)
    pub latency_p99_ms: Arc<Gauge>,
    /// Throughput (tasks per minute)
    pub throughput_per_min: Arc<Gauge>,
    /// Mean wait time (seconds)
    pub wait_time_secs: Arc<Gauge>,
    /// Queue saturation (0-100%)
    pub queue_saturation_percent: Arc<Gauge>,
    /// Latency histogram
    pub latency_histogram: Arc<Histogram>,
}

impl KanbanMetrics {
    /// Create new Kanban metrics
    pub fn new(registry: &Registry) -> Result<Self> {
        let queue_depth = Arc::new(
            Gauge::new("kanban_queue_depth", "Current queue depth (tasks waiting)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(queue_depth.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let latency_p99_ms = Arc::new(
            Gauge::new("kanban_latency_p99_ms", "P99 latency in milliseconds")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(latency_p99_ms.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let throughput_per_min = Arc::new(
            Gauge::new("kanban_throughput_per_min", "Tasks completed per minute")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(throughput_per_min.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let wait_time_secs = Arc::new(
            Gauge::new("kanban_wait_time_secs", "Mean task wait time in seconds")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(wait_time_secs.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let queue_saturation_percent = Arc::new(
            Gauge::new("kanban_queue_saturation_percent", "Queue saturation (0-100%)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(queue_saturation_percent.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let latency_histogram = Arc::new(
            Histogram::new("kanban_latency_seconds", "Request latency distribution")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(latency_histogram.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        Ok(Self {
            queue_depth,
            latency_p99_ms,
            throughput_per_min,
            wait_time_secs,
            queue_saturation_percent,
            latency_histogram,
        })
    }
}

/// Andon metrics (visibility, alerting, MTTD)
#[derive(Clone, Debug)]
pub struct AndonMetrics {
    /// Total log entries
    pub log_volume: Arc<IntCounter>,
    /// Alerts per minute
    pub alert_frequency_per_min: Arc<Gauge>,
    /// Mean Time To Detection (seconds)
    pub mttd_secs: Arc<Gauge>,
    /// Alert types
    pub alert_types_total: Arc<IntCounter>,
    /// Critical alerts
    pub critical_alerts: Arc<Gauge>,
}

impl AndonMetrics {
    /// Create new Andon metrics
    pub fn new(registry: &Registry) -> Result<Self> {
        let log_volume = Arc::new(
            IntCounter::new("andon_log_volume_total", "Total log entries")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(log_volume.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let alert_frequency_per_min = Arc::new(
            Gauge::new("andon_alerts_per_min", "Alerts per minute")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(alert_frequency_per_min.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let mttd_secs = Arc::new(
            Gauge::new("andon_mttd_secs", "Mean Time To Detection (seconds)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(mttd_secs.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let alert_types_total = Arc::new(
            IntCounter::new("andon_alert_types_total", "Total alert types triggered")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(alert_types_total.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let critical_alerts = Arc::new(
            Gauge::new("andon_critical_alerts", "Current critical alerts")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(critical_alerts.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        Ok(Self {
            log_volume,
            alert_frequency_per_min,
            mttd_secs,
            alert_types_total,
            critical_alerts,
        })
    }
}

/// Heijunka metrics (leveling, load balancing, utilization)
#[derive(Clone, Debug)]
pub struct HeijunkaMetrics {
    /// Load balance coefficient (0-1, 1 is perfect)
    pub load_balance_coefficient: Arc<Gauge>,
    /// Worker utilization (0-100%)
    pub worker_utilization_percent: Arc<Gauge>,
    /// Queue variance (stddev of queue depth)
    pub queue_variance: Arc<Gauge>,
    /// Workers active
    pub workers_active: Arc<IntGauge>,
    /// Workers idle
    pub workers_idle: Arc<IntGauge>,
}

impl HeijunkaMetrics {
    /// Create new Heijunka metrics
    pub fn new(registry: &Registry) -> Result<Self> {
        let load_balance_coefficient = Arc::new(
            Gauge::new("heijunka_load_balance_coeff", "Load balance coefficient (0-1)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(load_balance_coefficient.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let worker_utilization_percent = Arc::new(
            Gauge::new("heijunka_worker_utilization_percent", "Worker utilization (0-100%)")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(worker_utilization_percent.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let queue_variance = Arc::new(
            Gauge::new("heijunka_queue_variance", "Variance in queue depth")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(queue_variance.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let workers_active = Arc::new(
            IntGauge::new("heijunka_workers_active", "Active workers")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(workers_active.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        let workers_idle = Arc::new(
            IntGauge::new("heijunka_workers_idle", "Idle workers")
                .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?,
        );
        registry
            .register(Box::new(workers_idle.clone()))
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))?;

        Ok(Self {
            load_balance_coefficient,
            worker_utilization_percent,
            queue_variance,
            workers_active,
            workers_idle,
        })
    }
}

/// Master Kaizen metrics collection
#[derive(Clone)]
pub struct KaizenMetrics {
    /// Jidoka metrics
    pub jidoka: Arc<JidokaMetrics>,
    /// Kanban metrics
    pub kanban: Arc<KanbanMetrics>,
    /// Andon metrics
    pub andon: Arc<AndonMetrics>,
    /// Heijunka metrics
    pub heijunka: Arc<HeijunkaMetrics>,
    /// Metric event history (timestamp -> values)
    history: Arc<DashMap<String, Vec<crate::TimestampedValue>>>,
    /// SLO definitions
    slos: Arc<DashMap<String, crate::Slo>>,
    /// Registry for Prometheus
    registry: Arc<Registry>,
}

impl KaizenMetrics {
    /// Create new KaizenMetrics with Prometheus registry
    pub fn new() -> Result<Self> {
        let registry = Registry::new();
        let jidoka = Arc::new(JidokaMetrics::new(&registry)?);
        let kanban = Arc::new(KanbanMetrics::new(&registry)?);
        let andon = Arc::new(AndonMetrics::new(&registry)?);
        let heijunka = Arc::new(HeijunkaMetrics::new(&registry)?);

        Ok(Self {
            jidoka,
            kanban,
            andon,
            heijunka,
            history: Arc::new(DashMap::new()),
            slos: Arc::new(DashMap::new()),
            registry: Arc::new(registry),
        })
    }

    /// Record metric value to history
    pub fn record_value(&self, metric: String, value: f64, tags: std::collections::HashMap<String, String>) {
        let entry = crate::TimestampedValue {
            timestamp: chrono::Utc::now(),
            value,
            tags,
        };

        self.history
            .entry(metric)
            .or_insert_with(Vec::new)
            .push(entry);
    }

    /// Register SLO
    pub fn register_slo(&self, slo: crate::Slo) {
        self.slos.insert(slo.name.clone(), slo);
    }

    /// Get SLO by name
    pub fn get_slo(&self, name: &str) -> Option<crate::Slo> {
        self.slos.get(name).map(|r| r.clone())
    }

    /// Get all SLOs
    pub fn all_slos(&self) -> Vec<crate::Slo> {
        self.slos.iter().map(|r| r.value().clone()).collect()
    }

    /// Get metric history
    pub fn get_history(&self, metric: &str) -> Vec<crate::TimestampedValue> {
        self.history
            .get(metric)
            .map(|r| r.clone())
            .unwrap_or_default()
    }

    /// Get Prometheus registry
    pub fn registry(&self) -> Arc<Registry> {
        self.registry.clone()
    }

    /// Export Prometheus metrics in text format
    pub fn export_prometheus(&self) -> Result<String> {
        use prometheus::Encoder;
        let encoder = prometheus::TextEncoder::new();
        let metric_families = self.registry.gather();
        encoder
            .encode(&metric_families, &mut Vec::new())
            .map(|_| String::new())
            .map_err(|e| crate::KaizenError::MetricsError(e.to_string()))
    }
}

impl Default for KaizenMetrics {
    fn default() -> Self {
        Self::new().expect("Failed to create default KaizenMetrics")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_creation() {
        let metrics = KaizenMetrics::new().expect("Failed to create metrics");
        assert!(metrics.registry.is_some());
    }

    #[test]
    fn test_record_value() {
        let metrics = KaizenMetrics::new().expect("Failed to create metrics");
        let mut tags = std::collections::HashMap::new();
        tags.insert("service".to_string(), "payment".to_string());

        metrics.record_value("test_metric".to_string(), 42.0, tags);
        let history = metrics.get_history("test_metric");

        assert_eq!(history.len(), 1);
        assert_eq!(history[0].value, 42.0);
    }

    #[test]
    fn test_slo_management() {
        let metrics = KaizenMetrics::new().expect("Failed to create metrics");
        let slo = crate::Slo {
            name: "latency_p99".to_string(),
            metric: "kanban_latency_p99_ms".to_string(),
            target: 100.0,
            window_secs: 300,
            is_maximum: true,
        };

        metrics.register_slo(slo.clone());
        let retrieved = metrics.get_slo("latency_p99");

        assert!(retrieved.is_some());
        assert_eq!(retrieved.unwrap().target, 100.0);
    }
}
