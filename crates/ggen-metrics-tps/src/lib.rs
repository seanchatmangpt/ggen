//! Metrics collection and telemetry for A2A/TPS in ggen.
//!
//! Tracks throughput (λ), utilization (ρ), queue depth (L), and cycle time (W)
//! following queueing theory principles.

pub mod counters;
pub mod exporter;
pub mod gauges;
pub mod histograms;

use prometheus::Registry;
use std::sync::Arc;
use thiserror::Error;

pub use counters::Counters;
pub use exporter::Exporter;
pub use gauges::Gauges;
pub use histograms::Histograms;

/// Errors that can occur during metrics operations.
#[derive(Debug, Error)]
pub enum MetricsError {
    #[error("Prometheus error: {0}")]
    Prometheus(#[from] prometheus::Error),

    #[error("Export error: {0}")]
    Export(String),

    #[error("Invalid metric value: {0}")]
    InvalidValue(String),
}

pub type Result<T> = std::result::Result<T, MetricsError>;

/// Central metrics collector for A2A/TPS operations.
///
/// Provides unified access to counters, histograms, and gauges for tracking:
/// - λ (throughput): operations per second
/// - ρ (utilization): fraction of capacity in use
/// - L (queue depth): number of items waiting
/// - W (cycle time): end-to-end latency
#[derive(Clone)]
pub struct MetricsCollector {
    pub counters: Arc<Counters>,
    pub histograms: Arc<Histograms>,
    pub gauges: Arc<Gauges>,
    registry: Arc<Registry>,
}

impl MetricsCollector {
    /// Creates a new metrics collector with a fresh registry.
    pub fn new() -> Result<Self> {
        let registry = Registry::new();
        Self::with_registry(registry)
    }

    /// Creates a new metrics collector with a provided registry.
    pub fn with_registry(registry: Registry) -> Result<Self> {
        let counters = Arc::new(Counters::new(&registry)?);
        let histograms = Arc::new(Histograms::new(&registry)?);
        let gauges = Arc::new(Gauges::new(&registry)?);

        Ok(Self {
            counters,
            histograms,
            gauges,
            registry: Arc::new(registry),
        })
    }

    /// Returns a reference to the underlying Prometheus registry.
    pub fn registry(&self) -> &Registry {
        &self.registry
    }

    /// Records a receipt operation with timing.
    pub fn record_receipt(&self, duration_secs: f64) -> Result<()> {
        self.counters.receipts.inc();
        self.histograms.receipt_latency.observe(duration_secs);
        Ok(())
    }

    /// Records a packet operation with timing.
    pub fn record_packet(&self, duration_secs: f64) -> Result<()> {
        self.counters.packets.inc();
        self.histograms.packet_latency.observe(duration_secs);
        Ok(())
    }

    /// Records a task operation with timing.
    pub fn record_task(&self, duration_secs: f64) -> Result<()> {
        self.counters.tasks.inc();
        self.histograms.task_latency.observe(duration_secs);
        Ok(())
    }

    /// Updates queue depth (L in queueing theory).
    pub fn set_queue_depth(&self, depth: i64) -> Result<()> {
        self.gauges.queue_depth.set(depth);
        Ok(())
    }

    /// Updates token count.
    pub fn set_token_count(&self, count: i64) -> Result<()> {
        self.gauges.token_count.set(count);
        Ok(())
    }

    /// Updates utilization (ρ in queueing theory).
    pub fn set_utilization(&self, utilization: f64) -> Result<()> {
        if !(0.0..=1.0).contains(&utilization) {
            return Err(MetricsError::InvalidValue(format!(
                "Utilization must be between 0.0 and 1.0, got {}",
                utilization
            )));
        }
        self.gauges.utilization.set(utilization);
        Ok(())
    }

    /// Updates active agents count.
    pub fn set_active_agents(&self, count: i64) -> Result<()> {
        self.gauges.active_agents.set(count);
        Ok(())
    }

    /// Gathers all metrics for export.
    pub fn gather(&self) -> Vec<prometheus::proto::MetricFamily> {
        self.registry.gather()
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new().expect("Failed to create default MetricsCollector")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collector_creation() {
        let collector = MetricsCollector::new();
        assert!(collector.is_ok());
    }

    #[test]
    fn test_record_operations() {
        let collector = MetricsCollector::new().unwrap();

        assert!(collector.record_receipt(0.1).is_ok());
        assert!(collector.record_packet(0.2).is_ok());
        assert!(collector.record_task(0.3).is_ok());
    }

    #[test]
    fn test_set_gauges() {
        let collector = MetricsCollector::new().unwrap();

        assert!(collector.set_queue_depth(10).is_ok());
        assert!(collector.set_token_count(1000).is_ok());
        assert!(collector.set_utilization(0.75).is_ok());
        assert!(collector.set_active_agents(5).is_ok());
    }

    #[test]
    fn test_invalid_utilization() {
        let collector = MetricsCollector::new().unwrap();

        assert!(collector.set_utilization(-0.1).is_err());
        assert!(collector.set_utilization(1.5).is_err());
        assert!(collector.set_utilization(0.0).is_ok());
        assert!(collector.set_utilization(1.0).is_ok());
    }

    #[test]
    fn test_gather_metrics() {
        let collector = MetricsCollector::new().unwrap();

        collector.record_receipt(0.1).unwrap();
        collector.set_queue_depth(5).unwrap();

        let metrics = collector.gather();
        assert!(!metrics.is_empty());
    }
}
