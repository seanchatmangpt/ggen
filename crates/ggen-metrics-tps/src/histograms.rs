//! Histogram metrics for latency distributions.
//!
//! Tracks cycle time (W) distributions for different operation types:
//! - Receipt latency
//! - Packet latency
//! - Task latency
//! - End-to-end latency

use prometheus::{Histogram, HistogramOpts, Registry, Result as PrometheusResult};

/// Histogram metrics for latency tracking.
///
/// Records latency distributions to calculate:
/// - W (cycle time): percentiles (p50, p95, p99)
/// - SLO compliance
/// - Performance degradation
pub struct Histograms {
    /// Receipt processing latency distribution (seconds).
    pub receipt_latency: Histogram,

    /// Packet transmission latency distribution (seconds).
    pub packet_latency: Histogram,

    /// Task execution latency distribution (seconds).
    pub task_latency: Histogram,

    /// End-to-end operation latency distribution (seconds).
    pub e2e_latency: Histogram,

    /// Queue wait time distribution (seconds).
    pub queue_wait_time: Histogram,
}

impl Histograms {
    /// Creates and registers all histogram metrics.
    ///
    /// Uses buckets optimized for TPS operations:
    /// - Sub-millisecond: 0.001, 0.005
    /// - Milliseconds: 0.01, 0.05, 0.1, 0.5
    /// - Seconds: 1.0, 2.5, 5.0
    /// - Slow: 10.0, 30.0
    pub fn new(registry: &Registry) -> PrometheusResult<Self> {
        let buckets = vec![
            0.001, 0.005, 0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0,
        ];

        let receipt_latency = Histogram::with_opts(
            HistogramOpts::new(
                "ggen_tps_receipt_latency_seconds",
                "Receipt processing latency distribution",
            )
            .buckets(buckets.clone()),
        )?;

        let packet_latency = Histogram::with_opts(
            HistogramOpts::new(
                "ggen_tps_packet_latency_seconds",
                "Packet transmission latency distribution",
            )
            .buckets(buckets.clone()),
        )?;

        let task_latency = Histogram::with_opts(
            HistogramOpts::new(
                "ggen_tps_task_latency_seconds",
                "Task execution latency distribution",
            )
            .buckets(buckets.clone()),
        )?;

        let e2e_latency = Histogram::with_opts(
            HistogramOpts::new(
                "ggen_tps_e2e_latency_seconds",
                "End-to-end operation latency distribution",
            )
            .buckets(buckets.clone()),
        )?;

        let queue_wait_time = Histogram::with_opts(
            HistogramOpts::new(
                "ggen_tps_queue_wait_seconds",
                "Queue wait time distribution",
            )
            .buckets(buckets),
        )?;

        registry.register(Box::new(receipt_latency.clone()))?;
        registry.register(Box::new(packet_latency.clone()))?;
        registry.register(Box::new(task_latency.clone()))?;
        registry.register(Box::new(e2e_latency.clone()))?;
        registry.register(Box::new(queue_wait_time.clone()))?;

        Ok(Self {
            receipt_latency,
            packet_latency,
            task_latency,
            e2e_latency,
            queue_wait_time,
        })
    }

    /// Records receipt processing latency.
    pub fn observe_receipt(&self, duration_secs: f64) {
        self.receipt_latency.observe(duration_secs);
    }

    /// Records packet transmission latency.
    pub fn observe_packet(&self, duration_secs: f64) {
        self.packet_latency.observe(duration_secs);
    }

    /// Records task execution latency.
    pub fn observe_task(&self, duration_secs: f64) {
        self.task_latency.observe(duration_secs);
    }

    /// Records end-to-end operation latency.
    pub fn observe_e2e(&self, duration_secs: f64) {
        self.e2e_latency.observe(duration_secs);
    }

    /// Records queue wait time.
    pub fn observe_queue_wait(&self, duration_secs: f64) {
        self.queue_wait_time.observe(duration_secs);
    }

    /// Returns receipt latency statistics.
    pub fn get_receipt_stats(&self) -> HistogramStats {
        HistogramStats::from_histogram(&self.receipt_latency)
    }

    /// Returns packet latency statistics.
    pub fn get_packet_stats(&self) -> HistogramStats {
        HistogramStats::from_histogram(&self.packet_latency)
    }

    /// Returns task latency statistics.
    pub fn get_task_stats(&self) -> HistogramStats {
        HistogramStats::from_histogram(&self.task_latency)
    }

    /// Returns e2e latency statistics.
    pub fn get_e2e_stats(&self) -> HistogramStats {
        HistogramStats::from_histogram(&self.e2e_latency)
    }

    /// Returns queue wait statistics.
    pub fn get_queue_wait_stats(&self) -> HistogramStats {
        HistogramStats::from_histogram(&self.queue_wait_time)
    }
}

/// Statistics extracted from a histogram.
#[derive(Debug, Clone)]
pub struct HistogramStats {
    pub count: u64,
    pub sum: f64,
}

impl HistogramStats {
    fn from_histogram(h: &Histogram) -> Self {
        let metric = h.get_sample_count();
        Self {
            count: metric,
            sum: h.get_sample_sum(),
        }
    }

    /// Returns average latency if count > 0.
    pub fn average(&self) -> Option<f64> {
        if self.count > 0 {
            Some(self.sum / self.count as f64)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_histogram_creation() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry);
        assert!(histograms.is_ok());
    }

    #[test]
    fn test_observe_receipt() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_receipt(0.1);
        histograms.observe_receipt(0.2);
        histograms.observe_receipt(0.15);

        let stats = histograms.get_receipt_stats();
        assert_eq!(stats.count, 3);
        assert!((stats.sum - 0.45).abs() < 1e-10);
    }

    #[test]
    fn test_observe_packet() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_packet(0.05);

        let stats = histograms.get_packet_stats();
        assert_eq!(stats.count, 1);
        assert!((stats.sum - 0.05).abs() < 1e-10);
    }

    #[test]
    fn test_observe_task() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_task(1.0);
        histograms.observe_task(2.0);

        let stats = histograms.get_task_stats();
        assert_eq!(stats.count, 2);
        assert!((stats.sum - 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_observe_e2e() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_e2e(5.0);

        let stats = histograms.get_e2e_stats();
        assert_eq!(stats.count, 1);
        assert!((stats.sum - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_observe_queue_wait() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_queue_wait(0.01);

        let stats = histograms.get_queue_wait_stats();
        assert_eq!(stats.count, 1);
        assert!((stats.sum - 0.01).abs() < 1e-10);
    }

    #[test]
    fn test_stats_average() {
        let stats = HistogramStats {
            count: 4,
            sum: 10.0,
        };

        assert_eq!(stats.average(), Some(2.5));

        let empty_stats = HistogramStats { count: 0, sum: 0.0 };

        assert_eq!(empty_stats.average(), None);
    }

    #[test]
    fn test_multiple_observations() {
        let registry = Registry::new();
        let histograms = Histograms::new(&registry).unwrap();

        histograms.observe_receipt(0.1);
        histograms.observe_packet(0.2);
        histograms.observe_task(0.3);
        histograms.observe_e2e(0.6);
        histograms.observe_queue_wait(0.05);

        assert_eq!(histograms.get_receipt_stats().count, 1);
        assert_eq!(histograms.get_packet_stats().count, 1);
        assert_eq!(histograms.get_task_stats().count, 1);
        assert_eq!(histograms.get_e2e_stats().count, 1);
        assert_eq!(histograms.get_queue_wait_stats().count, 1);
    }
}
