//! Prometheus exporter for metrics.
//!
//! Provides HTTP endpoint for Prometheus scraping and
//! text format rendering of metrics.

use crate::{MetricsCollector, Result};
use prometheus::{Encoder, TextEncoder};

/// Prometheus metrics exporter.
///
/// Renders metrics in Prometheus text format for HTTP scraping.
pub struct Exporter {
    collector: MetricsCollector,
    encoder: TextEncoder,
}

impl Exporter {
    /// Creates a new exporter for the given metrics collector.
    pub fn new(collector: MetricsCollector) -> Self {
        Self {
            collector,
            encoder: TextEncoder::new(),
        }
    }

    /// Renders all metrics in Prometheus text format.
    pub fn render(&self) -> Result<String> {
        let metric_families = self.collector.gather();
        let mut buffer = Vec::new();

        self.encoder
            .encode(&metric_families, &mut buffer)
            .map_err(|e| crate::MetricsError::Export(e.to_string()))?;

        String::from_utf8(buffer).map_err(|e| crate::MetricsError::Export(e.to_string()))
    }

    /// Returns the content type for Prometheus metrics.
    pub fn content_type(&self) -> &'static str {
        "text/plain; version=0.0.4"
    }

    /// Returns a reference to the metrics collector.
    pub fn collector(&self) -> &MetricsCollector {
        &self.collector
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exporter_creation() {
        let collector = MetricsCollector::new().unwrap();
        let exporter = Exporter::new(collector);

        assert_eq!(exporter.content_type(), "text/plain; version=0.0.4");
    }

    #[test]
    fn test_render_empty_metrics() {
        let collector = MetricsCollector::new().unwrap();
        let exporter = Exporter::new(collector);

        let output = exporter.render();
        assert!(output.is_ok());

        let text = output.unwrap();
        assert!(!text.is_empty());
    }

    #[test]
    fn test_render_with_metrics() {
        let collector = MetricsCollector::new().unwrap();

        collector.record_receipt(0.1).unwrap();
        collector.record_packet(0.2).unwrap();
        collector.set_queue_depth(5).unwrap();
        collector.set_utilization(0.75).unwrap();

        let exporter = Exporter::new(collector);
        let output = exporter.render().unwrap();

        assert!(output.contains("ggen_tps_receipts_total"));
        assert!(output.contains("ggen_tps_packets_total"));
        assert!(output.contains("ggen_tps_queue_depth"));
        assert!(output.contains("ggen_tps_utilization"));
    }

    #[test]
    fn test_render_format() {
        let collector = MetricsCollector::new().unwrap();
        collector.counters.inc_receipts();

        let exporter = Exporter::new(collector);
        let output = exporter.render().unwrap();

        // Should contain HELP and TYPE annotations
        assert!(output.contains("# HELP"));
        assert!(output.contains("# TYPE"));

        // Should contain metric name and value
        assert!(output.contains("ggen_tps_receipts_total"));
    }

    #[test]
    fn test_collector_reference() {
        let collector = MetricsCollector::new().unwrap();
        collector.set_queue_depth(10).unwrap();

        let exporter = Exporter::new(collector);

        assert_eq!(exporter.collector().gauges.get_queue_depth(), 10);
    }

    #[test]
    fn test_multiple_renders() {
        let collector = MetricsCollector::new().unwrap();
        let exporter = Exporter::new(collector);

        let output1 = exporter.render().unwrap();
        exporter.collector().counters.inc_receipts();
        let output2 = exporter.render().unwrap();

        assert!(!output1.is_empty());
        assert!(!output2.is_empty());
        assert_ne!(output1, output2);
    }
}
