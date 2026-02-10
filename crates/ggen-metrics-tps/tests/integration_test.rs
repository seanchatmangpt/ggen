//! Integration tests for ggen-metrics-tps.

use ggen_metrics_tps::{Exporter, MetricsCollector};

#[test]
fn test_full_metrics_pipeline() {
    let collector = MetricsCollector::new().unwrap();

    // Record various operations
    collector.record_receipt(0.1).unwrap();
    collector.record_receipt(0.15).unwrap();
    collector.record_packet(0.05).unwrap();
    collector.record_task(1.0).unwrap();

    // Update gauges
    collector.set_queue_depth(10).unwrap();
    collector.set_token_count(5000).unwrap();
    collector.set_utilization(0.8).unwrap();
    collector.set_active_agents(3).unwrap();

    // Verify counters
    assert_eq!(collector.counters.get_receipts(), 2);
    assert_eq!(collector.counters.get_packets(), 1);
    assert_eq!(collector.counters.get_tasks(), 1);

    // Verify gauges
    assert_eq!(collector.gauges.get_queue_depth(), 10);
    assert_eq!(collector.gauges.get_token_count(), 5000);
    assert!((collector.gauges.get_utilization() - 0.8).abs() < 1e-10);
    assert_eq!(collector.gauges.get_active_agents(), 3);

    // Verify histograms
    let receipt_stats = collector.histograms.get_receipt_stats();
    assert_eq!(receipt_stats.count, 2);
    assert!((receipt_stats.sum - 0.25).abs() < 1e-10);
}

#[test]
fn test_exporter_integration() {
    let collector = MetricsCollector::new().unwrap();

    collector.record_receipt(0.1).unwrap();
    collector.set_queue_depth(5).unwrap();

    let exporter = Exporter::new(collector);
    let output = exporter.render().unwrap();

    assert!(output.contains("ggen_tps_receipts_total"));
    assert!(output.contains("ggen_tps_queue_depth"));
    assert!(output.contains("# HELP"));
    assert!(output.contains("# TYPE"));
}

#[test]
fn test_queueing_theory_metrics() {
    let collector = MetricsCollector::new().unwrap();

    // Simulate arrival rate (λ) tracking
    for _ in 0..10 {
        collector.counters.inc_tasks();
    }
    assert_eq!(collector.counters.get_tasks(), 10);

    // Track utilization (ρ)
    collector.set_utilization(0.75).unwrap();
    assert!((collector.gauges.get_utilization() - 0.75).abs() < 1e-10);

    // Track queue depth (L)
    collector.set_queue_depth(15).unwrap();
    assert_eq!(collector.gauges.get_queue_depth(), 15);

    // Track cycle time (W) via histograms
    collector.histograms.observe_e2e(2.5);
    collector.histograms.observe_e2e(3.0);
    collector.histograms.observe_e2e(2.8);

    let stats = collector.histograms.get_e2e_stats();
    assert_eq!(stats.count, 3);
    assert!((stats.average().unwrap() - 2.766666).abs() < 0.001);
}

#[test]
fn test_concurrent_metric_updates() {
    let collector = MetricsCollector::new().unwrap();

    // Simulate concurrent operations
    for i in 0..100 {
        collector.counters.inc_receipts();
        collector
            .histograms
            .observe_receipt(0.1 + (i as f64) * 0.001);
        collector.gauges.inc_queue_depth();
    }

    assert_eq!(collector.counters.get_receipts(), 100);
    assert_eq!(collector.gauges.get_queue_depth(), 100);

    let stats = collector.histograms.get_receipt_stats();
    assert_eq!(stats.count, 100);
}

#[test]
fn test_error_path_tracking() {
    let collector = MetricsCollector::new().unwrap();

    collector.counters.inc_errors();
    collector.counters.inc_errors();
    collector.counters.inc_errors();

    assert_eq!(collector.counters.get_errors(), 3);
}

#[test]
fn test_throughput_calculation() {
    let collector = MetricsCollector::new().unwrap();

    // Simulate 1 second of operations
    for _ in 0..50 {
        collector.counters.inc_receipts();
        collector.histograms.observe_receipt(0.02);
    }

    let stats = collector.histograms.get_receipt_stats();
    let total_time = stats.sum;
    let throughput = stats.count as f64 / total_time;

    assert_eq!(stats.count, 50);
    assert!(throughput > 0.0);
}

#[test]
fn test_memory_tracking() {
    let collector = MetricsCollector::new().unwrap();

    collector.gauges.set_memory_bytes(104857600); // 100MB
    assert_eq!(collector.gauges.get_memory_bytes(), 104857600);

    collector.gauges.set_memory_bytes(52428800); // 50MB
    assert_eq!(collector.gauges.get_memory_bytes(), 52428800);
}

#[test]
fn test_connection_tracking() {
    let collector = MetricsCollector::new().unwrap();

    assert_eq!(collector.gauges.get_connections(), 0);

    collector.gauges.inc_connections();
    collector.gauges.inc_connections();
    collector.gauges.inc_connections();
    assert_eq!(collector.gauges.get_connections(), 3);

    collector.gauges.dec_connections();
    assert_eq!(collector.gauges.get_connections(), 2);
}

#[test]
fn test_bytes_processed() {
    let collector = MetricsCollector::new().unwrap();

    collector.counters.add_bytes(1024);
    collector.counters.add_bytes(2048);
    collector.counters.add_bytes(4096);

    assert_eq!(collector.counters.get_bytes(), 7168);
}

#[test]
fn test_invalid_utilization() {
    let collector = MetricsCollector::new().unwrap();

    assert!(collector.set_utilization(-0.1).is_err());
    assert!(collector.set_utilization(1.5).is_err());
    assert!(collector.set_utilization(0.0).is_ok());
    assert!(collector.set_utilization(0.5).is_ok());
    assert!(collector.set_utilization(1.0).is_ok());
}

#[test]
fn test_histogram_buckets() {
    let collector = MetricsCollector::new().unwrap();

    // Test various latencies across bucket ranges
    collector.histograms.observe_receipt(0.001); // Sub-ms
    collector.histograms.observe_receipt(0.01); // 10ms
    collector.histograms.observe_receipt(0.1); // 100ms
    collector.histograms.observe_receipt(1.0); // 1s
    collector.histograms.observe_receipt(10.0); // 10s

    let stats = collector.histograms.get_receipt_stats();
    assert_eq!(stats.count, 5);
}

#[test]
fn test_queue_wait_time() {
    let collector = MetricsCollector::new().unwrap();

    collector.histograms.observe_queue_wait(0.05);
    collector.histograms.observe_queue_wait(0.1);
    collector.histograms.observe_queue_wait(0.15);

    let stats = collector.histograms.get_queue_wait_stats();
    assert_eq!(stats.count, 3);
    assert!((stats.sum - 0.3).abs() < 1e-10);
}
