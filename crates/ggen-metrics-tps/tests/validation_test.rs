//! Validation tests for metrics correctness.

use ggen_metrics_tps::MetricsCollector;

#[test]
fn test_counter_monotonicity() {
    let collector = MetricsCollector::new().unwrap();

    let mut prev_receipts = 0;
    for _ in 0..10 {
        let curr = collector.counters.inc_receipts();
        assert!(
            curr > prev_receipts,
            "Counters must be monotonically increasing"
        );
        prev_receipts = curr;
    }
}

#[test]
fn test_gauge_bidirectionality() {
    let collector = MetricsCollector::new().unwrap();

    collector.gauges.set_queue_depth(10);
    assert_eq!(collector.gauges.get_queue_depth(), 10);

    collector.gauges.inc_queue_depth();
    assert_eq!(collector.gauges.get_queue_depth(), 11);

    collector.gauges.dec_queue_depth();
    collector.gauges.dec_queue_depth();
    assert_eq!(collector.gauges.get_queue_depth(), 9);

    collector.gauges.set_queue_depth(0);
    assert_eq!(collector.gauges.get_queue_depth(), 0);
}

#[test]
fn test_histogram_accumulation() {
    let collector = MetricsCollector::new().unwrap();

    let observations = vec![0.1, 0.2, 0.15, 0.3, 0.25];
    let expected_sum: f64 = observations.iter().sum();

    for &obs in &observations {
        collector.histograms.observe_task(obs);
    }

    let stats = collector.histograms.get_task_stats();
    assert_eq!(stats.count, observations.len() as u64);
    assert!((stats.sum - expected_sum).abs() < 1e-10);
}

#[test]
fn test_utilization_bounds() {
    let collector = MetricsCollector::new().unwrap();

    // Valid utilization values
    let valid = vec![0.0, 0.25, 0.5, 0.75, 1.0];
    for &rho in &valid {
        assert!(
            collector.set_utilization(rho).is_ok(),
            "Valid utilization {} should succeed",
            rho
        );
    }

    // Invalid utilization values
    let invalid = vec![-0.1, -1.0, 1.1, 2.0, f64::NAN, f64::INFINITY];
    for &rho in &invalid {
        assert!(
            collector.set_utilization(rho).is_err(),
            "Invalid utilization {} should fail",
            rho
        );
    }
}

#[test]
fn test_metrics_independence() {
    let collector = MetricsCollector::new().unwrap();

    // Update one metric type
    collector.counters.inc_receipts();

    // Verify others are unaffected
    assert_eq!(collector.counters.get_packets(), 0);
    assert_eq!(collector.counters.get_tasks(), 0);
    assert_eq!(collector.gauges.get_queue_depth(), 0);
    assert_eq!(collector.histograms.get_packet_stats().count, 0);
}

#[test]
fn test_collector_cloneability() {
    let collector1 = MetricsCollector::new().unwrap();
    collector1.counters.inc_receipts();

    let collector2 = collector1.clone();
    collector2.counters.inc_receipts();

    // Both should see the updates (shared state via Arc)
    assert_eq!(collector1.counters.get_receipts(), 2);
    assert_eq!(collector2.counters.get_receipts(), 2);
}

#[test]
fn test_zero_count_histogram_average() {
    let collector = MetricsCollector::new().unwrap();

    let stats = collector.histograms.get_receipt_stats();
    assert_eq!(stats.count, 0);
    assert_eq!(stats.sum, 0.0);
    assert_eq!(stats.average(), None);
}

#[test]
fn test_large_counter_values() {
    let collector = MetricsCollector::new().unwrap();

    // Simulate high-volume operations
    for _ in 0..10000 {
        collector.counters.inc_receipts();
    }

    assert_eq!(collector.counters.get_receipts(), 10000);
}

#[test]
fn test_negative_gauge_values() {
    let collector = MetricsCollector::new().unwrap();

    collector.gauges.set_queue_depth(5);
    collector.gauges.set_queue_depth(-3);

    assert_eq!(collector.gauges.get_queue_depth(), -3);
}

#[test]
fn test_token_count_arithmetic() {
    let collector = MetricsCollector::new().unwrap();

    collector.gauges.set_token_count(1000);
    assert_eq!(collector.gauges.get_token_count(), 1000);

    collector.gauges.add_tokens(500);
    assert_eq!(collector.gauges.get_token_count(), 1500);

    collector.gauges.sub_tokens(200);
    assert_eq!(collector.gauges.get_token_count(), 1300);

    collector.gauges.sub_tokens(1300);
    assert_eq!(collector.gauges.get_token_count(), 0);
}

#[test]
fn test_registry_isolation() {
    let collector1 = MetricsCollector::new().unwrap();
    let collector2 = MetricsCollector::new().unwrap();

    collector1.counters.inc_receipts();

    // Separate registries should have independent metrics
    assert_eq!(collector1.counters.get_receipts(), 1);
    assert_eq!(collector2.counters.get_receipts(), 0);
}

#[test]
fn test_all_histogram_types() {
    let collector = MetricsCollector::new().unwrap();

    collector.histograms.observe_receipt(0.1);
    collector.histograms.observe_packet(0.2);
    collector.histograms.observe_task(0.3);
    collector.histograms.observe_e2e(0.6);
    collector.histograms.observe_queue_wait(0.05);

    assert_eq!(collector.histograms.get_receipt_stats().count, 1);
    assert_eq!(collector.histograms.get_packet_stats().count, 1);
    assert_eq!(collector.histograms.get_task_stats().count, 1);
    assert_eq!(collector.histograms.get_e2e_stats().count, 1);
    assert_eq!(collector.histograms.get_queue_wait_stats().count, 1);
}

#[test]
fn test_all_gauge_types() {
    let collector = MetricsCollector::new().unwrap();

    collector.gauges.set_queue_depth(10);
    collector.gauges.set_token_count(5000);
    collector.gauges.set_utilization(0.8);
    collector.gauges.set_active_agents(3);
    collector.gauges.set_memory_bytes(104857600);
    collector.gauges.set_connections(20);

    assert_eq!(collector.gauges.get_queue_depth(), 10);
    assert_eq!(collector.gauges.get_token_count(), 5000);
    assert!((collector.gauges.get_utilization() - 0.8).abs() < 1e-10);
    assert_eq!(collector.gauges.get_active_agents(), 3);
    assert_eq!(collector.gauges.get_memory_bytes(), 104857600);
    assert_eq!(collector.gauges.get_connections(), 20);
}

#[test]
fn test_all_counter_types() {
    let collector = MetricsCollector::new().unwrap();

    collector.counters.inc_receipts();
    collector.counters.inc_packets();
    collector.counters.inc_tasks();
    collector.counters.inc_errors();
    collector.counters.add_bytes(1024);

    assert_eq!(collector.counters.get_receipts(), 1);
    assert_eq!(collector.counters.get_packets(), 1);
    assert_eq!(collector.counters.get_tasks(), 1);
    assert_eq!(collector.counters.get_errors(), 1);
    assert_eq!(collector.counters.get_bytes(), 1024);
}
