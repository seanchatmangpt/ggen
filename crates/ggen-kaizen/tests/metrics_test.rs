//! Integration tests for metrics calculations.

use ggen_kaizen::metrics::*;
use chrono::{Duration, Utc};

#[test]
fn test_metrics_snapshot_builder_pattern() {
    // Arrange & Act
    let snapshot = MetricsSnapshot::new()
        .with_cycle_time(15.5)
        .unwrap()
        .with_defect_rate(0.02)
        .unwrap()
        .with_throughput(150.0)
        .unwrap()
        .with_quality_score(0.98)
        .unwrap()
        .with_cost_savings(5000.0);

    // Assert
    assert_eq!(snapshot.cycle_time_secs, Some(15.5));
    assert_eq!(snapshot.defect_rate, Some(0.02));
    assert_eq!(snapshot.throughput, Some(150.0));
    assert_eq!(snapshot.quality_score, Some(0.98));
    assert_eq!(snapshot.cost_savings, Some(5000.0));
}

#[test]
fn test_metrics_snapshot_validation() {
    // Arrange & Act & Assert - Negative cycle time
    assert!(MetricsSnapshot::new().with_cycle_time(-5.0).is_err());

    // Arrange & Act & Assert - Negative defect rate
    assert!(MetricsSnapshot::new().with_defect_rate(-0.1).is_err());

    // Arrange & Act & Assert - Negative throughput
    assert!(MetricsSnapshot::new().with_throughput(-10.0).is_err());

    // Arrange & Act & Assert - Invalid quality score
    assert!(MetricsSnapshot::new().with_quality_score(-0.1).is_err());
    assert!(MetricsSnapshot::new().with_quality_score(1.5).is_err());

    // Arrange & Act & Assert - Valid quality score boundaries
    assert!(MetricsSnapshot::new().with_quality_score(0.0).is_ok());
    assert!(MetricsSnapshot::new().with_quality_score(1.0).is_ok());
}

#[test]
fn test_custom_metrics() {
    // Arrange
    let mut snapshot = MetricsSnapshot::new();

    // Act
    snapshot.add_custom_metric("response_time_ms".to_string(), 125.5);
    snapshot.add_custom_metric("memory_usage_mb".to_string(), 512.0);
    snapshot.add_custom_metric("cpu_utilization_pct".to_string(), 67.3);

    // Assert
    assert_eq!(snapshot.get_custom_metric("response_time_ms"), Some(125.5));
    assert_eq!(snapshot.get_custom_metric("memory_usage_mb"), Some(512.0));
    assert_eq!(snapshot.get_custom_metric("cpu_utilization_pct"), Some(67.3));
    assert_eq!(snapshot.get_custom_metric("nonexistent"), None);
}

#[test]
fn test_aggregated_metrics_calculation() {
    // Arrange
    let start = Utc::now() - Duration::hours(1);
    let end = Utc::now();

    let snapshots = vec![
        MetricsSnapshot::new()
            .with_cycle_time(10.0)
            .unwrap()
            .with_defect_rate(0.05)
            .unwrap(),
        MetricsSnapshot::new()
            .with_cycle_time(12.0)
            .unwrap()
            .with_defect_rate(0.03)
            .unwrap(),
        MetricsSnapshot::new()
            .with_cycle_time(8.0)
            .unwrap()
            .with_defect_rate(0.04)
            .unwrap(),
    ];

    // Act
    let aggregated = AggregatedMetrics::from_snapshots(&snapshots, start, end).unwrap();

    // Assert
    assert_eq!(aggregated.sample_count, 3);
    assert_eq!(aggregated.avg_cycle_time_secs, Some(10.0));
    assert_eq!(aggregated.avg_defect_rate, Some(0.04));
}

#[test]
fn test_aggregated_metrics_partial_data() {
    // Arrange - Some snapshots have incomplete data
    let start = Utc::now();
    let end = Utc::now() + Duration::hours(1);

    let snapshots = vec![
        MetricsSnapshot::new().with_cycle_time(10.0).unwrap(),
        MetricsSnapshot::new().with_defect_rate(0.05).unwrap(),
        MetricsSnapshot::new()
            .with_cycle_time(12.0)
            .unwrap()
            .with_defect_rate(0.03)
            .unwrap(),
    ];

    // Act
    let aggregated = AggregatedMetrics::from_snapshots(&snapshots, start, end).unwrap();

    // Assert
    assert_eq!(aggregated.sample_count, 3);
    assert_eq!(aggregated.avg_cycle_time_secs, Some(11.0)); // Average of 10.0 and 12.0
    assert_eq!(aggregated.avg_defect_rate, Some(0.04)); // Average of 0.05 and 0.03
}

#[test]
fn test_aggregated_metrics_cost_savings() {
    // Arrange
    let start = Utc::now();
    let end = Utc::now() + Duration::hours(1);

    let snapshots = vec![
        MetricsSnapshot::new().with_cost_savings(1000.0),
        MetricsSnapshot::new().with_cost_savings(1500.0),
        MetricsSnapshot::new().with_cost_savings(2000.0),
    ];

    // Act
    let aggregated = AggregatedMetrics::from_snapshots(&snapshots, start, end).unwrap();

    // Assert
    assert_eq!(aggregated.total_cost_savings, Some(4500.0)); // Sum, not average
}

#[test]
fn test_calculate_improvement_lower_is_better() {
    // Arrange & Act - Cycle time improvement (lower is better)
    let improvement1 = AggregatedMetrics::calculate_improvement(10.0, 8.0, true);
    let improvement2 = AggregatedMetrics::calculate_improvement(10.0, 12.0, true);

    // Assert
    assert!((improvement1 - 20.0).abs() < f64::EPSILON); // 20% improvement
    assert!((improvement2 + 20.0).abs() < f64::EPSILON); // 20% degradation
}

#[test]
fn test_calculate_improvement_higher_is_better() {
    // Arrange & Act - Throughput improvement (higher is better)
    let improvement1 = AggregatedMetrics::calculate_improvement(100.0, 120.0, false);
    let improvement2 = AggregatedMetrics::calculate_improvement(100.0, 80.0, false);

    // Assert
    assert!((improvement1 - 20.0).abs() < f64::EPSILON); // 20% improvement
    assert!((improvement2 + 20.0).abs() < f64::EPSILON); // 20% degradation
}

#[test]
fn test_metrics_calculator_workflow() {
    // Arrange
    let mut calculator = MetricsCalculator::new();

    // Act - Add multiple snapshots over time
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(15.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(13.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(11.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());

    // Assert
    assert_eq!(calculator.snapshots().len(), 4);
    assert_eq!(
        calculator.latest_snapshot().unwrap().cycle_time_secs,
        Some(10.0)
    );
}

#[test]
fn test_metrics_calculator_cycle_time_trend() {
    // Arrange
    let mut calculator = MetricsCalculator::new();

    // Act - Add improving cycle times
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(15.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(12.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

    let trend = calculator.cycle_time_trend();

    // Assert
    assert!(trend.is_some());
    assert!(trend.unwrap() < 0.0); // Negative slope = improving
}

#[test]
fn test_metrics_calculator_defect_rate_trend() {
    // Arrange
    let mut calculator = MetricsCalculator::new();

    // Act - Add improving defect rates
    calculator.add_snapshot(MetricsSnapshot::new().with_defect_rate(0.10).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_defect_rate(0.08).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_defect_rate(0.05).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_defect_rate(0.03).unwrap());

    let trend = calculator.defect_rate_trend();

    // Assert
    assert!(trend.is_some());
    assert!(trend.unwrap() < 0.0); // Negative slope = improving
}

#[test]
fn test_metrics_calculator_throughput_trend() {
    // Arrange
    let mut calculator = MetricsCalculator::new();

    // Act - Add improving throughput
    calculator.add_snapshot(MetricsSnapshot::new().with_throughput(100.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_throughput(110.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_throughput(125.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_throughput(140.0).unwrap());

    let trend = calculator.throughput_trend();

    // Assert
    assert!(trend.is_some());
    assert!(trend.unwrap() > 0.0); // Positive slope = improving
}

#[test]
fn test_metrics_calculator_quality_score_trend() {
    // Arrange
    let mut calculator = MetricsCalculator::new();

    // Act - Add improving quality scores
    calculator.add_snapshot(MetricsSnapshot::new().with_quality_score(0.85).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_quality_score(0.90).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_quality_score(0.93).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_quality_score(0.96).unwrap());

    let trend = calculator.quality_score_trend();

    // Assert
    assert!(trend.is_some());
    assert!(trend.unwrap() > 0.0); // Positive slope = improving
}

#[test]
fn test_metrics_calculator_insufficient_data_for_trend() {
    // Arrange
    let mut calculator = MetricsCalculator::new();
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());

    // Act
    let trend = calculator.cycle_time_trend();

    // Assert
    assert_eq!(trend, None); // Need at least 2 data points
}

#[test]
fn test_metrics_calculator_snapshots_in_range() {
    // Arrange
    let mut calculator = MetricsCalculator::new();
    let now = Utc::now();

    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(12.0).unwrap());

    // Act
    let past = now - Duration::hours(1);
    let future = now + Duration::hours(1);
    let snapshots = calculator.snapshots_in_range(past, future);

    // Assert
    assert_eq!(snapshots.len(), 2);
}

#[test]
fn test_metrics_calculator_aggregate_period() {
    // Arrange
    let mut calculator = MetricsCalculator::new();
    let now = Utc::now();

    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(12.0).unwrap());
    calculator.add_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

    // Act
    let start = now - Duration::hours(1);
    let end = now + Duration::hours(1);
    let aggregated = calculator.aggregate_period(start, end).unwrap();

    // Assert
    assert_eq!(aggregated.sample_count, 3);
    assert_eq!(aggregated.avg_cycle_time_secs, Some(10.0));
}

#[test]
fn test_metrics_calculator_empty() {
    // Arrange
    let calculator = MetricsCalculator::new();

    // Act & Assert
    assert_eq!(calculator.snapshots().len(), 0);
    assert_eq!(calculator.latest_snapshot(), None);
    assert_eq!(calculator.cycle_time_trend(), None);
}
