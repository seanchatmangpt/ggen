//! Integration tests for improvement history tracking.

use ggen_kaizen::{
    history::*,
    metrics::MetricsSnapshot,
    pdca::PdcaCycle,
    Category, Improvement, Priority,
};
use chrono::{Duration, Utc};

fn create_test_improvement(id: &str, title: &str) -> Improvement {
    Improvement::new(
        id.to_string(),
        title.to_string(),
        format!("Test description for {}", title),
        Category::Performance,
        Priority::High,
        "test@example.com".to_string(),
    )
}

#[test]
fn test_improvement_record_lifecycle() {
    // Arrange
    let improvement = create_test_improvement("IMP-001", "Test Improvement");

    // Act
    let mut record = ImprovementRecord::new(improvement.clone());
    let cycle = PdcaCycle::new(improvement, 1);
    let snapshot = MetricsSnapshot::new().with_cycle_time(10.0).unwrap();

    record.add_cycle(cycle);
    record.add_metrics_snapshot(snapshot);

    // Assert
    assert_eq!(record.improvement.id, "IMP-001");
    assert_eq!(record.cycles.len(), 1);
    assert_eq!(record.metrics_snapshots.len(), 1);
    assert!(record.updated_at >= record.created_at);
}

#[test]
fn test_improvement_record_multiple_cycles() {
    // Arrange
    let improvement = create_test_improvement("IMP-002", "Multi-cycle Improvement");
    let mut record = ImprovementRecord::new(improvement.clone());

    // Act - Add multiple cycles
    for i in 1..=3 {
        let mut cycle = PdcaCycle::new(improvement.clone(), i);
        cycle.advance("Do".to_string()).ok();
        cycle.advance("Check".to_string()).ok();
        cycle.advance("Act".to_string()).ok();
        if i < 3 {
            cycle.complete().ok(); // Complete first two cycles
        }
        record.add_cycle(cycle);
    }

    // Assert
    assert_eq!(record.cycles.len(), 3);
    assert_eq!(record.completed_cycles_count(), 2);
    assert_eq!(record.latest_cycle().unwrap().cycle_number, 3);
}

#[test]
fn test_improvement_record_average_cycle_duration() {
    // Arrange
    let improvement = create_test_improvement("IMP-003", "Duration Test");
    let mut record = ImprovementRecord::new(improvement.clone());

    // Act - Add cycles with completion
    for i in 1..=3 {
        let mut cycle = PdcaCycle::new(improvement.clone(), i);
        cycle.advance("Do".to_string()).ok();
        cycle.advance("Check".to_string()).ok();
        cycle.advance("Act".to_string()).ok();
        cycle.complete().ok();
        record.add_cycle(cycle);
    }

    // Assert
    let avg_duration = record.average_cycle_duration();
    assert!(avg_duration.is_some());
    assert!(avg_duration.unwrap() >= Duration::zero());
}

#[test]
fn test_improvement_record_metrics_in_range() {
    // Arrange
    let improvement = create_test_improvement("IMP-004", "Metrics Range Test");
    let mut record = ImprovementRecord::new(improvement);
    let now = Utc::now();

    // Act - Add metrics snapshots
    record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
    record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(12.0).unwrap());

    // Assert
    let past = now - Duration::hours(1);
    let future = now + Duration::hours(1);
    let metrics = record.metrics_in_range(past, future);
    assert_eq!(metrics.len(), 2);
}

#[test]
fn test_trend_analysis_improving() {
    // Arrange & Act
    let analysis = TrendAnalysis::new(-0.5, 10); // Negative slope for lower-is-better metrics

    // Assert
    assert_eq!(analysis.direction, TrendDirection::Declining);
    assert!(analysis.confidence > 0.0);
    assert_eq!(analysis.sample_size, 10);
}

#[test]
fn test_trend_analysis_stable() {
    // Arrange & Act
    let analysis = TrendAnalysis::new(0.005, 10); // Near-zero slope

    // Assert
    assert_eq!(analysis.direction, TrendDirection::Stable);
}

#[test]
fn test_trend_analysis_insufficient_data() {
    // Arrange & Act
    let analysis = TrendAnalysis::new(0.5, 2); // Too few samples

    // Assert
    assert_eq!(analysis.direction, TrendDirection::Unknown);
    assert_eq!(analysis.confidence, 0.0);
}

#[test]
fn test_improvement_history_add_and_retrieve() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement = create_test_improvement("IMP-005", "Test");

    // Act
    let result = history.add_improvement(improvement);

    // Assert
    assert!(result.is_ok());
    assert_eq!(history.total_improvements(), 1);
    assert!(history.get_record("IMP-005").is_some());
    assert_eq!(history.get_record("IMP-005").unwrap().improvement.id, "IMP-005");
}

#[test]
fn test_improvement_history_duplicate_prevention() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement1 = create_test_improvement("IMP-006", "First");
    let improvement2 = create_test_improvement("IMP-006", "Duplicate");

    // Act
    history.add_improvement(improvement1).ok();
    let result = history.add_improvement(improvement2);

    // Assert
    assert!(result.is_err());
    assert_eq!(history.total_improvements(), 1);
}

#[test]
fn test_improvement_history_list_ids() {
    // Arrange
    let mut history = ImprovementHistory::new();

    // Act
    history.add_improvement(create_test_improvement("IMP-007", "First")).ok();
    history.add_improvement(create_test_improvement("IMP-008", "Second")).ok();
    history.add_improvement(create_test_improvement("IMP-009", "Third")).ok();

    let ids = history.list_ids();

    // Assert
    assert_eq!(ids.len(), 3);
    assert!(ids.contains(&&"IMP-007".to_string()));
    assert!(ids.contains(&&"IMP-008".to_string()));
    assert!(ids.contains(&&"IMP-009".to_string()));
}

#[test]
fn test_improvement_history_active_improvements() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement1 = create_test_improvement("IMP-010", "Active");
    let improvement2 = create_test_improvement("IMP-011", "Inactive");

    history.add_improvement(improvement1.clone()).ok();
    history.add_improvement(improvement2.clone()).ok();

    // Act - Add active cycle to first improvement
    let cycle1 = PdcaCycle::new(improvement1, 1);
    if let Some(record) = history.get_record_mut("IMP-010") {
        record.add_cycle(cycle1);
    }

    // Add completed cycle to second improvement
    let mut cycle2 = PdcaCycle::new(improvement2, 1);
    cycle2.advance("Do".to_string()).ok();
    cycle2.advance("Check".to_string()).ok();
    cycle2.advance("Act".to_string()).ok();
    cycle2.complete().ok();
    if let Some(record) = history.get_record_mut("IMP-011") {
        record.add_cycle(cycle2);
    }

    // Assert
    assert_eq!(history.active_improvements(), 1);
}

#[test]
fn test_improvement_history_records_by_date() {
    // Arrange
    let mut history = ImprovementHistory::new();

    // Act - Add improvements (they'll have different timestamps)
    history.add_improvement(create_test_improvement("IMP-012", "First")).ok();
    std::thread::sleep(std::time::Duration::from_millis(10));
    history.add_improvement(create_test_improvement("IMP-013", "Second")).ok();
    std::thread::sleep(std::time::Duration::from_millis(10));
    history.add_improvement(create_test_improvement("IMP-014", "Third")).ok();

    let records = history.records_by_date();

    // Assert
    assert_eq!(records.len(), 3);
    assert!(records[0].created_at <= records[1].created_at);
    assert!(records[1].created_at <= records[2].created_at);
}

#[test]
fn test_improvement_history_trend_analysis() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement = create_test_improvement("IMP-015", "Trend Test");
    history.add_improvement(improvement).ok();

    // Act - Add improving metrics
    if let Some(record) = history.get_record_mut("IMP-015") {
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(15.0).unwrap());
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(12.0).unwrap());
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());
    }

    let trend = history.analyze_improvement_trend("IMP-015", "cycle_time");

    // Assert
    assert!(trend.is_some());
    let analysis = trend.unwrap();
    assert!(analysis.slope < 0.0); // Improving (decreasing)
    assert!(analysis.confidence > 0.0);
    assert_eq!(analysis.sample_size, 4);
}

#[test]
fn test_improvement_history_trend_analysis_custom_metric() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement = create_test_improvement("IMP-016", "Custom Metric Test");
    history.add_improvement(improvement).ok();

    // Act - Add custom metrics
    if let Some(record) = history.get_record_mut("IMP-016") {
        let mut snapshot1 = MetricsSnapshot::new();
        snapshot1.add_custom_metric("response_time".to_string(), 500.0);
        record.add_metrics_snapshot(snapshot1);

        let mut snapshot2 = MetricsSnapshot::new();
        snapshot2.add_custom_metric("response_time".to_string(), 400.0);
        record.add_metrics_snapshot(snapshot2);

        let mut snapshot3 = MetricsSnapshot::new();
        snapshot3.add_custom_metric("response_time".to_string(), 300.0);
        record.add_metrics_snapshot(snapshot3);
    }

    let trend = history.analyze_improvement_trend("IMP-016", "response_time");

    // Assert
    assert!(trend.is_some());
    assert!(trend.unwrap().slope < 0.0); // Improving
}

#[test]
fn test_improvement_history_aggregate_all() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let now = Utc::now();

    // Act - Add multiple improvements with metrics
    for i in 1..=3 {
        let improvement = create_test_improvement(&format!("IMP-{}", i), &format!("Test {}", i));
        history.add_improvement(improvement).ok();

        if let Some(record) = history.get_record_mut(&format!("IMP-{}", i)) {
            record.add_metrics_snapshot(
                MetricsSnapshot::new()
                    .with_cycle_time(10.0 * i as f64)
                    .unwrap(),
            );
        }
    }

    let start = now - Duration::hours(1);
    let end = now + Duration::hours(1);
    let aggregated = history.aggregate_all(start, end).unwrap();

    // Assert
    assert_eq!(aggregated.sample_count, 3);
    assert_eq!(aggregated.avg_cycle_time_secs, Some(20.0)); // (10 + 20 + 30) / 3
}

#[test]
fn test_improvement_history_summary() {
    // Arrange
    let mut history = ImprovementHistory::new();

    // Act - Add improvements with various states
    let improvement1 = create_test_improvement("IMP-017", "Active");
    let improvement2 = create_test_improvement("IMP-018", "Completed");

    history.add_improvement(improvement1.clone()).ok();
    history.add_improvement(improvement2.clone()).ok();

    // Add active cycle
    let cycle1 = PdcaCycle::new(improvement1, 1);
    if let Some(record) = history.get_record_mut("IMP-017") {
        record.add_cycle(cycle1);
    }

    // Add completed cycle
    let mut cycle2 = PdcaCycle::new(improvement2, 1);
    cycle2.advance("Do".to_string()).ok();
    cycle2.advance("Check".to_string()).ok();
    cycle2.advance("Act".to_string()).ok();
    cycle2.complete().ok();
    if let Some(record) = history.get_record_mut("IMP-018") {
        record.add_cycle(cycle2);
    }

    let summary = history.summary();

    // Assert
    assert_eq!(summary.total_improvements, 2);
    assert_eq!(summary.active_improvements, 1);
    assert_eq!(summary.total_cycles, 2);
    assert_eq!(summary.completed_cycles, 1);
}

#[test]
fn test_improvement_history_empty() {
    // Arrange
    let history = ImprovementHistory::new();

    // Act & Assert
    assert_eq!(history.total_improvements(), 0);
    assert_eq!(history.active_improvements(), 0);
    assert!(history.list_ids().is_empty());
    assert!(history.get_record("nonexistent").is_none());
}

#[test]
fn test_improvement_history_update_existing_record() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let improvement = create_test_improvement("IMP-019", "Updatable");
    history.add_improvement(improvement.clone()).ok();

    // Act - Update the record
    if let Some(record) = history.get_record_mut("IMP-019") {
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
        record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

        let cycle = PdcaCycle::new(improvement, 1);
        record.add_cycle(cycle);
    }

    // Assert
    let record = history.get_record("IMP-019").unwrap();
    assert_eq!(record.metrics_snapshots.len(), 2);
    assert_eq!(record.cycles.len(), 1);
}

#[test]
fn test_improvement_record_latest_metrics() {
    // Arrange
    let improvement = create_test_improvement("IMP-020", "Latest Metrics Test");
    let mut record = ImprovementRecord::new(improvement);

    // Act
    record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(15.0).unwrap());
    record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(10.0).unwrap());
    record.add_metrics_snapshot(MetricsSnapshot::new().with_cycle_time(8.0).unwrap());

    // Assert
    let latest = record.latest_metrics();
    assert!(latest.is_some());
    assert_eq!(latest.unwrap().cycle_time_secs, Some(8.0));
}
