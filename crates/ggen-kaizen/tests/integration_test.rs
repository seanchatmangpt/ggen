//! End-to-end integration tests for kaizen tracking.

use ggen_kaizen::{
    history::{ImprovementHistory, TrendDirection},
    metrics::{MetricsCalculator, MetricsSnapshot},
    pdca::PdcaCycle,
    Category, Improvement, Priority,
};
use chrono::{Duration, Utc};

#[test]
fn test_end_to_end_kaizen_workflow() {
    // Arrange - Create improvement
    let improvement = Improvement::new(
        "KAIZEN-001".to_string(),
        "Reduce API response time".to_string(),
        "Optimize database queries to reduce API response time from 500ms to 200ms".to_string(),
        Category::Performance,
        Priority::Critical,
        "backend-team@example.com".to_string(),
    );

    let mut history = ImprovementHistory::new();
    let mut metrics_calculator = MetricsCalculator::new();

    // Act - Add improvement to history
    history.add_improvement(improvement.clone()).unwrap();

    // Act - Start PDCA cycle
    let mut cycle = PdcaCycle::new(improvement, 1);

    // Plan phase
    cycle.advance("Profiled API endpoints and identified slow queries".to_string()).unwrap();
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Identify slow queries".to_string());
        phase.add_objective("Design optimization strategy".to_string());
        phase.add_outcome("Found 5 N+1 query patterns".to_string());
    }

    // Capture baseline metrics
    let baseline = MetricsSnapshot::new()
        .with_cycle_time(0.5)
        .unwrap() // 500ms
        .with_defect_rate(0.02)
        .unwrap()
        .with_throughput(100.0)
        .unwrap();
    metrics_calculator.add_snapshot(baseline.clone());

    // Do phase
    cycle.advance("Implemented query optimizations and caching".to_string()).unwrap();
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_objective("Add database indexes".to_string());
        phase.add_objective("Implement query result caching".to_string());
        phase.add_outcome("Deployed optimizations to staging".to_string());
    }

    // Check phase - Measure improvements
    cycle.advance("Measured performance improvements".to_string()).unwrap();

    let measurement1 = MetricsSnapshot::new()
        .with_cycle_time(0.35)
        .unwrap() // 350ms
        .with_defect_rate(0.02)
        .unwrap()
        .with_throughput(120.0)
        .unwrap();
    metrics_calculator.add_snapshot(measurement1.clone());

    let measurement2 = MetricsSnapshot::new()
        .with_cycle_time(0.25)
        .unwrap() // 250ms
        .with_defect_rate(0.015)
        .unwrap()
        .with_throughput(140.0)
        .unwrap();
    metrics_calculator.add_snapshot(measurement2.clone());

    // Now in Act state after 3 advances - add Act phase objectives and outcomes
    if let Some(phase) = cycle.current_phase_mut() {
        phase.add_outcome("Response time reduced to 250ms".to_string());
        phase.add_outcome("Throughput increased by 40%".to_string());
        phase.add_objective("Deploy to production".to_string());
        phase.add_objective("Update documentation".to_string());
        phase.add_outcome("Successfully deployed".to_string());
    }

    // Complete the cycle (must be in Act state)
    cycle.complete().unwrap();

    // Add cycle to history
    if let Some(record) = history.get_record_mut("KAIZEN-001") {
        record.add_cycle(cycle);
        record.add_metrics_snapshot(baseline);
        record.add_metrics_snapshot(measurement1);
        record.add_metrics_snapshot(measurement2);
    }

    // Assert - Verify cycle completion
    let record = history.get_record("KAIZEN-001").unwrap();
    assert_eq!(record.completed_cycles_count(), 1);
    assert_eq!(record.metrics_snapshots.len(), 3);

    // Assert - Verify metrics trends
    let cycle_time_trend = metrics_calculator.cycle_time_trend();
    assert!(cycle_time_trend.is_some());
    assert!(cycle_time_trend.unwrap() < 0.0); // Improving (decreasing)

    let throughput_trend = metrics_calculator.throughput_trend();
    assert!(throughput_trend.is_some());
    assert!(throughput_trend.unwrap() > 0.0); // Improving (increasing)

    // Assert - Verify trend analysis
    let trend = history.analyze_improvement_trend("KAIZEN-001", "cycle_time");
    assert!(trend.is_some());
    let analysis = trend.unwrap();
    assert_eq!(analysis.direction, TrendDirection::Declining); // Lower is better
    assert!(analysis.confidence > 0.0);
}

#[test]
fn test_multiple_improvements_tracking() {
    // Arrange
    let mut history = ImprovementHistory::new();

    let improvement1 = Improvement::new(
        "KAIZEN-101".to_string(),
        "Reduce build time".to_string(),
        "Optimize build pipeline".to_string(),
        Category::Performance,
        Priority::High,
        "devops@example.com".to_string(),
    );

    let improvement2 = Improvement::new(
        "KAIZEN-102".to_string(),
        "Improve test coverage".to_string(),
        "Add missing unit tests".to_string(),
        Category::Quality,
        Priority::Medium,
        "qa@example.com".to_string(),
    );

    // Act - Add improvements
    history.add_improvement(improvement1.clone()).unwrap();
    history.add_improvement(improvement2.clone()).unwrap();

    // Create and complete cycle for first improvement
    let mut cycle1 = PdcaCycle::new(improvement1, 1);
    cycle1.advance("Do".to_string()).unwrap();
    cycle1.advance("Check".to_string()).unwrap();
    cycle1.advance("Act".to_string()).unwrap();
    cycle1.complete().unwrap();

    // Create active cycle for second improvement
    let mut cycle2 = PdcaCycle::new(improvement2, 1);
    cycle2.advance("Do".to_string()).unwrap();

    // Add cycles to history
    if let Some(record) = history.get_record_mut("KAIZEN-101") {
        record.add_cycle(cycle1);
    }
    if let Some(record) = history.get_record_mut("KAIZEN-102") {
        record.add_cycle(cycle2);
    }

    // Assert
    let summary = history.summary();
    assert_eq!(summary.total_improvements, 2);
    assert_eq!(summary.active_improvements, 1);
    assert_eq!(summary.total_cycles, 2);
    assert_eq!(summary.completed_cycles, 1);
}

#[test]
fn test_iterative_improvement_cycles() {
    // Arrange
    let improvement = Improvement::new(
        "KAIZEN-201".to_string(),
        "Iterative improvement test".to_string(),
        "Test multiple PDCA iterations".to_string(),
        Category::Process,
        Priority::Low,
        "process@example.com".to_string(),
    );

    let mut history = ImprovementHistory::new();
    history.add_improvement(improvement.clone()).unwrap();

    // Act - First iteration
    let mut cycle = PdcaCycle::new(improvement.clone(), 1);
    cycle.advance("First Do".to_string()).unwrap();
    cycle.advance("First Check".to_string()).unwrap();
    cycle.advance("First Act".to_string()).unwrap();
    cycle.iterate("Starting second iteration".to_string()).unwrap();

    // Second iteration
    cycle.advance("Second Do".to_string()).unwrap();
    cycle.advance("Second Check".to_string()).unwrap();
    cycle.advance("Second Act".to_string()).unwrap();
    cycle.complete().unwrap();

    // Add to history
    if let Some(record) = history.get_record_mut("KAIZEN-201") {
        record.add_cycle(cycle);
    }

    // Assert
    let record = history.get_record("KAIZEN-201").unwrap();
    assert_eq!(record.cycles.len(), 1);
    assert_eq!(record.latest_cycle().unwrap().phases.len(), 7); // 2 iterations: Plan, Do, Check, Act, Plan, Do, Check, Act
    assert!(record.latest_cycle().unwrap().is_completed());
}

#[test]
fn test_metrics_aggregation_across_improvements() {
    // Arrange
    let mut history = ImprovementHistory::new();
    let now = Utc::now();

    // Act - Create multiple improvements with metrics
    for i in 1..=5 {
        let improvement = Improvement::new(
            format!("KAIZEN-{}", 300 + i),
            format!("Improvement {}", i),
            "Test description".to_string(),
            Category::Performance,
            Priority::Medium,
            "team@example.com".to_string(),
        );

        history.add_improvement(improvement).unwrap();

        if let Some(record) = history.get_record_mut(&format!("KAIZEN-{}", 300 + i)) {
            // Add improving metrics
            for j in 0..3 {
                let cycle_time = 20.0 - (i as f64) - (j as f64);
                record.add_metrics_snapshot(
                    MetricsSnapshot::new()
                        .with_cycle_time(cycle_time)
                        .unwrap()
                        .with_throughput(100.0 + (i * j) as f64)
                        .unwrap(),
                );
            }
        }
    }

    // Assert - Aggregate all metrics
    let start = now - Duration::hours(1);
    let end = now + Duration::hours(1);
    let aggregated = history.aggregate_all(start, end).unwrap();

    assert_eq!(aggregated.sample_count, 15); // 5 improvements × 3 snapshots
    assert!(aggregated.avg_cycle_time_secs.is_some());
    assert!(aggregated.avg_throughput.is_some());
}

#[test]
fn test_improvement_with_custom_metrics() {
    // Arrange
    let improvement = Improvement::new(
        "KAIZEN-401".to_string(),
        "Custom metrics test".to_string(),
        "Track custom application metrics".to_string(),
        Category::Other("Custom".to_string()),
        Priority::Medium,
        "custom@example.com".to_string(),
    );

    let mut history = ImprovementHistory::new();
    history.add_improvement(improvement.clone()).unwrap();

    let mut cycle = PdcaCycle::new(improvement, 1);

    // Act - Track custom metrics
    cycle.advance("Do".to_string()).unwrap();

    if let Some(record) = history.get_record_mut("KAIZEN-401") {
        for i in 0..5 {
            let mut snapshot = MetricsSnapshot::new();
            snapshot.add_custom_metric("memory_usage_mb".to_string(), 1024.0 - (i as f64 * 100.0));
            snapshot.add_custom_metric("cpu_percent".to_string(), 80.0 - (i as f64 * 5.0));
            snapshot.add_custom_metric("error_count".to_string(), 50.0 - (i as f64 * 10.0));
            record.add_metrics_snapshot(snapshot);
        }
    }

    // Assert - Verify custom metric trends
    let memory_trend = history.analyze_improvement_trend("KAIZEN-401", "memory_usage_mb");
    assert!(memory_trend.is_some());
    assert!(memory_trend.unwrap().slope < 0.0); // Decreasing (improving)

    let cpu_trend = history.analyze_improvement_trend("KAIZEN-401", "cpu_percent");
    assert!(cpu_trend.is_some());
    assert!(cpu_trend.unwrap().slope < 0.0); // Decreasing (improving)

    let error_trend = history.analyze_improvement_trend("KAIZEN-401", "error_count");
    assert!(error_trend.is_some());
    assert!(error_trend.unwrap().slope < 0.0); // Decreasing (improving)
}

#[test]
fn test_cost_savings_tracking() {
    // Arrange
    let improvement = Improvement::new(
        "KAIZEN-501".to_string(),
        "Cloud cost optimization".to_string(),
        "Reduce monthly cloud infrastructure costs".to_string(),
        Category::Cost,
        Priority::High,
        "finance@example.com".to_string(),
    );

    let mut history = ImprovementHistory::new();
    history.add_improvement(improvement.clone()).unwrap();

    // Act - Track cost savings over time
    if let Some(record) = history.get_record_mut("KAIZEN-501") {
        record.add_metrics_snapshot(
            MetricsSnapshot::new()
                .with_cost_savings(1000.0)
        );
        record.add_metrics_snapshot(
            MetricsSnapshot::new()
                .with_cost_savings(1500.0)
        );
        record.add_metrics_snapshot(
            MetricsSnapshot::new()
                .with_cost_savings(2000.0)
        );
    }

    // Assert - Verify cost savings
    let record = history.get_record("KAIZEN-501").unwrap();
    let total_savings: f64 = record
        .metrics_snapshots
        .iter()
        .filter_map(|s| s.cost_savings)
        .sum();

    assert_eq!(total_savings, 4500.0);
}

#[test]
fn test_quality_improvement_tracking() {
    // Arrange
    let _improvement = Improvement::new(
        "KAIZEN-601".to_string(),
        "Improve code quality".to_string(),
        "Reduce technical debt and improve maintainability".to_string(),
        Category::Quality,
        Priority::High,
        "engineering@example.com".to_string(),
    );

    let mut metrics_calculator = MetricsCalculator::new();

    // Act - Track quality improvements
    for i in 0..5 {
        let quality_score = 0.70 + (i as f64 * 0.05); // 70% to 90%
        let defect_rate = 0.10 - (i as f64 * 0.02); // 10% to 2%

        metrics_calculator.add_snapshot(
            MetricsSnapshot::new()
                .with_quality_score(quality_score)
                .unwrap()
                .with_defect_rate(defect_rate)
                .unwrap(),
        );
    }

    // Assert - Verify quality trends
    let quality_trend = metrics_calculator.quality_score_trend();
    assert!(quality_trend.is_some());
    assert!(quality_trend.unwrap() > 0.0); // Improving (increasing)

    let defect_trend = metrics_calculator.defect_rate_trend();
    assert!(defect_trend.is_some());
    assert!(defect_trend.unwrap() < 0.0); // Improving (decreasing)
}

#[test]
fn test_empty_history_operations() {
    // Arrange
    let history = ImprovementHistory::new();

    // Act & Assert
    assert_eq!(history.total_improvements(), 0);
    assert_eq!(history.active_improvements(), 0);
    assert!(history.list_ids().is_empty());
    assert!(history.get_record("nonexistent").is_none());
    assert!(history.analyze_improvement_trend("nonexistent", "cycle_time").is_none());

    let summary = history.summary();
    assert_eq!(summary.total_improvements, 0);
    assert_eq!(summary.active_improvements, 0);
    assert_eq!(summary.total_cycles, 0);
    assert_eq!(summary.completed_cycles, 0);
}

#[test]
fn test_improvement_priority_workflow() {
    // Arrange
    let mut high_priority = Improvement::new(
        "KAIZEN-701".to_string(),
        "Critical security fix".to_string(),
        "Patch security vulnerability".to_string(),
        Category::Safety,
        Priority::Critical,
        "security@example.com".to_string(),
    );

    let low_priority = Improvement::new(
        "KAIZEN-702".to_string(),
        "Update documentation".to_string(),
        "Improve API documentation".to_string(),
        Category::Process,
        Priority::Low,
        "docs@example.com".to_string(),
    );

    // Act
    high_priority.add_tag("security".to_string());
    high_priority.add_tag("urgent".to_string());

    // Assert
    assert!(high_priority.priority > low_priority.priority);
    assert_eq!(high_priority.tags.len(), 2);
    assert_eq!(high_priority.category, Category::Safety);
}
