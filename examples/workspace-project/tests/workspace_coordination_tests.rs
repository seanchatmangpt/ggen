use workspace_project::*;
use coordination::*;

#[test]
fn test_build_coordinator_simple_chain() {
    let crates = vec![
        Crate::new("core".to_string(), "crates/core".to_string()),
        Crate::new("cli".to_string(), "crates/cli".to_string())
            .with_dependencies(vec!["core".to_string()]),
    ];

    let plan = BuildCoordinator::coordinate_builds(&crates, &["core".to_string(), "cli".to_string()]);
    assert_eq!(plan.steps.len(), 2);
    assert_eq!(plan.steps[0].crate_name, "core");
    assert_eq!(plan.steps[1].crate_name, "cli");
}

#[test]
fn test_build_coordinator_parallel_independence() {
    let crates = vec![
        Crate::new("core".to_string(), "crates/core".to_string()),
        Crate::new("utils".to_string(), "crates/utils".to_string()),
        Crate::new("web".to_string(), "crates/web".to_string()),
    ];

    let plan = BuildCoordinator::coordinate_builds(
        &crates,
        &["core".to_string(), "utils".to_string(), "web".to_string()],
    );

    assert_eq!(plan.steps.len(), 3);
    // Verify core is first
    assert_eq!(plan.steps[0].crate_name, "core");
}

#[test]
fn test_build_coordination_plan_parallel_groups() {
    let mut plan = BuildCoordinationPlan::new();

    for i in 0..4 {
        plan.add_build_step(BuildStep {
            crate_name: format!("crate{}", i),
            dependencies: vec![],
            parallel_compatible: true,
        });
    }

    plan.compute_parallel_groups();
    assert!(!plan.parallel_groups.is_empty());
}

#[test]
fn test_build_step_with_dependencies() {
    let step = BuildStep {
        crate_name: "cli".to_string(),
        dependencies: vec!["core".to_string(), "utils".to_string()],
        parallel_compatible: false,
    };

    assert_eq!(step.dependencies.len(), 2);
    assert!(!step.parallel_compatible);
}

#[test]
fn test_test_aggregator_flaky_test_detection_high_threshold() {
    let results = vec![
        TestResult::new("core".to_string(), 95, 5, 0, 2000, 90.0),
        TestResult::new("cli".to_string(), 50, 0, 0, 1500, 80.0),
    ];

    let flaky = TestAggregator::detect_flaky_tests(&results, 0.2);
    assert_eq!(flaky.len(), 1);
    assert_eq!(flaky[0].failure_rate, 0.05);
}

#[test]
fn test_test_aggregator_no_flaky_tests() {
    let results = vec![
        TestResult::new("core".to_string(), 100, 0, 0, 2000, 90.0),
        TestResult::new("cli".to_string(), 50, 0, 0, 1500, 80.0),
    ];

    let flaky = TestAggregator::detect_flaky_tests(&results, 0.01);
    assert!(flaky.is_empty());
}

#[test]
fn test_aggregated_test_report_empty() {
    let report = TestAggregator::generate_report(&[], &[]);
    assert_eq!(report.total_crates, 0);
    assert_eq!(report.total_tests, 0);
    assert_eq!(report.success_rate, 100.0);
}

#[test]
fn test_aggregated_test_report_comprehensive() {
    let test_results = vec![
        TestResult::new("core".to_string(), 50, 0, 2, 2000, 92.0),
        TestResult::new("cli".to_string(), 30, 1, 1, 1500, 85.0),
        TestResult::new("web".to_string(), 40, 0, 0, 1800, 88.0),
    ];

    let build_results = vec![
        BuildResult::success("core".to_string(), 1000),
        BuildResult::success("cli".to_string(), 1200),
        BuildResult::success("web".to_string(), 800),
    ];

    let report = TestAggregator::generate_report(&test_results, &build_results);
    assert_eq!(report.total_crates, 3);
    assert_eq!(report.total_tests, 123); // 50+30+40 + 2+1+0 (skipped counts)
    assert_eq!(report.total_failed, 1);
    assert!(report.success_rate < 100.0);
    assert!(report.average_coverage > 85.0);
}

#[test]
fn test_coverage_gap_all_above_target() {
    let results = vec![
        TestResult::new("core".to_string(), 30, 0, 0, 2000, 95.0),
        TestResult::new("cli".to_string(), 20, 0, 0, 1500, 90.0),
    ];

    let gaps = TestAggregator::identify_coverage_gaps(&results, 85.0);
    assert!(gaps.is_empty());
}

#[test]
fn test_coverage_gap_multiple_below_target() {
    let results = vec![
        TestResult::new("core".to_string(), 30, 0, 0, 2000, 60.0),
        TestResult::new("cli".to_string(), 20, 0, 0, 1500, 70.0),
        TestResult::new("web".to_string(), 25, 0, 0, 1600, 50.0),
    ];

    let gaps = TestAggregator::identify_coverage_gaps(&results, 85.0);
    assert_eq!(gaps.len(), 3);
    // Should be sorted by gap size
    assert!(gaps[0].gap_percentage >= gaps[1].gap_percentage);
}

#[test]
fn test_coverage_gap_verification() {
    let results = vec![TestResult::new("test-crate".to_string(), 20, 0, 0, 1500, 60.0)];

    let gaps = TestAggregator::identify_coverage_gaps(&results, 85.0);
    assert_eq!(gaps.len(), 1);
    assert_eq!(gaps[0].crate_name, "test-crate");
    assert_eq!(gaps[0].current_coverage, 60.0);
    assert_eq!(gaps[0].target_coverage, 85.0);
    assert_eq!(gaps[0].gap_percentage, 25.0);
}

#[test]
fn test_metrics_tracker_creation() {
    let tracker = MetricsTracker::new();
    let history = tracker.get_history();
    assert!(history.is_empty());
}

#[test]
fn test_metrics_tracker_single_snapshot() {
    let mut tracker = MetricsTracker::new();
    tracker.record_snapshot(MetricsSnapshot {
        timestamp: std::time::Instant::now(),
        success_rate: 95.0,
        average_coverage: 85.0,
        total_duration_ms: 5000,
    });

    let history = tracker.get_history();
    assert_eq!(history.len(), 1);
}

#[test]
fn test_metrics_tracker_trend_stable() {
    let mut tracker = MetricsTracker::new();

    for _ in 0..3 {
        tracker.record_snapshot(MetricsSnapshot {
            timestamp: std::time::Instant::now(),
            success_rate: 90.0,
            average_coverage: 80.0,
            total_duration_ms: 5000,
        });
    }

    assert!(matches!(tracker.calculate_trend(), MetricsTrend::Stable));
}

#[test]
fn test_metrics_tracker_trend_insufficient_data() {
    let tracker = MetricsTracker::new();
    assert!(matches!(tracker.calculate_trend(), MetricsTrend::Stable));
}

#[test]
fn test_flaky_test_report_structure() {
    let report = FlakyTestReport {
        crate_name: "test".to_string(),
        failure_rate: 0.1,
        failed_count: 2,
        total_count: 20,
    };

    assert_eq!(report.failure_rate, 0.1);
    assert_eq!(report.failed_count, 2);
}

#[test]
fn test_coordinator_build_order_complex_dependencies() {
    let mut coord = Coordinator::new();

    let core = Crate::new("core".to_string(), "crates/core".to_string());
    let utils = Crate::new("utils".to_string(), "crates/utils".to_string())
        .with_dependencies(vec!["core".to_string()]);
    let cli = Crate::new("cli".to_string(), "crates/cli".to_string())
        .with_dependencies(vec!["core".to_string(), "utils".to_string()]);

    coord.add_crate(core);
    coord.add_crate(utils);
    coord.add_crate(cli);

    let order = coord.determine_build_order().unwrap();
    assert_eq!(order.len(), 3);

    // Verify dependency order
    let core_idx = order.iter().position(|x| x == "core").unwrap();
    let utils_idx = order.iter().position(|x| x == "utils").unwrap();
    let cli_idx = order.iter().position(|x| x == "cli").unwrap();

    assert!(core_idx < utils_idx);
    assert!(core_idx < cli_idx);
    assert!(utils_idx < cli_idx);
}

#[test]
fn test_test_result_serialization() {
    let result = TestResult::new("test".to_string(), 50, 2, 1, 3000, 85.5);
    let json = serde_json::to_string(&result).unwrap();
    let deserialized: TestResult = serde_json::from_str(&json).unwrap();

    assert_eq!(result.crate_name, deserialized.crate_name);
    assert_eq!(result.passed, deserialized.passed);
    assert_eq!(result.coverage_percentage, deserialized.coverage_percentage);
}

#[test]
fn test_build_result_serialization() {
    let result = BuildResult::success("test".to_string(), 1500);
    let json = serde_json::to_string(&result).unwrap();
    let deserialized: BuildResult = serde_json::from_str(&json).unwrap();

    assert_eq!(result.crate_name, deserialized.crate_name);
    assert_eq!(result.success, deserialized.success);
}

#[test]
fn test_workspace_metrics_zero_division_protection() {
    let metrics = WorkspaceMetrics::new();
    assert_eq!(metrics.success_rate(), 0.0); // No tests
}

#[test]
fn test_aggregated_report_duration_accumulation() {
    let test_results = vec![
        TestResult::new("core".to_string(), 30, 0, 0, 2000, 90.0),
        TestResult::new("cli".to_string(), 20, 0, 0, 3000, 80.0),
    ];

    let build_results = vec![
        BuildResult::success("core".to_string(), 1000),
        BuildResult::success("cli".to_string(), 1500),
    ];

    let report = TestAggregator::generate_report(&test_results, &build_results);
    assert_eq!(report.test_duration_ms, 5000);
    assert_eq!(report.build_duration_ms, 2500);
}

#[test]
fn test_build_coordination_empty_crates() {
    let plan = BuildCoordinator::coordinate_builds(&[], &[]);
    assert!(plan.steps.is_empty());
}

#[test]
fn test_metrics_tracker_max_history_limit() {
    let mut tracker = MetricsTracker::new();

    // Record 150 snapshots (exceeds limit of 100)
    for i in 0..150 {
        tracker.record_snapshot(MetricsSnapshot {
            timestamp: std::time::Instant::now(),
            success_rate: 50.0 + (i as f32),
            average_coverage: 50.0 + (i as f32),
            total_duration_ms: 5000,
        });
    }

    let history = tracker.get_history();
    assert_eq!(history.len(), 100); // Should be capped at 100
}

#[test]
fn test_flaky_test_sorting_by_failure_rate() {
    let results = vec![
        TestResult::new("a".to_string(), 98, 2, 0, 2000, 90.0),  // 2%
        TestResult::new("b".to_string(), 95, 5, 0, 2000, 85.0),  // 5%
        TestResult::new("c".to_string(), 90, 10, 0, 2000, 80.0), // 10%
    ];

    let flaky = TestAggregator::detect_flaky_tests(&results, 0.2);
    assert_eq!(flaky.len(), 3);

    // Should be sorted descending by failure rate
    for i in 0..flaky.len() - 1 {
        assert!(flaky[i].failure_rate >= flaky[i + 1].failure_rate);
    }
}
