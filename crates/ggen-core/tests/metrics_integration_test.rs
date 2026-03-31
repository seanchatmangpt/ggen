//! Integration tests for Quality Metrics System
//!
//! Tests the complete metrics collection and reporting system covering:
//! - Code metrics from actual source files
//! - Process metrics from generation pipeline
//! - Six Sigma defect tracking
//! - TPS waste identification
//! - Flow efficiency calculation
//! - OEE calculation
//! - Kaizen improvement tracking

use ggen_core::metrics::*;
use std::fs;
use std::path::PathBuf;
use std::time::{Duration, SystemTime};

#[test]
fn test_code_metrics_from_real_files() {
    let mut collector = MetricsCollector::new();

    // Collect metrics from test files
    let test_files = vec![
        "tests/fixtures/sample_rust_code.rs".to_string(),
        "tests/fixtures/sample_toml.toml".to_string(),
    ];

    // Only process files that exist
    let existing_files: Vec<_> = test_files
        .into_iter()
        .filter(|p| PathBuf::from(p).exists())
        .collect();

    if !existing_files.is_empty() {
        let result = collector.collect_code_metrics(&existing_files);
        assert!(result.is_ok(), "Code metrics collection should succeed");

        let report = collector.report();
        assert!(report.code.total_lines > 0, "Should count total lines");
    }
}

#[test]
fn test_code_quality_score_calculation() {
    let mut metrics = CodeMetrics::new();
    metrics.total_lines = 1000;
    metrics.code_lines = 700;
    metrics.comment_lines = 200;
    metrics.blank_lines = 100;
    metrics.avg_complexity = 4.5;
    metrics.max_complexity = 8;
    metrics.test_coverage = 85.0;
    metrics.public_functions = 20;
    metrics.types_count = 10;
    metrics.unsafe_blocks = 2;
    metrics.unwrap_calls = 0;

    let score = metrics.quality_score();
    assert!(
        score >= 80.0,
        "High coverage and low complexity should score >= 80, got {}",
        score
    );
    assert!(score <= 100.0, "Score should not exceed 100");
}

#[test]
fn test_code_quality_penalty_for_unwrap() {
    let mut metrics = CodeMetrics::new();
    metrics.test_coverage = 90.0;
    metrics.avg_complexity = 3.0;
    metrics.unwrap_calls = 5;

    let score = metrics.quality_score();
    assert!(
        score < 90.0,
        "unwrap calls should reduce quality score, got {}",
        score
    );
}

#[test]
fn test_process_metrics_efficiency() {
    let mut metrics = ProcessMetrics::new();
    metrics.cycle_time = Duration::from_secs(100);
    metrics.development_time = Duration::from_secs(40);
    metrics.wait_time = Duration::from_secs(60);
    metrics.throughput = 2.0;
    metrics.first_time_yield = 95.0;
    metrics.rework_percentage = 5.0;
    metrics.gates_passed = 11;
    metrics.quality_gates = 11;

    let efficiency = metrics.efficiency();
    assert_eq!(
        efficiency, 40.0,
        "40% active time should equal 40% efficiency"
    );

    let velocity = metrics.velocity();
    assert_eq!(velocity, 14.0, "2 features/day * 7 days = 14 features/week");
}

#[test]
fn test_six_sigma_defect_free() {
    let mut metrics = DefectMetrics::new();
    metrics.total_units = 1000;
    metrics.defective_units = 0;
    metrics.total_defects = 0;
    metrics.opportunities_per_unit = 11;
    metrics.total_opportunities = 11000;

    assert_eq!(metrics.dpu(), 0.0, "No defects should give DPU = 0");
    assert_eq!(metrics.dpo(), 0.0, "No defects should give DPO = 0");
    assert_eq!(metrics.dpmo(), 0.0, "No defects should give DPMO = 0");
    assert_eq!(metrics.sigma_level(), 6.0, "Defect-free should be 6 Sigma");
    assert_eq!(
        metrics.yield_percentage(),
        100.0,
        "Defect-free should be 100% yield"
    );
}

#[test]
fn test_six_sigma_with_defects() {
    let mut metrics = DefectMetrics::new();
    metrics.total_units = 1000;
    metrics.defective_units = 50;
    metrics.total_defects = 100;
    metrics.opportunities_per_unit = 11;
    metrics.total_opportunities = 11000;

    let dpu = metrics.dpu();
    assert!(dpu > 0.0, "Should have positive DPU");
    assert_eq!(dpu, 0.1, "100 defects in 1000 units = 0.1 DPU");

    let dpmo = metrics.dpmo();
    assert!(dpmo > 0.0, "Should have positive DPMO");
    assert!(
        (dpmo - 9090.91).abs() < 0.1,
        "DPMO should be ~9090.91, got {}",
        dpmo
    );

    let sigma = metrics.sigma_level();
    assert!(sigma < 6.0, "Defects should reduce sigma level below 6");
    assert!(sigma >= 3.0, "100 defects should still be >= 3 Sigma");

    let yield_pct = metrics.yield_percentage();
    assert_eq!(yield_pct, 95.0, "950 good units out of 1000 = 95% yield");
}

#[test]
fn test_tps_waste_tracking() {
    let mut metrics = WasteMetrics::new();

    metrics.add_waste(WasteType::Defects);
    metrics.add_waste(WasteType::Defects);
    metrics.add_waste(WasteType::Waiting);
    metrics.add_waste(WasteType::Inventory);

    assert_eq!(metrics.total_waste, 4, "Should track 4 waste events");
    assert_eq!(
        *metrics.waste_counts.get(&WasteType::Defects).unwrap(),
        2,
        "Should track 2 defect wastes"
    );

    let most_common = metrics.most_common_waste();
    assert_eq!(
        most_common,
        Some(&WasteType::Defects),
        "Defects should be most common"
    );
}

#[test]
fn test_tps_waste_score() {
    let mut metrics = WasteMetrics::new();
    metrics.waste_reduction = 20.0;

    // Add various wastes
    metrics.add_waste(WasteType::Defects); // -15
    metrics.add_waste(WasteType::Waiting); // -3
    metrics.add_waste(WasteType::Overproduction); // -10

    let score = metrics.waste_score();
    assert_eq!(score, 72.0, "100 - 15 - 3 - 10 = 72");
}

#[test]
fn test_flow_metrics_efficiency() {
    let mut metrics = FlowMetrics::new();
    metrics.lead_time = Duration::from_secs(200);
    metrics.active_time = Duration::from_secs(50);
    metrics.wait_time = Duration::from_secs(100);
    metrics.queue_time = Duration::from_secs(50);
    metrics.wip = 5;
    metrics.batch_size = 1;

    let flow_eff = metrics.flow_efficiency();
    assert_eq!(
        flow_eff, 25.0,
        "50s active / 200s lead = 25% flow efficiency"
    );

    let throughput = metrics.expected_throughput();
    assert_eq!(throughput, 0.025, "5 WIP / 200s = 0.025 units per second");
}

#[test]
fn test_oee_world_class() {
    let mut metrics = OEEMetrics::new();
    metrics.availability = 95.0;
    metrics.performance = 95.0;
    metrics.quality = 95.0;

    metrics.calculate();

    let expected_oee = (95.0 * 95.0 * 95.0) / 10_000.0;
    assert!((metrics.oee - expected_oee).abs() < 0.01);

    assert!(
        metrics.is_world_class(),
        "95/95/95 should be world-class OEE"
    );
}

#[test]
fn test_oee_not_world_class() {
    let mut metrics = OEEMetrics::new();
    metrics.availability = 70.0;
    metrics.performance = 80.0;
    metrics.quality = 85.0;

    metrics.calculate();

    assert!((metrics.oee - 47.6).abs() < 0.01);
    assert!(
        !metrics.is_world_class(),
        "70/80/85 should not be world-class OEE"
    );
}

#[test]
fn test_kaizen_improvement_rate() {
    let mut metrics = KaizenMetrics::new();
    metrics.suggestions = 20;
    metrics.implemented = 15;
    metrics.kaizen_events = 5;
    metrics.days_since_last_kaizen = 7;

    metrics.calculate_improvement_rate();

    assert_eq!(
        metrics.improvement_rate, 75.0,
        "15 implemented / 20 suggestions = 75% rate"
    );
}

#[test]
fn test_metrics_report_overall_score() {
    let mut report = MetricsReport::new();

    // Set up good metrics
    report.code.test_coverage = 85.0;
    report.code.avg_complexity = 4.0;
    report.code.unwrap_calls = 0;

    report.process.development_time = Duration::from_secs(40);
    report.process.cycle_time = Duration::from_secs(100);

    report.defects.total_units = 1000;
    report.defects.total_defects = 10;

    let overall = report.overall_score();
    assert!(
        overall > 50.0,
        "Overall score should be > 50 for good metrics"
    );
    assert!(overall <= 100.0, "Overall score should not exceed 100");
}

#[test]
fn test_metrics_report_markdown_generation() {
    let report = MetricsReport::new();
    let markdown = report.to_markdown();

    assert!(markdown.contains("# Quality Metrics Report"));
    assert!(markdown.contains("## Overall Score"));
    assert!(markdown.contains("## Code Metrics"));
    assert!(markdown.contains("## Process Metrics"));
    assert!(markdown.contains("## Six Sigma Metrics"));
    assert!(markdown.contains("## TPS Waste Metrics"));
    assert!(markdown.contains("## Flow Metrics"));
    assert!(markdown.contains("## OEE Metrics"));
    assert!(markdown.contains("## Kaizen Metrics"));
}

#[test]
fn test_metrics_collector_integration() {
    let mut collector = MetricsCollector::new();

    // Add some test code
    let test_code = r#"
// Sample function
fn calculate(x: i32) -> i32 {
    x * 2
}

// Function with unwrap
fn get_value() -> i32 {
    let val = Some(42);
    val.unwrap()
}
"#;

    collector.analyze_source_file(test_code);

    let report = collector.report();
    assert_eq!(report.code.total_lines, 11);
    assert_eq!(report.code.unwrap_calls, 1);
    assert_eq!(report.code.comment_lines, 2);
}

#[test]
fn test_defect_and_waste_tracking() {
    let mut collector = MetricsCollector::new();

    // Add defects
    collector.add_defect();
    collector.add_defect();

    // Add wastes
    collector.add_waste(WasteType::Defects);
    collector.add_waste(WasteType::Waiting);

    let report = collector.report();
    assert_eq!(report.defects.total_defects, 2);
    assert_eq!(report.waste.total_waste, 2);
}

#[test]
fn test_all_waste_types() {
    let mut metrics = WasteMetrics::new();

    // Add all 9 waste types
    metrics.add_waste(WasteType::Overproduction);
    metrics.add_waste(WasteType::Waiting);
    metrics.add_waste(WasteType::Transportation);
    metrics.add_waste(WasteType::Overprocessing);
    metrics.add_waste(WasteType::Inventory);
    metrics.add_waste(WasteType::Motion);
    metrics.add_waste(WasteType::Defects);
    metrics.add_waste(WasteType::Mura);
    metrics.add_waste(WasteType::Muri);

    assert_eq!(metrics.total_waste, 9);
    assert_eq!(metrics.waste_counts.len(), 9);
}

#[test]
fn test_metrics_collector_into_report() {
    let mut collector = MetricsCollector::new();

    let test_code = "fn main() { println!(\"Hello\"); }";
    collector.analyze_source_file(test_code);

    let report = collector.into_report();
    assert!(report.code.total_lines > 0);
}

#[test]
fn test_code_metrics_default() {
    let metrics = CodeMetrics::new();
    assert_eq!(metrics.total_lines, 0);
    assert_eq!(metrics.test_coverage, 0.0);
    assert_eq!(metrics.unwrap_calls, 0);
}

#[test]
fn test_process_metrics_default() {
    let metrics = ProcessMetrics::new();
    assert_eq!(metrics.cycle_time, Duration::ZERO);
    assert_eq!(metrics.first_time_yield, 100.0);
}

#[test]
fn test_defect_metrics_default() {
    let metrics = DefectMetrics::new();
    assert_eq!(metrics.total_units, 0);
    assert_eq!(metrics.opportunities_per_unit, 11);
}

#[test]
fn test_waste_metrics_default() {
    let metrics = WasteMetrics::new();
    assert_eq!(metrics.total_waste, 0);
    assert!(metrics.waste_counts.is_empty());
}

#[test]
fn test_flow_metrics_default() {
    let metrics = FlowMetrics::new();
    assert_eq!(metrics.lead_time, Duration::ZERO);
    assert_eq!(metrics.wip, 0);
}

#[test]
fn test_oee_metrics_default() {
    let metrics = OEEMetrics::new();
    assert_eq!(metrics.availability, 100.0);
    assert_eq!(metrics.oee, 100.0);
}

#[test]
fn test_kaizen_metrics_default() {
    let metrics = KaizenMetrics::new();
    assert_eq!(metrics.suggestions, 0);
    assert_eq!(metrics.improvement_rate, 0.0);
}

#[test]
fn test_metrics_report_default() {
    let report = MetricsReport::new();
    assert!(report.timestamp <= SystemTime::now());
}

#[test]
fn test_waste_type_equality() {
    assert_eq!(WasteType::Defects, WasteType::Defects);
    assert_ne!(WasteType::Defects, WasteType::Waiting);
}

#[test]
fn test_waste_type_hashable() {
    use std::collections::HashSet;
    let mut set = HashSet::new();
    set.insert(WasteType::Defects);
    set.insert(WasteType::Waiting);
    assert_eq!(set.len(), 2);
}

#[test]
fn test_complexity_penalty() {
    let mut metrics = CodeMetrics::new();
    metrics.test_coverage = 90.0;
    metrics.avg_complexity = 15.0; // Very high complexity
    metrics.unwrap_calls = 0;

    let score = metrics.quality_score();
    assert!(
        score <= 80.0,
        "High complexity should significantly reduce score, got {}",
        score
    );
}

#[test]
fn test_perfect_code_quality() {
    let mut metrics = CodeMetrics::new();
    metrics.test_coverage = 100.0;
    metrics.avg_complexity = 1.0;
    metrics.unwrap_calls = 0;

    let score = metrics.quality_score();
    assert_eq!(score, 100.0, "Perfect metrics should score 100");
}

#[test]
fn test_sigma_level_thresholds() {
    let mut metrics = DefectMetrics::new();
    metrics.total_units = 1_000_000;

    // Test each sigma level - use simple defect-free for 6 sigma
    // Test defect-free gives 6 sigma
    metrics.total_defects = 0;
    assert_eq!(metrics.sigma_level(), 6.0, "Defect-free should be 6 sigma");

    // Test high defects gives lower sigma
    metrics.total_defects = 1_000_000;
    let sigma = metrics.sigma_level();
    assert!(sigma < 6.0, "High defects should reduce sigma below 6");
}

#[test]
fn test_zero_division_protection() {
    // Code metrics with zero lines
    let metrics = CodeMetrics::new();
    let score = metrics.quality_score();
    assert!(score >= 0.0 && score <= 100.0);

    // Process metrics with zero cycle time
    let process = ProcessMetrics::new();
    let efficiency = process.efficiency();
    assert_eq!(efficiency, 0.0);

    // Defect metrics with zero units
    let defects = DefectMetrics::new();
    let dpu = defects.dpu();
    assert_eq!(dpu, 0.0);

    // Flow metrics with zero lead time
    let flow = FlowMetrics::new();
    let flow_eff = flow.flow_efficiency();
    assert_eq!(flow_eff, 0.0);
}

#[test]
fn test_markdown_report_contains_all_sections() {
    let mut report = MetricsReport::new();
    report.code.test_coverage = 85.0;
    report.process.gates_passed = 11;
    report.defects.total_units = 1000;

    let markdown = report.to_markdown();

    // Verify key metrics are present
    assert!(markdown.contains("85.0"), "Should contain test coverage");
    assert!(markdown.contains("11/11"), "Should contain gates passed");
    assert!(
        markdown.contains("# Quality Metrics Report"),
        "Should have header"
    );
    assert!(
        markdown.contains("## Code Metrics"),
        "Should have Code Metrics section"
    );
    assert!(
        markdown.contains("## Six Sigma Metrics"),
        "Should have Six Sigma section"
    );
}

#[test]
fn test_source_file_analysis_counts_unsafe() {
    let mut collector = MetricsCollector::new();

    let code_with_unsafe = r#"
fn safe_function() -> i32 {
    42
}

unsafe fn dangerous_function() -> i32 {
    43
}

fn mixed() {
    unsafe {
        let _ = 1;
    }
}
"#;

    collector.analyze_source_file(code_with_unsafe);

    let report = collector.report();
    // Should count 2 "unsafe" keywords (1 in fn declaration, 1 in block)
    assert_eq!(report.code.unsafe_blocks, 2);
}
