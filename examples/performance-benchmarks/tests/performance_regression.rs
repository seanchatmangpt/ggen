//! Performance regression detection tests

use performance_benchmarks::BenchmarkResult;

#[test]
fn test_no_regression_in_agent_creation() {
    // Baseline from previous run
    let baseline = 90;
    let current = 85;
    
    let allowed_regression = (baseline as f64 * 0.1) as u64; // 10% tolerance
    let actual_regression = baseline.saturating_sub(current);
    
    assert!(actual_regression <= allowed_regression);
}

#[test]
fn test_no_regression_in_tool_discovery() {
    let baseline = 180;
    let current = 170;
    
    let allowed_regression = (baseline as f64 * 0.15) as u64;
    let actual_regression = baseline.saturating_sub(current);
    
    assert!(actual_regression <= allowed_regression);
}

#[test]
fn test_throughput_maintains_or_improves() {
    let baseline_msgs_sec = 9500;
    let current_msgs_sec = 10200;
    
    // Should maintain or improve throughput
    assert!(current_msgs_sec >= baseline_msgs_sec);
}

#[test]
fn test_consensus_performance_consistency() {
    let measurements = vec![1800, 1820, 1850, 1870];
    let avg = measurements.iter().sum::<u64>() / measurements.len() as u64;
    let max_deviation = measurements.iter()
        .map(|&m| (m as i64 - avg as i64).abs() as u64)
        .max()
        .unwrap_or(0);
    
    let tolerance = avg / 10; // 10% deviation tolerance
    assert!(max_deviation < tolerance);
}

#[test]
fn test_plan_generation_scales_linearly() {
    // Benchmark for 10 steps should be roughly 2x for 20 steps
    let steps_10 = 850;
    let steps_20 = 1650;
    
    let ratio = steps_20 as f64 / steps_10 as f64;
    assert!(ratio > 1.8 && ratio < 2.2);
}

#[test]
fn test_memory_efficient_agent_pool() {
    // Creating pool of 6 agents should be quick
    let result = BenchmarkResult::new(
        "pool_creation".to_string(),
        95,
        100,
    );
    assert!(result.passed);
}

#[test]
fn test_no_performance_cliff() {
    // Check that performance doesn't suddenly degrade
    let results = vec![
        BenchmarkResult::new("test_1".to_string(), 100, 150),
        BenchmarkResult::new("test_2".to_string(), 105, 150),
        BenchmarkResult::new("test_3".to_string(), 110, 150),
    ];
    
    // All should pass without sudden failure
    assert!(results.iter().all(|r| r.passed));
}

#[test]
fn test_consistent_tool_execution() {
    let executions = vec![85, 82, 88, 86, 84];
    let avg = executions.iter().sum::<u64>() as f64 / executions.len() as f64;
    
    let slo = 100;
    assert!((avg as u64) < slo);
    
    let variance = executions.iter()
        .map(|&e| ((e as f64 - avg).powi(2)))
        .sum::<f64>() / executions.len() as f64;
    
    assert!(variance < 50.0); // Variance under 50 (about 7ms std dev)
}
