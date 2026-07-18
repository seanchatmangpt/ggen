//! Benchmark correctness validation

use performance_benchmarks::BenchmarkResult;

#[test]
fn test_benchmark_result_passes() {
    let result = BenchmarkResult::new(
        "test".to_string(),
        50,
        100,
    );
    assert!(result.passed);
    assert_eq!(result.status(), "PASS");
}

#[test]
fn test_benchmark_result_fails() {
    let result = BenchmarkResult::new(
        "test".to_string(),
        150,
        100,
    );
    assert!(!result.passed);
    assert_eq!(result.status(), "FAIL");
}

#[test]
fn test_benchmark_slack_positive() {
    let result = BenchmarkResult::new(
        "test".to_string(),
        50,
        100,
    );
    assert!(result.slack_percentage() > 0.0);
}

#[test]
fn test_benchmark_slack_negative() {
    let result = BenchmarkResult::new(
        "test".to_string(),
        150,
        100,
    );
    assert!(result.slack_percentage() < 0.0);
}

#[test]
fn test_throughput_measurement() {
    let result = BenchmarkResult::new(
        "throughput".to_string(),
        10,
        20,
    ).with_throughput(10000);
    
    assert!(result.throughput.is_some());
    assert_eq!(result.throughput.unwrap(), 10000);
}

#[test]
fn test_multiple_benchmark_results() {
    let results = vec![
        BenchmarkResult::new("test1".to_string(), 50, 100),
        BenchmarkResult::new("test2".to_string(), 75, 100),
        BenchmarkResult::new("test3".to_string(), 120, 100),
    ];

    let passed = results.iter().filter(|r| r.passed).count();
    assert_eq!(passed, 2);
}

#[test]
fn test_slack_percentage_calculation() {
    let result = BenchmarkResult::new(
        "test".to_string(),
        80,
        100,
    );
    assert_eq!(result.slack_percentage(), 20.0);
}
