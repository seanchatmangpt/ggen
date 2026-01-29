//! Integration tests for benchmarking infrastructure.
//!
//! These tests verify the benchmarking and profiling functionality
//! using Chicago TDD patterns (state-based testing, real collaborators).

use ggen_core::benchmarks::{
    BackendBenchmark, BackendType, BenchmarkConfig, BenchmarkMetrics, MemoryProfile,
    MemoryTracker, ProfileConfig, Profiler,
};
use std::time::Duration;

#[tokio::test]
async fn test_backend_benchmark_single_run() {
    // Arrange
    let config = BenchmarkConfig {
        duration: Duration::from_millis(100),
        warmup_duration: Duration::from_millis(10),
        job_count: 100,
        concurrency: 5,
    };
    let benchmark = BackendBenchmark::new(config);

    // Act
    let result = benchmark.run_single(BackendType::ETS).await;

    // Assert - verify observable state
    assert!(result.is_ok(), "Benchmark should complete successfully");
    let benchmark_result = result.unwrap();
    assert_eq!(benchmark_result.metrics.backend, BackendType::ETS);
    assert!(benchmark_result.metrics.jobs_processed > 0);
    assert!(benchmark_result.metrics.throughput > 0.0);
}

#[tokio::test]
async fn test_backend_benchmark_comparison() {
    // Arrange
    let config = BenchmarkConfig {
        duration: Duration::from_millis(100),
        warmup_duration: Duration::from_millis(10),
        job_count: 100,
        concurrency: 5,
    };
    let mut benchmark = BackendBenchmark::new(config);
    let backends = vec![BackendType::ETS, BackendType::Redis];

    // Act
    let result = benchmark.run_comparison(backends).await;

    // Assert - verify observable state
    assert!(result.is_ok(), "Comparison should complete successfully");
    let report = result.unwrap();
    assert_eq!(report.results.len(), 2);
    assert!(report.results.contains_key(&BackendType::ETS));
    assert!(report.results.contains_key(&BackendType::Redis));
    assert!(!report.chart.is_empty());
    assert!(!report.summary.is_empty());
}

#[test]
fn test_benchmark_metrics_slo_verification() {
    // Arrange - ETS backend with high throughput
    let metrics = BenchmarkMetrics {
        backend: BackendType::ETS,
        jobs_processed: 50000,
        throughput: 50000.0,
        latency_p50: 1.0,
        latency_p95: 5.0,
        latency_p99: 8.0,
        peak_memory_bytes: 1024 * 1024,
        avg_memory_bytes: 512 * 1024,
        duration: Duration::from_secs(1),
    };

    // Act
    let meets_slo = metrics.meets_slo();

    // Assert - verify SLO check behavior
    assert!(meets_slo, "High-performance ETS should meet SLO");
}

#[test]
fn test_benchmark_metrics_slo_failure() {
    // Arrange - ETS backend with low throughput
    let metrics = BenchmarkMetrics {
        backend: BackendType::ETS,
        jobs_processed: 10000,
        throughput: 10000.0, // Below 40k threshold
        latency_p50: 1.0,
        latency_p95: 5.0,
        latency_p99: 8.0,
        peak_memory_bytes: 1024 * 1024,
        avg_memory_bytes: 512 * 1024,
        duration: Duration::from_secs(1),
    };

    // Act
    let meets_slo = metrics.meets_slo();

    // Assert - verify SLO check behavior
    assert!(!meets_slo, "Low-throughput ETS should fail SLO");
}

#[test]
fn test_memory_profile_allocation_tracking() {
    // Arrange
    let mut profile = MemoryProfile::new();

    // Act - record multiple allocations
    profile.record_allocation(1024);
    profile.record_allocation(2048);
    profile.record_allocation(4096);

    // Assert - verify state changes
    assert_eq!(profile.total_allocated, 7168);
    assert_eq!(profile.current_usage, 7168);
    assert_eq!(profile.peak_usage, 7168);
    assert_eq!(profile.allocation_count, 3);
}

#[test]
fn test_memory_profile_deallocation_tracking() {
    // Arrange
    let mut profile = MemoryProfile::new();
    profile.record_allocation(4096);
    profile.record_allocation(2048);

    // Act - record deallocations
    profile.record_deallocation(1024);
    profile.record_deallocation(2048);

    // Assert - verify state changes
    assert_eq!(profile.total_allocated, 6144);
    assert_eq!(profile.total_deallocated, 3072);
    assert_eq!(profile.current_usage, 3072);
    assert_eq!(profile.deallocation_count, 2);
}

#[test]
fn test_memory_profile_peak_usage_tracking() {
    // Arrange
    let mut profile = MemoryProfile::new();

    // Act - allocate and deallocate to test peak tracking
    profile.record_allocation(1024);
    profile.record_allocation(2048); // Peak: 3072
    profile.record_allocation(4096); // Peak: 7168
    profile.record_deallocation(2048); // Current: 5120, Peak remains 7168

    // Assert - verify peak is maintained
    assert_eq!(profile.peak_usage, 7168);
    assert_eq!(profile.current_usage, 5120);
}

#[test]
fn test_memory_profile_efficiency_calculation() {
    // Arrange
    let mut profile = MemoryProfile::new();

    // Act - allocate and deallocate 50% of memory
    profile.record_allocation(2000);
    profile.record_deallocation(1000);

    // Assert - verify efficiency calculation
    assert_eq!(profile.efficiency(), 0.5);
}

#[test]
fn test_memory_tracker_integration() {
    // Arrange
    let mut tracker = MemoryTracker::new();

    // Act - perform tracking operations
    tracker.track_allocation(1024);
    tracker.track_allocation(2048);
    tracker.track_deallocation(512);

    // Assert - verify tracker state
    let profile = tracker.profile();
    assert_eq!(profile.allocation_count, 2);
    assert_eq!(profile.deallocation_count, 1);
    assert_eq!(profile.total_allocated, 3072);
    assert_eq!(profile.total_deallocated, 512);
    assert!(tracker.elapsed() >= Duration::from_secs(0));
}

#[test]
fn test_profiler_memory_tracking_enabled() {
    // Arrange
    let config = ProfileConfig {
        enable_memory_tracking: true,
        ..Default::default()
    };
    let mut profiler = Profiler::new(config);

    // Act
    profiler.track_allocation(1024);
    profiler.track_allocation(2048);

    // Assert - verify tracking is working
    let report_result = profiler.memory_report();
    assert!(report_result.is_ok());
    let report = report_result.unwrap();
    assert!(report.contains("3072")); // Total allocated
}

#[test]
fn test_profiler_memory_tracking_disabled() {
    // Arrange
    let config = ProfileConfig {
        enable_memory_tracking: false,
        ..Default::default()
    };
    let profiler = Profiler::new(config);

    // Act
    let report_result = profiler.memory_report();

    // Assert - verify tracking is disabled
    assert!(report_result.is_err());
}

#[tokio::test]
async fn test_profiler_profile_closure() {
    // Arrange
    let config = ProfileConfig::default();
    let mut profiler = Profiler::new(config);

    // Act - profile an async operation
    let result = profiler
        .profile(async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            Ok::<_, ggen_utils::error::GgenError>(42)
        })
        .await;

    // Assert - verify profiling completed
    assert!(result.is_ok());
    let (value, duration) = result.unwrap();
    assert_eq!(value, 42);
    assert!(duration >= Duration::from_millis(10));
}

#[test]
fn test_comparison_report_json_serialization() {
    // Arrange
    let mut results = std::collections::HashMap::new();
    results.insert(
        BackendType::ETS,
        ggen_core::benchmarks::BenchmarkResult {
            config: BenchmarkConfig::default(),
            metrics: BenchmarkMetrics {
                backend: BackendType::ETS,
                jobs_processed: 1000,
                throughput: 1000.0,
                latency_p50: 1.0,
                latency_p95: 2.0,
                latency_p99: 3.0,
                peak_memory_bytes: 1024,
                avg_memory_bytes: 512,
                duration: Duration::from_secs(1),
            },
            timestamp: chrono::Utc::now(),
            meets_slo: false,
        },
    );

    let report = ggen_core::benchmarks::ComparisonReport {
        results,
        chart: "test".to_string(),
        summary: "test".to_string(),
    };

    // Act
    let json_result = report.to_json();

    // Assert - verify JSON serialization
    assert!(json_result.is_ok());
    let json = json_result.unwrap();
    assert!(json.contains("\"backend\":\"ETS\""));
    assert!(json.contains("\"throughput\":1000.0"));
}

#[test]
fn test_memory_profile_size_histogram() {
    // Arrange
    let mut profile = MemoryProfile::new();

    // Act - allocate various sizes
    profile.record_allocation(100);
    profile.record_allocation(200);
    profile.record_allocation(1024);
    profile.record_allocation(2048);

    // Assert - verify histogram is populated
    assert!(!profile.size_histogram.is_empty());
    assert!(profile.size_histogram.values().sum::<usize>() > 0);
}

#[test]
fn test_benchmark_metrics_percentile_calculation() {
    // Arrange
    let latencies: Vec<Duration> = (1..=100)
        .map(|i| Duration::from_millis(i))
        .collect();
    let memory_samples: Vec<usize> = vec![1000; 100];

    // Act
    let result = BenchmarkMetrics::from_samples(
        BackendType::Redis,
        &latencies,
        &memory_samples,
        Duration::from_secs(1),
    );

    // Assert - verify percentile calculations
    assert!(result.is_ok());
    let metrics = result.unwrap();
    assert!(metrics.latency_p50 > 0.0);
    assert!(metrics.latency_p95 > metrics.latency_p50);
    assert!(metrics.latency_p99 > metrics.latency_p95);
}
