//! Comprehensive marketplace stress test suite
//!
//! Integrates all stress testing components and provides a unified interface
//! for running comprehensive stress tests on the marketplace CLI.

mod utils;
mod stress;

use anyhow::Result;
use stress::{StressConfig, StressTestRunner};
use std::time::Duration;

/// Run all stress tests with default configuration
#[tokio::test]
#[ignore] // Run with: cargo test --test marketplace_stress_suite -- --ignored
async fn run_full_stress_suite() -> Result<()> {
    println!("\n=== Running Comprehensive Marketplace Stress Test Suite ===\n");

    let config = StressConfig {
        concurrency: 10,
        total_operations: 500,
        timeout: Duration::from_secs(300),
        include_destructive: false,
    };

    let runner = StressTestRunner::new(config.clone())?;

    // Test 1: Concurrent Search Stress
    println!("Test 1/5: Concurrent Search Stress");
    let search_metrics = runner.run_concurrent_search_stress().await?;
    println!("{}", search_metrics.report());
    assert!(search_metrics.operations_completed > 0, "No operations completed in search stress test");

    // Test 2: Rapid Sequential Operations
    println!("\nTest 2/5: Rapid Sequential Operations");
    let sequential_metrics = runner.run_rapid_sequential_stress().await?;
    println!("{}", sequential_metrics.report());
    assert!(sequential_metrics.operations_completed > 0, "No operations completed in sequential stress test");

    // Test 3: Large Dataset Handling
    println!("\nTest 3/5: Large Dataset Handling");
    let dataset_metrics = runner.run_large_dataset_stress().await?;
    println!("{}", dataset_metrics.report());
    assert!(dataset_metrics.operations_completed > 0, "No operations completed in dataset stress test");

    // Test 4: Memory Stress
    println!("\nTest 4/5: Memory Stress");
    let memory_metrics = runner.run_memory_stress().await?;
    println!("{}", memory_metrics.report());
    assert!(memory_metrics.peak_memory_bytes > 0, "No memory usage recorded");

    // Test 5: Filesystem Stress
    println!("\nTest 5/5: Filesystem Stress");
    let fs_metrics = runner.run_filesystem_stress().await?;
    println!("{}", fs_metrics.report());
    assert!(fs_metrics.operations_completed > 0, "No operations completed in filesystem stress test");

    println!("\n=== Stress Test Suite Completed Successfully ===\n");

    Ok(())
}

/// Run performance-focused stress tests
#[tokio::test]
#[ignore]
async fn run_performance_stress_suite() -> Result<()> {
    println!("\n=== Running Performance Stress Test Suite ===\n");

    let config = StressConfig {
        concurrency: 20,
        total_operations: 1000,
        timeout: Duration::from_secs(600),
        include_destructive: false,
    };

    let runner = StressTestRunner::new(config)?;

    println!("High Concurrency Search Test (20 concurrent operations)");
    let metrics = runner.run_concurrent_search_stress().await?;
    println!("{}", metrics.report());

    // Performance assertions
    assert!(metrics.throughput > 1.0, "Throughput too low: {} ops/sec", metrics.throughput);
    assert!(metrics.avg_latency_ms < 1000.0, "Average latency too high: {}ms", metrics.avg_latency_ms);

    println!("\n=== Performance Stress Test Completed ===\n");

    Ok(())
}

/// Run edge case stress tests
#[tokio::test]
#[ignore]
async fn run_edge_case_stress_suite() -> Result<()> {
    println!("\n=== Running Edge Case Stress Test Suite ===\n");

    let config = StressConfig {
        concurrency: 5,
        total_operations: 200,
        timeout: Duration::from_secs(120),
        include_destructive: false,
    };

    let runner = StressTestRunner::new(config)?;

    println!("Testing with edge case inputs");
    let metrics = runner.run_filesystem_stress().await?;
    println!("{}", metrics.report());

    // Edge cases should be handled gracefully
    let success_rate = metrics.operations_completed as f64
        / (metrics.operations_completed + metrics.operations_failed).max(1) as f64;

    assert!(success_rate > 0.5, "Success rate too low for edge cases: {:.2}%", success_rate * 100.0);

    println!("\n=== Edge Case Stress Test Completed ===\n");

    Ok(())
}

/// Run quick smoke test for CI
#[tokio::test]
async fn smoke_test_stress_infrastructure() -> Result<()> {
    let config = StressConfig {
        concurrency: 2,
        total_operations: 10,
        timeout: Duration::from_secs(30),
        include_destructive: false,
    };

    let runner = StressTestRunner::new(config)?;
    let metrics = runner.run_concurrent_search_stress().await?;

    assert!(metrics.operations_completed > 0, "Smoke test failed - no operations completed");

    Ok(())
}
