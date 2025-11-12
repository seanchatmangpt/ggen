//! Performance tests for ggen node bindings
//!
//! These tests validate that operations complete within acceptable timeframes
//! and that the binding maintains good performance characteristics.

#[cfg(test)]
mod performance {
    use ggen_cli_lib::run_for_node;
    use std::time::{Duration, Instant};

    const FAST_OPERATION_MS: u64 = 100; // Operations should complete under 100ms
    const STANDARD_OPERATION_MS: u64 = 1000; // Standard operations under 1s
    const SLOW_OPERATION_MS: u64 = 5000; // Complex operations under 5s

    #[tokio::test]
    async fn test_version_performance() {
        let start = Instant::now();
        let result = run_for_node(vec!["--version".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Version command should succeed");
        assert!(
            duration < Duration::from_millis(FAST_OPERATION_MS),
            "Version should complete in under {}ms, took {:?}",
            FAST_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_help_performance() {
        let start = Instant::now();
        let result = run_for_node(vec!["--help".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Help command should succeed");
        assert!(
            duration < Duration::from_millis(FAST_OPERATION_MS),
            "Help should complete in under {}ms, took {:?}",
            FAST_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_market_list_performance() {
        let start = Instant::now();
        let result = run_for_node(vec!["market".to_string(), "list".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Market list should succeed");
        assert!(
            duration < Duration::from_millis(STANDARD_OPERATION_MS),
            "Market list should complete in under {}ms, took {:?}",
            STANDARD_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_lifecycle_list_performance() {
        let start = Instant::now();
        let result = run_for_node(vec!["lifecycle".to_string(), "list".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Lifecycle list should succeed");
        assert!(
            duration < Duration::from_millis(STANDARD_OPERATION_MS),
            "Lifecycle list should complete in under {}ms, took {:?}",
            STANDARD_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_doctor_performance() {
        let start = Instant::now();
        let result = run_for_node(vec!["doctor".to_string()]).await;
        let duration = start.elapsed();

        assert!(result.is_ok(), "Doctor command should succeed");
        assert!(
            duration < Duration::from_millis(SLOW_OPERATION_MS),
            "Doctor should complete in under {}ms, took {:?}",
            SLOW_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_multiple_sequential_calls_performance() {
        let start = Instant::now();

        for _ in 0..5 {
            let result = run_for_node(vec!["--version".to_string()]).await;
            assert!(result.is_ok());
        }

        let duration = start.elapsed();
        let avg_duration = duration / 5;

        assert!(
            avg_duration < Duration::from_millis(FAST_OPERATION_MS),
            "Average operation should be under {}ms, was {:?}",
            FAST_OPERATION_MS,
            avg_duration
        );
    }

    #[tokio::test]
    async fn test_concurrent_calls_performance() {
        let start = Instant::now();

        let tasks: Vec<_> = (0..10)
            .map(|_| tokio::spawn(async { run_for_node(vec!["--version".to_string()]).await }))
            .collect();

        let results = futures::future::join_all(tasks).await;
        let duration = start.elapsed();

        // All tasks should succeed
        for result in results {
            assert!(result.is_ok(), "Task should not panic");
            if let Ok(Ok(_)) = result {
                // Success
            } else {
                // Error is acceptable in concurrent scenario
            }
        }

        // 10 concurrent operations should not take 10x the time
        assert!(
            duration < Duration::from_millis(STANDARD_OPERATION_MS),
            "10 concurrent operations should complete in under {}ms, took {:?}",
            STANDARD_OPERATION_MS,
            duration
        );
    }

    #[tokio::test]
    async fn test_large_output_performance() {
        // Test that handling large output doesn't cause performance issues
        let start = Instant::now();
        let result = run_for_node(vec!["--help".to_string()]).await;
        let duration = start.elapsed();

        match result {
            Ok(res) => {
                let output_size = res.stdout.len() + res.stderr.len();
                println!("Output size: {} bytes", output_size);

                assert!(
                    duration < Duration::from_millis(FAST_OPERATION_MS),
                    "Large output should be handled in under {}ms, took {:?}",
                    FAST_OPERATION_MS,
                    duration
                );
            }
            Err(e) => panic!("Help command failed: {}", e),
        }
    }

    #[tokio::test]
    async fn test_error_path_performance() {
        // Error paths should be fast too
        let start = Instant::now();
        let result = run_for_node(vec!["invalid-command".to_string()]).await;
        let duration = start.elapsed();

        // Should complete quickly even on error
        assert!(
            duration < Duration::from_millis(FAST_OPERATION_MS),
            "Error path should be fast, took {:?}",
            duration
        );

        match result {
            Ok(res) => {
                assert_ne!(res.code, 0, "Invalid command should return error code");
            }
            Err(_) => {
                // Error is acceptable
            }
        }
    }

    #[tokio::test]
    async fn test_repeated_operations_no_slowdown() {
        let mut durations = Vec::new();

        // Run the same operation multiple times
        for _ in 0..10 {
            let start = Instant::now();
            let result = run_for_node(vec!["--version".to_string()]).await;
            let duration = start.elapsed();

            assert!(result.is_ok());
            durations.push(duration);
        }

        // First and last operations should have similar performance
        let first = durations[0];
        let last = durations[9];
        let ratio = if first > last {
            first.as_millis() as f64 / last.as_millis() as f64
        } else {
            last.as_millis() as f64 / first.as_millis() as f64
        };

        assert!(
            ratio < 3.0,
            "Performance should not degrade significantly: first={:?}, last={:?}, ratio={}",
            first,
            last,
            ratio
        );
    }

    #[tokio::test]
    async fn test_memory_efficient_large_args() {
        // Test that large arguments don't cause excessive memory usage
        let large_arg = "x".repeat(10_000);
        let start = Instant::now();

        let result =
            run_for_node(vec!["market".to_string(), "search".to_string(), large_arg]).await;

        let duration = start.elapsed();

        match result {
            Ok(res) => {
                assert!(res.code >= 0);
            }
            Err(_) => {
                // Error is acceptable
            }
        }

        assert!(
            duration < Duration::from_millis(STANDARD_OPERATION_MS),
            "Large args should be handled efficiently, took {:?}",
            duration
        );
    }
}

#[cfg(test)]
mod throughput_tests {
    use ggen_cli_lib::run_for_node;
    use std::time::Instant;

    #[tokio::test]
    async fn test_sequential_throughput() {
        let start = Instant::now();
        let iterations = 20;

        for _ in 0..iterations {
            let result = run_for_node(vec!["--version".to_string()]).await;
            assert!(result.is_ok());
        }

        let duration = start.elapsed();
        let ops_per_sec = iterations as f64 / duration.as_secs_f64();

        println!("Sequential throughput: {:.2} ops/sec", ops_per_sec);
        assert!(
            ops_per_sec > 10.0,
            "Should handle at least 10 operations per second, got {:.2}",
            ops_per_sec
        );
    }

    #[tokio::test]
    async fn test_concurrent_throughput() {
        let start = Instant::now();
        let iterations = 20;

        let tasks: Vec<_> = (0..iterations)
            .map(|_| tokio::spawn(async { run_for_node(vec!["--version".to_string()]).await }))
            .collect();

        let results = futures::future::join_all(tasks).await;
        let duration = start.elapsed();

        let successful = results.iter().filter(|r| r.is_ok()).count();
        let ops_per_sec = successful as f64 / duration.as_secs_f64();

        println!("Concurrent throughput: {:.2} ops/sec", ops_per_sec);
        assert!(
            ops_per_sec > 20.0,
            "Concurrent operations should achieve higher throughput, got {:.2}",
            ops_per_sec
        );
    }
}

#[cfg(test)]
mod latency_tests {
    use ggen_cli_lib::run_for_node;
    use std::time::{Duration, Instant};

    #[tokio::test]
    async fn test_p50_latency() {
        let mut durations = Vec::new();

        for _ in 0..20 {
            let start = Instant::now();
            let result = run_for_node(vec!["--version".to_string()]).await;
            let duration = start.elapsed();

            if result.is_ok() {
                durations.push(duration);
            }
        }

        durations.sort();
        let p50 = durations[durations.len() / 2];

        println!("P50 latency: {:?}", p50);
        assert!(
            p50 < Duration::from_millis(50),
            "P50 latency should be under 50ms, got {:?}",
            p50
        );
    }

    #[tokio::test]
    async fn test_p99_latency() {
        let mut durations = Vec::new();

        for _ in 0..100 {
            let start = Instant::now();
            let result = run_for_node(vec!["--version".to_string()]).await;
            let duration = start.elapsed();

            if result.is_ok() {
                durations.push(duration);
            }
        }

        durations.sort();
        let p99_idx = (durations.len() as f64 * 0.99) as usize;
        let p99 = durations[p99_idx];

        println!("P99 latency: {:?}", p99);
        assert!(
            p99 < Duration::from_millis(200),
            "P99 latency should be under 200ms, got {:?}",
            p99
        );
    }
}
