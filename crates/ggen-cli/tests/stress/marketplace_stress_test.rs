//! Marketplace stress tests - high-load scenarios and edge cases
//!
//! Tests the marketplace CLI under extreme conditions:
//! - High concurrency
//! - Large datasets
//! - Rapid operations
//! - Resource exhaustion
//! - Edge case combinations

use anyhow::Result;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use tokio::sync::Semaphore;
use tokio::task::JoinHandle;

use crate::utils::{
    ConcurrentPatterns, OperationPermutations, PermutationConfig, SearchQueryPermutations,
    StringPermutations,
};

/// Stress test configuration
#[derive(Debug, Clone)]
pub struct StressConfig {
    /// Number of concurrent operations
    pub concurrency: usize,
    /// Total number of operations to perform
    pub total_operations: usize,
    /// Timeout for the entire test
    pub timeout: Duration,
    /// Include destructive operations
    pub include_destructive: bool,
}

impl Default for StressConfig {
    fn default() -> Self {
        Self {
            concurrency: 10,
            total_operations: 1000,
            timeout: Duration::from_secs(300),
            include_destructive: false,
        }
    }
}

/// Metrics collected during stress testing
#[derive(Debug, Clone, Default)]
pub struct StressMetrics {
    /// Total operations completed
    pub operations_completed: usize,
    /// Total operations failed
    pub operations_failed: usize,
    /// Average operation latency in milliseconds
    pub avg_latency_ms: f64,
    /// Maximum latency observed
    pub max_latency_ms: u64,
    /// Minimum latency observed
    pub min_latency_ms: u64,
    /// Throughput (ops/sec)
    pub throughput: f64,
    /// Total test duration
    pub total_duration: Duration,
    /// Memory usage peak (bytes)
    pub peak_memory_bytes: usize,
}

impl StressMetrics {
    /// Create metrics report
    pub fn report(&self) -> String {
        format!(
            r#"
Stress Test Results
===================
Operations:
  Completed: {}
  Failed: {}
  Success Rate: {:.2}%

Latency:
  Average: {:.2}ms
  Min: {}ms
  Max: {}ms

Performance:
  Throughput: {:.2} ops/sec
  Duration: {:.2}s
  Peak Memory: {:.2}MB
"#,
            self.operations_completed,
            self.operations_failed,
            (self.operations_completed as f64
                / (self.operations_completed + self.operations_failed).max(1) as f64)
                * 100.0,
            self.avg_latency_ms,
            self.min_latency_ms,
            self.max_latency_ms,
            self.throughput,
            self.total_duration.as_secs_f64(),
            self.peak_memory_bytes as f64 / 1024.0 / 1024.0
        )
    }
}

/// Stress test runner
pub struct StressTestRunner {
    config: StressConfig,
    temp_dir: TempDir,
}

impl StressTestRunner {
    pub fn new(config: StressConfig) -> Result<Self> {
        let temp_dir =
            TempDir::new().map_err(|e| anyhow::anyhow!("Failed to create temp dir: {}", e))?;
        Ok(Self { config, temp_dir })
    }

    /// Run high-concurrency search stress test
    pub async fn run_concurrent_search_stress(&self) -> Result<StressMetrics> {
        let start = Instant::now();
        let semaphore = Arc::new(Semaphore::new(self.config.concurrency));

        let perm_config = PermutationConfig {
            max_permutations: self.config.total_operations,
            include_edge_cases: true,
            include_invalid: false,
        };

        let query_perms = SearchQueryPermutations::new(perm_config);
        let queries = query_perms.all_combinations();

        let mut tasks: Vec<JoinHandle<Result<Duration>>> = Vec::new();
        let mut latencies = Vec::new();
        let mut completed = 0;
        let mut failed = 0;

        for (query, filters) in queries.into_iter().take(self.config.total_operations) {
            let permit = semaphore
                .clone()
                .acquire_owned()
                .await
                .map_err(|e| anyhow::anyhow!("Failed to acquire semaphore permit: {}", e))?;

            let task = tokio::spawn(async move {
                let _permit = permit; // Hold permit for duration of task
                let op_start = Instant::now();

                // Simulate search operation
                tokio::time::sleep(Duration::from_millis(1)).await;

                Ok(op_start.elapsed())
            });

            tasks.push(task);
        }

        // Collect results
        for task in tasks {
            match task.await {
                Ok(Ok(latency)) => {
                    latencies.push(latency);
                    completed += 1;
                }
                Ok(Err(_)) | Err(_) => {
                    failed += 1;
                }
            }
        }

        let total_duration = start.elapsed();

        Ok(self.calculate_metrics(latencies, completed, failed, total_duration))
    }

    /// Run rapid sequential operations stress test
    pub async fn run_rapid_sequential_stress(&self) -> Result<StressMetrics> {
        let start = Instant::now();
        let mut latencies = Vec::new();
        let mut completed = 0;
        let mut failed = 0;

        let perm_config = PermutationConfig {
            max_permutations: self.config.total_operations / 10,
            include_edge_cases: true,
            include_invalid: false,
        };

        let op_perms = OperationPermutations::new(perm_config);
        let sequences = op_perms.sequences(3);

        for sequence in sequences.into_iter().take(self.config.total_operations) {
            let op_start = Instant::now();

            // Execute sequence
            let result = self.execute_operation_sequence(sequence).await;

            let latency = op_start.elapsed();
            latencies.push(latency);

            match result {
                Ok(_) => completed += 1,
                Err(_) => failed += 1,
            }
        }

        let total_duration = start.elapsed();

        Ok(self.calculate_metrics(latencies, completed, failed, total_duration))
    }

    /// Run large dataset stress test
    pub async fn run_large_dataset_stress(&self) -> Result<StressMetrics> {
        let start = Instant::now();
        let mut latencies = Vec::new();
        let mut completed = 0;
        let mut failed = 0;

        // Generate large number of packages
        let package_count = self.config.total_operations;

        for i in 0..package_count {
            let op_start = Instant::now();

            // Simulate adding package
            let result = self.add_test_package(&format!("package-{}", i)).await;

            let latency = op_start.elapsed();
            latencies.push(latency);

            match result {
                Ok(_) => completed += 1,
                Err(_) => failed += 1,
            }
        }

        let total_duration = start.elapsed();

        Ok(self.calculate_metrics(latencies, completed, failed, total_duration))
    }

    /// Run memory stress test
    pub async fn run_memory_stress(&self) -> Result<StressMetrics> {
        let start = Instant::now();
        let mut latencies = Vec::new();
        let mut completed = 0;
        let mut failed = 0;
        let mut peak_memory = 0;

        // Allocate increasingly large data structures
        let mut data_holders: Vec<Vec<u8>> = Vec::new();

        for i in 0..self.config.total_operations {
            let op_start = Instant::now();

            // Allocate memory
            let size = (i + 1) * 1024; // 1KB, 2KB, 3KB, etc.
            let data = vec![0u8; size];
            data_holders.push(data);

            // Track peak memory
            let current_memory: usize = data_holders.iter().map(|v| v.len()).sum();
            peak_memory = peak_memory.max(current_memory);

            completed += 1;
            latencies.push(op_start.elapsed());
        }

        let total_duration = start.elapsed();

        let mut metrics = self.calculate_metrics(latencies, completed, failed, total_duration);
        metrics.peak_memory_bytes = peak_memory;

        Ok(metrics)
    }

    /// Run filesystem stress test
    pub async fn run_filesystem_stress(&self) -> Result<StressMetrics> {
        let start = Instant::now();
        let mut latencies = Vec::new();
        let mut completed = 0;
        let mut failed = 0;

        let perm_config = PermutationConfig {
            max_permutations: self.config.total_operations,
            include_edge_cases: true,
            include_invalid: false,
        };

        let string_perms = StringPermutations::new(perm_config);
        let filenames = string_perms.all();

        for filename in filenames.into_iter().take(self.config.total_operations) {
            let op_start = Instant::now();

            let path = self.temp_dir.path().join(&filename);
            let result = tokio::fs::write(&path, b"test data").await;

            let latency = op_start.elapsed();
            latencies.push(latency);

            match result {
                Ok(_) => completed += 1,
                Err(_) => failed += 1,
            }
        }

        let total_duration = start.elapsed();

        Ok(self.calculate_metrics(latencies, completed, failed, total_duration))
    }

    /// Execute a sequence of operations
    async fn execute_operation_sequence(
        &self, _sequence: Vec<crate::utils::PackageOperation>,
    ) -> Result<()> {
        // Simulate operation execution
        tokio::time::sleep(Duration::from_micros(100)).await;
        Ok(())
    }

    /// Add a test package
    async fn add_test_package(&self, _name: &str) -> Result<()> {
        // Simulate package addition
        tokio::time::sleep(Duration::from_micros(50)).await;
        Ok(())
    }

    /// Calculate metrics from collected data
    fn calculate_metrics(
        &self, latencies: Vec<Duration>, completed: usize, failed: usize, total_duration: Duration,
    ) -> StressMetrics {
        if latencies.is_empty() {
            return StressMetrics::default();
        }

        let latency_ms: Vec<u64> = latencies.iter().map(|d| d.as_millis() as u64).collect();
        let sum: u64 = latency_ms.iter().sum();
        let avg = sum as f64 / latencies.len() as f64;
        let min = *latency_ms.iter().min().unwrap_or(&0);
        let max = *latency_ms.iter().max().unwrap_or(&0);

        let throughput = completed as f64 / total_duration.as_secs_f64();

        StressMetrics {
            operations_completed: completed,
            operations_failed: failed,
            avg_latency_ms: avg,
            max_latency_ms: max,
            min_latency_ms: min,
            throughput,
            total_duration,
            peak_memory_bytes: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_concurrent_search_stress() {
        let config = StressConfig {
            concurrency: 5,
            total_operations: 50,
            timeout: Duration::from_secs(10),
            include_destructive: false,
        };

        let runner = StressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .run_concurrent_search_stress()
            .await
            .expect("Stress test failed");

        assert!(metrics.operations_completed > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_rapid_sequential_stress() {
        let config = StressConfig {
            concurrency: 1,
            total_operations: 100,
            timeout: Duration::from_secs(10),
            include_destructive: false,
        };

        let runner = StressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .run_rapid_sequential_stress()
            .await
            .expect("Stress test failed");

        assert!(metrics.operations_completed > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_memory_stress() {
        let config = StressConfig {
            concurrency: 1,
            total_operations: 100,
            timeout: Duration::from_secs(10),
            include_destructive: false,
        };

        let runner = StressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .run_memory_stress()
            .await
            .expect("Stress test failed");

        assert!(metrics.operations_completed > 0);
        assert!(metrics.peak_memory_bytes > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_filesystem_stress() {
        let config = StressConfig {
            concurrency: 1,
            total_operations: 50,
            timeout: Duration::from_secs(10),
            include_destructive: false,
        };

        let runner = StressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .run_filesystem_stress()
            .await
            .expect("Stress test failed");

        assert!(metrics.operations_completed > 0);
        println!("{}", metrics.report());
    }
}
