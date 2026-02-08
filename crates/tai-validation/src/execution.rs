//! Automated test execution pipeline with parallel batching

use crate::error::Result;
use serde::{Deserialize, Serialize};

/// Test batch configuration
#[derive(Debug, Clone)]
pub struct TestBatch {
    /// Batch ID
    pub batch_id: String,
    /// Test names in this batch
    pub tests: Vec<String>,
    /// Timeout for batch (seconds)
    pub timeout_secs: u64,
    /// Expected duration (for optimization)
    pub expected_duration_secs: f64,
}

impl TestBatch {
    /// Create new batch
    pub fn new(batch_id: String, tests: Vec<String>) -> Self {
        Self {
            batch_id,
            tests,
            timeout_secs: 30,
            expected_duration_secs: 10.0,
        }
    }

    /// Set timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Get test count
    pub fn test_count(&self) -> usize {
        self.tests.len()
    }
}

/// Single test result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    /// Test name
    pub test_name: String,
    /// Passed/Failed
    pub passed: bool,
    /// Duration (seconds)
    pub duration_secs: f64,
    /// Error message (if failed)
    pub error_message: Option<String>,
    /// Stdout output
    pub stdout: String,
    /// Stderr output
    pub stderr: String,
    /// Test started timestamp (ISO 8601)
    pub started_at: String,
}

impl TestResult {
    /// Create passing result
    pub fn passed(test_name: String, duration_secs: f64) -> Self {
        Self {
            test_name,
            passed: true,
            duration_secs,
            error_message: None,
            stdout: String::new(),
            stderr: String::new(),
            started_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Create failing result
    pub fn failed(test_name: String, duration_secs: f64, error: String) -> Self {
        Self {
            test_name,
            passed: false,
            duration_secs,
            error_message: Some(error),
            stdout: String::new(),
            stderr: String::new(),
            started_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Add stdout
    pub fn with_stdout(mut self, output: String) -> Self {
        self.stdout = output;
        self
    }

    /// Add stderr
    pub fn with_stderr(mut self, output: String) -> Self {
        self.stderr = output;
        self
    }
}

/// Aggregated test execution results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedResults {
    /// Total tests
    pub total_tests: usize,
    /// Passed tests
    pub passed_tests: usize,
    /// Failed tests
    pub failed_tests: usize,
    /// Skipped tests
    pub skipped_tests: usize,
    /// Total duration (seconds)
    pub total_duration_secs: f64,
    /// P95 latency (seconds)
    pub p95_latency_secs: f64,
    /// P99 latency (seconds)
    pub p99_latency_secs: f64,
    /// Max latency (seconds)
    pub max_latency_secs: f64,
    /// Individual test results
    pub results: Vec<TestResult>,
}

impl AggregatedResults {
    /// Create from test results
    pub fn from_results(results: Vec<TestResult>) -> Self {
        let total = results.len();
        let passed = results.iter().filter(|r| r.passed).count();
        let failed = results.iter().filter(|r| !r.passed).count();

        let mut durations: Vec<f64> = results.iter().map(|r| r.duration_secs).collect();
        durations.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let p95_idx = ((0.95 * durations.len() as f64) as usize).min(durations.len() - 1);
        let p99_idx = ((0.99 * durations.len() as f64) as usize).min(durations.len() - 1);
        let max = durations.last().copied().unwrap_or(0.0);

        Self {
            total_tests: total,
            passed_tests: passed,
            failed_tests: failed,
            skipped_tests: 0,
            total_duration_secs: durations.iter().sum(),
            p95_latency_secs: durations.get(p95_idx).copied().unwrap_or(0.0),
            p99_latency_secs: durations.get(p99_idx).copied().unwrap_or(0.0),
            max_latency_secs: max,
            results,
        }
    }

    /// Is all tests passing
    pub fn all_passed(&self) -> bool {
        self.failed_tests == 0
    }

    /// Get failed tests
    pub fn failed(&self) -> Vec<&TestResult> {
        self.results.iter().filter(|r| !r.passed).collect()
    }

    /// Get summary
    pub fn summary(&self) -> String {
        format!(
            "{}/{} tests passed in {:.1}s (failed: {}, p95: {:.2}s, p99: {:.2}s)",
            self.passed_tests,
            self.total_tests,
            self.total_duration_secs,
            self.failed_tests,
            self.p95_latency_secs,
            self.p99_latency_secs
        )
    }
}

/// Test execution pipeline
pub struct ExecutionPipeline {
    /// Batches to execute
    batches: Vec<TestBatch>,
    /// Maximum concurrent batches
    max_concurrent: usize,
    /// Enable retry on failure
    enable_retry: bool,
    /// Max retry attempts
    max_retries: u32,
}

impl ExecutionPipeline {
    /// Create new pipeline
    pub fn new() -> Self {
        Self {
            batches: Vec::new(),
            max_concurrent: 4,
            enable_retry: true,
            max_retries: 3,
        }
    }

    /// Add test batch
    pub fn add_batch(&mut self, batch: TestBatch) {
        self.batches.push(batch);
    }

    /// Set max concurrent batches
    pub fn with_max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent = max;
        self
    }

    /// Enable/disable retry
    pub fn with_retry(mut self, enable: bool, max_retries: u32) -> Self {
        self.enable_retry = enable;
        self.max_retries = max_retries;
        self
    }

    /// Execute all batches (stub - actual execution would use tokio)
    pub async fn execute(&self) -> Result<AggregatedResults> {
        // In production, this would:
        // 1. Execute batches in parallel (respecting max_concurrent)
        // 2. Monitor timeouts
        // 3. Retry failed tests
        // 4. Aggregate results

        Ok(AggregatedResults {
            total_tests: 0,
            passed_tests: 0,
            failed_tests: 0,
            skipped_tests: 0,
            total_duration_secs: 0.0,
            p95_latency_secs: 0.0,
            p99_latency_secs: 0.0,
            max_latency_secs: 0.0,
            results: Vec::new(),
        })
    }

    /// Get batch count
    pub fn batch_count(&self) -> usize {
        self.batches.len()
    }
}

impl Default for ExecutionPipeline {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_batch_creation() {
        let tests = vec!["test1".to_string(), "test2".to_string()];
        let batch = TestBatch::new("batch1".to_string(), tests);
        assert_eq!(batch.test_count(), 2);
    }

    #[test]
    fn test_batch_with_timeout() {
        let tests = vec!["test1".to_string()];
        let batch = TestBatch::new("batch1".to_string(), tests).with_timeout(60);
        assert_eq!(batch.timeout_secs, 60);
    }

    #[test]
    fn test_result_passed() {
        let result = TestResult::passed("test1".to_string(), 1.5);
        assert!(result.passed);
        assert_eq!(result.duration_secs, 1.5);
    }

    #[test]
    fn test_result_failed() {
        let result = TestResult::failed("test1".to_string(), 2.0, "Assertion failed".to_string());
        assert!(!result.passed);
        assert!(result.error_message.is_some());
    }

    #[test]
    fn test_aggregated_results() {
        let results = vec![
            TestResult::passed("test1".to_string(), 1.0),
            TestResult::passed("test2".to_string(), 2.0),
            TestResult::failed("test3".to_string(), 0.5, "Failed".to_string()),
        ];

        let agg = AggregatedResults::from_results(results);
        assert_eq!(agg.total_tests, 3);
        assert_eq!(agg.passed_tests, 2);
        assert_eq!(agg.failed_tests, 1);
        assert!(!agg.all_passed());
    }

    #[test]
    fn test_pipeline_creation() {
        let pipeline = ExecutionPipeline::new();
        assert_eq!(pipeline.batch_count(), 0);
    }

    #[test]
    fn test_pipeline_add_batch() {
        let mut pipeline = ExecutionPipeline::new();
        let tests = vec!["test1".to_string()];
        let batch = TestBatch::new("batch1".to_string(), tests);
        pipeline.add_batch(batch);
        assert_eq!(pipeline.batch_count(), 1);
    }
}
