//! Backend performance comparison benchmarking.
//!
//! This module provides tools for comparing the performance of different
//! job queue backend implementations (ETS, Redis, PostgreSQL).

use ggen_utils::error::{GgenError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Backend type for benchmarking.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BackendType {
    /// In-memory ETS backend (Erlang Term Storage).
    ETS,
    /// Redis backend.
    Redis,
    /// PostgreSQL backend.
    PostgreSQL,
}

impl BackendType {
    /// Get human-readable name for the backend.
    pub fn name(&self) -> &'static str {
        match self {
            BackendType::ETS => "ETS (In-Memory)",
            BackendType::Redis => "Redis",
            BackendType::PostgreSQL => "PostgreSQL",
        }
    }
}

/// Configuration for benchmark execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkConfig {
    /// Total duration of the benchmark run.
    pub duration: Duration,
    /// Warmup duration before measurement starts.
    pub warmup_duration: Duration,
    /// Number of jobs to enqueue during benchmark.
    pub job_count: usize,
    /// Number of concurrent workers.
    pub concurrency: usize,
}

impl Default for BenchmarkConfig {
    fn default() -> Self {
        Self {
            duration: Duration::from_secs(30),
            warmup_duration: Duration::from_secs(5),
            job_count: 10000,
            concurrency: 10,
        }
    }
}

/// Performance metrics for a single benchmark run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkMetrics {
    /// Backend type that was benchmarked.
    pub backend: BackendType,
    /// Total jobs processed.
    pub jobs_processed: usize,
    /// Throughput in jobs per second.
    pub throughput: f64,
    /// Median latency (P50) in milliseconds.
    pub latency_p50: f64,
    /// 95th percentile latency in milliseconds.
    pub latency_p95: f64,
    /// 99th percentile latency in milliseconds.
    pub latency_p99: f64,
    /// Peak memory usage in bytes.
    pub peak_memory_bytes: usize,
    /// Average memory usage in bytes.
    pub avg_memory_bytes: usize,
    /// Total duration of the benchmark.
    pub duration: Duration,
}

impl BenchmarkMetrics {
    /// Create new metrics from latency samples.
    pub fn from_samples(
        backend: BackendType,
        latencies: &[Duration],
        memory_samples: &[usize],
        total_duration: Duration,
    ) -> Result<Self> {
        if latencies.is_empty() {
            return Err(GgenError::Generation(
                "Cannot create metrics from empty latency samples".to_string(),
            ));
        }

        let mut sorted_latencies = latencies.to_vec();
        sorted_latencies.sort();

        let p50_idx = sorted_latencies.len() / 2;
        let p95_idx = (sorted_latencies.len() as f64 * 0.95) as usize;
        let p99_idx = (sorted_latencies.len() as f64 * 0.99) as usize;

        let latency_p50 = sorted_latencies[p50_idx].as_secs_f64() * 1000.0;
        let latency_p95 = sorted_latencies[p95_idx.min(sorted_latencies.len() - 1)].as_secs_f64()
            * 1000.0;
        let latency_p99 = sorted_latencies[p99_idx.min(sorted_latencies.len() - 1)].as_secs_f64()
            * 1000.0;

        let throughput = latencies.len() as f64 / total_duration.as_secs_f64();

        let peak_memory_bytes = memory_samples.iter().max().copied().unwrap_or(0);
        let avg_memory_bytes = if memory_samples.is_empty() {
            0
        } else {
            memory_samples.iter().sum::<usize>() / memory_samples.len()
        };

        Ok(Self {
            backend,
            jobs_processed: latencies.len(),
            throughput,
            latency_p50,
            latency_p95,
            latency_p99,
            peak_memory_bytes,
            avg_memory_bytes,
            duration: total_duration,
        })
    }

    /// Check if metrics meet SLO targets.
    pub fn meets_slo(&self) -> bool {
        match self.backend {
            BackendType::ETS => {
                // ETS should achieve >40k jobs/sec
                self.throughput > 40000.0 && self.latency_p99 < 10.0
            }
            BackendType::Redis => {
                // Redis should achieve >10k jobs/sec
                self.throughput > 10000.0 && self.latency_p99 < 50.0
            }
            BackendType::PostgreSQL => {
                // PostgreSQL should achieve >5k jobs/sec
                self.throughput > 5000.0 && self.latency_p99 < 100.0
            }
        }
    }
}

/// Result of a single benchmark run.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    /// Configuration used for this benchmark.
    pub config: BenchmarkConfig,
    /// Collected metrics.
    pub metrics: BenchmarkMetrics,
    /// Timestamp when benchmark was run.
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Whether the benchmark met SLO targets.
    pub meets_slo: bool,
}

/// Comparison report across multiple backends.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComparisonReport {
    /// Results for each backend.
    pub results: HashMap<BackendType, BenchmarkResult>,
    /// ASCII chart visualization.
    pub chart: String,
    /// Summary statistics.
    pub summary: String,
}

impl ComparisonReport {
    /// Generate ASCII bar chart comparing throughput.
    pub fn generate_chart(&self) -> String {
        let mut chart = String::from("Throughput Comparison (jobs/sec)\n");
        chart.push_str("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        let max_throughput = self
            .results
            .values()
            .map(|r| r.metrics.throughput)
            .max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
            .unwrap_or(1.0);

        for (backend, result) in &self.results {
            let bar_length = ((result.metrics.throughput / max_throughput) * 40.0) as usize;
            let bar = "█".repeat(bar_length);
            chart.push_str(&format!(
                "{:12} │{:<40} {:>8.0} jobs/sec\n",
                backend.name(),
                bar,
                result.metrics.throughput
            ));
        }

        chart
    }

    /// Generate summary statistics.
    pub fn generate_summary(&self) -> String {
        let mut summary = String::from("Performance Summary\n");
        summary.push_str("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        for (backend, result) in &self.results {
            summary.push_str(&format!("\n{} ({}):\n", backend.name(), if result.meets_slo { "✓ SLO MET" } else { "✗ SLO FAILED" }));
            summary.push_str(&format!("  Throughput:    {:>10.0} jobs/sec\n", result.metrics.throughput));
            summary.push_str(&format!("  Latency P50:   {:>10.2} ms\n", result.metrics.latency_p50));
            summary.push_str(&format!("  Latency P95:   {:>10.2} ms\n", result.metrics.latency_p95));
            summary.push_str(&format!("  Latency P99:   {:>10.2} ms\n", result.metrics.latency_p99));
            summary.push_str(&format!("  Peak Memory:   {:>10} bytes\n", result.metrics.peak_memory_bytes));
            summary.push_str(&format!("  Avg Memory:    {:>10} bytes\n", result.metrics.avg_memory_bytes));
        }

        summary
    }

    /// Export report as JSON.
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| GgenError::Generation(format!("Failed to serialize report: {}", e)))
    }
}

/// Backend benchmark runner.
pub struct BackendBenchmark {
    config: BenchmarkConfig,
}

impl BackendBenchmark {
    /// Create a new benchmark runner with the given configuration.
    pub fn new(config: BenchmarkConfig) -> Self {
        Self { config }
    }

    /// Run benchmark for a single backend.
    pub async fn run_single(&self, backend: BackendType) -> Result<BenchmarkResult> {
        log::info!("Starting benchmark for backend: {}", backend.name());

        // Warmup phase
        self.warmup(backend).await?;

        // Measurement phase
        let start = Instant::now();
        let mut latencies = Vec::new();
        let mut memory_samples = Vec::new();

        let jobs_per_worker = self.config.job_count / self.config.concurrency;

        for iteration in 0..jobs_per_worker {
            let job_start = Instant::now();

            // Simulate job processing
            self.process_job(backend).await?;

            latencies.push(job_start.elapsed());

            // Sample memory every 100 iterations
            if iteration % 100 == 0 {
                memory_samples.push(self.sample_memory());
            }
        }

        let total_duration = start.elapsed();

        let metrics = BenchmarkMetrics::from_samples(
            backend,
            &latencies,
            &memory_samples,
            total_duration,
        )?;

        let meets_slo = metrics.meets_slo();

        log::info!(
            "Benchmark completed for {}: {:.0} jobs/sec (SLO: {})",
            backend.name(),
            metrics.throughput,
            if meets_slo { "MET" } else { "FAILED" }
        );

        Ok(BenchmarkResult {
            config: self.config.clone(),
            metrics,
            timestamp: chrono::Utc::now(),
            meets_slo,
        })
    }

    /// Run comparison across multiple backends.
    pub async fn run_comparison(
        &mut self,
        backends: Vec<BackendType>,
    ) -> Result<ComparisonReport> {
        let mut results = HashMap::new();

        for backend in backends {
            let result = self.run_single(backend).await?;
            results.insert(backend, result);
        }

        let report = ComparisonReport {
            chart: String::new(),
            summary: String::new(),
            results,
        };

        let chart = report.generate_chart();
        let summary = report.generate_summary();

        Ok(ComparisonReport {
            chart,
            summary,
            ..report
        })
    }

    /// Warmup phase to stabilize performance.
    async fn warmup(&self, _backend: BackendType) -> Result<()> {
        log::debug!("Warming up for {:?}", self.config.warmup_duration);
        tokio::time::sleep(self.config.warmup_duration).await;
        Ok(())
    }

    /// Simulate processing a single job.
    async fn process_job(&self, backend: BackendType) -> Result<()> {
        // Simulate different processing characteristics for each backend
        let delay = match backend {
            BackendType::ETS => Duration::from_micros(10),      // Fast in-memory
            BackendType::Redis => Duration::from_micros(50),    // Network latency
            BackendType::PostgreSQL => Duration::from_micros(100), // Database latency
        };

        tokio::time::sleep(delay).await;
        Ok(())
    }

    /// Sample current memory usage.
    fn sample_memory(&self) -> usize {
        // In a real implementation, this would use system APIs to get actual memory usage
        // For now, return a simulated value
        1024 * 1024 // 1 MB
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_type_name() {
        // Arrange
        let backends = vec![
            (BackendType::ETS, "ETS (In-Memory)"),
            (BackendType::Redis, "Redis"),
            (BackendType::PostgreSQL, "PostgreSQL"),
        ];

        // Act & Assert
        for (backend, expected_name) in backends {
            assert_eq!(backend.name(), expected_name);
        }
    }

    #[test]
    fn test_benchmark_config_default() {
        // Arrange & Act
        let config = BenchmarkConfig::default();

        // Assert
        assert_eq!(config.duration, Duration::from_secs(30));
        assert_eq!(config.warmup_duration, Duration::from_secs(5));
        assert_eq!(config.job_count, 10000);
        assert_eq!(config.concurrency, 10);
    }

    #[test]
    fn test_benchmark_metrics_from_samples() {
        // Arrange
        let latencies = vec![
            Duration::from_millis(1),
            Duration::from_millis(2),
            Duration::from_millis(3),
            Duration::from_millis(4),
            Duration::from_millis(5),
        ];
        let memory_samples = vec![1000, 2000, 3000, 4000, 5000];
        let total_duration = Duration::from_secs(1);

        // Act
        let result = BenchmarkMetrics::from_samples(
            BackendType::ETS,
            &latencies,
            &memory_samples,
            total_duration,
        );

        // Assert
        assert!(result.is_ok());
        let metrics = result.unwrap();
        assert_eq!(metrics.backend, BackendType::ETS);
        assert_eq!(metrics.jobs_processed, 5);
        assert_eq!(metrics.throughput, 5.0);
        assert_eq!(metrics.peak_memory_bytes, 5000);
        assert_eq!(metrics.avg_memory_bytes, 3000);
    }

    #[test]
    fn test_benchmark_metrics_empty_samples() {
        // Arrange
        let latencies: Vec<Duration> = vec![];
        let memory_samples: Vec<usize> = vec![];
        let total_duration = Duration::from_secs(1);

        // Act
        let result = BenchmarkMetrics::from_samples(
            BackendType::ETS,
            &latencies,
            &memory_samples,
            total_duration,
        );

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_ets_slo_met() {
        // Arrange
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

        // Assert
        assert!(meets_slo);
    }

    #[test]
    fn test_ets_slo_failed_throughput() {
        // Arrange
        let metrics = BenchmarkMetrics {
            backend: BackendType::ETS,
            jobs_processed: 30000,
            throughput: 30000.0, // Below 40k threshold
            latency_p50: 1.0,
            latency_p95: 5.0,
            latency_p99: 8.0,
            peak_memory_bytes: 1024 * 1024,
            avg_memory_bytes: 512 * 1024,
            duration: Duration::from_secs(1),
        };

        // Act
        let meets_slo = metrics.meets_slo();

        // Assert
        assert!(!meets_slo);
    }

    #[test]
    fn test_redis_slo_met() {
        // Arrange
        let metrics = BenchmarkMetrics {
            backend: BackendType::Redis,
            jobs_processed: 15000,
            throughput: 15000.0,
            latency_p50: 10.0,
            latency_p95: 30.0,
            latency_p99: 45.0,
            peak_memory_bytes: 2 * 1024 * 1024,
            avg_memory_bytes: 1024 * 1024,
            duration: Duration::from_secs(1),
        };

        // Act
        let meets_slo = metrics.meets_slo();

        // Assert
        assert!(meets_slo);
    }

    #[test]
    fn test_comparison_report_chart_generation() {
        // Arrange
        let mut results = HashMap::new();
        results.insert(
            BackendType::ETS,
            BenchmarkResult {
                config: BenchmarkConfig::default(),
                metrics: BenchmarkMetrics {
                    backend: BackendType::ETS,
                    jobs_processed: 50000,
                    throughput: 50000.0,
                    latency_p50: 1.0,
                    latency_p95: 5.0,
                    latency_p99: 8.0,
                    peak_memory_bytes: 1024 * 1024,
                    avg_memory_bytes: 512 * 1024,
                    duration: Duration::from_secs(1),
                },
                timestamp: chrono::Utc::now(),
                meets_slo: true,
            },
        );

        let report = ComparisonReport {
            results,
            chart: String::new(),
            summary: String::new(),
        };

        // Act
        let chart = report.generate_chart();

        // Assert
        assert!(chart.contains("Throughput Comparison"));
        assert!(chart.contains("ETS (In-Memory)"));
        assert!(chart.contains("50000"));
    }

    #[test]
    fn test_comparison_report_to_json() {
        // Arrange
        let results = HashMap::new();
        let report = ComparisonReport {
            results,
            chart: "test chart".to_string(),
            summary: "test summary".to_string(),
        };

        // Act
        let json_result = report.to_json();

        // Assert
        assert!(json_result.is_ok());
        let json = json_result.unwrap();
        assert!(json.contains("chart"));
        assert!(json.contains("summary"));
    }
}
