//! Performance monitoring and benchmarking

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Instant;

/// Helper function for serde default
fn instant_now() -> Instant {
    Instant::now()
}

/// Performance metrics collection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Collected metrics
    metrics: HashMap<String, f64>,

    /// Start time (not serialized)
    #[serde(skip, default = "instant_now")]
    start_time: Instant,
}

impl PerformanceMetrics {
    /// Create new performance metrics
    pub fn new() -> Self {
        Self {
            metrics: HashMap::new(),
            start_time: Instant::now(),
        }
    }

    /// Record a metric
    pub fn record(&mut self, name: impl Into<String>, value: f64) {
        self.metrics.insert(name.into(), value);
    }

    /// Get a metric value
    pub fn get(&self, name: &str) -> Option<f64> {
        self.metrics.get(name).copied()
    }

    /// Get all metrics
    pub fn all(&self) -> &HashMap<String, f64> {
        &self.metrics
    }

    /// Get elapsed time in milliseconds
    pub fn elapsed_ms(&self) -> u64 {
        self.start_time.elapsed().as_millis() as u64
    }
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Benchmark result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Benchmark {
    /// Benchmark name
    pub name: String,

    /// Iterations run
    pub iterations: usize,

    /// Total duration in milliseconds
    pub total_ms: u64,

    /// Average duration per iteration in milliseconds
    pub avg_ms: f64,

    /// Minimum duration in milliseconds
    pub min_ms: u64,

    /// Maximum duration in milliseconds
    pub max_ms: u64,
}

impl Benchmark {
    /// Create a new benchmark
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            iterations: 0,
            total_ms: 0,
            avg_ms: 0.0,
            min_ms: u64::MAX,
            max_ms: 0,
        }
    }

    /// Record an iteration
    pub fn record_iteration(&mut self, duration_ms: u64) {
        self.iterations += 1;
        self.total_ms += duration_ms;
        self.avg_ms = self.total_ms as f64 / self.iterations as f64;
        self.min_ms = self.min_ms.min(duration_ms);
        self.max_ms = self.max_ms.max(duration_ms);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_metrics() {
        let mut metrics = PerformanceMetrics::new();

        metrics.record("test_metric", 42.0);
        assert_eq!(metrics.get("test_metric"), Some(42.0));

        metrics.record("another_metric", 99.5);
        assert_eq!(metrics.all().len(), 2);
    }

    #[test]
    fn test_benchmark() {
        let mut bench = Benchmark::new("test_benchmark");

        bench.record_iteration(10);
        bench.record_iteration(20);
        bench.record_iteration(30);

        assert_eq!(bench.iterations, 3);
        assert_eq!(bench.total_ms, 60);
        assert_eq!(bench.avg_ms, 20.0);
        assert_eq!(bench.min_ms, 10);
        assert_eq!(bench.max_ms, 30);
    }
}
