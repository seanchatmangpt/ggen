//! Concurrency Metrics Collection
//!
//! Comprehensive metrics collection for monitoring hyper-concurrent
//! agent execution performance.

use dashmap::DashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::time::Instant;

/// Concurrency metrics collector
#[derive(Debug)]
pub struct ConcurrencyMetrics {
    /// Total executions
    total_executions: AtomicU64,
    /// Successful executions
    successful_executions: AtomicU64,
    /// Failed executions
    failed_executions: AtomicU64,
    /// Timed out executions
    timed_out_executions: AtomicU64,
    /// Total execution time (ms)
    total_execution_time_ms: AtomicU64,
    /// Concurrent executions in progress
    in_progress: AtomicUsize,
    /// Peak concurrent executions
    peak_concurrent: AtomicUsize,
    /// Per-agent metrics
    agent_metrics: DashMap<String, AgentMetrics>,
    /// Time series data for recent executions
    time_series: RwLock<VecDeque<TimeSeriesPoint>>,
    /// Maximum time series points to keep
    max_time_series: usize,
    /// Metrics start time
    start_time: Instant,
}

/// Per-agent metrics
#[derive(Debug, Default)]
pub struct AgentMetrics {
    /// Total executions
    pub total: AtomicU64,
    /// Successful executions
    pub successes: AtomicU64,
    /// Failed executions
    pub failures: AtomicU64,
    /// Total execution time (ms)
    pub total_time_ms: AtomicU64,
    /// Min execution time (ms)
    pub min_time_ms: AtomicU64,
    /// Max execution time (ms)
    pub max_time_ms: AtomicU64,
}

/// Time series data point
#[derive(Debug, Clone)]
struct TimeSeriesPoint {
    /// Timestamp
    timestamp: Instant,
    /// Concurrent executions at this point
    concurrent: usize,
    /// Success rate at this point
    success_rate: f64,
    /// Average latency at this point (ms)
    avg_latency_ms: f64,
}

impl ConcurrencyMetrics {
    /// Create a new metrics collector
    pub fn new() -> Self {
        Self {
            total_executions: AtomicU64::new(0),
            successful_executions: AtomicU64::new(0),
            failed_executions: AtomicU64::new(0),
            timed_out_executions: AtomicU64::new(0),
            total_execution_time_ms: AtomicU64::new(0),
            in_progress: AtomicUsize::new(0),
            peak_concurrent: AtomicUsize::new(0),
            agent_metrics: DashMap::new(),
            time_series: RwLock::new(VecDeque::with_capacity(1000)),
            max_time_series: 1000,
            start_time: Instant::now(),
        }
    }

    /// Record execution start
    pub fn record_execution_start(&self, count: usize) {
        let current = self.in_progress.fetch_add(count, Ordering::Relaxed) + count;
        self.update_peak(current);
        self.record_time_series();
    }

    /// Record execution complete
    pub fn record_execution_complete(&self, count: usize) {
        self.in_progress.fetch_sub(count, Ordering::Relaxed);
    }

    /// Record successful execution
    pub fn record_success(&self, duration_ms: u64) {
        self.total_executions.fetch_add(1, Ordering::Relaxed);
        self.successful_executions.fetch_add(1, Ordering::Relaxed);
        self.total_execution_time_ms
            .fetch_add(duration_ms, Ordering::Relaxed);
    }

    /// Record failed execution
    pub fn record_failure(&self, duration_ms: u64) {
        self.total_executions.fetch_add(1, Ordering::Relaxed);
        self.failed_executions.fetch_add(1, Ordering::Relaxed);
        self.total_execution_time_ms
            .fetch_add(duration_ms, Ordering::Relaxed);
    }

    /// Record timed out execution
    pub fn record_timeout(&self, duration_ms: u64) {
        self.total_executions.fetch_add(1, Ordering::Relaxed);
        self.timed_out_executions.fetch_add(1, Ordering::Relaxed);
        self.total_execution_time_ms
            .fetch_add(duration_ms, Ordering::Relaxed);
    }

    /// Record agent-specific execution
    pub fn record_agent_execution(&self, agent_id: &str, duration_ms: u64, success: bool) {
        let metrics = self
            .agent_metrics
            .entry(agent_id.to_string())
            .or_insert_with(AgentMetrics::default);

        metrics.total.fetch_add(1, Ordering::Relaxed);
        metrics.total_time_ms.fetch_add(duration_ms, Ordering::Relaxed);

        if success {
            metrics.successes.fetch_add(1, Ordering::Relaxed);
        } else {
            metrics.failures.fetch_add(1, Ordering::Relaxed);
        }

        // Update min/max
        let mut min = metrics.min_time_ms.load(Ordering::Relaxed);
        while (min == 0 || duration_ms < min)
            && metrics
                .min_time_ms
                .compare_exchange_weak(min, duration_ms, Ordering::Relaxed, Ordering::Relaxed)
                .is_err()
        {
            min = metrics.min_time_ms.load(Ordering::Relaxed);
        }

        let mut max = metrics.max_time_ms.load(Ordering::Relaxed);
        while duration_ms > max
            && metrics
                .max_time_ms
                .compare_exchange_weak(max, duration_ms, Ordering::Relaxed, Ordering::Relaxed)
                .is_err()
        {
            max = metrics.max_time_ms.load(Ordering::Relaxed);
        }
    }

    /// Get success rate
    pub fn success_rate(&self) -> f64 {
        let total = self.total_executions.load(Ordering::Relaxed);
        let successes = self.successful_executions.load(Ordering::Relaxed);
        if total > 0 {
            successes as f64 / total as f64
        } else {
            1.0
        }
    }

    /// Get average execution time
    pub fn avg_execution_time_ms(&self) -> f64 {
        let total = self.total_executions.load(Ordering::Relaxed);
        let time = self.total_execution_time_ms.load(Ordering::Relaxed);
        if total > 0 {
            time as f64 / total as f64
        } else {
            0.0
        }
    }

    /// Get current concurrent executions
    pub fn current_concurrent(&self) -> usize {
        self.in_progress.load(Ordering::Relaxed)
    }

    /// Get peak concurrent executions
    pub fn peak_concurrent(&self) -> usize {
        self.peak_concurrent.load(Ordering::Relaxed)
    }

    /// Get metrics snapshot
    pub fn snapshot(&self) -> MetricsSnapshot {
        MetricsSnapshot {
            total_executions: self.total_executions.load(Ordering::Relaxed),
            successful_executions: self.successful_executions.load(Ordering::Relaxed),
            failed_executions: self.failed_executions.load(Ordering::Relaxed),
            timed_out_executions: self.timed_out_executions.load(Ordering::Relaxed),
            current_concurrent: self.in_progress.load(Ordering::Relaxed),
            peak_concurrent: self.peak_concurrent.load(Ordering::Relaxed),
            success_rate: self.success_rate(),
            avg_execution_time_ms: self.avg_execution_time_ms(),
            uptime_secs: self.start_time.elapsed().as_secs(),
            agents: self.agent_count(),
        }
    }

    /// Get agent-specific metrics
    pub fn agent_snapshot(&self, agent_id: &str) -> Option<AgentMetricsSnapshot> {
        let metrics = self.agent_metrics.get(agent_id)?;
        let total = metrics.total.load(Ordering::Relaxed);
        let successes = metrics.successes.load(Ordering::Relaxed);
        let total_time = metrics.total_time_ms.load(Ordering::Relaxed);

        Some(AgentMetricsSnapshot {
            agent_id: agent_id.to_string(),
            total_executions: total,
            successful_executions: successes,
            failed_executions: metrics.failures.load(Ordering::Relaxed),
            success_rate: if total > 0 { successes as f64 / total as f64 } else { 1.0 },
            avg_execution_time_ms: if total > 0 { total_time as f64 / total as f64 } else { 0.0 },
            min_execution_time_ms: metrics.min_time_ms.load(Ordering::Relaxed),
            max_execution_time_ms: metrics.max_time_ms.load(Ordering::Relaxed),
        })
    }

    /// Get all agent snapshots
    pub fn all_agent_snapshots(&self) -> Vec<AgentMetricsSnapshot> {
        self.agent_metrics
            .iter()
            .filter_map(|entry| self.agent_snapshot(entry.key()))
            .collect()
    }

    /// Get agent count
    pub fn agent_count(&self) -> usize {
        self.agent_metrics.len()
    }

    /// Reset all metrics
    pub fn reset(&self) {
        self.total_executions.store(0, Ordering::Relaxed);
        self.successful_executions.store(0, Ordering::Relaxed);
        self.failed_executions.store(0, Ordering::Relaxed);
        self.timed_out_executions.store(0, Ordering::Relaxed);
        self.total_execution_time_ms.store(0, Ordering::Relaxed);
        self.peak_concurrent.store(0, Ordering::Relaxed);
        self.agent_metrics.clear();
        self.time_series.write().clear();
    }

    /// Update peak concurrent
    fn update_peak(&self, current: usize) {
        let mut peak = self.peak_concurrent.load(Ordering::Relaxed);
        while current > peak {
            match self.peak_concurrent.compare_exchange_weak(
                peak,
                current,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(p) => peak = p,
            }
        }
    }

    /// Record time series point
    fn record_time_series(&self) {
        let point = TimeSeriesPoint {
            timestamp: Instant::now(),
            concurrent: self.in_progress.load(Ordering::Relaxed),
            success_rate: self.success_rate(),
            avg_latency_ms: self.avg_execution_time_ms(),
        };

        let mut series = self.time_series.write();
        series.push_back(point);
        while series.len() > self.max_time_series {
            series.pop_front();
        }
    }
}

impl Default for ConcurrencyMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    /// Total executions
    pub total_executions: u64,
    /// Successful executions
    pub successful_executions: u64,
    /// Failed executions
    pub failed_executions: u64,
    /// Timed out executions
    pub timed_out_executions: u64,
    /// Current concurrent executions
    pub current_concurrent: usize,
    /// Peak concurrent executions
    pub peak_concurrent: usize,
    /// Success rate (0.0 - 1.0)
    pub success_rate: f64,
    /// Average execution time (ms)
    pub avg_execution_time_ms: f64,
    /// Uptime in seconds
    pub uptime_secs: u64,
    /// Number of agents tracked
    pub agents: usize,
}

/// Agent-specific metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMetricsSnapshot {
    /// Agent ID
    pub agent_id: String,
    /// Total executions
    pub total_executions: u64,
    /// Successful executions
    pub successful_executions: u64,
    /// Failed executions
    pub failed_executions: u64,
    /// Success rate (0.0 - 1.0)
    pub success_rate: f64,
    /// Average execution time (ms)
    pub avg_execution_time_ms: f64,
    /// Minimum execution time (ms)
    pub min_execution_time_ms: u64,
    /// Maximum execution time (ms)
    pub max_execution_time_ms: u64,
}

/// Histogram for latency distribution
#[derive(Debug)]
pub struct LatencyHistogram {
    /// Bucket boundaries (ms)
    buckets: Vec<u64>,
    /// Counts per bucket
    counts: Vec<AtomicU64>,
    /// Total count
    total: AtomicU64,
    /// Sum of all values
    sum: AtomicU64,
}

impl LatencyHistogram {
    /// Create with default buckets
    pub fn new() -> Self {
        Self::with_buckets(vec![1, 5, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000])
    }

    /// Create with custom buckets
    pub fn with_buckets(buckets: Vec<u64>) -> Self {
        let counts = (0..=buckets.len())
            .map(|_| AtomicU64::new(0))
            .collect();

        Self {
            buckets,
            counts,
            total: AtomicU64::new(0),
            sum: AtomicU64::new(0),
        }
    }

    /// Record a value
    pub fn record(&self, value_ms: u64) {
        self.total.fetch_add(1, Ordering::Relaxed);
        self.sum.fetch_add(value_ms, Ordering::Relaxed);

        // Find bucket
        let bucket_idx = self
            .buckets
            .iter()
            .position(|&b| value_ms <= b)
            .unwrap_or(self.buckets.len());

        self.counts[bucket_idx].fetch_add(1, Ordering::Relaxed);
    }

    /// Get percentile value
    pub fn percentile(&self, p: f64) -> u64 {
        let total = self.total.load(Ordering::Relaxed);
        if total == 0 {
            return 0;
        }

        let target = ((total as f64) * p) as u64;
        let mut cumulative = 0u64;

        for (idx, count) in self.counts.iter().enumerate() {
            cumulative += count.load(Ordering::Relaxed);
            if cumulative >= target {
                return if idx < self.buckets.len() {
                    self.buckets[idx]
                } else {
                    self.buckets.last().copied().unwrap_or(0) * 2
                };
            }
        }

        self.buckets.last().copied().unwrap_or(0)
    }

    /// Get average
    pub fn average(&self) -> f64 {
        let total = self.total.load(Ordering::Relaxed);
        let sum = self.sum.load(Ordering::Relaxed);
        if total > 0 {
            sum as f64 / total as f64
        } else {
            0.0
        }
    }
}

impl Default for LatencyHistogram {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_creation() {
        let metrics = ConcurrencyMetrics::new();
        assert_eq!(metrics.current_concurrent(), 0);
        assert_eq!(metrics.success_rate(), 1.0);
    }

    #[test]
    fn test_record_success() {
        let metrics = ConcurrencyMetrics::new();
        metrics.record_success(100);
        metrics.record_success(200);

        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.total_executions, 2);
        assert_eq!(snapshot.successful_executions, 2);
        assert!((snapshot.avg_execution_time_ms - 150.0).abs() < 0.001);
    }

    #[test]
    fn test_record_failure() {
        let metrics = ConcurrencyMetrics::new();
        metrics.record_success(100);
        metrics.record_failure(50);

        let snapshot = metrics.snapshot();
        assert_eq!(snapshot.total_executions, 2);
        assert_eq!(snapshot.successful_executions, 1);
        assert_eq!(snapshot.failed_executions, 1);
        assert!((snapshot.success_rate - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_agent_metrics() {
        let metrics = ConcurrencyMetrics::new();
        metrics.record_agent_execution("agent-1", 100, true);
        metrics.record_agent_execution("agent-1", 200, true);
        metrics.record_agent_execution("agent-2", 50, false);

        let agent1 = metrics.agent_snapshot("agent-1").unwrap();
        assert_eq!(agent1.total_executions, 2);
        assert_eq!(agent1.success_rate, 1.0);

        let agent2 = metrics.agent_snapshot("agent-2").unwrap();
        assert_eq!(agent2.total_executions, 1);
        assert_eq!(agent2.success_rate, 0.0);
    }

    #[test]
    fn test_histogram() {
        let histogram = LatencyHistogram::new();
        histogram.record(10);
        histogram.record(50);
        histogram.record(100);
        histogram.record(500);
        histogram.record(1000);

        assert!(histogram.average() > 0.0);
        assert!(histogram.percentile(0.5) > 0);
    }
}
