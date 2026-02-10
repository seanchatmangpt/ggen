//! Metrics for tracking throughput variance and system health.
//!
//! This module provides metrics collection and analysis for monitoring
//! the smoothness of workload distribution and detecting burst patterns.

use std::collections::VecDeque;
use std::time::{Duration, Instant};

/// Metrics for load leveling operations.
#[derive(Debug, Clone, Default)]
pub struct LoadMetrics {
    total_submissions: u64,
    total_completions: u64,
    total_overflows: u64,
    total_underflows: u64,
    started_at: Option<Instant>,
}

impl LoadMetrics {
    /// Create a new metrics tracker.
    pub fn new() -> Self {
        Self {
            total_submissions: 0,
            total_completions: 0,
            total_overflows: 0,
            total_underflows: 0,
            started_at: Some(Instant::now()),
        }
    }

    /// Record a work submission.
    pub fn record_submission(&mut self) {
        self.total_submissions += 1;
    }

    /// Record a work completion.
    pub fn record_completion(&mut self) {
        self.total_completions += 1;
    }

    /// Record a buffer overflow event.
    pub fn record_overflow(&mut self) {
        self.total_overflows += 1;
    }

    /// Record a buffer underflow event.
    pub fn record_underflow(&mut self) {
        self.total_underflows += 1;
    }

    /// Get total submissions.
    pub fn total_submissions(&self) -> u64 {
        self.total_submissions
    }

    /// Get total completions.
    pub fn total_completions(&self) -> u64 {
        self.total_completions
    }

    /// Get total overflows.
    pub fn total_overflows(&self) -> u64 {
        self.total_overflows
    }

    /// Get total underflows.
    pub fn total_underflows(&self) -> u64 {
        self.total_underflows
    }

    /// Get items currently in-flight (submitted but not completed).
    pub fn in_flight(&self) -> u64 {
        self.total_submissions
            .saturating_sub(self.total_completions)
    }

    /// Get average throughput (items per second).
    pub fn average_throughput(&self) -> f64 {
        if let Some(started) = self.started_at {
            let elapsed = started.elapsed().as_secs_f64();
            if elapsed > 0.0 {
                return self.total_completions as f64 / elapsed;
            }
        }
        0.0
    }

    /// Get overflow rate (overflows per submission).
    pub fn overflow_rate(&self) -> f64 {
        if self.total_submissions > 0 {
            self.total_overflows as f64 / self.total_submissions as f64
        } else {
            0.0
        }
    }

    /// Reset all metrics.
    pub fn reset(&mut self) {
        self.total_submissions = 0;
        self.total_completions = 0;
        self.total_overflows = 0;
        self.total_underflows = 0;
        self.started_at = Some(Instant::now());
    }
}

/// Tracker for throughput variance over time windows.
pub struct ThroughputTracker {
    window_duration: Duration,
    completion_times: VecDeque<Instant>,
    window_samples: VecDeque<f64>,
    max_samples: usize,
}

impl ThroughputTracker {
    /// Create a new throughput tracker.
    pub fn new(window_duration: Duration) -> Self {
        Self {
            window_duration,
            completion_times: VecDeque::new(),
            window_samples: VecDeque::with_capacity(100),
            max_samples: 100,
        }
    }

    /// Record a completion event.
    pub fn record_completion(&mut self) {
        let now = Instant::now();
        self.completion_times.push_back(now);
        self.evict_old_completions();
        self.sample_window_rate();
    }

    /// Evict completion times outside the window.
    fn evict_old_completions(&mut self) {
        let cutoff = Instant::now() - self.window_duration;
        while let Some(&time) = self.completion_times.front() {
            if time < cutoff {
                self.completion_times.pop_front();
            } else {
                break;
            }
        }
    }

    /// Sample the current window rate.
    fn sample_window_rate(&mut self) {
        let rate = self.current_rate();
        self.window_samples.push_back(rate);

        if self.window_samples.len() > self.max_samples {
            self.window_samples.pop_front();
        }
    }

    /// Get the current throughput rate (items per second).
    pub fn current_rate(&self) -> f64 {
        if self.completion_times.is_empty() {
            return 0.0;
        }

        let window_secs = self.window_duration.as_secs_f64();
        if window_secs > 0.0 {
            self.completion_times.len() as f64 / window_secs
        } else {
            0.0
        }
    }

    /// Calculate variance of throughput over sampled windows.
    ///
    /// Returns the coefficient of variation (stddev / mean).
    pub fn variance(&self) -> f64 {
        if self.window_samples.len() < 2 {
            return 0.0;
        }

        let mean = self.mean_rate();
        if mean == 0.0 {
            return 0.0;
        }

        let variance: f64 = self
            .window_samples
            .iter()
            .map(|&rate| {
                let diff = rate - mean;
                diff * diff
            })
            .sum::<f64>()
            / self.window_samples.len() as f64;

        let stddev = variance.sqrt();
        stddev / mean // Coefficient of variation
    }

    /// Get the mean rate across all samples.
    pub fn mean_rate(&self) -> f64 {
        if self.window_samples.is_empty() {
            return 0.0;
        }

        let sum: f64 = self.window_samples.iter().sum();
        sum / self.window_samples.len() as f64
    }

    /// Get the minimum observed rate.
    pub fn min_rate(&self) -> f64 {
        self.window_samples
            .iter()
            .copied()
            .fold(f64::INFINITY, f64::min)
    }

    /// Get the maximum observed rate.
    pub fn max_rate(&self) -> f64 {
        self.window_samples
            .iter()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max)
    }

    /// Get the rate range (max - min).
    pub fn rate_range(&self) -> f64 {
        if self.window_samples.is_empty() {
            return 0.0;
        }
        self.max_rate() - self.min_rate()
    }

    /// Check if the throughput is stable within a threshold.
    pub fn is_stable(&self, threshold: f64) -> bool {
        self.variance() <= threshold
    }

    /// Get the number of completions in the current window.
    pub fn window_completions(&self) -> usize {
        self.completion_times.len()
    }

    /// Reset the tracker.
    pub fn reset(&mut self) {
        self.completion_times.clear();
        self.window_samples.clear();
    }

    /// Get percentile of rates (p between 0.0 and 1.0).
    pub fn percentile(&self, p: f64) -> f64 {
        if self.window_samples.is_empty() {
            return 0.0;
        }

        let mut sorted: Vec<f64> = self.window_samples.iter().copied().collect();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));

        let index = (p * (sorted.len() - 1) as f64).round() as usize;
        sorted[index]
    }

    /// Get the median rate (50th percentile).
    pub fn median_rate(&self) -> f64 {
        self.percentile(0.5)
    }

    /// Get the 95th percentile rate.
    pub fn p95_rate(&self) -> f64 {
        self.percentile(0.95)
    }

    /// Get the 99th percentile rate.
    pub fn p99_rate(&self) -> f64 {
        self.percentile(0.99)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_metrics_creation() {
        let metrics = LoadMetrics::new();
        assert_eq!(metrics.total_submissions(), 0);
        assert_eq!(metrics.total_completions(), 0);
        assert_eq!(metrics.total_overflows(), 0);
        assert_eq!(metrics.total_underflows(), 0);
    }

    #[test]
    fn test_record_submission() {
        let mut metrics = LoadMetrics::new();
        metrics.record_submission();
        metrics.record_submission();

        assert_eq!(metrics.total_submissions(), 2);
    }

    #[test]
    fn test_record_completion() {
        let mut metrics = LoadMetrics::new();
        metrics.record_completion();
        metrics.record_completion();
        metrics.record_completion();

        assert_eq!(metrics.total_completions(), 3);
    }

    #[test]
    fn test_record_overflow() {
        let mut metrics = LoadMetrics::new();
        metrics.record_overflow();

        assert_eq!(metrics.total_overflows(), 1);
    }

    #[test]
    fn test_in_flight() {
        let mut metrics = LoadMetrics::new();

        metrics.record_submission();
        metrics.record_submission();
        metrics.record_submission();
        assert_eq!(metrics.in_flight(), 3);

        metrics.record_completion();
        assert_eq!(metrics.in_flight(), 2);

        metrics.record_completion();
        assert_eq!(metrics.in_flight(), 1);
    }

    #[test]
    fn test_overflow_rate() {
        let mut metrics = LoadMetrics::new();

        metrics.record_submission();
        metrics.record_submission();
        metrics.record_submission();
        metrics.record_submission();
        metrics.record_overflow();

        assert_eq!(metrics.overflow_rate(), 0.25);
    }

    #[test]
    fn test_metrics_reset() {
        let mut metrics = LoadMetrics::new();

        metrics.record_submission();
        metrics.record_completion();
        metrics.record_overflow();

        metrics.reset();

        assert_eq!(metrics.total_submissions(), 0);
        assert_eq!(metrics.total_completions(), 0);
        assert_eq!(metrics.total_overflows(), 0);
    }

    #[test]
    fn test_throughput_tracker_creation() {
        let tracker = ThroughputTracker::new(Duration::from_secs(60));
        assert_eq!(tracker.window_completions(), 0);
        assert_eq!(tracker.current_rate(), 0.0);
    }

    #[test]
    fn test_record_completion_tracker() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        tracker.record_completion();
        tracker.record_completion();
        tracker.record_completion();

        assert_eq!(tracker.window_completions(), 3);
    }

    #[test]
    fn test_current_rate() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        tracker.record_completion();
        tracker.record_completion();

        let rate = tracker.current_rate();
        assert!(rate > 0.0);
    }

    #[test]
    fn test_variance_empty() {
        let tracker = ThroughputTracker::new(Duration::from_secs(60));
        assert_eq!(tracker.variance(), 0.0);
    }

    #[test]
    fn test_variance_calculation() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        // Generate some completions to create variance
        for _ in 0..10 {
            tracker.record_completion();
            std::thread::sleep(Duration::from_millis(10));
        }

        let variance = tracker.variance();
        // Just check that variance is calculated (non-negative)
        assert!(variance >= 0.0);
    }

    #[test]
    fn test_mean_rate() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        tracker.record_completion();
        tracker.record_completion();
        tracker.record_completion();

        let mean = tracker.mean_rate();
        assert!(mean >= 0.0);
    }

    #[test]
    fn test_min_max_rate() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(10));
        tracker.record_completion();

        let min = tracker.min_rate();
        let max = tracker.max_rate();

        assert!(min >= 0.0);
        assert!(max >= min);
    }

    #[test]
    fn test_rate_range() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        tracker.record_completion();
        std::thread::sleep(Duration::from_millis(10));
        tracker.record_completion();

        let range = tracker.rate_range();
        assert!(range >= 0.0);
    }

    #[test]
    fn test_is_stable() {
        let tracker = ThroughputTracker::new(Duration::from_secs(60));
        assert!(tracker.is_stable(0.2));
    }

    #[test]
    fn test_tracker_reset() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(60));

        tracker.record_completion();
        tracker.record_completion();

        tracker.reset();

        assert_eq!(tracker.window_completions(), 0);
        assert_eq!(tracker.current_rate(), 0.0);
    }

    #[test]
    fn test_percentile() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        for _ in 0..10 {
            tracker.record_completion();
            std::thread::sleep(Duration::from_millis(10));
        }

        let p50 = tracker.percentile(0.5);
        let p95 = tracker.percentile(0.95);
        let p99 = tracker.percentile(0.99);

        assert!(p50 >= 0.0);
        assert!(p95 >= p50);
        assert!(p99 >= p95);
    }

    #[test]
    fn test_median_rate() {
        let mut tracker = ThroughputTracker::new(Duration::from_secs(1));

        for _ in 0..5 {
            tracker.record_completion();
            std::thread::sleep(Duration::from_millis(10));
        }

        let median = tracker.median_rate();
        assert!(median >= 0.0);
    }

    #[test]
    fn test_eviction() {
        let mut tracker = ThroughputTracker::new(Duration::from_millis(100));

        tracker.record_completion();
        tracker.record_completion();

        // Wait for window to expire
        std::thread::sleep(Duration::from_millis(150));
        tracker.record_completion(); // Triggers eviction

        // Old completions should be evicted
        assert!(tracker.window_completions() <= 1);
    }
}
