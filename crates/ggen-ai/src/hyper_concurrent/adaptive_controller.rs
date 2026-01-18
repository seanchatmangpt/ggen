//! Adaptive Concurrency Controller
//!
//! Dynamically adjusts concurrency levels based on system load,
//! success rates, and execution times.

use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};
use tracing::{debug, info};

/// Adaptive concurrency controller
#[derive(Debug)]
pub struct AdaptiveConcurrencyController {
    /// Maximum allowed concurrency
    max_concurrency: usize,
    /// Minimum allowed concurrency
    min_concurrency: usize,
    /// Current recommended concurrency
    current_concurrency: AtomicUsize,
    /// Recent execution samples
    samples: RwLock<VecDeque<ExecutionSample>>,
    /// Maximum sample size
    max_samples: usize,
    /// Configuration
    config: AdaptiveConfig,
    /// Last adjustment time
    last_adjustment: RwLock<Instant>,
}

/// Configuration for adaptive behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdaptiveConfig {
    /// Target success rate (0.0 - 1.0)
    pub target_success_rate: f64,
    /// Target latency percentile (e.g., 0.95 for p95)
    pub target_latency_percentile: f64,
    /// Target latency in milliseconds
    pub target_latency_ms: u64,
    /// Adjustment interval in seconds
    pub adjustment_interval_secs: u64,
    /// Scale up threshold (success rate above this triggers scale up)
    pub scale_up_threshold: f64,
    /// Scale down threshold (success rate below this triggers scale down)
    pub scale_down_threshold: f64,
    /// Gradual scaling factor (how much to adjust per interval)
    pub scaling_factor: f64,
}

impl Default for AdaptiveConfig {
    fn default() -> Self {
        Self {
            target_success_rate: 0.95,
            target_latency_percentile: 0.95,
            target_latency_ms: 5000,
            adjustment_interval_secs: 10,
            scale_up_threshold: 0.98,
            scale_down_threshold: 0.90,
            scaling_factor: 0.2,
        }
    }
}

/// Execution sample for analysis
#[derive(Debug, Clone)]
struct ExecutionSample {
    /// Execution duration in milliseconds
    duration_ms: u64,
    /// Whether execution succeeded
    success: bool,
    /// Sample timestamp
    timestamp: Instant,
    /// Concurrent executions at sample time
    concurrent_at_sample: usize,
}

impl AdaptiveConcurrencyController {
    /// Create a new adaptive controller
    pub fn new(max_concurrency: usize) -> Self {
        Self {
            max_concurrency,
            min_concurrency: 1,
            current_concurrency: AtomicUsize::new(max_concurrency),
            samples: RwLock::new(VecDeque::with_capacity(1000)),
            max_samples: 1000,
            config: AdaptiveConfig::default(),
            last_adjustment: RwLock::new(Instant::now()),
        }
    }

    /// Create with custom configuration
    pub fn with_config(max_concurrency: usize, config: AdaptiveConfig) -> Self {
        Self {
            max_concurrency,
            min_concurrency: 1,
            current_concurrency: AtomicUsize::new(max_concurrency),
            samples: RwLock::new(VecDeque::with_capacity(1000)),
            max_samples: 1000,
            config,
            last_adjustment: RwLock::new(Instant::now()),
        }
    }

    /// Record an execution sample
    pub fn record_execution(&self, duration_ms: u64, success: bool) {
        let sample = ExecutionSample {
            duration_ms,
            success,
            timestamp: Instant::now(),
            concurrent_at_sample: self.current_concurrency.load(Ordering::Relaxed),
        };

        let mut samples = self.samples.write();
        samples.push_back(sample);

        // Trim old samples
        while samples.len() > self.max_samples {
            samples.pop_front();
        }

        // Check if we should adjust
        drop(samples);
        self.maybe_adjust();
    }

    /// Get recommended concurrency level
    pub fn recommended_concurrency(&self) -> usize {
        self.current_concurrency.load(Ordering::Relaxed)
    }

    /// Force recalculation of concurrency
    pub fn recalculate(&self) {
        self.adjust_concurrency();
    }

    /// Check if adjustment is needed
    fn maybe_adjust(&self) {
        let last = *self.last_adjustment.read();
        if last.elapsed() >= Duration::from_secs(self.config.adjustment_interval_secs) {
            self.adjust_concurrency();
        }
    }

    /// Adjust concurrency based on samples
    fn adjust_concurrency(&self) {
        let samples = self.samples.read();
        if samples.len() < 10 {
            return; // Need minimum samples
        }

        // Calculate metrics
        let recent_samples: Vec<_> = samples
            .iter()
            .filter(|s| s.timestamp.elapsed() < Duration::from_secs(60))
            .collect();

        if recent_samples.is_empty() {
            return;
        }

        let success_count = recent_samples.iter().filter(|s| s.success).count();
        let success_rate = success_count as f64 / recent_samples.len() as f64;

        // Calculate latency percentile
        let mut durations: Vec<u64> = recent_samples.iter().map(|s| s.duration_ms).collect();
        durations.sort_unstable();
        let p95_idx = (durations.len() as f64 * self.config.target_latency_percentile) as usize;
        let p95_latency = durations.get(p95_idx.min(durations.len() - 1)).copied().unwrap_or(0);

        drop(samples);

        // Determine adjustment
        let current = self.current_concurrency.load(Ordering::Relaxed);
        let new_concurrency = self.calculate_new_concurrency(current, success_rate, p95_latency);

        if new_concurrency != current {
            self.current_concurrency
                .store(new_concurrency, Ordering::Relaxed);
            *self.last_adjustment.write() = Instant::now();
            info!(
                "Adjusted concurrency: {} -> {} (success_rate: {:.2}%, p95_latency: {}ms)",
                current,
                new_concurrency,
                success_rate * 100.0,
                p95_latency
            );
        }
    }

    /// Calculate new concurrency level
    fn calculate_new_concurrency(&self, current: usize, success_rate: f64, p95_latency: u64) -> usize {
        let mut new_concurrency = current;

        // Scale based on success rate
        if success_rate >= self.config.scale_up_threshold
            && p95_latency < self.config.target_latency_ms
        {
            // Good performance, try to scale up
            let increase = ((current as f64) * self.config.scaling_factor).ceil() as usize;
            new_concurrency = (current + increase).min(self.max_concurrency);
            debug!("Scaling up concurrency due to good performance");
        } else if success_rate < self.config.scale_down_threshold
            || p95_latency > self.config.target_latency_ms * 2
        {
            // Poor performance, scale down
            let decrease = ((current as f64) * self.config.scaling_factor).ceil() as usize;
            new_concurrency = current.saturating_sub(decrease).max(self.min_concurrency);
            debug!("Scaling down concurrency due to poor performance");
        }

        new_concurrency
    }

    /// Get current statistics
    pub fn statistics(&self) -> AdaptiveStatistics {
        let samples = self.samples.read();
        let recent: Vec<_> = samples
            .iter()
            .filter(|s| s.timestamp.elapsed() < Duration::from_secs(60))
            .collect();

        let success_count = recent.iter().filter(|s| s.success).count();
        let success_rate = if recent.is_empty() {
            1.0
        } else {
            success_count as f64 / recent.len() as f64
        };

        let avg_latency = if recent.is_empty() {
            0.0
        } else {
            recent.iter().map(|s| s.duration_ms as f64).sum::<f64>() / recent.len() as f64
        };

        AdaptiveStatistics {
            current_concurrency: self.current_concurrency.load(Ordering::Relaxed),
            max_concurrency: self.max_concurrency,
            min_concurrency: self.min_concurrency,
            recent_success_rate: success_rate,
            avg_latency_ms: avg_latency,
            sample_count: samples.len(),
            last_adjustment_secs_ago: self.last_adjustment.read().elapsed().as_secs(),
        }
    }
}

/// Adaptive controller statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdaptiveStatistics {
    /// Current concurrency level
    pub current_concurrency: usize,
    /// Maximum allowed concurrency
    pub max_concurrency: usize,
    /// Minimum allowed concurrency
    pub min_concurrency: usize,
    /// Recent success rate
    pub recent_success_rate: f64,
    /// Average latency in milliseconds
    pub avg_latency_ms: f64,
    /// Total sample count
    pub sample_count: usize,
    /// Seconds since last adjustment
    pub last_adjustment_secs_ago: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_controller_creation() {
        let controller = AdaptiveConcurrencyController::new(10);
        assert_eq!(controller.recommended_concurrency(), 10);
        assert_eq!(controller.max_concurrency, 10);
    }

    #[test]
    fn test_record_execution() {
        let controller = AdaptiveConcurrencyController::new(10);
        controller.record_execution(100, true);
        controller.record_execution(200, false);

        let stats = controller.statistics();
        assert_eq!(stats.sample_count, 2);
    }

    #[test]
    fn test_statistics() {
        let controller = AdaptiveConcurrencyController::new(5);
        for _ in 0..10 {
            controller.record_execution(50, true);
        }

        let stats = controller.statistics();
        assert_eq!(stats.current_concurrency, 5);
        assert!(stats.recent_success_rate > 0.99);
    }

    #[test]
    fn test_custom_config() {
        let config = AdaptiveConfig {
            target_success_rate: 0.99,
            target_latency_ms: 1000,
            ..Default::default()
        };
        let controller = AdaptiveConcurrencyController::with_config(8, config);
        assert_eq!(controller.recommended_concurrency(), 8);
    }
}
