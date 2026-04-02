//! Capacity Estimation
//!
//! Estimates μ_capacity to inform admission control.
//! Uses exponential moving average of completion times.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::Duration;

const DEFAULT_CAPACITY: f64 = 100.0;
const ALPHA: f64 = 0.3; // EMA smoothing factor

/// Capacity estimator using exponential moving average
#[derive(Debug, Clone)]
pub struct CapacityEstimator {
    /// Average completion time in microseconds
    avg_duration_us: Arc<AtomicU64>,

    /// Total completed requests
    completed: Arc<AtomicU64>,
}

impl CapacityEstimator {
    /// Create new capacity estimator
    pub fn new() -> Self {
        Self {
            avg_duration_us: Arc::new(AtomicU64::new(0)),
            completed: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Record completion of request
    pub fn record_completion(&mut self, duration: Duration) {
        let duration_us = duration.as_micros() as u64;
        let prev_avg = self.avg_duration_us.load(Ordering::SeqCst);

        let new_avg = if prev_avg == 0 {
            duration_us
        } else {
            // Exponential moving average: EMA = α * current + (1-α) * prev
            let alpha_scaled = (ALPHA * 1000.0) as u64;
            let one_minus_alpha = 1000 - alpha_scaled;

            (alpha_scaled * duration_us + one_minus_alpha * prev_avg) / 1000
        };

        self.avg_duration_us.store(new_avg, Ordering::SeqCst);
        self.completed.fetch_add(1, Ordering::SeqCst);
    }

    /// Get estimated throughput (requests per second)
    pub fn throughput(&self) -> f64 {
        let avg_us = self.avg_duration_us.load(Ordering::SeqCst);

        if avg_us == 0 {
            return DEFAULT_CAPACITY;
        }

        // Convert microseconds to requests/second
        1_000_000.0 / avg_us as f64
    }

    /// Get total capacity (arbitrary units)
    pub fn total_capacity(&self) -> f64 {
        DEFAULT_CAPACITY
    }

    /// Get available capacity ratio (0.0 to 1.0)
    pub fn available_capacity(&self) -> f64 {
        let throughput = self.throughput();
        let ratio = throughput / DEFAULT_CAPACITY;

        // Clamp to [0.0, 1.0]
        ratio.clamp(0.0, 1.0)
    }

    /// Get average duration
    pub fn avg_duration(&self) -> Duration {
        let us = self.avg_duration_us.load(Ordering::SeqCst);
        Duration::from_micros(us)
    }

    /// Get total completed count
    pub fn completed_count(&self) -> u64 {
        self.completed.load(Ordering::SeqCst)
    }
}

impl Default for CapacityEstimator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capacity_estimator_creation() {
        let estimator = CapacityEstimator::new();
        assert_eq!(estimator.completed_count(), 0);
        assert_eq!(estimator.avg_duration(), Duration::from_micros(0));
    }

    #[test]
    fn test_capacity_estimator_default_throughput() {
        let estimator = CapacityEstimator::new();
        assert_eq!(estimator.throughput(), DEFAULT_CAPACITY);
        assert_eq!(estimator.available_capacity(), 1.0);
    }

    #[test]
    fn test_capacity_estimator_record() {
        let mut estimator = CapacityEstimator::new();

        estimator.record_completion(Duration::from_millis(10));
        assert_eq!(estimator.completed_count(), 1);
        assert!(estimator.avg_duration().as_millis() > 0);
    }

    #[test]
    fn test_capacity_estimator_throughput() {
        let mut estimator = CapacityEstimator::new();

        // Record 10ms completion -> 100 req/s throughput
        estimator.record_completion(Duration::from_millis(10));

        let throughput = estimator.throughput();
        assert!(throughput > 90.0 && throughput < 110.0);
    }

    #[test]
    fn test_capacity_estimator_ema() {
        let mut estimator = CapacityEstimator::new();

        // Record multiple completions
        estimator.record_completion(Duration::from_millis(10));
        estimator.record_completion(Duration::from_millis(20));
        estimator.record_completion(Duration::from_millis(15));

        assert_eq!(estimator.completed_count(), 3);

        // Should be smoothed average, not arithmetic mean
        let avg_ms = estimator.avg_duration().as_millis();
        assert!(avg_ms > 10 && avg_ms < 20);
    }

    #[test]
    fn test_capacity_estimator_available_capacity() {
        let mut estimator = CapacityEstimator::new();

        // Fast completions -> high capacity
        estimator.record_completion(Duration::from_micros(100));
        let high_capacity = estimator.available_capacity();
        assert!(high_capacity > 0.5);

        // Slow completions -> low capacity
        estimator.record_completion(Duration::from_millis(100));
        estimator.record_completion(Duration::from_millis(100));
        estimator.record_completion(Duration::from_millis(100));
        let low_capacity = estimator.available_capacity();
        assert!(low_capacity < high_capacity);
    }
}
