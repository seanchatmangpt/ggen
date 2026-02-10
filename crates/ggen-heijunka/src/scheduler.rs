//! Scheduler for smooth workload distribution.
//!
//! This module implements scheduling strategies that smooth λ (arrival rate)
//! over configurable time windows to prevent burst arrivals.

use crate::{HeijunkaError, Result};
use std::time::{Duration, Instant};

/// Scheduling strategy for work distribution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchedulingStrategy {
    /// Smooth distribution based on target throughput.
    Smoothed,

    /// Adaptive distribution based on current load.
    Adaptive,

    /// Token bucket rate limiting.
    TokenBucket,
}

/// Scheduler that calculates delays to achieve smooth workload distribution.
pub struct Scheduler {
    #[allow(dead_code)]
    window_duration: Duration,
    target_throughput: f64,
    strategy: SchedulingStrategy,
    last_scheduled: Option<Instant>,
    token_bucket: TokenBucket,
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new(
        window_duration: Duration, target_throughput: f64, strategy: SchedulingStrategy,
    ) -> Self {
        let token_bucket = TokenBucket::new(target_throughput, target_throughput * 2.0);

        Self {
            window_duration,
            target_throughput,
            strategy,
            last_scheduled: None,
            token_bucket,
        }
    }

    /// Calculate the delay needed to smooth the arrival rate.
    pub fn calculate_delay(&mut self, buffer_size: usize, current_rate: f64) -> Result<Duration> {
        match self.strategy {
            SchedulingStrategy::Smoothed => self.calculate_smoothed_delay(),
            SchedulingStrategy::Adaptive => {
                self.calculate_adaptive_delay(buffer_size, current_rate)
            }
            SchedulingStrategy::TokenBucket => self.calculate_token_bucket_delay(),
        }
    }

    /// Calculate delay for smoothed strategy.
    fn calculate_smoothed_delay(&mut self) -> Result<Duration> {
        let target_interval = Duration::from_secs_f64(1.0 / self.target_throughput);

        match self.last_scheduled {
            Some(last) => {
                let elapsed = last.elapsed();
                if elapsed < target_interval {
                    Ok(target_interval - elapsed)
                } else {
                    self.last_scheduled = Some(Instant::now());
                    Ok(Duration::ZERO)
                }
            }
            None => {
                self.last_scheduled = Some(Instant::now());
                Ok(Duration::ZERO)
            }
        }
    }

    /// Calculate delay for adaptive strategy.
    fn calculate_adaptive_delay(
        &mut self, buffer_size: usize, current_rate: f64,
    ) -> Result<Duration> {
        // Adapt based on buffer pressure and current rate
        let buffer_pressure = buffer_size as f64 / 1000.0; // Normalize to 0-1
        let rate_ratio = current_rate / self.target_throughput;

        let adjustment_factor = if rate_ratio > 1.0 {
            // Current rate too high, increase delay
            1.0 + (rate_ratio - 1.0) * 0.5
        } else if buffer_pressure > 0.8 {
            // Buffer filling up, decrease delay
            0.5
        } else {
            1.0
        };

        let base_interval = Duration::from_secs_f64(1.0 / self.target_throughput);
        let adjusted_interval = base_interval.mul_f64(adjustment_factor);

        match self.last_scheduled {
            Some(last) => {
                let elapsed = last.elapsed();
                if elapsed < adjusted_interval {
                    Ok(adjusted_interval - elapsed)
                } else {
                    self.last_scheduled = Some(Instant::now());
                    Ok(Duration::ZERO)
                }
            }
            None => {
                self.last_scheduled = Some(Instant::now());
                Ok(Duration::ZERO)
            }
        }
    }

    /// Calculate delay using token bucket algorithm.
    fn calculate_token_bucket_delay(&mut self) -> Result<Duration> {
        if self.token_bucket.try_consume(1.0) {
            self.last_scheduled = Some(Instant::now());
            Ok(Duration::ZERO)
        } else {
            Ok(Duration::from_millis(10)) // Small delay to wait for tokens
        }
    }

    /// Reset the scheduler state.
    pub fn reset(&mut self) {
        self.last_scheduled = None;
        self.token_bucket.reset();
    }

    /// Get the current strategy.
    pub fn strategy(&self) -> SchedulingStrategy {
        self.strategy
    }

    /// Set a new scheduling strategy.
    pub fn set_strategy(&mut self, strategy: SchedulingStrategy) {
        self.strategy = strategy;
        self.reset();
    }

    /// Get the target throughput.
    pub fn target_throughput(&self) -> f64 {
        self.target_throughput
    }

    /// Update the target throughput.
    pub fn set_target_throughput(&mut self, throughput: f64) -> Result<()> {
        if throughput <= 0.0 {
            return Err(HeijunkaError::InvalidConfig(
                "target_throughput must be greater than 0".to_string(),
            ));
        }
        self.target_throughput = throughput;
        self.token_bucket.set_rate(throughput);
        Ok(())
    }
}

/// Token bucket for rate limiting.
struct TokenBucket {
    capacity: f64,
    tokens: f64,
    refill_rate: f64,
    last_refill: Instant,
}

impl TokenBucket {
    fn new(refill_rate: f64, capacity: f64) -> Self {
        Self {
            capacity,
            tokens: capacity,
            refill_rate,
            last_refill: Instant::now(),
        }
    }

    fn refill(&mut self) {
        let now = Instant::now();
        let elapsed = now.duration_since(self.last_refill).as_secs_f64();
        let new_tokens = elapsed * self.refill_rate;

        self.tokens = (self.tokens + new_tokens).min(self.capacity);
        self.last_refill = now;
    }

    fn try_consume(&mut self, amount: f64) -> bool {
        self.refill();

        if self.tokens >= amount {
            self.tokens -= amount;
            true
        } else {
            false
        }
    }

    fn reset(&mut self) {
        self.tokens = self.capacity;
        self.last_refill = Instant::now();
    }

    fn set_rate(&mut self, rate: f64) {
        self.refill_rate = rate;
        self.capacity = rate * 2.0;
        self.tokens = self.tokens.min(self.capacity);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scheduler_creation() {
        let scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        assert_eq!(scheduler.target_throughput(), 100.0);
        assert_eq!(scheduler.strategy(), SchedulingStrategy::Smoothed);
    }

    #[test]
    fn test_smoothed_strategy_first_call() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        let delay = scheduler.calculate_delay(0, 0.0).unwrap();
        assert_eq!(delay, Duration::ZERO);
    }

    #[test]
    fn test_smoothed_strategy_sequential_calls() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        // First call - no delay
        let delay1 = scheduler.calculate_delay(0, 0.0).unwrap();
        assert_eq!(delay1, Duration::ZERO);

        // Immediate second call - should have delay
        let delay2 = scheduler.calculate_delay(0, 0.0).unwrap();
        assert!(delay2 > Duration::ZERO);
    }

    #[test]
    fn test_adaptive_strategy() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Adaptive);

        // Low buffer, normal rate
        let delay1 = scheduler.calculate_delay(10, 100.0).unwrap();
        assert_eq!(delay1, Duration::ZERO);

        // High buffer - should reduce delay
        let delay2 = scheduler.calculate_delay(900, 100.0).unwrap();
        // Delay might be zero or very small due to high buffer pressure
        assert!(delay2 <= Duration::from_millis(10));
    }

    #[test]
    fn test_token_bucket_strategy() {
        let mut scheduler = Scheduler::new(
            Duration::from_secs(60),
            100.0,
            SchedulingStrategy::TokenBucket,
        );

        // First call should succeed (bucket starts full)
        let delay = scheduler.calculate_delay(0, 0.0).unwrap();
        assert_eq!(delay, Duration::ZERO);
    }

    #[test]
    fn test_strategy_switch() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        assert_eq!(scheduler.strategy(), SchedulingStrategy::Smoothed);

        scheduler.set_strategy(SchedulingStrategy::Adaptive);
        assert_eq!(scheduler.strategy(), SchedulingStrategy::Adaptive);
    }

    #[test]
    fn test_set_target_throughput() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        assert!(scheduler.set_target_throughput(200.0).is_ok());
        assert_eq!(scheduler.target_throughput(), 200.0);
    }

    #[test]
    fn test_set_target_throughput_invalid() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        assert!(scheduler.set_target_throughput(0.0).is_err());
        assert!(scheduler.set_target_throughput(-1.0).is_err());
    }

    #[test]
    fn test_scheduler_reset() {
        let mut scheduler =
            Scheduler::new(Duration::from_secs(60), 100.0, SchedulingStrategy::Smoothed);

        // Make a call to set last_scheduled
        scheduler.calculate_delay(0, 0.0).ok();

        // Reset
        scheduler.reset();

        // Next call should behave like first call
        let delay = scheduler.calculate_delay(0, 0.0).unwrap();
        assert_eq!(delay, Duration::ZERO);
    }

    #[test]
    fn test_token_bucket_refill() {
        let mut bucket = TokenBucket::new(100.0, 200.0);

        // Consume all tokens
        assert!(bucket.try_consume(200.0));
        assert!(!bucket.try_consume(1.0));

        // Wait a bit and refill (simulated by manual refill)
        std::thread::sleep(Duration::from_millis(100));
        bucket.refill();

        // Should have some tokens now
        assert!(bucket.try_consume(1.0));
    }

    #[test]
    fn test_token_bucket_capacity_limit() {
        let mut bucket = TokenBucket::new(100.0, 200.0);

        // Wait and refill multiple times
        for _ in 0..5 {
            std::thread::sleep(Duration::from_millis(100));
            bucket.refill();
        }

        // Should not exceed capacity
        assert!(bucket.tokens <= 200.0);
    }

    #[test]
    fn test_token_bucket_reset() {
        let mut bucket = TokenBucket::new(100.0, 200.0);

        bucket.try_consume(150.0);
        assert!(bucket.tokens < 200.0);

        bucket.reset();
        assert_eq!(bucket.tokens, 200.0);
    }
}
