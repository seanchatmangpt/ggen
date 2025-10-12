//! Timeout utilities for async execution
//!
//! This module provides advanced timeout handling for the async executor,
//! including exponential backoff, retry mechanisms, and timeout strategies.

use crate::error::{Result, CleanroomError};
use std::time::Duration;
use std::future::Future;
use tokio::time::{timeout, sleep, Instant};

/// Timeout strategy for execution
#[derive(Debug, Clone)]
pub enum TimeoutStrategy {
    /// Fixed timeout
    Fixed(Duration),
    /// Exponential backoff with maximum timeout
    ExponentialBackoff {
        initial: Duration,
        max: Duration,
        multiplier: f64,
    },
    /// Adaptive timeout based on previous execution times
    Adaptive {
        base: Duration,
        factor: f64,
        max: Duration,
    },
}

impl Default for TimeoutStrategy {
    fn default() -> Self {
        Self::Fixed(Duration::from_secs(30))
    }
}

impl TimeoutStrategy {
    /// Create a fixed timeout strategy
    pub fn fixed(duration: Duration) -> Self {
        Self::Fixed(duration)
    }

    /// Create an exponential backoff strategy
    pub fn exponential_backoff(initial: Duration, max: Duration, multiplier: f64) -> Self {
        Self::ExponentialBackoff {
            initial,
            max,
            multiplier,
        }
    }

    /// Create an adaptive timeout strategy
    pub fn adaptive(base: Duration, factor: f64, max: Duration) -> Self {
        Self::Adaptive { base, factor, max }
    }

    /// Get the timeout duration for the given attempt
    pub fn timeout_for_attempt(&self, attempt: usize) -> Duration {
        match self {
            Self::Fixed(duration) => *duration,
            Self::ExponentialBackoff { initial, max, multiplier } => {
                let calculated = Duration::from_millis(
                    (initial.as_millis() as f64 * multiplier.powi(attempt as i32)) as u64
                );
                calculated.min(*max)
            }
            Self::Adaptive { base, factor, max } => {
                let calculated = Duration::from_millis(
                    (base.as_millis() as f64 * factor.powi(attempt as i32)) as u64
                );
                calculated.min(*max)
            }
        }
    }
}

/// Retry configuration
#[derive(Debug, Clone)]
pub struct RetryConfig {
    /// Maximum number of retry attempts
    pub max_attempts: usize,
    /// Timeout strategy
    pub timeout_strategy: TimeoutStrategy,
    /// Delay between retries
    pub retry_delay: Duration,
    /// Whether to retry on timeout
    pub retry_on_timeout: bool,
    /// Whether to retry on specific error types
    pub retry_on_errors: Vec<crate::error::ErrorKind>,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            timeout_strategy: TimeoutStrategy::default(),
            retry_delay: Duration::from_millis(100),
            retry_on_timeout: true,
            retry_on_errors: vec![
                crate::error::ErrorKind::Timeout,
                crate::error::ErrorKind::NetworkError,
            ],
        }
    }
}

impl RetryConfig {
    /// Create a new retry configuration
    pub fn new(max_attempts: usize) -> Self {
        Self {
            max_attempts,
            ..Default::default()
        }
    }

    /// Set timeout strategy
    pub fn with_timeout_strategy(mut self, strategy: TimeoutStrategy) -> Self {
        self.timeout_strategy = strategy;
        self
    }

    /// Set retry delay
    pub fn with_retry_delay(mut self, delay: Duration) -> Self {
        self.retry_delay = delay;
        self
    }

    /// Enable retry on timeout
    pub fn with_retry_on_timeout(mut self, enabled: bool) -> Self {
        self.retry_on_timeout = enabled;
        self
    }

    /// Add error types to retry on
    pub fn with_retry_on_errors(mut self, errors: Vec<crate::error::ErrorKind>) -> Self {
        self.retry_on_errors = errors;
        self
    }
}

/// Timeout executor with retry support
pub struct TimeoutExecutor {
    retry_config: RetryConfig,
}

impl TimeoutExecutor {
    /// Create a new timeout executor
    pub fn new(retry_config: RetryConfig) -> Self {
        Self { retry_config }
    }

    /// Execute with timeout and retry logic
    pub async fn execute_with_retry<F, T>(&self, future: F) -> Result<T>
    where
        F: Future<Output = Result<T>> + Send,
    {
        let mut last_error = None;
        
        for attempt in 0..self.retry_config.max_attempts {
            let timeout_duration = self.retry_config.timeout_strategy.timeout_for_attempt(attempt);
            
            match timeout(timeout_duration, future).await {
                Ok(result) => return result,
                Err(_) => {
                    // Timeout occurred
                    if self.retry_config.retry_on_timeout && attempt < self.retry_config.max_attempts - 1 {
                        last_error = Some(CleanroomError::timeout_error(&format!(
                            "Execution timed out after {}ms (attempt {})",
                            timeout_duration.as_millis(),
                            attempt + 1
                        )));
                        
                        // Wait before retry
                        sleep(self.retry_config.retry_delay).await;
                        continue;
                    } else {
                        return Err(CleanroomError::timeout_error(&format!(
                            "Execution timed out after {}ms (final attempt)",
                            timeout_duration.as_millis()
                        )));
                    }
                }
            }
        }
        
        Err(last_error.unwrap_or_else(|| {
            CleanroomError::internal_error("All retry attempts failed")
        }))
    }

    /// Execute with adaptive timeout based on previous execution times
    pub async fn execute_adaptive<F, T>(&self, future: F, previous_duration: Option<Duration>) -> Result<T>
    where
        F: Future<Output = Result<T>> + Send,
    {
        let timeout_duration = if let Some(prev) = previous_duration {
            // Use 2x previous duration as timeout, with minimum and maximum bounds
            let calculated = prev * 2;
            calculated.max(Duration::from_millis(100))
                .min(Duration::from_secs(300))
        } else {
            Duration::from_secs(30) // Default timeout
        };

        timeout(timeout_duration, future).await
            .map_err(|_| CleanroomError::timeout_error(&format!(
                "Execution timed out after {}ms",
                timeout_duration.as_millis()
            )))?
    }

    /// Execute with circuit breaker pattern
    pub async fn execute_with_circuit_breaker<F, T>(
        &self,
        future: F,
        circuit_breaker: &mut CircuitBreaker,
    ) -> Result<T>
    where
        F: Future<Output = Result<T>> + Send,
    {
        if !circuit_breaker.can_execute() {
            return Err(CleanroomError::internal_error("Circuit breaker is open"));
        }

        let start_time = Instant::now();
        let result = future.await;
        let duration = start_time.elapsed();

        match &result {
            Ok(_) => {
                circuit_breaker.record_success(duration);
            }
            Err(e) => {
                circuit_breaker.record_failure(duration);
                
                // Check if we should retry based on error type
                if self.retry_config.retry_on_errors.contains(&e.kind) {
                    // Could implement retry logic here
                }
            }
        }

        result
    }
}

/// Circuit breaker for fault tolerance
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    /// Current state
    state: CircuitState,
    /// Failure threshold
    failure_threshold: usize,
    /// Success threshold to close circuit
    success_threshold: usize,
    /// Timeout for half-open state
    timeout: Duration,
    /// Last failure time
    last_failure_time: Option<Instant>,
    /// Current failure count
    failure_count: usize,
    /// Current success count
    success_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

impl Default for CircuitBreaker {
    fn default() -> Self {
        Self {
            state: CircuitState::Closed,
            failure_threshold: 5,
            success_threshold: 3,
            timeout: Duration::from_secs(60),
            last_failure_time: None,
            failure_count: 0,
            success_count: 0,
        }
    }
}

impl CircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(failure_threshold: usize, success_threshold: usize, timeout: Duration) -> Self {
        Self {
            failure_threshold,
            success_threshold,
            timeout,
            ..Default::default()
        }
    }

    /// Check if execution is allowed
    pub fn can_execute(&self) -> bool {
        match self.state {
            CircuitState::Closed => true,
            CircuitState::Open => {
                if let Some(last_failure) = self.last_failure_time {
                    last_failure.elapsed() >= self.timeout
                } else {
                    false
                }
            }
            CircuitState::HalfOpen => true,
        }
    }

    /// Record a successful execution
    pub fn record_success(&mut self, _duration: Duration) {
        self.success_count += 1;
        self.failure_count = 0;

        if self.state == CircuitState::HalfOpen && self.success_count >= self.success_threshold {
            self.state = CircuitState::Closed;
            self.success_count = 0;
        }
    }

    /// Record a failed execution
    pub fn record_failure(&mut self, _duration: Duration) {
        self.failure_count += 1;
        self.success_count = 0;
        self.last_failure_time = Some(Instant::now());

        if self.failure_count >= self.failure_threshold {
            self.state = CircuitState::Open;
        } else if self.state == CircuitState::HalfOpen {
            self.state = CircuitState::Open;
        }
    }

    /// Get current state
    pub fn state(&self) -> &CircuitState {
        &self.state
    }

    /// Get failure count
    pub fn failure_count(&self) -> usize {
        self.failure_count
    }

    /// Get success count
    pub fn success_count(&self) -> usize {
        self.success_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_fixed_timeout_strategy() {
        let strategy = TimeoutStrategy::fixed(Duration::from_secs(30));
        assert_eq!(strategy.timeout_for_attempt(0), Duration::from_secs(30));
        assert_eq!(strategy.timeout_for_attempt(5), Duration::from_secs(30));
    }

    #[tokio::test]
    async fn test_exponential_backoff_strategy() {
        let strategy = TimeoutStrategy::exponential_backoff(
            Duration::from_millis(100),
            Duration::from_secs(5),
            2.0,
        );
        
        assert_eq!(strategy.timeout_for_attempt(0), Duration::from_millis(100));
        assert_eq!(strategy.timeout_for_attempt(1), Duration::from_millis(200));
        assert_eq!(strategy.timeout_for_attempt(2), Duration::from_millis(400));
        assert_eq!(strategy.timeout_for_attempt(10), Duration::from_secs(5)); // Capped at max
    }

    #[tokio::test]
    async fn test_adaptive_timeout_strategy() {
        let strategy = TimeoutStrategy::adaptive(
            Duration::from_millis(100),
            1.5,
            Duration::from_secs(5),
        );
        
        assert_eq!(strategy.timeout_for_attempt(0), Duration::from_millis(100));
        assert_eq!(strategy.timeout_for_attempt(1), Duration::from_millis(150));
        assert_eq!(strategy.timeout_for_attempt(2), Duration::from_millis(225));
    }

    #[tokio::test]
    async fn test_retry_config() {
        let config = RetryConfig::new(5)
            .with_timeout_strategy(TimeoutStrategy::fixed(Duration::from_secs(10)))
            .with_retry_delay(Duration::from_millis(500))
            .with_retry_on_timeout(true);
        
        assert_eq!(config.max_attempts, 5);
        assert_eq!(config.retry_delay, Duration::from_millis(500));
        assert!(config.retry_on_timeout);
    }

    #[tokio::test]
    async fn test_timeout_executor() {
        let config = RetryConfig::new(3)
            .with_timeout_strategy(TimeoutStrategy::fixed(Duration::from_millis(100)));
        let executor = TimeoutExecutor::new(config);
        
        let result = executor.execute_with_retry(async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            Ok::<i32, CleanroomError>(42)
        }).await;
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }

    #[tokio::test]
    async fn test_circuit_breaker() {
        let mut breaker = CircuitBreaker::new(3, 2, Duration::from_millis(100));
        
        // Initially closed
        assert!(breaker.can_execute());
        assert_eq!(breaker.state(), &CircuitState::Closed);
        
        // Record failures to open circuit
        breaker.record_failure(Duration::from_millis(10));
        breaker.record_failure(Duration::from_millis(10));
        breaker.record_failure(Duration::from_millis(10));
        
        assert_eq!(breaker.state(), &CircuitState::Open);
        assert!(!breaker.can_execute());
        
        // Wait for timeout
        tokio::time::sleep(Duration::from_millis(150)).await;
        
        // Should be half-open now
        assert!(breaker.can_execute());
        
        // Record successes to close circuit
        breaker.record_success(Duration::from_millis(10));
        breaker.record_success(Duration::from_millis(10));
        
        assert_eq!(breaker.state(), &CircuitState::Closed);
    }

    #[tokio::test]
    async fn test_adaptive_execution() {
        let config = RetryConfig::default();
        let executor = TimeoutExecutor::new(config);
        
        let previous_duration = Some(Duration::from_millis(100));
        let result = executor.execute_adaptive(
            async {
                tokio::time::sleep(Duration::from_millis(50)).await;
                Ok::<i32, CleanroomError>(42)
            },
            previous_duration,
        ).await;
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    }
}
