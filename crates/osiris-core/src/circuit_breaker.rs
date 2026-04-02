//! Jidoka Circuit Breaker
//!
//! Implements the Jidoka principle - stopping the line when problems occur

use crate::health::{HealthStatus, HealthCheckResult};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

/// Circuit breaker states
#[derive(Debug, Clone, PartialEq)]
pub enum CircuitState {
    /// Circuit is closed - requests are passed through
    Closed,
    /// Circuit is open - requests are immediately failed
    Open,
    /// Circuit is half-open - one request is allowed to test
    HalfOpen,
}

/// Jidoka Circuit Breaker implementation
pub struct JidokaCircuitBreaker {
    failure_threshold: usize,
    recovery_timeout_ms: u64,
    state: CircuitState,
    failures: Arc<RwLock<usize>>,
    last_failure_time: Arc<RwLock<Option<Instant>>>,
    success_count: Arc<RwLock<usize>>,
    failure_history: Arc<RwLock<Vec<Instant>>>,
    config: CircuitBreakerConfig,
}

/// Circuit breaker configuration
#[derive(Debug, Clone)]
pub struct CircuitBreakerConfig {
    pub failure_threshold: usize,
    pub recovery_timeout_ms: u64,
    pub half_open_max_requests: usize,
    pub failure_window_ms: u64,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            recovery_timeout_ms: 30000, // 30 seconds
            half_open_max_requests: 1,
            failure_window_ms: 60000, // 1 minute
        }
    }
}

impl JidokaCircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(failure_threshold: usize, recovery_timeout_ms: u64) -> Self {
        Self {
            failure_threshold,
            recovery_timeout_ms,
            state: CircuitState::Closed,
            failures: Arc::new(RwLock::new(0)),
            last_failure_time: Arc::new(RwLock::new(None)),
            success_count: Arc::new(RwLock::new(0)),
            failure_history: Arc::new(RwLock::new(Vec::new())),
            config: CircuitBreakerConfig {
                failure_threshold,
                recovery_timeout_ms,
                ..Default::default()
            },
        }
    }

    /// Create a circuit breaker with custom configuration
    pub fn with_config(config: CircuitBreakerConfig) -> Self {
        Self {
            failure_threshold: config.failure_threshold,
            recovery_timeout_ms: config.recovery_timeout_ms,
            state: CircuitState::Closed,
            failures: Arc::new(RwLock::new(0)),
            last_failure_time: Arc::new(RwLock::new(None)),
            success_count: Arc::new(RwLock::new(0)),
            failure_history: Arc::new(RwLock::new(Vec::new())),
            config,
        }
    }

    /// Execute a function with circuit breaker protection
    pub async fn execute<F, T, E>(&self, operation: F) -> Result<T, E>
    where
        F: FnOnce() -> futures::future::BoxFuture<'static, Result<T, E>>,
    {
        // Check circuit state
        match self.state {
            CircuitState::Open => {
                warn!("Circuit breaker is open - failing fast");
                return Err("Circuit breaker open" as E);
            }
            CircuitState::HalfOpen => {
                // Allow one request through in half-open state
                debug!("Circuit breaker is half-open - allowing single request");
                let result = operation().await;

                match result {
                    Ok(value) => {
                        self.on_success().await;
                        Ok(value)
                    }
                    Err(error) => {
                        self.on_failure().await;
                        Err(error)
                    }
                }
            }
            CircuitState::Closed => {
                let result = operation().await;

                match result {
                    Ok(value) => {
                        self.on_success().await;
                        Ok(value)
                    }
                    Err(error) => {
                        self.on_failure().await;
                        Err(error)
                    }
                }
            }
        }
    }

    /// Record a successful operation
    async fn on_success(&self) {
        let mut success_count = self.success_count.write().await;
        *success_count += 1;

        // If we were in half-open state, transition back to closed
        if self.state == CircuitState::HalfOpen {
            info!("Circuit breaker transitioning back to closed");
            self.state = CircuitState::Closed;
        }

        // Reset failure count if we have enough consecutive successes
        if *success_count >= self.config.half_open_max_requests {
            let mut failures = self.failures.write().await;
            *failures = 0;
            *success_count = 0;
        }
    }

    /// Record a failed operation
    async fn on_failure(&self) {
        let mut failures = self.failures.write().await;
        *failures += 1;

        let mut failure_history = self.failure_history.write().await;
        failure_history.push(Instant::now());

        // Keep only failures within the window
        let cutoff = Instant::now() - Duration::from_millis(self.config.failure_window_ms);
        failure_history.retain(|&time| time > cutoff);

        let mut last_failure_time = self.last_failure_time.write().await;
        *last_failure_time = Some(Instant::now());

        // Check if we should open the circuit
        if *failures >= self.config.failure_threshold {
            self.state = CircuitState::Open;
            error!(
                "Circuit breaker opened after {} failures",
                *failures
            );
        }
    }

    /// Check if the circuit breaker should attempt recovery
    pub async fn attempt_recovery(&self) -> bool {
        if self.state != CircuitState::Open {
            return false;
        }

        let last_failure_time = self.last_failure_time.read().await;
        let Some(failure_time) = *last_failure_time else {
            return false;
        };

        let elapsed = failure_time.elapsed().as_millis() as u64;
        if elapsed >= self.config.recovery_timeout_ms {
            info!("Circuit breaker timeout elapsed - transitioning to half-open");
            self.state = CircuitState::HalfOpen;
            true
        } else {
            false
        }
    }

    /// Get current circuit state
    pub async fn state(&self) -> CircuitState {
        self.state.clone()
    }

    /// Get current failure count
    pub async fn failure_count(&self) -> usize {
        let failures = self.failures.read().await;
        *failures
    }

    /// Get success count
    pub async fn success_count(&self) -> usize {
        let success_count = self.success_count.read().await;
        *success_count
    }

    /// Reset the circuit breaker
    pub async fn reset(&self) {
        info!("Resetting circuit breaker");
        self.state = CircuitState::Closed;
        let mut failures = self.failures.write().await;
        *failures = 0;
        let mut success_count = self.success_count.write().await;
        *success_count = 0;
        let mut failure_history = self.failure_history.write().await;
        failure_history.clear();
    }

    /// Get health status
    pub async fn health(&self) -> HealthStatus {
        match self.state {
            CircuitState::Closed => HealthStatus::Healthy,
            CircuitState::HalfOpen => HealthStatus::Warning,
            CircuitState::Open => HealthStatus::Critical,
        }
    }

    /// Get statistics
    pub async fn stats(&self) -> CircuitBreakerStats {
        let failures = self.failures.read().await;
        let success_count = self.success_count.read().await;
        let failure_history = self.failure_history.read().await;

        CircuitBreakerStats {
            state: self.state.clone(),
            failure_count: *failures,
            success_count: *success_count,
            failure_count_window: failure_history.len(),
            is_open: matches!(self.state, CircuitState::Open),
            is_half_open: matches!(self.state, CircuitState::HalfOpen),
            is_closed: matches!(self.state, CircuitState::Closed),
        }
    }
}

/// Circuit breaker statistics
#[derive(Debug, Clone)]
pub struct CircuitBreakerStats {
    pub state: CircuitState,
    pub failure_count: usize,
    pub success_count: usize,
    pub failure_count_window: usize,
    pub is_open: bool,
    pub is_half_open: bool,
    pub is_closed: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future;
    use std::time::Duration;

    #[tokio::test]
    async fn test_circuit_breaker_closed_state() {
        let breaker = JidokaCircuitBreaker::new(3, 1000);

        // Should allow requests in closed state
        let result = breaker.execute(|| {
            Box::pin(future::ready(Ok::<i32, &str>(42)))
        }).await;

        assert_eq!(result, Ok(42));
    }

    #[tokio::test]
    async fn test_circuit_breaker_failure_threshold() {
        let breaker = JidokaCircuitBreaker::new(2, 1000);

        // Fail twice to trigger threshold
        breaker.execute(|| Box::pin(future::ready(Err::<i32, &str>("error"))))
            .await
            .unwrap_err();

        breaker.execute(|| Box::pin(future::ready(Err::<i32, &str>("error"))))
            .await
            .unwrap_err();

        // Now circuit should be open
        assert_eq!(breaker.state().await, CircuitState::Open);

        // Next request should fail immediately
        let result = breaker.execute(|| {
            Box::pin(future::ready(Ok::<i32, &str>(42)))
        }).await;

        assert_eq!(result, Err("Circuit breaker open"));
    }

    #[tokio::test]
    async fn test_circuit_breaker_recovery() {
        let breaker = JidokaCircuitBreaker::new(2, 1000);

        // Fail twice to open circuit
        breaker.execute(|| Box::pin(future::ready(Err::<i32, &str>("error"))))
            .await
            .unwrap_err();

        breaker.execute(|| Box::pin(future::ready(Err::<i32, &str>("error"))))
            .await
            .unwrap_err();

        // Wait for recovery timeout
        tokio::time::sleep(Duration::from_millis(1100)).await;

        // Attempt recovery
        assert!(breaker.attempt_recovery().await);
        assert_eq!(breaker.state().await, CircuitState::HalfOpen);

        // Successful request in half-open should close circuit
        breaker.execute(|| Box::pin(future::ready(Ok::<i32, &str>(42))))
            .await
            .unwrap();

        assert_eq!(breaker.state().await, CircuitState::Closed);
    }

    #[tokio::test]
    async fn test_circuit_breaker_stats() {
        let breaker = JidokaCircuitBreaker::new(3, 1000);

        // Execute some operations
        for _ in 0..3 {
            breaker.execute(|| Box::pin(future::ready(Ok::<i32, &str>(42))))
                .await
                .unwrap();
        }

        let stats = breaker.stats().await;
        assert_eq!(stats.failure_count, 0);
        assert_eq!(stats.success_count, 3);
        assert!(stats.is_closed);
    }

    #[tokio::test]
    async fn test_circuit_breaker_health() {
        let breaker = JidokaCircuitBreaker::new(3, 1000);

        // Should be healthy when closed
        assert_eq!(breaker.health().await, HealthStatus::Healthy);

        // Fail enough to open circuit
        for _ in 0..3 {
            breaker.execute(|| Box::pin(future::ready(Err::<i32, &str>("error"))))
                .await
                .unwrap_err();
        }

        // Should be critical when open
        assert_eq!(breaker.health().await, HealthStatus::Critical);
    }
}