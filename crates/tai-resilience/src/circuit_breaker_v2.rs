//! Advanced circuit breaker implementation with multi-state machine
//!
//! The circuit breaker pattern prevents cascading failures by monitoring the health
//! of downstream services. This implementation supports:
//! - Multiple failure types (timeout, error, slow response)
//! - Configurable thresholds per failure type
//! - Half-Open state with health check probes
//! - Slow-start pattern for gradual traffic increase after recovery
//! - State persistence and metrics collection

use crate::error::{Error, Result};
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Duration as StdDuration;
use strum::{Display, EnumString};
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Circuit breaker state enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, EnumString, Serialize, Deserialize)]
#[strum(serialize_all = "UPPERCASE")]
pub enum CircuitBreakerState {
    /// Service is healthy, requests pass through
    Closed,
    /// Service is unhealthy, requests are rejected
    Open,
    /// Testing service recovery with limited requests
    HalfOpen,
    /// Service is recovering, gradually increasing traffic
    SlowStart,
}

/// Failure type for categorization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, EnumString, Serialize, Deserialize)]
#[strum(serialize_all = "UPPERCASE")]
pub enum FailureType {
    /// Request timeout
    Timeout,
    /// Service returned error
    Error,
    /// Service response too slow
    SlowResponse,
}

/// Configuration for circuit breaker behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitBreakerConfig {
    /// Name of the protected service
    pub name: String,

    /// Number of failures before opening circuit
    pub failure_threshold: u32,

    /// Number of successful requests in HalfOpen to close circuit
    pub success_threshold: u32,

    /// Duration before transitioning from Open to HalfOpen
    pub timeout_duration: StdDuration,

    /// Maximum requests to allow in HalfOpen state
    pub half_open_max_requests: u32,

    /// Duration for slow-start pattern after recovery
    pub slow_start_duration: StdDuration,

    /// Maximum concurrent requests allowed during normal operation
    pub max_concurrent_requests: Option<u32>,

    /// Timeout for individual requests
    pub request_timeout: StdDuration,

    /// Slow response threshold (requests slower than this are counted as slow)
    pub slow_response_threshold: StdDuration,

    /// Per-failure-type thresholds (override global threshold)
    pub failure_thresholds: std::collections::HashMap<FailureType, u32>,

    /// Enable slow-start pattern
    pub enable_slow_start: bool,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            name: "default-service".to_string(),
            failure_threshold: 5,
            success_threshold: 2,
            timeout_duration: StdDuration::from_secs(60),
            half_open_max_requests: 3,
            slow_start_duration: StdDuration::from_secs(120),
            max_concurrent_requests: Some(100),
            request_timeout: StdDuration::from_secs(30),
            slow_response_threshold: StdDuration::from_secs(5),
            failure_thresholds: std::collections::HashMap::new(),
            enable_slow_start: true,
        }
    }
}

/// Metrics collected by the circuit breaker
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitBreakerMetrics {
    /// Current state
    pub state: CircuitBreakerState,

    /// Total number of requests processed
    pub total_requests: u64,

    /// Number of successful requests
    pub successful_requests: u64,

    /// Number of failed requests
    pub failed_requests: u64,

    /// Number of rejected requests (circuit open)
    pub rejected_requests: u64,

    /// Failure counts by type
    pub failure_counts: std::collections::HashMap<FailureType, u64>,

    /// Current failure streak in Closed state
    pub current_failure_count: u32,

    /// Current success streak in HalfOpen state
    pub current_success_count: u32,

    /// Number of state transitions
    pub state_transitions: u64,

    /// Last state transition time
    pub last_state_change: Option<DateTime<Utc>>,

    /// Last request time
    pub last_request_time: Option<DateTime<Utc>>,

    /// Average response time (milliseconds)
    pub avg_response_time_ms: f64,

    /// Time circuit was opened (for debugging)
    pub circuit_opened_at: Option<DateTime<Utc>>,
}

impl Default for CircuitBreakerMetrics {
    fn default() -> Self {
        Self {
            state: CircuitBreakerState::Closed,
            total_requests: 0,
            successful_requests: 0,
            failed_requests: 0,
            rejected_requests: 0,
            failure_counts: std::collections::HashMap::new(),
            current_failure_count: 0,
            current_success_count: 0,
            state_transitions: 0,
            last_state_change: None,
            last_request_time: None,
            avg_response_time_ms: 0.0,
            circuit_opened_at: None,
        }
    }
}

/// Health check result
#[derive(Debug, Clone)]
pub struct HealthCheckResult {
    /// Is the service healthy
    pub healthy: bool,
    /// Reason for health status
    pub reason: String,
    /// Response time in milliseconds
    pub response_time_ms: u64,
}

/// Trait for health check implementations
#[async_trait]
pub trait HealthChecker: Send + Sync {
    /// Perform a health check on the service
    async fn check_health(&self) -> Result<HealthCheckResult>;
}

/// Default health checker (no-op)
pub struct NoOpHealthChecker;

#[async_trait]
impl HealthChecker for NoOpHealthChecker {
    async fn check_health(&self) -> Result<HealthCheckResult> {
        Ok(HealthCheckResult {
            healthy: true,
            reason: "NoOp health check".to_string(),
            response_time_ms: 1,
        })
    }
}

/// Advanced circuit breaker with multi-state support
pub struct CircuitBreaker {
    config: CircuitBreakerConfig,
    state: Arc<RwLock<CircuitBreakerState>>,
    metrics: Arc<RwLock<CircuitBreakerMetrics>>,
    last_failure_time: Arc<RwLock<Option<DateTime<Utc>>>>,
    slow_start_end_time: Arc<RwLock<Option<DateTime<Utc>>>>,
    health_checker: Arc<dyn HealthChecker>,
    request_semaphore: Arc<tokio::sync::Semaphore>,
}

impl CircuitBreaker {
    /// Create a new circuit breaker with the given configuration
    pub fn new(config: CircuitBreakerConfig) -> Result<Self> {
        Self::with_health_checker(config, Arc::new(NoOpHealthChecker))
    }

    /// Create a new circuit breaker with a custom health checker
    pub fn with_health_checker(
        config: CircuitBreakerConfig, health_checker: Arc<dyn HealthChecker>,
    ) -> Result<Self> {
        let max_permits = config.max_concurrent_requests.unwrap_or(100).max(1) as usize;

        Ok(Self {
            config,
            state: Arc::new(RwLock::new(CircuitBreakerState::Closed)),
            metrics: Arc::new(RwLock::new(CircuitBreakerMetrics::default())),
            last_failure_time: Arc::new(RwLock::new(None)),
            slow_start_end_time: Arc::new(RwLock::new(None)),
            health_checker,
            request_semaphore: Arc::new(tokio::sync::Semaphore::new(max_permits)),
        })
    }

    /// Get current state
    pub async fn get_state(&self) -> CircuitBreakerState {
        *self.state.read().await
    }

    /// Get current metrics
    pub async fn get_metrics(&self) -> CircuitBreakerMetrics {
        self.metrics.read().await.clone()
    }

    /// Execute a request through the circuit breaker
    pub async fn execute<F, T, E>(&mut self, f: F) -> Result<T>
    where
        F: std::future::Future<Output = std::result::Result<T, E>>,
        E: std::fmt::Display,
    {
        let state = self.get_state().await;

        match state {
            CircuitBreakerState::Closed => self.execute_closed(f).await,
            CircuitBreakerState::Open => self.execute_open().await,
            CircuitBreakerState::HalfOpen => self.execute_half_open(f).await,
            CircuitBreakerState::SlowStart => self.execute_slow_start(f).await,
        }
    }

    /// Execute request in Closed state
    async fn execute_closed<F, T, E>(&mut self, f: F) -> Result<T>
    where
        F: std::future::Future<Output = std::result::Result<T, E>>,
        E: std::fmt::Display,
    {
        // Check if we should transition to Open based on elapsed time
        self.check_transition_to_half_open().await;

        let _permit =
            self.request_semaphore.acquire().await.map_err(|e| {
                Error::internal(format!("Failed to acquire semaphore permit: {}", e))
            })?;

        let start = std::time::Instant::now();
        let result = f.await;
        let elapsed = start.elapsed();

        let mut metrics = self.metrics.write().await;
        metrics.total_requests += 1;
        metrics.last_request_time = Some(Utc::now());

        // Update average response time
        let response_time_ms = elapsed.as_millis() as f64;
        metrics.avg_response_time_ms =
            (metrics.avg_response_time_ms * (metrics.total_requests - 1) as f64 + response_time_ms)
                / metrics.total_requests as f64;

        match result {
            Ok(value) => {
                metrics.successful_requests += 1;
                metrics.current_failure_count = 0;
                debug!(
                    "Circuit breaker '{}' request succeeded (response time: {:.1}ms)",
                    self.config.name, response_time_ms
                );
                Ok(value)
            }
            Err(e) => {
                let error_msg = e.to_string();
                metrics.failed_requests += 1;
                metrics.current_failure_count += 1;

                // Determine failure type
                let failure_type = if elapsed > self.config.slow_response_threshold {
                    FailureType::SlowResponse
                } else {
                    FailureType::Error
                };

                *metrics.failure_counts.entry(failure_type).or_insert(0) += 1;

                warn!(
                    "Circuit breaker '{}' request failed: {} (failure count: {})",
                    self.config.name, error_msg, metrics.current_failure_count
                );

                // Check failure threshold
                let threshold = self
                    .config
                    .failure_thresholds
                    .get(&failure_type)
                    .copied()
                    .unwrap_or(self.config.failure_threshold);

                if metrics.current_failure_count >= threshold {
                    self.open_circuit().await;
                }

                Err(Error::internal(error_msg))
            }
        }
    }

    /// Execute request in Open state
    async fn execute_open(&self) -> Result<()> {
        let mut metrics = self.metrics.write().await;
        metrics.rejected_requests += 1;
        metrics.total_requests += 1;

        Err(Error::circuit_breaker_open(self.config.name.clone()))
    }

    /// Execute request in HalfOpen state
    async fn execute_half_open<F, T, E>(&mut self, f: F) -> Result<T>
    where
        F: std::future::Future<Output = std::result::Result<T, E>>,
        E: std::fmt::Display,
    {
        let mut metrics = self.metrics.write().await;

        // Check if we've exceeded max requests in HalfOpen
        if metrics.current_success_count >= self.config.half_open_max_requests {
            drop(metrics);
            self.close_circuit().await;
            return self.execute_closed(f).await;
        }

        drop(metrics);

        let start = std::time::Instant::now();
        let result = f.await;
        let elapsed = start.elapsed();

        let mut metrics = self.metrics.write().await;
        metrics.total_requests += 1;

        match result {
            Ok(value) => {
                metrics.successful_requests += 1;
                metrics.current_success_count += 1;
                debug!(
                    "Circuit breaker '{}' HalfOpen request succeeded",
                    self.config.name
                );

                if metrics.current_success_count >= self.config.success_threshold {
                    drop(metrics);
                    self.close_circuit().await;
                }

                Ok(value)
            }
            Err(e) => {
                metrics.failed_requests += 1;
                metrics.current_failure_count = 1;
                metrics.current_success_count = 0;

                warn!(
                    "Circuit breaker '{}' HalfOpen request failed: {}",
                    self.config.name, e
                );

                drop(metrics);
                self.open_circuit().await;

                Err(Error::internal(e.to_string()))
            }
        }
    }

    /// Execute request in SlowStart state
    async fn execute_slow_start<F, T, E>(&mut self, f: F) -> Result<T>
    where
        F: std::future::Future<Output = std::result::Result<T, E>>,
        E: std::fmt::Display,
    {
        // Check if slow-start period has ended
        let slow_start_end = self.slow_start_end_time.read().await;
        if let Some(end_time) = *slow_start_end {
            if Utc::now() > end_time {
                drop(slow_start_end);
                self.close_circuit().await;
                return self.execute_closed(f).await;
            }
        }
        drop(slow_start_end);

        // Allow request with reduced concurrency
        let _permit =
            self.request_semaphore.acquire().await.map_err(|e| {
                Error::internal(format!("Failed to acquire semaphore permit: {}", e))
            })?;

        let start = std::time::Instant::now();
        let result = f.await;
        let elapsed = start.elapsed();

        let mut metrics = self.metrics.write().await;
        metrics.total_requests += 1;
        metrics.last_request_time = Some(Utc::now());

        match result {
            Ok(value) => {
                metrics.successful_requests += 1;
                Ok(value)
            }
            Err(e) => {
                metrics.failed_requests += 1;
                Err(Error::internal(e.to_string()))
            }
        }
    }

    /// Transition circuit from Open to HalfOpen if timeout has passed
    async fn check_transition_to_half_open(&mut self) {
        let state = self.get_state().await;
        if state != CircuitBreakerState::Open {
            return;
        }

        let last_failure = self.last_failure_time.read().await;
        if let Some(failure_time) = *last_failure {
            let duration_since_failure =
                Utc::now().signed_duration_since(failure_time).to_std().ok();

            if let Some(duration) = duration_since_failure {
                if duration > self.config.timeout_duration {
                    drop(last_failure);
                    self.transition_to_half_open().await;
                }
            }
        }
    }

    /// Open the circuit
    async fn open_circuit(&mut self) {
        let mut state = self.state.write().await;
        if *state != CircuitBreakerState::Open {
            *state = CircuitBreakerState::Open;

            let mut metrics = self.metrics.write().await;
            metrics.state = CircuitBreakerState::Open;
            metrics.state_transitions += 1;
            metrics.last_state_change = Some(Utc::now());
            metrics.circuit_opened_at = Some(Utc::now());

            drop(metrics);

            *self.last_failure_time.write().await = Some(Utc::now());

            info!(
                "Circuit breaker '{}' opened at {}",
                self.config.name,
                Utc::now()
            );
        }
    }

    /// Transition to HalfOpen state
    async fn transition_to_half_open(&mut self) {
        let mut state = self.state.write().await;
        *state = CircuitBreakerState::HalfOpen;

        let mut metrics = self.metrics.write().await;
        metrics.state = CircuitBreakerState::HalfOpen;
        metrics.state_transitions += 1;
        metrics.last_state_change = Some(Utc::now());
        metrics.current_failure_count = 0;
        metrics.current_success_count = 0;

        info!(
            "Circuit breaker '{}' transitioned to HalfOpen",
            self.config.name
        );
    }

    /// Close the circuit
    async fn close_circuit(&mut self) {
        let mut state = self.state.write().await;
        let new_state = if self.config.enable_slow_start {
            CircuitBreakerState::SlowStart
        } else {
            CircuitBreakerState::Closed
        };

        *state = new_state;

        let mut metrics = self.metrics.write().await;
        metrics.state = new_state;
        metrics.state_transitions += 1;
        metrics.last_state_change = Some(Utc::now());
        metrics.current_failure_count = 0;
        metrics.current_success_count = 0;

        if new_state == CircuitBreakerState::SlowStart {
            drop(metrics);
            *self.slow_start_end_time.write().await =
                Some(Utc::now() + Duration::from_std(self.config.slow_start_duration).unwrap());

            info!(
                "Circuit breaker '{}' entered SlowStart phase",
                self.config.name
            );
        } else {
            info!("Circuit breaker '{}' closed", self.config.name);
        }
    }

    /// Perform periodic health check in HalfOpen state
    pub async fn health_check(&mut self) -> Result<()> {
        let state = self.get_state().await;

        if state == CircuitBreakerState::HalfOpen {
            match self.health_checker.check_health().await {
                Ok(result) => {
                    if result.healthy {
                        info!(
                            "Health check passed for '{}': {}",
                            self.config.name, result.reason
                        );
                        self.close_circuit().await;
                    } else {
                        warn!(
                            "Health check failed for '{}': {}",
                            self.config.name, result.reason
                        );
                        self.open_circuit().await;
                    }
                }
                Err(e) => {
                    warn!("Health check error for '{}': {}", self.config.name, e);
                    self.open_circuit().await;
                }
            }
        }

        Ok(())
    }

    /// Reset circuit to Closed state (useful for testing)
    pub async fn reset(&mut self) {
        let mut state = self.state.write().await;
        *state = CircuitBreakerState::Closed;

        let mut metrics = self.metrics.write().await;
        metrics.state = CircuitBreakerState::Closed;
        metrics.current_failure_count = 0;
        metrics.current_success_count = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_closes_on_success() {
        let config = CircuitBreakerConfig {
            name: "test-service".to_string(),
            ..Default::default()
        };

        let mut cb = CircuitBreaker::new(config).unwrap();

        let result = cb
            .execute(|| async { Ok::<(), String>("ok".to_string()) })
            .await;

        assert!(result.is_ok());
        assert_eq!(cb.get_state().await, CircuitBreakerState::Closed);
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_on_failures() {
        let config = CircuitBreakerConfig {
            name: "test-service".to_string(),
            failure_threshold: 2,
            ..Default::default()
        };

        let mut cb = CircuitBreaker::new(config).unwrap();

        for _ in 0..2 {
            let _ = cb
                .execute(|| async { Err::<(), String>("error".to_string()) })
                .await;
        }

        assert_eq!(cb.get_state().await, CircuitBreakerState::Open);
    }

    #[tokio::test]
    async fn test_circuit_breaker_rejects_in_open_state() {
        let config = CircuitBreakerConfig {
            name: "test-service".to_string(),
            failure_threshold: 1,
            ..Default::default()
        };

        let mut cb = CircuitBreaker::new(config).unwrap();

        // Trigger failure to open circuit
        let _ = cb
            .execute(|| async { Err::<(), String>("error".to_string()) })
            .await;

        assert_eq!(cb.get_state().await, CircuitBreakerState::Open);

        // Should be rejected
        let result = cb
            .execute(|| async { Ok::<(), String>("ok".to_string()) })
            .await;
        assert!(result.is_err());
        assert!(matches!(result, Err(Error::CircuitBreakerOpen { .. })));
    }

    #[tokio::test]
    async fn test_circuit_breaker_metrics() {
        let config = CircuitBreakerConfig {
            name: "test-service".to_string(),
            ..Default::default()
        };

        let mut cb = CircuitBreaker::new(config).unwrap();

        for _ in 0..5 {
            let _ = cb
                .execute(|| async { Ok::<(), String>("ok".to_string()) })
                .await;
        }

        let metrics = cb.get_metrics().await;
        assert_eq!(metrics.total_requests, 5);
        assert_eq!(metrics.successful_requests, 5);
    }
}
