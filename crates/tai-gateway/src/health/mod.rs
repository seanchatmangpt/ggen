//! Health checking and circuit breaker implementation for upstream services

use crate::error::{GatewayError, GatewayResult};
use chrono::{Duration, Utc};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Circuit breaker states
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CircuitState {
    Closed,
    Open,
    HalfOpen,
}

/// Circuit breaker configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitBreakerConfig {
    /// Failure threshold (number of failures before opening)
    pub failure_threshold: u32,
    /// Success threshold for half-open state (successes to close)
    pub success_threshold: u32,
    /// Timeout in seconds before trying again
    pub timeout_seconds: u64,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            success_threshold: 2,
            timeout_seconds: 60,
        }
    }
}

/// Circuit breaker for a single upstream
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    config: CircuitBreakerConfig,
    state: Arc<RwLock<CircuitState>>,
    failure_count: Arc<AtomicU32>,
    success_count: Arc<AtomicU32>,
    last_failure_time: Arc<AtomicU64>,
}

impl CircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(config: CircuitBreakerConfig) -> Self {
        Self {
            config,
            state: Arc::new(RwLock::new(CircuitState::Closed)),
            failure_count: Arc::new(AtomicU32::new(0)),
            success_count: Arc::new(AtomicU32::new(0)),
            last_failure_time: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Get current state
    pub async fn state(&self) -> CircuitState {
        *self.state.read().await
    }

    /// Record a successful call
    pub async fn record_success(&self) {
        let state = *self.state.read().await;

        match state {
            CircuitState::Closed => {
                // Reset failure count on success in closed state
                self.failure_count.store(0, Ordering::Relaxed);
            }
            CircuitState::HalfOpen => {
                let count = self.success_count.fetch_add(1, Ordering::Relaxed) + 1;
                if count >= self.config.success_threshold {
                    // Close circuit after enough successes
                    let mut s = self.state.write().await;
                    *s = CircuitState::Closed;
                    self.failure_count.store(0, Ordering::Relaxed);
                    self.success_count.store(0, Ordering::Relaxed);
                }
            }
            CircuitState::Open => {
                // Ignore success in open state
            }
        }
    }

    /// Record a failed call
    pub async fn record_failure(&self) {
        let state = *self.state.read().await;
        let now = Utc::now().timestamp() as u64;

        match state {
            CircuitState::Closed => {
                let count = self.failure_count.fetch_add(1, Ordering::Relaxed) + 1;
                self.last_failure_time.store(now, Ordering::Relaxed);

                if count >= self.config.failure_threshold {
                    // Open circuit after enough failures
                    let mut s = self.state.write().await;
                    *s = CircuitState::Open;
                }
            }
            CircuitState::HalfOpen => {
                // Reopen circuit on failure in half-open state
                let mut s = self.state.write().await;
                *s = CircuitState::Open;
                self.last_failure_time.store(now, Ordering::Relaxed);
                self.success_count.store(0, Ordering::Relaxed);
            }
            CircuitState::Open => {
                // Check if timeout has passed to move to half-open
                let last_failure = self.last_failure_time.load(Ordering::Relaxed);
                if now - last_failure >= self.config.timeout_seconds {
                    let mut s = self.state.write().await;
                    *s = CircuitState::HalfOpen;
                }
            }
        }
    }

    /// Check if request is allowed
    pub async fn is_request_allowed(&self) -> bool {
        let state = self.state().await;

        match state {
            CircuitState::Closed => true,
            CircuitState::HalfOpen => true,
            CircuitState::Open => {
                // Check if we should transition to half-open
                let last_failure = self.last_failure_time.load(Ordering::Relaxed);
                let now = Utc::now().timestamp() as u64;

                if now - last_failure >= self.config.timeout_seconds {
                    // Allow transitioning to half-open
                    true
                } else {
                    false
                }
            }
        }
    }
}

/// Health check probe for an upstream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthProbe {
    /// Probe path (e.g., "/health")
    pub path: String,
    /// Probe interval in seconds
    pub interval_seconds: u64,
    /// Probe timeout in seconds
    pub timeout_seconds: u64,
    /// Expected HTTP status code
    pub expected_status: u16,
}

impl Default for HealthProbe {
    fn default() -> Self {
        Self {
            path: "/health".to_string(),
            interval_seconds: 10,
            timeout_seconds: 5,
            expected_status: 200,
        }
    }
}

/// Health check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheckResult {
    /// Whether health check passed
    pub passed: bool,
    /// Response status code (if applicable)
    pub status_code: Option<u16>,
    /// Error message (if failed)
    pub error: Option<String>,
    /// Response time in milliseconds
    pub response_time_ms: u64,
    /// Timestamp of check
    pub timestamp: i64,
}

impl HealthCheckResult {
    /// Create a successful health check result
    pub fn success(status_code: u16, response_time_ms: u64) -> Self {
        Self {
            passed: true,
            status_code: Some(status_code),
            error: None,
            response_time_ms,
            timestamp: Utc::now().timestamp(),
        }
    }

    /// Create a failed health check result
    pub fn failure(error: String, response_time_ms: u64) -> Self {
        Self {
            passed: false,
            status_code: None,
            error: Some(error),
            response_time_ms,
            timestamp: Utc::now().timestamp(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_circuit_breaker_closed_state() {
        let config = CircuitBreakerConfig::default();
        let breaker = CircuitBreaker::new(config);

        assert_eq!(breaker.state().await, CircuitState::Closed);
        assert!(breaker.is_request_allowed().await);
    }

    #[tokio::test]
    async fn test_circuit_breaker_opens_on_failures() {
        let mut config = CircuitBreakerConfig::default();
        config.failure_threshold = 2;

        let breaker = CircuitBreaker::new(config);

        breaker.record_failure().await;
        assert_eq!(breaker.state().await, CircuitState::Closed);

        breaker.record_failure().await;
        assert_eq!(breaker.state().await, CircuitState::Open);
        assert!(!breaker.is_request_allowed().await);
    }

    #[tokio::test]
    async fn test_circuit_breaker_reset_on_success() {
        let mut config = CircuitBreakerConfig::default();
        config.failure_threshold = 5;

        let breaker = CircuitBreaker::new(config);

        breaker.record_failure().await;
        breaker.record_failure().await;
        assert_eq!(breaker.failure_count.load(Ordering::Relaxed), 2);

        breaker.record_success().await;
        assert_eq!(breaker.failure_count.load(Ordering::Relaxed), 0);
    }

    #[tokio::test]
    async fn test_circuit_breaker_half_open_state() {
        let mut config = CircuitBreakerConfig::default();
        config.failure_threshold = 1;
        config.success_threshold = 1;
        config.timeout_seconds = 0; // Immediate timeout for testing

        let breaker = CircuitBreaker::new(config);

        // Force to open
        breaker.record_failure().await;
        assert_eq!(breaker.state().await, CircuitState::Open);

        // Wait and check half-open
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        breaker.record_success().await;

        // Should transition to half-open then closed
        assert_eq!(breaker.state().await, CircuitState::Closed);
    }

    #[test]
    fn test_health_probe_default() {
        let probe = HealthProbe::default();
        assert_eq!(probe.path, "/health");
        assert_eq!(probe.expected_status, 200);
    }

    #[test]
    fn test_health_check_result_success() {
        let result = HealthCheckResult::success(200, 50);
        assert!(result.passed);
        assert_eq!(result.status_code, Some(200));
        assert_eq!(result.response_time_ms, 50);
    }

    #[test]
    fn test_health_check_result_failure() {
        let result = HealthCheckResult::failure("Connection timeout".to_string(), 5000);
        assert!(!result.passed);
        assert_eq!(result.error, Some("Connection timeout".to_string()));
    }
}
