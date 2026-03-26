//! Circuit breaker pattern for external API calls
//!
//! Prevents cascading failures by monitoring request/response patterns.
//! States: Closed (normal) -> Open (failing) -> HalfOpen (testing)

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use tracing::{error, info, warn};

/// Circuit breaker state enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum CircuitBreakerState {
    /// Normal operation - requests pass through
    Closed,
    /// Too many failures - requests blocked
    Open,
    /// Testing recovery - limited requests allowed
    HalfOpen,
}

impl std::fmt::Display for CircuitBreakerState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Closed => write!(f, "CLOSED"),
            Self::Open => write!(f, "OPEN"),
            Self::HalfOpen => write!(f, "HALF_OPEN"),
        }
    }
}

/// Configuration for circuit breaker behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitBreakerConfig {
    /// Number of failures before opening circuit
    pub failure_threshold: usize,
    /// Number of successes in half-open state to close circuit
    pub success_threshold: usize,
    /// Duration to wait before transitioning from Open to HalfOpen
    pub reset_timeout_secs: u64,
    /// Maximum number of requests allowed in HalfOpen state
    pub half_open_max_requests: usize,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            success_threshold: 2,
            reset_timeout_secs: 60,
            half_open_max_requests: 3,
        }
    }
}

/// Tracks state transitions and metrics for a circuit breaker
#[derive(Debug, Clone)]
pub struct CircuitBreakerMetrics {
    /// Current state
    pub state: CircuitBreakerState,
    /// Number of consecutive failures
    pub failure_count: usize,
    /// Number of consecutive successes
    pub success_count: usize,
    /// Time of last failure
    pub last_failure_time: Option<DateTime<Utc>>,
    /// When the circuit opened
    pub opened_at: Option<DateTime<Utc>>,
    /// Requests in half-open state
    pub half_open_requests: usize,
    /// Total requests processed
    pub total_requests: u64,
    /// Total failures recorded
    pub total_failures: u64,
}

impl Default for CircuitBreakerMetrics {
    fn default() -> Self {
        Self {
            state: CircuitBreakerState::Closed,
            failure_count: 0,
            success_count: 0,
            last_failure_time: None,
            opened_at: None,
            half_open_requests: 0,
            total_requests: 0,
            total_failures: 0,
        }
    }
}

/// Circuit breaker for protecting external API calls
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    config: CircuitBreakerConfig,
    metrics: Arc<Mutex<CircuitBreakerMetrics>>,
    name: String,
}

impl CircuitBreaker {
    /// Create a new circuit breaker with default configuration
    pub fn new(name: impl Into<String>) -> Self {
        Self::with_config(name, CircuitBreakerConfig::default())
    }

    /// Create a new circuit breaker with custom configuration
    pub fn with_config(name: impl Into<String>, config: CircuitBreakerConfig) -> Self {
        Self {
            config,
            metrics: Arc::new(Mutex::new(CircuitBreakerMetrics::default())),
            name: name.into(),
        }
    }

    /// Get the current state of the circuit breaker
    pub fn state(&self) -> CircuitBreakerState {
        let metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        metrics.state
    }

    /// Record a successful request
    pub fn record_success(&self) {
        let mut metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        metrics.total_requests += 1;

        match metrics.state {
            CircuitBreakerState::Closed => {
                metrics.failure_count = 0;
                info!("Circuit breaker '{}' closed: success recorded", self.name);
            }
            CircuitBreakerState::Open => {
                // Ignore successes when open
                warn!(
                    "Circuit breaker '{}' open: success request rejected",
                    self.name
                );
            }
            CircuitBreakerState::HalfOpen => {
                metrics.success_count += 1;
                metrics.half_open_requests += 1;

                if metrics.success_count >= self.config.success_threshold {
                    metrics.state = CircuitBreakerState::Closed;
                    metrics.failure_count = 0;
                    metrics.success_count = 0;
                    metrics.half_open_requests = 0;
                    info!("Circuit breaker '{}' recovered and closed", self.name);
                }
            }
        }
    }

    /// Record a failed request
    pub fn record_failure(&self) {
        let mut metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        metrics.total_requests += 1;
        metrics.total_failures += 1;
        metrics.failure_count += 1;
        metrics.last_failure_time = Some(Utc::now());

        match metrics.state {
            CircuitBreakerState::Closed => {
                if metrics.failure_count >= self.config.failure_threshold {
                    metrics.state = CircuitBreakerState::Open;
                    metrics.opened_at = Some(Utc::now());
                    error!(
                        "Circuit breaker '{}' opened after {} failures",
                        self.name, self.config.failure_threshold
                    );
                }
            }
            CircuitBreakerState::Open => {
                // Check if we should transition to HalfOpen
                if let Some(opened_at) = metrics.opened_at {
                    let reset_timeout = Duration::seconds(self.config.reset_timeout_secs as i64);
                    if Utc::now() > opened_at + reset_timeout {
                        metrics.state = CircuitBreakerState::HalfOpen;
                        metrics.success_count = 0;
                        metrics.failure_count = 0;
                        metrics.half_open_requests = 0;
                        info!(
                            "Circuit breaker '{}' transitioning to half-open state",
                            self.name
                        );
                    }
                }
            }
            CircuitBreakerState::HalfOpen => {
                metrics.state = CircuitBreakerState::Open;
                metrics.opened_at = Some(Utc::now());
                metrics.success_count = 0;
                metrics.half_open_requests = 0;
                error!(
                    "Circuit breaker '{}' reopened after failure in half-open state",
                    self.name
                );
            }
        }
    }

    /// Check if a request should be allowed
    pub fn can_request(&self) -> bool {
        let mut metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        match metrics.state {
            CircuitBreakerState::Closed => true,
            CircuitBreakerState::Open => {
                // Check if we should transition to HalfOpen
                if let Some(opened_at) = metrics.opened_at {
                    let reset_timeout = Duration::seconds(self.config.reset_timeout_secs as i64);
                    if Utc::now() > opened_at + reset_timeout {
                        metrics.state = CircuitBreakerState::HalfOpen;
                        metrics.success_count = 0;
                        metrics.failure_count = 0;
                        metrics.half_open_requests = 0;
                        info!(
                            "Circuit breaker '{}' transitioning to half-open state",
                            self.name
                        );
                        return metrics.half_open_requests < self.config.half_open_max_requests;
                    }
                }
                false
            }
            CircuitBreakerState::HalfOpen => {
                metrics.half_open_requests < self.config.half_open_max_requests
            }
        }
    }

    /// Get current metrics
    pub fn metrics(&self) -> CircuitBreakerMetrics {
        let metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        metrics.clone()
    }

    /// Reset the circuit breaker to closed state
    pub fn reset(&self) {
        let mut metrics = self
            .metrics
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        metrics.state = CircuitBreakerState::Closed;
        metrics.failure_count = 0;
        metrics.success_count = 0;
        metrics.opened_at = None;
        metrics.half_open_requests = 0;
        info!("Circuit breaker '{}' manually reset", self.name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_breaker_creation() {
        let cb = CircuitBreaker::new("test_api");
        assert_eq!(cb.state(), CircuitBreakerState::Closed);
    }

    #[test]
    fn test_circuit_breaker_closes_after_failures() {
        let cb = CircuitBreaker::new("test_api");

        for _ in 0..5 {
            cb.record_failure();
        }

        assert_eq!(cb.state(), CircuitBreakerState::Open);
        assert!(!cb.can_request());
    }

    #[test]
    fn test_circuit_breaker_success_resets_failures() {
        let cb = CircuitBreaker::new("test_api");

        cb.record_failure();
        cb.record_failure();
        cb.record_success();

        assert_eq!(cb.state(), CircuitBreakerState::Closed);
        assert!(cb.can_request());
    }

    #[test]
    fn test_circuit_breaker_transitions_to_half_open() {
        let config = CircuitBreakerConfig {
            failure_threshold: 2,
            success_threshold: 1,
            reset_timeout_secs: 0,
            half_open_max_requests: 3,
        };
        let cb = CircuitBreaker::with_config("test_api", config);

        // Open the circuit
        cb.record_failure();
        cb.record_failure();
        assert_eq!(cb.state(), CircuitBreakerState::Open);

        // After reset timeout (0 seconds), should transition to half-open
        std::thread::sleep(std::time::Duration::from_millis(10));
        assert!(cb.can_request());
        assert_eq!(cb.state(), CircuitBreakerState::HalfOpen);
    }

    #[test]
    fn test_circuit_breaker_half_open_recovery() {
        let config = CircuitBreakerConfig {
            failure_threshold: 2,
            success_threshold: 1,
            reset_timeout_secs: 0,
            half_open_max_requests: 3,
        };
        let cb = CircuitBreaker::with_config("test_api", config);

        // Open the circuit
        cb.record_failure();
        cb.record_failure();

        // Transition to half-open after timeout
        std::thread::sleep(std::time::Duration::from_millis(10));
        cb.can_request();

        // Successful request closes circuit
        cb.record_success();
        assert_eq!(cb.state(), CircuitBreakerState::Closed);
    }
}
