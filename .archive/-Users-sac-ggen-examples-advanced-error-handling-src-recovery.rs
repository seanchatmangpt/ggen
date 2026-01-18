//! Error recovery patterns and graceful degradation
//!
//! This module demonstrates:
//! - Fallback strategies
//! - Retry mechanisms with exponential backoff
//! - Circuit breakers
//! - Graceful degradation
//! - Error recovery without panicking

use crate::errors::{AppError, Result};
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::time::sleep;
use tracing::{debug, info, warn};

/// Retry configuration
#[derive(Debug, Clone)]
pub struct RetryConfig {
    pub max_attempts: usize,
    pub initial_delay: Duration,
    pub max_delay: Duration,
    pub backoff_multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_attempts: 3,
            initial_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(10),
            backoff_multiplier: 2.0,
        }
    }
}

/// Retry a fallible async operation with exponential backoff
///
/// ✅ CORRECT: Never panics, returns Result for caller to handle
pub async fn retry_with_backoff<F, Fut, T>(
    operation: F,
    config: RetryConfig,
    operation_name: &str,
) -> Result<T>
where
    F: Fn() -> Fut,
    Fut: std::future::Future<Output = Result<T>>,
{
    let mut delay = config.initial_delay;

    for attempt in 1..=config.max_attempts {
        match operation().await {
            Ok(result) => {
                if attempt > 1 {
                    info!(
                        "{} succeeded after {} attempts",
                        operation_name, attempt
                    );
                }
                return Ok(result);
            }
            Err(e) if attempt < config.max_attempts => {
                warn!(
                    "{} failed (attempt {}/{}): {}. Retrying in {:?}...",
                    operation_name, attempt, config.max_attempts, e, delay
                );
                sleep(delay).await;

                // Exponential backoff with max delay cap
                delay = Duration::from_millis(
                    ((delay.as_millis() as f64) * config.backoff_multiplier)
                        .min(config.max_delay.as_millis() as f64) as u64,
                );
            }
            Err(e) => {
                return Err(AppError::other(format!(
                    "{} failed after {} attempts: {}",
                    operation_name, config.max_attempts, e
                )));
            }
        }
    }

    unreachable!()
}

/// File reader with fallback locations
///
/// ✅ CORRECT: Tries multiple locations, returns Result
pub fn read_file_with_fallback(paths: &[PathBuf]) -> Result<String> {
    if paths.is_empty() {
        return Err(AppError::validation(
            "paths",
            "At least one path must be provided",
        ));
    }

    let mut errors = Vec::new();

    for path in paths {
        match std::fs::read_to_string(path) {
            Ok(content) => {
                debug!("Successfully read file from: {}", path.display());
                return Ok(content);
            }
            Err(e) => {
                debug!("Failed to read {}: {}", path.display(), e);
                errors.push((path.clone(), e));
            }
        }
    }

    // All attempts failed - construct helpful error
    let error_summary = errors
        .iter()
        .map(|(p, e)| format!("  - {}: {}", p.display(), e))
        .collect::<Vec<_>>()
        .join("\n");

    Err(AppError::other(format!(
        "Failed to read file from any of {} locations:\n{}",
        paths.len(),
        error_summary
    )))
}

/// Configuration loader with defaults
///
/// ✅ CORRECT: Falls back to defaults instead of panicking
pub fn load_config_or_default<T>(path: &Path) -> T
where
    T: Default + serde::de::DeserializeOwned,
{
    match std::fs::read_to_string(path) {
        Ok(content) => match toml::from_str::<T>(&content) {
            Ok(config) => {
                info!("Loaded configuration from {}", path.display());
                config
            }
            Err(e) => {
                warn!(
                    "Failed to parse config from {}: {}. Using defaults.",
                    path.display(),
                    e
                );
                T::default()
            }
        },
        Err(e) => {
            debug!(
                "Config file {} not found: {}. Using defaults.",
                path.display(),
                e
            );
            T::default()
        }
    }
}

/// Safe division that returns Option instead of panicking
///
/// ✅ CORRECT: Returns None instead of dividing by zero
pub fn safe_divide(numerator: f64, denominator: f64) -> Option<f64> {
    if denominator == 0.0 || denominator.is_nan() || numerator.is_nan() {
        None
    } else {
        Some(numerator / denominator)
    }
}

/// Process items with partial success
///
/// ✅ CORRECT: Processes what it can, returns both successes and failures
pub fn process_items_partial<T, R, F>(
    items: Vec<T>,
    processor: F,
) -> (Vec<R>, Vec<(T, AppError)>)
where
    F: Fn(T) -> Result<R>,
{
    let mut successes = Vec::new();
    let mut failures = Vec::new();

    for item in items {
        match processor(item) {
            Ok(result) => successes.push(result),
            Err(e) => failures.push((item, e)),
        }
    }

    (successes, failures)
}

/// Circuit breaker state
#[derive(Debug, Clone, PartialEq)]
pub enum CircuitState {
    Closed,
    Open { until: std::time::Instant },
    HalfOpen,
}

/// Circuit breaker for failing operations
///
/// ✅ CORRECT: Prevents cascading failures without panicking
pub struct CircuitBreaker {
    state: CircuitState,
    failure_count: usize,
    failure_threshold: usize,
    timeout: Duration,
    last_failure_time: Option<std::time::Instant>,
}

impl CircuitBreaker {
    pub fn new(failure_threshold: usize, timeout: Duration) -> Self {
        Self {
            state: CircuitState::Closed,
            failure_count: 0,
            failure_threshold,
            timeout,
            last_failure_time: None,
        }
    }

    /// Execute operation with circuit breaker protection
    pub fn call<F, T>(&mut self, operation: F) -> Result<T>
    where
        F: FnOnce() -> Result<T>,
    {
        // Check if we should attempt the operation
        match &self.state {
            CircuitState::Open { until } => {
                if std::time::Instant::now() < *until {
                    return Err(AppError::other(format!(
                        "Circuit breaker is open. Retry after {:?}",
                        until.duration_since(std::time::Instant::now())
                    )));
                }
                // Transition to half-open
                self.state = CircuitState::HalfOpen;
            }
            CircuitState::HalfOpen | CircuitState::Closed => {}
        }

        // Execute operation
        match operation() {
            Ok(result) => {
                // Success - reset circuit
                self.failure_count = 0;
                self.state = CircuitState::Closed;
                self.last_failure_time = None;
                Ok(result)
            }
            Err(e) => {
                // Failure - increment counter
                self.failure_count += 1;
                self.last_failure_time = Some(std::time::Instant::now());

                if self.failure_count >= self.failure_threshold {
                    // Open the circuit
                    let until = std::time::Instant::now() + self.timeout;
                    self.state = CircuitState::Open { until };
                    warn!(
                        "Circuit breaker opened after {} failures. Will retry after {:?}",
                        self.failure_count, self.timeout
                    );
                }

                Err(e)
            }
        }
    }

    pub fn state(&self) -> &CircuitState {
        &self.state
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[tokio::test]
    async fn test_retry_success_on_second_attempt() {
        let attempt_counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = attempt_counter.clone();

        let operation = move || {
            let counter = counter_clone.clone();
            async move {
                let attempt = counter.fetch_add(1, Ordering::SeqCst) + 1;
                if attempt == 1 {
                    Err(AppError::other("First attempt fails"))
                } else {
                    Ok("success")
                }
            }
        };

        let config = RetryConfig {
            max_attempts: 3,
            initial_delay: Duration::from_millis(10),
            max_delay: Duration::from_secs(1),
            backoff_multiplier: 2.0,
        };

        let result = retry_with_backoff(operation, config, "test_operation").await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "success");
    }

    #[test]
    fn test_read_file_with_fallback_success() {
        use tempfile::NamedTempFile;
        use std::io::Write;

        let mut temp_file = NamedTempFile::new().unwrap();
        write!(temp_file, "test content").unwrap();

        let paths = vec![
            PathBuf::from("/nonexistent/file.txt"),
            temp_file.path().to_path_buf(),
        ];

        let result = read_file_with_fallback(&paths);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "test content");
    }

    #[test]
    fn test_safe_divide() {
        assert_eq!(safe_divide(10.0, 2.0), Some(5.0));
        assert_eq!(safe_divide(10.0, 0.0), None);
        assert_eq!(safe_divide(f64::NAN, 2.0), None);
    }

    #[test]
    fn test_process_items_partial() {
        let items = vec![1, 2, 0, 4, 0, 6];

        let processor = |x: i32| {
            if x == 0 {
                Err(AppError::validation("value", "Cannot be zero"))
            } else {
                Ok(x * 2)
            }
        };

        let (successes, failures) = process_items_partial(items, processor);

        assert_eq!(successes, vec![2, 4, 8, 12]);
        assert_eq!(failures.len(), 2);
    }

    #[test]
    fn test_circuit_breaker() {
        let mut cb = CircuitBreaker::new(3, Duration::from_millis(100));

        // First 2 failures - circuit stays closed
        for _ in 0..2 {
            let result = cb.call(|| Err(AppError::other("failure")));
            assert!(result.is_err());
            assert_eq!(cb.state(), &CircuitState::Closed);
        }

        // Third failure - circuit opens
        let result = cb.call(|| Err(AppError::other("failure")));
        assert!(result.is_err());
        assert!(matches!(cb.state(), CircuitState::Open { .. }));

        // Circuit is open - operation not attempted
        let result = cb.call(|| Ok("success"));
        assert!(result.is_err());
    }
}
