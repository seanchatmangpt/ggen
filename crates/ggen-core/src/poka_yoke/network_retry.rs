//! Network retry with exponential backoff.
//!
//! Handles transient network failures gracefully.

use std::future::Future;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use ggen_utils::error::{Error, Result};

/// Circuit breaker state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CircuitState {
    Closed,  // Normal operation
    Open,    // Too many failures, reject requests
    HalfOpen, // Testing recovery
}

/// Circuit breaker for preventing cascading failures.
#[derive(Debug)]
struct CircuitBreaker {
    state: CircuitState,
    failures: usize,
    threshold: usize,
    last_failure: Option<Instant>,
    open_duration: Duration,
}

impl CircuitBreaker {
    fn new(threshold: usize, open_duration: Duration) -> Self {
        Self {
            state: CircuitState::Closed,
            failures: 0,
            threshold,
            last_failure: None,
            open_duration,
        }
    }

    fn is_open(&self) -> bool {
        if self.state == CircuitState::Open {
            // Check if enough time has passed to transition to half-open
            if let Some(last_failure) = self.last_failure {
                if last_failure.elapsed() > self.open_duration {
                    // Transition to half-open (will be updated by caller)
                    return false;
                }
            }
            return true;
        }
        false
    }

    fn record_success(&mut self) {
        self.state = CircuitState::Closed;
        self.failures = 0;
        self.last_failure = None;
    }

    fn record_failure(&mut self) {
        self.failures += 1;
        self.last_failure = Some(Instant::now());

        if self.failures >= self.threshold {
            self.state = CircuitState::Open;
        }
    }
}

/// Network retry with exponential backoff and circuit breaker.
///
/// # Strategy
///
/// - Exponential backoff: 1s, 2s, 4s, 8s, ...
/// - Max retries: Configurable
/// - Circuit breaker: Opens after N failures, prevents cascading
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::NetworkRetry;
/// use std::time::Duration;
///
/// async fn fetch_data() -> Result<String, ggen_core::error::Error> {
///     // Simulate network call
///     Ok("data".to_string())
/// }
///
/// async fn example() -> Result<(), ggen_core::error::Error> {
///     let retry = NetworkRetry::new(3, Duration::from_secs(1));
///     let result = retry.execute(|| fetch_data()).await?;
///     Ok(())
/// }
/// ```
pub struct NetworkRetry {
    max_retries: u32,
    initial_backoff: Duration,
    max_backoff: Duration,
    circuit_breaker: Arc<Mutex<CircuitBreaker>>,
}

impl NetworkRetry {
    /// Creates a new network retry handler.
    ///
    /// # Parameters
    ///
    /// - `max_retries`: Maximum retry attempts
    /// - `initial_backoff`: Initial backoff duration
    pub fn new(max_retries: u32, initial_backoff: Duration) -> Self {
        Self {
            max_retries,
            initial_backoff,
            max_backoff: Duration::from_secs(60),
            circuit_breaker: Arc::new(Mutex::new(CircuitBreaker::new(
                5,
                Duration::from_secs(30),
            ))),
        }
    }

    /// Executes operation with retry logic.
    ///
    /// # Errors
    ///
    /// Returns error if all retries exhausted or circuit breaker is open.
    pub async fn execute<F, Fut, T>(&self, mut operation: F) -> Result<T>
    where
        F: FnMut() -> Fut,
        Fut: Future<Output = Result<T>>,
    {
        // Check circuit breaker
        {
            let breaker = self.circuit_breaker.lock().expect("Circuit breaker lock poisoned");
            if breaker.is_open() {
                return Err(Error::network_error(
                    "Circuit breaker open - too many failures",
                ));
            }
        }

        let mut attempt = 0;
        let mut backoff = self.initial_backoff;

        loop {
            match operation().await {
                Ok(result) => {
                    self.circuit_breaker
                        .lock()
                        .expect("Circuit breaker lock poisoned")
                        .record_success();
                    return Ok(result);
                }
                Err(e) if self.is_retryable(&e) && attempt < self.max_retries => {
                    attempt += 1;
                    log::warn!(
                        "Network operation failed (attempt {}/{}), retrying after {:?}",
                        attempt,
                        self.max_retries,
                        backoff
                    );

                    // Exponential backoff
                    #[cfg(feature = "async")]
                    tokio::time::sleep(backoff).await;

                    #[cfg(not(feature = "async"))]
                    std::thread::sleep(backoff);

                    backoff = (backoff * 2).min(self.max_backoff);
                }
                Err(e) => {
                    self.circuit_breaker
                        .lock()
                        .expect("Circuit breaker lock poisoned")
                        .record_failure();
                    return Err(e);
                }
            }
        }
    }

    /// Checks if error is retryable.
    fn is_retryable(&self, error: &Error) -> bool {
        let msg = error.to_string().to_lowercase();
        msg.contains("network")
            || msg.contains("timeout")
            || msg.contains("connection")
            || msg.contains("dns")
            || msg.contains("temporary")
    }
}

impl Default for NetworkRetry {
    fn default() -> Self {
        Self::new(3, Duration::from_secs(1))
    }
}
