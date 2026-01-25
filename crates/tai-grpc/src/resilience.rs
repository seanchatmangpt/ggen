//! Resilience patterns: circuit breaker, retries, timeouts, load balancing

use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime};

use parking_lot::RwLock;
use tokio::time::sleep;
use tracing::{debug, warn};

use crate::Result as GrpcResult;
use crate::GrpcError;

/// Circuit breaker states
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CircuitState {
    /// Circuit is closed - requests pass through normally
    Closed,
    /// Circuit is open - requests fail fast
    Open,
    /// Circuit is half-open - testing if service recovered
    HalfOpen,
}

/// Configuration for circuit breaker behavior
#[derive(Debug, Clone)]
pub struct CircuitBreakerConfig {
    /// Number of failures before opening the circuit
    pub failure_threshold: u32,
    /// Time the circuit stays open before trying again
    pub open_timeout: Duration,
    /// Maximum number of half-open requests
    pub half_open_max_calls: u32,
}

impl Default for CircuitBreakerConfig {
    fn default() -> Self {
        Self {
            failure_threshold: 5,
            open_timeout: Duration::from_secs(30),
            half_open_max_calls: 3,
        }
    }
}

/// Circuit breaker for preventing cascading failures
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    inner: Arc<CircuitBreakerInner>,
}

#[derive(Debug)]
struct CircuitBreakerInner {
    state: RwLock<CircuitState>,
    failure_count: RwLock<u32>,
    success_count: RwLock<u32>,
    last_failure_time: RwLock<Option<Instant>>,
    config: CircuitBreakerConfig,
}

impl CircuitBreaker {
    /// Create a new circuit breaker with given configuration
    pub fn new(config: CircuitBreakerConfig) -> Self {
        Self {
            inner: Arc::new(CircuitBreakerInner {
                state: RwLock::new(CircuitState::Closed),
                failure_count: RwLock::new(0),
                success_count: RwLock::new(0),
                last_failure_time: RwLock::new(None),
                config,
            }),
        }
    }

    /// Get the current state of the circuit
    pub fn state(&self) -> CircuitState {
        *self.inner.state.read()
    }

    /// Record a successful call
    pub fn record_success(&self) {
        let mut state = self.inner.state.write();
        match *state {
            CircuitState::Closed => {
                // Reset failure count on success in closed state
                *self.inner.failure_count.write() = 0;
            }
            CircuitState::HalfOpen => {
                // Transition to closed if threshold reached
                let mut success_count = self.inner.success_count.write();
                *success_count += 1;
                if *success_count >= self.inner.config.half_open_max_calls {
                    *state = CircuitState::Closed;
                    *self.inner.failure_count.write() = 0;
                    *success_count = 0;
                    debug!("Circuit breaker closed: service recovered");
                }
            }
            CircuitState::Open => {
                // Stay open, don't count successes until half-open
            }
        }
    }

    /// Record a failed call
    pub fn record_failure(&self) {
        let mut state = self.inner.state.write();
        *self.inner.last_failure_time.write() = Some(Instant::now());

        match *state {
            CircuitState::Closed => {
                let mut failure_count = self.inner.failure_count.write();
                *failure_count += 1;
                if *failure_count >= self.inner.config.failure_threshold {
                    *state = CircuitState::Open;
                    *self.inner.success_count.write() = 0;
                    warn!(
                        "Circuit breaker opened: {} failures recorded",
                        *failure_count
                    );
                }
            }
            CircuitState::HalfOpen => {
                // Any failure in half-open state reopens the circuit
                *state = CircuitState::Open;
                *self.inner.failure_count.write() = 0;
                *self.inner.success_count.write() = 0;
                warn!("Circuit breaker reopened: failure during recovery");
            }
            CircuitState::Open => {
                // Stay open
            }
        }
    }

    /// Check if circuit should transition from open to half-open
    pub async fn check_recovery(&self) {
        let state = *self.inner.state.read();
        if state != CircuitState::Open {
            return;
        }

        if let Some(last_failure) = *self.inner.last_failure_time.read() {
            if last_failure.elapsed() >= self.inner.config.open_timeout {
                let mut state = self.inner.state.write();
                if *state == CircuitState::Open {
                    *state = CircuitState::HalfOpen;
                    *self.inner.failure_count.write() = 0;
                    *self.inner.success_count.write() = 0;
                    debug!("Circuit breaker half-open: testing recovery");
                }
            }
        }
    }

    /// Check if a call is allowed (circuit not open)
    pub fn is_call_allowed(&self) -> GrpcResult<()> {
        let state = *self.inner.state.read();
        match state {
            CircuitState::Open => Err(GrpcError::CircuitBreakerOpen {
                service: "service".to_string(),
                reason: "too many failures".to_string(),
            }),
            _ => Ok(()),
        }
    }
}

/// Configuration for retry behavior
#[derive(Debug, Clone)]
pub struct RetryConfig {
    /// Maximum number of retry attempts
    pub max_retries: u32,
    /// Initial backoff duration
    pub initial_backoff: Duration,
    /// Maximum backoff duration
    pub max_backoff: Duration,
    /// Backoff multiplier for exponential backoff
    pub backoff_multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            initial_backoff: Duration::from_millis(100),
            max_backoff: Duration::from_secs(10),
            backoff_multiplier: 2.0,
        }
    }
}

/// Retry with exponential backoff
pub struct RetryStrategy {
    config: RetryConfig,
}

impl RetryStrategy {
    /// Create a new retry strategy
    pub fn new(config: RetryConfig) -> Self {
        Self { config }
    }

    /// Execute a function with retries
    pub async fn execute<F, T>(&self, mut f: F) -> GrpcResult<T>
    where
        F: FnMut() -> futures::future::BoxFuture<'static, GrpcResult<T>>,
    {
        let mut backoff = self.config.initial_backoff;
        let mut attempt = 0;

        loop {
            match f().await {
                Ok(result) => return Ok(result),
                Err(err) => {
                    attempt += 1;
                    if attempt > self.config.max_retries {
                        return Err(err);
                    }

                    // Check if error is retryable
                    if !Self::is_retryable(&err) {
                        return Err(err);
                    }

                    debug!("Retry attempt {}/{}", attempt, self.config.max_retries);
                    sleep(backoff).await;

                    // Calculate next backoff
                    let next_backoff = Duration::from_millis(
                        (backoff.as_millis() as f64 * self.config.backoff_multiplier) as u64,
                    );
                    backoff = next_backoff.min(self.config.max_backoff);
                }
            }
        }
    }

    /// Determine if an error is retryable
    fn is_retryable(err: &GrpcError) -> bool {
        matches!(
            err,
            GrpcError::ConnectionError(_)
                | GrpcError::TimeoutError { .. }
                | GrpcError::ServiceUnavailable(_)
        )
    }
}

/// Load balancing strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadBalancingStrategy {
    /// Round-robin: rotate through endpoints
    RoundRobin,
    /// Least loaded: send to endpoint with fewest connections
    LeastLoaded,
    /// Random: randomly select endpoint
    Random,
}

/// Endpoint with load information
#[derive(Debug, Clone)]
pub struct Endpoint {
    /// Address of the endpoint
    pub address: String,
    /// Current number of active connections
    pub active_connections: u32,
    /// Total number of requests handled
    pub total_requests: u64,
    /// Number of failures
    pub failures: u32,
}

/// Load balancer for distributing requests across endpoints
#[derive(Debug, Clone)]
pub struct LoadBalancer {
    endpoints: Arc<RwLock<Vec<Endpoint>>>,
    strategy: LoadBalancingStrategy,
    current_index: Arc<RwLock<usize>>,
}

impl LoadBalancer {
    /// Create a new load balancer
    pub fn new(endpoints: Vec<String>, strategy: LoadBalancingStrategy) -> Self {
        let endpoints = endpoints
            .into_iter()
            .map(|address| Endpoint {
                address,
                active_connections: 0,
                total_requests: 0,
                failures: 0,
            })
            .collect();

        Self {
            endpoints: Arc::new(RwLock::new(endpoints)),
            strategy,
            current_index: Arc::new(RwLock::new(0)),
        }
    }

    /// Select the next endpoint based on strategy
    pub fn select_endpoint(&self) -> GrpcResult<Endpoint> {
        let endpoints = self.endpoints.read();

        if endpoints.is_empty() {
            return Err(GrpcError::ServiceUnavailable("No endpoints available".to_string()));
        }

        let selected = match self.strategy {
            LoadBalancingStrategy::RoundRobin => {
                let mut index = self.current_index.write();
                let endpoint = endpoints[*index % endpoints.len()].clone();
                *index = (*index + 1) % endpoints.len();
                endpoint
            }
            LoadBalancingStrategy::LeastLoaded => {
                endpoints
                    .iter()
                    .min_by_key(|e| e.active_connections)
                    .cloned()
                    .unwrap_or_else(|| endpoints[0].clone())
            }
            LoadBalancingStrategy::Random => {
                use std::collections::hash_map::RandomState;
                use std::hash::{BuildHasher, Hasher};

                let mut hasher = RandomState::new().build_hasher();
                let now = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap_or_default();
                hasher.write_u128(now.as_nanos());
                let index = (hasher.finish() as usize) % endpoints.len();
                endpoints[index].clone()
            }
        };

        Ok(selected)
    }

    /// Update endpoint connection count
    pub fn record_connection(&self, address: &str) {
        let mut endpoints = self.endpoints.write();
        if let Some(endpoint) = endpoints.iter_mut().find(|e| e.address == address) {
            endpoint.active_connections = endpoint.active_connections.saturating_add(1);
            endpoint.total_requests += 1;
        }
    }

    /// Decrement endpoint connection count
    pub fn release_connection(&self, address: &str) {
        let mut endpoints = self.endpoints.write();
        if let Some(endpoint) = endpoints.iter_mut().find(|e| e.address == address) {
            endpoint.active_connections = endpoint.active_connections.saturating_sub(1);
        }
    }

    /// Record a failure on an endpoint
    pub fn record_failure(&self, address: &str) {
        let mut endpoints = self.endpoints.write();
        if let Some(endpoint) = endpoints.iter_mut().find(|e| e.address == address) {
            endpoint.failures += 1;
        }
    }

    /// Get endpoint statistics
    pub fn get_stats(&self) -> Vec<Endpoint> {
        self.endpoints.read().clone()
    }
}

/// Timeout configuration
#[derive(Debug, Clone)]
pub struct TimeoutConfig {
    /// Default timeout for all RPC calls
    pub default_timeout: Duration,
    /// Maximum timeout allowed
    pub max_timeout: Duration,
}

impl Default for TimeoutConfig {
    fn default() -> Self {
        Self {
            default_timeout: Duration::from_secs(30),
            max_timeout: Duration::from_secs(60),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_breaker_state_transitions() {
        let config = CircuitBreakerConfig {
            failure_threshold: 3,
            open_timeout: Duration::from_millis(100),
            half_open_max_calls: 2,
        };
        let cb = CircuitBreaker::new(config);

        assert_eq!(cb.state(), CircuitState::Closed);

        // Simulate failures
        cb.record_failure();
        cb.record_failure();
        assert_eq!(cb.state(), CircuitState::Closed);

        cb.record_failure();
        assert_eq!(cb.state(), CircuitState::Open);

        // Record success when open - state should not change
        cb.record_success();
        assert_eq!(cb.state(), CircuitState::Open);
    }

    #[test]
    fn test_load_balancer_round_robin() {
        let lb = LoadBalancer::new(
            vec!["addr1".to_string(), "addr2".to_string(), "addr3".to_string()],
            LoadBalancingStrategy::RoundRobin,
        );

        let ep1 = lb.select_endpoint().unwrap();
        let ep2 = lb.select_endpoint().unwrap();
        let ep3 = lb.select_endpoint().unwrap();
        let ep4 = lb.select_endpoint().unwrap();

        assert_eq!(ep1.address, "addr1");
        assert_eq!(ep2.address, "addr2");
        assert_eq!(ep3.address, "addr3");
        assert_eq!(ep4.address, "addr1"); // Wraps around
    }

    #[test]
    fn test_load_balancer_least_loaded() {
        let lb = LoadBalancer::new(
            vec!["addr1".to_string(), "addr2".to_string()],
            LoadBalancingStrategy::LeastLoaded,
        );

        // Simulate connections
        lb.record_connection("addr1");
        lb.record_connection("addr1");
        lb.record_connection("addr2");

        let ep = lb.select_endpoint().unwrap();
        assert_eq!(ep.address, "addr2"); // Has fewer connections
    }
}
