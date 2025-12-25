//! Circuit Breaker pattern for fault tolerance
//!
//! Prevents cascading failures by temporarily disabling agents
//! that are experiencing high failure rates.

use dashmap::DashMap;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::time::{Duration, Instant};
use tracing::{debug, info, warn};

/// Circuit breaker state
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CircuitState {
    /// Circuit is closed - normal operation
    Closed,
    /// Circuit is open - rejecting requests
    Open,
    /// Circuit is half-open - testing if service recovered
    HalfOpen,
}

/// Per-agent circuit breaker state
#[derive(Debug)]
struct AgentCircuit {
    /// Current state
    state: RwLock<CircuitState>,
    /// Consecutive failure count
    failure_count: AtomicU32,
    /// Success count in half-open state
    half_open_successes: AtomicU32,
    /// Last failure timestamp
    last_failure: RwLock<Option<Instant>>,
    /// Last state change timestamp
    last_state_change: RwLock<Instant>,
    /// Total successes
    total_successes: AtomicU64,
    /// Total failures
    total_failures: AtomicU64,
}

impl AgentCircuit {
    fn new() -> Self {
        Self {
            state: RwLock::new(CircuitState::Closed),
            failure_count: AtomicU32::new(0),
            half_open_successes: AtomicU32::new(0),
            last_failure: RwLock::new(None),
            last_state_change: RwLock::new(Instant::now()),
            total_successes: AtomicU64::new(0),
            total_failures: AtomicU64::new(0),
        }
    }
}

/// Circuit breaker manager
#[derive(Debug)]
pub struct CircuitBreaker {
    /// Per-agent circuits
    circuits: DashMap<String, AgentCircuit>,
    /// Failure threshold to open circuit
    failure_threshold: u32,
    /// Time to wait before attempting reset (seconds)
    reset_timeout_secs: u64,
    /// Required successes in half-open to close circuit
    half_open_success_threshold: u32,
}

impl CircuitBreaker {
    /// Create a new circuit breaker
    pub fn new(failure_threshold: u32) -> Self {
        Self {
            circuits: DashMap::new(),
            failure_threshold,
            reset_timeout_secs: 30,
            half_open_success_threshold: 3,
        }
    }

    /// Create with custom configuration
    pub fn with_config(
        failure_threshold: u32,
        reset_timeout_secs: u64,
        half_open_success_threshold: u32,
    ) -> Self {
        Self {
            circuits: DashMap::new(),
            failure_threshold,
            reset_timeout_secs,
            half_open_success_threshold,
        }
    }

    /// Check if circuit is open (blocking requests)
    ///
    /// Uses single write lock to prevent TOCTOU race conditions during state transitions.
    pub fn is_open(&self, agent_id: &str) -> bool {
        let circuit = self
            .circuits
            .entry(agent_id.to_string())
            .or_insert_with(AgentCircuit::new);

        // Use single write lock for atomic check-and-transition (prevents TOCTOU)
        let mut state = circuit.state.write();
        let mut last_change = circuit.last_state_change.write();

        match *state {
            CircuitState::Open => {
                // Check if we should transition to half-open
                if last_change.elapsed() >= Duration::from_secs(self.reset_timeout_secs) {
                    // Atomic transition to half-open
                    *state = CircuitState::HalfOpen;
                    *last_change = Instant::now();
                    circuit.half_open_successes.store(0, Ordering::Relaxed);
                    info!("Circuit for {} transitioning to half-open", agent_id);
                    false
                } else {
                    true
                }
            }
            CircuitState::HalfOpen | CircuitState::Closed => false,
        }
    }

    /// Record a successful execution
    ///
    /// Uses single write lock for atomic state transitions (prevents TOCTOU).
    pub fn record_success(&self, agent_id: &str) {
        let circuit = self
            .circuits
            .entry(agent_id.to_string())
            .or_insert_with(AgentCircuit::new);

        circuit.total_successes.fetch_add(1, Ordering::Relaxed);

        // Use single write lock for atomic check-and-transition
        let mut state = circuit.state.write();
        match *state {
            CircuitState::Closed => {
                // Reset failure count on success
                circuit.failure_count.store(0, Ordering::Relaxed);
            }
            CircuitState::HalfOpen => {
                let successes = circuit.half_open_successes.fetch_add(1, Ordering::Relaxed) + 1;
                if successes >= self.half_open_success_threshold {
                    // Atomic transition to closed
                    *state = CircuitState::Closed;
                    *circuit.last_state_change.write() = Instant::now();
                    circuit.failure_count.store(0, Ordering::Relaxed);
                    info!("Circuit for {} closed after recovery", agent_id);
                }
            }
            CircuitState::Open => {
                // Shouldn't happen, but handle gracefully
                debug!("Success recorded for open circuit: {}", agent_id);
            }
        }
    }

    /// Record a failed execution
    ///
    /// Uses single write lock for atomic state transitions (prevents TOCTOU).
    pub fn record_failure(&self, agent_id: &str) {
        let circuit = self
            .circuits
            .entry(agent_id.to_string())
            .or_insert_with(AgentCircuit::new);

        circuit.total_failures.fetch_add(1, Ordering::Relaxed);
        *circuit.last_failure.write() = Some(Instant::now());

        // Use single write lock for atomic check-and-transition
        let mut state = circuit.state.write();
        match *state {
            CircuitState::Closed => {
                let failures = circuit.failure_count.fetch_add(1, Ordering::Relaxed) + 1;
                if failures >= self.failure_threshold {
                    // Atomic transition to open
                    *state = CircuitState::Open;
                    *circuit.last_state_change.write() = Instant::now();
                    warn!(
                        "Circuit for {} opened after {} failures",
                        agent_id, failures
                    );
                }
            }
            CircuitState::HalfOpen => {
                // Any failure in half-open returns to open (atomic)
                *state = CircuitState::Open;
                *circuit.last_state_change.write() = Instant::now();
                circuit.half_open_successes.store(0, Ordering::Relaxed);
                warn!("Circuit for {} re-opened after half-open failure", agent_id);
            }
            CircuitState::Open => {
                // Already open, just record the failure
                circuit.failure_count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    /// Get the current state of an agent's circuit
    pub fn get_state(&self, agent_id: &str) -> CircuitState {
        self.circuits
            .get(agent_id)
            .map(|c| *c.state.read())
            .unwrap_or(CircuitState::Closed)
    }

    /// Manually reset a circuit
    pub fn reset(&self, agent_id: &str) {
        if let Some(circuit) = self.circuits.get(agent_id) {
            *circuit.state.write() = CircuitState::Closed;
            circuit.failure_count.store(0, Ordering::Relaxed);
            circuit.half_open_successes.store(0, Ordering::Relaxed);
            *circuit.last_state_change.write() = Instant::now();
            info!("Circuit for {} manually reset", agent_id);
        }
    }

    /// Count open circuits
    pub fn open_count(&self) -> usize {
        self.circuits
            .iter()
            .filter(|c| *c.state.read() == CircuitState::Open)
            .count()
    }

    /// Get statistics for all circuits
    pub fn statistics(&self) -> Vec<CircuitStatistics> {
        self.circuits
            .iter()
            .map(|entry| {
                let circuit = entry.value();
                CircuitStatistics {
                    agent_id: entry.key().clone(),
                    state: *circuit.state.read(),
                    failure_count: circuit.failure_count.load(Ordering::Relaxed),
                    total_successes: circuit.total_successes.load(Ordering::Relaxed),
                    total_failures: circuit.total_failures.load(Ordering::Relaxed),
                    last_state_change: circuit.last_state_change.read().elapsed().as_secs(),
                }
            })
            .collect()
    }
}

/// Circuit statistics for an agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CircuitStatistics {
    /// Agent ID
    pub agent_id: String,
    /// Current circuit state
    pub state: CircuitState,
    /// Current failure count
    pub failure_count: u32,
    /// Total successful executions
    pub total_successes: u64,
    /// Total failed executions
    pub total_failures: u64,
    /// Seconds since last state change
    pub last_state_change: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circuit_starts_closed() {
        let cb = CircuitBreaker::new(3);
        assert!(!cb.is_open("test-agent"));
        assert_eq!(cb.get_state("test-agent"), CircuitState::Closed);
    }

    #[test]
    fn test_circuit_opens_on_threshold() {
        let cb = CircuitBreaker::new(3);
        let agent = "test-agent";

        cb.record_failure(agent);
        cb.record_failure(agent);
        assert!(!cb.is_open(agent));

        cb.record_failure(agent);
        assert!(cb.is_open(agent));
        assert_eq!(cb.get_state(agent), CircuitState::Open);
    }

    #[test]
    fn test_success_resets_failures() {
        let cb = CircuitBreaker::new(3);
        let agent = "test-agent";

        cb.record_failure(agent);
        cb.record_failure(agent);
        cb.record_success(agent);

        // Failure count should be reset
        cb.record_failure(agent);
        cb.record_failure(agent);
        assert!(!cb.is_open(agent));
    }

    #[test]
    fn test_manual_reset() {
        let cb = CircuitBreaker::new(3);
        let agent = "test-agent";

        cb.record_failure(agent);
        cb.record_failure(agent);
        cb.record_failure(agent);
        assert!(cb.is_open(agent));

        cb.reset(agent);
        assert!(!cb.is_open(agent));
        assert_eq!(cb.get_state(agent), CircuitState::Closed);
    }

    #[test]
    fn test_open_count() {
        let cb = CircuitBreaker::new(2);

        cb.record_failure("agent-1");
        cb.record_failure("agent-1");
        cb.record_failure("agent-2");
        cb.record_failure("agent-2");

        assert_eq!(cb.open_count(), 2);
    }

    #[test]
    fn test_statistics() {
        let cb = CircuitBreaker::new(3);
        cb.record_success("agent-1");
        cb.record_failure("agent-1");

        let stats = cb.statistics();
        assert_eq!(stats.len(), 1);
        assert_eq!(stats[0].agent_id, "agent-1");
        assert_eq!(stats[0].total_successes, 1);
        assert_eq!(stats[0].total_failures, 1);
    }
}
