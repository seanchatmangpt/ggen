//! Chaos engineering operations for testing system resilience
//!
//! Provides chaos scenarios and execution framework for validating graceful degradation
//! and recovery mechanisms in containerized environments.

use crate::testing::failure_injector::{FailureInjector, InjectionResult};
use std::time::{Duration, Instant};
use thiserror::Error;

/// Chaos engineering error types
#[derive(Debug, Error)]
pub enum ChaosError {
    /// Container operation failed
    #[error("Container operation failed: {0}")]
    ContainerError(String),

    /// Failure injection failed
    #[error("Failure injection failed: {0}")]
    InjectionFailed(String),

    /// Recovery verification failed
    #[error("Recovery verification failed: {0}")]
    RecoveryFailed(String),

    /// Timeout during chaos scenario
    #[error("Timeout during chaos scenario: {0}")]
    Timeout(String),

    /// Invalid scenario configuration
    #[error("Invalid scenario configuration: {0}")]
    InvalidConfig(String),
}

/// Result type for chaos operations
pub type Result<T> = std::result::Result<T, ChaosError>;

/// Chaos engineering scenarios
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChaosScenario {
    /// Kill container suddenly (simulates crash)
    ContainerFailure {
        /// Container ID or name
        container_id: String,
        /// Whether to verify recovery
        verify_recovery: bool,
    },

    /// Pause container to simulate network partition
    NetworkPartition {
        /// Container ID or name
        container_id: String,
        /// Duration of partition
        duration: Duration,
    },

    /// Set resource limits to simulate exhaustion
    ResourceExhaustion {
        /// Container ID or name
        container_id: String,
        /// Memory limit in bytes (None = unlimited)
        memory_limit: Option<u64>,
        /// CPU quota (0-100000, where 100000 = 1 CPU)
        cpu_quota: Option<i64>,
    },

    /// Multiple failures occurring concurrently
    ConcurrentFailures {
        /// List of scenarios to execute concurrently
        scenarios: Vec<ChaosScenario>,
    },
}

/// Recovery verification result
#[derive(Debug, Clone)]
pub struct RecoveryResult {
    /// Whether recovery was successful
    pub success: bool,
    /// Time taken to recover
    pub recovery_time: Duration,
    /// Error message if recovery failed
    pub error_message: Option<String>,
}

impl RecoveryResult {
    /// Create a successful recovery result
    pub fn success(recovery_time: Duration) -> Self {
        RecoveryResult {
            success: true,
            recovery_time,
            error_message: None,
        }
    }

    /// Create a failed recovery result
    pub fn failure(recovery_time: Duration, error: String) -> Self {
        RecoveryResult {
            success: false,
            recovery_time,
            error_message: Some(error),
        }
    }
}

/// Chaos executor for running chaos scenarios
pub struct ChaosExecutor {
    /// Failure injector for container manipulation
    injector: FailureInjector,
    /// Maximum timeout for recovery verification
    recovery_timeout: Duration,
}

impl ChaosExecutor {
    /// Create a new chaos executor
    ///
    /// # Arguments
    ///
    /// * `docker_host` - Docker daemon socket path (e.g., "unix:///var/run/docker.sock")
    ///
    /// # Errors
    ///
    /// Returns error if Docker connection cannot be established
    pub fn new(docker_host: String) -> Result<Self> {
        let injector = FailureInjector::new(docker_host)
            .map_err(|e| ChaosError::ContainerError(e.to_string()))?;

        Ok(ChaosExecutor {
            injector,
            recovery_timeout: Duration::from_secs(300), // 5 minutes default
        })
    }

    /// Set recovery timeout
    pub fn with_recovery_timeout(mut self, timeout: Duration) -> Self {
        self.recovery_timeout = timeout;
        self
    }

    /// Execute a chaos scenario
    ///
    /// # Arguments
    ///
    /// * `scenario` - Chaos scenario to execute
    ///
    /// # Errors
    ///
    /// Returns error if scenario execution fails
    pub fn execute_scenario(&self, scenario: &ChaosScenario) -> Result<InjectionResult> {
        match scenario {
            ChaosScenario::ContainerFailure {
                container_id,
                verify_recovery: _,
            } => self.simulate_container_failure(container_id),

            ChaosScenario::NetworkPartition {
                container_id,
                duration,
            } => self.simulate_network_partition(container_id, *duration),

            ChaosScenario::ResourceExhaustion {
                container_id,
                memory_limit,
                cpu_quota,
            } => self.simulate_resource_exhaustion(container_id, *memory_limit, *cpu_quota),

            ChaosScenario::ConcurrentFailures { scenarios } => {
                self.execute_concurrent_failures(scenarios)
            }
        }
    }

    /// Simulate sudden container failure (kill)
    fn simulate_container_failure(&self, container_id: &str) -> Result<InjectionResult> {
        self.injector
            .kill_container(container_id)
            .map_err(|e| ChaosError::InjectionFailed(e.to_string()))
    }

    /// Simulate network partition by pausing container
    fn simulate_network_partition(
        &self,
        container_id: &str,
        duration: Duration,
    ) -> Result<InjectionResult> {
        // Pause container
        self.injector
            .pause_container(container_id)
            .map_err(|e| ChaosError::InjectionFailed(e.to_string()))?;

        // Sleep for partition duration
        std::thread::sleep(duration);

        // Unpause container
        self.injector
            .unpause_container(container_id)
            .map_err(|e| ChaosError::InjectionFailed(e.to_string()))
    }

    /// Simulate resource exhaustion by setting limits
    fn simulate_resource_exhaustion(
        &self,
        container_id: &str,
        memory_limit: Option<u64>,
        cpu_quota: Option<i64>,
    ) -> Result<InjectionResult> {
        self.injector
            .set_resource_limits(container_id, memory_limit, cpu_quota)
            .map_err(|e| ChaosError::InjectionFailed(e.to_string()))
    }

    /// Execute multiple scenarios concurrently using threads
    fn execute_concurrent_failures(
        &self,
        scenarios: &[ChaosScenario],
    ) -> Result<InjectionResult> {
        use std::sync::{Arc, Mutex};
        use std::thread;

        let results = Arc::new(Mutex::new(Vec::new()));
        let mut handles = Vec::new();

        for scenario in scenarios {
            let scenario = scenario.clone();
            let results = Arc::clone(&results);
            let docker_host = self.injector.docker_host().to_string();

            let handle = thread::spawn(move || {
                let executor = ChaosExecutor::new(docker_host)?;
                let result = executor.execute_scenario(&scenario)?;
                results
                    .lock()
                    .map_err(|e| {
                        ChaosError::InjectionFailed(format!("Lock poisoned: {}", e))
                    })?
                    .push(result);
                Ok::<(), ChaosError>(())
            });

            handles.push(handle);
        }

        // Wait for all threads to complete
        for handle in handles {
            handle
                .join()
                .map_err(|e| ChaosError::InjectionFailed(format!("Thread panicked: {:?}", e)))?
                .map_err(|e| ChaosError::InjectionFailed(e.to_string()))?;
        }

        // Combine results
        let results = results
            .lock()
            .map_err(|e| ChaosError::InjectionFailed(format!("Lock poisoned: {}", e)))?;

        if results.is_empty() {
            return Err(ChaosError::InjectionFailed(
                "No scenarios executed".to_string(),
            ));
        }

        // Return first result for now (could be improved to aggregate)
        Ok(results[0].clone())
    }

    /// Verify system recovery after chaos injection
    ///
    /// # Arguments
    ///
    /// * `container_id` - Container to verify
    /// * `health_check` - Function to verify container health
    ///
    /// # Errors
    ///
    /// Returns error if recovery verification fails or times out
    pub fn verify_recovery<F>(
        &self,
        container_id: &str,
        health_check: F,
    ) -> Result<RecoveryResult>
    where
        F: Fn(&str) -> Result<bool>,
    {
        let start = Instant::now();

        loop {
            let elapsed = start.elapsed();

            if elapsed > self.recovery_timeout {
                return Ok(RecoveryResult::failure(
                    elapsed,
                    format!(
                        "Recovery timeout after {:?}",
                        self.recovery_timeout
                    ),
                ));
            }

            match health_check(container_id) {
                Ok(true) => {
                    return Ok(RecoveryResult::success(elapsed));
                }
                Ok(false) => {
                    // Not healthy yet, continue waiting
                    std::thread::sleep(Duration::from_secs(1));
                }
                Err(e) => {
                    return Ok(RecoveryResult::failure(
                        elapsed,
                        format!("Health check error: {}", e),
                    ));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chaos_scenario_container_failure() {
        // Arrange
        let scenario = ChaosScenario::ContainerFailure {
            container_id: "test-container".to_string(),
            verify_recovery: true,
        };

        // Act & Assert - verify scenario structure
        match scenario {
            ChaosScenario::ContainerFailure {
                container_id,
                verify_recovery,
            } => {
                assert_eq!(container_id, "test-container");
                assert!(verify_recovery);
            }
            _ => panic!("Wrong scenario type"),
        }
    }

    #[test]
    fn test_chaos_scenario_network_partition() {
        // Arrange
        let duration = Duration::from_secs(30);
        let scenario = ChaosScenario::NetworkPartition {
            container_id: "test-container".to_string(),
            duration,
        };

        // Act & Assert
        match scenario {
            ChaosScenario::NetworkPartition {
                container_id,
                duration: d,
            } => {
                assert_eq!(container_id, "test-container");
                assert_eq!(d, duration);
            }
            _ => panic!("Wrong scenario type"),
        }
    }

    #[test]
    fn test_chaos_scenario_resource_exhaustion() {
        // Arrange
        let scenario = ChaosScenario::ResourceExhaustion {
            container_id: "test-container".to_string(),
            memory_limit: Some(100_000_000), // 100MB
            cpu_quota: Some(50_000),         // 0.5 CPU
        };

        // Act & Assert
        match scenario {
            ChaosScenario::ResourceExhaustion {
                container_id,
                memory_limit,
                cpu_quota,
            } => {
                assert_eq!(container_id, "test-container");
                assert_eq!(memory_limit, Some(100_000_000));
                assert_eq!(cpu_quota, Some(50_000));
            }
            _ => panic!("Wrong scenario type"),
        }
    }

    #[test]
    fn test_recovery_result_success() {
        // Arrange
        let duration = Duration::from_secs(10);

        // Act
        let result = RecoveryResult::success(duration);

        // Assert
        assert!(result.success);
        assert_eq!(result.recovery_time, duration);
        assert!(result.error_message.is_none());
    }

    #[test]
    fn test_recovery_result_failure() {
        // Arrange
        let duration = Duration::from_secs(300);
        let error = "Timeout exceeded".to_string();

        // Act
        let result = RecoveryResult::failure(duration, error.clone());

        // Assert
        assert!(!result.success);
        assert_eq!(result.recovery_time, duration);
        assert_eq!(result.error_message, Some(error));
    }

    #[test]
    fn test_chaos_executor_with_recovery_timeout() {
        // Arrange
        let timeout = Duration::from_secs(600);

        // Act
        let executor_result = ChaosExecutor::new("unix:///var/run/docker.sock".to_string());

        // Assert - verify timeout setting (even if Docker not available)
        if let Ok(executor) = executor_result {
            let executor = executor.with_recovery_timeout(timeout);
            assert_eq!(executor.recovery_timeout, timeout);
        } else {
            // Docker not available in test environment - skip test
            eprintln!("Skipping test - Docker not available");
        }
    }
}
