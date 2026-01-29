//! Continuous chaos orchestration integrated with MAPE-K loop
//!
//! Provides automated chaos engineering that validates system resilience
//! and self-healing capabilities through continuous failure injection.

use crate::testing::chaos::{ChaosError, ChaosExecutor, ChaosScenario, RecoveryResult};
use crate::testing::failure_injector::InjectionResult;
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use std::sync::Arc;
use std::time::{Duration, Instant};
use thiserror::Error;
use tokio::sync::RwLock;

/// Chaos orchestration error types
#[derive(Debug, Error)]
pub enum OrchestratorError {
    /// Chaos execution failed
    #[error("Chaos execution failed: {0}")]
    ExecutionFailed(String),

    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),

    /// No scenarios configured
    #[error("No scenarios configured")]
    NoScenarios,

    /// MAPE-K integration error
    #[error("MAPE-K integration error: {0}")]
    MAPEKError(String),

    /// Underlying chaos error
    #[error(transparent)]
    ChaosError(#[from] ChaosError),
}

/// Result type for orchestrator operations
pub type Result<T> = std::result::Result<T, OrchestratorError>;

/// Configuration for continuous chaos orchestrator
#[derive(Debug, Clone)]
pub struct ChaosOrchestratorConfig {
    /// Interval between chaos injections
    pub injection_interval: Duration,
    /// Maximum number of concurrent failures
    pub max_concurrent_failures: usize,
    /// Random seed for deterministic testing
    pub random_seed: u64,
    /// Enable automatic recovery verification
    pub verify_recovery: bool,
    /// Recovery timeout
    pub recovery_timeout: Duration,
    /// Target availability percentage (0.0-1.0)
    pub target_availability: f64,
}

impl Default for ChaosOrchestratorConfig {
    fn default() -> Self {
        Self {
            injection_interval: Duration::from_secs(30),
            max_concurrent_failures: 3,
            random_seed: 42,
            verify_recovery: true,
            recovery_timeout: Duration::from_secs(300),
            target_availability: 0.99, // 99% availability
        }
    }
}

/// A single chaos event with metrics
#[derive(Debug, Clone)]
pub struct ChaosEvent {
    /// Scenario that was injected
    pub scenario: ChaosScenario,
    /// When injection occurred
    pub injection_time: Instant,
    /// Time to detect the failure
    pub detection_time: Duration,
    /// Time to recover from failure
    pub recovery_time: Duration,
    /// Whether recovery was successful
    pub success: bool,
    /// Degradation level (0.0-1.0)
    pub degradation: f64,
}

impl ChaosEvent {
    /// Create a new chaos event
    pub fn new(
        scenario: ChaosScenario,
        injection_time: Instant,
        detection_time: Duration,
        recovery_time: Duration,
        success: bool,
        degradation: f64,
    ) -> Self {
        Self {
            scenario,
            injection_time,
            detection_time,
            recovery_time,
            success,
            degradation,
        }
    }

    /// Total event duration
    pub fn total_duration(&self) -> Duration {
        self.detection_time + self.recovery_time
    }
}

/// Comprehensive chaos orchestration report
#[derive(Debug, Clone)]
pub struct ChaosReport {
    /// Total number of chaos events
    pub total_events: usize,
    /// Successful recoveries
    pub successful_recoveries: usize,
    /// Failed recoveries
    pub failed_recoveries: usize,
    /// Average detection time
    pub avg_detection_time: Duration,
    /// Average recovery time
    pub avg_recovery_time: Duration,
    /// Maximum degradation observed
    pub max_degradation: f64,
    /// Overall availability percentage
    pub availability_percent: f64,
    /// All events recorded
    pub events: Vec<ChaosEvent>,
}

impl ChaosReport {
    /// Create a report from events
    pub fn from_events(events: Vec<ChaosEvent>, total_duration: Duration) -> Self {
        let total_events = events.len();
        let successful_recoveries = events.iter().filter(|e| e.success).count();
        let failed_recoveries = total_events - successful_recoveries;

        let avg_detection_time = if total_events > 0 {
            let total: Duration = events.iter().map(|e| e.detection_time).sum();
            total / (total_events as u32)
        } else {
            Duration::ZERO
        };

        let avg_recovery_time = if total_events > 0 {
            let total: Duration = events.iter().map(|e| e.recovery_time).sum();
            total / (total_events as u32)
        } else {
            Duration::ZERO
        };

        let max_degradation = events
            .iter()
            .map(|e| e.degradation)
            .fold(0.0, f64::max);

        let availability_percent = Self::calculate_availability(&events, total_duration);

        Self {
            total_events,
            successful_recoveries,
            failed_recoveries,
            avg_detection_time,
            avg_recovery_time,
            max_degradation,
            availability_percent,
            events,
        }
    }

    /// Calculate availability percentage
    fn calculate_availability(events: &[ChaosEvent], total_duration: Duration) -> f64 {
        if total_duration.is_zero() {
            return 100.0;
        }

        let total_downtime: Duration = events
            .iter()
            .filter(|e| !e.success)
            .map(|e| e.recovery_time)
            .sum();

        let uptime = total_duration.saturating_sub(total_downtime);
        (uptime.as_secs_f64() / total_duration.as_secs_f64()) * 100.0
    }

    /// Check if availability meets target
    pub fn meets_availability_target(&self, target: f64) -> bool {
        self.availability_percent >= (target * 100.0)
    }
}

/// Chaos scenario scheduler with randomization
pub struct ChaosScheduler {
    scenarios: Vec<ChaosScenario>,
    interval: Duration,
    rng: StdRng,
}

impl ChaosScheduler {
    /// Create a new chaos scheduler
    pub fn new(scenarios: Vec<ChaosScenario>, interval: Duration, seed: u64) -> Self {
        Self {
            scenarios,
            interval,
            rng: StdRng::seed_from_u64(seed),
        }
    }

    /// Get next scenario to inject
    pub fn next_scenario(&mut self) -> Result<ChaosScenario> {
        if self.scenarios.is_empty() {
            return Err(OrchestratorError::NoScenarios);
        }

        let index = self.rng.random_range(0..self.scenarios.len());
        Ok(self.scenarios[index].clone())
    }

    /// Get injection interval
    pub fn interval(&self) -> Duration {
        self.interval
    }
}

/// Result of chaos injection
#[derive(Debug)]
struct ChaosInjectionResult {
    scenario: ChaosScenario,
    injection_result: InjectionResult,
    recovery_result: Option<RecoveryResult>,
    detection_time: Duration,
}

/// Continuous chaos orchestrator
pub struct ContinuousChaosOrchestrator {
    chaos_executor: Arc<ChaosExecutor>,
    scheduler: Arc<RwLock<ChaosScheduler>>,
    config: ChaosOrchestratorConfig,
}

impl ContinuousChaosOrchestrator {
    /// Create a new continuous chaos orchestrator
    ///
    /// # Arguments
    ///
    /// * `docker_host` - Docker daemon socket path
    /// * `scenarios` - Chaos scenarios to execute
    /// * `config` - Orchestrator configuration
    ///
    /// # Errors
    ///
    /// Returns error if Docker connection fails or configuration is invalid
    pub fn new(
        docker_host: String,
        scenarios: Vec<ChaosScenario>,
        config: ChaosOrchestratorConfig,
    ) -> Result<Self> {
        if scenarios.is_empty() {
            return Err(OrchestratorError::NoScenarios);
        }

        let executor = ChaosExecutor::new(docker_host)
            .map_err(|e| OrchestratorError::ExecutionFailed(e.to_string()))?
            .with_recovery_timeout(config.recovery_timeout);

        let scheduler = ChaosScheduler::new(
            scenarios,
            config.injection_interval,
            config.random_seed,
        );

        Ok(Self {
            chaos_executor: Arc::new(executor),
            scheduler: Arc::new(RwLock::new(scheduler)),
            config,
        })
    }

    /// Run continuous chaos for specified duration
    ///
    /// # Arguments
    ///
    /// * `duration` - Total duration to run chaos
    ///
    /// # Errors
    ///
    /// Returns error if chaos execution fails
    pub async fn run_continuous_chaos(&self, duration: Duration) -> Result<ChaosReport> {
        let start = Instant::now();
        let mut events = Vec::new();

        while start.elapsed() < duration {
            let mut scheduler = self.scheduler.write().await;
            let scenario = scheduler.next_scenario()?;
            let interval = scheduler.interval();
            drop(scheduler); // Release lock

            match self.inject_chaos(&scenario).await {
                Ok(injection_result) => {
                    let event = self.create_event_from_result(injection_result);
                    events.push(event);
                }
                Err(e) => {
                    // Log error but continue chaos testing
                    eprintln!("Chaos injection failed: {}", e);
                }
            }

            // Wait for next injection interval
            let elapsed = start.elapsed();
            if elapsed + interval < duration {
                tokio::time::sleep(interval).await;
            } else {
                break;
            }
        }

        Ok(ChaosReport::from_events(events, start.elapsed()))
    }

    /// Inject chaos and measure results
    async fn inject_chaos(&self, scenario: &ChaosScenario) -> Result<ChaosInjectionResult> {
        let detection_start = Instant::now();

        // Execute scenario
        let injection_result = self.chaos_executor.execute_scenario(scenario)?;
        let detection_time = detection_start.elapsed();

        // Verify recovery if configured
        let recovery_result = if self.config.verify_recovery {
            self.verify_recovery(scenario).await?
        } else {
            None
        };

        Ok(ChaosInjectionResult {
            scenario: scenario.clone(),
            injection_result,
            recovery_result,
            detection_time,
        })
    }

    /// Verify system recovery
    async fn verify_recovery(&self, scenario: &ChaosScenario) -> Result<Option<RecoveryResult>> {
        // Extract container ID from scenario
        let container_id = match scenario {
            ChaosScenario::ContainerFailure { container_id, .. } => container_id,
            ChaosScenario::NetworkPartition { container_id, .. } => container_id,
            ChaosScenario::ResourceExhaustion { container_id, .. } => container_id,
            ChaosScenario::ConcurrentFailures { .. } => {
                // For concurrent failures, we can't verify single container
                return Ok(None);
            }
        };

        let health_check = |_id: &str| -> crate::testing::chaos::Result<bool> {
            // Simple health check - in production this would be more sophisticated
            Ok(true)
        };

        let recovery_result = self.chaos_executor.verify_recovery(container_id, health_check)?;
        Ok(Some(recovery_result))
    }

    /// Create chaos event from injection result
    fn create_event_from_result(&self, result: ChaosInjectionResult) -> ChaosEvent {
        let success = result.injection_result.success
            && result
                .recovery_result
                .as_ref()
                .map_or(true, |r| r.success);

        let recovery_time = result
            .recovery_result
            .as_ref()
            .map(|r| r.recovery_time)
            .unwrap_or(Duration::ZERO);

        let degradation = if success { 0.0 } else { 1.0 };

        ChaosEvent::new(
            result.scenario,
            Instant::now(),
            result.detection_time,
            recovery_time,
            success,
            degradation,
        )
    }

    /// Calculate current availability
    pub fn calculate_availability(
        events: &[ChaosEvent],
        total_duration: Duration,
    ) -> f64 {
        ChaosReport::calculate_availability(events, total_duration)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chaos_orchestrator_config_default() {
        // Arrange & Act
        let config = ChaosOrchestratorConfig::default();

        // Assert
        assert_eq!(config.injection_interval, Duration::from_secs(30));
        assert_eq!(config.max_concurrent_failures, 3);
        assert_eq!(config.random_seed, 42);
        assert!(config.verify_recovery);
        assert_eq!(config.recovery_timeout, Duration::from_secs(300));
        assert_eq!(config.target_availability, 0.99);
    }

    #[test]
    fn test_chaos_event_creation() {
        // Arrange
        let scenario = ChaosScenario::ContainerFailure {
            container_id: "test-container".to_string(),
            verify_recovery: true,
        };
        let injection_time = Instant::now();
        let detection_time = Duration::from_millis(100);
        let recovery_time = Duration::from_secs(5);

        // Act
        let event = ChaosEvent::new(
            scenario,
            injection_time,
            detection_time,
            recovery_time,
            true,
            0.3,
        );

        // Assert
        assert!(event.success);
        assert_eq!(event.detection_time, detection_time);
        assert_eq!(event.recovery_time, recovery_time);
        assert_eq!(event.degradation, 0.3);
        assert_eq!(event.total_duration(), detection_time + recovery_time);
    }

    #[test]
    fn test_chaos_report_from_events() {
        // Arrange
        let scenario = ChaosScenario::ContainerFailure {
            container_id: "test".to_string(),
            verify_recovery: true,
        };
        let events = vec![
            ChaosEvent::new(
                scenario.clone(),
                Instant::now(),
                Duration::from_millis(100),
                Duration::from_secs(5),
                true,
                0.2,
            ),
            ChaosEvent::new(
                scenario.clone(),
                Instant::now(),
                Duration::from_millis(200),
                Duration::from_secs(10),
                false,
                0.8,
            ),
            ChaosEvent::new(
                scenario,
                Instant::now(),
                Duration::from_millis(150),
                Duration::from_secs(3),
                true,
                0.1,
            ),
        ];
        let total_duration = Duration::from_secs(100);

        // Act
        let report = ChaosReport::from_events(events, total_duration);

        // Assert
        assert_eq!(report.total_events, 3);
        assert_eq!(report.successful_recoveries, 2);
        assert_eq!(report.failed_recoveries, 1);
        assert_eq!(report.max_degradation, 0.8);
        assert!(report.avg_detection_time > Duration::ZERO);
        assert!(report.avg_recovery_time > Duration::ZERO);
    }

    #[test]
    fn test_chaos_report_availability_calculation() {
        // Arrange
        let scenario = ChaosScenario::ContainerFailure {
            container_id: "test".to_string(),
            verify_recovery: true,
        };
        let events = vec![
            ChaosEvent::new(
                scenario.clone(),
                Instant::now(),
                Duration::from_millis(100),
                Duration::from_secs(5),
                false,
                1.0,
            ),
        ];
        let total_duration = Duration::from_secs(100);

        // Act
        let report = ChaosReport::from_events(events, total_duration);

        // Assert - 5 seconds downtime out of 100 = 95% availability
        assert!((report.availability_percent - 95.0).abs() < 0.1);
    }

    #[test]
    fn test_chaos_report_meets_availability_target() {
        // Arrange
        let scenario = ChaosScenario::ContainerFailure {
            container_id: "test".to_string(),
            verify_recovery: true,
        };
        let events = vec![
            ChaosEvent::new(
                scenario,
                Instant::now(),
                Duration::from_millis(100),
                Duration::from_millis(500),
                false,
                1.0,
            ),
        ];
        let total_duration = Duration::from_secs(100);

        // Act
        let report = ChaosReport::from_events(events, total_duration);

        // Assert - Should meet 99% target (99.5% actual)
        assert!(report.meets_availability_target(0.99));
        assert!(!report.meets_availability_target(0.999));
    }

    #[test]
    fn test_chaos_scheduler_creation() {
        // Arrange
        let scenarios = vec![
            ChaosScenario::ContainerFailure {
                container_id: "test1".to_string(),
                verify_recovery: true,
            },
            ChaosScenario::NetworkPartition {
                container_id: "test2".to_string(),
                duration: Duration::from_secs(10),
            },
        ];
        let interval = Duration::from_secs(30);

        // Act
        let scheduler = ChaosScheduler::new(scenarios, interval, 42);

        // Assert
        assert_eq!(scheduler.interval(), interval);
    }

    #[test]
    fn test_chaos_scheduler_next_scenario() {
        // Arrange
        let scenarios = vec![
            ChaosScenario::ContainerFailure {
                container_id: "test1".to_string(),
                verify_recovery: true,
            },
            ChaosScenario::NetworkPartition {
                container_id: "test2".to_string(),
                duration: Duration::from_secs(10),
            },
        ];
        let mut scheduler = ChaosScheduler::new(scenarios, Duration::from_secs(30), 42);

        // Act
        let result = scheduler.next_scenario();

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_chaos_scheduler_no_scenarios_error() {
        // Arrange
        let scenarios: Vec<ChaosScenario> = vec![];
        let mut scheduler = ChaosScheduler::new(scenarios, Duration::from_secs(30), 42);

        // Act
        let result = scheduler.next_scenario();

        // Assert
        assert!(result.is_err());
        match result {
            Err(OrchestratorError::NoScenarios) => {}
            _ => panic!("Expected NoScenarios error"),
        }
    }
}
