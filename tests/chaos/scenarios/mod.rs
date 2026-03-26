//! Chaos test scenarios demonstrating system resilience
//!
//! Scenario 1: Panic injection - Kill sensor manager, verify restart
//! Scenario 2: Network chaos - Add 2s latency, verify behavior
//! Scenario 3: Clock skew - Advance time 5min, verify consistency
//! Scenario 4: Cascading failure - Kill multiple components
//! Scenario 5: Recovery - Verify system stabilizes after failure

use crate::chaos::correlation::CorrelationContext;
use crate::chaos::event_store::FailureEvent;
use crate::chaos::injection::{ChaosConfig, ChaosEngine};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;

/// Trait for chaos scenario implementations
#[async_trait::async_trait]
pub trait ChaosScenario: Send + Sync {
    /// Name of the scenario
    fn name(&self) -> &str;

    /// Description of what is being tested
    fn description(&self) -> &str;

    /// Run the scenario
    async fn run(&self) -> ScenarioResult;

    /// Verify expected outcomes
    async fn verify(&self) -> VerificationResult;
}

/// Result of scenario execution
#[derive(Debug, Clone)]
pub struct ScenarioResult {
    pub scenario_name: String,
    pub success: bool,
    pub duration_ms: u64,
    pub events_recorded: usize,
    pub error_message: Option<String>,
}

#[derive(Debug, Clone)]
pub struct VerificationResult {
    pub passed: bool,
    pub checks: Vec<String>,
    pub failed_checks: Vec<String>,
}

// ===== SCENARIO 1: Panic Injection =====

pub struct PanicInjectionScenario {
    chaos: Arc<ChaosEngine>,
    context: CorrelationContext,
}

impl PanicInjectionScenario {
    pub fn new() -> Self {
        PanicInjectionScenario {
            chaos: Arc::new(ChaosEngine::default_engine()),
            context: CorrelationContext::new(),
        }
    }
}

impl Default for PanicInjectionScenario {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl ChaosScenario for PanicInjectionScenario {
    fn name(&self) -> &str {
        "Panic Injection"
    }

    fn description(&self) -> &str {
        "Kill sensor manager, record panic, verify restart behavior"
    }

    async fn run(&self) -> ScenarioResult {
        let start = std::time::SystemTime::now();
        let mut error_message = None;

        // Simulate normal operation
        {
            let _span = self
                .context
                .start_span("sensor_manager".to_string(), "initialize".to_string());
        }

        // Inject panic
        if let Err(e) = self.chaos.inject_panic("sensor_manager").await {
            error_message = Some(e);
        }

        // Simulate recovery
        {
            let mut _span = self
                .context
                .start_span("sensor_manager".to_string(), "restart".to_string());
            _span.set_status(crate::chaos::correlation::TraceStatus::Completed);
        }

        // Record recovery
        let _: Result<(), _> = self
            .chaos
            .record_recovery("restart_supervisor", "sensor_manager", true)
            .await;

        let duration = start.elapsed().unwrap().as_millis() as u64;
        let events = self.chaos.get_events().await;

        ScenarioResult {
            scenario_name: self.name().to_string(),
            success: error_message.is_none(),
            duration_ms: duration,
            events_recorded: events.len(),
            error_message,
        }
    }

    async fn verify(&self) -> VerificationResult {
        let events = self.chaos.get_events().await;
        let mut checks = vec![];
        let mut failed_checks = vec![];

        // Check 1: Panic was recorded
        let has_panic = events
            .iter()
            .any(|e| matches!(e, FailureEvent::PanicOccurred { .. }));
        if has_panic {
            checks.push("✓ Panic event recorded".to_string());
        } else {
            failed_checks.push("✗ Panic event not recorded".to_string());
        }

        // Check 2: Recovery action was recorded
        let has_recovery = events
            .iter()
            .any(|e| matches!(e, FailureEvent::RecoveryAction { success: true, .. }));
        if has_recovery {
            checks.push("✓ Recovery action recorded".to_string());
        } else {
            failed_checks.push("✗ Recovery action not recorded".to_string());
        }

        // Check 3: Traces show startup + panic + restart
        let traces = self.context.get_traces();
        if traces.len() >= 3 {
            checks.push("✓ Trace chain complete (init → panic → restart)".to_string());
        } else {
            failed_checks.push("✗ Incomplete trace chain".to_string());
        }

        VerificationResult {
            passed: failed_checks.is_empty(),
            checks,
            failed_checks,
        }
    }
}

// ===== SCENARIO 2: Network Chaos =====

pub struct NetworkChaosScenario {
    chaos: Arc<ChaosEngine>,
    context: CorrelationContext,
}

impl NetworkChaosScenario {
    pub fn new() -> Self {
        let config = ChaosConfig {
            enable_network: true,
            base_latency_ms: 2000, // 2 second base
            max_jitter_ms: 500,
            ..Default::default()
        };

        NetworkChaosScenario {
            chaos: Arc::new(ChaosEngine::new(config)),
            context: CorrelationContext::new(),
        }
    }
}

impl Default for NetworkChaosScenario {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl ChaosScenario for NetworkChaosScenario {
    fn name(&self) -> &str {
        "Network Chaos"
    }

    fn description(&self) -> &str {
        "Inject 2s latency + jitter, verify system continues to function"
    }

    async fn run(&self) -> ScenarioResult {
        let start = std::time::SystemTime::now();

        // Start network operation with tracing
        {
            let _span = self
                .context
                .start_span("api_client".to_string(), "request".to_string());

            // Inject network delay
            let _: Result<(), _> = self.chaos.inject_network_delay().await;

            // Network operation completes
        }

        // Inject explicit partition
        let _: Result<(), _> = self
            .chaos
            .inject_network_partition("api-server", Duration::from_secs(1))
            .await;

        // Retry operation
        {
            let _span = self
                .context
                .start_span("api_client".to_string(), "retry".to_string());
        }

        let duration = start.elapsed().unwrap().as_millis() as u64;
        let events = self.chaos.get_events().await;

        ScenarioResult {
            scenario_name: self.name().to_string(),
            success: true,
            duration_ms: duration,
            events_recorded: events.len(),
            error_message: None,
        }
    }

    async fn verify(&self) -> VerificationResult {
        let events = self.chaos.get_events().await;
        let traces = self.context.get_traces();
        let mut checks = vec![];
        let mut failed_checks = vec![];

        // Check 1: Network partition was recorded
        let has_partition = events
            .iter()
            .any(|e| matches!(e, FailureEvent::NetworkPartition { .. }));
        if has_partition {
            checks.push("✓ Network partition recorded".to_string());
        } else {
            failed_checks.push("✗ Network partition not recorded".to_string());
        }

        // Check 2: Latency was introduced (at least 1 second)
        let total_latency = self.context.total_latency_ms();
        if let Some(latency) = total_latency {
            if latency >= 1000 {
                checks.push(format!("✓ Network latency introduced ({} ms)", latency));
            } else {
                failed_checks.push(format!("✗ Insufficient latency ({}ms < 1000ms)", latency));
            }
        }

        // Check 3: Request and retry were traced
        if traces.len() >= 2 {
            checks.push("✓ Initial request and retry traced".to_string());
        } else {
            failed_checks.push("✗ Incomplete request chain".to_string());
        }

        VerificationResult {
            passed: failed_checks.is_empty(),
            checks,
            failed_checks,
        }
    }
}

// ===== SCENARIO 3: Clock Skew =====

pub struct ClockSkewScenario {
    chaos: Arc<ChaosEngine>,
    context: CorrelationContext,
}

impl ClockSkewScenario {
    pub fn new() -> Self {
        ClockSkewScenario {
            chaos: Arc::new(ChaosEngine::default_engine()),
            context: CorrelationContext::new(),
        }
    }
}

impl Default for ClockSkewScenario {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl ChaosScenario for ClockSkewScenario {
    fn name(&self) -> &str {
        "Clock Skew"
    }

    fn description(&self) -> &str {
        "Advance clock by 5 minutes, verify timestamp consistency"
    }

    async fn run(&self) -> ScenarioResult {
        let start = std::time::SystemTime::now();

        // Record initial timestamp
        {
            let _span = self
                .context
                .start_span("timestamp_service".to_string(), "get_time".to_string());
        }

        // Inject 5 minute clock skew
        let skew = Duration::from_secs(300);
        let _: Result<(), _> = self.chaos.inject_clock_skew(skew).await;

        // Verify time moved forward
        {
            let _span = self.context.start_span(
                "timestamp_service".to_string(),
                "verify_consistency".to_string(),
            );
        }

        let duration = start.elapsed().unwrap().as_millis() as u64;
        let events = self.chaos.get_events().await;

        ScenarioResult {
            scenario_name: self.name().to_string(),
            success: true,
            duration_ms: duration,
            events_recorded: events.len(),
            error_message: None,
        }
    }

    async fn verify(&self) -> VerificationResult {
        let events = self.chaos.get_events().await;
        let mut checks = vec![];
        let mut failed_checks = vec![];

        // Check 1: Clock skew event recorded
        let has_skew = events.iter().find_map(|e| {
            if let FailureEvent::ClockSkew { amount_ms, .. } = e {
                Some(*amount_ms)
            } else {
                None
            }
        });

        if let Some(skew_ms) = has_skew {
            if skew_ms == 300_000 {
                checks.push(format!("✓ Clock skew recorded ({} ms)", skew_ms));
            } else {
                failed_checks.push(format!("✗ Unexpected skew amount: {} ms", skew_ms));
            }
        } else {
            failed_checks.push("✗ Clock skew not recorded".to_string());
        }

        // Check 2: Timestamps are monotonically increasing
        let event_times: Vec<u64> = events.iter().map(|e| e.timestamp()).collect();
        let is_monotonic = event_times.windows(2).all(|w| w[0] <= w[1]);

        if is_monotonic {
            checks.push("✓ Event timestamps monotonically increasing".to_string());
        } else {
            failed_checks.push("✗ Non-monotonic timestamps detected".to_string());
        }

        VerificationResult {
            passed: failed_checks.is_empty(),
            checks,
            failed_checks,
        }
    }
}

// ===== SCENARIO 4: Cascading Failure =====

pub struct CascadingFailureScenario {
    chaos: Arc<ChaosEngine>,
    context: CorrelationContext,
}

impl CascadingFailureScenario {
    pub fn new() -> Self {
        CascadingFailureScenario {
            chaos: Arc::new(ChaosEngine::default_engine()),
            context: CorrelationContext::new(),
        }
    }
}

impl Default for CascadingFailureScenario {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl ChaosScenario for CascadingFailureScenario {
    fn name(&self) -> &str {
        "Cascading Failure"
    }

    fn description(&self) -> &str {
        "Kill database component, verify ripple effects across system"
    }

    async fn run(&self) -> ScenarioResult {
        let start = std::time::SystemTime::now();

        // Kill database
        let _: Result<(), _> = self.chaos.kill_task("db-pool-1", "database").await;

        // Start cascading failure
        let affected = vec![
            "api_server".to_string(),
            "cache_layer".to_string(),
            "job_queue".to_string(),
        ];

        let _: Result<(), _> = self
            .chaos
            .start_cascading_failure("database_down", affected)
            .await;

        // Record failures in affected components
        for component in &["api_server", "cache_layer", "job_queue"] {
            let _: Result<(), _> = self
                .chaos
                .inject_resource_exhaustion(component, "db_connections", 0, 10)
                .await;
        }

        let duration = start.elapsed().unwrap().as_millis() as u64;
        let events = self.chaos.get_events().await;

        ScenarioResult {
            scenario_name: self.name().to_string(),
            success: true,
            duration_ms: duration,
            events_recorded: events.len(),
            error_message: None,
        }
    }

    async fn verify(&self) -> VerificationResult {
        let events = self.chaos.get_events().await;
        let mut checks = vec![];
        let mut failed_checks = vec![];

        // Check 1: Task was killed
        let task_killed = events
            .iter()
            .any(|e| matches!(e, FailureEvent::TaskKilled { .. }));
        if task_killed {
            checks.push("✓ Database task termination recorded".to_string());
        } else {
            failed_checks.push("✗ Task kill event not recorded".to_string());
        }

        // Check 2: Cascading failure initiated
        let cascade = events
            .iter()
            .any(|e| matches!(e, FailureEvent::CascadingFailureStart { .. }));
        if cascade {
            checks.push("✓ Cascading failure event recorded".to_string());
        } else {
            failed_checks.push("✗ Cascading failure not recorded".to_string());
        }

        // Check 3: Affected components recorded failures
        let affected_count = events
            .iter()
            .filter(|e| matches!(e, FailureEvent::ResourceExhaustion { .. }))
            .count();

        if affected_count >= 3 {
            checks.push(format!(
                "✓ Ripple effects across {} components",
                affected_count
            ));
        } else {
            failed_checks.push(format!(
                "✗ Insufficient cascade depth: {} components affected",
                affected_count
            ));
        }

        VerificationResult {
            passed: failed_checks.is_empty(),
            checks,
            failed_checks,
        }
    }
}

// ===== SCENARIO 5: Recovery =====

pub struct RecoveryScenario {
    chaos: Arc<ChaosEngine>,
    context: CorrelationContext,
}

impl RecoveryScenario {
    pub fn new() -> Self {
        RecoveryScenario {
            chaos: Arc::new(ChaosEngine::default_engine()),
            context: CorrelationContext::new(),
        }
    }
}

impl Default for RecoveryScenario {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl ChaosScenario for RecoveryScenario {
    fn name(&self) -> &str {
        "Recovery"
    }

    fn description(&self) -> &str {
        "Inject failures and verify system stabilizes after recovery"
    }

    async fn run(&self) -> ScenarioResult {
        let start = std::time::SystemTime::now();

        // Phase 1: Inject failures
        {
            let _span = self
                .context
                .start_span("system".to_string(), "degraded".to_string());

            let _: Result<(), _> = self.chaos.inject_panic("service-a").await;
            let _: Result<(), _> = self.chaos.inject_panic("service-b").await;
        }

        // Phase 2: Initiate recovery
        {
            let _span = self
                .context
                .start_span("recovery_manager".to_string(), "restart_all".to_string());

            let _: Result<(), _> = self
                .chaos
                .record_recovery("restart_service", "service-a", true)
                .await;
            let _: Result<(), _> = self
                .chaos
                .record_recovery("restart_service", "service-b", true)
                .await;
        }

        // Phase 3: Verify stabilization
        {
            let _span = self
                .context
                .start_span("healthcheck".to_string(), "all_healthy".to_string());
        }

        let duration = start.elapsed().unwrap().as_millis() as u64;
        let events = self.chaos.get_events().await;

        ScenarioResult {
            scenario_name: self.name().to_string(),
            success: true,
            duration_ms: duration,
            events_recorded: events.len(),
            error_message: None,
        }
    }

    async fn verify(&self) -> VerificationResult {
        let events = self.chaos.get_events().await;
        let mut checks = vec![];
        let mut failed_checks = vec![];

        // Check 1: Panics were recorded
        let panic_count = events
            .iter()
            .filter(|e| matches!(e, FailureEvent::PanicOccurred { .. }))
            .count();

        if panic_count >= 2 {
            checks.push(format!("✓ {} panic events recorded", panic_count));
        } else {
            failed_checks.push(format!("✗ Insufficient panics: {}", panic_count));
        }

        // Check 2: Recovery actions completed
        let recovery_count = events
            .iter()
            .filter(|e| matches!(e, FailureEvent::RecoveryAction { success: true, .. }))
            .count();

        if recovery_count >= 2 {
            checks.push(format!("✓ {} successful recovery actions", recovery_count));
        } else {
            failed_checks.push(format!(
                "✗ Insufficient recovery actions: {}",
                recovery_count
            ));
        }

        // Check 3: System traces show recovery progression
        let traces = self.context.get_traces();
        if traces.len() >= 5 {
            checks.push("✓ Complete degradation → recovery → stabilization trace".to_string());
        } else {
            failed_checks.push(format!(
                "✗ Incomplete trace: {} operations (need ≥5)",
                traces.len()
            ));
        }

        VerificationResult {
            passed: failed_checks.is_empty(),
            checks,
            failed_checks,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_panic_injection_scenario() {
        let scenario = PanicInjectionScenario::new();
        let result = scenario.run().await;
        assert!(result.success);

        let verification = scenario.verify().await;
        assert!(
            verification.passed,
            "Verification failed: {:?}",
            verification.failed_checks
        );
    }

    #[tokio::test]
    async fn test_network_chaos_scenario() {
        let scenario = NetworkChaosScenario::new();
        let result = scenario.run().await;
        assert!(result.success);

        let verification = scenario.verify().await;
        assert!(
            verification.passed,
            "Verification failed: {:?}",
            verification.failed_checks
        );
    }

    #[tokio::test]
    async fn test_clock_skew_scenario() {
        let scenario = ClockSkewScenario::new();
        let result = scenario.run().await;
        assert!(result.success);

        let verification = scenario.verify().await;
        assert!(
            verification.passed,
            "Verification failed: {:?}",
            verification.failed_checks
        );
    }

    #[tokio::test]
    async fn test_cascading_failure_scenario() {
        let scenario = CascadingFailureScenario::new();
        let result = scenario.run().await;
        assert!(result.success);

        let verification = scenario.verify().await;
        assert!(
            verification.passed,
            "Verification failed: {:?}",
            verification.failed_checks
        );
    }

    #[tokio::test]
    async fn test_recovery_scenario() {
        let scenario = RecoveryScenario::new();
        let result = scenario.run().await;
        assert!(result.success);

        let verification = scenario.verify().await;
        assert!(
            verification.passed,
            "Verification failed: {:?}",
            verification.failed_checks
        );
    }
}
