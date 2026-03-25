//! Recovery Orchestrator - Coordinates automated recovery strategies
//!
//! Implements multi-level recovery orchestration:
//! 1. Monitors supervisor for restart cascades
//! 2. Triggers circuit breaker isolation
//! 3. Initiates graceful degradation
//! 4. Coordinates consensus decisions
//! 5. Escalates to operator if automatic recovery fails

use crate::error::Result;
use crate::signals::{OSIRISSignal, SignalLevel};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;
use tracing::{error, info, warn};

/// Recovery state for a component
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecoveryState {
    /// Component is healthy
    Healthy,
    /// Component has restart threshold exceeded - isolation triggered
    Isolated,
    /// Component is in read-only mode
    ReadOnly,
    /// Operator intervention required
    RequiresOperator,
}

/// Escalation level for automatic recovery
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EscalationLevel {
    /// Recovery via supervisor restart
    Restart = 0,
    /// Circuit breaker isolation
    CircuitBreaker = 1,
    /// Graceful degradation
    GracefulDegradation = 2,
    /// Operator intervention
    Operator = 3,
}

/// Decision point in recovery flow
#[derive(Debug, Clone)]
pub struct RecoveryDecision {
    pub component: String,
    pub current_state: RecoveryState,
    pub escalation_level: EscalationLevel,
    pub action: RecoveryAction,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Actions the orchestrator can trigger
#[derive(Debug, Clone)]
pub enum RecoveryAction {
    None,
    RestartComponent,
    OpenCircuitBreaker,
    EnableReadOnly,
    AlertOperator { reason: String },
}

/// Component restart metrics
struct RestartMetrics {
    count: u32,
    #[allow(dead_code)]
    window_start: Instant,
    restart_times: Vec<Instant>,
}

impl RestartMetrics {
    fn new() -> Self {
        Self {
            count: 0,
            window_start: Instant::now(),
            restart_times: Vec::new(),
        }
    }

    fn record_restart(&mut self) {
        self.restart_times.push(Instant::now());
        // Keep only restarts within last 60s window
        let cutoff = Instant::now() - Duration::from_secs(60);
        self.restart_times.retain(|&t| t > cutoff);
        self.count = self.restart_times.len() as u32;
    }

    fn is_restart_cascade(&self, threshold: u32, window_secs: u64) -> bool {
        let now = Instant::now();
        let window = Duration::from_secs(window_secs);
        let recent = self
            .restart_times
            .iter()
            .filter(|&&t| now.duration_since(t) < window)
            .count() as u32;
        recent >= threshold
    }
}

/// Recovery Orchestrator: coordinates multi-level recovery strategies
pub struct RecoveryOrchestrator {
    /// Component restart metrics
    metrics: Arc<RwLock<HashMap<String, RestartMetrics>>>,
    /// Current recovery state per component
    states: Arc<RwLock<HashMap<String, RecoveryState>>>,
    /// Recovery history for analysis
    decisions: Arc<RwLock<Vec<RecoveryDecision>>>,
    /// Configuration
    config: OrchestratorConfig,
}

/// Orchestrator configuration
#[derive(Debug, Clone)]
pub struct OrchestratorConfig {
    /// Restart threshold for cascade detection
    pub restart_threshold: u32,
    /// Time window for restart cascade (seconds)
    pub cascade_window_secs: u64,
    /// Attempt automatic recovery up to this escalation level
    pub max_auto_escalation: EscalationLevel,
}

impl Default for OrchestratorConfig {
    fn default() -> Self {
        Self {
            restart_threshold: 5,
            cascade_window_secs: 60,
            max_auto_escalation: EscalationLevel::GracefulDegradation,
        }
    }
}

impl RecoveryOrchestrator {
    /// Create a new recovery orchestrator
    pub fn new(config: OrchestratorConfig) -> Self {
        Self {
            metrics: Arc::new(RwLock::new(HashMap::new())),
            states: Arc::new(RwLock::new(HashMap::new())),
            decisions: Arc::new(RwLock::new(Vec::new())),
            config,
        }
    }

    /// Report a component restart to the orchestrator
    pub async fn on_component_restart(&self, component_id: &str) -> Result<RecoveryDecision> {
        let mut metrics = self.metrics.write().await;
        let metric = metrics
            .entry(component_id.to_string())
            .or_insert_with(RestartMetrics::new);

        metric.record_restart();

        // Evaluate recovery cascade
        let decision = self
            .evaluate_recovery_cascade(
                component_id,
                metric.count,
                metric.is_restart_cascade(
                    self.config.restart_threshold,
                    self.config.cascade_window_secs,
                ),
            )
            .await?;

        // Record decision
        let mut decisions = self.decisions.write().await;
        decisions.push(decision.clone());
        if decisions.len() > 1000 {
            decisions.remove(0); // Keep history bounded
        }

        Ok(decision)
    }

    /// Evaluate recovery cascade and trigger escalation
    async fn evaluate_recovery_cascade(
        &self, component_id: &str, restart_count: u32, is_cascade: bool,
    ) -> Result<RecoveryDecision> {
        let mut states = self.states.write().await;

        if !is_cascade {
            return Ok(RecoveryDecision {
                component: component_id.to_string(),
                current_state: RecoveryState::Healthy,
                escalation_level: EscalationLevel::Restart,
                action: RecoveryAction::None,
                timestamp: chrono::Utc::now(),
            });
        }

        // Restart cascade detected - escalate
        info!(
            "Restart cascade detected for {}: {} restarts in {}s",
            component_id, restart_count, self.config.cascade_window_secs
        );

        // Determine escalation level
        let escalation = match restart_count {
            0..=2 => EscalationLevel::Restart,
            3..=4 => EscalationLevel::CircuitBreaker,
            5..=6 => EscalationLevel::GracefulDegradation,
            _ => EscalationLevel::Operator,
        };

        let (new_state, action) = self.trigger_recovery_action(component_id, escalation).await;
        states.insert(component_id.to_string(), new_state.clone());

        let decision = RecoveryDecision {
            component: component_id.to_string(),
            current_state: new_state,
            escalation_level: escalation,
            action,
            timestamp: chrono::Utc::now(),
        };

        info!("Recovery decision: {:?}", decision);
        Ok(decision)
    }

    /// Trigger appropriate recovery action based on escalation level
    async fn trigger_recovery_action(
        &self, component_id: &str, escalation: EscalationLevel,
    ) -> (RecoveryState, RecoveryAction) {
        match escalation {
            EscalationLevel::Restart => {
                info!("Attempting restart of component: {}", component_id);
                (RecoveryState::Healthy, RecoveryAction::RestartComponent)
            }
            EscalationLevel::CircuitBreaker => {
                warn!("Opening circuit breaker for component: {}", component_id);
                (RecoveryState::Isolated, RecoveryAction::OpenCircuitBreaker)
            }
            EscalationLevel::GracefulDegradation => {
                warn!(
                    "Initiating graceful degradation for component: {}",
                    component_id
                );
                (RecoveryState::ReadOnly, RecoveryAction::EnableReadOnly)
            }
            EscalationLevel::Operator => {
                error!("Component requires operator intervention: {}", component_id);
                (
                    RecoveryState::RequiresOperator,
                    RecoveryAction::AlertOperator {
                        reason: format!(
                            "Component {} failed to recover automatically",
                            component_id
                        ),
                    },
                )
            }
        }
    }

    /// Emit recovery signal
    pub fn create_recovery_signal(
        component: &str, escalation: EscalationLevel, action: &RecoveryAction,
    ) -> OSIRISSignal {
        let level = match escalation {
            EscalationLevel::Restart => SignalLevel::Info,
            EscalationLevel::CircuitBreaker => SignalLevel::Warning,
            EscalationLevel::GracefulDegradation => SignalLevel::Warning,
            EscalationLevel::Operator => SignalLevel::Critical,
        };

        let message = format!(
            "Recovery triggered for {}: {:?} -> {:?}",
            component, escalation, action
        );

        OSIRISSignal::with_routing(
            "recovery_event".to_string(),
            message,
            level,
            Some("recovery_orchestrator".to_string()),
            Some(component.to_string()),
        )
    }

    /// Get current recovery state of a component
    pub async fn get_component_state(&self, component_id: &str) -> RecoveryState {
        let states = self.states.read().await;
        states
            .get(component_id)
            .cloned()
            .unwrap_or(RecoveryState::Healthy)
    }

    /// Get all recovery states
    pub async fn get_all_states(&self) -> HashMap<String, RecoveryState> {
        self.states.read().await.clone()
    }

    /// Reset component to healthy state
    pub async fn reset_component(&self, component_id: &str) -> Result<()> {
        let mut states = self.states.write().await;
        states.insert(component_id.to_string(), RecoveryState::Healthy);

        let mut metrics = self.metrics.write().await;
        metrics.remove(component_id);

        info!("Component {} reset to healthy state", component_id);
        Ok(())
    }

    /// Get recovery history (last N decisions)
    pub async fn get_recovery_history(&self, limit: usize) -> Vec<RecoveryDecision> {
        let decisions = self.decisions.read().await;
        decisions.iter().rev().take(limit).cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_recovery_orchestrator_creation() {
        let orchestrator = RecoveryOrchestrator::new(OrchestratorConfig::default());
        assert_eq!(
            orchestrator.get_component_state("test").await,
            RecoveryState::Healthy
        );
    }

    #[tokio::test]
    async fn test_single_restart_no_escalation() {
        let orchestrator = RecoveryOrchestrator::new(OrchestratorConfig::default());
        let decision = orchestrator.on_component_restart("sensor").await.unwrap();
        assert_eq!(decision.escalation_level, EscalationLevel::Restart);
        assert_eq!(
            orchestrator.get_component_state("sensor").await,
            RecoveryState::Healthy
        );
    }

    #[tokio::test]
    async fn test_restart_cascade_triggers_circuit_breaker() {
        let orchestrator = RecoveryOrchestrator::new(OrchestratorConfig {
            restart_threshold: 3,
            cascade_window_secs: 60,
            max_auto_escalation: EscalationLevel::GracefulDegradation,
        });

        // Trigger 4 restarts to hit cascade threshold
        for _ in 0..4 {
            orchestrator.on_component_restart("api_gateway").await.ok();
        }

        let state = orchestrator.get_component_state("api_gateway").await;
        assert_eq!(state, RecoveryState::Isolated);
    }

    #[tokio::test]
    async fn test_reset_component() {
        let orchestrator = RecoveryOrchestrator::new(OrchestratorConfig::default());
        orchestrator.on_component_restart("worker").await.ok();

        orchestrator.reset_component("worker").await.ok();
        assert_eq!(
            orchestrator.get_component_state("worker").await,
            RecoveryState::Healthy
        );
    }

    #[tokio::test]
    async fn test_recovery_history() {
        let orchestrator = RecoveryOrchestrator::new(OrchestratorConfig::default());

        for i in 0..5 {
            orchestrator
                .on_component_restart(&format!("comp_{}", i))
                .await
                .ok();
        }

        let history = orchestrator.get_recovery_history(10).await;
        assert_eq!(history.len(), 5);
    }
}
