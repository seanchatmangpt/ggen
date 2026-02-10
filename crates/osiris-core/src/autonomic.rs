//! Autonomic State Management
//!
//! Implements the autonomic state machine for OSIRIS life management

use crate::{OSIRISSignal, SignalLevel};
use serde_json::Value;
use std::collections::HashMap;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Autonomic states in the OSIRIS lifecycle
#[derive(Debug, Clone, PartialEq)]
pub enum AutonomicState {
    /// Initial state - system is starting up
    Initializing,
    /// System is monitoring the environment
    Monitoring,
    /// System is analyzing patterns and data
    Analyzing,
    /// System is executing life patterns
    Executing,
    /// System is improving processes
    Improving,
    /// System is handling errors or anomalies
    ErrorHandling,
    /// System is in a safe state and stopped
    Stopped,
    /// System is shutting down
    Shutdown,
}

/// State transition rules and logic
pub struct StateTransition;

impl StateTransition {
    /// Valid transitions from each state
    pub fn valid_transitions(current: &AutonomicState) -> Vec<AutonomicState> {
        match current {
            AutonomicState::Initializing => vec![
                AutonomicState::Monitoring,
                AutonomicState::Stopped,
            ],
            AutonomicState::Monitoring => vec![
                AutonomicState::Analyzing,
                AutonomicState::Monitoring, // Stay in monitoring
                AutonomicState::ErrorHandling,
                AutonomicState::Stopped,
            ],
            AutonomicState::Analyzing => vec![
                AutonomicState::Executing,
                AutonomicState::Improving,
                AutonomicState::Monitoring,
                AutonomicState::ErrorHandling,
                AutonomicState::Stopped,
            ],
            AutonomicState::Executing => vec![
                AutonomicState::Analyzing,
                AutonomicState::Improving,
                AutonomicState::ErrorHandling,
                AutonomicState::Stopped,
            ],
            AutonomicState::Improving => vec![
                AutonomicState::Monitoring,
                AutonomicState::Executing,
                AutonomicState::ErrorHandling,
                AutonomicState::Stopped,
            ],
            AutonomicState::ErrorHandling => vec![
                AutonomicState::Monitoring,
                AutonomicState::Stopped,
                AutonomicState::Shutdown,
            ],
            AutonomicState::Stopped => vec![
                AutonomicState::Monitoring,
                AutonomicState::Shutdown,
            ],
            AutonomicState::Shutdown => vec![], // Terminal state
        }
    }

    /// Check if a transition is valid
    pub fn is_valid_transition(current: &AutonomicState, target: &AutonomicState) -> bool {
        Self::valid_transitions(current).contains(target)
    }
}

/// Autonomic state machine with context
pub struct AutonomicStateMachine {
    current: AutonomicState,
    history: Vec<(AutonomicState, AutonomicState, String)>, // (from, to, reason)
    context: HashMap<String, Value>,
}

impl AutonomicStateMachine {
    /// Create a new state machine starting in Initializing state
    pub fn new() -> Self {
        Self {
            current: AutonomicState::Initializing,
            history: Vec::new(),
            context: HashMap::new(),
        }
    }

    /// Get current state
    pub fn current(&self) -> &AutonomicState {
        &self.current
    }

    /// Transition to a new state
    pub async fn transition_to(&mut self, target: AutonomicState, reason: String) -> Result<(), String> {
        if !StateTransition::is_valid_transition(&self.current, &target) {
            let error = format!(
                "Invalid transition from {:?} to {:?}",
                self.current, target
            );
            warn!("{}", error);
            return Err(error);
        }

        debug!(
            "Transitioning from {:?} to {:?} - Reason: {}",
            self.current, target, reason
        );

        // Add transition to history
        self.history.push((self.current.clone(), target.clone(), reason.clone()));

        // Update current state
        self.current = target;

        // Emit transition signal
        let signal = OSIRISSignal::new(
            "state_transition",
            format!("State changed to {:?}", self.current),
            SignalLevel::Info,
        );

        // Signal would be emitted here in full implementation
        info!("State transition signal: {}", signal.message);

        Ok(())
    }

    /// Add context data to the state machine
    pub fn add_context(&mut self, key: String, value: Value) {
        self.context.insert(key, value);
    }

    /// Get context data
    pub fn get_context(&self, key: &str) -> Option<&Value> {
        self.context.get(key)
    }

    /// Get transition history
    pub fn history(&self) -> &[(AutonomicState, AutonomicState, String)] {
        &self.history
    }

    /// Check if the system is in a healthy state
    pub fn is_healthy(&self) -> bool {
        matches!(
            self.current,
            AutonomicState::Monitoring
                | AutonomicState::Analyzing
                | AutonomicState::Executing
                | AutonomicState::Improving
        )
    }

    /// Check if the system needs attention
    pub fn needs_attention(&self) -> bool {
        matches!(self.current, AutonomicState::ErrorHandling)
    }
}

impl Default for AutonomicStateMachine {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for AutonomicStateMachine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AutonomicStateMachine")
            .field("current", &self.current)
            .field("history_length", &self.history.len())
            .field("context_keys", &self.context.keys())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_transitions() {
        let current = AutonomicState::Monitoring;
        let valid_transitions = StateTransition::valid_transitions(&current);

        assert!(valid_transitions.contains(&AutonomicState::Analyzing));
        assert!(valid_transitions.contains(&AutonomicState::Monitoring));
        assert!(!valid_transitions.contains(&AutonomicState::Initializing));
    }

    #[test]
    fn test_invalid_transition() {
        let current = AutonomicState::Initializing;
        let target = AutonomicState::Executing;

        assert!(!StateTransition::is_valid_transition(&current, &target));
    }

    #[tokio::test]
    async fn test_state_machine_transitions() {
        let mut machine = AutonomicStateMachine::new();

        // Initial transition
        assert_eq!(*machine.current(), AutonomicState::Initializing);

        // Valid transition
        let result = machine
            .transition_to(AutonomicState::Monitoring, "System ready".to_string())
            .await;
        assert!(result.is_ok());
        assert_eq!(*machine.current(), AutonomicState::Monitoring);

        // Invalid transition
        let result = machine
            .transition_to(AutonomicState::Executing, "Should fail".to_string())
            .await;
        assert!(result.is_err());
    }

    #[test]
    fn test_context_management() {
        let mut machine = AutonomicStateMachine::new();

        let test_value = serde_json::json!({"test": "data"});
        machine.add_context("test_key".to_string(), test_value.clone());

        assert_eq!(machine.get_context("test_key"), Some(&test_value));
    }

    #[test]
    fn test_health_checks() {
        let mut machine = AutonomicStateMachine::new();

        // Starting state is not healthy
        assert!(!machine.is_healthy());
        assert!(!machine.needs_attention());

        // Transition to healthy state
        // Note: Can't transition in test without async, so we'll check directly
        machine.current = AutonomicState::Monitoring;
        assert!(machine.is_healthy());
        assert!(!machine.needs_attention());

        // Transition to error state
        machine.current = AutonomicState::ErrorHandling;
        assert!(!machine.is_healthy());
        assert!(machine.needs_attention());
    }
}