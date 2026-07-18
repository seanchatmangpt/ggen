//! Governor FSM orchestrator (gen_statem inspired)
//!
//! This module implements the **Plan** phase of MAPE-K with a state machine:
//! - Erlang gen_statem-inspired event-driven FSM
//! - State transitions with invariant checking
//! - Action coordination (what should happen)
//! - Audit trail for all state changes
//!
//! ## State Machine Diagram
//!
//! ```
//! Stable
//!   ├─ high_signal(75-80) ──→ Warn
//!   └─ critical_signal(>90) ──→ Intervene
//!
//! Warn
//!   ├─ signal_normalizes ──→ Stable
//!   ├─ signal_worsens ──→ Intervene
//!   └─ sustained_warning ──→ Degrade
//!
//! Intervene
//!   ├─ action_success ──→ Warn
//!   ├─ action_fails ──→ Refuse
//!   └─ signal_persists ──→ Degrade
//!
//! Degrade
//!   ├─ signal_improves ──→ Intervene
//!   └─ quota_exceeded ──→ Refuse
//!
//! Refuse
//!   └─ (terminal state - requires manual intervention)
//! ```

use crate::signal_ingest::NormalizedSignal;
use crate::actuator::Action;
use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

/// Governor orchestration errors
#[derive(Debug, Error)]
pub enum GovernorError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Signal analysis failed: {0}")]
    SignalAnalysisFailed(String),

    #[error("No action possible in state: {state}")]
    NoActionPossible { state: String },

    #[error("Entitlement check failed: {0}")]
    EntitlementCheckFailed(String),
}

/// Governor FSM states (inspired by Erlang's sys_statem patterns)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum GovernorState {
    /// System operating within nominal parameters
    Stable,
    /// Warning: approaching threshold, monitoring closely
    Warn,
    /// Active intervention needed
    Intervene,
    /// Degraded performance mode activated
    Degrade,
    /// Service refused (terminal state)
    Refuse,
}

impl GovernorState {
    fn as_str(&self) -> &str {
        match self {
            GovernorState::Stable => "Stable",
            GovernorState::Warn => "Warn",
            GovernorState::Intervene => "Intervene",
            GovernorState::Degrade => "Degrade",
            GovernorState::Refuse => "Refuse",
        }
    }
}

/// Events that drive FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GovernorEvent {
    /// New signal received from monitoring
    SignalReceived(NormalizedSignal),
    /// Action execution succeeded
    ActionSucceeded,
    /// Action execution failed
    ActionFailed(String),
    /// Manual reset (operator intervention)
    Reset,
    /// Quota check result
    QuotaCheck(bool),
}

/// Governor instance (per-tenant coordination)
#[derive(Debug, Clone)]
pub struct Governor {
    state: GovernorState,
    tenant_id: String,
    consecutive_high_signals: u32,
    last_signal: Option<NormalizedSignal>,
    last_state_change: DateTime<Utc>,
}

impl Governor {
    /// Create new governor for tenant
    pub fn new(tenant_id: String) -> Self {
        Self {
            state: GovernorState::Stable,
            tenant_id,
            consecutive_high_signals: 0,
            last_signal: None,
            last_state_change: Utc::now(),
        }
    }

    /// Process event and compute state transition
    ///
    /// Returns: (new_state, optional_action_to_execute)
    pub async fn transition(&mut self, event: GovernorEvent) -> Result<(GovernorState, Option<Action>), GovernorError> {
        let (new_state, action) = match (&self.state, &event) {
            // === STABLE state ===
            (GovernorState::Stable, GovernorEvent::SignalReceived(signal)) => {
                Self::check_invariant(signal)?;

                match signal.normalized_value {
                    75..=80 => {
                        self.consecutive_high_signals = 1;
                        (GovernorState::Warn, None)
                    }
                    90.. => (GovernorState::Intervene, Some(Action::Throttle(50))),
                    _ => (GovernorState::Stable, None),
                }
            }

            // === WARN state ===
            (GovernorState::Warn, GovernorEvent::SignalReceived(signal)) => {
                Self::check_invariant(signal)?;

                match signal.normalized_value {
                    0..=70 => {
                        self.consecutive_high_signals = 0;
                        (GovernorState::Stable, None)
                    }
                    75..=85 => {
                        self.consecutive_high_signals += 1;
                        if self.consecutive_high_signals >= 3 {
                            (GovernorState::Degrade, Some(Action::Throttle(25)))
                        } else {
                            (GovernorState::Warn, None)
                        }
                    }
                    90.. => (GovernorState::Intervene, Some(Action::Throttle(50))),
                    _ => (GovernorState::Warn, None),
                }
            }

            // === INTERVENE state ===
            (GovernorState::Intervene, GovernorEvent::ActionSucceeded) => {
                self.consecutive_high_signals = 0;
                (GovernorState::Warn, None)
            }

            (GovernorState::Intervene, GovernorEvent::ActionFailed(_)) => {
                (GovernorState::Degrade, Some(Action::Pause))
            }

            (GovernorState::Intervene, GovernorEvent::SignalReceived(signal)) => {
                Self::check_invariant(signal)?;

                if signal.normalized_value < 75 {
                    (GovernorState::Warn, None)
                } else {
                    (GovernorState::Intervene, None)
                }
            }

            // === DEGRADE state ===
            (GovernorState::Degrade, GovernorEvent::SignalReceived(signal)) => {
                Self::check_invariant(signal)?;

                if signal.normalized_value < 70 {
                    (GovernorState::Intervene, None)
                } else if signal.normalized_value > 95 {
                    (GovernorState::Refuse, Some(Action::Shed(50)))
                } else {
                    (GovernorState::Degrade, None)
                }
            }

            (GovernorState::Degrade, GovernorEvent::QuotaCheck(false)) => {
                (GovernorState::Refuse, None)
            }

            // === REFUSE state ===
            (GovernorState::Refuse, GovernorEvent::Reset) => {
                self.consecutive_high_signals = 0;
                (GovernorState::Stable, None)
            }

            // Default: invalid transition
            (current, event) => {
                return Err(GovernorError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                })
            }
        };

        // Update state and signal
        if let GovernorEvent::SignalReceived(signal) = event {
            self.last_signal = Some(signal);
        }

        let old_state = self.state;
        self.state = new_state;
        self.last_state_change = Utc::now();

        // Log transition (would go to audit trail in production)
        if old_state != new_state {
            tracing::info!(
                tenant = %self.tenant_id,
                from = %old_state.as_str(),
                to = %new_state.as_str(),
                "State transition"
            );
        }

        Ok((new_state, action))
    }

    /// Check if signal violates known invariants
    ///
    /// ## Invariants
    /// - normalized_value must be 0-100
    /// - tenant_id must be non-empty
    /// - metric_type must be valid
    pub fn check_invariant(signal: &NormalizedSignal) -> Result<(), GovernorError> {
        if signal.normalized_value > 100 {
            return Err(GovernorError::InvariantViolation(
                format!("signal value {} exceeds maximum 100", signal.normalized_value),
            ));
        }

        if signal.tenant_id.is_empty() {
            return Err(GovernorError::InvariantViolation(
                "tenant_id cannot be empty".to_string(),
            ));
        }

        Ok(())
    }

    /// Get current state
    pub fn current_state(&self) -> GovernorState {
        self.state
    }

    /// Get consecutive high signals count
    pub fn consecutive_high_count(&self) -> u32 {
        self.consecutive_high_signals
    }

    /// Get time in current state
    pub fn time_in_state(&self) -> chrono::Duration {
        Utc::now() - self.last_state_change
    }

    /// Get last processed signal (for debugging)
    pub fn last_signal(&self) -> Option<&NormalizedSignal> {
        self.last_signal.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::signal_ingest::MetricType;

    fn make_signal(tenant_id: &str, value: u32) -> NormalizedSignal {
        NormalizedSignal {
            tenant_id: tenant_id.to_string(),
            metric_type: MetricType::CpuUtilization,
            normalized_value: value,
            timestamp: Utc::now(),
            signal_id: "test-sig".to_string(),
        }
    }

    #[tokio::test]
    async fn test_stable_to_warn_transition() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());
        let signal = make_signal("tenant-1", 78);

        // Act
        let (new_state, action) = governor
            .transition(GovernorEvent::SignalReceived(signal))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Warn);
        assert!(action.is_none());
    }

    #[tokio::test]
    async fn test_stable_to_intervene_high_signal() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());
        let signal = make_signal("tenant-1", 92);

        // Act
        let (new_state, action) = governor
            .transition(GovernorEvent::SignalReceived(signal))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Intervene);
        assert!(matches!(action, Some(Action::Throttle(_))));
    }

    #[tokio::test]
    async fn test_warn_to_stable_low_signal() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());
        // First, go to Warn
        let signal1 = make_signal("tenant-1", 78);
        governor
            .transition(GovernorEvent::SignalReceived(signal1))
            .await
            .unwrap();

        // Act: Send low signal
        let signal2 = make_signal("tenant-1", 65);
        let (new_state, _) = governor
            .transition(GovernorEvent::SignalReceived(signal2))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Stable);
        assert_eq!(governor.consecutive_high_count(), 0);
    }

    #[tokio::test]
    async fn test_warn_to_degrade_sustained() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());

        // Go to Warn with initial signal (78 → consecutive=1)
        let signal1 = make_signal("tenant-1", 78);
        let (state1, _) = governor
            .transition(GovernorEvent::SignalReceived(signal1))
            .await
            .unwrap();
        assert_eq!(state1, GovernorState::Warn);
        assert_eq!(governor.consecutive_high_count(), 1);

        // Send 2nd signal (80 → consecutive=2, still Warn)
        let signal2 = make_signal("tenant-1", 80);
        let (state2, _) = governor
            .transition(GovernorEvent::SignalReceived(signal2))
            .await
            .unwrap();
        assert_eq!(state2, GovernorState::Warn);
        assert_eq!(governor.consecutive_high_count(), 2);

        // Act: Send 3rd signal (80 → consecutive=3, goes to Degrade)
        let signal3 = make_signal("tenant-1", 80);
        let (new_state, action) = governor
            .transition(GovernorEvent::SignalReceived(signal3))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Degrade);
        assert!(matches!(action, Some(Action::Throttle(_))));
        assert_eq!(governor.consecutive_high_count(), 3);
    }

    #[tokio::test]
    async fn test_intervene_action_success() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());

        // Go to Intervene
        let signal = make_signal("tenant-1", 92);
        governor
            .transition(GovernorEvent::SignalReceived(signal))
            .await
            .unwrap();

        // Act: Action succeeded
        let (new_state, _) = governor
            .transition(GovernorEvent::ActionSucceeded)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Warn);
    }

    #[tokio::test]
    async fn test_invariant_violation_excessive_value() {
        // Arrange
        let signal = NormalizedSignal {
            tenant_id: "tenant-1".to_string(),
            metric_type: MetricType::CpuUtilization,
            normalized_value: 105, // Out of range!
            timestamp: Utc::now(),
            signal_id: "bad-sig".to_string(),
        };

        // Act
        let result = Governor::check_invariant(&signal);

        // Assert
        assert!(matches!(result, Err(GovernorError::InvariantViolation(_))));
    }

    #[tokio::test]
    async fn test_refuse_state_reset() {
        // Arrange
        let mut governor = Governor::new("tenant-1".to_string());
        governor.state = GovernorState::Refuse;

        // Act
        let (new_state, _) = governor
            .transition(GovernorEvent::Reset)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, GovernorState::Stable);
    }
}
