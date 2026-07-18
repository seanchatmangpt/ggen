//! Governor finite state machine (FSM) integration tests
//!
//! Chicago TDD: Tests verify state machine behavior with real state objects.
//! Governor implements cost control FSM: Stable → Warn → Intervene → Degrade
//!
//! Uses real collaborators (threshold comparators), state verification (not just asserts).
//! Invariant enforcement tested: guards prevent invalid state transitions.

/// Billing signal with cost and usage data
#[derive(Debug, Clone)]
pub struct BillingSignal {
    /// Monthly cost in USD
    pub monthly_cost: f64,
    /// Cost forecast for full month
    pub forecasted_cost: f64,
    /// Usage as percentage of quota (0-100)
    pub usage_percent: f64,
    /// Tenant ID
    pub tenant_id: String,
}

/// Governor FSM state
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum GovernorState {
    /// All systems normal
    Stable,
    /// Warning threshold exceeded, monitor closely
    Warn,
    /// Take action to reduce costs
    Intervene,
    /// Severe throttling/degradation
    Degrade,
}

/// Thresholds that trigger state transitions
#[derive(Debug, Clone)]
pub struct GovernorThresholds {
    /// Forecast cost to trigger Warn (USD)
    pub warn_threshold: f64,
    /// Forecast cost to trigger Intervene (USD)
    pub intervene_threshold: f64,
    /// Forecast cost to trigger Degrade (USD)
    pub degrade_threshold: f64,
    /// Usage percent to trigger Warn (0-100)
    pub usage_warn_percent: f64,
    /// Usage percent to trigger Intervene (0-100)
    pub usage_intervene_percent: f64,
    /// Usage percent to trigger Degrade (0-100)
    pub usage_degrade_percent: f64,
}

impl GovernorThresholds {
    /// Create default thresholds
    pub fn default() -> Self {
        Self {
            warn_threshold: 80.0,
            intervene_threshold: 120.0,
            degrade_threshold: 150.0,
            usage_warn_percent: 70.0,
            usage_intervene_percent: 85.0,
            usage_degrade_percent: 95.0,
        }
    }

    /// Create custom thresholds
    pub fn custom(
        warn_threshold: f64,
        intervene_threshold: f64,
        degrade_threshold: f64,
        usage_warn_percent: f64,
        usage_intervene_percent: f64,
        usage_degrade_percent: f64,
    ) -> Self {
        Self {
            warn_threshold,
            intervene_threshold,
            degrade_threshold,
            usage_warn_percent,
            usage_intervene_percent,
            usage_degrade_percent,
        }
    }
}

/// Action that Governor emits
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GovernorAction {
    /// No action needed
    None,
    /// Send warning notification
    SendWarning(String),
    /// Throttle/rate-limit customer
    Throttle(String),
    /// Degrade service quality
    Degrade(String),
    /// Block all requests
    Block(String),
}

/// The Governor FSM: monitors costs and enforces quotas
#[derive(Debug, Clone)]
pub struct Governor {
    /// Current state
    pub state: GovernorState,
    /// Thresholds
    pub thresholds: GovernorThresholds,
    /// Last signal processed
    pub last_signal: Option<BillingSignal>,
    /// Last action emitted
    pub last_action: GovernorAction,
    /// Tenant ID
    pub tenant_id: String,
    /// History of state transitions
    pub transitions: Vec<(GovernorState, GovernorState, BillingSignal)>,
}

impl Governor {
    /// Create a new governor
    pub fn new(tenant_id: impl Into<String>, thresholds: GovernorThresholds) -> Self {
        Self {
            state: GovernorState::Stable,
            thresholds,
            last_signal: None,
            last_action: GovernorAction::None,
            tenant_id: tenant_id.into(),
            transitions: Vec::new(),
        }
    }

    /// Create with default thresholds
    pub fn new_default(tenant_id: impl Into<String>) -> Self {
        Self::new(tenant_id, GovernorThresholds::default())
    }

    /// Determine target state based on signal
    fn determine_target_state(&self, signal: &BillingSignal) -> GovernorState {
        // Check forecast cost thresholds (higher priority)
        if signal.forecasted_cost >= self.thresholds.degrade_threshold {
            return GovernorState::Degrade;
        }
        if signal.forecasted_cost >= self.thresholds.intervene_threshold {
            return GovernorState::Intervene;
        }
        if signal.forecasted_cost >= self.thresholds.warn_threshold {
            return GovernorState::Warn;
        }

        // Check usage thresholds
        if signal.usage_percent >= self.thresholds.usage_degrade_percent {
            return GovernorState::Degrade;
        }
        if signal.usage_percent >= self.thresholds.usage_intervene_percent {
            return GovernorState::Intervene;
        }
        if signal.usage_percent >= self.thresholds.usage_warn_percent {
            return GovernorState::Warn;
        }

        GovernorState::Stable
    }

    /// Determine action for a given state
    fn action_for_state(&self, state: GovernorState, signal: &BillingSignal) -> GovernorAction {
        match state {
            GovernorState::Stable => GovernorAction::None,
            GovernorState::Warn => {
                GovernorAction::SendWarning(format!(
                    "Cost forecast: ${:.2} (threshold: ${:.2})",
                    signal.forecasted_cost, self.thresholds.warn_threshold
                ))
            }
            GovernorState::Intervene => {
                GovernorAction::Throttle(format!(
                    "Rate limiting: usage {}%, threshold {}%",
                    signal.usage_percent, self.thresholds.usage_intervene_percent
                ))
            }
            GovernorState::Degrade => {
                GovernorAction::Degrade(format!(
                    "Service degradation: cost ${:.2} exceeds limit",
                    signal.forecasted_cost
                ))
            }
        }
    }

    /// Process a billing signal
    pub fn process_signal(&mut self, signal: BillingSignal) -> GovernorAction {
        let old_state = self.state;
        let target_state = self.determine_target_state(&signal);

        // Guard: Only allow valid transitions
        if !Self::is_valid_transition(old_state, target_state) {
            // Invalid transition - stay in current state
            self.last_action = GovernorAction::None;
            return GovernorAction::None;
        }

        // Perform state transition
        if old_state != target_state {
            self.state = target_state;
            self.transitions.push((old_state, target_state, signal.clone()));
        }

        // Emit action for new state
        let action = self.action_for_state(target_state, &signal);
        self.last_action = action.clone();
        self.last_signal = Some(signal);

        action
    }

    /// Check if transition is valid (guard)
    fn is_valid_transition(from: GovernorState, to: GovernorState) -> bool {
        // Allow transitions to more severe states
        // Allow transitions to less severe states
        // But some transitions may be guarded
        match (from, to) {
            // Cannot go Degrade → Stable directly (must go through intermediate)
            (GovernorState::Degrade, GovernorState::Stable) => false,
            // All other transitions allowed
            _ => true,
        }
    }

    /// Get allowed transitions from current state
    pub fn allowed_transitions(&self) -> Vec<GovernorState> {
        match self.state {
            GovernorState::Stable => vec![GovernorState::Warn, GovernorState::Stable],
            GovernorState::Warn => {
                vec![GovernorState::Stable, GovernorState::Intervene, GovernorState::Warn]
            }
            GovernorState::Intervene => {
                vec![GovernorState::Warn, GovernorState::Degrade, GovernorState::Intervene]
            }
            GovernorState::Degrade => vec![GovernorState::Intervene, GovernorState::Degrade],
        }
    }

    /// Get transition history
    pub fn history(&self) -> &[(GovernorState, GovernorState, BillingSignal)] {
        &self.transitions
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test 1: Initial state is Stable
    #[test]
    fn test_initial_state_is_stable() {
        // Arrange & Act
        let gov = Governor::new_default("tenant-1");

        // Assert
        assert_eq!(gov.state, GovernorState::Stable);
        assert_eq!(gov.last_action, GovernorAction::None);
        assert!(gov.last_signal.is_none());
    }

    // Test 2: Stable → Warn transition on cost
    #[test]
    fn test_transition_stable_to_warn_on_cost() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");
        let signal = BillingSignal {
            monthly_cost: 50.0,
            forecasted_cost: 90.0, // Above warn threshold (80)
            usage_percent: 60.0,
            tenant_id: "tenant-1".to_string(),
        };

        // Act
        let action = gov.process_signal(signal.clone());

        // Assert: State changed
        assert_eq!(gov.state, GovernorState::Warn);
        assert!(matches!(action, GovernorAction::SendWarning(_)));

        // Verify transition recorded
        assert_eq!(gov.transitions.len(), 1);
        assert_eq!(gov.transitions[0].0, GovernorState::Stable);
        assert_eq!(gov.transitions[0].1, GovernorState::Warn);
    }

    // Test 3: Warn → Intervene transition
    #[test]
    fn test_transition_warn_to_intervene() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // First: Stable → Warn
        let warn_signal = BillingSignal {
            monthly_cost: 50.0,
            forecasted_cost: 90.0,
            usage_percent: 60.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(warn_signal);
        assert_eq!(gov.state, GovernorState::Warn);

        // Act: Warn → Intervene
        let intervene_signal = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 125.0, // Above intervene threshold (120)
            usage_percent: 80.0,
            tenant_id: "tenant-1".to_string(),
        };
        let action = gov.process_signal(intervene_signal);

        // Assert
        assert_eq!(gov.state, GovernorState::Intervene);
        assert!(matches!(action, GovernorAction::Throttle(_)));
        assert_eq!(gov.transitions.len(), 2);
    }

    // Test 4: Intervene → Degrade transition
    #[test]
    fn test_transition_intervene_to_degrade() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Get to Intervene state first
        let signal1 = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal1);
        assert_eq!(gov.state, GovernorState::Intervene);

        // Act: Intervene → Degrade
        let signal2 = BillingSignal {
            monthly_cost: 140.0,
            forecasted_cost: 160.0, // Above degrade threshold (150)
            usage_percent: 95.0,
            tenant_id: "tenant-1".to_string(),
        };
        let action = gov.process_signal(signal2);

        // Assert
        assert_eq!(gov.state, GovernorState::Degrade);
        assert!(matches!(action, GovernorAction::Degrade(_)));
    }

    // Test 5: Warn → Stable (improvement)
    #[test]
    fn test_transition_warn_back_to_stable() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Get to Warn
        let warn_signal = BillingSignal {
            monthly_cost: 50.0,
            forecasted_cost: 85.0,
            usage_percent: 70.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(warn_signal);
        assert_eq!(gov.state, GovernorState::Warn);

        // Act: Improve back to Stable
        let stable_signal = BillingSignal {
            monthly_cost: 30.0,
            forecasted_cost: 60.0, // Below warn threshold
            usage_percent: 50.0,
            tenant_id: "tenant-1".to_string(),
        };
        let action = gov.process_signal(stable_signal);

        // Assert
        assert_eq!(gov.state, GovernorState::Stable);
        assert_eq!(action, GovernorAction::None);
    }

    // Test 6: Degrade → Intervene (improvement)
    #[test]
    fn test_transition_degrade_to_intervene() {
        // Arrange: Get to Degrade state
        let mut gov = Governor::new_default("tenant-1");

        let signal1 = BillingSignal {
            monthly_cost: 140.0,
            forecasted_cost: 160.0,
            usage_percent: 95.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal1);
        assert_eq!(gov.state, GovernorState::Degrade);

        // Act: Improve to Intervene
        let signal2 = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 125.0, // Below degrade, above intervene
            usage_percent: 85.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal2);

        // Assert
        assert_eq!(gov.state, GovernorState::Intervene);
    }

    // Test 7: Guard prevents Degrade → Stable (invalid transition)
    #[test]
    fn test_guard_prevents_degrade_to_stable() {
        // Arrange: Manually set to Degrade
        let mut gov = Governor::new_default("tenant-1");
        gov.state = GovernorState::Degrade;

        // Act: Try to jump to Stable with signal that would normally trigger it
        let signal = BillingSignal {
            monthly_cost: 10.0,
            forecasted_cost: 40.0, // Well below all thresholds
            usage_percent: 20.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal);

        // Assert: Cannot jump directly to Stable, stays in or goes to Intervene
        // Based on our guard, should not allow Degrade → Stable
        assert_ne!(gov.state, GovernorState::Stable);
    }

    // Test 8: Usage percent threshold triggers Warn
    #[test]
    fn test_usage_percent_triggers_warn() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Act: High usage but low cost
        let signal = BillingSignal {
            monthly_cost: 10.0,
            forecasted_cost: 40.0, // Below cost threshold
            usage_percent: 75.0,   // Above usage warn threshold (70)
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal);

        // Assert: Warn triggered by usage
        assert_eq!(gov.state, GovernorState::Warn);
    }

    // Test 9: Usage percent threshold triggers Intervene
    #[test]
    fn test_usage_percent_triggers_intervene() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Act
        let signal = BillingSignal {
            monthly_cost: 20.0,
            forecasted_cost: 50.0,
            usage_percent: 90.0, // Above intervene threshold (85)
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal);

        // Assert
        assert_eq!(gov.state, GovernorState::Intervene);
    }

    // Test 10: Custom thresholds are respected
    #[test]
    fn test_custom_thresholds() {
        // Arrange: Custom thresholds
        let thresholds = GovernorThresholds::custom(50.0, 100.0, 150.0, 50.0, 75.0, 90.0);
        let mut gov = Governor::new("tenant-1", thresholds);

        // Act: Signal that hits custom warn threshold
        let signal = BillingSignal {
            monthly_cost: 30.0,
            forecasted_cost: 60.0, // Above custom warn (50)
            usage_percent: 40.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(signal);

        // Assert
        assert_eq!(gov.state, GovernorState::Warn);
    }

    // Test 11: No state change when stable
    #[test]
    fn test_no_state_change_when_stable() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");
        assert_eq!(gov.state, GovernorState::Stable);

        // Act: Signal that keeps it stable
        let signal = BillingSignal {
            monthly_cost: 30.0,
            forecasted_cost: 50.0, // Below all thresholds
            usage_percent: 40.0,
            tenant_id: "tenant-1".to_string(),
        };
        let _ = gov.process_signal(signal);

        // Assert
        assert_eq!(gov.state, GovernorState::Stable);
        assert_eq!(gov.transitions.len(), 0, "No transitions should be recorded");
    }

    // Test 12: Allowed transitions from each state
    #[test]
    fn test_allowed_transitions_stable() {
        let gov = Governor::new_default("tenant-1");
        let allowed = gov.allowed_transitions();
        assert!(allowed.contains(&GovernorState::Warn));
        assert!(allowed.contains(&GovernorState::Stable));
    }

    #[test]
    fn test_allowed_transitions_warn() {
        let mut gov = Governor::new_default("tenant-1");
        gov.state = GovernorState::Warn;
        let allowed = gov.allowed_transitions();
        assert!(allowed.contains(&GovernorState::Stable));
        assert!(allowed.contains(&GovernorState::Intervene));
        assert!(allowed.contains(&GovernorState::Warn));
    }

    #[test]
    fn test_allowed_transitions_intervene() {
        let mut gov = Governor::new_default("tenant-1");
        gov.state = GovernorState::Intervene;
        let allowed = gov.allowed_transitions();
        assert!(allowed.contains(&GovernorState::Warn));
        assert!(allowed.contains(&GovernorState::Degrade));
        assert!(allowed.contains(&GovernorState::Intervene));
    }

    #[test]
    fn test_allowed_transitions_degrade() {
        let mut gov = Governor::new_default("tenant-1");
        gov.state = GovernorState::Degrade;
        let allowed = gov.allowed_transitions();
        assert!(!allowed.contains(&GovernorState::Stable));
        assert!(allowed.contains(&GovernorState::Intervene));
        assert!(allowed.contains(&GovernorState::Degrade));
    }

    // Test 13: Full lifecycle with multiple signals
    #[test]
    fn test_full_lifecycle_multiple_signals() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Act & Assert: Process sequence
        // Signal 1: Stable
        let s1 = BillingSignal {
            monthly_cost: 20.0,
            forecasted_cost: 40.0,
            usage_percent: 30.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(s1);
        assert_eq!(gov.state, GovernorState::Stable);

        // Signal 2: Warn
        let s2 = BillingSignal {
            monthly_cost: 50.0,
            forecasted_cost: 85.0,
            usage_percent: 70.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(s2);
        assert_eq!(gov.state, GovernorState::Warn);

        // Signal 3: Intervene
        let s3 = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(s3);
        assert_eq!(gov.state, GovernorState::Intervene);

        // Signal 4: Back to Warn
        let s4 = BillingSignal {
            monthly_cost: 60.0,
            forecasted_cost: 90.0,
            usage_percent: 70.0,
            tenant_id: "tenant-1".to_string(),
        };
        gov.process_signal(s4);
        assert_eq!(gov.state, GovernorState::Warn);

        // State: History recorded
        assert_eq!(gov.transitions.len(), 3);
    }

    // Test 14: Multi-tenant isolation
    #[test]
    fn test_multi_tenant_isolation() {
        // Arrange
        let mut gov1 = Governor::new_default("tenant-1");
        let mut gov2 = Governor::new_default("tenant-2");

        // Act
        let signal1 = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 160.0, // Degrade for tenant-1
            usage_percent: 95.0,
            tenant_id: "tenant-1".to_string(),
        };

        let signal2 = BillingSignal {
            monthly_cost: 10.0,
            forecasted_cost: 30.0, // Stable for tenant-2
            usage_percent: 20.0,
            tenant_id: "tenant-2".to_string(),
        };

        gov1.process_signal(signal1);
        gov2.process_signal(signal2);

        // Assert: Different states
        assert_eq!(gov1.state, GovernorState::Degrade);
        assert_eq!(gov2.state, GovernorState::Stable);
    }

    // Test 15: Action messages include details
    #[test]
    fn test_action_messages_are_informative() {
        // Arrange
        let mut gov = Governor::new_default("tenant-1");

        // Act
        let signal = BillingSignal {
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
            tenant_id: "tenant-1".to_string(),
        };
        let action = gov.process_signal(signal);

        // Assert: Action contains details
        match action {
            GovernorAction::Throttle(msg) => {
                assert!(msg.contains("85"));  // Usage percent
                assert!(msg.contains("85"));  // Threshold
            }
            _ => panic!("Expected Throttle action"),
        }
    }
}
