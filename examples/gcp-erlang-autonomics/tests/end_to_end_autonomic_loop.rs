//! End-to-end autonomic loop integration test
//!
//! Chicago TDD: Full signal→governor→actuator→receipt cycle with deterministic output verification.
//! This test simulates a complete autonomic system flow:
//!
//! 1. Arrange: Customer with entitlement, simulated billing spike
//! 2. Act: Signal ingested → governor FSM decides → actuator executes → receipt emitted
//! 3. Assert: Action executed, receipt in ledger, output is deterministic
//!
//! Run with `--test-threads=1` for deterministic results.

use std::collections::HashMap;
use serde_json::json;

// ===== Simplified versions of components for integration testing =====

#[derive(Debug, Clone)]
pub struct BillingEvent {
    pub tenant_id: String,
    pub monthly_cost: f64,
    pub forecasted_cost: f64,
    pub usage_percent: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GovernorState {
    Stable,
    Warn,
    Intervene,
    Degrade,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActuatorAction {
    None,
    SendWarning,
    Throttle,
    Degrade,
    Block,
}

#[derive(Debug, Clone)]
pub struct ExecutionReceipt {
    pub id: String,
    pub action_executed: ActuatorAction,
    pub tenant_id: String,
    pub previous_state: String,
    pub new_state: String,
}

// Governor: Simple FSM
pub struct Governor {
    state: GovernorState,
    warn_threshold: f64,
    intervene_threshold: f64,
    degrade_threshold: f64,
}

impl Governor {
    pub fn new() -> Self {
        Self {
            state: GovernorState::Stable,
            warn_threshold: 80.0,
            intervene_threshold: 120.0,
            degrade_threshold: 150.0,
        }
    }

    pub fn process_event(&mut self, event: &BillingEvent) -> ActuatorAction {
        let target_state = if event.forecasted_cost >= self.degrade_threshold {
            GovernorState::Degrade
        } else if event.forecasted_cost >= self.intervene_threshold {
            GovernorState::Intervene
        } else if event.forecasted_cost >= self.warn_threshold {
            GovernorState::Warn
        } else {
            GovernorState::Stable
        };

        self.state = target_state;

        match target_state {
            GovernorState::Stable => ActuatorAction::None,
            GovernorState::Warn => ActuatorAction::SendWarning,
            GovernorState::Intervene => ActuatorAction::Throttle,
            GovernorState::Degrade => ActuatorAction::Degrade,
        }
    }

    pub fn state(&self) -> GovernorState {
        self.state
    }
}

impl Default for Governor {
    fn default() -> Self {
        Self::new()
    }
}

// Actuator: Executes actions and produces receipts
pub struct Actuator {
    states: HashMap<String, String>,
    receipts: Vec<ExecutionReceipt>,
}

impl Actuator {
    pub fn new() -> Self {
        Self {
            states: HashMap::new(),
            receipts: Vec::new(),
        }
    }

    pub fn execute(&mut self, tenant_id: &str, action: ActuatorAction) -> ExecutionReceipt {
        let previous_state = self
            .states
            .get(tenant_id)
            .cloned()
            .unwrap_or_else(|| "normal".to_string());

        let new_state = match action {
            ActuatorAction::None => "normal".to_string(),
            ActuatorAction::SendWarning => "warning".to_string(),
            ActuatorAction::Throttle => "throttled".to_string(),
            ActuatorAction::Degrade => "degraded".to_string(),
            ActuatorAction::Block => "blocked".to_string(),
        };

        self.states.insert(tenant_id.to_string(), new_state.clone());

        let receipt = ExecutionReceipt {
            id: uuid::Uuid::new_v4().to_string(),
            action_executed: action,
            tenant_id: tenant_id.to_string(),
            previous_state,
            new_state,
        };

        self.receipts.push(receipt.clone());
        receipt
    }

    pub fn get_state(&self, tenant_id: &str) -> Option<&str> {
        self.states.get(tenant_id).map(|s| s.as_str())
    }

    pub fn receipts(&self) -> &[ExecutionReceipt] {
        &self.receipts
    }
}

impl Default for Actuator {
    fn default() -> Self {
        Self::new()
    }
}

// ===== Integration Tests =====

#[cfg(test)]
mod tests {
    use super::*;

    // Test 1: Stable billing → no action
    #[test]
    fn test_stable_billing_no_action() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let event = BillingEvent {
            tenant_id: "customer-1".to_string(),
            monthly_cost: 30.0,
            forecasted_cost: 50.0, // Below warn threshold
            usage_percent: 40.0,
        };

        // Act
        let action = governor.process_event(&event);
        let receipt = actuator.execute(&event.tenant_id, action.clone());

        // Assert
        assert_eq!(action, ActuatorAction::None);
        assert_eq!(governor.state(), GovernorState::Stable);
        assert_eq!(receipt.action_executed, ActuatorAction::None);
        assert_eq!(receipt.previous_state, "normal");
        assert_eq!(receipt.new_state, "normal");

        // State: Actuator unchanged
        assert_eq!(actuator.get_state("customer-1"), Some("normal"));
    }

    // Test 2: Cost spike → warn → throttle
    #[test]
    fn test_cost_spike_triggers_warn() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let event = BillingEvent {
            tenant_id: "customer-2".to_string(),
            monthly_cost: 50.0,
            forecasted_cost: 85.0, // Above warn, below intervene
            usage_percent: 65.0,
        };

        // Act
        let action = governor.process_event(&event);
        let receipt = actuator.execute(&event.tenant_id, action.clone());

        // Assert
        assert_eq!(action, ActuatorAction::SendWarning);
        assert_eq!(governor.state(), GovernorState::Warn);
        assert_eq!(receipt.previous_state, "normal");
        assert_eq!(receipt.new_state, "warning");

        // State: Receipt stored
        assert_eq!(actuator.receipts().len(), 1);
    }

    // Test 3: Major spike → intervention
    #[test]
    fn test_major_spike_triggers_intervention() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let event = BillingEvent {
            tenant_id: "customer-3".to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 125.0, // Above intervene threshold
            usage_percent: 85.0,
        };

        // Act
        let action = governor.process_event(&event);
        let receipt = actuator.execute(&event.tenant_id, action.clone());

        // Assert
        assert_eq!(action, ActuatorAction::Throttle);
        assert_eq!(governor.state(), GovernorState::Intervene);
        assert_eq!(receipt.new_state, "throttled");

        // State
        assert_eq!(actuator.get_state("customer-3"), Some("throttled"));
    }

    // Test 4: Severe crisis → degradation
    #[test]
    fn test_severe_spike_triggers_degradation() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let event = BillingEvent {
            tenant_id: "customer-4".to_string(),
            monthly_cost: 140.0,
            forecasted_cost: 160.0, // Above degrade threshold
            usage_percent: 95.0,
        };

        // Act
        let action = governor.process_event(&event);
        let receipt = actuator.execute(&event.tenant_id, action.clone());

        // Assert
        assert_eq!(action, ActuatorAction::Degrade);
        assert_eq!(governor.state(), GovernorState::Degrade);
        assert_eq!(receipt.new_state, "degraded");
    }

    // Test 5: Full cycle: normal → warn → stable
    #[test]
    fn test_full_cycle_escalation_and_recovery() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();
        let tenant_id = "customer-5";

        // Act 1: Escalate to Warn
        let event1 = BillingEvent {
            tenant_id: tenant_id.to_string(),
            monthly_cost: 50.0,
            forecasted_cost: 85.0,
            usage_percent: 70.0,
        };
        let action1 = governor.process_event(&event1);
        let receipt1 = actuator.execute(tenant_id, action1.clone());

        assert_eq!(action1, ActuatorAction::SendWarning);
        assert_eq!(receipt1.previous_state, "normal");
        assert_eq!(receipt1.new_state, "warning");

        // Act 2: Escalate to Intervene
        let event2 = BillingEvent {
            tenant_id: tenant_id.to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
        };
        let action2 = governor.process_event(&event2);
        let receipt2 = actuator.execute(tenant_id, action2.clone());

        assert_eq!(action2, ActuatorAction::Throttle);
        assert_eq!(receipt2.previous_state, "warning");
        assert_eq!(receipt2.new_state, "throttled");

        // Act 3: Recover to Stable
        let event3 = BillingEvent {
            tenant_id: tenant_id.to_string(),
            monthly_cost: 30.0,
            forecasted_cost: 50.0,
            usage_percent: 40.0,
        };
        let action3 = governor.process_event(&event3);
        let receipt3 = actuator.execute(tenant_id, action3.clone());

        // Assert: Full recovery
        assert_eq!(action3, ActuatorAction::None);
        assert_eq!(governor.state(), GovernorState::Stable);
        assert_eq!(receipt3.new_state, "normal");

        // State: Audit trail complete
        assert_eq!(actuator.receipts().len(), 3);
    }

    // Test 6: Multi-tenant independence
    #[test]
    fn test_multi_tenant_independence() {
        // Arrange
        let mut gov1 = Governor::new();
        let mut gov2 = Governor::new();
        let mut actuator = Actuator::new();

        // Act: Different events for different tenants
        let event1 = BillingEvent {
            tenant_id: "customer-1".to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 160.0, // Degrade
            usage_percent: 95.0,
        };

        let event2 = BillingEvent {
            tenant_id: "customer-2".to_string(),
            monthly_cost: 20.0,
            forecasted_cost: 40.0, // Stable
            usage_percent: 30.0,
        };

        let action1 = gov1.process_event(&event1);
        let action2 = gov2.process_event(&event2);

        actuator.execute(&event1.tenant_id, action1.clone());
        actuator.execute(&event2.tenant_id, action2.clone());

        // Assert: Independent states
        assert_eq!(gov1.state(), GovernorState::Degrade);
        assert_eq!(gov2.state(), GovernorState::Stable);

        assert_eq!(actuator.get_state("customer-1"), Some("degraded"));
        assert_eq!(actuator.get_state("customer-2"), Some("normal"));

        // Receipts for both
        assert_eq!(actuator.receipts().len(), 2);
    }

    // Test 7: Deterministic output for same input
    #[test]
    fn test_deterministic_output() {
        // Arrange
        let event = BillingEvent {
            tenant_id: "customer-det".to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
        };

        // Act: Process twice with identical input
        let (action1, state1) = {
            let mut gov = Governor::new();
            let action = gov.process_event(&event);
            (action, gov.state())
        };

        let (action2, state2) = {
            let mut gov = Governor::new();
            let action = gov.process_event(&event);
            (action, gov.state())
        };

        // Assert: Identical outputs
        assert_eq!(action1, action2);
        assert_eq!(state1, state2);
    }

    // Test 8: Receipt audit trail completeness
    #[test]
    fn test_receipt_audit_trail() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();
        let tenant_id = "customer-audit";

        // Act: Series of events
        let events = vec![
            BillingEvent {
                tenant_id: tenant_id.to_string(),
                monthly_cost: 20.0,
                forecasted_cost: 50.0,
                usage_percent: 40.0,
            },
            BillingEvent {
                tenant_id: tenant_id.to_string(),
                monthly_cost: 90.0,
                forecasted_cost: 125.0,
                usage_percent: 85.0,
            },
            BillingEvent {
                tenant_id: tenant_id.to_string(),
                monthly_cost: 40.0,
                forecasted_cost: 70.0,
                usage_percent: 55.0,
            },
        ];

        let mut receipt_count = 0;
        for event in events {
            let action = governor.process_event(&event);
            actuator.execute(tenant_id, action);
            receipt_count += 1;
        }

        // Assert: All receipts recorded
        assert_eq!(actuator.receipts().len(), receipt_count);

        // Verify receipt chain
        let receipts = actuator.receipts();
        for i in 1..receipts.len() {
            // Each receipt should reference previous state
            assert_eq!(receipts[i].previous_state, receipts[i - 1].new_state);
        }
    }

    // Test 9: State transitions are valid
    #[test]
    fn test_valid_state_transitions() {
        // Arrange
        let mut governor = Governor::new();

        // Assert: Start at Stable
        assert_eq!(governor.state(), GovernorState::Stable);

        // Act: Warn
        let event1 = BillingEvent {
            tenant_id: "customer".to_string(),
            monthly_cost: 50.0,
            forecasted_cost: 85.0,
            usage_percent: 70.0,
        };
        governor.process_event(&event1);
        assert_eq!(governor.state(), GovernorState::Warn);

        // Act: Intervene
        let event2 = BillingEvent {
            tenant_id: "customer".to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
        };
        governor.process_event(&event2);
        assert_eq!(governor.state(), GovernorState::Intervene);

        // Act: Degrade
        let event3 = BillingEvent {
            tenant_id: "customer".to_string(),
            monthly_cost: 140.0,
            forecasted_cost: 160.0,
            usage_percent: 95.0,
        };
        governor.process_event(&event3);
        assert_eq!(governor.state(), GovernorState::Degrade);

        // Act: Can recover through states
        let event4 = BillingEvent {
            tenant_id: "customer".to_string(),
            monthly_cost: 80.0,
            forecasted_cost: 115.0,  // Below intervene (120), so goes to Warn
            usage_percent: 80.0,
        };
        governor.process_event(&event4);
        assert_eq!(governor.state(), GovernorState::Warn);  // Should be Warn, not Intervene
    }

    // Test 10: Cost Circuit Breaker scenario (from prompt)
    #[test]
    fn test_cost_circuit_breaker_sku() {
        // Arrange: Customer with Cost Circuit Breaker SKU, billing spike
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let customer = "cost-circuit-breaker-customer";

        // Simulate steady state
        let steady = BillingEvent {
            tenant_id: customer.to_string(),
            monthly_cost: 50.0,
            forecasted_cost: 60.0, // Expected to stay low
            usage_percent: 50.0,
        };

        let action = governor.process_event(&steady);
        let receipt1 = actuator.execute(customer, action);
        assert_eq!(receipt1.new_state, "normal");

        // Act: Billing spike detected
        let spike = BillingEvent {
            tenant_id: customer.to_string(),
            monthly_cost: 120.0,
            forecasted_cost: 180.0, // 1.8x budget, exceeds degrade threshold
            usage_percent: 90.0,
        };

        let action = governor.process_event(&spike);
        assert_eq!(action, ActuatorAction::Degrade);
        assert_eq!(governor.state(), GovernorState::Degrade);

        let receipt2 = actuator.execute(customer, action);
        assert_eq!(receipt2.action_executed, ActuatorAction::Degrade);
        assert_eq!(receipt2.new_state, "degraded");

        // Assert: Autonomic system protected customer
        assert_eq!(actuator.get_state(customer), Some("degraded"));
        assert_eq!(actuator.receipts().len(), 2);

        // Verify deterministic: Same spike always produces Degrade action
        let mut gov2 = Governor::new();
        let action2 = gov2.process_event(&spike);
        assert_eq!(action2, ActuatorAction::Degrade);
    }

    // Test 11: End-to-end latency (should be fast)
    #[test]
    fn test_end_to_end_latency() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();

        let event = BillingEvent {
            tenant_id: "perf-test".to_string(),
            monthly_cost: 100.0,
            forecasted_cost: 125.0,
            usage_percent: 85.0,
        };

        // Act
        let start = std::time::Instant::now();
        let action = governor.process_event(&event);
        actuator.execute(&event.tenant_id, action);
        let elapsed = start.elapsed();

        // Assert: Should complete in microseconds
        assert!(elapsed.as_millis() < 100, "E2E cycle should be <100ms");
    }

    // Test 12: Recovery from degradation
    #[test]
    fn test_recovery_from_degradation() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();
        let customer = "recovery-test";

        // Degrade due to spike
        let spike = BillingEvent {
            tenant_id: customer.to_string(),
            monthly_cost: 150.0,
            forecasted_cost: 160.0,
            usage_percent: 95.0,
        };
        let action = governor.process_event(&spike);
        actuator.execute(customer, action);
        assert_eq!(actuator.get_state(customer), Some("degraded"));

        // Act: Monitor usage decreases
        let recovery = BillingEvent {
            tenant_id: customer.to_string(),
            monthly_cost: 60.0,
            forecasted_cost: 80.0, // Back to warn
            usage_percent: 60.0,
        };
        let action = governor.process_event(&recovery);
        actuator.execute(customer, action);

        // Assert: Back to warning (intermediate step)
        assert_eq!(governor.state(), GovernorState::Warn);
        assert_eq!(actuator.get_state(customer), Some("warning"));
    }

    // Test 13: Edge case - exactly at threshold
    #[test]
    fn test_exactly_at_threshold() {
        // Arrange
        let mut governor = Governor::new();

        // Act: Event exactly at warn threshold
        let event = BillingEvent {
            tenant_id: "threshold-test".to_string(),
            monthly_cost: 80.0,
            forecasted_cost: 80.0, // Exactly at warn threshold
            usage_percent: 70.0,
        };
        governor.process_event(&event);

        // Assert: Triggers warn (>= threshold)
        assert_eq!(governor.state(), GovernorState::Warn);
    }

    // Test 14: Rapid state changes
    #[test]
    fn test_rapid_state_changes() {
        // Arrange
        let mut governor = Governor::new();
        let mut actuator = Actuator::new();
        let customer = "rapid-test";

        let events = vec![
            (50.0, "normal"),       // Stable state = "normal" in actuator (< 80 threshold)
            (85.0, "warning"),      // Warn threshold >= 80
            (125.0, "throttled"),   // Intervene threshold >= 120
            (160.0, "degraded"),    // Degrade threshold >= 150
            (125.0, "throttled"),   // Back to intervene (>= 120, < 150)
            (85.0, "warning"),      // Back to warn (>= 80, < 120)
            (40.0, "normal"),       // Back to stable (< 80)
        ];

        // Act
        for (forecast, expected_state) in events {
            let event = BillingEvent {
                tenant_id: customer.to_string(),
                monthly_cost: forecast * 0.6, // Rough correlation
                forecasted_cost: forecast,
                usage_percent: (forecast / 160.0 * 100.0) as f64,
            };
            let action = governor.process_event(&event);
            actuator.execute(customer, action);

            // Verify state matches expected
            let actual = actuator.get_state(customer).unwrap_or("unknown");
            assert_eq!(
                actual, expected_state,
                "At forecast {}, expected {}, got {}",
                forecast, expected_state, actual
            );
        }

        // Assert: Full audit trail
        assert_eq!(actuator.receipts().len(), 7);
    }
}
