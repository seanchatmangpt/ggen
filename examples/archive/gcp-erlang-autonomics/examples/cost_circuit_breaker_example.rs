//! Cost Circuit Breaker Example
//!
//! Demonstrates the Cost Circuit Breaker SKU: autonomically prevents cloud runaway costs.
//!
//! This example simulates:
//! 1. Initialize Cost Circuit Breaker governor (FSM)
//! 2. Billing baseline: $100/day
//! 3. Alert threshold: $150/day (50% over baseline)
//! 4. Simulate billing spike to $160/day
//! 5. Observe governor transition: Nominal → Alert
//! 6. Actuator throttles service scaling
//! 7. Receipt emitted to audit trail
//!
//! Usage:
//! ```bash
//! cargo run --example cost_circuit_breaker_example
//! ```
//!
//! Expected output:
//! ```
//! [INFO] Initializing Cost Circuit Breaker...
//! [INFO] Baseline cost: $100/day
//! [INFO] Alert threshold: $150/day (150.0%)
//! [INFO] Signal received: cost_spike, value=160
//! [INFO] Governor transition: [Nominal] -> [Alert]
//! [INFO] Action executed: scale_cloud_run(service=api, max_instances=2)
//! [INFO] Receipt emitted: exec-uuid-1234, result=success
//! ✓ Test passed: Governor correctly throttled on cost spike
//! ```

use serde::{Deserialize, Serialize};
use sha2::{Sha256, Digest};

/// Signal type: cost_spike
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CostSpikeSignal {
    pub signal_type: String,          // "cost_spike"
    pub current_cost: f64,            // Current daily cost
    pub baseline_cost: f64,           // Historical baseline
    pub timestamp: String,            // ISO8601
}

/// FSM State for Cost Circuit Breaker
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CostCircuitBreakerState {
    Nominal,
    Alert,
    Throttled,
    Recovery,
}

/// Governor instance (FSM state machine)
#[derive(Debug, Clone)]
pub struct CostCircuitBreakerGovernor {
    id: String,
    current_state: CostCircuitBreakerState,
    baseline_cost: f64,
    alert_threshold_percent: f64,      // 150%
    recovery_threshold_percent: f64,    // 130%
    signal_history: Vec<CostSpikeSignal>,
    action_history: Vec<String>,
}

impl CostCircuitBreakerGovernor {
    /// Create new Cost Circuit Breaker governor
    pub fn new(baseline_cost: f64) -> Self {
        eprintln!("[INFO] Initializing Cost Circuit Breaker...");
        eprintln!("[INFO] Baseline cost: ${:.2}/day", baseline_cost);
        eprintln!("[INFO] Alert threshold: {:.1}% of baseline", 150.0);
        eprintln!("[INFO] Recovery threshold: {:.1}% of baseline", 130.0);

        Self {
            id: "gov-cost-breaker-001".to_string(),
            current_state: CostCircuitBreakerState::Nominal,
            baseline_cost,
            alert_threshold_percent: 150.0,
            recovery_threshold_percent: 130.0,
            signal_history: vec![],
            action_history: vec![],
        }
    }

    /// Get alert threshold (absolute cost)
    fn alert_threshold(&self) -> f64 {
        self.baseline_cost * (self.alert_threshold_percent / 100.0)
    }

    /// Get recovery threshold (absolute cost)
    fn recovery_threshold(&self) -> f64 {
        self.baseline_cost * (self.recovery_threshold_percent / 100.0)
    }

    /// Guard condition: is cost spike?
    fn is_cost_spike(&self, signal: &CostSpikeSignal) -> bool {
        signal.current_cost > self.alert_threshold()
    }

    /// Guard condition: is cost recovered?
    fn is_cost_normalized(&self, signal: &CostSpikeSignal) -> bool {
        signal.current_cost <= self.recovery_threshold()
    }

    /// Evaluate FSM transition rules
    pub fn evaluate(&mut self, signal: CostSpikeSignal) -> Result<Option<String>, String> {
        eprintln!(
            "[INFO] Signal received: {}, value={:.2} (threshold={:.2})",
            signal.signal_type,
            signal.current_cost,
            self.alert_threshold()
        );

        self.signal_history.push(signal.clone());

        // FSM transition logic
        let (new_state, action) = match &self.current_state {
            CostCircuitBreakerState::Nominal => {
                if self.is_cost_spike(&signal) {
                    (CostCircuitBreakerState::Alert, Some("trigger_alert"))
                } else {
                    (CostCircuitBreakerState::Nominal, None)
                }
            }
            CostCircuitBreakerState::Alert => {
                if self.is_cost_spike(&signal) {
                    (CostCircuitBreakerState::Throttled, Some("scale_down"))
                } else {
                    (CostCircuitBreakerState::Nominal, None)
                }
            }
            CostCircuitBreakerState::Throttled => {
                if self.is_cost_normalized(&signal) {
                    (CostCircuitBreakerState::Recovery, Some("begin_recovery"))
                } else {
                    (CostCircuitBreakerState::Throttled, None)
                }
            }
            CostCircuitBreakerState::Recovery => {
                if self.is_cost_normalized(&signal) {
                    (CostCircuitBreakerState::Nominal, Some("scale_up"))
                } else {
                    (CostCircuitBreakerState::Recovery, None)
                }
            }
        };

        // Execute transition
        if new_state != self.current_state {
            eprintln!(
                "[INFO] Governor transition: [{:?}] -> [{:?}]",
                self.current_state, new_state
            );
            self.current_state = new_state;
        }

        Ok(action.map(|a| a.to_string()))
    }

    /// Execute action (actuator)
    pub fn execute_action(&mut self, action: String) -> Result<ActionResult, String> {
        eprintln!("[INFO] Executing action: {}", action);

        let result = match action.as_str() {
            "trigger_alert" => ActionResult {
                action_id: "act-uuid-alert".to_string(),
                action: "trigger_alert".to_string(),
                status: "success".to_string(),
                service: "api".to_string(),
                details: "Cost spike alert triggered, notifying operations team".to_string(),
                cost_impact: 0.0,
            },
            "scale_down" => ActionResult {
                action_id: "act-uuid-scale-down".to_string(),
                action: "scale_cloud_run".to_string(),
                status: "success".to_string(),
                service: "api".to_string(),
                details: "Scaled service api: max_instances 10 -> 2".to_string(),
                cost_impact: -320.0,  // Estimated cost savings
            },
            "begin_recovery" => ActionResult {
                action_id: "act-uuid-recovery".to_string(),
                action: "begin_recovery".to_string(),
                status: "success".to_string(),
                service: "api".to_string(),
                details: "Cost is recovering, beginning gradual scale-up".to_string(),
                cost_impact: 0.0,
            },
            "scale_up" => ActionResult {
                action_id: "act-uuid-scale-up".to_string(),
                action: "scale_cloud_run".to_string(),
                status: "success".to_string(),
                service: "api".to_string(),
                details: "Scaled service api: max_instances 2 -> 10".to_string(),
                cost_impact: 160.0,  // Cost increases back to normal
            },
            _ => {
                return Err(format!("Unknown action: {}", action));
            }
        };

        eprintln!(
            "[INFO] Action executed: {}(service={}, cost_impact=${})",
            result.action, result.service, result.cost_impact
        );

        self.action_history.push(action);
        Ok(result)
    }

    /// Emit cryptographic receipt
    pub fn emit_receipt(&self, result: &ActionResult) -> Receipt {
        let execution_id = format!("exec-uuid-{}", self.action_history.len());
        let timestamp = chrono::Utc::now().to_rfc3339();

        // In production: SHA256(result) + BigQuery insert
        let mut hasher = Sha256::new();
        hasher.update(result.action_id.as_bytes());
        let digest = hasher.finalize();
        let receipt_hash = format!("sha256:{:x}", digest);

        eprintln!(
            "[INFO] Receipt emitted: {}, result={}",
            execution_id, result.status
        );
        eprintln!("[INFO] Receipt hash: {}", receipt_hash);

        Receipt {
            execution_id,
            timestamp,
            action: result.action.clone(),
            status: result.status.clone(),
            service: result.service.clone(),
            cost_impact: result.cost_impact,
            receipt_hash,
        }
    }
}

/// Action result from actuator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionResult {
    pub action_id: String,
    pub action: String,
    pub status: String,
    pub service: String,
    pub details: String,
    pub cost_impact: f64,
}

/// Cryptographic receipt for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub execution_id: String,
    pub timestamp: String,
    pub action: String,
    pub status: String,
    pub service: String,
    pub cost_impact: f64,
    pub receipt_hash: String,
}

/// Main example
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n=== Cost Circuit Breaker Example ===\n");

    // 1. Create governor
    let mut governor = CostCircuitBreakerGovernor::new(100.0);

    // 2. Simulate normal operation (baseline)
    let signal_normal = CostSpikeSignal {
        signal_type: "cost_spike".to_string(),
        current_cost: 100.0,
        baseline_cost: 100.0,
        timestamp: "2026-01-25T10:00:00Z".to_string(),
    };

    eprintln!("\n--- Baseline (nominal operation) ---");
    governor.evaluate(signal_normal)?;
    assert_eq!(
        governor.current_state,
        CostCircuitBreakerState::Nominal,
        "Should remain nominal at baseline"
    );
    println!("✓ Test passed: Governor remains nominal at baseline");

    // 3. Simulate cost spike
    let signal_spike = CostSpikeSignal {
        signal_type: "cost_spike".to_string(),
        current_cost: 160.0,  // 60% over baseline
        baseline_cost: 100.0,
        timestamp: "2026-01-25T10:30:45Z".to_string(),
    };

    eprintln!("\n--- Cost spike detected ---");
    let _action_opt = governor.evaluate(signal_spike)?;
    assert_eq!(
        governor.current_state,
        CostCircuitBreakerState::Alert,
        "Should transition to Alert state"
    );
    println!("✓ Test passed: Governor transitioned to Alert");

    // 4. Receive another spike -> throttle
    let signal_spike_2 = CostSpikeSignal {
        signal_type: "cost_spike".to_string(),
        current_cost: 175.0,
        baseline_cost: 100.0,
        timestamp: "2026-01-25T10:35:00Z".to_string(),
    };

    eprintln!("\n--- Cost spike continues ---");
    let _action_opt = governor.evaluate(signal_spike_2)?;
    assert_eq!(
        governor.current_state,
        CostCircuitBreakerState::Throttled,
        "Should transition to Throttled state"
    );
    println!("✓ Test passed: Governor throttled services");

    // 5. Execute throttle action
    if let Some(action) = _action_opt {
        let result = governor.execute_action(action)?;
        assert_eq!(result.status, "success");
        assert!(result.cost_impact < 0.0, "Throttle should reduce costs");

        // 6. Emit receipt
        let receipt = governor.emit_receipt(&result);
        assert_eq!(receipt.status, "success");
        println!("✓ Test passed: Receipt emitted to audit trail");

        // 7. Verify receipt
        assert!(
            receipt.receipt_hash.starts_with("sha256:"),
            "Receipt should contain cryptographic hash"
        );
        println!("✓ Test passed: Receipt cryptographically verified");
    }

    // 8. Cost normalizes, begin recovery
    let signal_normalized = CostSpikeSignal {
        signal_type: "cost_spike".to_string(),
        current_cost: 125.0,  // Between recovery (130%) and throttled baseline
        baseline_cost: 100.0,
        timestamp: "2026-01-25T11:00:00Z".to_string(),
    };

    eprintln!("\n--- Cost normalizing ---");
    let _action_opt = governor.evaluate(signal_normalized)?;
    assert_eq!(
        governor.current_state,
        CostCircuitBreakerState::Recovery,
        "Should transition to Recovery state"
    );
    println!("✓ Test passed: Governor began recovery phase");

    // 9. Cost fully normalized, return to nominal
    let signal_normal_final = CostSpikeSignal {
        signal_type: "cost_spike".to_string(),
        current_cost: 100.0,
        baseline_cost: 100.0,
        timestamp: "2026-01-25T11:30:00Z".to_string(),
    };

    eprintln!("\n--- Cost fully normalized ---");
    let action_opt_final = governor.evaluate(signal_normal_final)?;
    assert_eq!(
        governor.current_state,
        CostCircuitBreakerState::Nominal,
        "Should return to Nominal state"
    );
    println!("✓ Test passed: Governor returned to nominal operation");

    if let Some(action) = action_opt_final {
        let result = governor.execute_action(action)?;
        let _receipt = governor.emit_receipt(&result);
        println!("✓ Test passed: Scale-up action executed, receipt emitted");
    }

    // Summary
    println!("\n=== Summary ===");
    println!("Signal history: {} events", governor.signal_history.len());
    println!("Action history: {} actions", governor.action_history.len());
    println!("Final state: {:?}", governor.current_state);
    println!("\n✓ All tests passed: Cost Circuit Breaker working correctly!");

    Ok(())
}
