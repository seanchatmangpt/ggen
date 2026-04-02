//! E2E tests for Jidoka stop-the-line propagation
//!
//! Tests:
//! - Andon signal propagation through gates
//! - Stop-the-line cascade behavior
//! - Signal level escalation
//! - Gate execution halt on critical signals
//! - Recovery after signal clearance

use async_trait::async_trait;
use ggen_jidoka::{AndonSignal, Gate, ProductionLine, Result, Signal};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Mock gate that can be configured to emit specific signals
struct MockGate {
    name: String,
    signal: Arc<RwLock<AndonSignal>>,
    call_count: Arc<RwLock<usize>>,
}

impl MockGate {
    fn new(name: &str, signal: AndonSignal) -> Self {
        Self {
            name: name.to_string(),
            signal: Arc::new(RwLock::new(signal)),
            call_count: Arc::new(RwLock::new(0)),
        }
    }

    async fn set_signal(&self, signal: AndonSignal) {
        *self.signal.write().await = signal;
    }

    async fn get_call_count(&self) -> usize {
        *self.call_count.read().await
    }
}

#[async_trait]
impl Signal for MockGate {
    async fn check(&self) -> Result<AndonSignal> {
        Ok(*self.signal.read().await)
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Mock gate for testing"
    }
}

#[async_trait]
impl Gate for MockGate {
    async fn execute(&self) -> Result<AndonSignal> {
        *self.call_count.write().await += 1;
        Ok(*self.signal.read().await)
    }
}

/// Test andon signal propagation through gates
#[tokio::test]
async fn test_signal_propagation() {
    // Arrange
    let gate1 = Arc::new(MockGate::new("gate1", AndonSignal::Green));
    let gate2 = Arc::new(MockGate::new("gate2", AndonSignal::Green));
    let gate3 = Arc::new(MockGate::new("gate3", AndonSignal::Green));

    let mut line = ProductionLine::new();
    line.add_gate(gate1.clone())
        .add_gate(gate2.clone())
        .add_gate(gate3.clone());

    // Act - All gates pass
    let result = line.run().await;

    // Assert
    assert!(result.is_ok());
    let results = result.ok().unwrap();
    assert_eq!(results.len(), 3);

    for result in &results {
        assert_eq!(result.signal, AndonSignal::Green);
    }

    // Verify all gates were executed
    assert_eq!(gate1.get_call_count().await, 1);
    assert_eq!(gate2.get_call_count().await, 1);
    assert_eq!(gate3.get_call_count().await, 1);
}

/// Test stop-the-line cascade behavior
#[tokio::test]
async fn test_stop_the_line_cascade() {
    // Arrange
    let gate1 = Arc::new(MockGate::new("gate1", AndonSignal::Green));
    let gate2 = Arc::new(MockGate::new("gate2", AndonSignal::Red));
    let gate3 = Arc::new(MockGate::new("gate3", AndonSignal::Green));

    let mut line = ProductionLine::new();
    line.add_gate(gate1.clone())
        .add_gate(gate2.clone())
        .add_gate(gate3.clone());

    // Act - Gate 2 fails, line should stop
    let result = line.run().await;

    // Assert - Line should halt on red signal
    assert!(result.is_err());

    // Verify gates executed until failure
    assert_eq!(gate1.get_call_count().await, 1);
    assert_eq!(gate2.get_call_count().await, 1);
    assert_eq!(gate3.get_call_count().await, 0); // Should not execute
}

/// Test signal level escalation
#[tokio::test]
async fn test_signal_level_escalation() {
    // Arrange - Test Yellow (warning) vs Red (critical)
    let gate_yellow = Arc::new(MockGate::new("gate_yellow", AndonSignal::Yellow));
    let gate_red = Arc::new(MockGate::new("gate_red", AndonSignal::Red));

    // Act & Assert - Yellow allows continuation
    let mut line_yellow = ProductionLine::new();
    line_yellow.add_gate(gate_yellow.clone());
    let result = line_yellow.run().await;
    assert!(
        result.is_ok(),
        "Yellow signal should allow completion with warning"
    );

    // Act & Assert - Red halts immediately
    let mut line_red = ProductionLine::new();
    line_red.add_gate(gate_red.clone());
    let result = line_red.run().await;
    assert!(result.is_err(), "Red signal should halt the line");
}

/// Test gate execution halt on critical signals
#[tokio::test]
async fn test_critical_signal_halts_execution() {
    // Arrange - Create a line with multiple gates
    let gates: Vec<Arc<MockGate>> = (0..5)
        .map(|i| Arc::new(MockGate::new(&format!("gate{}", i), AndonSignal::Green)))
        .collect();

    // Set gate 2 to emit critical signal
    gates[2].set_signal(AndonSignal::Red).await;

    let mut line = ProductionLine::new();
    for gate in gates.iter() {
        line.add_gate(gate.clone());
    }

    // Act
    let result = line.run().await;

    // Assert - Line halted
    assert!(result.is_err());

    // Verify execution pattern: gates 0, 1, 2 executed; gates 3, 4 not executed
    assert_eq!(gates[0].get_call_count().await, 1);
    assert_eq!(gates[1].get_call_count().await, 1);
    assert_eq!(gates[2].get_call_count().await, 1);
    assert_eq!(gates[3].get_call_count().await, 0);
    assert_eq!(gates[4].get_call_count().await, 0);
}

/// Test recovery after signal clearance
#[tokio::test]
async fn test_recovery_after_clearance() {
    // Arrange
    let gate1 = Arc::new(MockGate::new("gate1", AndonSignal::Green));
    let gate2 = Arc::new(MockGate::new("gate2", AndonSignal::Red));
    let gate3 = Arc::new(MockGate::new("gate3", AndonSignal::Green));

    let mut line = ProductionLine::new();
    line.add_gate(gate1.clone())
        .add_gate(gate2.clone())
        .add_gate(gate3.clone());

    // Act - First run fails
    let result = line.run().await;
    assert!(result.is_err(), "First run should fail");

    // Clear the signal
    gate2.set_signal(AndonSignal::Green).await;

    // Rebuild line (simulating new run after fix)
    let mut line = ProductionLine::new();
    line.add_gate(gate1.clone())
        .add_gate(gate2.clone())
        .add_gate(gate3.clone());

    // Act - Second run after clearance
    let result = line.run().await;

    // Assert - Should succeed now
    assert!(result.is_ok(), "Second run should succeed after clearance");

    // Verify all gates executed in second run
    assert_eq!(gate1.get_call_count().await, 2); // Executed in both runs
    assert_eq!(gate2.get_call_count().await, 2); // Executed in both runs
    assert_eq!(gate3.get_call_count().await, 1); // Only executed in second run
}

/// Test multiple critical signals in sequence
#[tokio::test]
async fn test_multiple_critical_signals() {
    // Arrange - Multiple gates with critical signals
    let gate1 = Arc::new(MockGate::new("gate1", AndonSignal::Green));
    let gate2 = Arc::new(MockGate::new("gate2", AndonSignal::Red));
    let gate3 = Arc::new(MockGate::new("gate3", AndonSignal::Red));

    let mut line = ProductionLine::new();
    line.add_gate(gate1.clone())
        .add_gate(gate2.clone())
        .add_gate(gate3.clone());

    // Act
    let result = line.run().await;

    // Assert - Halts on first critical signal
    assert!(result.is_err());
    assert_eq!(gate1.get_call_count().await, 1);
    assert_eq!(gate2.get_call_count().await, 1);
    assert_eq!(gate3.get_call_count().await, 0); // Never reached
}

/// Test signal transition sequences
#[tokio::test]
async fn test_signal_transition_sequences() {
    // Arrange
    let gate = Arc::new(MockGate::new("transition_gate", AndonSignal::Green));
    let mut line = ProductionLine::new();
    line.add_gate(gate.clone());

    // Act & Assert - Green -> Yellow -> Red transitions
    gate.set_signal(AndonSignal::Green).await;
    let mut line = ProductionLine::new();
    line.add_gate(gate.clone());
    assert!(line.run().await.is_ok());

    gate.set_signal(AndonSignal::Yellow).await;
    let mut line = ProductionLine::new();
    line.add_gate(gate.clone());
    assert!(line.run().await.is_ok()); // Yellow allows completion

    gate.set_signal(AndonSignal::Red).await;
    let mut line = ProductionLine::new();
    line.add_gate(gate.clone());
    assert!(line.run().await.is_err()); // Red halts
}

/// Test concurrent gate execution with signal propagation
#[tokio::test]
async fn test_concurrent_signal_propagation() {
    // Arrange - Multiple independent production lines
    let num_lines = 10;
    let mut handles = Vec::new();

    for line_id in 0..num_lines {
        let handle = tokio::spawn(async move {
            let gate1 = Arc::new(MockGate::new(
                &format!("line{}-gate1", line_id),
                AndonSignal::Green,
            ));
            let gate2 = Arc::new(MockGate::new(
                &format!("line{}-gate2", line_id),
                if line_id % 3 == 0 {
                    AndonSignal::Red
                } else {
                    AndonSignal::Green
                },
            ));

            let mut line = ProductionLine::new();
            line.add_gate(gate1).add_gate(gate2);

            line.run().await
        });

        handles.push(handle);
    }

    // Act - Wait for all lines
    let results: Vec<_> = futures::future::join_all(handles)
        .await
        .into_iter()
        .map(|r| r.expect("Task panicked"))
        .collect();

    // Assert - Verify expected failure pattern
    let mut success_count = 0;
    let mut failure_count = 0;

    for (i, result) in results.iter().enumerate() {
        if i % 3 == 0 {
            assert!(result.is_err(), "Line {} should fail", i);
            failure_count += 1;
        } else {
            assert!(result.is_ok(), "Line {} should succeed", i);
            success_count += 1;
        }
    }

    assert_eq!(failure_count, 4); // Lines 0, 3, 6, 9
    assert_eq!(success_count, 6); // Remaining lines
}
