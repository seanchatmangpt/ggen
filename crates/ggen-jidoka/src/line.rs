//! Production line with stop-the-line behavior
//!
//! The production line executes gates in sequence and halts on red signals.

use crate::{AndonSignal, Gate, JidokaError, Result};
use std::sync::Arc;
use tracing::{error, info, warn};

/// Gate execution result
#[derive(Debug, Clone)]
pub struct GateResult {
    /// Gate name
    pub gate_name: String,
    /// Signal level
    pub signal: AndonSignal,
    /// Optional message
    pub message: Option<String>,
}

impl GateResult {
    /// Create a new gate result
    #[must_use]
    pub fn new(gate_name: impl Into<String>, signal: AndonSignal) -> Self {
        Self {
            gate_name: gate_name.into(),
            signal,
            message: None,
        }
    }

    /// Add a message to the result
    #[must_use]
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = Some(message.into());
        self
    }
}

/// Production line that executes quality gates
pub struct ProductionLine {
    /// Quality gates to execute
    gates: Vec<Arc<dyn Gate>>,
    /// Whether to stop on yellow signals
    stop_on_yellow: bool,
}

impl ProductionLine {
    /// Create a new production line
    #[must_use]
    pub fn new() -> Self {
        Self {
            gates: Vec::new(),
            stop_on_yellow: false,
        }
    }

    /// Add a gate to the production line
    pub fn add_gate(&mut self, gate: Arc<dyn Gate>) -> &mut Self {
        self.gates.push(gate);
        self
    }

    /// Configure whether to stop on yellow signals
    #[must_use]
    pub fn stop_on_yellow(mut self, stop: bool) -> Self {
        self.stop_on_yellow = stop;
        self
    }

    /// Run all gates and return results
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Any gate fails to execute
    /// - A red signal is encountered (line halted)
    /// - A yellow signal is encountered and `stop_on_yellow` is true
    pub async fn run(&self) -> Result<Vec<GateResult>> {
        let mut results = Vec::new();

        info!("🏭 Starting production line with {} gates", self.gates.len());

        for gate in &self.gates {
            let gate_name = gate.name();
            info!("🚪 Executing gate: {}", gate_name);

            match gate.execute().await {
                Ok(signal) => {
                    info!("   Signal: {}", signal);

                    let result = GateResult::new(gate_name, signal);
                    results.push(result.clone());

                    match signal {
                        AndonSignal::Red => {
                            error!("🚨 RED SIGNAL - HALTING THE LINE");
                            error!("   Gate: {}", gate_name);
                            error!("   Action: Fix the issue before proceeding");
                            return Err(JidokaError::LineHalted {
                                signal,
                                reason: format!("Gate '{}' raised red signal", gate_name),
                            });
                        }
                        AndonSignal::Yellow if self.stop_on_yellow => {
                            warn!("⚠️  YELLOW SIGNAL - STOPPING THE LINE");
                            warn!("   Gate: {}", gate_name);
                            warn!("   Action: Address warnings before release");
                            return Err(JidokaError::LineHalted {
                                signal,
                                reason: format!("Gate '{}' raised yellow signal", gate_name),
                            });
                        }
                        AndonSignal::Yellow => {
                            warn!("⚠️  YELLOW SIGNAL - WARNING DETECTED");
                            warn!("   Gate: {}", gate_name);
                            warn!("   Action: Address before release");
                        }
                        AndonSignal::Green => {
                            info!("✅ GREEN SIGNAL - GATE PASSED");
                        }
                    }
                }
                Err(e) => {
                    error!("❌ Gate execution failed: {}", e);
                    error!("   Gate: {}", gate_name);
                    let result = GateResult::new(gate_name, AndonSignal::Red)
                        .with_message(e.to_string());
                    results.push(result);
                    return Err(JidokaError::LineHalted {
                        signal: AndonSignal::Red,
                        reason: format!("Gate '{}' execution failed: {}", gate_name, e),
                    });
                }
            }
        }

        info!("✨ Production line completed successfully");
        Ok(results)
    }

    /// Get the number of gates in the line
    #[must_use]
    pub fn gate_count(&self) -> usize {
        self.gates.len()
    }
}

impl Default for ProductionLine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Signal;
    use async_trait::async_trait;

    // Mock gate for testing
    #[derive(Debug)]
    struct MockGate {
        name: String,
        signal: AndonSignal,
    }

    impl MockGate {
        fn new(name: impl Into<String>, signal: AndonSignal) -> Self {
            Self {
                name: name.into(),
                signal,
            }
        }
    }

    #[async_trait]
    impl Signal for MockGate {
        async fn check(&self) -> Result<AndonSignal> {
            Ok(self.signal)
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
            Ok(self.signal)
        }
    }

    #[tokio::test]
    async fn test_production_line_all_green() {
        // Arrange
        let mut line = ProductionLine::new();
        line.add_gate(Arc::new(MockGate::new("Gate 1", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 2", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 3", AndonSignal::Green)));

        // Act
        let results = line.run().await;

        // Assert
        assert!(results.is_ok());
        let results = results.unwrap();
        assert_eq!(results.len(), 3);
        assert!(results.iter().all(|r| r.signal == AndonSignal::Green));
    }

    #[tokio::test]
    async fn test_production_line_red_signal_halts() {
        // Arrange
        let mut line = ProductionLine::new();
        line.add_gate(Arc::new(MockGate::new("Gate 1", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 2", AndonSignal::Red)))
            .add_gate(Arc::new(MockGate::new("Gate 3", AndonSignal::Green)));

        // Act
        let result = line.run().await;

        // Assert
        assert!(result.is_err());
        match result {
            Err(JidokaError::LineHalted { signal, reason }) => {
                assert_eq!(signal, AndonSignal::Red);
                assert!(reason.contains("Gate 2"));
            }
            _ => panic!("Expected LineHalted error"),
        }
    }

    #[tokio::test]
    async fn test_production_line_yellow_continues_by_default() {
        // Arrange
        let mut line = ProductionLine::new();
        line.add_gate(Arc::new(MockGate::new("Gate 1", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 2", AndonSignal::Yellow)))
            .add_gate(Arc::new(MockGate::new("Gate 3", AndonSignal::Green)));

        // Act
        let results = line.run().await;

        // Assert
        assert!(results.is_ok());
        let results = results.unwrap();
        assert_eq!(results.len(), 3);
        assert_eq!(results[1].signal, AndonSignal::Yellow);
    }

    #[tokio::test]
    async fn test_production_line_stop_on_yellow() {
        // Arrange
        let mut line = ProductionLine::new().stop_on_yellow(true);
        line.add_gate(Arc::new(MockGate::new("Gate 1", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 2", AndonSignal::Yellow)))
            .add_gate(Arc::new(MockGate::new("Gate 3", AndonSignal::Green)));

        // Act
        let result = line.run().await;

        // Assert
        assert!(result.is_err());
        match result {
            Err(JidokaError::LineHalted { signal, reason }) => {
                assert_eq!(signal, AndonSignal::Yellow);
                assert!(reason.contains("Gate 2"));
            }
            _ => panic!("Expected LineHalted error"),
        }
    }

    #[tokio::test]
    async fn test_production_line_gate_count() {
        // Arrange
        let mut line = ProductionLine::new();
        line.add_gate(Arc::new(MockGate::new("Gate 1", AndonSignal::Green)))
            .add_gate(Arc::new(MockGate::new("Gate 2", AndonSignal::Green)));

        // Act
        let count = line.gate_count();

        // Assert
        assert_eq!(count, 2);
    }

    #[tokio::test]
    async fn test_gate_result_with_message() {
        // Arrange & Act
        let result = GateResult::new("Test Gate", AndonSignal::Yellow)
            .with_message("Unused variable detected");

        // Assert
        assert_eq!(result.gate_name, "Test Gate");
        assert_eq!(result.signal, AndonSignal::Yellow);
        assert_eq!(result.message, Some("Unused variable detected".to_string()));
    }
}
