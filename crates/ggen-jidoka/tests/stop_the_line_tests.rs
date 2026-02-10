//! Integration tests for stop-the-line behavior

use ggen_jidoka::{
    gate::{CompilerGate, Gate, LintGate, TestGate},
    line::ProductionLine,
    monitor::SignalMonitor,
    AndonSignal, JidokaError, Signal,
};
use std::sync::Arc;

/// Mock gate for testing
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

#[async_trait::async_trait]
impl Signal for MockGate {
    async fn check(&self) -> ggen_jidoka::Result<AndonSignal> {
        Ok(self.signal)
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Mock gate for testing"
    }
}

#[async_trait::async_trait]
impl Gate for MockGate {
    async fn execute(&self) -> ggen_jidoka::Result<AndonSignal> {
        Ok(self.signal)
    }
}

#[tokio::test]
async fn test_stop_the_line_on_red_signal() {
    // Arrange
    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(MockGate::new("Compile", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Test", AndonSignal::Red)))
        .add_gate(Arc::new(MockGate::new("Lint", AndonSignal::Green)));

    // Act
    let result = line.run().await;

    // Assert
    assert!(result.is_err());
    match result {
        Err(JidokaError::LineHalted { signal, reason }) => {
            assert_eq!(signal, AndonSignal::Red);
            assert!(reason.contains("Test"));
        }
        _ => panic!("Expected LineHalted error with red signal"),
    }
}

#[tokio::test]
async fn test_continue_on_yellow_by_default() {
    // Arrange
    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(MockGate::new("Compile", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Test", AndonSignal::Yellow)))
        .add_gate(Arc::new(MockGate::new("Lint", AndonSignal::Green)));

    // Act
    let result = line.run().await;

    // Assert - Should complete with yellow warning but not halt
    assert!(result.is_ok());
    let results = result.unwrap();
    assert_eq!(results.len(), 3);
    assert_eq!(results[1].signal, AndonSignal::Yellow);
}

#[tokio::test]
async fn test_stop_on_yellow_when_configured() {
    // Arrange
    let mut line = ProductionLine::new().stop_on_yellow(true);
    line.add_gate(Arc::new(MockGate::new("Compile", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Test", AndonSignal::Yellow)))
        .add_gate(Arc::new(MockGate::new("Lint", AndonSignal::Green)));

    // Act
    let result = line.run().await;

    // Assert
    assert!(result.is_err());
    match result {
        Err(JidokaError::LineHalted { signal, reason }) => {
            assert_eq!(signal, AndonSignal::Yellow);
            assert!(reason.contains("Test"));
        }
        _ => panic!("Expected LineHalted error with yellow signal"),
    }
}

#[tokio::test]
async fn test_all_green_signals_complete() {
    // Arrange
    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(MockGate::new("Compile", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Test", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Lint", AndonSignal::Green)));

    // Act
    let result = line.run().await;

    // Assert
    assert!(result.is_ok());
    let results = result.unwrap();
    assert_eq!(results.len(), 3);
    assert!(results.iter().all(|r| r.signal == AndonSignal::Green));
}

#[tokio::test]
async fn test_monitor_tracks_signals() {
    // Arrange
    let monitor = SignalMonitor::new();
    let gate = MockGate::new("Test Gate", AndonSignal::Green);

    // Act
    let signal = monitor.monitor_gate(&gate).await.unwrap();

    // Assert
    assert_eq!(signal, AndonSignal::Green);
    assert_eq!(monitor.event_count(), 1);

    let stats = monitor.get_stats("Test Gate").unwrap();
    assert_eq!(stats.total_checks, 1);
    assert_eq!(stats.green_count, 1);
    assert_eq!(stats.success_rate(), 1.0);
}

#[tokio::test]
async fn test_monitor_multiple_gates() {
    // Arrange
    let monitor = SignalMonitor::new();
    let gate1 = MockGate::new("Gate 1", AndonSignal::Green);
    let gate2 = MockGate::new("Gate 2", AndonSignal::Yellow);
    let gate3 = MockGate::new("Gate 1", AndonSignal::Red);

    // Act
    let _s1 = monitor.monitor_gate(&gate1).await.unwrap();
    let _s2 = monitor.monitor_gate(&gate2).await.unwrap();
    let _s3 = monitor.monitor_gate(&gate3).await.unwrap();

    // Assert
    assert_eq!(monitor.event_count(), 3);

    let stats1 = monitor.get_stats("Gate 1").unwrap();
    assert_eq!(stats1.total_checks, 2);
    assert_eq!(stats1.green_count, 1);
    assert_eq!(stats1.red_count, 1);

    let stats2 = monitor.get_stats("Gate 2").unwrap();
    assert_eq!(stats2.total_checks, 1);
    assert_eq!(stats2.yellow_count, 1);
}

#[tokio::test]
async fn test_production_line_with_monitor() {
    // Arrange
    let monitor = Arc::new(SignalMonitor::new());
    let mut line = ProductionLine::new();
    line.add_gate(Arc::new(MockGate::new("Compile", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Test", AndonSignal::Green)));

    // Act
    for gate in [
        Arc::new(MockGate::new("Compile", AndonSignal::Green)),
        Arc::new(MockGate::new("Test", AndonSignal::Green)),
    ] {
        let _ = monitor.monitor_gate(gate.as_ref()).await;
    }

    // Assert
    assert_eq!(monitor.event_count(), 2);
    let all_stats = monitor.get_all_stats();
    assert_eq!(all_stats.len(), 2);
}

#[test]
fn test_andon_signal_comparison() {
    // Arrange & Act & Assert
    assert!(AndonSignal::Green < AndonSignal::Yellow);
    assert!(AndonSignal::Yellow < AndonSignal::Red);
    assert!(AndonSignal::Green < AndonSignal::Red);
}

#[test]
fn test_gate_implementations_exist() {
    // Arrange & Act
    let compiler_gate = CompilerGate::new("/tmp");
    let test_gate = TestGate::new("/tmp");
    let lint_gate = LintGate::new("/tmp");

    // Assert
    assert_eq!(compiler_gate.name(), "Compiler Gate");
    assert_eq!(test_gate.name(), "Test Gate");
    assert_eq!(lint_gate.name(), "Lint Gate");
}
