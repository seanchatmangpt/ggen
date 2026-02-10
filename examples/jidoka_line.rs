//! Jidoka Production Line Example
//!
//! Demonstrates stop-the-line behavior with quality gates.
//! Red signals halt the line immediately.

use ggen_jidoka::{
    gate::{CompilerGate, LintGate, TestGate},
    AndonSignal, Gate, ProductionLine, Result,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Jidoka Production Line Example ===\n");
    println!("Simulating a production line with quality gates\n");

    // Example 1: All gates pass
    println!("Example 1: All gates GREEN (success path)");
    run_green_line().await?;

    println!("\n{}\n", "=".repeat(60));

    // Example 2: Red signal halts line
    println!("Example 2: RED signal stops the line");
    run_red_line().await;

    println!("\n{}\n", "=".repeat(60));

    // Example 3: Yellow warning continues
    println!("Example 3: YELLOW warning continues (configurable)");
    run_yellow_line().await?;

    Ok(())
}

async fn run_green_line() -> Result<()> {
    let mut line = ProductionLine::new();

    // Add mock gates that pass
    line.add_gate(Arc::new(MockGate::new("Syntax Check", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Type Check", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Unit Tests", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Integration Tests", AndonSignal::Green)));

    let results = line.run().await?;

    println!("\n✓ Production line completed successfully");
    println!("  Gates passed: {}/{}", results.len(), line.gate_count());

    for result in results {
        println!("  - {}: {}", result.gate_name, result.signal);
    }

    Ok(())
}

async fn run_red_line() {
    let mut line = ProductionLine::new();

    line.add_gate(Arc::new(MockGate::new("Syntax Check", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new("Type Check", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new(
            "Unit Tests",
            AndonSignal::Red,
        )))
        .add_gate(Arc::new(MockGate::new(
            "Integration Tests",
            AndonSignal::Green,
        )));

    match line.run().await {
        Ok(_) => println!("Unexpected success"),
        Err(e) => {
            println!("\n✓ Line halted as expected");
            println!("  Error: {}", e);
            println!("  Remaining gates were NOT executed (stopped at failure)");
        }
    }
}

async fn run_yellow_line() -> Result<()> {
    let mut line = ProductionLine::new();

    line.add_gate(Arc::new(MockGate::new("Syntax Check", AndonSignal::Green)))
        .add_gate(Arc::new(MockGate::new(
            "Linter",
            AndonSignal::Yellow,
        )))
        .add_gate(Arc::new(MockGate::new("Unit Tests", AndonSignal::Green)));

    let results = line.run().await?;

    println!("\n✓ Production line completed with warnings");
    println!("  Gates executed: {}", results.len());

    for result in results {
        let status = if result.signal.is_warning() {
            "⚠️"
        } else {
            "✓"
        };
        println!("  {} {}: {}", status, result.gate_name, result.signal);
    }

    Ok(())
}

// Mock gate for demonstration
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
impl ggen_jidoka::Signal for MockGate {
    async fn check(&self) -> Result<AndonSignal> {
        // Simulate gate execution time
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        Ok(self.signal)
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn description(&self) -> &str {
        "Mock quality gate for demonstration"
    }
}

#[async_trait::async_trait]
impl Gate for MockGate {
    async fn execute(&self) -> Result<AndonSignal> {
        self.check().await
    }
}
