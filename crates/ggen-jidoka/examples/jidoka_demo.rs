//! Demonstration of Jidoka gates and andon signal system

use ggen_jidoka::{
    gate::{CompilerGate, LintGate, TestGate},
    monitor::SignalMonitor,
    AndonSignal, Gate, ProductionLine, Signal,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🏭 Jidoka Quality Control System Demo");
    println!("=====================================\n");

    // Create a production line
    let mut line = ProductionLine::new();

    // Add quality gates
    let working_dir = std::env::current_dir()?.to_string_lossy().to_string();
    line.add_gate(Arc::new(CompilerGate::new(&working_dir)))
        .add_gate(Arc::new(TestGate::new(&working_dir)))
        .add_gate(Arc::new(LintGate::new(&working_dir)));

    println!("📋 Production line configured with {} gates:", line.gate_count());
    println!("   1. Compiler Gate - checks for compilation errors");
    println!("   2. Test Gate - checks for test failures");
    println!("   3. Lint Gate - checks for clippy warnings\n");

    // Run the production line
    println!("🚀 Starting production line...\n");

    match line.run().await {
        Ok(results) => {
            println!("\n✨ Production line completed successfully!");
            println!("\nGate Results:");
            for (i, result) in results.iter().enumerate() {
                println!(
                    "   {}. {} - {}",
                    i + 1,
                    result.gate_name,
                    result.signal
                );
            }
        }
        Err(e) => {
            println!("\n🚨 Production line halted!");
            println!("   Error: {}", e);
            println!("\n📝 Action required:");
            println!("   1. Investigate the root cause (5 Whys analysis)");
            println!("   2. Fix the issue at the source");
            println!("   3. Re-run the production line");
        }
    }

    // Demonstrate signal monitoring
    println!("\n\n📊 Signal Monitoring Demo");
    println!("=========================\n");

    let monitor = SignalMonitor::new();

    // Create mock gate for demonstration
    struct MockGate {
        name: String,
        signal: AndonSignal,
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
            "Mock gate for demonstration"
        }
    }

    #[async_trait::async_trait]
    impl Gate for MockGate {
        async fn execute(&self) -> ggen_jidoka::Result<AndonSignal> {
            Ok(self.signal)
        }
    }

    // Simulate multiple gate executions
    println!("Running gates and tracking signals...\n");

    let gates = vec![
        MockGate {
            name: "Quality Check 1".to_string(),
            signal: AndonSignal::Green,
        },
        MockGate {
            name: "Quality Check 2".to_string(),
            signal: AndonSignal::Green,
        },
        MockGate {
            name: "Quality Check 3".to_string(),
            signal: AndonSignal::Yellow,
        },
        MockGate {
            name: "Quality Check 1".to_string(),
            signal: AndonSignal::Green,
        },
    ];

    for gate in &gates {
        let signal = monitor.monitor_gate(gate).await?;
        println!("   {} → {}", gate.name(), signal);
    }

    // Display statistics
    println!("\n📈 Statistics:");
    let all_stats = monitor.get_all_stats();
    for (gate_name, stats) in all_stats {
        println!("\n   {}:", gate_name);
        println!("      Total checks: {}", stats.total_checks);
        println!("      Success rate: {:.1}%", stats.success_rate() * 100.0);
        println!("      Warning rate: {:.1}%", stats.warning_rate() * 100.0);
        println!("      Failure rate: {:.1}%", stats.failure_rate() * 100.0);
    }

    println!("\n✅ Demo completed!");

    Ok(())
}
