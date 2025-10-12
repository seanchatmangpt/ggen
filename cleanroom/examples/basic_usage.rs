//! Basic usage example for the cleanroom testing framework
//!
//! This example demonstrates how to use cleanroom for isolated testing
//! with deterministic execution environments.

use cleanroom::prelude::*;

fn main() -> Result<()> {
    println!("🧪 Cleanroom Testing Framework - Basic Usage Example\n");

    // Example 1: Basic command execution
    basic_command_test()?;

    // Example 2: Scenario-based testing
    scenario_test()?;

    println!("\n✅ All examples completed successfully!");
    Ok(())
}

/// Example 1: Basic command execution
fn basic_command_test() -> Result<()> {
    println!("📝 Example 1: Basic Command Execution");

    // Execute a simple command
    let result = cleanroom::run(["echo", "Hello, Cleanroom!"])?;

    println!("   ✓ Exit code: {}", result.exit_code);
    println!("   ✓ Stdout: {}", result.stdout.trim());
    println!("   ✓ Duration: {}ms", result.duration_ms);

    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("Hello, Cleanroom!"));
    println!();
    Ok(())
}

/// Example 2: Scenario-based testing
fn scenario_test() -> Result<()> {
    println!("📝 Example 2: Scenario-based Testing");

    // Create a test scenario
    let result = scenario("echo test")
        .step("echo hello".to_string(), ["echo", "hello"])
        .step("echo world".to_string(), ["echo", "world"])
        .run()?;

    println!("   ✓ Scenario completed in {}ms", result.duration_ms);
    println!("   ✓ Total steps: {}", result.steps.len());
    println!("   ✓ Backend used: {}", result.backend);

    for step in &result.steps {
        println!("   ✓ Step '{}': exit code {}", step.name, step.exit_code);
    }

    assert_eq!(result.exit_code, 0);
    println!();
    Ok(())
}
