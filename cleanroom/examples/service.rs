use cleanroom::prelude::*;

fn main() -> Result<()> {
    println!("🧪 Cleanroom Testing Framework - Service Example\n");

    // Example: Service lifecycle testing
    service_lifecycle_test()?;

    println!("\n✅ Service example completed successfully!");
    Ok(())
}

fn service_lifecycle_test() -> Result<()> {
    println!("📝 Example: Service Lifecycle Testing");

    // Create a scenario with service management
    let result = scenario("service test")
        .step(
            "setup".to_string(),
            ["sh", "-c", "echo 'Setting up service test'"],
        )
        .step(
            "test_service".to_string(),
            ["sh", "-c", "echo 'Service is running'"],
        )
        .run()?;

    println!("   ✓ Service test completed in {}ms", result.duration_ms);
    println!("   ✓ Total steps: {}", result.steps.len());
    println!("   ✓ Backend used: {}", result.backend);

    for step in &result.steps {
        println!("   ✓ Step '{}': exit code {}", step.name, step.exit_code);
    }

    assert_eq!(result.exit_code, 0);
    println!();
    Ok(())
}
