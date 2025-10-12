//! Simple cleanroom usage example
//!
//! This example demonstrates basic usage of the cleanroom testing framework.

use cleanroom::{CleanroomEnvironment, CleanroomConfig, CleanroomGuard};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create cleanroom environment with default configuration
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Create a guard for automatic cleanup
    let _guard = CleanroomGuard::new(Arc::new(environment.clone()));

    // Execute a simple test
    let result = environment.execute_test("simple_test", || {
        println!("Running simple test...");
        Ok("test passed")
    }).await?;

    println!("Test result: {}", result);

    // Get metrics
    let metrics = environment.get_metrics().await;
    println!("Tests executed: {}", metrics.tests_executed);
    println!("Tests passed: {}", metrics.tests_passed);
    println!("Containers started: {}", metrics.containers_started);

    Ok(())
}
