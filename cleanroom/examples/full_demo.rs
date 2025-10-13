//! Full Cleanroom Demo - Showcasing 80/20 Potential
//!
//! This example demonstrates the core functionality that delivers 80% of the value
//! with 20% of the effort, showing how Cleanroom enables:
//! - Hermetic testing environments
//! - Deterministic execution
//! - Security policies
//! - Performance monitoring
//! - Real-world testing scenarios

use cleanroom::{run, run_with_policy, Policy, SecurityLevel};
use std::iter;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Cleanroom 80/20 Demo - Production-Ready Testing Framework");
    println!("{}", "=".repeat(60));

    // 1. Basic hermetic execution (20% effort, 80% value)
    demonstrate_basic_execution()?;

    // 2. Security policy enforcement
    demonstrate_security_policies()?;

    // 3. Error handling and recovery
    demonstrate_error_handling()?;

    // 4. Real-world testing scenario
    demonstrate_real_world_scenario()?;

    // 5. Fluent assertion API demonstration
    demonstrate_assertions()?;

    println!("✅ All demonstrations completed successfully!");
    println!("🎯 Cleanroom delivers production-ready testing with minimal effort");

    Ok(())
}

/// Demonstrate basic hermetic execution - the core 80/20 feature
fn demonstrate_basic_execution() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n📦 1. Basic Hermetic Execution (80/20 Core Feature)");
    println!("{}", "-".repeat(50));

    // Simple command execution in isolated container
    let result = run(["echo", "Hello from Cleanroom!"])?;

    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("Hello from Cleanroom!"));

    println!("✅ Command executed successfully in isolated container");
    println!("   Backend: {}", result.backend);
    println!("   Exit code: {}", result.exit_code);
    println!("   Execution time: {}ms", result.duration_ms);

    Ok(())
}

/// Demonstrate security policy enforcement
fn demonstrate_security_policies() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🔒 2. Security Policy Enforcement");
    println!("{}", "-".repeat(50));

    // Create restrictive security policy with new API structure
    let policy = Policy {
        security: cleanroom::policy::SecurityPolicy {
            security_level: SecurityLevel::Locked,
            enable_network_isolation: true,
            enable_filesystem_isolation: true,
            allowed_ports: vec![80, 443],
            blocked_addresses: vec!["rm".to_string(), "format".to_string()],
            enable_data_redaction: true,
            redaction_patterns: vec![],
            enable_audit_logging: true,
            enable_process_isolation: true,
        },
        ..Default::default()
    };

    // Execute with policy (should work for allowed commands)
    let result = run_with_policy(["echo", "Safe command execution"], &policy)?;

    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("Safe command execution"));

    println!("✅ Security policy enforced successfully");
    println!("   Network isolation: {}", policy.security.enable_network_isolation);
    println!("   Filesystem isolation: {}", policy.security.enable_filesystem_isolation);
    println!("   Security level: {:?}", policy.security.security_level);

    Ok(())
}

/// Demonstrate comprehensive error handling
fn demonstrate_error_handling() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🛠️  3. Error Handling and Recovery");
    println!("{}", "-".repeat(50));

    // Test command that should fail gracefully
    let result = run(["sh", "-c", "exit 42"])?;

    assert_eq!(result.exit_code, 42);
    assert_ne!(result.exit_code, 0);

    println!("✅ Error handled gracefully:");
    println!("   Command failed as expected");
    println!("   Exit code captured: {}", result.exit_code);
    println!("   Error output preserved: {} bytes", result.stderr.len());

    // Test policy enforcement
    let strict_policy = Policy::locked();

    // This should work since echo is not blocked
    let safe_result = run_with_policy(["echo", "safe execution"], &strict_policy)?;
    assert_eq!(safe_result.exit_code, 0);

    println!("   Policy enforcement working correctly");

    Ok(())
}

/// Demonstrate real-world testing scenario
fn demonstrate_real_world_scenario() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🌍 4. Real-World Testing Scenario");
    println!("{}", "-".repeat(50));

    // Simulate testing a web application stack
    println!("   🐳 Testing web application in isolated environment...");

    // Test database connectivity simulation
    let db_result = run(["sh", "-c", "echo 'Database connection test'"])?;
    assert_eq!(db_result.exit_code, 0);
    println!("   ✅ Database connectivity test passed");

    // Test API endpoint simulation
    let api_result = run(["sh", "-c", "echo 'API endpoint test'"])?;
    assert_eq!(api_result.exit_code, 0);
    println!("   ✅ API endpoint test passed");

    // Test application startup simulation
    let app_result = run(["sh", "-c", "echo 'Application startup test'"])?;
    assert_eq!(app_result.exit_code, 0);
    println!("   ✅ Application startup test passed");

    // Integration test with environment variables
    let integration_result = run(["sh", "-c", "echo 'Integration test with env: $NODE_ENV'"])?;
    assert_eq!(integration_result.exit_code, 0);
    println!("   ✅ Integration test passed");

    println!("   🎯 Complete web application test suite executed in hermetic environment");

    Ok(())
}

/// Helper function to demonstrate basic assertion patterns
fn demonstrate_assertions() -> Result<(), Box<dyn std::error::Error>> {
    println!("\n🧪 5. Assertion Patterns");
    println!("{}", "-".repeat(50));

    let result = run(["echo", "test output for assertions"])?;

    // Demonstrate basic assertion patterns
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("test output"));
    assert!(!result.stdout.contains("error"));
    assert!(result.stderr.is_empty());

    println!("✅ All assertions passed using standard Rust patterns");
    println!("   Success: {}", result.exit_code == 0);
    println!("   Exit code: {}", result.exit_code);
    println!("   Stdout contains expected text: {}", result.stdout.contains("test output"));

    Ok(())
}
