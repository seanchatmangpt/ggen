//! Examples demonstrating the type-safe builder pattern
//!
//! This example shows how to use the CleanroomBuilder with compile-time
//! validation to create different types of cleanroom environments.

use cleanroom::builder::CleanroomBuilder;
use cleanroom::limits::ResourceLimits;
use cleanroom::policy::SecurityPolicy;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Cleanroom Builder Pattern Examples");
    println!("==================================");

    // Example 1: Minimal configuration
    println!("\n1. Minimal Configuration");
    let env = CleanroomBuilder::new().build_minimal().await?;
    println!(
        "✓ Built minimal environment with session ID: {}",
        env.session_id()
    );

    // Example 2: Secure environment
    println!("\n2. Secure Environment");
    let env = CleanroomBuilder::secure()
        .with_coverage_tracking(true)
        .with_tracing(true)
        .build()
        .await?;
    println!("✓ Built secure environment with locked security policy");

    // Example 3: Performance-optimized environment
    println!("\n3. Performance Environment");
    let env = CleanroomBuilder::performance()
        .with_coverage_tracking(false) // Disable for performance
        .build()
        .await?;
    println!("✓ Built performance environment with 60s timeout");

    // Example 4: Deterministic environment
    println!("\n4. Deterministic Environment");
    let seed = 42;
    let env = CleanroomBuilder::deterministic(seed)
        .with_snapshot_testing(true)
        .build()
        .await?;
    println!("✓ Built deterministic environment with seed: {}", seed);

    // Example 5: Development environment
    println!("\n5. Development Environment");
    let env = CleanroomBuilder::development()
        .with_max_concurrent_containers(5)
        .build()
        .await?;
    println!("✓ Built development environment with relaxed policies");

    // Example 6: Custom configuration with typestate
    println!("\n6. Custom Configuration");
    let custom_limits = ResourceLimits {
        max_cpu_usage_percent: 80.0,
        max_memory_usage_bytes: 1024 * 1024 * 1024, // 1GB
        max_disk_usage_bytes: 10 * 1024 * 1024 * 1024, // 10GB
        max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024, // 100MB/s
        max_container_count: 10,
        max_test_execution_time: Duration::from_secs(300),
        enable_resource_monitoring: true,
        resource_cleanup_timeout: Duration::from_secs(30),
    };

    let env = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(120))
        .with_security_policy(SecurityPolicy::locked())
        .with_resource_limits(custom_limits)
        .with_deterministic_execution(Some(123))
        .with_coverage_tracking(true)
        .with_snapshot_testing(true)
        .with_tracing(true)
        .build()
        .await?;
    println!("✓ Built custom environment with all features enabled");

    // Example 7: Configuration inspection
    println!("\n7. Configuration Inspection");
    let builder = CleanroomBuilder::secure().with_coverage_tracking(true);

    let config = builder.config();
    println!(
        "Security level: {:?}",
        config.security_policy.security_level
    );
    println!("Coverage tracking: {}", config.enable_coverage_tracking);
    println!(
        "Singleton containers: {}",
        config.enable_singleton_containers
    );

    let env = builder.build().await?;
    println!("✓ Built environment after inspection");

    println!("\n=== All Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_minimal_builder() {
        let env = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build minimal environment");

        assert_eq!(
            env.config().test_execution_timeout,
            Duration::from_secs(300)
        );
    }

    #[tokio::test]
    async fn test_secure_builder() {
        let env = CleanroomBuilder::secure()
            .build()
            .await
            .expect("Should build secure environment");

        assert_eq!(
            env.config().security_policy.security_level,
            cleanroom::policy::SecurityLevel::Locked
        );
    }

    #[tokio::test]
    async fn test_performance_builder() {
        let env = CleanroomBuilder::performance()
            .build()
            .await
            .expect("Should build performance environment");

        assert_eq!(env.config().test_execution_timeout, Duration::from_secs(60));
        assert!(env.config().enable_singleton_containers);
    }

    #[tokio::test]
    async fn test_deterministic_builder() {
        let seed = 42;
        let env = CleanroomBuilder::deterministic(seed)
            .build()
            .await
            .expect("Should build deterministic environment");

        assert!(env.config().enable_deterministic_execution);
        assert_eq!(env.config().deterministic_seed, Some(seed));
    }

    #[tokio::test]
    async fn test_development_builder() {
        let env = CleanroomBuilder::development()
            .build()
            .await
            .expect("Should build development environment");

        assert!(!env.config().enable_singleton_containers);
        assert!(env.config().enable_coverage_tracking);
        assert!(env.config().enable_tracing);
    }

    #[tokio::test]
    async fn test_custom_configuration() {
        let timeout = Duration::from_secs(90);
        let seed = 456;

        let env = CleanroomBuilder::new()
            .with_timeout(timeout)
            .with_deterministic_execution(Some(seed))
            .with_coverage_tracking(true)
            .build()
            .await
            .expect("Should build custom environment");

        assert_eq!(env.config().test_execution_timeout, timeout);
        assert_eq!(env.config().deterministic_seed, Some(seed));
        assert!(env.config().enable_coverage_tracking);
    }
}
