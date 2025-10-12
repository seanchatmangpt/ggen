//! Examples demonstrating RAII guard patterns
//!
//! This example shows how to use various RAII guards for resource management,
//! including container guards, resource guards, and scope guards.

use cleanroom::guards::{
    ContainerGuard, ResourceGuard, ScopeGuard, SessionGuard, TestGuard,
    scope, container_guard, resource_guard, session_guard, test_guard,
};
use cleanroom::guards::container::{
    ContainerHealthGuard, ContainerResourceGuard, ContainerLifecycleGuard,
    container_health_guard, container_resource_guard, container_lifecycle_guard,
};
use cleanroom::builder::CleanroomBuilder;
use cleanroom::error::Result;
use std::sync::Arc;
use std::time::Duration;
use std::sync::atomic::{AtomicUsize, Ordering};

#[tokio::main]
async fn main() -> Result<()> {
    println!("Cleanroom Guard Patterns Examples");
    println!("==================================");

    // Example 1: Basic scope guard
    println!("\n1. Basic Scope Guard");
    {
        let _guard = ScopeGuard::new()
            .add_cleanup_action(|| {
                println!("✓ Scope cleanup executed");
            });
        
        println!("  Inside scope - guard will cleanup on drop");
    }
    println!("  Outside scope - cleanup completed");

    // Example 2: Resource guard
    println!("\n2. Resource Guard");
    let resource = "important-resource".to_string();
    let guard = ResourceGuard::new(resource)
        .add_cleanup_action(|| {
            println!("✓ Resource cleanup executed");
        });
    
    println!("  Resource: {}", guard.resource());
    println!("  Has resource: {}", guard.has_resource());
    
    let resource = guard.cleanup();
    println!("  Retrieved resource: {}", resource);

    // Example 3: Container guard
    println!("\n3. Container Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    let container_id = "example-container".to_string();
    
    // Register container
    environment_arc.register_container(container_id.clone(), "container-123".to_string()).await?;
    
    let guard = ContainerGuard::new(environment_arc.clone(), container_id.clone())
        .add_cleanup_action(|| {
            println!("✓ Container cleanup executed");
        });
    
    println!("  Container ID: {}", guard.container_id());
    println!("  Is registered: {}", guard.is_registered().await);
    println!("  Uptime: {:?}", guard.created_at().elapsed());
    
    // Guard will cleanup on drop
    drop(guard);
    
    // Give a moment for cleanup
    tokio::time::sleep(Duration::from_millis(100)).await;
    println!("  Is still registered: {}", environment_arc.is_container_registered(&container_id).await);

    // Example 4: Session guard
    println!("\n4. Session Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let guard = SessionGuard::new(environment)
        .add_cleanup_action(|| {
            println!("✓ Session cleanup executed");
        });
    
    println!("  Session ID: {}", guard.session_id());
    println!("  Uptime: {:?}", guard.created_at().elapsed());
    
    // Guard will cleanup on drop
    drop(guard);

    // Example 5: Test guard
    println!("\n5. Test Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    let test_name = "example-test".to_string();
    
    let guard = TestGuard::new(environment_arc.clone(), test_name.clone())
        .add_cleanup_action(|| {
            println!("✓ Test cleanup executed");
        });
    
    println!("  Test name: {}", guard.test_name());
    println!("  Uptime: {:?}", guard.created_at().elapsed());
    
    let result = guard.execute_test(|| {
        println!("  Executing test logic");
        Ok::<i32, cleanroom::error::CleanroomError>(42)
    }).await?;
    
    println!("  Test result: {}", result);
    
    // Guard will cleanup on drop
    drop(guard);

    // Example 6: Container health guard
    println!("\n6. Container Health Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    let container_id = "health-container".to_string();
    
    // Register container
    environment_arc.register_container(container_id.clone(), "health-123".to_string()).await?;
    
    let guard = ContainerHealthGuard::new(environment_arc.clone(), container_id.clone())
        .add_cleanup_action(|| {
            println!("✓ Health guard cleanup executed");
        });
    
    println!("  Container ID: {}", guard.container_id());
    println!("  Initial health: {:?}", guard.health_status().await);
    
    // Perform health check
    let health = guard.perform_health_check().await?;
    println!("  Health check result: {:?}", health);
    println!("  Is healthy: {}", guard.is_healthy().await);
    
    // Update resource usage
    let resources = cleanroom::guards::container::ContainerResources {
        cpu_usage_percent: 25.0,
        memory_usage_bytes: 512 * 1024 * 1024,
        disk_usage_bytes: 2 * 1024 * 1024 * 1024,
        network_bytes_sent: 1000,
        network_bytes_received: 2000,
    };
    
    guard.update_resource_usage(resources.clone()).await;
    println!("  Resource usage updated");
    
    let current_usage = guard.resource_usage().await;
    println!("  CPU usage: {:.1}%", current_usage.cpu_usage_percent);
    println!("  Memory usage: {} bytes", current_usage.memory_usage_bytes);
    
    // Guard will cleanup on drop
    drop(guard);

    // Example 7: Container resource guard
    println!("\n7. Container Resource Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    let container_id = "resource-container".to_string();
    
    // Register container
    environment_arc.register_container(container_id.clone(), "resource-123".to_string()).await?;
    
    let guard = ContainerResourceGuard::new(
        environment_arc.clone(),
        container_id.clone(),
        80.0, // Max CPU %
        1024 * 1024 * 1024, // Max memory (1GB)
        10 * 1024 * 1024 * 1024, // Max disk (10GB)
    ).add_cleanup_action(|| {
        println!("✓ Resource guard cleanup executed");
    });
    
    println!("  Container ID: {}", guard.container_id());
    
    let (max_cpu, max_memory, max_disk) = guard.resource_limits();
    println!("  Max CPU: {:.1}%", max_cpu);
    println!("  Max memory: {} bytes", max_memory);
    println!("  Max disk: {} bytes", max_disk);
    
    // Check resource limits
    let resources = cleanroom::guards::container::ContainerResources {
        cpu_usage_percent: 50.0,
        memory_usage_bytes: 512 * 1024 * 1024,
        disk_usage_bytes: 5 * 1024 * 1024 * 1024,
        network_bytes_sent: 1000,
        network_bytes_received: 2000,
    };
    
    let within_limits = guard.check_resource_limits(&resources).await;
    println!("  Within limits: {}", within_limits);
    
    // Guard will cleanup on drop
    drop(guard);

    // Example 8: Container lifecycle guard with health monitoring
    println!("\n8. Container Lifecycle Guard");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    let container_id = "lifecycle-container".to_string();
    
    // Register container
    environment_arc.register_container(container_id.clone(), "lifecycle-123".to_string()).await?;
    
    let guard = ContainerLifecycleGuard::new(
        environment_arc.clone(),
        container_id.clone(),
        Duration::from_millis(500),
    ).start_health_monitoring()
    .add_cleanup_action(|| {
        println!("✓ Lifecycle guard cleanup executed");
    });
    
    println!("  Container ID: {}", guard.container_id());
    println!("  Health monitoring started");
    
    // Let it run for a bit
    tokio::time::sleep(Duration::from_millis(1500)).await;
    
    println!("  Health monitoring running...");
    
    // Guard will cleanup on drop, stopping health monitoring
    drop(guard);

    // Example 9: Convenience functions
    println!("\n9. Convenience Functions");
    let environment = CleanroomBuilder::new()
        .with_timeout(Duration::from_secs(30))
        .build()
        .await?;
    
    let environment_arc = Arc::new(environment);
    
    let _scope = scope();
    let _container = container_guard(environment_arc.clone(), "convenience".to_string());
    let _resource = resource_guard("convenience-resource".to_string());
    let _session = session_guard(CleanroomBuilder::new().build().await?);
    let _test = test_guard(environment_arc.clone(), "convenience-test".to_string());
    let _health = container_health_guard(environment_arc.clone(), "convenience-health".to_string());
    let _resource_guard = container_resource_guard(
        environment_arc.clone(),
        "convenience-resource".to_string(),
        80.0,
        1024 * 1024 * 1024,
        10 * 1024 * 1024 * 1024,
    );
    let _lifecycle = container_lifecycle_guard(
        environment_arc,
        "convenience-lifecycle".to_string(),
        Duration::from_millis(100),
    );
    
    println!("  All convenience guards created");
    
    // All guards will be dropped here, triggering cleanup
    drop(_scope);
    drop(_container);
    drop(_resource);
    drop(_session);
    drop(_test);
    drop(_health);
    drop(_resource_guard);
    drop(_lifecycle);

    // Example 10: Guard cleanup order
    println!("\n10. Guard Cleanup Order");
    let counter = Arc::new(AtomicUsize::new(0));
    let mut actions = Vec::new();
    
    for i in 0..5 {
        let counter_clone = counter.clone();
        actions.push(Box::new(move || {
            counter_clone.fetch_add(1, Ordering::SeqCst);
            println!("  Cleanup action {} executed", i);
        }) as Box<dyn FnOnce() + Send + Sync>);
    }
    
    let guard = ScopeGuard::new();
    let mut guard = guard;
    
    for action in actions {
        guard.cleanup_actions.push(action);
    }
    
    println!("  Initial counter: {}", counter.load(Ordering::SeqCst));
    
    // Guard will be dropped here, triggering cleanup in reverse order
    drop(guard);
    
    // Give a moment for cleanup to execute
    tokio::time::sleep(Duration::from_millis(100)).await;
    println!("  Final counter: {}", counter.load(Ordering::SeqCst));

    println!("\n=== All Guard Pattern Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[tokio::test]
    async fn test_scope_guard() {
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = counter.clone();
        
        let guard = ScopeGuard::new()
            .add_cleanup_action(move || {
                counter_clone.fetch_add(1, Ordering::SeqCst);
            });
        
        assert_eq!(counter.load(Ordering::SeqCst), 0);
        
        drop(guard);
        
        tokio::time::sleep(Duration::from_millis(10)).await;
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[tokio::test]
    async fn test_resource_guard() {
        let resource = "test-resource".to_string();
        let guard = ResourceGuard::new(resource)
            .add_cleanup_action(|| {
                println!("Resource cleanup executed");
            });
        
        assert_eq!(guard.resource(), "test-resource");
        assert!(guard.has_resource());
        
        let resource = guard.cleanup();
        assert_eq!(resource, "test-resource");
    }

    #[tokio::test]
    async fn test_container_guard() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let environment_arc = Arc::new(environment);
        let container_id = "test-container".to_string();
        
        environment_arc.register_container(container_id.clone(), "test-123".to_string()).await.unwrap();
        
        let guard = ContainerGuard::new(environment_arc.clone(), container_id.clone())
            .add_cleanup_action(|| {
                println!("Container cleanup executed");
            });
        
        assert_eq!(guard.container_id(), "test-container");
        assert!(guard.is_registered().await);
        
        drop(guard);
        
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert!(!environment_arc.is_container_registered(&container_id).await);
    }

    #[tokio::test]
    async fn test_session_guard() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let guard = SessionGuard::new(environment)
            .add_cleanup_action(|| {
                println!("Session cleanup executed");
            });
        
        assert!(!guard.session_id().is_nil());
        
        drop(guard);
    }

    #[tokio::test]
    async fn test_test_guard() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let environment_arc = Arc::new(environment);
        let test_name = "test".to_string();
        
        let guard = TestGuard::new(environment_arc, test_name.clone())
            .add_cleanup_action(|| {
                println!("Test cleanup executed");
            });
        
        assert_eq!(guard.test_name(), "test");
        
        let result = guard.execute_test(|| Ok::<i32, cleanroom::error::CleanroomError>(42)).await.unwrap();
        assert_eq!(result, 42);
        
        drop(guard);
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let environment_arc = Arc::new(environment);
        
        let _scope = scope();
        let _container = container_guard(environment_arc.clone(), "test".to_string());
        let _resource = resource_guard("test".to_string());
        let _session = session_guard(CleanroomBuilder::new().build_minimal().await.unwrap());
        let _test = test_guard(environment_arc, "test".to_string());
        
        // All guards will be dropped here, triggering cleanup
        drop(_scope);
        drop(_container);
        drop(_resource);
        drop(_session);
        drop(_test);
    }
}
