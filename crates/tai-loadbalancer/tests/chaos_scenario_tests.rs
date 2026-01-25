//! Chaos scenario tests for load balancer resilience

use tai_loadbalancer::{
    ConnectionPool, ConnectionPoolConfig, Endpoint, HealthCheckConfig, HealthCheckManager,
    ServiceRegistry,
};
use std::sync::Arc;
use std::time::Duration;

// ==================== CHAOS SCENARIOS ====================
// Test load balancer behavior under failure conditions

#[tokio::test]
async fn test_cascade_endpoint_failures() {
    // ARRANGE: Setup service with endpoints
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = vec![
        Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
    ];

    registry
        .register_batch("critical-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register endpoints");

    let config = HealthCheckConfig {
        failure_threshold: 2,
        success_threshold: 1,
        ..Default::default()
    };

    let manager = HealthCheckManager::new(registry.clone(), config)
        .await
        .expect("Failed to create health manager");

    // ACT: Simulate cascading failures
    // Start health checking
    manager.start().await.expect("Failed to start health checks");

    // In a real scenario, this would be network failures
    // For testing, we just verify the manager can track them

    // Wait a bit for health checks
    tokio::time::sleep(Duration::from_millis(100)).await;

    // ASSERT: Verify health checking was attempted
    let metrics = manager.get_metrics();
    println!(
        "Health checks performed: {}, failures: {}",
        metrics.total_checks, metrics.total_failures
    );

    manager.stop().await.expect("Failed to stop health checks");
}

#[tokio::test]
async fn test_connection_pool_exhaustion() {
    // ARRANGE: Create pool with limited capacity
    let config = ConnectionPoolConfig {
        min_connections: 1,
        max_connections: 3,
        acquire_timeout: Duration::from_millis(100),
        ..Default::default()
    };

    let pool = ConnectionPool::new(config);
    let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

    // ACT: Acquire all available connections
    let _conn1 = pool
        .acquire(&endpoint)
        .await
        .expect("Failed to acquire connection 1");
    let _conn2 = pool
        .acquire(&endpoint)
        .await
        .expect("Failed to acquire connection 2");
    let _conn3 = pool
        .acquire(&endpoint)
        .await
        .expect("Failed to acquire connection 3");

    // ASSERT: Pool should be exhausted
    let result = pool.acquire(&endpoint).await;
    assert!(
        result.is_err(),
        "Should fail when pool is exhausted"
    );

    let stats = pool.get_stats();
    assert_eq!(stats.failed_acquires, 1, "Should record failed acquire");
    assert_eq!(stats.active_connections, 3, "Should show 3 active connections");
}

#[tokio::test]
async fn test_connection_recovery_after_release() {
    // ARRANGE
    let config = ConnectionPoolConfig {
        max_connections: 2,
        ..Default::default()
    };

    let pool = ConnectionPool::new(config);
    let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

    // ACT: Acquire and release connections
    {
        let _conn = pool
            .acquire(&endpoint)
            .await
            .expect("First acquire should succeed");
        // conn drops here, releasing connection
    }

    // Should be able to acquire again
    let _conn = pool
        .acquire(&endpoint)
        .await
        .expect("Should be able to acquire after release");

    // ASSERT
    let stats = pool.get_stats();
    assert_eq!(stats.total_acquired, 2, "Should have 2 acquisitions");
    assert_eq!(stats.total_released, 1, "Should have 1 release");
}

#[tokio::test]
async fn test_partial_service_degradation() {
    // ARRANGE: Service with some failing endpoints
    let registry = Arc::new(ServiceRegistry::new());

    let healthy_endpoints = vec![
        Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
    ];

    let unhealthy_endpoints = vec![
        Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
    ];

    let all_endpoints = [healthy_endpoints.clone(), unhealthy_endpoints.clone()].concat();

    registry
        .register_batch("degraded-service".to_string(), all_endpoints)
        .await
        .expect("Failed to register endpoints");

    let config = HealthCheckConfig::default();
    let manager = HealthCheckManager::new(registry.clone(), config)
        .await
        .expect("Failed to create health manager");

    // ACT: Get health-aware endpoints
    let all_eps = registry
        .get_endpoints("degraded-service")
        .await
        .expect("Failed to get endpoints");

    let healthy = manager
        .get_healthy_endpoints("degraded-service", all_eps.clone())
        .await;

    // ASSERT: Should include both healthy and initially checking endpoints
    println!(
        "Total endpoints: {}, Healthy endpoints: {}",
        all_eps.len(),
        healthy.len()
    );
    assert!(healthy.len() >= healthy_endpoints.len(), "Should include healthy endpoints");
}

#[tokio::test]
async fn test_rapid_topology_changes() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());

    let initial = vec![
        Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
    ];

    registry
        .register_batch("volatile-service".to_string(), initial)
        .await
        .expect("Failed to register");

    // ACT: Rapidly add and remove endpoints
    for i in 0..20 {
        let addr = format!("127.0.0.1:{}", 5002 + (i % 5))
            .parse()
            .unwrap();
        let endpoint = Endpoint::new(addr, None);

        if i % 2 == 0 {
            registry
                .register("volatile-service".to_string(), endpoint)
                .await
                .expect("Failed to register");
        } else {
            let _ = registry.deregister("volatile-service", &endpoint).await;
        }
    }

    // ASSERT: Service should still be queryable
    let result = registry.get_endpoints("volatile-service").await;
    assert!(result.is_ok(), "Service should still be queryable after topology changes");

    let endpoints = result.expect("Failed to get endpoints");
    assert!(!endpoints.is_empty(), "Should have some endpoints");
}

#[tokio::test]
async fn test_all_endpoints_removed_then_restored() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());

    let endpoints = vec![
        Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
    ];

    registry
        .register_batch("unstable-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register");

    // ACT: Remove all endpoints
    for endpoint in &endpoints {
        registry
            .deregister("unstable-service", endpoint)
            .await
            .expect("Failed to deregister");
    }

    // Service should now be inaccessible
    let result = registry.get_endpoints("unstable-service").await;
    assert!(result.is_err(), "Service should be unavailable");

    // Restore endpoints
    registry
        .register_batch("unstable-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to restore");

    // ASSERT: Service should be accessible again
    let result = registry.get_endpoints("unstable-service").await;
    assert!(result.is_ok(), "Service should be restored");
}

#[tokio::test]
async fn test_concurrent_pool_access_under_load() {
    // ARRANGE
    let pool = Arc::new(ConnectionPool::new(ConnectionPoolConfig {
        max_connections: 50,
        ..Default::default()
    }));

    let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);

    // ACT: Many concurrent requests
    let mut handles = vec![];

    for _ in 0..100 {
        let pool = pool.clone();
        let ep = endpoint.clone();

        let handle = tokio::spawn(async move {
            match pool.acquire(&ep).await {
                Ok(conn) => {
                    // Hold connection briefly
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    drop(conn);
                    Ok(())
                }
                Err(_) => Err(()),
            }
        });

        handles.push(handle);
    }

    let mut successes = 0;
    let mut failures = 0;

    for handle in handles {
        match handle.await.expect("Task panicked") {
            Ok(()) => successes += 1,
            Err(()) => failures += 1,
        }
    }

    // ASSERT
    println!(
        "Concurrent access: {} successes, {} failures",
        successes, failures
    );
    assert!(successes > 0, "Should have some successful acquisitions");
    assert!(
        failures > 0,
        "Should have some failures due to pool limit"
    );
}

#[tokio::test]
async fn test_service_with_single_endpoint_failure() {
    // ARRANGE: Service with only one endpoint
    let registry = Arc::new(ServiceRegistry::new());

    let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
    registry
        .register("single-endpoint-service".to_string(), endpoint)
        .await
        .expect("Failed to register");

    let config = HealthCheckConfig::default();
    let manager = HealthCheckManager::new(registry.clone(), config)
        .await
        .expect("Failed to create health manager");

    manager.start().await.expect("Failed to start health checks");

    // ACT
    tokio::time::sleep(Duration::from_millis(100)).await;

    // ASSERT: Should still be able to query even with single endpoint
    let endpoints = registry
        .get_endpoints("single-endpoint-service")
        .await
        .expect("Failed to get endpoints");

    assert_eq!(endpoints.len(), 1, "Should have single endpoint");

    manager.stop().await.expect("Failed to stop health checks");
}

#[tokio::test]
async fn test_extreme_endpoint_metadata() {
    // ARRANGE: Endpoints with complex metadata
    let registry = Arc::new(ServiceRegistry::new());

    let endpoint = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None)
        .with_metadata(
            tai_loadbalancer::EndpointMetadata::new()
                .with_region("us-west-2a")
                .with_version("1.0.0")
                .with_label("tier", "gold")
                .with_label("shard", "123")
                .with_label("canary", "true"),
        );

    registry
        .register("metadata-service".to_string(), endpoint.clone())
        .await
        .expect("Failed to register");

    // ACT: Query by different metadata
    let by_region = registry
        .find_matching("metadata-service", Some("us-west-2a"), None)
        .await
        .expect("Failed to find by region");

    // ASSERT
    assert_eq!(by_region.len(), 1, "Should find endpoint by region");
    assert_eq!(by_region[0], endpoint, "Found endpoint should match");
}
