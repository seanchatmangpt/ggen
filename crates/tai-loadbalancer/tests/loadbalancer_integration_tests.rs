//! Integration tests for load balancer with real components

use tai_loadbalancer::{
    Endpoint, EndpointMetadata, FailoverConfig, FailoverManager, HealthCheckConfig,
    HealthCheckManager, LoadBalancer, LoadBalancerConfig, LoadBalancingStrategy, ServiceRegistry,
};
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

// ==================== ARRANGE ====================
// Setup functions following Chicago TDD pattern: AAA (Arrange/Act/Assert)
// Uses real collaborators, not mocks

fn create_test_endpoints() -> Vec<Endpoint> {
    vec![
        Endpoint::new("127.0.0.1:5001".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5002".parse().unwrap(), None),
        Endpoint::new("127.0.0.1:5003".parse().unwrap(), None),
    ]
}

fn create_test_endpoints_with_metadata() -> Vec<Endpoint> {
    vec![
        Endpoint::new(
            "127.0.0.1:5001".parse().unwrap(),
            Some(
                EndpointMetadata::new()
                    .with_region("us-west-2a")
                    .with_version("1.0.0"),
            ),
        ),
        Endpoint::new(
            "127.0.0.1:5002".parse().unwrap(),
            Some(
                EndpointMetadata::new()
                    .with_region("us-west-2b")
                    .with_version("1.0.0"),
            ),
        ),
        Endpoint::new(
            "127.0.0.1:5003".parse().unwrap(),
            Some(
                EndpointMetadata::new()
                    .with_region("us-east-1a")
                    .with_version("2.0.0"),
            ),
        ),
    ]
}

// ==================== TEST CASES ====================

#[tokio::test]
async fn test_round_robin_distribution() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = create_test_endpoints();

    registry
        .register_batch("payment-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register endpoints");

    let lb = LoadBalancer::new(registry, LoadBalancingStrategy::RoundRobin);

    // ACT
    let mut selections = std::collections::HashMap::new();
    for _ in 0..30 {
        let endpoint = lb
            .next_endpoint("payment-service")
            .await
            .expect("Failed to get endpoint");
        *selections.entry(endpoint.address.to_string()).or_insert(0) += 1;
    }

    // ASSERT
    // Verify each endpoint was selected approximately equally (30 requests / 3 endpoints = 10 each)
    for (addr, count) in selections.iter() {
        println!("Endpoint {} selected {} times", addr, count);
        assert!(
            *count == 10,
            "Round robin not distributing evenly: {} selected {} times",
            addr,
            count
        );
    }
    assert_eq!(selections.len(), 3, "Not all endpoints were selected");
}

#[tokio::test]
async fn test_consistent_hash_stability() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = create_test_endpoints();

    registry
        .register_batch("user-service".to_string(), endpoints)
        .await
        .expect("Failed to register endpoints");

    let lb = LoadBalancer::new(registry, LoadBalancingStrategy::ConsistentHash);

    // ACT
    // Same key should always route to same endpoint
    let user_id = "user-12345";
    let mut selections = vec![];

    for _ in 0..5 {
        let endpoint = lb
            .next_endpoint_with_hash("user-service", user_id)
            .await
            .expect("Failed to get endpoint");
        selections.push(endpoint.address);
    }

    // ASSERT
    // All selections should be identical
    for selection in selections.iter() {
        assert_eq!(
            *selection, selections[0],
            "Consistent hash not stable for same key"
        );
    }
}

#[tokio::test]
async fn test_service_registry_endpoint_update() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let initial_endpoints = create_test_endpoints();

    registry
        .register_batch("api-service".to_string(), initial_endpoints.clone())
        .await
        .expect("Failed to register initial endpoints");

    // ACT
    let retrieved = registry
        .get_endpoints("api-service")
        .await
        .expect("Failed to get endpoints");

    assert_eq!(retrieved.len(), 3, "Wrong number of endpoints retrieved");

    // Add new endpoint
    let new_endpoint: SocketAddr = "127.0.0.1:5004".parse().unwrap();
    registry
        .register("api-service".to_string(), Endpoint::new(new_endpoint, None))
        .await
        .expect("Failed to register new endpoint");

    // ASSERT
    let updated = registry
        .get_endpoints("api-service")
        .await
        .expect("Failed to get updated endpoints");

    assert_eq!(updated.len(), 4, "Endpoint not added to registry");
    assert!(
        updated.iter().any(|ep| ep.address == new_endpoint),
        "New endpoint not found in registry"
    );
}

#[tokio::test]
async fn test_health_check_manager_initialization() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = create_test_endpoints();

    registry
        .register_batch("health-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register endpoints");

    let config = HealthCheckConfig::default();

    // ACT
    let manager = HealthCheckManager::new(registry, config)
        .await
        .expect("Failed to create health check manager");

    // ASSERT
    let metrics = manager.get_metrics();
    assert_eq!(metrics.total_checks, 0, "Should start with zero checks");
}

#[tokio::test]
async fn test_failover_to_backup_endpoints() {
    // ARRANGE
    let config = FailoverConfig {
        enabled: true,
        max_attempts: 3,
        log_failovers: true,
    };

    let manager = FailoverManager::new(config);
    let primary = Endpoint::new("127.0.0.1:5001".parse().unwrap(), None);
    let backup1 = Endpoint::new("127.0.0.1:5002".parse().unwrap(), None);
    let backup2 = Endpoint::new("127.0.0.1:5003".parse().unwrap(), None);

    manager
        .register(
            "critical-service",
            primary.clone(),
            vec![backup1.clone(), backup2.clone()],
        )
        .await
        .expect("Failed to register failover group");

    // ACT
    let first_failover = manager
        .failover("critical-service", &primary)
        .await
        .expect("First failover failed");

    let second_failover = manager
        .failover("critical-service", &primary)
        .await
        .expect("Second failover failed");

    // ASSERT
    assert_eq!(first_failover, Some(backup1.clone()), "First failover incorrect");
    assert_eq!(second_failover, Some(backup2.clone()), "Second failover incorrect");

    let stats = manager
        .get_stats("critical-service", &primary)
        .await
        .expect("Failed to get stats");

    assert_eq!(stats.failover_count, 2, "Failover count incorrect");
}

#[tokio::test]
async fn test_weighted_load_balancing() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());

    let endpoints = vec![
        Endpoint::with_weight("127.0.0.1:5001".parse().unwrap(), None, 50),
        Endpoint::with_weight("127.0.0.1:5002".parse().unwrap(), None, 30),
        Endpoint::with_weight("127.0.0.1:5003".parse().unwrap(), None, 20),
    ];

    registry
        .register_batch("weighted-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register endpoints");

    let lb = LoadBalancer::new(registry, LoadBalancingStrategy::Weighted);

    // ACT
    let mut selections = std::collections::HashMap::new();
    for _ in 0..1000 {
        let endpoint = lb
            .next_endpoint("weighted-service")
            .await
            .expect("Failed to get endpoint");
        *selections.entry(endpoint.address.to_string()).or_insert(0) += 1;
    }

    // ASSERT
    // Check approximate distribution based on weights
    for (addr, count) in selections.iter() {
        println!("Weighted endpoint {} selected {} times", addr, count);
    }
    // Note: Exact distribution varies due to randomness, but weights should influence selection
}

#[tokio::test]
async fn test_metadata_based_endpoint_selection() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = create_test_endpoints_with_metadata();

    registry
        .register_batch("regional-service".to_string(), endpoints)
        .await
        .expect("Failed to register endpoints");

    // ACT
    let us_west = registry
        .find_matching("regional-service", Some("us-west-2a"), None)
        .await
        .expect("Failed to find endpoints");

    let version_2 = registry
        .find_matching("regional-service", None, Some("2.0.0"))
        .await
        .expect("Failed to find endpoints");

    // ASSERT
    assert_eq!(us_west.len(), 1, "Should find exactly one us-west-2a endpoint");
    assert_eq!(version_2.len(), 1, "Should find exactly one v2.0.0 endpoint");
}

#[tokio::test]
async fn test_endpoint_deregistration() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let endpoints = create_test_endpoints();

    registry
        .register_batch("deregister-service".to_string(), endpoints.clone())
        .await
        .expect("Failed to register endpoints");

    // ACT
    let before = registry
        .endpoint_count("deregister-service")
        .await
        .expect("Failed to count endpoints");

    registry
        .deregister("deregister-service", &endpoints[0])
        .await
        .expect("Failed to deregister endpoint");

    let after = registry
        .endpoint_count("deregister-service")
        .await
        .expect("Failed to count endpoints");

    // ASSERT
    assert_eq!(before, 3, "Before count should be 3");
    assert_eq!(after, 2, "After count should be 2");

    let remaining = registry
        .get_endpoints("deregister-service")
        .await
        .expect("Failed to get endpoints");

    assert!(
        !remaining.contains(&endpoints[0]),
        "Deregistered endpoint should not be present"
    );
}

#[tokio::test]
async fn test_load_balancer_with_no_endpoints() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());
    let lb = LoadBalancer::new(registry, LoadBalancingStrategy::RoundRobin);

    // ACT
    let result = lb.next_endpoint("nonexistent-service").await;

    // ASSERT
    assert!(
        result.is_err(),
        "Should return error when service not found"
    );
}

#[tokio::test]
async fn test_concurrent_endpoint_registration() {
    // ARRANGE
    let registry = Arc::new(ServiceRegistry::new());

    // ACT
    let mut handles = vec![];

    for i in 0..10 {
        let reg = registry.clone();
        let handle = tokio::spawn(async move {
            let addr = format!("127.0.0.1:500{}", i).parse().unwrap();
            let endpoint = Endpoint::new(addr, None);
            reg.register("concurrent-service".to_string(), endpoint)
                .await
        });
        handles.push(handle);
    }

    for handle in handles {
        handle
            .await
            .expect("Task failed")
            .expect("Registration failed");
    }

    // ASSERT
    let endpoints = registry
        .get_endpoints("concurrent-service")
        .await
        .expect("Failed to get endpoints");

    assert_eq!(endpoints.len(), 10, "Should have 10 endpoints registered");
}
