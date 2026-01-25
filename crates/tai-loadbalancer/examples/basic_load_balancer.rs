//! Basic load balancer example
//!
//! This example demonstrates:
//! - Creating a service registry
//! - Registering endpoints
//! - Creating a load balancer with different strategies
//! - Routing requests to endpoints
//! - Monitoring metrics

use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;
use tai_loadbalancer::{
    Endpoint, EndpointMetadata, IntegratedLoadBalancer, LoadBalancerConfig,
    LoadBalancingStrategy, ServiceRegistry,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== TAI Load Balancer Example ===\n");

    // Create service registry
    let registry = Arc::new(ServiceRegistry::new());

    // Register endpoints for payment service
    println!("Registering endpoints for 'payment-service'...");
    for i in 1..=3 {
        let addr: SocketAddr = format!("127.0.0.1:{}", 5000 + i).parse()?;
        let metadata = EndpointMetadata::new()
            .with_region(if i % 2 == 0 { "us-west-2a" } else { "us-west-2b" })
            .with_version("1.0.0");

        let endpoint = Endpoint::new(addr, Some(metadata));
        registry
            .register("payment-service".to_string(), endpoint)
            .await?;

        println!("  ✓ Registered endpoint: {}", addr);
    }

    // Create load balancer with round-robin strategy
    println!("\nCreating load balancer (Round-Robin strategy)...");
    let config = LoadBalancerConfig::default();
    let lb = IntegratedLoadBalancer::new(
        registry.clone(),
        LoadBalancingStrategy::RoundRobin,
        config,
    )
    .await?;

    println!("✓ Load balancer created\n");

    // Start health checking and rebalancing
    println!("Starting background services...");
    lb.start().await?;
    println!("✓ Health checks and rebalancing started\n");

    // Demonstrate round-robin routing
    println!("=== Round-Robin Routing (10 requests) ===");
    for i in 1..=10 {
        let endpoint = lb.next_endpoint("payment-service").await?;
        println!("Request {}: → {}", i, endpoint.address);
    }

    // Get metrics
    println!("\n=== Metrics ===");
    let metrics = lb.metrics();
    println!("Total requests: {}", metrics.total_requests);
    println!("Successful: {}", metrics.successful_requests);
    println!("Failed: {}", metrics.failed_requests);

    // Demonstrate consistent hash routing
    println!("\n=== Consistent Hash Routing (Session Affinity) ===");
    println!("Creating new load balancer with consistent hash strategy...");

    let lb_hash = IntegratedLoadBalancer::new(
        registry.clone(),
        LoadBalancingStrategy::ConsistentHash,
        LoadBalancerConfig::default(),
    )
    .await?;

    lb_hash.start().await?;

    let test_users = vec!["user-123", "user-456", "user-789"];
    for user_id in &test_users {
        let endpoint = lb_hash
            .next_endpoint_with_hash("payment-service", user_id)
            .await?;
        println!("{}: → {}", user_id, endpoint.address);
    }

    println!("\nVerifying hash consistency (same user → same endpoint):");
    for user_id in &test_users {
        let ep1 = lb_hash
            .next_endpoint_with_hash("payment-service", user_id)
            .await?;
        let ep2 = lb_hash
            .next_endpoint_with_hash("payment-service", user_id)
            .await?;

        if ep1.address == ep2.address {
            println!("✓ {}: Consistent ({})", user_id, ep1.address);
        } else {
            println!("✗ {}: Inconsistent!", user_id);
        }
    }

    // Demonstrate session affinity
    println!("\n=== Session Affinity ===");
    println!("Getting endpoint with session affinity for 'session-key':");

    let endpoint1 = lb
        .next_endpoint_with_affinity("payment-service", "session-key")
        .await?;
    println!("First request: → {}", endpoint1.address);

    let endpoint2 = lb
        .next_endpoint_with_affinity("payment-service", "session-key")
        .await?;
    println!("Second request: → {}", endpoint2.address);

    if endpoint1.address == endpoint2.address {
        println!("✓ Affinity maintained: Same endpoint selected");
    }

    // Demonstrate dynamic endpoint management
    println!("\n=== Dynamic Endpoint Management ===");
    println!("Current endpoint count: {}", {
        registry
            .endpoint_count("payment-service")
            .await
            .unwrap_or(0)
    });

    println!("Adding new endpoint (127.0.0.1:5004)...");
    let new_endpoint: SocketAddr = "127.0.0.1:5004".parse()?;
    registry
        .register(
            "payment-service".to_string(),
            Endpoint::new(new_endpoint, None),
        )
        .await?;

    println!(
        "Updated endpoint count: {}",
        registry
            .endpoint_count("payment-service")
            .await
            .unwrap_or(0)
    );

    // Demonstrate metadata-based routing
    println!("\n=== Metadata-Based Endpoint Selection ===");
    println!("Finding endpoints in region 'us-west-2a':");

    let west_endpoints = registry
        .find_matching("payment-service", Some("us-west-2a"), None)
        .await?;

    for endpoint in west_endpoints {
        println!("  ✓ {}", endpoint.address);
    }

    // Demonstrate configuration options
    println!("\n=== Configuration Example ===");
    println!("Creating load balancer with custom configuration...");

    let custom_config = LoadBalancerConfig {
        pool_config: tai_loadbalancer::ConnectionPoolConfig {
            min_connections: 5,
            max_connections: 50,
            acquire_timeout: Duration::from_secs(2),
            idle_timeout: Duration::from_secs(300),
            max_lifetime: Duration::from_secs(600),
        },
        health_check_config: tai_loadbalancer::HealthCheckConfig {
            check_interval: Duration::from_secs(10),
            check_timeout: Duration::from_secs(5),
            failure_threshold: 3,
            success_threshold: 2,
            grace_period: Duration::from_secs(5),
        },
        ..Default::default()
    };

    let lb_custom = IntegratedLoadBalancer::new(
        registry.clone(),
        LoadBalancingStrategy::RoundRobin,
        custom_config,
    )
    .await?;

    println!("✓ Custom load balancer created with:");
    println!("  - Min connections: 5");
    println!("  - Max connections: 50");
    println!("  - Health check interval: 10s");
    println!("  - Failure threshold: 3");

    // Cleanup
    println!("\n=== Cleanup ===");
    println!("Stopping load balancers...");
    lb.stop().await?;
    lb_hash.stop().await?;
    lb_custom.stop().await?;
    println!("✓ All services stopped\n");

    println!("=== Example Complete ===");

    Ok(())
}
