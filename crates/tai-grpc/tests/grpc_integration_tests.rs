//! Integration tests for gRPC service communication

use std::collections::HashMap;
use std::time::Duration;

use tai_grpc::grpc_client::{GrpcClient, GrpcClientConfig};
use tai_grpc::grpc_server::{GrpcServer, GrpcServerConfig};
use tai_grpc::resilience::{
    CircuitBreakerConfig, LoadBalancingStrategy, RetryConfig, TimeoutConfig,
};
use tai_grpc::tai::*;
use tokio::task;

#[tokio::test]
async fn test_governor_propose_policy() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50051".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    // Give server time to start
    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50051".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    let policy = Policy {
        id: "test-policy-1".to_string(),
        policy_type: "security".to_string(),
        policy_name: "test_policy".to_string(),
        version: 1,
        rules: HashMap::new(),
        enabled: true,
        created_at_ns: 0,
        updated_at_ns: 0,
    };

    let result = client.governor().propose_policy(policy).await;
    assert!(result.is_ok(), "Failed to propose policy");

    let receipt = result.unwrap();
    assert_eq!(receipt.status, "success");
}

#[tokio::test]
async fn test_coordinator_submit_signal() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50052".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50052".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    let signal = Signal {
        id: "test-signal-1".to_string(),
        signal_type: "status".to_string(),
        timestamp_ns: 0,
        metadata: HashMap::new(),
        payload: vec![],
        priority: 1,
    };

    let result = client.coordinator().submit_signal(signal).await;
    assert!(result.is_ok(), "Failed to submit signal");

    let receipt = result.unwrap();
    assert_eq!(receipt.status, "success");
}

#[tokio::test]
async fn test_scheduler_submit_task() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50053".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50053".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    let task_request = ScheduleTaskRequest {
        task_id: "task-1".to_string(),
        task_type: "build".to_string(),
        parameters: HashMap::new(),
        scheduled_time_ns: 0,
        priority: 1,
    };

    let result = client.scheduler().submit_task(task_request).await;
    assert!(result.is_ok(), "Failed to submit task");

    let receipt = result.unwrap();
    assert_eq!(receipt.status, "success");
}

#[tokio::test]
async fn test_multiple_concurrent_requests() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50054".parse().unwrap(),
        max_concurrent_streams: 1000,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50054".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    let client = std::sync::Arc::new(client);

    let mut handles = vec![];

    // Spawn 100 concurrent requests
    for i in 0..100 {
        let client = client.clone();
        let handle = task::spawn(async move {
            let policy = Policy {
                id: format!("policy-{}", i),
                policy_type: "test".to_string(),
                policy_name: format!("test_policy_{}", i),
                version: 1,
                rules: HashMap::new(),
                enabled: true,
                created_at_ns: 0,
                updated_at_ns: 0,
            };

            client.governor().propose_policy(policy).await
        });
        handles.push(handle);
    }

    // Wait for all requests
    for handle in handles {
        let result = handle.await;
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_ok(), "Request failed");
    }
}

#[tokio::test]
async fn test_get_policies() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50055".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50055".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    // Add a policy first
    let policy = Policy {
        id: "test-policy-1".to_string(),
        policy_type: "security".to_string(),
        policy_name: "test_policy".to_string(),
        version: 1,
        rules: HashMap::new(),
        enabled: true,
        created_at_ns: 0,
        updated_at_ns: 0,
    };

    let _ = client.governor().propose_policy(policy).await;

    // Now get policies
    let result = client.governor().get_policies("".to_string()).await;
    assert!(result.is_ok(), "Failed to get policies");
}

#[tokio::test]
async fn test_coordinator_get_status() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50056".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50056".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    let result = client.coordinator().get_status("test-service".to_string()).await;
    assert!(result.is_ok(), "Failed to get status");

    let status = result.unwrap();
    assert_eq!(status.service_name, "test-service");
}

#[tokio::test]
async fn test_scheduler_get_task_status() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50057".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50057".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    // Submit a task first
    let task_request = ScheduleTaskRequest {
        task_id: "task-1".to_string(),
        task_type: "build".to_string(),
        parameters: HashMap::new(),
        scheduled_time_ns: 0,
        priority: 1,
    };

    let _ = client.scheduler().submit_task(task_request).await;

    // Get task status
    let result = client.scheduler().get_task_status("task-1".to_string()).await;
    assert!(result.is_ok(), "Failed to get task status");

    let status = result.unwrap();
    assert_eq!(status.task_id, "task-1");
    assert_eq!(status.status, "pending");
}

#[tokio::test]
async fn test_health_checks() {
    let server_config = GrpcServerConfig {
        bind_addr: "127.0.0.1:50058".parse().unwrap(),
        max_concurrent_streams: 100,
        request_timeout_secs: 30,
        enable_reflection: true,
    };

    let server = GrpcServer::new(server_config);
    let _server_task = task::spawn(async move {
        let _ = server.start().await;
    });

    tokio::time::sleep(Duration::from_millis(500)).await;

    let client_config = GrpcClientConfig {
        server_addresses: vec!["127.0.0.1:50058".to_string()],
        retry_config: RetryConfig::default(),
        circuit_breaker_config: CircuitBreakerConfig::default(),
        timeout_config: TimeoutConfig::default(),
        load_balancing_strategy: LoadBalancingStrategy::RoundRobin,
        max_connection_retries: 3,
    };

    let client = GrpcClient::new(client_config)
        .await
        .expect("Failed to create client");

    // All services should report healthy
    let governor_health = client
        .governor()
        .state
        .read()
        .circuit_breaker
        .is_call_allowed();
    assert!(governor_health.is_ok());
}

#[test]
fn test_circuit_breaker_integration() {
    let config = CircuitBreakerConfig {
        failure_threshold: 3,
        open_timeout: Duration::from_millis(100),
        half_open_max_calls: 2,
    };

    let cb = tai_grpc::resilience::CircuitBreaker::new(config);

    // Test initial state
    assert_eq!(
        cb.state(),
        tai_grpc::resilience::CircuitState::Closed
    );

    // Record failures
    cb.record_failure();
    cb.record_failure();
    cb.record_failure();

    // Should be open now
    assert_eq!(
        cb.state(),
        tai_grpc::resilience::CircuitState::Open
    );

    // Check that calls are blocked
    assert!(cb.is_call_allowed().is_err());
}

#[test]
fn test_load_balancer_integration() {
    use tai_grpc::resilience::{LoadBalancer, LoadBalancingStrategy};

    let addresses = vec![
        "127.0.0.1:50051".to_string(),
        "127.0.0.1:50052".to_string(),
        "127.0.0.1:50053".to_string(),
    ];

    let lb = LoadBalancer::new(addresses, LoadBalancingStrategy::RoundRobin);

    // Test selection
    let ep1 = lb.select_endpoint().unwrap();
    let ep2 = lb.select_endpoint().unwrap();
    let ep3 = lb.select_endpoint().unwrap();
    let ep4 = lb.select_endpoint().unwrap();

    assert_ne!(ep1.address, ep2.address);
    assert_ne!(ep2.address, ep3.address);
    assert_ne!(ep3.address, ep4.address);
    assert_eq!(ep1.address, ep4.address);

    // Test connection tracking
    lb.record_connection(&ep1.address);
    lb.record_connection(&ep1.address);

    let stats = lb.get_stats();
    let ep1_stats = stats.iter().find(|e| e.address == ep1.address).unwrap();
    assert_eq!(ep1_stats.active_connections, 2);
}
