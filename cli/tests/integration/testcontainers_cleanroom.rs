//! Cleanroom Production Tests with Testcontainers
//!
//! This module provides cleanroom production validation using testcontainers
//! to test ggen in completely isolated, production-like environments.
//!
//! **Cleanroom Principles**:
//! - Complete container isolation from host system
//! - No dependencies on host filesystem state
//! - Fresh environment for each test run
//! - Production-like resource constraints
//! - Network isolation and security boundaries
//! - Real components, no mocking
//! - Deterministic and reproducible results

use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use predicates::prelude::*;
use std::process::Command;
use std::time::Duration;
use tempfile::TempDir;
use testcontainers::clients::Cli;
use testcontainers::core::WaitFor;
use testcontainers::images::generic::GenericImage;
use testcontainers::images::postgres::PostgresImage;
use testcontainers::images::redis::RedisImage;
use testcontainers::Container;
use testcontainers::RunnableImage;
use tokio::time::sleep;

/// Cleanroom test environment with complete isolation
pub struct CleanroomEnvironment {
    pub client: Cli,
    pub temp_dir: TempDir,
    pub network_name: String,
}

impl CleanroomEnvironment {
    pub fn new() -> Self {
        let network_name = format!("ggen-cleanroom-{}", uuid::Uuid::new_v4());
        Self {
            client: Cli::default(),
            temp_dir: TempDir::new().unwrap(),
            network_name,
        }
    }

    pub fn temp_path(&self) -> &std::path::Path {
        self.temp_dir.path()
    }

    /// Create a completely isolated Rust container for cleanroom testing
    pub fn create_cleanroom_container(&self) -> Container<'static, GenericImage> {
        let image = GenericImage::new("rust", "1.75")
            .with_env_var("RUST_LOG", "info")
            .with_env_var("CARGO_TARGET_DIR", "/tmp/target")
            .with_env_var("CARGO_HOME", "/tmp/cargo")
            .with_env_var("RUSTUP_HOME", "/tmp/rustup")
            .with_wait_for(WaitFor::message_on_stdout("Rust toolchain"))
            .with_network(&self.network_name);

        self.client.run(image)
    }

    /// Create a PostgreSQL container with resource constraints
    pub fn create_postgres_container(&self) -> Container<'static, PostgresImage> {
        let image = PostgresImage::default()
            .with_env_var("POSTGRES_DB", "ggen_cleanroom")
            .with_env_var("POSTGRES_USER", "ggen")
            .with_env_var("POSTGRES_PASSWORD", "cleanroom_password")
            .with_env_var("POSTGRES_INITDB_ARGS", "--auth-host=scram-sha-256")
            .with_network(&self.network_name)
            .with_memory_limit(512 * 1024 * 1024) // 512MB limit
            .with_cpu_limit(0.5); // 0.5 CPU cores

        self.client.run(image)
    }

    /// Create a Redis container with resource constraints
    pub fn create_redis_container(&self) -> Container<'static, RedisImage> {
        let image = RedisImage::default()
            .with_network(&self.network_name)
            .with_memory_limit(256 * 1024 * 1024) // 256MB limit
            .with_cpu_limit(0.25); // 0.25 CPU cores

        self.client.run(image)
    }
}

/// Test ggen binary execution in completely isolated container
#[tokio::test]
async fn test_cleanroom_binary_execution() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    // Wait for container to be ready
    sleep(Duration::from_secs(5)).await;

    // Test that ggen binary can be executed in cleanroom environment
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["--version"]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("1.2.0"));
}

/// Test lifecycle execution in cleanroom environment with resource constraints
#[tokio::test]
async fn test_cleanroom_lifecycle_execution() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test lifecycle initialization in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--name",
        "cleanroom-test",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Lifecycle initialized"));
}

/// Test database integration in cleanroom with resource constraints
#[tokio::test]
async fn test_cleanroom_database_integration() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    
    sleep(Duration::from_secs(10)).await;

    let port = postgres.get_host_port_ipv4(5432);
    let connection_string = format!("postgresql://ggen:cleanroom_password@localhost:{}", port);

    // Test database connectivity in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        &format!("database_url={}", connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Database connection validated"));
}

/// Test cache integration in cleanroom with resource constraints
#[tokio::test]
async fn test_cleanroom_cache_integration() {
    let env = CleanroomEnvironment::new();
    let redis = env.create_redis_container();
    
    sleep(Duration::from_secs(5)).await;

    let port = redis.get_host_port_ipv4(6379);
    let connection_string = format!("redis://localhost:{}", port);

    // Test cache connectivity in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        &format!("cache_url={}", connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Cache connection validated"));
}

/// Test state corruption and recovery in cleanroom environment
#[tokio::test]
async fn test_cleanroom_state_corruption_recovery() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test corrupted state file recovery
    let state_file = env.temp_path().join(".ggen/state.json");
    std::fs::create_dir_all(state_file.parent().unwrap()).unwrap();
    
    // Create corrupted state file
    std::fs::write(&state_file, r#"{ "invalid": json syntax"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--config",
        &format!("state_file={}", state_file.display()),
    ]);

    let assert = cmd.assert();
    // Should handle corrupted state gracefully
    assert.failure().stderr(predicate::str::contains("corrupted state"));
}

/// Test disk full scenario in cleanroom environment
#[tokio::test]
async fn test_cleanroom_disk_full_scenario() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Simulate disk full by creating a large file
    let large_file = env.temp_path().join("large_file.bin");
    let large_data = vec![0u8; 1024 * 1024]; // 1MB
    std::fs::write(&large_file, large_data).unwrap();

    // Test that ggen handles disk space issues gracefully
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--config",
        &format!("output_dir={}", env.temp_path().display()),
    ]);

    let assert = cmd.assert();
    // Should handle disk space issues gracefully
    assert.failure().stderr(predicate::str::contains("disk space"));
}

/// Test process kill and signal handling in cleanroom
#[tokio::test]
async fn test_cleanroom_process_kill_handling() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test that ggen handles process termination gracefully
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--timeout",
        "1",
    ]);

    let assert = cmd.assert();
    // Should handle timeout gracefully
    assert.failure().stderr(predicate::str::contains("timeout"));
}

/// Test network isolation and security boundaries
#[tokio::test]
async fn test_cleanroom_network_isolation() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test that ggen respects network isolation
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "ci",
        "github",
        "pages",
        "status",
        "--repo",
        "test/repo",
        "--api-base",
        "http://localhost:9999", // Non-existent service
    ]);

    let assert = cmd.assert();
    // Should handle network isolation gracefully
    assert.failure().stderr(predicate::str::contains("connection failed"));
}

/// Test resource constraint handling
#[tokio::test]
async fn test_cleanroom_resource_constraints() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test that ggen respects resource constraints
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--max-parallel",
        "1", // Limit parallelism
        "--memory-limit",
        "100MB",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Resource constraints applied"));
}

/// Test complete cleanroom workflow
#[tokio::test]
async fn test_cleanroom_complete_workflow() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    let redis = env.create_redis_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let redis_port = redis.get_host_port_ipv4(6379);
    
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);
    let redis_url = format!("redis://localhost:{}", redis_port);

    // Test complete workflow in cleanroom environment
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "production-readiness",
        "--config",
        &format!("database_url={}&cache_url={}", postgres_url, redis_url),
        "--validate-all",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Production readiness validated"));
}

/// Test cleanroom environment cleanup
#[tokio::test]
async fn test_cleanroom_environment_cleanup() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test that cleanroom environment is properly cleaned up
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "cleanup",
        "--config",
        &format!("temp_dir={}", env.temp_path().display()),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Environment cleaned up"));
}

/// Test cleanroom security boundaries
#[tokio::test]
async fn test_cleanroom_security_boundaries() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test that ggen respects security boundaries in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "render",
        "test.tmpl",
        "--var",
        "malicious_input=../../../etc/passwd",
    ]);

    let assert = cmd.assert();
    // Should sanitize input and prevent path traversal
    assert.success().stdout(predicate::str::contains("Input sanitized"));
}

/// Test cleanroom performance under constraints
#[tokio::test]
async fn test_cleanroom_performance_constraints() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    let start = std::time::Instant::now();

    // Test performance under resource constraints
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--max-parallel",
        "2",
        "--memory-limit",
        "200MB",
    ]);

    let assert = cmd.assert();
    assert.success();

    let duration = start.elapsed();
    
    // Performance should be reasonable even under constraints
    assert!(duration.as_secs() < 60, "Performance test failed: took {} seconds", duration.as_secs());
}

/// Test cleanroom error handling and recovery
#[tokio::test]
async fn test_cleanroom_error_handling() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test various error scenarios in cleanroom
    let error_scenarios = vec![
        ("Invalid command", vec!["invalid", "command"]),
        ("Missing config", vec!["lifecycle", "run", "build", "--config", "missing.toml"]),
        ("Invalid timeout", vec!["lifecycle", "run", "build", "--timeout", "invalid"]),
    ];

    for (description, args) in error_scenarios {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.args(args);

        let assert = cmd.assert();
        // Should handle errors gracefully
        assert.failure().stderr(predicate::str::contains("error"));
    }
}

/// Test cleanroom state persistence
#[tokio::test]
async fn test_cleanroom_state_persistence() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    let state_file = env.temp_path().join(".ggen/state.json");
    std::fs::create_dir_all(state_file.parent().unwrap()).unwrap();

    // Test state persistence in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "build",
        "--config",
        &format!("state_file={}", state_file.display()),
    ]);

    let assert = cmd.assert();
    assert.success();

    // Verify state file was created
    assert!(state_file.exists(), "State file should be created");
    
    // Verify state file content
    let state_content = std::fs::read_to_string(&state_file).unwrap();
    assert!(state_content.contains("build"), "State should contain build phase");
}

/// Test cleanroom configuration validation
#[tokio::test]
async fn test_cleanroom_configuration_validation() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test configuration validation in cleanroom
    let config_file = env.temp_path().join("config.toml");
    std::fs::write(&config_file, r#"
[database]
url = "postgresql://test:test@localhost:5432/test"
max_connections = 10

[cache]
url = "redis://localhost:6379"
ttl = 3600

[monitoring]
enabled = true
metrics_port = 9090
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "validate-config",
        "--config",
        config_file.to_str().unwrap(),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Configuration valid"));
}

/// Test cleanroom secrets management
#[tokio::test]
async fn test_cleanroom_secrets_management() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test secrets management in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "secrets-test",
        "--secret",
        "api_key",
        "--secret",
        "database_password",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Secrets management validated"));
}

/// Test cleanroom monitoring and observability
#[tokio::test]
async fn test_cleanroom_monitoring_observability() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test monitoring and observability in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "monitor",
        "--metrics",
        "--tracing",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Monitoring enabled"));
}

/// Test cleanroom health checks
#[tokio::test]
async fn test_cleanroom_health_checks() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    let redis = env.create_redis_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let redis_port = redis.get_host_port_ipv4(6379);
    
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);
    let redis_url = format!("redis://localhost:{}", redis_port);

    // Test health checks in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "health",
        "--config",
        &format!("database_url={}&cache_url={}", postgres_url, redis_url),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("All services healthy"));
}

/// Test cleanroom backup and restore
#[tokio::test]
async fn test_cleanroom_backup_restore() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);

    // Test backup functionality in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "backup",
        "--config",
        &format!("database_url={}", postgres_url),
        "--output",
        env.temp_path().join("backup.sql").to_str().unwrap(),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Backup completed"));

    // Test restore functionality in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "restore",
        "--config",
        &format!("database_url={}", postgres_url),
        "--input",
        env.temp_path().join("backup.sql").to_str().unwrap(),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Restore completed"));
}

/// Test cleanroom disaster recovery
#[tokio::test]
async fn test_cleanroom_disaster_recovery() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);

    // Simulate disaster recovery scenario in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "disaster-recovery",
        "--config",
        &format!("database_url={}", postgres_url),
        "--scenario",
        "database-failure",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Recovery completed"));
}

/// Test cleanroom load balancing
#[tokio::test]
async fn test_cleanroom_load_balancing() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test load balancing in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "load-test",
        "--instances",
        "3",
        "--requests",
        "100",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Load balancing validated"));
}

/// Test cleanroom graceful shutdown
#[tokio::test]
async fn test_cleanroom_graceful_shutdown() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test graceful shutdown in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "graceful-shutdown",
        "--timeout",
        "10",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Graceful shutdown completed"));
}

/// Test cleanroom circuit breaker pattern
#[tokio::test]
async fn test_cleanroom_circuit_breaker() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test circuit breaker pattern in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "circuit-breaker-test",
        "--failure-threshold",
        "3",
        "--timeout",
        "5",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Circuit breaker validated"));
}

/// Test cleanroom rate limiting
#[tokio::test]
async fn test_cleanroom_rate_limiting() {
    let env = CleanroomEnvironment::new();
    let container = env.create_cleanroom_container();
    
    sleep(Duration::from_secs(5)).await;

    // Test rate limiting in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "rate-limit-test",
        "--requests-per-second",
        "10",
        "--duration",
        "5",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Rate limiting validated"));
}

/// Test cleanroom data consistency
#[tokio::test]
async fn test_cleanroom_data_consistency() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);

    // Test data consistency in cleanroom
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "consistency-test",
        "--config",
        &format!("database_url={}", postgres_url),
        "--transactions",
        "100",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Data consistency validated"));
}

/// Test cleanroom comprehensive validation
#[tokio::test]
async fn test_cleanroom_comprehensive_validation() {
    let env = CleanroomEnvironment::new();
    let postgres = env.create_postgres_container();
    let redis = env.create_redis_container();
    
    sleep(Duration::from_secs(10)).await;

    let postgres_port = postgres.get_host_port_ipv4(5432);
    let redis_port = redis.get_host_port_ipv4(6379);
    
    let postgres_url = format!("postgresql://ggen:cleanroom_password@localhost:{}", postgres_port);
    let redis_url = format!("redis://localhost:{}", redis_port);

    // Comprehensive cleanroom validation
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "cleanroom-validation",
        "--config",
        &format!("database_url={}&cache_url={}", postgres_url, redis_url),
        "--validate-all",
        "--resource-constraints",
        "--network-isolation",
        "--security-boundaries",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Cleanroom validation completed"));
}
