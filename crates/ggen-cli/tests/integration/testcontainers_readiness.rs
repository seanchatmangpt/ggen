//! Production Readiness Validation with Testcontainers
//!
//! This module provides comprehensive production readiness validation using testcontainers
//! to test against real services and validate production deployment scenarios.

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

/// Testcontainers client for managing containers
pub struct TestEnvironment {
    pub client: Cli,
    pub temp_dir: TempDir,
}

impl TestEnvironment {
    pub fn new() -> Self {
        Self {
            client: Cli::default(),
            temp_dir: TempDir::new().unwrap(),
        }
    }

    pub fn temp_path(&self) -> &std::path::Path {
        self.temp_dir.path()
    }
}

/// PostgreSQL test container for database integration testing
pub struct PostgresTestContainer {
    pub container: Container<'static, PostgresImage>,
    pub connection_string: String,
}

impl PostgresTestContainer {
    pub fn new(client: &Cli) -> Self {
        let image = PostgresImage::default()
            .with_env_var("POSTGRES_DB", "ggen_test")
            .with_env_var("POSTGRES_USER", "ggen")
            .with_env_var("POSTGRES_PASSWORD", "ggen_password");

        let container = client.run(image);
        let port = container.get_host_port_ipv4(5432);
        let connection_string = format!("postgresql://ggen:ggen_password@localhost:{}", port);

        Self {
            container,
            connection_string,
        }
    }

    pub async fn wait_for_ready(&self) {
        // Wait for PostgreSQL to be ready
        sleep(Duration::from_secs(5)).await;
    }
}

/// Redis test container for caching and session storage testing
pub struct RedisTestContainer {
    pub container: Container<'static, RedisImage>,
    pub connection_string: String,
}

impl RedisTestContainer {
    pub fn new(client: &Cli) -> Self {
        let image = RedisImage::default();
        let container = client.run(image);
        let port = container.get_host_port_ipv4(6379);
        let connection_string = format!("redis://localhost:{}", port);

        Self {
            container,
            connection_string,
        }
    }

    pub async fn wait_for_ready(&self) {
        // Wait for Redis to be ready
        sleep(Duration::from_secs(2)).await;
    }
}

/// Mock API server container for testing external integrations
pub struct MockApiContainer {
    pub container: Container<'static, GenericImage>,
    pub base_url: String,
}

impl MockApiContainer {
    pub fn new(client: &Cli) -> Self {
        // Use a simple HTTP server image for mocking API responses
        let image = GenericImage::new("nginx", "alpine")
            .with_exposed_port(80)
            .with_wait_for(WaitFor::message_on_stdout("start worker process"));

        let container = client.run(image);
        let port = container.get_host_port_ipv4(80);
        let base_url = format!("http://localhost:{}", port);

        Self {
            container,
            base_url,
        }
    }

    pub async fn wait_for_ready(&self) {
        // Wait for nginx to be ready
        sleep(Duration::from_secs(3)).await;
    }
}

/// Production readiness test suite using testcontainers
#[tokio::test]
async fn test_production_readiness_database_integration() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    // Test database connectivity and schema validation
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        &format!("database_url={}", postgres.connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Database connection validated"));
}

#[tokio::test]
async fn test_production_readiness_cache_integration() {
    let env = TestEnvironment::new();
    let redis = RedisTestContainer::new(&env.client);
    redis.wait_for_ready().await;

    // Test Redis connectivity and caching
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        &format!("cache_url={}", redis.connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Cache connection validated"));
}

#[tokio::test]
async fn test_production_readiness_api_integration() {
    let env = TestEnvironment::new();
    let mock_api = MockApiContainer::new(&env.client);
    mock_api.wait_for_ready().await;

    // Test external API integration
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "ci",
        "github",
        "pages",
        "status",
        "--repo",
        "test/repo",
        "--api-base",
        &mock_api.base_url,
    ]);

    let assert = cmd.assert();
    // Should handle API errors gracefully
    assert.failure().stderr(predicate::str::contains("API integration validated"));
}

#[tokio::test]
async fn test_production_readiness_concurrent_operations() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    let redis = RedisTestContainer::new(&env.client);
    
    postgres.wait_for_ready().await;
    redis.wait_for_ready().await;

    // Test concurrent operations across multiple services
    let mut handles = Vec::new();

    // Spawn multiple concurrent operations
    for i in 0..5 {
        let postgres_url = postgres.connection_string.clone();
        let redis_url = redis.connection_string.clone();
        
        let handle = tokio::spawn(async move {
            let mut cmd = Command::cargo_bin("ggen").unwrap();
            cmd.args([
                "lifecycle",
                "run",
                "build",
                "--config",
                &format!("database_url={}&cache_url={}", postgres_url, redis_url),
            ]);

            let assert = cmd.assert();
            assert.success()
        });
        
        handles.push(handle);
    }

    // Wait for all operations to complete
    for handle in handles {
        handle.await.unwrap();
    }
}

#[tokio::test]
async fn test_production_readiness_error_handling() {
    let env = TestEnvironment::new();
    
    // Test with invalid database connection
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        "database_url=postgresql://invalid:invalid@localhost:9999/invalid",
    ]);

    let assert = cmd.assert();
    assert.failure().stderr(predicate::str::contains("Connection failed"));
}

#[tokio::test]
async fn test_production_readiness_performance_validation() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    let start = std::time::Instant::now();

    // Test performance under load
    let mut handles = Vec::new();
    for _ in 0..10 {
        let postgres_url = postgres.connection_string.clone();
        
        let handle = tokio::spawn(async move {
            let mut cmd = Command::cargo_bin("ggen").unwrap();
            cmd.args([
                "lifecycle",
                "run",
                "test",
                "--config",
                &format!("database_url={}", postgres_url),
            ]);

            let assert = cmd.assert();
            assert.success()
        });
        
        handles.push(handle);
    }

    // Wait for all operations to complete
    for handle in handles {
        handle.await.unwrap();
    }

    let duration = start.elapsed();
    
    // Performance assertion: all operations should complete within 30 seconds
    assert!(duration.as_secs() < 30, "Performance test failed: took {} seconds", duration.as_secs());
}

#[tokio::test]
async fn test_production_readiness_security_validation() {
    let env = TestEnvironment::new();
    
    // Test SQL injection protection
    let malicious_input = "'; DROP TABLE users; --";
    
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "template",
        "render",
        "test.tmpl",
        "--var",
        &format!("user_input={}", malicious_input),
    ]);

    let assert = cmd.assert();
    // Should sanitize input and not execute malicious SQL
    assert.success().stdout(predicate::str::contains("Input sanitized"));
}

#[tokio::test]
async fn test_production_readiness_resource_cleanup() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    // Test resource cleanup after operations
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "cleanup",
        "--config",
        &format!("database_url={}", postgres.connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Resources cleaned up"));
}

#[tokio::test]
async fn test_production_readiness_monitoring_integration() {
    let env = TestEnvironment::new();
    
    // Test monitoring and observability features
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

#[tokio::test]
async fn test_production_readiness_health_checks() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    let redis = RedisTestContainer::new(&env.client);
    
    postgres.wait_for_ready().await;
    redis.wait_for_ready().await;

    // Test health check endpoints
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "health",
        "--config",
        &format!("database_url={}&cache_url={}", postgres.connection_string, redis.connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("All services healthy"));
}

#[tokio::test]
async fn test_production_readiness_backup_and_restore() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    // Test backup functionality
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "backup",
        "--config",
        &format!("database_url={}", postgres.connection_string),
        "--output",
        env.temp_path().join("backup.sql").to_str().unwrap(),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Backup completed"));

    // Test restore functionality
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "restore",
        "--config",
        &format!("database_url={}", postgres.connection_string),
        "--input",
        env.temp_path().join("backup.sql").to_str().unwrap(),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Restore completed"));
}

#[tokio::test]
async fn test_production_readiness_disaster_recovery() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    // Simulate disaster recovery scenario
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "disaster-recovery",
        "--config",
        &format!("database_url={}", postgres.connection_string),
        "--scenario",
        "database-failure",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Recovery completed"));
}

#[tokio::test]
async fn test_production_readiness_load_balancing() {
    let env = TestEnvironment::new();
    
    // Test load balancing across multiple instances
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

#[tokio::test]
async fn test_production_readiness_graceful_shutdown() {
    let env = TestEnvironment::new();
    
    // Test graceful shutdown handling
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

#[tokio::test]
async fn test_production_readiness_configuration_validation() {
    let env = TestEnvironment::new();
    
    // Test configuration validation
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

#[tokio::test]
async fn test_production_readiness_secrets_management() {
    let env = TestEnvironment::new();
    
    // Test secrets management
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

#[tokio::test]
async fn test_production_readiness_circuit_breaker() {
    let env = TestEnvironment::new();
    
    // Test circuit breaker pattern
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

#[tokio::test]
async fn test_production_readiness_rate_limiting() {
    let env = TestEnvironment::new();
    
    // Test rate limiting
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

#[tokio::test]
async fn test_production_readiness_data_consistency() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    postgres.wait_for_ready().await;

    // Test data consistency across operations
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "consistency-test",
        "--config",
        &format!("database_url={}", postgres.connection_string),
        "--transactions",
        "100",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Data consistency validated"));
}

#[tokio::test]
async fn test_production_readiness_comprehensive_validation() {
    let env = TestEnvironment::new();
    let postgres = PostgresTestContainer::new(&env.client);
    let redis = RedisTestContainer::new(&env.client);
    let mock_api = MockApiContainer::new(&env.client);
    
    postgres.wait_for_ready().await;
    redis.wait_for_ready().await;
    mock_api.wait_for_ready().await;

    // Comprehensive production readiness validation
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "run",
        "production-readiness",
        "--config",
        &format!(
            "database_url={}&cache_url={}&api_base_url={}",
            postgres.connection_string,
            redis.connection_string,
            mock_api.base_url
        ),
        "--validate-all",
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Production readiness validated"));
}