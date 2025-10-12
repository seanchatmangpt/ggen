# Best Practices Guide

This guide provides production-ready patterns and recommendations for using the Cleanroom Testing Framework effectively.

## Table of Contents

1. [Overview](#overview)
2. [Configuration Best Practices](#configuration-best-practices)
3. [Container Management](#container-management)
4. [Test Organization](#test-organization)
5. [Performance Optimization](#performance-optimization)
6. [Security Best Practices](#security-best-practices)
7. [Monitoring and Observability](#monitoring-and-observability)
8. [Error Handling](#error-handling)
9. [CI/CD Integration](#cicd-integration)
10. [Production Deployment](#production-deployment)

## Overview

### Core Principles

The Cleanroom Testing Framework follows these core principles:

- **Deterministic**: Tests should produce consistent results
- **Isolated**: Tests should not interfere with each other
- **Fast**: Tests should execute quickly
- **Reliable**: Tests should be stable and not flaky
- **Maintainable**: Tests should be easy to understand and modify
- **Secure**: Tests should follow security best practices

### Best Practices Categories

1. **Configuration**: Proper configuration management
2. **Container Management**: Efficient container lifecycle management
3. **Test Organization**: Structured test organization
4. **Performance**: Performance optimization techniques
5. **Security**: Security considerations and practices
6. **Monitoring**: Observability and monitoring
7. **Error Handling**: Robust error handling
8. **CI/CD**: Continuous integration and deployment
9. **Production**: Production deployment considerations

## Configuration Best Practices

### 1. Configuration Management

#### Use Configuration Files

**Good**:
```toml
# cleanroom.toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 120
test_execution_timeout = 300
enable_deterministic_execution = true
deterministic_seed = 42

[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592  # 8GB
max_disk_usage_bytes = 107374182400  # 100GB
max_container_count = 10

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+"
]
```

**Avoid**:
```rust
// Hardcoded configuration
let config = CleanroomConfig {
    enable_singleton_containers: true,
    container_startup_timeout: Duration::from_secs(120),
    test_execution_timeout: Duration::from_secs(300),
    // ... many more fields
};
```

#### Environment-Specific Configuration

**Good**:
```toml
# cleanroom.toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 120
test_execution_timeout = 300

# Development overrides
[cleanroom.dev]
container_startup_timeout = 60
test_execution_timeout = 180

# Production overrides
[cleanroom.prod]
container_startup_timeout = 300
test_execution_timeout = 600
enable_monitoring = true
```

#### Configuration Validation

**Good**:
```rust
use cleanroom::{CleanroomConfig, Result};
use std::fs;

fn load_config(path: &str) -> Result<CleanroomConfig> {
    let content = fs::read_to_string(path)?;
    let config: CleanroomConfig = toml::from_str(&content)?;
    
    // Validate configuration
    config.validate()?;
    
    Ok(config)
}
```

### 2. Resource Limits

#### Set Appropriate Limits

**Good**:
```toml
[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592  # 8GB
max_disk_usage_bytes = 107374182400  # 100GB
max_network_bandwidth_bytes_per_sec = 104857600  # 100MB/s
max_container_count = 10
max_test_execution_time = 300
enable_resource_monitoring = true
resource_cleanup_timeout = 60
```

**Avoid**:
```toml
# Too restrictive
[cleanroom.resource_limits]
max_cpu_usage_percent = 10.0
max_memory_usage_bytes = 1073741824  # 1GB
max_container_count = 1

# Too permissive
[cleanroom.resource_limits]
max_cpu_usage_percent = 100.0
max_memory_usage_bytes = 17179869184  # 16GB
max_container_count = 100
```

### 3. Security Configuration

#### Enable Security Features

**Good**:
```toml
[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+"
]
enable_audit_logging = true
security_level = "High"
```

**Avoid**:
```toml
# Disabled security features
[cleanroom.security_policy]
enable_network_isolation = false
enable_filesystem_isolation = false
enable_process_isolation = false
enable_data_redaction = false
enable_audit_logging = false
security_level = "Minimal"
```

## Container Management

### 1. Singleton Pattern

#### Use Singleton Containers

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer, RedisContainer
};

#[tokio::test]
async fn test_database() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Singleton container - created once and reused
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    postgres.wait_for_ready().await?;
    
    // Your test logic here
}

#[tokio::test]
async fn test_another_database() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Reuses the same container from previous test
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    postgres.wait_for_ready().await?;
    
    // Your test logic here
}
```

**Avoid**:
```rust
// Creating new containers for each test
#[tokio::test]
async fn test_database() {
    let docker = Docker::new().unwrap();
    let postgres_image = Postgres::default();
    let postgres_container = docker.run(postgres_image);
    
    // Your test logic here
}

#[tokio::test]
async fn test_another_database() {
    let docker = Docker::new().unwrap();
    let postgres_image = Postgres::default();
    let postgres_container = docker.run(postgres_image);  // New container!
    
    // Your test logic here
}
```

### 2. Container Lifecycle

#### Proper Lifecycle Management

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer
};

#[tokio::test]
async fn test_with_proper_lifecycle() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Get or create container
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    // Wait for container to be ready
    postgres.wait_for_ready().await?;
    
    // Execute test
    let result = environment.execute_test("database_test", || {
        // Your test logic here
        Ok("test_passed")
    }).await?;
    
    // Cleanup is automatic via RAII
    // No manual cleanup needed
}
```

**Avoid**:
```rust
// Manual container management
#[tokio::test]
async fn test_with_manual_lifecycle() {
    let docker = Docker::new().unwrap();
    let postgres_image = Postgres::default();
    let postgres_container = docker.run(postgres_image);
    
    // Your test logic here
    
    // Manual cleanup - error prone
    drop(postgres_container);
}
```

### 3. Container Health Checks

#### Implement Health Checks

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer
};

#[tokio::test]
async fn test_with_health_checks() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    // Wait for container to be ready with health check
    postgres.wait_for_ready().await?;
    
    // Verify container is healthy
    assert!(postgres.is_healthy().await?);
    
    // Your test logic here
}
```

## Test Organization

### 1. Test Structure

#### Organize Tests by Feature

**Good**:
```
tests/
├── integration/
│   ├── database_tests.rs
│   ├── redis_tests.rs
│   ├── api_tests.rs
│   └── end_to_end_tests.rs
├── unit/
│   ├── service_tests.rs
│   ├── model_tests.rs
│   └── util_tests.rs
└── performance/
    ├── load_tests.rs
    ├── stress_tests.rs
    └── benchmark_tests.rs
```

#### Use Test Modules

**Good**:
```rust
// tests/integration/database_tests.rs
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer
};

mod common;

#[tokio::test]
async fn test_database_connection() {
    let environment = common::setup_environment().await;
    let postgres = common::get_postgres_container(&environment).await;
    
    // Test database connection
    assert!(postgres.is_connected().await?);
}

#[tokio::test]
async fn test_database_queries() {
    let environment = common::setup_environment().await;
    let postgres = common::get_postgres_container(&environment).await;
    
    // Test database queries
    let result = postgres.execute_query("SELECT 1").await?;
    assert_eq!(result, vec![vec!["1"]]);
}
```

### 2. Test Data Management

#### Use Test Fixtures

**Good**:
```rust
// tests/common.rs
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer, RedisContainer
};

pub async fn setup_environment() -> CleanroomEnvironment {
    let config = CleanroomConfig::default();
    CleanroomEnvironment::new(config).await.unwrap()
}

pub async fn get_postgres_container(environment: &CleanroomEnvironment) -> PostgresContainer {
    environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await.unwrap()
}

pub async fn get_redis_container(environment: &CleanroomEnvironment) -> RedisContainer {
    environment.get_or_create_container("redis", || {
        RedisContainer::new(&environment.docker_client, None)
    }).await.unwrap()
}

pub async fn setup_test_data(postgres: &PostgresContainer) {
    postgres.execute_sql("CREATE TABLE IF NOT EXISTS test_table (id SERIAL PRIMARY KEY, name TEXT)").await.unwrap();
    postgres.execute_sql("INSERT INTO test_table (name) VALUES ('test')").await.unwrap();
}

pub async fn cleanup_test_data(postgres: &PostgresContainer) {
    postgres.execute_sql("DROP TABLE IF EXISTS test_table").await.unwrap();
}
```

### 3. Test Isolation

#### Ensure Test Isolation

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer
};

#[tokio::test]
async fn test_user_creation() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    postgres.wait_for_ready().await?;
    
    // Create isolated test data
    let test_user_id = uuid::Uuid::new_v4();
    postgres.execute_sql(&format!("INSERT INTO users (id, name) VALUES ('{}', 'test_user')", test_user_id)).await?;
    
    // Test logic
    let user = postgres.execute_query(&format!("SELECT * FROM users WHERE id = '{}'", test_user_id)).await?;
    assert_eq!(user.len(), 1);
    
    // Cleanup is automatic
}
```

**Avoid**:
```rust
// Shared test data - can cause test interference
#[tokio::test]
async fn test_user_creation() {
    // Using shared test data
    let user = postgres.execute_query("SELECT * FROM users WHERE name = 'shared_test_user'").await?;
    // This can interfere with other tests
}
```

## Performance Optimization

### 1. Resource Management

#### Optimize Resource Usage

**Good**:
```toml
[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592  # 8GB
max_disk_usage_bytes = 107374182400  # 100GB
max_network_bandwidth_bytes_per_sec = 104857600  # 100MB/s
max_container_count = 10
max_test_execution_time = 300
enable_resource_monitoring = true
resource_cleanup_timeout = 60
```

#### Use Resource Monitoring

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};

#[tokio::test]
async fn test_with_resource_monitoring() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Check resource limits before test
    environment.check_resource_limits().await?;
    
    // Execute test
    let result = environment.execute_test("resource_test", || {
        // Your test logic here
        Ok("test_passed")
    }).await?;
    
    // Check resource usage after test
    let metrics = environment.get_metrics().await?;
    println!("CPU Usage: {:.1}%", metrics.resource_usage.peak_cpu_usage_percent);
    println!("Memory Usage: {} bytes", metrics.resource_usage.peak_memory_usage_bytes);
}
```

### 2. Caching and Optimization

#### Use Connection Pooling

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    PostgresContainer
};

#[tokio::test]
async fn test_with_connection_pooling() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    let postgres = environment.get_or_create_container("postgres", || {
        PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
    }).await?;
    
    postgres.wait_for_ready().await?;
    
    // Use connection pooling for multiple queries
    let pool = postgres.create_connection_pool(10).await?;
    
    // Execute multiple queries using the pool
    for i in 0..100 {
        let result = pool.execute_query(&format!("SELECT {}", i)).await?;
        assert_eq!(result, vec![vec![i.to_string()]]);
    }
}
```

#### Optimize Test Execution

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};

#[tokio::test]
async fn test_optimized_execution() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute multiple tests concurrently
    let futures = (0..10).map(|i| {
        environment.execute_test(&format!("test_{}", i), move || {
            // Your test logic here
            Ok(format!("test_{}_passed", i))
        })
    });
    
    let results = futures::future::join_all(futures).await;
    
    // Verify all tests passed
    for result in results {
        assert!(result.is_ok());
    }
}
```

## Security Best Practices

### 1. Data Protection

#### Use Data Redaction

**Good**:
```toml
[cleanroom.security_policy]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+",
    "secret\\s*=\\s*[^\\s]+",
    "private_key\\s*=\\s*[^\\s]+"
]
```

#### Implement Access Controls

**Good**:
```toml
[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080]
blocked_addresses = [
    "10.0.0.0/8",
    "172.16.0.0/12",
    "192.168.0.0/16"
]
```

### 2. Audit and Compliance

#### Enable Audit Logging

**Good**:
```toml
[cleanroom.security_policy]
enable_audit_logging = true
audit_level = "High"
compliance_standards = [
    "SOC2",
    "ISO27001",
    "GDPR"
]
```

#### Implement Security Monitoring

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};

#[tokio::test]
async fn test_with_security_monitoring() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Enable security monitoring
    environment.enable_security_monitoring().await?;
    
    // Execute test
    let result = environment.execute_test("security_test", || {
        // Your test logic here
        Ok("test_passed")
    }).await?;
    
    // Check security events
    let security_events = environment.get_security_events().await?;
    for event in security_events {
        println!("Security event: {:?}", event);
    }
}
```

## Monitoring and Observability

### 1. Metrics Collection

#### Enable Performance Monitoring

**Good**:
```toml
[cleanroom.performance_monitoring]
enable_monitoring = true
metrics_interval = 10
enable_profiling = false
enable_memory_tracking = true

[cleanroom.performance_monitoring.thresholds]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592
max_test_execution_time = 300
max_container_startup_time = 120
```

#### Use Custom Metrics

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};

#[tokio::test]
async fn test_with_custom_metrics() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Record custom metrics
    environment.record_metric("test_started", 1).await?;
    
    // Execute test
    let result = environment.execute_test("metrics_test", || {
        // Your test logic here
        Ok("test_passed")
    }).await?;
    
    // Record custom metrics
    environment.record_metric("test_completed", 1).await?;
    environment.record_metric("test_duration", result.duration_ms).await?;
}
```

### 2. Logging and Tracing

#### Implement Structured Logging

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};
use tracing::{info, warn, error};

#[tokio::test]
async fn test_with_structured_logging() {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Enable tracing
    environment.enable_tracing().await?;
    
    // Execute test with logging
    let result = environment.execute_test("logging_test", || {
        info!("Starting test execution");
        
        // Your test logic here
        
        info!("Test execution completed");
        Ok("test_passed")
    }).await?;
    
    // Check logs
    let logs = environment.get_logs().await?;
    for log in logs {
        println!("Log: {:?}", log);
    }
}
```

## Error Handling

### 1. Robust Error Handling

#### Use Specific Error Types

**Good**:
```rust
use cleanroom::{CleanroomError, Result};

#[tokio::test]
async fn test_with_error_handling() -> Result<()> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test with error handling
    let result = environment.execute_test("error_test", || {
        // Your test logic here
        Ok("test_passed")
    }).await;
    
    match result {
        Ok(value) => {
            println!("Test succeeded: {}", value);
        }
        Err(CleanroomError::ContainerError(msg)) => {
            eprintln!("Container error: {}", msg);
            return Err(CleanroomError::ContainerError(msg));
        }
        Err(CleanroomError::ResourceLimitExceeded(msg)) => {
            eprintln!("Resource limit exceeded: {}", msg);
            return Err(CleanroomError::ResourceLimitExceeded(msg));
        }
        Err(e) => {
            eprintln!("Unexpected error: {}", e);
            return Err(e);
        }
    }
    
    Ok(())
}
```

#### Implement Error Recovery

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig,
    CleanroomError
};

#[tokio::test]
async fn test_with_error_recovery() -> Result<()> {
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Execute test with retry logic
    let mut attempts = 0;
    let max_attempts = 3;
    
    loop {
        attempts += 1;
        
        match environment.execute_test("recovery_test", || {
            // Your test logic here
            Ok("test_passed")
        }).await {
            Ok(result) => {
                println!("Test succeeded on attempt {}: {}", attempts, result);
                break;
            }
            Err(CleanroomError::ContainerError(_)) => {
                if attempts >= max_attempts {
                    eprintln!("Test failed after {} attempts", max_attempts);
                    return Err(CleanroomError::ContainerError("Max retries exceeded".to_string()));
                }
                println!("Container error on attempt {}, retrying...", attempts);
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
            Err(e) => {
                eprintln!("Unexpected error: {}", e);
                return Err(e);
            }
        }
    }
    
    Ok(())
}
```

## CI/CD Integration

### 1. Pipeline Configuration

#### GitHub Actions

**Good**:
```yaml
name: Cleanroom Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          
      - name: Install Cleanroom
        run: cargo install cleanroom
        
      - name: Run Tests
        run: cleanroom test
        
      - name: Run Integration Tests
        run: cleanroom test --test integration_tests
        
      - name: Run Performance Tests
        run: cleanroom test --test performance_tests
```

#### GitLab CI

**Good**:
```yaml
stages:
  - test
  - integration
  - performance

variables:
  RUST_BACKTRACE: 1

test:
  stage: test
  image: rust:1.70
  script:
    - cargo install cleanroom
    - cleanroom test

integration:
  stage: integration
  image: rust:1.70
  services:
    - docker:dind
  script:
    - cargo install cleanroom
    - cleanroom test --test integration_tests

performance:
  stage: performance
  image: rust:1.70
  script:
    - cargo install cleanroom
    - cleanroom test --test performance_tests
```

### 2. Test Automation

#### Automated Test Execution

**Good**:
```bash
#!/bin/bash
# test_runner.sh

set -e

echo "Starting Cleanroom test suite..."

# Run unit tests
echo "Running unit tests..."
cleanroom test --test unit_tests

# Run integration tests
echo "Running integration tests..."
cleanroom test --test integration_tests

# Run performance tests
echo "Running performance tests..."
cleanroom test --test performance_tests

# Run security tests
echo "Running security tests..."
cleanroom test --test security_tests

echo "All tests completed successfully!"
```

## Production Deployment

### 1. Production Configuration

#### Production-Ready Configuration

**Good**:
```toml
# cleanroom.prod.toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 300
test_execution_timeout = 600
enable_deterministic_execution = true
deterministic_seed = 42
enable_coverage_tracking = true
enable_snapshot_testing = true
enable_tracing = true

[cleanroom.resource_limits]
max_cpu_usage_percent = 70.0
max_memory_usage_bytes = 17179869184  # 16GB
max_disk_usage_bytes = 214748364800   # 200GB
max_network_bandwidth_bytes_per_sec = 209715200  # 200MB/s
max_container_count = 50
max_test_execution_time = 600
enable_resource_monitoring = true
resource_cleanup_timeout = 120

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080, 9090]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+",
    "secret\\s*=\\s*[^\\s]+"
]
enable_audit_logging = true
security_level = "Maximum"

[cleanroom.performance_monitoring]
enable_monitoring = true
metrics_interval = 5
enable_profiling = true
enable_memory_tracking = true

[cleanroom.performance_monitoring.thresholds]
max_cpu_usage_percent = 70.0
max_memory_usage_bytes = 17179869184
max_test_execution_time = 600
max_container_startup_time = 300
```

### 2. Monitoring and Alerting

#### Production Monitoring

**Good**:
```rust
use cleanroom::{
    CleanroomEnvironment, CleanroomConfig
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = CleanroomConfig::from_file("cleanroom.prod.toml")?;
    let environment = CleanroomEnvironment::new(config).await?;
    
    // Enable monitoring
    environment.enable_monitoring().await?;
    
    // Start health check endpoint
    environment.start_health_check_server("0.0.0.0:8080").await?;
    
    // Start metrics endpoint
    environment.start_metrics_server("0.0.0.0:9090").await?;
    
    // Start alerting
    environment.start_alerting().await?;
    
    // Keep running
    tokio::signal::ctrl_c().await?;
    
    Ok(())
}
```

### 3. Backup and Recovery

#### Implement Backup Strategy

**Good**:
```bash
#!/bin/bash
# backup.sh

BACKUP_DIR="/opt/backups/cleanroom"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup configuration
tar -czf $BACKUP_DIR/config_$DATE.tar.gz /etc/cleanroom/

# Backup data
tar -czf $BACKUP_DIR/data_$DATE.tar.gz /opt/cleanroom/data/

# Backup logs
tar -czf $BACKUP_DIR/logs_$DATE.tar.gz /var/log/cleanroom/

# Clean up old backups
find $BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete

echo "Backup completed: $DATE"
```

## Summary

### Key Best Practices

1. **Configuration**: Use configuration files, not hardcoded values
2. **Containers**: Use singleton pattern for performance
3. **Tests**: Organize tests by feature and use proper isolation
4. **Performance**: Monitor resources and optimize usage
5. **Security**: Enable security features and data redaction
6. **Monitoring**: Implement comprehensive monitoring and alerting
7. **Error Handling**: Use specific error types and implement recovery
8. **CI/CD**: Automate test execution and reporting
9. **Production**: Use production-ready configuration and monitoring

### Performance Targets

- **Setup Time**: < 10 minutes
- **Test Execution**: < 5 seconds per test
- **Resource Usage**: < 80% CPU and memory
- **Availability**: > 99.9% uptime
- **Determinism**: 100% reproducible results

### Security Targets

- **Data Protection**: 100% sensitive data redaction
- **Network Isolation**: Complete network isolation
- **Access Control**: Strict access controls
- **Audit Trail**: Comprehensive audit logging
- **Compliance**: Meet SOC2, ISO27001, GDPR requirements

Following these best practices will help you build robust, performant, and secure tests with the Cleanroom Testing Framework.
