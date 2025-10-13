# Rust Testing Best Practices for Production-Ready Frameworks

**Project**: Cleanroom Testing Framework
**Version**: 1.0
**Date**: 2025-10-13
**Agent**: Researcher Agent (Hive Mind)
**Status**: Production Guidelines

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Production-Ready Criteria](#production-ready-criteria)
3. [Rust Testing Patterns](#rust-testing-patterns)
4. [Async Testing Best Practices](#async-testing-best-practices)
5. [Testcontainers Best Practices](#testcontainers-best-practices)
6. [Error Handling in Tests](#error-handling-in-tests)
7. [Performance Testing](#performance-testing)
8. [CI/CD Integration](#cicd-integration)
9. [Common Pitfalls to Avoid](#common-pitfalls-to-avoid)
10. [Production Readiness Checklist](#production-readiness-checklist)

---

## Executive Summary

This guide documents industry-leading best practices for building production-ready Rust testing frameworks, with a focus on async patterns, testcontainers, and reliability at scale.

### Key Principles

1. **Graceful Error Handling**: Never use `.expect()` or `.unwrap()` in production code
2. **Test Isolation**: Each test must be completely independent
3. **Deterministic Execution**: Tests must produce consistent results
4. **Fast Feedback**: Tests should complete in < 3s for CLI operations
5. **High Coverage**: 85%+ coverage on critical paths
6. **CI/CD Ready**: Tests must run reliably in automated environments

### The 80/20 Rule

Focus on the 20% of tests that provide 80% confidence:
- Integration tests for critical workflows
- E2E tests for user scenarios
- Performance benchmarks for SLOs
- Error handling for production failures

---

## Production-Ready Criteria

### What Makes a Testing Framework Production-Ready?

A production-ready testing framework must meet these criteria:

#### 1. Reliability (99%+ Test Pass Rate)
```rust
// ✅ GOOD: Reliable test with proper error handling
#[tokio::test]
async fn test_container_lifecycle() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default())
        .await
        .context("Failed to create cleanroom environment")?;

    let container_id = env.start_container("test")
        .await
        .context("Failed to start container")?;

    // Verify container is running
    assert!(env.is_container_running(&container_id).await?);

    // Cleanup is automatic via Drop trait
    Ok(())
}

// ❌ BAD: Unreliable test with panics
#[tokio::test]
async fn test_container_lifecycle_bad() {
    let env = CleanroomEnvironment::new(CleanroomConfig::default())
        .await
        .expect("This will panic if setup fails!");  // NEVER DO THIS

    let container_id = env.start_container("test")
        .await
        .unwrap();  // NEVER DO THIS

    assert!(env.is_container_running(&container_id).await.unwrap());
}
```

#### 2. Performance (< 3s for CLI Operations)
```rust
// ✅ GOOD: Performance-aware test
#[tokio::test]
async fn test_cli_performance() -> Result<()> {
    let start = Instant::now();

    let result = run_command(["ggen", "market", "search", "rust"]).await?;

    let duration = start.elapsed();
    assert!(
        duration < Duration::from_secs(3),
        "CLI operation took {:?}, expected < 3s",
        duration
    );

    result.assert_success()?;
    Ok(())
}
```

#### 3. Test Coverage (85%+ Critical Paths)
```toml
# Cargo.toml - Enable coverage tracking
[dev-dependencies]
tarpaulin = "0.27"

# Run coverage
# cargo tarpaulin --out Html --output-dir coverage/
```

#### 4. Isolation (No Shared State Between Tests)
```rust
// ✅ GOOD: Isolated test with unique resources
#[tokio::test]
async fn test_isolated_container() -> Result<()> {
    let test_id = Uuid::new_v4();
    let container_name = format!("test_container_{}", test_id);

    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;
    let container = env.create_container(&container_name).await?;

    // Test logic here

    // Cleanup automatic via Drop
    Ok(())
}

// ❌ BAD: Shared state between tests
static SHARED_ENV: OnceCell<CleanroomEnvironment> = OnceCell::new();

#[tokio::test]
async fn test_shared_bad() {
    let env = SHARED_ENV.get_or_init(|| /* ... */);
    // This breaks test isolation!
}
```

#### 5. Deterministic Execution (Reproducible Results)
```rust
// ✅ GOOD: Deterministic test with fixed seed
#[tokio::test]
async fn test_deterministic_execution() -> Result<()> {
    let mut config = CleanroomConfig::default();
    config.enable_deterministic_execution = true;
    config.deterministic_seed = Some(42); // Fixed seed

    let env = CleanroomEnvironment::new(config).await?;

    let result1 = env.execute_test("test1", || generate_random_data()).await?;
    let result2 = env.execute_test("test1", || generate_random_data()).await?;

    assert_eq!(result1, result2, "Results must be deterministic");
    Ok(())
}
```

---

## Rust Testing Patterns

### 1. Unit Tests vs Integration Tests

#### Unit Tests (in `src/` files)
```rust
// src/container.rs
pub struct Container {
    id: String,
    status: Status,
}

impl Container {
    pub fn new(id: String) -> Self {
        Self { id, status: Status::Starting }
    }

    pub fn is_ready(&self) -> bool {
        self.status == Status::Running
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_container_creation() {
        let container = Container::new("test123".to_string());
        assert_eq!(container.id, "test123");
        assert!(!container.is_ready());
    }

    #[test]
    fn test_container_ready_state() {
        let mut container = Container::new("test123".to_string());
        container.status = Status::Running;
        assert!(container.is_ready());
    }
}
```

#### Integration Tests (in `tests/` directory)
```rust
// tests/integration_tests.rs
use cleanroom::{CleanroomEnvironment, CleanroomConfig};
use anyhow::Result;

#[tokio::test]
async fn test_complete_workflow() -> Result<()> {
    // Test the entire system end-to-end
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Execute complete workflow
    let result = env.execute_test("workflow", || {
        // Real workflow logic
        Ok("success".to_string())
    }).await?;

    assert_eq!(result, "success");
    Ok(())
}
```

### 2. Test Organization

```
project/
├── src/
│   ├── lib.rs                 # Library root
│   ├── container.rs           # With inline unit tests
│   └── cleanroom.rs           # With inline unit tests
├── tests/
│   ├── integration_tests.rs   # General integration tests
│   ├── testcontainer_e2e.rs   # Container-specific E2E tests
│   ├── performance_tests.rs   # Performance benchmarks
│   └── common/
│       └── mod.rs             # Shared test utilities
└── benches/
    └── benchmarks.rs          # Criterion benchmarks
```

### 3. Test Fixtures and Helpers

```rust
// tests/common/mod.rs - Shared test utilities
use cleanroom::{CleanroomEnvironment, CleanroomConfig};
use anyhow::Result;
use std::sync::Arc;

/// Create test environment with default config
pub async fn test_environment() -> Result<Arc<CleanroomEnvironment>> {
    let config = CleanroomConfig {
        enable_singleton_containers: true,
        container_startup_timeout: Duration::from_secs(30),
        ..Default::default()
    };

    let env = CleanroomEnvironment::new(config).await?;
    Ok(Arc::new(env))
}

/// Create test environment with custom config
pub async fn test_environment_with_config(
    config: CleanroomConfig
) -> Result<Arc<CleanroomEnvironment>> {
    let env = CleanroomEnvironment::new(config).await?;
    Ok(Arc::new(env))
}

/// Skip test if Docker is not available
pub async fn skip_if_no_docker() -> bool {
    std::process::Command::new("docker")
        .arg("--version")
        .output()
        .is_err()
}
```

### 4. Property-Based Testing

```rust
// Use proptest for property-based testing
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_container_name_validation(name in "[a-zA-Z0-9_-]{1,100}") {
        // Test that any valid name is accepted
        let result = validate_container_name(&name);
        assert!(result.is_ok());
    }

    #[test]
    fn test_resource_limits(cpu in 0.1f64..100.0, memory in 1u64..100_000) {
        // Test that any valid resource limit is accepted
        let limits = ResourceLimits {
            max_cpu_percent: cpu,
            max_memory_mb: memory,
            ..Default::default()
        };
        assert!(limits.validate().is_ok());
    }
}
```

---

## Async Testing Best Practices

### 1. Using tokio::test

```rust
// ✅ GOOD: Proper async test setup
#[tokio::test]
async fn test_async_operation() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Await all async operations
    let result = env.execute_test("test", || async {
        tokio::time::sleep(Duration::from_millis(100)).await;
        Ok("success".to_string())
    }).await?;

    assert_eq!(result, "success");
    Ok(())
}

// ❌ BAD: Mixing sync and async incorrectly
#[tokio::test]
async fn test_async_bad() {
    let env = CleanroomEnvironment::new(CleanroomConfig::default())
        .await
        .unwrap();  // Don't unwrap!

    // This blocks the async runtime - BAD!
    std::thread::sleep(Duration::from_secs(1));
}
```

### 2. Testing Concurrent Operations

```rust
// ✅ GOOD: Testing concurrent operations safely
#[tokio::test]
async fn test_concurrent_operations() -> Result<()> {
    let env = Arc::new(
        CleanroomEnvironment::new(CleanroomConfig::default()).await?
    );

    // Spawn multiple concurrent tasks
    let handles: Vec<_> = (0..10)
        .map(|i| {
            let env = Arc::clone(&env);
            tokio::spawn(async move {
                env.execute_test(
                    &format!("test_{}", i),
                    || Ok(format!("result_{}", i))
                ).await
            })
        })
        .collect();

    // Wait for all to complete
    for handle in handles {
        let result = handle.await??;
        assert!(result.starts_with("result_"));
    }

    Ok(())
}
```

### 3. Timeout Handling

```rust
use tokio::time::{timeout, Duration};

// ✅ GOOD: Test with timeout protection
#[tokio::test]
async fn test_with_timeout() -> Result<()> {
    let result = timeout(
        Duration::from_secs(5),
        long_running_operation()
    ).await;

    match result {
        Ok(inner_result) => {
            inner_result.context("Operation failed")?;
        }
        Err(_) => {
            anyhow::bail!("Operation timed out after 5s");
        }
    }

    Ok(())
}
```

### 4. Async Cleanup with Drop

```rust
// ✅ GOOD: RAII pattern for async cleanup
pub struct CleanroomGuard {
    env: Arc<CleanroomEnvironment>,
}

impl CleanroomGuard {
    pub fn new(env: Arc<CleanroomEnvironment>) -> Self {
        Self { env }
    }
}

impl Drop for CleanroomGuard {
    fn drop(&mut self) {
        // Schedule async cleanup
        let env = Arc::clone(&self.env);
        tokio::spawn(async move {
            if let Err(e) = env.cleanup().await {
                eprintln!("Cleanup error: {}", e);
            }
        });
    }
}

#[tokio::test]
async fn test_with_guard() -> Result<()> {
    let env = Arc::new(
        CleanroomEnvironment::new(CleanroomConfig::default()).await?
    );
    let _guard = CleanroomGuard::new(Arc::clone(&env));

    // Test logic here

    // Cleanup happens automatically when _guard drops
    Ok(())
}
```

---

## Testcontainers Best Practices

### 1. Container Lifecycle Management

```rust
use testcontainers::{clients, GenericImage, RunnableImage};

// ✅ GOOD: Proper container lifecycle with RAII
#[tokio::test]
async fn test_postgres_container() -> Result<()> {
    let docker = clients::Cli::default();

    // Container starts here
    let postgres = docker
        .run(RunnableImage::from(
            GenericImage::new("postgres", "15")
                .with_env_var("POSTGRES_PASSWORD", "test")
                .with_wait_for(testcontainers::core::WaitFor::message_on_stderr(
                    "database system is ready to accept connections"
                ))
        ));

    // Get connection details
    let port = postgres.get_host_port_ipv4(5432);
    let host = "127.0.0.1";

    // Use container
    test_database_operations(host, port).await?;

    // Container automatically stops when postgres drops
    Ok(())
}
```

### 2. Singleton Container Pattern

```rust
use once_cell::sync::OnceCell;
use std::sync::Arc;
use tokio::sync::Mutex;

// ✅ GOOD: Singleton pattern for expensive containers
static POSTGRES_CONTAINER: OnceCell<Arc<Mutex<PostgresContainer>>> = OnceCell::new();

async fn get_postgres_container() -> Arc<Mutex<PostgresContainer>> {
    POSTGRES_CONTAINER
        .get_or_init(|| {
            Arc::new(Mutex::new(
                PostgresContainer::new("testdb", "testuser", "testpass")
            ))
        })
        .clone()
}

#[tokio::test]
async fn test_with_singleton() -> Result<()> {
    let container = get_postgres_container().await;
    let guard = container.lock().await;

    // Use shared container
    test_database_operations(&guard).await?;

    Ok(())
}
```

### 3. Dynamic Port Mapping

```rust
// ✅ GOOD: Always use dynamic port mapping
#[tokio::test]
async fn test_dynamic_ports() -> Result<()> {
    let docker = clients::Cli::default();

    let redis = docker.run(
        GenericImage::new("redis", "7")
            .with_exposed_port(6379)  // Container port
    );

    // Get dynamically assigned host port
    let host_port = redis.get_host_port_ipv4(6379);
    let connection_string = format!("redis://127.0.0.1:{}", host_port);

    // Use dynamic connection string
    test_redis_operations(&connection_string).await?;

    Ok(())
}

// ❌ BAD: Hardcoded ports cause parallel test failures
#[tokio::test]
async fn test_hardcoded_ports_bad() -> Result<()> {
    // This will fail when running tests in parallel!
    let connection_string = "redis://127.0.0.1:6379";
    test_redis_operations(&connection_string).await?;
    Ok(())
}
```

### 4. Container Customization

```rust
// ✅ GOOD: Customizing containers for specific tests
#[tokio::test]
async fn test_custom_postgres() -> Result<()> {
    let docker = clients::Cli::default();

    let postgres = docker.run(
        RunnableImage::from(
            GenericImage::new("postgres", "15")
                .with_env_var("POSTGRES_DB", "testdb")
                .with_env_var("POSTGRES_USER", "testuser")
                .with_env_var("POSTGRES_PASSWORD", "testpass")
                .with_env_var("POSTGRES_INITDB_ARGS", "-A scram-sha-256")
        )
        .with_network("cleanroom-test-network")
        .with_volume("/tmp/test-data", "/var/lib/postgresql/data")
    );

    // Test with custom configuration
    test_database_operations(&postgres).await?;

    Ok(())
}
```

### 5. Health Checks and Readiness

```rust
// ✅ GOOD: Wait for container to be ready
#[tokio::test]
async fn test_with_health_check() -> Result<()> {
    let docker = clients::Cli::default();

    let container = docker.run(
        GenericImage::new("myapp", "latest")
            .with_wait_for(testcontainers::core::WaitFor::http(
                testcontainers::core::HttpWaitStrategy::new("/health")
                    .with_expected_status_code(200)
                    .with_timeout(Duration::from_secs(30))
            ))
    );

    // Container is guaranteed to be ready
    test_application(&container).await?;

    Ok(())
}
```

### 6. Container Networks

```rust
// ✅ GOOD: Testing multi-container scenarios
#[tokio::test]
async fn test_multi_container_network() -> Result<()> {
    let docker = clients::Cli::default();
    let network = docker.create_network("test-network")?;

    // Start PostgreSQL
    let postgres = docker.run(
        GenericImage::new("postgres", "15")
            .with_network(&network)
            .with_env_var("POSTGRES_PASSWORD", "test")
    );

    // Start Redis
    let redis = docker.run(
        GenericImage::new("redis", "7")
            .with_network(&network)
    );

    // Start application that connects to both
    let app = docker.run(
        GenericImage::new("myapp", "latest")
            .with_network(&network)
            .with_env_var("DATABASE_URL", "postgres://postgres@postgres:5432/test")
            .with_env_var("REDIS_URL", "redis://redis:6379")
    );

    // Test integrated system
    test_integrated_application(&app).await?;

    // All containers and network cleaned up automatically
    Ok(())
}
```

---

## Error Handling in Tests

### 1. Never Use .expect() or .unwrap() in Production

```rust
// ❌ BAD: Using expect/unwrap (will crash on errors)
pub fn bad_container_creation() -> Container {
    let config = load_config().expect("Config must exist!");  // CRASHES
    let docker = connect_docker().unwrap();  // CRASHES
    docker.create_container(&config).expect("Must work!")  // CRASHES
}

// ✅ GOOD: Proper error handling with context
pub fn good_container_creation() -> Result<Container> {
    let config = load_config()
        .context("Failed to load configuration")?;

    let docker = connect_docker()
        .context("Failed to connect to Docker daemon")?;

    let container = docker.create_container(&config)
        .context("Failed to create container")?;

    Ok(container)
}
```

### 2. Error Context with anyhow

```rust
use anyhow::{Context, Result};

// ✅ GOOD: Rich error context for debugging
#[tokio::test]
async fn test_with_context() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default())
        .await
        .context("Failed to create cleanroom environment")?;

    let container = env.start_container("postgres")
        .await
        .context("Failed to start PostgreSQL container")?;

    container.wait_for_ready()
        .await
        .context("PostgreSQL container failed to become ready")?;

    let result = container.execute_sql("SELECT 1")
        .await
        .context("Failed to execute test query")?;

    assert_eq!(result, "1");
    Ok(())
}
```

### 3. Custom Error Types with thiserror

```rust
use thiserror::Error;

// ✅ GOOD: Custom error types for domain-specific errors
#[derive(Error, Debug)]
pub enum CleanroomError {
    #[error("Container {0} not found")]
    ContainerNotFound(String),

    #[error("Container {0} failed to start: {1}")]
    ContainerStartFailed(String, String),

    #[error("Resource limit exceeded: {0}")]
    ResourceLimitExceeded(String),

    #[error("Docker connection failed")]
    DockerConnectionFailed(#[from] DockerError),

    #[error("Configuration error: {0}")]
    ConfigError(String),
}

pub type Result<T> = std::result::Result<T, CleanroomError>;

// Usage in tests
#[tokio::test]
async fn test_custom_errors() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    match env.start_container("nonexistent").await {
        Err(CleanroomError::ContainerNotFound(name)) => {
            assert_eq!(name, "nonexistent");
        }
        Ok(_) => panic!("Should have failed with ContainerNotFound"),
        Err(e) => panic!("Wrong error type: {}", e),
    }

    Ok(())
}
```

### 4. Test Error Recovery

```rust
// ✅ GOOD: Testing error recovery paths
#[tokio::test]
async fn test_error_recovery() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Simulate failure condition
    env.inject_failure("disk_full").await?;

    // Attempt operation that should fail
    let result = env.start_container("test").await;
    assert!(result.is_err());

    // Clear failure condition
    env.clear_failure("disk_full").await?;

    // Retry should succeed
    let container = env.start_container("test").await?;
    assert!(container.is_running());

    Ok(())
}
```

---

## Performance Testing

### 1. Benchmark with Criterion

```rust
// benches/benchmarks.rs
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use cleanroom::{CleanroomEnvironment, CleanroomConfig};

fn benchmark_container_startup(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("container_startup", |b| {
        b.to_async(&runtime).iter(|| async {
            let env = CleanroomEnvironment::new(
                black_box(CleanroomConfig::default())
            ).await.unwrap();

            let container = env.start_container(
                black_box("bench_container")
            ).await.unwrap();

            black_box(container)
        });
    });
}

fn benchmark_test_execution(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();

    c.bench_function("test_execution", |b| {
        b.to_async(&runtime).iter(|| async {
            let env = CleanroomEnvironment::new(CleanroomConfig::default())
                .await
                .unwrap();

            let result = env.execute_test("bench", || {
                Ok(black_box("result".to_string()))
            }).await.unwrap();

            black_box(result)
        });
    });
}

criterion_group!(benches, benchmark_container_startup, benchmark_test_execution);
criterion_main!(benches);
```

### 2. Performance SLO Tests

```rust
// ✅ GOOD: Testing performance SLOs
#[tokio::test]
async fn test_cli_performance_slo() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Test: CLI operations must complete in < 3s
    let start = Instant::now();
    let result = env.execute_command(["ggen", "market", "search", "rust"]).await?;
    let duration = start.elapsed();

    assert!(
        duration < Duration::from_secs(3),
        "CLI operation took {:?}, exceeded 3s SLO",
        duration
    );

    // Test: Memory usage must be < 100MB
    let memory = env.get_memory_usage().await?;
    assert!(
        memory < 100_000_000,
        "Memory usage {} bytes, exceeded 100MB SLO",
        memory
    );

    Ok(())
}
```

### 3. Load Testing

```rust
// ✅ GOOD: Testing under load
#[tokio::test]
async fn test_load_performance() -> Result<()> {
    let env = Arc::new(
        CleanroomEnvironment::new(CleanroomConfig::default()).await?
    );

    let start = Instant::now();

    // Execute 100 concurrent tests
    let handles: Vec<_> = (0..100)
        .map(|i| {
            let env = Arc::clone(&env);
            tokio::spawn(async move {
                env.execute_test(
                    &format!("load_test_{}", i),
                    || Ok(format!("result_{}", i))
                ).await
            })
        })
        .collect();

    // Wait for all to complete
    for handle in handles {
        handle.await??;
    }

    let duration = start.elapsed();
    let throughput = 100.0 / duration.as_secs_f64();

    // Assert throughput SLO
    assert!(
        throughput >= 10.0,
        "Throughput {:.2} tests/sec, expected >= 10/sec",
        throughput
    );

    Ok(())
}
```

---

## CI/CD Integration

### 1. GitHub Actions Configuration

```yaml
# .github/workflows/tests.yml
name: Tests

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  unit-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          components: rustfmt, clippy
          override: true

      - name: Cache cargo registry
        uses: actions/cache@v3
        with:
          path: ~/.cargo/registry
          key: ${{ runner.os }}-cargo-registry-${{ hashFiles('**/Cargo.lock') }}

      - name: Run unit tests
        run: cargo test --lib --all-features

  integration-tests:
    runs-on: ubuntu-latest
    services:
      docker:
        image: docker:dind
        options: --privileged

    steps:
      - uses: actions/checkout@v3

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          override: true

      - name: Run integration tests
        run: cargo test --test integration_tests

      - name: Run E2E tests
        run: cargo test --test testcontainer_e2e_test

  coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          override: true

      - name: Install tarpaulin
        run: cargo install cargo-tarpaulin

      - name: Generate coverage
        run: cargo tarpaulin --out Xml --all-features

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          files: ./cobertura.xml
          fail_ci_if_error: true
```

### 2. Docker-Less CI Testing

```rust
// ✅ GOOD: CI-friendly tests that work without Docker
#[tokio::test]
async fn test_ci_friendly() -> Result<()> {
    // Use mock backend in CI
    let backend = if std::env::var("CI").is_ok() {
        Backend::Mock
    } else {
        Backend::Docker
    };

    let config = CleanroomConfig {
        backend,
        ..Default::default()
    };

    let env = CleanroomEnvironment::new(config).await?;

    // Test logic works with both mock and real containers
    let result = env.execute_test("ci_test", || {
        Ok("success".to_string())
    }).await?;

    assert_eq!(result, "success");
    Ok(())
}
```

---

## Common Pitfalls to Avoid

### 1. Runtime Conflicts (Sync in Async)

```rust
// ❌ BAD: Blocking in async context
#[tokio::test]
async fn test_blocking_bad() {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await.unwrap();

    // This blocks the async runtime - BAD!
    let result = run_sync_command(["echo", "hello"]);  // Sync call in async

    assert!(result.is_ok());
}

// ✅ GOOD: Use spawn_blocking for sync operations
#[tokio::test]
async fn test_blocking_good() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Properly handle blocking operations
    let result = tokio::task::spawn_blocking(|| {
        run_sync_command(["echo", "hello"])
    }).await??;

    assert!(result.is_ok());
    Ok(())
}
```

### 2. Forgetting to Await

```rust
// ❌ BAD: Not awaiting futures
#[tokio::test]
async fn test_not_awaited_bad() {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await.unwrap();

    // This creates a future but doesn't execute it!
    env.start_container("test");  // Missing .await

    // Container was never started
}

// ✅ GOOD: Always await async operations
#[tokio::test]
async fn test_awaited_good() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Properly await async operations
    let container = env.start_container("test").await?;

    assert!(container.is_running());
    Ok(())
}
```

### 3. Shared Mutable State

```rust
// ❌ BAD: Shared mutable state without synchronization
static mut COUNTER: i32 = 0;

#[tokio::test]
async fn test_shared_state_bad_1() {
    unsafe { COUNTER += 1; }  // Race condition!
}

#[tokio::test]
async fn test_shared_state_bad_2() {
    unsafe { COUNTER += 1; }  // Race condition!
}

// ✅ GOOD: Use proper synchronization primitives
use std::sync::atomic::{AtomicI32, Ordering};

static COUNTER: AtomicI32 = AtomicI32::new(0);

#[tokio::test]
async fn test_shared_state_good_1() {
    COUNTER.fetch_add(1, Ordering::SeqCst);  // Safe!
}

#[tokio::test]
async fn test_shared_state_good_2() {
    COUNTER.fetch_add(1, Ordering::SeqCst);  // Safe!
}
```

### 4. Memory Leaks in Tests

```rust
// ❌ BAD: Not cleaning up resources
#[tokio::test]
async fn test_memory_leak_bad() {
    let env = Box::leak(Box::new(
        CleanroomEnvironment::new(CleanroomConfig::default()).await.unwrap()
    ));

    // Memory leaked - never cleaned up!
}

// ✅ GOOD: Proper resource cleanup with RAII
#[tokio::test]
async fn test_no_leak_good() -> Result<()> {
    let env = CleanroomEnvironment::new(CleanroomConfig::default()).await?;

    // Test logic

    // env dropped automatically, cleanup happens
    Ok(())
}
```

### 5. Port Conflicts

```rust
// ❌ BAD: Hardcoded ports cause parallel test failures
#[tokio::test]
async fn test_port_conflict_bad_1() -> Result<()> {
    let server = start_server("127.0.0.1:8080").await?;  // Hardcoded port
    test_server(&server).await?;
    Ok(())
}

#[tokio::test]
async fn test_port_conflict_bad_2() -> Result<()> {
    let server = start_server("127.0.0.1:8080").await?;  // Same port - FAILS!
    test_server(&server).await?;
    Ok(())
}

// ✅ GOOD: Dynamic port allocation
#[tokio::test]
async fn test_port_dynamic_good_1() -> Result<()> {
    let server = start_server("127.0.0.1:0").await?;  // OS assigns port
    let port = server.local_addr()?.port();
    test_server(&server).await?;
    Ok(())
}

#[tokio::test]
async fn test_port_dynamic_good_2() -> Result<()> {
    let server = start_server("127.0.0.1:0").await?;  // Different port - OK!
    let port = server.local_addr()?.port();
    test_server(&server).await?;
    Ok(())
}
```

---

## Production Readiness Checklist

### Critical Requirements (Must Have for v1)

- [ ] **Test Coverage**: 85%+ on critical paths
- [ ] **Test Pass Rate**: 99%+ reliability
- [ ] **Performance**: CLI operations < 3s
- [ ] **Error Handling**: No `.expect()`/`.unwrap()` in production
- [ ] **Test Isolation**: No shared state between tests
- [ ] **Deterministic**: Tests produce consistent results
- [ ] **CI/CD Ready**: Tests run in automated environments
- [ ] **Documentation**: All public APIs documented
- [ ] **Examples**: Working examples for common use cases

### Integration Testing

- [ ] **Container Lifecycle**: Start, stop, cleanup
- [ ] **Multi-Container**: Test container interactions
- [ ] **Network Isolation**: Verify network boundaries
- [ ] **Resource Limits**: Validate limit enforcement
- [ ] **Error Recovery**: Test failure scenarios
- [ ] **Concurrent Execution**: Parallel test execution
- [ ] **Performance Benchmarks**: Baseline measurements

### Production Failure Scenarios

- [ ] **OOM Kill**: Container out-of-memory
- [ ] **Disk Full**: Disk space exhaustion
- [ ] **Network Partition**: Network failures
- [ ] **Container Crash**: Unexpected container termination
- [ ] **Docker Daemon Down**: Docker unavailable
- [ ] **Timeout Handling**: Long-running operations

### Performance Testing

- [ ] **Startup Time**: Container startup < 10s
- [ ] **Throughput**: Tests/second baseline
- [ ] **Memory Usage**: < 100MB for CLI operations
- [ ] **CPU Usage**: < 80% under load
- [ ] **Load Testing**: 100+ concurrent tests

### CI/CD Integration

- [ ] **GitHub Actions**: Automated test runs
- [ ] **Coverage Reports**: Codecov integration
- [ ] **Performance Regression**: Benchmark tracking
- [ ] **Mock Backend**: Tests work without Docker
- [ ] **Parallel Execution**: Tests run in parallel safely

---

## Conclusion

Building a production-ready testing framework requires:

1. **Reliability**: Graceful error handling, no panics
2. **Performance**: Fast feedback, < 3s operations
3. **Isolation**: Independent tests, no shared state
4. **Determinism**: Reproducible results
5. **Coverage**: 85%+ on critical paths
6. **CI/CD**: Automated, reliable test execution

Follow these best practices to build robust, maintainable, and production-ready testing infrastructure.

---

## References

- [Rust Async Book](https://rust-lang.github.io/async-book/)
- [Testcontainers Rust](https://github.com/testcontainers/testcontainers-rs)
- [Tokio Testing Guide](https://tokio.rs/tokio/topics/testing)
- [Criterion Benchmarking](https://github.com/bheisler/criterion.rs)
- [Proptest Property Testing](https://github.com/proptest-rs/proptest)

---

**Document Version**: 1.0
**Last Updated**: 2025-10-13
**Maintained By**: Researcher Agent (Hive Mind)
**Next Review**: 2025-10-20
