# Docker Validation Best Practices for Testcontainers

**Author**: Research Agent (Hive Mind Validation Swarm)
**Date**: 2025-10-13
**Status**: Comprehensive Research Report
**Mission**: Prevent false positives in Docker-based testing

---

## Executive Summary

This document provides industry best practices for validating Docker integration in test frameworks, with specific focus on detecting and preventing false positives in containerized testing. Based on research from Docker, Testcontainers community, and real-world Rust implementations, this guide ensures your tests actually validate what they claim to test.

**Key Findings**:
- **6 critical mock implementations** identified in cleanroom codebase
- **4+ false positive tests** that pass without Docker
- **Industry consensus**: Real containers > Mocks for integration testing
- **Ryuk container**: Essential for cleanup validation

---

## Table of Contents

1. [Industry Best Practices](#1-industry-best-practices)
2. [False Positive Patterns](#2-false-positive-patterns)
3. [Detection Strategies](#3-detection-strategies)
4. [Prevention Techniques](#4-prevention-techniques)
5. [Testcontainers Validation](#5-testcontainers-validation)
6. [Rust-Specific Patterns](#6-rust-specific-patterns)
7. [Validation Checklist](#7-validation-checklist)
8. [Real-World Examples](#8-real-world-examples)

---

## 1. Industry Best Practices

### 1.1 Testcontainers Philosophy

**Core Principle**: Use real dependencies instead of mocks for integration testing.

#### Why Real Containers Over Mocks?

**Problems with Mocks** (Source: Docker, Testcontainers Community):

1. **Maintenance Burden**
   - You must code validation logic in mocks
   - Replicate what service/database should do
   - Update mocks whenever validation logic changes
   - If spread across codebase, maintenance becomes very hard

2. **False Confidence**
   - Mocks can lead to false positives if not used carefully
   - They simulate behavior rather than executing real code
   - Tests pass but production fails

3. **Outdated State**
   - Mocks can assert against outdated validation states
   - Real service behavior changes but mock doesn't
   - Creates divergence between tests and reality

**Advantages of Real Containers** (Source: Docker Blog):

1. **Reliability**
   - Tests interact with real instances of external resources
   - More reliable and closer to real-world scenarios
   - Confidence that comes from testing against real dependencies

2. **Shift-Left Testing**
   - Detect issues earlier in developer's inner loop
   - No need to wait for staging environment
   - Lightweight experience with reliability of integration tests

3. **Simplicity**
   - No more need for mocks or complicated environment configurations
   - Define test dependencies as code
   - Simply run tests and containers are created/deleted automatically

### 1.2 Best Practices from Docker (2025)

#### 1. Dynamic Port Mapping

**Best Approach**: Use Testcontainers built-in dynamic port mapping.

```rust
// ❌ BAD - Hardcoded ports cause collisions
let port = 5432;
let connection_string = format!("postgresql://localhost:{}", port);

// ✅ GOOD - Dynamic port mapping
let container = postgres_container.start()?;
let port = container.get_host_port_ipv4(5432)?;
let connection_string = format!("postgresql://localhost:{}", port);
```

**Why**: Hardcoded ports clash with existing resources, causing test failures.

#### 2. Pin Image Versions

**Best Practice**: Don't use `latest` tag.

```rust
// ❌ BAD - Flakiness when new version released
let image = GenericImage::new("postgres", "latest");

// ✅ GOOD - Same version as production
let image = GenericImage::new("postgres", "15.2");
```

**Why**: `latest` introduces flakiness when new versions are released. Use same version as production to ensure you can trust test outcomes.

#### 3. Avoid Static Names

**Best Practice**: Use randomly assigned names.

```rust
// ❌ BAD - Static names clash with existing resources
let container = ContainerRequest::from(image)
    .with_name("my-postgres");

// ✅ GOOD - Random assigned names
let container = ContainerRequest::from(image);
// Testcontainers assigns unique names automatically
```

**Why**: Static names may clash with existing resources (containers, networks, volumes), causing tests to fail.

#### 4. Don't Hardcode Connection Strings

**Biggest Mistake**: Hardcoding connection strings.

```rust
// ❌ BAD - Testcontainers assigns dynamic ports
let connection = "postgresql://localhost:5432/testdb";

// ✅ GOOD - Use dynamic ports from container
let host = container.get_host()?;
let port = container.get_host_port_ipv4(5432)?;
let connection = format!("postgresql://{}:{}/testdb", host, port);
```

**Why**: Testcontainers assigns dynamic ports to avoid conflicts.

### 1.3 Container Health Checks

**Critical for Integration Testing**: Ensure services are actually ready.

#### Basic Health Check Implementation

```dockerfile
# HTTP Service Example
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD curl -f http://localhost/health || exit 1

# Database Example
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD pg_isready -U postgres || exit 1
```

#### Health Check Configuration (Source: Docker Community)

| Option | Default | Purpose |
|--------|---------|---------|
| `--interval` | 30s | Time between health checks |
| `--timeout` | 30s | Health check timeout |
| `--start-period` | 0s | Initialization time before checking |
| `--retries` | 3 | Consecutive failures to mark unhealthy |

#### Validation Strategies by Service Type

**HTTP Services**:
```rust
// ✅ GOOD - Check readiness endpoint
async fn wait_for_http_ready(container: &Container) -> Result<()> {
    let port = container.get_host_port_ipv4(8080)?;
    let url = format!("http://localhost:{}/health", port);

    for _ in 0..30 {
        if let Ok(response) = reqwest::get(&url).await {
            if response.status().is_success() {
                return Ok(());
            }
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    Err(anyhow!("Service not ready"))
}
```

**Database Services**:
```rust
// ✅ GOOD - Verify database accepts connections AND is ready to serve
async fn wait_for_postgres_ready(container: &PostgresContainer) -> Result<()> {
    let connection_string = container.connection_string();

    for _ in 0..30 {
        if let Ok(conn) = sqlx::PgConnection::connect(&connection_string).await {
            // Not just TCP connection - verify it can execute queries
            if let Ok(_) = sqlx::query("SELECT 1").execute(&mut conn).await {
                return Ok(());
            }
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    Err(anyhow!("Database not ready"))
}
```

**Why Strengthen Health Checks**: A database might respond to a ping but still not be ready to accept connections. Basic TCP connection isn't always enough.

#### Common Pitfall: Missing Dependencies

```dockerfile
# ❌ BAD - curl not installed but used in health check
HEALTHCHECK CMD curl -f http://localhost/health || exit 1

# ✅ GOOD - Ensure tool exists
RUN apt-get update && apt-get install -y curl
HEALTHCHECK CMD curl -f http://localhost/health || exit 1
```

**Why**: Classic example - health check uses `curl` but `curl` is not installed, causing health check to fail with no clear indication that `curl` is the issue.

---

## 2. False Positive Patterns

### 2.1 Mock Implementation Red Flags

#### Pattern 1: Formatted String Returns

**Symptom**: Method returns formatted string with input echoed back.

```rust
// 🔴 RED FLAG - Mock implementation
pub async fn execute_sql(&self, sql: &str) -> Result<String> {
    // Returns input as output - NOT executing SQL
    Ok(format!("Mock result for SQL: {}", sql))
}

// 🔴 RED FLAG - Mock implementation
pub async fn execute_command(&self, command: &str) -> Result<String> {
    // Returns command text - NOT executing command
    Ok(format!("Mock result for Redis command: {}", command))
}
```

**Detection**: Search for patterns like `format!("Mock result for")` or `format!("{}", input)`.

**Why False Positive**: Tests pass because they check result is Ok, but no actual operation occurred.

#### Pattern 2: Always-Ok Connections

**Symptom**: Connection test always returns Ok without checking.

```rust
// 🔴 RED FLAG - Always succeeds
pub async fn test_connection(&self) -> Result<()> {
    // No actual connection attempt
    Ok(())
}
```

**Detection**: Search for methods named `test_connection`, `ping`, `health_check` that immediately return `Ok(())`.

**Why False Positive**: Test assumes connection works, but never validates connectivity.

#### Pattern 3: TODO Comments

**Symptom**: Implementation contains TODO comments indicating incomplete implementation.

```rust
// 🔴 RED FLAG - Placeholder implementation
pub async fn execute_sql(&self, sql: &str) -> Result<String> {
    // TODO: Implement proper SQL execution with testcontainers API
    Ok(format!("Mock result for SQL: {}", sql))
}
```

**Detection**: Grep for `TODO.*implement.*real|proper|actual` in container implementations.

**Why False Positive**: Developer explicitly marked as incomplete but tests may still pass.

#### Pattern 4: Hardcoded Status/Metrics

**Symptom**: Container status or metrics are hardcoded rather than queried from Docker.

```rust
// 🔴 RED FLAG - Hardcoded status
pub fn status(&self) -> ContainerStatus {
    ContainerStatus::Running  // Always returns Running!
}

// 🔴 RED FLAG - Simulated metrics
pub fn metrics(&self) -> ContainerMetrics {
    ContainerMetrics {
        cpu_usage_percent: 0.0,  // Fake values
        memory_usage_bytes: 1024, // Fake values
        uptime_seconds: 0,        // Fake values
    }
}
```

**Detection**: Check if status/metrics methods query actual Docker daemon or return constants.

**Why False Positive**: Tests checking status pass, but not validating real container state.

### 2.2 Test Naming Anti-Patterns

#### Pattern 1: Tests Named "Mock"

```rust
// 🔴 RED FLAG - Test explicitly says it's mocked
#[tokio::test]
async fn test_container_creation_mock() {
    let container = PostgresContainer::new_async(...).await;
    assert!(container.is_ok());  // Only tests struct creation!
}
```

**Why False Positive**: Test name admits it's not integration testing, but counts toward "test coverage".

#### Pattern 2: Tests That Don't Test

```rust
// 🔴 RED FLAG - Creates container but doesn't use it
#[tokio::test]
async fn test_postgres_integration() {
    let container = PostgresContainer::new_async(...).await?;
    // Container created but no SQL executed!
    assert!(container.is_ok());
}
```

**Why False Positive**: Test claims to test integration but only validates struct creation.

#### Pattern 3: Tests That Skip Real Validation

```rust
// 🔴 RED FLAG - Skips when Docker is needed
#[tokio::test]
async fn test_docker_integration() {
    if !is_docker_available().await {
        println!("Docker not available, skipping test");
        return;  // Test passes even though it didn't run!
    }
    // Real test code here
}
```

**Why False Positive**: CI shows test "passed" but it was skipped. Should use `#[ignore]` or mark as conditional.

### 2.3 Cleanup Validation False Positives

#### Pattern 1: No Cleanup Verification

```rust
// 🔴 RED FLAG - No verification cleanup happened
#[tokio::test]
async fn test_resource_cleanup() {
    let container = PostgresContainer::new_async(...).await?;
    // Container goes out of scope...
    // But did it actually get cleaned up?
}
```

**Missing**: Should verify container no longer exists in Docker.

```rust
// ✅ GOOD - Verify cleanup
#[tokio::test]
async fn test_resource_cleanup() {
    let container_id = {
        let container = PostgresContainer::new_async(...).await?;
        container.id().to_string()
    }; // Container dropped here

    // Verify container is gone
    tokio::time::sleep(Duration::from_secs(1)).await;
    let output = Command::new("docker")
        .args(&["ps", "-a", "--filter", &format!("id={}", container_id)])
        .output()?;

    assert!(output.stdout.is_empty(), "Container not cleaned up!");
}
```

#### Pattern 2: No Ryuk Verification

**Critical**: Testcontainers uses Ryuk sidecar for cleanup. Verify it's running!

```rust
// ✅ GOOD - Verify Ryuk is managing cleanup
#[tokio::test]
async fn test_ryuk_cleanup_manager() {
    let container = PostgresContainer::new_async(...).await?;

    // Verify Ryuk container exists
    let output = Command::new("docker")
        .args(&["ps", "--filter", "name=ryuk", "--format", "{{.Names}}"])
        .output()?;

    let ryuk_containers: Vec<_> = String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter(|line| line.contains("ryuk"))
        .collect();

    assert!(!ryuk_containers.is_empty(), "Ryuk container not running!");
}
```

**Why Important**: Ryuk (Resource Reaper) is responsible for cleanup. If not running, containers may leak.

---

## 3. Detection Strategies

### 3.1 Static Analysis Patterns

#### Grep Patterns for Mock Detection

```bash
# Find mock implementations
grep -r "format.*Mock result" src/
grep -r "TODO.*implement.*proper" src/
grep -r "Simplified.*return mock" src/

# Find always-Ok methods
grep -A 3 "test_connection\|health_check" src/ | grep -B 3 "Ok(())"

# Find hardcoded status
grep -r "ContainerStatus::Running" src/ | grep -v "match\|if\|=="

# Find formatted string returns
grep -r 'Ok(format!(".*{}".*))' src/
```

#### Rust-Specific Patterns

```bash
# Find methods that don't use self
rg "pub.*async fn.*\(&self.*\).*Result" -A 5 | grep "Ok(format!"

# Find TODOs in container implementations
rg "TODO" src/containers.rs src/backend/

# Find tests named "mock"
rg "#\[tokio::test\]" tests/ -A 1 | grep "mock"
```

### 3.2 Runtime Validation

#### Strategy 1: Docker Daemon Check

```rust
/// Verify tests actually require Docker
async fn require_docker() -> Result<()> {
    let output = Command::new("docker")
        .arg("info")
        .output()?;

    if !output.status.success() {
        return Err(anyhow!("Docker daemon not running"));
    }
    Ok(())
}

#[tokio::test]
async fn test_real_docker_integration() {
    require_docker().await.expect("Docker required for this test");
    // Real integration test code
}
```

#### Strategy 2: Container Process Verification

```rust
/// Verify container is actually running
async fn verify_container_running(container_id: &str) -> Result<()> {
    let output = Command::new("docker")
        .args(&["inspect", "--format", "{{.State.Running}}", container_id])
        .output()?;

    let running = String::from_utf8_lossy(&output.stdout).trim() == "true";
    if !running {
        return Err(anyhow!("Container {} not running", container_id));
    }
    Ok(())
}
```

#### Strategy 3: Actual Operation Validation

```rust
// ✅ GOOD - Verify SQL actually executes
#[tokio::test]
async fn test_postgres_real_execution() {
    let container = PostgresContainer::new_async(...).await?;

    // Don't trust execute_sql() - verify with real connection
    let conn = sqlx::PgConnection::connect(&container.connection_string()).await?;

    // Execute real SQL
    let row: (i32,) = sqlx::query_as("SELECT 1")
        .fetch_one(&mut conn)
        .await?;

    assert_eq!(row.0, 1, "Real SQL execution failed");
}
```

### 3.3 Ryuk Container Validation

#### Understanding Ryuk

**Purpose**: Testcontainers uses Ryuk (Resource Reaper) to clean up containers, networks, and volumes after tests complete.

**How It Works** (Source: Testcontainers):
1. Testcontainers assigns special labels to created resources
2. Ryuk container monitors for JVM/process exit
3. When process exits, Ryuk removes all resources with matching labels
4. Works even when test process exits abnormally (SIGKILL)

**Configuration**:
```bash
# Disable Ryuk (not recommended)
export TESTCONTAINERS_RYUK_DISABLED=true

# Verify Ryuk is privileged (required)
docker inspect testcontainers-ryuk | grep Privileged
```

#### Validation Strategy

```rust
/// Verify Ryuk is managing cleanup
async fn verify_ryuk_active() -> Result<()> {
    let output = Command::new("docker")
        .args(&["ps", "--filter", "ancestor=testcontainers/ryuk:*"])
        .output()?;

    let ryuk_running = !output.stdout.is_empty();
    if !ryuk_running {
        return Err(anyhow!("Ryuk container not running - cleanup may fail"));
    }
    Ok(())
}

/// Verify container has cleanup label
async fn verify_cleanup_label(container_id: &str) -> Result<()> {
    let output = Command::new("docker")
        .args(&["inspect", "--format", "{{.Config.Labels}}", container_id])
        .output()?;

    let labels = String::from_utf8_lossy(&output.stdout);
    if !labels.contains("org.testcontainers") {
        return Err(anyhow!("Container missing cleanup label"));
    }
    Ok(())
}
```

---

## 4. Prevention Techniques

### 4.1 Test Organization

#### Separate Unit from Integration Tests

```
tests/
├── unit/                    # No Docker required
│   ├── environment.rs      # Test CleanroomEnvironment struct
│   ├── config.rs           # Test configuration parsing
│   └── metrics.rs          # Test metrics calculation
│
├── integration/            # Docker required
│   ├── postgres.rs         # Real PostgreSQL operations
│   ├── redis.rs            # Real Redis operations
│   └── docker_backend.rs   # Real Docker exec
│
└── e2e/                    # Full end-to-end
    └── full_stack.rs       # Complete workflow tests
```

#### Use Cargo Features

```toml
[features]
default = []
docker-integration = []

[[test]]
name = "integration_tests"
path = "tests/integration/mod.rs"
required-features = ["docker-integration"]
```

```rust
// Only run when feature enabled
#[cfg(feature = "docker-integration")]
mod integration_tests {
    // Real Docker tests here
}
```

### 4.2 Test Attributes

#### Use #[ignore] for Slow Tests

```rust
// Require explicit opt-in for slow tests
#[tokio::test]
#[ignore]
async fn test_full_postgres_migration() {
    // This test pulls large images and runs migrations
}
```

Run with: `cargo test -- --ignored`

#### Use #[should_panic] Appropriately

```rust
// Document expected failures
#[tokio::test]
#[should_panic(expected = "Docker daemon not running")]
async fn test_without_docker_panics() {
    let _container = PostgresContainer::new_async(...).await.unwrap();
}
```

### 4.3 Clear Naming Conventions

```rust
// ✅ GOOD - Clear about what's tested
#[tokio::test]
async fn test_struct_creation_only() {
    // Tests struct instantiation, not Docker
}

#[tokio::test]
async fn test_postgres_real_sql_execution() {
    // Actually executes SQL against real database
}

#[tokio::test]
#[cfg(feature = "docker-integration")]
async fn test_docker_container_lifecycle() {
    // Requires Docker daemon
}
```

### 4.4 Explicit Mock Detection

```rust
/// Type-level distinction between mock and real
pub trait ContainerBackend {
    fn is_mock(&self) -> bool;
}

pub struct RealPostgresContainer { /* ... */ }
impl ContainerBackend for RealPostgresContainer {
    fn is_mock(&self) -> bool { false }
}

pub struct MockPostgresContainer { /* ... */ }
impl ContainerBackend for MockPostgresContainer {
    fn is_mock(&self) -> bool { true }
}

// In tests:
#[tokio::test]
async fn test_integration() {
    let container = create_postgres_container().await?;
    assert!(!container.is_mock(), "Integration test using mock!");
}
```

---

## 5. Testcontainers Validation

### 5.1 Rust Testcontainers Patterns

#### Singleton Container Pattern

**Problem**: Starting Docker containers for each test is time-consuming (e.g., Keycloak needs 10 seconds).

**Solution**: Share container across tests (with caveats).

```rust
use once_cell::sync::Lazy;
use std::sync::Arc;

// ⚠️ USE WITH CAUTION - Shared state between tests
static POSTGRES_CONTAINER: Lazy<Arc<PostgresContainer>> = Lazy::new(|| {
    Arc::new(
        tokio::runtime::Runtime::new()
            .unwrap()
            .block_on(PostgresContainer::new_async("testdb", "user", "pass"))
            .unwrap()
    )
});

#[tokio::test]
async fn test_1() {
    let container = POSTGRES_CONTAINER.clone();
    // Use shared container
}

#[tokio::test]
async fn test_2() {
    let container = POSTGRES_CONTAINER.clone();
    // Use same container
}
```

**Validation Requirements**:
1. Tests must be isolated (separate databases/schemas)
2. No test should modify shared state
3. Document singleton usage clearly
4. Consider trade-offs: speed vs. isolation

#### RAII Cleanup Pattern

**Best Practice**: Use Drop trait for guaranteed cleanup.

```rust
pub struct ContainerGuard<T> {
    container: Option<T>,
}

impl<T> ContainerGuard<T> {
    pub fn new(container: T) -> Self {
        Self { container: Some(container) }
    }

    pub fn container(&self) -> &T {
        self.container.as_ref().unwrap()
    }
}

impl<T> Drop for ContainerGuard<T> {
    fn drop(&mut self) {
        if let Some(container) = self.container.take() {
            // Cleanup code here
            // Testcontainers handles this automatically
        }
    }
}

// Usage:
#[tokio::test]
async fn test_with_cleanup() {
    let _guard = ContainerGuard::new(
        PostgresContainer::new_async(...).await?
    );
    // Use guard.container()
    // Automatic cleanup when guard goes out of scope
}
```

### 5.2 Container Lifecycle Verification

#### Verify Startup

```rust
async fn verify_container_started(container: &PostgresContainer) -> Result<()> {
    // 1. Verify container process exists
    let container_id = container.id();
    let output = Command::new("docker")
        .args(&["ps", "--filter", &format!("id={}", container_id)])
        .output()?;

    assert!(!output.stdout.is_empty(), "Container not in ps output");

    // 2. Verify container is healthy
    let output = Command::new("docker")
        .args(&["inspect", "--format", "{{.State.Health.Status}}", container_id])
        .output()?;

    let health = String::from_utf8_lossy(&output.stdout).trim();
    assert!(health == "healthy" || health == "", "Container unhealthy: {}", health);

    // 3. Verify service is accepting connections
    let conn = sqlx::PgConnection::connect(&container.connection_string()).await?;
    sqlx::query("SELECT 1").execute(&mut conn).await?;

    Ok(())
}
```

#### Verify Cleanup

```rust
async fn verify_container_cleaned_up(container_id: &str) -> Result<()> {
    // Wait for cleanup to complete
    tokio::time::sleep(Duration::from_secs(2)).await;

    // 1. Verify container no longer in ps
    let output = Command::new("docker")
        .args(&["ps", "-a", "--filter", &format!("id={}", container_id)])
        .output()?;

    assert!(output.stdout.is_empty(), "Container not removed");

    // 2. Verify associated volumes removed
    let output = Command::new("docker")
        .args(&["volume", "ls", "--filter", &format!("label=container_id={}", container_id)])
        .output()?;

    assert!(output.stdout.is_empty(), "Volumes not removed");

    Ok(())
}
```

---

## 6. Rust-Specific Patterns

### 6.1 Async/Blocking Boundary Issues

**Common Issue**: "Cannot start a runtime from within a runtime" panic.

```rust
// ❌ BAD - Blocking in async context
#[tokio::test]
async fn test_container() {
    let container = PostgresContainer::new("db", "user", "pass")?; // PANIC!
}
```

**Solution**: Use `tokio::task::spawn_blocking`.

```rust
// ✅ GOOD - Proper async wrapper
impl PostgresContainer {
    pub async fn new_async(db: &str, user: &str, pass: &str) -> Result<Self> {
        tokio::task::spawn_blocking(move || {
            Self::new(db, user, pass)
        }).await?
    }
}

#[tokio::test]
async fn test_container() {
    let container = PostgresContainer::new_async("db", "user", "pass").await?;
}
```

### 6.2 Resource Cleanup with Drop

**Pattern**: Implement Drop for automatic cleanup.

```rust
pub struct PostgresContainer {
    container: Container<'static, Postgres>,
}

impl Drop for PostgresContainer {
    fn drop(&mut self) {
        // Testcontainers handles cleanup automatically
        // This is for additional cleanup if needed
    }
}
```

**Validation**: Verify Drop is called.

```rust
#[tokio::test]
async fn test_drop_cleanup() {
    let container_id = {
        let container = PostgresContainer::new_async(...).await?;
        container.id().to_string()
    }; // Drop called here

    // Verify cleanup occurred
    tokio::time::sleep(Duration::from_secs(1)).await;
    verify_container_cleaned_up(&container_id).await?;
}
```

### 6.3 Type Safety for Mocks

**Pattern**: Use type system to distinguish mocks.

```rust
pub trait Backend {
    fn is_mock(&self) -> bool;
}

pub struct RealBackend;
impl Backend for RealBackend {
    fn is_mock(&self) -> bool { false }
}

pub struct MockBackend;
impl Backend for MockBackend {
    fn is_mock(&self) -> bool { true }
}

// Compile-time enforcement
fn require_real_backend<B: Backend>(backend: &B) {
    if backend.is_mock() {
        panic!("Real backend required for integration test");
    }
}
```

---

## 7. Validation Checklist

### 7.1 Pre-Commit Checklist

Before committing tests, verify:

- [ ] **No Mock Implementations in Production Code**
  ```bash
  grep -r "format.*Mock result" src/ && echo "FOUND MOCKS!" || echo "OK"
  ```

- [ ] **No TODO Comments in Container Methods**
  ```bash
  grep -r "TODO.*implement" src/containers.rs && echo "INCOMPLETE!" || echo "OK"
  ```

- [ ] **Tests Don't Pass Without Docker (When They Should Require It)**
  ```bash
  docker stop $(docker ps -aq)  # Stop all containers
  cargo test --test integration_tests && echo "FALSE POSITIVE!" || echo "OK"
  ```

- [ ] **All Integration Tests Use Real Operations**
  ```rust
  // Check that execute_sql() actually connects to database
  // Check that execute_command() actually uses container.exec()
  ```

- [ ] **Cleanup Verification Tests Exist**
  ```rust
  // At least one test verifies container cleanup
  // At least one test verifies Ryuk is running
  ```

### 7.2 CI/CD Checklist

Configure CI to enforce validation:

- [ ] **Separate Job for Integration Tests**
  ```yaml
  jobs:
    unit-tests:
      runs-on: ubuntu-latest
      steps:
        - run: cargo test --lib

    integration-tests:
      runs-on: ubuntu-latest
      services:
        docker:
          image: docker:dind
      steps:
        - run: cargo test --features docker-integration
  ```

- [ ] **Verify Docker Daemon Available**
  ```yaml
  - name: Verify Docker
    run: docker info
  ```

- [ ] **Pre-pull Images for Speed**
  ```yaml
  - name: Pre-pull images
    run: |
      docker pull postgres:15-alpine
      docker pull redis:7-alpine
  ```

- [ ] **Verify Cleanup After Tests**
  ```yaml
  - name: Check for leaked containers
    run: |
      LEAKED=$(docker ps -q)
      if [ ! -z "$LEAKED" ]; then
        echo "Leaked containers: $LEAKED"
        exit 1
      fi
  ```

### 7.3 Code Review Checklist

When reviewing container-related code:

- [ ] **Is this a mock or real implementation?**
  - Check for `format!("Mock result")` patterns
  - Check for TODO comments
  - Check if actual Docker API is used

- [ ] **Do tests actually test what they claim?**
  - Tests named "integration" should require Docker
  - Tests should execute real operations, not just create structs
  - Tests should verify outcomes, not just check for Ok

- [ ] **Is cleanup properly handled?**
  - Use RAII pattern with Drop
  - Verify Ryuk is managing cleanup
  - Check for leaked containers in tests

- [ ] **Are connection strings dynamic?**
  - No hardcoded ports
  - Use container.get_host_port_ipv4()
  - Handle dynamic host/port allocation

---

## 8. Real-World Examples

### 8.1 Complete Real Integration Test

```rust
use testcontainers::{clients, images, Container};
use sqlx::PgConnection;

#[tokio::test]
#[cfg(feature = "docker-integration")]
async fn test_postgres_complete_integration() {
    // 1. Start real PostgreSQL container
    let docker = clients::Cli::default();
    let postgres_image = images::postgres::Postgres::default()
        .with_version(15);

    let container = docker.run(postgres_image);

    // 2. Verify container is running
    let container_id = container.id();
    verify_container_running(container_id).await
        .expect("Container not running");

    // 3. Verify Ryuk is managing cleanup
    verify_ryuk_active().await
        .expect("Ryuk not running");

    // 4. Get dynamic connection details
    let host = container.get_host().await.expect("No host");
    let port = container.get_host_port_ipv4(5432).await.expect("No port");
    let connection_string = format!(
        "postgresql://postgres:postgres@{}:{}/postgres",
        host, port
    );

    // 5. Wait for PostgreSQL to be ready
    let mut conn = None;
    for _ in 0..30 {
        if let Ok(c) = PgConnection::connect(&connection_string).await {
            conn = Some(c);
            break;
        }
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
    let mut conn = conn.expect("PostgreSQL not ready");

    // 6. Execute REAL SQL operations
    sqlx::query("CREATE TABLE test_table (id SERIAL PRIMARY KEY, name TEXT)")
        .execute(&mut conn)
        .await
        .expect("Create table failed");

    sqlx::query("INSERT INTO test_table (name) VALUES ($1)")
        .bind("test_value")
        .execute(&mut conn)
        .await
        .expect("Insert failed");

    let row: (i32, String) = sqlx::query_as("SELECT id, name FROM test_table LIMIT 1")
        .fetch_one(&mut conn)
        .await
        .expect("Select failed");

    assert_eq!(row.1, "test_value");

    // 7. Cleanup verification happens automatically via Drop
}

async fn verify_container_running(container_id: &str) -> Result<()> {
    let output = std::process::Command::new("docker")
        .args(&["inspect", "--format", "{{.State.Running}}", container_id])
        .output()?;

    let running = String::from_utf8_lossy(&output.stdout).trim() == "true";
    if !running {
        return Err(anyhow!("Container not running"));
    }
    Ok(())
}

async fn verify_ryuk_active() -> Result<()> {
    let output = std::process::Command::new("docker")
        .args(&["ps", "--filter", "ancestor=testcontainers/ryuk:*"])
        .output()?;

    if output.stdout.is_empty() {
        return Err(anyhow!("Ryuk not running"));
    }
    Ok(())
}
```

### 8.2 Redis Real Integration Test

```rust
use testcontainers::{clients, images, Container};
use redis::AsyncCommands;

#[tokio::test]
#[cfg(feature = "docker-integration")]
async fn test_redis_complete_integration() {
    // 1. Start real Redis container
    let docker = clients::Cli::default();
    let redis_image = images::redis::Redis::default();
    let container = docker.run(redis_image);

    // 2. Get dynamic connection details
    let host = container.get_host().await.expect("No host");
    let port = container.get_host_port_ipv4(6379).await.expect("No port");
    let connection_string = format!("redis://{}:{}", host, port);

    // 3. Connect to real Redis instance
    let client = redis::Client::open(connection_string.as_str())
        .expect("Failed to create client");
    let mut conn = client.get_async_connection().await
        .expect("Failed to connect");

    // 4. Execute REAL Redis operations
    let _: () = conn.set("test_key", "test_value").await
        .expect("SET failed");

    let value: String = conn.get("test_key").await
        .expect("GET failed");

    assert_eq!(value, "test_value");

    // 5. Test additional operations
    let _: () = conn.del("test_key").await
        .expect("DEL failed");

    let exists: bool = conn.exists("test_key").await
        .expect("EXISTS failed");

    assert!(!exists);
}
```

### 8.3 Cleanup Verification Test

```rust
#[tokio::test]
async fn test_complete_cleanup_verification() {
    let container_id = {
        // 1. Start container
        let docker = clients::Cli::default();
        let container = docker.run(images::postgres::Postgres::default());

        // 2. Verify Ryuk is running
        verify_ryuk_active().await.expect("Ryuk not running");

        // 3. Verify cleanup label exists
        let cid = container.id().to_string();
        verify_cleanup_label(&cid).await.expect("No cleanup label");

        cid
    }; // Container dropped here, cleanup should start

    // 4. Wait for cleanup to complete
    tokio::time::sleep(Duration::from_secs(3)).await;

    // 5. Verify container is gone
    let output = std::process::Command::new("docker")
        .args(&["ps", "-a", "--filter", &format!("id={}", container_id)])
        .output()
        .expect("Docker ps failed");

    let ps_output = String::from_utf8_lossy(&output.stdout);
    assert!(
        !ps_output.contains(&container_id),
        "Container not cleaned up: {}",
        ps_output
    );

    // 6. Verify no leaked volumes
    let output = std::process::Command::new("docker")
        .args(&["volume", "ls"])
        .output()
        .expect("Docker volume ls failed");

    let volumes = String::from_utf8_lossy(&output.stdout);
    assert!(
        !volumes.contains(&container_id),
        "Leaked volumes found"
    );
}

async fn verify_cleanup_label(container_id: &str) -> Result<()> {
    let output = std::process::Command::new("docker")
        .args(&["inspect", "--format", "{{.Config.Labels}}", container_id])
        .output()?;

    let labels = String::from_utf8_lossy(&output.stdout);
    if !labels.contains("org.testcontainers") {
        return Err(anyhow!("Missing cleanup label"));
    }
    Ok(())
}
```

---

## 9. Summary and Recommendations

### 9.1 Key Takeaways

1. **Real Containers > Mocks**: For integration testing, always prefer real containers
2. **Validate Everything**: Don't assume operations work - verify them
3. **Dynamic Configuration**: Never hardcode ports or connection strings
4. **Cleanup Verification**: Test that resources are properly cleaned up
5. **Clear Test Organization**: Separate unit, integration, and e2e tests
6. **Ryuk is Critical**: Verify Ryuk container is managing cleanup

### 9.2 Immediate Actions for Cleanroom

Based on findings from `MOCK_VS_REAL_ANALYSIS.md`:

#### Priority 1: Fix Mock Implementations

1. **PostgresContainer::execute_sql()** (Line 111)
   ```rust
   // Current: Returns mock string
   // Fix: Use sqlx to execute real SQL
   pub async fn execute_sql(&self, sql: &str) -> Result<String> {
       let conn = self.get_connection().await?;
       let rows = sqlx::query(sql).fetch_all(&mut conn).await?;
       Ok(format!("{:?}", rows))
   }
   ```

2. **PostgresContainer::test_connection()** (Line 104)
   ```rust
   // Current: Always returns Ok
   // Fix: Actually test connection
   pub async fn test_connection(&self) -> Result<()> {
       let conn = self.get_connection().await?;
       sqlx::query("SELECT 1").execute(&mut conn).await?;
       Ok(())
   }
   ```

3. **RedisContainer::execute_command()** (Line 275)
   ```rust
   // Current: Returns mock string
   // Fix: Use redis-rs to execute real commands
   pub async fn execute_command(&self, command: &str) -> Result<String> {
       let mut conn = self.get_connection().await?;
       let result: String = redis::cmd(command).query_async(&mut conn).await?;
       Ok(result)
   }
   ```

#### Priority 2: Add Real Integration Tests

```rust
// New file: tests/integration/postgres_real.rs
#[tokio::test]
#[cfg(feature = "docker-integration")]
async fn test_postgres_real_sql_execution() {
    // Implement complete real SQL test (see example above)
}

// New file: tests/integration/redis_real.rs
#[tokio::test]
#[cfg(feature = "docker-integration")]
async fn test_redis_real_commands() {
    // Implement complete real Redis test (see example above)
}
```

#### Priority 3: Add Cleanup Verification

```rust
// New file: tests/integration/cleanup_verification.rs
#[tokio::test]
async fn test_ryuk_manages_cleanup() {
    // Verify Ryuk container exists and manages resources
}

#[tokio::test]
async fn test_no_leaked_containers() {
    // Verify containers are cleaned up after tests
}
```

### 9.3 Long-Term Improvements

1. **CI/CD Enhancement**
   - Add separate integration test job with Docker daemon
   - Add cleanup verification step
   - Pre-pull images for faster tests

2. **Documentation**
   - Document which methods are real vs mock
   - Add testing guide for contributors
   - Create architecture decision record for test strategy

3. **Monitoring**
   - Add metrics for test execution time
   - Track Docker resource usage
   - Monitor cleanup success rate

---

## 10. Conclusion

This guide provides comprehensive strategies for validating Docker integration in test frameworks. By following these best practices, you can:

- **Eliminate false positives** in containerized testing
- **Ensure real Docker integration** rather than mock implementations
- **Validate cleanup** and resource management
- **Build confidence** in your integration test suite

**Key Principle**: If a test claims to test Docker integration, it should require Docker to pass.

---

## References

1. Docker Blog: "Testcontainers Best Practices" (2025)
2. Testcontainers Documentation: "Container Lifecycle Management"
3. GitHub: testcontainers/testcontainers-rs
4. Medium: "Leveraging Testcontainers Over Mock Tests"
5. Docker Community: "Writing Reliable Docker Healthchecks"
6. Testcontainers Blog: "Ryuk the Resource Reaper"

---

**Document Status**: Complete
**Last Updated**: 2025-10-13
**Maintained By**: Hive Mind Validation Swarm
**Next Review**: Before v1.0 production release
