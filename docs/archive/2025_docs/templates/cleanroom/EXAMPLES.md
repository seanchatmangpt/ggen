<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Cleanroom Templates - Usage Examples](#cleanroom-templates---usage-examples)
  - [📦 Quick Start](#-quick-start)
    - [Example 1: Minimal Test Setup](#example-1-minimal-test-setup)
  - [🗄️ Database Testing Examples](#-database-testing-examples)
    - [Example 2: PostgreSQL Integration](#example-2-postgresql-integration)
    - [Example 3: Redis Cache Testing](#example-3-redis-cache-testing)
  - [🤖 Swarm Coordination Examples](#-swarm-coordination-examples)
    - [Example 4: Multi-Agent Testing](#example-4-multi-agent-testing)
  - [🚀 CI/CD Pipeline Examples](#-cicd-pipeline-examples)
    - [Example 5: GitHub Actions Setup](#example-5-github-actions-setup)
  - [📊 Performance Testing Examples](#-performance-testing-examples)
    - [Example 6: Benchmark Suite](#example-6-benchmark-suite)
  - [🔧 Complete Project Examples](#-complete-project-examples)
    - [Example 7: Full-Stack Application](#example-7-full-stack-application)
  - [🎯 Real-World Use Cases](#-real-world-use-cases)
    - [Example 8: Microservices Testing](#example-8-microservices-testing)
    - [Example 9: E2E Testing with Swarm](#example-9-e2e-testing-with-swarm)
  - [🔍 Debugging Examples](#-debugging-examples)
    - [Example 10: Verbose Testing](#example-10-verbose-testing)
  - [📈 Performance Optimization](#-performance-optimization)
    - [Example 11: Optimized Benchmarks](#example-11-optimized-benchmarks)
  - [🛡️ Security Testing](#-security-testing)
    - [Example 12: Security Policy Testing](#example-12-security-policy-testing)
  - [🎓 Best Practices](#-best-practices)
    - [Example 13: Deterministic Testing](#example-13-deterministic-testing)
    - [Example 14: Resource Limits](#example-14-resource-limits)
  - [🔧 Troubleshooting](#-troubleshooting)
    - [Common Issues](#common-issues)
  - [📚 Additional Resources](#-additional-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Cleanroom Templates - Usage Examples

This document provides comprehensive examples for using the cleanroom gpack templates.

## 📦 Quick Start

### Example 1: Minimal Test Setup

Create a basic hermetic test environment:

```bash
# Create project
cargo init my-test-project
cd my-test-project

# Add cleanroom dependency
cargo add clnrm

# Generate test environment
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=my-test-project \
  --var test_name=basic_tests \
  --var determinism=42

# Run tests
cargo test
```

**Output:**
```
my-test-project/
└── tests/
    └── test_environment.rs    # Hermetic test suite
```

---

## 🗄️ Database Testing Examples

### Example 2: PostgreSQL Integration

Set up PostgreSQL container testing:

```bash
# Generate PostgreSQL tests
ggen template generate cleanroom/postgres-container.tmpl \
  --var project_name=backend-api \
  --var test_suite_name=database_tests \
  --var db_name=api_db \
  --var db_user=api_user \
  --var db_password=secure_pass \
  --var determinism=42

# Add testcontainers dependencies
cargo add testcontainers testcontainers-modules --features postgres

# Run PostgreSQL tests
cargo test postgres_container
```

**Generated Test File:**
```rust
// tests/postgres_container_test.rs

#[tokio::test]
async fn test_database_tests_basic_connection() -> Result<()> {
    let postgres = GenericImage::new("postgres", "16-alpine")
        .with_env_var("POSTGRES_DB", "api_db")
        .with_env_var("POSTGRES_USER", "api_user")
        .with_env_var("POSTGRES_PASSWORD", "secure_pass")
        .start()
        .await?;

    // Test connection...
    Ok(())
}
```

### Example 3: Redis Cache Testing

Set up Redis container testing:

```bash
# Generate Redis tests
ggen template generate cleanroom/redis-container.tmpl \
  --var project_name=cache-service \
  --var test_suite_name=cache_tests \
  --var determinism=42

# Run Redis tests
cargo test redis_container
```

---

## 🤖 Swarm Coordination Examples

### Example 4: Multi-Agent Testing

Set up swarm coordination for parallel testing:

```bash
# Generate swarm tests
ggen template generate cleanroom/swarm-coordinator.tmpl \
  --var project_name=distributed-app \
  --var test_suite_name=swarm_tests \
  --var swarm_size=10 \
  --var determinism=42

# Run swarm tests
cargo test swarm_coordinator
```

**Test Output:**
```
running 8 tests
test test_swarm_tests_init ... ok
test test_swarm_tests_spawn_agents ... ok
test test_swarm_tests_task_orchestration ... ok
test test_swarm_tests_status_monitoring ... ok
test test_swarm_tests_agent_metrics ... ok
test test_swarm_tests_parallel_execution ... ok
test test_swarm_tests_error_recovery ... ok
test test_swarm_tests_cleanup ... ok

test result: ok. 8 passed; 0 failed; 0 ignored; 0 measured
```

---

## 🚀 CI/CD Pipeline Examples

### Example 5: GitHub Actions Setup

Generate complete CI/CD pipeline:

```bash
# Generate CI pipeline
ggen template generate cleanroom/ci-pipeline.tmpl \
  --var project_name=production-service \
  --var rust_version=1.75.0 \
  --var determinism=42

# Verify generated workflow
cat .github/workflows/cleanroom-ci.yml

# Commit and push
git add .github/workflows/cleanroom-ci.yml
git commit -m "Add cleanroom CI pipeline"
git push
```

**Pipeline Features:**
- ✅ Code formatting (rustfmt)
- ✅ Linting (clippy)
- ✅ Unit tests
- ✅ Integration tests
- ✅ Container tests
- ✅ Swarm coordination tests
- ✅ Security audits
- ✅ Performance benchmarks
- ✅ Production readiness checks

---

## 📊 Performance Testing Examples

### Example 6: Benchmark Suite

Set up comprehensive performance benchmarks:

```bash
# Generate benchmark suite
ggen template generate cleanroom/benchmark-suite.tmpl \
  --var project_name=performance-test \
  --var benchmark_name=cleanroom_perf \
  --var determinism=42

# Run benchmarks
cargo bench

# View results
open target/criterion/report/index.html
```

**Benchmark Output:**
```
basic_echo_command      time:   [12.456 ms 12.789 ms 13.123 ms]
hermetic_execution      time:   [15.234 ms 15.567 ms 15.890 ms]
deterministic_execution time:   [16.123 ms 16.456 ms 16.789 ms]
```

---

## 🔧 Complete Project Examples

### Example 7: Full-Stack Application

Generate complete test suite for a full-stack application:

```bash
# Create project
cargo init fullstack-app
cd fullstack-app

# Add all dependencies
cargo add clnrm testcontainers testcontainers-modules tokio anyhow serde serde_json
cargo add --dev criterion

# Generate all templates
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=fullstack-app \
  --var test_name=integration_tests

ggen template generate cleanroom/postgres-container.tmpl \
  --var project_name=fullstack-app \
  --var db_name=fullstack_db

ggen template generate cleanroom/redis-container.tmpl \
  --var project_name=fullstack-app

ggen template generate cleanroom/swarm-coordinator.tmpl \
  --var project_name=fullstack-app \
  --var swarm_size=8

ggen template generate cleanroom/ci-pipeline.tmpl \
  --var project_name=fullstack-app

ggen template generate cleanroom/benchmark-suite.tmpl \
  --var project_name=fullstack-app

# Run all tests
cargo test --verbose

# Run benchmarks
cargo bench
```

**Project Structure:**
```
fullstack-app/
├── .github/
│   └── workflows/
│       └── cleanroom-ci.yml          # CI/CD pipeline
├── benches/
│   └── cleanroom_benchmarks.rs       # Performance tests
├── tests/
│   ├── test_environment.rs           # Basic hermetic tests
│   ├── postgres_container_test.rs    # Database tests
│   ├── redis_container_test.rs       # Cache tests
│   └── swarm_coordinator_test.rs     # Swarm tests
├── src/
│   └── lib.rs
└── Cargo.toml
```

---

## 🎯 Real-World Use Cases

### Example 8: Microservices Testing

Test a microservices architecture:

```bash
# Service 1: API Gateway
cd services/api-gateway
ggen template generate cleanroom/test-environment.tmpl --var project_name=api-gateway
ggen template generate cleanroom/redis-container.tmpl --var project_name=api-gateway

# Service 2: User Service
cd ../user-service
ggen template generate cleanroom/test-environment.tmpl --var project_name=user-service
ggen template generate cleanroom/postgres-container.tmpl --var project_name=user-service

# Service 3: Order Service
cd ../order-service
ggen template generate cleanroom/test-environment.tmpl --var project_name=order-service
ggen template generate cleanroom/postgres-container.tmpl --var project_name=order-service
ggen template generate cleanroom/redis-container.tmpl --var project_name=order-service

# Run all service tests
cd ../..
cargo test --workspace
```

### Example 9: E2E Testing with Swarm

End-to-end testing with parallel agent coordination:

```bash
# Generate E2E test suite
ggen template generate cleanroom/swarm-coordinator.tmpl \
  --var project_name=e2e-tests \
  --var test_suite_name=end_to_end \
  --var swarm_size=15 \
  --var determinism=42

# Run E2E tests with swarm coordination
cargo test end_to_end -- --test-threads=1
```

---

## 🔍 Debugging Examples

### Example 10: Verbose Testing

Run tests with detailed output:

```bash
# Run with verbose output
RUST_LOG=debug cargo test -- --nocapture

# Run specific test with details
cargo test test_postgres_container_basic_connection -- --nocapture

# Run benchmarks with verbose output
cargo bench -- --verbose
```

---

## 📈 Performance Optimization

### Example 11: Optimized Benchmarks

Optimize benchmark configuration:

```bash
# Generate benchmarks
ggen template generate cleanroom/benchmark-suite.tmpl \
  --var project_name=optimized-app

# Run with release profile
cargo bench --profile release

# Generate flamegraph
cargo flamegraph --bench cleanroom_benchmarks
```

---

## 🛡️ Security Testing

### Example 12: Security Policy Testing

Test different security levels:

```rust
// In your test file
use clnrm::{Policy, SecurityLevel};

#[test]
fn test_locked_security() -> Result<()> {
    let policy = Policy::locked();
    let result = run_with_policy(["echo", "secure"], &policy)?;
    assert_eq!(result.exit_code, 0);
    Ok(())
}

#[test]
fn test_sandbox_security() -> Result<()> {
    let policy = Policy::sandbox();
    let result = run_with_policy(["echo", "sandboxed"], &policy)?;
    assert_eq!(result.exit_code, 0);
    Ok(())
}
```

---

## 🎓 Best Practices

### Example 13: Deterministic Testing

Always use deterministic seeds for reproducibility:

```bash
# Generate with consistent seed
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=reproducible-tests \
  --var determinism=12345

# Run tests multiple times - should produce identical results
cargo test
cargo test
cargo test
```

### Example 14: Resource Limits

Test with resource constraints:

```rust
#[test]
fn test_resource_limits() -> Result<()> {
    let policy = Policy {
        resources: clnrm::policy::ResourcePolicy {
            max_memory_usage_bytes: 100 * 1024 * 1024, // 100MB
            max_cpu_usage_percent: 50.0,
            max_execution_time_ms: 5000,
            ..Default::default()
        },
        ..Default::default()
    };

    let result = run_with_policy(["echo", "limited"], &policy)?;
    assert_eq!(result.exit_code, 0);
    Ok(())
}
```

---

## 🔧 Troubleshooting

### Common Issues

**Issue 1: Container not starting**
```bash
# Check Docker is running
docker ps

# Pull required images
docker pull postgres:16-alpine
docker pull redis:7-alpine
```

**Issue 2: Tests failing**
```bash
# Clean and rebuild
cargo clean
cargo build
cargo test
```

**Issue 3: Benchmark issues**
```bash
# Ensure criterion is added
cargo add --dev criterion

# Run benchmarks
cargo bench
```

---

## 📚 Additional Resources

- [Cleanroom Documentation](../../cleanroom/README.md)
- [Testcontainers Guide](https://docs.rs/testcontainers/)
- [Criterion Benchmarking](https://bheisler.github.io/criterion.rs/book/)
- [GitHub Actions CI/CD](https://docs.github.com/en/actions)

---

**Happy Testing with Cleanroom!** 🚀
