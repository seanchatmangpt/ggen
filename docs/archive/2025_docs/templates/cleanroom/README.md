<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Cleanroom Testing Framework - Gpack Templates](#cleanroom-testing-framework---gpack-templates)
  - [🚀 Overview](#-overview)
  - [📦 Installation](#-installation)
    - [Install Ggen CLI](#install-ggen-cli)
    - [Install Cleanroom](#install-cleanroom)
  - [📋 Available Templates](#-available-templates)
    - [1. Test Environment (`test-environment.tmpl`)](#1-test-environment-test-environmenttmpl)
    - [2. PostgreSQL Container (`postgres-container.tmpl`)](#2-postgresql-container-postgres-containertmpl)
    - [3. Redis Container (`redis-container.tmpl`)](#3-redis-container-redis-containertmpl)
    - [4. Swarm Coordinator (`swarm-coordinator.tmpl`)](#4-swarm-coordinator-swarm-coordinatortmpl)
    - [5. CI/CD Pipeline (`ci-pipeline.tmpl`)](#5-cicd-pipeline-ci-pipelinetmpl)
    - [6. Benchmark Suite (`benchmark-suite.tmpl`)](#6-benchmark-suite-benchmark-suitetmpl)
  - [🎯 Quick Start](#-quick-start)
    - [Complete Project Setup](#complete-project-setup)
  - [📚 Examples](#-examples)
    - [Example 1: Basic Hermetic Testing](#example-1-basic-hermetic-testing)
    - [Example 2: Database Integration Testing](#example-2-database-integration-testing)
    - [Example 3: Swarm Coordination Testing](#example-3-swarm-coordination-testing)
    - [Example 4: Complete CI/CD Setup](#example-4-complete-cicd-setup)
  - [🔧 Configuration](#-configuration)
    - [Custom Determinism Seeds](#custom-determinism-seeds)
    - [Security Policies](#security-policies)
  - [📊 Testing Workflow](#-testing-workflow)
    - [1. Development Testing](#1-development-testing)
    - [2. Performance Testing](#2-performance-testing)
    - [3. CI/CD Pipeline](#3-cicd-pipeline)
  - [🛠️ Best Practices](#-best-practices)
    - [1. Use Deterministic Seeds](#1-use-deterministic-seeds)
    - [2. Clean Up Containers](#2-clean-up-containers)
    - [3. Parallel Test Execution](#3-parallel-test-execution)
  - [📖 Documentation](#-documentation)
    - [Cleanroom Framework](#cleanroom-framework)
    - [Container Testing](#container-testing)
    - [Swarm Coordination](#swarm-coordination)
  - [🤝 Contributing](#-contributing)
  - [📝 License](#-license)
  - [🔗 Links](#-links)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Cleanroom Testing Framework - Gpack Templates

Production-ready templates for building hermetic, deterministic test environments using the Cleanroom Testing Framework.

## 🚀 Overview

The Cleanroom gpack provides comprehensive templates for:

- **Hermetic Test Environments** - Isolated, reproducible testing
- **Container Integration** - PostgreSQL, Redis, and more
- **Swarm Coordination** - Multi-agent parallel testing
- **CI/CD Pipelines** - GitHub Actions integration
- **Performance Benchmarks** - Criterion-based profiling

## 📦 Installation

### Install Ggen CLI

```bash
# Install ggen CLI
cargo install ggen

# Verify installation
ggen --version
```

### Install Cleanroom

```bash
# Add cleanroom to your project
cargo add clnrm testcontainers testcontainers-modules tokio anyhow

# Or install as CLI
cargo install cleanroom
```

## 📋 Available Templates

### 1. Test Environment (`test-environment.tmpl`)

Basic hermetic test environment with security policies and deterministic execution.

**Variables:**
- `project_name` - Project name (default: "my-project")
- `test_name` - Test suite name (default: "integration_test")
- `determinism` - Determinism seed (default: 42)

**Usage:**
```bash
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=my-app \
  --var test_name=api_tests \
  --var determinism=42
```

**Generated:** `my-app/tests/test_environment.rs`

### 2. PostgreSQL Container (`postgres-container.tmpl`)

PostgreSQL container integration tests with schema creation and CRUD operations.

**Variables:**
- `project_name` - Project name
- `test_suite_name` - Test suite name (default: "postgres_integration")
- `db_name` - Database name (default: "testdb")
- `db_user` - Database user (default: "testuser")
- `db_password` - Database password (default: "testpass")
- `determinism` - Determinism seed

**Usage:**
```bash
ggen template generate cleanroom/postgres-container.tmpl \
  --var project_name=my-app \
  --var db_name=mydb
```

**Generated:** `my-app/tests/postgres_container_test.rs`

### 3. Redis Container (`redis-container.tmpl`)

Redis container integration tests with string, list, hash, set operations and pub/sub.

**Variables:**
- `project_name` - Project name
- `test_suite_name` - Test suite name (default: "redis_integration")
- `determinism` - Determinism seed

**Usage:**
```bash
ggen template generate cleanroom/redis-container.tmpl \
  --var project_name=my-app
```

**Generated:** `my-app/tests/redis_container_test.rs`

### 4. Swarm Coordinator (`swarm-coordinator.tmpl`)

Multi-agent swarm coordination tests with task orchestration and monitoring.

**Variables:**
- `project_name` - Project name
- `test_suite_name` - Test suite name (default: "swarm_coordination")
- `swarm_size` - Number of agents (default: 5)
- `determinism` - Determinism seed

**Usage:**
```bash
ggen template generate cleanroom/swarm-coordinator.tmpl \
  --var project_name=my-app \
  --var swarm_size=10
```

**Generated:** `my-app/tests/swarm_coordinator_test.rs`

### 5. CI/CD Pipeline (`ci-pipeline.tmpl`)

GitHub Actions CI/CD pipeline with cleanroom tests, security audits, and benchmarks.

**Variables:**
- `project_name` - Project name
- `rust_version` - Rust version (default: "1.75.0")
- `determinism` - Determinism seed

**Usage:**
```bash
ggen template generate cleanroom/ci-pipeline.tmpl \
  --var project_name=my-app \
  --var rust_version=1.75.0
```

**Generated:** `.github/workflows/cleanroom-ci.yml`

### 6. Benchmark Suite (`benchmark-suite.tmpl`)

Comprehensive performance benchmarks using Criterion for cleanroom execution.

**Variables:**
- `project_name` - Project name
- `benchmark_name` - Benchmark suite name (default: "cleanroom_performance")
- `determinism` - Determinism seed

**Usage:**
```bash
ggen template generate cleanroom/benchmark-suite.tmpl \
  --var project_name=my-app
```

**Generated:** `my-app/benches/cleanroom_benchmarks.rs`

## 🎯 Quick Start

### Complete Project Setup

```bash
# 1. Create new project
cargo init my-cleanroom-app
cd my-cleanroom-app

# 2. Add dependencies
cargo add clnrm testcontainers testcontainers-modules tokio anyhow serde serde_json
cargo add --dev criterion

# 3. Generate all templates
ggen template generate cleanroom/test-environment.tmpl --var project_name=my-cleanroom-app
ggen template generate cleanroom/postgres-container.tmpl --var project_name=my-cleanroom-app
ggen template generate cleanroom/redis-container.tmpl --var project_name=my-cleanroom-app
ggen template generate cleanroom/swarm-coordinator.tmpl --var project_name=my-cleanroom-app
ggen template generate cleanroom/ci-pipeline.tmpl --var project_name=my-cleanroom-app
ggen template generate cleanroom/benchmark-suite.tmpl --var project_name=my-cleanroom-app

# 4. Run tests
cargo test

# 5. Run benchmarks
cargo bench
```

## 📚 Examples

### Example 1: Basic Hermetic Testing

```bash
# Generate basic test environment
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=web-service \
  --var test_name=api_tests

# Run tests
cargo test api_tests
```

### Example 2: Database Integration Testing

```bash
# Generate database tests
ggen template generate cleanroom/postgres-container.tmpl \
  --var project_name=backend \
  --var db_name=production_db

ggen template generate cleanroom/redis-container.tmpl \
  --var project_name=backend

# Run database tests
cargo test postgres_container
cargo test redis_container
```

### Example 3: Swarm Coordination Testing

```bash
# Generate swarm tests
ggen template generate cleanroom/swarm-coordinator.tmpl \
  --var project_name=distributed-system \
  --var swarm_size=20

# Run swarm tests
cargo test swarm_coordinator
```

### Example 4: Complete CI/CD Setup

```bash
# Generate CI pipeline
ggen template generate cleanroom/ci-pipeline.tmpl \
  --var project_name=production-api \
  --var rust_version=1.75.0

# Commit and push
git add .github/workflows/cleanroom-ci.yml
git commit -m "Add cleanroom CI pipeline"
git push
```

## 🔧 Configuration

### Custom Determinism Seeds

All templates support custom determinism seeds for reproducible testing:

```bash
ggen template generate cleanroom/test-environment.tmpl \
  --var project_name=my-app \
  --var determinism=12345
```

### Security Policies

Templates include three security levels:

1. **Default** - Balanced security
2. **Sandbox** - Enhanced isolation
3. **Locked** - Maximum security

Customize in generated tests:

```rust
let policy = Policy::locked(); // Maximum security
let policy = Policy::sandbox(); // Enhanced isolation
let policy = Policy::default(); // Balanced
```

## 📊 Testing Workflow

### 1. Development Testing

```bash
# Run all tests
cargo test

# Run specific test suite
cargo test test_environment
cargo test postgres_container
cargo test swarm_coordinator
```

### 2. Performance Testing

```bash
# Run benchmarks
cargo bench

# View benchmark results
open target/criterion/report/index.html
```

### 3. CI/CD Pipeline

The generated CI pipeline includes:

- ✅ Code formatting checks
- ✅ Clippy lints
- ✅ Unit tests
- ✅ Integration tests
- ✅ Container tests
- ✅ Swarm coordination tests
- ✅ Security audits
- ✅ Performance benchmarks
- ✅ Production readiness checks

## 🛠️ Best Practices

### 1. Use Deterministic Seeds

Always use deterministic seeds for reproducible tests:

```rust
let policy = Policy {
    determinism: Some(clnrm::policy::DeterminismConfig {
        seed: 42,
        ..Default::default()
    }),
    ..Default::default()
};
```

### 2. Clean Up Containers

Containers are automatically cleaned up, but you can verify:

```bash
# Check for leftover containers
docker ps -a | grep cleanroom

# Clean up manually if needed
docker system prune -f
```

### 3. Parallel Test Execution

Enable parallel testing in `Cargo.toml`:

```toml
[profile.test]
opt-level = 2
```

## 📖 Documentation

### Cleanroom Framework

- [Cleanroom README](../../cleanroom/README.md)
- [API Documentation](../../cleanroom/docs/)

### Container Testing

- [Testcontainers Documentation](https://docs.rs/testcontainers/)
- [PostgreSQL Testing Guide](../../cleanroom/docs/postgres.md)
- [Redis Testing Guide](../../cleanroom/docs/redis.md)

### Swarm Coordination

- [Swarm CLI Documentation](../../cleanroom/examples/swarm_cli_usage.sh)
- [Multi-Agent Testing](../../cleanroom/docs/swarm.md)

## 🤝 Contributing

Contributions welcome! Please read [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## 📝 License

MIT OR Apache-2.0

## 🔗 Links

- [Cleanroom Repository](https://github.com/seanchatmangpt/ggen/tree/master/cleanroom)
- [Ggen CLI](https://github.com/seanchatmangpt/ggen)
- [Issue Tracker](https://github.com/seanchatmangpt/ggen/issues)

---

**Cleanroom delivers production-ready hermetic testing with minimal effort!** 🚀
