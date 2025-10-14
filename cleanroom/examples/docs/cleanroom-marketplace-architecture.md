# Cleanroom-Marketplace Integration Architecture

**Version**: 1.0.0
**Date**: 2025-10-13
**Status**: Design Specification
**Authors**: Architecture Team

---

## Executive Summary

This document defines the comprehensive architecture for integrating cleanroom's hermetic testing framework with ggen's marketplace (gpacks) and lifecycle management systems. The integration enables developers to discover, install, and execute cleanroom test environments through ggen's unified CLI, marketplace discovery, and lifecycle automation.

**Key Goals:**
1. Package cleanroom templates as discoverable gpacks in the marketplace
2. Integrate cleanroom testing into ggen lifecycle phases (init, test, validate, deploy)
3. Enable automated CI/CD workflows with cleanroom-based validation
4. Provide production-ready test environment templates

**Key Benefits:**
- **80/20 Rule Applied**: Focus on high-value test patterns that deliver maximum reliability
- **Marketplace Discovery**: Search and install proven test configurations
- **Lifecycle Automation**: Seamless integration with ggen's build/test/deploy workflow
- **Production Readiness**: Validate deployments with hermetic testing before production

---

## 1. System Architecture Overview

### 1.1 Component Relationships

```
┌─────────────────────────────────────────────────────────────────┐
│                        GGEN ECOSYSTEM                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐ │
│  │              │      │              │      │              │ │
│  │  MARKETPLACE │─────▶│   LIFECYCLE  │─────▶│  CLEANROOM   │ │
│  │   (gpacks)   │      │  (make.toml) │      │  (testing)   │ │
│  │              │      │              │      │              │ │
│  └──────────────┘      └──────────────┘      └──────────────┘ │
│         │                      │                      │        │
│         │                      │                      │        │
│         ▼                      ▼                      ▼        │
│  ┌──────────────────────────────────────────────────────────┐ │
│  │              INTEGRATION LAYER                           │ │
│  ├──────────────────────────────────────────────────────────┤ │
│  │ • Template Discovery  • Hook System   • CLI Integration │ │
│  │ • Package Management  • State Tracking • JSON Output    │ │
│  └──────────────────────────────────────────────────────────┘ │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

### 1.2 Integration Points

| Component | Integration Method | Purpose |
|-----------|-------------------|---------|
| **Marketplace** | Cleanroom gpacks with templates | Distribute test configurations |
| **Lifecycle** | Test/validate phase hooks | Execute cleanroom tests in workflow |
| **Cleanroom CLI** | JSON output format | Machine-readable test results |
| **State Management** | `.ggen/state.json` tracking | Test history and caching |
| **Production Readiness** | Validation gates | Pre-deployment verification |

---

## 2. Marketplace Integration Design

### 2.1 Cleanroom Gpack Structure

**Gpack ID Convention**: `io.ggen.cleanroom.<category>.<name>`

```
marketplace/packages/cleanroom-test-environments/
├── gpack.toml                 # Package manifest
├── package.toml               # Marketplace metadata
├── make.toml                  # Lifecycle integration
├── README.md                  # Documentation
├── templates/                 # Test environment templates
│   ├── rust-web-service.tmpl # Web service test setup
│   ├── database-test.tmpl    # Database test configuration
│   ├── api-integration.tmpl  # API integration tests
│   └── multi-container.tmpl  # Multi-service orchestration
└── examples/                  # Usage examples
    ├── basic-usage.rs
    ├── advanced-workflow.rs
    └── ci-cd-integration.sh
```

### 2.2 Gpack Manifest (gpack.toml)

```toml
[gpack]
id = "io.ggen.cleanroom.test-environments"
name = "Cleanroom Test Environments"
version = "1.0.0"
description = "Production-ready hermetic test environments for Rust projects"
license = "MIT"
ggen_compat = ">=1.2.0"
cleanroom_version = ">=0.1.0"

[dependencies]
ggen-core = "^1.2.0"
ggen-cleanroom = "^0.1.0"
testcontainers = "^0.22"

[templates]
patterns = ["templates/*.tmpl"]

[lifecycle]
# Provide lifecycle tasks this gpack supports
phases = ["test", "validate", "benchmark"]

[metadata]
category = "testing"
keywords = ["testing", "cleanroom", "hermetic", "isolation", "rust"]
repository = "https://github.com/seanchatmangpt/ggen"
documentation = "https://docs.rs/ggen-cleanroom"
```

### 2.3 Package Metadata (package.toml)

```toml
[package]
id = "io.ggen.cleanroom.test-environments"
name = "Cleanroom Test Environments"
version = "1.0.0"
author = "ggen-team"
category = "testing"
tags = ["cleanroom", "testing", "hermetic", "isolated", "rust"]

[versions."1.0.0"]
git_url = "https://github.com/seanchatmangpt/ggen.git"
git_rev = "main"
sha256 = "calculated-hash-value"

[features]
default = ["basic-tests"]
basic-tests = []
advanced-tests = ["multi-container", "benchmarking"]
multi-container = []
benchmarking = []
```

### 2.4 Template Examples

#### Rust Web Service Test Template

```yaml
---
# templates/rust-web-service.tmpl
to: "tests/cleanroom_web_test.rs"
vars:
  service_name: "{{ project_name }}"
  port: 8080
  test_timeout: 60
---
use cleanroom::{CleanroomEnv, CleanroomConfig};
use testcontainers::{clients::Cli, images::postgres::Postgres};

#[tokio::test]
async fn test_{{ service_name }}_integration() {
    // Create cleanroom environment
    let config = CleanroomConfig::builder()
        .timeout_seconds({{ test_timeout }})
        .enable_containers(true)
        .build();

    let mut env = CleanroomEnv::new(config)
        .expect("Failed to create cleanroom");

    // Run test in isolated environment
    let result = env.run_test(|env| {
        // Start PostgreSQL container
        let docker = Cli::default();
        let postgres = docker.run(Postgres::default());
        let port = postgres.get_host_port_ipv4(5432);

        // Set environment variables
        env.set_env("DATABASE_URL",
            format!("postgres://postgres@localhost:{}/test", port))?;
        env.set_env("PORT", "{{ port }}")?;

        // Start service and run tests
        // ... test implementation ...

        Ok(())
    });

    assert!(result.status.is_success());
}
```

#### Database Test Template

```yaml
---
# templates/database-test.tmpl
to: "tests/cleanroom_db_test.rs"
vars:
  db_type: "postgres"
  schema_path: "schema.sql"
---
use cleanroom::{CleanroomEnv, CleanroomConfig};
use testcontainers::{clients::Cli, images::postgres::Postgres};

#[tokio::test]
async fn test_database_migrations() {
    let config = CleanroomConfig::default();
    let mut env = CleanroomEnv::new(config)
        .expect("Failed to create cleanroom");

    let result = env.run_test(|env| {
        let docker = Cli::default();
        let container = docker.run(Postgres::default());
        let port = container.get_host_port_ipv4(5432);

        // Load schema
        let schema = env.read_file("{{ schema_path }}")?;

        // Run migrations
        // ... migration logic ...

        Ok(())
    });

    assert!(result.status.is_success());
}
```

### 2.5 Marketplace Commands

```bash
# Search for cleanroom packages
ggen market search "cleanroom test"

# View package details
ggen market info io.ggen.cleanroom.test-environments

# Install cleanroom test templates
ggen market add io.ggen.cleanroom.test-environments

# List installed templates
ggen template list | grep cleanroom

# Generate test from template
ggen template generate rust-web-service.tmpl \
    --var project_name=my-api \
    --var port=8080

# Update cleanroom packages
ggen market update io.ggen.cleanroom.test-environments
```

### 2.6 Registry Integration

**Update `registry/index.json`:**

```json
{
  "packs": {
    "io.ggen.cleanroom.test-environments": {
      "id": "io.ggen.cleanroom.test-environments",
      "name": "Cleanroom Test Environments",
      "description": "Production-ready hermetic test environments with container isolation",
      "tags": ["cleanroom", "testing", "hermetic", "rust", "testcontainers"],
      "keywords": ["testing", "integration", "isolated", "deterministic"],
      "category": "testing",
      "author": "ggen-team",
      "latest_version": "1.0.0",
      "versions": {
        "1.0.0": {
          "version": "1.0.0",
          "git_url": "https://github.com/seanchatmangpt/ggen.git",
          "git_rev": "main",
          "sha256": "calculated-hash",
          "requires": {
            "ggen": ">=1.2.0",
            "cleanroom": ">=0.1.0"
          }
        }
      },
      "license": "MIT",
      "homepage": "https://github.com/seanchatmangpt/ggen",
      "repository": "https://github.com/seanchatmangpt/ggen",
      "documentation": "https://docs.rs/ggen-cleanroom"
    }
  }
}
```

---

## 3. Lifecycle Integration Design

### 3.1 Lifecycle Hooks Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    LIFECYCLE PHASES                          │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  init ──▶ setup ──▶ build ──▶ test ──▶ validate ──▶ deploy │
│                               ▲        ▲                     │
│                               │        │                     │
│                          CLEANROOM  CLEANROOM                │
│                          UNIT TESTS INTEGRATION              │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

### 3.2 Make.toml Integration

**Complete lifecycle configuration with cleanroom:**

```toml
[project]
name = "rust-web-service"
type = "service"
version = "1.0.0"

# ============================================================================
# LIFECYCLE: INIT (Setup cleanroom test structure)
# ============================================================================

[lifecycle.init]
description = "Initialize project with cleanroom test structure"
commands = [
    "cargo init --lib",
    "mkdir -p tests examples docs",
    "ggen template generate rust-web-service.tmpl --var project_name=${PROJECT_NAME}",
]

# ============================================================================
# LIFECYCLE: TEST (Run cleanroom tests)
# ============================================================================

[lifecycle.test]
description = "Run all tests including cleanroom integration tests"
commands = [
    "cargo test --lib",                    # Unit tests
    "cargo test --test '*' -- --nocapture", # Integration tests (cleanroom)
]
cache = true
outputs = ["target/test-results/"]

[lifecycle."test:unit"]
description = "Run unit tests only"
command = "cargo test --lib"

[lifecycle."test:integration"]
description = "Run cleanroom integration tests"
command = "cargo test --test '*'"

[lifecycle."test:cleanroom"]
description = "Run cleanroom tests with detailed output"
command = "cleanroom test run --file tests/*_test.rs --output json"

# ============================================================================
# LIFECYCLE: VALIDATE (Production readiness checks)
# ============================================================================

[lifecycle.validate]
description = "Validate deployment readiness with cleanroom"
commands = [
    "cleanroom env create --name validation-env",
    "cleanroom swarm init --topology mesh --agents 3",
    "cleanroom bench --bench-type swarm --iterations 10",
    "cleanroom swarm status --output json",
]

[lifecycle."validate:coverage"]
description = "Check test coverage requirements"
command = "cargo tarpaulin --out json --output-dir coverage"

[lifecycle."validate:performance"]
description = "Run performance benchmarks in cleanroom"
command = "cleanroom bench --bench-type performance --output json"

# ============================================================================
# LIFECYCLE: BENCHMARK (Performance testing)
# ============================================================================

[lifecycle.benchmark]
description = "Run comprehensive benchmarks"
commands = [
    "cargo bench",
    "cleanroom bench --bench-type swarm --iterations 100",
]

# ============================================================================
# HOOKS: Quality Gates
# ============================================================================

[hooks]
# Always run tests before building
before_build = ["test"]

# Validate before deployment
before_deploy = ["validate", "validate:coverage", "validate:performance"]

# Notify on deployment
after_deploy = ["notify:slack"]

# Cleanup after tests
after_test = ["cleanup:containers"]

# ============================================================================
# CLEANUP TASKS
# ============================================================================

[lifecycle."cleanup:containers"]
description = "Cleanup cleanroom test containers"
command = "cleanroom env cleanup"

[lifecycle."notify:slack"]
description = "Send deployment notification"
command = "echo '✅ Deployment complete'"

# ============================================================================
# ENVIRONMENT-SPECIFIC CONFIGURATION
# ============================================================================

[env.development]
CLEANROOM_TIMEOUT = "60"
CLEANROOM_PARALLEL = "true"
RUST_LOG = "debug"

[env.staging]
CLEANROOM_TIMEOUT = "120"
CLEANROOM_PARALLEL = "true"
RUST_LOG = "info"

[env.production]
CLEANROOM_TIMEOUT = "300"
CLEANROOM_PARALLEL = "false"  # Sequential for reliability
RUST_LOG = "warn"
```

### 3.3 Lifecycle Commands

```bash
# Initialize project with cleanroom
ggen lifecycle run init

# Run cleanroom tests
ggen lifecycle run test

# Run specific test types
ggen lifecycle run test:unit
ggen lifecycle run test:integration
ggen lifecycle run test:cleanroom

# Validate for production
ggen lifecycle validate --env production

# Check production readiness
ggen lifecycle readiness

# Full pipeline
ggen lifecycle pipeline "init setup build test validate deploy"

# Environment-specific execution
ggen lifecycle run deploy --env staging
ggen lifecycle run deploy --env production
```

### 3.4 Production Readiness Integration

**Cleanroom requirements tracked in `.ggen/production_readiness.json`:**

```json
{
  "requirements": {
    "cleanroom-unit-tests": {
      "id": "cleanroom-unit-tests",
      "category": "testing",
      "status": "complete",
      "verification": "cargo test --lib",
      "last_verified": "2025-10-13T10:00:00Z"
    },
    "cleanroom-integration-tests": {
      "id": "cleanroom-integration-tests",
      "category": "testing",
      "status": "complete",
      "verification": "cleanroom test run --output json",
      "last_verified": "2025-10-13T10:05:00Z"
    },
    "cleanroom-test-coverage": {
      "id": "cleanroom-test-coverage",
      "category": "quality",
      "status": "complete",
      "verification": "cargo tarpaulin --out json",
      "threshold": ">=80%",
      "current_value": "87.5%",
      "last_verified": "2025-10-13T10:10:00Z"
    },
    "cleanroom-performance-benchmark": {
      "id": "cleanroom-performance-benchmark",
      "category": "performance",
      "status": "complete",
      "verification": "cleanroom bench --output json",
      "threshold": "<100ms p95",
      "current_value": "85ms p95",
      "last_verified": "2025-10-13T10:15:00Z"
    }
  }
}
```

**Production readiness commands:**

```bash
# Check overall readiness
ggen lifecycle readiness

# Output:
# Production Readiness: 4/4 requirements met
# ✅ cleanroom-unit-tests (testing)
# ✅ cleanroom-integration-tests (testing)
# ✅ cleanroom-test-coverage (quality) - 87.5%
# ✅ cleanroom-performance-benchmark (performance) - 85ms p95

# Update requirement status
ggen lifecycle readiness-update cleanroom-unit-tests complete

# Validate for deployment
ggen lifecycle validate --env production

# Output:
# Validation Status: PASSED
# ✅ All tests passing
# ✅ Coverage meets threshold (87.5% >= 80%)
# ✅ Performance meets SLA (85ms < 100ms)
# ✅ No security vulnerabilities
# ✅ Ready for production deployment
```

---

## 4. Automation Workflows

### 4.1 CI/CD Pipeline Integration

**GitHub Actions workflow (`.github/workflows/cleanroom-ci.yml`):**

```yaml
name: Cleanroom CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

env:
  GGEN_REGISTRY_URL: "https://seanchatmangpt.github.io/ggen/registry/"
  RUST_BACKTRACE: 1

jobs:
  cleanroom-tests:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          profile: minimal
          toolchain: stable

      - name: Install ggen CLI
        run: cargo install --path cli

      - name: Install cleanroom gpack
        run: ggen market add io.ggen.cleanroom.test-environments

      - name: Initialize project
        run: ggen lifecycle run init

      - name: Run unit tests
        run: ggen lifecycle run test:unit

      - name: Run cleanroom integration tests
        run: ggen lifecycle run test:cleanroom

      - name: Generate coverage report
        run: ggen lifecycle run validate:coverage

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v4
        with:
          files: ./coverage/cobertura.xml

      - name: Run performance benchmarks
        run: ggen lifecycle run validate:performance

      - name: Check production readiness
        run: ggen lifecycle readiness

      - name: Store test results
        uses: actions/upload-artifact@v4
        with:
          name: cleanroom-test-results
          path: |
            target/test-results/
            coverage/
            benchmarks/

  deploy-staging:
    needs: cleanroom-tests
    if: github.ref == 'refs/heads/develop'
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Validate for staging
        run: ggen lifecycle validate --env staging

      - name: Deploy to staging
        run: ggen lifecycle run deploy --env staging

  deploy-production:
    needs: cleanroom-tests
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Validate for production
        run: ggen lifecycle validate --env production

      - name: Deploy to production
        run: ggen lifecycle run deploy --env production
```

### 4.2 Test-Driven Development Workflow

```bash
# 1. RED: Write failing test
ggen template generate rust-web-service.tmpl \
    --var project_name=user-service \
    --var port=8080

# 2. Run test (should fail)
ggen lifecycle run test:cleanroom
# Output: FAILED - 1 test failed

# 3. GREEN: Implement feature
vim src/lib.rs

# 4. Run test (should pass)
ggen lifecycle run test:cleanroom
# Output: PASSED - 1 test passed

# 5. REFACTOR: Improve design
vim src/lib.rs

# 6. Verify tests still pass
ggen lifecycle run test
# Output: PASSED - All tests passed

# 7. Check coverage
ggen lifecycle run validate:coverage
# Output: Coverage: 87.5% (meets 80% threshold)
```

### 4.3 Local Development Workflow

```bash
# Daily development cycle
alias gdev="ggen lifecycle run dev"
alias gtest="ggen lifecycle run test"
alias gbuild="ggen lifecycle run build"

# Start development with auto-reload
gdev

# Before committing
gtest && gbuild

# Validate locally before pushing
ggen lifecycle validate --env development

# Pre-deployment check
ggen lifecycle readiness
```

### 4.4 Multi-Environment Testing

```bash
# Test in development environment
ggen lifecycle run test --env development

# Test in staging-like environment
ggen lifecycle run test --env staging

# Production-like validation
ggen lifecycle validate --env production

# Environment comparison
ggen lifecycle run benchmark --env development > dev-bench.json
ggen lifecycle run benchmark --env staging > staging-bench.json
diff dev-bench.json staging-bench.json
```

---

## 5. API Specifications

### 5.1 Cleanroom CLI JSON Output

**Standard output format for machine consumption:**

```json
{
  "command": "cleanroom test run",
  "version": "0.1.0",
  "timestamp": "2025-10-13T10:30:00Z",
  "environment": {
    "id": "env-abc-123",
    "name": "test-env",
    "created_at": "2025-10-13T10:29:00Z"
  },
  "tests": [
    {
      "name": "test_web_service_integration",
      "file": "tests/cleanroom_web_test.rs",
      "status": "passed",
      "duration_ms": 1250,
      "assertions": 5,
      "containers": [
        {
          "type": "postgres",
          "version": "16",
          "port": 5432
        }
      ]
    },
    {
      "name": "test_database_migrations",
      "file": "tests/cleanroom_db_test.rs",
      "status": "passed",
      "duration_ms": 850,
      "assertions": 3,
      "containers": [
        {
          "type": "postgres",
          "version": "16",
          "port": 5433
        }
      ]
    }
  ],
  "summary": {
    "total": 2,
    "passed": 2,
    "failed": 0,
    "skipped": 0,
    "duration_ms": 2100,
    "coverage_percent": 87.5
  },
  "metrics": {
    "cpu_usage_percent": 23.4,
    "memory_mb": 512,
    "disk_io_mb": 125
  }
}
```

### 5.2 Lifecycle Integration API

**Rust API for custom lifecycle integration:**

```rust
use ggen_cleanroom::{CleanroomEnv, CleanroomConfig, TestResult};
use anyhow::Result;

/// Run cleanroom tests from lifecycle phase
pub async fn run_cleanroom_tests(
    config: &CleanroomConfig,
    test_files: Vec<PathBuf>,
) -> Result<TestResult> {
    let mut env = CleanroomEnv::new(config.clone())?;

    for test_file in test_files {
        env.run_test_file(&test_file).await?;
    }

    let results = env.collect_results()?;

    // Update production readiness state
    if results.all_passed() && results.coverage >= 80.0 {
        update_production_readiness("cleanroom-tests", "complete")?;
    }

    Ok(results)
}

/// Update production readiness tracking
fn update_production_readiness(
    requirement_id: &str,
    status: &str,
) -> Result<()> {
    let state_path = Path::new(".ggen/production_readiness.json");
    let mut state: ProductionReadiness = load_state(state_path)?;

    state.requirements.insert(
        requirement_id.to_string(),
        Requirement {
            id: requirement_id.to_string(),
            status: status.to_string(),
            last_verified: Utc::now(),
            ..Default::default()
        },
    );

    save_state(state_path, &state)?;
    Ok(())
}
```

### 5.3 Template Variable API

**Variables available in cleanroom templates:**

```yaml
---
# Template frontmatter with variable definitions
to: "tests/cleanroom_{{ test_name }}_test.rs"
vars:
  # Project context (from ggen)
  project_name: "{{ project.name }}"
  project_type: "{{ project.type }}"
  project_version: "{{ project.version }}"

  # Cleanroom configuration
  cleanroom_timeout: 60
  enable_containers: true
  enable_benchmarking: false

  # Test configuration
  test_name: "integration"
  test_type: "integration"  # unit, integration, e2e

  # Container configuration
  containers:
    - type: "postgres"
      version: "16"
      port: 5432
    - type: "redis"
      version: "7"
      port: 6379

  # Environment variables
  env_vars:
    RUST_LOG: "debug"
    DATABASE_URL: "postgres://postgres@localhost:5432/test"
    REDIS_URL: "redis://localhost:6379"
---
```

### 5.4 Hook System API

**Hooks for lifecycle integration:**

```rust
use ggen_lifecycle::{Hook, HookContext, HookResult};

/// Before test hook - setup cleanroom environment
pub struct CleanroomSetupHook;

impl Hook for CleanroomSetupHook {
    fn name(&self) -> &str {
        "cleanroom-setup"
    }

    fn execute(&self, ctx: &HookContext) -> HookResult {
        // Create cleanroom environment
        let config = CleanroomConfig::from_context(ctx)?;
        let env = CleanroomEnv::new(config)?;

        // Store environment ID in context
        ctx.set("cleanroom_env_id", env.id())?;

        Ok(())
    }
}

/// After test hook - cleanup and report
pub struct CleanroomCleanupHook;

impl Hook for CleanroomCleanupHook {
    fn name(&self) -> &str {
        "cleanroom-cleanup"
    }

    fn execute(&self, ctx: &HookContext) -> HookResult {
        // Get environment ID from context
        let env_id = ctx.get("cleanroom_env_id")?;

        // Cleanup environment
        CleanroomEnv::cleanup(env_id)?;

        // Generate report
        let results = ctx.get("test_results")?;
        generate_test_report(results)?;

        Ok(())
    }
}
```

---

## 6. Workflow Diagrams

### 6.1 Complete Development Workflow

```
┌─────────────────────────────────────────────────────────────────┐
│                     GGEN + CLEANROOM WORKFLOW                   │
└─────────────────────────────────────────────────────────────────┘

┌─────────────┐
│  Developer  │
└──────┬──────┘
       │
       ▼
┌─────────────────────┐
│ ggen market search  │──▶ Search for test templates
│ "cleanroom test"    │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ ggen market add     │──▶ Install test templates
│ io.ggen.cleanroom.* │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ ggen lifecycle run  │──▶ Initialize project structure
│ init                │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ ggen template gen   │──▶ Generate test from template
│ rust-web-service    │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ Implement Feature   │──▶ Write production code
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ ggen lifecycle run  │──▶ Run cleanroom tests
│ test:cleanroom      │
└──────┬──────────────┘
       │
       ├─────▶ [PASSED] ──▶ Continue
       │
       └─────▶ [FAILED] ──▶ Fix code, retry
       │
       ▼
┌─────────────────────┐
│ ggen lifecycle run  │──▶ Validate for deployment
│ validate            │
└──────┬──────────────┘
       │
       ▼
┌─────────────────────┐
│ ggen lifecycle      │──▶ Check production readiness
│ readiness           │
└──────┬──────────────┘
       │
       ├─────▶ [READY] ──▶ Deploy
       │
       └─────▶ [NOT READY] ──▶ Address gaps
       │
       ▼
┌─────────────────────┐
│ ggen lifecycle run  │──▶ Deploy to production
│ deploy --env prod   │
└─────────────────────┘
```

### 6.2 CI/CD Pipeline Flow

```
┌────────────────────────────────────────────────────────────────┐
│                      CI/CD PIPELINE                            │
└────────────────────────────────────────────────────────────────┘

[Git Push] ──▶ [GitHub Actions Trigger]
                      │
                      ▼
              ┌───────────────┐
              │   Checkout    │
              └───────┬───────┘
                      │
                      ▼
              ┌───────────────┐
              │ Install ggen  │
              │ Install CLI   │
              └───────┬───────┘
                      │
                      ▼
              ┌────────────────────┐
              │ Install Cleanroom  │
              │ Gpack              │
              └────────┬───────────┘
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ init               │
              └────────┬───────────┘
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ test:unit          │
              └────────┬───────────┘
                       │
                       ├──▶ [FAIL] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ test:cleanroom     │
              └────────┬───────────┘
                       │
                       ├──▶ [FAIL] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ validate:coverage  │
              └────────┬───────────┘
                       │
                       ├──▶ [<80%] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ validate:perf      │
              └────────┬───────────┘
                       │
                       ├──▶ [SLOW] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle     │
              │ readiness          │
              └────────┬───────────┘
                       │
                       ├──▶ [NOT READY] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle     │
              │ validate --env prod│
              └────────┬───────────┘
                       │
                       ├──▶ [FAIL] ──▶ [Exit 1]
                       │
                       ▼
              ┌────────────────────┐
              │ ggen lifecycle run │
              │ deploy --env prod  │
              └────────┬───────────┘
                       │
                       ▼
              ┌────────────────────┐
              │   ✅ SUCCESS       │
              └────────────────────┘
```

### 6.3 Test Execution Flow

```
┌────────────────────────────────────────────────────────────────┐
│              CLEANROOM TEST EXECUTION FLOW                     │
└────────────────────────────────────────────────────────────────┘

[ggen lifecycle run test:cleanroom]
              │
              ▼
      ┌───────────────┐
      │ Load make.toml│
      └───────┬───────┘
              │
              ▼
      ┌───────────────┐
      │ Execute Hooks │
      │ before_test   │
      └───────┬───────┘
              │
              ▼
      ┌────────────────────┐
      │ Spawn Cleanroom    │
      │ Environment        │
      └────────┬───────────┘
              │
              ├──▶ Create TempDir
              ├──▶ Initialize Containers
              └──▶ Set Environment Variables
              │
              ▼
      ┌────────────────────┐
      │ Run Tests          │
      │ (cargo test)       │
      └────────┬───────────┘
              │
              ├──▶ Unit Tests
              ├──▶ Integration Tests
              └──▶ E2E Tests
              │
              ▼
      ┌────────────────────┐
      │ Collect Results    │
      └────────┬───────────┘
              │
              ├──▶ Test Status
              ├──▶ Coverage Data
              ├──▶ Performance Metrics
              └──▶ Container Logs
              │
              ▼
      ┌────────────────────┐
      │ Generate Report    │
      │ (JSON output)      │
      └────────┬───────────┘
              │
              ▼
      ┌────────────────────┐
      │ Update State       │
      │ (.ggen/state.json) │
      └────────┬───────────┘
              │
              ▼
      ┌────────────────────┐
      │ Execute Hooks      │
      │ after_test         │
      └────────┬───────────┘
              │
              ▼
      ┌────────────────────┐
      │ Cleanup Containers │
      │ Remove TempDir     │
      └────────┬───────────┘
              │
              ▼
      ┌────────────────────┐
      │   Return Results   │
      └────────────────────┘
```

---

## 7. Template Package Structure

### 7.1 Complete Gpack Layout

```
marketplace/packages/cleanroom-test-environments/
├── gpack.toml                      # Package manifest
├── package.toml                    # Marketplace metadata
├── make.toml                       # Lifecycle integration
├── README.md                       # Documentation
├── CHANGELOG.md                    # Version history
├── LICENSE                         # MIT license
│
├── templates/                      # Test templates
│   ├── rust-web-service.tmpl      # Web service test
│   ├── database-test.tmpl         # Database test
│   ├── api-integration.tmpl       # API integration
│   ├── multi-container.tmpl       # Multi-service
│   ├── benchmark-test.tmpl        # Performance test
│   └── e2e-test.tmpl              # End-to-end test
│
├── examples/                       # Usage examples
│   ├── basic-usage.rs             # Simple example
│   ├── advanced-workflow.rs       # Complex workflow
│   ├── ci-cd-integration.sh       # CI/CD script
│   └── docker-compose.yml         # Container setup
│
├── docs/                           # Documentation
│   ├── getting-started.md
│   ├── api-reference.md
│   ├── best-practices.md
│   └── troubleshooting.md
│
├── tests/                          # Gpack tests
│   ├── template_validation.rs
│   └── integration_test.rs
│
└── .github/                        # GitHub integration
    └── workflows/
        └── validate-gpack.yml     # CI for gpack
```

### 7.2 Template Frontmatter Specification

```yaml
---
# Template metadata
name: "rust-web-service-test"
description: "Cleanroom integration test for Rust web services"
version: "1.0.0"
author: "ggen-team"

# Output configuration
to: "tests/cleanroom_{{ test_name }}_test.rs"

# Required variables
vars:
  project_name: "{{ project.name }}"
  service_name: "{{ test_name }}"
  port: 8080
  test_timeout: 60

# Optional variables with defaults
optional_vars:
  db_type: "postgres"
  db_version: "16"
  enable_redis: false
  enable_minio: false

# Dependencies
requires:
  ggen: ">=1.2.0"
  cleanroom: ">=0.1.0"
  testcontainers: ">=0.22"

# Container configuration
containers:
  - type: "{{ db_type }}"
    version: "{{ db_version }}"
    port: 5432
    env:
      POSTGRES_DB: "testdb"
      POSTGRES_USER: "postgres"
      POSTGRES_PASSWORD: "postgres"

# Lifecycle integration
lifecycle:
  test: "cargo test --test {{ test_name }}"
  cleanup: "cleanroom env cleanup"

# RDF metadata (optional)
rdf:
  namespace: "http://ggen.io/cleanroom/test"
  type: "TestTemplate"

# SPARQL validation (optional)
sparql:
  validate: |
    PREFIX test: <http://ggen.io/cleanroom/test#>
    SELECT ?result WHERE {
      ?test test:status "passed" .
    }
---
```

### 7.3 Multi-Template Packages

**Package with multiple related templates:**

```
marketplace/packages/cleanroom-microservices/
├── gpack.toml
├── templates/
│   ├── service-a/
│   │   ├── api-test.tmpl
│   │   ├── db-test.tmpl
│   │   └── integration-test.tmpl
│   │
│   ├── service-b/
│   │   ├── api-test.tmpl
│   │   └── cache-test.tmpl
│   │
│   └── orchestration/
│       ├── multi-service-test.tmpl
│       └── e2e-test.tmpl
│
└── examples/
    └── microservices-setup.rs
```

**Usage:**

```bash
# Install microservices test suite
ggen market add io.ggen.cleanroom.microservices

# Generate tests for service-a
ggen template generate service-a/api-test.tmpl \
    --var service_name=user-service

# Generate orchestration tests
ggen template generate orchestration/multi-service-test.tmpl \
    --var services="user-service,product-service,order-service"
```

---

## 8. Production Readiness Gates

### 8.1 Readiness Requirements

**Cleanroom-specific production requirements:**

```json
{
  "requirements": {
    "cleanroom-unit-tests": {
      "id": "cleanroom-unit-tests",
      "name": "Unit Tests (Cleanroom)",
      "category": "testing",
      "priority": "critical",
      "status": "pending",
      "verification_command": "cargo test --lib",
      "success_criteria": "all_passed",
      "description": "All unit tests pass in isolated cleanroom"
    },
    "cleanroom-integration-tests": {
      "id": "cleanroom-integration-tests",
      "name": "Integration Tests (Cleanroom)",
      "category": "testing",
      "priority": "critical",
      "status": "pending",
      "verification_command": "cleanroom test run --output json",
      "success_criteria": "all_passed && containers_cleaned",
      "description": "All integration tests pass with proper cleanup"
    },
    "cleanroom-test-coverage": {
      "id": "cleanroom-test-coverage",
      "name": "Test Coverage",
      "category": "quality",
      "priority": "high",
      "status": "pending",
      "verification_command": "cargo tarpaulin --out json",
      "success_criteria": ">=80%",
      "threshold": 80.0,
      "description": "Code coverage meets minimum threshold"
    },
    "cleanroom-performance": {
      "id": "cleanroom-performance",
      "name": "Performance Benchmarks",
      "category": "performance",
      "priority": "high",
      "status": "pending",
      "verification_command": "cleanroom bench --output json",
      "success_criteria": "p95 < 100ms",
      "threshold": {
        "p50": 50,
        "p95": 100,
        "p99": 200
      },
      "description": "Performance meets SLA requirements"
    },
    "cleanroom-container-isolation": {
      "id": "cleanroom-container-isolation",
      "name": "Container Isolation",
      "category": "security",
      "priority": "high",
      "status": "pending",
      "verification_command": "cleanroom swarm status --output json",
      "success_criteria": "no_leaks && all_cleaned",
      "description": "Containers properly isolated and cleaned up"
    }
  }
}
```

### 8.2 Validation Commands

```bash
# Check all cleanroom requirements
ggen lifecycle readiness --category testing

# Output:
# Production Readiness (Testing): 2/3 requirements met
# ✅ cleanroom-unit-tests - COMPLETE
# ✅ cleanroom-integration-tests - COMPLETE
# ❌ cleanroom-test-coverage - PENDING (current: 72%, required: 80%)

# Update requirement manually
ggen lifecycle readiness-update cleanroom-unit-tests complete

# Validate specific requirement
ggen lifecycle readiness-verify cleanroom-test-coverage

# Validate all requirements
ggen lifecycle validate --env production

# Show blockers
ggen lifecycle readiness --show-blockers

# Output:
# Production Blockers:
# ❌ cleanroom-test-coverage (quality) - 72% < 80%
#    → Increase test coverage by 8%
```

### 8.3 Automated Verification

**Hook to automatically update readiness:**

```toml
[hooks]
after_test = ["update-test-readiness"]

[lifecycle."update-test-readiness"]
description = "Update production readiness after tests"
script = """
#!/bin/bash
set -e

# Run tests and capture results
RESULTS=$(cargo test --all -- --nocapture 2>&1 | tee test-results.txt)
EXIT_CODE=$?

# Update readiness based on results
if [ $EXIT_CODE -eq 0 ]; then
    ggen lifecycle readiness-update cleanroom-unit-tests complete
    ggen lifecycle readiness-update cleanroom-integration-tests complete
else
    ggen lifecycle readiness-update cleanroom-unit-tests failed
    ggen lifecycle readiness-update cleanroom-integration-tests failed
fi

# Check coverage
COVERAGE=$(cargo tarpaulin --out json | jq -r '.files[].covered_percent' | awk '{sum+=$1; count++} END {print sum/count}')

if (( $(echo "$COVERAGE >= 80" | bc -l) )); then
    ggen lifecycle readiness-update cleanroom-test-coverage complete
else
    ggen lifecycle readiness-update cleanroom-test-coverage "blocked:coverage=${COVERAGE}%"
fi

exit $EXIT_CODE
"""
```

---

## 9. Implementation Roadmap

### Phase 1: Foundation (Week 1-2)

**Goals:**
- Create initial cleanroom gpack structure
- Implement basic marketplace integration
- Create first test templates

**Tasks:**
1. Create `marketplace/packages/cleanroom-test-environments/` directory
2. Write `gpack.toml` and `package.toml`
3. Create 3 core templates:
   - `rust-web-service.tmpl`
   - `database-test.tmpl`
   - `api-integration.tmpl`
4. Update `registry/index.json`
5. Test local installation: `ggen market add io.ggen.cleanroom.test-environments`

**Deliverables:**
- Working gpack installable from local registry
- 3 functional test templates
- Basic documentation

### Phase 2: Lifecycle Integration (Week 3-4)

**Goals:**
- Integrate cleanroom with lifecycle system
- Implement test/validate phase hooks
- Create production readiness gates

**Tasks:**
1. Add cleanroom lifecycle phases to `make.toml` examples
2. Implement `before_test` and `after_test` hooks
3. Create production readiness requirements
4. Add validation commands
5. Create example `make.toml` with full integration

**Deliverables:**
- Full lifecycle integration
- Production readiness tracking
- Automated validation gates

### Phase 3: Automation (Week 5-6)

**Goals:**
- Create CI/CD integration templates
- Implement automated workflows
- Add monitoring and reporting

**Tasks:**
1. Create GitHub Actions workflow template
2. Implement test result reporting
3. Add coverage tracking
4. Create performance benchmarking workflow
5. Add Slack/email notifications

**Deliverables:**
- GitHub Actions templates
- Automated CI/CD workflows
- Comprehensive reporting

### Phase 4: Advanced Features (Week 7-8)

**Goals:**
- Multi-container orchestration
- Advanced testing patterns
- Performance optimization

**Tasks:**
1. Create multi-container test templates
2. Implement E2E testing workflows
3. Add performance benchmarking
4. Create advanced examples
5. Write comprehensive documentation

**Deliverables:**
- Advanced test templates
- Complete documentation
- Tutorial and best practices guide

### Phase 5: Polish and Release (Week 9-10)

**Goals:**
- Documentation and examples
- Testing and validation
- Public release

**Tasks:**
1. Write complete documentation
2. Create tutorial videos/guides
3. Add more examples
4. Test all workflows end-to-end
5. Publish to marketplace

**Deliverables:**
- Complete documentation
- Public marketplace release
- Announcement and promotion

---

## 10. Best Practices and Guidelines

### 10.1 Template Design

**DO:**
- ✅ Use descriptive variable names
- ✅ Provide sensible defaults
- ✅ Include comprehensive comments
- ✅ Add error handling
- ✅ Document expected behavior

**DON'T:**
- ❌ Hardcode values
- ❌ Use `.unwrap()` or `.expect()`
- ❌ Ignore cleanup
- ❌ Skip documentation
- ❌ Assume container availability

### 10.2 Lifecycle Integration

**DO:**
- ✅ Use hooks for quality gates
- ✅ Track outputs in state
- ✅ Enable caching for expensive operations
- ✅ Provide descriptive phase names
- ✅ Use environment-specific configuration

**DON'T:**
- ❌ Skip validation phases
- ❌ Ignore test failures
- ❌ Deploy without readiness check
- ❌ Mix environments
- ❌ Hardcode credentials

### 10.3 Testing Patterns

**DO:**
- ✅ Test in isolation (cleanroom)
- ✅ Use testcontainers for dependencies
- ✅ Clean up resources
- ✅ Measure coverage
- ✅ Benchmark performance

**DON'T:**
- ❌ Share state between tests
- ❌ Rely on external services
- ❌ Leave containers running
- ❌ Skip cleanup on failure
- ❌ Ignore slow tests

### 10.4 CI/CD Integration

**DO:**
- ✅ Run tests on every commit
- ✅ Block deployment on test failure
- ✅ Track metrics over time
- ✅ Notify on failures
- ✅ Cache dependencies

**DON'T:**
- ❌ Deploy without validation
- ❌ Skip integration tests
- ❌ Ignore coverage drops
- ❌ Deploy on test failures
- ❌ Skip performance checks

---

## 11. Troubleshooting

### Common Issues

**Issue:** Templates not found after installation

```bash
# Solution: Check registry URL
echo $GGEN_REGISTRY_URL

# Verify gpack installation
ggen packs | grep cleanroom

# Reinstall if needed
ggen market remove io.ggen.cleanroom.test-environments
ggen market add io.ggen.cleanroom.test-environments
```

**Issue:** Cleanroom tests fail in CI but pass locally

```bash
# Solution: Check environment differences
ggen lifecycle run test --env ci --verbose

# Enable debug logging
RUST_LOG=debug ggen lifecycle run test

# Check container availability
docker ps -a
cleanroom swarm status
```

**Issue:** Production readiness always shows pending

```bash
# Solution: Manually verify requirements
ggen lifecycle readiness-verify cleanroom-unit-tests

# Check validation command
cargo test --lib --verbose

# Update status manually
ggen lifecycle readiness-update cleanroom-unit-tests complete
```

**Issue:** Performance benchmarks inconsistent

```bash
# Solution: Use cleanroom isolation
cleanroom bench --bench-type performance --iterations 100

# Check system resources
cleanroom swarm metrics

# Run sequential tests
ggen lifecycle run validate:performance --env production
```

---

## 12. Appendix

### A. Complete File Listings

**A.1 Minimal Gpack Structure**

```
cleanroom-test-environments/
├── gpack.toml
├── package.toml
├── templates/
│   └── rust-web-service.tmpl
└── README.md
```

**A.2 Complete Gpack Structure**

```
cleanroom-test-environments/
├── gpack.toml
├── package.toml
├── make.toml
├── README.md
├── CHANGELOG.md
├── LICENSE
├── templates/
│   ├── rust-web-service.tmpl
│   ├── database-test.tmpl
│   ├── api-integration.tmpl
│   ├── multi-container.tmpl
│   ├── benchmark-test.tmpl
│   └── e2e-test.tmpl
├── examples/
│   ├── basic-usage.rs
│   ├── advanced-workflow.rs
│   └── ci-cd-integration.sh
├── docs/
│   ├── getting-started.md
│   ├── api-reference.md
│   └── best-practices.md
└── tests/
    ├── template_validation.rs
    └── integration_test.rs
```

### B. Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `GGEN_REGISTRY_URL` | Marketplace registry URL | GitHub Pages |
| `CLEANROOM_TIMEOUT` | Test timeout in seconds | 60 |
| `CLEANROOM_PARALLEL` | Enable parallel tests | true |
| `RUST_LOG` | Logging level | info |
| `GGEN_ENV` | Environment name | development |

### C. Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Test failure |
| 3 | Validation failure |
| 4 | Production readiness blocked |
| 130 | Interrupted (Ctrl+C) |

### D. References

- [Cleanroom CLI Documentation](/Users/sac/ggen/docs/CLEANROOM_CLI_COMPLETE.md)
- [Lifecycle System Design](/Users/sac/ggen/docs/LIFECYCLE_SYSTEM_DESIGN.md)
- [Marketplace Guide](/Users/sac/ggen/docs/marketplace.md)
- [Production Readiness](/Users/sac/ggen/docs/LIFECYCLE_PRODUCTION_GAPS_ULTRATHINK.md)

---

## Document Revision History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2025-10-13 | Architecture Team | Initial design specification |

---

**END OF ARCHITECTURE DOCUMENT**

This architecture provides a comprehensive integration between cleanroom's hermetic testing framework and ggen's marketplace and lifecycle systems, enabling developers to discover, install, and execute production-ready test environments through a unified CLI and workflow automation system.
