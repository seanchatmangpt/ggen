# Lifecycle Testing Guide

Comprehensive guide for testing ggen's lifecycle management system.

## Overview

The lifecycle testing suite validates:
- **Phase transitions**: Execution flow from init → setup → build → test → deploy
- **Production readiness**: Validation and tracking of production requirements
- **Deployment workflows**: Staging and production deployment scenarios
- **Rollback scenarios**: Recovery from failed deployments
- **Integration**: Lifecycle + marketplace workflows
- **Isolation**: Clnrm container-based testing

## Test Structure

```
tests/
├── integration/
│   ├── lifecycle_tests.rs         # Core lifecycle functionality
│   └── lifecycle_clnrm_tests.rs   # Container-isolated tests
├── lifecycle_bdd.rs               # Behavior-driven tests (London School TDD)
└── lifecycle_edge_cases.rs        # Edge cases and error scenarios
```

## Running Tests

### All Lifecycle Tests
```bash
# Run all lifecycle tests
cargo test lifecycle

# Run with detailed output
cargo test lifecycle -- --nocapture

# Run specific test file
cargo test --test lifecycle_tests
cargo test --test lifecycle_clnrm_tests
```

### Integration Tests Only
```bash
# Run lifecycle integration tests
cargo test --test marketplace_tests_main integration::lifecycle_tests

# Run clnrm container tests
cargo test --test marketplace_tests_main integration::lifecycle_clnrm_tests
```

### BDD Tests
```bash
# Run behavior-driven development tests
cargo test --test lifecycle_bdd
```

### Edge Case Tests
```bash
# Run edge case and error handling tests
cargo test --test lifecycle_edge_cases
```

## Test Categories

### 1. Phase Transition Tests

Tests that verify the execution flow through lifecycle phases.

**Key Tests:**
- `test_basic_phase_execution`: Single phase execution
- `test_full_lifecycle_pipeline`: Complete init → deploy pipeline
- `test_phase_with_multiple_commands`: Multi-command phases
- `test_phase_failure_stops_pipeline`: Error propagation

**Example:**
```rust
#[test]
fn test_full_lifecycle_pipeline() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "full-lifecycle"

[lifecycle.init]
command = "echo 'Phase: init'"

[lifecycle.setup]
command = "echo 'Phase: setup'"

[lifecycle.build]
command = "echo 'Phase: build'"

[lifecycle.test]
command = "echo 'Phase: test'"

[lifecycle.deploy]
command = "echo 'Phase: deploy'"
"#)?;

    let ctx = fixture.create_context()?;

    run_pipeline(&ctx, &vec![
        "init".to_string(),
        "setup".to_string(),
        "build".to_string(),
        "test".to_string(),
        "deploy".to_string(),
    ])?;

    // Verify all phases executed
    let state = fixture.load_state()?;
    assert_eq!(state.phase_history.len(), 5);

    Ok(())
}
```

### 2. Production Readiness Tests

Tests for production readiness tracking and validation.

**Key Tests:**
- `test_readiness_tracker_initialization`: Tracker setup
- `test_readiness_requirement_lifecycle`: Requirement management
- `test_readiness_report_generation`: Report generation
- `test_readiness_validation_with_validator`: Automated validation

**Example:**
```rust
#[test]
fn test_readiness_requirement_lifecycle() -> Result<()> {
    let mut tracker = ReadinessTracker::new("test-project".to_string());

    // Add critical requirement
    let req = ReadinessRequirement {
        id: "auth-basic".to_string(),
        name: "Basic Authentication".to_string(),
        description: "Implement JWT-based authentication".to_string(),
        category: ReadinessCategory::Critical,
        status: ReadinessStatus::Placeholder,
        components: vec!["src/auth.rs".to_string()],
        dependencies: vec![],
        effort_hours: Some(8),
        priority: 10,
        last_assessed: chrono::Utc::now(),
        notes: None,
    };

    tracker.add_requirement(req);

    // Update status
    tracker.update_status("auth-basic", ReadinessStatus::Complete)?;

    // Generate report
    let report = tracker.generate_report();
    assert_eq!(report.completed, 1);

    Ok(())
}
```

### 3. Deployment Validation Tests

Tests for staging and production deployment workflows.

**Key Tests:**
- `test_deployment_to_staging`: Staging deployment flow
- `test_deployment_to_production`: Production deployment with checks
- `test_deployment_validation_failure_prevents_deploy`: Pre-deployment validation

**Example:**
```rust
#[test]
fn test_deployment_to_production() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "production-deployment"

[lifecycle.pre-deploy-checks]
commands = [
    "echo 'Checking production readiness'",
    "echo 'Validating environment variables'",
    "echo 'Running security scan'"
]

[lifecycle.deploy-production]
commands = [
    "echo 'Building production artifacts'",
    "echo 'Deploying to production'",
    "echo 'Running health checks'"
]

[lifecycle.post-deploy-monitoring]
command = "echo 'Setting up monitoring'"

[hooks]
before_deploy-production = ["pre-deploy-checks"]
after_deploy-production = ["post-deploy-monitoring"]
"#)?;

    let ctx = fixture.create_context()?;
    run_phase(&ctx, "deploy-production")?;

    // Verify all phases executed
    fixture.assert_phase_executed("pre-deploy-checks")?;
    fixture.assert_phase_executed("deploy-production")?;
    fixture.assert_phase_executed("post-deploy-monitoring")?;

    Ok(())
}
```

### 4. Rollback and Recovery Tests

Tests for handling deployment failures and rollback scenarios.

**Key Tests:**
- `test_rollback_after_failed_deployment`: Rollback execution
- `test_state_recovery_after_interruption`: State persistence
- `test_cache_invalidation_on_failure`: Cache management

**Example:**
```rust
#[test]
fn test_rollback_after_failed_deployment() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "rollback-test"

[lifecycle.deploy]
command = "exit 1"

[lifecycle.rollback]
commands = [
    "echo 'Restoring previous version'",
    "echo 'Clearing failed artifacts'",
    "echo 'Verifying rollback successful'"
]
"#)?;

    let ctx = fixture.create_context()?;

    // Deploy fails
    let deploy_result = run_phase(&ctx, "deploy");
    assert!(deploy_result.is_err());

    // Execute rollback
    run_phase(&ctx, "rollback")?;
    fixture.assert_phase_executed("rollback")?;

    Ok(())
}
```

### 5. Lifecycle + Marketplace Integration

Tests for integration between lifecycle and marketplace systems.

**Key Tests:**
- `test_marketplace_package_installation_in_setup`: Package installation
- `test_template_generation_in_init_phase`: Template generation
- `test_end_to_end_marketplace_lifecycle_flow`: Complete E2E workflow

**Example:**
```rust
#[test]
fn test_end_to_end_marketplace_lifecycle_flow() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?;

    fixture.write_make_toml(r#"
[project]
name = "e2e-marketplace-lifecycle"

[lifecycle.init]
command = "echo 'ggen lifecycle run init'"

[lifecycle.setup]
commands = [
    "echo 'ggen market search rust-web'",
    "echo 'ggen market add rust-axum-service'",
    "echo 'ggen template generate rust-axum-service:main.tmpl'"
]

[lifecycle.build]
command = "echo 'cargo build --release'"

[lifecycle.test]
command = "echo 'cargo test'"

[lifecycle.validate]
command = "echo 'ggen lifecycle readiness'"

[lifecycle.deploy]
commands = [
    "echo 'ggen lifecycle validate --env production'",
    "echo 'ggen lifecycle run deploy --env production'"
]
"#)?;

    let ctx = fixture.create_context()?;

    // Run complete e2e flow
    run_pipeline(&ctx, &vec![
        "init".to_string(),
        "setup".to_string(),
        "build".to_string(),
        "test".to_string(),
        "validate".to_string(),
        "deploy".to_string(),
    ])?;

    let state = fixture.load_state()?;
    assert_eq!(state.phase_history.len(), 6);

    Ok(())
}
```

### 6. Clnrm Container Isolation Tests

Tests using cleanroom (clnrm) containers for complete isolation.

**Key Tests:**
- `test_clnrm_basic_phase_execution`: Basic container execution
- `test_clnrm_environment_isolation`: Environment isolation
- `test_clnrm_reproducible_builds`: Build reproducibility
- `test_clnrm_staging_environment`: Staging environment simulation
- `test_clnrm_production_environment`: Production environment simulation
- `test_clnrm_parallel_workspaces`: Monorepo testing

**Example:**
```rust
#[test]
fn test_clnrm_production_environment() -> Result<()> {
    skip_if_no_clnrm!();

    let production = ClnrmContainer::new("production-env")?;

    production.write_file("make.toml", r#"
[project]
name = "production-deployment"

[lifecycle.pre-production-checks]
commands = [
    "echo 'Running production readiness checks'",
    "echo 'Validating security requirements'",
    "echo 'Checking resource availability'"
]

[lifecycle.deploy-production]
commands = [
    "echo 'ENVIRONMENT=production'",
    "echo 'MONITORING=enabled'",
    "echo 'Deploying to production cluster'"
]

[hooks]
before_deploy-production = ["pre-production-checks"]
"#)?;

    assert!(production.project_path.exists());
    Ok(())
}
```

## Test Utilities

### LifecycleTestFixture

Provides isolated test environment for lifecycle testing.

```rust
struct LifecycleTestFixture {
    temp_dir: TempDir,
    project_root: PathBuf,
    state_path: PathBuf,
}

impl LifecycleTestFixture {
    fn new() -> Result<Self>;
    fn write_make_toml(&self, content: &str) -> Result<()>;
    fn write_file(&self, relative_path: &str, content: &str) -> Result<()>;
    fn create_context(&self) -> Result<Context>;
    fn load_state(&self) -> Result<LifecycleState>;
    fn assert_phase_executed(&self, phase_name: &str) -> Result<()>;
    fn create_rust_project(&self) -> Result<()>;
}
```

### ClnrmContainer

Provides containerized test environment using cleanroom.

```rust
struct ClnrmContainer {
    name: String,
    temp_dir: TempDir,
    project_path: PathBuf,
}

impl ClnrmContainer {
    fn new(name: &str) -> Result<Self>;
    fn write_file(&self, relative_path: &str, content: &str) -> Result<()>;
    fn exec(&self, cmd: &str, args: &[&str]) -> Result<std::process::Output>;
    fn run_lifecycle_phase(&self, phase: &str) -> Result<()>;
    fn load_state(&self) -> Result<LifecycleState>;
}
```

## Best Practices

### 1. Test Isolation

Each test should be completely isolated:
```rust
#[test]
fn test_isolated() -> Result<()> {
    let fixture = LifecycleTestFixture::new()?; // Fresh environment
    // ... test code
    Ok(())
} // Cleanup automatic
```

### 2. Use Descriptive Names

Test names should clearly describe what is being tested:
```rust
✅ test_deployment_validation_failure_prevents_deploy
❌ test_deploy
```

### 3. Verify State Changes

Always verify that state changes are persisted:
```rust
run_phase(&ctx, "build")?;
let state = fixture.load_state()?;
assert!(state.phase_history.iter().any(|h| h.phase == "build"));
```

### 4. Test Both Success and Failure

Test both success paths and error conditions:
```rust
// Success case
let result = run_phase(&ctx, "build");
assert!(result.is_ok());

// Failure case
let result = run_phase(&ctx, "failing-phase");
assert!(result.is_err());
```

### 5. Use Fixtures for Common Setup

Create reusable fixtures for common scenarios:
```rust
impl LifecycleTestFixture {
    fn create_rust_project(&self) -> Result<()> {
        self.write_file("Cargo.toml", CARGO_TOML_TEMPLATE)?;
        self.write_file("src/main.rs", MAIN_RS_TEMPLATE)?;
        Ok(())
    }
}
```

## Coverage Goals

- **Overall**: >80% code coverage
- **Critical paths**: 100% coverage
  - Phase execution
  - Hook execution
  - State persistence
  - Error handling
- **Integration tests**: Cover all major workflows
- **Container tests**: Verify isolation and reproducibility

## Running Coverage

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage report
cargo tarpaulin \
    --out Html \
    --output-dir coverage \
    --all-features \
    --workspace

# View report
open coverage/index.html
```

## CI/CD Integration

Tests run automatically in CI:
```yaml
- name: Run Lifecycle Tests
  run: |
    cargo test lifecycle
    cargo test --test lifecycle_tests
    cargo test --test lifecycle_clnrm_tests
```

## Troubleshooting

### Test Failures

**State file not found:**
```
Error: State file does not exist
```
Solution: Ensure `.ggen` directory is created before loading state.

**Phase not executed:**
```
Phase 'build' was not executed
```
Solution: Verify phase name matches make.toml configuration.

**Container tests skipped:**
```
⚠️  Skipping clnrm test: cleanroom not available
```
Solution: Install clnrm or ensure it's in PATH.

### Performance Issues

If tests are slow:
1. Check for unnecessary file I/O
2. Use in-memory state when possible
3. Run tests in parallel: `cargo test -- --test-threads=4`

## Examples

See:
- `tests/integration/lifecycle_tests.rs` - Complete test examples
- `tests/integration/lifecycle_clnrm_tests.rs` - Container test examples
- `tests/lifecycle_bdd.rs` - BDD test examples
- `tests/lifecycle_edge_cases.rs` - Edge case examples

## Resources

- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Integration Testing Best Practices](https://doc.rust-lang.org/book/ch11-03-test-organization.html)
- [Clnrm Documentation](../../cleanroom/README.md)
- [Lifecycle System Documentation](../lifecycle.md)
