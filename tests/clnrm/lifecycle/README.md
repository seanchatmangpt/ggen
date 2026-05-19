# Lifecycle CLNRM Tests with OTEL Validation

This directory contains comprehensive lifecycle test suites converted from Rust tests to `.clnrm.toml` format with full OpenTelemetry (OTEL) validation.

## Test Files Overview

### 1. `init.clnrm.toml` - Initialization Tests (327 lines)
**Tests Converted: 8 scenarios**

- Basic lifecycle initialization
- Multi-step initialization with multiple commands
- Template generation during init phase
- Init with state persistence
- Init failure handling and error recovery
- Rust project initialization
- Init with marketplace package integration
- Concurrent initialization isolation

**Key Validations:**
- `mcpp.lifecycle.init` span execution
- `mcpp.lifecycle.command.execute` for multi-step commands
- `mcpp.template.generate` for template operations
- `mcpp.lifecycle.state.save` for state persistence
- Error spans for failure scenarios
- Concurrent execution isolation

### 2. `phases.clnrm.toml` - Phase Transition Tests (469 lines)
**Tests Converted: 10 scenarios**

- Full lifecycle pipeline with strict ordering (init → setup → build → test → deploy)
- Phase with multiple commands
- Pipeline failure stops execution (validated with span absence)
- Before and after hooks execution order
- Circular hook detection
- Phase caching and cache hits/misses
- State persistence across phases
- Parallel workspace builds
- Error handling with detailed context
- Cache invalidation on command change

**Key Validations:**
- `must_precede` / `must_follow` for phase ordering
- `mcpp.lifecycle.hooks.orchestrate` for hook management
- `mcpp.cache.key.generate`, `mcpp.cache.hit`, `mcpp.cache.miss`
- `mcpp.lifecycle.state.load` and `.save` for persistence
- `mcpp.workspace.build` for parallel execution
- `should_not_exist` for phases that must not run

### 3. `deploy.clnrm.toml` - Deployment Tests (550 lines)
**Tests Converted: 10 scenarios**

- Staging deployment with validation
- Production deployment with comprehensive pre/post checks
- Deployment validation failure prevents deploy
- Blue-green deployment strategy
- Canary deployment with gradual rollout
- Multi-region deployment (us-east, eu-west, ap-south)
- Deployment with artifact publishing
- Deployment with database migrations
- Zero-downtime deployment
- Deployment verification with smoke tests

**Key Validations:**
- `mcpp.lifecycle.deploy-*` for environment-specific deploys
- `mcpp.deploy.build`, `.healthcheck`, `.verify` spans
- `mcpp.deploy.prevented` when validation fails
- `mcpp.artifacts.generate` and `.publish` for artifacts
- `mcpp.deploy.monitor.canary` for canary monitoring
- `mcpp.test.execute` for smoke tests

### 4. `rollback.clnrm.toml` - Rollback & Recovery Tests (521 lines)
**Tests Converted: 10 scenarios**

- Basic rollback after failed deployment
- Automatic rollback on validation failure
- State recovery after interruption
- Rollback with database restore
- Partial rollback with component isolation
- Rollback with traffic shifting
- Rollback verification and health checks
- Cache invalidation on failure
- Snapshot-based rollback
- Rollback with notification

**Key Validations:**
- `mcpp.lifecycle.rollback` execution
- `mcpp.rollback.restore` for restoration operations
- `mcpp.rollback.trigger` for automatic triggers
- `mcpp.lifecycle.restore-database` for DB rollback
- `mcpp.traffic.monitor` for traffic management
- `mcpp.rollback.healthcheck` for verification
- `mcpp.notification.send` for alerting

### 5. `readiness.clnrm.toml` - Production Readiness Tests (552 lines)
**Tests Converted: 10 scenarios**

- Basic readiness tracking
- Readiness requirement lifecycle (placeholder → in_progress → complete)
- Production readiness validation
- Readiness report generation with statistics
- Critical requirement blocking deployment
- Readiness dependency checking (DAG validation)
- Readiness with effort estimation
- Readiness file scanning for `.unwrap()` detection
- Readiness categories and prioritization
- Full production readiness workflow

**Key Validations:**
- `mcpp.lifecycle.readiness.check` for readiness evaluation
- `mcpp.readiness.requirement.evaluate` for requirement checking
- `mcpp.readiness.requirement.update` for status transitions
- `mcpp.validation.file_scan` for code quality checks
- `mcpp.validation.pattern_match` for anti-pattern detection
- `mcpp.deploy.blocked` when critical requirements missing
- `mcpp.readiness.dependency.validate` for dependency DAG

## Total Test Coverage

**Original Rust Tests:**
- `lifecycle_tests.rs`: 22 test functions (1,002 lines)
- `lifecycle_clnrm_tests.rs`: 11 test functions (768 lines)
- **Total: 33 tests, 1,770 lines**

**Converted CLNRM Tests:**
- **48 test scenarios** across 5 files
- **2,419 lines** of CLNRM configuration
- **Every test includes OTEL span validation**

## OTEL Validation Features

### 1. Span Validation
```toml
[[scenario.expect.span]]
name = "mcpp.lifecycle.init"
attributes = { "lifecycle.phase" = "init", "project.name" = "test-project" }
min_duration_ms = 1
```

### 2. Temporal Ordering
```toml
[scenario.expect.temporal]
strict_ordering = [
  "mcpp.lifecycle.init",
  "mcpp.lifecycle.setup",
  "mcpp.lifecycle.build",
  "mcpp.lifecycle.test",
  "mcpp.lifecycle.deploy"
]
```

### 3. Window Containment
```toml
[scenario.expect.window]
parent = "mcpp.lifecycle.deploy-production"
must_contain = ["mcpp.deploy.build", "mcpp.deploy.healthcheck", "mcpp.deploy.verify"]
min_child_spans = 4
```

### 4. Graph Validation
```toml
[scenario.expect.graph]
must_include = [
  ["mcpp.lifecycle.init", "mcpp.lifecycle.build"],
  ["mcpp.lifecycle.build", "mcpp.lifecycle.test"]
]
acyclic = true
dag_validated = true
```

### 5. Error Validation
```toml
[[scenario.expect.span]]
name = "mcpp.lifecycle.deploy"
expect_error = true
attributes = { "error.type" = "validation_failed" }

[[scenario.expect.span]]
name = "mcpp.lifecycle.test"
should_not_exist = true  # Must not run after error
```

### 6. Attestation & Determinism
```toml
[scenario.expect.attestation]
sha256 = true
hermetic = true
reproducible = true
files_created = [".mcpp/state.json"]

[expect.determinism]
require_same_traces_on_retry = true
retry_count = 2
```

## Key OTEL Spans Defined

### Lifecycle Phase Spans
- `mcpp.lifecycle.init`
- `mcpp.lifecycle.setup`
- `mcpp.lifecycle.build`
- `mcpp.lifecycle.test`
- `mcpp.lifecycle.deploy`
- `mcpp.lifecycle.rollback`
- `mcpp.lifecycle.validate`
- `mcpp.lifecycle.readiness`

### Command Execution Spans
- `mcpp.lifecycle.phase.execute`
- `mcpp.lifecycle.command.execute`

### State Management Spans
- `mcpp.lifecycle.state.load`
- `mcpp.lifecycle.state.save`
- `mcpp.lifecycle.state.persist`

### Deployment Spans
- `mcpp.deploy.build`
- `mcpp.deploy.push`
- `mcpp.deploy.verify`
- `mcpp.deploy.healthcheck`
- `mcpp.deploy.monitor.canary`
- `mcpp.deploy.prevented`

### Rollback Spans
- `mcpp.rollback.restore`
- `mcpp.rollback.trigger`
- `mcpp.rollback.healthcheck`

### Readiness Spans
- `mcpp.readiness.requirement.evaluate`
- `mcpp.readiness.requirement.update`
- `mcpp.readiness.requirement.validate`
- `mcpp.readiness.report.generate`
- `mcpp.readiness.validation.result`

### Cache Spans
- `mcpp.cache.key.generate`
- `mcpp.cache.hit`
- `mcpp.cache.miss`
- `mcpp.cache.invalidate`

### Error Handling Spans
- `mcpp.lifecycle.error.handle`
- `mcpp.lifecycle.error.capture`

### Marketplace Spans
- `mcpp.marketplace.search`
- `mcpp.marketplace.add`

### Artifact Spans
- `mcpp.artifacts.generate`
- `mcpp.artifacts.publish`

### Template Spans
- `mcpp.template.generate`

### Validation Spans
- `mcpp.validation.code_scan`
- `mcpp.validation.pattern_match`
- `mcpp.validation.file_scan`

## Running Tests

### Run All Lifecycle Tests
```bash
clnrm run tests/clnrm/lifecycle/*.clnrm.toml
```

### Run Specific Test Suite
```bash
clnrm run tests/clnrm/lifecycle/init.clnrm.toml
clnrm run tests/clnrm/lifecycle/phases.clnrm.toml
clnrm run tests/clnrm/lifecycle/deploy.clnrm.toml
clnrm run tests/clnrm/lifecycle/rollback.clnrm.toml
clnrm run tests/clnrm/lifecycle/readiness.clnrm.toml
```

### Run Specific Scenario
```bash
clnrm run tests/clnrm/lifecycle/init.clnrm.toml --scenario basic_lifecycle_init
clnrm run tests/clnrm/lifecycle/phases.clnrm.toml --scenario full_lifecycle_pipeline_ordered
```

### Generate Reports
```bash
clnrm run tests/clnrm/lifecycle/*.clnrm.toml --report-format json --output /tmp/lifecycle-report.json
```

## Test Environment Setup

### Prerequisites
1. **OTEL Collector**: Running at `http://localhost:4318`
2. **Docker/Container Runtime**: For isolated test execution
3. **Ggen CLI**: Installed and in PATH
4. **CLNRM**: Latest version with OTEL support

### Environment Variables
```bash
export OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:4318"
export RUST_LOG="info"
export GGEN_HOME="/tmp/mcpp-test/.mcpp"
```

### Start OTEL Collector
```bash
docker run -d --name otel-collector \
  -p 4317:4317 -p 4318:4318 \
  otel/opentelemetry-collector:latest
```

## Validation Coverage

### ✅ Temporal Ordering
- All phase transitions validated with `must_precede` / `must_follow`
- Strict ordering enforced for critical workflows
- DAG validation for dependency graphs

### ✅ Hermeticity
- Container isolation for all tests
- Network and filesystem isolation
- Deterministic execution with fixed seeds

### ✅ Error Handling
- All error scenarios have `expect_error = true`
- Error spans captured and validated
- Rollback triggered on failures

### ✅ State Persistence
- State file creation validated
- State loading/saving span verification
- Cross-phase state continuity

### ✅ Deployment Strategies
- Blue-green deployment validated
- Canary deployment with monitoring
- Zero-downtime deployment flows
- Multi-region deployment

### ✅ Rollback & Recovery
- Automatic rollback triggers
- State restoration validation
- Database rollback scenarios
- Partial rollback isolation

### ✅ Production Readiness
- Critical requirement tracking
- Dependency graph validation
- Code quality scanning
- Deployment blocking enforcement

## Performance Expectations

### Initialization Tests
- Max duration: 5000ms
- Max memory: 512MB

### Phase Transition Tests
- Max duration: 10000ms
- Max memory: 512MB

### Deployment Tests
- Max duration: 15000ms
- Max memory: 1024MB

### Rollback Tests
- Max duration: 10000ms
- Max memory: 512MB

### Readiness Tests
- Max duration: 8000ms
- Max memory: 512MB

## Migration Notes

### What Changed from Rust Tests
1. **Test Format**: Rust `#[test]` functions → CLNRM `[[scenario]]` blocks
2. **Assertions**: Rust `assert!` → OTEL `expect.span` validations
3. **Fixtures**: Rust `LifecycleTestFixture` → CLNRM container isolation
4. **Execution**: Rust `tokio::test` → CLNRM hermetic containers
5. **Validation**: Rust assertions → OTEL temporal ordering + graph validation

### What Was Preserved
1. **Test Logic**: All 33 original test scenarios preserved
2. **Coverage**: 100% of original test functionality
3. **Validation Depth**: Enhanced with OTEL observability
4. **Reproducibility**: Improved with hermetic containers

### What Was Enhanced
1. **Observability**: Full OTEL trace validation
2. **Isolation**: Complete hermetic execution
3. **Determinism**: Fixed seeds and timestamps
4. **Validation**: Temporal ordering + graph validation
5. **Reporting**: JSON reports with metrics

## Next Steps

### Delete Original Rust Tests
Once CLNRM tests are validated:
```bash
rm mcpp-core/tests/integration/lifecycle_tests.rs
rm mcpp-core/tests/integration/lifecycle_clnrm_tests.rs
```

### Update CI/CD
Add CLNRM lifecycle tests to CI:
```yaml
- name: Run Lifecycle CLNRM Tests
  run: |
    clnrm run tests/clnrm/lifecycle/*.clnrm.toml \
      --report-format json \
      --output test-results/lifecycle-report.json
```

### Add to Documentation
Update test documentation to reference CLNRM tests instead of Rust tests.

## References

- CLNRM Documentation: `docs/cleanroom/README.md`
- OTEL Span Specification: `docs/cleanroom/OTEL_VALIDATION.md`
- Lifecycle System: `docs/lifecycle.md`
- Original Tests: `mcpp-core/tests/integration/lifecycle_*.rs` (to be removed)
