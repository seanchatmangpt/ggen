# GGEN to CLNRM Adaptation Guide

**Purpose**: Practical guide for adapting GGEN patterns and components to the CLNRM (Cleanroom Testing Framework) project.

**Analysis Date**: 2025-10-17
**Target Project**: CLNRM v0.4.0 (Rust-based hermetic integration testing framework)

---

## Executive Summary

GGEN and CLNRM share **remarkable architectural alignment**:
- Both are Rust-based frameworks with production-grade standards
- Both emphasize hermetic isolation and deterministic execution
- Both use testcontainers for integration testing
- Both follow strict error handling (no `.expect()` in production)
- Both have comprehensive CI/CD pipelines

**Key Opportunity**: GGEN's cleanroom testing framework (`ggen-cleanroom/`) is **directly compatible** with CLNRM's mission and can be integrated with minimal adaptation.

---

## 1. Component Adaptation Matrix

### Priority 1: CRITICAL (Immediate Integration)

#### 1.1 Cleanroom Testing Framework

**Source**: `ggen-cleanroom/src/`
**Target**: `crates/clnrm-core/src/cleanroom/`

**Files to Adapt**:
```
ggen-cleanroom/src/
├── mod.rs              → clnrm-core/src/cleanroom/mod.rs
├── surfaces.rs         → clnrm-core/src/cleanroom/surfaces.rs
├── policy.rs           → clnrm-core/src/cleanroom/policy.rs
├── attestation.rs      → clnrm-core/src/cleanroom/attestation.rs
└── forensics.rs        → clnrm-core/src/cleanroom/forensics.rs
```

**Adaptation Steps**:

1. **Copy base implementation**:
   ```bash
   # Copy cleanroom module to CLNRM
   cp -r /Users/sac/ggen/ggen-core/src/cleanroom/* \
         /Users/sac/clnrm/crates/clnrm-core/src/cleanroom/
   ```

2. **Update module paths**:
   ```rust
   // In clnrm-core/src/lib.rs
   pub mod cleanroom;

   // Re-export public types
   pub use cleanroom::{
       CleanroomEnv,
       Policy,
       Attestation,
       ForensicsEngine,
   };
   ```

3. **Adapt to CLNRM's backend trait**:
   ```rust
   // GGEN's cleanroom uses testcontainers directly
   // CLNRM abstracts containers via Backend trait

   // Adapter implementation
   use clnrm_core::backend::{Backend, ContainerBackend};

   impl CleanroomEnv {
       pub fn with_backend<B: Backend>(backend: B) -> Self {
           // Wrap testcontainers in CLNRM's Backend trait
           Self {
               backend: Box::new(backend),
               policy: Policy::default(),
               forensics: ForensicsEngine::new(),
           }
       }
   }
   ```

4. **Integrate with CLNRM's service plugins**:
   ```rust
   // CLNRM's ServicePlugin trait
   pub trait ServicePlugin {
       fn start(&self) -> Result<ServiceHandle>;
       fn stop(&self) -> Result<()>;
       fn health_check(&self) -> Result<bool>;
   }

   // Adapter for GGEN's cleanroom containers
   pub struct CleanroomServicePlugin {
       container: CleanroomContainer,
   }

   impl ServicePlugin for CleanroomServicePlugin {
       fn start(&self) -> Result<ServiceHandle> {
           let handle = self.container.start()?;
           Ok(ServiceHandle::new(handle.id(), handle.ports()))
       }
   }
   ```

**Integration Test**:
```rust
#[tokio::test]
async fn test_cleanroom_clnrm_integration() -> Result<()> {
    // Use CLNRM's environment
    let env = CleanroomEnvironment::new().await?;

    // Register GGEN's cleanroom as a service
    let cleanroom_plugin = CleanroomServicePlugin::new("postgres:15");
    env.register_service(Box::new(cleanroom_plugin)).await?;

    // Start and verify
    let handle = env.start_service("postgres").await?;
    assert!(handle.is_running());

    Ok(())
}
```

**Estimated Effort**: 2-3 days
**Impact**: ⭐⭐⭐⭐⭐ (Critical - direct feature enhancement)

---

#### 1.2 Error Handling Patterns

**Source**: Throughout GGEN codebase
**Target**: All CLNRM production code

**Pattern to Adopt**:
```rust
// ❌ Current CLNRM pattern (needs improvement)
pub fn execute_test(&self) -> Result<()> {
    let result = risky_operation().expect("Failed");  // FORBIDDEN
    Ok(())
}

// ✅ GGEN pattern (production-grade)
pub fn execute_test(&self) -> Result<TestOutput, CleanroomError> {
    let result = risky_operation()
        .map_err(|e| CleanroomError::ExecutionFailed {
            reason: format!("Test execution failed: {}", e),
        })?;

    Ok(TestOutput::from(result))
}
```

**Domain-Specific Error Types**:
```rust
// Adapt GGEN's error pattern to CLNRM

#[derive(Debug, thiserror::Error)]
pub enum CleanroomError {
    #[error("Container initialization failed: {reason}")]
    ContainerInitFailed { reason: String },

    #[error("Test execution failed in container '{container}': {reason}")]
    TestExecutionFailed {
        container: String,
        reason: String,
    },

    #[error("Service plugin '{plugin}' failed to start: {reason}")]
    ServiceStartFailed {
        plugin: String,
        reason: String,
    },

    #[error("Backend error: {0}")]
    Backend(#[from] BackendError),

    #[error("Configuration error: {0}")]
    Config(#[from] ConfigError),
}
```

**Audit Script**:
```bash
# Find all .expect() and .unwrap() calls in production code
grep -r "\.expect(" crates/clnrm-core/src --include="*.rs" | grep -v "test"
grep -r "\.unwrap(" crates/clnrm-core/src --include="*.rs" | grep -v "test"

# Goal: Zero results in production paths
```

**Estimated Effort**: 3-5 days (codebase audit + refactoring)
**Impact**: ⭐⭐⭐⭐⭐ (Critical - production readiness)

---

#### 1.3 CI/CD Workflow Patterns

**Source**: `ggen/.github/workflows/`
**Target**: `clnrm/.github/workflows/`

**Key Workflows to Adapt**:

1. **Test Suite Workflow** (`ci.yml`):
   ```yaml
   # Adapt GGEN's multi-OS testing
   strategy:
     matrix:
       os: [ubuntu-latest, macos-latest]
       rust: [stable, nightly]

   steps:
     - uses: actions/checkout@v4
     - uses: dtolnay/rust-toolchain@master
       with:
         toolchain: ${{ matrix.rust }}
     - uses: taiki-e/install-action@cargo-nextest
     - run: cargo nextest run --workspace --all-features
   ```

2. **Clippy with Zero Warnings** (`lint.yml`):
   ```yaml
   # GGEN's strict linting
   - run: cargo clippy --workspace --all-features -- -D warnings
   ```

3. **Coverage Reporting** (`codecov.yml`):
   ```yaml
   # Adapt GGEN's coverage pipeline
   - name: Install tarpaulin
     run: cargo install cargo-tarpaulin
   - name: Generate coverage
     run: cargo tarpaulin --workspace --out Xml
   - uses: codecov/codecov-action@v4
   ```

4. **Release Automation** (`release.yml`):
   ```yaml
   # GGEN's semantic versioning and Homebrew integration
   on:
     push:
       tags:
         - 'v*'
   jobs:
     release:
       - run: cargo build --release
       - run: ./scripts/update-homebrew-formula.sh
   ```

**Estimated Effort**: 1-2 days
**Impact**: ⭐⭐⭐⭐ (High - automated quality gates)

---

### Priority 2: HIGH (Near-Term Enhancement)

#### 2.1 Template System for Test Generation

**Source**: `ggen-core/src/template.rs`, `pipeline.rs`
**Target**: `clnrm-core/src/template/` (new module)

**Use Case**: Generate CLNRM test cases from templates

**Example Template**:
```yaml
---
# CLNRM test template
to: "tests/{{service}}_integration.rs"
vars:
  service: "postgres"
  version: "15"
  port: 5432
---
#[tokio::test]
async fn test_{{service}}_integration() -> Result<()> {
    let env = CleanroomEnvironment::new().await?;

    let plugin = GenericContainerPlugin::new(
        "{{service}}",
        "{{service}}:{{version}}"
    );
    env.register_service(Box::new(plugin)).await?;

    let handle = env.start_service("{{service}}").await?;

    // Verify service is running
    assert!(handle.is_running());

    Ok(())
}
```

**Adaptation Steps**:

1. **Extract GGEN's template parser**:
   ```rust
   // Minimal adaptation - just parse and render
   use ggen_core::{Template, Pipeline};

   pub struct ClnrmTemplateGenerator {
       pipeline: Pipeline,
   }

   impl ClnrmTemplateGenerator {
       pub fn generate_test(&self, template_path: &Path, vars: &BTreeMap<String, String>) -> Result<PathBuf> {
           let pipeline = Pipeline::new()?;
           let plan = pipeline.render_file(template_path, vars, false)?;
           plan.execute()?;
           Ok(plan.output_path)
       }
   }
   ```

2. **Add CLNRM-specific template functions**:
   ```rust
   // Register CLNRM-specific Tera functions
   pub fn register_clnrm_functions(tera: &mut Tera) {
       // Service helpers
       tera.register_function("service_port", service_port_fn);
       tera.register_function("service_env", service_env_fn);

       // Container helpers
       tera.register_function("container_name", container_name_fn);
       tera.register_function("health_check", health_check_fn);
   }
   ```

**Estimated Effort**: 3-5 days
**Impact**: ⭐⭐⭐⭐ (High - developer productivity)

---

#### 2.2 Lifecycle DAG Pattern

**Source**: `ggen-core/src/lifecycle/`
**Target**: `clnrm-core/src/lifecycle/` (enhancement)

**Use Case**: Orchestrate CLNRM test execution with dependency resolution

**Current CLNRM**: Sequential test execution
**GGEN Pattern**: DAG-based execution with automatic dependency resolution

**Adaptation**:
```rust
// Define CLNRM test phases
pub enum TestPhase {
    Setup,           // Container initialization
    PreTest,         // Pre-test hooks
    Execute,         // Test execution
    Validate,        // Assertion validation
    Teardown,        // Cleanup
}

// Build execution DAG
let mut lifecycle = Lifecycle::new();

lifecycle.add_phase(Phase {
    name: "setup".to_string(),
    dependencies: vec![],  // No dependencies
    commands: vec![
        Command::Shell("docker network create clnrm-net".to_string()),
    ],
});

lifecycle.add_phase(Phase {
    name: "execute".to_string(),
    dependencies: vec!["setup".to_string()],  // Depends on setup
    commands: vec![
        Command::Test("cargo test --test integration".to_string()),
    ],
});

lifecycle.add_phase(Phase {
    name: "teardown".to_string(),
    dependencies: vec!["execute".to_string()],  // Always runs after execute
    commands: vec![
        Command::Shell("docker network rm clnrm-net".to_string()),
    ],
});

// Execute with automatic ordering
lifecycle.run("teardown").await?;  // Runs setup → execute → teardown
```

**Estimated Effort**: 5-7 days
**Impact**: ⭐⭐⭐ (Medium - advanced feature)

---

### Priority 3: MEDIUM (Optional Enhancement)

#### 3.1 OpenTelemetry Integration

**Source**: `ggen-core/src/telemetry.rs`
**Target**: `clnrm-core/src/telemetry/` (enhancement)

**CLNRM Status**: Basic OTEL support exists in `src/telemetry/`
**GGEN Enhancement**: Production-ready OTLP exporters

**Adaptation**:
```rust
// Enhance CLNRM's telemetry with GGEN's patterns

pub fn init_clnrm_telemetry(config: &OtelConfig) -> Result<OtelGuard> {
    // GGEN's OTLP pipeline
    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .http()
                .with_endpoint(&config.otlp_endpoint)
        )
        .install_batch(opentelemetry_sdk::runtime::Tokio)?;

    // CLNRM-specific spans
    tracing_subscriber::registry()
        .with(tracing_opentelemetry::layer().with_tracer(tracer))
        .with(clnrm_telemetry_layer())  // CLNRM-specific layer
        .init();

    Ok(OtelGuard::new())
}

fn clnrm_telemetry_layer() -> impl Layer {
    tracing_subscriber::fmt::layer()
        .with_target(true)
        .with_thread_ids(true)
        .with_test_name()  // CLNRM-specific: include test name
        .with_container_id()  // CLNRM-specific: include container ID
}
```

**Estimated Effort**: 2-3 days
**Impact**: ⭐⭐⭐ (Medium - observability enhancement)

---

#### 3.2 Post-Quantum Cryptography

**Source**: `ggen-core/src/pqc.rs`
**Target**: `clnrm-core/src/security/pqc.rs` (new module)

**Use Case**: Sign test results and artifacts for tamper-proof auditing

**Adaptation**:
```rust
// Adapt GGEN's PQC for CLNRM test result signing

pub struct TestResultSigner {
    signer: PqcSigner,
}

impl TestResultSigner {
    pub fn sign_result(&self, result: &TestResult) -> Result<SignedTestResult> {
        let serialized = serde_json::to_vec(result)?;
        let signature = self.signer.sign(&serialized)?;

        Ok(SignedTestResult {
            result: result.clone(),
            signature,
            signed_at: Utc::now(),
        })
    }
}

pub struct TestResultVerifier {
    verifier: PqcVerifier,
}

impl TestResultVerifier {
    pub fn verify_result(&self, signed: &SignedTestResult) -> Result<bool> {
        let serialized = serde_json::to_vec(&signed.result)?;
        self.verifier.verify(&serialized, &signed.signature)
    }
}
```

**Estimated Effort**: 3-5 days
**Impact**: ⭐⭐ (Low-Medium - advanced security feature)

---

## 2. Dependency Alignment

### Shared Dependencies

**Already Aligned**:
```toml
# Both projects use:
tokio = "1.47"
serde = "1.0"
anyhow = "1.0"
thiserror = "2.0"
tracing = "0.1"
testcontainers = "0.25"
tempfile = "3"
```

**GGEN Dependencies to Add to CLNRM**:
```toml
[dependencies]
# Template system
tera = "1.20"
gray_matter = { version = "0.3.2", features = ["yaml"] }

# Post-quantum cryptography (optional)
pqcrypto-mldsa = "0.1"
pqcrypto-traits = "0.3"

# Enhanced telemetry
opentelemetry = "0.21"
opentelemetry-otlp = "0.14"
tracing-opentelemetry = "0.22"

# Caching (if using template system)
lru = "0.16"
ahash = "0.8"
```

---

## 3. Testing Strategy Integration

### GGEN's Testing Pyramid Applied to CLNRM

**Level 1: Unit Tests** (Fast, many)
```rust
// Adapt GGEN's test structure
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_service_plugin_creation() {
        let plugin = GenericContainerPlugin::new("test", "alpine:latest");
        assert_eq!(plugin.service_type(), "generic_container");
    }
}
```

**Level 2: Integration Tests** (Medium speed, fewer)
```rust
// GGEN's cleanroom pattern
#[tokio::test]
async fn test_postgres_integration() -> Result<()> {
    let env = CleanroomEnv::new()
        .with_postgres("15")
        .with_seed(42)
        .build()
        .await?;

    let client = env.postgres().client();
    // Test database operations
    Ok(())
}
```

**Level 3: Property-Based Tests** (Slow, verify invariants)
```rust
// Adapt GGEN's proptest usage
proptest! {
    #[test]
    fn test_container_lifecycle_is_idempotent(
        image in "[a-z]+:[0-9]+"
    ) {
        let env1 = create_env(&image)?;
        let env2 = create_env(&image)?;

        // Same image = same behavior
        prop_assert_eq!(env1.state(), env2.state());
    }
}
```

**Level 4: BDD Tests** (User acceptance)
```rust
// GGEN's cucumber integration
#[given("a cleanroom environment")]
async fn given_cleanroom_env(world: &mut TestWorld) {
    world.env = CleanroomEnvironment::new().await.unwrap();
}

#[when("I start a postgres service")]
async fn when_start_postgres(world: &mut TestWorld) {
    let plugin = GenericContainerPlugin::new("postgres", "postgres:15");
    world.env.register_service(Box::new(plugin)).await.unwrap();
    world.handle = world.env.start_service("postgres").await.unwrap();
}

#[then("the service should be running")]
async fn then_service_running(world: &mut TestWorld) {
    assert!(world.handle.is_running());
}
```

---

## 4. Build System Integration

### Cargo Make Tasks

**Adapt GGEN's Makefile.toml**:
```toml
# Add to clnrm/Makefile.toml

[tasks.quick]
description = "Quick dev workflow (format + test)"
dependencies = ["fmt", "test"]

[tasks.dev]
description = "Full dev workflow (format + lint + test)"
dependencies = ["fmt", "lint", "test"]

[tasks.production-readiness]
description = "Comprehensive production validation"
dependencies = [
    "test",
    "lint",
    "build-release",
    "clippy-strict",
]

[tasks.clippy-strict]
description = "Clippy with zero warnings"
command = "cargo"
args = ["clippy", "--all-targets", "--all-features", "--", "-D", "warnings"]

[tasks.test-cleanroom]
description = "Run cleanroom integration tests"
command = "cargo"
args = ["test", "--test", "cleanroom_integration"]

[tasks.audit]
description = "Security audit"
command = "cargo"
args = ["audit"]
```

---

## 5. Documentation Integration

### Adapt GGEN's Documentation Structure

**Create in CLNRM**:
```
clnrm/docs/
├── architecture/
│   ├── cleanroom-design.md      # Adapt from GGEN
│   ├── testing-patterns.md      # Adapt from GGEN
│   └── error-handling.md        # Adapt from GGEN
├── guides/
│   ├── quickstart.md            # Adapt from GGEN
│   ├── testing-guide.md         # New for CLNRM
│   └── plugin-development.md    # New for CLNRM
└── reference/
    ├── api.md                   # API documentation
    ├── configuration.md         # Config reference
    └── cli.md                   # CLI command reference
```

**GGEN's Progressive Help System**:
```rust
// Adapt for CLNRM CLI
pub fn show_help(level: UserLevel) -> String {
    match level {
        UserLevel::Newcomer => {
            "🚀 Welcome to CLNRM!\n\
             Essential commands:\n\
             • clnrm init - Initialize test configuration\n\
             • clnrm run - Execute tests\n\
             • clnrm self-test - Validate CLNRM installation"
        }
        UserLevel::Intermediate => {
            "📚 CLNRM Commands:\n\
             • clnrm run <test> - Run specific test\n\
             • clnrm plugins - List available service plugins\n\
             • clnrm report - Generate test reports"
        }
        UserLevel::Advanced => {
            "🔧 Advanced CLNRM:\n\
             • clnrm run --otel-exporter <endpoint> - Enable OTEL\n\
             • clnrm self-test --suite <suite> - Run specific suite\n\
             • clnrm debug <test> - Debug test execution"
        }
    }
}
```

---

## 6. Step-by-Step Integration Plan

### Week 1: Foundation

**Day 1-2: Error Handling Audit**
- [ ] Find all `.expect()` and `.unwrap()` calls
- [ ] Create domain-specific error types
- [ ] Refactor production code paths

**Day 3-4: Cleanroom Module Integration**
- [ ] Copy GGEN's cleanroom module
- [ ] Adapt to CLNRM's Backend trait
- [ ] Create adapter for ServicePlugin

**Day 5: CI/CD Enhancement**
- [ ] Add GGEN's GitHub Actions workflows
- [ ] Enable cargo-nextest
- [ ] Set up code coverage reporting

### Week 2: Advanced Features

**Day 1-3: Template System (Optional)**
- [ ] Extract GGEN's template parser
- [ ] Create CLNRM-specific template functions
- [ ] Add test generation examples

**Day 4-5: Testing Enhancement**
- [ ] Add property-based tests
- [ ] Integrate BDD scenarios
- [ ] Create shared test utilities

### Week 3: Production Readiness

**Day 1-2: Documentation**
- [ ] Adapt GGEN's documentation structure
- [ ] Create quickstart guide
- [ ] Add architecture decision records

**Day 3-4: Telemetry Enhancement**
- [ ] Improve OTEL integration
- [ ] Add CLNRM-specific spans
- [ ] Set up metrics collection

**Day 5: Validation**
- [ ] Run production readiness checklist
- [ ] Benchmark performance SLOs
- [ ] Validate dogfooding compliance

---

## 7. Success Metrics

### Definition of Done (Per Component)

**Cleanroom Integration**:
- [ ] All GGEN cleanroom tests pass in CLNRM
- [ ] Hermetic isolation verified
- [ ] Deterministic execution (100% reproducible)
- [ ] Resource limits enforced

**Error Handling**:
- [ ] Zero `.expect()` calls in production code
- [ ] All functions return `Result<T, CleanroomError>`
- [ ] Comprehensive error messages
- [ ] Error context propagation

**CI/CD**:
- [ ] All tests pass on Ubuntu + macOS
- [ ] Clippy with zero warnings
- [ ] Code coverage >85%
- [ ] Security audit passes

**Template System** (Optional):
- [ ] Can generate test files from templates
- [ ] CLNRM-specific functions registered
- [ ] Example templates documented

---

## 8. Risk Mitigation

### Potential Issues & Solutions

**Issue 1: Dependency Conflicts**
- **Risk**: GGEN and CLNRM may have conflicting dependency versions
- **Mitigation**: Use workspace.dependencies to force version alignment
- **Fallback**: Vendor specific versions if incompatible

**Issue 2: Backend Abstraction Mismatch**
- **Risk**: GGEN's cleanroom uses testcontainers directly, CLNRM uses Backend trait
- **Mitigation**: Create adapter layer between GGEN's containers and CLNRM's Backend
- **Fallback**: Fork GGEN's cleanroom and refactor to use Backend trait

**Issue 3: Test Coverage Regression**
- **Risk**: Refactoring error handling may break existing tests
- **Mitigation**: Run tests after each refactoring step, fix immediately
- **Fallback**: Keep parallel implementations temporarily

**Issue 4: Build Time Increase**
- **Risk**: Adding template system may slow builds
- **Mitigation**: Make template system optional with feature flag
- **Fallback**: Extract to separate crate

---

## 9. Maintenance Strategy

### Keeping GGEN Adaptations Up-to-Date

**Monitoring**:
- Watch GGEN repository for relevant updates
- Subscribe to GGEN release notifications
- Monitor GGEN's cleanroom module changes

**Update Process**:
1. Review GGEN release notes for cleanroom changes
2. Test changes in isolation
3. Merge compatible changes to CLNRM
4. Update CLNRM documentation

**Version Tracking**:
```toml
# Document GGEN source version in Cargo.toml
[package.metadata.ggen-adaptation]
cleanroom-module-version = "1.2.0"
template-system-version = "1.2.0"
last-sync-date = "2025-10-17"
```

---

## 10. Conclusion

### Recommended Immediate Actions

**Top 3 Priorities**:

1. **Integrate Cleanroom Module** (2-3 days, ⭐⭐⭐⭐⭐)
   - Direct compatibility with CLNRM's mission
   - Minimal adaptation required
   - Immediate value

2. **Audit Error Handling** (3-5 days, ⭐⭐⭐⭐⭐)
   - Critical for production readiness
   - GGEN's pattern is exemplary
   - Must-have for v1.0

3. **Enhance CI/CD** (1-2 days, ⭐⭐⭐⭐)
   - GGEN's workflows are production-grade
   - Easy to adapt
   - Automated quality gates

**Long-Term Opportunities**:
- Template system for test generation
- Lifecycle DAG for complex test orchestration
- Post-quantum cryptography for audit trails

**Success Criteria**:
- [ ] CLNRM reaches 90%+ production readiness score
- [ ] Zero `.expect()` calls in production code
- [ ] Cleanroom isolation verified with GGEN's tests
- [ ] CI/CD passing on all platforms

---

**Adaptation Complete**: CLNRM can leverage GGEN's battle-tested patterns to accelerate v1.0 production readiness.
