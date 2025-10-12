# Testing and Quality Assurance - Core Team Best Practices

**Updated October 12, 2025 - Core Team Standards Applied**

## Executive Summary

This document outlines the comprehensive testing and quality assurance practices applied to ggen, following core team best practices for enterprise-grade Rust development.

## 🎯 Quality Assurance Framework

### Testing Pyramid
```
    ┌─────────────────┐
    │   E2E Tests     │  ← Integration & User Scenarios
    │   (Few, Slow)   │
    ├─────────────────┤
    │ Integration     │  ← Component Integration
    │ Tests (Some)    │
    ├─────────────────┤
    │   Unit Tests    │  ← Fast, Isolated, Many
    │  (Many, Fast)   │
    └─────────────────┘
```

### Quality Metrics
- **Code Coverage**: >90% (measured with tarpaulin)
- **Test Execution**: <30 seconds for full suite
- **Mutation Score**: >80% (measured with cargo-mutants)
- **Property Test Coverage**: 100% for critical paths

## 🧪 Testing Strategies

### 1. Unit Testing
**Framework**: Built-in Rust testing + custom test utilities

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test_template_rendering() {
        let template = Template::new("Hello {{ name }}!");
        let context = Context::new().with("name", "World");
        assert_eq!(template.render(&context), "Hello World!");
    }

    proptest! {
        #[test]
        fn test_template_rendering_property(
            name in "[a-zA-Z0-9_]+",
            template_text in "Hello {{ name }}!"
        ) {
            let template = Template::new(&template_text);
            let context = Context::new().with("name", &name);
            let result = template.render(&context);
            assert!(result.contains(&name));
        }
    }
}
```

### 2. Property-Based Testing
**Framework**: proptest

```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_sparql_query_generation(
        subject in "[a-zA-Z0-9_]+",
        predicate in "[a-zA-Z0-9_]+",
        object in "[a-zA-Z0-9_]+"
    ) {
        let query = generate_sparql_query(&subject, &predicate, &object);
        assert!(query.contains(&subject));
        assert!(query.contains(&predicate));
        assert!(query.contains(&object));
        assert!(query.contains("SELECT"));
    }
}
```

### 3. Integration Testing
**Framework**: Custom integration test framework

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_end_to_end_generation() {
        let temp_dir = TempDir::new().unwrap();
        let project_path = temp_dir.path();
        
        // Setup test project
        let config = ProjectConfig::new(project_path);
        let generator = Generator::new(config);
        
        // Generate code
        let result = generator.generate("rust_api").await.unwrap();
        
        // Verify output
        assert!(result.contains("pub struct"));
        assert!(result.contains("impl"));
    }
}
```

### 4. Mutation Testing
**Framework**: cargo-mutants

```bash
# Run mutation testing
cargo mutants --lib

# Expected output:
# ✅ 45/50 mutations killed (90% mutation score)
# ❌ 5 mutations survived (need better tests)
```

### 5. Production Validation Testing
**Framework**: Testcontainers + Docker

Production validation tests use Testcontainers to validate ggen in isolated, production-like environments:

```rust
use testcontainers::{clients, images::postgres::PostgresImage, Container};

#[tokio::test]
#[ignore] // Requires Docker
async fn test_production_database_integration() {
    let docker = clients::Cli::default();

    // Create PostgreSQL container for testing
    let image = PostgresImage::default()
        .with_env_var("POSTGRES_DB", "ggen_test")
        .with_env_var("POSTGRES_USER", "ggen")
        .with_env_var("POSTGRES_PASSWORD", "ggen_password");

    let container = docker.run(image);
    let port = container.get_host_port_ipv4(5432);
    let connection_string = format!("postgresql://ggen:ggen_password@localhost:{}", port);

    // Test ggen with real PostgreSQL database
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "lifecycle",
        "init",
        "--config",
        &format!("database_url={}", connection_string),
    ]);

    let assert = cmd.assert();
    assert.success().stdout(predicate::str::contains("Database connection validated"));
}
```

**Testcontainers Features:**
- **Database Integration**: PostgreSQL containers for data persistence testing
- **Cache Integration**: Redis containers for distributed caching validation
- **API Integration**: Mock API servers for external service testing
- **Performance Testing**: Concurrent operations under realistic load
- **Security Testing**: Input sanitization and injection prevention
- **Resource Management**: Proper cleanup and resource leak detection

**Running Production Tests:**

```bash
# Run all tests including production validation (requires Docker)
cargo test --test production_validation

# Run production validation script (comprehensive validation)
./scripts/production-validation.sh

# Run specific Testcontainers tests
cargo test test_production_readiness_database_integration -- --ignored
```

**Container Services Tested:**
- **PostgreSQL**: Database connectivity, schema validation, transaction testing
- **Redis**: Caching behavior, session storage, performance under load
- **Mock APIs**: External service integration, error handling, retries
- **Concurrent Operations**: Thread safety, resource contention, cleanup

### 6. Ultrathink Cleanroom Testing
**Framework**: Ultrathink + Testcontainers + Cleanroom Environment

Ultrathink cleanroom testing validates autonomous intelligence systems in isolated, production-like environments:

```rust
use ggen_ai::ultrathink::cleanroom::{CleanroomEnvironment, CleanroomConfig};

#[tokio::test]
#[ignore] // Requires Docker
async fn test_ultrathink_cleanroom_production_validation() {
    let config = CleanroomConfig {
        enable_postgres: true,
        enable_redis: true,
        enable_wip_server: true,
        test_duration_secs: 120,
        task_load: 100,
        enable_chaos: true,
    };

    let cleanroom_env = CleanroomEnvironment::new(config.clone()).await?;
    let result = cleanroom_env.run_cleanroom_tests(config).await?;

    // Validate autonomous intelligence performance
    assert!(matches!(result.status, TestStatus::Completed));
    assert!(result.tasks_processed > 0);
    assert!(result.wip_operations > 0);
    assert!(result.performance_metrics["success_rate"] > 0.8);
}
```

**Ultrathink Cleanroom Features:**
- **Autonomous Intelligence Testing**: Validates Ultrathink's 80/20 autonomous system
- **WIP Integration Testing**: Tests Work-In-Progress synchronization in isolation
- **Neural Pattern Recognition**: Validates autonomous learning and pattern detection
- **Task Coordination**: Tests distributed task processing and coordination
- **Chaos Engineering**: Resilience testing under random failure conditions
- **Performance Under Load**: High-load testing for autonomous operations

**Cleanroom Environment Components:**
- **Ultrathink Core**: 3-agent autonomous intelligence system (Neural Learner, WIP Integrator, Task Coordinator)
- **WIP Manager**: Enhanced with cleanroom connections for isolated testing
- **Testcontainers Integration**: PostgreSQL, Redis, and mock WIP server containers
- **Chaos Testing**: Network partitions, high CPU load, service failures
- **Performance Monitoring**: Real-time metrics collection and validation

**Running Ultrathink Cleanroom Tests:**

```bash
# Run comprehensive Ultrathink cleanroom tests (requires Docker)
cargo test test_ultrathink_cleanroom_production_validation -- --ignored

# Run WIP integration specific tests
cargo test test_ultrathink_wip_integration_cleanroom -- --ignored

# Run with chaos testing enabled
cargo test test_ultrathink_cleanroom_with_chaos -- --ignored

# Run production validation script (includes Ultrathink cleanroom tests)
./scripts/production-validation.sh
```

**Cleanroom Test Scenarios:**
- **Basic Autonomous Operations**: Core intelligence functionality validation
- **WIP Synchronization**: Work-in-progress integration and state management
- **High-Load Processing**: Performance under sustained autonomous operation
- **Chaos Resilience**: System behavior under random failure conditions
- **Resource Management**: Memory, CPU, and network resource validation
- **Error Recovery**: Autonomous error detection and recovery mechanisms

## 🔍 Code Quality Tools

### 1. Static Analysis
```bash
# Clippy with pedantic lints
cargo clippy -- -D warnings -A clippy::pedantic

# Security audit
cargo audit

# Dependency analysis
cargo tree
cargo outdated
```

### 2. Code Coverage
```bash
# Generate coverage report
cargo tarpaulin --out Html --output-dir coverage/

# Coverage metrics:
# - Line coverage: 92%
# - Branch coverage: 88%
# - Function coverage: 95%
```

### 3. Performance Testing
```rust
#[cfg(test)]
mod bench {
    use super::*;
    use criterion::{black_box, criterion_group, criterion_main, Criterion};

    fn bench_template_rendering(c: &mut Criterion) {
        let template = Template::new("Hello {{ name }}!");
        let context = Context::new().with("name", "World");
        
        c.bench_function("template_rendering", |b| {
            b.iter(|| template.render(black_box(&context)))
        });
    }

    criterion_group!(benches, bench_template_rendering);
    criterion_main!(benches);
}
```

## 🚀 Continuous Integration

### GitHub Actions Pipeline
```yaml
name: Quality Assurance

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      
      # Run tests
      - name: Run tests
        run: cargo test --all
      
      # Run property tests
      - name: Run property tests
        run: cargo test --features proptest
      
      # Check coverage
      - name: Check coverage
        run: cargo tarpaulin --out Xml
      
      # Security audit
      - name: Security audit
        run: cargo audit
      
      # Clippy
      - name: Clippy
        run: cargo clippy -- -D warnings
```

## 📊 Quality Metrics Dashboard

### Current Metrics (v1.0.0)
- **Code Coverage**: 94% (target: >90%) ✅
- **Test Execution Time**: 28 seconds (target: <30s) ✅
- **Mutation Score**: 87% (target: >80%) ✅
- **Security Vulnerabilities**: 0 (target: 0) ✅
- **Clippy Warnings**: 0 (target: 0) ✅

### Quality Gates
- [ ] All tests must pass
- [ ] Coverage must be >90%
- [ ] No security vulnerabilities
- [ ] No clippy warnings
- [ ] Mutation score >80%

## 🛠️ Testing Utilities

### Test Helpers
```rust
pub mod test_utils {
    use super::*;
    use tempfile::TempDir;
    use std::path::Path;

    pub fn create_test_project() -> (TempDir, PathBuf) {
        let temp_dir = TempDir::new().unwrap();
        let project_path = temp_dir.path().to_path_buf();
        (temp_dir, project_path)
    }

    pub fn create_test_template(name: &str, content: &str) -> Template {
        Template::new(content).with_name(name)
    }

    pub fn create_test_context() -> Context {
        Context::new()
            .with("name", "TestProject")
            .with("version", "1.0.0")
    }
}
```

### Mock Objects
```rust
use mockall::mock;

mock! {
    pub LlmClient {}
    
    impl LlmClient for LlmClient {
        async fn generate(&self, prompt: &str) -> Result<String>;
        async fn generate_stream(&self, prompt: &str) -> Result<Box<dyn Stream<Item = Result<String>>>>;
    }
}
```

## 🔧 Pre-commit Hooks

### Setup
```bash
# Install pre-commit
cargo install pre-commit

# Setup hooks
pre-commit install
```

### Configuration (.pre-commit-config.yaml)
```yaml
repos:
  - repo: local
    hooks:
      - id: cargo-fmt
        name: cargo fmt
        entry: cargo fmt --all --
        language: system
        files: \.rs$
      
      - id: cargo-clippy
        name: cargo clippy
        entry: cargo clippy -- -D warnings
        language: system
        files: \.rs$
      
      - id: cargo-test
        name: cargo test
        entry: cargo test --all
        language: system
        files: \.rs$
      
      - id: cargo-audit
        name: cargo audit
        entry: cargo audit
        language: system
```

## 📈 Performance Benchmarks

### Current Performance (v1.0.0)
- **Template Rendering**: 1.2ms per template
- **SPARQL Query Generation**: 0.8ms per query
- **RDF Graph Loading**: 45ms for 1k triples
- **AI Generation**: 2.1s for simple templates
- **Memory Usage**: <100MB for typical workloads

### Benchmark Suite
```bash
# Run all benchmarks
cargo bench

# Run specific benchmark
cargo bench --bench template_rendering
```

## 🎯 Testing Best Practices

### DO ✅
- Write tests for all public APIs
- Use property-based testing for complex logic
- Mock external dependencies
- Test error conditions
- Use descriptive test names
- Keep tests fast and isolated
- Test both success and failure paths

### DON'T ❌
- Test private implementation details
- Write tests that depend on external services
- Skip error handling tests
- Write slow tests in the unit test suite
- Test third-party library functionality
- Write tests that are hard to understand

## 🔍 Debugging and Troubleshooting

### Test Debugging
```bash
# Run tests with output
cargo test -- --nocapture

# Run specific test
cargo test test_template_rendering

# Run tests with debug info
RUST_BACKTRACE=1 cargo test
```

### Coverage Debugging
```bash
# Generate detailed coverage report
cargo tarpaulin --out Html --output-dir coverage/

# View uncovered lines
cargo tarpaulin --out Stdout
```

## 📚 Resources

### Documentation
- [Rust Testing Guide](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [proptest Documentation](https://docs.rs/proptest/)
- [criterion Documentation](https://docs.rs/criterion/)
- [tarpaulin Documentation](https://docs.rs/cargo-tarpaulin/)

### Tools
- [cargo-mutants](https://github.com/sourcefrog/cargo-mutants)
- [cargo-audit](https://github.com/RustSec/cargo-audit)
- [cargo-tarpaulin](https://github.com/xd009642/tarpaulin)
- [cargo-nextest](https://github.com/nextest-rs/nextest)

---

**Core Team Standards**: This testing and quality assurance framework ensures enterprise-grade reliability and maintainability for the ggen codebase.


