# Test Priorities - Quick Reference

**Target**: 95%+ Coverage | **Current**: ~47% | **Gap**: 1,565 tests needed

## 🚨 IMMEDIATE ACTION REQUIRED

### P0: Compilation Blockers (Fix Today)
```rust
// File: crates/ggen-marketplace-v2/src/registry_rdf.rs
// Issue: 4 Send trait violations blocking workspace compilation
// Lines: 165, 196 (get_package, all_packages)
// Fix: Convert to SparqlEvaluator, collect iterators before .await
```

**Command to verify fix**:
```bash
cd crates/ggen-marketplace-v2
cargo build --lib
```

---

## 📊 Test Gap Summary by Priority

### P1: Critical (Days 1-3, 300 tests)
| Module | Tests | LOC | Why Critical |
|--------|-------|-----|--------------|
| **Lifecycle/optimization** | 40 | 1,200 | Production deployment performance |
| **Lifecycle/production** | 50 | 1,500 | ReadinessTracker for releases |
| **Ontology/sigma_runtime** | 50 | 1,500 | Complex state management |
| **Ontology/validators** | 40 | 1,200 | Domain validation logic |
| **Ontology/constitution** | 30 | 900 | Invariant checking |
| **Ontology/control_loop** | 35 | 1,050 | Autonomous operations |
| **Ontology/pattern_miner** | 25 | 750 | Pattern detection |
| **Lifecycle/state_validation** | 20 | 600 | State correctness |
| **Lifecycle/template_phase** | 10 | 300 | Template integration |

---

### P2: High (Days 4-6, 400 tests)
| Module | Tests | LOC | Why Important |
|--------|-------|-----|---------------|
| **Graph/core edge cases** | 40 | 1,200 | Core RDF operations |
| **Graph/query** | 30 | 900 | SPARQL reliability |
| **Graph/update** | 20 | 600 | Data mutation safety |
| **Templates/file_tree_generator** | 35 | 1,050 | File generation |
| **Templates/format** | 30 | 900 | Format handling |
| **Templates/business_logic** | 20 | 600 | Business rules |
| **RDF/schema** | 35 | 1,050 | Schema validation |
| **RDF/template_metadata** | 35 | 1,050 | Metadata extraction |
| **RDF/validation** | 25 | 750 | Data integrity |
| **Marketplace RDF** | 50 | 1,500 | RDF mapping |
| **Marketplace registry** | 40 | 1,200 | Package discovery |
| **Marketplace search** | 40 | 1,200 | SPARQL queries |

---

### P3: Medium (Days 7-9, 445 tests)
| Module | Tests | LOC | Coverage Goal |
|--------|-------|-----|---------------|
| **generator.rs** | 30 | 900 | 90% |
| **pipeline.rs** | 35 | 1,050 | 90% |
| **register.rs** | 40 | 1,200 | 85% |
| **registry.rs** | 45 | 1,350 | 85% |
| **resolver.rs** | 40 | 1,200 | 85% |
| **snapshot.rs** | 35 | 1,050 | 80% |
| **delta.rs** | 40 | 1,200 | 85% |
| **merge.rs** | 45 | 1,350 | 85% |
| **Other utilities** | 40 | 1,200 | 80% |
| **CLI commands** | 40 | 1,200 | 85% |
| **CLI output** | 30 | 900 | 80% |
| **CLI errors** | 25 | 750 | 80% |

---

### P4: Specialized (Days 10-12, 420 tests)
- **Performance**: 250 tests, 7,500 LOC (benchmarks, load, scaling)
- **Security**: 170 tests, 5,500 LOC (injection, auth, crypto)

---

## 🎯 Quick Start Guide

### Day 1 Morning: Fix Compilation
```bash
# 1. Fix ggen-marketplace-v2
cd crates/ggen-marketplace-v2
# Edit src/registry_rdf.rs and src/rdf_mapper.rs
# Replace Store::query() with SparqlEvaluator
# Collect iterators before .await

# 2. Verify fix
cargo build --lib
cargo test --lib
```

### Day 1 Afternoon: Setup Test Infrastructure
```bash
# Install coverage tools
cargo install cargo-tarpaulin

# Create test utilities
mkdir -p crates/ggen-core/tests/utils
touch crates/ggen-core/tests/utils/mod.rs
touch crates/ggen-core/tests/utils/fixtures.rs
touch crates/ggen-core/tests/utils/mocks.rs
```

### Day 2-3: Critical Tests
```bash
# Focus order:
# 1. lifecycle/optimization.rs
# 2. lifecycle/production.rs
# 3. ontology/sigma_runtime.rs
# 4. ontology/validators.rs

# Run tests continuously
cargo watch -x "test --package ggen-core"
```

---

## 📋 Test Template

### Unit Test Template
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_handle_valid_input() {
        // Arrange
        let input = setup_test_input();

        // Act
        let result = function_under_test(input);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected_value);
    }

    #[test]
    fn should_fail_on_invalid_input() {
        // Arrange
        let invalid = create_invalid_input();

        // Act
        let result = function_under_test(invalid);

        // Assert
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "Expected error message"
        );
    }

    #[test]
    fn should_handle_edge_case() {
        // Arrange
        let edge_case = create_edge_case();

        // Act
        let result = function_under_test(edge_case);

        // Assert
        assert_matches!(result, Ok(expected_pattern));
    }
}
```

### Integration Test Template
```rust
#[tokio::test]
async fn test_module_integration() -> Result<()> {
    // Setup
    let temp_dir = TempDir::new()?;
    let graph = Graph::new()?;
    let config = create_test_config();

    // Execute workflow
    let step1 = execute_step1(&graph, &config).await?;
    let step2 = execute_step2(&graph, step1).await?;
    let final_result = execute_step3(&graph, step2).await?;

    // Verify
    assert_eq!(final_result.status, Status::Success);
    assert!(final_result.output.exists());

    // Cleanup
    temp_dir.close()?;
    Ok(())
}
```

### Performance Test Template
```rust
#[test]
fn test_performance_under_100ms() {
    use std::time::Instant;

    let input = generate_test_data(1000);
    let start = Instant::now();

    let _result = function_under_test(input);

    let duration = start.elapsed();
    assert!(
        duration.as_millis() < 100,
        "Function took {}ms, expected <100ms",
        duration.as_millis()
    );
}

#[bench]
fn bench_critical_path(b: &mut Bencher) {
    let input = generate_test_data(1000);
    b.iter(|| {
        black_box(function_under_test(black_box(&input)))
    });
}
```

---

## 🔍 Coverage Commands

### Generate Coverage Report
```bash
# HTML report (viewable in browser)
cargo tarpaulin --out Html --output-dir coverage

# Open in browser
open coverage/index.html

# Check specific module
cargo tarpaulin --packages ggen-core -- --test-threads=1
```

### Coverage Targets
```bash
# Overall target: 95%
# Minimum by module:
# - Critical modules (lifecycle, ontology): 95%
# - High priority (graph, templates, rdf): 90%
# - Medium priority (utilities): 85%
# - Low priority (CLI, AI): 80%
```

---

## ⚡ Test Execution Strategies

### Run Tests by Speed
```bash
# Fast tests only (<100ms)
cargo test --lib -- --test-threads=8

# Integration tests (slower)
cargo test --test '*' -- --test-threads=1

# All tests
cargo test --all
```

### Run Tests by Module
```bash
# Lifecycle tests
cargo test --package ggen-core lifecycle

# Graph tests
cargo test --package ggen-core graph

# Ontology tests
cargo test --package ggen-core ontology
```

### Watch Mode (Development)
```bash
# Install cargo-watch
cargo install cargo-watch

# Auto-run tests on file changes
cargo watch -x "test --package ggen-core -- --nocapture"
```

---

## 📈 Daily Progress Tracking

### Day-by-Day Checklist

**Day 1**: ☐ Fix compilation, ☐ Setup infrastructure
**Day 2**: ☐ lifecycle/optimization (40 tests)
**Day 3**: ☐ lifecycle/production (50 tests), ☐ ontology/sigma_runtime (25 tests)
**Day 4**: ☐ ontology/sigma_runtime (25 tests), ☐ ontology/validators (40 tests)
**Day 5**: ☐ ontology/constitution (30 tests), ☐ ontology/control_loop (35 tests)
**Day 6**: ☐ ontology/pattern_miner (25 tests), ☐ graph/core (40 tests)
**Day 7**: ☐ graph/query+update (50 tests), ☐ templates (35 tests)
**Day 8**: ☐ templates (50 tests), ☐ RDF (45 tests)
**Day 9**: ☐ RDF (50 tests), ☐ marketplace (50 tests)
**Day 10**: ☐ utilities (200 tests)
**Day 11**: ☐ utilities (145 tests), ☐ CLI (95 tests)
**Day 12**: ☐ Performance (250 tests), ☐ Security (170 tests)

### Coverage Milestones
- Day 3: 60% coverage (600 tests total)
- Day 6: 75% coverage (1,100 tests total)
- Day 9: 85% coverage (1,400 tests total)
- Day 12: 95% coverage (1,565 new + existing tests)

---

## 🎯 Focus Areas per Day

| Day | Morning (4h) | Afternoon (4h) | Tests | Coverage Δ |
|-----|-------------|----------------|-------|------------|
| 1 | Fix compilation | Setup infrastructure | 0 | +0% |
| 2 | lifecycle/optimization | lifecycle/production (part 1) | 65 | +5% |
| 3 | lifecycle/production (part 2) | ontology/sigma_runtime | 60 | +5% |
| 4 | ontology/validators | ontology/constitution | 70 | +6% |
| 5 | ontology/control_loop | ontology/pattern_miner | 60 | +5% |
| 6 | graph tests | template/file_tree_generator | 75 | +6% |
| 7 | template/format+logic | RDF/schema | 85 | +7% |
| 8 | RDF/validation+metadata | marketplace RDF | 110 | +8% |
| 9 | marketplace registry+search | utilities (part 1) | 125 | +9% |
| 10 | utilities (part 2) | utilities (part 3) | 200 | +10% |
| 11 | utilities (part 4) | CLI tests | 240 | +12% |
| 12 | Performance tests | Security tests | 420 | +22% |

---

## 🛠️ Test Utilities to Build

### Priority 1: Test Fixtures
```rust
// crates/ggen-core/tests/utils/fixtures.rs
pub fn create_test_graph() -> Graph { /* ... */ }
pub fn create_test_template() -> Template { /* ... */ }
pub fn create_temp_dir() -> TempDir { /* ... */ }
pub fn load_fixture_file(name: &str) -> String { /* ... */ }
```

### Priority 2: Mocks
```rust
// crates/ggen-core/tests/utils/mocks.rs
pub struct MockRegistry { /* ... */ }
pub struct MockFileSystem { /* ... */ }
pub struct MockGitHubClient { /* ... */ }
```

### Priority 3: Assertions
```rust
// crates/ggen-core/tests/utils/assertions.rs
pub fn assert_graph_contains(graph: &Graph, turtle: &str) { /* ... */ }
pub fn assert_file_exists(path: &Path) { /* ... */ }
pub fn assert_template_renders(template: &Template, expected: &str) { /* ... */ }
```

---

## 📊 Expected Outcomes

### Week 1 (Days 1-3)
- ✅ Compilation fixed
- ✅ Test infrastructure ready
- ✅ 300 critical tests written
- ✅ 60% coverage achieved
- ✅ All critical modules tested

### Week 2 (Days 4-9)
- ✅ 700 additional tests written
- ✅ 85% coverage achieved
- ✅ All high-priority modules tested
- ✅ Medium-priority modules started

### Week 3 (Days 10-12)
- ✅ 1,565 total new tests
- ✅ 95%+ coverage achieved
- ✅ Performance baselines established
- ✅ Security audit clean
- ✅ All tests passing consistently

---

**Remember**: Fix compilation FIRST, then test in priority order. Quality over quantity - ensure each test is valuable and maintainable.
