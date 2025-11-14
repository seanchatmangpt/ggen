# Testing Architecture - Gap Prevention Framework

**System Architect**: Comprehensive testing architecture design
**Created**: 2025-11-13
**Purpose**: Prevent gap accumulation through systematic testing standards

---

## Executive Summary

This architecture defines a comprehensive, multi-layered testing framework for ggen that prevents gap accumulation by enforcing systematic test coverage at every level. The design follows the 80/20 principle: **20% of architectural decisions prevent 80% of future gaps**.

### Key Metrics (Current vs Target)

| Metric | Current | Target | Gap |
|--------|---------|--------|-----|
| **Test/Source Ratio** | 0.5:1 | 1.2:1 | **Gap: 140% increase needed** |
| **ggen-core Coverage** | 38 test files / 76 source files | 91 test files | **Gap: 53 missing** |
| **ggen-domain Coverage** | 14 test files / 52 source files | 62 test files | **Gap: 48 missing** |
| **Chicago TDD Migration** | ~20% | 100% | **Gap: 80%** |
| **Module Test Coverage** | Ad-hoc | 100% modules | **Gap: Systematic enforcement** |

### Critical Findings

1. **Structural Gap**: Test-to-source ratio of 0.5:1 indicates **50% under-testing**
2. **Domain Layer Gap**: ggen-domain has only **27% test coverage** (14/52 files)
3. **Core Layer Gap**: ggen-core has only **50% test coverage** (38/76 files)
4. **Consistency Gap**: Mixed testing styles prevent systematic validation
5. **Enforcement Gap**: No automated validation of test requirements

---

## Architecture Principles

### 1. Poka-Yoke (Error-Proofing) Design

Testing architecture follows the same Poka-Yoke principles as the codebase:

```rust
// Compiler enforces test coverage
#[deny(missing_tests)]  // Future: Enforce test coverage at compile time
#[deny(untested_modules)] // Future: Prevent modules without tests

// Test architecture enforces correctness
#[test_coverage(min = 85)]  // Minimum coverage requirement
#[chicago_tdd_only]  // Enforce Chicago TDD style
```

**Principle**: If it compiles, tests are enforced. Prevention is better than detection.

### 2. Layered Testing Pyramid

```
    ┌─────────────────────────────┐
    │   E2E Tests (5%)            │  ← User scenarios, production validation
    │   ~50 tests                 │
    ├─────────────────────────────┤
    │   Integration Tests (15%)   │  ← Component integration, API contracts
    │   ~150 tests                │
    ├─────────────────────────────┤
    │   Module Tests (30%)        │  ← Module boundaries, cross-module
    │   ~300 tests                │
    ├─────────────────────────────┤
    │   Unit Tests (50%)          │  ← Fast, isolated, comprehensive
    │   ~500 tests                │
    └─────────────────────────────┘

    Total Target: ~1000 tests (vs current ~500)
    Expected Runtime: <45s (with Chicago TDD optimizations)
```

### 3. Mandatory Test Coverage by Layer

Each layer has **required minimum coverage**:

| Layer | Coverage | Enforcement |
|-------|----------|-------------|
| **Public APIs** | 100% | Compile-time error if untested |
| **Domain Logic** | 95% | Pre-commit hook failure |
| **Core Functions** | 90% | CI pipeline failure |
| **Utility Functions** | 85% | Code review requirement |
| **Private Helpers** | 70% | Optional but encouraged |

### 4. Chicago TDD Enforcement

**All tests MUST use Chicago TDD patterns**:

- ✅ **Arrange-Act-Assert** structure
- ✅ **Real objects** over mocks
- ✅ **State verification** over behavior
- ✅ **Timeouts from config** (no hardcoded values)
- ✅ **Consistent test macros** (`test!`, `async_test!`)

---

## Module-Level Testing Standards

### Per-Module Test Requirements

**Every module MUST have**:

1. **Unit Tests** (`tests/unit/module_name.rs`)
   - Test pure functions
   - Test data transformations
   - Test error conditions
   - Minimum 85% line coverage

2. **Integration Tests** (`tests/integration/module_name.rs`)
   - Test module boundaries
   - Test cross-module interactions
   - Test API contracts
   - Minimum 75% integration coverage

3. **Performance Tests** (`benches/module_name.rs`)
   - Benchmark critical paths
   - Regression detection
   - Memory profiling
   - SLA validation

4. **Property Tests** (`tests/property/module_name.rs`)
   - Property-based testing for complex logic
   - Fuzz testing for parsers
   - Invariant validation

### Module Test Checklist Template

Create `tests/MODULE_TEST_CHECKLIST.md` for each module:

```markdown
# Module Test Checklist: [MODULE_NAME]

**Module**: crates/ggen-[module]/src/[submodule]
**Owner**: [Team/Person]
**Last Updated**: [Date]

## Test Coverage

- [ ] Unit tests exist for all public functions
- [ ] Integration tests cover module boundaries
- [ ] Property tests for complex logic
- [ ] Performance benchmarks for hot paths
- [ ] Error conditions tested
- [ ] Edge cases documented and tested

## Chicago TDD Compliance

- [ ] All tests use `test!` or `async_test!` macros
- [ ] AAA pattern enforced
- [ ] Real objects used (minimal mocking)
- [ ] Timeouts use `integration_timeout()` from config
- [ ] No hardcoded test data in root folder

## Coverage Metrics

- **Line Coverage**: [X]% (min: 85%)
- **Branch Coverage**: [X]% (min: 75%)
- **Integration Coverage**: [X]% (min: 75%)

## Automated Validation

- [ ] Pre-commit hooks pass
- [ ] CI pipeline validates coverage
- [ ] Mutation testing score >80%
```

---

## Crate-Level Testing Structure

### ggen-core Test Architecture

**Current**: 76 source files, 38 test files (50% coverage)
**Target**: 76 source files, 91 test files (120% coverage - includes integration/property tests)

```
crates/ggen-core/
├── src/
│   ├── lib.rs (20 modules)
│   ├── cache/
│   ├── cli_generator/
│   ├── config/
│   ├── delta/
│   ├── generator/
│   ├── github/
│   ├── lifecycle/
│   ├── rdf/
│   ├── templates/
│   └── ... (11 more modules)
│
└── tests/
    ├── unit/
    │   ├── mod.rs
    │   ├── cache_tests.rs              ← MISSING
    │   ├── cli_generator_tests.rs      ← MISSING
    │   ├── config_tests.rs             ← MISSING
    │   ├── delta_tests.rs              ← MISSING
    │   ├── generator_tests.rs          ← EXISTS
    │   ├── github_tests.rs             ← MISSING
    │   ├── lifecycle_tests.rs          ← EXISTS
    │   ├── rdf_tests.rs                ← EXISTS
    │   ├── templates_tests.rs          ← EXISTS
    │   └── ... (20 total needed)
    │
    ├── integration/
    │   ├── mod.rs
    │   ├── cache_integration.rs        ← MISSING
    │   ├── template_pipeline.rs        ← MISSING
    │   ├── lifecycle_e2e.rs            ← EXISTS
    │   ├── marketplace_integration.rs  ← EXISTS
    │   └── ... (15 total needed)
    │
    ├── property/
    │   ├── mod.rs
    │   ├── package_properties.rs       ← EXISTS
    │   ├── version_properties.rs       ← EXISTS
    │   ├── template_properties.rs      ← MISSING
    │   └── ... (10 total needed)
    │
    └── security/
        ├── mod.rs
        ├── input_validation.rs         ← EXISTS
        ├── injection_tests.rs          ← MISSING
        └── ... (5 total needed)

Total gap: 53 missing test files
```

### ggen-domain Test Architecture

**Current**: 52 source files, 14 test files (27% coverage)
**Target**: 52 source files, 62 test files (120% coverage)

```
crates/ggen-domain/
├── src/
│   ├── lib.rs (10 modules)
│   ├── ai/
│   │   ├── analyze.rs
│   │   ├── generate.rs
│   │   └── optimize.rs
│   ├── graph/
│   │   ├── export.rs
│   │   ├── query.rs
│   │   └── visualize.rs
│   ├── marketplace/
│   │   ├── install.rs
│   │   ├── list.rs
│   │   ├── publish.rs
│   │   ├── registry.rs
│   │   └── search.rs
│   ├── template/
│   │   ├── generate.rs
│   │   ├── lint.rs
│   │   ├── list.rs
│   │   └── show.rs
│   └── ... (6 more modules)
│
└── tests/
    ├── unit/
    │   ├── mod.rs
    │   ├── ai/
    │   │   ├── analyze_tests.rs        ← MISSING
    │   │   ├── generate_tests.rs       ← MISSING
    │   │   └── optimize_tests.rs       ← MISSING
    │   ├── graph/
    │   │   ├── export_tests.rs         ← EXISTS
    │   │   ├── query_tests.rs          ← EXISTS
    │   │   └── visualize_tests.rs      ← MISSING
    │   ├── marketplace/
    │   │   ├── install_tests.rs        ← MISSING
    │   │   ├── list_tests.rs           ← MISSING
    │   │   ├── publish_tests.rs        ← MISSING
    │   │   ├── registry_tests.rs       ← MISSING
    │   │   └── search_tests.rs         ← MISSING
    │   ├── template/
    │   │   ├── generate_tests.rs       ← MISSING
    │   │   ├── lint_tests.rs           ← MISSING
    │   │   ├── list_tests.rs           ← MISSING
    │   │   └── show_tests.rs           ← MISSING
    │   └── ... (30 total needed)
    │
    ├── integration/
    │   ├── mod.rs
    │   ├── ai_integration.rs           ← MISSING
    │   ├── graph_integration.rs        ← EXISTS
    │   ├── marketplace_e2e.rs          ← EXISTS
    │   ├── template_pipeline.rs        ← MISSING
    │   └── ... (15 total needed)
    │
    └── chicago_tdd/
        ├── marketplace/
        │   ├── install_tdd.rs          ← MISSING
        │   └── search_tdd.rs           ← MISSING
        └── ... (10 total needed)

Total gap: 48 missing test files
```

### ggen-cli Test Architecture

**Current**: Mix of clap integration tests
**Target**: Complete domain + CLI integration coverage

```
crates/ggen-cli/
├── src/
│   ├── lib.rs
│   └── cmds/
│       ├── ai.rs
│       ├── hook.rs
│       ├── marketplace.rs
│       ├── project.rs
│       ├── template.rs
│       └── utils.rs
│
└── tests/
    ├── integration/
    │   ├── mod.rs
    │   ├── ai_cmd_tests.rs             ← MISSING
    │   ├── hook_cmd_tests.rs           ← EXISTS
    │   ├── marketplace_cmd_tests.rs    ← EXISTS
    │   ├── project_cmd_tests.rs        ← MISSING
    │   ├── template_cmd_tests.rs       ← EXISTS
    │   └── utils_cmd_tests.rs          ← EXISTS
    │
    ├── domain/
    │   ├── ai/
    │   │   └── analyze_tests.rs        ← EXISTS
    │   ├── project/
    │   │   ├── init_tests.rs           ← EXISTS
    │   │   └── build_tests.rs          ← EXISTS
    │   └── utils/
    │       ├── doctor_tests.rs         ← EXISTS
    │       └── env_tests.rs            ← EXISTS
    │
    └── e2e/
        ├── complete_workflows.rs       ← MISSING
        └── production_scenarios.rs     ← MISSING
```

---

## CI/CD Integration Architecture

### Pre-Commit Hooks (Mandatory)

**File**: `.git/hooks/pre-commit` (auto-installed by `cargo make setup`)

```bash
#!/bin/bash
# Pre-commit hook: Enforce test coverage

echo "🔍 Pre-commit: Validating test coverage..."

# 1. Run quick unit tests
cargo test --lib --quiet || {
    echo "❌ Unit tests failed"
    exit 1
}

# 2. Check test coverage (fast check)
cargo tarpaulin --lib --skip-clean --timeout 30 --out Stdout | \
    grep "Coverage" | \
    awk '{if ($2 < 85) exit 1}'
if [ $? -ne 0 ]; then
    echo "❌ Coverage below 85%"
    exit 1
fi

# 3. Verify Chicago TDD compliance
grep -r "#\[tokio::test\]\|#\[test\]" crates/*/tests/*.rs && {
    echo "❌ Found non-Chicago TDD tests"
    echo "   Use 'test!' or 'async_test!' macros instead"
    exit 1
}

# 4. Check for hardcoded timeouts
grep -r "async_test_with_timeout!" crates/*/tests/*.rs | \
    grep -E "30|60|5" && {
    echo "⚠️  Found hardcoded timeouts - use integration_timeout() instead"
    echo "   (Warning only - not blocking)"
}

echo "✅ Pre-commit validation passed"
```

### GitHub Actions: Test Coverage Validation

**File**: `.github/workflows/test-coverage.yml`

```yaml
name: Test Coverage Validation

on:
  pull_request:
    branches: [master, develop]
  push:
    branches: [master]

jobs:
  validate-coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable

      - name: Install tarpaulin
        run: cargo install cargo-tarpaulin

      - name: Run tests with coverage
        run: |
          cargo tarpaulin --workspace --out Xml --timeout 300

      - name: Validate coverage thresholds
        run: |
          COVERAGE=$(grep -oP 'line-rate="\K[^"]+' cobertura.xml | head -1)
          COVERAGE_PERCENT=$(echo "$COVERAGE * 100" | bc)

          if (( $(echo "$COVERAGE_PERCENT < 85" | bc -l) )); then
            echo "❌ Coverage $COVERAGE_PERCENT% is below 85% threshold"
            exit 1
          fi

          echo "✅ Coverage $COVERAGE_PERCENT% meets threshold"

      - name: Validate per-crate coverage
        run: |
          # ggen-core must be >90%
          # ggen-domain must be >95%
          # ggen-cli must be >85%
          ./scripts/validate-crate-coverage.sh

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v3
        with:
          files: ./cobertura.xml
          fail_ci_if_error: true
```

### Nightly: Comprehensive Test Suite

**File**: `.github/workflows/nightly-comprehensive.yml`

```yaml
name: Nightly Comprehensive Tests

on:
  schedule:
    - cron: '0 2 * * *'  # 2 AM daily
  workflow_dispatch:

jobs:
  comprehensive:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run mutation testing
        run: |
          cargo install cargo-mutants
          cargo mutants --workspace --timeout 600

      - name: Run property tests (extended)
        run: |
          PROPTEST_CASES=10000 cargo test --features proptest

      - name: Run performance benchmarks
        run: |
          cargo bench --workspace

      - name: Security audit
        run: |
          cargo audit
          cargo deny check

      - name: Generate comprehensive report
        run: |
          ./scripts/generate-nightly-report.sh
```

---

## Test Organization Standards

### Directory Structure Template

**Every crate MUST follow this structure**:

```
crates/[crate-name]/
├── src/
│   ├── lib.rs
│   └── [modules]/
│
├── tests/
│   ├── unit/
│   │   ├── mod.rs
│   │   └── [module]_tests.rs      # One per src module
│   │
│   ├── integration/
│   │   ├── mod.rs
│   │   └── [feature]_integration.rs
│   │
│   ├── property/
│   │   ├── mod.rs
│   │   └── [module]_properties.rs
│   │
│   ├── security/
│   │   ├── mod.rs
│   │   └── [concern]_security.rs
│   │
│   ├── chicago_tdd/
│   │   ├── mod.rs
│   │   └── [module]_tdd.rs         # Exemplary TDD tests
│   │
│   └── common/
│       ├── mod.rs                  # Test config helpers
│       ├── fixtures.rs
│       └── test_helpers.rs
│
├── benches/
│   └── [module]_benchmarks.rs
│
└── tests/                          # Top-level integration tests
    ├── MODULE_TEST_CHECKLIST.md
    └── COVERAGE_REPORT.md
```

### File Naming Conventions

| Test Type | File Pattern | Example |
|-----------|--------------|---------|
| **Unit Tests** | `tests/unit/[module]_tests.rs` | `cache_tests.rs` |
| **Integration** | `tests/integration/[feature]_integration.rs` | `marketplace_integration.rs` |
| **Property** | `tests/property/[module]_properties.rs` | `template_properties.rs` |
| **Security** | `tests/security/[concern]_security.rs` | `injection_tests.rs` |
| **Chicago TDD** | `tests/chicago_tdd/[module]_tdd.rs` | `install_tdd.rs` |
| **E2E** | `tests/e2e/[workflow]_e2e.rs` | `complete_workflows.rs` |
| **Performance** | `benches/[module]_benchmarks.rs` | `cache_benchmarks.rs` |

---

## Test Quality Standards

### 1. Test Naming Convention

**Pattern**: `test_[action]_[condition]_[expected_outcome]`

```rust
test!(test_install_package_with_valid_name_succeeds, {
    // Arrange
    let package = "valid-package";

    // Act
    let result = install(package);

    // Assert
    assert_ok!(result);
});

async_test!(test_search_with_empty_query_returns_all_packages, async {
    // Arrange
    let query = "";

    // Act
    let results = search(query).await.unwrap();

    // Assert
    assert!(!results.is_empty());
});
```

### 2. Test Documentation

**Every test MUST have**:

```rust
/// Tests that installing a package with a valid name succeeds.
///
/// # Test Scenario
/// - Arrange: Create a valid package name
/// - Act: Attempt to install the package
/// - Assert: Installation succeeds without errors
///
/// # Chicago TDD
/// - Uses real registry (not mocked)
/// - Verifies final state (package installed)
/// - Uses config-based timeout
test!(test_install_package_with_valid_name_succeeds, {
    // Implementation
});
```

### 3. Test Data Management

**NO test data in root folder**:

```
✅ tests/fixtures/sample_package.toml
✅ tests/data/test_graph.ttl
✅ benches/data/large_template.tera

❌ test_package.toml  (in root)
❌ sample.ttl         (in root)
❌ fixture.json       (in root)
```

### 4. Test Independence

**Every test MUST be independent**:

```rust
async_test!(test_independent_example, async {
    // Arrange: Create isolated test environment
    let temp_dir = TempDir::new().unwrap();
    let registry = create_test_registry(&temp_dir).unwrap();

    // Act: Test with isolated registry
    let result = search("test", &registry).await.unwrap();

    // Assert: Verify isolated result
    assert_eq!(result.len(), 1);

    // Cleanup: Automatic via TempDir::drop
});
```

---

## Automated Test Generation

### Script: Generate Test Skeleton

**File**: `scripts/generate-test-skeleton.sh`

```bash
#!/bin/bash
# Generate test skeleton for a module

MODULE=$1
CRATE=$2

if [ -z "$MODULE" ] || [ -z "$CRATE" ]; then
    echo "Usage: $0 <module> <crate>"
    echo "Example: $0 cache ggen-core"
    exit 1
fi

# Create test directories
mkdir -p "crates/$CRATE/tests/unit"
mkdir -p "crates/$CRATE/tests/integration"
mkdir -p "crates/$CRATE/tests/property"

# Generate unit test skeleton
cat > "crates/$CRATE/tests/unit/${MODULE}_tests.rs" <<EOF
//! Unit tests for $MODULE module
//!
//! Chicago TDD: Real objects, state verification, AAA pattern

use chicago_tdd_tools::prelude::*;
use ${CRATE//-/_}::$MODULE::*;

test!(test_${MODULE}_creation_succeeds, {
    // Arrange
    let input = create_test_input();

    // Act
    let result = create($input);

    // Assert
    assert_ok!(result);
});

async_test!(test_${MODULE}_async_operation_completes, async {
    // Arrange
    let input = create_test_input();

    // Act
    let result = async_operation($input).await.unwrap();

    // Assert
    assert!(!result.is_empty());
});

// TODO: Add more tests following Chicago TDD patterns
EOF

echo "✅ Generated test skeleton: crates/$CRATE/tests/unit/${MODULE}_tests.rs"
echo ""
echo "Next steps:"
echo "1. Implement test cases following Chicago TDD"
echo "2. Run: cargo test --test ${MODULE}_tests"
echo "3. Update MODULE_TEST_CHECKLIST.md"
```

### Script: Validate Test Coverage

**File**: `scripts/validate-test-coverage.sh`

```bash
#!/bin/bash
# Validate that every module has required tests

CRATE=$1
if [ -z "$CRATE" ]; then
    echo "Usage: $0 <crate>"
    exit 1
fi

echo "🔍 Validating test coverage for $CRATE..."

# Find all modules
MODULES=$(find "crates/$CRATE/src" -name "*.rs" | \
    grep -v "lib.rs" | \
    sed 's|.*/||' | \
    sed 's|\.rs||')

MISSING_TESTS=0

for MODULE in $MODULES; do
    UNIT_TEST="crates/$CRATE/tests/unit/${MODULE}_tests.rs"

    if [ ! -f "$UNIT_TEST" ]; then
        echo "❌ Missing unit tests: $MODULE"
        MISSING_TESTS=$((MISSING_TESTS + 1))
    fi
done

if [ $MISSING_TESTS -eq 0 ]; then
    echo "✅ All modules have unit tests"
    exit 0
else
    echo "❌ $MISSING_TESTS modules missing tests"
    echo ""
    echo "Generate missing tests with:"
    echo "  ./scripts/generate-test-skeleton.sh <module> $CRATE"
    exit 1
fi
```

---

## Documentation Standards

### Per-Module Test Documentation

**File**: `crates/[crate]/tests/MODULE_TEST_CHECKLIST.md`

```markdown
# Module Test Checklist: [MODULE_NAME]

**Module**: crates/[crate]/src/[module]
**Owner**: [Team]
**Last Updated**: [Date]

## Coverage Status

| Test Type | Status | Coverage | Target | Gap |
|-----------|--------|----------|--------|-----|
| Unit Tests | ✅ | 92% | 85% | +7% |
| Integration Tests | ⚠️ | 72% | 75% | -3% |
| Property Tests | ✅ | N/A | Required | Complete |
| Security Tests | ❌ | 0% | 85% | -85% |

## Test Inventory

### Unit Tests (`tests/unit/[module]_tests.rs`)

- [x] test_creation_succeeds
- [x] test_with_invalid_input_fails
- [ ] test_edge_case_handling
- [ ] test_error_propagation

### Integration Tests (`tests/integration/[module]_integration.rs`)

- [x] test_full_workflow_succeeds
- [ ] test_cross_module_interaction
- [ ] test_api_contract_validation

### Property Tests (`tests/property/[module]_properties.rs`)

- [x] property_round_trip_consistency
- [ ] property_idempotency

## Chicago TDD Compliance

- [x] All tests use `test!` or `async_test!`
- [x] AAA pattern enforced
- [x] Real objects used
- [x] Config-based timeouts
- [ ] No hardcoded test data in root

## Action Items

1. [ ] Complete security tests
2. [ ] Add missing integration tests
3. [ ] Increase coverage to 85%
4. [ ] Update documentation
```

---

## Migration Strategy

### Phase 1: Foundation (Week 1-2)

**Immediate Actions**:

1. ✅ **Create architectural blueprint** (this document)
2. ⏳ **Setup automated validation scripts**:
   - `scripts/generate-test-skeleton.sh`
   - `scripts/validate-test-coverage.sh`
   - `scripts/validate-crate-coverage.sh`
3. ⏳ **Install pre-commit hooks**:
   - Add to `Makefile.toml` as `setup` target
   - Enforce Chicago TDD compliance
4. ⏳ **Create test templates**:
   - Unit test template
   - Integration test template
   - Module checklist template

### Phase 2: Critical Path (Week 3-4)

**Priority: ggen-domain (95% target coverage)**

1. ⏳ **Generate missing unit tests** (48 files):
   - `ai/analyze_tests.rs`
   - `ai/generate_tests.rs`
   - `ai/optimize_tests.rs`
   - `marketplace/install_tests.rs`
   - `marketplace/search_tests.rs`
   - `template/generate_tests.rs`
   - `template/lint_tests.rs`
   - ... (41 more)

2. ⏳ **Add integration tests** (15 files):
   - `ai_integration.rs`
   - `marketplace_e2e.rs`
   - `template_pipeline.rs`
   - ... (12 more)

### Phase 3: Core Infrastructure (Week 5-6)

**Priority: ggen-core (90% target coverage)**

1. ⏳ **Generate missing unit tests** (53 files):
   - `cache_tests.rs`
   - `cli_generator_tests.rs`
   - `config_tests.rs`
   - ... (50 more)

2. ⏳ **Add integration tests** (15 files)

### Phase 4: CLI Integration (Week 7)

**Priority: ggen-cli (85% target coverage)**

1. ⏳ **Complete CLI integration tests**
2. ⏳ **Add E2E workflow tests**

### Phase 5: Enforcement (Week 8)

**Enable strict validation**:

1. ⏳ **Enable CI coverage gates**:
   - ggen-domain: 95%
   - ggen-core: 90%
   - ggen-cli: 85%
2. ⏳ **Enable pre-push validation**:
   - Block commits below threshold
   - Enforce Chicago TDD compliance
3. ⏳ **Enable nightly comprehensive tests**:
   - Mutation testing
   - Property testing (10K cases)
   - Performance regression

---

## Success Metrics

### Coverage Targets (8 Weeks)

| Week | ggen-domain | ggen-core | ggen-cli | Overall |
|------|-------------|-----------|----------|---------|
| 0 (Now) | 27% | 50% | 70% | 49% |
| 2 | 40% | 55% | 75% | 57% |
| 4 | 70% | 65% | 80% | 72% |
| 6 | 85% | 75% | 85% | 82% |
| 8 | **95%** | **90%** | **85%** | **90%** |

### Quality Metrics (8 Weeks)

| Metric | Current | Target |
|--------|---------|--------|
| **Chicago TDD Adoption** | 20% | 100% |
| **Test Execution Time** | ~30s | <45s |
| **Mutation Score** | Unknown | >80% |
| **Module Coverage** | Ad-hoc | 100% |
| **Pre-commit Pass Rate** | N/A | 98% |

---

## Continuous Improvement

### Monthly Reviews

**Every month**:

1. ⏳ **Review coverage reports**:
   - Identify gaps
   - Update checklist
2. ⏳ **Update test standards**:
   - Incorporate lessons learned
   - Improve templates
3. ⏳ **Refactor slow tests**:
   - Optimize test runtime
   - Improve test parallelization

### Quarterly Audits

**Every quarter**:

1. ⏳ **Comprehensive mutation testing**
2. ⏳ **Performance regression analysis**
3. ⏳ **Security audit**
4. ⏳ **Test architecture review**

---

## Architectural Decisions

### ADR-001: Chicago TDD as Standard

**Decision**: All tests MUST use Chicago TDD patterns.

**Rationale**:
- Consistency prevents gaps
- Real objects improve reliability
- State verification catches more bugs
- Config-based timeouts improve maintainability

**Consequences**:
- Requires migration of existing tests
- Learning curve for new contributors
- Better test quality long-term

### ADR-002: Mandatory Test Coverage Thresholds

**Decision**: Enforce minimum coverage per crate (domain: 95%, core: 90%, cli: 85%).

**Rationale**:
- Critical domain logic requires highest coverage
- Core infrastructure requires high reliability
- CLI integration allows some flexibility

**Consequences**:
- Blocks PRs below threshold
- Requires comprehensive test suite
- Prevents gap accumulation

### ADR-003: Pre-Commit Validation

**Decision**: Enforce test validation at pre-commit stage.

**Rationale**:
- Prevention is better than detection
- Immediate feedback loop
- Reduces CI failures

**Consequences**:
- Slightly slower commit process
- Requires local validation tools
- Catches issues early

---

## Summary

This testing architecture provides a comprehensive framework that:

1. **Prevents Gaps**: Systematic enforcement at every level
2. **Enforces Quality**: Chicago TDD, coverage thresholds, automated validation
3. **Scales Efficiently**: Template-based generation, automated scripts
4. **Maintains Consistency**: Standards, checklists, documentation

**Next Steps**: Execute Phase 1 (Foundation) to enable automated validation and test generation.

---

**Architecture Status**: ✅ **Complete**
**Implementation Status**: ⏳ **Phase 1 In Progress**
**Target Completion**: 8 weeks
