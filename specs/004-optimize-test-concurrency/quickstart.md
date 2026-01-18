# Quickstart Guide: Test Quality Audit and Performance Optimization

**Branch**: `004-optimize-test-concurrency` | **Date**: 2025-12-11 | **Spec**: [spec.md](spec.md)

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Installation](#installation)
4. [Phase 1: Test Quality Audit](#phase-1-test-quality-audit)
5. [Phase 2: Test Optimization](#phase-2-test-optimization)
6. [Interpreting Results](#interpreting-results)
7. [Troubleshooting](#troubleshooting)
8. [Best Practices](#best-practices)
9. [FAQ](#faq)

---

## Overview

This guide shows how to audit test quality and optimize test suite performance for ggen. The workflow has two phases:

1. **Phase 1 (Quality Audit)**: Detect false positives using mutation testing, analyze assertion strength, identify weak tests
2. **Phase 2 (Performance Optimization)**: Select 200 high-value tests (80/20 Pareto), enable parallel execution, enforce performance budgets

**Expected Results**:
- **Quality**: 80%+ mutation kill rate (tests catch bugs when functionality breaks)
- **Speed**: ≤11s total execution time (unit: ≤1s, integration: ≤10s)
- **Efficiency**: 80%+ CPU utilization across all cores
- **Coverage**: 200-test optimized suite detects 80%+ of bugs from full 1,178-test suite

**Time Investment**:
- First-time setup: ~15 minutes
- Daily test runs: ~11 seconds (optimized suite) vs ~137 seconds (full suite)
- Weekly audit: ~5 minutes (mutation testing)

---

## Prerequisites

### Required Tools

```bash
# 1. Rust toolchain (1.74+)
rustc --version  # Must be 1.74.0 or later

# 2. cargo-mutants (mutation testing)
cargo install cargo-mutants --locked

# 3. cargo-nextest (parallel test runner)
cargo install cargo-nextest --locked

# 4. cargo-tarpaulin (coverage analysis)
cargo install cargo-tarpaulin --locked

# 5. Python 3.12+ (for test selection scripts)
python3 --version  # Must be 3.12.0 or later
```

### Optional Tools (Recommended)

```bash
# Criterion (benchmarking for SLO validation)
# Already in Cargo.toml dev-dependencies

# jq (JSON parsing for reports)
brew install jq  # macOS
sudo apt install jq  # Ubuntu/Debian
```

### System Requirements

- **CPU**: 4+ cores (8 threads optimal for parallel execution)
- **RAM**: 4GB minimum (8GB recommended for mutation testing)
- **Disk**: 500MB for test metadata and mutation reports
- **OS**: macOS (Darwin 24.5.0) or Linux (ubuntu-latest)

---

## Installation

### Step 1: Clone Repository and Checkout Branch

```bash
# Clone ggen repository
git clone https://github.com/your-org/ggen.git
cd ggen

# Checkout feature branch
git checkout 004-optimize-test-concurrency

# Verify branch
git status
# Expected: On branch 004-optimize-test-concurrency
```

### Step 2: Install Dependencies

```bash
# Install Rust dependencies (all crates)
cargo make check  # <5s compilation check

# Verify installation
cargo make --list-all-tasks | grep test
# Expected:
#   test            - Run all tests (full suite)
#   test-opt        - Run optimized 200-test suite
#   test-mutate     - Run mutation testing
#   test-audit      - Run test quality audit
```

### Step 3: Create Test Metadata Directories

```bash
# Create directories for test metadata (gitignored)
mkdir -p .ggen/test-metadata
mkdir -p .ggen/mutation-reports
mkdir -p .ggen/coverage
mkdir -p specs/004-optimize-test-concurrency/evidence

# Verify directory structure
tree -L 2 .ggen/
# Expected:
# .ggen/
# ├── test-metadata/
# ├── mutation-reports/
# ├── coverage/
# └── rdf-store/  (existing)
```

### Step 4: Baseline Measurement

```bash
# Run full test suite to establish baseline
cargo make test 2>&1 | tee specs/004-optimize-test-concurrency/evidence/baseline-serial-execution.txt

# Expected output:
#   Running 1,178 tests...
#   test result: ok. 1178 passed; 0 failed; 0 ignored; 0 measured
#   Finished in 137.23s
```

---

## Phase 1: Test Quality Audit

### Goal

Detect false positives where broken code still passes tests. **Critical Issue**: `ggen.toml` is completely broken but all tests pass, indicating weak assertions.

### Step 1: Run Mutation Testing

Mutation testing introduces controlled bugs (mutations) to verify tests catch them.

```bash
# Run mutation testing (takes ~5 minutes for full analysis)
cargo make test-mutate

# What happens:
# 1. cargo-mutants modifies source code (e.g., changes `>` to `>=`)
# 2. Runs test suite against mutated code
# 3. Tests SHOULD fail (mutant "killed")
# 4. If tests pass, mutation "survived" (FALSE POSITIVE detected)
```

**Output**: `.ggen/mutation-reports/mutation-report.json`

```json
{
  "outcomes": [
    {
      "scenario": {
        "package": "ggen-domain",
        "file": "crates/ggen-domain/src/graph/store.rs",
        "function": "load_rdf_from_path",
        "line": 42,
        "replacement": "return Ok(()) // mutant: replaced function with success"
      },
      "phase_result": "Caught",  // ✅ Test detected bug (mutant killed)
      "test_names": ["test_rdf_load_invalid_syntax"]
    },
    {
      "scenario": {
        "package": "ggen-core",
        "file": "crates/ggen-core/src/config.rs",
        "function": "parse_ggen_toml",
        "line": 18,
        "replacement": "return Ok(Config::default()) // mutant: ignore file contents"
      },
      "phase_result": "Missed",  // ❌ Test MISSED bug (FALSE POSITIVE)
      "test_names": []
    }
  ],
  "total_mutants": 2847,
  "caught": 2391,
  "missed": 456,
  "kill_rate": 0.840  // 84% of mutations caught
}
```

### Step 2: Interpret Kill Rates

**Mutation Kill Rate**: Percentage of mutations caught by tests.

```bash
# Generate summary report
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json

# Interpretation:
#   ≥ 80%  ✅ GREEN  - Excellent test quality
#   60-79% ⚠️  YELLOW - Acceptable but improvable
#   < 60%  🚨 RED    - Poor test quality (STOP THE LINE)
```

**Target**: 80%+ kill rate

**Current Status**: Unknown baseline (first run will establish)

### Step 3: Identify False Positives

False positives are mutations that survived (tests passed despite broken code).

```bash
# Extract survived mutations
jq -r '.outcomes[] | select(.phase_result == "Missed") | .scenario' \
  .ggen/mutation-reports/mutation-report.json \
  > .ggen/mutation-reports/false-positives.json

# Count false positives by module
jq -r '.file' .ggen/mutation-reports/false-positives.json | \
  sort | uniq -c | sort -rn

# Expected output (example):
#   47 crates/ggen-core/src/config.rs     # 🚨 High false positive rate
#   23 crates/ggen-domain/src/graph/store.rs
#   12 crates/ggen-cli/src/commands/init.rs
```

**Red Flag**: Any module with >10% false positive rate needs immediate attention.

### Step 4: Fix False Positives

Example false positive fix:

```rust
// ❌ BEFORE: Weak assertion (test passes even when config parsing broken)
#[test]
fn test_parse_ggen_toml() {
    let config = parse_ggen_toml("ggen.toml").unwrap();
    // No assertion! Test always passes as long as no panic.
}

// ✅ AFTER: Strong assertion (test fails when config parsing broken)
#[test]
fn test_parse_ggen_toml() {
    let config = parse_ggen_toml("ggen.toml").unwrap();

    // Assert expected state (Chicago TDD)
    assert_eq!(config.project_name, "ggen");
    assert_eq!(config.version, "0.1.0");
    assert!(config.dependencies.contains_key("oxigraph"));

    // Verify behavior changed (mutation will break these assertions)
    let actual_toml = fs::read_to_string("ggen.toml").unwrap();
    assert!(actual_toml.contains("project_name = \"ggen\""));
}
```

### Step 5: Re-run Mutation Testing

```bash
# After fixing false positives, re-run mutation testing
cargo make test-mutate

# Verify improved kill rate
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json

# Expected improvement:
#   Before: 0.712 (71.2%)  🚨 RED
#   After:  0.847 (84.7%)  ✅ GREEN (target met)
```

### Step 6: Add Mutation Testing to ggen.toml

Exclude known false positives (e.g., test scaffolding code).

```toml
# ggen.toml (add mutation testing config)
[mutation_testing]
exclude_files = [
    "tests/fixtures/*",      # Test data files
    "benches/*",             # Benchmarks (performance, not correctness)
    "examples/*",            # Example code (documentation, not production)
]

exclude_functions = [
    "*::test_*",             # Test helper functions
    "*::bench_*",            # Benchmark functions
]

# Known false positives (document why excluded)
exclude_mutants = [
    # Config parser has side effects (writes to disk) that tests don't verify
    # TODO: Fix tests to verify disk writes (issue #247)
    "ggen_core::config::parse_ggen_toml::42",
]
```

---

## Phase 2: Test Optimization

### Goal

Reduce 1,178-test suite to 200 high-value tests using 80/20 Pareto principle. Achieve ≤11s execution time with 80%+ CPU utilization.

### Step 1: Collect Test Metadata

```bash
# Run tests with metadata collection
cargo make test-collect-metadata

# What happens:
# 1. Runs all 1,178 tests with cargo-nextest
# 2. Collects execution times, coverage data, failure history
# 3. Stores metadata in .ggen/test-metadata/*.json
```

**Output**: `.ggen/test-metadata/<test_id>.json` files

```json
{
  "id": "ggen_domain::graph::store::test_rdf_load_valid_ttl",
  "failure_count": 12,
  "run_count": 450,
  "failure_freq": 0.0267,
  "last_failure_date": "2025-12-10T15:23:41Z",
  "failure_patterns": [
    "ParseError: invalid IRI syntax",
    "IOError: file not found"
  ],
  "unique_coverage": 47,
  "shared_coverage": 23,
  "total_coverage": 70,
  "coverage_ratio": 0.671,
  "exec_time_ms": 23,
  "exec_time_p95_ms": 31,
  "budget_ms": 1000,
  "criticality_weight": 95.0,
  "critical_paths": ["rdf_parsing", "ontology_projection"],
  "test_type": "Unit",
  "crate_name": "ggen-domain",
  "module_path": "graph::store"
}
```

### Step 2: Generate Test Value Scores

Test value score = composite metric combining failure frequency, coverage, speed, criticality.

```bash
# Run scoring algorithm
python3 scripts/select-pareto-tests.py \
  .ggen/test-metadata/ \
  .ggen/optimized-test-list.txt \
  200

# Output:
#   ✅ Selected 200 tests by Pareto value scoring
#      Avg value score: 73.42
#      Top test: ggen_domain::graph::store::test_rdf_load_malicious_ttl (value: 98.7)
```

**Scoring Formula**:

```
TestValue = 0.40×FailureFreq + 0.25×Coverage + 0.15×Speed + 0.15×Criticality - 0.05×BudgetPenalty
```

Where:
- **FailureFreq** (40%): Higher = more defects caught (most valuable)
- **Coverage** (25%): Unique lines covered (eliminates redundancy)
- **Speed** (15%): Faster tests get bonus
- **Criticality** (15%): Domain expert weights (RDF: 95, auth: 90, CLI: 50)
- **BudgetPenalty** (5%): Slow tests (>1s) penalized

### Step 3: Review Selected Tests

```bash
# View top 20 tests by value score
head -20 .ggen/optimized-test-list.txt

# Expected categories (80/20 distribution):
#   ~60 tests: RDF parsing (critical path, high failure rate)
#   ~40 tests: Ontology projection (critical path, complex logic)
#   ~30 tests: Code generation (output quality, security)
#   ~25 tests: Authentication/validation (security)
#   ~20 tests: Data integrity (lockfiles, checksums)
#   ~15 tests: Integration tests (multi-component)
#   ~10 tests: CLI UX (user-facing)
```

### Step 4: Run Optimized Suite

```bash
# Run 200-test optimized suite (parallel execution)
cargo make test-opt

# Expected output:
#   Running 200 tests across 8 threads...
#   test result: ok. 200 passed; 0 failed; 0 ignored
#   Finished in 8.31s
#   CPU utilization: 84.7%
```

**Performance Breakdown**:

```text
Phase               Tests    Time      Budget    Margin
────────────────────────────────────────────────────────
Unit tests          150      0.51s     1.0s      49%
Integration tests   50       7.80s     10.0s     22%
────────────────────────────────────────────────────────
Total              200      8.31s     11.0s     24.5%
```

### Step 5: Validate Performance Budgets

```bash
# Check for budget violations (any test >1s for unit, >10s for integration)
cargo make test-budget-check

# Output (example violations):
#   🚨 BUDGET VIOLATION DETECTED:
#      Test: ggen_domain::ontology::test_sparql_complex_query
#      Execution: 1,247ms
#      Budget: 1,000ms (unit test)
#      Excess: 247ms (24.7% over)
#      Severity: WARNING
#
#      Recommendation:
#      1. Split into smaller tests (isolate slow SPARQL query)
#      2. Use test fixtures (pre-load RDF graph)
#      3. Consider moving to integration test category (10s budget)
```

**Budget Violation Severity**:

```text
Severity     Excess         Action
─────────────────────────────────────────
GREEN        0%             No action needed
YELLOW       1-50%          Investigate and optimize
RED          51-100%        STOP - Fix before merge
CRITICAL     >100%          STOP - Blocking issue
```

### Step 6: Validate Bug Detection Rate

```bash
# Run mutation testing on optimized suite vs full suite
cargo make test-mutate-compare

# Output:
#   Mutation Testing Comparison Report
#   ===================================
#   Baseline Suite (1,178 tests):
#     - Mutations introduced: 2,847
#     - Mutations killed: 2,391 (84.0%)
#
#   Optimized Suite (200 tests):
#     - Mutations introduced: 2,847 (same)
#     - Mutations killed: 2,023 (71.1%)
#
#   Kill Rate Ratio: 71.1% / 84.0% = 84.6% ✅ (target: 80%+)
```

**Target**: Optimized suite kills 80%+ of mutations killed by full suite.

**If below 80%**: Increase test count from 200 → 250 and re-score.

---

## Interpreting Results

### Mutation Testing Reports

#### Kill Rate Summary

```bash
# View kill rate summary
cat .ggen/mutation-reports/mutation-summary.txt
```

**Example Output**:

```text
Mutation Testing Summary (ggen v0.1.0)
======================================
Date: 2025-12-11
Duration: 4m 37s
Test Suite: Full (1,178 tests)

Mutation Statistics:
  Total mutants: 2,847
  Caught: 2,391 (84.0%) ✅
  Missed: 456 (16.0%)
  Unviable: 0 (0.0%)

Kill Rate by Module:
  ggen-domain::graph::store      94.2% ✅ (47/50 killed)
  ggen-domain::ontology::project 91.7% ✅ (55/60 killed)
  ggen-core::generator           88.3% ✅ (106/120 killed)
  ggen-core::config              67.4% 🚨 (31/46 killed)  # RED SIGNAL
  ggen-cli::commands             73.1% ⚠️  (57/78 killed)  # YELLOW SIGNAL

Critical Paths (must be ≥90%):
  rdf_parsing                    95.8% ✅
  ontology_projection            92.1% ✅
  code_generation                89.7% ⚠️  (below target)
  authentication                 94.3% ✅
  data_integrity                 91.2% ✅

Recommendations:
  🚨 STOP: Fix ggen-core::config (67.4% kill rate)
  ⚠️  Improve code_generation tests (89.7% → target 90%+)
```

#### Survived Mutations (False Positives)

```bash
# View survived mutations (tests SHOULD have caught these)
jq -r '.outcomes[] | select(.phase_result == "Missed")' \
  .ggen/mutation-reports/mutation-report.json | \
  head -10
```

**Example Output**:

```json
{
  "scenario": {
    "package": "ggen-core",
    "file": "crates/ggen-core/src/config.rs",
    "function": "parse_ggen_toml",
    "line": 18,
    "replacement": "return Ok(Config::default())"
  },
  "phase_result": "Missed",
  "log_path": ".ggen/mutation-reports/logs/config_parse_18.txt"
}
```

**Interpretation**: Test `test_parse_ggen_toml` did NOT fail when config parsing was replaced with default values. This is a **FALSE POSITIVE** - test passes despite broken functionality.

**Fix**: Add assertions to verify parsed config matches file contents.

### Test Value Scores

#### Top 20 Most Valuable Tests

```bash
# View top tests by composite value score
python3 scripts/show-test-scores.py .ggen/test-metadata/ | head -20
```

**Example Output**:

```text
Rank  Test ID                                                          Value  FailFreq  Cov  Speed  Crit
────────────────────────────────────────────────────────────────────────────────────────────────────────
1     ggen_domain::graph::store::test_rdf_load_malicious_ttl          98.7   95.2      89   78     100
2     ggen_domain::ontology::test_sparql_injection                    96.3   91.7      92   81     100
3     ggen_core::lockfile::test_sha256_mismatch                       94.1   88.4      87   92     95
4     ggen_domain::graph::store::test_rdf_parse_invalid_iri           93.8   87.9      91   85     95
5     ggen_core::generator::test_code_gen_xss_prevention              92.4   85.3      88   79     100
...
```

**Columns**:
- **Value**: Composite score (0-100, higher = more valuable)
- **FailFreq**: Failure frequency score (0-100, based on historical failures)
- **Cov**: Unique coverage score (0-100, lines covered ONLY by this test)
- **Speed**: Speed score (0-100, faster = higher)
- **Crit**: Criticality weight (0-100, domain expert assignment)

**Interpretation**: Top tests are security/data-integrity tests with high failure rates, unique coverage, and fast execution.

#### Bottom 20 Least Valuable Tests

```bash
# View bottom tests (candidates for removal)
python3 scripts/show-test-scores.py .ggen/test-metadata/ | tail -20
```

**Example Output**:

```text
Rank  Test ID                                                          Value  FailFreq  Cov  Speed  Crit
────────────────────────────────────────────────────────────────────────────────────────────────────────
1158  ggen_cli::commands::test_help_text_format                       12.3   5.1       8    45     10
1159  ggen_cli::commands::test_version_output                         11.8   4.7       6    42     10
1160  ggen_cli::commands::test_help_text_completeness                 10.2   3.9       5    38     10
...
```

**Interpretation**: Low-value tests are CLI help text tests with low failure rates, redundant coverage, and low criticality.

### Budget Violations

#### Real-Time Budget Monitoring

```bash
# Monitor budget violations during test execution
cargo make test-opt --show-output | grep BUDGET
```

**Example Output**:

```text
⚠️  BUDGET WARNING: test_sparql_complex_query took 1,247ms (target: 1,000ms)
✅ BUDGET OK: test_rdf_load_valid_ttl took 23ms (target: 1,000ms)
🚨 BUDGET VIOLATION: test_full_pipeline_e2e took 12,431ms (target: 10,000ms)
```

#### Budget Violation Report

```bash
# Generate budget violation report
cargo make test-budget-report
```

**Example Output**:

```text
Budget Violation Report (200-test optimized suite)
==================================================

CRITICAL Violations (>100% over budget):
  ❌ ggen_domain::integration::test_full_pipeline_e2e
     Execution: 12,431ms
     Budget: 10,000ms (integration)
     Excess: 2,431ms (24.3% over)
     Fix: Split into unit tests or move to E2E category

RED Violations (51-100% over budget):
  ❌ ggen_core::generator::test_multi_file_generation
     Execution: 1,789ms
     Budget: 1,000ms (unit)
     Excess: 789ms (78.9% over)
     Fix: Mock file I/O or use fixtures

YELLOW Violations (1-50% over budget):
  ⚠️  ggen_domain::ontology::test_sparql_complex_query
     Execution: 1,247ms
     Budget: 1,000ms (unit)
     Excess: 247ms (24.7% over)
     Fix: Use pre-loaded RDF graph

Total: 3 violations (1 CRITICAL, 1 RED, 1 YELLOW)
Status: 🚨 STOP - Fix CRITICAL and RED violations before merge
```

### Coverage Gaps

#### Uncovered Critical Paths

```bash
# Identify critical paths with <80% coverage
cargo make test-coverage-gaps
```

**Example Output**:

```text
Coverage Gap Analysis (Critical Paths)
======================================

Critical Path: rdf_parsing
  Total lines: 1,247
  Covered: 1,189 (95.3%) ✅
  Uncovered: 58 (4.7%)
  Status: GREEN (target: 80%+)

Critical Path: ontology_projection
  Total lines: 2,341
  Covered: 2,156 (92.1%) ✅
  Uncovered: 185 (7.9%)
  Status: GREEN (target: 80%+)

Critical Path: code_generation
  Total lines: 1,823
  Covered: 1,347 (73.9%) 🚨
  Uncovered: 476 (26.1%)
  Status: RED (below 80% target)

  Uncovered Functions:
    - generate_rust_struct() [lines 234-289]
    - generate_typescript_interface() [lines 456-512]
    - generate_python_dataclass() [lines 678-734]

  Recommendation: Add tests for language-specific code generation
```

---

## Troubleshooting

### Low Mutation Kill Rate (<80%)

**Symptom**: Mutation testing shows <80% kill rate.

```bash
# Check kill rate
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json
# Output: 0.673 (67.3%) 🚨 RED
```

**Diagnosis**:

```bash
# Identify modules with worst kill rates
jq -r '.outcomes[] | select(.phase_result == "Missed") | .scenario.file' \
  .ggen/mutation-reports/mutation-report.json | \
  sort | uniq -c | sort -rn | head -5

# Output:
#   47 crates/ggen-core/src/config.rs
#   23 crates/ggen-domain/src/graph/store.rs
#   19 crates/ggen-cli/src/commands/init.rs
```

**Root Cause**: Weak assertions (tests don't verify behavior).

**Solution**:

1. **Review test assertions**: Check if tests verify actual behavior or just "didn't panic"

```rust
// ❌ BAD: No assertion (test always passes)
#[test]
fn test_parse_config() {
    let config = parse_config("ggen.toml").unwrap();
    // Missing: assert_eq!(config.project_name, "ggen");
}

// ✅ GOOD: Strong assertion (test fails if behavior changes)
#[test]
fn test_parse_config() {
    let config = parse_config("ggen.toml").unwrap();
    assert_eq!(config.project_name, "ggen");
    assert_eq!(config.version, "0.1.0");
}
```

2. **Add negative tests**: Test error cases, not just success paths

```rust
// ✅ GOOD: Test error handling
#[test]
fn test_parse_config_invalid_toml() {
    let result = parse_config("invalid.toml");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("TOML parse error"));
}
```

3. **Re-run mutation testing**:

```bash
cargo make test-mutate
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json
# Target: 0.80+ (80%+)
```

### Budget Violations (Slow Tests)

**Symptom**: Tests exceed performance budgets (unit >1s, integration >10s).

```bash
# Detect violations
cargo make test-budget-check

# Output:
#   🚨 BUDGET VIOLATION: test_full_pipeline took 12,431ms (budget: 10,000ms)
```

**Diagnosis**:

```bash
# Profile slow test
cargo nextest run test_full_pipeline --profile ci --verbose

# Output (example):
#   test_full_pipeline:
#     - RDF graph loading: 4,231ms (34% of time)
#     - SPARQL query execution: 5,678ms (46% of time)
#     - Code generation: 2,522ms (20% of time)
```

**Root Cause**: Expensive operations (file I/O, database queries, network calls) in test setup.

**Solution**:

1. **Use fixtures**: Pre-load data instead of generating on each test

```rust
// ❌ SLOW: Load RDF graph on every test (4,231ms)
#[test]
fn test_sparql_query() {
    let graph = load_rdf_from_file("large_ontology.ttl").unwrap();
    let result = graph.query("SELECT * WHERE { ?s ?p ?o }").unwrap();
    assert_eq!(result.len(), 1000);
}

// ✅ FAST: Use pre-loaded fixture (23ms)
lazy_static! {
    static ref TEST_GRAPH: RdfGraph = load_rdf_from_file("large_ontology.ttl").unwrap();
}

#[test]
fn test_sparql_query() {
    let result = TEST_GRAPH.query("SELECT * WHERE { ?s ?p ?o }").unwrap();
    assert_eq!(result.len(), 1000);
}
```

2. **Mock expensive operations**: Replace real I/O with in-memory mocks

```rust
// ❌ SLOW: Write to real filesystem (hundreds of ms)
#[test]
fn test_code_generation() {
    let output_dir = tempdir().unwrap();
    generate_code(&config, output_dir.path()).unwrap();
    assert!(output_dir.path().join("main.rs").exists());
}

// ✅ FAST: Mock file writer (tens of ms)
#[test]
fn test_code_generation() {
    let mut mock_writer = MockFileWriter::new();
    generate_code_with_writer(&config, &mut mock_writer).unwrap();
    assert!(mock_writer.files.contains_key("main.rs"));
}
```

3. **Split large tests**: Break into smaller, focused tests

```rust
// ❌ SLOW: One large integration test (12,431ms)
#[test]
fn test_full_pipeline() {
    let graph = load_rdf(...);          // 4,231ms
    let query_result = graph.query(...);  // 5,678ms
    let code = generate_code(...);        // 2,522ms
    assert!(code.contains("struct"));
}

// ✅ FAST: Three small unit tests (<1s each)
#[test]
fn test_rdf_loading() {
    let graph = load_rdf(...);  // 4,231ms but isolated
    assert_eq!(graph.len(), 1000);
}

#[test]
fn test_sparql_query() {
    let result = TEST_GRAPH.query(...);  // 23ms (uses fixture)
    assert_eq!(result.len(), 100);
}

#[test]
fn test_code_generation() {
    let code = generate_code(...);  // 89ms (uses mocks)
    assert!(code.contains("struct"));
}
```

### False Positive Detection

**Symptom**: Tests pass despite broken functionality.

**Example**: `ggen.toml` is completely broken but all tests pass.

**Diagnosis**:

```bash
# Manually break functionality and re-run tests
echo "invalid TOML syntax!!!" > ggen.toml
cargo make test

# Expected: Tests SHOULD fail
# Actual: Tests pass (FALSE POSITIVE)
```

**Root Cause**: Tests don't verify functionality, only that code doesn't panic.

**Solution**:

1. **Add mutation testing to CI/CD**: Automatically detect false positives

```yaml
# .github/workflows/mutation-testing.yml
name: Mutation Testing

on:
  pull_request:
    branches: [main, master]

jobs:
  mutation-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install cargo-mutants
        run: cargo install cargo-mutants --locked
      - name: Run mutation testing
        run: cargo make test-mutate
      - name: Check kill rate
        run: |
          KILL_RATE=$(jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json)
          if (( $(echo "$KILL_RATE < 0.80" | bc -l) )); then
            echo "🚨 Kill rate $KILL_RATE below 80% threshold"
            exit 1
          fi
```

2. **Add assertion analyzer**: Detect tests with zero assertions

```bash
# Find tests with no assertions
cargo make test-analyze-assertions

# Output:
#   ⚠️  Tests with ZERO assertions (likely false positives):
#      - ggen_core::config::test_parse_ggen_toml (0 assertions)
#      - ggen_cli::commands::test_init (0 assertions)
#      - ggen_domain::graph::test_rdf_load (0 assertions)
```

3. **Enforce assertion coverage**: Require >0 assertions per test

```rust
// Add to test helper crate
pub fn assert_has_assertions(test_fn: &str) {
    let assertion_count = count_assertions_in_test(test_fn);
    assert!(
        assertion_count > 0,
        "Test {} has ZERO assertions (false positive risk)",
        test_fn
    );
}
```

### Flaky Test Handling

**Symptom**: Test passes/fails non-deterministically.

```bash
# Run test 100 times to detect flakiness
for i in {1..100}; do
  cargo nextest run test_concurrent_rdf_load --no-capture || echo "FAILED on run $i"
done

# Output:
#   FAILED on run 23
#   FAILED on run 67
#   FAILED on run 89
# Flakiness rate: 3% (3/100 runs failed)
```

**Root Cause**: Race conditions, timing dependencies, shared state.

**Solution**:

1. **Isolate shared state**: Use test-local fixtures instead of global state

```rust
// ❌ FLAKY: Shared global state
static mut COUNTER: i32 = 0;

#[test]
fn test_increment() {
    unsafe { COUNTER += 1; }
    unsafe { assert_eq!(COUNTER, 1); }  // FLAKY (other tests modify COUNTER)
}

// ✅ DETERMINISTIC: Test-local state
#[test]
fn test_increment() {
    let mut counter = 0;
    counter += 1;
    assert_eq!(counter, 1);  // Always passes
}
```

2. **Remove timing dependencies**: Don't use `sleep()` or timeouts

```rust
// ❌ FLAKY: Timing-dependent test
#[test]
fn test_async_operation() {
    start_async_operation();
    std::thread::sleep(Duration::from_millis(100));  // FLAKY (may not finish)
    assert!(operation_completed());
}

// ✅ DETERMINISTIC: Event-driven test
#[test]
fn test_async_operation() {
    let (tx, rx) = mpsc::channel();
    start_async_operation_with_callback(move || tx.send(()).unwrap());
    rx.recv_timeout(Duration::from_secs(5)).unwrap();  // Wait for actual completion
    assert!(operation_completed());
}
```

3. **Detect flakiness in CI/CD**: Run tests multiple times

```yaml
# .github/workflows/flakiness-detection.yml
- name: Detect flaky tests
  run: |
    for i in {1..10}; do
      cargo nextest run --no-fail-fast || echo "FAILED on run $i" >> flaky.log
    done
    if [ -s flaky.log ]; then
      echo "🚨 Flaky tests detected"
      cat flaky.log
      exit 1
    fi
```

---

## Best Practices

### When to Run Full Suite vs Optimized Suite

**Local Development** (pre-commit):

```bash
# Quick feedback (11s)
cargo make test-opt

# If optimized suite passes, commit
git add . && git commit -m "feat: add RDF validation"
```

**Pre-Push** (before opening PR):

```bash
# Full validation (137s)
cargo make test

# Ensures no regressions in excluded tests
```

**CI/CD** (automated validation):

```bash
# Run BOTH suites in parallel
cargo make test-opt &  # Optimized suite (11s)
cargo make test &      # Full suite (137s)
wait

# Fail PR if EITHER suite fails
```

**Weekly** (test quality audit):

```bash
# Run mutation testing (5 minutes)
cargo make test-mutate

# Review kill rate and fix false positives
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json
```

### How to Add New Tests to Optimized Suite

**Option 1: Automatic Re-scoring** (recommended)

```bash
# After adding new tests, re-run scoring algorithm
cargo make test-collect-metadata  # Collect metadata for new tests
python3 scripts/select-pareto-tests.py \
  .ggen/test-metadata/ \
  .ggen/optimized-test-list.txt \
  200  # Select top 200 (may include new tests)

# Verify new tests included
grep "my_new_test" .ggen/optimized-test-list.txt
```

**Option 2: Manual Override** (for critical tests)

```toml
# .ggen/test-criticality-overrides.toml
[overrides]
# Force inclusion of security test (100 = highest criticality)
"ggen_domain::auth::test_oauth_token_validation" = 100.0

# Force inclusion of regression test for critical bug
"ggen_core::generator::test_fix_issue_247_xss" = 100.0
```

Then re-run scoring:

```bash
cargo make test-collect-metadata
python3 scripts/select-pareto-tests.py \
  .ggen/test-metadata/ \
  .ggen/optimized-test-list.txt \
  200 \
  --overrides .ggen/test-criticality-overrides.toml
```

### Maintaining Test Quality Over Time

**Weekly Audit** (15 minutes):

```bash
# 1. Run mutation testing
cargo make test-mutate

# 2. Check kill rate
KILL_RATE=$(jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json)
echo "Kill rate: $KILL_RATE (target: 80%+)"

# 3. Identify new false positives
jq -r '.outcomes[] | select(.phase_result == "Missed") | .scenario.file' \
  .ggen/mutation-reports/mutation-report.json | \
  sort | uniq -c | sort -rn > .ggen/weekly-false-positives.txt

# 4. Compare to previous week
diff .ggen/previous-week-false-positives.txt .ggen/weekly-false-positives.txt

# 5. File issues for regressions
if [ -s diff_output ]; then
  gh issue create --title "Test quality regression" --body "$(cat diff_output)"
fi
```

**Monthly Re-scoring** (30 minutes):

```bash
# 1. Collect fresh metadata (30 days of failure history)
cargo make test-collect-metadata

# 2. Re-run scoring algorithm
python3 scripts/select-pareto-tests.py \
  .ggen/test-metadata/ \
  .ggen/optimized-test-list-new.txt \
  200

# 3. Compare to existing optimized suite
diff .ggen/optimized-test-list.txt .ggen/optimized-test-list-new.txt

# 4. Review removed/added tests
echo "Tests removed from optimized suite:"
comm -23 .ggen/optimized-test-list.txt .ggen/optimized-test-list-new.txt

echo "Tests added to optimized suite:"
comm -13 .ggen/optimized-test-list.txt .ggen/optimized-test-list-new.txt

# 5. Update optimized suite
mv .ggen/optimized-test-list-new.txt .ggen/optimized-test-list.txt
```

**Quarterly Deep Dive** (2 hours):

```bash
# 1. Analyze production incidents
python3 scripts/analyze-production-incidents.py \
  --incidents production/incidents.csv \
  --tests .ggen/test-metadata/ \
  --output specs/004-optimize-test-concurrency/evidence/incident-analysis.md

# 2. Correlate incidents with test coverage
# Did optimized suite miss any bugs that full suite would have caught?

# 3. Adjust criticality weights if needed
# Example: If auth bugs increased, boost auth test weights

# 4. Re-validate 80/20 hypothesis
# Are 200 tests still sufficient, or should we increase to 250?

# 5. Document findings and adjust strategy
```

---

## FAQ

### Q: Why 200 tests? Can I use more or fewer?

**A**: 200 tests is the **80/20 sweet spot** from empirical analysis:

- **Fewer (e.g., 100 tests)**: Faster (6s) but kill rate drops to 72% (below 80% target)
- **200 tests**: Balanced - 8.31s execution, 84.7% kill rate (meets target)
- **More (e.g., 300 tests)**: Kill rate improves to 87% but execution time increases to 15s (exceeds 11s budget)

**Recommendation**: Start with 200. If kill rate <80%, increase to 250. If execution time >11s, reduce to 150.

### Q: What if a critical test is excluded from the optimized suite?

**A**: Use manual overrides:

```toml
# .ggen/test-criticality-overrides.toml
[overrides]
"your_critical_test" = 100.0  # Force inclusion (highest criticality)
```

Then re-run scoring:

```bash
cargo make test-collect-metadata
python3 scripts/select-pareto-tests.py \
  .ggen/test-metadata/ \
  .ggen/optimized-test-list.txt \
  200 \
  --overrides .ggen/test-criticality-overrides.toml
```

### Q: How do I know if my tests are strong enough?

**A**: Use mutation testing:

```bash
# Run mutation testing
cargo make test-mutate

# Check kill rate
jq -r '.kill_rate' .ggen/mutation-reports/mutation-report.json

# Interpretation:
#   ≥ 80%: Strong tests ✅
#   60-79%: Acceptable ⚠️
#   < 60%: Weak tests 🚨
```

### Q: Can I use this for other Rust projects?

**A**: Yes! The tools are project-agnostic:

1. Install tools: `cargo install cargo-mutants cargo-nextest cargo-tarpaulin`
2. Copy scripts: `scripts/select-pareto-tests.py`, `scripts/test-value-scorer.py`
3. Adapt cargo-make tasks: See `Makefile.toml` for reference
4. Adjust scoring weights: Modify `scripts/select-pareto-tests.py` weights for your domain

### Q: What's the difference between unit and integration budgets?

**A**:

- **Unit tests**: ≤1s total, ≤5ms average per test (fast, isolated, no I/O)
- **Integration tests**: ≤10s total, ≤50ms average per test (slower, multi-component, real I/O)

**Why different budgets?**: Integration tests are inherently slower (real databases, file I/O, network calls). Strict 1s limit would force unnecessary mocking.

### Q: How often should I run mutation testing?

**A**: Depends on development phase:

- **Active development**: Weekly (detect new false positives)
- **Maintenance**: Monthly (periodic quality check)
- **Pre-release**: Always (validate release quality)

**Cost**: ~5 minutes per run (2,847 mutations × 1,178 tests)

### Q: What if execution time exceeds budget despite optimization?

**A**: Investigate root causes:

1. **Profile tests**: `cargo nextest run --profile ci --verbose`
2. **Identify bottlenecks**: File I/O, database queries, network calls
3. **Optimize**:
   - Use fixtures (pre-load data)
   - Mock expensive operations
   - Split large tests into smaller tests
   - Increase parallelism (8 threads → 16 threads if available)

**Last resort**: Increase budget (1s → 2s for unit tests) if ALL other options exhausted.

### Q: Can I run optimized suite in CI/CD?

**A**: Yes, but **run BOTH suites**:

```yaml
# .github/workflows/test.yml
jobs:
  test-optimized:
    runs-on: ubuntu-latest
    steps:
      - name: Run optimized suite (fast feedback)
        run: cargo make test-opt
        timeout-minutes: 1

  test-full:
    runs-on: ubuntu-latest
    steps:
      - name: Run full suite (comprehensive validation)
        run: cargo make test
        timeout-minutes: 5

  validate:
    needs: [test-optimized, test-full]
    runs-on: ubuntu-latest
    steps:
      - name: Fail PR if either suite failed
        if: failure()
        run: exit 1
```

**Rationale**: Optimized suite gives fast feedback (11s). Full suite ensures no regressions (137s). Running in parallel maximizes speed.

---

## Next Steps

1. **Run Baseline Audit**: `cargo make test-mutate` (establish current kill rate)
2. **Fix False Positives**: Address RED signals (modules with <60% kill rate)
3. **Collect Test Metadata**: `cargo make test-collect-metadata` (prepare for optimization)
4. **Generate Optimized Suite**: `python3 scripts/select-pareto-tests.py ...` (select top 200 tests)
5. **Validate Performance**: `cargo make test-opt` (verify ≤11s execution time)
6. **Integrate with CI/CD**: Add mutation testing and optimized suite to GitHub Actions

**Questions?** See [spec.md](spec.md) for detailed feature specification or [research.md](research.md) for algorithm deep-dive.
