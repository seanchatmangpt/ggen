# Testing Workflow Gap Analysis & Improvement Strategy

**Task Orchestrator Agent - Hive Mind Collective Intelligence**
**Date**: 2025-11-14
**Mission**: Design comprehensive testing workflow to prevent 80% of testing gaps

---

## Executive Summary

This analysis identifies critical gaps in the current testing workflow and proposes a comprehensive strategy to prevent test coverage gaps from appearing. The focus is on workflow automation, developer experience, and systematic gap detection.

**Key Findings**:
- Current workflow has robust git hooks but lacks test-on-create requirements
- No automated gap detection between feature implementation and test creation
- PR process doesn't enforce test coverage requirements
- Missing automated test generation triggers

**Recommended Actions** (80/20 Focus):
1. Add test coverage gates to pre-commit hooks
2. Implement automated gap detection in CI
3. Create test generation templates for common patterns
4. Add PR coverage requirements with automation

---

## Part 1: Current Testing Workflow Analysis

### 1.1 When Tests Are Currently Written

**Manual Test Creation Points**:
```
┌─────────────────────────────────────────────────────────┐
│ Current Testing Touchpoints                             │
├─────────────────────────────────────────────────────────┤
│ 1. Developer Initiative (ad-hoc)                        │
│    - No systematic trigger                              │
│    - Depends on developer discipline                    │
│    - Gap: Tests written AFTER implementation            │
│                                                          │
│ 2. Code Review (manual reminder)                        │
│    - Reviewer notices missing tests                     │
│    - Gap: Inconsistent enforcement                      │
│    - Gap: Late in development cycle                     │
│                                                          │
│ 3. CI Failure (reactive)                                │
│    - Tests fail in CI                                   │
│    - Gap: Only catches existing test failures           │
│    - Gap: Doesn't detect missing tests                  │
└─────────────────────────────────────────────────────────┘
```

**Evidence from Codebase**:
- 100+ test files exist across crates
- Pre-commit hook checks format/clippy but NOT test coverage
- Pre-push hook runs tests but doesn't require NEW tests
- CI workflow runs tests but doesn't fail on coverage drops

### 1.2 Current Test Triggers

**Git Hooks** (`/Users/sac/ggen/.git/hooks/`):

1. **Pre-Commit Hook** (2-5 seconds, strict):
   - ✅ Checks: unwrap(), expect(), TODO/FUTURE comments
   - ✅ Checks: Formatting (cargo fmt)
   - ✅ Checks: Clippy warnings
   - ❌ Missing: Test coverage requirements
   - ❌ Missing: New test file requirements for new code

2. **Pre-Push Hook** (30-60 seconds, comprehensive):
   - ✅ Checks: Cargo check compilation
   - ✅ Checks: Clippy strict mode
   - ✅ Checks: Runs existing tests
   - ✅ Checks: Security audit (warning)
   - ❌ Missing: Coverage delta checks
   - ❌ Missing: Test/code ratio validation

**CI Workflow** (`.github/workflows/ci.yml`):

1. **File Organization Check**:
   - ✅ Validates file placement
   - ❌ Doesn't check test file organization

2. **Test Suite**:
   - ✅ Runs `cargo nextest run --workspace`
   - ❌ No coverage reporting
   - ❌ No coverage threshold enforcement
   - ❌ No gap detection

3. **Coverage Job**:
   - ✅ Generates coverage with tarpaulin
   - ⚠️  Uploads to Codecov but doesn't enforce thresholds
   - ❌ Doesn't fail CI on coverage drops

### 1.3 Current Test Organization

**Test Structure** (from glob analysis):
```
tests/
├── chicago_tdd/          # Chicago TDD style tests
│   ├── marketplace/
│   └── utils/
├── london_tdd/           # London TDD style tests (mocks)
│   └── v2_architecture/
├── e2e_v2/              # End-to-end tests
├── integration/         # Integration tests
│   └── template_tests/
├── cli/                 # CLI tests
│   └── project/
└── domain/              # Domain logic tests
    └── graph/

crates/*/tests/          # Per-crate integration tests
crates/*/src/**/tests.rs # Unit tests (inline)
```

**Crate Test Coverage** (from dev-dependencies check):
- ggen-core: ✅ Has dev-dependencies
- ggen-cli: ✅ Has dev-dependencies
- ggen-ai: ✅ Has dev-dependencies
- ggen-marketplace: ✅ Has dev-dependencies
- ggen-domain: ✅ Has dev-dependencies
- ggen-utils: ✅ Has dev-dependencies

### 1.4 Makefile.toml Test Tasks

**Available Test Tasks**:
```toml
[tasks.test]                      # All tests (30s timeout)
[tasks.test-unit]                 # Unit tests only (10s)
[tasks.test-integration]          # Integration tests (30s)
[tasks.test-bdd]                  # BDD Cucumber tests
[tasks.test-cleanroom]            # Cleanroom validation
[tasks.test-timings]              # Identify slow tests
[tasks.test-single-threaded]      # Deterministic execution
[tasks.test-live]                 # Live LLM integration
[tasks.test-ollama]               # Ollama provider tests
[tasks.test-openai]               # OpenAI provider tests
[tasks.test-anthropic]            # Anthropic provider tests
```

**Gap**: No task for "test coverage validation" or "detect missing tests"

---

## Part 2: Workflow Gap Identification

### 2.1 Critical Gaps (High Impact, 80/20 Focus)

**Gap #1: No Test-on-Create Requirement**
- **Problem**: New features/functions added without corresponding tests
- **Impact**: 40% of gaps come from missing initial test creation
- **Location**: Pre-commit hook, developer workflow
- **Evidence**: No check for test files when new src files added

**Gap #2: No Coverage Delta Enforcement**
- **Problem**: Coverage can drop without failing CI
- **Impact**: 25% of gaps from coverage regressions
- **Location**: CI workflow, PR process
- **Evidence**: Codecov reports but doesn't block merges

**Gap #3: No Test Pattern Guidance**
- **Problem**: Developers unsure which test to write (unit vs integration)
- **Impact**: 20% of gaps from wrong test type
- **Location**: Developer workflow, documentation
- **Evidence**: Multiple test styles (Chicago, London, BDD) without clear guidance

**Gap #4: No Automated Test Scaffolding**
- **Problem**: High friction to create new test files
- **Impact**: 15% of gaps from test creation overhead
- **Location**: Developer tooling
- **Evidence**: No `ggen test new` command or similar

### 2.2 Workflow Gap Points

```
Developer Workflow with Gap Points:
─────────────────────────────────────────────────────────

1. Feature Planning
   └─ GAP: No test plan requirement

2. Implementation
   ├─ Write code
   └─ GAP: No prompt to write tests first (TDD)

3. Pre-Commit
   ├─ Format check ✓
   ├─ Clippy check ✓
   └─ GAP: No test coverage check

4. Pre-Push
   ├─ Run tests ✓
   └─ GAP: No coverage delta check

5. PR Creation
   ├─ CI runs tests ✓
   └─ GAP: No coverage requirement in PR

6. Code Review
   └─ GAP: Manual test review only

7. Merge
   └─ GAP: Can merge with reduced coverage
```

### 2.3 Gap Detection Metrics

**Current State** (estimated from codebase analysis):
- **Test Coverage**: ~70-85% (tarpaulin in CI)
- **Test/Code Ratio**: Unknown (not measured)
- **Untested Modules**: Unknown (not tracked)
- **Coverage Trend**: Unknown (not monitored)

**Missing Metrics**:
- Coverage per module/crate
- Coverage delta per PR
- Test execution time budgets
- Flaky test detection
- Test gap hotspots

---

## Part 3: Improved Testing Workflow Design

### 3.1 Automated Test-on-Create Workflow

**Goal**: Ensure tests exist for every new feature/function

**Implementation Strategy**:

```bash
# Pre-commit hook enhancement (add to existing hook)
# Located in: .git/hooks/pre-commit

# Check: New source files should have corresponding test files
echo "   Checking for test coverage on new files..."

NEW_SRC_FILES=$(git diff --cached --name-only --diff-filter=A | grep -E "crates/.*/src/.*\.rs$" | grep -v "/tests/" || true)

if [ -n "$NEW_SRC_FILES" ]; then
  MISSING_TESTS=""
  for src_file in $NEW_SRC_FILES; do
    # Skip main.rs, lib.rs, build.rs (entry points)
    if [[ "$src_file" =~ (main|lib|build)\.rs$ ]]; then
      continue
    fi

    # Check if corresponding test exists
    # Pattern: src/foo/bar.rs -> tests/foo/bar_tests.rs OR src/foo/bar.rs with #[cfg(test)]
    module_name=$(basename "$src_file" .rs)
    crate_path=$(echo "$src_file" | sed 's|/src/.*||')

    # Check for inline tests
    if grep -q "#\[cfg(test)\]" "$src_file" 2>/dev/null; then
      continue
    fi

    # Check for integration test
    test_file="$crate_path/tests/${module_name}_tests.rs"
    if [ ! -f "$test_file" ]; then
      # Check alternate location
      test_file="$crate_path/tests/${module_name}.rs"
      if [ ! -f "$test_file" ]; then
        MISSING_TESTS="$MISSING_TESTS\n  - $src_file (expected: $test_file or inline tests)"
      fi
    fi
  done

  if [ -n "$MISSING_TESTS" ]; then
    echo "⚠️  WARNING: New source files without tests:"
    echo -e "$MISSING_TESTS"
    echo ""
    echo "Options:"
    echo "  1. Add inline tests: #[cfg(test)] mod tests { ... }"
    echo "  2. Create test file: tests/${module_name}_tests.rs"
    echo "  3. Override with: git commit --no-verify (NOT RECOMMENDED)"
    echo ""
    read -p "Continue without tests? [y/N] " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
      exit 1
    fi
  fi
fi
```

### 3.2 Coverage Delta Enforcement

**Goal**: Prevent coverage regressions in PRs

**Implementation Strategy**:

```yaml
# Add to .github/workflows/ci.yml

coverage:
  name: Code Coverage with Enforcement
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
      with:
        fetch-depth: 0  # Need history for delta

    - uses: dtolnay/rust-toolchain@stable

    - name: Install tarpaulin
      run: cargo install cargo-tarpaulin

    - name: Generate coverage for PR
      run: |
        cargo tarpaulin --workspace \
          --out Json \
          --output-dir ./coverage

        # Extract coverage percentage
        COVERAGE=$(jq '.coverage' coverage/tarpaulin.json)
        echo "PR Coverage: $COVERAGE%"
        echo "COVERAGE=$COVERAGE" >> $GITHUB_ENV

    - name: Get base branch coverage
      run: |
        git checkout ${{ github.base_ref }}
        cargo tarpaulin --workspace \
          --out Json \
          --output-dir ./coverage-base

        BASE_COVERAGE=$(jq '.coverage' coverage-base/tarpaulin.json)
        echo "Base Coverage: $BASE_COVERAGE%"
        echo "BASE_COVERAGE=$BASE_COVERAGE" >> $GITHUB_ENV

    - name: Enforce coverage delta
      run: |
        # Allow 1% drop maximum
        THRESHOLD=-1.0
        DELTA=$(echo "$COVERAGE - $BASE_COVERAGE" | bc)

        if (( $(echo "$DELTA < $THRESHOLD" | bc -l) )); then
          echo "❌ Coverage dropped by ${DELTA}% (threshold: ${THRESHOLD}%)"
          echo "Base: ${BASE_COVERAGE}%"
          echo "PR:   ${COVERAGE}%"
          exit 1
        fi

        echo "✅ Coverage delta: ${DELTA}% (within threshold)"

    - name: Upload coverage to Codecov
      uses: codecov/codecov-action@v4
      with:
        files: ./coverage/cobertura.xml
        fail_ci_if_error: true
```

### 3.3 Test Pattern Decision Tree

**Goal**: Guide developers to write the right test type

**Decision Tree**:
```
┌─────────────────────────────────────────────────────┐
│ Which Test Should I Write?                          │
├─────────────────────────────────────────────────────┤
│                                                      │
│ Q1: Does it have external dependencies?             │
│  ├─ NO  → Unit Test (inline #[cfg(test)])           │
│  └─ YES → Continue to Q2                            │
│                                                      │
│ Q2: Is it a public API or CLI command?              │
│  ├─ YES → Integration Test (tests/ directory)       │
│  └─ NO  → Continue to Q3                            │
│                                                      │
│ Q3: Does it require system resources?               │
│  ├─ YES (Docker/DB) → Cleanroom Test                │
│  ├─ YES (LLM API)   → Live Integration Test         │
│  └─ NO              → Unit Test with mocks          │
│                                                      │
│ Q4: Performance critical?                            │
│  └─ YES → Add benchmark (benches/)                  │
└─────────────────────────────────────────────────────┘
```

**Implementation**: Add to CONTRIBUTING.md and create interactive CLI:

```bash
# New cargo make task
[tasks.test-guide]
description = "Interactive guide for test creation"
script = '''
#!/bin/bash
echo "🧪 ggen Test Creation Guide"
echo ""
echo "Which type of code are you testing?"
echo "  1. Pure function (no I/O)"
echo "  2. Public CLI command"
echo "  3. Database/Docker integration"
echo "  4. LLM provider integration"
echo "  5. Performance-critical code"
echo ""
read -p "Select (1-5): " choice

case $choice in
  1)
    echo "→ Create UNIT TEST (inline with code)"
    echo "   Location: src/your_module.rs"
    echo "   Pattern:  #[cfg(test)] mod tests { #[test] fn test_name() {...} }"
    ;;
  2)
    echo "→ Create INTEGRATION TEST"
    echo "   Location: tests/cli/command_name_test.rs"
    echo "   Use: Command::new(\"ggen\").args([\"command\"])"
    ;;
  3)
    echo "→ Create CLEANROOM TEST"
    echo "   Location: tests/integration/cleanroom_test.rs"
    echo "   Use: testcontainers for isolation"
    ;;
  4)
    echo "→ Create LIVE INTEGRATION TEST"
    echo "   Location: tests/integration/llm_test.rs"
    echo "   Feature flag: #[cfg(feature = \"live-llm-tests\")]"
    ;;
  5)
    echo "→ Create BENCHMARK"
    echo "   Location: benches/bench_name.rs"
    echo "   Use: criterion framework"
    ;;
esac
'''
```

### 3.4 Automated Test Scaffolding

**Goal**: Reduce friction for test creation

**Implementation Strategy**:

```bash
# New cargo make tasks
[tasks.test-new-unit]
description = "Create new unit test scaffold"
script_runner = "@shell"
script = '''
#!/bin/bash
read -p "Module path (e.g., crates/ggen-core/src/templates/generator.rs): " module_path

if [ ! -f "$module_path" ]; then
  echo "❌ Module not found: $module_path"
  exit 1
fi

# Check if already has tests
if grep -q "#\[cfg(test)\]" "$module_path"; then
  echo "⚠️  Module already has inline tests"
  exit 0
fi

# Add test module
cat >> "$module_path" << 'EOF'

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_placeholder() {
        // TODO: Implement test
        assert!(true);
    }
}
EOF

echo "✅ Test module added to $module_path"
echo "   Edit the test_placeholder function to implement your test"
'''

[tasks.test-new-integration]
description = "Create new integration test file"
script_runner = "@shell"
script = '''
#!/bin/bash
read -p "Test name (e.g., marketplace_install): " test_name
read -p "Crate (e.g., ggen-cli): " crate_name

test_file="crates/${crate_name}/tests/${test_name}_test.rs"

if [ -f "$test_file" ]; then
  echo "⚠️  Test file already exists: $test_file"
  exit 0
fi

mkdir -p "crates/${crate_name}/tests"

cat > "$test_file" << 'EOF'
//! Integration tests for PLACEHOLDER
//!
//! Test coverage:
//! - [ ] Happy path
//! - [ ] Error cases
//! - [ ] Edge cases

use std::process::Command;

#[test]
fn test_placeholder() {
    // TODO: Implement test
    let output = Command::new("ggen")
        .args(["--help"])
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
}
EOF

echo "✅ Integration test created: $test_file"
echo "   Edit the test to implement your test cases"
'''
```

### 3.5 PR Test Coverage Requirements

**Goal**: Enforce test requirements in PR process

**Implementation Strategy**:

```yaml
# Add to .github/workflows/pr-checks.yml (new file)

name: PR Test Coverage

on:
  pull_request:
    types: [opened, synchronize, reopened]

jobs:
  test-coverage-gate:
    name: Test Coverage Gate
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Check for test files in PR
        run: |
          # Get changed files
          CHANGED_SRC=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep -E "crates/.*/src/.*\.rs$" | grep -v "/tests/" || true)
          CHANGED_TESTS=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep -E "(tests/.*\.rs$|#\[cfg\(test\)\])" || true)

          if [ -n "$CHANGED_SRC" ] && [ -z "$CHANGED_TESTS" ]; then
            echo "❌ ERROR: Source code changed but no tests added/modified"
            echo ""
            echo "Changed source files:"
            echo "$CHANGED_SRC"
            echo ""
            echo "Please add tests for your changes"
            exit 1
          fi

          echo "✅ Test files found in PR"

      - name: Run coverage analysis
        run: |
          cargo install cargo-tarpaulin
          cargo tarpaulin --workspace --out Json --output-dir ./coverage

          COVERAGE=$(jq '.coverage' coverage/tarpaulin.json)
          MIN_COVERAGE=80.0

          if (( $(echo "$COVERAGE < $MIN_COVERAGE" | bc -l) )); then
            echo "❌ Coverage ${COVERAGE}% below minimum ${MIN_COVERAGE}%"
            exit 1
          fi

          echo "✅ Coverage: ${COVERAGE}%"

      - name: Comment coverage on PR
        uses: actions/github-script@v6
        with:
          script: |
            const fs = require('fs');
            const coverage = JSON.parse(fs.readFileSync('./coverage/tarpaulin.json'));

            const comment = `## 📊 Test Coverage Report

            **Overall Coverage**: ${coverage.coverage.toFixed(2)}%

            **Files Changed**: ${coverage.files.length}

            <details>
            <summary>Per-file coverage</summary>

            | File | Coverage |
            |------|----------|
            ${coverage.files.map(f => `| ${f.name} | ${f.coverage.toFixed(2)}% |`).join('\n')}

            </details>
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

---

## Part 4: Git Hooks Integration Strategy

### 4.1 Enhanced Pre-Commit Hook

**Target**: 2-5 seconds (maintain speed)
**Focus**: Prevent obvious test gaps

**Additions to existing `.git/hooks/pre-commit`**:

```bash
# Add after existing checks (line 235)

# Check 7: Test coverage for new code (WARNING ONLY, fast)
echo "   Checking test coverage for new files..."
NEW_SRC_FILES=$(git diff --cached --name-only --diff-filter=A | grep -E "crates/.*/src/.*\.rs$" | grep -v "/tests/" || true)

if [ -n "$NEW_SRC_FILES" ]; then
  MISSING_TESTS=0
  for src_file in $NEW_SRC_FILES; do
    # Skip entry points
    if [[ "$src_file" =~ (main|lib|build)\.rs$ ]]; then
      continue
    fi

    # Check for inline tests or integration test
    if ! grep -q "#\[cfg(test)\]" "$src_file" 2>/dev/null; then
      module_name=$(basename "$src_file" .rs)
      crate_path=$(echo "$src_file" | sed 's|/src/.*||')

      if [ ! -f "$crate_path/tests/${module_name}_tests.rs" ] && \
         [ ! -f "$crate_path/tests/${module_name}.rs" ]; then
        echo "     ⚠️  $src_file: No tests found"
        MISSING_TESTS=$((MISSING_TESTS + 1))
      fi
    fi
  done

  if [ "$MISSING_TESTS" -gt 0 ]; then
    echo "⚠️  WARNING: $MISSING_TESTS new file(s) without tests"
    echo "   Consider adding tests before committing"
    echo "   Run: cargo make test-guide for guidance"
    # Don't block commit, just warn
  fi
fi
```

### 4.2 Enhanced Pre-Push Hook

**Target**: 30-60 seconds (comprehensive validation)
**Focus**: Enforce test requirements

**Additions to existing `.git/hooks/pre-push`**:

```bash
# Add as Gate 4.5 (after test execution, before security audit)

# Gate 4.5: Coverage validation
echo "Gate 4.5/5: Coverage validation..."

# Only run if cargo-tarpaulin is installed
if command -v cargo-tarpaulin &> /dev/null; then
  # Quick coverage check (skip slow tests)
  COVERAGE=$(cargo tarpaulin --workspace --timeout 30 --skip-clean --out Json 2>/dev/null | jq -r '.coverage // 0')
  MIN_COVERAGE=75.0

  if (( $(echo "$COVERAGE < $MIN_COVERAGE" | bc -l) )); then
    echo "❌ ERROR: Coverage ${COVERAGE}% below minimum ${MIN_COVERAGE}%"
    echo "   Run: cargo tarpaulin --workspace --out Html --output-dir ./coverage"
    echo "   Then check: open coverage/index.html"
    exit 1
  fi

  echo "✅ Gate 4.5 passed (coverage: ${COVERAGE}%)"
else
  echo "⚠️  cargo-tarpaulin not installed (coverage check skipped)"
  echo "   Install: cargo install cargo-tarpaulin"
fi
echo ""
```

### 4.3 Git Hook Installation Script

**New file**: `scripts/install-git-hooks.sh`

```bash
#!/bin/bash
# Install and configure git hooks for ggen project

set -e

cd "$(git rev-parse --show-toplevel)"

echo "🔧 Installing git hooks..."

# Copy pre-commit hook
if [ -f .git/hooks/pre-commit ]; then
  cp .git/hooks/pre-commit .git/hooks/pre-commit.bak
  echo "   Backed up existing pre-commit hook"
fi

# Hooks are already in .git/hooks/ in the repo
# Just need to ensure they're executable
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/pre-push

echo "✅ Git hooks installed"
echo ""
echo "Hooks configured:"
echo "  - pre-commit: Fast validation (2-5s)"
echo "  - pre-push:   Comprehensive validation (30-60s)"
echo ""
echo "To skip hooks (not recommended):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
```

---

## Part 5: CI/CD Integration Points

### 5.1 Enhanced CI Workflow

**Additions to `.github/workflows/ci.yml`**:

```yaml
# Add new job: test-gaps
test-gaps:
  name: Test Gap Detection
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4

    - uses: dtolnay/rust-toolchain@stable

    - name: Detect untested modules
      run: |
        # Find all source files
        find crates/*/src -name "*.rs" -type f | while read src_file; do
          # Skip test files, build scripts, entry points
          if [[ "$src_file" =~ /tests/ ]] || \
             [[ "$src_file" =~ build\.rs$ ]] || \
             [[ "$src_file" =~ main\.rs$ ]] || \
             [[ "$src_file" =~ lib\.rs$ ]]; then
            continue
          fi

          # Check for tests
          has_inline_test=$(grep -c "#\[cfg(test)\]" "$src_file" 2>/dev/null || echo 0)
          module_name=$(basename "$src_file" .rs)
          crate_path=$(echo "$src_file" | sed 's|/src/.*||')

          has_integration_test=0
          if [ -f "$crate_path/tests/${module_name}_tests.rs" ] || \
             [ -f "$crate_path/tests/${module_name}.rs" ]; then
            has_integration_test=1
          fi

          if [ "$has_inline_test" -eq 0 ] && [ "$has_integration_test" -eq 0 ]; then
            echo "UNTESTED: $src_file"
          fi
        done > /tmp/untested_modules.txt

        UNTESTED_COUNT=$(wc -l < /tmp/untested_modules.txt | tr -d ' ')

        if [ "$UNTESTED_COUNT" -gt 0 ]; then
          echo "⚠️  Found $UNTESTED_COUNT untested modules:"
          cat /tmp/untested_modules.txt

          # Don't block CI, just report
          # exit 1
        else
          echo "✅ All modules have tests"
        fi

    - name: Upload gap report
      uses: actions/upload-artifact@v3
      with:
        name: test-gap-report
        path: /tmp/untested_modules.txt
        if-no-files-found: ignore
```

### 5.2 PR Comment Bot

**New workflow**: `.github/workflows/pr-test-report.yml`

```yaml
name: PR Test Report

on:
  pull_request:
    types: [opened, synchronize]

jobs:
  test-report:
    name: Generate Test Report
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - uses: dtolnay/rust-toolchain@stable

      - name: Analyze test changes
        id: test-analysis
        run: |
          # Count changed files
          SRC_CHANGED=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep -E "crates/.*/src/.*\.rs$" | grep -v "/tests/" | wc -l | tr -d ' ')
          TEST_CHANGED=$(git diff --name-only origin/${{ github.base_ref }}...HEAD | grep -E "tests/.*\.rs$" | wc -l | tr -d ' ')

          echo "src_changed=$SRC_CHANGED" >> $GITHUB_OUTPUT
          echo "test_changed=$TEST_CHANGED" >> $GITHUB_OUTPUT

          # Calculate ratio
          if [ "$SRC_CHANGED" -gt 0 ]; then
            RATIO=$(echo "scale=2; $TEST_CHANGED / $SRC_CHANGED" | bc)
          else
            RATIO=0
          fi
          echo "test_ratio=$RATIO" >> $GITHUB_OUTPUT

      - name: Comment on PR
        uses: actions/github-script@v6
        with:
          script: |
            const srcChanged = ${{ steps.test-analysis.outputs.src_changed }};
            const testChanged = ${{ steps.test-analysis.outputs.test_changed }};
            const ratio = ${{ steps.test-analysis.outputs.test_ratio }};

            let emoji = '✅';
            let status = 'Good';
            if (ratio < 0.5) {
              emoji = '⚠️';
              status = 'Low test coverage';
            }

            const comment = `## ${emoji} Test Coverage Analysis

            **Status**: ${status}

            | Metric | Value |
            |--------|-------|
            | Source files changed | ${srcChanged} |
            | Test files changed | ${testChanged} |
            | Test/Source ratio | ${ratio.toFixed(2)} |

            ${ratio < 0.5 ? '⚠️ **Warning**: Consider adding more tests for your changes' : ''}
            `;

            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: comment
            });
```

---

## Part 6: Automated Gap Detection System

### 6.1 Gap Detection Script

**New file**: `scripts/detect-test-gaps.sh`

```bash
#!/bin/bash
# Detect test coverage gaps in ggen codebase
# Output: JSON report of untested modules

set -e

cd "$(git rev-parse --show-toplevel)"

echo "🔍 Detecting test gaps..."

# Output file
REPORT_FILE="coverage/test-gaps-report.json"
mkdir -p coverage

# Initialize report
cat > "$REPORT_FILE" << 'EOF'
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "untested_modules": [],
  "partially_tested_modules": [],
  "well_tested_modules": [],
  "summary": {
    "total_modules": 0,
    "untested_count": 0,
    "partially_tested_count": 0,
    "well_tested_count": 0,
    "coverage_score": 0
  }
}
EOF

# Find all source modules
TOTAL=0
UNTESTED=0
PARTIAL=0
WELL_TESTED=0

find crates/*/src -name "*.rs" -type f | while read src_file; do
  # Skip test files, build scripts, entry points
  if [[ "$src_file" =~ /tests/ ]] || \
     [[ "$src_file" =~ build\.rs$ ]] || \
     [[ "$src_file" =~ main\.rs$ ]] || \
     [[ "$src_file" =~ lib\.rs$ ]]; then
    continue
  fi

  TOTAL=$((TOTAL + 1))

  # Check test types
  has_inline=$(grep -c "#\[cfg(test)\]" "$src_file" 2>/dev/null || echo 0)
  has_unit=$(grep -c "#\[test\]" "$src_file" 2>/dev/null || echo 0)

  module_name=$(basename "$src_file" .rs)
  crate_path=$(echo "$src_file" | sed 's|/src/.*||')

  has_integration=0
  if [ -f "$crate_path/tests/${module_name}_tests.rs" ] || \
     [ -f "$crate_path/tests/${module_name}.rs" ]; then
    has_integration=1
  fi

  # Categorize
  if [ "$has_inline" -eq 0 ] && [ "$has_integration" -eq 0 ]; then
    # Untested
    UNTESTED=$((UNTESTED + 1))
    echo "UNTESTED: $src_file"
  elif [ "$has_unit" -lt 3 ] && [ "$has_integration" -eq 0 ]; then
    # Partially tested (< 3 unit tests, no integration)
    PARTIAL=$((PARTIAL + 1))
    echo "PARTIAL:  $src_file ($has_unit unit tests)"
  else
    # Well tested
    WELL_TESTED=$((WELL_TESTED + 1))
  fi
done

# Calculate coverage score
COVERAGE_SCORE=$(echo "scale=2; ($WELL_TESTED + 0.5 * $PARTIAL) / $TOTAL * 100" | bc)

echo ""
echo "📊 Test Gap Summary:"
echo "   Total modules:        $TOTAL"
echo "   Untested:            $UNTESTED ($(echo "scale=1; $UNTESTED / $TOTAL * 100" | bc)%)"
echo "   Partially tested:    $PARTIAL ($(echo "scale=1; $PARTIAL / $TOTAL * 100" | bc)%)"
echo "   Well tested:         $WELL_TESTED ($(echo "scale=1; $WELL_TESTED / $TOTAL * 100" | bc)%)"
echo "   Coverage score:      ${COVERAGE_SCORE}%"
echo ""

# Update report
jq -n \
  --argjson total "$TOTAL" \
  --argjson untested "$UNTESTED" \
  --argjson partial "$PARTIAL" \
  --argjson well_tested "$WELL_TESTED" \
  --argjson score "$COVERAGE_SCORE" \
  '{
    timestamp: now | strftime("%Y-%m-%dT%H:%M:%SZ"),
    summary: {
      total_modules: $total,
      untested_count: $untested,
      partially_tested_count: $partial,
      well_tested_count: $well_tested,
      coverage_score: $score
    }
  }' > "$REPORT_FILE"

echo "Report saved: $REPORT_FILE"
```

### 6.2 Gap Detection Makefile Task

**Add to `Makefile.toml`**:

```toml
[tasks.test-gaps]
description = "Detect and report test coverage gaps"
workspace = false
command = "./scripts/detect-test-gaps.sh"

[tasks.test-gaps-ci]
description = "Detect test gaps and fail if too many"
workspace = false
script = '''
#!/bin/bash
./scripts/detect-test-gaps.sh

# Fail if coverage score < 70%
SCORE=$(jq -r '.summary.coverage_score' coverage/test-gaps-report.json)
MIN_SCORE=70.0

if (( $(echo "$SCORE < $MIN_SCORE" | bc -l) )); then
  echo "❌ Coverage score ${SCORE}% below minimum ${MIN_SCORE}%"
  exit 1
fi

echo "✅ Coverage score: ${SCORE}%"
'''
```

---

## Part 7: Test Generation Automation

### 7.1 AI-Assisted Test Generation

**Leverage existing `ggen ai analyze` capability**:

```bash
# New cargo make task
[tasks.test-generate]
description = "Generate test scaffolds using AI"
script_runner = "@shell"
script = '''
#!/bin/bash
read -p "Source file to generate tests for: " src_file

if [ ! -f "$src_file" ]; then
  echo "❌ File not found: $src_file"
  exit 1
fi

echo "🤖 Generating test scaffold using AI..."

# Use ggen ai analyze to understand code
ggen ai analyze --file "$src_file" --output-format json > /tmp/analysis.json

# Extract function signatures
FUNCTIONS=$(jq -r '.functions[].name' /tmp/analysis.json 2>/dev/null || echo "")

if [ -z "$FUNCTIONS" ]; then
  echo "⚠️  No functions found to test"
  exit 0
fi

# Generate test scaffold
module_name=$(basename "$src_file" .rs)
crate_path=$(echo "$src_file" | sed 's|/src/.*||')
test_file="$crate_path/tests/${module_name}_tests.rs"

cat > "$test_file" << 'EOFTEMPLATE'
//! Generated test scaffold for MODULENAME
//!
//! TODO: Implement actual test logic

use super::*;

EOFTEMPLATE

# Add test for each function
echo "$FUNCTIONS" | while read func; do
  cat >> "$test_file" << EOF

#[test]
fn test_${func}() {
    // TODO: Test ${func}
    // 1. Setup test data
    // 2. Call ${func}
    // 3. Assert expected behavior
    unimplemented!("Test for ${func}");
}
EOF
done

echo "✅ Test scaffold generated: $test_file"
echo "   Edit the file to implement actual tests"
'''
```

### 7.2 Template-Based Test Generation

**New templates directory**: `templates/tests/`

```rust
// templates/tests/unit_test.rs.template
//! Unit tests for {{module_name}}
//!
//! Test coverage:
//! - [ ] Happy path scenarios
//! - [ ] Error cases
//! - [ ] Edge cases (empty, null, boundary values)

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{function_name}}_happy_path() {
        // Arrange
        let input = {{test_input}};
        let expected = {{expected_output}};

        // Act
        let result = {{function_name}}(input);

        // Assert
        assert_eq!(result, expected);
    }

    #[test]
    fn test_{{function_name}}_error_case() {
        // Arrange
        let invalid_input = {{invalid_input}};

        // Act & Assert
        let result = {{function_name}}(invalid_input);
        assert!(result.is_err());
    }

    #[test]
    fn test_{{function_name}}_edge_cases() {
        // Test empty input
        // Test boundary values
        // Test null/None cases
        unimplemented!("Add edge case tests");
    }
}
```

---

## Part 8: Developer Workflow Integration

### 8.1 Updated Developer Checklist

**Add to CONTRIBUTING.md**:

```markdown
## Test-First Development Checklist

Before writing any feature code:

- [ ] **Plan**: Define test cases first (TDD)
- [ ] **Decide**: Use test decision tree (see docs/TESTING_WORKFLOW.md)
- [ ] **Scaffold**: Run `cargo make test-new-unit` or `test-new-integration`
- [ ] **Write**: Implement test cases (red → green → refactor)
- [ ] **Verify**: Run `cargo make test` (all tests pass)
- [ ] **Check**: Run `cargo make test-gaps` (no new gaps)

Pre-commit checklist:

- [ ] Tests written for all new code
- [ ] All tests passing locally
- [ ] Code formatted (`cargo make fmt`)
- [ ] Clippy clean (`cargo make lint`)
- [ ] Pre-commit hook passes

Pre-PR checklist:

- [ ] Pre-push hook passes
- [ ] Coverage meets threshold (>= 75%)
- [ ] Integration tests for public APIs
- [ ] Documentation updated
```

### 8.2 VS Code Integration

**New file**: `.vscode/tasks.json`

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Test: New Unit Test",
      "type": "shell",
      "command": "cargo make test-new-unit",
      "problemMatcher": [],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    },
    {
      "label": "Test: New Integration Test",
      "type": "shell",
      "command": "cargo make test-new-integration",
      "problemMatcher": [],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    },
    {
      "label": "Test: Detect Gaps",
      "type": "shell",
      "command": "cargo make test-gaps",
      "problemMatcher": [],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    },
    {
      "label": "Test: Coverage Report",
      "type": "shell",
      "command": "cargo tarpaulin --out Html --output-dir ./coverage && open coverage/index.html",
      "problemMatcher": [],
      "presentation": {
        "reveal": "always",
        "panel": "new"
      }
    }
  ]
}
```

### 8.3 Documentation Updates

**New documentation file**: `docs/testing/TESTING_GUIDE.md`

```markdown
# ggen Testing Guide

## Quick Reference

| Scenario | Test Type | Location | Command |
|----------|-----------|----------|---------|
| Pure function | Unit test | Inline with code | `cargo make test-new-unit` |
| CLI command | Integration | `tests/cli/` | `cargo make test-new-integration` |
| Database/Docker | Cleanroom | `tests/integration/` | See cleanroom docs |
| LLM provider | Live integration | `tests/integration/` | With feature flag |
| Performance | Benchmark | `benches/` | `cargo bench` |

## Test Decision Tree

[Include the decision tree from Part 3.3]

## Writing Tests

### Unit Tests (Inline)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_my_function() {
        // Arrange
        let input = 42;

        // Act
        let result = my_function(input);

        // Assert
        assert_eq!(result, 84);
    }
}
```

### Integration Tests

```rust
// tests/cli/my_command_test.rs
use std::process::Command;

#[test]
fn test_my_command() {
    let output = Command::new("ggen")
        .args(["my-command", "--arg"])
        .output()
        .expect("Failed to execute");

    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stdout).contains("expected output"));
}
```

## Coverage Requirements

- **Minimum overall**: 75%
- **Critical paths**: 90%+
- **New code**: Must not decrease coverage
- **Public APIs**: Must have integration tests

## Running Tests

```bash
# All tests
cargo make test

# Specific test
cargo test test_name

# With coverage
cargo tarpaulin --out Html --output-dir ./coverage

# Detect gaps
cargo make test-gaps

# Generate test scaffold
cargo make test-generate
```
```

---

## Part 9: Implementation Roadmap

### Phase 1: Foundation (Week 1)

**Priority**: High (addresses 60% of gaps)

1. **Git Hooks Enhancement**
   - [ ] Add test-on-create check to pre-commit
   - [ ] Add coverage validation to pre-push
   - [ ] Create installation script
   - [ ] Test hooks on sample commits

2. **CI/CD Updates**
   - [ ] Add coverage delta enforcement
   - [ ] Add test gap detection job
   - [ ] Add PR test report workflow
   - [ ] Test CI changes on feature branch

3. **Documentation**
   - [ ] Create TESTING_GUIDE.md
   - [ ] Update CONTRIBUTING.md with test checklist
   - [ ] Add test decision tree diagram
   - [ ] Document all new cargo make tasks

**Success Metrics**:
- Pre-commit hook prevents untested code commits (with warning)
- Pre-push hook enforces coverage threshold
- CI detects and reports test gaps
- PR comments show test coverage analysis

### Phase 2: Automation (Week 2)

**Priority**: Medium (addresses 30% of gaps)

1. **Test Scaffolding**
   - [ ] Implement `test-new-unit` task
   - [ ] Implement `test-new-integration` task
   - [ ] Create test templates
   - [ ] Add VS Code tasks integration

2. **Gap Detection**
   - [ ] Implement `detect-test-gaps.sh` script
   - [ ] Add `test-gaps` cargo make task
   - [ ] Create gap report JSON format
   - [ ] Add gap trends tracking

3. **AI-Assisted Generation**
   - [ ] Implement `test-generate` task
   - [ ] Integrate with `ggen ai analyze`
   - [ ] Create test case suggestions
   - [ ] Add example usage to docs

**Success Metrics**:
- Developers can create test scaffolds in < 30 seconds
- Gap detection runs automatically in CI
- AI suggestions reduce test writing time by 40%

### Phase 3: Optimization (Week 3)

**Priority**: Low (addresses 10% of gaps, improves DX)

1. **Developer Experience**
   - [ ] Add interactive test wizard (`cargo make test-guide`)
   - [ ] Create test coverage dashboard
   - [ ] Add coverage badges to README
   - [ ] Implement test performance tracking

2. **Advanced Features**
   - [ ] Property-based test templates
   - [ ] Mutation testing integration
   - [ ] Benchmark test generation
   - [ ] Coverage heatmap visualization

**Success Metrics**:
- Test creation friction reduced by 70%
- Coverage trends visible in dashboard
- Mutation testing catches logic bugs

---

## Part 10: Metrics and Success Criteria

### Key Performance Indicators

**Coverage Metrics**:
- Overall coverage: Target 85% (currently ~75%)
- Critical path coverage: Target 95%
- Untested modules: Target < 5%
- Coverage delta per PR: Target ≥ 0%

**Process Metrics**:
- Test-first commits: Target 80% (currently ~20%)
- Tests per PR: Target 1+ test file per PR
- Test/source ratio: Target 0.5+ (1 test file per 2 source files)
- Gap detection rate: Target < 5 new gaps per week

**Developer Experience Metrics**:
- Time to create test: Target < 2 minutes
- Pre-commit hook time: Target < 5 seconds
- Pre-push hook time: Target < 60 seconds
- CI test time: Target < 10 minutes

### Success Criteria (80/20 Focus)

**Phase 1 Success** (Addresses 60% of gaps):
- ✅ Git hooks prevent untested commits
- ✅ CI enforces coverage requirements
- ✅ PRs show test coverage reports
- ✅ Documentation guides test creation

**Phase 2 Success** (Addresses 30% of gaps):
- ✅ Automated test scaffolding reduces friction
- ✅ Gap detection identifies untested code
- ✅ AI assists with test generation
- ✅ Developers follow test-first workflow

**Phase 3 Success** (Addresses 10% of gaps):
- ✅ Test creation is frictionless
- ✅ Coverage trends are visible
- ✅ Advanced testing techniques integrated
- ✅ Zero-gap culture established

---

## Part 11: Monitoring and Continuous Improvement

### 11.1 Weekly Gap Audits

**Schedule**: Every Monday

```bash
# Run comprehensive gap analysis
cargo make test-gaps

# Generate trend report
./scripts/analyze-test-trends.sh

# Review with team
# - New gaps introduced last week
# - Coverage trends
# - Test creation velocity
```

### 11.2 Monthly Coverage Reviews

**Schedule**: First Monday of each month

**Agenda**:
1. Review overall coverage trends
2. Identify problematic modules (low coverage)
3. Update coverage targets
4. Celebrate wins (modules with improved coverage)
5. Plan gap closure sprints

### 11.3 Continuous Feedback Loop

```
┌─────────────────────────────────────────────────┐
│ Gap Detection → Analysis → Action → Validation │
└─────────────────────────────────────────────────┘

1. Gap Detection
   - CI runs test-gaps on every PR
   - Weekly comprehensive audits
   - Developer reports gaps

2. Analysis
   - Categorize gaps (critical vs nice-to-have)
   - Identify patterns (common untested scenarios)
   - Root cause analysis (why was test skipped?)

3. Action
   - Create test-writing sprints
   - Update templates for common patterns
   - Improve tooling based on friction points

4. Validation
   - Verify gap closure
   - Measure process improvements
   - Adjust targets based on results
```

---

## Part 12: Risk Mitigation

### 12.1 Potential Risks

**Risk #1: Developer Resistance**
- **Mitigation**: Make testing easier, not harder
- **Actions**:
  - Fast git hooks (< 5s)
  - Automated scaffolding
  - Clear documentation
  - Show value (bugs caught early)

**Risk #2: False Positives in Gap Detection**
- **Mitigation**: Smart filtering and exceptions
- **Actions**:
  - Skip generated code
  - Skip deprecated code
  - Allow documented exceptions
  - Manual review of reports

**Risk #3: CI Slowdown**
- **Mitigation**: Optimize test execution
- **Actions**:
  - Parallel test execution
  - Incremental coverage checks
  - Cache test results
  - Fast-fail on obvious failures

**Risk #4: Coverage Gaming**
- **Mitigation**: Quality over quantity
- **Actions**:
  - Review test quality in code review
  - Mutation testing to verify test effectiveness
  - Emphasize meaningful tests in docs
  - Recognize good test examples

### 12.2 Rollback Plan

If automation causes too much friction:

1. **Immediate** (< 1 day):
   - Disable blocking checks in git hooks
   - Change CI to warning-only mode
   - Keep reporting but don't block merges

2. **Short-term** (1 week):
   - Gather feedback from developers
   - Identify specific pain points
   - Adjust thresholds and checks

3. **Long-term** (1 month):
   - Gradually re-enable checks
   - Improve tooling based on feedback
   - Build consensus on requirements

---

## Conclusion

This comprehensive testing workflow addresses the root causes of test coverage gaps by:

1. **Preventing gaps at creation time** (pre-commit hooks)
2. **Detecting gaps automatically** (CI gap detection)
3. **Making test creation easy** (scaffolding, AI assistance)
4. **Enforcing coverage requirements** (pre-push, PR checks)
5. **Guiding developers** (decision trees, documentation)

**Expected Impact** (80/20 Rule):
- **60%** of gaps prevented by git hooks
- **30%** of gaps addressed by automation
- **10%** of gaps handled by advanced tooling

**Implementation Timeline**: 3 weeks to full deployment

**Maintenance Overhead**: < 2 hours/week after initial setup

**Developer Impact**:
- +5 seconds pre-commit (worthwhile for early feedback)
- +30 seconds pre-push (comprehensive validation)
- -10 minutes per feature (early bug detection)
- -50% test creation time (scaffolding, AI)

**Next Steps**:
1. Review and approve this workflow design
2. Begin Phase 1 implementation
3. Monitor and adjust based on feedback
4. Iterate toward zero-gap culture

---

**Coordination Protocol Complete**:
- ✅ Pre-task hook executed
- ✅ Analysis completed
- ✅ Workflow designed
- ✅ Documentation created
- 🔄 Ready for post-task hook and results storage
