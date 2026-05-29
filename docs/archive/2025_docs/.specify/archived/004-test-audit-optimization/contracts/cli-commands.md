# CLI Command Specifications - Feature 004

## Overview

This document defines the command-line interface contracts for the test audit and optimization system. All commands follow ggen's design principles: deterministic output, structured JSON, zero-cost abstractions.

---

## Command: `ggen test audit`

### Purpose
Analyze test suite quality, coverage, and value metrics. Outputs structured JSON report with actionable recommendations.

### Signature
```bash
ggen test audit [OPTIONS] [PATHS]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `PATHS` | `Vec<PathBuf>` | No | `tests/` | Test directories to audit |

### Options

| Flag | Short | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--output` | `-o` | `PathBuf` | stdout | Output file path for JSON report |
| `--format` | `-f` | `enum` | `json` | Output format: `json`, `yaml`, `human` |
| `--kill-rate-threshold` | | `f64` | `0.80` | Minimum acceptable mutation kill rate (0.0-1.0) |
| `--test-count-threshold` | | `usize` | `200` | Minimum test count threshold |
| `--coverage-threshold` | | `f64` | `0.80` | Minimum code coverage threshold (0.0-1.0) |
| `--fail-on-threshold` | | `bool` | `false` | Exit with code 2 if thresholds not met |
| `--include-suggestions` | | `bool` | `true` | Include actionable improvement suggestions |
| `--verbose` | `-v` | `bool` | `false` | Include detailed metrics per test file |

### Exit Codes

| Code | Meaning | Condition |
|------|---------|-----------|
| `0` | Success | Audit completed, all thresholds met |
| `1` | Warning | Audit completed, some thresholds not met (if `--fail-on-threshold=false`) |
| `2` | Error | Audit completed, thresholds not met (if `--fail-on-threshold=true`) |
| `3` | Critical | Audit failed to complete (IO error, parse error, etc.) |

### Output Schema
See `test-metadata-schema.json` for complete structure.

### Examples

#### Basic audit (stdout JSON)
```bash
ggen test audit
```

Output:
```json
{
  "summary": {
    "total_tests": 245,
    "test_files": 18,
    "mutation_kill_rate": 0.87,
    "code_coverage": 0.82,
    "execution_time_ms": 1234,
    "thresholds_met": true
  },
  "files": [...]
}
```

#### Audit with custom thresholds and fail-fast
```bash
ggen test audit --kill-rate-threshold 0.90 --fail-on-threshold tests/aci/
```

Exit code: `2` (if kill rate < 90%)

#### Human-readable output
```bash
ggen test audit --format human
```

Output:
```
Test Audit Report
=================
Total Tests: 245
Test Files: 18
Mutation Kill Rate: 87.0% ✓
Code Coverage: 82.0% ✓
Execution Time: 1.23s

Recommendations:
  [HIGH] Add mutation tests for crates/ggen-core/src/graph/store.rs (kill rate: 65%)
  [MED]  Increase parallelism for slow test files (4 files > 500ms)
```

---

## Command: `ggen test optimize`

### Purpose
Generate optimized test execution plan using value-based selection, parallelization, and budget enforcement.

### Signature
```bash
ggen test optimize [OPTIONS] [PATHS]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `PATHS` | `Vec<PathBuf>` | No | `tests/` | Test directories to optimize |

### Options

| Flag | Short | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--output` | `-o` | `PathBuf` | stdout | Output file path for execution plan JSON |
| `--format` | `-f` | `enum` | `json` | Output format: `json`, `yaml`, `shell-script` |
| `--threads` | `-j` | `usize` | `num_cpus::get()` | Number of parallel test threads |
| `--time-budget-ms` | | `u64` | `60000` | Maximum execution time budget (milliseconds) |
| `--value-threshold` | | `f64` | `0.70` | Minimum value score to include test (0.0-1.0) |
| `--strategy` | | `enum` | `greedy-knapsack` | Selection strategy: `greedy-knapsack`, `all`, `fast-only` |
| `--dry-run` | | `bool` | `false` | Show plan without executing |
| `--execute` | | `bool` | `false` | Execute the optimized plan immediately |

### Exit Codes

| Code | Meaning | Condition |
|------|---------|-----------|
| `0` | Success | Optimization completed, plan generated |
| `1` | Warning | Plan generated but budget constraints violated |
| `2` | Error | Cannot satisfy constraints (value threshold too high, budget too low) |
| `3` | Critical | Optimization failed (invalid metadata, IO error, etc.) |

### Output Schema
See `value-score-schema.json` for complete structure.

### Examples

#### Generate optimized plan with 30s budget
```bash
ggen test optimize --time-budget-ms 30000 --threads 8 --output plan.json
```

Output (`plan.json`):
```json
{
  "plan": {
    "total_tests": 180,
    "estimated_time_ms": 28500,
    "threads": 8,
    "strategy": "greedy-knapsack",
    "batches": [
      {
        "batch_id": 0,
        "thread_id": 0,
        "tests": ["tests/aci/tool_selection_tests.rs::test_best_match", ...],
        "estimated_time_ms": 3500
      },
      ...
    ]
  },
  "excluded_tests": [
    {
      "path": "tests/slow_integration.rs",
      "reason": "value_score_below_threshold",
      "value_score": 0.65
    }
  ]
}
```

#### Execute plan immediately
```bash
ggen test optimize --time-budget-ms 30000 --execute
```

Output:
```
Executing optimized test plan (180 tests, 8 threads)...
[PASS] Batch 0/22 (3.5s) ✓
[PASS] Batch 1/22 (3.2s) ✓
...
Total: 180 tests, 28.5s, 100% pass rate
```

#### Shell script output (for CI integration)
```bash
ggen test optimize --format shell-script --output run-tests.sh
```

Output (`run-tests.sh`):
```bash
#!/bin/bash
# Generated by ggen test optimize
# Strategy: greedy-knapsack, Budget: 60000ms, Threads: 8

cargo test --test aci -- tool_selection_tests::test_best_match &
cargo test --test aci -- skill_invocation_tests::test_invoke_success &
...
wait
```

---

## Command: `ggen test mutate`

### Purpose
Run mutation testing to measure test suite effectiveness. Injects faults and verifies tests catch them.

### Signature
```bash
ggen test mutate [OPTIONS] [PATHS]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `PATHS` | `Vec<PathBuf>` | No | `crates/` | Source directories to mutate |

### Options

| Flag | Short | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--output` | `-o` | `PathBuf` | stdout | Output file path for mutation report JSON |
| `--format` | `-f` | `enum` | `json` | Output format: `json`, `yaml`, `human` |
| `--mutators` | | `Vec<String>` | `all` | Mutation operators: `replace-binary-op`, `negate-condition`, `remove-return`, `all` |
| `--timeout-multiplier` | | `f64` | `2.0` | Timeout multiplier for mutant execution (baseline * multiplier) |
| `--kill-rate-target` | | `f64` | `0.80` | Target mutation kill rate (0.0-1.0) |
| `--fail-on-target` | | `bool` | `false` | Exit with code 2 if kill rate below target |
| `--parallel` | `-j` | `usize` | `num_cpus::get()` | Number of parallel mutation executions |
| `--sample-rate` | | `f64` | `1.0` | Sample rate for mutations (0.0-1.0, for faster runs) |

### Exit Codes

| Code | Meaning | Condition |
|------|---------|-----------|
| `0` | Success | Mutation testing completed, kill rate meets target |
| `1` | Warning | Mutation testing completed, kill rate below target (if `--fail-on-target=false`) |
| `2` | Error | Kill rate below target (if `--fail-on-target=true`) |
| `3` | Critical | Mutation testing failed (compilation error, timeout, etc.) |

### Output Schema
See `mutation-report-schema.json` for complete structure.

### Examples

#### Basic mutation testing
```bash
ggen test mutate crates/ggen-core/src/
```

Output:
```json
{
  "summary": {
    "total_mutants": 342,
    "killed": 298,
    "survived": 44,
    "timeout": 0,
    "kill_rate": 0.871,
    "execution_time_ms": 45600
  },
  "mutants": [
    {
      "id": "mut_001",
      "file": "crates/ggen-core/src/graph/store.rs",
      "line": 145,
      "operator": "replace-binary-op",
      "original": "==",
      "mutated": "!=",
      "status": "killed",
      "killed_by": ["tests/graph/store_tests.rs::test_upsert"]
    },
    ...
  ]
}
```

#### Fast mutation testing (10% sample)
```bash
ggen test mutate --sample-rate 0.1 --parallel 16
```

Output:
```
Mutation Testing (10% sample, 16 threads)
Total Mutants: 34 (sampled from 342)
Killed: 29 (85.3%)
Survived: 5 (14.7%)
Execution Time: 4.2s

Survived Mutants:
  [HIGH] crates/ggen-core/src/graph/store.rs:145 (replace-binary-op)
  [MED]  crates/ggen-domain/src/types.rs:89 (negate-condition)
```

#### Specific mutation operators
```bash
ggen test mutate --mutators replace-binary-op,negate-condition --kill-rate-target 0.90 --fail-on-target
```

Exit code: `2` (if kill rate < 90%)

---

## Command: `ggen test budget-check`

### Purpose
Validate test execution against time/resource budgets. Fails fast if budget exceeded.

### Signature
```bash
ggen test budget-check [OPTIONS] [PATHS]
```

### Arguments

| Argument | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `PATHS` | `Vec<PathBuf>` | No | `tests/` | Test directories to check |

### Options

| Flag | Short | Type | Default | Description |
|------|-------|------|---------|-------------|
| `--output` | `-o` | `PathBuf` | stdout | Output file path for budget report JSON |
| `--format` | `-f` | `enum` | `json` | Output format: `json`, `yaml`, `human` |
| `--time-budget-ms` | | `u64` | `60000` | Time budget (milliseconds) |
| `--memory-budget-mb` | | `usize` | `500` | Memory budget (megabytes) |
| `--fail-on-violation` | | `bool` | `true` | Exit with code 2 if budget exceeded |
| `--include-breakdown` | | `bool` | `true` | Include per-file budget breakdown |

### Exit Codes

| Code | Meaning | Condition |
|------|---------|-----------|
| `0` | Success | All budgets satisfied |
| `1` | Warning | Budget violated (if `--fail-on-violation=false`) |
| `2` | Error | Budget violated (if `--fail-on-violation=true`) |
| `3` | Critical | Budget check failed (measurement error, etc.) |

### Output Schema
See `budget-violation-schema.json` for complete structure.

### Examples

#### Basic budget check
```bash
ggen test budget-check --time-budget-ms 30000
```

Output:
```json
{
  "summary": {
    "budget_met": false,
    "time_budget_ms": 30000,
    "time_used_ms": 35200,
    "memory_budget_mb": 500,
    "memory_used_mb": 245,
    "violations": [
      {
        "type": "time",
        "budget": 30000,
        "actual": 35200,
        "overage": 5200,
        "percentage": 117.3
      }
    ]
  },
  "breakdown": [...]
}
```

Exit code: `2` (time budget exceeded)

#### Strict budget enforcement
```bash
ggen test budget-check --time-budget-ms 20000 --memory-budget-mb 300 --fail-on-violation
```

Output (human format):
```
Budget Check Report
===================
Time Budget: 20.0s
Time Used: 35.2s ✗ (176%)
Memory Budget: 300 MB
Memory Used: 245 MB ✓

VIOLATIONS:
  [CRITICAL] Time budget exceeded by 15.2s (76% overage)

Recommendations:
  - Use 'ggen test optimize --time-budget-ms 20000' to reduce execution time
  - Consider increasing parallelism with --threads flag
  - Review slow tests: tests/integration/end_to_end.rs (8.5s)
```

Exit code: `2`

---

## Integration with `cargo make`

### Makefile.toml Targets

```toml
[tasks.test-audit]
description = "Audit test suite quality and coverage"
command = "ggen"
args = ["test", "audit", "--format", "human", "--fail-on-threshold"]

[tasks.test-optimize]
description = "Generate optimized test execution plan"
command = "ggen"
args = ["test", "optimize", "--time-budget-ms", "30000", "--dry-run"]

[tasks.test-mutate]
description = "Run mutation testing"
command = "ggen"
args = ["test", "mutate", "--sample-rate", "0.1", "--format", "human"]

[tasks.test-budget-check]
description = "Validate test execution against budgets"
command = "ggen"
args = ["test", "budget-check", "--time-budget-ms", "60000", "--fail-on-violation"]

[tasks.ci-test-full]
description = "Full test validation for CI (audit + mutate + budget)"
dependencies = ["test-audit", "test-mutate", "test-budget-check"]
```

### Usage in CI/CD

```yaml
# .github/workflows/ci.yml
- name: Test Audit
  run: cargo make test-audit

- name: Mutation Testing
  run: cargo make test-mutate

- name: Budget Enforcement
  run: cargo make test-budget-check
```

---

## Error Messages

All commands use structured, actionable error messages:

### Format
```json
{
  "error": {
    "code": "THRESHOLD_NOT_MET",
    "message": "Mutation kill rate below threshold",
    "details": {
      "actual": 0.75,
      "expected": 0.80,
      "difference": 0.05
    },
    "recommendations": [
      "Add mutation tests for crates/ggen-core/src/graph/store.rs (kill rate: 65%)",
      "Review survived mutants in mutation-report.json",
      "Consider using --sample-rate 0.1 for faster iteration"
    ]
  }
}
```

### Common Error Codes

| Code | Message | Recommendation |
|------|---------|----------------|
| `THRESHOLD_NOT_MET` | Threshold not satisfied | Review detailed metrics, adjust thresholds or improve tests |
| `BUDGET_EXCEEDED` | Time/memory budget exceeded | Use `ggen test optimize` to reduce execution time |
| `INVALID_METADATA` | Test metadata missing/corrupt | Run `ggen test audit` to regenerate metadata |
| `COMPILATION_ERROR` | Mutant failed to compile | Check mutation operators, may indicate dead code |
| `IO_ERROR` | File read/write failed | Verify permissions, disk space, file paths |

---

## Design Principles Alignment

### Deterministic Output
- All commands produce identical output for identical inputs
- JSON schemas enforce strict structure
- Exit codes are deterministic based on thresholds

### Zero-Cost Abstractions
- CLI parsing uses `clap` with zero-cost derive macros
- JSON serialization uses `serde` with compile-time codegen
- No runtime overhead for schema validation

### Structured Data
- All outputs are valid JSON/YAML (parseable by tools)
- Human-readable format is generated from JSON (single source of truth)
- Exit codes encode semantic meaning (success/warning/error/critical)

### Actionable Recommendations
- Every error includes concrete next steps
- Recommendations reference specific files, thresholds, commands
- No ambiguous messages ("test failed" ❌, "test X failed due to Y, try Z" ✅)

---

## Future Extensions

### Planned Commands
- `ggen test watch` - Continuous test execution with file watching
- `ggen test compare` - Compare test metrics across commits/branches
- `ggen test dashboard` - Web-based test analytics dashboard

### Planned Options
- `--profile` - Save/load test execution profiles
- `--baseline` - Compare against baseline metrics
- `--export-prometheus` - Export metrics in Prometheus format
