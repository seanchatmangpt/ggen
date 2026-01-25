<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Benchmark Integration Guide](#benchmark-integration-guide)
  - [Overview](#overview)
  - [Quick Start](#quick-start)
    - [Run All Thesis Benchmarks](#run-all-thesis-benchmarks)
    - [Run Specific Benchmark Group](#run-specific-benchmark-group)
  - [Output Interpretation](#output-interpretation)
    - [Criterion Output Format](#criterion-output-format)
    - [Success Criteria](#success-criteria)
  - [Integration with CI/CD](#integration-with-cicd)
    - [GitHub Actions Workflow](#github-actions-workflow)
    - [GitLab CI Configuration](#gitlab-ci-configuration)
    - [Jenkins Pipeline](#jenkins-pipeline)
  - [Baseline Management](#baseline-management)
    - [Save Baseline](#save-baseline)
    - [Compare Against Baseline](#compare-against-baseline)
    - [View Comparison Results](#view-comparison-results)
  - [Local Development Workflow](#local-development-workflow)
    - [Before Starting Work](#before-starting-work)
    - [During Development](#during-development)
    - [Before Submitting PR](#before-submitting-pr)
  - [Performance Profiling](#performance-profiling)
    - [Identify Performance Bottlenecks](#identify-performance-bottlenecks)
    - [Memory Profiling](#memory-profiling)
  - [Dissertation Integration](#dissertation-integration)
    - [Success Criteria Validation](#success-criteria-validation)
  - [Troubleshooting](#troubleshooting)
    - [Benchmark Fails to Compile](#benchmark-fails-to-compile)
    - [Performance Regression](#performance-regression)
    - [Inconsistent Results](#inconsistent-results)
  - [Reporting](#reporting)
    - [Generate Report](#generate-report)
    - [Example Report Format](#example-report-format)
  - [Adding New Benchmarks](#adding-new-benchmarks)
    - [Template](#template)
    - [Documentation Template](#documentation-template)
  - [Maintenance](#maintenance)
    - [Monthly Reviews](#monthly-reviews)
    - [Quarterly Deep Dives](#quarterly-deep-dives)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Benchmark Integration Guide

## Overview

This guide explains how to run, interpret, and integrate the thesis validation benchmarks into your development workflow and CI/CD pipelines.

## Quick Start

### Run All Thesis Benchmarks

```bash
cd /home/user/ggen/crates/ggen-core
cargo bench --bench thesis_validation_benchmarks
```

### Run Specific Benchmark Group

```bash
# Performance scalability (H4 hypothesis)
cargo bench --bench thesis_validation_benchmarks -- H4_PerformanceScalability

# Enterprise scalability (RQ4)
cargo bench --bench thesis_validation_benchmarks -- RQ4_EnterpriseScalability

# Deterministic generation (H3)
cargo bench --bench thesis_validation_benchmarks -- H3_DeterministicGeneration

# Correctness metrics
cargo bench --bench thesis_validation_benchmarks -- CorrectnessMetrics

# Scaling complexity
cargo bench --bench thesis_validation_benchmarks -- ScalingComplexity

# Memory efficiency
cargo bench --bench thesis_validation_benchmarks -- MemoryEfficiency

# Generation quality
cargo bench --bench thesis_validation_benchmarks -- GenerationQuality
```

## Output Interpretation

### Criterion Output Format

```
Benchmarking H4_PerformanceScalability/500_triples: Collecting 100 samples
H4_PerformanceScalability/500_triples
                        time:   [2.5 ms 2.6 ms 2.8 ms]
                        found 0 outliers among 100 samples (0.00%)

Benchmarking H4_PerformanceScalability/5000_triples: Collecting 100 samples
H4_PerformanceScalability/5000_triples
                        time:   [85.2 ms 87.4 ms 89.8 ms]
                        found 2 outliers among 100 samples (2.00%)
```

**Interpretation**:
- `time:`: Mean time with confidence interval (95%)
- `[low, point, high]`: Lower bound, point estimate, upper bound
- Status: ✅ if within thesis success criteria, ❌ if exceeds

### Success Criteria

| Benchmark | Criterion | Status |
|-----------|-----------|--------|
| H4 (5000 triples) | <100ms | ✅ if 87.4ms < 100ms |
| H4 (50000 triples) | <500ms | ✅ if <500ms |
| RQ4 Memory | <500MB | ✅ if measurement OK |
| H3 Determinism | 100 runs identical | ✅ if all hashes match |
| Correctness | 100% coverage | ✅ if all assertions pass |

## Integration with CI/CD

### GitHub Actions Workflow

```yaml
name: Thesis Validation Benchmarks

on:
  push:
    branches: [main, development]
  pull_request:
    branches: [main]

jobs:
  benchmark:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: dtolnay/rust-toolchain@stable

      - name: Run Thesis Validation Benchmarks
        run: |
          cd crates/ggen-core
          cargo bench --bench thesis_validation_benchmarks -- --output-format bencher | tee output.txt

      - name: Store Benchmark Result
        uses: benchmark-action/github-action-benchmark@v1
        with:
          tool: 'cargo'
          output-file-path: output.txt
          github-token: ${{ secrets.GITHUB_TOKEN }}
          auto-push: true

      - name: Check for Performance Regressions
        run: |
          if cargo bench --bench thesis_validation_benchmarks 2>&1 | grep -q "regressed"; then
            echo "❌ Performance regression detected"
            exit 1
          else
            echo "✅ All benchmarks within thresholds"
          fi
```

### GitLab CI Configuration

```yaml
thesis-benchmarks:
  stage: test
  script:
    - cd crates/ggen-core
    - cargo bench --bench thesis_validation_benchmarks
  artifacts:
    paths:
      - target/criterion/
    expire_in: 1 week
  allow_failure: false  # Fail pipeline if benchmarks regress
```

### Jenkins Pipeline

```groovy
pipeline {
    agent any

    stages {
        stage('Benchmark') {
            steps {
                dir('crates/ggen-core') {
                    sh 'cargo bench --bench thesis_validation_benchmarks'
                }
            }
        }

        stage('Analyze Results') {
            steps {
                script {
                    def result = sh(
                        script: 'cargo bench --bench thesis_validation_benchmarks 2>&1 | grep -c "regressed"',
                        returnStdout: true
                    ).trim()

                    if (result.toInteger() > 0) {
                        error("Performance regression detected: ${result} benchmarks regressed")
                    }
                }
            }
        }
    }

    post {
        always {
            publishHTML([
                reportDir: 'crates/ggen-core/target/criterion',
                reportFiles: 'report/index.html',
                reportName: 'Criterion Benchmarks'
            ])
        }
    }
}
```

## Baseline Management

### Save Baseline

```bash
cargo bench --bench thesis_validation_benchmarks -- --save-baseline dissertation_baseline
```

### Compare Against Baseline

```bash
cargo bench --bench thesis_validation_benchmarks -- --baseline dissertation_baseline
```

### View Comparison Results

```bash
# Open HTML report
open crates/ggen-core/target/criterion/report/index.html
```

## Local Development Workflow

### Before Starting Work

```bash
# Save current performance baseline
cargo bench --bench thesis_validation_benchmarks -- --save-baseline before_changes
```

### During Development

```bash
# Quick sanity check (subset of benchmarks)
cargo bench --bench thesis_validation_benchmarks -- H4_PerformanceScalability/5000_triples

# Full validation
cargo bench --bench thesis_validation_benchmarks
```

### Before Submitting PR

```bash
# Compare against baseline
cargo bench --bench thesis_validation_benchmarks -- --baseline before_changes

# If regression detected, optimize and re-run
# Acceptable: <5% regression for new features
# Unacceptable: >10% regression
```

## Performance Profiling

### Identify Performance Bottlenecks

```bash
# Generate flamegraph
cargo bench --bench thesis_validation_benchmarks -- --profile-time 10

# Use perf for Linux
perf record -g target/release/deps/thesis_validation_benchmarks-*
perf report
```

### Memory Profiling

```bash
# Using valgrind (Linux)
valgrind --tool=massif --massif-out-file=massif.out \
  ./target/release/deps/thesis_validation_benchmarks-*

massif-visualizer massif.out
```

## Dissertation Integration

### Success Criteria Validation

After running benchmarks, verify:

1. **H3 (Determinism)**
   - [ ] 100 runs produce identical SHA-256 hashes
   - [ ] All hashes in output match: `all 100 runs identical`

2. **H4 (Performance)**
   - [ ] 5000 triples: <100ms (typical: ~85ms)
   - [ ] 50000 triples: <500ms (typical: ~400ms)
   - [ ] No quadratic scaling (verify scaling factor <30x for 50x input)

3. **RQ4 (Enterprise Scalability)**
   - [ ] Blog API: <50ms
   - [ ] E-commerce: <100ms
   - [ ] ERP system: <400ms
   - [ ] Memory: <500MB

4. **Correctness**
   - [ ] H1: Specification gap <1% (0% expected)
   - [ ] Entity coverage: 100%
   - [ ] Type consistency: 100%
   - [ ] All assertions pass

5. **Scaling Complexity**
   - [ ] Performance scaling is sub-linear (O(n) or O(n log n))
   - [ ] NOT quadratic O(n²)
   - [ ] Verify: 50x input ≈ 20-40x time, not 2500x

## Troubleshooting

### Benchmark Fails to Compile

```bash
# Check dependencies
cargo tree --duplicates

# Update dependencies
cargo update

# Rebuild
cargo clean
cargo bench --bench thesis_validation_benchmarks
```

### Performance Regression

1. **Small regression (1-5%)**
   - Expected from code changes
   - Investigate only if >10%

2. **Medium regression (5-10%)**
   - Profile to identify cause
   - Consider optimization

3. **Large regression (>10%)**
   - ❌ FAIL: Do not merge
   - Profile and optimize before resubmitting

### Inconsistent Results

```bash
# Ensure consistent system state
- Close background applications
- Disable CPU turbo-boost:
  echo 1 | sudo tee /sys/devices/system/cpu/intel_pstate/no_turbo
- Increase sample size:
  cargo bench --bench thesis_validation_benchmarks -- --sample-size 500
```

## Reporting

### Generate Report

```bash
cargo bench --bench thesis_validation_benchmarks -- --verbose

# Extract results
cargo bench --bench thesis_validation_benchmarks 2>&1 | tee benchmark_results.txt
```

### Example Report Format

```markdown
# Thesis Validation Benchmark Report
Date: 2026-01-06

## Summary
- ✅ All benchmarks passed
- Performance within dissertation criteria
- No regressions vs. baseline

## Detailed Results

### H4: Performance Scalability
- 5000 triples: 87.4ms ✅ (<100ms)
- 50000 triples: 398.7ms ✅ (<500ms)

### RQ4: Enterprise Scalability
- Blog API (450t): 23.5ms ✅
- E-commerce (3200t): 89.2ms ✅
- ERP (48000t): 387.4ms ✅

### H3: Determinism
- 100 runs: All identical ✅

### Correctness
- H1 gap: 0.0% ✅
- Entity coverage: 100% ✅
- Type consistency: 100% ✅

## Recommendations
- Continue monitoring performance
- Re-baseline after major optimization work
- Consider adding more case studies
```

## Adding New Benchmarks

### Template

```rust
fn new_benchmark_group(c: &mut Criterion) {
    let mut group = c.benchmark_group("MyBenchmarkGroup");

    group.bench_function("test_name", |b| {
        b.iter(|| {
            // Test code here
            black_box(result)
        });
    });

    group.finish();
}

// Add to criterion_group! macro:
criterion_group!(
    name = benches;
    config = Criterion::default().significance_level(0.1);
    targets = my_new_benchmark_group
);
```

### Documentation Template

```markdown
## NewBenchmarkGroup

**Hypothesis**: [Describe what you're testing]

**Success Criteria**: [Define thresholds]

**Test Cases**: [List scenarios]

**Real-World Mapping**: [How does this relate to actual usage?]
```

## Maintenance

### Monthly Reviews

```bash
# Compare against baseline from start of month
cargo bench --bench thesis_validation_benchmarks -- --baseline month_baseline

# Update if improvements detected
cargo bench --bench thesis_validation_benchmarks -- --save-baseline month_baseline
```

### Quarterly Deep Dives

- Profile hot paths
- Analyze outliers
- Optimize critical sections
- Update case studies with new measurements

## References

- Dissertation Chapter 11: Evaluation Methodology
- Performance Metrics Table: Page XXX
- Baseline Comparison Table: Page XXX
- Criterion Documentation: https://bheisler.github.io/criterion.rs/book/

---

**Last Updated**: January 2026
**Framework Version**: ggen 0.5.1
**Benchmark Suite**: thesis_validation_benchmarks v1.0
