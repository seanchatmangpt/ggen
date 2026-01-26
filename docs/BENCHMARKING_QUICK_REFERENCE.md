# Benchmarking Quick Reference

## Essential Commands

### Quick Performance Check
```bash
# Fastest way to check SLO compliance
cargo make quick-perf
```

### Full Benchmarking
```bash
# Complete benchmark pipeline
cargo make benchmark-full
```

### Individual Measurements

#### Collect SLO Metrics
```bash
cargo make slo-track
# Results: .metrics/results/
```

#### Detect Regressions
```bash
cargo make slo-detect-regression
# Report: .metrics/regression_report_*.txt
```

#### View Dashboard
```bash
cargo make slo-dashboard
open .metrics/benchmark_dashboard.html
```

## Run Specific Benchmarks

### Core SLO Framework
```bash
cargo make bench-core-slo
cargo bench --bench core_slo_framework
```

### Build Metrics
```bash
cargo make bench-detailed-build
cargo bench --bench detailed_build_metrics
```

### Test Performance
```bash
cargo make bench-test-execution
cargo bench --bench test_execution_benchmarks
```

### Comprehensive Suite
```bash
cargo make bench-comprehensive
cargo bench --bench comprehensive_performance_suite
```

## Monitoring

### Continuous Monitoring (Watch Mode)
```bash
cargo make monitor-perf
# Runs SLO checks every 5 minutes (Ctrl+C to stop)
```

### Real-time Build Monitoring
```bash
cargo make perf-monitor-build
```

### Memory Monitoring
```bash
cargo make perf-monitor-memory [duration_seconds]
# Example: cargo make perf-monitor-memory 60
```

## Data and Reporting

### View Recent Results
```bash
# List all recent measurements
ls -la .metrics/results/

# View latest first build time
cat .metrics/results/first_build_*.txt | tail -1

# View all measurements
grep "value=" .metrics/results/*.txt
```

### View Baselines
```bash
# List baselines
cat .metrics/baselines/*.baseline

# Extract baseline value
grep "value=" .metrics/baselines/first_build_time.baseline
```

### Export Results
```bash
# Export to JSON
cargo make benchmark-export

# Analyze exported data
python3 -c "import json; print(json.dumps(json.load(open('.metrics/export_*.json')), indent=2))"
```

### Check Dashboard
```bash
# Open interactive dashboard
open .metrics/benchmark_dashboard.html

# Or with alternative browser
firefox .metrics/benchmark_dashboard.html
chromium .metrics/benchmark_dashboard.html
```

## SLO Target Reference

| Metric | Target | Category |
|--------|--------|----------|
| First Build | ≤ 15s | Build Performance |
| Incremental Build | ≤ 2s | Build Performance |
| Release Build | ≤ 30s | Build Performance |
| Test Build | ≤ 10s | Build Performance |
| RDF Processing | ≤ 5s | Runtime Performance |
| CLI Scaffolding | ≤ 3s | Runtime Performance |
| Generation Memory | ≤ 100MB | Resource Usage |
| Binary Size | ≤ 500MB | Binary Metrics |
| Full Test Suite | ≤ 30s | Test Performance |
| Unit Tests | ≤ 10s | Test Performance |
| Integration Tests | ≤ 20s | Test Performance |

## Common Workflows

### Before Commit
```bash
# Quick performance check
cargo make quick-perf

# If successful, proceed to commit
git add ...
git commit -m "..."
```

### After Optimization
```bash
# Measure improvement
cargo make slo-track

# Check for regressions
cargo make slo-detect-regression

# View dashboard
cargo make slo-dashboard && open .metrics/benchmark_dashboard.html
```

### Weekly Performance Review
```bash
# Full benchmarking
cargo make benchmark-full

# Export data for analysis
cargo make benchmark-export

# Review trends in dashboard
```

### Continuous Integration
```bash
# In CI/CD pipeline
cargo make benchmark-full

# Check regression report
if [ -f .metrics/regression_report_*.txt ]; then
    cat .metrics/regression_report_*.txt | head -20
fi
```

## Troubleshooting

### Benchmarks Timeout
```bash
# Run specific benchmark with longer timeout
timeout 120s cargo bench --bench core_slo_framework

# Or run without timeout (not recommended)
cargo bench --bench core_slo_framework -- --verbose
```

### SLO Violation Occurred
```bash
# 1. Check what changed
git diff HEAD~1

# 2. Profile the operation
cargo bench --bench comprehensive_performance_suite

# 3. Identify bottleneck
# Look at the slowest benchmark

# 4. Optimize and re-measure
cargo make slo-track

# 5. Verify improvement
cargo make slo-detect-regression
```

### Dashboard Not Loading
```bash
# Verify file exists
ls -la .metrics/benchmark_dashboard.html

# Regenerate if needed
cargo make slo-dashboard

# Check browser developer tools for errors
```

### Scripts Not Executable
```bash
# Make scripts executable
chmod +x scripts/*.sh

# Verify
ls -l scripts/*.sh | grep "x"
```

## File Organization

```
Project Root/
├── benches/
│   ├── core_slo_framework.rs
│   ├── detailed_build_metrics.rs
│   ├── test_execution_benchmarks.rs
│   └── comprehensive_performance_suite.rs
├── scripts/
│   ├── slo_tracking_system.sh
│   ├── regression_detection.sh
│   └── benchmark_dashboard_generator.sh
├── docs/
│   ├── BENCHMARKING_GUIDE.md
│   ├── BENCHMARKING_IMPLEMENTATION_SUMMARY.md
│   └── BENCHMARKING_QUICK_REFERENCE.md (this file)
├── .metrics/
│   ├── comprehensive_suite_metadata.json
│   ├── baselines/
│   ├── results/
│   ├── history/
│   └── benchmark_dashboard.html
└── Makefile.toml (updated with benchmark tasks)
```

## Task Status Indicators

- 🟢 **GREEN**: All SLOs met, no issues
- 🟡 **YELLOW**: Approaching SLO limits, minor degradation
- 🔴 **RED**: SLO violated, immediate attention needed

## Useful Aliases

Add to your `.bashrc` or `.zshrc`:

```bash
# Quick SLO check
alias perf-check='cargo make quick-perf'

# Full benchmarking
alias perf-full='cargo make benchmark-full'

# View dashboard
alias perf-dash='cargo make slo-dashboard && open .metrics/benchmark_dashboard.html'

# Detect regressions
alias perf-regression='cargo make slo-detect-regression'

# Monitor
alias perf-monitor='cargo make monitor-perf'

# Export
alias perf-export='cargo make benchmark-export'
```

Usage:
```bash
perf-check        # Quick check
perf-full         # Full pipeline
perf-dash         # View dashboard
perf-regression   # Check regressions
perf-monitor      # Continuous monitoring
perf-export       # Export data
```

## Environment Variables

### Customize SLO Targets
Edit `scripts/slo_tracking_system.sh`:

```bash
# Build SLO targets (seconds)
SLO_FIRST_BUILD_SECS=15
SLO_INCREMENTAL_BUILD_SECS=2
SLO_RDF_PROCESSING_SECS=5

# Memory targets (MB)
SLO_GENERATION_MEMORY_MB=100
SLO_BINARY_SIZE_MB=500

# CLI target (seconds)
SLO_CLI_SCAFFOLDING_SECS=3
```

### Regression Threshold
Edit `scripts/regression_detection.sh`:

```bash
# Alert on >10% regression (modify as needed)
REGRESSION_THRESHOLD=10
```

## Quick Diagnostics

### Check System Performance
```bash
# Compare current vs baseline
for metric in first_build incremental_build test_execution binary_size; do
    echo "=== $metric ==="
    baseline=$(grep value .metrics/baselines/${metric}*.baseline 2>/dev/null | head -1)
    current=$(grep value .metrics/results/${metric}*.txt 2>/dev/null | tail -1)
    echo "Baseline: $baseline"
    echo "Current:  $current"
    echo ""
done
```

### Identify Performance Regressions
```bash
# Run regression detection
cargo make slo-detect-regression

# View detailed report
cat .metrics/regression_report_*.txt
```

### Performance Profiling
```bash
# Build with profiling enabled
RUSTFLAGS="-g" cargo build --release

# Profile with perf (Linux)
perf record ./target/release/ggen [args]
perf report

# Or use flamegraph
cargo flamegraph --bin ggen
```

## Best Practices

1. **Run Before Commit**: Always run `quick-perf` before pushing
2. **Track Trends**: Use weekly `benchmark-full` runs for trending
3. **Investigate Regressions**: Don't ignore yellow/red status
4. **Document Changes**: Note code changes affecting performance
5. **Use Baselines**: Understand baseline before optimization
6. **Profile First**: Measure before optimizing
7. **Test Locally**: Run benchmarks on your machine first
8. **Archive Results**: Keep historical data for analysis

## Support

For detailed information:
- See [BENCHMARKING_GUIDE.md](./BENCHMARKING_GUIDE.md) for complete guide
- See [BENCHMARKING_IMPLEMENTATION_SUMMARY.md](./BENCHMARKING_IMPLEMENTATION_SUMMARY.md) for implementation details
- Check `.metrics/comprehensive_suite_metadata.json` for configuration

---

**Last Updated**: 2026-01-26 | **Version**: 1.0.0
