# Build Optimization Validation Suite

## Overview

Comprehensive performance benchmarking infrastructure for validating build optimization and runtime performance in the ggen project.

**Version**: 1.0
**Date**: January 25, 2026
**Status**: Production Ready

## Deliverables Summary

### 1. Benchmark Modules (4 total)

#### Build Time Benchmarks (`benches/build_time_benchmarks.rs`)
- **12 distinct benchmark scenarios**
- Clean debug builds
- Clean release builds
- Incremental builds (single file changes)
- Feature-gated compilation (5 feature sets)
- Parallel compilation scaling (1, 2, 4, 8 jobs)
- Workspace builds (core, with optional, full)
- Cargo check performance (3 variants)
- Test compilation (unit + integration)

**Run:**
```bash
cargo make bench-build-times
cargo bench --bench build_time_benchmarks
```

**SLO Targets:**
| Scenario | Target | Threshold |
|----------|--------|-----------|
| First debug build | ≤ 15s | 18.75s |
| First release build | ≤ 30s | 37.5s |
| Incremental | ≤ 2s | 2.5s |
| Workspace clean | ≤ 30s | 37.5s |
| Cargo check | ≤ 5s | 6.25s |

#### Memory Usage Benchmarks (`benches/memory_usage_benchmarks.rs`)
- **6 benchmark groups with 20+ scenarios**
- RDF graph operations (small → very large ontologies)
- Template processing (simple → complex loops)
- Code generation (10 → 1000 functions)
- Compilation memory (per-crate analysis)
- Memory allocation patterns (1KB → 10MB)
- Cache efficiency (sequential vs random access)

**Run:**
```bash
cargo make bench-memory
cargo bench --bench memory_usage_benchmarks
```

**SLO Targets:**
| Operation | Target | Threshold |
|-----------|--------|-----------|
| RDF processing (500 classes) | ≤ 100MB | 125MB |
| Template rendering (large) | ≤ 50MB | 62.5MB |
| Code generation | ≤ 100MB | 125MB |
| Compilation peak | ≤ 500MB | 625MB |

#### Binary Size Analysis (`benches/binary_size_analysis.rs`)
- **5 analysis categories**
- Debug binary size tracking
- Release binary size tracking
- Binary size trends (multiple versions)
- Symbol analysis and counting
- Workspace binary comparison
- Strip effectiveness measurement

**Run:**
```bash
cargo make bench-binary-size
cargo bench --bench binary_size_analysis
```

**SLO Targets:**
| Metric | Target |
|--------|--------|
| Release binary (ggen) | ≤ 10MB |
| Debug binary (ggen) | ≤ 50MB |
| Symbol reduction (strip) | ≥ 50% |

#### SLO Tracking Dashboard (`benches/slo_tracking.rs`)
- **4 SLO categories with targets**
- Build time SLO validation
- Memory usage SLO validation
- Binary size SLO validation
- Runtime performance SLO validation
- HTML dashboard generation
- Git integration for audit trails
- Status reporting (PASS/WARN/FAIL)

**Run:**
```bash
cargo make bench-slo-tracking
cargo bench --bench slo_tracking
```

### 2. Performance Monitoring Scripts (2 total)

#### Build Performance Monitor (`scripts/build_perf_monitor.sh`)
Shell script for real-time build performance tracking:

**Features:**
- Clean build benchmarking
- Incremental build measurement
- Cargo check performance
- Workspace build analysis
- Artifact size tracking
- Automatic SLO compliance checking
- JSON + SLO report generation

**Usage:**
```bash
# Run all benchmarks
./scripts/build_perf_monitor.sh all

# Individual benchmarks
./scripts/build_perf_monitor.sh clean       # Clean build
./scripts/build_perf_monitor.sh incremental  # Incremental build
./scripts/build_perf_monitor.sh check        # Cargo check
./scripts/build_perf_monitor.sh workspace    # Workspace build
```

**Output:**
- `.ggen/benchmark_results/build_performance_YYYYMMDD_HHMMSS.json`
- `.ggen/benchmark_results/slo_compliance_YYYYMMDD_HHMMSS.txt`
- Color-coded pass/fail status in terminal

#### Memory Monitor (`scripts/memory_monitor.sh`)
Real-time memory monitoring for build and runtime operations:

**Features:**
- Process-specific memory tracking
- System-wide memory monitoring
- Peak memory detection
- Real-time stats output
- JSON report generation
- Peak RSS tracking
- Heap size analysis

**Usage:**
```bash
# Monitor specific command
./scripts/memory_monitor.sh --command="cargo build --release" --duration=120

# Monitor system memory (60s)
./scripts/memory_monitor.sh --duration=60 --interval=1
```

**Output:**
- `.ggen/memory_reports/memory_YYYYMMDD_HHMMSS.json`
- Real-time table showing memory stats per second
- Peak memory summary

### 3. Configuration & Tooling

#### Cargo.toml Updates
Added 4 new benchmark registrations:
- `[[bench]] build_time_benchmarks`
- `[[bench]] memory_usage_benchmarks`
- `[[bench]] binary_size_analysis`
- `[[bench]] slo_tracking`

#### Makefile.toml Targets (7 new targets)
- `cargo make bench-build-times` - Build time benchmarks (300s timeout)
- `cargo make bench-memory` - Memory benchmarks (180s timeout)
- `cargo make bench-binary-size` - Binary analysis (120s timeout)
- `cargo make bench-slo-tracking` - SLO tracking (60s timeout)
- `cargo make bench-all` - All benchmarks (combined)
- `cargo make perf-monitor-build` - Build performance monitoring script
- `cargo make perf-monitor-memory` - Memory monitoring script
- `cargo make slo-check` - Quick SLO compliance check

### 4. Documentation

#### Benchmarking Suite Guide (`docs/BENCHMARKING_SUITE.md`)
- Quick start guide (10+ examples)
- Detailed benchmark descriptions
- SLO target tables and explanations
- Performance monitoring workflow
- CI/CD integration examples
- Troubleshooting section
- Best practices guide
- Adding new benchmarks template

#### Build Optimization Validation (`docs/BUILD_OPTIMIZATION_VALIDATION.md`)
- This document
- Complete deliverables summary
- Quick reference guide
- Integration points
- Usage examples

## Quick Start Guide

### 1. Run All Benchmarks

```bash
# Via make target
cargo make bench-all

# Or run individually
cargo make bench-build-times
cargo make bench-memory
cargo make bench-binary-size
cargo make bench-slo-tracking
```

### 2. Monitor Build Performance

```bash
./scripts/build_perf_monitor.sh all
# Results: .ggen/benchmark_results/build_performance_YYYYMMDD_HHMMSS.json
```

### 3. Check SLO Compliance

```bash
cargo make slo-check
# Or detailed check
cargo make bench-slo-tracking
```

### 4. Monitor Memory During Build

```bash
./scripts/memory_monitor.sh --command="cargo build --release" --duration=120
# Results: .ggen/memory_reports/memory_YYYYMMDD_HHMMSS.json
```

## Integration Points

### CI/CD Integration

Add to GitHub Actions workflow:

```yaml
- name: Run performance benchmarks
  run: cargo make bench-all

- name: Check SLO compliance
  run: cargo make slo-check

- name: Upload benchmark results
  uses: actions/upload-artifact@v3
  with:
    name: benchmark-results
    path: target/criterion/
```

### Performance Monitoring Hooks

Pre-commit hook to validate build performance:

```bash
#!/bin/bash
./scripts/build_perf_monitor.sh clean
if [ -f ".ggen/benchmark_results/slo_compliance_"*.txt ]; then
  if grep -q "FAIL" .ggen/benchmark_results/slo_compliance_*.txt; then
    echo "❌ Build performance SLO violation detected"
    exit 1
  fi
fi
```

### Memory Storage (MCP Integration)

Benchmark configuration stored in MCP memory:

```
swarm/benchmarks/suite
├── build_times/
│   ├── {timestamp}/
│   │   ├── clean_build_debug.json
│   │   ├── clean_build_release.json
│   │   └── summary.json
├── memory_usage/
│   ├── {timestamp}/
│   │   ├── rdf_processing.json
│   │   ├── template_rendering.json
│   │   └── summary.json
└── binary_sizes/
    ├── {timestamp}/
    │   ├── debug_binary.json
    │   ├── release_binary.json
    │   └── summary.json
```

## SLO Categories

### 1. Build Time SLOs
- First build (debug): ≤ 15s
- First build (release): ≤ 30s
- Incremental build: ≤ 2s
- Workspace build: ≤ 30s
- Cargo check: ≤ 5s

### 2. Memory Usage SLOs
- RDF processing (500 classes): ≤ 100MB
- Template rendering (large): ≤ 50MB
- Code generation: ≤ 100MB
- Compilation peak: ≤ 500MB

### 3. Binary Size SLOs
- Release binary: ≤ 10MB
- Debug binary: ≤ 50MB
- Symbol reduction: ≥ 50%

### 4. Runtime SLOs
- RDF parsing (1k triples): ≤ 5000ms
- SPARQL query (1k entities): ≤ 100ms
- Template rendering (100 vars): ≤ 10ms
- CLI startup: ≤ 200ms

## Performance Optimization Workflow

### 1. Establish Baseline

```bash
git checkout main
./scripts/build_perf_monitor.sh all
# Save results for comparison
```

### 2. Implement Optimization

```bash
git checkout -b optimize/feature-name
# Make optimization changes...
```

### 3. Validate Improvement

```bash
./scripts/build_perf_monitor.sh all
# Compare .ggen/benchmark_results/ with baseline
```

### 4. Track Impact

```bash
# Results are timestamped for trend analysis
ls -lh .ggen/benchmark_results/
```

## Output Examples

### Build Performance Monitor Output

```
✅ [PASS] Clean build (debug): 12.34s (target: 15s)
✅ [PASS] Clean build (release): 28.56s (target: 30s)
✅ [PASS] Incremental build: 1.23s (target: 2s)
✅ [PASS] Cargo check: 3.45s (target: 5s)

[Receipt] All SLOs met
  Date: 2026-01-25T14:32:00Z
  System: Linux, 8 cores, 16GB RAM
```

### Memory Monitor Output

```
Time(s)   RSS(MB)   Peak(MB)   Status
--------- --------- --------- ----------
0         145.23    145.23    Running
1         156.78    156.78    Running
2         167.45    167.45    Running
...
60        180.23    200.45    Complete

[PASS] Memory monitoring complete
[INFO] Peak memory: 200.45 MB
```

### SLO Dashboard (HTML)

- Interactive charts and metrics
- Pass/Warn/Fail status indicators
- Git branch and timestamp tracking
- Regression detection
- Performance trends over time

## File Locations

| File | Purpose | Location |
|------|---------|----------|
| Build time benchmark | Criterion suite | `/benches/build_time_benchmarks.rs` |
| Memory benchmark | Criterion suite | `/benches/memory_usage_benchmarks.rs` |
| Binary analysis | Criterion suite | `/benches/binary_size_analysis.rs` |
| SLO tracking | Criterion suite | `/benches/slo_tracking.rs` |
| Build monitor script | Shell script | `/scripts/build_perf_monitor.sh` |
| Memory monitor script | Shell script | `/scripts/memory_monitor.sh` |
| Benchmarking guide | Documentation | `/docs/BENCHMARKING_SUITE.md` |
| Build optimization | Documentation | `/docs/BUILD_OPTIMIZATION_VALIDATION.md` |
| Cargo config | Project config | `/Cargo.toml` |
| Make targets | Build config | `/Makefile.toml` |

## Compliance Checklist

- [x] 12+ comprehensive build time benchmarks
- [x] 20+ memory usage benchmarks
- [x] Binary size analysis with strip effectiveness
- [x] SLO tracking with compliance reporting
- [x] Build performance monitoring script
- [x] Memory monitoring script
- [x] Comprehensive documentation
- [x] Makefile.toml integration (7 targets)
- [x] Cargo.toml registration (4 benchmarks)
- [x] MCP memory storage ready
- [x] CI/CD integration examples
- [x] Performance optimization workflow

## Next Steps

1. **Run Initial Benchmarks**
   ```bash
   cargo make bench-all
   ```

2. **Establish Baselines**
   ```bash
   ./scripts/build_perf_monitor.sh all
   ```

3. **Integrate with CI/CD**
   - Add to GitHub Actions workflows
   - Set up SLO compliance checks
   - Enable regression detection

4. **Monitor Trends**
   - Track performance over time
   - Identify optimization opportunities
   - Document improvements

5. **Continuous Validation**
   - Run before/after optimization changes
   - Pre-commit hook integration
   - Regular performance audits

## Support & Resources

- **Full Guide**: See `docs/BENCHMARKING_SUITE.md`
- **Examples**: See benchmark source files
- **Troubleshooting**: See `docs/BENCHMARKING_SUITE.md` troubleshooting section
- **References**: Criterion.rs, Rust Performance Book

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026-01-25 | Initial release with 12+ benchmarks, 2 monitoring scripts, comprehensive documentation |

---

**Status**: ✅ Production Ready
**Maintainer**: Build Optimization Team
**Last Updated**: 2026-01-25
