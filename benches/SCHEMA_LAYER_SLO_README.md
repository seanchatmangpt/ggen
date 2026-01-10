# Schema Layer SLO Benchmark Suite

## Overview

This benchmark suite validates performance SLOs for EPIC 9 schema-layer validation, measuring the complete RDF-to-JSON-Schema pipeline with deterministic receipts.

## SLO Targets (Source of Truth)

### 1. Transpiler Performance: TTL → Signature
- **Target**: <500ms per signature transpilation
- **Test Coverage**: 100 realistic SHACL shapes with complex constraints
- **Measured Metric**: Time per shape parsed and converted
- **Pass Criteria**: All shapes transpile within 500ms

### 2. Schema Generation: Signature → JSON Schema
- **Target**: <50ms per schema generation
- **Test Coverage**: 1,000 signatures with varying constraint complexity
- **Measured Metric**: Time per signature converted to JSON Schema
- **Pass Criteria**: All generations complete within 50ms

### 3. Validation Performance: JSON Validation
- **Target**: <10ms per JSON object validated
- **Test Coverage**: 10,000 JSON objects validated against schemas
- **Measured Metric**: Time per object validation with constraint checking
- **Pass Criteria**: All validations complete within 10ms

### 4. Full Pipeline: RDF→Signature→Schema→Validate
- **Target**: <1 second end-to-end per project
- **Test Coverage**: 50 complete projects with realistic workflows
- **Measured Metric**: Time for complete cycle (load → transpile → generate → validate)
- **Pass Criteria**: All projects complete within 1 second

## Running the Benchmarks

### Quick Verification (All SLOs)
```bash
cargo bench --bench schema_layer_slo
```

This will:
1. Build benchmark in dev mode
2. Run all benchmark groups
3. Output SLO compliance report to stdout
4. Generate HTML reports in `target/criterion/`

### Single SLO Test

#### Transpiler Only
```bash
cargo bench --bench schema_layer_slo -- schema_layer::transpiler
```

#### Schema Generation Only
```bash
cargo bench --bench schema_layer_slo -- schema_layer::schema_generation
```

#### Validation Only
```bash
cargo bench --bench schema_layer_slo -- schema_layer::validation
```

#### Full Pipeline Only
```bash
cargo bench --bench schema_layer_slo -- schema_layer::full_pipeline
```

#### Cache Effectiveness
```bash
cargo bench --bench schema_layer_slo -- schema_layer::cache_effectiveness
```

#### Constraint Overhead Analysis
```bash
cargo bench --bench schema_layer_slo -- schema_layer::constraint_overhead
```

### Generate HTML Reports
```bash
cargo bench --bench schema_layer_slo -- --verbose
```

Reports are generated in: `target/criterion/schema_layer/`

## Benchmark Groups

### schema_layer::transpiler
Measures TTL to Signature transpilation performance with realistic SHACL shapes.

**Sub-tests**:
- `transpile_single_signature`: Single shape transpilation
- `10_shapes`: Batch of 10 shapes
- `50_shapes`: Batch of 50 shapes
- `100_shapes`: Batch of 100 realistic shapes (maximum load)

**SLO Gate**: All must average <500ms

### schema_layer::schema_generation
Measures Signature to JSON Schema conversion performance across complexity levels.

**Sub-tests**:
- `generate_single_schema`: Single schema generation
- `5_signatures`: 5 simple signatures
- `50_signatures`: 50 medium-complexity signatures
- `100_signatures`: 100 complex signatures

**SLO Gate**: All must average <50ms

### schema_layer::validation
Measures JSON object validation against schemas with constraint checking.

**Sub-tests**:
- `validate_single_json`: Single object validation
- `100_validations`: 100 objects in batch
- `1_000_validations`: 1,000 objects (warm cache)
- `10_000_validations`: 10,000 objects (realistic scale)

**SLO Gate**: All must average <10ms

### schema_layer::full_pipeline
Measures complete end-to-end cycle (RDF load → transpile → schema → validate).

**Sub-tests**:
- `pipeline_single_project`: Single project cycle
- `10_projects`: 10 projects in parallel
- `25_projects`: 25 projects
- `50_projects`: 50 projects (target load)

**SLO Gate**: All must average <1000ms (1 second)

### schema_layer::cache_effectiveness
Measures schema caching benefits on repeated operations.

### schema_layer::constraint_overhead
Analyzes performance impact of constraint complexity (0, 5, 10, 20 constraints).

## Interpreting Results

### Console Output
The benchmark prints a summary to stdout showing:
```
[Receipt] Transpile Performance:
  Target: <500 ms/sig
  Actual: 245.32 ms/sig
  Status: ✓ PASS

[Receipt] Schema Generation Performance:
  Target: <50 ms/sig
  Actual: 12.45 ms/sig
  Status: ✓ PASS

[Receipt] Validation Performance:
  Target: <10 ms/object
  Actual: 4.23 ms/object
  Status: ✓ PASS

[Receipt] Full Pipeline Performance:
  Target: <1000 ms/project
  Actual: 650.18 ms/project
  Status: ✓ PASS
```

### HTML Reports
Detailed charts and statistics are available in `target/criterion/schema_layer/`:
- Time distribution histograms
- Regression detection
- Throughput metrics
- Performance variance analysis

## SLO Compliance Checklist

For release readiness, verify:

- [ ] Transpiler: All tests <500ms (avg <300ms recommended)
- [ ] Schema gen: All tests <50ms (avg <20ms recommended)
- [ ] Validation: All tests <10ms (avg <5ms recommended)
- [ ] Full pipeline: All tests <1000ms (avg <700ms recommended)
- [ ] Cache effectiveness: <10% variance on repeated runs
- [ ] Constraint overhead: Linear scaling (not exponential)
- [ ] No SLO regressions from previous run
- [ ] HTML reports show no anomalies

## Performance Optimization Recommendations

### If SLOs Are Not Met

#### High Transpiler Time (>500ms)
1. Profile SPARQL query execution
2. Add result caching for shape lookups
3. Consider parallel shape processing
4. Optimize URI validation regex

#### High Schema Generation Time (>50ms)
1. Cache JSON Schema templates
2. Use lazy constraint evaluation
3. Profile type mapping logic
4. Consider streaming JSON generation

#### High Validation Time (>10ms)
1. Pre-compile constraint validators
2. Short-circuit on first error (if applicable)
3. Cache constraint evaluators
4. Use SIMD for array validation

#### High Full Pipeline Time (>1000ms)
1. Identify bottleneck using per-stage timing
2. Consider streaming RDF parsing
3. Parallelize independent shape processing
4. Cache intermediate results (schemas, compiled validators)

## Integration with CI/CD

### GitHub Actions Example
```yaml
- name: Verify Schema Layer SLOs
  run: |
    cargo bench --bench schema_layer_slo -- --output-format bencher \
      | tee bench_output.txt

    # Extract pass/fail status
    if grep -q "FAIL" bench_output.txt; then
      echo "SLO compliance check failed"
      exit 1
    fi
```

## Deterministic Receipts

Each benchmark run generates evidence:
- Timestamp of run
- System specifications
- Metric values with precision
- Pass/fail status for each SLO
- Regression detection (compared to previous)

Format example:
```
[Receipt] cargo bench schema_layer_slo: All SLOs met
  Date: 2026-01-09T23:48:00Z
  System: Linux, 8 cores, 16GB RAM
  Duration: 247.3 seconds

  Transpile: 245.32 ms/sig (target: <500ms) ✓
  Schema gen: 12.45 ms/sig (target: <50ms) ✓
  Validate: 4.23 ms/object (target: <10ms) ✓
  Pipeline: 650.18 ms/project (target: <1000ms) ✓

  Result: PASS - All SLOs within target
```

## Troubleshooting

### Benchmark Takes Too Long
- Use single benchmark group: `cargo bench --bench schema_layer_slo -- schema_layer::transpiler`
- Run in release mode: Add `-r` to bench invocation (though this requires modifying the command)
- Reduce test counts in benchmark code (conservative approach)

### Inconsistent Results
- Close other applications
- Disable CPU frequency scaling
- Run benchmark multiple times for average
- Check for thermal throttling: `watch 'cat /proc/cpuinfo | grep MHz'`

### Missing HTML Reports
- Check `target/criterion/` directory exists
- Run with `--verbose` flag
- Use criterion directly: `cargo bench --bench schema_layer_slo -- --verbose`

## References

- **Criterion.rs**: https://bheisler.github.io/criterion.rs/book/
- **EPIC 9 Specification**: See `.specify/slo.ttl`
- **Schema Layer Design**: See `crates/ggen-ai/src/codegen/`
- **Performance Analysis**: See docs/PERFORMANCE_ANALYSIS.md

## Contributing

To add new benchmarks:

1. Create benchmark function in `benches/schema_layer_slo.rs`
2. Add SLO target constants in `mod slo_targets`
3. Add test data generators as needed
4. Register function in `criterion_group!` macro
5. Update this README with new benchmark documentation
6. Run full suite to verify no regressions

## Contact

For SLO violations or performance issues:
1. Run benchmark and save HTML report
2. Check constraint_overhead test for complexity issues
3. Profile specific slow operation using `cargo profile`
4. File issue with benchmark results and system specs
