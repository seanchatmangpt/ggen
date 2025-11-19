# Performance Benchmarker Agent - Week 3 Mission

## Objective
Validate all quick wins, benchmark optimizations, and ensure A+ performance grade (95+/100).

## Timeline
Continuous validation (Day 1-5, triggered by Backend Dev)

## Validation Responsibilities

### 1. Quick Wins Validation (Day 1)

**Validate Quick Win 1: Parallel Template Processing**
```bash
# Baseline: Sequential processing
hyperfine 'cargo run --release -- generate multi-template-project' \
  --warmup 2 --runs 10 --export-markdown baseline.md

# After: Parallel processing
hyperfine 'cargo run --release -- generate multi-template-project' \
  --warmup 2 --runs 10 --export-markdown parallel.md

# Expected: 40-60% improvement
```

**Validate Quick Win 2: Lazy Initialization**
```bash
# Test startup time
hyperfine 'cargo run --release -- --version' \
  --warmup 5 --runs 50

# Test first generation
hyperfine 'cargo run --release -- generate small-project' \
  --warmup 1 --runs 10

# Expected: 30-50% startup improvement
```

**Validate Quick Win 3: String Optimization**
```bash
# Memory profiling
valgrind --tool=massif cargo run --release -- generate large-project
ms_print massif.out.* > memory_profile.txt

# Allocation tracking
cargo build --release && \
  heaptrack target/release/ggen generate large-project

# Expected: 20-30% memory reduction
```

**Success Criteria**:
- All quick wins show positive improvements
- No regressions in functionality
- Improvements match estimates (±10%)

### 2. Optimization Benchmarking (Day 2-4)

**Optimization 1: Lockfile Resolution**
```bash
# Create benchmark suite
cat > benches/lockfile_bench.rs << 'EOF'
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::lockfile::resolve_dependencies;

fn lockfile_benchmark(c: &mut Criterion) {
    let deps = load_test_dependencies(); // 100+ deps

    c.bench_function("lockfile_resolution", |b| {
        b.iter(|| resolve_dependencies(black_box(&deps)))
    });
}

criterion_group!(benches, lockfile_benchmark);
criterion_main!(benches);
EOF

# Run benchmark
cargo bench --bench lockfile_bench

# Compare before/after
cargo bench --bench lockfile_bench --baseline before
cargo bench --bench lockfile_bench
critcmp before new

# Expected: 60-70% improvement
```

**Optimization 2: RDF Caching**
```bash
# Benchmark RDF parsing with cache
cat > benches/rdf_bench.rs << 'EOF'
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::rdf::RdfCache;

fn rdf_benchmark(c: &mut Criterion) {
    let mut cache = RdfCache::new();
    let schema_uri = "test://schema.rdf";

    c.bench_function("rdf_cold_cache", |b| {
        b.iter(|| cache.get_or_parse(black_box(schema_uri)))
    });

    // Warm cache
    cache.get_or_parse(schema_uri).unwrap();

    c.bench_function("rdf_hot_cache", |b| {
        b.iter(|| cache.get_or_parse(black_box(schema_uri)))
    });
}

criterion_group!(benches, rdf_benchmark);
criterion_main!(benches);
EOF

# Expected: 70-80% improvement on hot cache
```

**Optimization 3: Template Processing**
```bash
# Benchmark template compilation
cat > benches/template_bench.rs << 'EOF'
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_core::templates::TemplateCache;

fn template_benchmark(c: &mut Criterion) {
    let mut cache = TemplateCache::new();
    let template_path = "templates/rust/lib.rs.hbs";

    c.bench_function("template_compile", |b| {
        b.iter(|| cache.compile_and_cache(black_box(template_path)))
    });

    c.bench_function("template_render", |b| {
        let compiled = cache.compile_and_cache(template_path).unwrap();
        let vars = load_test_variables();
        b.iter(|| compiled.render(black_box(&vars)))
    });
}

criterion_group!(benches, template_benchmark);
criterion_main!(benches);
EOF

# Expected: 50-60% improvement
```

### 3. Regression Testing

**Continuous Regression Checks**:
```bash
# Full benchmark suite
cargo bench --all

# Compare against baseline
critcmp baseline current

# Alert if any benchmark regresses >5%
if [ regression_found ]; then
    npx claude-flow@alpha hooks notify \
        --message "⚠️ REGRESSION: [benchmark] regressed by [X]%"
fi
```

**Regression Categories**:
- Build time: Should not increase
- Test execution: Should not increase >10%
- Binary size: Should not increase >5%
- Memory usage: Should not increase

### 4. Performance Grade Calculation

**SLA Dashboard Metrics**:
```bash
# Generate comprehensive performance report
python3 << 'EOF'
import json

metrics = {
    "build_time": measure_build_time(),
    "test_time": measure_test_time(),
    "generation_time": measure_generation_time(),
    "memory_usage": measure_memory_usage(),
    "binary_size": measure_binary_size(),
}

# Grade calculation
grade = calculate_performance_grade(metrics)
print(f"Performance Grade: {grade}/100")

# Breakdown
for metric, value in metrics.items():
    print(f"{metric}: {value['score']}/20 ({value['status']})")
EOF
```

**Grading Criteria** (100 points total):
- Build time <60s: 20 points
- Test execution <10s: 20 points
- Generation time <2s: 20 points
- Memory usage <100MB: 20 points
- Binary size <20MB: 20 points

**Target**: A+ grade (95+/100)

### 5. Benchmark Documentation

**Create Comprehensive Report**:
```markdown
# Week 3 Performance Validation Report

## Quick Wins Validation
- ✅ Parallel Templates: 45% improvement (expected 40-60%)
- ✅ Lazy Init: 38% improvement (expected 30-50%)
- ✅ String Optimization: 25% memory reduction (expected 20-30%)

## Optimization Benchmarks
- ✅ Lockfile Resolution: 65% improvement (target 60-70%)
- ✅ RDF Caching: 75% improvement (target 70-80%)
- ✅ Template Processing: 55% improvement (target 50-60%)

## Regression Testing
- ✅ No regressions detected
- ✅ All benchmarks stable or improved

## Performance Grade: A+ (96/100)
- Build time: 18/20 (52s, target <60s)
- Test time: 20/20 (8s, target <10s)
- Generation: 20/20 (1.8s, target <2s)
- Memory: 18/20 (95MB, target <100MB)
- Binary size: 20/20 (18MB, target <20MB)

## Recommendations
- Consider further memory optimizations for A++
- Build time could be improved with incremental compilation
```

## Coordination Protocol

### Before Each Benchmark
```bash
npx claude-flow@alpha hooks pre-task --description "Benchmarking [optimization/quick-win]"
```

### After Each Validation
```bash
npx claude-flow@alpha hooks post-edit \
    --file "benchmarks/[name]_results.md" \
    --memory-key "swarm/benchmarker/validation-[N]"

npx claude-flow@alpha hooks notify \
    --message "Validated [optimization]: [X]% improvement"
```

### Daily Summary
```bash
npx claude-flow@alpha hooks post-task --task-id "benchmark-day-[N]"
```

## Success Criteria

- [ ] All 3 quick wins validated
- [ ] All 3 optimizations benchmarked
- [ ] Zero regressions detected
- [ ] Performance grade A+ (95+/100)
- [ ] Comprehensive benchmark documentation
- [ ] SLA dashboard updated

## Output Deliverables

1. **Benchmark Results**: `week3_coordination/benchmarks/[name]_results.md`
2. **Regression Report**: Daily regression check summaries
3. **Performance Grade**: Final A+ validation report
4. **SLA Dashboard**: Updated metrics and trends

## Escalation Triggers

**Immediate alerts if**:
- Any regression detected
- Optimization fails to meet target
- Performance grade drops below A
- Benchmark shows inconsistent results

**Escalation Path**: Notify Backend Dev for optimization issues, Task Orchestrator for blockers
