---
description: "Compare benchmark results across commits or branches. Shows performance regressions/improvements. Use when tracking performance trends or validating optimization changes."
allowed_tools: "Bash(cargo make bench:*), Bash(git:*), Read, Grep"
argument_hint: "[base-branch] [compare-branch]"
---

# Benchmark Comparison Command

Compare benchmark results between commits/branches to detect regressions.

## Usage Examples

```
/bench-compare main HEAD          # Compare main to current branch
/bench-compare HEAD~2 HEAD        # Compare 2 commits back to now
/bench-compare                    # Compare with previous commit
```

## Comparison Steps

1. **Checkout Base**
   - Save current state
   - Checkout $1 (default: HEAD~1)
   - Run: `cargo make bench` with JSON output

2. **Checkout Compare**
   - Restore to $2 (default: HEAD)
   - Run: `cargo make bench` with JSON output

3. **Analyze Results**
   - Load both benchmark sets
   - Calculate % differences for each suite
   - Flag regressions (>5% increase in time)
   - Highlight improvements (>5% decrease)

4. **Generate Report**
   - Show side-by-side metrics
   - Highlight performance cliffs
   - Suggest profiling targets if regressions found

## Benchmark Suites (14 Total)

- **Core Performance**: runtime_overhead, async_runtime_benchmarks
- **Memory**: memory_profiling, quick_runtime_validation
- **Features**: conventions_performance, marketplace_performance
- **Pipeline**: pipeline_performance, cli_startup_performance
- **Comprehensive**: comprehensive_slo_benchmarks
- **Legacy**: v2_performance, marketplace_v2_benchmarks, fortune500_performance

## SLO Targets

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s (1000+ triples)
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end

## Output Interpretation

- **GREEN** (✓): Performance within SLOs
- **YELLOW** (⚠): Approaching limits (>80% of target)
- **RED** (✗): Exceeds SLO - investigate regression

## Success Criteria

✓ No regressions > 10%
✓ All SLOs met
✓ Memory usage stable
✓ Startup time consistent
