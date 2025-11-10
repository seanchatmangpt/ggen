# Performance - Close SLO Gaps

Guide to identifying performance gaps, meeting SLOs, and ensuring performance requirements are met.

## Quick Reference

### Commands
```bash
# Check SLO compliance
cargo make slo-check

# Check cleanroom performance
cargo make cleanroom-slo-check

# Profile performance
cargo make cleanroom-profile

# Run benchmarks
cargo make bench
```

## SLO Requirements

### Build Performance
- First build: ≤ 15 seconds
- Incremental build: ≤ 2 seconds

### Runtime Performance
- RDF processing: ≤ 5 seconds for 1k+ triples
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3 seconds end-to-end

### Quality
- 100% reproducible outputs
- Deterministic transformation

## How to Identify Gaps

### Build Performance Gaps
Check:
- First build exceeds 15 seconds
- Incremental build exceeds 2 seconds
- Unnecessary recompilations
- Dependency caching not working

### Runtime Performance Gaps
Identify:
- RDF processing exceeds 5 seconds for 1k triples
- Memory usage exceeds 100MB
- CLI operations exceed 3 seconds
- Operations slower than expected

### Performance Regressions
Find:
- Performance degraded from baseline
- New operations slower than old
- Memory usage increased
- Build times increased

## What to Fix

### Build Performance Issues
**Problem**: Builds too slow
**Fix**: Optimize dependencies, enable caching, reduce recompilations
**Verify**: Build times meet SLOs

### Runtime Performance Issues
**Problem**: Operations too slow
**Fix**: Profile hot paths, optimize algorithms, reduce allocations
**Verify**: Runtime performance meets SLOs

### Memory Issues
**Problem**: Memory usage too high
**Fix**: Stream large data, reuse buffers, reduce allocations
**Verify**: Memory usage within limits

### Performance Regressions
**Problem**: Performance degraded
**Fix**: Identify regression, optimize, restore performance
**Verify**: Performance meets baseline

## Gaps to Close Checklist

### Build Performance
- [ ] First build ≤ 15 seconds
- [ ] Incremental build ≤ 2 seconds
- [ ] No unnecessary recompilations
- [ ] Dependency caching works

### Runtime Performance
- [ ] RDF processing ≤ 5s for 1k+ triples
- [ ] Generation memory ≤ 100MB
- [ ] CLI scaffolding ≤ 3s end-to-end
- [ ] Large graphs streamed (not loaded entirely)

### Optimization
- [ ] Hot paths optimized
- [ ] Repeated operations cached
- [ ] Allocations minimized
- [ ] LTO enabled in release builds

### Monitoring
- [ ] Performance benchmarks exist
- [ ] Regression tests catch issues
- [ ] Profiling identifies bottlenecks
- [ ] SLOs tracked and reported

## Common Gaps to Fix

### Gap: Build Too Slow
**Problem**: Build times exceed SLOs
**Fix**: Optimize dependencies, enable caching, reduce recompilations
**Verify**: Build times meet SLOs

### Gap: Runtime Too Slow
**Problem**: Operations exceed time limits
**Fix**: Profile, optimize hot paths, reduce allocations
**Verify**: Runtime performance meets SLOs

### Gap: Memory Usage Too High
**Problem**: Memory exceeds limits
**Fix**: Stream large data, reuse buffers, reduce allocations
**Verify**: Memory usage within limits

### Gap: Performance Regression
**Problem**: Performance degraded from baseline
**Fix**: Identify regression, optimize, restore performance
**Verify**: Performance meets baseline

## Performance Principles

### Measure First
- Profile before optimizing
- Identify hot paths
- Measure actual performance
- Compare to SLOs

### Optimize Hot Paths
- Focus on code that runs frequently
- Optimize algorithms
- Reduce allocations
- Cache expensive operations

### Stream Large Data
- Don't load entire files into memory
- Process data incrementally
- Use streaming APIs
- Limit memory usage

## Commands for Gap Analysis

```bash
# Check SLO compliance
cargo make slo-check

# Profile performance
cargo make cleanroom-profile

# Run benchmarks
cargo make bench

# Measure build times
time cargo make build
time cargo make build-release
```
