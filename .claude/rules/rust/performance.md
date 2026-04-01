---
auto_load: false
category: rust
priority: high
version: 6.0.1
---

# Performance SLOs

## The Failure Mode

You will optimize code you have not profiled. Stop.

SHALLOW GREEN is the failure mode here. You run `cargo make check`, it passes, and you declare performance acceptable. A passing build proves compilation succeeds. It proves nothing about runtime behavior, allocation patterns, or latency characteristics. The only evidence that counts is a measurement from `cargo make slo-check` or a Criterion benchmark.

Unverified performance claims fall at the bottom of the evidence hierarchy. You do not claim performance is acceptable because the code looks fast. You claim it because `cargo make slo-check` passed.

## SLOs

| Metric | Target | Validation |
|--------|--------|------------|
| First build | <=15s | `cargo make slo-check` |
| Incremental build | <=2s | `cargo make slo-check` |
| RDF processing | <=5s/1k triples | `ggen sync --audit` |
| Generation memory | <=100MB | Runtime profiling |
| CLI scaffolding | <=3s end-to-end | Integration tests |
| Reproducibility | 100% | Hash verification |

These are not aspirations. They are constraints. Code that violates an SLO is not ready for merge.

## Verification Commands

```bash
cargo make slo-check     # Verify all SLOs
cargo make bench         # Run Criterion benchmarks
cargo make audit         # Security vulnerability scan
```

Run `cargo make slo-check` before claiming any performance-related work is complete. Run `cargo make bench` when you need to measure the effect of a specific change. Run `cargo make audit` to catch dependency-level performance regressions.

## Performance Discipline

Profile first. Do not optimize code because you suspect it is slow. Measure it with Criterion or `cargo make slo-check`. The data tells you where the hot path is. Your intuition will lie to you.

Prefer references over owned values. A `&str` does not allocate. A `String` does. A `&[T]` does not allocate. A `Vec<T>` does. Pass by reference until profiling proves you need ownership.

Prefer stack allocation over heap allocation. Stack variables are free to create and destroy. Heap allocations go through the allocator, fragment memory, and trigger cache misses. `String`, `Vec`, and `Box` are heap allocations. Use them when you need dynamic size or owned data, not by default.

Minimize allocations in hot paths. The 80/20 rule applies: 20% of your code accounts for 80% of runtime. Profile to find that 20%. Allocate outside the loop. Reuse buffers. Pre-allocate capacity with `Vec::with_capacity` when you know the size.

Optimize the hot path, not the cold path. Code that runs once at startup does not need the same treatment as code that runs in a tight inner loop. Time spent optimizing cold code is time stolen from optimizing hot code.

If you have not run `cargo make slo-check`, you do not claim performance is acceptable.
