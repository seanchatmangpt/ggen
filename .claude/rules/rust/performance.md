---
auto_load: false
category: rust
priority: high
version: 6.0.0
---

# 🚀 Performance SLOs

## Targets (Must Meet)
| Metric | Target | Validation |
|--------|--------|------------|
| First build | ≤15s | `cargo make slo-check` |
| Incremental build | ≤2s | `cargo make slo-check` |
| RDF processing | ≤5s/1k+ triples | `ggen sync --audit` |
| Generation memory | ≤100MB | Runtime profiling |
| CLI scaffolding | ≤3s end-to-end | Integration tests |
| Reproducibility | 100% | Hash verification |

## Validation Commands
```bash
cargo make slo-check     # Verify all SLOs
cargo make bench         # Run benchmarks
cargo make audit         # Security check
```

## Performance Patterns
- Use references over owned values
- Prefer stack allocation
- Minimize heap allocations
- Optimize hot paths (20% of code)
- Profile before optimizing

## Claude Flow Benefits
- 84.8% SWE-Bench solve rate
- 32.3% token reduction
- 2.8-4.4x speed improvement
- 27+ neural models available
