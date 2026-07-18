---
auto_load: false
category: rust
priority: high
version: 6.0.0
---

# 🚀 Performance SLOs

## What `just slo-check` actually measures

Confirmed against the real `slo-check` recipe in `justfile` (do not trust build-time numbers
below without re-checking this — an earlier version of this doc claimed "≤15s first build /
≤2s incremental build" SLOs that `slo-check` has never measured):
- `cargo bench --bench cli_startup_performance -- --test` (Phase 1: CLI startup performance,
  root package only)
- A real wall-clock `date`-based timing assertion around `cargo test -p ggen-engine --test
  receipt_chain_e2e` (Phase 2), failing loudly if it exceeds a 180s threshold — genuine
  elapsed-time measurement, not a printed claim.

There is no `ggen sync --audit` flag (`sync run` only accepts `--dry-run`/`--watch`) and no
build-time (first-build / incremental-build) SLO check in the current `slo-check` recipe.
"RDF processing", "Generation memory", "CLI scaffolding", and "Reproducibility" below are aspirational
targets, not currently automated in `slo-check` — treat them as goals, not verified gates.

## Targets (Aspirational — not all are currently automated, see above)
| Metric | Target | Validation |
|--------|--------|------------|
| CLI startup | automated | `just slo-check` (Phase 1, `cli_startup_performance` bench) |
| Receipt-chain wall-clock | ≤180s | `just slo-check` (Phase 2, real `date`-based timing) |
| RDF processing | ≤5s/1k+ triples | Not currently automated |
| Generation memory | ≤100MB | Not currently automated (runtime profiling only) |
| CLI scaffolding | ≤3s end-to-end | Integration tests (not part of `slo-check`) |
| Reproducibility | 100% | Hash verification (receipt chain) |

## Validation Commands
```bash
just slo-check     # Verify the two real SLOs above
just bench         # Run benchmarks (root ggen package only, not --workspace)
just audit         # Security check (cargo audit)
```

## Performance Patterns
- Use references over owned values
- Prefer stack allocation
- Minimize heap allocations
- Optimize hot paths (20% of code)
- Profile before optimizing
