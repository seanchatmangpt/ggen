---
auto_load: false
category: rust
priority: high
version: 6.0.0
---

# 🚀 Performance SLOs

`just slo-check` measures only two real things: Phase 1 `cargo bench --bench
cli_startup_performance -- --test` (root package); Phase 2 wall-clock timing around `cargo
test -p ggen-engine --test receipt_chain_e2e`, failing past 180s. RDF processing/generation
memory/CLI scaffolding/reproducibility targets below are aspirational, not automated.

| Metric | Target | Validation |
|---|---|---|
| CLI startup | automated | `slo-check` Phase 1 |
| Receipt-chain | ≤180s | `slo-check` Phase 2 |
| RDF processing | ≤5s/1k+ triples | not automated |
| Generation memory | ≤100MB | not automated |
| CLI scaffolding | ≤3s | not automated |
| Reproducibility | 100% | hash verification |

Commands: `just slo-check`, `just bench` (root only), `just audit`. Patterns: references over
owned, stack over heap, minimize allocations, optimize the hot 20%.
