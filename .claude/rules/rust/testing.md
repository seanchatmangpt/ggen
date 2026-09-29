---
auto_load: false
category: rust
priority: critical
version: 6.0.0
---

# 🧪 Chicago TDD (MANDATORY)

State-based verification, real collaborators, AAA. Forbidden patterns:
[Testing Forbidden](testing-forbidden.md). `tempfile::TempDir` IS acceptable (real file I/O).

Test types: Unit <150s, Integration <30s, BDD/Property/Snapshot/Security/Determinism/
Performance via Cucumber/proptest/insta/Custom/RNG_SEED=42/Criterion.

DoD: public APIs tested, error paths covered (80%+ coverage target, aspirational — no gate
computes it), assertions on observable state not mocks, never claim without running tests.

Commands: `just test-lib` (30s), `just test` (30s→600s cold), `just slo-check`. Coverage: one
real 2026-08-02 measurement, `ggen-cheat-scanner` = 55.6% — not a workspace gate. Mutation
score not computed anywhere — treat "≥60%" as aspirational.

80/20 focus: error paths+cleanup, concurrency edges, real dependency integration.
