---
version: 26.7.4
last_updated: 2026-07-17
gate: mandatory — read before every agent dispatch
---

# Coding-Agent Mistakes — Mandatory Gate

**Strongest rule: every patch must deepen authority or reduce drift.** Feature added while old
bypass stays intact fails this rule.

**Five classes**: Decorative Completion (exits 0, no durable state changed). Epistemic Bypass
(RDF/SPARQL logic hardcoded inline — exception: `capability_registry.rs::
resolve_capability_to_packs`, small closed taxonomy). Fail-Open (warning not `Err`). Legacy
Path Contamination (old bypass left reachable). Contract Drift (receipt no longer describes
what ran).

Real files: `ggen-engine/src/sync.rs`, `ggen-graph/`, `install.rs::verify_trust_tier`,
`.ggen/packs.lock`+`.ggen-v2/receipt.json` (verify with `jq`).

**6-Question Patch Contract**: real state changed; authoritative path touched; negative path
now fails correctly; invariant protecting from drift; legacy path removed; proof object.

Deepening authority=harder to bypass. Reducing drift=proof objects accurately reflect what ran.
Neither = reconsider before submitting.

**See also**: [Andon](andon/signals.md), [OTEL](otel-validation.md), [Testing](rust/testing.md).
