# Receipt: i5-wo06-reconcile (WO-06 implementation reconciliation)

Standing: PARTIAL_ALIVE

- **Date**: 2026-11-03
- **Lane**: IMPLEMENT (I5) — WO-06 reconcile
- **Repo/checkout**: /Users/sac/ggen (canonical, no worktree)
- **Standing**: PARTIAL_ALIVE (lib unit court ALIVE below; two e2e failures
  attributed to another lane's in-flight change, disclosed)

## Contract items and outcomes

1. **R7 ENVELOPE_GRAPH_MISMATCH — already landed by G3, verified, not
   duplicated.** Evidence: `crates/ggen-abb-sbb/src/envelope_court.rs:53-61`
   — digest-agreement branch precedes the cap lookup (identity before
   policy), `Refused { code: "ENVELOPE_GRAPH_MISMATCH" }`, exit 2 via the
   unchanged `ReplayVerdict::exit_code()` mapping. `grep -n
   ENVELOPE_GRAPH_MISMATCH` returned :55 (branch) + :24 (doc row).

2. **R8 freshness delegation — doc comment was MISSING, added.**
   `envelope_court.rs` module doc now names `verify_receipt`
   (`crates/ggen-abb-sbb/src/lib.rs:1056` — real line, WO-06's `:1037` was
   stale) as the ontology-freshness authority; the court does not duplicate
   freshness. No code change. `cargo check -p ggen-abb-sbb` → Finished, 0
   errors.

3. **R10 CONFLICT — RESOLUTIONS.md R11 row appended.** WO-06 R10
   ("exact equality is law, do not loosen to `<=`") is SUPERSEDED by G3's
   landed ceiling law `measured <= declared` (HIGH-1 fix). The R11 row
   documents the corrected law (at-or-under ceiling passes, over-ceiling
   refuses with `CAP_CEILING_MISMATCH`, exit 2) with the
   `envelope_court.rs` citation.

4. **R9 ENVELOPE_CAP_FALLBACK — implemented in
   `crates/ggen-engine/src/envelope.rs`.** New `CapFallback` enum
   (`MissingReticle` / `MissingIndividual` / `MalformedLiterals`, each with
   a stable `kind()` string). New `extract_caps_checked(root, envelope) ->
   (CapTable, Option<CapFallback>)` is the testable artifact surface;
   `extract_caps` delegates to it and every fallback site emits one
   structured `tracing::warn!(code = "ENVELOPE_CAP_FALLBACK", kind, key =
   envelope IRI)`. Fail-soft contract untouched: defaults, min-clamped,
   never a refusal. No new deps (`tracing` was already in ggen-engine's
   Cargo.toml). The no-envelope-declared common path emits nothing.

## Commands run + exits

- `cargo test -p ggen-engine --lib envelope` → **8 passed, 0 failed**
  (includes 3 new R9 courts: malformed literal → `MalformedLiterals`,
  absent individual → `MissingIndividual`, healthy envelope → `None`, plus
  the updated missing-reticle court asserting
  `Some(CapFallback::MissingReticle)` and the no-envelope path asserting
  `None`). Exit 0.
- `cargo check -p ggen-abb-sbb` → Finished, exit 0.
- `cargo test -p ggen-engine envelope` (full filter, spec acceptance cmd) →
  **2 FAILED in `tests/envelope_caps_e2e.rs`** (lines 187, 234): both fail
  with `[FM-LAW-015] EA admission gate: ea.graph.json is absent and no
  admission opt-out was given`. Attributed to another lane's in-flight K2
  change (the EA-admission gate landing in `SyncOptions` without its e2e
  fixtures updated), NOT to this lane's diff — this lane touched only
  `extract_caps` observability, which precedes validation and cannot emit
  FM-LAW-015. Chicago classification: pre-existing/concurrent, not
  session-introduced by I5.

## SLA minimal-unblock disclosure (same-checkout fanout law)

`crates/ggen-engine/tests/consumer_mode_fixture_only_e2e.rs` (NOT lane-owned)
was syntax-broken mid-edit by the K2 lane (two `skip_ea_admission: true,`
lines injected BEFORE the `SyncOptions {` literal — parse error blocking all
of ggen-engine's test compile). I5 applied the minimal unblock: moved one
comment + `skip_ea_admission: true,` INSIDE the struct literal, removing the
duplicate. Disclosed here per rule 6; the K2 lane supersedes this diff on
landing. The unrelated `;;` warning in `tests/combinatorial_matrix.rs:596`
was left for its owner.

## Mutation-court notes

- F2/F3 falsifiers addressed by construction: the R9 tests assert on the
  returned `Option<CapFallback>` artifact (real emitted value), not on a
  mock or call count; removing the `fallback()` emission path flips
  `malformed_cap_literal_falls_back_loud` and
  `missing_envelope_individual_falls_back_loud` to FAIL (fallback becomes
  `None`).
- Log-capture assertion of the `tracing::warn!` itself was not added (no
  existing log-capture harness in ggen-engine's test deps; adding one was
  not warranted) — the structured event and the returned flag share one
  code path (`fallback()`), so the flag pins the emission site.

## Files touched (lane-owned except the disclosed unblock)

- `crates/ggen-engine/src/envelope.rs` (owned)
- `crates/ggen-abb-sbb/src/envelope_court.rs` (owned, doc-only)
- `docs/jira/v26.11.3/RESOLUTIONS.md` (owned, R11 appended)
- `docs/jira/v26.11.3/receipts/i5-wo06-reconcile.md` (owned, this file)
- `crates/ggen-engine/tests/consumer_mode_fixture_only_e2e.rs` (SLA
  unblock of another lane's broken in-flight edit, disclosed above)

## Falsifier status

- F1 (digest branch vacuous): N/A — branch landed by G3 with its own courts.
- F2/F3: addressed (artifact assertions, no mocks).
- F4 (exit-code pins): untouched mapping, no kernel type change.
