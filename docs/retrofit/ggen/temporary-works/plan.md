# ggen Retrofit — Temporary Works Plan

Scope: the top-4 cheap-and-safe intervention portfolio
(`docs/retrofit/ggen/intervention/decision-matrix.md`, "Top 4" section). This document decides,
per item, whether a temporary work is required and — where it is — specifies its full shape.

## As-found status of each portfolio item (checked on this branch before writing this plan)

| # | Item | As-found status (this branch, verified before writing) |
|---|------|----------------------------------------------------------|
| 2 | Fix hardcoded `andon: Green` in `sync.rs` | **Already landed** — `crates/ggen-engine/src/sync.rs:2461-2476` derives `legacy_andon` from `epoch.andon` (the real v2 precedence machinery), with an inline comment: "The legacy top-level `Andon` field is derived from the same v2 precedence, not hardcoded -- this is the hardcoded-`Green` literal this migration replaces." Landed by PR #346 (`feat(l5): four evidence producers for the Unknown equivalence classes (condition 15)`) and prior work on this same branch/lineage (`afa346249`, `0ea86999c`). No further work, no temporary work needed for this item. |
| 1 | Wire `ggen-verify-pack` evidence producers into ggen's own sync | **Still open** — `grep -n "verify-pack\|ggen-verify" ggen.toml` on this branch returns nothing: this repo's own `ggen.toml` does not invoke the pack. The pack and its 4 evidence producers exist and are proven elsewhere (PR #346/#351 for other consumers), but ggen's own receipt still carries 6/8 Unknown equivalence classes. **This is the one live-path cutover in this portfolio** — see below. |
| 5 | Close 12-fixture sabotage-coverage gap (3 packs) | Additive strengthening: adds new `#[test]` fns to existing test files, using an already-proven pattern (`ma_case_hook_actuation.rs`, `self_monitoring_hook_actuation.rs`). No existing consumer depends on the *absence* of these tests; nothing is cut over. **No temporary work needed.** |
| 8 | Fix `just sync` / `just sync-dry` invalid flags | Confirmed still broken (`justfile:415` passes `--audit true`, `justfile:419` passes `--dry_run true`; the live `sync run` verb only accepts `--dry-run`/`--watch`). Both recipes currently fail on invocation (exit 1), so nothing today depends on them succeeding — flipping them from "always errors" to "runs correctly" is a pure repair with no load transfer between two working states. **No temporary work needed.** |

Per the task's own rule: additive strengthenings and repairs of a currently-non-functional path
get **NONE**, stated honestly rather than defaulted into a temporary-works template. Item #2 needs
none either, because it was resolved before this plan was written — it is preservation of a
fix already in place, not new load-bearing work by this agent.

## The one load-bearing cutover: item #1 (verify-pack evidence wiring)

### Why this is load-bearing

`ggen sync run`'s receipt (`.ggen-v2/receipt.json`) is consumed by:
- `ggen receipt verify` (chain + signature verification, `crates/ggen-engine/src/verbs/receipt.rs`)
- `just pre-commit`'s `guard-pack-proofs` gate (re-syncs + re-tests `examples/receiptctl`)
- Every future commit's evidentiary claim about what actually ran (`compare_verify_class`,
  `verify_check_axis`, `verify_outcome_map` in `sync.rs` already exist and are wired to *consume*
  `ver:Check` facts the moment they are admitted — the machinery is real, only the producer side
  is unwired for this repo's own `ggen.toml`)

Wiring `ggen-verify-pack` into this repo's own `ggen.toml` changes what every future `ggen sync
run` receipt on this repo looks like: 6 of 8 equivalence classes flip from the honest `Unknown`
they are today to `Equivalent`/`Divergent` based on real admitted `ver:Check` facts. This is
exactly the "changes what every future receipt looks like" case the task instructions call out
by name. It gets a full temporary-works specification.

### Temporary work specification

| Field | Value |
|-------|-------|
| **Identity** | `TW-GGEN-001` — "Verify-pack evidence lane, dual-run parallel wiring" |
| **Owner** | Agent implementing item #1 (this portfolio's cheap-safe item #1); escalates to repo maintainer (`seanchatmangpt`) if unresolved past expiration |
| **Supported load** | Carries the *transition* traffic: every `ggen sync run` invocation on this repo, during the window where the verify-pack's 4 evidence producers are wired into `ggen.toml` but their receipt-shape change has not yet been confirmed stable across `just pre-commit`'s `guard-pack-proofs` gate and at least one full CI run on `pull_request`. The old state (6/8 classes `Unknown`, honest but uninformative) remains the fallback the moment the new state fails to validate. |
| **Start condition** | The `[[generation.rules]]` (or pack `[packs]` entry, per whichever of the two `ggen.toml` schemas — see `.claude/rules/architecture.md`, "ggen.toml has two schemas" — this repo uses) referencing `ggen-verify-pack` is added to this repo's own `ggen.toml`, and the first `ggen sync run` after that edit succeeds. |
| **Inspection cadence** | Every `ggen sync run` on this repo during the transition window: diff the emitted receipt's `v2.equivalence` map against the pre-wiring baseline (6/8 `Unknown`) and confirm no class *regresses* from a real value back to `Unknown` or flips to `Divergent` without a corresponding, understood cause (e.g., a genuinely stale local check result, not a wiring bug). Minimum: inspect after every sync during the window, not sampled. |
| **Failure response** | If any equivalence class comes back `Divergent` for a reason that is a wiring defect (not a real drift) — e.g., a `ver:Check` fact with a malformed `ver:name` landing on the wrong axis via `verify_check_axis`'s substring match — treat it as an Andon signal per `.claude/rules/andon/signals.md`: stop, do not merge, fix the wiring, re-run. Do not suppress with `#[allow(...)]`-equivalent (i.e., do not special-case the check out of the pack) without recording the exception in this plan. |
| **Rollback** | Revert the `ggen.toml` edit that adds the `ggen-verify-pack` reference (single-commit, single-file revert — see `rollback.md`). This restores the honest 6/8-`Unknown` receipt shape immediately; no data migration, no receipt-chain break, because `ReceiptEpochV2`'s `Unknown` classes are always a valid, expected state (never promoted, per the load-bearing invariant already enforced in `sync.rs`'s doc comments). |
| **Expiration** | 14 days from `TW-GGEN-001` start, or 3 consecutive green `just pre-commit` runs (including `guard-pack-proofs`) plus one green `pull_request`-triggered CI run on a PR carrying this change, whichever comes first. |
| **Removal trigger** | On expiration by success: promote — delete this temporary-work record from `.tcps/retrofit/ggen/temporary-works.ttl` (mark `Closed`), fold the wiring into the permanent `ggen.toml` state with no further scaffolding (there is no separate "temporary" code path to remove — the temp work here is the *inspection regime*, not a parallel code artifact, since verify-pack wiring is itself the target permanent state). On expiration by failure (no 3 green cycles inside 14 days, or a wiring defect that cannot be fixed within the window): roll back per `rollback.md` and re-open item #1 in the intervention decision matrix as deferred, citing the specific failure. |
| **Max lifetime** | 14 days (hard cap — matches Expiration; no silent extension). |
| **Andon consequence of overrun** | If `TW-GGEN-001` is still open past its 14-day max lifetime without either a Closed or a Rolled-back disposition recorded in `.tcps/retrofit/ggen/temporary-works.ttl`, that overrun is itself an Andon-level signal (HIGH, per `.claude/rules/andon/signals.md`'s table) — record it as a defect in the next retrofit pass, do not let it lapse silently. |

### Load-transfer description (see `load-transfer.mmd` for the diagram)

- **Before**: `ggen sync run` on this repo emits a receipt where `tests`/`gates`/5 other equivalence
  classes are `Unknown` (no `ver:` evidence admitted). This is the load-bearing state today —
  every consumer of the receipt (verify, pre-commit gate, human reviewers) already treats `Unknown`
  as the honest, expected value.
- **During transition (`TW-GGEN-001` open)**: both states coexist observably — the *code path* is
  cut over immediately (one `ggen.toml` edit, one commit), but the *trust* in the new receipt shape
  is provisional until 3 green `pre-commit` cycles + 1 green CI run confirm the wiring doesn't
  silently misclassify or drop `ver:Check` facts. The inspection cadence above is the load path
  during this window.
- **After**: the verify-pack evidence lane is the sole, permanent producer of `ver:Check` facts for
  this repo's own receipts; `TW-GGEN-001` closes; 6/8 equivalence classes are real going forward.

## Portfolio items with no temporary work (honest NONE)

- **#2** (andon derivation): already fixed by prior work on this branch; nothing for this pass to
  transition.
- **#5** (sabotage-fixture coverage): pure additive test coverage; no live path changes, no old
  state to carry load away from.
- **#8** (`just sync`/`sync-dry` flags): repair of a recipe that currently always fails; there is
  no working old behavior whose load needs to be transferred — going from "always errors" to
  "works" has no transition window to protect.
