# WO-03: Unify the Two Admission Gates (composer ↔ kernel depgraph, Stage 2c)

- **Standing**: UNVERIFIED (spec; nothing manufactured)
- **Milestone**: v26.11.3
- **Owner**: coordinator-dispatched lane
- **Ground truth (re-verified on disk 2026-11-03)**:
  - `crates/ggen-marketplace/src/packs_registry/composer.rs` — the composer gate: `DuplicateCapability`, `UnboundRequirement`, `DuplicateArtifactPath`, `CyclicDependencies` refusals (struct at lines 107-120, members confirmed at 114/120); deterministic BTreeSet-based Kahn topological sort (`topological_sort()` at lines 416/479). Composer derives `PartialEq` on the plan/order, so determinism is type-proven.
  - `crates/ggen-abb-sbb/src/depgraph.rs` — the kernel depgraph gate: closure cycles, unbound ports, consumer edges. Module doc-comment at **line 3** claims refusal on "artifact collisions" — no artifact-collision check exists in the file; the composer owns it. FALSE DOC CLAIM.
  - `crates/ggen-cli/src/cmds/pack.rs:1258-1264` — comment claims the composer's topological `order` "has unstable tie-breaks among dependency-free packs (nondeterministic across identical runs)"; then `order.sort()` lexicographically re-orders the sequence, destroying dependencies-before-dependents. FALSE COMMENT + DESTRUCTIVE SORT. The composer's `PartialEq` derive already proves determinism; the sort is unjustified and harmful.
  - `crates/ggen-engine/src/sync.rs:1007` — Stage 2c re-extracts dependency facts from disk and re-runs kernel admission, re-deriving a graph the composer already computed during `ggen pack compose`. Two graphs, two cycle verdicts, two orders for the same pack set.

## Problem

`ggen pack compose` and Stage 2c kernel admission are two admission gates over the same composed pack set, with three drift surfaces:

1. **One set, two verdicts.** The same requirement can pass composer admission yet fail kernel admission (or the reverse) with no registered divergence.
2. **One graph, two orders.** Stage 2c re-extraction can produce an order different from the composer's proven-deterministic one; pack.rs `order.sort()` compounds this by destroying the composer order outright.
3. **False load-bearing comments.** pack.rs "unstable tie-breaks" and depgraph.rs:3 "artifact collisions" assert behavior that does not exist; every reader inherits the false model.

## Change spec

### 1. Persist PackCompositionPlan from `ggen pack compose`; Stage 2c consumes it when present

- Add `--out <path>` to the `pack compose` CLI verb (`crates/ggen-cli/src/cmds/pack.rs`). When set, serialize the plan as JSON to the path (create parent dirs); stdout projection stays (additive, not a replacement).
- `PackCompositionPlan` is marketplace-owned and not currently `Serialize`; the CLI hand-projects JSON at pack.rs:1258ff. Move that projection into the marketplace crate (a `Serialize` impl or `to_json()` on the type — implementer's choice) so the wire shape stays marketplace-owned and the CLI is a thin wrapper.
- **Stage 2c change** (in `crates/ggen-engine/src/sync.rs`, Stage 2c region at line 1007): if a persisted plan file is present (path supplied via CLI → sync envelope; exact transport — CLI flag, manifest key, or `.ggen-v2/` sidecar convention per RESOLUTIONS.md — implementer proposes, coordinator rules at integration), Stage 2c consumes it instead of re-extracting dependency facts from disk: one graph, one cycle verdict, one order. When absent, fall back to today's re-extraction path unchanged (backward compatible; no-`--out` behavior identical to current).
- The persisted plan carries: `pack_ids`, `provides`, topological `order`, and the composer's refusal/cycle verdict, so Stage 2c can honor the composer's verdict rather than re-litigate it.

### 2. Fix the false comment and remove the destructive sort (pack.rs)

- Delete the "unstable tie-breaks" comment (pack.rs:1258-1264) and the `order.sort()`; emit/persist the composer's topological order verbatim (dependencies before dependents).
- Determinism is already proven by the composer's `PartialEq` derive — no new tie-break machinery required.
- **Acceptance test (topological-property test)**: with a fixture of ≥5 packs forming a DAG (a→b→c plus an independent d, plus a diamond a→{b,c}→e), assert for every edge u→v that `pos(u) < pos(v)` in the emitted `order` — both in the stdout JSON and in the `--out` file. With the sort present, `e` sorts before `a` and the property fails; with the sort removed it passes. (RED under current code — the sort breaks it; GREEN after removal.)

### 3. Reconcile port-vs-URN vocabularies

- Document the mapping in ONE place: `crates/ggen-marketplace/src/packs_registry/composer.rs` module doc-comment gains a "Vocabulary mapping" section: `capabilities.requires` URN (e.g. `urn:ggen:cap:...`) ↔ kernel graph `requires` port string (e.g. pack name or `[graph]` port id). State the normalization rule (exact-string vs prefix/URN-strip) chosen by the implementer and the coordinator rules it at integration.
- Fix `crates/ggen-abb-sbb/src/depgraph.rs:3` doc-comment: remove "artifact collisions" from the refusal list (composer owns it), OR — preferred if cheap — add the artifact-collision check to the kernel (`DuplicateArtifactPath`-equivalent on `PackManifest.artifacts` within the closure). Either is acceptable; the doc must match the code after this WO.

### 4. Reconcile cycle laws (RESOLUTIONS row)

- Composer drops capability-cycle edges silently (to keep Kahn terminating); kernel refuses all cycles. Add a row to `docs/jira/v26.11.3/RESOLUTIONS.md`: which law governs which edge type — proposed: **kernel-refuses-all** governs `[graph] depends_on`/consumer edges (hard structural cycles); **composer-drops** governs `capabilities.requires` URN cycles (they are ports, not structure — a capability cycle is resolvable at runtime, a structural cycle is not). Coordinator rules final wording; the WO's falsifier requires the row to exist and name both edge types.

## Acceptance

1. **End-to-end single-order run**: a composed-set fixture runs `ggen pack compose --out plan.json` → `ggen sync`, and the sync honors the plan's `order` — the order in the plan equals the order Stage 2c admits (one graph, one cycle verdict, one order). Assert plan order == admitted order on a ≥5-pack DAG fixture.
2. **Topological-property test** for the pack.rs sort removal (spec in §2): RED before, GREEN after.
3. **Contradiction test**: a requirement that passes composer admission but fails kernel admission must either (a) be impossible on the fixture corpus (asserted by running both gates on the same fixture and asserting equal verdicts), or (b) the divergence is registered in RESOLUTIONS.md with the governing law named. Either outcome is a pass; silence is not.
4. All touched-crate test suites pass (`cargo test -p ggen-marketplace -p ggen-abb-sbb -p ggen-engine -p ggen-cli`).

## Non-goals

- No change to composer refusal semantics, Kahn implementation, or `PartialEq`-derived determinism.
- No change to Stage 2c behavior when no plan file is present (fallback unchanged).
- No new vocabulary normalization beyond the documented mapping in §3 (mapping documented; deeper unification is a future WO).

## Falsifier

- A composed fixture where the plan `order` differs from the Stage 2c-admitted order, or a requirement passing composer yet failing kernel without a RESOLUTIONS row, or the topological-property test still passing with the pack.rs `order.sort()` re-inserted (proves the test is vacuous).

## Lane boundaries

- `crates/ggen-cli/src/cmds/pack.rs` — this lane.
- `crates/ggen-engine/src/sync.rs` — SHARED SEAM: check `docs/jira/v26.11.3/RESOLUTIONS.md` sole-writer map before editing the Stage 2c region; if another lane owns it this milestone, disclose any minimal unblock in both receipts per the compile-freeze SLA.
- `crates/ggen-abb-sbb/src/depgraph.rs` doc-comment — this lane (doc-only, additive).
- `crates/ggen-marketplace/src/packs_registry/composer.rs` doc-comment + optional Serialize — this lane.
- RESOLUTIONS.md cycle-law row — propose text; coordinator merges at integration.
