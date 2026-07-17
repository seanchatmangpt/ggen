# CONSOLIDATE-002 — Verifier Receipt

**Role:** VERIFIER + RECEIPT (independent re-verification; trust evidence, not narration).
**Checkpoint:** Merge the THREE remaining order-sensitive publish branches in
`ServerState::analyze_and_observe` (`crates/ggen-lsp/src/state.rs`) into ONE
species-driven loop. Last triplication; a 4th species becomes pure data.
**Verdict:** **ALIVE.**
**Date:** 2026-05-30 | **Repo:** ggen @ `/Users/sac/ggen` | **Base:** `main` @ fe8c72b5 (clean).

---

## 1. What changed (real state, not stdout)

Single modified source file: `crates/ggen-lsp/src/state.rs`
(`git diff --stat HEAD` = `192 +/-, 84 insertions, 108 deletions`).

The three former order-sensitive Phase-A publish branches (TPL / HARNESS / OUT)
and the three Phase-C stale-clear branches are now TWO species-driven loops over a
function-local, ORDER-FIXED `[TPL, HARNESS, OUT]` `Species` descriptor array
(state.rs:554–628). Each `Species` carries `code: &'static str`, `is_trigger: bool`,
`groups: Vec<(PathBuf, Vec<Diagnostic>)>`, `flagged: HashSet<Url>`.

- **Phase A** (state.rs:581–601): one `for sp in &mut species` loop; per-group
  publish via `observe_diagnostics`, with the unified self-merge guard
  `edited_self_url == anchor && !published_self`.
- **Phase B** (state.rs:603–609): self-fallback `if !published_self` — UNCHANGED,
  strictly between A and C.
- **Phase C** (state.rs:611–628): one `for sp in &species` loop gated by
  `sp.is_trigger`, keyed by `sp.code` → `clears_for(sp.code, uri, &sp.flagged)`.

Triplicate locals `current_flagged`, `current_harness_flagged`, `current_out_flagged`
are GONE (grep: zero matches in state.rs). Confirmed the merge actually removed the
triplicate branches.

New (untracked) test artifacts, all under `crates/ggen-lsp/`:
- `tests/consolidate_002_sequence_equivalence.rs`
- `tests/fixtures/consolidate_002_multispecies/` (ggen.toml, queries/items.rq,
  schema/domain.ttl, templates/item.tera, **golden-sequence.jsonl**)

Receipt/docs (under `docs/receipts/`): `CONSOLIDATE_002_PRE_INVENTORY.md` (architect),
`CONSOLIDATE_002_RECEIPT.md` (this file).

---

## 2. Authoritative path touched

Editor-flow OCEL emission stage: `analyze_and_observe` → `observe_diagnostics` →
`IntelLog::append` (append-only NDJSON to
`.ggen/ocel/agent-edit-events.ocel.jsonl`). The merge preserves the exact
`observe_diagnostics` CALL ORDER, which IS the on-disk OCEL event SEQUENCE.

---

## 3. The proof is REAL sequence-equivalence (NOT pass-count)

`tests/consolidate_002_sequence_equivalence.rs`:

- Reads the EXTERNAL on-disk OCEL log (`read_log_lines`, same surface as
  `oracle_tape_generator.rs`) in file = call = write order.
- Normalizes each event to the tuple `NormEvent { activity, file (root-relative),
  code }`, dropping nondeterministic fields (event `id`/blake3, `timestamp`,
  `run_id`/`session_id`, `receipt_id`, TempDir path prefix).
- Asserts `assert_eq!(actual, golden)` on `Vec<NormEvent>` — **ordered,
  element-by-element equality**, NOT a set/multiset/count. The source comment
  states this explicitly: "Ordered equality — NOT a multiset / pass-count comparison."

**Anti-vacuity (falsifiability) — independently executed by the verifier:**
I reordered the committed golden (OUT block before TPL block, same 6 events / same
counts) and re-ran `multispecies_sequence_matches_golden` → it **FAILED** with
"multi-species OCEL sequence diverged from the committed golden … FAKE-LIVE." A
set/count-based assertion would have PASSED on that reorder. This proves the test
detects order. Golden restored byte-identically afterward (`diff` clean).

**Golden provenance — independently re-derived by the verifier:**
I checked out `state.rs` at HEAD (PRE-MERGE), ran the `#[ignore]` generator
(`generate_golden_sequence`) against pre-merge production code, and diffed its
output against the committed golden → **IDENTICAL**. So the committed golden is the
genuine pre-merge truth, not a post-merge self-consistency artifact. Then I restored
post-merge `state.rs` + the committed golden and confirmed the assertion test passes.

Committed golden (the proof key, in emit order):
```
DiagnosticRaised  /templates/item.tera  GGEN-TPL-001
RouteSelected     /templates/item.tera  GGEN-TPL-001
RepairSuggested   /templates/item.tera  GGEN-TPL-001
DiagnosticRaised  /ggen.toml            GGEN-OUT-001
RouteSelected     /ggen.toml            GGEN-OUT-001
RepairSuggested   /ggen.toml            GGEN-OUT-001
```
TPL (`.tera`) chain BEFORE OUT (`ggen.toml`) chain — exactly the fixed
`[TPL, HARNESS, OUT]` Phase-A order. `golden_is_genuinely_multispecies` guards that
the golden contains BOTH a TPL-001-on-`.tera` and an OUT-001-on-`ggen.toml` event
(so the equivalence can never go vacuous via a future fixture edit).

---

## 4. Behavior-preservation invariant (no drift)

The unified Phase-A guard is `edited == anchor && !published_self` for ALL species,
whereas pre-merge TPL/HARNESS branches lacked the `!published_self` clause. This is
behavior-preserving and independently confirmed:

- TPL anchors on `entry.template_path` (`.tera`), OUT on `entry.manifest_path`
  (`ggen.toml`), HARNESS on `Cargo.toml` (analyzers/mod.rs:43,67,101). Trigger gates
  are mutually exclusive by edited-file basename (`ggen.toml` vs `Cargo.toml`), so at
  most one species self-matches per pass. When TPL/HARNESS could self-match,
  `published_self` is still `false`, so adding the clause changes no observable emit.
- `own_diags` is `std::mem::take`-n at most once (the global `published_self`
  short-circuit) — merge-once preserved; no double-publish on a self-anchored edit.
- `tpl_clears_for` public shim NOT removed (state.rs:165); the stale-clear test
  drives it directly and still passes.

---

## 5. Negative path / sabotage confirmed

| Sabotage | Outcome (observed) |
|---|---|
| Reorder committed golden (OUT before TPL) | `multispecies_sequence_matches_golden` FAILS — order is enforced |
| Generate golden from PRE-MERGE code | byte-identical to committed golden — provenance honest |

---

## 6. Gate evidence (re-run independently)

| Gate | Command | Result |
|---|---|---|
| Compile | `cargo make check` | ✓ Finished (workspace) |
| ggen-lsp tests | `cargo test -p ggen-lsp` | ✓ ALL pass, 0 failed across every binary |
| Sequence test | `consolidate_002_sequence_equivalence` | ✓ 2 passed, 1 ignored (generator) |
| Clippy | `cargo clippy -p ggen-lsp --no-deps -- -D warnings` | ✓ 0 warnings |
| Fmt | `cargo fmt -p ggen-lsp -- --check` | ✓ clean (exit 0) |
| No-test-edit guard | `git diff HEAD -- crates/ggen-lsp/tests/` | ✓ 0 lines (only NEW untracked files) |
| No `#[allow]` added | grep `+...#[allow]` in state.rs diff | ✓ none |
| Triplication removed | grep `current_*_flagged` in state.rs | ✓ none |

### Existing ALIVE-checkpoint tests — all UNCHANGED counts, all pass

| Test binary | Passed |
|---|---|
| `ggen_tpl_001_living_loop` | 5 |
| `ggen_out_001_living_loop` | 7 |
| `ggen_harness_001_living_loop` | 7 |
| `ggen_tpl_001_stale_clear` | 9 |
| `ggen_tpl_001_did_close_clear` | 2 |
| `ggen_tpl_001_regression` | 6 |
| `ggen_live_buffer_001` | 1 |
| `lib.rs` unit tests | 163 |

No prior ALIVE checkpoint regressed (CONSOLIDATE-001 clears loop, LIVE-BUFFER-001,
did_close, regression all green and untouched).

---

## 7. Verdict

**ALIVE.** The three publish branches (and three clear branches) are one
species-driven loop over a fixed `[TPL, HARNESS, OUT]` descriptor; the multi-species
normalized OCEL `(activity, file, code)` sequence is byte/sequence-identical to the
independently-reproduced pre-merge golden; the sequence test is a REAL ordered
assertion (proven falsifiable by a reorder); all existing tests pass UNCHANGED with
no test-file edits; clippy 0; fmt clean; no `#[allow]`; scope confined to
`crates/ggen-lsp/` + `docs/receipts/`. `intel/mine.rs` not touched. No commit made.
