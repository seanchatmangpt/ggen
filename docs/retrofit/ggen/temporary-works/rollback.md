# Rollback — TW-GGEN-001 (verify-pack evidence lane wiring)

Applies only to item #1 (`ggen-verify-pack` wiring into this repo's own `ggen.toml`) — the sole
load-bearing temporary work in this pass. Items #2, #5, #8 have no temporary work (see
`plan.md`) and therefore no rollback procedure of their own beyond ordinary `git revert`.

## Trigger conditions (any one is sufficient)

1. 14-day max lifetime for `TW-GGEN-001` exceeded without reaching 3 green `just pre-commit`
   cycles (including `guard-pack-proofs`) plus 1 green `pull_request`-triggered CI run.
2. A wiring defect surfaces during the transition window (per the Inspecting state in
   `transition.mmd`) that cannot be fixed within a single Andon-response cycle — e.g., the
   verify-pack's producer templates emit `ver:Check` facts with `ver:name` values that
   `verify_check_axis`'s substring match cannot route correctly, and fixing that requires a
   change to `verify_check_axis` itself (out of scope for a `ggen.toml`-only wiring change).
3. Any regression of a previously-real equivalence class back to `Unknown`, or a `Divergent`
   verdict on `tests`/`gates` traced to a wiring cause rather than genuine drift, that is not
   resolved within one sync-run inspection cycle.

## Procedure

1. **Identify the wiring commit.** The temporary work is scoped to a single, identifiable commit
   (or small commit range) that adds the `ggen-verify-pack` reference to this repo's own
   `ggen.toml`. Confirm with `git log --oneline -- ggen.toml` which commit(s) introduced the
   pack reference.
2. **Revert.**
   ```bash
   git revert <wiring-commit-sha>
   ```
   A plain `git revert` (not `reset --hard` — see the repo's own "FIX FORWARD ONLY" rule in
   `~/CLAUDE.md`) creates a new commit restoring the pre-wiring `ggen.toml`. This is the only
   file the temporary work touches; no companion files need reverting.
3. **Verify the receipt shape returns to baseline.** Run `ggen sync run` (or `ggen sync run
   --dry-run` for a non-mutating check) and confirm the emitted receipt's `v2.equivalence` map
   shows the same 6/8-`Unknown` pattern documented as the as-found baseline in `plan.md`. This is
   the concrete, falsifiable rollback-succeeded check — not an assertion.
4. **Do not touch the receipt chain.** `Unknown` equivalence classes are an always-valid state in
   `ReceiptEpochV2` (the machinery treats absence of evidence as honest, not as a special error
   case) — rolling back does not require any receipt-chain surgery, re-signing, or history
   rewrite. The chain simply continues with receipts that, once again, honestly report 6/8
   classes as `Unknown`.
5. **Record the disposition.** Update `.tcps/retrofit/ggen/temporary-works.ttl`: set
   `tw:status "RolledBack"` on the `TW-GGEN-001` individual, with `tw:rolledBackAt` (ISO-8601
   timestamp) and `tw:rollbackReason` (free text naming which trigger condition above applied).
6. **Re-open item #1.** Update `docs/retrofit/ggen/intervention/decision-matrix.md`'s row for
   item #1: change its Priority cell from "Do now (cheap-safe)" to "Deferred — rollback
   `<date>`, see temporary-works/rollback.md", and add one sentence naming the specific failure
   that triggered rollback (not a generic "blocked").

## What rollback does NOT require

- No data migration: `ver:Check` facts already admitted into prior receipts remain valid history;
  rollback only stops *new* facts from being admitted going forward.
- No CI/branch-protection changes.
- No coordination with sibling agents beyond the normal `pull --rebase` discipline, since the
  temporary work is scoped to one file (`ggen.toml`) and one commit.

## Post-rollback andon note

A rollback is not a failure to hide — per the non-termination law, it is WORK: record the
specific defect that caused it (the wiring bug, the missed threshold, whatever it was) as its own
tracked item in a future retrofit pass, with consequence classified and an obligation created,
rather than silently re-deferring item #1 with no trace of why the first attempt didn't land.
