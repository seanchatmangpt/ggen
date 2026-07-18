⚠️ **SUPERSEDED — fully executed.** Every action item this document proposes (delete
`crates/ggen-core/`, delete `wizard.rs`/`sigma.rs`/`inverse_sync.rs`, remove the
`ggen-core-retired` feature, retarget the justfile) was carried out by PR #259 ("remove
ggen-core, rewrite README from first principles", merged 2026-07-17) and subsequent justfile
edits — neither of which this document was updated to reflect. `crates/ggen-core/` no longer
exists on disk. Kept for historical context only; do not read the "Current state" section below
as current.

# ggen-core Removal Proposal

Continuation of [00-OVERVIEW](00-OVERVIEW.md)'s Phase 6/7, which this ticket set always
scoped as the final step ([11-DELETION-AND-DEFINITION-OF-DONE](11-DELETION-AND-DEFINITION-OF-DONE.md))
but which PR #255 (`2026-ggen-core-replacement`, merged to `main` at `cbf173f82`,
2026-07-17) did not execute — that PR stopped at **disconnect** (`ggen-core` moved from
`[workspace] members` to `exclude` in root `Cargo.toml`), not **delete**. This document
re-verifies ticket 11's plan against the actual post-merge state and proposes closing Phase
6/7 now.

## Current state (verified live, 2026-07-17, post-#255/#256/#257)

- `crates/ggen-core/` is 9.4 MB on disk, 562 `.rs` files.
- Root `Cargo.toml`: `exclude = ["examples/7-agent-validation", "crates/ggen-core"]` —
  confirmed the only place `ggen-core` appears in workspace configuration.
- **Zero live Cargo dependency edges.** Grepped every crate's `Cargo.toml` for
  `ggen-core`/`ggen_core` — every hit is a comment (migration attribution notes like "ported
  from ggen-core/...", the `ggen-core-retired` feature *name*, or historical dated notes).
  No `[dependencies] ggen-core = ...` entry exists anywhere in the workspace.
- **~150 files still contain textual `ggen_core::`/`ggen-core` references**, which is a
  larger surface than ticket 11's "zero in-workspace dependents" framing implied at a glance —
  but every one checked resolves to one of:
  1. Doc comments/migration notes citing where code was "ported from ggen-core/..." (the
     majority of the `ggen-config`/`ggen-marketplace`/`ggen-engine` hits).
  2. Source inside files whose `pub mod` declaration is commented out in the owning
     `mod.rs` (e.g. `crates/ggen-cli/src/cmds/{wizard,sigma}.rs`, `inverse_sync.rs` —
     confirmed live: `crates/ggen-cli/src/cmds/mod.rs:45` has
     `// pub mod inverse_sync;`) — these files are not compiled by default and cannot be,
     since `ggen-cli`'s `Cargo.toml` has no `ggen-core` dependency to satisfy their
     `use ggen_core::...` imports.
  3. Test files gated behind the `ggen-core-retired` feature (`crates/ggen-cli/tests/{utils,
     ontology,graph,receipt,mcp,sync}_command_test.rs`, `coherence_gate_test.rs`,
     `inverse_sync_cmd_test.rs`, `pack_cache_test.rs` per `cmds/mod.rs`'s comment listing
     "the remaining real ggen-core consumers") — off by default, `cargo build --workspace`/
     `just check` (confirmed clean this session, multiple times) never touches them.
  4. `examples/7-agent-validation` and `examples/archive_ggen_core/*` — already excluded
     from the workspace / already archived this session (#256).
  - **Net: `just check`/`cargo build --workspace` compiling clean today is proof by
    construction that none of these ~150 hits are live, unconditional dependents** — if any
    were, the build would already be broken, since no path to `ggen-core`'s code exists
    without either a `Cargo.toml` dependency edge (none exist) or one of the two dead-code
    gates above.
- Ticket 11's one open verification item — whether `receipt/provenance_envelope.rs` was
  load-bearing via `ggen-cli/src/cmds/inverse_sync.rs` — is now **resolved**: `inverse_sync`
  is confirmed not compiled (`// pub mod inverse_sync;`), so that concern no longer blocks
  removal. `chain_linking.rs` was not independently re-checked in this pass; recommend the
  same "confirm zero external references" grep ticket 11 specifies, immediately before
  deletion, not assumed clean from this document alone.

## What's changed since ticket 11 was written (pre-merge)

Ticket 11 was drafted while the migration was still in flight and hedges heavily ("must
verify," "unclassified," "not yet re-audited"). Since then:
- `ggen-cli` and `ggen-lsp` (the two ticket 11 named as blocking dependents) are both fully
  re-pointed — confirmed via the same Cargo.toml dependency-edge check above.
- The root package (`ggen`) lost its own `ggen-core` dependency in the same pass (per
  `Cargo.toml`'s own comments on the `autobins = false` / bin-collision cleanup, session
  2026-07-16/17).
- Docs (#256) and tests (#257) both had substantial ggen-core-era cleanup passes this
  session, independent of this ticket, which incidentally removed some of the residual
  textual noise ticket 11 would otherwise have had to account for.

## Proposal

**Execute ticket 11's Phase 6/7 as originally scoped, updated for the current baseline:**

1. Re-run ticket 11's one still-open check before touching anything:
   `grep -rn "chain_linking" crates/ggen-core/src/` and confirm (as already done here for
   `provenance_envelope`) that nothing outside `crates/ggen-core/` itself references it.
2. Delete `crates/ggen-core/` in full (`git rm -r`, not a bare `rm` — preserves history).
   Remove the now-dead `"crates/ggen-core"` entry from root `Cargo.toml`'s `exclude` list
   (an `exclude` entry for a path that no longer exists is dead configuration, not
   fix-forward preservation — the code itself is what non-deletion doctrine protects, and
   git history already preserves it after deletion).
3. Delete the now-permanently-unreachable gated files ticket 11 lists as "confirmed dead
   code, do not port" (`receipt/{mod,chain,envelope,error,receipt_impl}.rs`, `naming.rs`,
   `manufacturing/{mod,gates,operator}.rs`, `packs/install.rs:33-39`) — these live inside
   `crates/ggen-core/` so step 2 already removes them; called out here only because ticket
   11 treats them as a distinct sub-decision from "delete the whole crate."
4. **Decision (user, 2026-07-17): abandon, don't port.** Remove the `ggen-core-retired`
   feature and every file it gates (`crates/ggen-cli/src/cmds/{wizard,sigma,inverse_sync}.rs`
   and the 9 test files ticket 11/`cmds/mod.rs` name) outright, with no re-implementation
   pass in `ggen-engine` first. Per ticket 11's own 6-question patch contract, item 5: "a
   `ggen-core` compatibility shim crate left 'for compatibility' is Legacy Path
   Contamination." A permanently-disabled feature gating permanently-dead code is the same
   contamination in a different shape, and it has sat disabled since the 2026-07-16 CLI
   routing flip with no port ever materializing — that is itself the signal this
   functionality is not worth carrying forward. Git history preserves the code if anyone
   needs to resurrect it later; this is a real removal, not a soft deprecation.
5. Remove the 3 stale `-p ggen-core` justfile targets ticket 11 already identified
   (`justfile:100,106` under `test-phase2`, plus `slo-check`'s
   `cargo test -p ggen-core --test inverse_receipt_chain_test`) and retarget per ticket 11's
   own candidate (`cargo test -p ggen-engine --test receipt_chain_e2e`), but first close the
   gap ticket 11 flagged and left open: neither the old nor the new target actually asserts
   the "<5s" timing claim the justfile comment makes. Either add a real
   `Instant`/`Duration` assertion or correct the comment — do not carry the false claim
   forward silently (this is the exact "Decorative Completion" mistake class this repo's own
   `.claude/rules/coding-agent-mistakes.md` names).
6. Run the Phase 7 Definition of Done exactly as ticket 11 specifies: `just check`,
   `just test`, `just lint`, retargeted `just slo-check`, plus the OTEL verification (real
   `ggen sync run` output showing all five `pipeline.*` spans, cross-checked against the
   receipt's actual file list — not just "tests passed").

## Risk / what could go wrong

- **Ticket 11's "unclassified" `chain_linking.rs` isn't re-verified in this document** — do
  the one remaining grep (step 1) before deleting, not after.
- **Hidden feature-gated logic loss — accepted, not a blocker.** `ggen-core-retired`-gated
  files (`wizard.rs`, `sigma.rs`, `inverse_sync.rs`) may contain functionality nobody has
  re-implemented in `ggen-engine`. The user has made the call to abandon this functionality
  rather than port it (see step 4 above) — recorded here so the tradeoff is explicit in the
  historical record, not silently lost. If this decision needs revisiting later, the code is
  still recoverable from git history after deletion.
- **This session's own JTBD verification workflow found `ggen doctor run` has a live,
  unrelated bug** (unconditional wrong-schema parser — see the tracked GitHub issue filed
  this session) — unrelated to ggen-core removal, but worth fixing or at least being aware
  of before claiming a clean Phase 7 Definition of Done, since `doctor` is one of the nouns
  the DoD gate exercises.

## Sequencing

Executes as its own PR, immediately after #256 and #257 both land on `main` — not bundled
with them, so the removal's diff stays reviewable on its own and doesn't get entangled with
the docs/examples archival or the test-cleanup + cheat-scanner work. Same PR-based workflow
as #255/#256/#257 (this repo's `main` branch requires PRs; direct pushes are rejected by
branch protection).
