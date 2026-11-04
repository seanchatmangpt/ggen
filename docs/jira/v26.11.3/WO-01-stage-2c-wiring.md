# WO-01: Stage 2c Admission Wiring — Five Defect Fixes in sync.rs

## Date

2026-11-03

## Repo

/Users/sac/ggen (canonical checkout; no worktrees)

## Lane

WRITER / implement lane — admits-path only. Conflicts with WO-03 (plan/manufacture integration) are fenced, not resolved here.

## Subject

`crates/ggen-engine/src/sync.rs` (~lines 1007-1095) — the Stage 2c admission seam currently calls the kernel with placeholder/unwired arguments and fails open on missing inputs.

## Task

Fix the five Stage 2c wiring defects, grounded in the Explore findings:

1. **(a) Caps not passed.** The admit call is 2-arg `admit(...)`. Translate `res:Envelope` capabilities into `EnvelopeCaps` (envelope.rs `extract_caps` → kernel `CapTable`, carrying `capability.max_artifacts`) and call `admit_with_caps`. The seam note at ggen-abb-sbb `lib.rs:47-56` is aspirational today — this WO makes the call real.
2. **(b) Refusals stringified.** Kernel `Refusal` variants are flattened into the string form of `[FM-LAW-015]`, destroying the typed code. Add a new `AppError` constructor carrying the refusal variant (or its registry code) so callers match on the variant, not a string.
3. **(c) Placeholder digest/generation.** Stop hardcoding `expected_graph_digest: None` and `artifact_baseline_generation: 0`. Pin the digest from the loaded graph and wire the 2c-epoch lease generation — `sync.rs:983` already reads the receipt-log tail; use it.
4. **(d) Fence, not fix.** `plan()`/`manufacture()` integration is deferred to WO-03. This WO touches the admit path only.
5. **(e) Missing ea.graph.json fails open.** At `sync.rs:1046` a missing `ea.graph.json` silently skips the stage. Make it explicit: refuse unless a manifest flag or `--skip-ea-admission` is set. Fail-closed default per R3 in RESOLUTIONS.md.

## Acceptance

Falsifiable, command-cited. All from `/Users/sac/ggen`:

1. **Mutation courts — each defect re-introduced flips a named test.** For each of the five defects, a documented mutation (revert to 2-arg admit / stringify the refusal / restore `None`+`0` / delete the fail-closed guard) causes a named test in the suite to fail; restoring the fix makes it pass. Record per-defect: mutation, test name, `cargo test -p ggen-engine <test>` exit codes before/after.
2. **Per-SBB admission test with real envelope caps.** A Chicago-style test drives admission for each SBB with a real `res:Envelope` → `extract_caps` → `CapTable` chain (real files, no mocks); asserts the kernel received the expected `max_artifacts` and admits within caps.
3. **Typed refusal preservation.** A test asserts a kernel refusal surfaces as the new `AppError` variant with the registry code intact — no `[FM-LAW-015]` stringification on the refusal path (`grep -n "FM-LAW-015" crates/ggen-engine/src/sync.rs` shows no refusal-mapping hit for kernel refusals).
4. **Digest/epoch wiring.** A test asserts the pinned `expected_graph_digest` equals the loaded graph's digest and `artifact_baseline_generation` equals the receipt-log tail epoch; a mismatched digest refuses.
5. **Fail-closed EA gate.** Missing `ea.graph.json` with no flag → typed refusal; with `--skip-ea-admission` or manifest flag → proceeds with a loud skip marker.
6. **Grep gate — `admit_with_caps` is called.** `grep -n "admit_with_caps" crates/ggen-engine/src/sync.rs` exits 0 with a call site inside the Stage 2c block; the 2-arg `admit(` call is gone from that block.
7. **Build + full engine suite green.** `cargo build -p ggen-engine` and `cargo test -p ggen-engine` exit 0.

## Constraints

- **No kernel API changes**: ggen-abb-sbb is frozen for this WO. `admit_with_caps`, `EnvelopeCaps`, `CapTable`, `Refusal` are consumed as-is.
- **Chicago tests**: real collaborators (real envelope files, real receipt log, real graph), assert on final state; no mocks, no stubs.
- Admit path only — no edits to `plan()`/`manufacture()` integration (WO-03 owns that).
- Fail-closed is the default; skips must be explicit and loud, never silent.
- One canonical checkout; no worktrees; no `git reset --hard`.

## Standing

UNVERIFIED — no acceptance command has been run against a fixed tree yet.

## Falsifiers

- `grep -n "admit_with_caps" crates/ggen-engine/src/sync.rs` exits 1 (wiring not landed) or shows only a comment.
- Any of the five mutations re-applied still passes the full `cargo test -p ggen-engine` suite (court is vacuous).
- Kernel refusal still renders as `[FM-LAW-015]` string on any test path.
- `ea.graph.json` absent → sync proceeds without a typed refusal and without an explicit skip flag.
- Any test under this WO constructs a `Mock`/stub in place of a real envelope/receipt-log collaborator.

## Fences

- Do not modify `crates/ggen-abb-sbb` (frozen kernel surface for this WO).
- Do not touch `plan()`/`manufacture()` wiring — WO-03's lane.
- Do not add sleeps or timing-based assertions; poll real state.
- Do not widen the skip escape hatch beyond the manifest flag and `--skip-ea-admission`.
