# Chesterton's fence inventory

**Status:** Real investigation, not speculation — every candidate below was checked against `git log --all`, `git log --diff-filter=D --summary`, or a live command run, in this worktree, 2026-07-31. For each candidate the verdict is either "reason found: X, see commit Y" or "no reason found after investigation of [what was checked]". Candidates were selected from the fifteen individuals in `ontology/v26.8.1/legacy-capabilities.ttl` that looked, on first read, like they might be removable without cost.

## Candidate 1: `crates/ggen-core` (deleted)

**Looked removable because:** the crate no longer exists on disk and nothing in the current workspace depends on it.

**Investigation:** `git log --oneline --all -- crates/ggen-core` shows a full history ending in `9cef6e40f` / `0eec35f49` ("remove ggen-core, rewrite README from first principles", #259) and, before that, `cbf173f82` ("Retire ggen-core in favor of a first-principles engine", #255). The commit body of `9cef6e40f` explicitly references `docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md`'s Phase 6/7 and states PR #255 only disconnected the crate (moved to `exclude`); #259 executed the outright deletion "now that every real dependent has been confirmed gone".

**Verdict:** reason found — this was a two-phase, deliberate, ticketed removal (docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md and 14-GGEN-CORE-REMOVAL-PROPOSAL.md), not an accidental deletion. Disposition: `REPLACED` by `ggen-engine`. No fence to restore.

## Candidate 2: `ggen wizard` / `ggen sigma` / `ggen inverse_sync` commands (deleted)

**Looked removable because:** three whole CLI command files were deleted in the same commit as `ggen-core`, and they sound like plausible product features to keep.

**Investigation:** `git log --oneline --all -- crates/ggen-cli/src/cmds/wizard.rs` shows the file's full life: introduced at `d0b9ff1c6` ("feat(wizard): Add ggen wizard <verb> commands to CLI"), extended through `858d74684` (DSPy I/O shaping) and `b613ca4d5`/`efb70ecaf` ("Implement ggen wizard bootstrap factory"), then deleted at `9cef6e40f`. The deleting commit's own body says these were "abandoned not ported per the proposal's explicit decision" — i.e. the removal proposal doc explicitly chose not to re-point them at `ggen-engine`, rather than losing them accidentally. `.claude/rules/architecture.md`'s Crate Map corroborates: "the experimental, default-off `ggen wizard`/`sigma`/`inverse_sync` commands ... were deleted in the same pass rather than re-pointed".

**Verdict:** reason found — deliberate, documented decision (not silence, not accident) to drop these as out-of-scope for the ggen-core replacement rather than migrate them. Disposition: `REFUSED`, all three. If InterviewAssist or a future phase wants an interactive bootstrap wizard again, it would be new work against `ggen-engine`, not a restoration.

## Candidate 3: `ggen-a2a-mcp`, `ggen-lsp-mcp`, `ggen-lsp-a2a` (deleted as standalone crates)

**Looked removable because:** three entire crates (with their own Cargo.toml, tests, and protocol code) vanished from the workspace member list.

**Investigation:** `git log --oneline --all --diff-filter=D -- crates/ggen-a2a-mcp/Cargo.toml crates/ggen-lsp-mcp/Cargo.toml crates/ggen-lsp-a2a/Cargo.toml` finds exactly one commit, `bde78f7d5` ("chore(consolidation): phase 4 - fold lsp trio into ggen-lsp behind features"). Two earlier commits on the same file set, `e6a616ffc` and `065e11d94`/`58741e7e5`, show the *reason* this became possible: the crates' custom hand-rolled MCP protocol code was first replaced by the `rmcp` 1.3.0 crate, which shrank the amount of protocol plumbing enough that folding three crates into one behind Cargo features (`mcp`, `a2a`) was a net simplification rather than a loss.

**Verdict:** reason found — a real, staged consolidation (custom-protocol-to-rmcp swap, then crate fold-in), not a silent drop. Disposition: `SUBSUMED` by `ggen-lsp`'s `mcp`/`a2a` features, all three. Fence justified; no restoration needed as long as `ggen-lsp`'s feature-gated modules keep the same protocol surface (this session did not independently re-verify wire-level protocol parity — that is exactly the kind of equivalence-verifier work later phases must wire in).

## Candidate 4: `crates/stpnt` and the original `crates/genesis-core` (deleted)

**Looked removable because:** the deleting commit message itself calls them "dead crates".

**Investigation:** `git log --oneline --all --diff-filter=D -- crates/stpnt crates/genesis-core` finds one commit, `dfa3664a5` ("chore(consolidation): phase 2 - remove stpnt and genesis-core (dead crates)"). The commit message asserts zero dependents at removal time. This session did **not** independently re-derive that claim (e.g. by checking out the pre-removal tree and running `cargo tree` for reverse-dependents) — it is trusting the commit author's own stated rationale, which is weaker evidence than an independently reproduced check.

**Verdict:** reason found, but with a caveat — the stated reason ("dead code, zero dependents") is plausible and matches `.claude/rules/architecture.md`'s "Removed in the 2026-07 consolidation pass" list, but this session did not re-verify the zero-dependents claim against pre-removal history. Disposition: `REFUSED` for `stpnt` (no successor referenced anywhere); `ARCHIVED` (not `SUBSUMED`) for the original `genesis-core`, because a plausible successor (`genesis-core-v2`) exists by name and domain but no commit in this session's evidence links them directly — upgrading that disposition to `SUBSUMED` needs a commit-level check this session did not perform.

## Candidate 5: `just sync` (`ggen sync --audit true`) and `just sync-dry` (`ggen sync --dry_run true`)

**Looked removable because:** at first glance these look like recipes that were simply never updated after a refactor — a stale-doc problem, easy to "fix" by just editing the justfile.

**Investigation:** ran both recipes directly (per `CLAUDE.md`'s documented transcripts, re-confirmed this session is consistent with the current `crates/ggen-engine/src/verbs/sync.rs` clap definition, which accepts only `--dry-run`/`--watch`, no `--audit`, and `--dry-run` as a bare switch not a value-taking flag). Searched `git log --all` for any commit that ever added an `--audit` flag to the sync verb or made `--dry-run` value-taking — found none. This is genuinely unresolved: it is not clear from history whether `--audit` was a planned feature that got dropped, a copy-paste error from a different tool's flag convention, or aspirational documentation that predated the actual verb implementation.

**Verdict:** **no reason found** after investigating the sync verb's `git log --all` history and the justfile's own history for these two recipes. This is a genuine open fence — not yet safe to "just fix" by guessing which side (the justfile or the verb) is wrong, since a wrong guess could silently change sync's real argument contract. Disposition: `UNKNOWN` for both (`legacy_sync_audit_flag`, `legacy_sync_dry_run_value_flag`). Recommendation for a later phase: check whether `--audit`/value-taking `--dry-run` ever existed on *any* branch (not just the ones this session's `git log --all` walked, which does cover all refs) before deciding whether to add the flag or fix the justfile.

## Candidate 6: `ggen.toml`'s two incompatible schemas

**Looked removable because:** having two independently-defined, incompatible parsers for the same config file file sounds like unintentional drift that should be unified.

**Investigation:** `.claude/rules/architecture.md`'s "ggen.toml has two schemas" section documents the split precisely (`ggen_engine::generation_rules::has_generation_rules` pre-parse dispatch, `crates/ggen-config/src/manifest/types.rs`'s `GgenManifest` vs. `crates/ggen-engine/src/config.rs`'s `GgenConfig`) but does not cite a single commit explaining *why* both were kept rather than unified. This session searched `git log --all` for commits touching both `crates/ggen-config/src/manifest` and `crates/ggen-engine/src/config.rs` in the same change and found none that reconciles them — they appear to have evolved independently rather than as a deliberate two-schema design.

**Verdict:** **no reason found** after investigating architecture.md's own account and searching for a reconciling commit. Unlike candidates 1–4, this does not look like a documented deliberate decision — it reads as unowned drift that the repository's own documentation flags but has not resolved. Disposition: `UNKNOWN` (`legacy_ggen_toml_dual_schema`). This is the strongest candidate in this inventory for a real cleanup or an explicit "this is intentional because X" ADR in a later phase — right now neither the "unify" fence nor the "keep both, they serve different purposes" fence has documented support.
