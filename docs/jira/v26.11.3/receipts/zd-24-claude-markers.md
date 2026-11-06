# ZD-24: CLAUDE.md merge-conflict markers

Standing: PASS

**Date**: 2026-11-04 · **Branch**: `v26-11-3-wave` · **Status**: RESOLVED (uncommitted)

## Subject

`/Users/sac/ggen/CLAUDE.md` carried raw conflict markers `<<<<<<< GENERATED` (line 1) /
`=======` (line 72) / `>>>>>>> MANUAL` (line 74) wrapping the Crate Map / Commands /
Diagnostic Codes content, plus the bare stub `// Add your manual code here` (line 73).
HEAD (checked: `git show HEAD:CLAUDE.md`) contains the same 3 marker lines — the conflicted
state is committed, not a local artifact.

## Generator check (per task)

No generator owns this file. `.specify/repo-facts.ttl` supplies the crate-map *content*
("GENERATED regions of CLAUDE.md / README.md (Merge) are rendered from this", but no script
in `scripts/`, `.specify/`, or the justfile renders CLAUDE.md itself). The crate-map parity
test (`crates/ggen-config/tests/system_crate_map_parity_test.rs`) compares repo-facts.ttl ↔
Cargo.toml only; it does not read CLAUDE.md or regenerate it. Hand-resolve is the lawful fix;
no template emitted the markers.

## Resolution

Kept the GENERATED-side content (the richer, parity-tested surface), dropped the 3 marker
lines and the stub line. File: 74 → 70 lines.

## Commands / exits

```
grep -c '<<<<<<<' CLAUDE.md            → 0        (exit 1: zero matches)
grep -c '=======\|>>>>>>>\|Add your manual' CLAUDE.md → 0 (exit 1: zero matches)
wc -l CLAUDE.md                        → 70
cargo test -p ggen-config --test system_crate_map_parity_test → 1 passed; 1 FAILED
```

## Parity test result (honest)

The parity suite runs 2 tests: `cargo_toml_finds_real_workspace_crate_members` — **PASS** —
and `repo_facts_ttl_crate_map_matches_cargo_toml_workspace_members` — **FAIL**, but NOT
session-introduced and NOT CLAUDE.md-related: the test compares `.specify/repo-facts.ttl` ↔
`Cargo.toml` and panics at
`system_crate_map_parity_test.rs:137` with:

> crate(s) present in Cargo.toml [workspace] members but MISSING from .specify/repo-facts.ttl's
> rf:Crate facts: ["ggen-scip", "ggen-sjira"]

Neither file is modified in this session (both match HEAD; `git status` clean on both;
`Cargo.toml` last touched by commit 301849a9c "version: bump workspace to 26.11.3" which
admitted the two new members without the corresponding repo-facts.ttl entries). Fixing it
requires adding `rf:Crate` individuals for `ggen-scip`/`ggen-sjira` to `.specify/repo-facts.ttl`
— outside this lane's file ownership (CLAUDE.md + this receipt only).

## Concurrency note

Mid-session, CLAUDE.md reverted to the conflicted state once (a concurrent lane overwrote it;
observed via grep after a prior successful edit + verify). Re-applied the same resolution and
re-verified; final on-disk state is marker-free.

## Standing

ALIVE for the owned surface (markers gone, verified by grep on disk). Do NOT commit (per
task instruction). The repo-facts.ttl gap is a live pre-existing HEAD defect, BLOCKED for this
lane, filed here for the coordinator.
