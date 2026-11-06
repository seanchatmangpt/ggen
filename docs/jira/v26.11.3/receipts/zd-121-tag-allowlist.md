# zd-121: Tag Falsifier Sweep `--allow-divergence` Allowlist

Standing: ALIVE

- **Date**: 2026-11-04
- **Lane**: zd-121
- **Artifacts**: `scripts/tag_falsifier_sweep.sh` (modified), this receipt.
- **Standing**: ALIVE (executed this session, exits recorded below)

## What

Implements zd-118 option (b): a repeatable `--allow-divergence <sha>` flag on
`scripts/tag_falsifier_sweep.sh`. SHAs on the list are reported as
KNOWN-DIVERGENCE (documented) instead of UNREACHABLE and do not fail the exit.
Without the flag, behavior is unchanged (default accept exit 1 per zd-118).

## Semantics

- Entries are resolved to full 40-char commit SHAs via `git rev-parse` at
  parse time; an entry that does not resolve to a commit in the target repo
  fails fast (exit 2) rather than silently allowing nothing.
- Cited tokens may be abbreviated (7-40 hex), so matching is by prefix in
  either direction between the cited token and the allowlist entry.
- An allowlist entry that matched no cited SHA emits a loud WARNING on stderr
  (unmatched allowlist entries are disclosed, never silent).
- A SHA on the list that IS reachable is counted REACHABLE, not
  KNOWN-DIVERGENCE — the allowlist only reclassifies genuinely-unreachable
  citations.

## Verification

- `bash -n` — PASS (SYNTAX-OK)
- `shellcheck` — PASS, zero findings (SHELLCHECK-CLEAN)
- **Mutation witness (no flag, default unchanged)**:
  `scripts/tag_falsifier_sweep.sh /Users/sac/ggen --at-ref e4f231a55d9e641f60529498449d1f51077488fe`
  → **exit 1**, `UNREACHABLE: 1`, `KNOWN-DIVERGENCE (allowed): 0`
  (`UNREACHABLE: 10fcf3d6b cited in docs/jira/v26.11.3/closure/publish-runbook.md`)
- **Rehearsal WITH the flag at the terminal head** (HEAD of v26-11-3-wave,
  `e4f231a55d9e641f60529498449d1f51077488fe`):
  `scripts/tag_falsifier_sweep.sh /Users/sac/ggen --at-ref e4f231a55d9e641f60529498449d1f51077488fe --allow-divergence 10fcf3d6b`
  → **exit 0**, PASS:
  - cited SHAs scanned: 171
  - reachable: 27
  - foreign (skipped, disclosed): 143
  - UNREACHABLE: 0
  - KNOWN-DIVERGENCE (allowed): **1** — `10fcf3d6b`
    (docs/jira/v26.11.3/closure/publish-runbook.md) — exactly one, everything
    else unchanged.

## The allowed divergence

`10fcf3d6b` (ggen tag `v26.15.2`, Homebrew release, divergent main line) is
cited in `docs/jira/v26.11.3/closure/publish-runbook.md` precisely because the
runbook documents it is NOT an ancestor of the release branch — the doc is the
receipt of the divergence. Same rationale as zd-118's finding; the allowlist
converts that documented divergence from a failing refusal into a disclosed
KNOWN-DIVERGENCE classification.

## Commands / exits

- `bash -n scripts/tag_falsifier_sweep.sh` → 0
- `shellcheck scripts/tag_falsifier_sweep.sh` → 0
- `scripts/tag_falsifier_sweep.sh /Users/sac/ggen --at-ref e4f231a5...` (no flag) → exit 1 (default preserved, per zd-118)
- `scripts/tag_falsifier_sweep.sh /Users/sac/ggen --at-ref e4f231a5... --allow-divergence 10fcf3d6b` → exit 0, 1 KNOWN-DIVERGENCE, 0 UNREACHABLE
