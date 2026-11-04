# zd-118: Tag Falsifier Sweep (CARRY-19 prep)

- **Date**: 2026-11-04
- **Lane**: zd-118
- **Artifacts**: `scripts/tag_falsifier_sweep.sh` (new), this receipt.
- **Standing**: ALIVE (script executed this session on 3 real repos, exits recorded below)

## What

Bash falsifier sweep verifying every commit SHA cited in
`docs/jira/v26.11.3/closure/*.md` and `docs/jira/v26.11.3/receipts/*.md` is
reachable from the v26.11.3 tag (or `--at-ref <sha>` pre-tag rehearsal).

Method per file: `git show HEAD:<file> | grep -oE '[0-9a-f]{7,40}'` → dedupe →
resolve in-repo via `git cat-file -t` (non-resolving = FOREIGN, disclosed and
skipped per-repo) → `git merge-base --is-ancestor <sha> <ref>`.

Disclosed heuristic: tokens matching `^[0-9]+$` (pure decimal — dates, byte
counts, PR numbers) are dropped as non-SHA; a digit-only token in a commit SHA
is vanishingly unlikely. Uppercase hex accepted and normalized to lowercase.

## Verification

- `bash -n` — PASS (SYNTAX-OK)
- `shellcheck` — PASS, zero findings (SHELLCHECK-CLEAN)

## Rehearsal receipts (--at-ref mode, tag not yet cut)

| repo | ref | files | cited SHAs | reachable | foreign | UNREACHABLE | exit |
|---|---|---|---|---|---|---|---|
| ggen | `468291ae3364...f93d` (HEAD, v26-11-3-wave) | 149 | 161 | 23 | 137 | **1** | 1 |
| cgap | `4bd598d8e327...9e888` (HEAD) | 82 | 88 | 15 | 73 | 0 | 0 |
| ggen-marketplace | `7364405a048a...c224ae6` (HEAD) | 12 | 20 | 15 | 5 | 0 | 0 |

Note the falsifier works as designed: it refuses loudly (exit 1) rather than
waving through.

## The 1 ggen UNREACHABLE — known divergence, not a defect

`10fcf3d6b` (ggen tag `v26.15.2`, Homebrew release, on the divergent main
line) is cited in `docs/jira/v26.11.3/closure/publish-runbook.md` **because**
the runbook documents that it is NOT an ancestor of the release branch — the
doc is the receipt of the divergence, and the sweep correctly refuses. This is
exactly the CARRY-19 expectation: per-repo scoping discloses it rather than
silently passing.

Options at tag time (recorded for the tag cutter): either (a) accept exit 1 on
this known-documented divergence, or (b) add a documented-divergence allowlist
argument to the script. Default is (a) — a sweep that can exit 1 is the point.

## Foreign-SHA class (expected, disclosed)

137 foreign SHAs in ggen's docs are cross-repo citations (smalt/xaas/claude
repos, pre-repair lsp-max SHAs) that don't resolve in ggen — disclosed per
file in the script's FOREIGN list, skipped gracefully. 73 in cgap, 0 in
ggen-marketplace.

## Commands / exits

- `scripts/tag_falsifier_sweep.sh /Users/sac/ggen --at-ref 468291ae...` → exit 1 (expected refusal; known divergence above)
- `scripts/tag_falsifier_sweep.sh /Users/sac/cgap --at-ref 4bd598d8...` → exit 0 PASS
- `scripts/tag_falsifier_sweep.sh /Users/sac/ggen-marketplace --at-ref 7364405a...` → exit 0 PASS
