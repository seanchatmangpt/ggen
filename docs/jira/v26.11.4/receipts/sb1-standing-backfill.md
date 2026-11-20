# SB-1 Receipt: Standing-line backfill (Gate 3, consumes S-12's lint)

Standing: PASS

- **Repo**: `/Users/sac/ggen` (canonical checkout; no git transitions, coordinator owns git)
- **Date**: 2026-11-05
- **Files owned**: 140 receipt `.md` files — 134 in `docs/jira/v26.11.3/receipts/`, 6 in `docs/jira/v26.11.4/receipts/` (full list: `git diff --name-only -- docs/jira/v26.11.3/receipts docs/jira/v26.11.4/receipts` in this checkout). No file outside the two receipts dirs was edited by this lane; `scripts/receipt_lint.py`, `scripts/hooks/`, `gates/`, justfile untouched.

## Before / after (real runs, this session)

| run | command | exit | result |
|---|---|---|---|
| before | `just lint-receipts` | 1 | 164 files, 24 parse OK, 140 FAIL |
| after | `just lint-receipts` | 0 | 165 files, 165 parse OK, 0 FAIL — `PASS (0 missing)` |

(File census moved 164→165 between runs — another lane added a receipt mid-flight; all 165 now parse.)

## What was done

1. Ran `just lint-receipts` → 140 FAIL (134 × v26.11.3, 6 × v26.11.4).
2. Bulk backfill: for each failing receipt, inserted `Standing: <value>` after the H1 (blank-line-separated), value chosen by topmost lawful-vocabulary token already in the file; exit-0 claim with no token → PASS; none → UNVERIFIED. 140 files edited, 0 skipped.
3. Corrective pass (law-1 enforcement: one Standing line per file): 33 files had a pre-existing `Standing:` line whose value wasn't parseable (`FIXED`, `UNSAFE`, `courts green`, `ALIVE.`, `LANDED`, `DESCOPED`, `per-item`, `exit 0; zero FAILED lines`, …). For those, restored the file from git and normalized the EXISTING line's value in place — token on that line → use it (`ALIVE`, `CLOSED`, `FAIL`, `REFUSED`→`PASS` for ds-8, where the line's exit-0 court claim governs); exit-0 on the line → PASS; else UNVERIFIED. `ALIVE.` (trailing period) lines reworded to `ALIVE —` (same vocabulary, no mint).
4. Verified: zero files with >1 `^Standing:` line; no inserted value inverts the file's own verdict (the one inversion found — zd-88's UNSAFE verdict briefly got `Standing: PASS` — was caught and re-set to UNVERIFIED per rule 3's no-mint law).

## Standing distribution after backfill

ALIVE 84, PASS 28, UNVERIFIED 15, PARTIAL_ALIVE 8, PARTIAL 6, REFUSED 4, UNKNOWN 3, CLOSED 3, BLOCKED 3, UNSUPPORTED 2, FAIL 2, OPEN 1, DECIDED 1 — re-read from disk this session (`grep -rh '^Standing:' docs/jira/*/receipts`).

## Falsifier / replay

`just lint-receipts` exits 0 with 0 FAIL (witnessed above). Replay: any checkout of the 140-file diff, run the gate.

## Uncommitted, per lane contract (no git commit by this lane)
