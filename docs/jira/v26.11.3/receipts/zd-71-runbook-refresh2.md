# ZD-71 — publish-runbook second as-of refresh

Date: 2026-11-04. Subject: /Users/sac/ggen @ v26-11-3-wave tip
58a3b4108d6b7cf98a2b8429511ec3afa497d771 (PR #824 head). Scope:
`docs/jira/v26.11.3/closure/publish-runbook.md` §5 only — appended
"2026-11-04 second as-of refresh (ZD-71)" subsection; §1-§4 untouched
(explicitly superseded by the new subsection). No git actions taken
(uncommitted, per contract).

## Method (all live, this session)

- `git fetch origin` + `rev-parse`/`merge-base --is-ancestor` per repo;
  `git cat-file -t` on every cited SHA (all six returned `commit`).
- `gh pr view 824 --json statusCheckRollup,...` (live).
- `git status --porcelain`, `git tag -l`, `python3 json.load(baseline.json)`.
- Receipts on disk: cgap zd-43/zd-53/zd-55/zd-63, marketplace zd-62,
  ggen zd-66/zd-69 — read from disk, not recalled.

## Live SHAs verified

| Repo | SHA | cat-file | ancestry |
|---|---|---|---|
| ggen | 58a3b4108 (= origin/v26-11-3-wave, = PR #824 head) | commit | self / ancestor-of-origin-true |
| cgap | e3233191 (= origin/main) | commit | ancestor-of-main true |
| ggen-marketplace | b408361c1 (= origin/main) | commit | ancestor-of-main true |
| lsp-max | bd97e5c (= origin/master) | commit | ancestor-of-master true |
| arcus | fed6db3 (0 remotes) | commit | n/a (local-only terminal) |
| claude-code-config-lsp | 6a04124 (= origin/main) | commit | ancestor-of-main true |

Negative findings: no `v26.11.3` tag in any of the three repos (latest:
v26.11.0 / v26.11.2 / v26.10.8-3); ggen `v26.15.2` (10fcf3d6b, commit)
re-verified NOT an ancestor of the release branch.

## Key re-counts (changed vs ZB-2 / zd-31)

- PR #824: 14 failing / 0 pending (was 13+3); fail set composition
  shifted (`Deep / integration_deep`, `observe-manufacture-falsify-replay`
  now failing; `Deep / core_deep`, `Deep / quality_deep` no longer in
  fail set). Merge gate unsatisfied.
- cgap dirty: 23 → 2 (terminal sweep e3233191 landed; 2 untracked
  cgap-ffi entries remain — the zd-43 dirty-worktree class for cgap-ffi).
- marketplace: origin/main advanced d451e63e4 → b408361c1 (zd-49/zd-52
  push landed); dirty 1 → 28 entries (uncommitted ZD-62/MKT-01 edits +
  zd-28/zd-62 receipts).
- baseline.json: 327/10/9/1 → **347 packs, ALIVE 326 / WARN 13 /
  REFUSED 7 / SKIPPED 1** (live json.load; ZD-49 message cites an
  intermediate 332/5, superseded by the uncommitted ZD-62
  re-projection).
- DoD: 12/12 MEASURED rows in cgap dod-checklist.md (grep count 12).
- Dry-run ladder: 7/20 exit 0 (L0 fully green), 11 registry-dep-missing
  (self-healing at Stage 3), 2 dirty-worktree (cgap-audit since
  committed; cgap-ffi still open).
- MKT-01 design settled: ambient ggen 26.11.3 court identity + disclosed
  `--skip-ea-admission` opt-out in `scripts/qualify_packs.py`
  (zd-62-mkt01-adoption.md on disk in ggen-marketplace).
- lsp-max tip moved c261f613 → bd97e5c (ZD-47 probe committed + ZD-54
  drift fix, pushed); arcus tip moved 1c08fd1 → fed6db3 (W2-D, still
  0 remotes).

## Standing

PARTIAL — documentation refresh; every value above is from a live
command this session; no gates executed, no git transitions performed.
Runbook falsifier law (§2) applied to each cited SHA: all exist, all
lineage claims verified. zd-67/68/70 receipts absent from
`docs/jira/v26.11.3/receipts/` at write time — cited as in-flight, not
landed.
