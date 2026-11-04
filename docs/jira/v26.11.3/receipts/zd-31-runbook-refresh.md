# ZD-31: publish-runbook refresh (ZB-2)

Date: 2026-11-04. Repo: /Users/sac/ggen, branch `v26-11-3-wave`.
Owns: `docs/jira/v26.11.3/closure/publish-runbook.md` (refresh only, §5
added + stale values struck/replaced), this receipt. Not committed
(per lane law — coordinator owns git).

## Trigger

ZB-2 finding: the runbook's as-of SHAs were stale — the
`v26-11-3-wave` tip had moved 87d04ccea → 72b743d23 since the runbook
was written. Law applied: every number in the refresh came from a live
command this session; each cited SHA re-verified with `git cat-file -t`
and `git merge-base --is-ancestor` per the runbook's own falsifier law.

## Live verification ladder (all commands run this session)

| Claim | Command | Result |
|---|---|---|
| ggen tip = 72b743d23 | `git rev-parse HEAD` | `72b743d230a2bb0a8c93c1f28ef1dc4c5fa29825` |
| 87d04ccea exists + ancestor | `git cat-file -t` / `merge-base --is-ancestor` | commit / pass |
| v26.15.2 (10fcf3d6b) off-lineage | `git merge-base --is-ancestor 10fcf3d6b HEAD` | fail → NOT ancestor (confirms disclosure) |
| no v26.11.* tag on ggen | `git tag -l 'v26.11*'` | only `v26.11.0` |
| engine pin resolved | `grep version crates/ggen-engine/Cargo.toml` | line 10: `version.workspace = true` (26.11.3) |
| PR #824 | `gh pr view 824` | OPEN, MERGEABLE, UNSTABLE, head 72b743d23; 13 failing, 3 in-progress |
| cgap tip | `git rev-parse HEAD` in /Users/sac/cgap | 37721e2fa (f561b563 verified ancestor) |
| cgap dirty | `git status --porcelain \| wc -l` | 23 (14 M / 9 ??) |
| cgap-mcp / wire-shm publish lines | grep crates/*/Cargo.toml | cgap-mcp:9 `publish = false`, wire-shm:7 `publish = false`; 19 crates `publish = true` |
| cgap licenses gate | grep ci.yml | `.github/workflows/ci.yml:148,170,174-175` — `cargo deny check licenses` BLOCKING since YD-1, LicenseRef-P28-SIL |
| marketplace tip vs origin | `git rev-parse HEAD` / `origin/main` in /Users/sac/ggen-marketplace | HEAD e7c58d5de, origin d451e63e4 → local 1 ahead |
| marketplace dirty | `git status --porcelain` | 1 file: `packs/industry-manufacturing-dflss/templates/shacl-manufacturing-shapes.ttl.tmpl` |
| d451e63e4 / 7834fd045 ancestry | `cat-file -t` + `merge-base --is-ancestor` | both commit + ancestor of marketplace HEAD |
| marketplace CHANGELOG cut | head CHANGELOG.md | opens `## [v26.11.3] — 2026-11-04`; no `[Unreleased]` |
| baseline re-projected | `python3 json.load(qualification/baseline.json)` | schema v1, packs 347; counts ALIVE 327 / WARN 10 / REFUSED 9 / SKIPPED 1 |
| lsp-max resolved | `git rev-parse origin/master` in /Users/sac/lsp-max | = c261f613 (pushed); HEAD = c261f613; only untracked `target-finalverify/` scratch |
| arcus local-only | `git rev-parse HEAD` / `git remote -v` in /Users/sac/arcus | 1c08fd1 ("YD-2 toolchain pin, go1.26.10, vulncheck clean"), 0 remotes |
| ggen tag v26.15.2 exists | `git tag -l 'v26.15*'` | v26.15.0/.1/.2 present |

## Runbook changes

- § header: added refresh pointer; §5 supersedes stale values.
- §1 ggen header SHA 87d04ccea → 72b743d23 (with ancestor note); §1.1
  PR-#824 check list replaced with live 13-failing/3-pending rollup.
- §1.2 struck and marked RESOLVED (`version.workspace = true`).
- §1 cgap header f561b563 → 37721e2fa; item 1 updated to live 23-entry
  dirty count; item 3 struck RESOLVED (publish=false lines); new item 4
  (licenses gate blocking); renumbered.
- §1 marketplace header → e7c58d5de / d451e63e4 push state; item 1
  push/dirty facts; item 2 CHANGELOG DONE; item 3 baseline 313→347 with
  live counts; 7834fd045 ancestry retained (re-verified).
- §2 tag-at table cgap row f561b563 → 37721e2fa; negative findings
  updated (arcus 3a6cffc → 1c08fd1; lsp-max c977c1b6 → c261f613 pushed,
  origin/master = c261f613).
- §3 Phase 2 publish-ambiguity note resolved; Phase 3 step 1 marked
  no-op.
- New §5 "2026-11-04 refresh (ZB-2)": per-repo readiness table
  (ggen BLOCKERS-3→recount, cgap dirty, marketplace local-ahead, lsp-max
  resolved, arcus local-only), resolved publish-hygiene items (engine
  version.workspace, cgap-mcp/wire-shm publish=false, licenses gate
  blocking, CHANGELOG cut, baseline 347), still-open list.

## Corrections to the prompt-supplied baseline (recorded, not silently fixed)

- Prompt said marketplace "clean + pushed d451e63e4" — live: d451e63e4
  is origin/main, but local HEAD is e7c58d5de (1 unpushed commit) with
  1 dirty file. Recorded as local-ahead, not clean.
- Prompt said lsp-max "resolved c261f613" — confirmed; superseded
  runbook's c977c1b6 (which still exists as an ancestor-lineage commit).

## Standing

ALIVE for the verification ladder (all commands executed, outputs
above). PARTIAL for publish readiness itself — PR #824 unmerged, tags
unminted, dirty trees outstanding. Runbook not committed (lane law).
