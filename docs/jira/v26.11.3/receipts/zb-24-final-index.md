# ZB-24 — final coverage pass at terminal (index refresh 4)

- **Date**: 2026-11-04
- **Repo/branch**: `/Users/sac/ggen`, `v26-11-3-wave` (no git commands run; coordinator owns git)
- **Files owned**: `docs/jira/v26.11.3/RECEIPTS.md`, `docs/jira/v26.11.3/README.md`, this receipt
- **Subject**: ggen @ v26-11-3-wave tip (uncommitted working tree)

## Census

`ls docs/jira/v26.11.3/receipts/ | wc -l` = **130** receipt .md files on disk
before this lane's own receipt, **131** with zb-24-final-index.md on disk
(previous census: zd-58's 119-before / 120-with).

## Coverage gate

Mention-level gate over receipt stems vs `RECEIPTS.md` + `README.md`:

```
for f in receipts/*.md; do s=$(basename "$f" .md); \
  grep -qi "$s" README.md RECEIPTS.md || echo "UNCOVERED: $s"; done
```

At lane start the gate exited 1 with 10 uncovered stems (zd-44, zd-48,
zd-59, zd-66, zd-67, zd-68, zd-69, zd-71, zd-86, zd-87). After this lane's
rows in both index files, the gate exits with **UNCOVERED []** (re-run
included this receipt's own stem zb-24-final-index, covered by the self-row).

## Rows added (one row per uncovered stem, standing re-read from bodies on disk)

| Stem | Standing (from body) | Exit record (from body) |
|---|---|---|
| zd-44-skip-flag | CARRY-04 CLOSED — resolved state (a): flag already wired CLI→gate; gap closed = bypass-marked receipt law landed with fail-closed court | `sync_run_skip` court 2 passed; 0 failed; `--lib` 236/0 (2 ignored); full `--test cli_boundary` 33/0; scratch consumer: no flag → FM-LAW-015 exit 1, `--skip-ea-admission` → exit 0 with receipt bypass mark; 3 failing tests in full run classified pre-existing, none session-introduced |
| zd-48-fm-codes | UNSTATED in body (courts table carries the evidence; census stale-premise correction: 52 not 37 at start, 45 of 119 at end) | 8 FM courts ×2 → 8 passed / 0 failed across 5 files (run 2 on lane-local target root after a concurrent lane swept the shared `target/debug`, disclosed); parity court red pre-existing on HEAD → 3 passed / 0 failed after the FM-KEY-013 registry row |
| zd-59-carry05 | CARRY-05 RESOLVED-DISCLOSED; test surface ALIVE under ambient ggen 26.11.3 | `sync_surface_test` ×2 → 6 passed; 0 failed, exit 0 (14.84s/14.33s); closing fixture commit e9829bead cat-file verified; descope-ledger rows closed only after reading their landing receipts (zd-45/zd-46/zd-47); rows without closing receipts deliberately left open |
| zd-66-doc-clippy | ALIVE for the depgraph.rs:345 fix + two whitespace fixes; crate-level clippy gate PARTIAL (blocked solely on :38, out of lane) | `git diff --check` on both fixed files clean exit 0; clippy `--all-targets -D warnings` (abb-sbb-manufacture.yml:40 invocation): :345 lint gone; `cargo check --locked -p ggen-abb-sbb` exit 0 |
| zd-67-ea-batch2 | ALIVE (all commands run this session, exits cited inline) | `validate_ea_graph` over all four graphs → 4 ADMITTED lines, zero REFUSED; per-root sync exits tai 0, cyberpunk 0 ×2 idempotent, inventoryctl 0, lsp-contract 0; pre-pinning admission refused SBB_DIGEST_MISMATCH (non-vacuity); FM-PACK-008 re-lock executed; CI fix identified as commit-surface (untracked ea.graph.json), no workflow edit |
| zd-68-premise-rot | UNSTATED in body (verification ladder carries the evidence; premise-rot law applied per testing-discipline SDK-parity register) | repair-constructor embedded python ×2 exit 0 idempotent green; self-host seed `sync run` ×2 exit 0 (10m11s run 1) + receipt verify; tpot2 rotted pin observed failing before the applied-markers fix (drift refusal path reachable — non-vacuity); book consumer test re-derived per-consumer (graph present → real admission; absent → per-consumer opt-out, never global bypass) |
| zd-69-enospc-lean | FIXED (uncommitted, working tree, per body Status); real gate is the next PR-#824 push rerun — UNVERIFIED by design | YAML `safe_load` OK ×4; three disk-hygiene blocks added (core-team constructor, gall-001 replay, ci.yml integration_deep reclaim upgraded) with `df -h /` before/after; lean-action step `build: true` → `build: false` (manifest generated in-job via existing `lake update` step); committing lake-manifest.json was preferred but none exists in-tree, disclosed |
| zd-71-runbook-refresh2 | PARTIAL — documentation refresh; every value from a live command this session; no gates executed, no git transitions | live ladder: `git fetch` + rev-parse/merge-base per repo (6 SHAs cat-file verified `commit`); `gh pr view 824` live (14 failing / 0 pending, fail-set composition shifted); cgap dirty 23→2; marketplace dirty 1→28; baseline.json 347 packs (ALIVE 326 / WARN 13 / REFUSED 7 / SKIPPED 1); dry-run ladder 7/20 exit 0 |
| zd-86-tai-evidence | Workflow-level ALIVE (ZB-19's two suspected defects do not exist — both misattributions from echoed script text); whitespace unblocks fixed verified exit 0; ea.graph.json staged + kernel-validated; full TAI job green on runners UNVERIFIED (needs next push) | `gh run view` on both runs → real fail steps named ("Manufacture TAI-REBUILD-001 twice" FM-LAW-015 on untracked ea.graph.json; docs_deep `git diff --check` exit 2 on 2 whitespace defects); `git diff --check` working tree exit 0 after; `validate_ea_graph` exit 0 ADMITTED; yaml.safe_load ×3 workflows OK; SLA minimal-unblock disclosures: CLAUDE.md EOF + zd-16:22 trailing space (other lanes' files) |
| zd-87-fmkey-ttl | UNSTATED in body (three-way cross-check + court evidence carry it; closes the pre-existing `fm_diagnostic_codes_ttl_matches_real_source` failure ZD-44 classified) | `fm_diagnostic_codes_ttl_matches_real_source` ×2 → 1 passed; 0 failed; full product_mirror_conformance 3 passed / 0 failed; `prod:fm_KEY_13` declared grounded in the file's existing DiagnosticCode individuals |

## Brief-candidate check (not on disk at this lane's run)

`ls docs/jira/v26.11.3/receipts/` carries no zd-60, zd-70, zd-76, zd-77,
zd-79, zd-85, zd-88, zd-90, zd-91 or zd-96 file — none rowed; nothing on
disk is uncovered (gate UNCOVERED [] above is the witness).

## Method notes

- Standing words re-read from each receipt body on disk, never from memory;
  UNSTATED where the body carries none.
- Both index files received the same 10 rows (RECEIPTS.md new zb-24 section;
  README.md post-close section) plus the self-row; counts updated in both.
- No commits made, per contract.
