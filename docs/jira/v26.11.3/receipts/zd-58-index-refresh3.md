# ZD-58: Index Refresh 3 (post-zd-23 landings)

- Date: 2026-11-04
- Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`
- Owns: `docs/jira/v26.11.3/RECEIPTS.md` + `docs/jira/v26.11.3/README.md`
  (rows only) + this receipt. No commits (coordinator owns git).
- Subject: zd-23's 107-census predates the latest landings — index every
  receipt on disk but unrowed.

## Census

`ls docs/jira/v26.11.3/receipts/ | wc -l` = 119 at lane start (before this
receipt), 120 with `zd-58-index-refresh3.md` on disk.

Brief candidates NOT on disk at this run (disclosed, not rowed):
zd-29, zd-32 (seal-reverify lives in `closure/`, not `receipts/` — the
index convention covers `receipts/` .md files only, so closure docs are
out of scope for this refresh, disclosed), zd-35, zd-36, zd-41, zd-42,
zd-43, zd-44, zd-48, zd-49, zd-52 through zd-58 (except this receipt).
zd-18's ledger is `closure/wave-ledger-v26.11.3.md`, not a receipts/ file —
not in scope, matching zd-23's treatment.

## Uncovered set (coverage gate, before this lane's edits)

Receipt .md files on disk vs mentions in RECEIPTS.md + README.md
(mention-level matching, per-stem grep):

```
UNCOVERED: zd-16-eof-sweep
UNCOVERED: zd-17-ambient-ggen
UNCOVERED: zd-27-repo-facts
UNCOVERED: zd-31-runbook-refresh
UNCOVERED: zd-38-advisory-ignores
UNCOVERED: zd-39-polyglot-sccache
UNCOVERED: zd-40-lsp-contract-graph
UNCOVERED: zd-45-receipt-seam
UNCOVERED: zd-46-zero-verb-audit
UNCOVERED: zd-47-relay-probe
UNCOVERED: zd-50-wo-drift
UNCOVERED: zd-51-r80
```

12 uncovered. No receipts landed mid-pass this run (coverage gate was clean
except the 12 on first post-edit run).

## Rows added

All 12 uncovered receipts rowed in BOTH `RECEIPTS.md` (new "Post-close
receipts (zd-58 index refresh, 2026-11-04)" section) and `README.md`
(post-close table tail). Standing words re-read from each receipt body on
disk this session in full, never from memory; UNSTATED where the body
carries none:

| Receipt | Standing (from body) |
|---|---|
| zd-16-eof-sweep.md | ALIVE on the verified surface (280 tracked .md files EOF-normalized, sweep re-run 0 candidates; uncommitted) |
| zd-17-ambient-ggen.md | ALIVE (ambient ggen 26.10.11 → 26.11.3, 4 verification commands exit 0) |
| zd-27-repo-facts.md | UNSTATED in body (the "Standing / residual drift" section names residual ttl-intro drift disclosed, not fixed; green gates carry the evidence) |
| zd-31-runbook-refresh.md | ALIVE for the verification ladder; PARTIAL for publish readiness itself (PR #824 unmerged, tags unminted, dirty trees) |
| zd-38-advisory-ignores.md | UNSTATED in body (before/after gates table carries the evidence; registered divergence carried for RUSTSEC-2026-0194/0195 per xc-8) |
| zd-39-polyglot-sccache.md | FIXED (uncommitted, per body Status); full CI job execution NOT run — first push-triggered run named the real gate, so the CI-path fix is UNVERIFIED by design (supersedes ZD-5's fix for cyberpunk-tv-platform.yml) |
| zd-40-lsp-contract-graph.md | ALIVE for `self-host/lsp-contract` (FM-LAW-015 satisfied with a kernel-admitted SBB); full ggen-lsp compile/test chain out of scope |
| zd-45-receipt-seam.md | ALIVE |
| zd-46-zero-verb-audit.md | ALIVE for the noun-surface claim (binary witnesses this session; 23 dead verbs rewired, phantom noun `mod` removed) |
| zd-47-relay-probe.md | ADMITTED (per body; all branches executed incl. shim-court + env-override witnesses) |
| zd-50-wo-drift.md | ALIVE for the landed surface (WO standing-word sweep exits clean ×1) |
| zd-51-r80.md | ALIVE (R80-12 CLOSED already-landed; R80-11 promoted UNKNOWN → PARTIAL_ALIVE, not rounded up; R80-6 scope-check only, L-effort for CARRY-03) |

Counts lines updated: RECEIPTS.md xd-17 paragraph gained the zd-58
re-census sentence; README.md post-close intro gained the zd-58 re-census
sentence.

## Coverage gate (after this lane's edits)

```
for f in receipts/*.md; do stem=$(basename "$f" .md);
  grep -qi "$stem" RECEIPTS.md README.md || echo "UNCOVERED: $stem"; done
```

Post-edit run: `UNCOVERED-END` with zero preceding lines — gate clean over
120 receipts on disk (119 + this receipt).

## Verification

- Pre-edit coverage gate: 12 UNCOVERED rows (listed above).
- Post-edit coverage gate: UNCOVERED = [] over 120 receipts on disk.
- Standing words sourced only from receipt bodies read this session in
  full (all 12 listed above).

## Standing

ALIVE for the index refresh (coverage gate re-run post-edit exits clean;
every receipt .md on disk at this run has a row in both canonical indexes).
