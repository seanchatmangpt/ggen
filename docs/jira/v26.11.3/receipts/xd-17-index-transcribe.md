# XD-17 — Canonical Index Transcribe (XD-3 successor)

Date: 2026-11-04. Lane: XD-17 (index-only). Repo: /Users/sac/ggen, branch
v26-11-3-wave. Zero src/test edits, zero git commands, no commit. Files
touched (only): `docs/jira/v26.11.3/README.md`,
`docs/jira/v26.11.3/RECEIPTS.md`, this receipt.

## Task

XD-3 (receipts/xd-3-index-refresh.md) found the 12 post-seal ggen receipts
(sx-1, af-5, af-7, ab-13, ab-14, ga-11, ga-12, af-42, gd-2, xc-7, xc-8,
xd-2) carried rows only in the cl-close-2.md addendum — README.md and
RECEIPTS.md carried none. This lane transcribed those rows into both
canonical index surfaces, plus rows for receipts landed after XD-3.

## Rows written

- README.md new section "Post-close receipts (xd-17 transcribe,
  2026-11-04)": 18 ggen rows (the 12 XD-3 rows + xd-8-key-mode-gate.md +
  xd-10-publish-blockers.md + ds-8.md found uncovered by this lane's own
  coverage check — full DS-8 lane receipt, distinct from
  ds-8-cli-skeleton.md, standing UNSTATED in body) + xd-13-fmt-sweep.md,
  xd-11-fmlaw015.md, xd-15-tcps-lock.md each landed mid-pass (absorbed at
  re-census; xd-11 PASS standing, PUB-1 blocking row resolvable to CLOSED;
  xd-15 PARTIAL_ALIVE) in the existing new-wave row format
  (Lane | Receipt | Standing | Gate (exit record)), each with the standing
  word and exit record from the receipt body on disk.
- README.md subsection "Cross-repo receipts (pointer-only)": 5 pointer rows
  (cgap bk-1-takeover, af-37-p99-torture, rx-2-conformance, gd-1-dod-pack;
  arcus xd-4-govulncheck) following the pointer convention the cl-close-2
  addendum already used (Receipt | Host | Standing | Exit record) — the
  convention exists, so none was invented.
- RECEIPTS.md new section "Post-close receipts (xd-17 transcribe)": same 18
  ggen rows in RECEIPTS.md's own row format (Lane | Receipt | Standing |
  Exit record) + 5 pointer-only cross-repo rows.
- xd-14, xd-15: NOT on disk at this lane's run (ls'd 2026-11-04) — no rows
  written, not invented.

## Standing words (source)

Re-read from the cl-close-2.md addendum rows, which XD-3 read from each
receipt body on disk this session (2026-11-04); xd-8 and xd-10 standing
words read directly from their receipt bodies this session. UNSTATED used
where a body carries no standing word (sx-1, af-7, ab-13, xc-8, xc-7),
never rounded up.

## Counts (re-read at use time)

- `ls docs/jira/v26.11.3/receipts/ | wc -l` at lane start: **92**; 93 with
  this receipt on disk (self-subject); 94→95→96 at successive verification
  re-censuses as xd-13-fmt-sweep.md, xd-11-fmlaw015.md, xd-15-tcps-lock.md
  each landed mid-pass (each absorbed with its own row, disclosed in-row).
  Every dir entry is a receipt .md (no _LANES.md or subdirs in this dir —
  the earlier lane-start assumption that _LANES.md was counted was wrong;
  re-read at use time).
- RECEIPTS.md row reconciliation (grep-verified at gate): 44 lines starting
  `| ` = 6 header rows + **38 data rows** (15 pre-existing lane rows: 10
  WO landed + 2 orphan-recovery + 1 cl-close self-row + 2 open-WO; + 18
  post-close ggen rows + 5 cross-repo pointer rows). RECEIPTS.md's
  per-lane tables deliberately do not aim at 1-row-per-receipt — the full
  per-receipt register is README.md's tables; the reconciliation to the
  dir listing is the pooled coverage gate below (96/96), not row-count
  equality.

## Verification (real commands, this session)

- Count: `ls docs/jira/v26.11.3/receipts/ | wc -l` → 96 at final gate run.
- Coverage: mention-level pooled gate (every receipts/*.md stem mentioned
  in README.md or RECEIPTS.md); first strict-`.md`-citation pass flagged
  k6-self-dogfood and r80-02..r80-12, which are cited without the `.md`
  extension (lane table + R80 reconciliation mapping line) — script
  artifacts, resolved by mention-level matching, not by weakening intent.
- Dangling receipts citations: zero (remaining non-resolving `.md` names
  are doc links and foreign-host receipts, not receipts/ files).

```
$ python3 /tmp/xd17_gate2.py
receipt .md files on disk: 96
UNCOVERED (mention-level, pooled README+RECEIPTS): []
exit=0
$ ls receipts/ | wc -l
96
$ grep -c '^| ' RECEIPTS.md   # 6 header + 38 data rows
44
$ grep -c '^|---' RECEIPTS.md
6
```

## Standing

ALIVE for the transcribe scope: every row written this session from receipt
bodies on disk (directly or via the cl-close-2 addendum rows this session),
all counts from real `ls | wc -l`, absent receipts recorded absent.
Falsifier: any row's standing word disagrees with its receipt body; any
count disagrees with `ls | wc -l`; a receipt on disk with no index row or
pointer.

## See Also

- receipts/xd-3-index-refresh.md (the drift finding this lane closes)
- receipts/cl-close-2.md (addendum rows transcribed)
