# ZD-23: Index Refresh 2 (post-XD-17 landings)

- Date: 2026-11-04
- Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`
- Owns: `docs/jira/v26.11.3/RECEIPTS.md` + `docs/jira/v26.11.3/README.md`
  (rows only) + this receipt. No commits (coordinator owns git).
- Subject: XD-17's transcribe predates the latest landings — index every
  receipt on disk but unrowed.

## Census

`ls docs/jira/v26.11.3/receipts/ | wc -l` = 103 at lane start (before this
receipt), 104 with `zd-23-index-refresh2.md` on disk. zd-15 / zd-16 /
zd-17 / zd-18 were NOT on disk at this run (candidates from the brief that
did not materialize — disclosed, not rowed). zd-21-shacl-syntax.md landed
mid-pass during this lane's verification run; zd-15-template-fmt.md and
zd-24-claude-markers.md landed during the final coverage re-run — 107
total at final re-census — each absorbed with its own row.

## Uncovered set (coverage gate, before this lane's edits)

Receipt .md files on disk vs mentions in RECEIPTS.md + README.md
(mention-level matching, per-stem grep):

```
UNCOVERED: ab-20-tokens-stress
UNCOVERED: xd-14-ea-graph-ci
UNCOVERED: xd-18-ci-residuals
UNCOVERED: zd-11-sync431
UNCOVERED: zd-14-ci-final
UNCOVERED: zd-5-sccache
UNCOVERED: zd-9-self-host-observation
```

(xd-18-ci-residuals was on disk but unrowed despite being named in the
brief as "if not already rowed" — it was not.)

## Rows added

All 7 uncovered receipts rowed in BOTH `RECEIPTS.md` (new "Post-close
receipts (zd-23 index refresh, 2026-11-04)" section) and `README.md`
(post-close table tail), plus the mid-pass landing zd-21-shacl-syntax.md
(absorbed at re-census). Standing words re-read from each receipt body on
disk, never from memory; UNSTATED where the body carries none:

| Receipt | Standing (from body) |
|---|---|
| ab-20-tokens-stress.md | UNSTATED (gate tables carry the evidence) |
| xd-14-ea-graph-ci.md | ALIVE 3 of 4 checks; PARTIAL self-host |
| xd-18-ci-residuals.md | UNSTATED (verification ladder carries the evidence) |
| zd-5-sccache.md | FIXED (uncommitted); CI path UNVERIFIED by design (job not executed) |
| zd-9-self-host-observation.md | ALIVE |
| zd-11-sync431.md | RESOLVED-DISCLOSED (pinned-by-law); ALIVE for the ruling |
| zd-14-ci-final.md | Fixes 1-3,5,7 ALIVE; Fix 4 ALIVE-as-verified; Fix 6 PARTIAL |
| zd-21-shacl-syntax.md | ALIVE (mid-pass landing, absorbed at re-census) |
| zd-15-template-fmt.md | ALIVE (mid-pass landing; closes ZD-14's fmt-gate flap) |
| zd-24-claude-markers.md | RESOLVED (uncommitted); ALIVE for owned surface; repo-facts.ttl gap BLOCKED for that lane, filed for coordinator |

Counts lines updated: RECEIPTS.md xd-17 paragraph gained the zd-23
re-census sentence; README.md post-close intro corrected (xd-14 was not on
disk at XD-17's run but landed later, and is rowed here) and gained the
re-census sentence.

## Coverage gate (after this lane's edits)

```
for f in receipts/*.md; do stem=$(basename "$f" .md);
  grep -qi "$stem" RECEIPTS.md README.md || echo "UNCOVERED: $stem"; done
```

Post-edit runs:
- Run A (after the initial 7 rows): `UNCOVERED: zd-21-shacl-syntax` —
  mid-pass landing, absorbed with its own row.
- Run B (after zd-21 row): `UNCOVERED: zd-15-template-fmt` +
  `UNCOVERED: zd-24-claude-markers` — landed during the final coverage
  re-run, absorbed with their own rows.
- Run C (final): UNCOVERED-END with zero preceding lines — gate clean.

## Verification

- Pre-edit coverage gate: 7 UNCOVERED rows (listed above).
- Post-edit coverage gate run C: UNCOVERED = [] over 107 receipts on disk
  (actual output above).
- Standing words sourced only from receipt bodies read this session in
  full (ab-20, xd-14, xd-18, zd-5, zd-9, zd-11, zd-14, zd-21, zd-15,
  zd-24).

## Standing

ALIVE for the index refresh (coverage gate re-run post-edit exits clean;
every receipt .md on disk at this run has a row in both canonical indexes).
