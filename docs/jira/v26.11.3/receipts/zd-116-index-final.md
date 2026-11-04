# ZD-116 — final coverage pass (index refresh 5)

- **Date**: 2026-11-04
- **Repo/branch**: `/Users/sac/ggen`, `v26-11-3-wave` @ 468291ae3 (no git commands run; coordinator owns git)
- **Files owned**: `docs/jira/v26.11.3/RECEIPTS.md`, `docs/jira/v26.11.3/README.md`, this receipt
- **Subject**: ggen @ v26-11-3-wave tip (uncommitted working tree)

## Census

`ls docs/jira/v26.11.3/receipts/ | wc -l` = **139** receipt .md files on
disk before this lane's own receipt, **140** with zd-116-index-final.md
on disk (previous census: zb-24's 130-before / 131-with);
zd-112-terminal-gates.md and zd-118-tag-sweep.md landed mid-pass during
this lane's verification runs — **142** at final re-census — each
absorbed with its own row.

## Coverage gate

Mention-level gate over receipt stems vs `RECEIPTS.md` + `README.md`:

```
for f in receipts/*.md; do s=$(basename "$f" .md); \
  grep -qF "$s" RECEIPTS.md README.md || echo "UNCOVERED: $s"; done
```

At lane start the gate exited 1 with 8 uncovered stems (zb-21, zd-60,
zd-88, zd-90, zd-101, zd-106, zd-108, zd-110). After this lane's rows
in both index files plus the zb-24 self-row in RECEIPTS.md, the gate
exits with **UNCOVERED []** (exit 0). zb-26/zb-27 are NOT on disk —
nothing to row (never a row for an absent file). zd-112 and zd-118
landed mid-pass and are absorbed with their own rows.

## Rows added (standing re-read from bodies on disk, never memory)

12 rows in RECEIPTS.md's new zd-116 section (zb-24 self-row + the 8
stems above + the zd-112/zd-118 mid-pass absorbs + this self-row), the
same 12 rows in README.md's post-close table. Standings: ZB-21
PARTIAL; ZD-60 ALIVE; ZD-88 UNSAFE-verdict (UNSTATED as a
standing-vocabulary word); ZD-90 UNSTATED; ZD-101 UNSTATED; ZD-106
ALIVE; ZD-108 ALIVE; ZD-110 UNSTATED; ZD-112 UNSAFE-verdict (UNSTATED
as a standing-vocabulary word); ZD-118 ALIVE; ZB-24 ALIVE (self-row);
ZD-116 ALIVE (this self-row).

## Method notes

- Standing words re-read from each receipt body on disk; UNSTATED
  where the body carries none.
- No commits made, per contract (coordinator sweeps).
- Non-vacuity: the gate fired at lane start (8 stems) and was re-run
  clean only after the rows landed.
