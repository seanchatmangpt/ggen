# AF-7 Receipt-Path Drift Reconcile

Standing: reconciled — all 5 canonical copies present; pointer line confirmed on all 5 originals

Date: 2026-11-03. Lane: AF-7 (implementation). No git operations.

## Drift

Five v26.11.3 wave receipts (ab-*) landed under
`/Users/sac/cgap/docs/jira/v26.11.2/lanes/` instead of the canonical
`docs/jira/v26.11.3/receipts/` directory — the lanes followed cgap's
lanes/ convention.

## Reconciled receipts

| Receipt | Drifted path (pointer added) | Canonical path (copy) |
|---|---|---|
| AB-2 | cgap/docs/jira/v26.11.2/lanes/ab-2-fips202.md | cgap/docs/jira/v26.11.3/receipts/ab-2-fips202.md |
| AB-3 | cgap/docs/jira/v26.11.2/lanes/ab-3-chacha.md | cgap/docs/jira/v26.11.3/receipts/ab-3-chacha.md |
| AB-10 | cgap/docs/jira/v26.11.2/lanes/ab-10-writerv-stress.md | cgap/docs/jira/v26.11.3/receipts/ab-10-writerv-stress.md |
| AB-12 | cgap/docs/jira/v26.11.2/lanes/ab-12-blake3-stress.md | cgap/docs/jira/v26.11.3/receipts/ab-12-blake3-stress.md |
| AB-15 | cgap/docs/jira/v26.11.2/lanes/ab-15-ffi-stress.md | cgap/docs/jira/v26.11.3/receipts/ab-15-ffi-stress.md |

All paths relative to /Users/sac/cgap/. AB-15 was found by the stray sweep
(the task named 4; the sweep found a 5th, ab-15-ffi-stress.md, under
v26.11.2/lanes/ and reconciled it identically).

## Pointer lines

Each drifted original in v26.11.2/lanes/ now carries as its first line:

```
Canonical location: docs/jira/v26.11.3/receipts/<name>
```

## Convention ruling going forward

- `docs/jira/v26.11.3/receipts/` is canonical for this wave's receipts.
- `docs/jira/v26.11.2/lanes/` is for lane working docs, not receipts.
- Future v26.11.3 wave receipts land in v26.11.3/receipts/ directly.

## Verification

- Copies byte-exact: sizes match pre-edit originals (4156, 4036, 3562,
  4761, 7823 bytes).
- Pointer line confirmed via head -2 on all 5 originals.
- Post-edit sweep for ab-*.md: only the 5 reconciled files in lanes/,
  all 5 canonical copies present in v26.11.3/receipts/ (9 ab-* total,
  4 pre-existing canonical + 5 newly copied).
