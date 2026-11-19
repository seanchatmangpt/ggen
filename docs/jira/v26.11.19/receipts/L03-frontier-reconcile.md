# WAVE-L03 Frontier / Reconcile JTBD
Standing: VERIFIED

## Identity

- Lane: 03
- Landed: `fd4f99bbd4971fb1ca5a0f3a3a2382aaba32261e` → `origin/preserve/v26.11.16-main`
- Date: 2026-11-19

## Consequence

- `ggen sjira frontier` loads embedded canonical pack authority index when
  `--authority` is absent (Elixir `Authority.index_from/1` parity with reconcile).
- CLI backlog identities B1-valid (`O1`/`O2`).
- Courts: CLI frontier 5 + reconcile 2 EXIT=0; kernel reconcile 16; shared
  frontier 4; mutual admit digest match.

## Replay

See `/Users/sac/srve/docs/jira/v26.11.19/receipts/L03-frontier-reconcile.md`.

## Standing

VERIFIED
