# WAVE-L03 Frontier / Reconcile JTBD
Standing: PASS

## Identity

- Lane: 03
- Code land: `fd4f99bbd4971fb1ca5a0f3a3a2382aaba32261e` → `origin/preserve/v26.11.16-main`
- Tip (this receipt demote): recorded after land; see `git rev-parse HEAD`
- Date: 2026-11-19

## Consequence

- `ggen sjira frontier` loads embedded canonical pack authority index when
  `--authority` is absent (Elixir `Authority.index_from/1` parity with reconcile).
- CLI backlog identities B1-valid (`O1`/`O2`).
- Courts: CLI frontier 5 + reconcile 2 EXIT=0; kernel reconcile 16; shared
  frontier 4; mutual admit digest match; Elixir frontier shape 2 EXIT=0.

## Replay (coordinator re-run 2026-11-19)

| Court | EXIT | Count |
|---|---|---|
| `cargo test -p ggen-sjira --test reconcile_test` | 0 | 16 |
| `cargo test -p ggen-sjira --test shared_fixture_frontier_test` | 0 | 4 |
| `cargo test -p ggen-sjira --test shared_fixture_admission_test` | 0 | 1 |
| `cargo test -p ggen-sjira --test work_order_admission_test` | 0 | 15 |
| `cargo test -p ggen-sjira --test projection_test` | 0 | 13 |
| `cargo test -p ggen-cli-lib --test cli_sjira_test frontier_` | 0 | 5 |
| `cargo test -p ggen-cli-lib --test cli_sjira_test reconcile_` | 0 | 2 |
| `mix test …frontier_shape_court_test.exs` (ggen_igniter) | 0 | 2 |
| `just sjira-mutual-admit` | 0 | MUTUAL_ADMIT_OK |
| `target/debug/ggen sjira admit …/golden_admit.json` | 0 | digest match |

Digest: `sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`

Canonical mirror: `/Users/sac/srve/docs/jira/v26.11.19/receipts/L03-frontier-reconcile.md`.

## Standing

PASS — closed vocab (not VERIFIED/ALIVE). GH013: land via preserve only.
