# WP9 KernelDifferential — Elixir ↔ Rust mutual admit
Standing: VERIFIED

Repo: `/Users/sac/ggen` (`crates/ggen-sjira`). Elixir SoT:
`/Users/sac/ggen_igniter`. Date: 2026-11-19. No git ops in this lane.

Supersedes the PARTIAL standing from the WP9 mutual-admit verifier pass once
`projection_test` falsifiers were restored to `[]` with `path_scope: ["lib/"]`.

## Golden

`crates/ggen-sjira/tests/fixtures/golden_admit.json` (`SJ-ALIGN-001`).

## Replay

| Command | EXIT | Result |
|---|---|---|
| `cargo test -p ggen-sjira --test projection_test` | 0 | 12 passed, 0 failed |
| `cargo test -p ggen-sjira --test shared_fixture_admission_test` | 0 | 1 passed, 0 failed |
| `cargo test -p ggen-sjira --test work_order_admission_test` | 0 | 15 passed, 0 failed |
| Elixir `admit_work_order(golden)` | 0 | `ELIXIR_ADMIT_OK "SJ-ALIGN-001"` |
| Local `sjira admit` golden | 0 | `admitted: true` |

## Digests (byte-identical)

`sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`
(Elixir and Rust).

## Fixture law (projection_test `order()`)

- `falsifiers: []`
- `path_scope: ["lib/"]`

## Standing

VERIFIED.
