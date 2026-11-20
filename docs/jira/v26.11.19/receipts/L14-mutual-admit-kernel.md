# L14 mutual-admit + KernelDifferential / WP9
Standing: VERIFIED

## Identity
- Lane: WAVE-L14
- Date: 2026-11-19
- JTBD: CF-WP9 KernelDifferential Elixir↔Rust mutual admit (re-proved for v26.11.19)
- Surfaces:
  - `/Users/sac/ggen` (`just sjira-mutual-admit` + focused `ggen-sjira` cargo)
  - `/Users/sac/ggen_igniter` (Elixir `admit_work_order/1`)
  - `/Users/sac/fnox` (`pipeline/tests/test_kernel_differential.py`)
- Golden: `/Users/sac/ggen/crates/ggen-sjira/tests/fixtures/golden_admit.json` (`SJ-ALIGN-001`)

## Consequence
One committed golden admits on both kernels with matching definition digests.
WP9 focused cargo courts (projection / shared_fixture / work_order) EXIT=0.
fnox KernelDifferential Chicago pytest EXIT=0. No HITL. No inherited VERIFIED.

## Replay (2026-11-19)

| Check | Command | Exit | Result |
|---|---|---|---|
| Mutual admit | `cd /Users/sac/ggen && just sjira-mutual-admit` | 0 | `MUTUAL_ADMIT_OK rust=ok elixir=ok` |
| Elixir digest | (inside mutual-admit) | 0 | `ELIXIR_ADMIT_OK SJ-ALIGN-001 sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184` |
| WP9 cargo | `cargo test -p ggen-sjira --test projection_test --test work_order_admission_test --test shared_fixture_admission_test` | 0 | projection 13 + shared 1 + work_order 15 |
| KernelDifferential | `pytest -q pipeline/tests/test_kernel_differential.py` (fnox AGENTS.md `--with` set) | 0 | 3 passed |

Logs: `L14-sjira-mutual-admit.log`, `L14-cargo-wp9-rerun.log`, `L14-fnox-kernel-differential.log`.

## Digests (byte-identical)

`sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`

## Fixture note (projection_test `order()`)

Current on-disk law (skeptic anti-vacuity): non-empty `falsifiers` + seeded
`projections/c1.unbound.rq`; `path_scope: ["lib/"]`. Prior v26.11.18 empty-falsifiers
claim superseded by this re-proof; courts still EXIT=0.

## SHAs

| Repo | HEAD at L14 close |
|---|---|
| ggen | `d2d4988e6fb9575831a21decbea500ef58d28d19` (= `origin/preserve/v26.11.16-main`) |
| ggen_igniter | `12e7be2772f3330cf8eff9331f64c70e72e83524` |
| fnox | `a88e3922c1a5d25966c6f17bc19284706e2e3031` |
| srve | `4a22a77797324bafd4239f86d9d4a1a5ee962359` |

Courts that flipped VERIFIED ran EXIT=0 earlier this session (HEADs through
`505dcc9da`…`31ba588e`); tip matches preserve.

## GH013 / land

| Push | Exit | Note |
|---|---|---|
| `git push origin HEAD:preserve/v26.11.16-main` | 0 | `Everything up-to-date` (twice) |
| `git push origin HEAD:main` | non-0 | pre-push Full Tier: Check timed out; unit-tests hit disk full — GH013/main residual |

## Standing

VERIFIED — mutual-admit + WP9 cargo + KernelDifferential all EXIT=0 with matching
digests. Version COMPLETE is out of this lane (other WAVE rows remain).
