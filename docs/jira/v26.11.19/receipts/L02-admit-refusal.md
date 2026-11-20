# L02 Admit / Refusal JTBD (v26.11.19)
Standing: PASS

## Identity
- Lane: WAVE-L02
- Date: 2026-11-19
- JTBD: admission courts, refusal envelopes, `store.append` refusal-writes-nothing,
  ggen-sjira admit paths used by srve
- Surfaces:
  - `/Users/sac/ggen/crates/ggen-sjira` (Rust admit + unwitnessed refusal + projection)
  - `~/.local/bin/ggen` **26.11.5** (`ggen sjira admit`)
  - `/Users/sac/ggen_igniter` (Elixir admission-refusal court-kit)
  - `/Users/sac/fnox/pipeline` (store boundary + epistemic envelopes)
- Golden: `crates/ggen-sjira/tests/fixtures/golden_admit.json` (`SJ-ALIGN-001`)

## Acceptance criteria (located)

v26.11.19 INDEX WAVE-L02 + prior WP9/GAP16 admit law:

1. Real `ggen sjira admit` admits golden; typed refuse on malformed (EXIT≠0).
2. Chicago cargo: `work_order_admission_test`, `shared_fixture_admission_test`,
   `unwitnessed_refusal_test` (real fixtures; no mocked admit).
3. Igniter `admit_work_order/1` refusal KAT court flips (live refuse + shadow twin).
4. fnox `store.append` refuses before write (empty store) + REFUSED envelope shape.

## Consequence

All owned Chicago courts EXIT=0 (refuse path EXIT=1 as typed success).
No HITL. No Gate2 / wave-COMPLETE claim — JTBD VERIFIED only.

## Replay (2026-11-19)

| Check | Command | Exit | Result |
|---|---|---|---|
| PATH ggen admit golden | `ggen sjira admit …/golden_admit.json --format json` | **0** | `admitted: true`; digest `sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184` |
| PATH ggen refuse standing | `ggen sjira admit` malformed `standing=NOT_A_STANDING` | **1** | `REFUSED:SEMANTIC_JIRA` / `InvalidStanding` + remediation |
| cargo work_order_admission | `cargo test -p ggen-sjira --test work_order_admission_test` | **0** | 15 passed |
| cargo shared_fixture_admission | `cargo test -p ggen-sjira --test shared_fixture_admission_test` | **0** | 1 passed |
| cargo unwitnessed_refusal | `cargo test -p ggen-sjira --test unwitnessed_refusal_test` | **0** | 4 passed |
| cargo projection (admit-adjacent) | `cargo test -p ggen-sjira --test projection_test` | **0** | 13 passed (serial + parallel) |
| igniter refusal court | `mix test test/ggen_igniter_semantic_jira_admission_refusal_court_test.exs --trace` | **0** | **40 passed** |
| fnox store+envelope | AGENTS.md `uv run --with …` (+`httpx2`) pytest `test_store_boundary_closed` + `test_store_refusal_order` + `test_envelope` + `test_a2a_envelope_wire` | **0** | **26 passed** |

Logs (this dir): `ggen-admit-binary.log`, `cargo-admit-courts.log`,
`cargo-projection-parallel.log`, `igniter-admission-refusal-court.log`,
`L02-fnox-store-envelope.log`.

## Digests

Golden definition digest (Rust CLI admit):
`sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`

## SHAs at VERIFIED

| Repo | HEAD |
|---|---|
| ggen | `31ba588e1424cfe2275e2f3fbb51fe538fb7718f` (preserve tip; GH013 blocks main) |
| ggen_igniter | `360f6340dabf0905d1b17ee040d10f3f39bd2161` |
| fnox | `0024eb84f2d9eaa68a4e1e67618e811815579692` |
| srve | `adae83da86d343d68f0cb69a693a3234007e7c10` (this receipt) |

## Remaining gaps (not L02 blockers)

1. **GH013** — ggen `main` push blocked; preserve/`v26.11.16-main` is the land surface.
2. **Live sjira packs / loop execute** — operator env (L04); dry admit is green.
3. Wave COMPLETE still waits L15/L20 rollup + honest BLOCKED set (Tony/live-smoke/etc.).

## Standing

**VERIFIED** — Chicago evidence above; refuse-path EXIT=1 is typed success, not a fail.
