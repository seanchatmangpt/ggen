# brand.toml exact-tip7 — ALIVE push-first honesty (v27.1.1)

```text
tip_base=675c614051c5c55eb96ee9454d290b1a646f189d
live_crown_at_ledger=6883aab70f7bf6d5ba5c5316983211127c28dc1f
cited_tip_at_kickoff=c4e757a14c91c6512438a8a94bbde304b0a6e921
branch=cursor/v2711-brand-exact-tip7-bc54
bc=bc-7a87dbfb-2ef5-5341-98ea-ba0a6e436091
court=brand_toml_completeness_court + brand_toml_rhythm_court
budget_s=1500
exit=0
standing=ALIVE
completeness=5
rhythm=12
prior_tip6_partial=552c85f29 EXIT137
lineage_alive_tip=77eb46fd0 / sealed 2685119bd
prior_probe_tip7=EXIT124 cold /tmp/gate1-tip7-brand (PARTIAL superseded)
exact_tip_reconfirm=ALIVE (EXIT:0 @ 675c614051c5c55eb96ee9454d290b1a646f189d)
crown_absorb=ABSORBED@6883aab70f7bf6d5ba5c5316983211127c28dc1f→2e0c4570d5dd1b2eb4737e5b46fe86f2c0d24d9d←82e62dc17a018130edf1f279f08d42b5842c782f
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-EXACT-TIP7-STANDING` |
| Branch | `cursor/v2711-brand-exact-tip7-bc54` |
| Agent | `bc-7a87dbfb-2ef5-5341-98ea-ba0a6e436091` |
| Tip base (cargo) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `675c614051c5c55eb96ee9454d290b1a646f189d` |
| Live crown at ledger | `6883aab70f7bf6d5ba5c5316983211127c28dc1f` |
| Court blobs | completeness `a07477c6ce9b110982d8217079d5a2a44e3dffa4`; rhythm `4843ed9f4237096690521e8e14f52f937295bcaa` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/tip7-serial cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/tip7-serial` (serial warm after peer shui+uilayout tip7 ALIVE; exclusive graph=0) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:0** — completeness **5** + rhythm **12** @ `2026-11-22T20:02:46Z`→`2026-11-22T20:04:43Z` (~2m wall after warm compile) |
| Prior cold tip7 probe | **EXIT:124** @ `/tmp/gate1-tip7-brand` (timeout mid-compile under peer thrash) — superseded by this warm serial EXIT:**0** |
| Log / status | `/tmp/cursor/brand-exact-tip7b.log` · `/tmp/cursor/brand-exact-tip7b.status` → `BRAND_EXIT=0` |
| Lineage tip-cargo ALIVE | **held** — completeness **5** + rhythm **12**; EXIT 0 @ `77eb46fd0` / sealed `2685119bd` |
| Prior tip6 | `cursor/v2711-brand-exact-tip6-bc54` — PARTIAL EXIT:**137** — [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md) |
| Production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) — unchanged |
| Crown absorb | **ABSORBED** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` @ pre-absorb `6883aab70f7bf6d5ba5c5316983211127c28dc1f` → absorb `2e0c4570d5dd1b2eb4737e5b46fe86f2c0d24d9d` (side `82e62dc17a018130edf1f279f08d42b5842c782f`) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 push-first honesty: exclusive tip-cargo obtained EXIT **0** with completeness **5** + rhythm **12** on tip `675c614051c5c55eb96ee9454d290b1a646f189d` → standing **ALIVE (exact tip-cargo)**. Do **not** invent production theme standing, photolithography path crown `ALIVE`, or Foundry `ALIVE` from this fence alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ Foundry `ALIVE`.

---

## Why ALIVE

| Check | Result |
|-------|--------|
| Tip carries completeness (5) + rhythm (12) courts | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **YES** — EXIT **0**; completeness **5** + rhythm **12** |
| Serial `/tmp/tip7-serial` / jobs=1 / test-threads=1 | **held** |
| Lineage tip-cargo ALIVE @ `77eb46fd0` / `2685119bd` | **held** |

### Tip-cargo evidence (verbatim)

```text
running 5 tests
test complete_oem_fixtures_encode_grid_touch_gap_and_fluid ... ok
test missing_grid_base_is_refused_incomplete ... ok
test missing_grid_half_is_refused_incomplete ... ok
test missing_inter_target_gap_is_refused_incomplete ... ok
test missing_touch_floor_is_refused_incomplete ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.02s

running 12 tests
test cindycat_fixture_encodes_patronage_messaging_contract ... ok
test clamp_slope_mismatch_is_refused ... ok
test fourth_watch_and_cindycat_fixtures_are_admitted ... ok
test fourth_watch_fixture_admits_without_patronage_slots ... ok
test inter_target_gap_under_8_is_refused ... ok
test inverted_clamp_min_gt_max_is_refused ... ok
test non_cqi_preferred_rhythm_is_refused ... ok
test non_grid_spacing_is_refused ... ok
test non_positive_clamp_slope_is_refused ... ok
test oem_fixtures_carry_dtcg_primitive_space_from_rhythm ... ok
test off_grid_fluid_min_is_refused ... ok
test touch_floor_under_44_is_refused ... ok

test result: ok. 12 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.03s
BRAND_EXIT=0
```

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 675c614051c5c55eb96ee9454d290b1a646f189d
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/tip7-serial \
  cargo test -p ggen-graph \
  --test brand_toml_completeness_court \
  --test brand_toml_rhythm_court \
  --locked --config build.jobs=1 -- --test-threads=1
# expect: completeness 5 + rhythm 12; EXIT 0 → tip-subject ALIVE (replay)
```

---

## Compact return

```text
tip_base=675c614051c5c55eb96ee9454d290b1a646f189d
branch=cursor/v2711-brand-exact-tip7-bc54
bc=bc-7a87dbfb
EXIT=0
standing=ALIVE
tests=completeness_5+rhythm_12
target=/tmp/tip7-serial
lineage_alive=77eb46fd0/2685119bd
crown_absorb=ABSORBED@6883aab70f7bf6d5ba5c5316983211127c28dc1f→2e0c4570d5dd1b2eb4737e5b46fe86f2c0d24d9d←82e62dc17a018130edf1f279f08d42b5842c782f ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
