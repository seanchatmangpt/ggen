# Gate1 remaining falsifiers ledger (v27.1.1)

```text
crown_tip=59b9fae1cfab7ebf263f98068aa76cf42907d7c9
branch=cursor/v2711-gate1-remaining-retip-bc54
utc=2026-11-22T18:55Z
cargo=none (docs-only retip after ALIVE trio absorb)
ManagePullRequest=not_used
invented_ALIVE=none
live_tip_ALIVE_invented=none
Foundry_ALIVE_invented=none
```

| Field | Value |
|-------|-------|
| Document ID | `GATE1-REMAINING-FALSIFIERS` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`59b9fae1cfab7ebf263f98068aa76cf42907d7c9`** (`59b9fae1c`; ⊇ brand/Taffy/uilayout exact tip-cargo ALIVE absorb @ `2685119bd`) |
| Sidebranch | `cursor/v2711-gate1-remaining-retip-bc54` |
| Cargo / tip-cargo | **NOT RUN** (docs-only retip; trio already EXIT:**0** @ `2685119bd`; shui tip7 EXIT0 **in flight**) |
| Spec bind | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §6–§10 / §17–§18; companions [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md), [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** This ledger is a docs-only pin of Gate1 falsifier standings at live crown
`59b9fae1c` after the ALIVE trio absorb. It does **not** run cargo. Exact tip-cargo
`ALIVE` rows below are sealed at cargo tip **`2685119bd`** — that is **≠** a live-tip
(`59b9fae1c`) reconfirm unless a later EXIT:**0** on the live tip is recorded.
Never invent tip-subject / live-tip `ALIVE` from blob identity, lineage alone, or
this markdown file. Never invent Foundry / path-crown / production-theme `ALIVE`.

---

## Remaining falsifiers

| falsifier | standing | evidence | exact next |
|-----------|----------|----------|------------|
| Truex `shui:` product admit (D3) | **ALIVE** | Sibling Chicago admit-court **6/6**; EXIT **0** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` (merged `b4672131`; [truex#6](https://github.com/seanchatmangpt/truex/pull/6)); reconfirm [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) / [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md). ≠ Foundry / path-crown `ALIVE`. | `cd /tmp/truex && git rev-parse HEAD` → `68d2d233…`; `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** @ cargo tip `c4e757a14` / crown land `3ae705d1c+` | `brand_toml_hotswap_court` **6 passed; EXIT:0** @ `c4e757a14` (`/tmp/only-fafc690f-tip3-courts`); fence ceiling **PARTIAL_ALIVE**; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md) | sealed — do **not** re-run under tip7 thrash unless sources drift; replay only if needed: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| brand.toml production theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Hot-swap / rhythm / fluid `ALIVE` ≠ production `ALIVE`. | Gate2+ / OOR path — production theme stays **UNSUPPORTED** until all production falsifiers exit **0** |
| ggen `shui_public_prefix_court` (fence) | **PARTIAL_ALIVE** (EXIT0 **in flight** — tip7) | Tip6 probe EXIT **137** @ tip_base `c661d228a` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md). Lineage tip-cargo **ALIVE** **4**/EXIT:**0** @ `008a81d21` (blob `3cda065f`) **held** — not tip-subject substitute. Tip7 serial cargo compiling under `/tmp/gate1-tip7-shui` — **do not invent EXIT:0**. | tip7 in flight: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** (pending) |
| Taffy AOT preflight (`taffy_aot_preflight_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**7**) | EXIT:**0**; **7 passed** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719` onto crown; [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md). **≠ live-tip (`59b9fae1c`) reconfirm** — sealed at `2685119bd` only. Photolithography / Foundry remain **PARTIAL** / not promoted. | sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| ui_layout digest (`ui_layout_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**11**) | EXIT:**0**; **11 passed** @ tip `2685119bd` (`/tmp/ggen-uilayout-exact-retip5`); land `59b9fae1c`; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md). **≠ live-tip (`59b9fae1c`) reconfirm** — sealed at `2685119bd` only. Sibling `ui-layout-court.ts` **UNSUPPORTED**. | sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-retip5 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** @ `2685119bd` (**5+12**) | EXIT:**0**; completeness **5** + rhythm **12** @ tip `2685119bd` (`/tmp/ggen-brand-exact-tip`); land `bf3165322`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). **≠ live-tip (`59b9fae1c`) reconfirm** — sealed at `2685119bd` only. Full DTCG/CASTLE inventory **UNSUPPORTED**. Production theme **UNSUPPORTED**. | sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |

---

## Sequencing law (post ALIVE trio)

1. **Do not** launch parallel Gate1 `ggen-graph` tip-cargo under shared-pod thrash.
2. Trio (brand completeness+rhythm / Taffy / uilayout) is **ALIVE (exact tip-cargo)** at `2685119bd` — **≠** live-tip reconfirm; tip7 does **not** need to re-prove them unless sources drift.
3. Hot-swap exact tip-cargo remains **ALIVE** @ `c4e757a14` — tip7 does **not** re-prove unless sources drift.
4. **shui** fence is the remaining Gate1 tip-cargo falsifier: **PARTIAL_ALIVE**, tip7 EXIT0 **in flight** — do not invent EXIT:**0** / tip-subject `ALIVE` until status file says `0`.
5. Truex product **ALIVE** is sibling vitest — outside tip7 cargo thrash; replay only if disputed.
6. Production theme remains **UNSUPPORTED**. Foundry / path-crown remain **not ALIVE**.

---

## Compact return

```text
Truex=ALIVE@68d2d233
hotswap=ALIVE(exact tip-cargo)@c4e757a14 prod=UNSUPPORTED
shui=PARTIAL tip7_EXIT0_in_flight
Taffy=ALIVE(exact tip-cargo)@2685119bd (7) ≠live-tip-reconfirm
uilayout=ALIVE(exact tip-cargo)@2685119bd (11) ≠live-tip-reconfirm
brand_completeness+rhythm=ALIVE(exact tip-cargo)@2685119bd (5+12) ≠live-tip-reconfirm
cargo=NOT_RUN docs-only-retip
crown_tip=59b9fae1c
side=cursor/v2711-gate1-remaining-retip-bc54
```
