# Gate1 remaining falsifiers ledger (v27.1.1)

```text
crown_tip=cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3
branch=cursor/v2711-gate1-shui-alive-close-bc54
utc=2026-11-22T19:07Z
cargo=none (docs-only retip after shui fence EXIT0 ALIVE 4/4 absorb @ edf2ec22a)
ManagePullRequest=not_used
invented_ALIVE=none
live_tip_ALIVE_invented=none
Foundry_ALIVE_invented=none
Foundry=BLOCKED_HITL
tip_cargo_neq_live_tip_reconfirm=held
```

| Field | Value |
|-------|-------|
| Document ID | `GATE1-REMAINING-FALSIFIERS` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`cfd93c1cbd2cc95c13b7c325bf88a59f22c988f3`** (`cfd93c1cb`; ⊇ shui exact tip-cargo ALIVE absorb @ `edf2ec22a` ← cargo `a4b3d3fb4`; ⊇ ui_layout absorb `6f0f64611`; ⊇ brand/Taffy/uilayout trio @ `2685119bd`) |
| Sidebranch | `cursor/v2711-gate1-shui-alive-close-bc54` |
| Cargo / tip-cargo | **NOT RUN** (docs-only close retip; all Gate1 tip-cargo falsifiers already EXIT:**0** sealed; **≠** live-tip reconfirm) |
| Spec bind | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §6–§10 / §17–§18; companions [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md), [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md), [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md), [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** This ledger is a docs-only pin of Gate1 falsifier standings at live crown
`cfd93c1cb` after the shui fence EXIT:**0** ALIVE absorb (`edf2ec22a`). It does
**not** run cargo. Exact tip-cargo `ALIVE` rows below are sealed at their cargo tips
(`a4b3d3fb4` / `2685119bd` / `c4e757a14` / …) — that is **≠** a live-tip
(`cfd93c1cb`) reconfirm unless a later EXIT:**0** on the live tip is recorded.
Never invent tip-subject / live-tip `ALIVE` from blob identity, lineage alone, or
this markdown file. Never invent Foundry / path-crown / production-theme `ALIVE`.
Foundry remains **BLOCKED_HITL**.

---

## Remaining falsifiers

| falsifier | standing | evidence | exact next |
|-----------|----------|----------|------------|
| Truex `shui:` product admit (D3) | **ALIVE** | Sibling Chicago admit-court **6/6**; EXIT **0** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` (merged `b4672131`; [truex#6](https://github.com/seanchatmangpt/truex/pull/6)); reconfirm [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) / [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md). ≠ Foundry / path-crown `ALIVE`. | `cd /tmp/truex && git rev-parse HEAD` → `68d2d233…`; `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** @ cargo tip `c4e757a14` / crown land `3ae705d1c+` | `brand_toml_hotswap_court` **6 passed; EXIT:0** @ `c4e757a14` (`/tmp/only-fafc690f-tip3-courts`); fence ceiling **PARTIAL_ALIVE**; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md). **≠ live-tip (`cfd93c1cb`) reconfirm**. | sealed — do **not** re-run under thrash unless sources drift; replay only if needed: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| brand.toml production theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Hot-swap / rhythm / fluid `ALIVE` ≠ production `ALIVE`. | Gate2+ / OOR path — production theme stays **UNSUPPORTED** until all production falsifiers exit **0** |
| ggen `shui_public_prefix_court` (fence) | **ALIVE (exact tip-cargo)** @ `a4b3d3fb4` (**4**) | EXIT:**0**; **4 passed** @ tip `a4b3d3fb4` (`/tmp/gate1-shui-alive`); tip-wins absorb `edf2ec22a` onto crown; court blob `3cda065f`; [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md). Supersedes tip6/tip7 PARTIAL EXIT137. **≠ live-tip (`cfd93c1cb`) reconfirm** — sealed at `a4b3d3fb4` only. ≠ Foundry `ALIVE`. | sealed at `a4b3d3fb4` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-shui-alive cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** |
| Taffy AOT preflight (`taffy_aot_preflight_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**7**) | EXIT:**0**; **7 passed** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719` onto crown; tip7 reconfirm absorb `4ebcb9d46`; [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md). **≠ live-tip (`cfd93c1cb`) reconfirm** — sealed at `2685119bd` only. Photolithography / Foundry remain **PARTIAL** / **BLOCKED_HITL**. | sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| ui_layout digest (`ui_layout_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**11**; also `376d0d283`) | EXIT:**0**; **11 passed** @ tip `2685119bd` / reconfirm `376d0d283`; tip-wins absorb `6f0f64611` onto crown; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md) / [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md). **≠ live-tip (`cfd93c1cb`) reconfirm** — sealed at cargo tips only. Sibling `ui-layout-court.ts` **UNSUPPORTED**. | sealed — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-retip5 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** @ `2685119bd` (**5+12**) | EXIT:**0**; completeness **5** + rhythm **12** @ tip `2685119bd` (`/tmp/ggen-brand-exact-tip`); land `bf3165322`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). **≠ live-tip (`cfd93c1cb`) reconfirm** — sealed at `2685119bd` only. Full DTCG/CASTLE inventory **UNSUPPORTED**. Production theme **UNSUPPORTED**. | sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |
| Foundry finish / `--live` (horizontal spine) | **BLOCKED_HITL** | Spine **PARTIAL**; foundry `--live` **BLOCKED_HITL** (`Model access is not configured`); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md). Gate1 tip-cargo `ALIVE` ≠ Foundry `ALIVE`. **Never invent Foundry ALIVE.** | Founder HITL: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0**; plus cos DB / WebAuthn / revops residuals |

---

## Sequencing law (post shui ALIVE close)

1. **Do not** launch parallel Gate1 `ggen-graph` tip-cargo under shared-pod thrash.
2. **All Gate1 tip-cargo falsifiers are ALIVE (exact tip-cargo)** — sealed at cargo tips; **≠** live-tip (`cfd93c1cb`) reconfirm. Do **not** re-prove unless sources drift.
3. Hot-swap exact tip-cargo remains **ALIVE** @ `c4e757a14` — **≠** live-tip reconfirm.
4. **shui** fence is **ALIVE (exact tip-cargo)** @ `a4b3d3fb4` (**4**/EXIT:**0**; absorb `edf2ec22a`) — tip6/tip7 PARTIAL superseded; **≠** live-tip reconfirm.
5. Truex product **ALIVE** is sibling vitest — outside cargo thrash; replay only if disputed.
6. Production theme remains **UNSUPPORTED**. Foundry remains **BLOCKED_HITL** / path-crown **PARTIAL** — **not ALIVE**.

---

## Compact return

```text
Truex=ALIVE@68d2d233
hotswap=ALIVE(exact tip-cargo)@c4e757a14 prod=UNSUPPORTED
shui=ALIVE(exact tip-cargo)@a4b3d3fb4 (4) absorb=edf2ec22a ≠live-tip-reconfirm
Taffy=ALIVE(exact tip-cargo)@2685119bd (7) ≠live-tip-reconfirm
uilayout=ALIVE(exact tip-cargo)@2685119bd/376d0d283 (11) ≠live-tip-reconfirm
brand_completeness+rhythm=ALIVE(exact tip-cargo)@2685119bd (5+12) ≠live-tip-reconfirm
Foundry=BLOCKED_HITL
cargo=NOT_RUN docs-only-retip tip_cargo≠live_tip_reconfirm
crown_tip=cfd93c1cb
side=cursor/v2711-gate1-shui-alive-close-bc54
```
