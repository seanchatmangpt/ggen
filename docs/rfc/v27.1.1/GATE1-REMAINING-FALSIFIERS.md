# Gate1 remaining falsifiers ledger (v27.1.1)

```text
crown_tip=c19ad401d (⊇ Construct ALIVE tip-wins #956)
branch=cursor/v2711-construct-alive-retip-bc54
utc=2026-11-23T00:25Z
cargo=tip7 sealed ALIVE (shui4+uilayout11+brand23+taffy7); Construct ALIVE@45086131007
ManagePullRequest=PR849
invented_ALIVE=none
live_tip_ALIVE_invented=none
Foundry_ALIVE_invented=none
Foundry=BLOCKED_HITL
path_crown=PARTIAL
tip7_CLOSED_ALIVE=held
construct_lock_sync=7bb443e99
construct_full_job=ALIVE@45086131007 (≠ Foundry ALIVE)
```

| Field | Value |
|-------|-------|
| Document ID | `GATE1-REMAINING-FALSIFIERS` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`c19ad401d`** (⊇ Construct full **ALIVE** @ `45086131007` + IA portable + Gate1 tip7 + G2-11 Mount) |
| Sidebranch | `cursor/v2711-construct-alive-retip-bc54` |
| Cargo / tip-cargo | tip7 live-tip **ALIVE** sealed (shui **4** + uilayout **11** + brand **23** + taffy **7**); Construct full job **ALIVE** @ [`45086131007`](https://github.com/seanchatmangpt/ggen/actions/runs/45086131007) — **≠** Foundry ALIVE |
| Spec bind | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §6–§10 / §17–§20; companions [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md), [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md), [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Gate1 tip7 live-tip reconfirm is **CLOSED/ALIVE** at tip `b07cda77c+`
(shui **4** + uilayout **11** + brand **23** + taffy **7**). Exact tip-cargo
`ALIVE` rows remain sealed at their cargo tips. Construct full job is **ALIVE**
@ `45086131007` on subject `43d6301bb` — **≠** Foundry / path-crown / zero-HITL
`ALIVE`. Never invent Foundry / path-crown / production-theme / zero-HITL
`ALIVE`. Path crown remains **PARTIAL**. Foundry remains **BLOCKED_HITL**.

---

## Remaining falsifiers

| falsifier | standing | evidence | exact next |
|-----------|----------|----------|------------|
| Truex `shui:` product admit (D3) | **ALIVE** | Sibling Chicago admit-court **6/6**; EXIT **0** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` (merged `b4672131`; [truex#6](https://github.com/seanchatmangpt/truex/pull/6)); reconfirm [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) / [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md). ≠ Foundry / path-crown `ALIVE`. | `cd /tmp/truex && git rev-parse HEAD` → `68d2d233…`; `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** @ cargo tip `c4e757a14` / crown land `3ae705d1c+` | `brand_toml_hotswap_court` **6 passed; EXIT:0** @ `c4e757a14` (`/tmp/only-fafc690f-tip3-courts`); fence ceiling **PARTIAL_ALIVE**; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm**. | sealed — do **not** re-run under tip7 thrash unless sources drift; replay only if needed: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| brand.toml production theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Hot-swap / rhythm / fluid `ALIVE` ≠ production `ALIVE`. | Gate2+ / OOR path — production theme stays **UNSUPPORTED** until all production falsifiers exit **0** |
| ggen `shui_public_prefix_court` (fence) | **ALIVE (exact tip-cargo)** @ `a4b3d3fb4` (**4**) + **live-tip reconfirm ALIVE** @ `9dc7388b2` (**4**/EXIT:**0**) | Sealed tip-cargo EXIT:**0**; **4 passed** @ tip `a4b3d3fb4` (`/tmp/gate1-shui-alive`); tip-wins absorb `edf2ec22a`; [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md). Live-tip reconfirm **4**/EXIT:**0** @ `9dc7388b2` (`/tmp/gate1-tip7-shui`; [`SHUI-PUBLIC-PREFIX-LIVE-TIP-RECONFIRM-STANDING.md`](./SHUI-PUBLIC-PREFIX-LIVE-TIP-RECONFIRM-STANDING.md)) supersedes tip7 thrash PARTIAL EXIT:**101** for live-tip class. ≠ Foundry `ALIVE`. | sealed — replay: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** |
| Taffy AOT preflight (`taffy_aot_preflight_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**7**) + tip7 **ALIVE** @ `63da993cc` | EXIT:**0**; **7 passed** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719`; tip7 reconfirm EXIT:**0** absorb `4ebcb9d46` @ `63da993cc`; tip6 absorb `2e452873a`; [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) / [`TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm** — sealed at cargo tips. Photolithography / Foundry remain **PARTIAL** / **BLOCKED_HITL**. | sealed — tip7 **ALIVE**; replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| ui_layout digest (`ui_layout_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**11**) + **live-tip reconfirm ALIVE** @ `9dc7388b2` (**11**/EXIT:**0**) | EXIT:**0**; **11 passed** @ tip `2685119bd`; tip-wins absorb `6f0f64611`; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md). Live-tip reconfirm **11**/EXIT:**0** @ `9dc7388b2` (`/tmp/gate1-tip7-brand`; [`UILAYOUT-LIVE-TIP-RECONFIRM-STANDING.md`](./UILAYOUT-LIVE-TIP-RECONFIRM-STANDING.md)). Sibling `ui-layout-court.ts` **UNSUPPORTED**. ≠ Foundry `ALIVE`. | sealed — replay: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-brand cargo test -p ggen-graph --test ui_layout_court --locked --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** @ `cfd93c1cb` tip6 (**5+12**; lineage `2685119bd`) | EXIT:**0**; completeness **5** + rhythm **12** @ tip `cfd93c1cb` (`/tmp/ggen-brand-exact-tip6`); tip-wins absorb `31b495a6a`; [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md); lineage [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm** — sealed at `cfd93c1cb` only. Full DTCG/CASTLE inventory **UNSUPPORTED**. Production theme **UNSUPPORTED**. | sealed at `cfd93c1cb` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |
| Photolithography path crown | **PARTIAL** (non-ALIVE) | Gate1 tip-cargo green ≠ path crown; hop-5 **PARTIAL** / Foundry `--live` **BLOCKED_HITL**; [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) (photolith absorb `6cdb97492`). | Close hop-5 `--live` → **0** before any path-crown promote — **never invent path-crown ALIVE** |
| Construct core-team full job | **ALIVE** | Run [`45086131007`](https://github.com/seanchatmangpt/ggen/actions/runs/45086131007) EXIT **0** on `43d6301bb`; tip-wins #956 @ `c19ad401d`; [`CONSTRUCT-FULL-ALIVE-STANDING.md`](./CONSTRUCT-FULL-ALIVE-STANDING.md). **≠** Foundry / path-crown `ALIVE`. | sealed — replay only if Construct workflow / IA sources drift |
| Foundry finish / `--live` (horizontal spine) | **BLOCKED_HITL** | Spine **PARTIAL**; foundry `--live` **BLOCKED_HITL** (`Model access is not configured`; provider_config **ABSENT**; no model API keys this env); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md). Construct/Gate1 `ALIVE` ≠ Foundry `ALIVE`. **Never invent Foundry ALIVE.** | Founder HITL exact CLI: `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** (**not** `zcode /login`; Linux needs Z.AI Coding Plan API Key / `/login`); plus cos DB / WebAuthn / revops residuals |

---

## tip7 live-tip reconfirm (closed 2026-11-22 2:35 PM PST)

| subject | standing | exact next |
|---------|----------|------------|
| Taffy tip7 | **ALIVE** EXIT:**0** **7**/7 @ live tip `b07cda77c` (`/tmp/gate1-tip7-taffy.log`); prior absorb `4ebcb9d46` held | sealed |
| shui tip7 | **ALIVE** EXIT:**0** **4**/4 @ live tip (`/tmp/gate1-tip7-shui.log`) — prior EXIT:**101** thrash **superseded**; sealed tip-cargo @ `a4b3d3fb4` held | sealed |
| uilayout tip7 | **ALIVE** EXIT:**0** **11**/11 @ live tip (`/tmp/gate1-tip7-uilayout.log`) — prior open/in-flight **superseded**; sealed tip-cargo @ `2685119bd` held | sealed |
| brand tip7 (completeness+hotswap+rhythm) | **ALIVE** EXIT:**0** **5+6+12** @ live tip (`/tmp/gate1-tip7-brand.log`); sealed tip-cargo @ `cfd93c1cb`/`c4e757a14` held | sealed |

Companion: [`GATE1-TIP7-LIVE-RECONFIRM-STANDING.md`](./GATE1-TIP7-LIVE-RECONFIRM-STANDING.md). **≠ Foundry / path-crown ALIVE.**

---

## Sequencing law (post photolith / tip-cargo ALIVE)

1. **Do not** launch parallel Gate1 `ggen-graph` tip-cargo under shared-pod thrash — tip7 diagnosis owns cargo.
2. **Gate1 tip-cargo falsifiers are ALIVE (exact tip-cargo)** — sealed at cargo tips; **≠** live-tip (`ae34bf4bc`+) reconfirm. Do **not** re-prove sealed rows unless sources drift.
3. Hot-swap exact tip-cargo remains **ALIVE** @ `c4e757a14` — **≠** live-tip reconfirm.
4. **shui** fence tip-cargo **ALIVE** @ `a4b3d3fb4` (**4**/EXIT:**0**; absorb `edf2ec22a`) — tip7 thrash EXIT:**101** remains **open** (≠ demote sealed ALIVE).
5. **uilayout** tip-cargo **ALIVE** @ `2685119bd` — tip7 reconfirm still **open/in-flight**.
6. **Taffy** tip7 **ALIVE** @ `63da993cc` / absorb `4ebcb9d46`.
7. Truex product **ALIVE** is sibling vitest — outside cargo thrash; replay only if disputed.
8. Production theme remains **UNSUPPORTED**. Path crown **PARTIAL**. Foundry **BLOCKED_HITL** — **not ALIVE**.

---

## Compact return

```text
Truex=ALIVE@68d2d233
hotswap=ALIVE(exact tip-cargo)@c4e757a14 prod=UNSUPPORTED
shui=ALIVE(exact tip-cargo)@a4b3d3fb4 (4) absorb=edf2ec22a ≠live-tip-reconfirm
shui_tip7=PARTIAL_ALIVE EXIT101 open (≠demote sealed)
Taffy=ALIVE(exact tip-cargo)@2685119bd (7) tip7=ALIVE@63da993cc/4ebcb9d46 ≠live-tip-reconfirm
uilayout=ALIVE(exact tip-cargo)@2685119bd/376d0d283 (11) tip7=open/in-flight ≠live-tip-reconfirm
brand_completeness+rhythm=ALIVE(exact tip-cargo)@cfd93c1cb tip6 (5+12) ≠live-tip-reconfirm
path_crown=PARTIAL
Foundry=BLOCKED_HITL
cargo=NOT_RUN docs-only-retip tip7_owns_cargo tip_cargo≠live_tip_reconfirm
crown_tip=ae34bf4bc (⊇2e452873a/6cdb97492 photolith)
side=cursor/v2711-gate1-remaining-retip-bc54
next_verifier=shui_tip7_EXIT0 then uilayout_tip7_EXIT0 ; founder bun_bin_zcode.js_login + foundry_--live
```
