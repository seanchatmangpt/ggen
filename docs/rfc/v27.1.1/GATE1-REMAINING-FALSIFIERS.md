# Gate1 remaining falsifiers ledger (v27.1.1)

```text
crown_tip=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
branch=cursor/v2711-gate1-remaining-retip-bc54
utc=2026-11-22T19:42Z
cargo=none (docs-only retip after photolith absorb @ 6cdb97492 + taffy tip6 @ 2e452873a + zcode retip5 @ ae34bf4bc)
ManagePullRequest=not_used
invented_ALIVE=none
live_tip_ALIVE_invented=none
Foundry_ALIVE_invented=none
Foundry=BLOCKED_HITL
path_crown=PARTIAL
tip_cargo_neq_live_tip_reconfirm=held
tip7_open=shui_PARTIAL_EXIT101 + uilayout_in_flight
taffy_tip7=ALIVE@63da993cc/absorb_4ebcb9d46
prior_close=700ff435e@31b495a6a
```

| Field | Value |
|-------|-------|
| Document ID | `GATE1-REMAINING-FALSIFIERS` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4`** (`ae34bf4bc`; ⊇ zcode POWL retip5 `ae34bf4bc` / taffy tip6 absorb `2e452873a` / photolith founder-CLI honesty `6cdb97492` / Foundry HITL exact CLI `4af3c6ec5` / shui tip-cargo ALIVE absorb `edf2ec22a` ← `a4b3d3fb4` / shui tip7 PARTIAL absorb `d35482374` / brand tip6 `31b495a6a` / ui_layout `6f0f64611` / Taffy tip-cargo+tip7 `c93277719`+`4ebcb9d46`) |
| Sidebranch | `cursor/v2711-gate1-remaining-retip-bc54` |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty retip; tip7 diagnosis owns cargo; sealed tip-cargo EXIT:**0** held; **≠** live-tip reconfirm) |
| Spec bind | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §6–§10 / §17–§20; companions [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md), [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md), [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md), [`SHUI-PUBLIC-PREFIX-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP7-STANDING.md), [`TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md), [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** This ledger is a docs-only pin of Gate1 falsifier standings at live crown
`ae34bf4bc` after photolith absorb (`6cdb97492`) + taffy tip6 (`2e452873a`) +
zcode retip5. It does **not** run cargo (tip7 serial owns EXIT0 thrash). Exact
tip-cargo `ALIVE` rows below are sealed at their cargo tips (`a4b3d3fb4` /
`cfd93c1cb` brand tip6 / `2685119bd` / `c4e757a14` / …) — that is **≠** a
live-tip (`ae34bf4bc`+) reconfirm unless a later EXIT:**0** on the live tip is
recorded.
**tip7 open (held):** shui tip7 thrash **PARTIAL_ALIVE** EXIT:**101** (≠ demote
sealed tip-cargo ALIVE @ `a4b3d3fb4`); uilayout tip7 **open/in-flight**
(`cursor/v2711-uilayout-exact-tip7-bc54`). Taffy tip7 **ALIVE** EXIT:**0** @
`63da993cc` / absorb `4ebcb9d46`. Never invent tip-subject / live-tip `ALIVE`
from blob identity, lineage alone, or this markdown file. Never invent Foundry /
path-crown / production-theme `ALIVE`. Path crown remains **PARTIAL**. Foundry
remains **BLOCKED_HITL**.

---

## Remaining falsifiers

| falsifier | standing | evidence | exact next |
|-----------|----------|----------|------------|
| Truex `shui:` product admit (D3) | **ALIVE** | Sibling Chicago admit-court **6/6**; EXIT **0** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` (merged `b4672131`; [truex#6](https://github.com/seanchatmangpt/truex/pull/6)); reconfirm [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) / [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md). ≠ Foundry / path-crown `ALIVE`. | `cd /tmp/truex && git rev-parse HEAD` → `68d2d233…`; `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** @ cargo tip `c4e757a14` / crown land `3ae705d1c+` | `brand_toml_hotswap_court` **6 passed; EXIT:0** @ `c4e757a14` (`/tmp/only-fafc690f-tip3-courts`); fence ceiling **PARTIAL_ALIVE**; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm**. | sealed — do **not** re-run under tip7 thrash unless sources drift; replay only if needed: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| brand.toml production theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Hot-swap / rhythm / fluid `ALIVE` ≠ production `ALIVE`. | Gate2+ / OOR path — production theme stays **UNSUPPORTED** until all production falsifiers exit **0** |
| ggen `shui_public_prefix_court` (fence) | **ALIVE (exact tip-cargo)** @ `a4b3d3fb4` (**4**) + tip7 thrash **PARTIAL_ALIVE** EXIT:**101** (open) | Sealed tip-cargo EXIT:**0**; **4 passed** @ tip `a4b3d3fb4` (`/tmp/gate1-shui-alive`); tip-wins absorb `edf2ec22a`; court blob `3cda065f`; [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md). Tip6/prior tip7 PARTIAL EXIT137 superseded for tip-cargo class. Later tip7 thrash **PARTIAL_ALIVE** EXIT:**101** ([`SHUI-PUBLIC-PREFIX-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP7-STANDING.md); absorb `d35482374`) ≠ demote sealed ALIVE. **≠ live-tip (`ae34bf4bc`+) reconfirm**. ≠ Foundry `ALIVE`. | **tip7 open:** `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** (quiet exclusive target; tip7 diagnosis owns cargo) |
| Taffy AOT preflight (`taffy_aot_preflight_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**7**) + tip7 **ALIVE** @ `63da993cc` | EXIT:**0**; **7 passed** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719`; tip7 reconfirm EXIT:**0** absorb `4ebcb9d46` @ `63da993cc`; tip6 absorb `2e452873a`; [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) / [`TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm** — sealed at cargo tips. Photolithography / Foundry remain **PARTIAL** / **BLOCKED_HITL**. | sealed — tip7 **ALIVE**; replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| ui_layout digest (`ui_layout_court`) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**11**; also `376d0d283`) + tip7 **open/in-flight** | EXIT:**0**; **11 passed** @ tip `2685119bd` / reconfirm `376d0d283`; tip-wins absorb `6f0f64611`; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md) / [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md). Tip7 side `cursor/v2711-uilayout-exact-tip7-bc54` / `bc-404c8bbd` — **open/in-flight** (no tip7 EXIT:**0** standing yet; tip7 diagnosis owns cargo). **≠ live-tip (`ae34bf4bc`+) reconfirm**. Sibling `ui-layout-court.ts` **UNSUPPORTED**. | **tip7 open:** `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-uilayout cargo test -p ggen-graph --test ui_layout_court --locked --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** (serial after shui tip7 quiet; tip7 diagnosis owns cargo) |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** @ `cfd93c1cb` tip6 (**5+12**; lineage `2685119bd`) | EXIT:**0**; completeness **5** + rhythm **12** @ tip `cfd93c1cb` (`/tmp/ggen-brand-exact-tip6`); tip-wins absorb `31b495a6a`; [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md); lineage [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). **≠ live-tip (`ae34bf4bc`+) reconfirm** — sealed at `cfd93c1cb` only. Full DTCG/CASTLE inventory **UNSUPPORTED**. Production theme **UNSUPPORTED**. | sealed at `cfd93c1cb` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |
| Photolithography path crown | **PARTIAL** (non-ALIVE) | Gate1 tip-cargo green ≠ path crown; hop-5 **PARTIAL** / Foundry `--live` **BLOCKED_HITL**; [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) (photolith absorb `6cdb97492`). | Close hop-5 `--live` → **0** before any path-crown promote — **never invent path-crown ALIVE** |
| Foundry finish / `--live` (horizontal spine) | **BLOCKED_HITL** | Spine **PARTIAL**; foundry `--live` **BLOCKED_HITL** (`Model access is not configured`); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md). Gate1 tip-cargo `ALIVE` ≠ Foundry `ALIVE`. **Never invent Foundry ALIVE.** | Founder HITL exact CLI: `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** (**not** `zcode /login`); plus cos DB / WebAuthn / revops residuals |

---

## tip7 open items (cargo owned by tip7 diagnosis)

| subject | standing | exact next (tip7 owns cargo — this ledger does **not** run) |
|---------|----------|-------------------------------------------------------------|
| Taffy tip7 | **ALIVE** EXIT:**0** @ `63da993cc` / absorb `4ebcb9d46` | sealed — no tip7 reopen unless sources drift |
| shui tip7 | **PARTIAL_ALIVE** EXIT:**101** (target wiped mid-oxrocksdb; absorb `d35482374`) — sealed tip-cargo ALIVE @ `a4b3d3fb4` held / ≠ demoted | `CARGO_TARGET_DIR=/tmp/gate1-tip7-shui` … `shui_public_prefix_court` → **4**/EXIT:**0** |
| uilayout tip7 | **open/in-flight** (`cursor/v2711-uilayout-exact-tip7-bc54`; gate waited on shui tip7 standing) | `CARGO_TARGET_DIR=/tmp/gate1-tip7-uilayout` … `ui_layout_court` → **11**/EXIT:**0** |

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
