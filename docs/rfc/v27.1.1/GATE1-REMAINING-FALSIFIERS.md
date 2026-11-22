# Gate1 remaining falsifiers ledger (v27.1.1)

```text
crown_tip=3ae705d1c86c76c33836e5ccc78f0c018e546e30
branch=cursor/v2711-gate1-remaining-falsifiers-bc54
utc=2026-11-22T18:50Z
cargo=none (tip7 owns EXIT0)
ManagePullRequest=not_used
invented_ALIVE=none
```

| Field | Value |
|-------|-------|
| Document ID | `GATE1-REMAINING-FALSIFIERS` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`3ae705d1c86c76c33836e5ccc78f0c018e546e30`** (`3ae705d1c`; brand hot-swap exact-retip4 ALIVE land) |
| Sidebranch | `cursor/v2711-gate1-remaining-falsifiers-bc54` |
| Cargo / tip-cargo | **NOT RUN** (docs-only; **tip7 owns Gate1 EXIT0** serial) |
| Spec bind | [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) §6–§10 / §17–§18; companions [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md), [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md), [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** This ledger is a docs-only pin of Gate1 falsifier standings at live crown
`3ae705d1c`. It does **not** run cargo. Tip6 exact-tip probes that exited **137**
remain **PARTIAL_ALIVE** — lineage tip-cargo `ALIVE` is **held**, not upgraded.
**Tip7 serial owns all Gate1 EXIT0 cargos.** Never invent tip-subject `ALIVE` from
blob identity, lineage alone, or this markdown file. Never invent Foundry /
path-crown / production-theme `ALIVE`.

---

## Remaining falsifiers

| falsifier | standing | evidence | exact next |
|-----------|----------|----------|------------|
| Truex `shui:` product admit (D3) | **ALIVE** | Sibling Chicago admit-court **6/6**; EXIT **0** @ `truex@68d2d2331696f18949f4877b994320018b6365e5` (merged `b4672131`; [truex#6](https://github.com/seanchatmangpt/truex/pull/6)); reconfirm [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) / [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md). ≠ Foundry / path-crown `ALIVE`. | `cd /tmp/truex && git rev-parse HEAD` → `68d2d233…`; `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** @ crown **`3ae705d1c`** / cargo tip `c4e757a14` | `brand_toml_hotswap_court` **6 passed; EXIT:0** @ `c4e757a14` (`/tmp/only-fafc690f-tip3-courts`); fence ceiling **PARTIAL_ALIVE**; [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md) | sealed at tip — do **not** re-run under tip7 thrash unless sources drift; replay only if needed: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| brand.toml production theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Hot-swap / rhythm / fluid `ALIVE` ≠ production `ALIVE`. | Gate2+ / OOR path — production theme stays **UNSUPPORTED** until all production falsifiers exit **0** (not tip7 Gate1 EXIT0) |
| ggen `shui_public_prefix_court` (fence) | **PARTIAL_ALIVE** (tip6; tip7 serial pending) | Tip6 probe EXIT **137** @ tip_base `c661d228a` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md). Lineage tip-cargo **ALIVE** **4**/EXIT:**0** @ `008a81d21` (blob `3cda065f`) **held** — not tip-subject substitute. | tip7 serial: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip7 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** |
| Taffy AOT preflight (`taffy_aot_preflight_court`) | **PARTIAL_ALIVE** (tip6; tip7 serial pending) | Tip6 probe EXIT **137** @ tip_base `552c85f29` — [`TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md). Lineage tip-cargo **ALIVE** **7**/EXIT:**0** @ `19c08a3b0` / absorb `cde02d1f2` **held**. | tip7 serial: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-tip7 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| ui_layout digest (`ui_layout_court`) | **PARTIAL_ALIVE** (tip6; tip7 serial pending) | Tip6 probe EXIT **137** @ tip_base `4abd4adfb` — [`UILAYOUT-EXACT-TIP6-STANDING.md`](./UILAYOUT-EXACT-TIP6-STANDING.md). Lineage exact tip-cargo **ALIVE** **11**/EXIT:**0** @ `a508e6bae` **held**. Sibling `ui-layout-court.ts` **UNSUPPORTED**. | tip7 serial: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip7 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| brand.toml completeness + rhythm | **PARTIAL_ALIVE** (tip6; tip7 serial pending) | Tip6 probe EXIT **137** @ tip_base `552c85f29` — [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md); retip5 [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). Lineage tip-cargo **ALIVE** completeness **5** + rhythm **12**/EXIT:**0** @ `77eb46fd0` **held**. Full DTCG/CASTLE inventory **UNSUPPORTED**. | tip7 serial: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip7 --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |

---

## Sequencing law (tip7 owns EXIT0)

1. **Do not** launch parallel Gate1 `ggen-graph` tip-cargo under shared-pod thrash.
2. Tip7 agent runs the PARTIAL rows **serially** (`jobs=1`, isolated `CARGO_TARGET_DIR`, quiet window) until each EXIT **0** or typed thrash/OOM refusal.
3. Hot-swap exact tip-cargo is already **ALIVE** at crown `3ae705d1c` — tip7 does **not** need to re-prove it unless sources drift.
4. Truex product **ALIVE** is sibling vitest — outside tip7 cargo thrash; replay only if disputed.
5. Production theme remains **UNSUPPORTED** regardless of tip7 Gate1 EXIT0 outcomes.

---

## Compact return

```text
Truex=ALIVE@68d2d233
hotswap=ALIVE(exact tip-cargo)@3ae705d1c/c4e757a14 prod=UNSUPPORTED
shui=PARTIAL tip6+tip7_serial
Taffy=PARTIAL tip6+tip7_serial
uilayout=PARTIAL tip6+tip7_serial
brand_completeness+rhythm=PARTIAL tip6+tip7_serial lineage@77eb46fd0
cargo=NOT_RUN tip7_owns_EXIT0
crown_tip=3ae705d1c
side=cursor/v2711-gate1-remaining-falsifiers-bc54
```
