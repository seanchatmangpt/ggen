# Photolithography path — push-first honesty ledger (v27.1.1)

```text
tip=37b8aa0113ae7f540287ae5bcdd42dbb259ebe4d
path=perimeter → .ttl/.toml → ggen+Taffy AOT → igniter clean-room → zcode POWL/OCEL
path_crown=PARTIAL (non-ALIVE)
foundry=BLOCKED_HITL (no Foundry ALIVE)
taffy_hop=ALIVE(exact tip-cargo)@2685119bd ≠ path_crown_ALIVE ≠ Foundry_ALIVE
side=cursor/v2711-photolith-taffy-alive-bc54
agent=bc-f8dfcebc-0e39-5b0f-a3d9-880b68474aa2
```

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-PATH-STANDING-27.1.1` |
| Branch | `cursor/v2711-photolith-taffy-alive-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.1 hop chain |
| Companions | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md); [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md); [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md) |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `37b8aa0113ae7f540287ae5bcdd42dbb259ebe4d` (`37b8aa011`; past `59b9fae1c`; ⊇ Taffy absorb `c93277719` / brand `bf3165322` / uilayout `59b9fae1c` / ALIVE-trio falsifiers retip) |
| Agent | `bc-f8dfcebc-0e39-5b0f-a3d9-880b68474aa2` |
| UTC | `2026-11-22T18:57Z` |
| Cargo | **NOT RUN** (docs-only retip; Taffy/brand/uilayout sealed EXIT:**0** @ `2685119bd`; shui tip7 EXIT0 owns remaining Gate1 cargo) |
| Cheap probe | igniter mount `git diff --exit-code` → **0** @ `/home/ubuntu/ggen_igniter` `b5a15528…`; zcode mount **PRESENT** `/agent/repos/zcode-cli` @ `f32df08` |
| Status | LEDGER — Taffy hop **ALIVE (exact tip-cargo)** @ `2685119bd` / absorb `c93277719`; path crown **PARTIAL** / **non-ALIVE**; Foundry `--live` **BLOCKED_HITL**; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Honesty law (held):**
`PRESENT` ≠ `lineage tip-cargo ALIVE` ≠ `exact tip-cargo ALIVE` ≠
`photolithography_path_crown_ALIVE` ≠ `Foundry_ALIVE`.
Taffy hop tip-cargo **ALIVE** (**7** @ `2685119bd`, absorb `c93277719`) ≠ path crown
**ALIVE** ≠ Foundry **ALIVE**. Sealed exact tip-cargo rows are at cargo tip
`2685119bd` — that is **≠** a live-tip (`37b8aa011`) reconfirm unless a later
EXIT:**0** on the live tip is recorded. Igniter clean-room may hold **ALIVE** —
say so. Zcode POWL path remains **PARTIAL**. Foundry `--live` stays
**BLOCKED_HITL** until founder `zcode /login`.

```text
perimeter transducer
  → .ttl / .toml
  → ggen + Taffy AOT
  → ggen_igniter clean-room (git diff --exit-code == 0)
  → zcode-cli POWL/OCEL
```

---

## 1. Path hop table

| Hop | Evidence SHA / file | Standing | EXIT if any | Exact next verifier command |
|-----|---------------------|----------|-------------|------------------------------|
| 1 — Perimeter transducer (`shui:` fence + Truex admit) | Lineage tip-cargo **ALIVE** @ `008a81d21` (**4** EXIT **0**; blob `3cda065f`); tip6 exact-tip **PARTIAL_ALIVE** EXIT **137** @ tip_base `c661d228a` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md); tip7 EXIT0 **in flight** (do **not** invent EXIT:**0**) — [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md); Truex product **ALIVE** @ `truex@68d2d233` (**6/6**) | Fence: **PARTIAL_ALIVE** (tip7 in flight; lineage **ALIVE** held). Truex: **ALIVE**. Hop aggregate: **PARTIAL_ALIVE**. **≠** path crown **ALIVE** | Tip6 fence **137**; tip7 pending | tip7: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0**. Truex replay: `cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| 2 — `.ttl` / `.toml` (brand rhythm + completeness) | Exact tip-cargo **ALIVE** @ `2685119bd` — completeness **5** + rhythm **12** EXIT **0** (`/tmp/ggen-brand-exact-tip`); land `bf3165322`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). Hot-swap exact tip-cargo **ALIVE** @ `c4e757a14` (**6**). Full DTCG/CASTLE inventory **UNSUPPORTED**; production theme **UNSUPPORTED** | Brand courts: **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip reconfirm). Full DTCG/CASTLE / production: **UNSUPPORTED**. Hop slice: **PARTIAL_ALIVE** (ALIVE courts + UNSUPPORTED production). **≠** path crown **ALIVE** | Brand **0** | Sealed — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0**. Production stays **UNSUPPORTED** |
| 3 — ggen + Taffy AOT preflight | Exact tip-cargo **ALIVE** — **7 passed; EXIT:0** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719` onto crown (past `59b9fae1c` / live tip `37b8aa011`); [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md). Prior tip6 EXIT **137** / retip7 PARTIAL **superseded** | **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip `37b8aa011` reconfirm). **≠** `photolithography_path_crown_ALIVE`. **≠** `Foundry_ALIVE` | **0** (sealed) | Sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| 3b — ui-layout digest consume (in-repo fence) | Exact tip-cargo **ALIVE** — **11 passed; EXIT:0** @ tip `2685119bd` (`/tmp/ggen-uilayout-exact-retip5`); land `59b9fae1c`; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md). Sibling `ui-layout-court.ts` **UNSUPPORTED** | **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip reconfirm). **≠** path crown **ALIVE** | **0** (sealed) | Sealed — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-retip5 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| 4 — ggen_igniter clean-room | Lineage **ALIVE** held — retip3 crown `5a1129cc9` / absorb evidence `893070836` / prior `d0fec2f2f` / `f1492ed55`; mount `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc`; this land cheap probe `git diff --exit-code` → **0**; BrandTheme manufacture compare **ALIVE** (held; [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md)) | Clean-room slice: **ALIVE (lineage)** + cheap reconfirm EXIT **0**. **≠** path crown **ALIVE**. **≠** Foundry `--live` **ALIVE** | Clean-room **0** (this land) | `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0**. Contract (optional): `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| 5 — zcode-cli POWL/OCEL | Path pin tip `83c954e5f` — [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); refusal lineage **ALIVE** @ `cd7c2433b` (sjira **6** + bound **4**); tip-cargo honesty **PARTIAL** EXIT **137** @ `2685119bd` — [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md); mount **PRESENT** `@ f32df08`; `--live` **BLOCKED_HITL** (`foundry_exit=1`; Model access not configured). **Not Foundry ALIVE** | Hop-5 slice: **PARTIAL**. Refusal lineage: **ALIVE** (held). Tip-cargo: **PARTIAL**. Foundry `--live`: **BLOCKED_HITL**. **refusal ≠ Foundry ALIVE**. **≠** path crown **ALIVE** | Tip-cargo **137**; `--live` **1** | Tip-cargo: warm `CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked --config build.jobs=1 -- --test-threads=1` + `… remediation_bound --lib` → **6+4**/EXIT:**0**. Live: `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| **Path crown** | This ledger @ tip `37b8aa011`; aggregate of hops 1–5 | **PARTIAL** / **non-ALIVE** — Taffy tip-cargo **ALIVE** ≠ path crown **ALIVE** | — | Close hop-1 shui tip7 EXIT **0** **and** hop-5 tip-cargo EXIT **0** **and** hop-5 `--live` → **0** before any path-crown promote |
| **Foundry / zero-HITL** | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) | **BLOCKED_HITL** / **PARTIAL** — **not** Foundry `ALIVE` — **never invent** | `--live` **1** | Founder: `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **0** |

---

## 2. Gate1 court standing (post ALIVE trio)

| Court | Cargo tip | Standing | EXIT | Notes |
|-------|-----------|----------|------|-------|
| `shui_public_prefix_court` | tip6 `c661d228a` / tip7 in flight | **PARTIAL_ALIVE** | tip6 **137**; tip7 pending | Lineage **ALIVE** @ `008a81d21` (**4**) held — do **not** invent tip7 EXIT:**0** |
| `brand_toml_{completeness,rhythm}_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**5+12**) | Land `bf3165322`; ≠ live-tip reconfirm; ≠ path crown |
| `taffy_aot_preflight_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**7**) | Absorb `c93277719`; ≠ path crown ≠ Foundry |
| `ui_layout_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**11**) | Land `59b9fae1c`; ≠ path crown |
| `zcode_powl_ocel` refuse path | `2685119bd` | **PARTIAL** | **137** | Lineage refuse **ALIVE** @ `cd7c2433b` (**6+4**) held; ≠ Foundry |

Trio (brand / Taffy / uilayout) sealed at `2685119bd` — tip7 does **not** re-prove unless sources drift. This land does **not** re-run cargo.

---

## 3. Ancestry vs live crown `37b8aa011`

Zero-cost `git merge-base --is-ancestor` (all **ok**):

| Evidence SHA | Role |
|--------------|------|
| `e24ce7a73` | Mandated crown floor |
| `2685119bd` | ALIVE-trio cargo tip (brand / Taffy / uilayout) |
| `c93277719` | Taffy exact tip-cargo absorb onto crown |
| `bf3165322` | brand.toml exact tip-cargo land |
| `59b9fae1c` | ui_layout exact tip-cargo land (crown now past) |
| `008a81d21` | shui lineage tip-cargo ALIVE |
| `cd7c2433b` | zcode refusal lineage tip-cargo ALIVE |
| `5a1129cc9` | igniter clean-room retip3 crown |
| `83c954e5f` | zcode path honesty pin |
| `c4e757a14` | brand hot-swap exact tip-cargo ALIVE |

---

## 4. Explicit non-claims

- Taffy tip-cargo **ALIVE** (**7** @ `2685119bd`) ≠ `photolithography_path_crown_ALIVE` ≠ `Foundry_ALIVE`.
- Exact tip-cargo **ALIVE** @ `2685119bd` ≠ live-tip (`37b8aa011`) reconfirm.
- Igniter clean-room EXIT **0** ≠ path crown **ALIVE** ≠ Foundry `--live` **ALIVE**.
- Zcode refusal lineage **ALIVE** ≠ Foundry / `--live` **ALIVE**; hop-5 remains **PARTIAL**.
- Source **PRESENT** / mount **PRESENT** ≠ any crown **ALIVE**.
- This ledger does **not** invent path-crown or Foundry `ALIVE`.
- No ManagePullRequest. No force-push. No heavy cargo this land.

---

## 5. Founder next command

```bash
cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live
# PASS: foundry_exit=0
# FAIL: foundry_exit=1 + Model access not configured → keep BLOCKED_HITL
# Antifalsifier: Taffy tip-cargo ALIVE ≠ path_crown ALIVE ≠ Foundry ALIVE
```

---

## 6. Provenance

| Event | Note |
|-------|------|
| Path honesty audit | 2026-11-22 — `cursor/v2711-photolith-path-retip-bc54` (`bc-f7833d1a`) from live crown `e6d00965a`; docs-only; tip6 Gate1 **PARTIAL_ALIVE** EXIT **137**; igniter **ALIVE (lineage)**; hop-5 **PARTIAL** / `--live` **BLOCKED_HITL**; path crown **PARTIAL**; **no** Foundry `ALIVE` |
| Taffy-alive path retip | 2026-11-22 — `cursor/v2711-photolith-taffy-alive-bc54` (`bc-f8dfcebc`) from live crown `37b8aa011` (past `59b9fae1c`); docs-only; Taffy hop **ALIVE (exact tip-cargo)** @ `2685119bd` / absorb `c93277719`; brand+uilayout sealed **ALIVE** @ same cargo tip; shui fence **PARTIAL_ALIVE** (tip7 in flight); igniter clean-room **ALIVE** + cheap EXIT **0**; hop-5 **PARTIAL** / Foundry **BLOCKED_HITL**; path crown **PARTIAL**; **Taffy ALIVE ≠ path_crown ALIVE ≠ Foundry ALIVE**; no cargo; no ManagePullRequest |

### Compact return

```text
tip=37b8aa0113ae7f540287ae5bcdd42dbb259ebe4d
hop1=PARTIAL_ALIVE(shui tip7 in flight; lineage ALIVE@008a81d21; Truex ALIVE@68d2d233)
hop2=PARTIAL_ALIVE(brand ALIVE exact tip-cargo@2685119bd 5+12; prod UNSUPPORTED)
hop3=ALIVE(exact tip-cargo)@2685119bd/absorb c93277719 (7) ≠ path_crown ≠ Foundry
hop3b=ALIVE(exact tip-cargo)@2685119bd (11) ≠ path_crown
hop4=ALIVE(lineage)+cheap EXIT0 @b5a15528 (≠ path/Foundry)
hop5=PARTIAL (refusal lineage ALIVE@cd7c2433b; tip-cargo EXIT137; --live BLOCKED_HITL)
path_crown=PARTIAL  foundry=BLOCKED_HITL
exact_next_founder=cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live → 0
```
