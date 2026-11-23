# Photolithography path — push-first honesty ledger (v27.1.1)

```text
tip=43d6301bb (pytest+lock; prior 7bb443e99)
path=perimeter → .ttl/.toml → ggen+Taffy AOT → igniter clean-room → zcode POWL/OCEL
path_crown=PARTIAL (non-ALIVE)
foundry=BLOCKED_HITL foundry_exit=1 (no Foundry ALIVE)
tip7=ALIVE(shui4+uilayout11+brand23+taffy7) ≠ path_crown_ALIVE ≠ Foundry_ALIVE
igniter_hop=ALIVE EXIT0 @b5a15528 ≠ path_crown_ALIVE ≠ Foundry_ALIVE
construct=IN_PROGRESS@45086131007 (IA core-team ladder; tip frozen)
ia_portable=ALIVE_173_pass on #956 (tip-wins after Construct EXIT0)
cos=503_DB_UNAVAILABLE healthz=200
revops=NXDOMAIN
side=cursor/v2711-construct-pytest-retip-bc54
agent=bc-c5848e85-7857-4989-9a23-d4416c85bc54
```

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-PATH-STANDING-27.1.1` |
| Branch | `cursor/v2711-construct-pytest-retip-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.1 hop chain |
| Companions | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md); [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md); [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md); [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md); [`INTERVIEW-ASSIST-PORTABLE-PATH-STANDING.md`](./INTERVIEW-ASSIST-PORTABLE-PATH-STANDING.md) |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `43d6301bb6e3d6d715d19bea3bc08a4952780e91` (pytest tip-wins; lock sync `7bb443e99`; Gate1 tip7 ALIVE; G2-11 Mount ALIVE; Construct full job **IN_PROGRESS**) |
| Agent | `bc-c5848e85-7857-4989-9a23-d4416c85bc54` |
| UTC | `2026-11-23T00:20Z` |
| Cargo | **NOT RUN** this retip (docs + OOR probe; Gate1 tip7 sealed; Foundry `--live` re-probe **exit 1** held) |
| Cheap probe | igniter `git diff --exit-code` → **0** @ `/home/ubuntu/ggen_igniter`; cos `/readyz` **503** `DB_UNAVAILABLE`; `/healthz` **200**; revops **NXDOMAIN**; Foundry `--live` **foundry_exit=1**; IA portable local **173/7 EXIT0** on #956 |
| Status | LEDGER — shui/Taffy/uilayout/brand/igniter **ALIVE**; hop-5 **PARTIAL** / Foundry **BLOCKED_HITL**; path crown **PARTIAL** / **non-ALIVE**; Construct **IN_PROGRESS** @ `45086131007`; **never invent Foundry `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Honesty law (held):**
`PRESENT` ≠ `lineage tip-cargo ALIVE` ≠ `exact tip-cargo ALIVE` ≠
`photolithography_path_crown_ALIVE` ≠ `Foundry_ALIVE`.
Shui hop tip-cargo **ALIVE** (**4**/4 @ `a4b3d3fb4`, absorb `edf2ec22a`) ≠ path crown
**ALIVE** ≠ Foundry **ALIVE**. Taffy hop tip-cargo **ALIVE** (**7** @ `2685119bd`,
absorb `c93277719`) ≠ path crown **ALIVE** ≠ Foundry **ALIVE**. Sealed exact
tip-cargo rows are at cargo tips `a4b3d3fb4` / `2685119bd` — that is **≠** a
live-tip (`4af998e9b`) reconfirm unless a later EXIT:**0** on the live tip is
recorded. Shui tip7 thrash reconfirm **PARTIAL_ALIVE** EXIT:**101**
([`SHUI-PUBLIC-PREFIX-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP7-STANDING.md))
does **not** demote sealed ALIVE @ `a4b3d3fb4`. Igniter clean-room holds
**ALIVE** — say so. Zcode POWL hop remains **PARTIAL** (`--live`
**BLOCKED_HITL**). Foundry `--live` stays **BLOCKED_HITL** until founder
`bun bin/zcode.js login` (OAuth HITL; slash `/login` invalid; `zcode` absent
from PATH — [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md)).
**Never invent Foundry `ALIVE`.**

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
| 1 — Perimeter transducer (`shui:` fence + Truex admit) | Exact tip-cargo **ALIVE** @ `a4b3d3fb4` — **4 passed; EXIT:0** (`/tmp/gate1-shui-alive`); tip-wins absorb `edf2ec22a` (pre-absorb `516fa63fc`; side `8ba2bdd2a`); court blob `3cda065f`; [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md). Tip6/prior tip7 PARTIAL EXIT137 **superseded** for tip-subject. Later tip7 thrash **PARTIAL_ALIVE** EXIT:**101** ([`SHUI-PUBLIC-PREFIX-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP7-STANDING.md)) ≠ demote sealed ALIVE. Lineage **ALIVE** @ `008a81d21` held. Truex product **ALIVE** @ `truex@68d2d233` (**6/6**) | Fence: **ALIVE (exact tip-cargo)** @ `a4b3d3fb4` (**4**/4). Truex: **ALIVE**. Hop aggregate: **ALIVE**. **≠** path crown **ALIVE**. **≠** Foundry **ALIVE** | **0** (sealed) | Sealed at `a4b3d3fb4` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-shui-alive cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0**. Truex replay: `cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| 2 — `.ttl` / `.toml` (brand rhythm + completeness) | Exact tip-cargo **ALIVE** @ `2685119bd` — completeness **5** + rhythm **12** EXIT **0** (`/tmp/ggen-brand-exact-tip`); land `bf3165322`; tip6 absorb `31b495a6a`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md). Hot-swap exact tip-cargo **ALIVE** @ `c4e757a14` (**6**). Full DTCG/CASTLE inventory **UNSUPPORTED**; production theme **UNSUPPORTED** | Brand courts: **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip reconfirm). Full DTCG/CASTLE / production: **UNSUPPORTED**. Hop slice: **PARTIAL_ALIVE** (ALIVE courts + UNSUPPORTED production). **≠** path crown **ALIVE** | Brand **0** | Sealed — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0**. Production stays **UNSUPPORTED** |
| 3 — ggen + Taffy AOT preflight | Exact tip-cargo **ALIVE** — **7 passed; EXIT:0** @ tip `2685119bd` (`/tmp/gate1-taffy`); tip-wins absorb `c93277719`; tip7 reconfirm absorb `4ebcb9d46` @ `63da993cc`; [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) / [`TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP7-STANDING.md) | **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip `4af3c6ec5` reconfirm). **≠** `photolithography_path_crown_ALIVE`. **≠** `Foundry_ALIVE` | **0** (sealed) | Sealed at `2685119bd` — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| 3b — ui-layout digest consume (in-repo fence) | Exact tip-cargo **ALIVE** — **11 passed; EXIT:0** @ tip `2685119bd` (`/tmp/ggen-uilayout-exact-retip5`); land `59b9fae1c`; tip-wins absorb `6f0f64611`; [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md). Sibling `ui-layout-court.ts` **UNSUPPORTED** | **ALIVE (exact tip-cargo)** @ `2685119bd` (**≠** live-tip reconfirm). **≠** path crown **ALIVE** | **0** (sealed) | Sealed — replay only if sources drift: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-retip5 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| 4 — ggen_igniter clean-room | Lineage **ALIVE** held — cheap retip crown `4af998e9b` / retip3 `5a1129cc9` / absorb evidence `893070836` / prior `d0fec2f2f` / `f1492ed55`; mount `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc`; this land cheap probe `git diff --exit-code` → **0** (`/tmp/cursor/igniter-cheap.md`); BrandTheme manufacture compare **ALIVE** (held; [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md)) | Clean-room slice: **ALIVE (lineage)** + cheap reconfirm EXIT **0**. **≠** path crown **ALIVE**. **≠** Foundry `--live` **ALIVE** | Clean-room **0** (this land) | `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0**. Contract (optional): `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| 5 — zcode-cli POWL/OCEL | Path pin tip `83c954e5f` — [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); refusal lineage **ALIVE** @ `cd7c2433b` (sjira **6** + bound **4**); retip4 refusal tip-cargo **ALIVE** **6+4**/EXIT:**0** @ `54164206f` / absorb `491801669` — [`ZCODE-POWL-RETIP4-STANDING.md`](./ZCODE-POWL-RETIP4-STANDING.md); mount **PRESENT** `@ f32df08`; `--live` **BLOCKED_HITL** (`foundry_exit=1`; Model access not configured). **Not Foundry ALIVE** | Hop-5 slice: **PARTIAL**. Refusal tip-cargo: **ALIVE** (held). Foundry `--live`: **BLOCKED_HITL**. **refusal ≠ Foundry ALIVE**. **≠** path crown **ALIVE** | Refusal **0**; `--live` **1** | Refusal sealed — replay only if sources drift. Live (exact CLI ≡ Foundry HITL card): `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** (**not** `zcode /login`) |
| **Path crown** | This ledger @ tip `4af998e9b`; aggregate of hops 1–5 | **PARTIAL** / **non-ALIVE** — shui/Taffy tip-cargo **ALIVE** ≠ path crown **ALIVE** | — | Close hop-5 `--live` → **0** before any path-crown promote (hop-1 shui sealed; refusal tip-cargo sealed) |
| **Foundry / zero-HITL** | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md) | **BLOCKED_HITL** / **PARTIAL** — **not** Foundry `ALIVE` — **never invent** | `--live` **1** | Founder: `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **0** |

---

## 2. Gate1 court standing (post shui ALIVE close)

| Court | Cargo tip | Standing | EXIT | Notes |
|-------|-----------|----------|------|-------|
| `shui_public_prefix_court` | `a4b3d3fb4` | **ALIVE (exact tip-cargo)** | **0** (**4**/4) | Absorb `edf2ec22a`; tip6/prior tip7 PARTIAL superseded; tip7 thrash EXIT101 ≠ demote; ≠ live-tip reconfirm; ≠ path crown; ≠ Foundry |
| `brand_toml_{completeness,rhythm}_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**5+12**) | Land `bf3165322`; tip6 absorb `31b495a6a`; ≠ live-tip reconfirm; ≠ path crown |
| `taffy_aot_preflight_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**7**) | Absorb `c93277719`; tip7 reconfirm `4ebcb9d46`; ≠ path crown ≠ Foundry |
| `ui_layout_court` | `2685119bd` | **ALIVE (exact tip-cargo)** | **0** (**11**) | Land `59b9fae1c`; absorb `6f0f64611`; ≠ path crown |
| `zcode_powl_ocel` refuse path | `54164206f` | **ALIVE (exact tip-cargo refusal)** | **0** (**6+4**) | Absorb `491801669`; `--live` still **BLOCKED_HITL**; hop-5 **PARTIAL**; ≠ Foundry |

All Gate1 tip-cargo falsifiers sealed ALIVE at cargo tips — **≠** live-tip (`4af998e9b`) reconfirm. This land does **not** re-run cargo. Foundry remains **BLOCKED_HITL**.

---

## 3. Ancestry vs live crown `4af998e9b`

Zero-cost `git merge-base --is-ancestor` (all **ok**):

| Evidence SHA | Role |
|--------------|------|
| `cfd93c1cb` | Mandated tip floor (`cfd93c1cb+`) |
| `15eefb628` | Prior path ledger pin (shui-alive retip) |
| `4af3c6ec5` | Foundry HITL exact CLI / prior path pin |
| `6cdb97492` | Photolith founder-CLI absorb |
| `4af998e9b` | Live crown tip this pin (uilayout exact-tip6 absorb) |
| `a4b3d3fb4` | shui exact tip-cargo ALIVE (**4**/4) |
| `edf2ec22a` | shui exact tip-cargo absorb onto crown |
| `516fa63fc` | shui exact-tip6 ALIVE absorb (pre-`edf2ec22a`) |
| `2685119bd` | ALIVE-trio cargo tip (brand / Taffy / uilayout) |
| `c93277719` | Taffy exact tip-cargo absorb onto crown |
| `4ebcb9d46` | Taffy tip7 reconfirm absorb |
| `bf3165322` | brand.toml exact tip-cargo land |
| `31b495a6a` | brand exact-tip6 ALIVE absorb |
| `59b9fae1c` | ui_layout exact tip-cargo land |
| `6f0f64611` | ui_layout tip-wins absorb |
| `008a81d21` | shui lineage tip-cargo ALIVE |
| `cd7c2433b` | zcode refusal lineage tip-cargo ALIVE |
| `491801669` | zcode retip4 refusal tip-cargo ALIVE absorb |
| `5a1129cc9` | igniter clean-room retip3 crown |
| `83c954e5f` | zcode path honesty pin |
| `c4e757a14` | brand hot-swap exact tip-cargo ALIVE |
| `700ff435e` | Gate1 close retip (shui ALIVE 4/4 falsifiers ledger) |
| `d35482374` | shui tip7 PARTIAL EXIT101 absorb (≠ demote sealed ALIVE) |

---

## 4. Explicit non-claims

- Shui tip-cargo **ALIVE** (**4**/4 @ `a4b3d3fb4`) ≠ `photolithography_path_crown_ALIVE` ≠ `Foundry_ALIVE`.
- Taffy tip-cargo **ALIVE** (**7** @ `2685119bd`) ≠ `photolithography_path_crown_ALIVE` ≠ `Foundry_ALIVE`.
- Exact tip-cargo **ALIVE** @ cargo tips ≠ live-tip (`4af998e9b`) reconfirm.
- Shui tip7 thrash EXIT:**101** ≠ demote sealed shui ALIVE @ `a4b3d3fb4`.
- Igniter clean-room EXIT **0** ≠ path crown **ALIVE** ≠ Foundry `--live` **ALIVE**.
- Zcode refusal tip-cargo **ALIVE** ≠ Foundry / `--live` **ALIVE**; hop-5 remains **PARTIAL**.
- Source **PRESENT** / mount **PRESENT** ≠ any crown **ALIVE**.
- This ledger does **not** invent path-crown or Foundry `ALIVE`.
- No ManagePullRequest. No force-push. No cargo this land (Gate1 tip7 serial owns cargo).

---

## 5. Founder next command

```bash
cd /agent/repos/zcode-cli
export PATH="/home/ubuntu/.bun/bin:$PATH"
bun bin/zcode.js login          # Z.AI OAuth (HITL) — NOT `zcode /login`
bun scripts/powl-foundry-loop.ts --live
# PASS: foundry_exit=0 / JSON exitCode==0
# FAIL: foundry_exit=1 + Model access not configured → keep BLOCKED_HITL
# Antifalsifier: shui/Taffy tip-cargo ALIVE ≠ path_crown ALIVE ≠ Foundry ALIVE
```

---

## 6. Provenance

| Event | Note |
|-------|------|
| Path honesty audit | 2026-11-22 — `cursor/v2711-photolith-path-retip-bc54` (`bc-f7833d1a`) from live crown `e6d00965a`; docs-only; tip6 Gate1 **PARTIAL_ALIVE** EXIT **137**; igniter **ALIVE (lineage)**; hop-5 **PARTIAL** / `--live` **BLOCKED_HITL**; path crown **PARTIAL**; **no** Foundry `ALIVE` |
| Tip-wins absorb | 2026-11-22 — `cursor/v2711-photolith-taffy-absorb-4aa2` onto crown; side `28cd3c2e3`; tip pins retipped; path crown PARTIAL; Foundry BLOCKED_HITL; no ManagePullRequest |
| Taffy-alive path retip | 2026-11-22 — `cursor/v2711-photolith-taffy-alive-bc54` (`bc-f8dfcebc`) from live crown `1db31e20d` (past `59b9fae1c`); docs-only; Taffy hop **ALIVE (exact tip-cargo)** @ `2685119bd` / absorb `c93277719`; brand+uilayout sealed **ALIVE** @ same cargo tip; shui fence **PARTIAL_ALIVE** (tip7 in flight); igniter clean-room **ALIVE** + cheap EXIT **0**; hop-5 **PARTIAL** / Foundry **BLOCKED_HITL**; path crown **PARTIAL**; **Taffy ALIVE ≠ path_crown ALIVE ≠ Foundry ALIVE**; no cargo; no ManagePullRequest |
| Shui-alive path retip | 2026-11-22 — `cursor/v2711-photolith-shui-alive-bc54` (`bc-9dff3f82`) from live crown `15eefb628` (past `cfd93c1cb+` / Gate1 close); docs-only; shui hop **ALIVE (exact tip-cargo)** **4**/4 @ `a4b3d3fb4` / absorb `edf2ec22a`; Taffy/uilayout/igniter **ALIVE**; hop-5 **PARTIAL** / Foundry **BLOCKED_HITL**; path crown **PARTIAL**; **shui ALIVE ≠ path_crown ALIVE ≠ Foundry ALIVE**; no cargo; no ManagePullRequest |
| Founder-CLI honesty retip | 2026-11-22 — `cursor/v2711-photolith-path-retip-bc54` (`bc-18c3bbc1`) from live crown `4af3c6ec5`; docs-only; material gap = hop-5 exact next verifier still cited invalid `zcode /login` after Foundry HITL card pinned `bun bin/zcode.js login`; tip pin advanced; shui tip7 EXIT101 noted non-demoting; hop matrix standings held; path crown **PARTIAL**; Foundry **BLOCKED_HITL**; **no** cargo; **no** ManagePullRequest; **never invent Foundry `ALIVE`** |
| Igniter cheap EXIT0 retip | 2026-11-22 — `cursor/v2711-igniter-cheap-bc54` (`bc-b1fbe1c8`) from live crown `4af998e9b`; docs-only; mount `/home/ubuntu/ggen_igniter` @ `b5a15528…` `git diff --exit-code` → **0**; tip pin advanced; hop-4 **ALIVE (lineage)** + cheap EXIT **0** held; path crown **PARTIAL**; Foundry **BLOCKED_HITL**; **no** cargo; **no** ManagePullRequest; **never invent Foundry `ALIVE`** |

### Compact return

```text
tip=4af998e9be1804a5ea0ccf6b4f2e0de20493aaa4
hop1=ALIVE(shui exact tip-cargo@a4b3d3fb4 4/4 absorb edf2ec22a; Truex ALIVE@68d2d233) ≠ path_crown ≠ Foundry
hop2=PARTIAL_ALIVE(brand ALIVE exact tip-cargo@2685119bd 5+12; prod UNSUPPORTED)
hop3=ALIVE(exact tip-cargo)@2685119bd/absorb c93277719 (7) ≠ path_crown ≠ Foundry
hop3b=ALIVE(exact tip-cargo)@2685119bd (11) ≠ path_crown
hop4=ALIVE(lineage)+cheap EXIT0 @b5a15528 (≠ path/Foundry)
hop5=PARTIAL (refusal tip-cargo ALIVE@491801669; --live BLOCKED_HITL)
path_crown=PARTIAL  foundry=BLOCKED_HITL
exact_next_founder=cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live → 0
```
