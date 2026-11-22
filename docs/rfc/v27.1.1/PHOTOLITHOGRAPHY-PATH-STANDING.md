# Photolithography path — push-first honesty ledger (v27.1.1)

```text
tip=e6d00965a0b0b96df9e284d857de1371c89a3dd2
path=perimeter → .ttl/.toml → ggen+Taffy AOT → igniter clean-room → zcode POWL/OCEL
path_crown=PARTIAL (non-ALIVE)
foundry=BLOCKED_HITL (no Foundry ALIVE)
tip6_gate1=PARTIAL_ALIVE EXIT137 (do not upgrade)
side=cursor/v2711-photolith-path-retip-bc54
agent=bc-f7833d1a-c09b-5c25-9c10-847b2893780c
```

| Field | Value |
|-------|-------|
| Document ID | `PHOTOLITHOGRAPHY-PATH-STANDING-27.1.1` |
| Branch | `cursor/v2711-photolith-path-retip-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §13.1 hop chain |
| Companions | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md); [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md); [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md); [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); Gate1 tip6 courts (§2) |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `e6d00965a0b0b96df9e284d857de1371c89a3dd2` (`e6d00965a`; ⊇ Gate matrix §18 `c4e757a14` / `552c85f29` / `e24ce7a73`+) |
| Agent | `bc-f7833d1a-c09b-5c25-9c10-847b2893780c` |
| UTC | `2026-11-22T18:25Z` |
| Cargo | **NOT RUN** (docs-only; tip7 serial owns Gate1 EXIT0; tip6 thrash held) |
| Cheap probe | igniter mount `git diff --exit-code` → **0** @ `/home/ubuntu/ggen_igniter` `b5a15528…`; zcode mount **PRESENT** `/agent/repos/zcode-cli` @ `f32df08` |
| Status | LEDGER — path crown **PARTIAL** / **non-ALIVE**; Foundry `--live` **BLOCKED_HITL**; tip6 Gate1 courts **PARTIAL_ALIVE** EXIT **137** — **do not upgrade** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Honesty law (held):**
`PRESENT` ≠ `lineage tip-cargo ALIVE` ≠ `exact tip reconfirm ALIVE` ≠
`path crown ALIVE` ≠ `Foundry ALIVE`. Tip6 Gate1 courts are **PARTIAL_ALIVE**
EXIT **137** — do **not** upgrade. Igniter clean-room may hold lineage
**ALIVE** — say so. Foundry `--live` stays **BLOCKED_HITL** until founder
`zcode /login`.

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
| 1 — Perimeter transducer (`shui:` fence + Truex admit) | Lineage tip-cargo **ALIVE** @ `008a81d21` (**4** EXIT **0**; blob `3cda065f`); tip6 exact-tip **PARTIAL_ALIVE** @ tip_base `c661d228a` — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md); Truex product **ALIVE** @ `truex@68d2d233` (**6/6**); path pin [`PERIMETER-TTL-TOML-PATH-STANDING.md`](./PERIMETER-TTL-TOML-PATH-STANDING.md) | Fence: **PARTIAL_ALIVE** (tip6; lineage **ALIVE** held). Truex: **ALIVE**. Hop aggregate: **PARTIAL_ALIVE** | Tip6 fence **137** | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** (tip7 serial). Truex replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → **0** |
| 2 — `.ttl` / `.toml` (brand rhythm + completeness) | Lineage tip-cargo **ALIVE** @ `77eb46fd0` (completeness **5** + rhythm **12** EXIT **0**); tip6 exact-tip **PARTIAL_ALIVE** @ tip_base `552c85f29` — [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md); fixtures `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml`; DTCG/CASTLE inventory **UNSUPPORTED** | **PARTIAL_ALIVE** (tip6; lineage **ALIVE** held). Full DTCG/CASTLE: **UNSUPPORTED** | Tip6 **137** | `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-exact-tip6 --config build.jobs=1 -- --test-threads=1` → **5+12**/EXIT:**0** |
| 3 — ggen + Taffy AOT preflight | Lineage tip-cargo **ALIVE** @ `19c08a3b0` / absorb `cde02d1f2` (**7** EXIT **0**); tip6 exact-tip **PARTIAL_ALIVE** @ tip_base `552c85f29` — [`TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md); path slice [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md); court blob `d8923b07e` **PRESENT** | Path slice: **PARTIAL_ALIVE** (lineage **ALIVE** held; exact tip **PENDING**/EXIT **137**; consumer bind open) | Tip6 **137** | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy-tip6 cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| 3b — ui-layout digest consume (in-repo fence) | Lineage exact tip-cargo **ALIVE** @ `a508e6bae` (**11** EXIT **0**); tip6 exact-tip **PARTIAL_ALIVE** @ tip_base `4abd4adfb` — [`UILAYOUT-EXACT-TIP6-STANDING.md`](./UILAYOUT-EXACT-TIP6-STANDING.md) | **PARTIAL_ALIVE** (tip6; lineage **ALIVE** held) | Tip6 **137** | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip6 --config build.jobs=1 -- --test-threads=1` → **11**/EXIT:**0** |
| 4 — ggen_igniter clean-room | Lineage **ALIVE** held — retip3 crown `5a1129cc9` / absorb evidence `893070836` / prior `d0fec2f2f` / `f1492ed55`; mount `/home/ubuntu/ggen_igniter` @ `b5a15528eb6792dd366fe744ca87f24a80cc46fc`; this land cheap probe `git diff --exit-code` → **0**; BrandTheme manufacture compare **ALIVE** (held; [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md)) | Clean-room slice: **ALIVE (lineage)** + cheap reconfirm EXIT **0**. **≠** path crown **ALIVE**. **≠** Foundry `--live` **ALIVE** | Clean-room **0** (this land) | `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0**. Contract (optional): `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| 5 — zcode-cli POWL/OCEL | Path pin tip `83c954e5f` — [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); refusal lineage **ALIVE** @ `cd7c2433b` (sjira **6** + bound **4**); tip-cargo honesty **PARTIAL** EXIT **137** @ `2685119bd` — [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md); mount **PRESENT** `@ f32df08`; `--live` **BLOCKED_HITL** (`foundry_exit=1`; Model access not configured) | Hop-5 slice: **PARTIAL** / **PARTIAL_ALIVE**. Refusal lineage: **ALIVE** (held). Tip-cargo: **PARTIAL**. Foundry `--live`: **BLOCKED_HITL**. **refusal ≠ Foundry ALIVE** | Tip-cargo **137**; `--live` **1** | Tip-cargo: warm `CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked --config build.jobs=1 -- --test-threads=1` + `… remediation_bound --lib` → **6+4**/EXIT:**0**. Live: `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| **Path crown** | This ledger @ tip `e6d00965a`; aggregate of hops 1–5 | **PARTIAL** / **non-ALIVE** | — | Close tip7 Gate1 EXIT **0** serial **and** hop-5 `--live` → **0** before any path-crown promote |
| **Foundry / zero-HITL** | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md); [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) | **BLOCKED_HITL** / **PARTIAL** — **not** Foundry `ALIVE` | `--live` **1** | Founder: `cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live` → **0** |

---

## 2. Tip6 Gate1 courts (do not upgrade)

| Court | Tip base (probe) | Standing | EXIT | Lineage ALIVE held |
|-------|------------------|----------|------|--------------------|
| `shui_public_prefix_court` | `c661d228a` | **PARTIAL_ALIVE** | **137** | `008a81d21` (**4**) |
| `brand_toml_{completeness,rhythm}_court` | `552c85f29` | **PARTIAL_ALIVE** | **137** | `77eb46fd0` (**5+12**) |
| `taffy_aot_preflight_court` | `552c85f29` | **PARTIAL_ALIVE** | **137** | `19c08a3b0` (**7**) |
| `ui_layout_court` | `4abd4adfb` | **PARTIAL_ALIVE** | **137** | `a508e6bae` (**11**) |
| `zcode_powl_ocel` refuse path | `2685119bd` | **PARTIAL** | **137** | `cd7c2433b` (**6+4**) |

Tip7 serial owns Gate1 EXIT **0**. This land does **not** re-run cargo.

---

## 3. Ancestry vs live crown `e6d00965a`

Zero-cost `git merge-base --is-ancestor` (all **ok**):

| Evidence SHA | Role |
|--------------|------|
| `e24ce7a73` | Mandated crown floor |
| `008a81d21` | shui lineage tip-cargo ALIVE |
| `77eb46fd0` / `c91b22561` | brand.toml lineage tip-cargo ALIVE |
| `19c08a3b0` | Taffy lineage tip-cargo ALIVE |
| `a508e6bae` | ui_layout lineage tip-cargo ALIVE |
| `cd7c2433b` | zcode refusal lineage tip-cargo ALIVE |
| `5a1129cc9` | igniter clean-room retip3 crown |
| `83c954e5f` | zcode path honesty pin |
| `552c85f29` / `c4e757a14` | Gate matrix §18 tip cites (ancestors of `e6d00965a`) |

---

## 4. Explicit non-claims

- Tip6 EXIT **137** ≠ lineage demotion (lineage **ALIVE** held where cited).
- Lineage tip-cargo **ALIVE** ≠ exact tip reconfirm **ALIVE** on live crown.
- Igniter clean-room EXIT **0** ≠ path crown **ALIVE** ≠ Foundry `--live` **ALIVE**.
- Zcode refusal lineage **ALIVE** ≠ Foundry / `--live` **ALIVE**.
- Source **PRESENT** / mount **PRESENT** ≠ any crown **ALIVE**.
- This ledger does **not** invent path-crown or Foundry `ALIVE`.
- No ManagePullRequest. No force-push. No heavy cargo this land.

---

## 5. Founder next command

```bash
cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live
# PASS: foundry_exit=0
# FAIL: foundry_exit=1 + Model access not configured → keep BLOCKED_HITL
# Antifalsifier: path hops ALIVE/PARTIAL ≠ Foundry ALIVE; refusal ALIVE ≠ --live ALIVE
```

---

## 6. Provenance

| Event | Note |
|-------|------|
| Path honesty audit | 2026-11-22 — `cursor/v2711-photolith-path-retip-bc54` (`bc-f7833d1a`) from live crown `e6d00965a`; docs-only; cites tip6 Gate1 **PARTIAL_ALIVE** EXIT **137** (no upgrade); igniter clean-room **ALIVE (lineage)** + cheap EXIT **0**; hop-5 **PARTIAL** / `--live` **BLOCKED_HITL**; path crown **PARTIAL**; **no** Foundry `ALIVE`; no cargo; no ManagePullRequest |

### Compact return

```text
tip=e6d00965a0b0b96df9e284d857de1371c89a3dd2
hop1=PARTIAL_ALIVE(tip6 EXIT137; lineage ALIVE@008a81d21; Truex ALIVE@68d2d233)
hop2=PARTIAL_ALIVE(tip6 EXIT137; lineage ALIVE@77eb46fd0)
hop3=PARTIAL_ALIVE(tip6 EXIT137; lineage ALIVE@19c08a3b0)
hop3b=PARTIAL_ALIVE(tip6 EXIT137; lineage ALIVE@a508e6bae)
hop4=ALIVE(lineage)+cheap EXIT0 @b5a15528 (≠ path/Foundry)
hop5=PARTIAL (refusal lineage ALIVE@cd7c2433b; tip-cargo EXIT137; --live BLOCKED_HITL)
path_crown=PARTIAL  foundry=BLOCKED_HITL
exact_next_founder=cd /agent/repos/zcode-cli && zcode /login && bun scripts/powl-foundry-loop.ts --live → 0
```
